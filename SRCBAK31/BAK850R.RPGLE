000100110531     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PKXBND) ACTGRP(PROBAS)
000200110531     H*PARMS CVTOPT(*NONE)
000300110531     H DECEDIT('0,') DATEDIT(*DMY.)
000400110531      ********************************************************************
000500110531      *  BAK850R -  ANALISI AZIONI UTENTE                                *
000600110531      ********************************************************************
000700110531      *------------------------------------------------------------------*
000800110531      *--- File Input
000900110531     FKPLOG04L  IF   E           K DISK    RENAME(KPLOG000:KPLOG004)
001000110531X0202FKPLOG05L  IF   E           K DISK    RENAME(KPLOG000:KPLOG005)
001100110531  "  FKPLOG06L  IF   E           K DISK    RENAME(KPLOG000:KPLOG006)
001200110531X0202FKPLOG07L  IF   E           K DISK    RENAME(KPLOG000:KPLOG007)
001300110531X0308FKPLOG08L  IF   E           K DISK    RENAME(KPLOG000:KPLOG008)
001400110531  "  FKPLOG09L  IF   E           K DISK    RENAME(KPLOG000:KPLOG009)
001500110531X0308FKPLOG10L  IF   E           K DISK    RENAME(KPLOG000:KPLOG010)
001600110531     FKFSIF01L  IF   E           K DISK
001700110531X0127FKPPRF01L  IF   E           K DISK
001800110531      *--- File Update
001900110531     FKPLOG01L  UF   E           K DISK    RENAME(KPLOG000:KPLOG001)
002000110531      *--- File Display
002100110531     FBAK850D   CF   E             WORKSTN
002200110531     F                                     SFILE(K850S1:S1NRR)
002300110531     F                                     INFDS(INFS1)
002400110531      *------------------------------------------------------------------*
002500110531      *--- Schiera per QCMDEXC
002600110531     D CMD             S             80    DIM(7) CTDATA PERRCD(1)
002700110531     D CDS             S             80    DIM(1) CTDATA PERRCD(1)
002800110531      *--- Schiera Mesi dell'anno
002900110531     D MES             S              2  0 DIM(12) CTDATA PERRCD(12)
003000110531      *--- Schiera WORK
003100110531     D SK1             S              1    DIM(50)
003200110531     D JB              S              1    DIM(60)
003300110531     D WA1             S              1    DIM(4)
003400110531     D WA2             S              1    DIM(4)
003500110531      *------------------------------------------------------------------*
003600110531     D KPJBA         E DS
003700110531      *--- INFDS del Video
003800110531     D INFS1           DS           512
003900110531     D  BINRIG               370    370
004000110531     D  BINCOL               371    371
004100110531     D  SFLNRR               378    379B 0
004200110531      *--- DS Data
004300110531     D                 DS
004400110531     D  DATA                   1      6  0
004500110531     D  GG                     1      2  0
004600110531     D  MM                     3      4  0
004700110531     D  AA                     5      6  0
004800110531      *--- DS Data
004900110531     D                 DS
005000110531     D  DATAV                  1      7  0
005100110531     D  SSV                    1      1  0
005200110531     D  AAV                    2      3  0
005300110531     D  MMV                    4      5  0
005400110531     D  GGV                    6      7  0
005500110531      *--- DS DATA / ORA
005600110531     D DATORA          DS
005700110531     D  DATS                   1      6  0
005800110531     D  DATAA                  1      6
005900110531     D  ORAA                   7     12
006000110531     D  ORAS                   7     12  0
006100110531     D  GG4                    1      2  0
006200110531     D  MM4                    3      4  0
006300110531     D  AA4                    5      6  0
006400110531     D  ORA4                   7      8  0
006500110531     D  MIN4                   9     10  0
006600110531     D  SEC4                  11     12  0
006700110531      *--- DATA / ORA SCHEDULAZIONE
006800110531     D SCHEDS          DS
006900110531     D  DSCHE                  1      6
007000110531     D  OSCHE                  7     10
007100110531      *--- DS per CHKOBJ
007200110531     D                 DS
007300110531     D  CMD1                   1     80
007400110531     D  NOME                  12     21
007500110531     D  TIPO                  32     38
007600110531      *--- DS per Comandi JOB
007700110531     D                 DS
007800110531     D  CMD2                   1     80
007900110531     D  NUMJOB                12     17
008000110531     D  NOMLAV                12     21
008100110531     D  PRUNOJ                19     39
008200110531X0281X***  PTYBLK                41     50
008300110531     D  NEWPR                 49     49
008400110531     D  NEWJQ                 58     67
008500110531      *--- DS per Comandi JOB
008600110531     D                 DS
008700110531     D  CMD3                   1     80
008800110531     D  NMLAV                 12     21
008900110531     D  NEWDS                 50     55
009000110531     D  NEWOS                 66     71
009100110531      *--- DS per KPJBA Azione
009200110531     D JBADS           DS           502
009300110531     D  NPU                    1     10
009400110531     D  NTM                   20     29
009500110531     D  NRL                   47     52  0
009600110531     D  STJ                   53     53
009700110531     D  CDA                   60     63
009800110531     D  TIA                   64     64
009900110531     D  NSI                   92    101
010000110531     D  PTY                  155    155
010100110531     D  JBUF                 209    246
010200110531     D  JBU1                 247    306
010300110531     D  JBU2                 307    366
010400110531     D  JBU3                 367    426
010500110531     D  JBU4                 427    486
010600110531     D  JBU5                 487    502
010700110531      *---
010800110531      * PARAMETRI DI I/O di BAK036R
010900110531X0195D BAK036DS      E DS                  INZ
011000110531  "  X***** DS036           DS
011100110531  "  X*****  JNAM36                 1     10
011200110531  "  X*****  UNAM36                11     20
011300110531  "  X*****  JNBR36                21     26
011400110531  "  X*****  JOBQ36                27     36
011500110531  "  X*****  JOBP36                37     37
011600110531X0195X*****  JSTS36                38     47
011700110531X0026D*-------------
011800110531  "  D* Dati di ambiente ottenuti da XSOC
011900110531  "  D SOC001        E DS                  EXTNAME(XSOC001DS)
012000110531  "  D  SALDOC       E                     EXTFLD(XSCSALDOC)
012100110531  "  D  SALDOA       E                     EXTFLD(XSCSALDOA)
012200110531  "  D*-------------
012300110531  "  D* DS Interna per dati di output di XSOC
012400110531  "  D XSOCDS          DS          1000
012500110531  "  D*-------------
012600110531  "  D* REPERISCO NOME PGM
012700110531  "  D STATUS         SDS           333
012800110531X0026D  DSPGM            *PROC
012900110531X0265X*** ex-X0099 *-------------
013000110531  "  X***     "    Ricerca azioni
013100110531X0265X*** ex-X0099 BAK986DS      E DS                  INZ
013200110531X0099D*-------------
013300110531X0097D* S.I.
013400110531  "  D K988DS        E DS                  EXTNAME(BAK988DS)
013500110531  "  D  NSI988       E                     EXTFLD(KNMSI988)
013600110531  "  D  DSI988       E                     EXTFLD(KDSSI988)
013700110531  "  D* PROFILI
013800110531  "  D K980DS        E DS                  EXTNAME(BAK980DS)
013900110531  "  D  PRF980       E                     EXTFLD(PROFILO980)
014000110531  "  D  DES980       E                     EXTFLD(DESCR980)
014100110531  "  D  MNI980       E                     EXTFLD(MENUINZ980)
014200110531  "  D  SIF980       E                     EXTFLD(SISINF980)
014300110531  "  D*-------------
014400110531  "  D CURSOR          DS
014500110531  "  D  RIRI                   1      2B 0 INZ
014600110531  "  D  R$$                    2      2
014700110531  "  D  COCO                   3      4B 0 INZ
014800110531X0097D  C$$                    4      4
014900110531X0127D*-------------
015000110531X0180X****** ex-X0127 Scomposizione campo PRFUSF
015100110531  "  X***** ex-X0127 PRFUSF          DS
015200110531  "  X***** ex-X0127 PRFPgA                 1     10A
015300110531  "  X***** ex-X0127 PRFLbA                11     20A
015400110531  "  X***** ex-X0127 PRFAAU                21     21A
015500110531X0180X*-------------
015600110531     D*-- Costanti
015700110531     D PAGINA          C                   CONST(12)
015800110531     D DAORC           C                   CONST('-Immiss.-')
015900110531     D COPRC           C                   CONST('- Input -')
016000110531     D DAORA           C                   CONST(' -Inizio-')
016100110531     D COPRA           C                   CONST('- Esec. -')
016200110531     D DAORE           C                   CONST(' - Fine -')
016300110531     D COPRE           C                   CONST('- Esec. -')
016400110531X0191D*-------------
016500110531  "  D* Parametri User-Exit "controlla rcd LOG" BAK850RU1
016600110531  "  D KPLOGDSU1     E DS                  ExtName(KPLOG00F)
016700110531  "  D RtnCdeU1        S              1A
016800110531  "  D*-------------
016900110531  "  D* Parametri User-Exit "controlla coda batch" BAK850RU2
017000110531  "  D JobQU2          S             10A
017100110531X0191D RtnCdeU2        S              1A
017200110531X0237D*-------------
017300110531  "  D* Parametri User-Exit "contr. possibilità limitate utente" BAK850RU3
017400110531X0237D RtnCdeU3        S              1A
017500110531X0127D*-------------
017600110531X0282D WPagina         S              4S 0
017700110531X0282D $F13            S              1A
017800110531X0127D $PgmAziUte      S              1A
017900110531X0265X*** ex-X0099 File            S             10A
018000110531  "  X***     "    SisInf          S             10A
018100110531  "  X***     "    Lingua          S              1A
018200110531  "  X***     "    Scope           S              1A
018300110531X0265X*** ex-X0099 Errore          S              1A
018400110531X0174X*LibL            S            240A
018500110531X0265X*** ex-X0174 LibL            S           2500A
018600110531X0265X*** ex-X0099 $OvrDB          S              1A
018700110531X0202D $WildPRUT       S               N
018800110531  "  D $WildTERM       S               N
018900110531  "  D $WildCINP       S               N
019000110531  "  D D0PRUT_Low      S                   Like(D0PRUT)
019100110531  "  D D0PRUT_High     S                   Like(D0PRUT)
019200110531  "  D D0TERM_Low      S                   Like(D0TERM)
019300110531  "  D D0TERM_High     S                   Like(D0TERM)
019400110531  "  D D0CINP_Low      S                   Like(D0CINP)
019500110531X0202D D0CINP_High     S                   Like(D0CINP)
019600110531X0099D*-------------
019700110531X0308D WIsoDtDA        S               D   inz
019800110531X0308D WIsoDtA         S               D   inz
019900110531X0308D WDacA           S                   Like(LogDac)
020000110531X0308D WDaiA           S                   Like(LogDai)
020100110531X0308D WDafA           S                   Like(LogDaf)
020200110531      *----------------------------------------------------*
020300110531      *   FLUSSO PRINCIPALE                                *
020400110531      *----------------------------------------------------*
020500110531      *
020600110531     C                   SETON                                        09
020700110531     C                   EXSR      DEFCAM
020800110531     C     *INKC         DOWEQ     '0'
020900110531X0026C     *INKL         ANDEQ     '0'
021000110531      * Emissione video Guida
021100110531X0097C     EMTD0         TAG
021200110531  "  C     *IN99         IFEQ      *ON
021300110531  "  C                   SETOFF                                       99
021400110531  "  C                   WRITE     K850D0
021500110531  "  C                   SETON                                        99
021600110531X0097C                   ENDIF
021700110531     C                   EXFMT     K850D0
021800110531     C     *INKC         IFEQ      '0'
021900110531X0026C     *INKL         ANDEQ     '0'
022000110531X0097 * Ricerca
022100110531  "  C     *INKD         IFEQ      *ON
022200110531  "  C                   EXSR      SEARCH
022300110531  "  C                   GOTO      EMTD0
022400110531X0097C                   ENDIF
022500110531      * Controllo video Guida
022600110531     C                   EXSR      CTRD0
022700110531     C     *IN99         IFEQ      '0'
022800110531     C     SUBF          TAG
022900110531      * Gestione Subfile Azioni
023000110531     C                   EXSR      GESS1
023100110531     C                   ENDIF
023200110531     C                   ENDIF
023300110531     C                   ENDDO
023400110531      * Fine Elaborazione
023500110531     C                   SETON                                        LR
023600110531      *
023700110531      *----------------------------------------------------*
023800110531      * CTRD0 - CONTROLLO VIDEO GUIDA                      *
023900110531      *----------------------------------------------------*
024000110531      *
024100110531     C     CTRD0         BEGSR
024200110531     C                   SETOFF                                       999897
024300110531     C                   SETOFF                                       969594
024400110531     C                   SETOFF                                       939291
024500110531     C                   SETOFF                                       90
024600110531X0308C                   SETOFF                                       596061
024700110531  "  C                   SETOFF                                       62
024800110531  "  C                   Clear                   WDacA
024900110531  "  C                   Clear                   WDaiA
025000110531X0308C                   Clear                   WDafA
025100110531      *
025200110531      * Controllo Tipo Analisi Azioni
025300110531     C     D0TIAN        IFNE      'C'
025400110531     C     D0TIAN        ANDNE     'A'
025500110531     C     D0TIAN        ANDNE     'E'
025600110531     C                   SETON                                        9599
025700110531     C                   ENDIF
025800110531      *
025900110531      * Controllo Tipo Azioni
026000110531     C     D0TIAZ        IFNE      ' '
026100110531     C     D0TIAZ        ANDNE     'B'
026200110531     C     D0TIAZ        ANDNE     'I'
026300110531     C                   SETON                                        9199
026400110531     C                   ENDIF
026500110531     C     D0PRUT        IFNE      *BLANK
026600110531      * Controllo esistenza Profilo Utente
026700110531X0202 * (se non è un nome generico)
026800110531  "  C                   If        %SubSt(D0PRUT:%Len(%TrimR(D0PRUT)):1)
026900110531X0202C                              <> '*'
027000110531     C                   MOVE      CMD(1)        CMD1
027100110531     C                   MOVEL     D0PRUT        NOME
027200110531     C                   MOVEL     '*USRPRF'     TIPO
027300110531     C                   CALL      'QCMDEXC'                            98
027400110531     C                   PARM                    CMD1
027500110531     C                   PARM      80            LUNG             15 5
027600110531     C   98              SETON                                        99
027700110531X0202C                   EndIf
027800110531     C                   ENDIF
027900110531     C     D0SINF        IFNE      *BLANK
028000110531      * Controllo esistenza Sistema Informativo
028100110531     C     D0SINF        CHAIN     KFSIF01L                           92
028200110531     C   92              SETON                                        99
028300110531     C                   ENDIF
028400110531     C     D0TERM        IFNE      *BLANK
028500110531      * Controllo esistenza Terminale
028600110531X0202 * (se non è un nome generico)
028700110531  "  C                   If        %SubSt(D0TERM:%Len(%TrimR(D0TERM)):1)
028800110531X0202C                              <> '*'
028900110531     C                   MOVE      CMD(1)        CMD1
029000110531     C                   MOVEL     D0TERM        NOME
029100110531     C                   MOVEL     '*DEVD  '     TIPO
029200110531     C                   CALL      'QCMDEXC'                            97
029300110531     C                   PARM                    CMD1
029400110531     C                   PARM      80            LUNG
029500110531     C   97              SETON                                        99
029600110531X0202C                   EndIf
029700110531     C                   ENDIF
029800110531     C     D0CINP        IFNE      *BLANK
029900110531      * Controllo esistenza Coda Input
030000110531X0202 * (se non è un nome generico)
030100110531  "  C                   If        %SubSt(D0CINP:%Len(%TrimR(D0CINP)):1)
030200110531X0202C                              <> '*'
030300110531     C                   MOVE      CMD(1)        CMD1
030400110531     C                   MOVEL     D0CINP        NOME
030500110531     C                   MOVEL     '*JOBQ  '     TIPO
030600110531     C                   CALL      'QCMDEXC'                            96
030700110531     C                   PARM                    CMD1
030800110531     C                   PARM      80            LUNG
030900110531     C   96              SETON                                        99
031000110531X0202C                   EndIf
031100110531     C                   ENDIF
031200110531      * Controllo Data
031300110531X0202X*****D0TIAN        IFNE      'C'
031400110531X0202X*****D0MM          ORNE      0
031500110531X0308x**   D0MM          IFNE      0
031600110531  "  x**   D0AA          ORNE      0
031700110531  "  x**   D0GG          ORNE      0
031800110531  "  x**   D0MM          IFGT      12
031900110531  "  x**   D0MM          ORLT      1
032000110531  "  x**                 SETON                                        9499
032100110531  "  x**                 ELSE
032200110531  "  x**   D0MM          IFEQ      2
032300110531  "  x**   D0AA          DIV       4             RIS               2 0
032400110531  "  x**                 MVR                     RES               1 0
032500110531  "  x**   RES           IFEQ      0
032600110531  "  x**                 Z-ADD     29            MES(2)
032700110531  "  x**                 ELSE
032800110531  "  x**                 Z-ADD     28            MES(2)
032900110531  "  x**                 ENDIF
033000110531  "  x**                 ENDIF
033100110531  "  x**   D0GG          IFGT      MES(D0MM)
033200110531  "  x**   D0GG          ORLT      1
033300110531  "  x**                 SETON                                        9499
033400110531  "  x**                 ENDIF
033500110531  "  x**                 ENDIF
033600110531  "  x**                 ENDIF
033700110531  "   *
033800110531  "   * NOTA BENE, serve una data !!!!
033900110531  "   * Controllo Data DA
034000110531  "  C                   Clear                   WISODTda
034100110531  "  C                   Z-ADD     D0GG          GG
034200110531  "  C                   Z-ADD     D0MM          MM
034300110531  "  C                   Z-ADD     D0AA          AA
034400110531  "  C     *DMY          TEST(D)                 Data                   94
034500110531  "  C   94              SetON                                        99
034600110531  "  C  N94*DMY          Move      Data          WISODTda
034700110531  "   *
034800110531  "   * Controllo Data A
034900110531  "  C                   Clear                   WISODTa
035000110531  "  C                   Z-ADD     D0GGa         GG
035100110531  "  C                   Z-ADD     D0MMa         MM
035200110531  "  C                   Z-ADD     D0AAa         AA
035300110531  "  C     *DMY          TEST(D)                 Data                   60
035400110531  "  C   60              SetON                                        99
035500110531  "  C  N60*DMY          Move      Data          WISODTa
035600110531  "   *
035700110531  "   * Controllo range date
035800110531  "  C                   If        WISODTa < WISODtda
035900110531  "  C                   SetON                                        9961
036000110531X0308C                   ENDIF
036100110531      *
036200110531      *
036300110531      * Controllo Ora
036400110531X0308C                   Clear                   WOraDA
036500110531     C     D0ORA         IFGT      23
036600110531     C     D0MIN         ORGT      59
036700110531     C     D0SEC         ORGT      59
036800110531     C                   SETON                                        9399
036900110531X0308C                   Else
037000110531  "  C                   Z-ADD     D0Ora         GG
037100110531  "  C                   Z-ADD     D0Min         MM
037200110531  "  C                   Z-ADD     D0Sec         AA
037300110531X0308C                   Z-ADD     Data          WOraDA            6 0
037400110531     C                   ENDIF
037500110531      *
037600110531X0308 * Controllo Ora a
037700110531 "   C                   Clear                   WOraA             6 0
037800110531 "   C     D0ORAa        IFGT      23
037900110531 "   C     D0MINa        ORGT      59
038000110531 "   C     D0SECa        ORGT      59
038100110531 "   C                   SETON                                        5999
038200110531 "   C                   Else
038300110531 "   C                   Z-ADD     D0Oraa        GG
038400110531 "   C                   Z-ADD     D0Mina        MM
038500110531 "   C                   Z-ADD     D0Seca        AA
038600110531 "   C                   Z-ADD     Data          WOraA
038700110531 "   C                   ENDIF
038800110531 "    *
038900110531 "    * Controllo range ore
039000110531 "   C                   If        WISODTa = WISODtda
039100110531 "   C                             and WOraA < WOraDA
039200110531 "   C                   SetON                                        9962
039300110531X0308C                   ENDIF
039400110531      *
039500110531     C                   ENDSR
039600110531      *
039700110531      *----------------------------------------------------*
039800110531      * GESS1 - GESTIONE SFL AZIONI                        *
039900110531      *----------------------------------------------------*
040000110531      *
040100110531     C     GESS1         BEGSR
040200110531X0202 *
040300110531  "  C                   Eval      $WildPRUT = %Scan('*':D0PRUT) > 1
040400110531  "  C                   Eval      $WildTERM = %Scan('*':D0TERM) > 1
040500110531X0202C                   Eval      $WildCINP = %Scan('*':D0CINP) > 1
040600110531X0277X*                  Move      *All'9'       All9             20
040700110531X0277C                   Move      *hival        All9             20
040800110531X0202 *
040900110531     C     *INKE         DOUEQ     '0'
041000110531      * Inizializzazione Subfile
041100110531     C                   EXSR      INZS1
041200110531      * Ora parzializzazione
041300110531     C                   Z-ADD     D0ORA         GG
041400110531     C                   Z-ADD     D0MIN         MM
041500110531     C                   Z-ADD     D0SEC         AA
041600110531     C                   Z-ADD     DATA          ORA               6 0
041700110531      * Inversione data parzializzazione
041800110531     C                   Z-ADD     D0GG          AA
041900110531     C                   Z-ADD     D0MM          MM
042000110531     C                   Z-ADD     D0AA          GG
042100110531X0202 * Imposta limiti profilo parzializzazione
042200110531  "  C                   If        $WildPRUT = *On
042300110531  "  C                   Eval      D0PRUT_Low = %SubSt(D0PRUT:1:
042400110531  "  C                                          %Len(%TrimR(D0PRUT))-1)
042500110531  "  C     D0PRUT_Low    Cat       All9:0        D0PRUT_High
042600110531  "  C                   Else
042700110531  "  C                   Eval      D0PRUT_Low = D0PRUT
042800110531  "  C                   Eval      D0PRUT_High = D0PRUT
042900110531  "  C                   EndIf
043000110531  "   * Imposta limiti terminale parzializzazione
043100110531  "  C                   If        $WildTERM = *On
043200110531  "  C                   Eval      D0TERM_Low = %SubSt(D0TERM:1:
043300110531  "  C                                          %Len(%TrimR(D0TERM))-1)
043400110531  "  C     D0TERM_Low    Cat       All9:0        D0TERM_High
043500110531  "  C                   Else
043600110531  "  C                   Eval      D0TERM_Low = D0TERM
043700110531  "  C                   Eval      D0TERM_High = D0TERM
043800110531  "  C                   EndIf
043900110531  "   * Imposto limiti coda parzializzazione
044000110531  "  C                   If        $WildCINP = *On
044100110531  "  C                   Eval      D0CINP_Low = %SubSt(D0CINP:1:
044200110531  "  C                                          %Len(%TrimR(D0CINP))-1)
044300110531  "  C     D0CINP_Low    Cat       All9:0        D0CINP_High
044400110531  "  C                   Else
044500110531  "  C                   Eval      D0CINP_Low = D0CINP
044600110531  "  C                   Eval      D0CINP_High = D0CINP
044700110531X0202C                   EndIf
044800110531      *
044900110531X0308 * Imposto il limite superiore elab. per Tipo Analisi Azioni
045000110531  "  C                   If        WIsoDtA <> *loval
045100110531  "  C                   Select
045200110531  "  C     D0TIAN        WhenEQ    'C'
045300110531  "  C     *CYMD         Move      WIsoDtA       WDacA
045400110531  "  C     D0TIAN        WhenEQ    'A'
045500110531  "  C     *CYMD         Move      WIsoDtA       WDaiA
045600110531  "  C     D0TIAN        WhenEQ    'E'
045700110531  "  C     *CYMD         Move      WIsoDtA       WDafA
045800110531  "  C                   EndSL
045900110531X0308C                   Endif
046000110531      *
046100110531      * Posizionamento e prima Lettura per Tipo Analisi Azioni
046200110531     C                   SELECT
046300110531      * in coda
046400110531     C     D0TIAN        WHENEQ    'C'
046500110531     C                   Z-ADD     0             KDF
046600110531     C                   Z-ADD     0             KOF
046700110531     C                   Z-ADD     0             KDI
046800110531     C                   Z-ADD     0             KOI
046900110531X0247C                   CLEAR                   KPC
047000110531X0202C                   Z-ADD     DATA          KDC
047100110531  "  C     D0AA          IFLT      40
047200110531  "  C     DATA          ANDNE     0
047300110531  "  C                   MOVEL     1             KDC
047400110531  "  C                   ENDIF
047500110531  "  C                   Z-ADD     ORA           KOC
047600110531  "  C                   MOVE      D0PRUT_Low    KUS
047700110531  "  C                   MOVE      D0CINP_Low    KCI
047800110531  "  X*****KLOG2         SETLL     KPLOG04L
047900110531  "  X*****KLOG2         READE     KPLOG04L                               72
048000110531  "   * - posizionamento
048100110531  "  C                   ExSr      SetKPLOG_C
048200110531  "   * - lettura
048300110531X0202C                   ExSr      ReadKPLOG_C
048400110531      *
048500110531      * attive
048600110531     C     D0TIAN        WHENEQ    'A'
048700110531     C                   Z-ADD     0             KDF
048800110531     C                   Z-ADD     0             KOF
048900110531     C                   Z-ADD     DATA          KDI
049000110531     C     D0AA          IFLT      40
049100110531X0202C     DATA          ANDNE     0
049200110531     C                   MOVEL     1             KDI
049300110531     C                   ENDIF
049400110531     C                   Z-ADD     ORA           KOI
049500110531X0202C                   MOVE      D0PRUT_Low    KUS
049600110531  "  C                   MOVE      D0CINP_Low    KCE
049700110531  "  X*****KLOG2         SETLL     KPLOG04L
049800110531  "  X*****KDF           READE     KPLOG04L                               72
049900110531  "   * - posizionamento
050000110531  "  C                   ExSr      SetKPLOG_A
050100110531  "   * - lettura
050200110531X0202C                   ExSr      ReadKPLOG_A
050300110531      *
050400110531      * eseguite
050500110531     C     D0TIAN        WHENEQ    'E'
050600110531X0308C                   movel     D0Azn         KCDA
050700110531     C                   Z-ADD     DATA          KDF
050800110531     C     D0AA          IFLT      40
050900110531X0202C     DATA          ANDNE     0
051000110531     C                   MOVEL     1             KDF
051100110531     C                   ENDIF
051200110531     C                   Z-ADD     ORA           KOF
051300110531X0202X*****              Z-ADD     0             KDI
051400110531  "  X*****              Z-ADD     0             KOI
051500110531  "  C                   MOVE      D0PRUT_Low    KUS
051600110531  "  C                   MOVE      D0CINP_Low    KCE
051700110531  "  X*****KLOG2         SETLL     KPLOG04L
051800110531  "  X*****              READ      KPLOG04L                               72
051900110531  "   * - posizionamento
052000110531  "  C                   ExSr      SetKPLOG_E
052100110531  "   * - lettura
052200110531X0202C                   ExSr      ReadKPLOG_E
052300110531     C                   ENDSL
052400110531      *
052500110531      * Caricamento 1a pagina Subfile
052600110531     C                   EXSR      ROLLS1
052700110531      *
052800110531     C     S1NRR         COMP      0                                      90
052900110531     C     *IN90         IFEQ      '0'
053000110531     C                   Z-ADD     S1NRR         C1NRR
053100110531      * Caricamento 2a pagina Subfile
053200110531     C                   EXSR      ROLLS1
053300110531      *
053400110531     C     *INKC         DOWEQ     '0'
053500110531     C     *INKE         ANDEQ     '0'
053600110531     C     *INKL         ANDEQ     '0'
053700110531      *
053800110531      * Emissione Subfile Azioni
053900110531     C                   EXSR      EMTS1
054000110531      *
054100110531     C     *INKC         IFEQ      '0'
054200110531     C     *INKE         ANDEQ     '0'
054300110531     C     *INKL         ANDEQ     '0'
054400110531      *
054500110531X0282C                   SELECT
054600110531  "   * F13=Ripetizione
054700110531  "  C     *INKM         WHENEQ    '1'
054800110531  "  C                   Eval      $F13 = *On
054900110531  "  C                   EXSR      GesF13
055000110531  "   * Roll
055100110531  "  X***  *IN69         IFEQ      '1'
055200110531X0282C     *IN69         WHENEQ    '1'
055300110531      * Caricamento altre 2 pagine
055400110531     C                   EXSR      ROLLS1
055500110531     C                   Z-ADD     S1NRR         C1NRR
055600110531     C                   EXSR      ROLLS1
055700110531X0282X***                ELSE
055800110531      *
055900110531      * Controllo opzioni inserite
056000110531X0282C                   OTHER
056100110531     C                   EXSR      CTRS1
056200110531X0282X***                ENDIF
056300110531X0282C                   ENDSL
056400110531     C                   ENDIF
056500110531     C                   ENDDO
056600110531      *
056700110531X0026C                   MOVE      '0'           *INKL
056800110531     C                   ENDIF
056900110531     C                   ENDDO
057000110531     C                   ENDSR
057100110531X0202 *----------------------------------------------------*
057200110531  "   * Posizionamento KPLOG analisi azioni in CODA        *
057300110531  "   *----------------------------------------------------*
057400110531X0202C     SetKPLOG_C    BEGSR
057500110531      *
057600110531     C                   Select
057700110531      *
057800110531      * 1) profilo
057900110531     C                   When      D0PRUT <> *Blank
058000110531X0247X***  k07log05      SetLL     KPLOG05L
058100110531X0247C     k08log05      SetLL     KPLOG05L
058200110531      *
058300110531      * 2) coda input
058400110531     C                   When      D0CINP <> *Blank
058500110531X0247X***  k07log07      SetLL     KPLOG07L
058600110531X0247C     k08log07      SetLL     KPLOG07L
058700110531      *
058800110531      * 3) altro...
058900110531     C                   Other
059000110531X0247X***  k06log04      SetLL     KPLOG04L
059100110531X0247C     k07log04      SetLL     KPLOG04L
059200110531      *
059300110531     C                   EndSl
059400110531      *
059500110531X0202C                   ENDSR
059600110531X0202 *----------------------------------------------------*
059700110531  "   * Lettura KPLOG analisi azioni in CODA               *
059800110531  "   *----------------------------------------------------*
059900110531X0202C     ReadKPLOG_C   BEGSR
060000110531      *
060100110531     C                   Select
060200110531      *
060300110531      * 1) profilo
060400110531     C                   When      D0PRUT <> *Blank
060500110531      * - generico
060600110531     C                   If        $WildPRUT = *On
060700110531     C                   Read      KPLOG05L                               72
060800110531     C                   Eval      *In72 = *In72 or
060900110531     C                             LOGNMU > D0PRUT_High
061000110531      * - fisso
061100110531     C                   Else
061200110531     C     k05log05      ReadE     KPLOG05L                               72
061300110531     C                   EndIf
061400110531      *
061500110531      * 2) coda input
061600110531     C                   When      D0CINP <> *Blank
061700110531      * - generica
061800110531     C                   If        $WildCINP = *On
061900110531     C                   Read      KPLOG07L                               72
062000110531     C                   Eval      *In72 = *In72 or
062100110531     C                             LOGCBI > D0CINP_High
062200110531      * - fissa
062300110531     C                   Else
062400110531     C     k05log07      ReadE     KPLOG07L                               72
062500110531     C                   EndIf
062600110531      *
062700110531      * 3) altro...
062800110531     C                   Other
062900110531     C     k04log04      ReadE     KPLOG04L                               72
063000110531      *
063100110531     C                   EndSl
063200110531      *
063300110531X0202C                   ENDSR
063400110531X0202 *----------------------------------------------------*
063500110531  "   * Posizionamento KPLOG analisi azioni ATTIVE         *
063600110531  "   *----------------------------------------------------*
063700110531X0202C     SetKPLOG_A    BEGSR
063800110531      *
063900110531     C                   Select
064000110531      *
064100110531      * 1) profilo
064200110531     C                   When      D0PRUT <> *Blank
064300110531     C     k05log05      SetLL     KPLOG05L
064400110531      *
064500110531      * 2) coda esecuzione
064600110531     C                   When      D0CINP <> *Blank
064700110531     C     k05log06      SetLL     KPLOG06L
064800110531      *
064900110531      * 3) altro...
065000110531     C                   Other
065100110531     C     k04log04      SetLL     KPLOG04L
065200110531      *
065300110531     C                   EndSl
065400110531      *
065500110531X0202C                   ENDSR
065600110531X0202 *----------------------------------------------------*
065700110531  "   * Lettura KPLOG analisi azioni ATTIVE                *
065800110531  "   *----------------------------------------------------*
065900110531X0202C     ReadKPLOG_A   BEGSR
066000110531      *
066100110531     C                   Select
066200110531      *
066300110531      * 1) profilo
066400110531     C                   When      D0PRUT <> *Blank
066500110531      * - generico
066600110531     C                   If        $WildPRUT = *On
066700110531     C                   Read      KPLOG05L                               72
066800110531     C                   Eval      *In72 = *In72 or
066900110531     C                             LOGNMU > D0PRUT_High
067000110531      * - fisso
067100110531     C                   Else
067200110531     C     k03log05      ReadE     KPLOG05L                               72
067300110531     C                   EndIf
067400110531      *
067500110531      * 2) coda esecuzione
067600110531     C                   When      D0CINP <> *Blank
067700110531      * - generica
067800110531     C                   If        $WildCINP = *On
067900110531     C                   Read      KPLOG06L                               72
068000110531     C                   Eval      *In72 = *In72 or
068100110531     C                             LOGCBE > D0CINP_High
068200110531      * - fissa
068300110531     C                   Else
068400110531     C     k03log06      ReadE     KPLOG06L                               72
068500110531     C                   EndIf
068600110531      *
068700110531      * 3) altro...
068800110531     C                   Other
068900110531     C     k02log04      ReadE     KPLOG04L                               72
069000110531      *
069100110531     C                   EndSl
069200110531      *
069300110531X0202C                   ENDSR
069400110531X0202 *----------------------------------------------------*
069500110531  "   * Posizionamento KPLOG analisi azioni ESEGUITE       *
069600110531  "   *----------------------------------------------------*
069700110531X0202C     SetKPLOG_E    BEGSR
069800110531      *
069900110531X0308C                   IF        D0Azn <> *blank
070000110531  "  C                   Select
070100110531  "   * 1) Azione + profilo
070200110531  "  C                   When      D0PRUT <> *Blank
070300110531  "  C     k04log09      SetLL     KPLOG09L
070400110531  "   * 2) Azione + coda esecuzione
070500110531  "  C                   When      D0CINP <> *Blank
070600110531  "  C     k04log10      SetLL     KPLOG10L
070700110531  "   * 3) solo azione
070800110531  "  C                   Other
070900110531  "  C     k03log08      SetLL     KPLOG08L
071000110531  "  C                   EndSl
071100110531X0308C                   ELSE
071200110531     C                   Select
071300110531      * 1) profilo
071400110531     C                   When      D0PRUT <> *Blank
071500110531     C     k03log05      SetLL     KPLOG05L
071600110531      * 2) coda esecuzione
071700110531     C                   When      D0CINP <> *Blank
071800110531     C     k03log06      SetLL     KPLOG06L
071900110531      * 3) altro...
072000110531     C                   Other
072100110531     C     k02log04      SetLL     KPLOG04L
072200110531     C                   EndSl
072300110531X0308C                   Endif
072400110531      *
072500110531X0202C                   ENDSR
072600110531X0202 *----------------------------------------------------*
072700110531  "   * Lettura KPLOG analisi azioni ESEGUITE              *
072800110531  "   *----------------------------------------------------*
072900110531X0202C     ReadKPLOG_E   BEGSR
073000110531      *
073100110531X0308C                   IF        D0Azn <> *blank
073200110531 "   C                   Select
073300110531 "    * 1) azione + profilo
073400110531 "   C                   When      D0PRUT <> *Blank
073500110531 "    * - generico
073600110531 "   C                   If        $WildPRUT = *On
073700110531 "   C                   Read      KPLOG09L                               72
073800110531 "   C                   Eval      *In72 = *In72 or
073900110531 "   C                             LOGNMU > D0PRUT_High
074000110531 "   C                             or LOGCDA > D0AZN
074100110531 "    * - fisso
074200110531 "   C                   Else
074300110531 "   C     K02Log09      ReadE     KPLOG09L                               72
074400110531 "   C                   EndIf
074500110531 "    * 2) azione + coda esecuzione
074600110531 "   C                   When      D0CINP <> *Blank
074700110531 "    * - generica
074800110531 "   C                   If        $WildCINP = *On
074900110531 "   C                   Read      KPLOG10L                               72
075000110531 "   C                   Eval      *In72 = *In72 or
075100110531 "   C                             LOGCBE > D0CINP_High
075200110531 "   C                             or LOGCDA > D0AZN
075300110531 "    * - fissa
075400110531 "   C                   Else
075500110531 "   C     K02Log10      ReadE     KPLOG10L                               72
075600110531 "   C                   EndIf
075700110531 "    * 3) Azione
075800110531 "   C                   Other
075900110531 "   C     KCDA          ReadE     KPLOG08L                               72
076000110531 "   C                   EndSl
076100110531X0308C                   ELSE
076200110531      *
076300110531     C                   Select
076400110531      * 1) profilo
076500110531     C                   When      D0PRUT <> *Blank
076600110531      * - generico
076700110531     C                   If        $WildPRUT = *On
076800110531     C                   Read      KPLOG05L                               72
076900110531     C                   Eval      *In72 = *In72 or
077000110531     C                             LOGNMU > D0PRUT_High
077100110531      * - fisso
077200110531     C                   Else
077300110531     C     KUS           ReadE     KPLOG05L                               72
077400110531     C                   EndIf
077500110531      * 2) coda esecuzione
077600110531     C                   When      D0CINP <> *Blank
077700110531      * - generica
077800110531     C                   If        $WildCINP = *On
077900110531     C                   Read      KPLOG06L                               72
078000110531     C                   Eval      *In72 = *In72 or
078100110531     C                             LOGCBE > D0CINP_High
078200110531      * - fissa
078300110531     C                   Else
078400110531     C     KCE           ReadE     KPLOG06L                               72
078500110531     C                   EndIf
078600110531      * 3) altro...
078700110531     C                   Other
078800110531     C                   Read      KPLOG04L                               72
078900110531     C                   EndSl
079000110531X0308C                   Endif
079100110531      *
079200110531X0202C                   ENDSR
079300110531X0202 *----------------------------------------------------*
079400110531  "   * Ottimizzazione accessi a KPLOG                     *
079500110531  "   *----------------------------------------------------*
079600110531X0202C     OttimAccKPLOG BEGSR
079700110531      *
079800110531     C                   Select
079900110531      *
080000110531      * in coda
080100110531     C                   When      D0TiAn = 'C'
080200110531      *
080300110531X0308C                   Select
080400110531 "    * Profilo generico
080500110531 "   C                   When      D0PRUT <> *Blank
080600110531 "   C                   If        $WildPRUT = *On
080700110531 "    * Se ho superato il limite vado al profilo dopo
080800110531 "   C                   If        LOGDAI <> 0 or LOGDAF <> 0
080900110531 "   C                             or (LOGDAC>WDacA and WDacA<>0)
081000110531 "   C     LOGNMU        SetGT     KPLOG05L
081100110531 "   C                   Else
081200110531 "    * Se invece sono sotto mi riposiziono
081300110531X0308C                   eval      KUS = LogNMU
081400110531X0313x***                eval      KPC = *blank
081500110531X0313C                   eval      KPC = LogPty
081600110531X0308C     k08log05      SetLL     KPLOG05L
081700110531 "   C                   Endif
081800110531 "    *
081900110531 "   C                   Else
082000110531X0308C                   eval      KPC = LogPty
082100110531X0313x***  k06log05      SetGT     KPLOG05L
082200110531X0313C     k08log05      SetLL     KPLOG05L
082300110531X0308C                   Endif
082400110531 "    *
082500110531 "    * Coda di input generica
082600110531 "   C                   When      D0CINP <> *Blank
082700110531 "   C                   If        $WildCINP = *On
082800110531 "    * Se ho superato il limite vado alla coda  dopo
082900110531 "   C                   If        LOGDAI <> 0 or LOGDAF <> 0
083000110531 "   C                             or (LOGDAC>WDacA and WDacA<>0)
083100110531 "   C     LOGCBI        SetGT     KPLOG07L
083200110531 "   C                   Else
083300110531 "    * Se invece sono sotto mi riposiziono
083400110531X0308C                   eval      Kci = LogCbi
083500110531X0313C                   eval      KPC = LogPty
083600110531X0313x***                eval      KPC = *blank
083700110531X0308C     k08log07      SetLL     KPLOG07L
083800110531 "   C                   Endif
083900110531 "    *
084000110531X0308C                   Else
084100110531X0313C                   eval      KPC = LogPty
084200110531 "   C     k08log07      SetLL     KPLOG07L
084300110531X0313x***  k06log07      SetGT     KPLOG07L
084400110531X0308C                   Endif
084500110531 "    * altro...
084600110531X0308C                   Other
084700110531X0313x**** K05log04      SetGT     KPLOG04L
084800110531 "    * Se ho superato il limite vado a fine file
084900110531 "   C                   If        LOGDAI <> 0 or LOGDAF <> 0
085000110531 "   C                             or (LOGDAC>WDacA and WDacA<>0)
085100110531 "   C     *hival        SetGT     KPLOG04L
085200110531 "   C                   Else
085300110531 "    * Se invece sono sotto mi riposiziono
085400110531 "   C                   eval      KPC = LogPty
085500110531 "   C     K06log04      SetLL     KPLOG04L
085600110531X0313C                   Endif
085700110531X0308C                   EndSL
085800110531 "    *
085900110531 "   x***                Select
086000110531 "   x*** profilo generico
086100110531 "   x***                When      $WildPRUT = *On and
086200110531 "   x***                          (LOGDAI <> 0 or LOGDAF <> 0)
086300110531 "   x***  LOGNMU        SetGT     KPLOG05L
086400110531 "   x*** coda input generica
086500110531 "   x***                When      $WildCINP = *On and
086600110531 "   x***                          (LOGDAI <> 0 or LOGDAF <> 0)
086700110531 "   x***  LOGCBI        SetGT     KPLOG07L
086800110531X0308x***                EndSl
086900110531      *
087000110531      * attive
087100110531     C                   When      D0TiAn = 'A'
087200110531      *
087300110531X0308c                   Select
087400110531 "    * Profilo generico
087500110531 "   C                   When      D0PRUT <> *Blank
087600110531 "   C                   If        $WildPRUT = *On
087700110531 "    * Se ho superato il limite vado al profilo dopo
087800110531 "   C                   If        LOGDAF <> 0
087900110531 "   C                             or (LOGDAI>WDaiA and WDaiA<>0)
088000110531 "   C     LOGNMU        SetGT     KPLOG05L
088100110531 "   C                   Else
088200110531 "    * Se invece sono sotto mi riposiziono
088300110531 "   C                   eval      KUS = LogNMU
088400110531 "   C     k05log05      SetLL     KPLOG05L
088500110531 "   C                   Endif
088600110531X0308C                   Else
088700110531X0313C     k05log05      SetGT     KPLOG05L
088800110531X0313x***  k06log05      SetGT     KPLOG05L
088900110531X0308C                   Endif
089000110531 "    *
089100110531 "    * Coda di esecuzione
089200110531 "   C                   When      D0CINP <> *Blank
089300110531 "   C                   If        $WildCINP = *On
089400110531 "    * Se ho superato il limite vado alla coda  dopo
089500110531 "   C                   If        LOGDAF <> 0
089600110531 "   C                             or (LOGDAI>WDaiA and WDaiA<>0)
089700110531 "   C     LOGCBE        SetGT     KPLOG06L
089800110531 "   C                   Else
089900110531 "    * Se invece sono sotto mi riposiziono
090000110531 "   C                   eval      Kce = LogCBE
090100110531 "   C     k05log06      SetLL     KPLOG06L
090200110531 "   C                   Endif
090300110531 "    *
090400110531X0308C                   Else
090500110531X0313C     k05log06      SetGT     KPLOG06L
090600110531X0313x***  k06log06      SetGT     KPLOG06L
090700110531X0308C                   Endif
090800110531X0313x**altro...                                   new
090900110531X0313x****               Other                     new
091000110531X0313x**** k05log04      SetGT     KPLOG04L        new
091100110531X0308C                   EndSL
091200110531 "    *
091300110531 "   x***                Select
091400110531 "   x*** profilo generico
091500110531 "   x***                When      $WildPRUT = *On and
091600110531 "   x***                          LOGDAF <> 0
091700110531 "   x***  LOGNMU        SetGT     KPLOG05L
091800110531 "   x*** coda input generica
091900110531 "   x***                When      $WildCINP = *On and
092000110531 "   x***                          LOGDAF <> 0
092100110531 "   x***  LOGCBE        SetGT     KPLOG06L
092200110531X0308x***                EndSl
092300110531      *
092400110531      * eseguite
092500110531     C                   When      D0TiAn = 'E'
092600110531      *
092700110531X0308c                   Select
092800110531 "    *
092900110531 "    * Azione + Profilo
093000110531 "   C                   When      D0Azn        <> *Blank
093100110531 "   C                             and   D0PRUT <> *Blank
093200110531 "    * profilo generico
093300110531 "   C                   If        $WildPRUT = *On
093400110531 "   C                   eval      KUS = LogNMU
093500110531 "    * Se ho superato il limite vado alla coda  dopo
093600110531 "   C                   If        LOGDAF > WDafA  and WDafA<>0
093700110531 "   C     K02Log09      SetGT     KPLOG09L
093800110531 "   C                   Else
093900110531 "    * Se invece sono sotto mi riposiziono
094000110531 "   C     k04log09      SetLL     KPLOG09L
094100110531 "   C                   Endif
094200110531 "    *
094300110531 "   C                   Else
094400110531 "   C     *hival        SetLL     KPLOG09L
094500110531 "   C                   Endif
094600110531 "    *
094700110531 "    * Azione + Coda di esecuzione
094800110531 "   C                   When      D0Azn         <> *Blank
094900110531 "   C                             and    D0CINP <> *Blank
095000110531 "   C                   If        $WildCINP = *On
095100110531 "   C                   eval      Kce = LogCBE
095200110531 "    * Se ho superato il limite vado alla coda  dopo
095300110531 "   C                   If        LOGDAF > WDafA  and WDafA<>0
095400110531 "   C     K02Log10      SetGT     KPLOG10L
095500110531 "   C                   Else
095600110531 "    * Se invece sono sotto mi riposiziono
095700110531 "   C     k04log10      SetLL     KPLOG10L
095800110531 "   C                   Endif
095900110531 "    *
096000110531 "   C                   Else
096100110531 "   C     *hival        SetLL     KPLOG10L
096200110531 "   C                   Endif
096300110531 "    *
096400110531 "    * Azione
096500110531 "   C                   When      D0Azn        <> *Blank
096600110531 "   C     *hival        SetLL     KPLOG08L
096700110531 "    *
096800110531 "    * Profilo generico
096900110531 "   C                   When      D0PRUT <> *Blank
097000110531 "   C                   If        $WildPRUT = *On
097100110531 "    * Se ho superato il limite vado alla coda  dopo
097200110531 "   C                   If        LOGDAF > WDafA  and WDafA<>0
097300110531 "   C     LOGNMU        SetGT     KPLOG05L
097400110531 "   C                   Else
097500110531 "    * Se invece sono sotto mi riposiziono
097600110531 "   C                   eval      KUS = LogNMU
097700110531 "   C     k03log05      SetLL     KPLOG05L
097800110531 "   C                   Endif
097900110531 "    *
098000110531 "   C                   Else
098100110531 "   C     *hival        SetLL     KPLOG05L
098200110531 "   C                   Endif
098300110531 "    *
098400110531 "    * Coda di esecuzione
098500110531 "   C                   When      D0CINP <> *Blank
098600110531 "   C                   If        $WildCINP = *On
098700110531 "    * Se ho superato il limite vado alla coda  dopo
098800110531 "   C                   If        LOGDAF > WDafA  and WDafA<>0
098900110531 "   C     LOGCBE        SetGT     KPLOG06L
099000110531 "   C                   Else
099100110531 "    * Se invece sono sotto mi riposiziono
099200110531 "   C                   eval      Kce = LogCBE
099300110531 "   C     k03log06      SetLL     KPLOG06L
099400110531 "   C                   Endif
099500110531 "    *
099600110531 "   C                   Else
099700110531 "   C     *hival        SetLL     KPLOG06L
099800110531 "   C                   Endif
099900110531 "    * altro...
100000110531 "   C                   Other
100100110531 "   C     *hival        SetLL     KPLOG04L
100200110531 "   C                   EndSL
100300110531 "   x***                Select
100400110531 "   x**- profilo generico
100500110531 "   x**                 When      $WildPRUT = *On
100600110531 "   x**- coda input generica
100700110531 "   x**                 When      $WildCINP = *On
100800110531X0308x**                 EndSl
100900110531      *
101000110531     C                   EndSl
101100110531      *
101200110531X0202C                   ENDSR
101300110531X0282 *----------------------------------------------------*
101400110531  "   * Gestione F13=Ripetizione                           *
101500110531  "   *----------------------------------------------------*
101600110531X0282C     GesF13        BEGSR
101700110531      *
101800110531      * E' simile alla funzione F13 del PDM:
101900110531      * riporta l'opzione della riga del sfl su cui è posizionato il cursore
102000110531      * dalla riga successiva fino alla fine del sfl stesso senza
102100110531      * sovrapporsi alle opzioni preesistenti
102200110531      *
102300110531      * solo se il sfl NON è vuoto
102400110531     C                   If        NMax <> 0 and
102500110531      * e solo se posizionato sul sfl...
102600110531     C                             H1NMRC = 'K850S1'
102700110531      *  salvo numero record su cui sono posizionato
102800110531     C                   Z-Add     C1Rcd         WC1Rcd            3 0
102900110531      *  leggo il rcd
103000110531     C     C1Rcd         Chain     K850S1                             21
103100110531      *  solo se l'opz. è valorizzata...
103200110531     C                   If        S1Opz > 0
103300110531      *   memorizzo l'opzione da ripetere
103400110531     C                   Z-Add     S1Opz         WOpz              2 0
103500110531      *   memorizzo il numero record da cui ripetere
103600110531     C                   Z-Add     C1Rcd         $X                3 0
103700110531      *   aggiorno il rcd
103800110531     C                   ExSr      AtrS1
103900110531     C                   SetOn                                        75
104000110531     C                   Update    K850S1
104100110531     C                   SetOff                                       75
104200110531      *   aggiorno l'opzione dal rcd successivo fino alla fine del sfl
104300110531      *   (solo se non ne esiste già un'altra)
104400110531     C                   Add       1             $X
104500110531     C                   DoW       $X <= NMax
104600110531     C     $X            Chain     K850S1                             21
104700110531     C   21              Leave
104800110531     C                   If        S1Opz = 0
104900110531     C                   Z-Add     WOpz          S1Opz
105000110531     C                   EndIf
105100110531     C                   ExSr      AtrS1
105200110531     C                   SetOn                                        75
105300110531     C                   Update    K850S1
105400110531     C                   SetOff                                       75
105500110531     C                   Add       1             $X
105600110531     C                   EndDo
105700110531      *   carico tutte le altre righe possibili ripetendo l'opzione
105800110531     C                   Eval      WPagina = 9999
105900110531     C                   EXSR      ROLLS1
106000110531     C                   Eval      WPagina = PAGINA
106100110531      *   ripristino il posizionamento iniziale della pagina
106200110531     C                   Z-Add     WC1Rcd        C1Nrr
106300110531     C                   EndIf
106400110531     C                   EndIf
106500110531      *
106600110531X0282C                   ENDSR
106700110531      *----------------------------------------------------*
106800110531      * INZS1 - INIZIALIZZAZIONE SFL AZIONI                *
106900110531      *----------------------------------------------------*
107000110531      *
107100110531     C     INZS1         BEGSR
107200110531     C                   SETOFF                                       KE
107300110531     C                   Z-ADD     0             S1NRR             4 0
107400110531     C                   Z-ADD     0             NMAX              4 0
107500110531     C                   Z-ADD     0             C1NRR             4 0
107600110531     C                   SETON                                          70
107700110531     C                   SETOFF                                         71
107800110531     C                   WRITE     K850C1
107900110531     C                   SETOFF                                         70
108000110531X0182X*** ex-X0127 se pgm richiamato in modalità "analisi azioni utente" e
108100110531X0202X*** ex-X0127 se l'utente ha possibilità limitate --> vis. solo opz.5
108200110531X0182X***** ex-X0127     If        $PgmAziUte = *On and
108300110531X0182X***** ex-X0127               PRFAAU = '1'
108400110531X0202 * se utente ha possibilità limitate --> visualizzo solo alcune opzioni
108500110531X0182C                   If        PRFAAU = '1'
108600110531X0127C                   Eval      *In08 = *On
108700110531  "  C                   Else
108800110531  "  C                   Eval      *In08 = *Off
108900110531X0127C                   EndIf
109000110531X0237 * User-Exit "controlla possibilità limitate utente"
109100110531  "  C                   If        *In08 = *Off
109200110531  "  C                   Call      'BAK850RU3'                          22
109300110531  "  C                   Parm                    KPJBA
109400110531  "  C                   Parm      *Off          RtnCdeU3
109500110531  "  C                   If        *In22 = *Off and RtnCdeU3 <> *Off
109600110531  "  C                   Eval      *In08 = *On
109700110531  "  C                   EndIf
109800110531X0237C                   EndIf
109900110531     C                   ENDSR
110000110531      *
110100110531      *----------------------------------------------------*
110200110531      * ROLLS1 - CARICAMENTO SFL AZIONI (PAGINA)           *
110300110531      *----------------------------------------------------*
110400110531      *
110500110531     C     ROLLS1        BEGSR
110600110531     C                   SETOFF                                       75
110700110531X0197X*****              MOVE      *BLANK        S1OPZ
110800110531X0197C                   CLEAR                   S1OPZ
110900110531     C                   Z-ADD     0             NP                2 0
111000110531     C                   Z-ADD     NMAX          S1NRR
111100110531     C     *IN72         DOWEQ     '0'
111200110531X0282X***  NP            ANDLT     PAGINA
111300110531X0282C     NP            ANDLT     WPagina
111400110531X0191 * Controlla validità rcd letto
111500110531X0191C                   ExSr      ChkRcdLOG
111600110531      * Controlla altre Parzializzazioni
111700110531X0191X*****              EXSR      PARZIA
111800110531X0191C  N50              EXSR      PARZIA
111900110531     C     *IN50         IFEQ      '0'
112000110531      * Valorizza campi Subfile
112100110531     C                   EXSR      VALS1
112200110531X0197 * attributi sfl
112300110531X0197C                   ExSr      AtrS1
112400110531X0282C                   If        $F13 = *On
112500110531  "  C                   Eval      S1Opz = WOpz
112600110531  "  C                   SetOn                                        75
112700110531X0282C                   EndIf
112800110531     C                   ADD       1             S1NRR
112900110531     C                   ADD       1             NP
113000110531     C                   WRITE     K850S1
113100110531X0282C                   SetOff                                       75
113200110531     C                   ENDIF
113300110531      * Lettura per Tipo Analisi Azioni
113400110531     C                   SELECT
113500110531      * in coda
113600110531     C     D0TIAN        WHENEQ    'C'
113700110531X0202X*****KLOG2         READE     KPLOG04L                               72
113800110531  "   * - lettura
113900110531X0202C                   ExSr      ReadKPLOG_C
114000110531      * attive
114100110531     C     D0TIAN        WHENEQ    'A'
114200110531X0202X*****KDF           READE     KPLOG04L                               72
114300110531  "   * - lettura
114400110531X0202C                   ExSr      ReadKPLOG_A
114500110531      * eseguite
114600110531     C     D0TIAN        WHENEQ    'E'
114700110531X0202X*****              READ      KPLOG04L                               72
114800110531  "   * - lettura
114900110531X0202C                   ExSr      ReadKPLOG_E
115000110531     C                   ENDSL
115100110531     C                   ENDDO
115200110531     C                   Z-ADD     S1NRR         NMAX                 71
115300110531      ** SALVA GLI ULTIMI CAMPI LETTI
115400110531     C  N72              DO
115500110531     C                   MOVE      LOGATT        SAVATT
115600110531     C                   MOVE      LOGNRA        SAVNRA
115700110531     C                   MOVE      LOGNME        SAVNME
115800110531     C                   MOVE      LOGCBI        SAVCBI
115900110531     C                   MOVE      LOGCBE        SAVCBE
116000110531     C                   MOVE      LOGDAC        SAVDAC
116100110531     C                   MOVE      LOGORC        SAVORC
116200110531     C                   MOVE      LOGDAI        SAVDAI
116300110531     C                   MOVE      LOGORI        SAVORI
116400110531     C                   MOVE      LOGDAF        SAVDAF
116500110531     C                   MOVE      LOGORF        SAVORF
116600110531     C                   MOVE      LOGDEA        SAVDEA
116700110531     C                   MOVEL     LOGKPJ        SAVKP1          256
116800110531     C                   MOVE      LOGKPJ        SAVKP2          256
116900110531     C                   MOVE      LOGSTP        SAVSTP
117000110531     C                   END
117100110531     C                   ENDSR
117200110531      *
117300110531      *----------------------------------------------------*
117400110531      * EMTS1 - EMISSIONE SUBFILE AZIONI                   *
117500110531      *----------------------------------------------------*
117600110531      *
117700110531     C     EMTS1         BEGSR
117800110531X0282C                   Eval      $F13 = *Off
117900110531      * Emissione video Titolo per Tipo Analisi Azione
118000110531     C                   SELECT
118100110531     C     D0TIAN        WHENEQ    'C'
118200110531     C                   WRITE     K850D1C
118300110531     C                   MOVEL     DAORC         C1DAOR
118400110531     C                   MOVEL     COPRC         C1COPR
118500110531     C     D0TIAN        WHENEQ    'A'
118600110531     C                   WRITE     K850D1A
118700110531     C                   MOVEL     DAORA         C1DAOR
118800110531     C                   MOVEL     COPRA         C1COPR
118900110531     C     D0TIAN        WHENEQ    'E'
119000110531     C                   WRITE     K850D1E
119100110531     C                   MOVEL     DAORE         C1DAOR
119200110531     C                   MOVEL     COPRE         C1COPR
119300110531     C                   ENDSL
119400110531      * Emissione video Tasti Funzionali
119500110531     C                   WRITE     K850D2
119600110531      * Emissione Subfile
119700110531     C                   EXFMT     K850C1
119800110531     C                   MOVE      C1MODE        *IN09
119900110531     C                   Z-ADD     SFLNRR        C1NRR
120000110531     C                   ENDSR
120100110531X0191 *----------------------------------------------------*
120200110531  "   * Controlla validità rcd LOG                         *
120300110531  "   *----------------------------------------------------*
120400110531X0191C     ChkRcdLOG     BEGSR
120500110531      *
120600110531     C                   Eval      *In50 = *Off
120700110531      *
120800110531      * User-Exit "controlla rcd LOG"
120900110531     C                   Call      'BAK850RU1'                          22
121000110531     C                   Parm                    KPLOGDSU1
121100110531     C                   Parm      *Off          RtnCdeU1
121200110531      *
121300110531     C                   If        *In22 = *Off and RtnCdeU1 <> *Off
121400110531     C                   Eval      *In50 = *On
121500110531     C                   EndIf
121600110531      *
121700110531X0191C                   ENDSR
121800110531      *
121900110531      *----------------------------------------------------*
122000110531      * PARZIA - CONTROLLA ALTRE PARZIALIZZAZIONI          *
122100110531      *----------------------------------------------------*
122200110531      *
122300110531     C     PARZIA        BEGSR
122400110531      *
122500110531     C                   SETOFF                                       50
122600110531     C                   MOVEL     LOGKPJ        JBADS
122700110531X0202 *
122800110531  "   * Data
122900110531  "  C                   Select
123000110531  "   * - in coda
123100110531  "  C                   When      D0TiAn = 'C'
123200110531  "   * scarto quelle con data inizio <> 0 perchè sono già attive
123300110531  "   *  e quelle con data fine <> 0 perchè sono già eseguite
123400110531  "  C                   If        LOGDAI <> 0 or LOGDAF <> 0
123500110531  "   * scarto quelle con data ins. in coda < della data in parzializzazione
123600110531X0202C                             or LOGDAC < KDC
123700110531X0308 * scarto quelle con data ins.in coda > della data limite parzializz.
123800110531X0308C                             or (LOGDAC>WDacA and WDacA<>0)
123900110531X0202C                   Eval      *In50 = *On
124000110531  "  C                   EndIf
124100110531  "   * - attive
124200110531  "  C                   When      D0TiAn = 'A'
124300110531  "   * scarto quelle con data inizio = 0 perchè sono ancora in coda
124400110531  "   *  e quelle con data fine <> 0 perchè sono già eseguite
124500110531  "  C                   If        LOGDAI = 0 or LOGDAF <> 0
124600110531  "   * scarto quelle con data inizio < della data in parzializzazione
124700110531X0202C                             or LOGDAI < KDI
124800110531X0308 * scarto quelle con data inizio > della data limite parzializz.
124900110531X0308C                             or (LOGDAI>WDaiA and WDaiA<>0)
125000110531X0202C                   Eval      *In50 = *On
125100110531  "  C                   EndIf
125200110531  "   * - eseguite
125300110531  "  C                   When      D0TiAn = 'E'
125400110531  "   * scarto quelle con data fine = 0 perchè sono ancora in coda o attive
125500110531  "  C                   If        LOGDAF = 0
125600110531  "   * scarto quelle con data fine < della data in parzializzazione
125700110531X0202C                             or LOGDAF < KDF
125800110531X0308 * scarto quelle con data fine > della data limite parzializz.
125900110531X0308C                             or (LOGDAF>WDafA and WDafA<>0)
126000110531X0202C                   Eval      *In50 = *On
126100110531  "  C                   EndIf
126200110531  "  C                   EndSl
126300110531  "   * ottimizzazione accessi!!!
126400110531  "  C   50              ExSr      OttimAccKPLOG
126500110531  "  C   50              GoTo      End_PARZIA
126600110531  "   *
126700110531  "   * Ora
126800110531  "  C                   Select
126900110531  "   * - in coda
127000110531  "  C                   When      D0TiAn = 'C'
127100110531X0202 * scarto quelle con ora ins. in coda < dell'ora in parzializzazione
127200110531X0227 * (solo per la data iniziale impostata in parzializzazione)
127300110531X0202C                   If        LOGORC < KOC
127400110531X0227C                             and LOGDAC = KDC
127500110531X0202C                   Eval      *In50 = *On
127600110531X0202C                   EndIf
127700110531X0308 * scarto quelle con ora ins.in coda > ora fine in parzializz.
127800110531 "    * (solo per la data finale impostata in parzializzazione)
127900110531 "   C                   If        LOGORc > WOraA
128000110531 "   C                             and WOraA <> 0
128100110531 "   C                             and LOGDac = WDacA
128200110531 "   C                   Eval      *In50 = *On
128300110531X0308C                   EndIf
128400110531X0202 * - attive
128500110531  "  C                   When      D0TiAn = 'A'
128600110531X0202 * scarto quelle con ora inizio < dell'ora in parzializzazione
128700110531X0227 * (solo per la data iniziale impostata in parzializzazione)
128800110531X0202C                   If        LOGORI < KOI
128900110531X0227C                             and LOGDAI = KDI
129000110531X0202C                   Eval      *In50 = *On
129100110531X0202C                   EndIf
129200110531X0308 * scarto quelle con ora inizio dell'ora fine in parzializz.
129300110531 "    * (solo per la data finale impostata in parzializzazione)
129400110531 "   C                   If        LOGORi > WOraA
129500110531 "   C                             and WOraA <> 0
129600110531 "   C                             and LOGDAi = WDaiA
129700110531 "   C                   Eval      *In50 = *On
129800110531X0308C                   EndIf
129900110531X0202 * - eseguite
130000110531  "  C                   When      D0TiAn = 'E'
130100110531X0202 * scarto quelle con ora fine < dell'ora in parzializzazione
130200110531X0227 * (solo per la data iniziale impostata in parzializzazione)
130300110531X0202C                   If        LOGORF < KOF
130400110531X0227C                             and LOGDAF = KDF
130500110531X0202C                   Eval      *In50 = *On
130600110531X0202C                   EndIf
130700110531X0308 * scarto quelle con ora fine > dell'ora fine in parzializz.
130800110531 "    * (solo per la data finale impostata in parzializzazione)
130900110531 "   c                   If        LOGORF > WOraA
131000110531 "   C                             and WOraA <> 0
131100110531 "   C                             and LOGDAF = WDafA
131200110531 "   C                   Eval      *In50 = *On
131300110531X0308C                   EndIf
131400110531X0202C                   EndSl
131500110531X0202C   50              GoTo      End_PARZIA
131600110531      *
131700110531      * Solo anomale
131800110531X0202X*****D0ERR         IFEQ      'S'
131900110531X0202C  N50D0ERR         IFEQ      'S'
132000110531     C     LOGATT        ANDNE     'E'
132100110531     C                   SETON                                        50
132200110531     C                   ENDIF
132300110531      *
132400110531      * Codice azione
132500110531X0202X*****D0AZN         IFNE      *BLANK
132600110531X0202C  N50D0AZN         IFNE      *BLANK
132700110531     C                   MOVEA     D0AZN         WA1
132800110531     C                   MOVEA     CDA           WA2
132900110531     C                   Z-ADD     1             WX                1 0
133000110531     C     ' '           LOOKUP    WA1(WX)                                58
133100110531     C   58              MOVEA     *BLANK        WA2(WX)
133200110531     C                   MOVEA     WA1           WA1C              4
133300110531     C                   MOVEA     WA2           WA2C              4
133400110531     C     WA1C          IFNE      WA2C
133500110531     C                   SETON                                        50
133600110531     C                   ENDIF
133700110531     C                   ENDIF
133800110531      *
133900110531      * Tipo Azione
134000110531X0202X*****D0TIAZ        IFNE      *BLANK
134100110531X0202C  N50D0TIAZ        IFNE      *BLANK
134200110531     C     D0TIAZ        IFEQ      'I'
134300110531     C     TIA           ANDNE     *BLANK
134400110531     C                   SETON                                        50
134500110531     C                   ELSE
134600110531     C     D0TIAZ        IFEQ      'B'
134700110531     C     TIA           ANDNE     'B'
134800110531     C                   SETON                                        50
134900110531     C                   ENDIF
135000110531     C                   ENDIF
135100110531     C                   ENDIF
135200110531      *
135300110531      * Profilo Utente
135400110531     C  N50D0PRUT        IFNE      *BLANK
135500110531X0202X*****D0PRUT        ANDNE     NPU
135600110531  "  C                   If        NPU < D0PRUT_Low or
135700110531X0202C                             NPU > D0PRUT_High
135800110531     C                   SETON                                        50
135900110531X0202C                   EndIf
136000110531     C                   ENDIF
136100110531      *
136200110531      * Sistema Informativo
136300110531     C  N50D0SINF        IFNE      *BLANK
136400110531     C     D0SINF        ANDNE     NSI
136500110531     C                   SETON                                        50
136600110531     C                   ENDIF
136700110531      *
136800110531      * Terminale
136900110531     C  N50D0TERM        IFNE      *BLANK
137000110531X0202X*****D0TERM        ANDNE     NTM
137100110531  "  C                   If        NTM < D0TERM_Low or
137200110531X0202C                             NTM > D0TERM_High
137300110531     C                   SETON                                        50
137400110531X0202C                   EndIf
137500110531     C                   ENDIF
137600110531      *
137700110531      * Coda Input/Esecuzione
137800110531     C  N50D0CINP        IFNE      *BLANK
137900110531      * SE CODA INPUT BLANK VALORIZZO IL CAMPO
138000110531     C                   MOVEL     LOGCBI        S1CODA
138100110531     C     S1CODA        IFEQ      *BLANK
138200110531X0195X*****              CLEAR                   DS036
138300110531  "  X*****              MOVEL     LOGNME        JNAM36
138400110531  "  X*****              MOVEL     '*ALL'        UNAM36
138500110531  "  X*****              MOVEL     '*ALL'        JNBR36
138600110531  "  C                   CLEAR                   BAK036DS
138700110531  "  C                   MOVEL     LOGNME        JOBNAM036
138800110531  "  C                   MOVEL     '*ALL'        USRNAM036
138900110531X0195C                   MOVEL     '*ALL'        JOBNBR036
139000110531     C                   CALL      'BAK036R'                            99
139100110531X0195X*****              PARM                    DS036
139200110531  "  C                   PARM                    BAK036DS
139300110531  "  X*****              MOVEL     JOBQ36        S1CODA
139400110531X0195C                   MOVEL     JOBQ036       S1CODA
139500110531     C                   ENDIF
139600110531      * controllo in base al tipo analisi azione
139700110531     C                   SELECT
139800110531      * in coda --> coda input
139900110531     C     D0TIAN        WHENEQ    'C'
140000110531X0202X*****D0CINP        IFNE      S1CODA
140100110531  "  C                   IF        S1CODA < D0CINP_Low or
140200110531X0202C                             S1CODA > D0CINP_High
140300110531     C                   SETON                                        50
140400110531     C                   ENDIF
140500110531      * attive/eseguite --> coda esecuzione
140600110531     C     D0TIAN        WHENEQ    'A'
140700110531     C     D0TIAN        OREQ      'E'
140800110531X0202X*****D0CINP        IFNE      LOGCBE
140900110531  "  C                   IF        LOGCBE < D0CINP_Low or
141000110531X0202C                             LOGCBE > D0CINP_High
141100110531     C                   SETON                                        50
141200110531     C                   ENDIF
141300110531     C                   ENDSL
141400110531     C                   ENDIF
141500110531X0202X*** Ora
141600110531  "  X**N50D0ORA         IFNE      *ZERO
141700110531  "  X**   D0MIN         ORNE      *ZERO
141800110531  "  X**   D0SEC         ORNE      *ZERO
141900110531  "  X**                 Z-ADD     D0ORA         GG
142000110531  "  X**                 Z-ADD     D0MIN         MM
142100110531  "  X**                 Z-ADD     D0SEC         AA
142200110531  "  X**                 Z-ADD     D0GG          GGV
142300110531  "  X**                 Z-ADD     D0MM          MMV
142400110531  "  X**                 Z-ADD     D0AA          AAV
142500110531  "  X**   AAV           IFLT      40
142600110531  "  X**                 Z-ADD     1             SSV
142700110531  "  X**                 ELSE
142800110531  "  X**                 Z-ADD     0             SSV
142900110531  "  X**                 ENDIF
143000110531  "  X**                 SELECT
143100110531  "  X**   D0TIAN        WHENEQ    'C'
143200110531  "  X**   LOGORC        IFLT      DATA
143300110531  "  X**   LOGDAF        ANDLE     DATAV
143400110531  "  X**                 SETON                                        50
143500110531  "  X**                 ENDIF
143600110531  "  X**   D0TIAN        WHENEQ    'A'
143700110531  "  X**   LOGORI        IFLT      DATA
143800110531  "  X**   LOGDAF        ANDLE     DATAV
143900110531  "  X**                 SETON                                        50
144000110531  "  X**                 ENDIF
144100110531  "  X**   D0TIAN        WHENEQ    'E'
144200110531  "  X**   LOGORF        IFLT      DATA
144300110531  "  X**   LOGDAF        ANDLE     DATAV
144400110531  "  X**                 SETON                                        50
144500110531  "  X**                 ENDIF
144600110531  "  X**                 ENDSL
144700110531X0202X**                 ENDIF
144800110531      *
144900110531X0202X*****              ENDSR
145000110531X0202C     End_PARZIA    ENDSR
145100110531      *
145200110531      *----------------------------------------------------*
145300110531      * VALS1 - VALORIZZA CAMPI SUBFILE AZIONI             *
145400110531      *----------------------------------------------------*
145500110531      *
145600110531     C     VALS1         BEGSR
145700110531     C     STJ           IFEQ      'H'
145800110531     C                   MOVE      'H'           S1HLD
145900110531     C                   ELSE
146000110531     C                   CLEAR                   S1HLD
146100110531     C                   END
146200110531     C     LOGATT        IFEQ      'E'
146300110531     C                   MOVE      'E'           S1ERR
146400110531     C                   ELSE
146500110531     C                   CLEAR                   S1ERR
146600110531     C                   ENDIF
146700110531     C                   MOVEL     LOGDEA        S1DESA
146800110531      * SE AZIONE ANNULLATA LO SEGNALA
146900110531     C     LOGATT        IFEQ      'A'
147000110531     C                   MOVEL     'A'           S1ERR
147100110531     C                   END
147200110531     C                   MOVEL     LOGDEA        S1DES
147300110531X0163C                   MOVEL     CDA           S1COAZ
147400110531     C                   MOVEL     NPU           S1PRUT
147500110531     C                   MOVEL     NSI           S1SINF
147600110531     C                   Z-ADD     NRL           S1NUML
147700110531     C                   Z-ADD     LOGNRA        S1NRAZ
147800110531     C                   MOVEL     LOGNME        S1NOML
147900110531     C                   MOVEL     PTY           S1PTY
148000110531     C                   SELECT
148100110531     C     D0TIAN        WHENEQ    'C'
148200110531     C                   MOVEL     JBUF          SCHEDS
148300110531     C     SCHEDS        IFNE      *BLANK
148400110531     C     SCHEDS        ANDNE     *ALL'0'
148500110531     C                   MOVEL     'S'           S1ERR
148600110531     C                   ENDIF
148700110531     C                   Z-ADD     LOGDAC        DATA
148800110531     C                   EXSR      INVDAT
148900110531     C                   Z-ADD     DATA          S1DATA
149000110531     C                   Z-ADD     LOGORC        S1ORA
149100110531     C                   MOVEL     LOGCBI        S1CODA
149200110531     C     S1CODA        IFEQ      *BLANK
149300110531     C     S1PTY         OREQ      *BLANK
149400110531X0195X*****              CLEAR                   DS036
149500110531  "  X*****              MOVEL     LOGNME        JNAM36
149600110531  "  X*****              MOVEL     '*ALL'        UNAM36
149700110531  "  X*****              MOVEL     '*ALL'        JNBR36
149800110531  "  C                   CLEAR                   BAK036DS
149900110531  "  C                   MOVEL     LOGNME        JOBNAM036
150000110531  "  C                   MOVEL     '*ALL'        USRNAM036
150100110531X0195C                   MOVEL     '*ALL'        JOBNBR036
150200110531     C                   CALL      'BAK036R'                            99
150300110531X0195X*****              PARM                    DS036
150400110531  "  C                   PARM                    BAK036DS
150500110531  "  X*****              MOVEL     JOBQ36        S1CODA
150600110531  "  X*****              MOVEL     JOBP36        S1PTY
150700110531  "  C                   MOVEL     JOBQ036       S1CODA
150800110531X0195C                   MOVEL     JOBPTY036     S1PTY
150900110531     C                   ENDIF
151000110531     C     D0TIAN        WHENEQ    'A'
151100110531     C                   Z-ADD     LOGDAI        DATA
151200110531     C                   EXSR      INVDAT
151300110531     C                   Z-ADD     DATA          S1DATA
151400110531     C                   Z-ADD     LOGORI        S1ORA
151500110531     C                   MOVEL     LOGCBE        S1CODA
151600110531     C     D0TIAN        WHENEQ    'E'
151700110531     C                   Z-ADD     LOGDAF        DATA
151800110531     C                   EXSR      INVDAT
151900110531     C                   Z-ADD     DATA          S1DATA
152000110531     C                   Z-ADD     LOGORF        S1ORA
152100110531     C                   MOVEL     LOGCBE        S1CODA
152200110531     C                   ENDSL
152300110531X0127X*****PRO850        IFEQ      'U'
152400110531X0127 * se pgm richiamato in modalità "analisi azioni utente"
152500110531X0182 * oppure pgm richiamato in modalità "analisi azioni"
152600110531X0182 * e l'utente ha possibilità limitate -->
152700110531X0127 *  l'utente non può agire sulle azioni di altri utenti
152800110531X0127C     $PgmAziUte    IFEQ      *On
152900110531     C     NPU           ANDNE     KNMUS
153000110531X0182C     $PgmAziUte    OREQ      *Off
153100110531  "  C     PRFAAU        ANDEQ     '1'
153200110531X0182C     NPU           ANDNE     KNMUS
153300110531X0197X*****              SETON                                        76
153400110531     C                   MOVE      *ON           S1PROT
153500110531     C                   ELSE
153600110531X0197X*****              SETOFF                                       76
153700110531     C                   MOVE      *OFF          S1PROT
153800110531     C                   ENDIF
153900110531     C                   ENDSR
154000110531X0197 *----------------------------------------------------*
154100110531  "   * ATRS1 - Attributi sfl                              *
154200110531  "   *----------------------------------------------------*
154300110531X0197C     ATRS1         BEGSR
154400110531      *
154500110531      * protezione opzione
154600110531     C                   Eval      *In76 = S1Prot
154700110531      *
154800110531      * esistenza msg QSYSOPR
154900110531     C                   Call      'BAK223R'
155000110531     C                   Parm      S1NrAz        NrAz223           9 0
155100110531     C                   Parm      *Off          Esito223          1
155200110531      * (blink descrizione breve azione)
155300110531     C                   Eval      *In55 = Esito223
155400110531      *
155500110531X0197C                   ENDSR
155600110531      *
155700110531      *----------------------------------------------------*
155800110531      * CTRS1 - CONTROLLO OPZIONI                          *
155900110531      *----------------------------------------------------*
156000110531      *
156100110531     C     CTRS1         BEGSR
156200110531X0281X***                MOVE      ' '           PRIMO7
156300110531X0281C                   MOVE      ' '           PRIMO22
156400110531     C                   SETOFF                                       50
156500110531     C                   SETOFF                                       10
156600110531     C                   READC     K850S1                                 40
156700110531     C     *IN40         DOWEQ     '0'
156800110531X0282X***                SETOFF                                       9975
156900110531X0282C                   SETOFF                                       9965
157000110531X0182X*** ex-X0127 se pgm richiamato in modalità "analisi azioni utente" e
157100110531X0202X*** ex-X0127 se l'utente ha possibilità limitate --> è ab. solo opz.5
157200110531X0182X***** ex-X0127     If        $PgmAziUte = *On and
157300110531X0182X***** ex-X0127               PRFAAU = '1' and
157400110531X0202 * se utente ha possibilità limitate --> abilitate solo alcune opzioni
157500110531X0182C                   If        PRFAAU = '1' and
157600110531X0197X***** ex-X0127               S1Opz <> *Blank and S1Opz <> '5'
157700110531X0197C                             S1Opz <> 0 and S1Opz <> 5
157800110531X0202C                             and S1Opz <> 8
157900110531X0127C                   SetOn                                        9974
158000110531X0127C                   Else
158100110531     C                   SELECT
158200110531      * 2=Modifica
158300110531X0197X*****S1OPZ         WHENEQ    '2'
158400110531X0197C     S1OPZ         WHENEQ    2
158500110531     C     S1NRAZ        CHAIN(N)  KPLOG01L                           99
158600110531     C  N99LOGATT        IFEQ      'A'
158700110531     C                   SETON                                        99
158800110531     C                   END
158900110531     C  N50
159000110531     CAN 99              SETON                                        82
159100110531     C     *IN99         IFEQ      '0'
159200110531     C                   MOVEL     LOGKPJ        JBADS
159300110531     C     TIA           IFEQ      'B'
159400110531X0202C     LOGDAF        IFEQ      0
159500110531X0202C     LOGDAI        ANDEQ     0
159600110531     C                   MOVEL     S1NOML        D4NOML
159700110531     C                   MOVE      S1CODA        D4NEWC
159800110531     C                   MOVE      S1PTY         D4NEWP
159900110531X0169X*****JBUF          IFNE      *BLANK
160000110531  "  X*****              MOVEL     JBUF          DATORA
160100110531  "  X*****              ELSE
160200110531  "  X*****              CLEAR                   DATS
160300110531  "  X*****              CLEAR                   ORAS
160400110531  "  X*****              ENDIF
160500110531  "  C                   CLEAR                   DATORA
160600110531  "  C                   MOVEL     JBUF          SCHEDS
160700110531  "  C     SCHEDS        IFNE      *BLANK
160800110531  "  C     SCHEDS        ANDNE     *ALL'0'
160900110531  "  C                   MOVEL     SCHEDS        DATORA
161000110531X0169C                   ENDIF
161100110531     C     DATAA         IFNE      *BLANK
161200110531     C                   MOVE      GG4           D4AA
161300110531     C                   MOVE      MM4           D4MM
161400110531     C                   MOVE      AA4           D4GG
161500110531     C                   ENDIF
161600110531     C     ORAA          IFNE      *BLANK
161700110531     C                   MOVE      ORA4          D4ORA
161800110531     C                   MOVE      MIN4          D4MIN
161900110531     C                   ENDIF
162000110531     C     *INKL         DOUEQ     '1'
162100110531     C     *INKC         OREQ      '1'
162200110531     C     *IN80         OREQ      '0'
162300110531     C     *IN81         ANDEQ     '0'
162400110531     C     *IN77         ANDEQ     '0'
162500110531     C     *IN78         ANDEQ     '0'
162600110531     C     *IN79         ANDEQ     '0'
162700110531     C                   SETOFF                                       07
162800110531     C                   EXFMT     K850D4
162900110531     C     *INKL         IFEQ      '0'
163000110531     C                   EXSR      CTRD4
163100110531      * Data schedulazione
163200110531     C     D4GG          IFNE      0
163300110531     C     D4MM          ORNE      0
163400110531     C     D4GG          ORNE      0
163500110531     C                   Z-ADD     D4GG          AA4
163600110531     C                   Z-ADD     D4MM          MM4
163700110531     C                   Z-ADD     D4AA          GG4
163800110531     C                   ENDIF
163900110531      * Ora schedulazione
164000110531     C     D4ORA         IFNE      *ZERO
164100110531     C     D4MIN         ORNE      *ZERO
164200110531X0169X*****D4SEC         ORNE      *ZERO
164300110531     C                   Z-ADD     D4ORA         ORA4
164400110531     C                   Z-ADD     D4MIN         MIN4
164500110531X0169X*****              Z-ADD     D4SEC         SEC4
164600110531X0169C                   Z-ADD     0             SEC4
164700110531     C                   ENDIF
164800110531      *
164900110531     C     *IN80         IFEQ      '0'
165000110531     C     *IN81         ANDEQ     '0'
165100110531     C     *IN77         ANDEQ     '0'
165200110531     C     *IN78         ANDEQ     '0'
165300110531     C     *IN79         ANDEQ     '0'
165400110531     C                   MOVE      CMD(2)        CMD2
165500110531     C                   MOVEL     LOGKPJ        JBADS
165600110531     C                   MOVEL     LOGNME        NOMLAV
165700110531     C                   MOVEL     D4NEWP        NEWPR
165800110531     C                   MOVEL     D4NEWC        NEWJQ
165900110531     C                   CALL      'QCMDEXC'                            99
166000110531     C                   PARM                    CMD2
166100110531     C                   PARM      80            LUNG
166200110531     C  N50
166300110531     CAN 99              SETON                                        82
166400110531X0281C     *IN99         IFEQ      '0'
166500110531     C     DATS          IFNE      *ZERO
166600110531     C     ORAS          ORNE      *ZERO
166700110531     C                   SETON                                        10
166800110531      * Data / Ora schedulazione
166900110531     C                   MOVE      CDS(1)        CMD3
167000110531     C                   MOVEL     LOGKPJ        JBADS
167100110531     C                   MOVEL     LOGNME        NMLAV
167200110531     C                   CLEAR                   NEWDS
167300110531     C                   CLEAR                   NEWOS
167400110531     C     DATS          IFNE      *ZERO
167500110531     C                   MOVEL     DATS          DATA
167600110531     C                   EXSR      INVDAT
167700110531     C                   MOVEL     DATA          NEWDS
167800110531     C                   ENDIF
167900110531     C     ORAS          IFNE      *ZERO
168000110531     C                   MOVEL     ORAS          NEWOS
168100110531     C                   ENDIF
168200110531     C                   CALL      'QCMDEXC'                            99
168300110531     C                   PARM                    CMD3
168400110531     C                   PARM      80            LUNG
168500110531     C  N50
168600110531     CAN 99              SETON                                        82
168700110531     C                   ENDIF
168800110531X0281C                   ENDIF
168900110531     C     *IN99         IFEQ      '0'
169000110531     C     S1NRAZ        CHAIN     KPLOG01L                           99
169100110531     C                   MOVEL     D4NEWC        LOGCBI
169200110531     C                   MOVEL     D4NEWP        PTY
169300110531X0169X*****              MOVEL     DATORA        JBUF
169400110531X0169C                   MOVEL     DATORA        SCHEDS
169500110531X0169C                   MOVEL     SCHEDS        JBUF
169600110531     C     *IN10         IFEQ      '1'
169700110531     C                   MOVEL     'S'           STJ
169800110531     C                   ENDIF
169900110531     C                   MOVEL     JBADS         LOGKPJ
170000110531     C  N99              UPDATE    KPLOG001
170100110531     C                   ENDIF
170200110531     C                   ENDIF
170300110531     C                   ENDIF
170400110531     C                   ENDDO
170500110531X0202C                   ELSE
170600110531  "  C                   SETON                                        99
170700110531  "  C  N50              SETON                                        66
170800110531X0202C                   ENDIF
170900110531     C                   ELSE
171000110531     C                   SETON                                        99
171100110531     C  N50              SETON                                        89
171200110531     C                   ENDIF
171300110531     C                   ENDIF
171400110531     C   KL              SETOFF                                       KL
171500110531X0147X***** ex-X0024     MOVE      '1'           *INKE
171600110531X0147C  N99              MOVE      '1'           *INKE
171700110531      * 3=Congelamento
171800110531X0197X*****S1OPZ         WHENEQ    '3'
171900110531X0197C     S1OPZ         WHENEQ    3
172000110531     C     S1NRAZ        CHAIN     KPLOG01L                           99
172100110531     C  N99LOGATT        IFEQ      'A'
172200110531     C                   SETON                                        99
172300110531     C                   END
172400110531     C  N50
172500110531     CAN 99              SETON                                        83
172600110531     C     *IN99         IFEQ      '0'
172700110531     C                   MOVE      CMD(3)        CMD2
172800110531     C                   MOVEL     LOGKPJ        JBADS
172900110531     C     TIA           IFEQ      'B'
173000110531     C                   MOVEL     LOGNME        NOMLAV
173100110531     C                   ELSE
173200110531     C                   EXSR      VALCMD
173300110531     C                   ENDIF
173400110531     C                   CALL      'QCMDEXC'                            99
173500110531     C                   PARM                    CMD2
173600110531     C                   PARM      80            LUNG
173700110531     C  N50
173800110531     CAN 99              SETON                                        83
173900110531     C     *IN99         IFEQ      '0'
174000110531     C                   MOVEL     LOGKPJ        JBADS
174100110531     C                   MOVEL     'H'           STJ
174200110531     C                   MOVEL     JBADS         LOGKPJ
174300110531     C                   UPDATE    KPLOG001
174400110531     C                   ENDIF
174500110531     C                   ENDIF
174600110531      * 4=Annullamento
174700110531X0197X*****S1OPZ         WHENEQ    '4'
174800110531X0197C     S1OPZ         WHENEQ    4
174900110531     C     D0TIAN        ANDNE     'E'
175000110531     C  NKF              SETON                                        419975
175100110531     C     S1NRAZ        CHAIN     KPLOG01L                           99
175200110531     C  N99LOGATT        IFEQ      'A'
175300110531     C                   SETON                                        99
175400110531     C                   END
175500110531     C  N50
175600110531     CAN 99              SETON                                        84
175700110531     C     *IN99         IFEQ      '0'
175800110531     C                   MOVEL     LOGKPJ        JBADS
175900110531     C     *INKF         IFEQ      '1'
176000110531     C*
176100110531     C     D0TIAN        IFEQ      'C'
176200110531     C* SE ANALISI AZIONI IN CODA ANNULLO SOLO SE NON È ATTIVO
176300110531     C                   EXSR      CHKACT
176400110531     C*
176500110531     C     ACTIVE        IFEQ      *ON
176600110531     C                   UNLOCK    KPLOG01L
176700110531     C                   GOTO      A001
176800110531     C                   ENDIF
176900110531     C                   ENDIF
177000110531     C*
177100110531     C                   MOVE      CMD(4)        CMD2
177200110531     C                   MOVEL     LOGKPJ        JBADS
177300110531     C     TIA           IFEQ      'B'
177400110531     C                   MOVEL     LOGNME        NOMLAV
177500110531     C                   ELSE
177600110531     C                   EXSR      VALCMD
177700110531     C                   ENDIF
177800110531     C                   CALL      'QCMDEXC'                            99
177900110531     C                   PARM                    CMD2
178000110531     C                   PARM      80            LUNG
178100110531     C  N50
178200110531     CAN 99              SETON                                        84
178300110531     C                   MOVEL     'A'           LOGATT
178400110531     C                   TIME                    ORA               6 0
178500110531     C                   Z-ADD     ORA           LOGORF
178600110531     C                   Z-ADD     UDATE         DATA
178700110531     C                   EXSR      INVDAT
178800110531     C                   Z-ADD     DATA          LOGDAF
178900110531      * se l' anno è inferiore a 40 assumo che siamo nel XXI° secolo
179000110531     C     GG            IFLT      40
179100110531     C                   MOVEL     1             LOGDAF
179200110531     C                   ENDIF
179300110531     C     LOGDAI        IFEQ      0
179400110531     C                   Z-ADD     LOGDAF        LOGDAI
179500110531     C                   Z-ADD     ORA           LOGORI
179600110531     C                   ENDIF
179700110531     C                   UPDATE    KPLOG001
179800110531     C                   ENDIF
179900110531     C                   ENDIF
180000110531     C     A001          TAG
180100110531      * 5=Visualizzazione
180200110531X0197X*****S1OPZ         WHENEQ    '5'
180300110531X0197C     S1OPZ         WHENEQ    5
180400110531     C     S1NRAZ        CHAIN(N)  KPLOG01L                           99
180500110531     C  N99LOGATT        IFEQ      'A'
180600110531     C                   SETON                                        99
180700110531     C                   END
180800110531     C  N50
180900110531     CAN 99              SETON                                        85
181000110531     C     *IN99         IFEQ      '0'
181100110531     C                   EXSR      VALD3
181200110531     C                   EXFMT     K850D3
181300110531     C                   ENDIF
181400110531     C   KL              SETOFF                                       KL
181500110531      * 6=Rilascio
181600110531X0197X*****S1OPZ         WHENEQ    '6'
181700110531X0197C     S1OPZ         WHENEQ    6
181800110531     C     S1NRAZ        CHAIN     KPLOG01L                           99
181900110531     C  N99LOGATT        IFEQ      'A'
182000110531     C                   SETON                                        99
182100110531     C                   END
182200110531     C  N50
182300110531     CAN 99              SETON                                        86
182400110531     C     *IN99         IFEQ      '0'
182500110531     C                   MOVE      CMD(6)        CMD2
182600110531     C                   MOVEL     LOGKPJ        JBADS
182700110531     C     TIA           IFEQ      'B'
182800110531     C                   MOVEL     LOGNME        NOMLAV
182900110531     C                   ELSE
183000110531     C                   EXSR      VALCMD
183100110531     C                   ENDIF
183200110531     C                   CALL      'QCMDEXC'                            99
183300110531     C                   PARM                    CMD2
183400110531     C                   PARM      80            LUNG
183500110531     C  N50
183600110531     CAN 99              SETON                                        86
183700110531     C     *IN99         IFEQ      '0'
183800110531     C                   MOVEL     LOGKPJ        JBADS
183900110531     C                   MOVEL     ' '           STJ
184000110531     C                   MOVEL     JBADS         LOGKPJ
184100110531     C                   UPDATE    KPLOG001
184200110531     C                   ENDIF
184300110531     C                   ENDIF
184400110531X0197X****** 7=Modifica MULTIPLA
184500110531  "  X*****S1OPZ         WHENEQ    '7'
184600110531  "   * 22=Modifica multipla
184700110531X0197C     S1OPZ         WHENEQ    22
184800110531X0147C     S1NRAZ        CHAIN(N)  KPLOG01L                           99
184900110531  "  C  N99LOGATT        IFEQ      'A'
185000110531  "  C                   SETON                                        99
185100110531  "  C                   END
185200110531  "  C  N50
185300110531  "  CAN 99              SETON                                        82
185400110531  "  C     *IN99         IFEQ      '0'
185500110531  "  C                   MOVEL     LOGKPJ        JBADS
185600110531X0147C     TIA           IFEQ      'B'
185700110531X0202C     LOGDAF        IFEQ      0
185800110531X0202C     LOGDAI        ANDEQ     0
185900110531     C     S1ERR         IFNE      'A'
186000110531X0281X***  PRIMO7        IFEQ      ' '
186100110531X0281C     PRIMO22       IFEQ      ' '
186200110531     C                   CLEAR                   K850D4
186300110531     C                   CLEAR                   DATS
186400110531     C                   CLEAR                   ORAS
186500110531B2   C     *INKL         DOUEQ     '1'
186600110531     C     *INKC         OREQ      '1'
186700110531     C     *IN80         OREQ      '0'
186800110531     C     *IN81         ANDEQ     '0'
186900110531     C     *IN77         ANDEQ     '0'
187000110531     C     *IN78         ANDEQ     '0'
187100110531     C     *IN79         ANDEQ     '0'
187200110531     C                   SETON                                        07
187300110531     C                   EXFMT     K850D4
187400110531B3   C     *INKL         IFEQ      '0'
187500110531     C                   EXSR      CTRD4
187600110531E3   C                   ENDIF
187700110531E2   C                   ENDDO
187800110531     C*
187900110531     C     *INKL         IFEQ      '0'
188000110531     C     *INKC         ANDEQ     '0'
188100110531X0281X***                MOVE      '1'           PRIMO7            1
188200110531X0281C                   MOVE      '1'           PRIMO22           1
188300110531     C                   ENDIF
188400110531E1   C                   ENDIF
188500110531     C*
188600110531B1   C     *INKL         IFEQ      '0'
188700110531     C     *INKC         ANDEQ     '0'
188800110531     C     S1NRAZ        CHAIN(N)  KPLOG01L                           99
188900110531     C                   MOVE      CMD(2)        CMD2
189000110531X0281X***                CLEAR                   PTYBLK
189100110531     C                   MOVEL     LOGKPJ        JBADS
189200110531     C                   MOVEL     LOGNME        NOMLAV
189300110531     C                   MOVEL     D4NEWC        NEWJQ
189400110531X0281C                   MOVEL     D4NEWP        NEWPR
189500110531     C                   CALL      'QCMDEXC'                            99
189600110531     C                   PARM                    CMD2
189700110531     C                   PARM      80            LUNG
189800110531     C  N50
189900110531     CAN 99              SETON                                        82
190000110531X0281C     *IN99         IFEQ      '0'
190100110531     C     S1NRAZ        CHAIN     KPLOG01L                           99
190200110531     C                   MOVEL     D4NEWC        LOGCBI
190300110531X0281C                   MOVEL     D4NEWP        PTY
190400110531B2   C     *IN10         IFEQ      '1'
190500110531     C                   MOVEL     'S'           STJ
190600110531E2   C                   ENDIF
190700110531X0281C                   MOVEL     JBADS         LOGKPJ
190800110531     C  N99              UPDATE    KPLOG001
190900110531X0281C                   ENDIF
191000110531B1   C                   ENDIF
191100110531     C   KL              SETOFF                                       KL
191200110531     C                   END
191300110531X0202C                   ELSE
191400110531  "  C                   SETON                                        99
191500110531  "  C  N50              SETON                                        66
191600110531X0202C                   ENDIF
191700110531X0147C                   ELSE
191800110531  "  C                   SETON                                        99
191900110531  "  C  N50              SETON                                        89
192000110531  "  C                   ENDIF
192100110531X0147C                   ENDIF
192200110531X0281C  N99              MOVE      '1'           *INKE
192300110531      * 8=DSPJOB
192400110531X0197X*****S1OPZ         WHENEQ    '8'
192500110531X0197C     S1OPZ         WHENEQ    8
192600110531     C     S1NRAZ        CHAIN(N)  KPLOG01L                           99
192700110531     C  N99LOGATT        IFEQ      'A'
192800110531     C                   SETON                                        99
192900110531     C                   END
193000110531     C  N50
193100110531     CAN 99              SETON                                        85
193200110531     C     *IN99         IFEQ      '0'
193300110531     C                   MOVE      CMD(5)        CMD2
193400110531     C                   MOVEL     LOGKPJ        JBADS
193500110531     C     TIA           IFEQ      'B'
193600110531     C                   MOVEL     LOGNME        NOMLAV
193700110531     C                   ELSE
193800110531     C                   EXSR      VALCMD
193900110531     C                   ENDIF
194000110531     C                   CALL      'QCMDEXC'                            99
194100110531     C                   PARM                    CMD2
194200110531     C                   PARM      80            LUNG
194300110531     C  N50
194400110531     CAN 99              SETON                                        85
194500110531     C                   ENDIF
194600110531      * 9=Riesecuzione
194700110531X0197X*****S1OPZ         WHENEQ    '9'
194800110531X0197C     S1OPZ         WHENEQ    9
194900110531     C     S1NRAZ        CHAIN(N)  KPLOG01L                           99
195000110531     C  N99LOGATT        IFEQ      'A'
195100110531     C                   SETON                                        99
195200110531     C                   END
195300110531     C  N50
195400110531     CAN 99              SETON                                        87
195500110531     C     *IN99         IFEQ      '0'
195600110531     C                   MOVEL     LOGKPJ        JBADS
195700110531     C     TIA           IFEQ      'B'
195800110531     C     LOGDAF        IFNE      0
195900110531     C                   MOVEL     KPJBA         JBADS
196000110531      *
196100110531     C                   MOVEL     LOGKPJ        KPJBA
196200110531X0208X*****              CALL      'BAK850R1'
196300110531  "  X*****              PARM                    KPJBU
196400110531  "  X*****              PARM      '0'           EXIT              1
196500110531  "  X*****EXIT          IFEQ      '0'
196600110531  "   * pgm gestione KPJBU
196700110531  "  C                   Call      'BAK400R'
196800110531  "  C                   Parm                    KPJBA
196900110531X0208 *
197000110531     C                   CALL      'BCH09'
197100110531     C                   PARM                    KPJBA
197200110531     C                   CALL      'BCH10'
197300110531     C                   PARM                    KPJBA
197400110531      *
197500110531X0208X*****              ENDIF
197600110531     C                   MOVEL     JBADS         KPJBA
197700110531     C                   ELSE
197800110531     C                   SETON                                        99
197900110531     C  N50              SETON                                        88
198000110531     C                   ENDIF
198100110531     C                   ELSE
198200110531     C                   SETON                                        99
198300110531     C  N50              SETON                                        89
198400110531     C                   ENDIF
198500110531     C                   ENDIF
198600110531X0197 * 7=Messaggi QSYSOPR
198700110531  "  C     S1OPZ         WHENEQ    7
198800110531  "  C                   CALL      'BAK220R'
198900110531  "  C                   PARM      S1NRAZ        NrAz220           9 0
199000110531  "  C                   PARM      *OFF          Esito220          1
199100110531  "  C                   IF        Esito220 = '1'
199200110531  "  C                   SetOn                                        99
199300110531  "  C  N50              SetOn                                        67
199400110531X0197C                   EndIf
199500110531      *
199600110531     C                   ENDSL
199700110531X0127C                   EndIf
199800110531      * Gestione Errore
199900110531     C     *IN99         IFEQ      '1'
200000110531     C  N50              Z-ADD     S1NRR         C1NRR
200100110531     C  N50              SETON                                        50
200200110531X0282C                   SETON                                        65
200300110531     C                   SETON                                        75
200400110531     C                   ELSE
200500110531X0197X**N41              MOVE      *BLANK        S1OPZ
200600110531X0197C  N41              CLEAR                   S1OPZ
200700110531     C                   ENDIF
200800110531X0197X***** ex-X0147     Eval      *in76 = S1Prot
200900110531  "   * attributi sfl
201000110531X0197C                   ExSr      AtrS1
201100110531     C                   UPDATE    K850S1
201200110531     C                   READC     K850S1                                 40
201300110531     C**
201400110531     C** A FINE LETTURA DEL SFL REIMPOSTA I CAMPI SPORCATI
201500110531     C** DA UNA EVENTUALE SCELTA DI OPZIONE...
201600110531     C     *IN40         IFEQ      '1'
201700110531     C                   MOVE      SAVATT        LOGATT
201800110531     C                   MOVE      SAVNRA        LOGNRA
201900110531     C                   MOVE      SAVNME        LOGNME
202000110531     C                   MOVE      SAVCBI        LOGCBI
202100110531     C                   MOVE      SAVCBE        LOGCBE
202200110531     C                   MOVE      SAVDAC        LOGDAC
202300110531     C                   MOVE      SAVORC        LOGORC
202400110531     C                   MOVE      SAVDAI        LOGDAI
202500110531     C                   MOVE      SAVORI        LOGORI
202600110531     C                   MOVE      SAVDAF        LOGDAF
202700110531     C                   MOVE      SAVORF        LOGORF
202800110531     C                   MOVE      SAVDEA        LOGDEA
202900110531     C                   MOVEL     SAVKP1        LOGKPJ
203000110531     C                   MOVE      SAVKP2        LOGKPJ
203100110531     C                   MOVE      SAVSTP        LOGSTP
203200110531     C                   ENDIF
203300110531     C                   ENDDO
203400110531     C                   ENDSR
203500110531      *
203600110531      *----------------------------------------------------*
203700110531      * CTRD4 - CONTR. WINDOWS MODIFICA CODA E PRIORITA'   *
203800110531      *----------------------------------------------------*
203900110531      *
204000110531     C     CTRD4         BEGSR
204100110531      *
204200110531     C                   SETOFF                                       8081
204300110531     C                   SETOFF                                       7778
204400110531     C                   SETOFF                                       7379
204500110531      * Controllo Coda
204600110531     C                   MOVE      CMD(1)        CMD1
204700110531     C                   MOVEL     D4NEWC        NOME
204800110531     C                   MOVEL     '*JOBQ  '     TIPO
204900110531     C                   CALL      'QCMDEXC'                            80
205000110531     C                   PARM                    CMD1
205100110531     C                   PARM      80            LUNG
205200110531X0191 * se la coda esiste...
205300110531  "  C                   If        *In80 = *Off
205400110531  "   * User-Exit "controlla coda batch"
205500110531  "  C                   Call      'BAK850RU2'                          22
205600110531  "  C                   Parm                    KPJBA
205700110531  "  C                   Parm      D4NEWC        JobQU2
205800110531  "  C                   Parm      *Off          RtnCdeU2
205900110531  "  C                   If        *In22 = *Off and RtnCdeU2 <> *Off
206000110531  "  C                   Eval      *In80 = *On
206100110531  "  C                   EndIf
206200110531X0191C                   EndIf
206300110531      * Controllo Priorità
206400110531X0281X**N07D4NEWP        IFNE      '6'
206500110531X0281C     D4NEWP        IFNE      '6'
206600110531     C     D4NEWP        ANDNE     '5'
206700110531     C     D4NEWP        ANDNE     '4'
206800110531     C     D4NEWP        ANDNE     '7'
206900110531X0024C     D4NEWP        ANDNE     '8'
207000110531X0024C     D4NEWP        ANDNE     '9'
207100110531     C                   SETON                                        81
207200110531     C                   ENDIF
207300110531      * Controllo Data
207400110531     C  N07D4AA          IFNE      0
207500110531     C     D4MM          ORNE      0
207600110531     C     D4GG          ORNE      0
207700110531     C     D4MM          IFGT      12
207800110531     C     D4MM          ORLT      1
207900110531     C                   SETON                                        78
208000110531     C                   ELSE
208100110531     C     D4MM          IFEQ      2
208200110531     C     D4AA          DIV       4             RIS               2 0
208300110531     C                   MVR                     RES               1 0
208400110531     C     RES           IFEQ      0
208500110531     C                   Z-ADD     29            MES(2)
208600110531     C                   ELSE
208700110531     C                   Z-ADD     28            MES(2)
208800110531     C                   ENDIF
208900110531     C                   ENDIF
209000110531     C     D4GG          IFGT      MES(D4MM)
209100110531     C     D4GG          ORLT      1
209200110531     C                   SETON                                        78
209300110531     C                   ENDIF
209400110531     C                   ENDIF
209500110531     C                   ENDIF
209600110531      * Controllo Ora
209700110531     C  N07D4ORA         IFNE      0
209800110531     C     D4MIN         ORNE      0
209900110531     C     D4ORA         IFGT      23
210000110531     C     D4MIN         ORGT      59
210100110531X0169X*****D4SEC         ORGT      59
210200110531     C                   SETON                                        77
210300110531     C                   ENDIF
210400110531     C                   ENDIF
210500110531      *
210600110531     C  N07D4GG          IFEQ      *ZERO
210700110531     C     D4MM          ANDEQ     *ZERO
210800110531     C     D4AA          ANDEQ     *ZERO
210900110531     C     S1ERR         ANDEQ     'S'
211000110531     C                   SETON                                        73
211100110531     C                   ENDIF
211200110531      *
211300110531     C  N07D4ORA         IFEQ      *ZERO
211400110531     C     D4MIN         ANDEQ     *ZERO
211500110531X0169X*****D4SEC         ANDEQ     *ZERO
211600110531     C     S1ERR         ANDEQ     'S'
211700110531     C                   SETON                                        79
211800110531     C                   ENDIF
211900110531     C                   ENDSR
212000110531      *
212100110531      *----------------------------------------------------*
212200110531      * VALD3 - VALORIZZAZIONE VIDEATA VISUALIZZ.          *
212300110531      *----------------------------------------------------*
212400110531      *
212500110531     C     VALD3         BEGSR
212600110531     C                   Z-ADD     LOGNRA        D3NRAZ
212700110531     C                   MOVEL     LOGDEA        D3DEAZ
212800110531     C                   MOVEL     LOGNME        D3NOML
212900110531     C                   MOVEL     LOGCBI        D3CBIN
213000110531     C                   MOVEL     LOGCBE        D3CBES
213100110531     C*
213200110531     C                   Z-ADD     LOGDAC        DATA
213300110531     C                   EXSR      INVDAT
213400110531     C                   Z-ADD     DATA          D3DINS
213500110531     C                   Z-ADD     LOGORC        D3OINS
213600110531     C*
213700110531     C                   MOVEL     LOGKPJ        JBADS
213800110531X0169X*****JBUF          IFNE      *BLANK
213900110531     C                   MOVEL     JBUF          SCHEDS
214000110531X0169X*****              ELSE
214100110531  "  X*****              CLEAR                   SCHEDS
214200110531  "  X*****              ENDIF
214300110531  "  C     SCHEDS        IFNE      *BLANK
214400110531X0169C     SCHEDS        ANDNE     *ALL'0'
214500110531     C                   MOVEL     DSCHE         DATA
214600110531     C                   EXSR      INVDAT
214700110531     C                   Z-ADD     DATA          D3DSCH
214800110531     C                   MOVEL     OSCHE         D3OSCH
214900110531X0169C                   ELSE
215000110531  "  C                   CLEAR                   D3DSCH
215100110531  "  C                   CLEAR                   D3OSCH
215200110531X0169C                   ENDIF
215300110531     C*
215400110531     C                   Z-ADD     LOGDAI        DATA
215500110531     C                   EXSR      INVDAT
215600110531     C                   Z-ADD     DATA          D3DINI
215700110531     C                   Z-ADD     LOGORI        D3OINI
215800110531     C                   Z-ADD     LOGDAF        DATA
215900110531     C                   EXSR      INVDAT
216000110531     C                   Z-ADD     DATA          D3DFIN
216100110531     C                   Z-ADD     LOGORF        D3OFIN
216200110531     C*
216300110531     C                   MOVEL     LOGKPJ        JBADS
216400110531     C                   MOVEL     CDA           D3COAZ
216500110531     C                   MOVEL     NPU           D3PRUT
216600110531     C                   MOVEL     NSI           D3SINF
216700110531     C                   Z-ADD     NRL           D3NUML
216800110531     C                   MOVEL     NTM           D3TERM
216900110531     C                   MOVEL     STJ           D3STJB
217000110531     C                   MOVEL     PTY           D3PRJB
217100110531X0202C                   IF        TIA = *BLANK
217200110531  "  C                   MOVEL     'I'           D3TPAZ
217300110531  "  C                   ELSE
217400110531  "  C                   MOVEL     TIA           D3TPAZ
217500110531X0202C                   ENDIF
217600110531     C*
217700110531     C                   MOVEA     JBU1          JB
217800110531     C                   EXSR      JJ
217900110531     C                   MOVEA     JB            D3JBU1
218000110531     C*
218100110531     C                   MOVEA     JBU2          JB
218200110531     C                   EXSR      JJ
218300110531     C                   MOVEA     JB            D3JBU2
218400110531     C*
218500110531     C                   MOVEA     JBU3          JB
218600110531     C                   EXSR      JJ
218700110531     C                   MOVEA     JB            D3JBU3
218800110531     C*
218900110531     C                   MOVEA     JBU4          JB
219000110531     C                   EXSR      JJ
219100110531     C                   MOVEA     JB            D3JBU4
219200110531     C*
219300110531     C                   MOVEA     JBU5          JB
219400110531     C                   EXSR      JJ
219500110531     C                   MOVEA     JB            D3JBU5
219600110531     C                   ENDSR
219700110531     C*
219800110531     C     JJ            BEGSR
219900110531     C                   DO        60            J                 3 0
220000110531     C     JB(J)         IFLT      X'40'
220100110531     C                   MOVE      X'1C'         JB(J)
220200110531     C                   ENDIF
220300110531     C                   ENDDO
220400110531     C                   ENDSR
220500110531      *
220600110531      *----------------------------------------------------*
220700110531      * VALCMD - VALORIZZ. DS COMANDI JOB (QCMDEXC)        *
220800110531      *----------------------------------------------------*
220900110531      *
221000110531     C     VALCMD        BEGSR
221100110531     C                   MOVEL     S1NUML        NUMJOB
221200110531     C                   MOVEA     *BLANK        SK1
221300110531     C                   MOVEA     S1PRUT        SK1(1)
221400110531     C                   Z-ADD     1             X                 3 0
221500110531     C     ' '           LOOKUP    SK1(X)                                 49
221600110531     C                   MOVEA     '/'           SK1(X)
221700110531     C                   ADD       1             X
221800110531     C                   MOVEA     S1NOML        SK1(X)
221900110531     C                   MOVEA     SK1           PRUNOJ
222000110531     C                   ENDSR
222100110531      *
222200110531      *----------------------------------------------------*
222300110531      * CHKACT - CONTROLLA CHE LAVORO NON SIA ATTIVO       *
222400110531      *----------------------------------------------------*
222500110531      *
222600110531     C     CHKACT        BEGSR
222700110531X0195X*****              CLEAR                   DS036
222800110531  "  X*****              MOVEL     LOGNME        JNAM36
222900110531  "  X*****              MOVEL     '*ALL'        UNAM36
223000110531  "  X*****              MOVEL     '*ALL'        JNBR36
223100110531  "  C                   MOVEL     LOGNME        JOBNAM036
223200110531  "  C                   MOVEL     '*ALL'        USRNAM036
223300110531X0195C                   MOVEL     '*ALL'        JOBNBR036
223400110531     C                   CALL      'BAK036R'                            99
223500110531X0195X*****              PARM                    DS036
223600110531X0195C                   PARM                    BAK036DS
223700110531     C* ATTIVO
223800110531X0195X*****JSTS36        IFEQ      '*ACTIVE'
223900110531X0195C     JOBSTS036     IFEQ      '*ACTIVE'
224000110531     C                   MOVEL     *ON           ACTIVE            1
224100110531     C* NON ATTIVO
224200110531     C                   ELSE
224300110531     C* CONGELA
224400110531     C                   MOVE      CMD(3)        CMD2
224500110531     C                   MOVEL     LOGKPJ        JBADS
224600110531     C     TIA           IFEQ      'B'
224700110531     C                   MOVEL     LOGNME        NOMLAV
224800110531     C                   ELSE
224900110531     C                   EXSR      VALCMD
225000110531     C                   ENDIF
225100110531     C                   CALL      'QCMDEXC'                            99
225200110531     C                   PARM                    CMD2
225300110531     C                   PARM      80            LUNG
225400110531     C* RITESTA
225500110531     C                   MOVEL     *OFF          ACTIVE
225600110531X0195X*****              CLEAR                   DS036
225700110531  "  X*****              MOVEL     LOGNME        JNAM36
225800110531  "  X*****              MOVEL     '*ALL'        UNAM36
225900110531  "  X*****              MOVEL     '*ALL'        JNBR36
226000110531  "  C                   CLEAR                   BAK036DS
226100110531  "  C                   MOVEL     LOGNME        JOBNAM036
226200110531  "  C                   MOVEL     '*ALL'        USRNAM036
226300110531X0195C                   MOVEL     '*ALL'        JOBNBR036
226400110531     C                   CALL      'BAK036R'                            99
226500110531X0195X*****              PARM                    DS036
226600110531X0195C                   PARM                    BAK036DS
226700110531     C* ATTIVO
226800110531X0195X*****JSTS36        IFEQ      '*ACTIVE'
226900110531X0195C     JOBSTS036     IFEQ      '*ACTIVE'
227000110531     C                   MOVEL     *ON           ACTIVE            1
227100110531     C* SCONGELA
227200110531     C                   MOVE      CMD(6)        CMD2
227300110531     C                   MOVEL     LOGKPJ        JBADS
227400110531     C     TIA           IFEQ      'B'
227500110531     C                   MOVEL     LOGNME        NOMLAV
227600110531     C                   ELSE
227700110531     C                   EXSR      VALCMD
227800110531     C                   ENDIF
227900110531     C                   CALL      'QCMDEXC'                            99
228000110531     C                   PARM                    CMD2
228100110531     C                   PARM      80            LUNG
228200110531     C                   ENDIF
228300110531     C                   ENDIF
228400110531     C                   ENDSR
228500110531      *
228600110531      *----------------------------------------------------*
228700110531      * INVDAT - INVERSIONE DATA                           *
228800110531      *----------------------------------------------------*
228900110531      *
229000110531     C     INVDAT        BEGSR
229100110531     C                   Z-ADD     DATA          DATROV            6 0
229200110531     C                   MOVEL     AA            DATROV
229300110531     C                   MOVE      GG            DATROV
229400110531     C                   Z-ADD     DATROV        DATA
229500110531     C                   ENDSR
229600110531      *
229700110531      *----------------------------------------------------*
229800110531      * INVDAT1- INVERSIONE DATA SCHEDULAZIONE             *
229900110531      *----------------------------------------------------*
230000110531      *
230100110531     C     INVDA1        BEGSR
230200110531     C                   Z-ADD     DATS          DATRO1            6 0
230300110531     C                   MOVEL     AA4           DATRO1
230400110531     C                   MOVE      GG4           DATRO1
230500110531     C                   Z-ADD     DATRO1        DATS
230600110531     C                   ENDSR
230700110531      *
230800110531X0026 *----------------------------------------------------
230900110531  "   * INIZIALIZZAZIONE TESTATA DEL VIDEO
231000110531  "   *----------------------------------------------------
231100110531  "  C     INZT1         BEGSR
231200110531  "   *
231300110531  "   * Reperisco dati società
231400110531  "  C                   CLEAR                   XSOCDS
231500110531  "  C                   CALL      'BAK500R'
231600110531  "  C                   PARM                    XSOCDS
231700110531  "   *
231800110531  "   * Controlli dati società
231900110531  "  C     XSOCDS        IFNE      *BLANKS
232000110531  "  C                   MOVEL     XSOCDS        SOC001
232100110531  "  C                   ENDIF
232200110531  "   *
232300110531  "   * Valorizzazione campi univoci testate
232400110531  "  C                   MOVEL     KNSIF         NOMSIF
232500110531  "  C                   MOVEL     XSCDSI        NOMDIT
232600110531  "  C                   MOVEL     DSPGM         NOMPGM
232700110531  "   *
232800110531X0026C                   ENDSR
232900110531      *----------------------------------------------------*
233000110531      * DEFCAM - DEFINIZIONE CAMPI E KLIST                 *
233100110531      *----------------------------------------------------*
233200110531      *
233300110531     C     DEFCAM        BEGSR
233400110531      *
233500110531     C     *ENTRY        PLIST
233600110531     C                   PARM                    KPJBA
233700110531      *
233800110531X0026C                   EXSR      INZT1
233900110531      *
234000110531X0127X*****              MOVEL     KPJBU         PRO850            1
234100110531  "   * verifico se pgm chiamato in modalità "analisi azioni" (azione K850)
234200110531  "   *  oppure "analisi azioni utente" (azione K851)
234300110531  "  C                   If        KCDAZ = 'K851'
234400110531  "  C                   Eval      $PgmAziUte = *On
234500110531  "  C                   Else
234600110531  "  C                   Eval      $PgmAziUte = *Off
234700110531  "  C                   EndIf
234800110531X0127 *
234900110531     C                   MOVE      *BLANK        $INZS1            1
235000110531X0282C                   Eval      WPagina = PAGINA
235100110531      *
235200110531     C     *LIKE         DEFINE    LOGDAI        KDI
235300110531     C     *LIKE         DEFINE    LOGORI        KOI
235400110531     C     *LIKE         DEFINE    LOGDAF        KDF
235500110531X0308C     *LIKE         DEFINE    LOGCDA        KCDA
235600110531     C     *LIKE         DEFINE    LOGORF        KOF
235700110531X0247C     *LIKE         DEFINE    PTY           KPC
235800110531X0202C     *LIKE         DEFINE    LOGDAC        KDC
235900110531  "  C     *LIKE         DEFINE    LOGORC        KOC
236000110531  "  C     *LIKE         DEFINE    LOGNMU        KUS
236100110531  "  C     *LIKE         DEFINE    LOGCBE        KCE
236200110531X0202C     *LIKE         DEFINE    LOGCBI        KCI
236300110531     C     *LIKE         DEFINE    LOGATT        SAVATT
236400110531     C     *LIKE         DEFINE    LOGNRA        SAVNRA
236500110531     C     *LIKE         DEFINE    LOGNME        SAVNME
236600110531     C     *LIKE         DEFINE    LOGCBI        SAVCBI
236700110531     C     *LIKE         DEFINE    LOGCBE        SAVCBE
236800110531     C     *LIKE         DEFINE    LOGDAC        SAVDAC
236900110531     C     *LIKE         DEFINE    LOGORC        SAVORC
237000110531     C     *LIKE         DEFINE    LOGDAI        SAVDAI
237100110531     C     *LIKE         DEFINE    LOGORI        SAVORI
237200110531     C     *LIKE         DEFINE    LOGDAF        SAVDAF
237300110531     C     *LIKE         DEFINE    LOGORF        SAVORF
237400110531     C     *LIKE         DEFINE    LOGDEA        SAVDEA
237500110531     C     *LIKE         DEFINE    LOGSTP        SAVSTP
237600110531     C     *LIKE         DEFINE    DATS          WWDAT
237700110531      *
237800110531X0202X*****KLOG2         KLIST
237900110531  "  X*****              KFLD                    KDF
238000110531  "  X*****              KFLD                    KOF
238100110531  "  X*****              KFLD                    KDI
238200110531X0202X*****              KFLD                    KOI
238300110531X0313C     k06log04      KList
238400110531  "  C                   KFld                    KDF
238500110531  "  C                   KFld                    KOF
238600110531  "  C                   KFld                    KDI
238700110531  "  C                   KFld                    KOI
238800110531  "  C                   KFld                    KPC
238900110531X0313C                   KFld                    KDC
239000110531X0247X***  k06log04      KList
239100110531X0247C     k07log04      KList
239200110531X0202C                   KFld                    KDF
239300110531  "  C                   KFld                    KOF
239400110531  "  C                   KFld                    KDI
239500110531X0202C                   KFld                    KOI
239600110531X0247C                   KFld                    KPC
239700110531X0202C                   KFld                    KDC
239800110531X0202C                   KFld                    KOC
239900110531      *
240000110531X0308C     k05log04      KList
240100110531  "  C                   KFld                    KDF
240200110531  "  C                   KFld                    KOF
240300110531  "  C                   KFld                    KDI
240400110531  "  C                   KFld                    KOI
240500110531X0308C                   KFld                    KPC
240600110531      *
240700110531X0202C     k04log04      KList
240800110531  "  C                   KFld                    KDF
240900110531  "  C                   KFld                    KOF
241000110531  "  C                   KFld                    KDI
241100110531  "  C                   KFld                    KOI
241200110531  "   *
241300110531  "  C     k02log04      KList
241400110531  "  C                   KFld                    KDF
241500110531  "  C                   KFld                    KOF
241600110531X0202 *
241700110531X0247X*** ex-X0202 k07log05      KList
241800110531X0247C     k08log05      KList
241900110531X0202C                   KFld                    KUS
242000110531  "  C                   KFld                    KDF
242100110531  "  C                   KFld                    KOF
242200110531  "  C                   KFld                    KDI
242300110531X0202C                   KFld                    KOI
242400110531X0247C                   KFld                    KPC
242500110531X0202C                   KFld                    KDC
242600110531X0202C                   KFld                    KOC
242700110531      *
242800110531X0308C     k06log05      KList
242900110531  "  C                   KFld                    KUS
243000110531  "  C                   KFld                    KDF
243100110531  "  C                   KFld                    KOF
243200110531  "  C                   KFld                    KDI
243300110531  "  C                   KFld                    KOI
243400110531X0308C                   KFld                    KPC
243500110531      *
243600110531X0202C     k05log05      KList
243700110531  "  C                   KFld                    KUS
243800110531  "  C                   KFld                    KDF
243900110531  "  C                   KFld                    KOF
244000110531  "  C                   KFld                    KDI
244100110531  "  C                   KFld                    KOI
244200110531  "   *
244300110531  "  C     k03log05      KList
244400110531  "  C                   KFld                    KUS
244500110531  "  C                   KFld                    KDF
244600110531  "  C                   KFld                    KOF
244700110531X0202 *
244800110531X0308C     k06log06      KList
244900110531  "  C                   KFld                    KCE
245000110531  "  C                   KFld                    KDF
245100110531  "  C                   KFld                    KOF
245200110531  "  C                   KFld                    KDI
245300110531  "  C                   KFld                    KOI
245400110531X0308C                   KFld                    KPC
245500110531      *
245600110531X0202C     k05log06      KList
245700110531  "  C                   KFld                    KCE
245800110531  "  C                   KFld                    KDF
245900110531  "  C                   KFld                    KOF
246000110531  "  C                   KFld                    KDI
246100110531  "  C                   KFld                    KOI
246200110531  "   *
246300110531  "  C     k03log06      KList
246400110531  "  C                   KFld                    KCE
246500110531X0202C                   KFld                    KDF
246600110531X0227X*****              KFld                    KDF
246700110531X0227C                   KFld                    KOF
246800110531X0202 *
246900110531X0247X*** ex-X0202 k07log07      KList
247000110531X0247C     k08log07      KList
247100110531X0202C                   KFld                    KCI
247200110531  "  C                   KFld                    KDF
247300110531  "  C                   KFld                    KOF
247400110531  "  C                   KFld                    KDI
247500110531X0202C                   KFld                    KOI
247600110531X0247C                   KFld                    KPC
247700110531X0202C                   KFld                    KDC
247800110531X0202C                   KFld                    KOC
247900110531      *
248000110531X0308C     k06log07      KList
248100110531  "  C                   KFld                    KCI
248200110531  "  C                   KFld                    KDF
248300110531  "  C                   KFld                    KOF
248400110531  "  C                   KFld                    KDI
248500110531  "  C                   KFld                    KOI
248600110531X0308C                   KFld                    KPC
248700110531      *
248800110531X0202C     k05log07      KList
248900110531  "  C                   KFld                    KCI
249000110531  "  C                   KFld                    KDF
249100110531  "  C                   KFld                    KOF
249200110531  "  C                   KFld                    KDI
249300110531X0202C                   KFld                    KOI
249400110531      *
249500110531X0308 * Nuovi logici con l'azione in chiave
249600110531  "   * kpLog08
249700110531  "  C     k03log08      KList
249800110531  "  C                   KFld                    KCda
249900110531  "  C                   KFld                    KDF
250000110531  "  C                   KFld                    KOF
250100110531  "   *
250200110531  "   * kpLog09
250300110531  "  C     k02log09      KList
250400110531  "  C                   KFld                    KCda
250500110531  "  C                   KFld                    KUS
250600110531  "  C     k04log09      KList
250700110531  "  C                   KFld                    KCda
250800110531  "  C                   KFld                    KUS
250900110531  "  C                   KFld                    KDF
251000110531  "  C                   KFld                    KOF
251100110531  "   *
251200110531  "   * kpLog10
251300110531  "  C     k02log10      KList
251400110531  "  C                   KFld                    KCda
251500110531  "  C                   KFld                    KCE
251600110531  "  C     k04log10      KList
251700110531  "  C                   KFld                    KCda
251800110531  "  C                   KFld                    KCE
251900110531  "  C                   KFld                    KDF
252000110531  "  C                   KFld                    KOF
252100110531X0308 *
252200110531      * Default video Guida
252300110531X0127 * se pgm richiamato in modalità "analisi azioni utente"...
252400110531  "  X*****PRO850        IFEQ      'U'
252500110531X0127C     $PgmAziUte    IFEQ      *On
252600110531X0182 * oppure l'utente ha possibilità limitate...
252700110531X0182C     PRFAAU        OREQ      '1'
252800110531X0127 * - propongo il profilo
252900110531     C                   MOVEL     KNMUS         D0PRUT
253000110531X0182X*** ex-X0127 - se l'utente ha possibilità limitate --> prot. profilo
253100110531  "  X***** ex-X0127     If        PRFAAU = '1'
253200110531  "   * - se pgm richiamato in modalità "analisi azioni utente"
253300110531  "   *   e l'utente ha possibilità limitate --> proteggo profilo
253400110531  "  C                   If        $PgmAziUte = *On and
253500110531X0182C                             PRFAAU = '1'
253600110531X0127C                   Eval      *In06 = *On
253700110531  "  C                   Else
253800110531  "  C                   Eval      *In06 = *Off
253900110531X0127C                   EndIf
254000110531     C                   ENDIF
254100110531     C                   Z-ADD     UDATE         DATA
254200110531     C                   Z-ADD     GG            D0GG
254300110531     C                   Z-ADD     MM            D0MM
254400110531     C                   Z-ADD     AA            D0AA
254500110531X0308C                   Z-ADD     GG            D0GGa
254600110531 "   C                   Z-ADD     MM            D0MMa
254700110531X0308C                   Z-ADD     AA            D0AAa
254800110531X0313C                   Z-ADD     23            D0OraA
254900110531 "   C                   Z-ADD     59            D0MinA
255000110531X0313C                   Z-ADD     59            D0SecA
255100110531     C                   MOVE      'N'           D0ERR
255200110531     C                   MOVE      *OFF          $INZS1
255300110531X0097C                   Z-ADD     0             CURR              2 0
255400110531  "  C                   Z-ADD     0             CURC              2 0
255500110531  "  C                   Z-ADD     0             H1RIGA
255600110531X0097C                   Z-ADD     0             H1COLO
255700110531      *
255800110531     C                   ENDSR
255900110531X0097C***************************************************************
256000110531  "  C* RICERCHE
256100110531  "  C***************************************************************
256200110531X0097C     SEARCH        BEGSR
256300110531     C*
256400110531     C* determino Riga/Colonna del cursore
256500110531     C                   MOVE      BINRIG        R$$
256600110531     C                   MOVE      BINCOL        C$$
256700110531     C                   Z-ADD     RIRI          CURR
256800110531     C                   Z-ADD     COCO          CURC
256900110531     C*
257000110531     C                   SELECT
257100110531     C*
257200110531     C* Profilo
257300110531     C     H1NMFL        WHENEQ    'D0PRUT'
257400110531X0127C     *In06         ANDEQ     *Off
257500110531     C                   RESET                   K980DS
257600110531     C                   MOVE      '01'          OPZ980
257700110531     C                   MOVEL     K980DS        KPJBU
257800110531     C                   CALL      'BAK980R'
257900110531     C                   PARM                    KPJBA
258000110531     C                   MOVEL     KPJBU         K980DS
258100110531     C     PRF980        IFNE      *BLANK
258200110531     C                   MOVEL     PRF980        D0PRUT
258300110531     C                   ENDIF
258400110531     C*
258500110531     C* Sistema informativo
258600110531     C     H1NMFL        WHENEQ    'D0SINF'
258700110531     C                   RESET                   K988DS
258800110531     C                   MOVE      '01'          OPZ988
258900110531     C                   MOVEL     K988DS        KPJBU
259000110531     C                   CALL      'BAK988R'
259100110531     C                   PARM                    KPJBA
259200110531     C                   MOVEL     KPJBU         K988DS
259300110531     C     NSI988        IFNE      *BLANK
259400110531     C                   MOVEL     NSI988        D0SINF
259500110531     C                   ENDIF
259600110531X0265X*** ex-X0099
259700110531  "  X***     "    FMTD1 - Azione
259800110531  "  X***     "    H1NMFL        WHENEQ    'D0AZN'
259900110531  "  X***     "    se s.i. impostato correttamente eseguo OVRDBF in libl del s.i.
260000110531  "  X***     "          Eval      $OvrDB = *Off
260100110531X0265X*** ex-X0099       If        D0SINF <> *Blank
260200110531X0177X*****              CallB     'BAK003R1'
260300110531X0265X*** ex-X0177       CallB     'BAK003R'
260400110531  "  X*** ex-X0099       Parm                    D0SINF
260500110531  "  X***     "          Parm                    KFLRC
260600110531  "  X***     "          Parm                    LibL
260700110531  "  X***     "          Parm                    Errore
260800110531  "  X***     "          If        Errore = *Off
260900110531  "  X***     "    eseguo OVRDBF file azioni
261000110531  "  X***     "          Eval      $OvrDB = *On
261100110531  "  X***     "          Eval      File = 'KFAZN10F'
261200110531  "  X***     "          Eval      SisInf = D0SINF
261300110531  "  X***     "          Eval      Lingua = KFLRC
261400110531  "  X***     "          Eval      Scope = 'J'
261500110531  "  X***     "          ExSr      ExeOVRDBF
261600110531  "  X***     "          EndIf
261700110531  "  X***     "          EndIf
261800110531  "  X***     "    pgm ricerca azioni
261900110531  "  X***     "          Reset                   BAK986DS
262000110531  "  X***     "          Eval      Opz986 = '01'
262100110531  "  X***     "          MoveL     BAK986DS      KPJBU
262200110531  "  X***     "          CALL      'BAK986R'
262300110531  "  X***     "          PARM                    KPJBA
262400110531  "  X***     "          MoveL     KPJBU         BAK986DS
262500110531  "  X***     "          If        KCAZN986 <> *Blank
262600110531  "  X***     "          Eval      D0AZN = KCAZN986
262700110531  "  X***     "          EndIf
262800110531  "  X***     "    se eseguito OVRDBF eseguo DLTOVR file azioni
262900110531  "  X***     "          If        $OvrDB = *On
263000110531  "  X***     "          ExSr      ExeDLTOVR
263100110531X0265X*** ex-X0099       EndIf
263200110531     C*
263300110531     C* Ricerca non abilitata in questa posizione
263400110531     C                   OTHER
263500110531     C                   SETON                                        6899
263600110531     C*
263700110531     C                   ENDSL
263800110531     C* imposto pos. del cursore
263900110531     C                   Z-ADD     CURR          H1RIGA
264000110531     C                   Z-ADD     CURC          H1COLO
264100110531     C*
264200110531X0097C                   ENDSR
264300110531X0265X*** ATTENZIONE: eliminata subroutine ExeOVRDBF (ex-X0099)
264400110531X0265X*** ATTENZIONE: eliminata subroutine ExeDLTOVR (ex-X0099)
264500110531X0127C****************************************************************
264600110531  "  C* Operazioni iniziali
264700110531  "  C****************************************************************
264800110531X0127C     *INZSR        BEGSR
264900110531     C*
265000110531     C* aggancio profilo per reperire possibilità dell'utente nel pgm
265100110531     C     KNMUS         Chain     KPPRF01L
265200110531     C*
265300110531X0127C                   ENDSR
265400110531** 'CMD' - Schiera per QCMDX
265500110531CHKOBJ OBJ(          ) OBJTYPE(       )
265600110531CHGJOB JOB(      /                     ) JOBPTY( )  JOBQ(          )
265700110531HLDJOB JOB(      /                     )
265800110531ENDJOB JOB(      /                     ) OPTION(*IMMED)
265900110531DSPJOB JOB(      /                     )
266000110531RLSJOB JOB(      /                     )
266100110531** 'CDS' - Schiera per QCMDX
266200110531CHGJOB JOB(      /                     ) SCDDATE(      ) SCDTIME(      )
266300110531** 'MES' - Schiera Mesi dell'anno
266400110531312831303130313130313031
