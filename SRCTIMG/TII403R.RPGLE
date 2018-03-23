000100170320     H DFTACTGRP(*NO) BNDDIR('PJXBND':'YDR') ACTGRP('QILE')
000200990205     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300850705     F* OFN200R  *---------------------------------------------------*
000400850705     F*----------*                                                   *
000500850705     F*              - IMMISSIONE BOLLE FORNITORI -                  *
000600850705     F*              ------------------------------                  *
000700850705     F*                                                    AZ.: N200 *
000800850705     F*                                                              *
000900990205     F*         FILE : OFN200D  - tmmof00F - tmmof01L - tmfop01L     *
001000990205     F*                ANTAB00F - tmamp01L - tmsmp01L - tmmmp00F     *
001100910715 RIC F*                CNACO00F - COAMA01L                           *
001200850705     F*                                                              *
001300990317     F*         PGM. : tii002r - RICERCA CAUSALI DI MAGAZZINO        *
001400990317     F*                tii005R - INTERROG.LISTINI MATERIE PRIME      *
001500990317     F*                tii103R - RICERCA ALFABETICA FORNITORI        *
001600990317     F*                tii104R - RICERCA ALFABETICA MATERIE PRIME    *
001700990317     F*                tii108R - RICERCA MAGAZZINI                   *
001800990317     F*                TII116R - RICERCA LISTINI MATERIE PRIME       *
001900850705     F*                                                              *
002000850705     F*--------------------------------------------------------------*
002100990317     FTII403D   CF   E             WORKSTN
002200000000     F                                     SFILE(SFL1:REC1)
002300990205     Ftmmof00F  UF A E             DISK
002400990205     F                                     RENAME(tmmof000:ORDFIS)
002500990205     Ftmmof01L  IF   E           K DISK
002600990205     F                                     RENAME(tmmof000:ORDRIG)
002700000000     F                                     INFDS(INF1)
002800990205     Ftmfop01L  IF   E           K DISK
002900990205RIC  Fanent01l  IF   E           K DISK
003000000000     FANTAB00F  IF   E           K DISK
003100990205     Ftmamp01L  IF   E           K DISK
003200990205     Ftmsmp01L  UF A E           K DISK
003300990205     Ftmlmp01L  IF   E           K DISK
003400990205     Ftmmmp00F  O    E             DISK
003500010725     FTNTBE01l  IF   E           k disk
003600000000     FOFPRTSTD  O    F  132        PRINTER OFLIND(*INOF)
003700000000     F                                     USROPN
003800990205RIC  F*---------------------------------------------------------------
003900000000     D AZ              S              1    DIM(15)                              *AZIONI CAUS.
004000860403     D ERP             S             40    DIM(13) CTDATA PERRCD(1)             * SEGNALAZIONI
004100910715     D ERR             S             40    DIM(10) CTDATA PERRCD(1)             * ERRORI
004200000000     D*--- SCHIERE DI STAMPA -----------------------------------*
004300000000     D ST1             S             66    DIM(2) CTDATA PERRCD(1)
004400000000     D ST2             S             66    DIM(2) CTDATA PERRCD(1)
004500000000     D ST3             S             66    DIM(2) CTDATA PERRCD(1)
004600000000     D ST4             S             66    DIM(2) CTDATA PERRCD(1)
004700000000     D ST5             S             66    DIM(2) CTDATA PERRCD(1)
004800000000     D ST6             S             66    DIM(2) CTDATA PERRCD(1)
004900000000     D ST7             S             66    DIM(2) CTDATA PERRCD(1)
005000000000     D SCM             S             50    DIM(1) CTDATA PERRCD(1)              * CMD OVRPRTF
005100000000     D MPHDT1          DS           512
005200860403     D*-----DATA AREA NUMERATORE MOVIM. MAGAZZINO
005300860403     D  NUMDTA                 1      5  0
005400860403     D  FILL1                  6      8
005500860403     D  DATDTA                 9     14  0
005600860403     D  FILL2                 15     17
005700860403     D  ANNDTA                18     19  0
005800860403     D  FIL03                 20     50
005900860403     D  DGIO                  51     56  0
006000860403     D  FIL04                 57    100
006100860403     D  FILL5                101    300
006200860403     D  GIOTOL               301    302  0
006300860403     D  FILL6                303    512
006400000000     D OFNDT1          DS           512
006500860403     D*------------- NUMERATORE BOLLETTINI DI ENTRATA
006600000000     D  NUMBLT                 1      5  0
006700000000     D  FILOF1                 6      8
006800000000     D  DATBLT                 9     14  0
006900000000     D  FILOF2                15     17
007000000000     D  ANNBLT                18     19  0
007100000000     D  FILOF3                20    256
007200000000     D  FILOF4               257    512
007300000000     D OFNDT2          DS           512
007400860403     D*------------- NUMERAT.CODICE INDIVIDUALE RIGA
007500000000     D  NUMCRG                 1      7  0
007600000000     D  FILDT2                 8    256
007700000000     D  FILDT3               257    512
007800000000     D                 DS
007900860528     D*------------- SCOMPOSIZIONE CODICE / VERSIONE :
008000871008     D  MOFMAT                 1     12
008100871008     D  CODBAR                 1     12
008200000000     D INF1            DS           512
008300860403     D*------------- RELREC LOGICO :
008400000000     D  NREL1                397    400B 0
008500000000     D DSMAG         E DS                  EXTNAME(ANTAB153)
008600860403     D*------------- TABELLA MAGAZZINI :
008700000000     D TB152         E DS                  EXTNAME(ANTAB152)
008800860403     D*------------- TABELLA CAUSALI :
008900000000     D  AZE                   30     34
009000000000     D                                     DIM(5)                               *ENTRATE
009100000000     D  AZU                   39     43
009200000000     D                                     DIM(5)                               *USCITE
009300000000     D  AZG                   48     52
009400000000     D                                     DIM(5)                               *GESTIONALI
009500910715     D TB155           DS
009600910715     D*------------- TABELLA CATEGORIA MERCEOLOGICA:
009700910715     D  CAMPO                  1     74
009800910715     D  FLGATT                75     75
009900990205     D RECSAL        E DS                  EXTNAME(tmsmp00F)
010000990211     D  QTK                   86    145P 2
010100000000     D                                     DIM(10)                              *Q.TA'  SALDI
010200000000     D PAR8A0          DS
010300860403     D*------------- PARAMETRI PER INTERR. LISTINI :
010400000000     D  LIS8A0                 1      3
010500000000     D  CLA8A0                 4      6
010600000000     D  SOT8A0                 7     11
010700000000     D  CAT8A0                12     13
010800000000     D  ART8A0                14     20
010900000000     D  FIL8A0                21    255
011000000000     D  RIC8A0               256    256
011100860528     D KPJBA         E DS
011101171019     D IMOFDS00      E DS
011200860528     D*------------- PASSAGGIO PARAMETRI :
011300860528     D  MSGERR               453    502
011400990205     D soc001        E DS                  EXTNAME(xsoc001ds)
011500990205     D xsocds          DS          1000
011600010725    >D DGED          E DS
011700990205     d dataiso         s               d   datfmt(*iso)
011800990205     d dataeur         s               d   datfmt(*eur)
011900170320     d primogiro       s              1
012000170313      *__________________________________________________________
012100170313     D Skmessaggi      s            100a   DIM(255) varying
012400170313     D nrMessaggi      s             10I 0 inz
012500170313     D idMessaggio     s              7    dim(255)
012600170313     D tipoMessaggio   s              1    dim(255)
012700170313     D idcampo         s             10    dim(255)
012800170313     D testomessaggio  s            255a   DIM(255) varying
013100170313     d ydrereds      e ds                  extname(ydrere00f)
013200170313     d                                     QUALIFIED INZ(*EXTDFT)
013201171019     D pgmname         s             10    INZ('TII411R')
013300170313      /COPY GAITRASRC/SRCCONST,YDRERE
013400170313      /COPY GAITRASRC/SRCPROTOPR,YDRERE
013401171019     C*----------------------------------------------------*
013402171019     d ydrtfrjob       pr                  extpgm('YDRTFRJOB')
013403171019     d  kpjba                              likeds(kpjba)
013404171019     d  PGMNAME                            LIKE(PGMNAME)
013405171019     d
013500000000     C*----------------------------------------------------*
013600000000     C     *ENTRY        PLIST                                                  *
013700000000     C                   PARM                    KPJBA                          *
013800000000     C*                                                    *
013900000000     C     *DTAARA       DEFINE                  MPHDT1                         *
014000000000     C     *DTAARA       DEFINE                  OFNDT1                         *
014100000000     C     *DTAARA       DEFINE                  OFNDT2                         *
014200860403     C     *LIKE         DEFINE    KPJBU         KPJBUW                         *
014300990205     C*--- FILE ORDINI FORNIT. "tmmof01L"                  *
014400000000     C     KEYORD        KLIST                                                  *
014500000000     C                   KFLD                    CODFOR                         *
014600000000     C                   KFLD                    ORDINE            5 0          *
014700860528RIC  C******************************************************
014800990205RIC  C*--- KEY RICERCA C.D.C. SU "anent00F"                *
014900990205RIC  C     KEYCDC        KLIST                                                  *
015000990205RIC  C                   KFLD                    wsoc                           *
015100990205RIC  C                   KFLD                    tipocdc           3            *
015200990205RIC  C                   KFLD                    CCOSTO            8            *
015300990205RIC  C                   move      'CDC'         tipocdc                        *
015400860528RIC  C******************************************************
015500000000     C*--- FILE ANAGRAFICO TABELLE "ANTAB00F"              *
015600860404     C     KEYTAB        KLIST                                                  *
015700000000     C                   KFLD                    TABGRU                         *
015800000000     C                   KFLD                    TABCOD                         *
015900990205     C*--- FILE MATERIE PRIME "tmamp01L"                   *
016000000000     C     KEYART        KLIST                                                  *
016100000000     C                   KFLD                    CODART                         *
016200990205     C*--- FILE MATERIE PRIME "tmamp01L" X DELTA CONS.     *
016300000000     C     KEYDEL        KLIST                                                  *
016400000000     C                   KFLD                    MOFMAT                         *
016500990205     C*--- FILE SALDI MAGAZZINO M.P. "tmsmp01L"            *
016600000000     C     KEYSAL        KLIST                                                  *
016700990205     C                   KFLD                    ANCOMP            4 0          *
016800000000     C                   KFLD                    MAGAZZ            7            *
016900000000     C                   KFLD                    MOFMAT                         *
017000990205     C*--- FILE LISTINI "tmlmp01L"                         *
017100000000     C     KEYLIS        KLIST                                                  *
017200000000     C                   KFLD                    SC1LIS                         *
017300000000     C                   KFLD                    SC1MAT                         *
017400860403     C*----------- OVERRAID FILE DI STAMPA ----------------*
017500860403     C                   Z-ADD     50            LENGT            15 5          *
017600860403     C                   MOVEA     SCM           COMAND           50            *
017700860403     C                   CALL      'QCAEXEC'                                    *
017800860403     C                   PARM                    COMAND                         *
017900860403     C                   PARM                    LENGT                          *
018000000000     C*--- RICERCA DITTA ----------------------------------*
018100990205     C                   MOVEL     'SOC001'      TIPXSC
018200990205     C                   MOVEL     *blank        SOCXSC
018300990205     C                   EXSR      REPSOC
018400990205     C     RTNXSC        IFNE      '1'
018500990205     C                   MOVEL     XSOCDS        SOC001
018600990205     C                   MOVEL     xscrgs        desdit
018700990205     C                   MOVEL     xscsoc        wsoc              3
018800990205     C                   MOVEL     xscsoc        socpas            3
018900990205     c                   end
019000990205     c                   time                    como14           14 0
019100990205     c                   move      como14        woggi             8 0
019200990205     c                   move      woggi         dataeur
019300990205     c                   move      dataeur       dataiso
019400990205     c                   move      dataiso       woggig            8 0
019500000000     C                   Z-ADD     59            PAGE1             3 0          *
019600990205     C                   Z-ADD     woggi         DATMOV                         *
019700010725     C*--- Reperisco la divisa corrente ------------------------------*
019800010725     C     ktbe          KLIST
019900010725     C                   KFLD                    kcod
020000010725     C                   KFLD                    kke1
020100010725     C     *like         Define    tbecod        kcod
020200010725     C     *like         Define    tbeke1        kke1
020300010725     C                   move      'GED'         kcod
020400010725     C                   move      *blanks       kke1
020500010725     C                   movel     'MAGAZZINO'   kke1
020600010725     c                   clear                   dged
020700010725     C     ktbe          CHAIN     tntbe01l                           54
020800010725     C  n54              movel     tbeuni        dged
020900860403     C*---------- GIRO DATA GIORNALE :                     *
021000860403     C     TAGMPH        TAG                                                    *
021100860403     C     *LOCK         IN        MPHDT1                               90      *
021200860403     C   90              GOTO      TAGMPH                                       *
021300860403     C                   OUT       MPHDT1                                       *
021400990205     C     *dmy          MOVE      DGIO          dataeur                        *
021500990205     c                   move      dataeur       dataiso
021600990205     c                   move      dataiso       datgio            8 0
021700990205     C                   z-add     ANNDTA        ANNCOM                         *
021800990205     c                   if        anncom > 40
021900990205     c                   add       1900          anncom
022000990205     c                   else
022100990205     c                   add       2000          anncom
022200990205     c                   end
022300990205     C                   z-add     ANNcom        ANNdta4           4 0          *
022400860403     C*----EMISSIONE TESTATA ------------------------------*
022500990205     C                   MOVE      *all'9'       NOVE             12
022600871008     C                   MOVEL     '?'           INTERR           12
022700000000     C                   WRITE     OFN200T1                                     *
022800000000     C                   MOVEA     *ZEROS        *IN(1)                         *
022900990205     C                   MOVE      '?       '    CODFOR                         *
023000000000     C                   MOVE      *BLANKS       NOMFOR                         *
023100870105     C                   MOVE      NOVE          CODART                         *
023200860403     C                   MOVEL     ERP(12)       DESART                         *
023300860403     C                   MOVE      '99999'       NUMORD                         *
023400860224     C                   MOVE      '9999999'     CODMAG                         *
023500860224     C                   MOVEL     ERP(12)       DESMAG                         *
023600990205     C                   MOVE      '99999999'    CODCDC                         *
023700860224     C                   MOVEL     ERP(12)       DESCDC                         *
023800000000     C                   MOVE      *ZEROS        NUMDOC                         *
023900990205     C                   MOVE      woggi         DATDOC                         *
024000000000     C                   MOVE      *BLANKS       AIUTO                          *
024100860403     C                   MOVE      '999'         LISTRA                         *
024200860403     C                   MOVE      '0'           COLITR                         *
024300871008     C                   MOVE      *ALL'*'       SAVMAT           12            *
024400860403     C                   MOVE      '?  '         OLDCAU            3            *
024500000000     C*--- EMISSIONE PROMPT RICHIESTA FORNITORE :          *
024600860403     C     TAG001        TAG                                                    *
024700000000     C                   EXFMT     OFN200PT                                     *
024800000000     C                   MOVEA     *ZEROS        *IN(75)                        *
024900990409     C   Kc              GOTO      FINE                                         *
025000000000     C*--- RICERCA ALFABETICA PARTICOLARITA' FORNITORE ----*
025100000000     C     CODFOR        IFEQ      '?      '                                    *
025200990317     C                   CALL      'TII103R'                                    *
025300000000     C                   PARM                    CODFOR                         *
025400000000     C                   PARM                    NOMFOR                         *
025500860403     C                   SETON                                            91    *
025600000000     C                   GOTO      TAG001                                       *
025700000000     C                   END                                                    *
025800000000     C*--- RICERCA PER CODICE FORNITORE -------------------*
025900000000     C                   TESTN                   CODFOR               46        *
026000000000     C     *IN(46)       CABEQ     '0'           TAG001                   91    *
026100990205     C     CODFOR        CHAIN     tmfop01L                           91        *
026200000000     C   91              GOTO      ERRPMT                                       *
026300000000     C                   MOVE      FOPRSI        NOMFOR                         *
026400990205     C     CODFOR        SETLL     tmmof01L                           97        *
026500000000     C   97              GOTO      ERRPMT                                       *
026600000000     C*--- RICERCA ALFABETICA ARTICOLI --------------------*
026700870105     C     CODART        IFNE      NOVE                                         *
026800870105     C     CODART        IFEQ      INTERR                                       *
026900000000     C                   MOVE      *BLANKS       CODART                         *
027000990317     C                   CALL      'TII104R'                                    *
027100000000     C                   PARM                    CODART                         *
027200000000     C                   PARM                    DESART                         *
027300860403     C                   SETON                                            92    *
027400000000     C                   GOTO      TAG001                                       *
027500000000     C                   END                                                    *
027600000000     C*--- CONTROLLO CODICE PRODOTTO ----------------------*
027700990205     C     KEYART        CHAIN     tmamp01L                           92        *
027800000000     C   92              GOTO      ERRPMT                                       *
027900000000     C                   MOVEL     AMPDMT        DESART                         *
028000000000     C                   END                                                    *
028100000000     C*--- CONTROLLO NUMERO ORDINE ------------------------*
028200000000     C     NUMORD        IFNE      99999                                        *
028300000000     C                   Z-ADD     NUMORD        ORDINE                         *
028400990205     C     KEYORD        SETLL     tmmof01L                           93        *
028500000000     C   93              GOTO      ERRPMT                                       *
028600860403     C     NUMORD        CABLE     0             ERRPMT                 9393    *
028700000000     C                   END                                                    *
028800000000     C*--- CONTROLLO CODICE MAGAZZINO ---------------------*
028900860131     C     CODMAG        CABEQ     '9999999'     TAGCDC                         *
029000000000     C     CODMAG        IFEQ      '?      '                                    *
029100000000     C                   MOVE      *BLANKS       CODMAG                         *
029200000000     C                   MOVE      *BLANKS       DESM75           75            *
029300990317     C                   CALL      'TII108R'                                    *
029400000000     C                   PARM                    CODMAG                         *
029500000000     C                   PARM                    DESM75                         *
029600000000     C                   MOVEL     DESM75        DESMAG                         *
029700860403     C                   SETON                                            94    *
029800000000     C                   GOTO      TAG001                                       *
029900000000     C                   END                                                    *
030000000000     C                   MOVE      '153'         TABGRU                         *
030100000000     C                   MOVE      CODMAG        TABCOD                         *
030200860404     C     KEYTAB        CHAIN     ANTAB00F                           94        *
030300000000     C   94              GOTO      ERRPMT                                       *
030400000000     C  N94              DO                                                     *
030500000000     C                   MOVEL     TABDES        DSMAG                          *
030600000000     C                   MOVEL     DES153        DESMAG                         *
030700000000     C     CODCDC        IFEQ      *BLANKS                                      *
030800990318     C                   MOVEl     CDC153        CODCDC                         *
030900000000     C                   END                                                    *
031000000000     C                   END                                                    *
031100000000     C*--- CONTROLLO C.D.C. -------------------------------*
031200860131     C     TAGCDC        TAG                                                    *
031300990205     C     CODCDC        CABEQ     '99999999'    TAGDOC                         *
031400990205     C     CODCDC        IFEQ      '?       '                                   *
031500000000     C                   MOVE      *ZEROS        CCOSTO                         *
031600000000     C                   MOVE      *BLANKS       DESL48           48            *
031700990317     C                   CALL      'TII102R'                                    *
031800000000     C                   PARM                    CCOSTO                         *
031900000000     C                   PARM                    DESL48                         *
032000000000     C                   MOVE      CCOSTO        CODCDC                         *
032100000000     C                   MOVEL     DESL48        DESCDC                         *
032200860403     C                   SETON                                            95    *
032300000000     C                   GOTO      TAG001                                       *
032400000000     C                   END                                                    *
032500990205     C     CODCDC        CABEQ     *blank        ERRPMT                   95    *
032600860528 RIC C******************************************************
032700860528 RIC C                   MOVE      CODCDC        CCOSTO                         *
032800990205 RIC C     KEYCDC        CHAIN     anent01l                           95        *
032900000000     C   95              GOTO      ERRPMT                                       *
033000000000     C*--- CONTROLLO DATA DOCUMENTO -----------------------*
033100860131     C     TAGDOC        TAG                                                    *
033200990205     c     *eur          test(d)                 datdoc                 96
033300990205     c                   if        *in96
033400170320     c     *eur          test(d)                 datdoc                 96
033500170320     c  n96*eur          move      datdoc        dataeur
033600990205     c  n96              move      dataeur       datdoc
033601180115     c  n96              move      dataeur       dataiso
033602180116     c  n96              move      dataiso       datdocamg         8 0
033700990205     c                   end
033701180116     c                   if        not *in96
033702180116     c                   move      datdoc        dataeur
033704180116     c                   move      dataeur       dataiso
033705180116     c                   move      dataiso       datdocamg         8 0
033706180116     c     datdocamg     comp      woggig                             96
033708180116     c                   endif
033800990205     c   96              goto      errpmt
033900860403     C     NUMDOC        CABLE     0             ERRPMT                 9696    *
034000860403     C*---------- CAMBIO DATA MOVIMENTO -------------------*
034100940831     C   KD              SETON                                        04        *
034200860403     C   KD              GOTO      TAG001                                       *
034300990205     C                   MOVE      DATMOV        dataeur                        *
034400990205     C                   move      dataeur       dataiso                        *
034500990205     C                   move      dataiso       XDTAMG            8 0          *
034600860403     C     XDTAMG        CABLE     DATGIO        ERRPMT                 9999    *
034700860403     C                   SETOFF                                           04    *
034800860403     C*---------- CONTROLLO ANNO --------------------------*
034900990205     C     ANNDTA4       SUB       ANNCOM        UNO               2 08485      *
035000860403     C   84UNO           COMP      1                                  8080      *
035100860403     C   85UNO           COMP      -1                                 8080      *
035200860403     C   80              MOVEL     ERP(13)       AIUTO                          *
035300860403     C   80              SETON                                          89      *
035400860403     C   80              GOTO      TAG001                                       *
035500860403     C*--------- CONTROLLO LISTINO ------------------------*
035600860403     C     LISTRA        IFNE      '999'                                        *
035700000000     C     LISTRA        IFEQ      '?  '                                        *
035800000000     C                   MOVE      *BLANKS       LISTRA                         *
035900000000     C                   MOVE      *BLANKS       DESL75           75            *
036000990317     C                   CALL      'TII116R'                                    *
036100000000     C                   PARM                    LISTRA                         *
036200000000     C                   PARM                    DESL75                         *
036300000000     C                   MOVEL     DESL75        DESLIS                         *
036400860403     C                   SETON                                            90    *
036500000000     C                   GOTO      TAG001                                       *
036600000000     C                   END                                                    *
036700860403     C*-------------                                       *
036800000000     C                   MOVE      *BLANKS       DESLIS                         *
036900000000     C                   MOVE      '161'         TABGRU                         *
037000000000     C                   MOVE      *ZEROS        TABCOD                         *
037100000000     C                   MOVE      LISTRA        TABCOD                         *
037200860404     C     KEYTAB        CHAIN     ANTAB00F                           46        *
037300000000     C     *IN(46)       CABEQ     '1'           ERRPMT                   90    *
037400000000     C                   MOVEL     TABDES        DESLIS                         *
037500860403     C     COLITR        CABEQ     0             ERRPMT                   90    *
037600860403     C                   END                                                    *
037700000000     C*--- CONTROLLO PRESENZA ERRORI SU PROMPT ------------*
037800000000     C     ERRPMT        TAG                                                    *
037900000000     C     90            DO        99            X                 3 0          *
038000000000     C     *IN(X)        COMP      '1'                                    80    *
038100000000     C  N80              END                                                    *
038200000000     C   80              DO                                                     * ERRORI
038300000000     C     X             SUB       89            IN                2 0          *
038400000000     C                   MOVEA     ERP(IN)       AIUTO                          *
038500000000     C                   GOTO      TAG001                                       *
038600000000     C                   END                                                    *
038700000000     C*---                                                 *
038800000000     C                   MOVEA     *ZEROS        *IN(10)                        *
038900000000     C*--- RICERCA ORDINE ---------------------------------*
039000000000     C     SUCAR         TAG                                                    *
039100000000     C                   Z-ADD     0             REC1              5 0          *
039200000000     C                   Z-ADD     0             RECT              5 0          *
039300000000     C                   Z-ADD     0             SC1REC                         *
039400000000     C                   Z-ADD     0             SC1VID                         *
039500860403     C                   SETOFF                                           40    *
039600000000     C*--- PULIZIA SUBFILE "1" ----------------------------*
039700000000     C                   SETON                                            70    *
039800000000     C                   WRITE     SFL1CT                                       *
039900000000     C                   SETOFF                                           70    *
040000000000     C                   MOVEA     *ZEROS        *IN(75)                        *
040100000000     C                   Z-ADD     1             CURSOR                         * POSIZ.CURSORE
040200000000     C*--- PREPARA KLIST PER POSIZIONAMENTO ---------------*
040300000000     C     NUMORD        IFEQ      99999                                        *
040400000000     C                   Z-ADD     0             ORDINE                         *
040500000000     C                   ELSE                                                   *
040600000000     C                   Z-ADD     NUMORD        ORDINE                         *
040700000000     C                   END                                                    *
040800990205     C     KEYORD        SETLL     tmmof01L                           97        *
040900990205     C   97              goto      errpmt                                       *
041000000000     C*--- LETTURA RIGHE ORDINE ---------------------------*
041100000000     C     TAG002        TAG                                                    *
041200000000     C*          -------------                             *
041300990205     C     CODFOR        READE     tmmof01L                               46    *
041400000000     C   46              GOTO      DTAARA                                       *
041500000000     C*--- TEST NUMERO ORDINE                              *
041600000000     C     NUMORD        IFNE      99999                                        *
041700000000     C     NUMORD        CABLT     MOFNOR        DTAARA                         *
041800000000     C                   END                                                    *
041900000000     C*--- NON CARICO FASI "BB" - "FA"                     *
042000000000     C     MOFFAS        IFEQ      'BB'                                         *
042100860403     C                   MOVE      *ALL'*'       SAVMAT                         *
042200000000     C                   GOTO      TAG002                                       *
042300000000     C                   END                                                    *
042400000000     C     MOFFAS        IFEQ      'FA'                                         *
042500860403     C                   MOVE      *ALL'*'       SAVMAT                         *
042600000000     C                   GOTO      TAG002                                       *
042700000000     C                   END                                                    *
042800860403     C*--- TEST TIPI RIGA DA OMETTERE ---------------------*
042900860403     C     MOFTRG        CABEQ     'D'           TAG002                         *DEBITO
043000860403     C     MOFTRG        CABEQ     'I'           TAG002                         *IVA
043100860403     C     MOFTRG        CABEQ     'S'           TAG002                         *SPESE
043200860403     C     MOFTRG        CABEQ     '6'           TAG002                         *COMM. X FORNIT.
043300860403     C     MOFTRG        CABEQ     '7'           TAG002                         *COMM. X USO INT.
043400860403     C*--- CARICAMENTO RIGHE PRODOTTO ---------------------*
043500000000     C     MOFTRG        IFGE      '1'                                          *
043600000000     C     MOFTRG        IFLE      '3'                                          *
043700860403     C*--- TEST CODICE PRODOTTO E VERSIONE :               *
043800870105     C     CODART        IFNE      NOVE                                         *
043900860403     C     CODART        CABNE     MOFMAT        TAG002                         *
044000860403     C                   END                                                    *
044100860403     C*--- TEST SU MAGAZZINO RICHIESTO :                   *
044200860131     C     CODMAG        IFNE      '9999999'                                    *
044300860129     C     MOFMAG        CABNE     CODMAG        TAG002                         *
044400860131     C                   END                                                    *
044500860403     C     REC1          ADD       1             REC1                 40        *
044600000000     C                   Z-ADD     NREL1         SC1REC                         * NRR FISICO
044700000000     C                   Z-ADD     REC1          SC1VID                         * NRR VIDEO
044800000000     C                   MOVEL     CODBAR        SF1ART                         *
044900150421     c                   clear                   mofdcn6
045000150421     c                   if        mofdcn > 0
045100150421     c                   move      mofdcn        dataiso
045200150421     c                   move      dataiso       dataeur
045300150421     c     *dmy          move      dataeur       mofdcn6
045400150421     c                   endif
045500000000     C                   WRITE     SFL1                                         *
045600000000     C                   MOVE      MOFMAT        SAVMAT                         *
045700000000     C*--- CARICAMENTO COMMENTI DI RIGA X RIGHE 1 2 3 -----*
045800000000     C     MOFCOM        IFNE      *BLANKS                                      *
045900000000     C                   SETON                                            74    *
046000000000     C     REC1          ADD       1             REC1                           *
046100000000     C                   Z-ADD     NREL1         SC1REC                         * NRR FISICO
046200000000     C                   Z-ADD     REC1          SC1VID                         * NRR VIDEO
046300000000     C                   MOVE      *ZEROS        MOFNOR                         *
046400000000     C                   MOVE      *BLANKS       SF1ART                         *
046500860403     C                   MOVE      *BLANKS       MOFDMT                         *
046600860403     C                   MOVEL     MOFCOM        MOFDMT                         *
046700860403     C                   MOVE      *BLANKS       MOFTRG                         *
046800860403     C                   MOVE      *BLANKS       MOFTSR                         *
046900000000     C                   MOVE      *BLANKS       MOFUMQ                         *
047000000000     C                   MOVE      *ZEROS        MOFQTA                         *
047100000000     C                   MOVE      *BLANKS       MOFMAG                         *
047200860403     C                   MOVE      *BLANKS       MOFFAS                         *
047300150421     c                   clear                   mofdcn6
047400000000     C                   WRITE     SFL1                                         *
047500000000     C                   SETOFF                                           74    *
047600000000     C                   END                                                    *
047700000000     C                   GOTO      TAG002                                       *
047800000000     C                   END                                                    *
047900000000     C                   END                                                    *
048000860403     C*--- CARICAMENTO RIGHE ORDINI 'C' -------------------*
048100860403     C     MOFTRG        IFEQ      'C'                                          *
048200860403     C*---------- ESCLUSIONE RIGA:                         *
048300870105     C     CODART        CABNE     NOVE          TAG002                         *
048400860403     C     CODMAG        CABNE     '9999999'     TAG002                         *
048500860403     C*----------                                          *
048600860403     C     REC1          ADD       1             REC1                 40        *
048700860403     C                   Z-ADD     NREL1         SC1REC                         * NRR FISICO
048800860403     C                   Z-ADD     REC1          SC1VID                         * NRR VIDEO
048900860403     C                   MOVE      *BLANKS       SF1ART                         *
049000860403     C                   WRITE     SFL1                                         *
049100860403     C*--- CARICAMENTO COMMENTO DI RIGA :                  *
049200860403     C     MOFCOM        IFNE      *BLANKS                                      *
049300860403     C                   SETON                                            74    *
049400860403     C     REC1          ADD       1             REC1                           *
049500860403     C                   Z-ADD     NREL1         SC1REC                         * NRR FISICO
049600860403     C                   Z-ADD     REC1          SC1VID                         * NRR VIDEO
049700860403     C                   MOVE      *ZEROS        MOFNOR                         *
049800860403     C                   MOVE      *BLANKS       SF1ART                         *
049900860403     C                   MOVE      *BLANKS       MOFDMT                         *
050000860403     C                   MOVEL     MOFCOM        MOFDMT                         *
050100860403     C                   MOVE      *BLANKS       MOFTRG                         *
050200860403     C                   MOVE      *BLANKS       MOFTSR                         *
050300860403     C                   MOVE      *BLANKS       MOFUMQ                         *
050400860403     C                   MOVE      *ZEROS        MOFQTA                         *
050500860403     C                   MOVE      *BLANKS       MOFMAG                         *
050600860403     C                   MOVE      *BLANKS       MOFFAS                         *
050700150421     c                   clear                   mofdcn6
050800860403     C                   WRITE     SFL1                                         *
050900860403     C                   SETOFF                                           74    *
051000860403     C                   END                                                    *
051100860403     C                   GOTO      TAG002                                       *
051200860403     C                   END                                                    *
051300000000     C*--- CARICAMENTO RIGA DI COMMENTO -------------------*
051400860403     C     MOFTRG        IFGE      '8'                                          *COMM. X MAGAZZ.
051500860403     C     MOFTRG        IFLE      '9'                                          *COMM. GENERICO
051600860403     C*----------  COMMENTO DI ARTICOLO:                   *
051700860403     C     MOFTRG        COMP      '8'                                    61    *COMM. X MAGAZZ.
051800000000     C   61MOFMAT        CABNE     SAVMAT        TAG002                         *
051900860403     C*----------  COMMENTO GENERICO :                     *
052000860403     C  N40MOFTRG        CABEQ     '9'           TAG002                         *COMM. GENERICO
052100860403     C*----------
052200000000     C                   SETON                                            74    *
052300000000     C     REC1          ADD       1             REC1                           *
052400000000     C                   Z-ADD     NREL1         SC1REC                         * NRR FISICO
052500000000     C                   Z-ADD     REC1          SC1VID                         * NRR VIDEO
052600000000     C                   MOVE      *ZEROS        MOFNOR                         *
052700000000     C                   MOVE      *BLANKS       SF1ART                         *
052800860403     C                   MOVE      *BLANKS       MOFTRG                         *
052900860403     C                   MOVE      *BLANKS       MOFTSR                         *
053000000000     C                   MOVE      *BLANKS       MOFUMQ                         *
053100000000     C                   MOVE      *ZEROS        MOFQTA                         *
053200000000     C                   MOVE      *BLANKS       MOFMAG                         *
053300860403     C                   MOVE      *BLANKS       MOFFAS                         *
053400150421     c                   clear                   mofdcn6
053500000000     C                   WRITE     SFL1                                         *
053600000000     C                   SETOFF                                           74    *
053700000000     C*--- CARICAMENTO COMMENTI DI RIGA X RIGHE 6 8 9 -----*
053800000000     C     MOFCOM        IFNE      *BLANKS                                      *
053900000000     C                   SETON                                            74    *
054000000000     C     REC1          ADD       1             REC1                           *
054100000000     C                   Z-ADD     NREL1         SC1REC                         * NRR FISICO
054200000000     C                   Z-ADD     REC1          SC1VID                         * NRR VIDEO
054300000000     C                   MOVE      *ZEROS        MOFNOR                         *
054400000000     C                   MOVE      *BLANKS       SF1ART                         *
054500860403     C                   MOVE      *BLANKS       MOFDMT                         *
054600860403     C                   MOVEL     MOFCOM        MOFDMT                         *
054700860403     C                   MOVE      *BLANKS       MOFTRG                         *
054800860403     C                   MOVE      *BLANKS       MOFTSR                         *
054900000000     C                   MOVE      *BLANKS       MOFUMQ                         *
055000000000     C                   MOVE      *ZEROS        MOFQTA                         *
055100000000     C                   MOVE      *BLANKS       MOFMAG                         *
055200860403     C                   MOVE      *BLANKS       MOFFAS                         *
055300150421     c                   clear                   mofdcn6
055400000000     C                   WRITE     SFL1                                         *
055500000000     C                   SETOFF                                           74    *
055600000000     C                   END                                                    *
055700000000     C                   END                                                    *
055800000000     C                   END                                                    *
055900860403     C                   GOTO      TAG002                                       *
056000000000     C*--- LETTURA NUMERO MOVIMENTO DA AREA DATI ----------*
056100000000     C     DTAARA        TAG                                                    *
056200000000     C*          -------------                             *
056300000000     C                   Z-ADD     REC1          RECT                     97    *
056400860403     C  N40              SETON                                            97    *
056500990205     C   97              GOTO      ERRPMT                                       * NESSUN ORDINE
056600860403     C*----------                                          *
056700000000     C     SULOCK        TAG                                                    *
056800000000     C     *LOCK         IN        MPHDT1                               46      *
056900000000     C   46              GOTO      SULOCK                                       *
057000000000     C                   ADD       1             NUMDTA                         *
057100000000     C     NUMDTA        COMP      0                                      46    *
057200000000     C   46              ADD       1             NUMDTA                         *
057300990205     c                   move      datmov        dataeur
057400990205     C     *dmy          MOVE      DATaeur       DATDTA                         *
057500000000     C                   OUT       MPHDT1                                       *
057600860403     C                   Z-ADD     NUMDTA        NUMMOV            5 0    69    *
057700860403     C*---------- EMISSIONE FONDO -------------------------*
057800000000     C     TAG003        TAG                                                    *
057900000000     C*--- PULIZIA PROMPT RIGA DI FONDO                    *
058000860403     C                   Z-ADD     0             SC2REC                         * NRR FIS.FONDO
058100860403     C                   Z-ADD     0             SC2VID                         * NRR VID.FONDO
058200000000     C                   MOVE      *BLANKS       SC1MAT                         *
058300860403     C                   MOVE      *BLANKS       SC1TRG                         *
058400860403     C                   Z-ADD     0             MOFQTA                         *
058500860403     C                   MOVE      *BLANKS       SC1ART                         *
058600000000     C                   MOVE      *BLANKS       SC1DMT                         *
058700860403     C                   MOVE      *BLANKS       SC1SAC                         *
058800860403     C                   MOVE      '?  '         SC1CAU                         *
058900860403     C                   MOVE      *BLANKS       SC1UMQ                         *
059000000000     C                   Z-ADD     0             SC1QTA                         *
059100000000     C                   MOVE      *BLANKS       SC1MAG                         *
059200000000     C                   MOVE      *BLANKS       SC1CDC                         *
059300000000     C                   MOVE      *BLANKS       SC1LIS                         *
059400000000     C                   Z-ADD     0             SC1COL                         *
059500860403     C                   MOVE      *BLANKS       ERRORE                         *
059600860403     C                   Z-ADD     0             AMPPDC                         *
059700990205     C                   Z-ADD     0             PRZTRA           14 5          * PREZZO TRASP.
059800000000     C                   Z-ADD     0             SC1REC                         * NRR FIS. SFL1
059900000000     C                   Z-ADD     0             SC1VID                         * NRR VIDEO
060000910715     C                   MOVE      *BLANKS       SC1ATM                         * MATRICOLA
060100910715     C                   SETOFF                                         5069    *
060200000000     C*--- EMISSIONE RIGA DI FONDO CON ERRORI -------------*
060300000000     C     TAG004        TAG                                                    *
060400000000     C     CURSOR        DIV       12            CURSOR                         * POSIZ.CURSORE
060500000000     C                   MVR                     RESTO             5 065        *
060600000000     C     CURSOR        MULT      12            CURSOR                         *
060700000000     C   65CURSOR        ADD       1             CURSOR                         *
060800000000     C     EMISF1        TAG                                                    *
060900000000     C     RECT          COMP      *ZEROS                             7171      *
061000000000     C                   WRITE     OFN200TS                                     * TESTATA
061100000000     C                   WRITE     PULISCI                                      * PULIZIA VIDEO
061200000000     C                   EXFMT     SFL1CT                                       * SFL1CT
061300000000     C                   SETOFF                                       72        *
061400000000     C                   MOVEA     *ZEROS        *IN(75)                        *
061500910715     C                   SETOFF                                       6051      *
061600860403     C*--------------                                      *
061700990205     C   Kl              DO                                                     *
061800000000     C                   GOTO      TAG001                                       *
061900000000     C                   END                                                    *
062000000000     C*--- LEGGO RIGHE RICHIAMATE -------------------------*
062100000000     C   71              READC     SFL1                                   10    *
062200000000     C   71
062300000000     CANN10              DO                                                     *
062400000000     C     SCELTA        CABEQ     *BLANKS       EMISF1                         *
062500000000     C     SC1REC        CHAIN     ORDFIS                             99        * AIUTO !
062600000000     C   99              MOVEL     ERP(11)       AIUTO                          *
062700000000     C   99              SETON                                        80        *
062800000000     C   99              GOTO      TAG001                                       *
062900000000     C                   EXCEPT    RILORD                                       *
063000000000     C                   SETON                                            14    * HI+BL RIGA
063100000000     C     SC1VID        CHAIN     SFL1                               99        * AIUTO !
063200000000     C   99              MOVEL     ERP(11)       AIUTO                          *
063300000000     C   99              SETON                                        80        *
063400000000     C   99              GOTO      TAG001                                       *
063500000000     C                   MOVE      SCELTA        SCELTW                         *
063600000000     C                   MOVE      *BLANKS       SCELTA                         *
063700000000     C                   UPDATE    SFL1                                         *
063800000000     C                   Z-ADD     SC1VID        CURSOR               15  14    * POSIZ.CURSORE
063900910715     C*--- CONTROLLO SE LA RIGA PRESCELTA APPARTIENE AD UN *
064000910715     C*--- MATERIALE DI TIPO ATTREZZATURA -----------------*
064100990205     C     SF1ART        CHAIN     tmamp01L                           83        *
064200910715     C  N83              DO                                                     *
064300910715     C                   MOVE      '155'         TABGRU                         *
064400910715     C                   MOVE      *ZEROS        TABCOD                         *
064500910715     C                   MOVE      AMPCLM        TABCOD                         *
064600910715     C     KEYTAB        CHAIN     ANTAB00F                           78        *
064700910715     C  N78              MOVEL     TABDES        TB155                          *
064800910715     C   78              MOVE      *BLANKS       TB155                          *
064900910715     C     FLGATT        IFEQ      'A'                                          *
065000910715     C                   SETON                                        50        *
065100910715     C                   ELSE                                                   *
065200910715     C                   SETOFF                                       50        *
065300910715     C                   END                                                    *
065400910715     C                   END                                                    *
065500000000     C*--- CARICO RIGA FONDO                               *
065600860403     C                   MOVE      MOFTRG        SC1TRG                         *
065700000000     C                   MOVE      SF1ART        SC1ART                         *
065800000000     C                   MOVE      MOFMAT        SC1MAT                         *
065900000000     C                   MOVEL     MOFDMT        SC1DMT                         *
066000860403     C                   MOVE      *BLANKS       SC1SAC                         *
066100860403     C                   MOVE      OLDCAU        SC1CAU                         *
066200000000     C                   MOVE      MOFUMQ        SC1UMQ                         *
066300000000     C                   Z-ADD     MOFQTA        SC1QTA                         *
066400860403     C     MOFMAG        IFEQ      *BLANKS                                      *
066500860403     C                   MOVE      CODMAG        MOFMAG                         *
066600860403     C                   END                                                    *
066700860403     C     MOFMAG        IFEQ      '9999999'                                    *
066800860403     C                   MOVE      '?      '     MOFMAG                         *
066900860403     C                   END                                                    *
067000860403     C                   MOVE      MOFMAG        SC1MAG                         *
067100990205     C     MOFCDC        IFEQ      '99999999'                                   *
067200000000     C                   MOVE      CODCDC        SC1CDC                         *
067300000000     C                   ELSE                                                   *
067400000000     C                   MOVE      MOFCDC        SC1CDC                         *
067500000000     C                   END                                                    *
067600000000     C                   MOVE      LISTRA        SC1LIS                         *
067700000000     C                   MOVE      COLITR        SC1COL                         *
067800000000     C                   Z-ADD     SC1REC        SC2REC                         *NRR FIS.FONDO
067900000000     C                   Z-ADD     SC1VID        SC2VID                         *NRR VID.FONDO
068000860403     C                   SETON                                        0169      *
068100000000     C                   GOTO      EMISF1                                       *
068200000000     C                   END                                                    *
068300000000     C*--- SE NON SCELTA NESSUNA RIGA RIEMETTE SUBFILE ----*
068400000000     C  N01              GOTO      EMISF1                                       *
068500000000     C*--- RIGA RICHIAMATA PER ERRORE ---------------------*
068600000000     C   KA              DO                                                     *
068700000000     C     SC2VID        CHAIN     SFL1                               17        *
068800000000     C                   SETOFF                                       011415    *
068900000000     C  N17              UPDATE    SFL1                                 17      *
069000000000     C                   GOTO      TAG003                                       *
069100000000     C                   END                                                    *
069200000000     C*--- CONTROLLO RICHIESTA SALDO/ACCONTO --------------*
069300860403     C     SC1SAC        COMP      'S'                                7575      *
069400860403     C   75SC1SAC        CABNE     'A'           ERRORI               7575      *
069500860403     C     SC1SAC        IFEQ      'A'                                          *
069600860403     C     SC1QTA        CABGE     MOFQTA        ERRORI               76  76    *
069700000000     C                   END                                                    *
069800860403     C*---------- CONTROLLO CAUSALE -----------------------*
069900860403     C     SC1CAU        IFEQ      '?  '                                        *
070000860403     C                   MOVE      'MP'          PROCAU            2            *
070100860403     C                   MOVE      *BLANKS       SC1CAU                         *
070200860403     C                   MOVE      *BLANKS       DESCAU           25            *
070300860403     C                   MOVE      *BLANKS       AZICAU           15            *
070400990317     C                   CALL      'TII002R'                                    *
070500860403     C                   PARM                    KPJBA                          *
070600860403     C                   PARM                    PROCAU                         *
070700860403     C                   PARM                    SC1CAU                         *
070800860403     C                   PARM                    DESCAU                         *
070900860403     C                   PARM                    AZICAU                         *
071000860403     C                   SETON                                            78    *
071100860403     C                   GOTO      TAG004                                       *
071200860403     C                   END                                                    *
071300860403     C*----------------                                    *
071400860403     C                   MOVE      '152'         TABGRU                         *
071500860403     C                   MOVE      *ZEROS        TABCOD                         *
071600860403     C                   MOVEL     'MP'          CINQUE            5            *
071700860403     C                   MOVE      SC1CAU        CINQUE            5            *
071800860403     C                   MOVE      CINQUE        TABCOD                         *
071900860404     C     KEYTAB        CHAIN     ANTAB00F                           78        *
072000860403     C   781             CABEQ     1             ERRORI                         *
072100860403     C                   MOVEL     TABDES        TB152                          *
072200860403     C                   MOVEL     DES152        DESCAU                         *
072300860403     C                   MOVEA     AZE           AZ(1)                          *
072400860403     C                   MOVEA     AZU           AZ(6)                          *
072500860403     C                   MOVEA     AZG           AZ(11)                         *
072600860403     C     AZ(15)        COMP      '+'                                7878      *
072700860403     C                   MOVE      SC1CAU        OLDCAU                         *
072800860403     C*--- CONTROLLO QUANTITA' ----------------------------*
072900860403     C     SC1QTA        CABEQ     *ZEROS        ERRORI                 7777    *
073000860403     C*--- CONTROLLO CODICE MAGAZZINO ---------------------*
073100860403     C     SC1MAG        IFEQ      '?      '                                    *
073200860403     C                   MOVE      *BLANKS       SC1MAG                         *
073300860403     C                   MOVE      *BLANKS       DESM75                         *
073400990317     C                   CALL      'TII108R'                                    *
073500860403     C                   PARM                    SC1MAG                         *
073600860403     C                   PARM                    DESM75                         *
073700860403     C                   SETON                                            79    *
073800860403     C                   GOTO      TAG004                                       *
073900860403     C                   END                                                    *
074000860403     C*-----------                                         *
074100860403     C     SC1MAG        IFEQ      *BLANKS                                      *
074200860403     C                   MOVE      CODMAG        SC1MAG                         *
074300860403     C                   END                                                    *
074400860403     C                   MOVE      '153'         TABGRU                         *
074500860403     C                   MOVE      SC1MAG        TABCOD                         *
074600860404     C     KEYTAB        CHAIN     ANTAB00F                           79        *
074700860403     C   791             CABEQ     1             ERRORI                         *
074800860403     C*--- CONTROLLO C.D.C. -------------------------------*
074900860403     C     SC1CDC        IFEQ      *BLANKS                                      *
075000860403     C                   MOVE      CODCDC        SC1CDC                         *
075100860403     C                   END                                                    *
075200990205     C     SC1CDC        IFEQ      '99999999'                                   *
075300990205     C                   MOVE      '?       '    SC1CDC                         *
075400860403     C                   END                                                    *
075500990205     C     SC1CDC        IFEQ      '?       '                                   *
075600860403     C                   MOVE      *ZEROS        CCOSTO                         *
075700860403     C                   MOVE      *BLANKS       DESL48           48            *
075800990317     C                   CALL      'TII102R'                                    *
075900860403     C                   PARM                    CCOSTO                         *
076000860403     C                   PARM                    DESL48                         *
076100860403     C                   MOVE      CCOSTO        SC1CDC                         *
076200860403     C                   SETON                                            80    *
076300860403     C                   GOTO      TAG004                                       *
076400860403     C*------------                                        *
076500860403     C                   ELSE                                                   *
076600990205     C     SC1CDC        CABEQ     *blank        ERRORI                   80    *
076700860528RIC  C                   MOVE      SC1CDC        CCOSTO                         *
076800990205RIC  C     KEYCDC        CHAIN     anent01l                           80        *
076900990205     c   80              goto      errori
077000990205     c                   end
077100860403     C*--- CONTROLLO LISTINO TRASPORTI SE RICHIESTO -------*
077200860403     C     SC1TRG        CABEQ     'C'           NONLIS                         *
077300860403     C     SC1LIS        CABEQ     '999'         NONLIS                         *
077400860403     C     SC1LIS        IFEQ      '?      '                                    *
077500860403     C                   MOVE      *BLANKS       SC1LIS                         *
077600860403     C                   MOVE      *BLANKS       DESL75                         *
077700990317     C                   CALL      'TII116R'                                    *
077800860403     C                   PARM                    SC1LIS                         *
077900860403     C                   PARM                    DESL75                         *
078000860403     C                   SETON                                            81    *
078100860403     C                   GOTO      TAG004                                       *
078200860403     C                   END                                                    *
078300860403     C                   MOVE      '161'         TABGRU                         *
078400860403     C                   MOVE      *ZEROS        TABCOD                         *
078500860403     C                   MOVE      SC1LIS        TABCOD                         *
078600860404     C     KEYTAB        CHAIN     ANTAB00F                           81        *
078700860403     C   81              GOTO      ERRORI                                       *
078800860403     C*----------- INTERR.LISTINO ONERI -------------------*
078900860403     C     SC1COL        IFEQ      9                                            *
079000860403     C                   MOVEL     KPJBU         KPJBUW                         *
079100860403     C                   MOVE      SC1LIS        LIS8A0                         *
079200860403     C                   MOVE      'XXX'         CLA8A0                         *
079300860403     C                   MOVE      'XXXXX'       SOT8A0                         *
079400860403     C                   MOVE      'XX'          CAT8A0                         *
079500860403     C                   MOVE      MOFMAT        ART8A0                         *
079600860403     C                   MOVE      'R'           RIC8A0                         *
079700860403     C                   MOVEL     PAR8A0        KPJBU                          *
079800990317     C                   CALL      'TII005R'                                    *
079900860403     C                   PARM                    KPJBA                          *
080000860403     C                   MOVEL     KPJBUW        KPJBU                          *
080100860403     C                   SETON                                            81    *
080200860403     C                   GOTO      TAG004                                       *
080300860403     C                   END                                                    *
080400990205     C     KEYLIS        CHAIN     tmlmp01L                           82        *
080500860403     C   821             CABEQ     1             ERRORI                         *
080600860403     C     SC1COL        IFEQ      1                                            *
080700860403     C                   Z-ADD     LMPVP1        PRZTRA                         *
080800860403     C                   END                                                    *
080900860403     C     SC1COL        IFEQ      2                                            *
081000860403     C                   Z-ADD     LMPVP2        PRZTRA                         *
081100860403     C                   END                                                    *
081200860403     C     SC1COL        IFEQ      3                                            *
081300860403     C                   Z-ADD     LMPVP3        PRZTRA                         *
081400860403     C                   END                                                    *
081500860403     C     SC1COL        IFEQ      4                                            *
081600860403     C                   Z-ADD     LMPVP4        PRZTRA                         *
081700860403     C                   END                                                    *
081800860403     C     SC1COL        IFEQ      5                                            *
081900860403     C                   Z-ADD     LMPVP5        PRZTRA                         *
082000860403     C                   END                                                    *
082100860403     C     PRZTRA        CABEQ     *ZEROS        ERRORI                   82    *
082200860403     C     NONLIS        TAG                                                    *
082300990205     C*--- REPERISCO DATI ANAGRAFICI DA tmamp01L ----------*
082400860403     C     SC1TRG        IFNE      'C'                                          *
082500990205     C     KEYDEL        CHAIN     tmamp01L                           83        *
082600860403     C   831             CABEQ     1             ERRORI                         *
082700860403     C                   ELSE                                                   *
082800860403     C                   Z-ADD     0             AMPPDC                         *
082900860403     C                   END                                                    *
083000000000     C*--- CONTROLLO DELTA CONSEGNA X ENTRATA A SALDO -----*
083100000000     C  N77              DO                                                     *
083200860403     C     FORZAT        CABEQ     'S'           ERRORI                         * QUANT.FORZATA
083300860403     C     SC1SAC        CABNE     'S'           ERRORI                         * NON E' SALDO
083400860403     C     MOFQTA        CABEQ     SC1QTA        ERRORI                         * QUANT.UGUALI
083500860403     C*----------                                          *
083600860403     C     SC1TRG        IFNE      'C'                                          *
083700990205     C     KEYDEL        CHAIN     tmamp01L                           83        *
083800860403     C   831             CABEQ     1             ERRORI                         *
083900860403     C                   END                                                    * RIGA 'C'
084000860403     C*----------                                          *
084100860403     C     AMPPDC        CABEQ     *ZEROS        ERRORI                   83    * DELTA ZERO
084200000000     C     AMPPDC        DIV       100           PERSCO            5 4          *
084300000000     C     MOFQTA        MULT(H)   PERSCO        SCOSTA            9 2          *
084400000000     C     MOFQTA        ADD       SCOSTA        QTAPIU            9 2          *
084500860403     C     SC1QTA        CABGT     QTAPIU        ERRORI               83        * ENTR.TROPPO
084600000000     C     MOFQTA        SUB       SCOSTA        QTAMEN            9 2          *
084700860403     C     SC1QTA        CABLT     QTAMEN        ERRORI                 83      * ENTR.POCO
084800000000     C                   END                                                    *
084900000000     C*--- CONTROLLO PRESENZA ERRORI ----------------------*
085000000000     C     ERRORI        TAG                                                    *
085100860403     C     *IN(83)       COMP      '1'                                    60    * ABILIT.FORZAT
085200000000     C     75            DO        83            X                              *
085300000000     C     *IN(X)        COMP      '1'                                    72    *
085400000000     C  N72              END                                                    *
085500000000     C   72              DO                                                     * ERRORI
085600000000     C                   SETOFF                                       15        *
085700000000     C     X             SUB       74            IN                2 0          *
085800000000     C                   MOVEA     ERR(IN)       ERRORE                         *
085900000000     C                   GOTO      TAG004                                       *
086000000000     C                   END                                                    *
086100000000     C*--- OPERAZIONI SU ORDINI / SALDI / MOVIMENTI -------*
086200000000     C*--- RICERCA RIGA FILE E RILASCIO RECORD ------------*
086300000000     C     SC2REC        CHAIN     ORDFIS                             99        * AIUTO !
086400000000     C   99              MOVEL     ERP(11)       AIUTO                          *
086500000000     C   99              SETON                                        80        *
086600000000     C   99              GOTO      TAG001                                       *
086700000000     C                   EXCEPT    RILORD                                       *
086800860403     C     SC1TRG        CABEQ     'C'           RICDT2                         * RIGA 'C'
086900000000     C*--- TOLGO QUANTITA' ORDINE/PRODUZIONE/IMPEGNATA-----*
087000000000     C     MOFQTA        MULT      -1            MOFQTA                         *
087100000000     C                   MOVE      MOFMAG        MAGAZZ                         * MAG.VECCH.
087200000000     C                   MOVE      ANNCOM        ANCOMP                         * ANN.VECCH.
087300000000     C                   SETOFF                                       38        *
087400000000     C                   EXSR      AGGSAL                                       *
087500000000     C*--- AGGIORNA MOVIMENTO MAGAZZINO -------------------*
087600850619     C                   MOVE      'Q'           MMPATT                         *ATTIVITA'
087700990205     C                   MOVE      xsccut        MMPUTE                         *UTENTE
087800990205     C                   Z-ADD     woggig        MMPDMN                         *DATA MANUT.
087900000000     C                   MOVE      SC1MAG        MMPMAG                         *MAGAZZINO
088000000000     C                   Z-ADD     ANNCOM        MMPAAC                         *ANNO
088100990205     C     *dmy          move      DATDTA        DATAeur                        *
088200990205     C                   move      dataeur       dataiso                        *
088300990205     C                   move      DATAIso       MMPDMV                         *DATA MOVIM.
088400000000     C                   Z-ADD     NUMMOV        MMPNMV                         *NUM. MOVIM.
088500990205     C                   move      DATDOC        DATAeur                        *
088600990205     C                   move      dataeur       dataiso                        *
088700990205     C                   move      DATAIso       MMPDDC                         *DATA DOCUM.
088800000000     C                   Z-ADD     NUMDOC        MMPNDC                         *NUM. DOCUM.
088900000000     C                   MOVE      SC1CAU        MMPCAU                         *CAUSALE
089000000000     C                   MOVEA     AZ            MMPOPC                         *AZIONI
089100000000     C                   MOVE      SC1CDC        MMPCDC                         *C/COSTO
089200000000     C                   MOVE      MOFMAT        MMPMAT                         *PRODOTTO
089300000000     C                   MOVE      AMPUMA        MMPUMA                         *U.M.ANAG.
089400000000     C     SC1QTA        MULT      AMPFCQ        MMPQTA                         *QUANTITA' ANAG
089500860403     C*--- CALCOLO PREZZO SCONTATO ------------------------*
089600990205     C                   Z-ADD     MOFPRZ        IMP              14 5          *
089700990205     C                   Z-ADD     0             SCONTO           14 5          *
089800860206     C     IMP           MULT      MOFSC1        SCONTO                         *
089900860206     C     SCONTO        DIV       100           SCONTO                         *
090000860206     C     IMP           SUB       SCONTO        IMP                            *
090100870202     C     IMP           MULT      MOFSC2        SCONTO                         *
090200870202     C     SCONTO        DIV       100           SCONTO                         *
090300870202     C     IMP           SUB       SCONTO        IMP                            *
090400860206     C                   Z-ADD     0             SCONTO                         *
090500870202     C     IMP           MULT      MOFSC3        SCONTO                         *
090600860206     C     SCONTO        DIV       100           SCONTO                         *
090700860528     C     IMP           SUB       SCONTO        MMPPRZ                         *
090800000000     C     MMPPRZ        MULT      MOFCMB        MMPPRZ                         *PREZZO IN LIRE
090900000000     C     MMPPRZ        ADD       PRZTRA        MMPPRZ                         *PREZZO + TRASP
091000860528     C     MMPPRZ        DIV       AMPFCQ        MMPPRZ                         *PREZZO IN UMA
091100010725     c                   if        §gedcn <> *blank
091200010725     c                   move      §gedcn        mmpval
091300010725     c                   else
091400990316     c                   move      xscdiv        mmpval
091500010725     c                   end
091600990316     c                   z-add     1             mmpcmb
091700000000     C                   MOVE      SC1MAG        MMPMCP                         *MAG.C/P.
091800000000     C                   MOVEL     TIG153        MMPFGQ                         *MOV.A QUANTITA
091900000000     C                   MOVE      TIG153        MMPFGV                         *MOV.A VALORE
092000000000     C                   MOVE      TIM153        MMPFGT                         *TIPO MAGAZZINO
092100860403     C*-----------RICERCA LEGAME RIGA ---------------------*
092200860403     C     RICDT2        TAG                                                    *
092300000000     C     *LOCK         IN        OFNDT2                               46      *
092400860403     C   46              GOTO      RICDT2                                       *
092500000000     C                   ADD       1             NUMCRG                         *
092600000000     C     NUMCRG        COMP      0                                      46    *
092700000000     C   46              ADD       1             NUMCRG                         *
092800000000     C                   OUT       OFNDT2                                       *
092900860403     C*--- STAMPA BOLLETTINO DI ENTRATA MATERIALI ---------*
093000990315     C                   MOVEL     NOMFOR        STAFOR           25            *
093100860403     C                   EXSR      RUTSTA                                       *
093200860403     C     SC1TRG        CABEQ     'C'           ACCTAG                         * RIGA 'C'
093300860403     C*-----------  SCRIVE MOVIMENTO MAGAZZINO ------------*
093400860403     C                   Z-ADD     NUMCRG        MMPCRG                         *NUMERO RIGA OR
093500990205     C                   WRITE     tmmmp000                                     *WRITE
093600860403     C*----------- AGGIORNA SALDI MAGAZZINO ---------------*
093700000000     C                   MOVE      SC1MAG        MAGAZZ                         * MAG.ENTR.
093800000000     C                   MOVE      ANNCOM        ANCOMP                         * ANNO ENTR.
093900000000     C                   SETON                                        38        *
094000000000     C                   EXSR      AGGSAL                                       *
094100000000     C                   SETOFF                                       38        *
094200000000     C*--- ENTRATA IN ACCONTO -----------------------------*
094300860403     C     ACCTAG        TAG                                                    *
094400860403     C     SC1SAC        IFEQ      'A'                                          *
094500000000     C     SC2REC        CHAIN     ORDFIS                             99        * AIUTO !
094600000000     C   99              MOVEL     ERP(11)       AIUTO                          *
094700000000     C   99              SETON                                        80        *
094800000000     C   99              GOTO      TAG001                                       *
094900000000     C                   MOVE      MOFTSR        SAVTSR            1            *
095000000000     C     MOFQTA        SUB       SC1QTA        QUARES           11 2          * QUANT.RESIDUA
095100000000     C*--- CORREGGE RIGA IN RIGA DI RESIDUO                *
095200990205     C                   move      xsccut        MOFUTE                         *
095300990205     C                   Z-ADD     woggig        MOFDMN                         *
095400000000     C                   MOVE      QUARES        MOFQTA                         *
095500000000     C                   MOVE      'R'           MOFTSR                         *
095600000000     C                   SETOFF                                           39    *
095700000000     C                   EXSR      UPORD                                        *
095800860403     C*--- AGG. QUANT. ORD. /PROD. /IMP.-------------------*
095900860403     C     SC1TRG        IFNE      'C'                                          * RIGA 'C'
096000000000     C                   MOVE      MOFMAG        MAGAZZ                         * MAG.
096100000000     C                   MOVE      ANNCOM        ANCOMP                         * ANNO
096200000000     C                   SETOFF                                           38    *
096300000000     C                   EXSR      AGGSAL                                       *
096400860403     C                   END                                                    *
096500860403     C*---------- BOLLETTAZIONE RIGA ----------------------*
096600990205     C                   move      xsccut        MOFUTE                         *
096700990205     C                   Z-ADD     woggig        MOFDMN                         *
096800000000     C                   MOVE      'BB'          MOFFAS                         *
096900000000     C                   MOVE      SAVTSR        MOFTSR                         *
097000000000     C                   Z-ADD     SC1QTA        MOFQTA                         *
097100000000     C                   Z-ADD     NUMCRG        MOFCRG                         *
097200990205     C                   move      DATDOC        DATAeur                        *
097300990205     C                   move      dataeur       dataiso                        *
097400990205     C                   move      DATAIso       MOFDBL                         *DATA BOLLA
097500000000     C                   Z-ADD     NUMDOC        MOFNBL                         *NUM. BOLLA
097600000000     C                   MOVE      SC1MAG        MOFMAG                         *
097700000000     C                   MOVE      SC1CDC        MOFCDC                         *
097800000000     C                   SETON                                            39    *
097900170313     c                   exsr      GesStime
098000000000     C                   EXSR      UPORD                                        *
098100000000     C                   SETOFF                                           39    *
098200000000     C                   END                                                    *
098300000000     C*--- ENTRATA A SALDO --------------------------------*
098400860403     C     SC1SAC        IFEQ      'S'                                          *
098500000000     C     SC2REC        CHAIN     ORDFIS                             99        * AIUTO !
098600000000     C   99              MOVEL     ERP(11)       AIUTO                          *
098700000000     C   99              SETON                                        80        *
098800000000     C   99              GOTO      TAG001                                       *
098900000000     C*--- BOLLETTAZ.RIGA                                  *
099000990205     C                   move      xsccut        MOFUTE                         *
099100990205     C                   Z-ADD     woggig        MOFDMN                         *
099200000000     C                   MOVE      'BB'          MOFFAS                         *
099300000000     C                   Z-ADD     SC1QTA        MOFQTA                         *
099400000000     C                   Z-ADD     NUMCRG        MOFCRG                         *
099500990205     C                   move      DATDOC        DATAeur                        *
099600990205     C                   move      dataeur       dataiso                        *
099700990205     C                   move      dataiso       MOFDBL                         *DATA BOLLA
099800000000     C                   Z-ADD     NUMDOC        MOFNBL                         *NUM. BOLLA
099900000000     C                   MOVE      SC1MAG        MOFMAG                         *
100000000000     C                   MOVE      SC1CDC        MOFCDC                         *
100100000000     C                   SETOFF                                           39    *
100200170313     c                   exsr      GesStime
100300000000     C                   EXSR      UPORD                                        *
100400000000     C                   END                                                    *
100500000000     C*--- RICARICA SUBFILE -------------------------------*
100600000000     C                   SETOFF                                       0115      *
100700000000     C                   GOTO      SUCAR                                        *
100800000000     C*--- FINE LAVORO ------------------------------------*
100900000000     C     FINE          TAG                                                    *
101000000000     C*          -------------                             *
101100000000     C*--- STAMPA CHIUSURA BOLLETTINO ---------------------*
101200000000     C   09              EXCEPT    RIGFIN                                       *
101300000000     C*--- ERRORE X DITTA INESISTENTE ---------------------*
101400000000     C     FINERR        TAG                                                    *
101500000000     C   90              MOVEL     'KPJB207'     KMSMN                          *
101600000000     C                   SETON                                            LR    *
101700000000     C**--- AGGIORNAMENTO RIGHE ORDINI -------------------**
101800000000     C***------------------------------------------------***
101900000000     CSR   UPORD         BEGSR                                                  *
102000000000     C  N39              UPDATE    ORDFIS                                       * UPDATE
102100000000     C   39              WRITE     ORDFIS                                       * WRITE
102200000000     C                   SETOFF                                           39    *
102300000000     C                   MOVE      *BLANKS       SCELTW            1            *
102400000000     CSR                 ENDSR                                                  *
102500000000     C**--- AGGIORNAMENTO CAMPI SU SALDI M.P. ------------**
102600000000     C***------------------------------------------------***
102700000000     CSR   AGGSAL        BEGSR                                                  *
102800000000     C*---                                                 *
102900990205     C     KEYSAL        CHAIN     tmsmp01L                           39        *
103000000000     C   39              DO                                                     *
103100000000     C                   MOVE      ANCOMP        SMPAAC                         *
103200000000     C                   MOVE      MAGAZZ        SMPMAG                         *
103300000000     C                   MOVE      MOFMAT        SMPMAT                         *
103400000000     C                   MOVE      AMPUMA        SMPUMA                         *
103500000000     C                   Z-ADD     0             SMPPLF                         *
103600000000     C                   Z-ADD     0             SMPPLV                         *
103700000000     C                   Z-ADD     0             SMPQGI                         *
103800000000     C                   Z-ADD     0             SMPQOR                         *
103900000000     C                   Z-ADD     0             SMPQPR                         *
104000000000     C                   Z-ADD     0             SMPQIM                         *
104100870518     C                   Z-ADD     0             SMPQIP                         *
104200000000     C                   Z-ADD     0             SMPQGF                         *
104300000000     C                   Z-ADD     0             SMPQE1                         *
104400000000     C                   Z-ADD     0             SMPQE2                         *
104500000000     C                   Z-ADD     0             SMPQE3                         *
104600000000     C                   Z-ADD     0             SMPQE4                         *
104700000000     C                   Z-ADD     0             SMPQE5                         *
104800000000     C                   Z-ADD     0             SMPQU1                         *
104900000000     C                   Z-ADD     0             SMPQU2                         *
105000000000     C                   Z-ADD     0             SMPQU3                         *
105100000000     C                   Z-ADD     0             SMPQU4                         *
105200000000     C                   Z-ADD     0             SMPQU5                         *
105300990205     C                   Z-ADD     woggig        SMPDMS                         *
105400000000     C                   Z-ADD     0             SMPNMS                         *
105500000000     C                   Z-ADD     0             SMPPMC                         *
105600000000     C                   MOVE      *BLANKS       SMPFLG                         *
105700000000     C                   END                                                    *
105800860403     C*--- TOLGO QUANTITA' ORDINE/PROD./IMPEGN ------------*
105900000000     C  N38              DO                                                     *
106000000000     C                   Z-ADD     0             QTSALD           11 2          *
106100000000     C     MOFQTA        MULT(H)   AMPFCQ        QTSALD                         *
106200000000     C     MOFFAS        IFEQ      'OR'                                         *
106300000000     C  N39SMPQOR        ADD       QTSALD        SMPQOR                         *
106400000000     C   39              Z-ADD     QTSALD        SMPQOR                         *
106500000000     C                   END                                                    *
106600000000     C     MOFFAS        IFEQ      'CL'                                         *
106700000000     C  N39SMPQPR        ADD       QTSALD        SMPQPR                         *
106800000000     C   39              Z-ADD     QTSALD        SMPQPR                         *
106900000000     C                   END                                                    *
107000000000     C     MOFFAS        IFEQ      'IM'                                         *
107100000000     C  N39SMPQIM        ADD       QTSALD        SMPQIM                         *
107200000000     C   39              Z-ADD     QTSALD        SMPQIM                         *
107300000000     C                   END                                                    *
107400000000     C                   END                                                    *
107500000000     C*--- AGGIUNGO QUANTITA' ENTRATA                      *
107600000000     C   38              DO                                                     *
107700000000     C*--- CONVERSIONE QUANTITA' :                         *
107800000000     C     SC1QTA        MULT(H)   AMPFCQ        QTAANA           15 5          *
107900000000     C*--- AGGIORNAMENTO ENTRATA                           *
108000000000     C     1             DO        5             Y                 2 0          *
108100000000     C     AZ(Y)         COMP      '+'                                    37    *
108200000000     C     AZ(Y)         COMP      '-'                                    36    *
108300000000     C   37QTK(Y)        ADD       QTAANA        QTK(Y)                         *
108400000000     C   36QTK(Y)        SUB       QTAANA        QTK(Y)                         *
108500000000     C                   END                                                    *
108600000000     C*---                                                 *
108700000000     C     6             DO        10            Y                 2 0          *
108800000000     C     AZ(Y)         COMP      '+'                                    37    *
108900000000     C     AZ(Y)         COMP      '-'                                    36    *
109000000000     C   37QTK(Y)        ADD       QTAANA        QTK(Y)                         *
109100000000     C   36QTK(Y)        SUB       QTAANA        QTK(Y)                         *
109200000000     C                   END                                                    *
109300000000     C*--------- AGGIORNAMENTO GIACENZA                    *
109400000000     C     AZ(15)        COMP      '+'                                    37    *
109500000000     C   37SMPQGF        ADD       QTAANA        SMPQGF                         *GIACENZA +
109600000000     C                   END                                                    *
109700000000     C**--- AGGIORNO SALDI M.P. --------------------------**
109800000000     C                   MOVE      ' '           SMPATT                         *
109900990205     C                   move      xsccut        SMPUTE                         *
110000990205     C                   Z-ADD     woggig        SMPDMN                         *
110100990205     C  N39              UPDATE    tmsmp000                                     *
110200990205     C   39              WRITE     tmsmp000                                     *
110300000000     CSR                 ENDSR                                                  *
110400000000     C***------------------------------------------------***
110500000000     C*--- ROUTINE EMISSIONE STAMPA -----------------------*
110600000000     C***------------------------------------------------***
110700000000     CSR   RUTSTA        BEGSR                                                  *
110800860403     C                   OPEN      OFPRTSTD                             47      *
110900860403     C                   SETON                                          09      *
111000000000     C     PAGE1         IFGE      58                                           *
111100000000     C     PAGE1         IFEQ      58                                           *
111200000000     C                   EXCEPT    RIGFIN                                       *
111300000000     C                   END                                                    *
111400860403     C*--------- RICERCA NUMERO BOLLETTINO :               *
111500000000     C     SUNBOL        TAG                                                    *
111600000000     C     *LOCK         IN        OFNDT1                               46      *
111700000000     C   46              GOTO      SUNBOL                                       *
111800000000     C                   ADD       1             NUMBLT                         *
111900000000     C     NUMBLT        COMP      0                                      46    *
112000000000     C   46              ADD       1             NUMBLT                         *
112100990205     C                   MOVE      DATMOV        DATaeur                        *
112200990205     C     *dmy          MOVE      DATaeur       DATBLT                         *
112300000000     C                   OUT       OFNDT1                                       *
112400000000     C                   Z-ADD     NUMBLT        NBOLL             5 0          *
112500860403     C*------------                                        *
112600000000     C                   EXCEPT    RIGTES                                       *
112700000000     C                   Z-ADD     8             PAGE1                          *
112800000000     C                   END                                                    *
112900000000     C                   EXCEPT    RIGDET                                       *
113000000000     CSR                 ENDSR                                                  *
113100990205     C*----------------------------------------------------*
113200990205     C* Reperimento dati società
113300990205     C*----------------------------------------------------*
113400990205     C     REPSOC        BEGSR
113500990205     C*
113600990205     C                   CALLB     'XSOC'
113700990205     C                   PARM                    TIPXSC            6
113800990205     C                   PARM                    SOCXSC            3
113900990205     C                   PARM                    CDSXSC            9 0
114000990205     C                   PARM                    MODXSC            3
114100990205     C                   PARM      *blanks       RTNXSC            1
114200990205     C                   PARM                    XSOCDS
114300990205     C                   PARM                    KPJBA
114400990205     C*
114500990205     C                   ENDSR
121701171019     C*----------------------------------------------------*
121702171019     C* GesStime inserimento modifica dati stime
121703171019     C*----------------------------------------------------*
121704171019     C     GesStime      begsr
121705171102      * considero solo righe con entrata merce magazzino superiore al 1 novembre data CUT-OFF
121706171102     c                   if        mofdbl < 20171101
121707171102     c                   leavesr
121708171102     c                   endif
121709171019      /free
121710171019
121711171019        //ricava variabili da passare a programma
121712171019       if mofflg = *blank;
121713171019       clear idriga;
121714171019        else ;
121715171019       idriga = %int(mofflg);
121716171019       endif;
121717171019        // Per l'inserimento delle righe speciali senza articolo forziamo il codice
121718171019       if mofmat <> *blank;
121719171019        chain (mofmat) tmamp01l ;
121720171019         if %found(tmamp01l);
121721171019           articolo = 'ACQ'+ ampscm;
121722171019            else;
121723171019           articolo = 'ACQ00000';
121724171019         endif ;
121725171019        else;
121726171019          articolo = 'ACQ00000';
121727171019       endif;
121728171019       prezzo = mofprz * mofqta;
121729171019        kpjbu = IMOFDS00 ;
121730171019
121731171019           //chiamata a pgm scrittura stime
121732171020        YDRTFRJOB(kpjba:pgmname);
121733171019
121736171019         imofds00 = kpjbu;
121737171020
121738171019        //ritorno se tutto ok trovo un numero positivo in esito che indica la riga di registrazione
121739171019        //che va riportata sul file ordine per essere collegata
121740171019
121741171019       if esito > 0;
121742171019        mofflg=%char(idriga);
121743171019         else;
121744171019        esito=YDRere_GetMessaggi(nrMessaggi : idMessaggio : tipoMessaggio
121745171019              : idCampo : testoMessaggio);
121746171019        exsr  InvMail;
121747171019       endif;
121748171019
121749171019
121750171019      /end-free
121751171019     C                   ENDSR
121800170502     C*----------------------------------------------------*
121900170502     C* InvMail  invia mail segnalazione
122000170502     C*----------------------------------------------------*
122100170502     C     InvMail       begsr
122200170502     c                   move      mofdor        dataiso
122300170502     c                   move      dataiso       dataeur
122400170502     c                   move      dataeur       dataord           8 0
122500170502     C* Inizializzo variabili
122600170502      *?   SE non aggiunge o aggiorna invia mail segnalazione
122700170502     C                   movel     *blanks       wrkEml          253
122800170502     C                   movel     *blanks       wrkEmcc         253
122900170502     C                   movel     *blanks       wrkMsg         5000
123000170502     C                   movel     *blanks       wrkOgg           44
123100170502     C                   movel     *blanks       wrkesit           1
123201171115     c                   if        %subst(knsif:7:1) = 'P'
123202171115     C                   eval      wrkEml= 'Michele.malipiero@brt.it'
123203171115     c                   else
123204171115     C                   eval      wrkEml= 'Acquisti@BRT.it'
123205171115     c                   endif
123300170502
123400171025     C                   eval      wrkOgg= 'FATTURE DA RICEVERE da -
123500171025     C                              Entrata Magazzino non riuscito'
123600171025     C                   eval      wrkMsg= 'Riga Ordine Nr.  '
123700170502     C                             +  %editc(MofNor:'Z')
123800170502     C                             + '/ ' + %editc(dataord:'Y')
123900170502     c                             + ' Fornitore ' + moffor
124000170502     c                             + ' Articolo '
124100170502     C                             + '/ ' + mofmat + ' / ' + mofdmt
124101171025     C                             + '/ ' + ARTICOLO
124200170502     c                             + ' Quantità '
124300170502     C                             + '/ ' + %editc(mofqta:'2')
124400170502     c                             + ' Prezzo '
124500170502     C                             + '/ ' + %editc(mofprz:'2') +':/N' +
124600170502     C                             %editc(Nrmessaggi:'Z') + ' '
124700170502     C                             + testomessaggio(1) +':/N'
124800170502     C                             + testomessaggio(2) +':/N'
124900170502     C                             + testomessaggio(3) +':/N'
125000170502     C*
125100170502     C                   call(e)   'TIS701C1'
125200170502     C                   parm                    wrkEml
125300170502     C                   parm                    wrkEmcc
125400170502     C                   parm                    wrkOgg
125500170502     C                   parm                    wrkMsg
125600170502     C                   parm                    wrkesit
125700170502     c                   endsr
125800860403     O*------------------------------------ EMISSIONE TESTATA :
125900000000     OOFPRTSTD  E            RIGTES         1 01
126000000000     O*-1/8-
126100000000     O                       ST1                132
126200000000     O                       DESDIT              20
126300000000     O          E            RIGTES         1
126400000000     O                       ST2                132
126500000000     O                       NBOLL         Z     43
126600990205     O                       DATMOV              99 '  /  /    '
126700000000     O          E            RIGTES         2
126800000000     O                       ST3                132
126900000000     O                       KNMUS              132
127000000000     O          E            RIGTES         1
127100000000     O                       ST4                132
127200000000     O          E            RIGTES         1
127300000000     O                       ST5                132
127400000000     O          E            RIGTES         1
127500000000     O                       ST4                132
127600000000     O          E            RIGTES         1
127700000000     O                       ST6                132
127800000000     O*------------------------------------ EMISSIONE DETTAGLIO :
127900000000     O*-9/60-
128000000000     O          E            RIGDET         1
128100000000     O                       PAGE1                3
128200000000     O                       ST6                132
128300871008     O                       MOFMAT              13
128400871008     O                       MOFDMT              44
128500000000     O                       SC1QTA        4     57
128600860403     O                       SC1SAC              59
128700000000     O                       SC1MAG              67
128800990315     O                       STAFOR              93
128900990315     O                       MOFNOR        Z    101
129000990315     O                       NUMDOC        Z    109
129100990205     O                       DATDOC             120 '  /  /    '
129200990205     O                       MOFQTA        4    132
129300000000     O*------------------------------------ EMISSIONE FINE MODULO :
129400000000     O          E            RIGFIN         1
129500000000     O                       ST6                132
129600000000     O          E            RIGFIN         2
129700000000     O                       ST4                132
129800000000     O          E            RIGFIN         1
129900000000     O                       ST7                132
130000860403     O/EJECT
130100860403     OORDFIS    E            RILORD
130200860403     O*--- RILASCIO RIGA ORDINI ----*
130300880114**    ERP  SCHIERA ERRORI PROMPT -----------------------------------------------
130400860403*- Cod. Listino Oneri Aggiuntivi ERRATO! ERP,01                               90
130500860403*- Cod. FORNITORE  ERRATO !!!            ERP,02                               91
130600860403*- Cod. PRODOTTO   ERRATO !!!            ERP,03                               92
130700860403*- Ordine non Trovato     !!!            ERP,04                               93
130800860403*- Cod. MAGAZZINO  ERRATO !!!            ERP,05                               94
130900860403*- Cod. C/COSTO    ERRATO !!!            ERP,06                               95
131000860403*- DOCUMENTO (Numero o Data) ERRATO !!!  ERP,07                               96
131100860403*- Non Trovati Ordini Corrispondenti !!! ERP,08                               97
131200860403*- FORNITORE/ORDINE  Bloccato da ------> ERP,09                               98
131300860403*- Data Movimento non Valida (Giornale)! ERP,10                               99
131400860403****- ERRORE FILE. CONTATTARE CED. -**** ERP,11                               99
131500860403(9.9 = Tutti)                            ERP,12                               99
131600860404*- Anno di Competenza Magazzino ERRATO ! ERP,13                               99
131700860403**     ERR   SCHIERA ERRORI SUBFILE---------------------------------------------
131800860403*- Specificare Consegna a SALDO/ACCONTO  ERR,1                                75
131900860403*- Acconto non Ammesso, Usare  SALDO  -* ERR,2                                76
132000860403*- Inserire Quantità !!!              -* ERR,3                                77
132100860403*- Cod. CAUSALE Movimento ERRATA !!!  -* ERR,4                                78
132200860403*- Cod. MAGAZZINO ERRATO  !!!         -* ERR,5                                79
132300860403*- Cod. C/COSTO   ERRATO  !!!         -* ERR,6                                80
132400860403*- Cod. LISTINO   ERRATO  !!!         -* ERR,7                                81
132500860403*- Non  Trovato Prezzo di Listino !!! -* ERR,8                                82
132600860403*-Max/Min. Scostam. da Ordine Ammesso--> ERR,9                                83
132700910715*- Codice matricola errato !!!!!!     -* ERR,10                               51
132800000000**   ST1
132900000000xxxxxxxxxxxxxxxxxxxx ***------------------------------------------  ST1
133000000000-------------------------------------***                            ST1
133100000000**   ST2
133200000000                     **  Bollettino N.xxxxx       ENTRATA  MATERIA  ST2
133300990317LI  DA FORNITORI  DEL  xx/xx/xx       **        Pgm...: TII403R     ST2
133400000000**   ST3
133500000000                     ***------------------------------------------  ST3
133600000000-------------------------------------***        Utente: xxxxxxxxxx  ST3
133700000000**   ST4
133800990205:------------:------------------------------:--------------:------  ST4
133900990315-:-------------------------:-------:------------------:----------:  ST4
134000000000**   ST5
134100871008: Articolo   :Descrizione                   :   Quantita'  :Magazz  ST5
134200990315.:      F O R N I T O R E  : N.Ord :  N.Bolla   Del   :Qta'Ordine:  ST5
134300000000**   ST6
134400871008:            :                              :              :        ST6
134500990315 :                         :       :                  :          :  ST6
134600000000**   ST7
134700000000  Magazzino..........................                               ST7
134800000000                        Ufficio Acquisti........................    ST7
134900000000**   SCM   SCHIERA PER COMANDO OVRPTRF
135000880114OVRPRTF FILE(OFPRTSTD) COPIES(1) HOLD(*YES)
