000100990126     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990126     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     F*-OCO150R-*-OF/38-*--------------------------------------------*
000400000000     F*-----------------*                                            *
000500880915     F*         INTERROGAZIONE PTF. O.C. PER DATA ORDINE
000600000000     F*         ------------------------------------------           *
000700000000     F*                                                              *
000800990126     F*         FILE : OCO150D  - tmclp01L - tmamp01L - ANTAB00F -   *
000900000000     F*                                                              *
001000990317     F*         PGM.:  TII119R - RICERCA ALFABETICA FORNITORI        *
001100990317     F*                tii104R - RICERCA ALFABETICA MATERIE PRIME    *
001200990317     F*                TII114R - RICERCA GRUPPO   MERCEOLOGICO       *
001300990317     F*                TII115R - RICERCA SETTORE  MERCEOLOGICO       *
001400990317     F*                TII110R - RICERCA CLASSE   MERCEOLOGICA       *
001500990317     F*                TII111R - RICERCA S/CLASSE MERCEOLOGICA       *
001600990317     F*                tii108R - RICERCA MAGAZZINI                   *
001700990317     F*                TII300R - REVISIONE ORDINI                    *
001800990317     F*                TII301R1- INTERROGAZIONE ORDINI X FORNITORE   *
001900990317     F*                TII301R2- INTERROGAZIONE ORDINI X ARTICOLO    *
002000000000     F*                RICDATE - CONTROLLO INVERSIONE DATA           *
002100000000     F*                                                              *
002200000000     F*--------------------------------------------------------------*
002300000000     F*                                                              *
002400000000     F*   "PARAMETRO DI RICHIAMO"                                    *
002500000000     F*                        F : INTERROGAZIONE X FORNITORE        *
002600000000     F*                        A : INTERROGAZIONE X ARTICOLO         *
002700000000     F*                        O : REVISIONE        ORDINE           *
002800000000     F*                        I : INTERROGAZIONE   ORDINI           *
002900000000     F*                        P : RICHIAMO PROMPT INIZIALE          *
003000000000     F*                        E : ERRORE NON ESISTONO ORDINI        *
003100000000     F*                                                              *
003200000000     F*--------------------------------------------------------------*
003300990126     Ftmclp01L  IF   E           K DISK
003400990126     Ftmamp01L  IF   E           K DISK
003500000000     FANTAB00F  IF   E           K DISK
003600990317     FTII301D   CF   E             WORKSTN
003700000000     F                                     IGNORE(SFL1)
003800000000     F                                     IGNORE(SFL1CT)
003900000000     F                                     IGNORE(SFL2)
004000000000     F                                     IGNORE(SFL2CT)
004100870514     F                                     IGNORE(OCO150FD)
004200000000     D*--------------------------------------------------*
004300000000     D ERR             S             40    DIM(14) CTDATA PERRCD(1)             * ERRORI
004400000000     D*--------------------------------------------------*
004500000000     D KPJBA         E DS
004600000000     D*--- PASSAGGIO PARAMETRI
004700990126     D  FOR150               247    254
004800990126     D  DESFOR               255    302
004900990126     D  MAT150               407    418
005000990126     D  DESMAT               311    340
005100990126     D  DAL150               303    310  0
005200990126     D  AL150                345    352  0
005300000000     D  SNV150               353    353
005400000000     D  TOR150               354    355
005500000000     D  TR1150               356    356
005600000000     D  TR2150               357    357
005700000000     D  TR3150               358    358
005800000000     D  TR4150               359    359
005900000000     D  TR5150               360    360
006000000000     D  TR6150               361    361
006100000000     D  TR7150               362    362
006200000000     D  TR8150               363    363
006300000000     D  TR9150               364    364
006400000000     D  TRG                  356    364
006500000000     D  GRM150               365    366
006600000000     D  SEM150               367    368
006700000000     D  CLM150               369    371
006800000000     D  SCM150               372    376
006900000000     D  FA1150               377    378
007000000000     D  FA2150               379    380
007100000000     D  FA3150               381    382
007200000000     D  FA4150               383    384
007300000000     D  FA5150               385    386
007400000000     D  FAS                  377    386
007500000000     D  MAG150               387    393
007600000000     D  DOR150               394    398  0
007700990126     D  NOR150               399    406  0
007800000000     D  RIC150               442    442
007900000000     D  UTE150               443    452
008000000000     D  MSGERR               453    502
008100000000     D PARAME          DS           502
008200000000     D*--- PARAMETRI X INTERROGAZIONE :
008300000000     D  PARJBA                 1    247
008400000000     D  PARJBU               247    502
008500990126     D  FORPAR               247    254
008600990126     D  DESF                 255    302
008700990126     D  MATPAR               407    418
008800000000     D  DESM                 311    340
008900990126     D  DALPAR               303    310  0
009000990126     D  ALPAR                345    352  0
009100000000     D  SNVPAR               353    353
009200000000     D  DATPAR               354    393
009300000000     D  DORPAR               394    398  0
009400990126     D  NORPAR               399    406  0
009500000000     D  STAPAR               441    441
009600000000     D  RICPAR               442    442
009700000000     D  UTEPAR               443    452
009800000000     D  MSGPAR               453    502
009900990126     D soc001        E DS                  EXTNAME(xsoc001ds)
010000990126     D xsocds          DS          1000
010100990126     d dataiso         s               d   datfmt(*iso)
010200990126     d dataeur         s               d   datfmt(*eur)
010300000000     C*----------------------------------------------------*
010400000000     C     *ENTRY        PLIST                                                  *
010500000000     C                   PARM                    KPJBA                          *
010600990126     C*---------- RICERCA DITTA :
010700990126     C                   MOVEL     'SOC001'      TIPXSC
010800990126     C                   MOVEL     *blank        SOCXSC
010900990126     C                   EXSR      REPSOC
011000990126     C     RTNXSC        IFNE      '1'
011100990126     C                   MOVEL     XSOCDS        SOC001
011200990126     C                   MOVEL     xscrgs        desdit
011300990126     C                   MOVEL     xscsoc        wsoc              3
011400990126     c                   end
011500990126     c                   time                    como14           14 0
011600990126     c                   move      como14        woggi             8 0
011700990126     c                   move      woggi         dataeur
011800990126     c                   move      dataeur       dataiso
011900990126     c                   move      dataiso       woggig            8 0
012000990126     C*                                                    *
012100000000     C*--- FILE ANAGRAFICO MATERIE PRIME :                 *
012200000000     C     KEYAMP        KLIST                                                  *
012300000000     C                   KFLD                    MAT150                         *
012400000000     C*--- FILE ANAGRAFICO FORNITORI PARTICOLARITA :       *
012500000000     C     KEYFOP        KLIST                                                  *
012600000000     C                   KFLD                    FOR150                         *
012700000000     C*--- FILE ANAGRAFICO TABELLE :                       *
012800000000     C     KEYTAB        KLIST                                                  *
012900000000     C                   KFLD                    TABGRU                         *
013000000000     C                   KFLD                    TABCOD                         *
013100000000     C*--- IMPOSTAZIONI INIZIALI SU PROMPT ----------------*
013200990126     C                   MOVEL     *BLANKS       INTER            12            *
013300990126     C                   MOVEL     '?'           INTER                          *
013400990126     c                   move      *all'9'       nove             12
013500000000     C                   Z-ADD     0             DAL                            *
013600990126     C                   Z-ADD     woggi         AL                             *
013700000000     C                   MOVE      *ZEROS        DAL150                         *
013800000000     C                   MOVE      *ZEROS        AL150                          *
013900000000     C                   MOVE      'N'           SNV150                         *
014000000000     C                   MOVE      '? '          TOR150                         *
014100000000     C                   MOVEL     '123DCIS'     TRG                            *
014200000000     C                   MOVE      '99'          GRM150                         *
014300000000     C                   MOVE      '99'          SEM150                         *
014400000000     C                   MOVE      '999'         CLM150                         *
014500000000     C                   MOVE      '99999'       SCM150                         *
014600000000     C                   MOVE      'OR'          FA1150                         *
014700870518     C                   MOVE      'AS'          FA2150                         *
014800870518     C                   MOVE      'BB'          FA3150                         *
014900870518     C                   MOVE      'FA'          FA4150                         *
015000870518     C                   MOVE      '  '          FA5150                         *
015100000000     C                   MOVE      '9999999'     MAG150                         *
015200000000     C                   MOVE      *ZEROS        DOR150                         *
015300000000     C                   MOVE      *ZEROS        NOR150                         *
015400870514     C                   WRITE     OCO150T1                                     * TESTATA
015500000000     C*----------------------------------------------------*
015600000000     C     EMISPT        TAG                                                    *
015700870514     C                   EXFMT     OCO150PT                                     * PROMPT
015800990409     C   Kc              GOTO      FINE                                         *
015900000000     C                   MOVEA     *ZEROS        *IN(1)                         *
016000000000     C                   MOVE      *BLANKS       DESTOR                         *
016100000000     C                   MOVE      *BLANKS       DESGRM                         *
016200000000     C                   MOVE      *BLANKS       DESSEM                         *
016300000000     C                   MOVE      *BLANKS       DESCLM                         *
016400000000     C                   MOVE      *BLANKS       DESSCM                         *
016500000000     C                   MOVE      *BLANKS       DESMAG                         *
016600000000     C                   MOVE      *BLANKS       RIC150                         *
016700000000     C*--- CONTROLLO SE SCELTO SIA FORNIT. CHE ARTICOLO ---*
016800000000     C     FOR150        IFEQ      *BLANKS                                      *
016900000000     C     MAT150        IFEQ      *BLANKS                                      *
017000000000     C                   SETON                                        90        *
017100000000     C   90              SETON                                        8182      *
017200000000     C   90              MOVEL     ERR(1)        ERRORE                         *
017300000000     C   90              GOTO      EMISPT                                       *
017400000000     C                   END                                                    *
017500000000     C                   END                                                    *
017600000000     C*----------------- FORNITORE ------------------------*
017700000000     C     FOR150        CABEQ     *BLANKS       RICMAT                         *
017800990126     C     FOR150        IFEQ      '?       '                                   *
017900990126     C                   MOVE      *ZEROS        FOR014            8            *
018000000000     C                   MOVEL     DESFOR        RAG014           30            *
018100990317     C                   CALL      'TII119R'                                    *
018200000000     C                   PARM                    FOR014                         *
018300000000     C                   PARM                    RAG014                         *
018400000000     C                   MOVE      FOR014        FOR150                         *
018500000000     C                   MOVE      *BLANKS       DESFOR                         *
018600000000     C                   MOVEL     RAG014        DESFOR                         *
018700990126     C     FOR150        COMP      '99999999'                             90    *
018800000000     C   90              SETON                                        81        *
018900000000     C   90              MOVEL     ERR(2)        ERRORE                         *
019000000000     C                   GOTO      EMISPT                                       *
019100000000     C                   END                                                    *
019200000000     C*------------                                        *
019300000000     C                   MOVE      *BLANKS       DESFOR                         *
019400990126     C     KEYFOP        CHAIN     tmclp01L                           90        *
019500000000     C   90              SETON                                        81        *
019600000000     C   90              MOVEL     ERR(2)        ERRORE                         *
019700000000     C   90              GOTO      EMISPT                                       *
019800000000     C                   MOVE      *BLANKS       DESFOR                         *
019900000000     C                   MOVEL     CLPRSI        DESFOR                         *
020000000000     C*----------------- ARTICOLO -------------------------*
020100000000     C     RICMAT        TAG                                                    *
020200000000     C     MAT150        CABEQ     *BLANKS       RICDAT                         *
020300871002     C     MAT150        IFEQ      INTER                                        *
020400000000     C                   MOVE      *BLANKS       MAT150                         *
020500990317     C                   CALL      'TII104R'                                    *
020600000000     C                   PARM                    MAT150                         *
020700000000     C                   PARM                    DESMAT                         *
020800000000     C     MAT150        COMP      NOVE                                   90    *
020900000000     C   90              SETON                                        82        *
021000000000     C   90              MOVEL     ERR(3)        ERRORE                         *
021100000000     C                   GOTO      EMISPT                                       *
021200000000     C                   END                                                    *
021300000000     C*------------------                                  *
021400000000     C                   MOVE      *BLANKS       DESMAT                         *
021500990126     C     KEYAMP        CHAIN     tmamp01L                           90        *
021600000000     C   90              SETON                                        82        *
021700000000     C   90              MOVEL     ERR(3)        ERRORE                         *
021800000000     C   90              GOTO      EMISPT                                       *
021900000000     C                   MOVEL     AMPDMT        DESMAT                         *
022000000000     C*--- ROVESCIO E CONTROLLO DATA LIMITE INFERIORE -----*
022100000000     C     RICDAT        TAG                                                    *
022200000000     C     DAL           IFNE      0                                            *
022300990126     c     *eur          test(d)                 dal                    90
022400990126     c                   if        *in90
022500990126     c     *dmy          test(d)                 dal                    90
022600990126     c  n90*dmy          move      dal           dataeur
022700990126     c  n90              move      dataeur       dal
022800990126     c                   else
022900990126     c                   move      dal           dataeur
023000990126     c                   end
023100990126     c   90              goto      errdat
023200990126     c                   move      dataeur       dataiso
023300990126     c                   move      dataiso       dal150
023400000000     C                   END                                                    *
023500000000     C*--- ROVESCIO E CONTROLLO DATA LIMITE SUPERIORE -----*
023600990126     C     al            IFNE      0                                            *
023700990126     c     *eur          test(d)                 al                     90
023800990126     c                   if        *in90
023900990126     c     *dmy          test(d)                 al                     90
024000990126     c  n90*dmy          move      al            dataeur
024100990126     c  n90              move      dataeur       al
024200990126     c                   else
024300990126     c                   move      al            dataeur
024400990126     c                   end
024500990126     c   90              goto      errdat
024600990126     c                   move      dataeur       dataiso
024700990126     c                   move      dataiso       al150
024800990126     C                   END                                                    *
024900000000     C*--- CONTROLLO INCROCIATO FRA DATE LIMITI -----------*
025000000000     C     DAL150        COMP      AL150                              90        *
025100000000     C     ERRDAT        TAG                                                    *
025200000000     C   90              SETON                                        83        *
025300000000     C   90              MOVEL     ERR(4)        ERRORE                         *
025400000000     C   90              GOTO      EMISPT                                       *
025500000000     C*--- RICERCA TIPO ORDINE ----------------------------*
025600000000     C     TOR150        IFNE      '99'                                         *
025700000000     C     TOR150        IFEQ      '? '                                         *
025800000000     C                   MOVE      *BLANKS       TOR150                         *
025900000000     C                   MOVE      *BLANKS       DESTOR           25            *
026000990317     C                   CALL      'TII117R'                                    *
026100000000     C                   PARM                    TOR150                         *
026200000000     C                   PARM                    DESTOR                         *
026300000000     C                   SETON                                            84    *
026400000000     C                   GOTO      EMISPT                                       *
026500000000     C                   END                                                    *
026600000000     C*--------                                            *
026700000000     C                   MOVE      '162'         TABGRU                         *
026800000000     C                   MOVE      *ZEROS        TABCOD                         *
026900000000     C                   MOVE      TOR150        TABCOD                         *
027000000000     C     KEYTAB        CHAIN     ANTAB00F                           90        *
027100000000     C   90              SETON                                          84      *
027200000000     C   90              MOVEL     ERR(5)        ERRORE                         *
027300000000     C   90              GOTO      EMISPT                                       *
027400000000     C                   MOVEL     TABDES        DESTOR                         *
027500000000     C                   END                                                    *
027600000000     C*---------- CONTROLLO PRESENZA TIPO RIGA ------------*
027700000000     C     TRG           IFEQ      *BLANKS                                      *
027800000000     C                   SETON                                          9085    *
027900000000     C                   MOVEL     ERR(6)        ERRORE                         *
028000000000     C                   GOTO      EMISPT                                       *
028100000000     C                   END                                                    *
028200000000     C*--- RICERCA GRUPPO MERCEOLOGICO --------------------*
028300000000     C     GRM150        IFNE      '99'                                         *
028400000000     C     GRM150        IFEQ      '? '                                         *
028500000000     C                   MOVE      *BLANKS       GRM150                         *
028600000000     C                   MOVE      *BLANKS       DESGRM           25            *
028700990317     C                   CALL      'TII114R'                                    *
028800000000     C                   PARM                    GRM150                         *
028900000000     C                   PARM                    DESGRM                         *
029000000000     C                   SETON                                            86    *
029100000000     C                   GOTO      EMISPT                                       *
029200000000     C                   END                                                    *
029300000000     C*--------                                            *
029400000000     C                   MOVE      '159'         TABGRU                         *
029500000000     C                   MOVE      *ZEROS        TABCOD                         *
029600000000     C                   MOVE      GRM150        TABCOD                         *
029700000000     C     KEYTAB        CHAIN     ANTAB00F                           90        *
029800000000     C   90              SETON                                          86      *
029900000000     C   90              MOVEL     ERR(7)        ERRORE                         *
030000000000     C   90              GOTO      EMISPT                                       *
030100000000     C                   MOVEL     TABDES        DESGRM                         *
030200000000     C                   END                                                    *
030300000000     C*--- RICERCA SETTORE MERCEOLOGICO -------------------*
030400000000     C     SEM150        IFNE      '99'                                         *
030500000000     C     SEM150        IFEQ      '? '                                         *
030600000000     C                   MOVE      *BLANKS       SEM150                         *
030700000000     C                   MOVE      *BLANKS       DESSEM           25            *
030800990317     C                   CALL      'TII115R'                                    *
030900000000     C                   PARM                    SEM150                         *
031000000000     C                   PARM                    DESSEM                         *
031100000000     C                   SETON                                            87    *
031200000000     C                   GOTO      EMISPT                                       *
031300000000     C                   END                                                    *
031400000000     C*--------                                            *
031500000000     C                   MOVE      '160'         TABGRU                         *
031600000000     C                   MOVE      *ZEROS        TABCOD                         *
031700000000     C                   MOVE      SEM150        TABCOD                         *
031800000000     C     KEYTAB        CHAIN     ANTAB00F                           90        *
031900000000     C   90              SETON                                          87      *
032000000000     C   90              MOVEL     ERR(8)        ERRORE                         *
032100000000     C   90              GOTO      EMISPT                                       *
032200000000     C                   MOVEL     TABDES        DESSEM                         *
032300000000     C                   END                                                    *
032400000000     C*--- RICERCA CLASSE MERCEOLOGICA --------------------*
032500000000     C     CLM150        IFNE      '999'                                        *
032600000000     C     CLM150        IFEQ      '?  '                                        *
032700000000     C                   MOVE      *BLANKS       CLM150                         *
032800000000     C                   MOVE      *BLANKS       DESCLM           25            *
032900990317     C                   CALL      'TII110R'                                    *
033000000000     C                   PARM                    CLM150                         *
033100000000     C                   PARM                    DESCLM                         *
033200000000     C                   SETON                                            88    *
033300000000     C                   GOTO      EMISPT                                       *
033400000000     C                   END                                                    *
033500000000     C*--------                                            *
033600000000     C                   MOVE      '155'         TABGRU                         *
033700000000     C                   MOVE      *ZEROS        TABCOD                         *
033800000000     C                   MOVE      CLM150        TABCOD                         *
033900000000     C     KEYTAB        CHAIN     ANTAB00F                           90        *
034000000000     C   90              SETON                                          88      *
034100000000     C   90              MOVEL     ERR(9)        ERRORE                         *
034200000000     C   90              GOTO      EMISPT                                       *
034300000000     C                   MOVEL     TABDES        DESCLM                         *
034400000000     C                   END                                                    *
034500000000     C*--- RICERCA S/CLASSE MERCEOLOGICA ------------------*
034600000000     C     SCM150        IFNE      '99999'                                      *
034700000000     C     SCM150        IFEQ      '?    '                                      *
034800000000     C                   MOVE      *BLANKS       SCM150                         *
034900000000     C                   MOVE      *BLANKS       DESSCM           25            *
035000990317     C                   CALL      'TII111R'                                    *
035100000000     C                   PARM                    SCM150                         *
035200000000     C                   PARM                    DESSCM                         *
035300000000     C                   SETON                                            89    *
035400000000     C                   GOTO      EMISPT                                       *
035500000000     C                   END                                                    *
035600000000     C*--------                                            *
035700000000     C                   MOVE      '156'         TABGRU                         *
035800000000     C                   MOVE      *ZEROS        TABCOD                         *
035900000000     C                   MOVE      SCM150        TABCOD                         *
036000000000     C     KEYTAB        CHAIN     ANTAB00F                           90        *
036100000000     C   90              SETON                                          89      *
036200000000     C   90              MOVEL     ERR(10)       ERRORE                         *
036300000000     C   90              GOTO      EMISPT                                       *
036400000000     C                   MOVEL     TABDES        DESSCM                         *
036500000000     C                   END                                                    *
036600000000     C*---------- CONTROLLO PRESENZA FASI -----------------*
036700000000     C     FAS           IFEQ      *BLANKS                                      *
036800000000     C                   SETON                                          9091    *
036900000000     C                   MOVEL     ERR(11)       ERRORE                         *
037000000000     C                   GOTO      EMISPT                                       *
037100000000     C                   END                                                    *
037200000000     C*--- RICERCA MAGAZZINO ------------------------------*
037300000000     C     MAG150        IFNE      '9999999'                                    *
037400000000     C     MAG150        IFEQ      '?      '                                    *
037500000000     C                   MOVE      *BLANKS       MAG150                         *
037600000000     C                   MOVE      *BLANKS       DAT153           75            *
037700990317     C                   CALL      'TII108R'                                    *
037800000000     C                   PARM                    MAG150                         *
037900000000     C                   PARM                    DAT153                         *
038000000000     C                   MOVEL     DAT153        DESMAG                         *
038100000000     C                   SETON                                        92        *
038200000000     C                   GOTO      EMISPT                                       *
038300000000     C                   END                                                    *
038400000000     C*----------                                          *
038500000000     C                   MOVE      '153'         TABGRU                         *
038600000000     C                   MOVE      *ZEROS        TABCOD                         *
038700000000     C                   MOVE      MAG150        TABCOD                         *
038800000000     C     KEYTAB        CHAIN     ANTAB00F                           90        *
038900000000     C   90              SETON                                        92        *
039000000000     C   90              MOVEL     ERR(12)       ERRORE                         *
039100000000     C   90              GOTO      EMISPT                                       *
039200000000     C                   MOVEL     TABDES        DESMAG                         *
039300000000     C                   END                                                    *
039400000000     C*--- INTERROGAZIONE PER CLIENTE ---------------------*
039500000000     C     FOR150        IFNE      *BLANKS                                      *
039600000000     C                   MOVE      'F'           RIC150                         *
039700000000     C                   GOTO      INTERR                                       *
039800000000     C                   END                                                    *
039900000000     C*--- INTERROGAZIONE PER ARTICOLO --------------------*
040000000000     C     MAT150        IFNE      *BLANKS                                      *
040100000000     C                   MOVE      'A'           RIC150                         *
040200000000     C                   GOTO      INTERR                                       *
040300000000     C                   END                                                    *
040400000000     C*----------------------------------------------------*
040500000000     C*--- RICHIAMO PROGRAMMI DI INTERROGAZIONE -----------*
040600000000     C*----------------------------------------------------*
040700000000     C     INTERR        TAG                                                    *
040800000000     C                   MOVEL     KPJBA         PARJBA                         *
040900000000     C*--- INTERROGAZIONE PER FORNITORE -------------------*
041000000000     C     RIC150        IFEQ      'F'                                          *
041100000000     C                   MOVE      *BLANKS       MAT150                         *
041200000000     C                   MOVEL     *BLANKS       DESMAT                         *
041300000000     C                   MOVEL     KPJBU         PARJBU                         *
041400000000     C                   Z-ADD     0             NORPAR                         *
041500000000     C                   Z-ADD     0             DORPAR                         *
041600000000     C                   MOVE      RIC150        RICPAR                         *
041700990317     C                   CALL      'TII301R1'                                   *
041800000000     C                   PARM                    PARAME                         *
041900000000     C     STAPAR        CABEQ     'S'           EMISPT                         *STAMPA
042000000000     C                   MOVE      PARJBU        KPJBU                          *
042100000000     C                   GOTO      INTERR                                       *
042200000000     C                   END                                                    *
042300000000     C*--- INTERROGAZIONE PER ARTICOLO --------------------*
042400000000     C     RIC150        IFEQ      'A'                                          *
042500000000     C                   MOVE      *BLANKS       FOR150                         *
042600000000     C                   MOVEL     *BLANKS       DESFOR                         *
042700000000     C                   MOVEL     KPJBU         PARJBU                         *
042800000000     C                   Z-ADD     0             NORPAR                         *
042900000000     C                   Z-ADD     0             DORPAR                         *
043000000000     C                   MOVE      RIC150        RICPAR                         *
043100990317     C                   CALL      'TII301R2'                                   *
043200000000     C                   PARM                    PARAME                         *
043300000000     C     STAPAR        CABEQ     'S'           EMISPT                         *STAMPA
043400000000     C                   MOVE      PARJBU        KPJBU                          *
043500000000     C                   GOTO      INTERR                                       *
043600000000     C                   END                                                    *
043700000000     C*--- REVISIONE ORDINE -------------------------------*
043800000000     C     RIC150        IFEQ      'O'                                          *
043900000000     C                   MOVEL     *BLANKS       DESFOR                         *
044000000000     C                   MOVE      *BLANKS       MAT150                         *
044100000000     C                   MOVEL     *BLANKS       DESMAT                         *
044200000000     C                   MOVEL     KPJBU         PARJBU                         *
044300000000     C                   MOVE      RIC150        RICPAR                         *
044400000000     C                   MOVE      *BLANKS       UTE150                         *
044500990317     C                   CALL      'TII300R'                                    *
044600000000     C                   PARM                    PARAME                         *
044700000000     C                   MOVE      PARJBU        KPJBU                          *
044800000000     C*--- SEGNALA BLOCCO FORNITORE :                      *
044900000000     C     UTE150        IFNE      *BLANK                                       *
045000000000     C                   SETON                                        818290    *
045100000000     C                   MOVE      ERR(14)       ERRORE                         *
045200000000     C                   MOVE      UTE150        ERRORE                         *
045300000000     C                   GOTO      EMISPT                                       *
045400000000     C                   END                                                    *
045500000000     C                   GOTO      INTERR                                       *
045600000000     C                   END                                                    *
045700000000     C*--- INTERROGAZIONE ORDINI --------------------------*
045800000000     C     RIC150        IFEQ      'I'                                          *
045900000000     C                   MOVEL     *BLANKS       DESFOR                         *
046000000000     C                   MOVE      *BLANKS       MAT150                         *
046100000000     C                   MOVEL     *BLANKS       DESMAT                         *
046200000000     C                   Z-ADD     0             NOR150                         *
046300000000     C                   Z-ADD     0             DOR150                         *
046400000000     C                   MOVEL     KPJBU         PARJBU                         *
046500000000     C                   MOVE      RIC150        RICPAR                         *
046600000000     C                   MOVE      *BLANKS       UTE150                         *
046700990317     C                   CALL      'TII300R'                                    *
046800000000     C                   PARM                    PARAME                         *
046900000000     C                   MOVE      PARJBU        KPJBU                          *
047000000000     C*--- SEGNALA BLOCCO FORNITORE :                      *
047100000000     C     UTE150        IFNE      *BLANK                                       *
047200000000     C                   SETON                                        818290    *
047300000000     C                   MOVE      ERR(14)       ERRORE                         *
047400000000     C                   MOVE      UTE150        ERRORE                         *
047500000000     C                   GOTO      EMISPT                                       *
047600000000     C                   END                                                    *
047700000000     C                   GOTO      INTERR                                       *
047800000000     C                   END                                                    *
047900000000     C*--- RICHIAMA PROMPT INIZIALE -----------------------*
048000000000     C     RIC150        CABEQ     'P'           EMISPT                         *
048100000000     C*--- ERRORE NON ESISTONO ORDINI ---------------------*
048200000000     C     RIC150        COMP      'E'                                    90    *
048300000000     C   90              MOVEL     ERR(13)       ERRORE                         *
048400000000     C   90              GOTO      EMISPT                                       *
048500000000     C*--- FINE LAVORO ------------------------------------*
048600000000     C     FINE          TAG                                                    *
048700000000     C   90              MOVEL     'KPJB207'     KMSMN                          *
048800000000     C                   SETON                                            LR    *
048900990126     C*----------------------------------------------------*
049000990126     C* Reperimento dati società
049100990126     C*----------------------------------------------------*
049200990126     C     REPSOC        BEGSR
049300990126     C*
049400990126     C                   CALLB     'XSOC'
049500990126     C                   PARM                    TIPXSC            6
049600990126     C                   PARM                    SOCXSC            3
049700990126     C                   PARM                    CDSXSC            9 0
049800990126     C                   PARM                    MODXSC            3
049900990126     C                   PARM      *blanks       RTNXSC            1
050000990126     C                   PARM                    XSOCDS
050100990126     C                   PARM                    KPJBA
050200990126     C*
050300990126     C                   ENDSR
050400000000     C*----------------------------------------------------*
050500000000**   ERR
050600000000*- Battuto sia CLIENTE E che ARTICOLO -*   ERR,1                            9897
050700000000*- Codice CLIENTE              ERRATO -*   ERR,2                              98
050800000000*- Codice ARTICOLO             ERRATO -*   ERR,3                              97
050900000000*- Date Limiti  DAL..AL...     ERRATE -*   ERR,4                              87
051000000000*- TIPO ORDINE                 ERRATO -*   ERR,5                              95
051100000000*- INSERIRE almeno un tipo RIGA !!!!! -*   ERR,6                              94
051200000000*- GRUPPO Merceologico Errato  ERRATO -*   ERR,7                              93
051300000000*- SETTORE Merceologico        ERRATO -*   ERR,8                              92
051400000000*- CLASSE Merceologica         ERRATA -*   ERR,9                              91
051500000000*- S/CLASSE Merceologica       ERRATA -*   ERR,10                             90
051600000000*- INSERIRE almeno una FASE    !!!!!! -*   ERR,11                             89
051700000000*- Codice Magazzino            ERRATO -*   ERR,12                             88
051800000000*- No ORDINI con parametri richiesti  -*   ERR,13                             86
051900000000*- CLIENTE   Bloccato da ---> xxxxxxxxxx   ERR,14
