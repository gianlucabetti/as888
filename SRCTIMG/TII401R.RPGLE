000100990204     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990204     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300860207     F*-OFN150R-*-OF/38-*--------------------------------------------*
000400860207     F*-----------------*                                            *
000500880915     F*         INTERROGAZIONE PORTAFOGLIO ORDINI FORNITORI          *
000600880915     F*         -------------PER DATA ORDINE -------------           *
000700860207     F*                                                              *
000800990204     F*         FILE : OFN150D  - tmfop01L - tmamp01L - ANTAB00F -   *
000900860207     F*                                                              *
001000990317     F*         PGM.:  tii103R - RICERCA ALFABETICA FORNITORI        *
001100990317     F*                tii104R - RICERCA ALFABETICA MATERIE PRIME    *
001200990317     F*                TII114R - RICERCA GRUPPO   MERCEOLOGICO       *
001300990317     F*                TII115R - RICERCA SETTORE  MERCEOLOGICO       *
001400990317     F*                TII110R - RICERCA CLASSE   MERCEOLOGICA       *
001500990317     F*                TII111R - RICERCA S/CLASSE MERCEOLOGICA       *
001600990317     F*                tii108R - RICERCA MAGAZZINI                   *
001700990317     F*                TII400R - REVISIONE ORDINI                    *
001800990317     F*                TII401R1- INTERROGAZIONE ORDINI X FORNITORE   *
001900990317     F*                TII401R2- INTERROGAZIONE ORDINI X ARTICOLO    *
002000860207     F*                RICDATE - CONTROLLO INVERSIONE DATA           *
002100860207     F*                                                              *
002200860207     F*--------------------------------------------------------------*
002300860207     F*                                                              *
002400860207     F*   "PARAMETRO DI RICHIAMO"                                    *
002500860207     F*                        F : INTERROGAZIONE X FORNITORE        *
002600860207     F*                        A : INTERROGAZIONE X ARTICOLO         *
002700860207     F*                        O : REVISIONE        ORDINE           *
002800860207     F*                        I : INTERROGAZIONE   ORDINI           *
002900860207     F*                        P : RICHIAMO PROMPT INIZIALE          *
003000860207     F*                        E : ERRORE NON ESISTONO ORDINI        *
003100860207     F*                                                              *
003200860207     F*--------------------------------------------------------------*
003300990204     Ftmfop01L  IF   E           K DISK
003400990204     Ftmamp01L  IF   E           K DISK
003500000000     FANTAB00F  IF   E           K DISK
003600990317     FTII401D   CF   E             WORKSTN
003700000000     F                                     IGNORE(SFL1)
003800000000     F                                     IGNORE(SFL1CT)
003900000000     F                                     IGNORE(SFL2)
004000000000     F                                     IGNORE(SFL2CT)
004100860207     F                                     IGNORE(OFN150FD)
004200851025     D*--------------------------------------------------*
004300860207     D ERR             S             40    DIM(14) CTDATA PERRCD(1)             * ERRORI
004400851025     D*--------------------------------------------------*
004500990204     D KPJBA         E DS
004600990204     D*--- PASSAGGIO PARAMETRI
004700990204     D  FOR150               247    254
004800990204     D  DESFOR               255    302
004900990204     D  MAT150               407    418
005000990204     D  DESMAT               311    340
005100990204     D  DAL150               303    310  0
005200990204     D  AL150                345    352  0
005300990204     D  SNV150               353    353
005400990204     D  TOR150               354    355
005500990204     D  TR1150               356    356
005600990204     D  TR2150               357    357
005700990204     D  TR3150               358    358
005800990204     D  TR4150               359    359
005900990204     D  TR5150               360    360
006000990204     D  TR6150               361    361
006100990204     D  TR7150               362    362
006200990204     D  TR8150               363    363
006300990204     D  TR9150               364    364
006400990204     D  TRG                  356    364
006500990204     D  GRM150               365    366
006600990204     D  SEM150               367    368
006700990204     D  CLM150               369    371
006800990204     D  SCM150               372    376
006900990204     D  FA1150               377    378
007000990204     D  FA2150               379    380
007100990204     D  FA3150               381    382
007200990204     D  FA4150               383    384
007300990204     D  FA5150               385    386
007400990204     D  FAS                  377    386
007500990204     D  MAG150               387    393
007600990204     D  DOR150               394    398  0
007700990204     D  NOR150               399    406  0
007800990204     D  RIC150               442    442
007900990204     D  UTE150               443    452
008000990204     D  MSGERR               453    502
008100990204     D PARAME          DS           502
008200990204     D*--- PARAMETRI X INTERROGAZIONE :
008300990204     D  PARJBA                 1    247
008400990204     D  PARJBU               247    502
008500990204     D  FORPAR               247    254
008600990204     D  DESF                 255    302
008700990204     D  MATPAR               407    418
008800990204     D  DESM                 311    340
008900990204     D  DALPAR               303    310  0
009000990204     D  ALPAR                345    352  0
009100990204     D  SNVPAR               353    353
009200990204     D  DATPAR               354    393
009300990204     D  DORPAR               394    398  0
009400990204     D  NORPAR               399    406  0
009500990204     D  STAPAR               441    441
009600990204     D  RICPAR               442    442
009700990204     D  UTEPAR               443    452
009800990204     D  MSGPAR               453    502
009900990204     D soc001        E DS                  EXTNAME(xsoc001ds)
010000990204     D xsocds          DS          1000
010100990204     d dataiso         s               d   datfmt(*iso)
010200990204     d dataeur         s               d   datfmt(*eur)
010300000000     C*----------------------------------------------------*
010400860207     C     *ENTRY        PLIST                                                  *
010500860207     C                   PARM                    KPJBA                          *
010600990204     C*---------- RICERCA DITTA :
010700990204     C                   MOVEL     'SOC001'      TIPXSC
010800990204     C                   MOVEL     *blank        SOCXSC
010900990204     C                   EXSR      REPSOC
011000990204     C     RTNXSC        IFNE      '1'
011100990204     C                   MOVEL     XSOCDS        SOC001
011200990204     C                   MOVEL     xscrgs        desdit
011300990204     C                   MOVEL     xscsoc        wsoc              3
011400990204     c                   end
011500990204     c                   time                    como14           14 0
011600990204     c                   move      como14        woggi             8 0
011700990204     c                   move      woggi         dataeur
011800990204     c                   move      dataeur       dataiso
011900990204     c                   move      dataiso       woggig            8 0
012000860207     C*--- FILE ANAGRAFICO MATERIE PRIME :                 *
012100860207     C     KEYAMP        KLIST                                                  *
012200860207     C                   KFLD                    MAT150                         *
012300860207     C*--- FILE ANAGRAFICO FORNITORI PARTICOLARITA :       *
012400860207     C     KEYFOP        KLIST                                                  *
012500860207     C                   KFLD                    FOR150                         *
012600860207     C*--- FILE ANAGRAFICO TABELLE :                       *
012700860207     C     KEYTAB        KLIST                                                  *
012800860207     C                   KFLD                    TABGRU                         *
012900860207     C                   KFLD                    TABCOD                         *
013000000000     C*--- IMPOSTAZIONI INIZIALI SU PROMPT ----------------*
013100990204     C                   MOVEL     *BLANKS       INTER            12            *
013200990204     C                   MOVEL     '?'           INTER                          *
013300990204     c                   move      *all'9'       nove             12
013400990204     C                   Z-ADD     0             DAL                            *
013500990204     C                   Z-ADD     woggi         AL                             *
013600860207     C                   MOVE      *ZEROS        DAL150                         *
013700860207     C                   MOVE      *ZEROS        AL150                          *
013800860207     C                   MOVE      'N'           SNV150                         *
013900860207     C                   MOVE      '? '          TOR150                         *
014000860207     C                   MOVEL     '123DCIS'     TRG                            *
014100860207     C                   MOVE      '99'          GRM150                         *
014200860207     C                   MOVE      '99'          SEM150                         *
014300860207     C                   MOVE      '999'         CLM150                         *
014400860207     C                   MOVE      '99999'       SCM150                         *
014500860207     C                   MOVE      'OR'          FA1150                         *
014600860207     C                   MOVE      'BB'          FA4150                         *
014700860207     C                   MOVE      'FA'          FA5150                         *
014800860207     C                   MOVE      '9999999'     MAG150                         *
014900860207     C                   MOVE      *ZEROS        DOR150                         *
015000860207     C                   MOVE      *ZEROS        NOR150                         *
015100860207     C                   WRITE     OFN150T1                                     * TESTATA
015200000000     C*----------------------------------------------------*
015300860207     C     EMISPT        TAG                                                    *
015400860207     C                   EXFMT     OFN150PT                                     * PROMPT
015500990409     C   Kc              GOTO      FINE                                         *
015600860207     C                   MOVEA     *ZEROS        *IN(1)                         *
015700860207     C                   MOVE      *BLANKS       DESTOR                         *
015800860207     C                   MOVE      *BLANKS       DESGRM                         *
015900860207     C                   MOVE      *BLANKS       DESSEM                         *
016000860207     C                   MOVE      *BLANKS       DESCLM                         *
016100860207     C                   MOVE      *BLANKS       DESSCM                         *
016200860207     C                   MOVE      *BLANKS       DESMAG                         *
016300860207     C                   MOVE      *BLANKS       RIC150                         *
016400860207     C*--- CONTROLLO SE SCELTO SIA FORNIT. CHE ARTICOLO ---*
016500860207     C     FOR150        IFEQ      *BLANKS                                      *
016600860207     C     MAT150        IFEQ      *BLANKS                                      *
016700860207     C                   SETON                                        90        *
016800860207     C   90              SETON                                        8182      *
016900860207     C   90              MOVEL     ERR(1)        ERRORE                         *
017000860207     C   90              GOTO      EMISPT                                       *
017100860207     C                   END                                                    *
017200860207     C                   END                                                    *
017300860207     C*----------------- FORNITORE ------------------------*
017400860207     C     FOR150        CABEQ     *BLANKS       RICMAT                         *
017500990204     C     FOR150        IFEQ      '?       '                                   *
017600990204     C                   MOVE      *ZEROS        FOR014            8            *
017700860224     C                   MOVEL     DESFOR        RAG014           30            *
017800990317     C                   CALL      'TII103R'                                    *
017900860224     C                   PARM                    FOR014                         *
018000860224     C                   PARM                    RAG014                         *
018100860224     C                   MOVE      FOR014        FOR150                         *
018200860207     C                   MOVE      *BLANKS       DESFOR                         *
018300860224     C                   MOVEL     RAG014        DESFOR                         *
018400990204     C     FOR150        COMP      '99999999'                             90    *
018500860207     C   90              SETON                                        81        *
018600860207     C   90              MOVEL     ERR(2)        ERRORE                         *
018700860207     C                   GOTO      EMISPT                                       *
018800860207     C                   END                                                    *
018900860207     C*------------                                        *
019000860207     C                   MOVE      *BLANKS       DESFOR                         *
019100990204     C     KEYFOP        CHAIN     tmfop01L                           90        *
019200860207     C   90              SETON                                        81        *
019300860207     C   90              MOVEL     ERR(2)        ERRORE                         *
019400860207     C   90              GOTO      EMISPT                                       *
019500860207     C                   MOVE      *BLANKS       DESFOR                         *
019600860207     C                   MOVEL     FOPRSI        DESFOR                         *
019700860207     C*----------------- ARTICOLO -------------------------*
019800860207     C     RICMAT        TAG                                                    *
019900860207     C     MAT150        CABEQ     *BLANKS       RICDAT                         *
020000871006     C     MAT150        IFEQ      INTER                                        *
020100860207     C                   MOVE      *BLANKS       MAT150                         *
020200990317     C                   CALL      'TII104R'                                    *
020300860207     C                   PARM                    MAT150                         *
020400860207     C                   PARM                    DESMAT                         *
020500870105     C     MAT150        COMP      NOVE                                   90    *
020600860207     C   90              SETON                                        82        *
020700860207     C   90              MOVEL     ERR(3)        ERRORE                         *
020800860207     C                   GOTO      EMISPT                                       *
020900860207     C                   END                                                    *
021000860207     C*------------------                                  *
021100860207     C                   MOVE      *BLANKS       DESMAT                         *
021200990204     C     KEYAMP        CHAIN     tmamp01L                           90        *
021300860207     C   90              SETON                                        82        *
021400860207     C   90              MOVEL     ERR(3)        ERRORE                         *
021500860207     C   90              GOTO      EMISPT                                       *
021600860207     C                   MOVEL     AMPDMT        DESMAT                         *
021700860207     C*--- ROVESCIO E CONTROLLO DATA LIMITE INFERIORE -----*
021800860207     C     RICDAT        TAG                                                    *
021900990204     C     DAL           IFNE      0                                            *
022000990204     c     *eur          test(d)                 dal                    90
022100990204     c                   if        *in90
022200990204     c     *dmy          test(d)                 dal                    90
022300990204     c  n90*dmy          move      dal           dataeur
022400990204     c  n90              move      dataeur       dal
022500990204     c                   else
022600990204     c                   move      dal           dataeur
022700990204     c                   end
022800990204     c   90              goto      errdat
022900990204     c                   move      dataeur       dataiso
023000990204     c                   move      dataiso       dal150
023100990204     C                   END                                                    *
023200860207     C*--- ROVESCIO E CONTROLLO DATA LIMITE SUPERIORE -----*
023300990204     C     al            IFNE      0                                            *
023400990204     c     *eur          test(d)                 al                     90
023500990204     c                   if        *in90
023600990204     c     *dmy          test(d)                 al                     90
023700990204     c  n90*dmy          move      al            dataeur
023800990204     c  n90              move      dataeur       al
023900990204     c                   else
024000990204     c                   move      al            dataeur
024100990204     c                   end
024200990204     c   90              goto      errdat
024300990204     c                   move      dataeur       dataiso
024400990204     c                   move      dataiso       al150
024500990204     C                   END                                                    *
024600860207     C*--- CONTROLLO INCROCIATO FRA DATE LIMITI -----------*
024700860207     C     DAL150        COMP      AL150                              90        *
024800860207     C     ERRDAT        TAG                                                    *
024900860207     C   90              SETON                                        83        *
025000860207     C   90              MOVEL     ERR(4)        ERRORE                         *
025100860207     C   90              GOTO      EMISPT                                       *
025200860207     C*--- RICERCA TIPO ORDINE ----------------------------*
025300860207     C     TOR150        IFNE      '99'                                         *
025400860207     C     TOR150        IFEQ      '? '                                         *
025500860207     C                   MOVE      *BLANKS       TOR150                         *
025600860207     C                   MOVE      *BLANKS       DESTOR           25            *
025700990317     C                   CALL      'TII117R'                                    *
025800860207     C                   PARM                    TOR150                         *
025900860207     C                   PARM                    DESTOR                         *
026000860207     C                   SETON                                            84    *
026100860207     C                   GOTO      EMISPT                                       *
026200860207     C                   END                                                    *
026300860207     C*--------                                            *
026400860207     C                   MOVE      '162'         TABGRU                         *
026500860207     C                   MOVE      *ZEROS        TABCOD                         *
026600860207     C                   MOVE      TOR150        TABCOD                         *
026700860207     C     KEYTAB        CHAIN     ANTAB00F                           90        *
026800860207     C   90              SETON                                          84      *
026900860207     C   90              MOVEL     ERR(5)        ERRORE                         *
027000860207     C   90              GOTO      EMISPT                                       *
027100860207     C                   MOVEL     TABDES        DESTOR                         *
027200860207     C                   END                                                    *
027300860207     C*---------- CONTROLLO PRESENZA TIPO RIGA ------------*
027400860207     C     TRG           IFEQ      *BLANKS                                      *
027500860207     C                   SETON                                          9085    *
027600860207     C                   MOVEL     ERR(6)        ERRORE                         *
027700860207     C                   GOTO      EMISPT                                       *
027800860207     C                   END                                                    *
027900860207     C*--- RICERCA GRUPPO MERCEOLOGICO --------------------*
028000860207     C     GRM150        IFNE      '99'                                         *
028100860207     C     GRM150        IFEQ      '? '                                         *
028200860207     C                   MOVE      *BLANKS       GRM150                         *
028300860207     C                   MOVE      *BLANKS       DESGRM           25            *
028400990317     C                   CALL      'TII114R'                                    *
028500860207     C                   PARM                    GRM150                         *
028600860207     C                   PARM                    DESGRM                         *
028700860207     C                   SETON                                            86    *
028800860207     C                   GOTO      EMISPT                                       *
028900860207     C                   END                                                    *
029000860207     C*--------                                            *
029100860207     C                   MOVE      '159'         TABGRU                         *
029200860207     C                   MOVE      *ZEROS        TABCOD                         *
029300860207     C                   MOVE      GRM150        TABCOD                         *
029400860207     C     KEYTAB        CHAIN     ANTAB00F                           90        *
029500860207     C   90              SETON                                          86      *
029600860207     C   90              MOVEL     ERR(7)        ERRORE                         *
029700860207     C   90              GOTO      EMISPT                                       *
029800860207     C                   MOVEL     TABDES        DESGRM                         *
029900860207     C                   END                                                    *
030000860207     C*--- RICERCA SETTORE MERCEOLOGICO -------------------*
030100860207     C     SEM150        IFNE      '99'                                         *
030200860207     C     SEM150        IFEQ      '? '                                         *
030300860207     C                   MOVE      *BLANKS       SEM150                         *
030400860207     C                   MOVE      *BLANKS       DESSEM           25            *
030500990317     C                   CALL      'TII115R'                                    *
030600860207     C                   PARM                    SEM150                         *
030700860207     C                   PARM                    DESSEM                         *
030800860207     C                   SETON                                            87    *
030900860207     C                   GOTO      EMISPT                                       *
031000860207     C                   END                                                    *
031100860207     C*--------                                            *
031200860207     C                   MOVE      '160'         TABGRU                         *
031300860207     C                   MOVE      *ZEROS        TABCOD                         *
031400860207     C                   MOVE      SEM150        TABCOD                         *
031500860207     C     KEYTAB        CHAIN     ANTAB00F                           90        *
031600860207     C   90              SETON                                          87      *
031700860207     C   90              MOVEL     ERR(8)        ERRORE                         *
031800860207     C   90              GOTO      EMISPT                                       *
031900860207     C                   MOVEL     TABDES        DESSEM                         *
032000860207     C                   END                                                    *
032100860207     C*--- RICERCA CLASSE MERCEOLOGICA --------------------*
032200860207     C     CLM150        IFNE      '999'                                        *
032300860207     C     CLM150        IFEQ      '?  '                                        *
032400860207     C                   MOVE      *BLANKS       CLM150                         *
032500860207     C                   MOVE      *BLANKS       DESCLM           25            *
032600990317     C                   CALL      'TII110R'                                    *
032700860207     C                   PARM                    CLM150                         *
032800860207     C                   PARM                    DESCLM                         *
032900860207     C                   SETON                                            88    *
033000860207     C                   GOTO      EMISPT                                       *
033100860207     C                   END                                                    *
033200860207     C*--------                                            *
033300860207     C                   MOVE      '155'         TABGRU                         *
033400860207     C                   MOVE      *ZEROS        TABCOD                         *
033500860207     C                   MOVE      CLM150        TABCOD                         *
033600860207     C     KEYTAB        CHAIN     ANTAB00F                           90        *
033700860207     C   90              SETON                                          88      *
033800860207     C   90              MOVEL     ERR(9)        ERRORE                         *
033900860207     C   90              GOTO      EMISPT                                       *
034000860207     C                   MOVEL     TABDES        DESCLM                         *
034100860207     C                   END                                                    *
034200860207     C*--- RICERCA S/CLASSE MERCEOLOGICA ------------------*
034300860207     C     SCM150        IFNE      '99999'                                      *
034400860207     C     SCM150        IFEQ      '?    '                                      *
034500860207     C                   MOVE      *BLANKS       SCM150                         *
034600860207     C                   MOVE      *BLANKS       DESSCM           25            *
034700990317     C                   CALL      'TII111R'                                    *
034800860207     C                   PARM                    SCM150                         *
034900860207     C                   PARM                    DESSCM                         *
035000860207     C                   SETON                                            89    *
035100860207     C                   GOTO      EMISPT                                       *
035200860207     C                   END                                                    *
035300860207     C*--------                                            *
035400860207     C                   MOVE      '156'         TABGRU                         *
035500860207     C                   MOVE      *ZEROS        TABCOD                         *
035600860207     C                   MOVE      SCM150        TABCOD                         *
035700860207     C     KEYTAB        CHAIN     ANTAB00F                           90        *
035800860207     C   90              SETON                                          89      *
035900860207     C   90              MOVEL     ERR(10)       ERRORE                         *
036000860207     C   90              GOTO      EMISPT                                       *
036100860207     C                   MOVEL     TABDES        DESSCM                         *
036200860207     C                   END                                                    *
036300860207     C*---------- CONTROLLO PRESENZA FASI -----------------*
036400860207     C     FAS           IFEQ      *BLANKS                                      *
036500860207     C                   SETON                                          9091    *
036600860207     C                   MOVEL     ERR(11)       ERRORE                         *
036700860207     C                   GOTO      EMISPT                                       *
036800860207     C                   END                                                    *
036900000000     C*--- RICERCA MAGAZZINO ------------------------------*
037000860207     C     MAG150        IFNE      '9999999'                                    *
037100860207     C     MAG150        IFEQ      '?      '                                    *
037200860207     C                   MOVE      *BLANKS       MAG150                         *
037300860207     C                   MOVE      *BLANKS       DAT153           75            *
037400990317     C                   CALL      'TII108R'                                    *
037500860207     C                   PARM                    MAG150                         *
037600860207     C                   PARM                    DAT153                         *
037700860207     C                   MOVEL     DAT153        DESMAG                         *
037800860207     C                   SETON                                        92        *
037900860207     C                   GOTO      EMISPT                                       *
038000860207     C                   END                                                    *
038100860207     C*----------                                          *
038200860207     C                   MOVE      '153'         TABGRU                         *
038300860207     C                   MOVE      *ZEROS        TABCOD                         *
038400860207     C                   MOVE      MAG150        TABCOD                         *
038500860207     C     KEYTAB        CHAIN     ANTAB00F                           90        *
038600860207     C   90              SETON                                        92        *
038700860207     C   90              MOVEL     ERR(12)       ERRORE                         *
038800860207     C   90              GOTO      EMISPT                                       *
038900860207     C                   MOVEL     TABDES        DESMAG                         *
039000860207     C                   END                                                    *
039100000000     C*--- INTERROGAZIONE PER CLIENTE ---------------------*
039200860207     C     FOR150        IFNE      *BLANKS                                      *
039300860207     C                   MOVE      'F'           RIC150                         *
039400860207     C                   GOTO      INTERR                                       *
039500860207     C                   END                                                    *
039600000000     C*--- INTERROGAZIONE PER ARTICOLO --------------------*
039700860207     C     MAT150        IFNE      *BLANKS                                      *
039800860207     C                   MOVE      'A'           RIC150                         *
039900860207     C                   GOTO      INTERR                                       *
040000860207     C                   END                                                    *
040100000000     C*--- RICHIAMO PROGRAMMI DI INTERROGAZIONE -----------*
040200851025     C*----------------------------------------------------*
040300860207     C     INTERR        TAG                                                    *
040400860207     C                   MOVEL     KPJBA         PARJBA                         *
040500860210     C*--- INTERROGAZIONE PER FORNITORE -------------------*
040600860207     C     RIC150        IFEQ      'F'                                          *
040700860207     C                   MOVE      *BLANKS       MAT150                         *
040800860207     C                   MOVEL     *BLANKS       DESMAT                         *
040900860207     C                   MOVEL     KPJBU         PARJBU                         *
041000860207     C                   Z-ADD     0             NORPAR                         *
041100860207     C                   Z-ADD     0             DORPAR                         *
041200860207     C                   MOVE      RIC150        RICPAR                         *
041300990317     C                   CALL      'TII401R1'                                   *
041400860207     C                   PARM                    PARAME                         *
041500860210     C     STAPAR        CABEQ     'S'           EMISPT                         *STAMPA
041600860207     C                   MOVE      PARJBU        KPJBU                          *
041700860207     C                   GOTO      INTERR                                       *
041800860207     C                   END                                                    *
041900851025     C*--- INTERROGAZIONE PER ARTICOLO --------------------*
042000860207     C     RIC150        IFEQ      'A'                                          *
042100860207     C                   MOVE      *BLANKS       FOR150                         *
042200860207     C                   MOVEL     *BLANKS       DESFOR                         *
042300860207     C                   MOVEL     KPJBU         PARJBU                         *
042400860207     C                   Z-ADD     0             NORPAR                         *
042500860207     C                   Z-ADD     0             DORPAR                         *
042600860207     C                   MOVE      RIC150        RICPAR                         *
042700990317     C                   CALL      'TII401R2'                                   *
042800860207     C                   PARM                    PARAME                         *
042900860210     C     STAPAR        CABEQ     'S'           EMISPT                         *STAMPA
043000860207     C                   MOVE      PARJBU        KPJBU                          *
043100860207     C                   GOTO      INTERR                                       *
043200860207     C                   END                                                    *
043300851025     C*--- REVISIONE ORDINE -------------------------------*
043400860207     C     RIC150        IFEQ      'O'                                          *
043500860207     C                   MOVEL     *BLANKS       DESFOR                         *
043600860207     C                   MOVE      *BLANKS       MAT150                         *
043700860207     C                   MOVEL     *BLANKS       DESMAT                         *
043800860207     C                   MOVEL     KPJBU         PARJBU                         *
043900860207     C                   MOVE      RIC150        RICPAR                         *
044000860207     C                   MOVE      *BLANKS       UTE150                         *
044100990317     C                   CALL      'TII400R'                                    *
044200860207     C                   PARM                    PARAME                         *
044300860207     C                   MOVE      PARJBU        KPJBU                          *
044400860207     C*--- SEGNALA BLOCCO FORNITORE :                      *
044500860207     C     UTE150        IFNE      *BLANK                                       *
044600860210     C                   SETON                                        818290    *
044700860207     C                   MOVE      ERR(14)       ERRORE                         *
044800860207     C                   MOVE      UTE150        ERRORE                         *
044900860207     C                   GOTO      EMISPT                                       *
045000860207     C                   END                                                    *
045100860207     C                   GOTO      INTERR                                       *
045200860207     C                   END                                                    *
045300851025     C*--- INTERROGAZIONE ORDINI --------------------------*
045400860207     C     RIC150        IFEQ      'I'                                          *
045500860207     C                   MOVEL     *BLANKS       DESFOR                         *
045600860207     C                   MOVE      *BLANKS       MAT150                         *
045700860207     C                   MOVEL     *BLANKS       DESMAT                         *
045800860207     C                   Z-ADD     0             NOR150                         *
045900860207     C                   Z-ADD     0             DOR150                         *
046000860207     C                   MOVEL     KPJBU         PARJBU                         *
046100860207     C                   MOVE      RIC150        RICPAR                         *
046200860207     C                   MOVE      *BLANKS       UTE150                         *
046300990317     C                   CALL      'TII400R'                                    *
046400860207     C                   PARM                    PARAME                         *
046500860207     C                   MOVE      PARJBU        KPJBU                          *
046600860207     C*--- SEGNALA BLOCCO FORNITORE :                      *
046700860207     C     UTE150        IFNE      *BLANK                                       *
046800860210     C                   SETON                                        818290    *
046900860207     C                   MOVE      ERR(14)       ERRORE                         *
047000860207     C                   MOVE      UTE150        ERRORE                         *
047100860207     C                   GOTO      EMISPT                                       *
047200860207     C                   END                                                    *
047300860207     C                   GOTO      INTERR                                       *
047400860207     C                   END                                                    *
047500851025     C*--- RICHIAMA PROMPT INIZIALE -----------------------*
047600860207     C     RIC150        CABEQ     'P'           EMISPT                         *
047700851025     C*--- ERRORE NON ESISTONO ORDINI ---------------------*
047800860207     C     RIC150        COMP      'E'                                    90    *
047900860207     C   90              MOVEL     ERR(13)       ERRORE                         *
048000860207     C   90              GOTO      EMISPT                                       *
048100000000     C*--- FINE LAVORO ------------------------------------*
048200860207     C     FINE          TAG                                                    *
048300860207     C                   SETON                                            LR    *
048400990204     C*----------------------------------------------------*
048500990204     C* Reperimento dati società
048600990204     C*----------------------------------------------------*
048700990204     C     REPSOC        BEGSR
048800990204     C*
048900990204     C                   CALLB     'XSOC'
049000990204     C                   PARM                    TIPXSC            6
049100990204     C                   PARM                    SOCXSC            3
049200990204     C                   PARM                    CDSXSC            9 0
049300990204     C                   PARM                    MODXSC            3
049400990204     C                   PARM      *blanks       RTNXSC            1
049500990204     C                   PARM                    XSOCDS
049600990204     C                   PARM                    KPJBA
049700990204     C*
049800990204     C                   ENDSR
049900000000     C*----------------------------------------------------*
050000860207**   ERR
050100860207*- Battuto sia FORNITORE che ARTICOLO -*   ERR,1                            9897
050200860207*- Codice FORNITORE            ERRATO -*   ERR,2                              98
050300860207*- Codice ARTICOLO             ERRATO -*   ERR,3                              97
050400860207*- Date Limiti  DAL..AL...     ERRATE -*   ERR,4                              87
050500860207*- TIPO ORDINE                 ERRATO -*   ERR,5                              95
050600860207*- INSERIRE almeno un tipo RIGA !!!!! -*   ERR,6                              94
050700860207*- GRUPPO Merceologico Errato  ERRATO -*   ERR,7                              93
050800860207*- SETTORE Merceologico        ERRATO -*   ERR,8                              92
050900860207*- CLASSE Merceologica         ERRATA -*   ERR,9                              91
051000860207*- S/CLASSE Merceologica       ERRATA -*   ERR,10                             90
051100860207*- INSERIRE almeno una FASE    !!!!!! -*   ERR,11                             89
051200860207*- Codice Magazzino            ERRATO -*   ERR,12                             88
051300860207*- No ORDINI con parametri richiesti  -*   ERR,13                             86
051400860207*- FORNITORE Bloccato da ---> xxxxxxxxxx   ERR,14
