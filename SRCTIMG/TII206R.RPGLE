000100990122     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990122     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     F* MPH121R  *-MP/38-*-------------------------------------------*
000400000000     F*------------------*                                           *
000500000000     F*       - STAMPA SITUAZIONE GIACENZA E DISPONIB. MAG. M.P. -   *
000600000000     F*       ----------------------------------------------------   *
000700000000     F*                                                   AZN.: H121 *
000800990122     F*         FILE : -ANTAB00F-tmamp01L-tmamp09L-tmamp10L          *
000900990122     F*         FILE : -tmsmp01L-MPPRTSTD                            *
001000000000     F*                                                              *
001100000000     F*         PGM. : RIC001R   - RICERCA INTESTAZIONE DITTA        *
001200000000     F*--------------------------------------------------------------*
001300000000     F*    IND.  01 - RICHIESTA GIACENZA                             *
001400000000     F*     "    02 - RICHIESTA DISPONIBILITA                        *
001500850118     F*     "   N04 - RICHIESTO UN       PRODOTTO                    *
001600850118     F*     "    04 - RICHIESTO TUTTI I  PRODOTTI                    *
001700850118     F*     "   N05 - RICHIESTO UNA      CLASSE MERCEOLOGICA         *
001800850118     F*     "    05 - RICHIESTO TUTTE    CLASSI MERCEOLOGICHE        *
001900850118     F*     "   N06 - RICHIESTO UNA      S/CLASSE MERCEOLOGICA       *
002000850118     F*     "    06 - RICHIESTO TUTTE    S/CLASSE MERCEOLOGICA       *
002100880105     F*                                                              *
002200000000     F*--------------------------------------------------------------*
002300000000     FANTAB00F  IF   E           K DISK
002400990122     Ftmamp01L  IF   E           K DISK    USROPN
002500990122     F                                     RENAME(tmamp000:ALLART)
002600990122     Ftmamp09L  IF   E           K DISK    USROPN
002700990122     F                                     RENAME(tmamp000:CLSART)
002800990122     Ftmamp10L  IF   E           K DISK    USROPN
002900990122     F                                     RENAME(tmamp000:SCLART)
003000990122     Ftmsmp01L  IF   E           K DISK
003100850118     FMPPRTSTD  O    F  132        PRINTER OFLIND(*INOF)
003200000000     F*--------------------------------------------------------------*
003300850118     D MAG             S              7    DIM(100)                             *MAGAZZINI
003400000000     D TEN             S             11  2 DIM(5)                               *TOT.ENT.FILE
003500850121     D ENS             S              8  1 DIM(5)                               *ENTRATE STAMPA
003600000000     D TUS             S             11  2 DIM(5)                               *TOT.USC.FILE
003700850121     D USS             S              8  1 DIM(5)                               *USCITE  STAMPA
003800870519     D TDS             S             11  2 DIM(5)                               *TOT.DIS.FILE
003900850118     D ERR             S             50    DIM(6) CTDATA PERRCD(1)              *ERRORI
004000850118     D TUM             S              1    DIM(4) CTDATA PERRCD(1)              *TIPI U.M.
004100850118     D DUM             S             10    DIM(4) ALT(TUM)
004200850118     D SK1             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 1
004300850118     D SK2             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 2
004400850118     D SK3             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 3
004500850118     D SK4             S             66    DIM(2) CTDATA PERRCD(1)              *
004600850118     D SK5             S             66    DIM(2) CTDATA PERRCD(1)              *
004700880105     D SK6             S             66    DIM(2) CTDATA PERRCD(1)              *
004800880104     D*                   SK7     1   2 66               *
004900850118     D SK8             S             66    DIM(2) CTDATA PERRCD(1)              *
005000850118     D SK9             S             66    DIM(2) CTDATA PERRCD(1)              *
005100880104     D*                   SK10    1   2 66               *
005200000000     D*--------------------------------------------------------------*
005300000000     D TB151         E DS                  EXTNAME(ANTAB151)
005400850118     D*------------- DESCRIZIONI ENTRATE/USCITE :
005500850118     D KEY151          DS
005600850118     D*------------- CHIAVE TABELLA 151 :
005700850118     D  PROCED                 1      2
005800850118     D  EU                     3      3
005900850118     D  DESPRC                 1      3
006000850118     D TB153         E DS                  EXTNAME(ANTAB153)
006100850118     D*------------- DESCRIZIONE MAGAZZINI :
006200000000     D DESENT          DS
006300000000     D*------------- DESCRIZIONI COLONNE ENTRATE :
006400000000     D  DE1                    1      9
006500000000     D  DE2                   10     18
006600000000     D  DE3                   19     27
006700000000     D  DE4                   28     36
006800000000     D  DE5                   37     45
006900000000     D DESUSC          DS
007000000000     D*------------- DESCRIZIONI COLONNE USCITE :
007100000000     D  DU1                    1      9
007200000000     D  DU2                   10     18
007300000000     D  DU3                   19     27
007400000000     D  DU4                   28     36
007500000000     D  DU5                   37     45
007600990122     D RECSAL        E DS                  EXTNAME(tmsmp00F)
007700990211     D  DS                    56     85P 2
007800870519     D                                     DIM(5)                               *DISPON. FILE
007900990211     D  EN                    86    115P 2
008000000000     D                                     DIM(5)                               *ENTRATE FILE
008100990211     D  US                   116    145P 2
008200000000     D                                     DIM(5)                               *USCITE  FILE
008300000000     D KPJBA         E DS
008400000000     D*----- PASSAGGIO PARAMETRI
008500990122     D  AAA120               247    250  0
008600990122     D  SCE120               251    251
008700990122     D  UMI120               252    252
008800990122     D  MAG120               253    259
008900990122     D  CLS120               260    262
009000990122     D  SCL120               263    267
009100990122     D  MAT120               284    295
009200990122     D  SAL120               277    283  0
009300990122     D  desdit               433    452
009400990122     D  MSGERR               453    502
009500000000     C*----------------------------------------------------*
009600000000     C     *ENTRY        PLIST                                                  *
009700000000     C                   PARM                    KPJBA                          *
009800990122     C                   MOVE      *all'9'       NOVE             12
009900000000     C*---------- ANAGRAFICO MATERIE PRIME :               *
010000000000     C     KEYAMP        KLIST                                                  *
010100850118     C                   KFLD                    MAT120                         *
010200000000     C*---------- SALDI DI MAGAZZINO :                     *
010300850118     C     KEYMAG        KLIST                                                  *
010400990122     C                   KFLD                    AAa120                         *
010500850118     C                   KFLD                    MAGAZZ            7            *
010600850118     C                   KFLD                    AMPMAT                         *
010700850118     C     KEYMG1        KLIST                                                  *
010800990122     C                   KFLD                    AAa120                         *
010900850118     C                   KFLD                    MAGAZZ            7            *
011000000000     C*---------- ANAGRAFICO TABELLE :                     *
011100000000     C     KEYTAB        KLIST                                                  *
011200000000     C                   KFLD                    TABGRU                         *
011300000000     C                   KFLD                    TABCOD                         *
011400850118     C*---------- IMPOSTO TIPO RICHIESTA :                 *
011500850118     C     SCE120        COMP      'G'                                    01    *GIACENZA
011600850118     C     SCE120        COMP      'D'                                    02    *DISPONIB.
011700850118     C  N01
011800850118     CANN02              GOTO      FINE                                         *
011900850118     C*---------- RICERCA MAGAZZINI :                      *
012000850118     C                   Z-ADD     0             M                 3 0          *
012100850118     C                   MOVE      '153'         TABGRU                         *
012200880105     C     TABGRU        SETLL     ANTAB00F                               90    *
012300880105     C  N90              MOVEL     ERR(2)        MSGERR                         *
012400880105     C  N90              GOTO      FINE                                         *
012500850118     C     RICMAG        TAG                                                    *
012600850118     C     TABGRU        READE     ANTAB000                               91    *
012700850118     C   91
012800850118     CAN 71              GOTO      AVANTI                                       *
012900850118     C   91
013000850118     CANN71              SETON                                            90    *
013100880105     C   91              MOVEL     ERR(2)        MSGERR                         *
013200880105     C   91              GOTO      FINE                                         *
013300850118     C*----------------------------------------------------*
013400850118     C     MAG120        IFNE      '9999999'                                    *
013500850118     C     MAG120        CABNE     TABCOD        RICMAG                         *
013600850118     C                   MOVE      MAG120        MAGAZZ                         *
013700850118     C                   ELSE                                                   *
013800850118     C                   MOVE      TABCOD        MAGAZZ                         *
013900850118     C                   END                                                    *
014000990122     C     KEYMG1        SETLL     tmsmp01L                           91  92    *
014100850118     C   91                                                                     *
014200850118     CORN92              GOTO      RICMAG                                       *
014300850118     C     M             ADD       1             M                    71        *
014400850118     C                   MOVE      TABCOD        MAG(M)                         *
014500850118     C     M             CABEQ     100           AVANTI                         *
014600850118     C                   GOTO      RICMAG                                       *
014700850118     C     AVANTI        TAG                                                    *
014800850118     C*---------- RICERCA TIPO UNITA DI MISURA :           *
014900850118     C                   Z-ADD     1             A                 1 0          *
015000850118     C     UMI120        LOOKUP    TUM(A)                                 92    *
015100850118     C*---------- ENTRATE/USCITE :                         *
015200850118     C   01              DO                                                     *
015300850118     C                   MOVE      'MP'          PROCED                         *
015400850118     C                   MOVE      '151'         TABGRU                         *
015500850118     C                   MOVE      *ZEROS        TABCOD                         *
015600850118     C                   MOVE      'E'           EU                             *
015700850118     C                   MOVE      DESPRC        TABCOD                         *
015800850118     C     KEYTAB        CHAIN     ANTAB00F                           85        *
015900850118     C   85              MOVE      *ALL'*'       DESENT                         *
016000850118     C  N85              MOVEL     TABDES        DESENT                         *ENTRATE
016100850118     C                   MOVE      'U'           EU                             *
016200850118     C                   MOVE      DESPRC        TABCOD                         *
016300850118     C     KEYTAB        CHAIN     ANTAB00F                           85        *
016400850118     C   85              MOVE      *ALL'*'       DESUSC                         *
016500850118     C  N85              MOVEL     TABDES        DESUSC                         *USCITE
016600850118     C                   END                                                    *
016700850118     C*---------- ATTIVO ARCHIVIO RICHIESTO :              *
016800861230     C     MAT120        COMP      NOVE                               101004    *ALL PRODOTTI
016900850118     C  N10CLS120        COMP      '999'                              111105    *ALL CLASSI
017000850118     C  N10
017100850118     CANN11SCL120        COMP      '99999'                            121206    *ALL S/CLASSI
017200850118     C*----------                                          *
017300850118     C                   Z-ADD     0             X                 3 0          *
017400850118     C     RITENT        TAG                                                    *
017500850118     C     X             IFEQ      100                                          *
017600850118     C                   SETON                                            90    *
017700850118     C                   MOVEL     ERR(3)        MSGERR                         *
017800850118     C                   GOTO      FINE                                         *
017900850118     C                   END                                                    *
018000850118     C   04
018100850118     CAN 05
018200850118     CAN 06              SETON                                            10    *
018300990122     C   10              OPEN      tmamp01L                             90      *
018400990122     C   11              OPEN      tmamp09L                             90      *
018500990122     C   12              OPEN      tmamp10L                             90      *
018600850118     C   90X             ADD       1             X                              *
018700850118     C   90              GOTO      RITENT                                       *
018800850118     C*---------- RICERCA MAGAZZINI -----------------------*
018900850118     C     1             DO        M             X                              *
019000850118     C                   SETOFF                                           71    *
019100850118     C                   MOVE      MAG(X)        MAGAZZ                         *
019200850118     C                   MOVE      '153'         TABGRU                         *
019300850118     C                   MOVE      *BLANKS       TABCOD                         *
019400850118     C                   MOVE      MAGAZZ        TABCOD                         *
019500850118     C     KEYTAB        CHAIN     ANTAB00F                           91        *
019600850118     C   91              GOTO      ENDMAG                                       *
019700850118     C                   MOVEL     TABDES        TB153                          *
019800871014     C                   MOVE      *BLANKS       OLDMAT           12            *
019900850118     C                   MOVE      *BLANKS       OLDCLS            3            *
020000850118     C                   MOVE      *BLANKS       OLDSCL            5            *
020100880104     C                   Z-ADD     53            CONTA             3 0          *
020200850118     C                   Z-ADD     0             PAGE2             3 0          *
020300850205     C                   SETOFF                                           20    *
020400850118     C                   EXSR      RUTMAT                                       *
020500850118     C     ENDMAG        TAG                                                    *
020600850118     C                   END                                                    *
020700850118     C*---------- NON TROVATI SALDI :                      *
020800850118     C  N72              SETON                                            90    *
020900850118     C   90              MOVEL     ERR(4)        MSGERR                         *
021000850118     C*----------------------------------------------------*
021100850118     C     FINE          TAG                                                    *
021200850118     C   90              MOVEL     'KPJB207'     KMSMN                          *
021300850118     C                   SETON                                            LR    *
021400850118     C***------------------------------------------------***
021500850118     C**--------- LETTURA ANAGRAFICO ARTICOLI ------------**
021600850118     C***------------------------------------------------***
021700850118     CSR   RUTMAT        BEGSR                                                  *
021800850118     C   10
021900990122     CANN04MAT120        SETLL     tmamp01L                           90        *
022000850118     C   10
022100990122     CAN 04OLDMAT        SETLL     tmamp01L                           90        *
022200890228     C   11
022300990122     CANN05CLS120        SETLL     tmamp09L                           90        *
022400890228     C   11
022500990122     CAN 05OLDCLS        SETLL     tmamp09L                           90        *
022600890228     C   12
022700990122     CANN06SCL120        SETLL     tmamp10L                           90        *
022800890228     C   12
022900990122     CAN 06OLDSCL        SETLL     tmamp10L                           90        *
023000850118     C   90              GOTO      ENDMAT                                       *
023100850118     C                   MOVE      'XXXXXXX'     OLDMAT                         *
023200861230     C                   MOVEL     'XXXXXXX'     OLDMAT                         *
023300850118     C                   MOVE      'XXX'         OLDCLS                         *
023400850118     C                   MOVE      'XXXXX'       OLDSCL                         *
023500850118     C     RICMAT        TAG                                                    *
023600850118     C   10
023700850118     CAN 04              READ      ALLART                                 91    *
023800850118     C   10
023900850118     CANN04MAT120        READE     ALLART                                 91    *
024000850118     C   11
024100850118     CAN 05              READ      CLSART                                 91    *
024200850118     C   11
024300850118     CANN05CLS120        READE     CLSART                                 91    *
024400850118     C   12
024500850118     CAN 06              READ      SCLART                                 91    *
024600850118     C   12
024700850118     CANN06SCL120        READE     SCLART                                 91    *
024800850118     C   91
024900850118     CAN 20              EXSR      RUTTOT                                       *
025000850118     C   91              GOTO      ENDMAT                                       *
025100850118     C*---------- CONTROLLO ROTTURA PRODOTTO :             *
025200861230     C     MAT120        IFNE      NOVE                                         *
025300850118     C     MAT120        CABNE     AMPMAT        RICMAT                         *
025400850118     C                   END                                                    *
025500850118     C     AMPMAT        IFNE      OLDMAT                                       *
025600850118     C                   MOVE      AMPMAT        OLDMAT                         *
025700880104     C                   MOVEL     AMPDMT        DESART           21            *
025800880104     C                   MOVEL     AMPDMT        DESMAT            8
025900850118     C                   Z-ADD     0             TEN                            *
026000850118     C                   Z-ADD     0             TUS                            *
026100850118     C                   Z-ADD     0             TDS                            *
026200850118     C                   SETOFF                                           20    *
026300850118     C                   END                                                    *
026400860530     C*---------- CONTROLLO ROTTURA CLASSE :               *
026500860530     C     CLS120        IFNE      '999'                                        *
026600860530     C     CLS120        CABNE     AMPCLM        RICMAT                         *
026700860530     C                   END                                                    *
026800850118     C**--------- LETTURA ANAGRAFICO ARTICOLI ---------2--**
026900850118     C***------------------------------------------------***
027000850118     C     AMPCLM        IFNE      OLDCLS                                       *
027100850118     C                   MOVE      '155'         TABGRU                         *
027200850118     C                   MOVE      *ZEROS        TABCOD                         *
027300850118     C                   MOVE      AMPCLM        TABCOD                         *
027400850118     C     KEYTAB        CHAIN     ANTAB00F                           92        *
027500850118     C  N92              MOVEL     TABDES        DESCLS           25            *
027600850118     C   92              MOVE      *ALL'*'       DESCLS                         *
027700850118     C                   MOVE      AMPCLM        OLDCLS                         *
027800850118     C                   END                                                    *
027900850118     C*---------- CONTROLLO ROTTURA S/CLASSE :             *
028000850118     C     SCL120        IFNE      '99999'                                      *
028100850118     C     SCL120        CABNE     AMPSCM        RICMAT                         *
028200850118     C                   END                                                    *
028300850118     C     AMPSCM        IFNE      OLDSCL                                       *
028400850118     C                   MOVE      '156'         TABGRU                         *
028500850118     C                   MOVE      *ZEROS        TABCOD                         *
028600850118     C                   MOVE      AMPSCM        TABCOD                         *
028700850118     C     KEYTAB        CHAIN     ANTAB00F                           92        *
028800850118     C  N92              MOVEL     TABDES        DESSCL           25            *
028900850118     C   92              MOVE      *ALL'*'       DESSCL                         *
029000850118     C                   MOVE      AMPSCM        OLDSCL                         *
029100850118     C                   END                                                    *
029200850118     C*---------- IMPOSTO UNITA DI MISURA :                *
029300850118     C     UMI120        COMP      'A'                                    85    *UNITA MISURA
029400850118     C   85              MOVE      AMPUMA        UMIS              3            *ANAGRAFICA
029500000000     C   85              Z-ADD     1             FCVF             11 5    86    *
029600850118     C     UMI120        COMP      'Q'                                    85    *           A
029700850118     C   85              MOVE      AMPUMQ        UMIS                           *ACQUISTO
029800000000     C   85              Z-ADD     AMPFCQ        FCVF                     86    *
029900850118     C     UMI120        COMP      'S'                                    85    *           A
030000850118     C   85              MOVE      AMPUMS        UMIS                           *STOCCAGGIO
030100000000     C   85              Z-ADD     AMPFCS        FCVF                     86    *
030200850118     C     UMI120        COMP      'V'                                    85    *           A
030300850118     C   85              MOVE      AMPUMV        UMIS                           *VENDITA
030400000000     C   85              Z-ADD     AMPFCV        FCVF                     86    *
030500850118     C  N20              MOVE      UMIS          UMIT              3            *
030600860530     C                   SETOFF                                           76    *
030700990122     C     KEYMAG        CHAIN     tmsmp01L                           91        *
030800850118     C  N91              EXSR      RUTCAL                                       *
030900860530     C  N91
031000860530     CAN 76              GOTO      RICMAT                                       *
031100850121     C  N91
031200850121     CAN 73                                                                     *
031300850121     CORN91
031400850121     CAN 74              SETON                                        20        *
031500850121     C  N91
031600850121     CAN 20UMIS          IFNE      UMIT                                         *
031700850118     C                   MOVE      '*?*'         UMIT                           *
031800850118     C                   SETON                                            99    *NO TOTALI
031900850118     C                   END                                                    *
032000850118     C                   GOTO      RICMAT                                       *
032100850118     CSR   ENDMAT        ENDSR                                                  *
032200850118     C**--------- ROUTINE CALCOLO -----------------------***
032300850118     C***------------------------------------------------***
032400850118     CSR   RUTCAL        BEGSR                                                  *
032500850121     C                   SETOFF                                       7374      *
032600860522     C*---------- GIACENZA --------------------------------*
032700850118     C   01              DO                                                     *GIACENZA
032800850118     C  N86EN            DIV(H)    FCVF          EN                             *
032900850118     C  N86US            DIV(H)    FCVF          US                             *
033000850121     C                   XFOOT     EN            ENTRAT           11 27373      *
033100850121     C                   XFOOT     US            USCITE           11 27474      *
033200850121     C  N73
033300850121     CANN74              GOTO      ENDCAL                                       *
033400850118     C                   Z-ADD     EN            ENS                            *
033500850118     C                   Z-ADD     US            USS                            *
033600850118     C     TEN           ADD       EN            TEN                            *ENTRATE
033700850118     C     TUS           ADD       US            TUS                            *USCITE
033800850121     C     ENTRAT        SUB       USCITE        GIACEN            8 1          *
033900860522     C*---------- CONTROLLO SALDO MINIMO :                 *
034000860530     C     GIACEN        CABGT     SAL120        ENDCAL               76        *
034100850118     C                   END                                                    *
034200860522     C*---------- DISPONIBILITA ---------------------------*
034300850118     C   02              DO                                                     *DISPONIBILITA
034400850118     C  N86DS            DIV(H)    FCVF          DS                             *
034500850121     C                   XFOOT     DS            DISPON           11 27373      *
034600850121     C  N73              GOTO      ENDCAL                                       *
034700850121     C     TDS           ADD       DS            TDS                            *DISPON.
034800880113     C     DS(5)         SUB       DS(4)         DISPON
034900880113     C     DISPON        SUB       DS(3)         DISPON
035000880113     C*DISPONIBILITA=GIACENZA-ASSEGNATO-ASSEGNATO PROVA
035100860522     C*---------- CONTROLLO SALDO MINIMO :                 *
035200860530     C     DISPON        CABGT     SAL120        ENDCAL               76        *
035300850118     C                   END                                                    *
035400860522     C*----------------------------------------------------*
035500850118     C                   EXSR      RUTSTA                                       *
035600850118     C                   SETON                                          7172    *
035700850121     CSR   ENDCAL        ENDSR                                                  *
035800850118     C**--------- RUTINE EMISSIONE STAMPA ---------------***
035900850118     C***------------------------------------------------***
036000850118     CSR   RUTSTA        BEGSR                                                  *
036100880104     C     CONTA         IFGE      53                                           *
036200850118     C                   EXCEPT    TESTA                                        *
036300880104     C   01              EXCEPT    TESTA1                                       *
036400880104     C                   Z-ADD     0             CONTA                    75    *
036500880104     C                   END                                                    *
036600850118     C                   EXCEPT    DETAIL                                       *
036700880104     C                   ADD       1             CONTA
036800850121     C                   SETOFF                                           75    *
036900850118     CSR                 ENDSR                                                  *
037000850118     C**--------- RUTINE EMISSIONE TOTALI ---------------***
037100850118     C***------------------------------------------------***
037200850118     CSR   RUTTOT        BEGSR                                                  *
037300850118     C  N99              DO                                                     *
037400850118     C                   SETON                                            98    *
037500850118     C   01              Z-ADD     TEN           EN                             *
037600850118     C   01              Z-ADD     TUS           US                             *
037700850118     C   02              Z-ADD     TDS           DS                             *
037800850118     C                   SETON                                            86    *
037900850118     C                   EXSR      RUTCAL                                       *
038000850118     C                   END                                                    *
038100850118     C                   SETOFF                                         9899    *
038200850121     C                   SETON                                            75    *
038300850118     CSR                 ENDSR                                                  *
038400850118     O*--------------------------------------------------------------*
038500850118     O*     TESTATA                                                  *
038600850118     O*--------------------------------------------------------------*
038700850118     OMPPRTSTD  E            TESTA            01
038800850118     O                       SK1                132
038900850118     O                       DESDIT              20
039000850118     O                       UDATE         Y    118
039100850118     O                       PAGE          Z    132
039200850118     O          E            TESTA       1
039300850118     O                       SK2                132
039400850118     O               01      ERR(5)              93
039500850118     O               02      ERR(6)              93
039600850121     O                       AAA120             103
039700850118     O          E            TESTA       1
039800850118     O                       SK3                132
039900850118     O                       KNMUS              132
040000860522     O                                           93 'Saldo Minimo'
040100860522     O                       SAL120             101 '     0 '
040200850118     O          E            TESTA       1
040300850121     O                                           41 'Magazzino :'
040400850118     O                       MAGAZZ              49
040500850118     O                       DES153              80
040600880105     O                       UMI120             121
040700880105     O                                          122 '-'
040800880105     O                       DUM(A)             132
040900850121     O          E            TESTA       0  0
041000850121     O                       MAGAZZ              49
041100880105     O                       UMI120             121
041200880105     O                       DUM(A)             132
041300880104     O          E            TESTA       1
041400880104     O                       PAGE               132
041500850118     O               01      SK4                132
041600850118     O               02      SK9                132
041700880104     O          E       02   TESTA       1
041800880104     O                       SK8                132
041900880105     O          E            TESTA       1
042000880104     O                       PAGE               132
042100880104     O               01      SK5                132
042200880104     O               02      SK9                132
042300880105     O          E    01      TESTA1      0  0
042400880104     O                       PAGE               132
042500850118     O               01      SK5                132
042600880105     O               01      DE1                 32
042700880105     O               01      DE2                 42
042800880105     O               01      DE3                 52
042900880105     O               01      DE4                 62
043000880105     O               01      DE5                 72
043100880105     O               01      DU1                 82
043200880105     O               01      DU2                 92
043300880105     O               01      DU3                102
043400880105     O               01      DU4                112
043500880105     O               01      DU5                122
043600880105     O               01                         132 ' GIACENZA '
043700880105     O          E    01      TESTA1      1
043800880105     O                       SK6                132
043900850118     O*--------------------------------------------------------------*
044000850118     O*     DETTAGLIO   GIACENZA                                     *
044100850118     O*--------------------------------------------------------------*
044200850118     O          E    01      DETAIL      1
044300880104     O                       OLDMAT              12
044400880104     O                       DESMAT              21
044500880105     O                       ENS(1)        M     32
044600880105     O                       ENS(2)        M     42
044700880105     O                       ENS(3)        M     52
044800880105     O                       ENS(4)        M     62
044900880105     O                       ENS(5)        M     72
045000880105     O                       USS(1)        M     82
045100880105     O                       USS(2)        M     92
045200880105     O                       USS(3)        M    102
045300880105     O                       USS(4)        M    112
045400880105     O                       USS(5)        M    122
045500880105     O                       GIACEN        M    132
045600850118     O*--------------------------------------------------------------*
045700850118     O*     DETTAGLIO   DISPONIBILITA'                               *
045800850118     O*--------------------------------------------------------------*
045900850118     O          E    02      DETAIL      1
046000880104     O                       OLDMAT              13
046100880104     O                       DESART              36
046200880104     O                       DS(1)         K     52
046300880104     O                       DS(5)         K     68
046400880104     O                       DS(2)         K     84
046500880104     O                       DS(3)         K    100
046600880104     O                       DS(4)         K    116
046700880104     O                       DISPON        K    132
046800850118     O*--------------------------------------------------------------*
046900880104**    ERR                       ERRORI
047000850118*- Cod. DITTA non Valido - Fare inserimento     -*   Err,1
047100850118*- STAMPA non ESEGUITA - non trovati Magazzini  -*   ERR,2
047200850118*- STAMPA non ESEGUITA - Archivi non Disponibili-*   ERR,3
047300850118*- STAMPA non ESEGUITA - non Trovati SALDI - !! -*   ERR,4
047400850118*- Stampa Situazione Riepilogativa di Magazzino -*   ERR,5
047500850118*- Stampa Riepilogo Disponibilità  di Magazzino -*   ERR,6
047600850118**    TUM
047700850118AANAGRAFICA
047800850118QACQUISTO
047900850118SSTOCCAGGIO
048000850118VVENDITA
048100850118**   SK1
048200850118*-DESDIT-----------****-------------------------------------------SK01
048300850118-------------------------------------*** il XX/XX/XX      Pag. XXXSK01
048400850118**   SK2
048500850118                    ** Gest.Magazz. -M.P.- XXXXXXXXXXXXXXXXXXXXXXXSK02
048600990317XXXXXXXXXXXXXXXXXXXXXXXXXXX Anno 19xx **        Pgm.:   TII206R   SK02
048700850118**   SK3
048800850118                    ***-------------------------------------------SK03
048900860522-------------------------------------***        Utente  XXXXXXXXXXSK03
049000850118**   SK4
049100880105*----------------------------E N T R A T E----------------------**SK04
049200850118*---------------------U S C I T E---------------------*----------*SK04
049300880104**   SK5
049400880105*   PRODOTTO          *         *         *         *         *   SK05
049500880105      *         *         *         *         *         *        *SK05
049600880105**   SK6
049700880105*-----------------------------------------------------------------SK06
049800880105-----------------------------------------------------------------*SK06
049900880104**   SK8
050000880105*            PRODOTTO               *  In Ordine    *   Giacenza  SK08
050100880105  *  Ord. Cliente *   Assegnato   *  Ass.in Priva * Disponibilità SK08
050200850118**   SK9
050300880104*-----------------------------------*---------------*-------------SK09
050400880104--*---------------*---------------*----------------*-------------*SK09
