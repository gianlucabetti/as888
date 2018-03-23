000100990127     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990127     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H* OCO305R *-OC/38-*--------------------------------------------*
000400000000     H*-----------------*                                            *
000500870630     H*             - CONFERMA ASSEGNAZIONE SIMULATA -               *
000600000000     H*             ----------------------------------               *
000700000000     H*                                                      AZ.O305 *
000800990127     H*         FILE : tmmoc13L - tmsmp01L -                         *
000900000000     H*--------------------------------------------------------------*
001000990127     Ftmmoc13L  UF A E           K DISK
001100990127     Ftmsmp01L  UF   E           K DISK
001200871229     FOCPRTSTD  O    F  132        PRINTER USROPN
001300871222     D MPHDT1          DS
001400871222     D*--- DATA AREA N.MOVIMENTO MAGAZZINO :
001500871222     D  NUMMOV                 1      5  0
001600871222     D  FILL1                  6      8
001700871222     D  DATDTA                 9     14  0
001800871222     D  FILL2                 15     17
001900871222     D  ANNDTA                18     19  0
002000871222     D  FIL03                 20     50
002100871222     D  DGIO                  51     56  0
002200871222     D  FIL04                 57    100
002300871222     D  FILL5                101    300
002400871222     D  GIOTOL               301    302  0
002500871222     D  FILL6                303    512
002600870629     D ORDDS           DS
002700870930     D  MOCMAT                 1     12
002800870930     D  MOCMAG                13     19
002900870629     D PREORD          DS
003000870930     D  PREMAT                 1     12
003100870930     D  PREMAG                13     19
003200870629     D KPJBA         E DS
003300870629     I*--- PASSAGGIO PARAMETRI
003400870629     C*----------------------------------------------------*
003500870629     C     *ENTRY        PLIST                                                  *
003600870629     C                   PARM                    KPJBA                          *
003700870629     C******************************************************
003800990127     C*--- FILE SALDI MAGAZZINO M.P. "tmsmp02L"            *
003900870629     C     KEYSAL        KLIST                                                  *
004000990127     C                   KFLD                    ANNDTA4           4 0          *
004100870629     C                   KFLD                    PREMAG                         *
004200870629     C                   KFLD                    PREMAT                         *
004300870629     C*----------------------------------------------------*
004400870629     C*AZZERAMENTI
004500870629     C                   Z-ADD     0             TOTORD                         *
004600871222     C                   Z-ADD     0             TOTASS           11 2          *
004700870629     C******************************************************
004800871222     C     *DTAARA       DEFINE                  MPHDT1                         *
004900871222     C                   IN        MPHDT1
005000990127     c                   z-add     anndta        anndta4
005100990127     c                   if        anndta > 40
005200990127     c                   add       1900          anndta4
005300990127     c                   else
005400990127     c                   add       2000          anndta4
005500990127     c                   end
005600870629     C     SU            TAG
005700990127     C                   READ      tmmoc000                               02
005800870630     C   02              DO
005900871222     C     TOTORD        IFGT      0
006000870630     C                   EXSR      AGGSAL
006100871222     C                   END
006200870630     C                   GOTO      FINE
006300870630     C                   END
006400870630     C*ELABORA TUTTE LE RIGHE 'OR' DI TIPO RIGA 1 CON ASSEGNAZIONE
006500870630     C*IN PROVA MAGGIORE DI 0
006600870630     C     MOCFAS        CABNE     'OR'          SU
006700870630     C     MOCTRG        CABNE     '1'           SU
006800870629     C     MOCASS        CABEQ     0             SU
006900870629     C  N99              MOVEL     ORDDS         PREORD
007000870629     C  N99              SETON                                            99
007100870629     C     ORDDS         IFNE      PREORD
007200870629     C                   EXSR      AGGSAL
007300870629     C                   Z-ADD     0             TOTORD
007400870629     C                   Z-ADD     0             TOTASS
007500870629     C                   MOVEL     ORDDS         PREORD
007600870629     C                   END
007700870629     C     MOCFLS        IFEQ      'S'
007800870629     C                   EXSR      ASSORD
007900871222     C                   ELSE
008000870629     C                   EXSR      ASSPAR
008100870629     C                   END
008200870629     C                   GOTO      SU
008300870629     C     FINE          TAG
008400870629     C                   SETON                                        LR
008500870629     C******************************************************
008600870629     C*PER CAMBIO DI ARTICOLO AGGIORNO I SALDI DI MAGAZZINO            AZI
008700870629     C     AGGSAL        BEGSR
008800990127     C     KEYSAL        CHAIN     tmsmp000                           10
008900871229     C   10
009000871229     CANN88              OPEN      OCPRTSTD
009100871229     C   10
009200871229     CANN88              SETON                                        88
009300871222     C   10              EXCEPT    ERRORE
009400870629     C   10              GOTO      END1
009500870629     C  N10              DO
009600870629     C     SMPQPR        SUB       TOTORD        SMPQPR
009700870630     C     SMPQIP        SUB       TOTASS        SMPQIP
009800870630     C                   ADD       TOTASS        SMPQIM
009900990127     C                   UPDATE    tmsmp000
010000870629     C                   END
010100870629     C     END1          ENDSR
010200870629     C******************************************************
010300870629     C*SE LA RIGA E' A SALDO  ESEGUO L'ASSEGNAZIONE
010400870629     C*E AGGIORNO IL FILE
010500870629     C     ASSORD        BEGSR
010600870629     C                   MOVEL     'AS'          MOCFAS
010700870629     C                   ADD       MOCQTA        TOTORD           11 2
010800870630     C                   ADD       MOCASS        TOTASS
010900870629     C                   Z-ADD     MOCASS        MOCQTA
011000870629     C                   Z-ADD     0             MOCASS
011100990127     C                   UPDATE    tmmoc000
011200870629     C                   ENDSR
011300870629     C******************************************************
011400870629     C*SE LA RIGA NON E' SALDO  E LA QUANTITA' ASSEGNATA
011500870629     C*E' DIVERSA DALLA QUANTITA ORDINATA CREO UN ALTRA RIGA
011600870629     C* CHE E' IL RESIDUO DELLE DUE
011700870629     C     ASSPAR        BEGSR
011800870629     C*************************
011900870629     C                   Z-ADD     0             RIGORD
012000871222     C                   Z-ADD     0             RIGASS           11 2
012100870629     C                   Z-ADD     0             RIGTOT           11 2
012200870629     C                   Z-ADD     MOCQTA        RIGORD           11 2
012300870629     C                   Z-ADD     MOCASS        RIGASS
012400870629     C                   MOVEL     'AS'          MOCFAS
012500870629     C                   ADD       MOCASS        TOTASS
012600871222     C                   ADD       MOCASS        TOTORD
012700870629     C                   Z-ADD     MOCASS        MOCQTA
012800870629     C                   Z-ADD     0             MOCASS
012900990127     C                   UPDATE    tmmoc000
013000870629     C*AGGIORNAMENTO DELLA RIGA CHE RIMANE 'OR' PERCHE' C'E'
013100870629     C*RESIDUO
013200870629     C     RIGORD        SUB       RIGASS        RIGTOT
013300871222     C     RIGTOT        IFGT      0
013400870629     C                   Z-ADD     RIGTOT        MOCQTA
013500870629     C                   MOVEL     'R'           MOCTSR
013600870629     C                   MOVEL     *BLANKS       MOCFLS
013700870629     C                   MOVEL     'OR'          MOCFAS
013800990127     C                   WRITE     tmmoc000                             70
013900870629     C                   END
014000870629     C                   ENDSR
014100871222     C******************************************************
014200871222     OOCPRTSTD  E            ERRORE           01
014300871222     O                       UDATE         Y    118
014400871222     O                                           72 'CONFERMA ASSEGNAZIONE'
014500871222     O                                           81 'SIMULATA'
014600990317     O                                          132 'PGM:TII308R'
014700871222     O          E            ERRORE           03
014800871222     O                                           12 'MAGAZZINO'
014900871222     O                                           24 'MATERIALE'
015000871222     O                                           42 'TOT.ORDINATO'
015100871222     O                                           56 'TOT.ASSEGNATO'
015200871222     O          E            ERRORE      2
015300871222     O                       PREMAG              11
015400871222     O                       PREMAT              26
015500871222     O                       TOTORD        2     42
015600871222     O                       TOTASS        2     58
015700871222     O                                           85 'MANCA RECORDS SALDI'
