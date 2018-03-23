000100990422     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990422     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     F* MPH945R  *-MP/38-*-------------------------------------------*
000400000000     F*------------------*                                           *
000500000000     F*       - APERTURA NUOVO ESERCIZIO -                           *
000600860530     F*       - GENERAZIONE MOVIMENTI GIACENZA INIZIALE              *
000700000000     F*       ----------------------------                           *
000800000000     F*                                                   AZN.: H945 *
000900000000     F*--------------------------------------------------------------*
001000990422     FtmsMP01L  UF   E           K DISK
001100990422     F                                     RENAME(tmsMP000:tmsMP01)
001200990422     FtmsMP02L  UF A E           K DISK
001300990422     F                                     RENAME(tmsMP000:tmsMP02)
001400860103     FANTAB00F  IF   E           K DISK
001500990422     FtmlMP01L  IF   E           K DISK
001600990422     FtmmMP00F  O  A E             DISK
001700860103     FMPPRTSTD  O    F  132        PRINTER
001800860530     D*-----------
001900990422     D PZ              S             14  5 DIM(5)
002000860103     D*
002100860103     D TAB152        E DS                  EXTNAME(ANTAB152)
002200860103     D  EN                    30     34
002300860103     D  US                    39     43
002400860103     D  GE                    48     52
002500860103     D TAB153        E DS                  EXTNAME(ANTAB153)
002600000000     D KPJBA         E DS
002700990422     D  MSGERR               453    502
002800000000     D*-----PASSAGGIO PARAMETRI
002900990422     D ds945           DS
003000990422     D  vaa945                        4  0
003100990422     D  MAG945                        7
003200990422     D  Dtd945                        8  0
003300990422     D  Nrd945                        7  0
003400990422     D  naa945                        4  0
003500990422     D  Dtm945                        8  0
003600990422     D  Nrm945                        7  0
003700990422     D  LIS945                        3
003800990422     D  ind945                        1  0
003900990422     d  soc945                        3
004000990422     d dataiso         s               d   datfmt(*iso)
004100990422     d dataeur         s               d   datfmt(*eur)
004200990422     D soc001        E DS                  EXTNAME(xsoc001ds)
004300990422     D xsocds          DS          1000
004400860530     I*-----------
004500990422     ItmlMP000
004600860103     I              LMPVP1                      PZ(1)
004700860103     I              LMPVP2                      PZ(2)
004800860103     I              LMPVP3                      PZ(3)
004900860103     I              LMPVP4                      PZ(4)
005000860103     I              LMPVP5                      PZ(5)
005100000000     C*----------------------------------------------------*
005200000000     C     *ENTRY        PLIST                                                  *
005300000000     C                   PARM                    KPJBA                          *
005400990422     c                   movel     kpjbu         ds945
005500990422     C*---------- RICERCA DITTA :                          *
005600990422     C                   MOVEL     'SOC001'      TIPXSC
005700990422     C                   MOVEL     *blank        SOCXSC
005800990422     C                   EXSR      REPSOC
005900990422     C     RTNXSC        IFNE      '1'
006000990422     C                   MOVEL     XSOCDS        SOC001
006100990422     c                   end
006200990422     c                   time                    como14           14 0
006300990422     c                   movel     como14        woggi             8 0
006400860103     C*---------- ANAGRAFICO LISTINI
006500860103     C*                                                    *
006600860103     C     KLIS          KLIST
006700860103     C                   KFLD                    LIS945
006800990422     C                   KFLD                    SmpmAT
006900860103     C*
007000860103     C*--------------FILE SALDI MATERIE PRIME
007100860103     C     KSALDI        KLIST
007200860103     C                   KFLD                    NAA945
007300990422     C                   KFLD                    SmpmAT
007400990422     C                   KFLD                    SmpmAG
007500860103     C*
007600860103     C*---------- ANAGRAFICO TABELLE :                     *
007700860103     C     KEYTAB        KLIST                                                  *
007800860103     C                   KFLD                    TABGRU                         *
007900860103     C                   KFLD                    TABCOD                         *
008000860103     C*
008100860103     C*---- RICERCA TCODICE CAUSALE 'GIA'
008200860103     C                   MOVE      '152'         TABGRU                         *
008300860103     C                   MOVE      *ZEROS        TABCOD                         *
008400860103     C                   MOVEL     'MP'          CINQUE            5            *
008500860103     C                   MOVE      'GIA'         CINQUE            5            *
008600860103     C                   MOVE      CINQUE        TABCOD                         *
008700860103     C     KEYTAB        CHAIN     ANTAB00F                           92        *
008800860103     C   92              GOTO      ENDLR
008900860103     C                   MOVEL     TABDES        TAB152
009000860103     C                   MOVEL     US            COM10            10
009100860103     C                   MOVE      GE            COM10
009200860103     C                   MOVEL     EN            MMPOPC
009300860103     C                   MOVE      COM10         MMPOPC
009400860103     C*
009500860103     C                   Z-ADD     IND945        I                 1 0
009600860224     C                   Z-ADD     66            PAGE1             2 0
009700860103     C*
009800860103     C     MAG945        COMP      '9999999'                              90
009900860530     C  N90              DO
010000860103     C                   MOVE      '153'         TABGRU                         *
010100860103     C                   MOVE      MAG945        TABCOD                         *
010200860103     C     KEYTAB        CHAIN     ANTAB00F                           92        *
010300860103     C                   MOVEL     TABDES        TAB153
010400860103     C                   MOVEL     TIG153        MMPFGQ
010500860103     C                   MOVE      TIG153        MMPFGV
010600860103     C                   MOVE      TIM153        MMPFGT
010700860103     C                   END
010800860103     C*
010900860103     C*--- CAMPI COMUNI FILE MOVIMENTI DI MAGAZZINO MATERIE PRIME
011000990422     C                   move      xsccut        MMPUTE
011100990422     C                   Z-ADD     woggi         MMPDMN
011200860103     C                   Z-ADD     NAA945        MMPAAC
011300860103     C                   Z-ADD     DTM945        MMPDMV
011400860103     C                   Z-ADD     NRM945        MMPNMV
011500860103     C                   Z-ADD     DTD945        MMPDDC
011600860103     C                   Z-ADD     NRD945        MMPNDC
011700860103     C                   MOVEL     'GIA'         MMPCAU
011800860103     C                   MOVE      '9999999'     MMPCDC
011900860103     C                   Z-ADD     0             MMPCRG
012000860103     C*
012100860103     C*----------------------------------------------------*
012200990422     C     VAA945        SETLL     tmsMP01                                      *
012300000000     C*                                                    *
012400000000     C                   DO        *HIVAL                                       *
012500000000     C*                                                    *
012600990422     C     VAA945        READE     tmsMP01                                10    *
012700860103     C*
012800000000     C     *IN10         IFEQ      '0'                                          *
012900990422     C  N90SmpmAG        CABNE     MAG945        E0100
013000860103     C     SMPQGF        CABEQ     0             E0100
013100860109     C     SMPQGF        IFLT      0
013200860224     C     PAGE1         IFGE      60
013300860109     C                   EXCEPT    TESTA
013400860224     C                   Z-ADD     8             PAGE1
013500860109     C                   END
013600860109     C                   EXCEPT    ERR3
013700860109     C                   GOTO      E0100
013800860109     C                   END
013900860103     C   90              DO
014000860103     C                   MOVE      '153'         TABGRU                         *
014100860103     C                   MOVE      SMPMAG        TABCOD                         *
014200860103     C     KEYTAB        CHAIN     ANTAB00F                           92        *
014300860103     C                   MOVEL     TABDES        TAB153
014400860109     C     GIA153        CABNE     'S'           E0100
014500860103     C                   MOVEL     TIG153        MMPFGQ
014600860103     C                   MOVE      TIG153        MMPFGV
014700860103     C                   MOVE      TIM153        MMPFGT
014800860103     C                   END
014900990422     C     KLIS          CHAIN     tmlMP01L                           91
015000860530     C  N91              DO
015100860103     C     PZ(I)         IFEQ      0
015200860224     C     PAGE1         IFGE      60
015300860103     C                   EXCEPT    TESTA
015400860224     C                   Z-ADD     8             PAGE1
015500860103     C                   END
015600860103     C                   EXCEPT    ERR1
015700860103     C                   END
015800860530     C                   Z-ADD     PZ(I)         MMPPRZ
015900860530     C                   END
016000860530     C   91              DO
016100860224     C     PAGE1         IFGE      60
016200860103     C                   EXCEPT    TESTA
016300860224     C                   Z-ADD     8             PAGE1
016400860103     C                   END
016500860103     C                   EXCEPT    ERR2
016600860530     C                   Z-ADD     0             MMPPRZ
016700860103     C                   END
016800990422     C                   UPDATE    tmsMP01
016900860103     C                   MOVE      SMPMAG        MMPMAG
017000860103     C                   MOVE      SMPMAT        MMPMAT
017100860103     C                   MOVE      SMPUMA        MMPUMA
017200860103     C                   Z-ADD     SMPQGF        MMPQTA
017300860103     C                   MOVE      SMPMAG        MMPMCP
017400990422     C                   WRITE     tmmmp000                                     *
017500990422     C     KSALDI        CHAIN     tmsMP02                            93
017600860103     C                   Z-ADD     MMPQTA        SMPQGI
017700990422     C  N93              UPDATE    tmsMP02
017800860530     C   93              DO
017900990422     C                   Z-ADD     woggi         SMPDMN
018000990422     C                   move      xsccut        SMPUTE
018100860103     C                   Z-ADD     NAA945        SMPAAC
018200860103     C                   Z-ADD     0             SMPPLF
018300860103     C                   Z-ADD     0             SMPPLV
018400860103     C                   Z-ADD     0             SMPQOR
018500860103     C                   Z-ADD     0             SMPQPR
018600860103     C                   Z-ADD     0             SMPQIM
018700870518     C                   Z-ADD     0             SMPQIP
018800860103     C                   Z-ADD     0             SMPQGF
018900860103     C                   Z-ADD     0             SMPQE1
019000860103     C                   Z-ADD     0             SMPQE2
019100860103     C                   Z-ADD     0             SMPQE3
019200860103     C                   Z-ADD     0             SMPQE4
019300860103     C                   Z-ADD     0             SMPQE5
019400860103     C                   Z-ADD     0             SMPQU1
019500860103     C                   Z-ADD     0             SMPQU2
019600860103     C                   Z-ADD     0             SMPQU3
019700860103     C                   Z-ADD     0             SMPQU4
019800860103     C                   Z-ADD     0             SMPQU5
019900860103     C                   Z-ADD     0             SMPDMS
020000860103     C                   Z-ADD     0             SMPNMS
020100860103     C                   Z-ADD     0             SMPPMC
020200860103     C                   MOVE      *BLANK        SMPFLG
020300990422     C                   WRITE     tmsMP02
020400860103     C                   END
020500860103     C     E0100         TAG
020600000000     C                   END                                                    *
020700000000     C*                                                    *
020800000000     C  N10              END                                                    *
020900000000     C*                                                    *
021000860103     C     ENDLR         TAG
021100000000     C                   SETON                                        LR        *
021200990422     C*----------------------------------------------------*
021300990422     C* Reperimento dati società
021400990422     C*----------------------------------------------------*
021500990422     C     REPSOC        BEGSR
021600990422     C*
021700990422     C                   CALLB     'XSOC'
021800990422     C                   PARM                    TIPXSC            6
021900990422     C                   PARM      soc945        SOCXSC            3
022000990422     C                   PARM                    CDSXSC            9 0
022100990422     C                   PARM                    MODXSC            3
022200990422     C                   PARM      *blanks       RTNXSC            1
022300990422     C                   PARM                    XSOCDS
022400990422     C                   PARM                    KPJBA
022500990422     C*
022600990422     C                   ENDSR
022700000000     C*----------------------------------------------------*
022800860225     OMPPRTSTD  E            TESTA          2 01
022900860103     O                                           50 'ERRORI SU LISTINO X GENE'
023000860103     O                                           74 'RAZIONE MOVIMENTI GIACEN'
023100860103     O                                           85 'ZA INIZIALE'
023200990422     O                       woggi               15 '  /  /    '
023300990422     O                                           25 'TII214R'
023400860103     O                                          100 'PAG.'
023500860103     O                       PAGE          Z    105
023600860221     O                       KNMUS              120
023700860221     O                       KNSIF              132
023800860103     O          E            TESTA          2
023900860224     O                                           20 'LISTINO'
024000860103     O                                           35 'MATERIALE'
024100860224     O                                           47 'VERS.'
024200860224     O                                           60 'MAGAZZ.'
024300860103     O                                          100 'ERRORE'
024400860224     O          E            ERR1           1
024500860224     O                       LIS945              18
024600860224     O                       PAGE1               32
024700871021     O                       SMPMAT              39
024800860224     O                       SMPMAG              60
024900860103     O                                          107 '*** PREZZO = 0'
025000860224     O          E            ERR2           1
025100860224     O                       LIS945              18
025200860224     O                       PAGE1               32
025300871021     O                       SMPMAT              39
025400860224     O                       SMPMAG              60
025500860103     O                                          107 '*** NO LISTINO'
025600860224     O          E            ERR3           1
025700860224     O                       LIS945              18
025800860224     O                       PAGE1               32
025900871021     O                       SMPMAT              39
026000860224     O                       SMPMAG              60
026100860109     O                                          109 '*** QTA NEGATIVA'
026200860109     O                       SMPQGF        K    132
