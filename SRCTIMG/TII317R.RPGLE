000100990203     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990203     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300990203     Ftmmoc12L  IF   E           K DISK
000400990203     Ftmsmp01L  IF   E           K DISK
000500990203     Ftmamp01L  IF   E           K DISK
000600000000     FQSYSPRT   O    F  132        PRINTER
000700880223     D OC              S              5  0 DIM(4)                               ORD.CLIENTE X SET
000800000000     D SOL             S              5  0 DIM(4)                               DA SOLLE. X SETT.
000900000000     D SET             S              6  0 DIM(4)                               SETIIMANE
001000990504     D DAT             S              8  0 DIM(4)                               DATE A VIDEO S
001100880223     D DAO             S              5  0 DIM(4)                               DA ORD. X SETT.
001200990203     D SET8            S              8  0 DIM(4)                               SETIIMANE
001300880223     D SC              S             66    DIM(10) CTDATA PERRCD(1)
001400880224     D GIO             S              2  0 DIM(12) CTDATA PERRCD(1)             GIORNI DEI MES
001500000000     D PARAM           DS
001600990203     D  AMELA                         4  0
001700990203     d  socpas                        3
001800000000     D KPJBA         E DS
001900000000     D  LIBSYS                99    107
002000000000     D  MSGERR               453    502
002100990914     D MPHDT1          DS
002200880223     D*-----DATA AREA NUMERATORE MOVIM. MAGAZZINO
002300880223     D  NUMDTA                 1      5  0
002400880223     D  FILL1                  6      8
002500880223     D  DATDTA                 9     14  0
002600880223     D  FILL2                 15     17
002700880223     D  ANNDTA                18     19  0
002800880223     D  FIL03                 20     50
002900880223     D  DGIO                  51     56  0
003000880223     D  FIL04                 57    100
003100880223     D  FILL5                101    300
003200880223     D  GIOTOL               301    302  0
003300880223     D  FILL6                303    512
003400990203     D soc001        E DS                  EXTNAME(xsoc001ds)
003500990203     D xsocds          DS          1000
003600990203     d dataiso         s               d   datfmt(*iso)
003700990203     d dataeur         s               d   datfmt(*eur)
003800990203      *-------------------------------------------------
003900000000     C     *ENTRY        PLIST
004000000000     C                   PARM                    KPJBA
004100990203     C                   MOVEL     KPJBU         PARAM
004200990203     C*---------- RICERCA DITTA :
004300990203     C                   MOVEL     'SOC001'      TIPXSC
004400990203     C                   MOVEL     *blank        SOCXSC
004500990203     C                   EXSR      REPSOC
004600990203     C     RTNXSC        IFNE      '1'
004700990203     C                   MOVEL     XSOCDS        SOC001
004800990203     C                   MOVEL     xscrgs        desdit           20
004900990203     c                   end
005000000000     C                   MOVEL     AMELA         SET(1)
005100000000     C                   MOVEL     AMELA         SET(2)
005200880223     C     AMELA         ADD       1             AMELA1            4 0
005300890203     C                   MOVE      AMELA1        MM1               2 0
005400881205     C     MM1           IFGT      12
005500881205     C                   ADD       100           AMELA1
005600881205     C                   MOVE      01            AMELA1
005700881205     C                   END
005800881205     C** + 1 ANNO
005900880223     C                   MOVEL     AMELA1        SET(3)
006000880223     C                   MOVEL     AMELA1        SET(4)
006100990916     C                   MOVE      AMELA         MM                2 0
006200990916     C                   MOVEL     GIO(MM)       DAT(2)
006300990916     C                   MOVE      GIO(MM)       set(2)
006400990916     C     MM            ADD       1             MM
006500990916     C     MM            IFEQ      13
006600990916     C                   Z-ADD     1             MM
006700990916     C                   END
006800990916     C** + 1 ANNO
006900990916     C                   MOVEL     GIO(MM)       DAT(4)
007000990916     C                   MOVE      GIO(MM)       set(4)
007100880223     C                   MOVE      15            SET(1)
007200880223     C                   MOVE      15            SET(3)
007300880223     C** SETTIMANE DELLA STATISTICA
007400880224     C                   DO        4             A
007500990504     c                   if        set(a) > 0
007600990504     c     *ymd          move      set(a)        dataiso
007700990504     c                   move      dataiso       dataeur
007800990504     c                   move      dataeur       dat(a)
007900990504     c                   else
008000990504     c                   z-add     0             dat(a)
008100990504     c                   end
008200880224     C                   END
008300880224     C** DATE PER STAMPA VIDSEO
008400880223     C                   Z-ADD     66            RIG
008500880223     C     *DTAARA       DEFINE                  MPHDT1                         *
008600880223     C                   IN        MPHDT1
008700880223     C     KSAL          KLIST                                                  *
008800990203     C                   KFLD                    ANNDTA4                        *
008900880223     C                   KFLD                    MAGAZZ                         *
009000880223     C                   MOVEL     '1'           MAGAZZ            7
009100990203     c                   z-add     anndta        anndta4           4 0
009200990203     c                   if        anndta > 40
009300990203     c                   add       1900          anndta4
009400990203     c                   else
009500990203     c                   add       2000          anndta4
009600990203     c                   end
009700990203     C     KSAL          SETLL     tmsmp01L                               01
009800880223     C  N01              GOTO      FINE
009900880223     C     START         TAG
010000990203     C     KSAL          READE     tmsmp01L                               02
010100880223     C   02              GOTO      FINE
010200880223     C                   Z-ADD     0             OF
010300880223     C                   Z-ADD     0             OC
010400880223     C                   Z-ADD     0             SCORTA
010500880223     C                   MOVEL     *BLANKS       DESMAT
010600880223     C                   Z-ADD     0             DIS               5 0
010700880223     C                   Z-ADD     SMPQOR        OF                5 0
010800880223     C** ORDINATO FORNITORE
010900880223     C                   EXSR      CALDIS
011000880223     C** CALCOLO DISPONIBILITA
011100880223     C                   EXSR      CAROC
011200880223     C** CARICA ORDINATO CLIENTE PER SETTIMANA
011300880223     C     KAMP          KLIST
011400880223     C                   KFLD                    SMPMAT
011500990203     C     KAMP          CHAIN     tmamp01L                           03
011600880223     C  N03              Z-ADD     AMPLVM        SCORTA            5 0
011700880223     C** SCORTA
011800880223     C                   MOVEL     AMPDMT        DESMAT           10
011900880223     C** DESCRIZIONE MATERIALE
012000880223     C                   XFOOT     OC            TOTOC             7 0
012100880223     C     SCORTA        IFEQ      0
012200880223     C     DIS           ANDEQ     0
012300880223     C     TOTOC         ANDEQ     0
012400880223     C     OF            ANDEQ     0
012500880223     C                   GOTO      START
012600880223     C                   END
012700880223     C** SE ORDINATO CLIENTE
012800880223     C**    ORDINATO FORNITORE
012900880223     C**    SCORTA
013000880223     C**    DISPONIBILITA
013100880223     C** ------------UGUALE A ZWERO SCARTO L'ARTICOLO
013200880223     C                   EXSR      CALFAB
013300880223     C                   GOTO      START
013400880223     C     FINE          TAG
013500880223     C                   SETON                                        LR
013600880223     C     CALDIS        BEGSR
013700880223     C**----------------------------------
013800880223     C** CALCOLO DISPONIBILITA REAEL DELL'ARTICO LO
013900880223     C**----------------------------------
014000880223     C     SMPQGF        SUB       SMPQIM        DIS
014100880223     C                   SUB       SMPQIP        DIS
014200880223     C                   ENDSR
014300880223     C     CAROC         BEGSR
014400880223     C**----------------------------------
014500880223     C**  CALCOLO ORDINATO CLIENTE PER SETTIMANA
014600880223     C** CONSIDERANDO LA DATA DI CONSEGNA PREVISTA
014700880223     C**----------------------------------
014800880223     C     KORC          KLIST
014900880223     C                   KFLD                    SMPMAT
015000990203     C     KORC          SETLL     tmmoc12L                               04
015100880223     C  N04              GOTO      ENDOC
015200880223     C     SUOC          TAG
015300990203     C     KORC          READE     tmmoc12L                               05
015400880223     C   05              GOTO      ENDOC
015500880223     C     MOCFAS        CABNE     'OR'          SUOC
015600880223     C** SOLO FASE OR
015700990203     c                   movea     *zeros        set8
015800990203     c                   if        set(1) > 0
015900990203     c     *ymd          test(d)                 set(1)                 66
016000990203     c  n66*ymd          move      set(1)        dataiso
016100990203     c  n66              move      dataiso       set8(1)
016200990203     c                   end
016300990203     C     MOCDCP        IFLE      SET8(1)
016400880223     C                   ADD       MOCQTA        OC(1)
016500880223     C                   GOTO      SUOC
016600880223     C                   END
016700990203     c                   if        set(2) > 0
016800990203     c     *ymd          test(d)                 set(2)                 66
016900990203     c  n66*ymd          move      set(2)        dataiso
017000990203     c  n66              move      dataiso       set8(2)
017100990203     c                   end
017200990203     C     MOCDCP        IFLE      SET8(2)
017300880223     C                   ADD       MOCQTA        OC(2)
017400880223     C                   GOTO      SUOC
017500880223     C                   END
017600990203     c                   if        set(3) > 0
017700990203     c     *ymd          test(d)                 set(3)                 66
017800990203     c  n66*ymd          move      set(3)        dataiso
017900990203     c  n66              move      dataiso       set8(3)
018000990203     c                   end
018100990203     C     MOCDCP        IFLE      SET8(3)
018200880223     C                   ADD       MOCQTA        OC(3)
018300880223     C                   GOTO      SUOC
018400880223     C                   END
018500990203     c                   if        set(4) > 0
018600990203     c     *ymd          test(d)                 set(4)                 66
018700990203     c  n66*ymd          move      set(4)        dataiso
018800990203     c  n66              move      dataiso       set8(4)
018900990203     c                   end
019000990203     C     MOCDCP        IFLE      SET8(4)
019100880223     C                   ADD       MOCQTA        OC(4)
019200880223     C                   GOTO      SUOC
019300880223     C                   END
019400880223     C                   GOTO      SUOC
019500880223     C     ENDOC         TAG
019600880223     C                   ENDSR
019700880223     C     CALFAB        BEGSR
019800880223     C**----------------------------------
019900880223     C** CALCOLO FABBISOGNO PER ARTCIOLO
020000880223     C**----------------------------------
020100880223     C                   Z-ADD     0             PREDAO            5 0
020200880223     C                   Z-ADD     0             DAO               5 0
020300880223     C                   Z-ADD     0             SOL               5 0
020400880223     C                   Z-ADD     0             OCC               5 0
020500880223     C                   Z-ADD     OF            OFCOM             5 0
020600880223     C                   Z-ADD     DIS           DISCOM            5 0
020700880223     C                   Z-ADD     0             TOTSOL            5 0
020800880223     C                   Z-ADD     0             TOTDAO            5 0
020900880225     C                   Z-ADD     SCORTA        SCOR              5 0
021000880223     C                   DO        4             A                 2 0
021100880225     C     OC(A)         ADD       SCOR          OCC
021200880225     C                   Z-ADD     0             SCOR
021300880224     C     OCC           IFGT      0
021400880223     C     OCC           IFLE      DISCOM
021500880223     C     DISCOM        SUB       OCC           DISCOM
021600880223     C                   ELSE
021700880223     C     OCC           SUB       DISCOM        OCC
021800880223     C                   Z-ADD     0             DISCOM
021900880223     C     OFCOM         IFGE      OCC
022000880223     C     OFCOM         SUB       OCC           OFCOM
022100880223     C                   Z-ADD     OCC           SOL(A)
022200880223     C                   Z-ADD     0             OCC
022300880223     C                   ELSE
022400880223     C     OCC           SUB       OFCOM         OCC
022500880223     C                   Z-ADD     OFCOM         SOL(A)
022600880223     C                   Z-ADD     0             OFCOM
022700880223     C                   Z-ADD     OCC           DAO(A)
022800880224     C                   END
022900880223     C                   END
023000880224     C                   END
023100880223     C                   Z-ADD     DAO(A)        PREDAO
023200880223     C                   ADD       DAO(A)        TOTDAO
023300880223     C                   ADD       SOL(A)        TOTSOL
023400880223     C                   END
023500880223     C                   ADD       1             RIG
023600880223     C     RIG           IFGT      60
023700880223     C                   EXCEPT    TEST
023800880223     C                   Z-ADD     5             RIG               2 0
023900880223     C                   END
024000880223     C                   EXCEPT    DETT
024100880223     C                   ENDSR
024200990203     C*----------------------------------------------------*
024300990203     C* Reperimento dati società
024400990203     C*----------------------------------------------------*
024500990203     C     REPSOC        BEGSR
024600990203     C*
024700990203     C                   CALLB     'XSOC'
024800990203     C                   PARM                    TIPXSC            6
024900990203     C                   PARM      socpas        SOCXSC            3
025000990203     C                   PARM                    CDSXSC            9 0
025100990203     C                   PARM                    MODXSC            3
025200990203     C                   PARM      *blanks       RTNXSC            1
025300990203     C                   PARM                    XSOCDS
025400990203     C                   PARM                    KPJBA
025500990203     C*
025600990203     C                   ENDSR
025700990203      *------------------------------------------------------
025800880223     OQSYSPRT   E            TEST             02
025900990203     O                       desdit              25
026000880225     O                                         + 20 '* STATISTICA FABBISOGNI '
026100880225     O                                              '*'
026200880225     O                       UDATE             + 10 '  /  /  '
026300880223     O          E            TEST             03
026400880223     O                       SC(1)               66
026500880223     O                       SC(2)              132
026600880223     O          E            TEST             04
026700880224     O                       SC(3)               66
026800880224     O                       SC(4)              132
026900990504     O                       DAT(1)              54 '  /  /    '
027000990504     O                       DAT(2)              68 '  /  /    '
027100990504     O                       DAT(3)              84 '  /  /    '
027200990504     O                       DAT(4)             101 '  /  /    '
027300880223     O          E            TEST             05
027400880224     O                       SC(5)               66
027500880224     O                       SC(6)              132
027600880224     O          E            TEST             06
027700880223     O                       SC(1)               66
027800880223     O                       SC(2)              132
027900880223     O          E            DETT        1
028000880224     O                       SC(7)               66
028100880224     O                       SC(8)              132
028200880224     O                       SMPMAT              12
028300880223     O                       DESMAT            +  1
028400880223     O                       DIS           Z   +  1
028500880223     O                       SCORTA        Z   +  1
028600880223     O                       OF            Z   +  1
028700880223     O                       OC(1)         Z   +  1
028800880223     O                       SOL(1)        Z   +  0
028900880223     O                       DAO(1)        Z   +  0
029000880223     O                       OC(2)         Z   +  1
029100880223     O                       SOL(2)        Z   +  0
029200880223     O                       DAO(2)        Z   +  0
029300880223     O                       OC(3)         Z   +  1
029400880223     O                       SOL(3)        Z   +  0
029500880223     O                       DAO(3)        Z   +  0
029600880223     O                       OC(4)         Z   +  1
029700880223     O                       SOL(4)        Z   +  0
029800880223     O                       DAO(4)        Z   +  0
029900880224     O                       TOTSOL        Z    117
030000880224     O                       TOTDAO        Z    130
030100880224**
030200880224-----------------------+-----+-----+-----+---------------+--------
030300880223-------+---------------+---------------+-------------------------+
030400880224                       '                 '               '
030500880224       '               '               '       T O T A L I       '
030600880223 MATERIALE             ' DIS 'SCOR.' O.F.' O.C.  SOL  ORD' O.C.  S
030700880224OL  ORD' O.C.  SOL  ORD' O.C.  SOL  ORD' DA SOLLEC.  DA ORDINARE '
030800880223                       '                 '               '
030900880224       '               '               '                         '
031000880224**
03110088022431
03120088022428
03130088022431
03140088022430
03150088022431
03160088022430
03170088022431
03180088022431
03190088022430
03200088022431
03210088022430
03220088022431
