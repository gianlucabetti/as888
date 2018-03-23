000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200020619     H* TNVR28R *----------------------------------------------------*
000300020619     H* PULIZIA ARCHIVI LEGATI A FILE tntam
000400000000     H*--------------------------------------------------------------*
000500020619     FTNtam01L  iF   E           K DISK
000600020619     FTItpt01L  UF   E           K DISK
000700020619     FTItad04L  UF   E           K DISK
000800020619     FTItpd01L  UF   E           K DISK
000900020619     FTItgc01L  UF   E           K DISK
001000020325      *
001100020325     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001200020325      *
001300020325     D$Totale          s              5  0 inz
001400020705
001500020705     d kksc            s                   like(tamksc)
001600020705     d kctr            s                   like(tamctr)
001700020705     d kprg            s                   like(tamprg)
001800020705
001900921027     C*****************************************************************
002000020705
002100020705     c     ktar          klist
002200020705     c                   kfld                    kksc
002300020705     c                   kfld                    kctr
002400020705     c                   kfld                    kprg
002500020325      *
002600020325     C                   Except    TESVIS
002700020619      * Titad
002800020619     C                   move      *off          $Prima
002900020619     C     *LOVAL        SETLL     TItad04L
003000020619     C                   READ      TItad04L                               30
003100020128      * 30 on - Fine lettura
003200020128     C     *IN30         DOWEQ     '0'
003300020128      *
003400020705     c                   eval      kksc = tadksc
003500020705     c                   eval      kctr = tadctr
003600020705     c                   eval      kprg = tadprg
003700020705
003800020705     C     ktar          CHAIN     TNtam01L                           31
003900020128     C     *IN31         IFEQ      '1'
004000020619     C   OF              Except    TESVIS
004100020619     C   OF              setoff                                       OF
004200020619      *
004300020619     C     $Prima        ifeq      *off
004400020619     C                   Except    TEStad
004500020619     C                   move      *on           $Prima            1
004600020619     C                   endif
004700020619      *
004800020619     C                   Except    DELtad
004900020325     C                   Add       1             $Totale
005000020619     C                   DELETE    TItad000
005100020128     C                   ENDIF
005200020128      *
005300020619     C                   READ      TItad04L                               30
005400020128     C                   ENDDO
005500020325     C     $Totale       ifgt      0
005600020325     C   OF              Except    TESVIS
005700020325     C   OF              setoff                                       OF
005800020619     C                   Except    TEStad
005900020325     C                   Except    TOTALE
006000020325     C                   z-add     *zeros        $Totale
006100020325     C                   endif
006200020128      *
006300020128      * TiOpt
006400020327     C                   move      *off          $Prima
006500020619     C     *LOVAL        SETLL     TItpT01L
006600020619     C                   READ      TItpT01L                               30
006700020128      * 30 on - Fine lettura
006800020128     C     *IN30         DOWEQ     '0'
006900020128      *
007000020705     c                   eval      kksc = tptksc
007100020705     c                   eval      kctr = tptctr
007200020705     c                   eval      kprg = tptprg
007300020705
007400020705     C     ktar          CHAIN     tntam01L                           31
007500020128     C     *IN31         IFEQ      '1'
007600020325     C   OF              Except    TESVIS
007700020325     C   OF              setoff                                       OF
007800020327      *
007900020327     C     $Prima        ifeq      *off
008000020619     C                   Except    TEStPT
008100020327     C                   move      *on           $Prima
008200020327     C                   endif
008300020327      *
008400020619     C                   Except    DELtPT
008500020325     C                   Add       1             $Totale
008600020619     C                   DELETE    TItpt01L
008700020128     C                   ENDIF
008800020128      *
008900020619     C                   READ      TItpt01L                               30
009000020128     C                   ENDDO
009100020325     C     $Totale       ifgt      0
009200020325     C   OF              Except    TESVIS
009300020325     C   OF              setoff                                       OF
009400020619     C                   Except    TEStpt
009500020325     C                   Except    TOTALE
009600020325     C                   z-add     *zeros        $Totale
009700020325     C                   endif
009800020128      *
009900020128      * TiOpd
010000020619     C                   move      *off          $Prima
010100020619     C     *LOVAL        SETLL     TItPD01L
010200020619     C                   READ      TItPD01L                               30
010300020128      * 30 on - Fine lettura
010400020128     C     *IN30         DOWEQ     '0'
010500020128      *
010600020705     c                   eval      kksc = tpdksc
010700020705     c                   eval      kctr = tpdctr
010800020705     c                   eval      kprg = tpdprg
010900020705
011000020705     C     ktar          CHAIN     TNtam01L                           31
011100020128     C     *IN31         IFEQ      '1'
011200020619     C   OF              Except    TESVIS
011300020619     C   OF              setoff                                       OF
011400020619      *
011500020619     C     $Prima        ifeq      *off
011600020619     C                   Except    TEStpd
011700020619     C                   move      *on           $Prima
011800020619     C                   endif
011900020619      *
012000020619     C                   Except    DELtPd
012100020619      *
012200020325     C                   Add       1             $Totale
012300020619     C                   DELETE    TItPD01L
012400020128     C                   ENDIF
012500020128      *
012600020619     C                   READ      TItPD01L                               30
012700020128     C                   ENDDO
012800020325     C     $Totale       ifgt      0
012900020325     C   OF              Except    TESVIS
013000020325     C   OF              setoff                                       OF
013100020619     C                   Except    TEStpD
013200020325     C                   Except    TOTALE
013300020325     C                   z-add     *zeros        $Totale
013400020325     C                   endif
013500020128      *
013600020128      * TiOgc
013700020619     C                   move      *off          $Prima
013800020619     C     *LOVAL        SETLL     TItGC01L
013900020619     C                   READ      TItGC01L                               30
014000020128      * 30 on - Fine lettura
014100020128     C     *IN30         DOWEQ     '0'
014200020128      *
014300020705     c                   eval      kksc = tgcksc
014400020705     c                   eval      kctr = tgcctr
014500020705     c                   eval      kprg = tgcprg
014600020705
014700020705     C     ktar          CHAIN     TNtam01L                           31
014800020128     C     *IN31         IFEQ      '1'
014900020619     C   OF              Except    TESVIS
015000020619     C   OF              setoff                                       OF
015100020619      *
015200020619     C     $Prima        ifeq      *off
015300020619     C                   Except    TEStgc
015400020619     C                   move      *on           $Prima
015500020619     C                   endif
015600020619      *
015700020619     C                   Except    DELtgc
015800020325     C                   Add       1             $Totale
015900020619     C                   DELETE    TItGC01L
016000020128     C                   ENDIF
016100020128      *
016200020619     C                   READ      TItGC01L                               30
016300020128     C                   ENDDO
016400020325     C     $Totale       ifgt      0
016500020325     C   OF              Except    TESVIS
016600020325     C   OF              setoff                                       OF
016700020619     C                   Except    TEStGC
016800020325     C                   Except    TOTALE
016900020325     C                   z-add     *zeros        $Totale
017000020325     C                   endif
017100020111      *
017200020114     C                   SETON                                        LR
017300020114      *
017400020325     OQSYSPRT   E            TESVIS         2 02
017500020325     O                                         + 10 'CANCELLAZIONE RECORDS '
017600020619     O                                         +  1 'FILES LEGATI A TNTAM01L:'
017700020619     O                                         +  8 'TNVR28R1'
017800020325     O                       UDATE             +  5 '  /  /  '
017900020325     O                                         +  5 'PAG.'
018000020325     O                       PAGE1         Z
018100020325      *
018200020619     O          E            TEStad         1
018300020619     O                                         +  1 'TITAD00F:'
018400020619     O          E            DELtad         1
018500020619     O                       TadKSC            + 11
018600020619     O                       Tadctr            +  4
018700020619     O                       Tadprg            +  4
018800020325      *
018900020619     O          E            TEStpT         1
019000020619     O                                         +  1 'TITPT00F:'
019100020325      *
019200020619     O          E            DELtpT         1
019300020619     O                       TPTKSC            + 11
019400020619     O                       Tptctr            +  4
019500020619     O                       Tptprg            +  4
019600020325     O                       TPTDUV            +  4 '    /  /  '
019700020325      *
019800020619     O          E            TEStpD         1
019900020619     O                                         +  1 'TITPD00F:'
020000020619     O          E            DELtpd         1
020100020619     O                       TPDKSC            + 11
020200020619     O                       TpDctr            +  4
020300020619     O                       Tpdprg            +  4
020400020325      *
020500020619     O          E            TEStGC         1
020600020325     O                                         +  1 'TIOGC00F:'
020700020619     O          E            DELtgc         1
020800020619     O                       tgcKSC            + 11
020900020619     O                       tgcctr            +  4
021000020619     O                       tgcprg            +  4
021100020619     O                       tgcduv            +  4 '    /  /  '
021200020325      *
021300020325      *
021400020327     O          E            TOTALE         2
021500020325     O                                         +  1 'TOTALE  :'
021600020325     O                       $Totale           +  2
