000100951009     H DECEDIT('0,') DATEDIT(*DMY.)
000200980625      * ---------------------------------------------------------
000300990820     fFNDCT00F  IP   E           K DISK
000400990820      * ---------------------------------------------------------
000500990820     D KPJBA         E DS
000600990820     D fidn11ds      E DS
000700980625      * ---------------------------------------------------------
000800990909     C     DCTATB        IFEQ      ' '
000900990909     C     DCTCCH        ANDNE     'RD'
001000990908    1c     dctdch        ifgt      *zeros
001100990908     c     dctdch        ANDLE     19990610
001200990908     C
001300990908     C     DCTFIL        OREQ      05
001400990908     c     dctnca        andeq     271
001500990908     c
001600990908     C     DCTFIL        OREQ      63
001700990908     c     dctnca        andeq     9
001800990908     c
001900990908     C     DCTFIL        OREQ      26
002000990908     c     dctnca        andeq     26
002100990909     c
002200990909     C     DCTFIL        OREQ      43
002300990909     c     dctnca        andeq     748
002400990909     c
002500990909     C     DCTFIL        OREQ      62
002600990909     c     dctnca        andeq     129
002700990909     c
002800990909     C     DCTFIL        OREQ      113
002900990909     c     dctnca        andeq     50
003000990909     c
003100990909     C     DCTFIL        OREQ      4
003200990909     c     dctnca        andeq     46
003300990909     c
003400990909     C     DCTFIL        OREQ      20
003500990909     c     dctnca        andeq     284
003600990909     c
003700990909     C     DCTFIL        OREQ      90
003800990909     c     dctnca        andeq     40
003900990909     c
004000990909     C     DCTFIL        OREQ      7
004100990909     c     dctnca        andeq     99
004200990909     c
004300990909     C     DCTFIL        OREQ      39
004400990909     c     dctnca        andeq     35
004500990909     c
004600990909     C     DCTFIL        OREQ      5
004700990909     c     dctnca        andeq     579
004800990909     c
004900990909     C     DCTFIL        OREQ      83
005000990909     c     dctnca        andeq     20
005100990909     c
005200990909     C     DCTFIL        OREQ      67
005300990909     c     dctnca        andeq     170
005400990909     c
005500990909     C     DCTFIL        OREQ      22
005600990909     c     dctnca        andeq     68
005700990909     c
005800990909     C     DCTFIL        OREQ      32
005900990909     c     dctnca        andeq     75
006000990909     c
006100990909     C     DCTFIL        OREQ      110
006200990909     c     dctnca        andeq     70
006300990909     c
006400990909     C     DCTFIL        OREQ      114
006500990909     c     dctnca        andeq     1
006600990909     c
006700990909     C     DCTFIL        OREQ      28
006800990909     c     dctnca        andeq     375
006900990909     c
007000990909     C     DCTFIL        OREQ      103
007100990909     c     dctnca        andeq     21
007200990909     c
007300990909     C     DCTFIL        OREQ      43
007400990909     c     dctnca        andeq     373
007500990909     c
007600990909     C
007700990820     c                   clear                   fidn11ds
007800990908     c                   movel     'F'           i11mod
007900990820     c                   z-add     dctaac        i11aac
008000990820     c                   z-add     dctfil        i11fil
008100990820     c                   z-add     dctnca        i11nca
008200990820     c                   call      'FIDN11R'
008300990820     c                   parm                    kpjba
008400990820     c                   parm                    fidn11ds
008500990908    2c                   ENDIF
008600990909    2c                   ENDIF
