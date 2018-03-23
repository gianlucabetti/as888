000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     F*--------------------------------------------------------------*
000300020322     D KPJBA         E DS
000400071107     D fnlr48ds      E DS
000500071107     D trul90ds      E DS
000600071107     D DSARBD        E DS                  INZ
000700071107     D DSARBK        E DS                  INZ
000800071107     D DSARBG        E DS                  INZ
000900071107     D DARBT         E DS                  INZ
001000130930     D DSARBn        E DS                  INZ
001100980528     I***************************************************************
001200980528     C     *ENTRY        PLIST
001300130308     c                   parm                    kpjba
001400130930     c                   movel     kpjbu         ffr               1
001500130308     c
001600071107     c                   clear                   fnlr48ds
001700140416     C                   z-add     2014          D48AAS
001800130930     C                   z-add     001           D48LNP
001900071107     C                   z-add     00            D48NRS
002000140624     C                   z-add     460712        D48NSP
002100140624     C                   MOVEL     '1 '          D48CBO
002200140624     C                   MOVEL     'CS'          D48CVB
002300130308     C                   MOVEL     'E'           D48TLA
002400071107     C                   MOVEL     'A'           D48TBO
002500071107     C                   MOVEL     'S'           D48F12
002600140624     C                   MOVEL     'G'           D48TRC
002700130308     C                   MOVEL     ' '           D48ERR
002800130308     C                   MOVEL     *BLANK        D48MSG
002900130930     c                   movel     ffr           d48ffr
003000140624     c                   eval      §bgdcr=20140625
003100140624     c                   eval      §bgtc1='S'
003200071107     C*
003300071107     C                   MOVEL     FNLR48DS      KPJBU
003400071107     C                   CALL      'FNLR48R'
003500071107     C                   PARM                    KPJBA
003600071107     C                   PARM                    DSARBD
003700071107     C                   PARM                    DSARBK
003800071107     C                   PARM                    DARBT
003900071107     C                   PARM                    DSARBG
004000071107     C                   PARM                    trul90ds
004100131008     C                   PARM                    DSARBn
004200071107     C*
004300080805     c                   movel     kpjbu         fnlr48ds
004400020322     C*
004500020322     C                   SETON                                        LR
