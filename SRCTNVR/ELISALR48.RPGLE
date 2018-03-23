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
002000140416     C                   z-add     419203        D48NSP
002100140416     C                   MOVEL     '2 '          D48CBO
002200140416     C                   MOVEL     'I0'          D48CVB
002300130308     C                   MOVEL     'E'           D48TLA
002400071107     C                   MOVEL     'A'           D48TBO
002500071107     C                   MOVEL     'S'           D48F12
002600140416     C                   MOVEL     'D'           D48TRC
002700130308     C                   MOVEL     ' '           D48ERR
002800130308     C                   MOVEL     *BLANK        D48MSG
002900130930     c                   movel     ffr           d48ffr
003000140416     c                   eval      §bdrsd='A.C.M.A. S.P.A.'
003100140416     c                   eval      §bdind='VIA CRISTOFORO COLOMBO 1'
003200140416     c                   eval      §bdprd='BO'
003300140416     c                   eval      §bdcpi='08709190154'
003400140416     c                   eval      §bdtel='994665522'
003500140416     c                   eval      §bdref='sigra Qva'
003600140416     c                   eval      §bdcad ='40131   '
003700140416     c                   eval      §bdlod='BOLOGNA   '
003800140416     c                   eval      §bdFIN='C'
003900140416     c                   eval      §bdFFd='S'
004000071107     C*
004100071107     C                   MOVEL     FNLR48DS      KPJBU
004200071107     C                   CALL      'FNLR48R'
004300071107     C                   PARM                    KPJBA
004400071107     C                   PARM                    DSARBD
004500071107     C                   PARM                    DSARBK
004600071107     C                   PARM                    DARBT
004700071107     C                   PARM                    DSARBG
004800071107     C                   PARM                    trul90ds
004900131008     C                   PARM                    DSARBn
005000071107     C*
005100080805     c                   movel     kpjbu         fnlr48ds
005200020322     C*
005300020322     C                   SETON                                        LR
