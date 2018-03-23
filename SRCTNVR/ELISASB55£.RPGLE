000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     F*--------------------------------------------------------------*
000300020322     D KPJBA         E DS
000400130403     D tnsd99ds      E DS
000500980528     I***************************************************************
000600130403     c     *entry        plist
000700130403     c                   parm                    data              8
000800130403     c                   parm                    ora               4
000900130403     c                   clear                   tnsd99ds
001000130403     C                   MOVEL     ' '           D98TLA                         *TIPO LANCIO
001100130403     C                   MOVEL     'A'           D98TBO                         *TIPO BOLLA
001200130403     C                   Z-ADD     2013          D98AAS
001300130403     C                   Z-ADD     031           D98LNP
001400130403     C                   Z-ADD     00            D98NRS
001500130403     C                   Z-ADD     0058291       D98NSP
001600130403     C                   movel     data          D98DFC
001700130403     C                   movel     ora           D98OFC
001800130403     C                   CALL      'TNSD99R'
001900130403     C                   PARM                    TNSD99DS
002000020322     C*
002100130403     c     d98nci        dsply
002200020322     C                   SETON                                        LR
