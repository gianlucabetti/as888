000100001121     H DECEDIT('0,') DATEDIT(*DMY.)
000200001121     H* TNVR04R *----------------------------------------------------*
000300000000     H*-----------------*                                            *
000400001121     H*         - RIPASSA IL SIMFEL AL CHIAMANTE           -         *
000500000000     H*--------------------------------------------------------------*
000600001121     D UT�DSE        E DS                  EXTNAME(UT�DSE0F)
000700001121     D CNCR80        E DS
000800900131     C*---------------------------------------------------------------*
000900001121     C     *ENTRY        PLIST
001000001121     C                   PARM                    FILELA            3
001100001121      *
001200001121     C                   Z-ADD     1             CODUT
001300001121     C                   CALL      'X�PARUT'
001400001121     C                   PARM                    UT�DSE
001500001121     C     CODUT         CABEQ     -1            FINE
001600001121     C*
001700001121     C                   MOVEL     REC80         CNCR80
001800001121     C                   MOVE      SIMFEL        FILELA
001900030606     C                   if        filela = '000'
002000030606     c                   move      SimPOU        FILELA
002100030606     c                   end
002200000616     C*
002300000000     C     FINE          TAG
002400000000     C                   SETON                                        LR
002500001023     C*-----------------------------------------------------*
