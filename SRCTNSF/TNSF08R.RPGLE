000100110926     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) ACTGRP(QILE)
000200110926     H*PARMS BNDDIR(PJXBND PJCBND)
000300110926     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000400110926     H/TITLE        -- FATTURAZIONE AUTOMATICA --  PRIMA NOTA        *
000500110926     H*--------------------------------------------------------------*
000600110926     H**
000700110926     D PARAM           DS
000800110926     D  VIDDFI                        8  0
000900110926     D  VIDDFF                        8  0
001000110926     D  VIDDSP                        8  0
001100110926     D  VIDTFT                        1  0
001200110926     D  VIDTCL                        1  0
001300110926     D  VIDTSC                        1
001400110926     D  FIL                           3  0 dim(40)
001500110926     D  VIDRB                         2
001600110926     D  VIDDFT                        8  0
001700110926     D  VIDKSC                        7  0
001800110926     D  wsocieta                      3
001900110926     D  VIDDIV                        3
002000110926     D  VIDAGG                        1
002100110926     D  VIDSTA                        1
002200110926     D
002300110926     D KPJBA         E DS
002400110926     D ndc071ds      E DS                  inz
002500110926      *****************************************************************
002600110926     c     *ENTRY        PLIST
002700110926     c                   PARM                    KPJBA
002800110926     c                   MOVEL     *ZEROS        PARAM
002900110926     c                   MOVEL     KPJBU         PARAM
003000110926     C*
003100110926     C* contabilizzazione batch
003200110926     c                   reset                   ndc071ds
003300110926     C                   MOVE      wsocieta      SOCieta071
003400110926     C                   MOVE      KNRAZ         rifint071
003500110926     c                   move      *loval        dtregda071
003600110926     c                   move      *hival        dtrega071
003700110926     c                   move      *loval        subnumd071
003800110926     c                   move      *hival        subnuma071
003900110926     C                   MOVE      *off          solcont071
004000110926    >C                   MOVE      *off          interat071
004100110926     C                   move      *off          err071
004200110926     c                   move      *off          control071
004300110926     C                   MOVEL     NDC071DS      kpjbu
004400110926     C*
004500110926     C                   CALL      'NDC071C'
004600110926     C                   parm                    kpjba
004700110926     C                   movel     kpjbu         ndc071ds
004800110926     C*
004900110926     c                   seton                                        lr
