000100050118     H DECEDIT('0,') DATEDIT(*YMD.)
000200021214      *
000300050404     Ffnarbv0r  iF   E             DISK
000400050404     Ftitas30c  iF   E           K DISK
000500021214     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000600021214      *--------------------------------------------------
000700050323     c*
000800050404     C                   read      fnarbv0r
000900050404    1c                   DOW       not %EOF(fnarbv0r)
001000050404     c     karb          chain     titas30c
001100050404     c                   if        %found(titas30c)
001200050404     c                   if        taslna=400
001300050404     c                   except    bolla
001400050404     c                   endif
001500050404     c                   endif
001600050404     C                   read      fnarbv0r
001700050404     c                   enddo
001800021216      *
001900021214     C                   eval       *inlr = *on
002000021214      *---------------------------------------------------------
002100021214     c     *inzsr        BEGSR
002200021214      *
002300050118     C     Karb          KLIST
002400050404     C                   KFLD                    arbaas
002500050404     C                   KFLD                    arblnp
002600050404     C                   KFLD                    arbnrs
002700050404     C                   KFLD                    arbnsp
002800021216     C*
002900021214     C                   ENDSR
003000021214      *---------------------------------------------------------
003100050404     OQSYSPRT   E            bolla       1  1
003200050329     O                                              'SPED.'
003300050404     O                       arbAAS            +  2
003400050404     O                       arbLNP            +  1
003500050404     O                       arbNRS            +  1
003600050404     O                       arbNSP        Z   +  1
003700050404     O                       taslna            +  1
003800050404     O                       tasmgs            +  1 '  /  '
003900050404     O                                         +  2 'TASDCM'
004000050404     O                       TASDCM            +  1 '    /  /  '
004100050404     O                                         +  2 'arbdtv'
004200050404     O                       arbdtv            +  1 '    /  /  '
