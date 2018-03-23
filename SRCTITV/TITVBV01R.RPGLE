000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400150610     FTIVGD00F  O    E             DISK
000500030924
000600150610     D KPJBA         e ds
000700030325
000800030924     C* MAIN/
000900150610     C*
001000150610     C                   clear                   tivgd000
001100150610     C                   eval      vgdDTA = 'TEST PER HEALTH CHECK RADWARE'
001200150610     C                   eval      vgdTIP = '&1'
001300150610     C                   eval      vgdKSU = '0STERCHK'
001400150610     C                   eval      vgdTSC = 'WW'
001500150610     C                   eval      vgdDAT = datcor
001600150610     C                   eval      vgdPGM = 'TITVBV01R'
001700150610     C                   write     tivgd000
001800150610     C*
001900150610     C                   clear                   tivgd000
002000150610     C                   eval      vgdDTA = 'TEST PER HEALTH CHECK RADWARE'
002100150610     C                   eval      vgdTIP = '&2'
002200150610     C                   eval      vgdKSU = '0STERCHK'
002300150610     C                   eval      vgdTSC = 'WW'
002400150610     C                   eval      vgdDAT = datcor
002500150610     C                   eval      vgdPGM = 'TITVBV01R'
002600150610     C                   write     tivgd000
002700150610     C*
002800921023     C                   SETON                                        LR
002900031103
003000031103
003100991027
003200991027      /TITLE Operazioni iniziali.
003300991027     C     *inzsr        BEGSR
003400991027     C*
003500991027     C     *ENTRY        PLIST
003600150610     C                   parm                    KPJBA
003700040216     C*
003800150610     C* Reperisco la data corrente
003900150610     C                   z-add     *zeros        datcor            8 0
004000150610     C                   eval      datcor = %dec(%date() : *ISO)
004100991027     C*
004200991027     C                   ENDSR
