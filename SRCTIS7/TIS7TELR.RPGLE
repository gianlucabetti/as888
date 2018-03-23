000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300000629
000400090225     Ftivgdtmp  uF   E             DISK
000500090225     Ftivawwwt  O    E             DISK
000600991027
000700991027     D prmppt          s             50
000800991027     D prmesito        s              1
000900991027     D wrkesito        s                   like(prmesito)
001000000629     D*----------
001100991027
001200991027     C                   exsr      traduci
001300991027     C                   seton                                        lr
001400991027
001500000629
001600000629     C     traduci       begsr
001700000629     C*
001800090225     C                   read      tivgdtmp
001900000629     C                   dow       not %eof
002000000629     C*
002100090225     C                   eval      vawDTA = vgdDTA
002200090225     C                   write     tivaw000
002300090225     C                   delete    tivgdtmp
002400000629     C*
002500090225     C                   read      tivgdtmp
002600000727     C                   enddo
002700000629     C*
002800000629     C                   eval      wrkesito = '0'
002900000629     C*
003000000727     C                   endsr
003100000629
003200991027
003300991027      /TITLE Operazioni iniziali.
003400991027     C     *inzsr        BEGSR
003500991027     C*
003600991027     C     *ENTRY        PLIST
003700991027     C                   parm                    prmppt
003800991027     C     wrkesito      parm      wrkesito      prmesito
003900991027     C*
004000991027     C                   ENDSR
