000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300000629
000400061012     Fldtxg100t uF   E             DISK
000500061012     Fldtxg1wwt O    E             DISK    rename(ldtxg:ldtxgw)
000600061012     Fldtxg100s O    E             DISK    rename(ldtxg:ldtxgs)
000700991027
000800991027     D prmppt          s             50
000900991027     D prmesito        s              1
001000991027     D wrkesito        s                   like(prmesito)
001100000629     D*----------
001200991027
001300991027     C                   exsr      traduci
001400991027     C                   seton                                        lr
001500991027
001600000629
001700000629     C     traduci       begsr
001800000629     C*
001900061012     C                   read      ldtxg100t
002000000629     C                   dow       not %eof
002100000629     C*
002200020722     C                   write     ldtxgw
002300020722     C                   write     ldtxgs
002400061012     C                   delete    ldtxg100t
002500000629     C*
002600061012     C                   read      ldtxg100t
002700000727     C                   enddo
002800000629     C*
002900000629     C                   eval      wrkesito = '0'
003000000629     C*
003100000727     C                   endsr
003200000629
003300991027
003400991027      /TITLE Operazioni iniziali.
003500991027     C     *inzsr        BEGSR
003600991027     C*
003700991027     C     *ENTRY        PLIST
003800991027     C                   parm                    prmppt
003900991027     C     wrkesito      parm      wrkesito      prmesito
004000991027     C*
004100991027     C                   ENDSR
