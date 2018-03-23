000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300000629
000400050512     FWP2SMWGP0TuF   E             DISK
000500060828     FWP2LED001TO    E             DISK
000600050512     FWP2SMWGP0SO    E             DISK    rename(XX2SMWG:XX2SMWGS)
000700991027
000800060828     D ds_In         e ds                  extname(WP2SMWGP0T)
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200000629     D*----------
001300991027
001400991027     C                   exsr      traduci
001500991027     C                   seton                                        lr
001600991027
001700000629
001800000629     C     traduci       begsr
001900000629     C*
002000050512     C                   read      WP2SMWGP0T
002100000629     C                   dow       not %eof
002200000629     C*
002300060828     C                   eval      dati = ds_in
002400060828     C                   write     WP2LED0001
002500050512     C                   write     XX2SMWGS
002600050512     C                   delete    WP2SMWGP0T
002700000629     C*
002800050512     C                   read      WP2SMWGP0T
002900000727     C                   enddo
003000000629     C*
003100000629     C                   eval      wrkesito = '0'
003200000629     C*
003300000727     C                   endsr
003400000629
003500991027
003600991027      /TITLE Operazioni iniziali.
003700991027     C     *inzsr        BEGSR
003800991027     C*
003900991027     C     *ENTRY        PLIST
004000991027     C                   parm                    prmppt
004100991027     C     wrkesito      parm      wrkesito      prmesito
004200991027     C*
004300991027     C                   ENDSR
