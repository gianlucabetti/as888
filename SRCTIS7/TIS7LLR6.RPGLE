000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300000629
000400011001     Fwp2liv60t uF   E             DISK
000500011001     Fwp2liv6wt O    E             DISK    rename(xx2liv6:xx2liv6w)
000600011001     Fwp2liv60s O    E             DISK    rename(xx2liv6:xx2liv6s)
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
001900011001     C                   read      wp2liv60t
002000000629     C                   dow       not %eof
002100000629     C*
002200011001     C                   write     xx2liv6w
002300011001     C                   write     xx2liv6s
002400011001     C                   delete    wp2liv60t
002500000629     C*
002600011001     C                   read      wp2liv60t
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
