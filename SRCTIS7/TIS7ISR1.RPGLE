000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300000629
000400041130     Fwp2stk50t uF   E             DISK
000500041130     Fwp2stk5wt O    E             DISK    rename(xx2stk5:xx2stk5w)
000600041130     Fwp2stk50s O    E             DISK    rename(xx2stk5:xx2stk5s)
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
001900041130     C                   read      wp2stk50t
002000000629     C                   dow       not %eof
002100000629     C*
002200041130     C                   write     xx2stk5w
002300041130     C                   write     xx2stk5s
002400041130     C                   delete    wp2stk50t
002500000629     C*
002600041130     C                   read      wp2stk50t
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
