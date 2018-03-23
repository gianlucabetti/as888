000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090325     H DFTACTGRP(*YES)
000300000629
000400090904     Ftivgdtmp  uF   E             DISK
000500090325     F                                     infsr(*pssr)
000600070622     Fwp2genwwt O    E             DISK    rename(wp2gen:wp2genw) prefix(w_)
000700090323     F                                     commit
000800090325     F                                     infsr(*pssr)
000900991027
001000991027     D prmppt          s             50
001100991027     D prmesito        s              1
001200991027     D wrkesito        s                   like(prmesito)
001300000629     D*----------
001400991027
001500991027     C                   exsr      traduci
001600090325     C                   commit
001700090325     C*
001800991027     C                   seton                                        lr
001900991027
002000000629
002100000629     C     traduci       begsr
002200000629     C*
002300090904     C                   read      tivgdtmp
002400090904     C                   dow       not %eof(tivgdtmp)
002500000629     C*
002600070622     C                   movel     *zeros        wLenAlfa          4
002700070622     C                   z-add     *zeros        wLenNum           4 0
002800070622     C                   eval      wLenAlfa = %subst(prmppt:1:4)
002900070622     C                   move(p)   wLenAlfa      wLenNum
003000090904     C                   eval      w_TXDATI =  %subst(vgdDTA:1:wLenNum)+X'00'
003100070622     C*
003200070622     C                   write     wp2genw
003300000629     C*
003400090904     C                   read      tivgdtmp
003500000727     C                   enddo
003600000629     C*
003700000629     C                   eval      wrkesito = '0'
003800000629     C*
003900000727     C                   endsr
004000090325
004100090325
004200090325
004300090325     C     *pssr         BEGSR
004400090325     C*
004500090325     C                   eval      wrkesito = '2'
004600090325     C                   rolbk
004700090325     C*
004800090325     C                   seton                                        LR
004900090325     C*
005000090325     C                   ENDSR     '*CANCL'
005100090325     C***
005200090325
005300090325
005400000629
005500991027
005600991027      /TITLE Operazioni iniziali.
005700991027     C     *inzsr        BEGSR
005800991027     C*
005900991027     C     *ENTRY        PLIST
006000991027     C                   parm                    prmppt
006100991027     C     wrkesito      parm      wrkesito      prmesito
006200991027     C*
006300991027     C                   ENDSR
