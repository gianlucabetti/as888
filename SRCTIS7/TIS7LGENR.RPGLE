000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090325     H DFTACTGRP(*YES)
000300000629
000400070622     Fwp2gen00t uF   E             DISK
000500090323     F                                     commit
000600090325     F                                     infsr(*pssr)
000700070622     Fwp2genwwt O    E             DISK    rename(wp2gen:wp2genw) prefix(w_)
000800090323     F                                     commit
000900090325     F                                     infsr(*pssr)
001000070622     Fwp2gen00s O    E             DISK    rename(wp2gen:wp2gens)
001100090323     F                                     commit
001200090325     F                                     infsr(*pssr)
001300991027
001400991027     D prmppt          s             50
001500991027     D prmesito        s              1
001600991027     D wrkesito        s                   like(prmesito)
001700000629     D*----------
001800991027
001900991027     C                   exsr      traduci
002000090325     C                   commit
002100090325     C*
002200991027     C                   seton                                        lr
002300991027
002400000629
002500000629     C     traduci       begsr
002600000629     C*
002700070622     C                   read      wp2gen00t
002800000629     C                   dow       not %eof
002900000629     C*
003000070622     C                   movel     *zeros        wLenAlfa          4
003100070622     C                   z-add     *zeros        wLenNum           4 0
003200070622     C                   eval      wLenAlfa = %subst(prmppt:1:4)
003300070622     C                   move(p)   wLenAlfa      wLenNum
003400070622     C                   eval      w_TXDATI =  %subst(TXDATI:1:wLenNum)+X'00'
003500070622     C***                eval      w_TXDATI =  %subst(TXDATI:1:wLenNum)
003600070622     C*
003700070622     C                   write     wp2genw
003800070622     C                   write     wp2gens
003900070622     C                   delete    wp2gen00t
004000000629     C*
004100070622     C                   read      wp2gen00t
004200000727     C                   enddo
004300000629     C*
004400000629     C                   eval      wrkesito = '0'
004500000629     C*
004600000727     C                   endsr
004700090325
004800090325
004900090325
005000090325     C     *pssr         BEGSR
005100090325     C*
005200090325     C                   eval      wrkesito = '2'
005300090325     C                   rolbk
005400090325     C*
005500090325     C                   seton                                        LR
005600090325     C*
005700090325     C                   ENDSR     '*CANCL'
005800090325     C***
005900090325
006000090325
006100000629
006200991027
006300991027      /TITLE Operazioni iniziali.
006400991027     C     *inzsr        BEGSR
006500991027     C*
006600991027     C     *ENTRY        PLIST
006700991027     C                   parm                    prmppt
006800991027     C     wrkesito      parm      wrkesito      prmesito
006900991027     C*
007000991027     C                   ENDSR
