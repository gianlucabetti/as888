000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090910     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300000629
000400090904     Ftivgdtmp  uF   E             DISK
000500090325     F                                     infsr(*pssr)
000600090910     Fwp2vawwwt O    E             DISK    prefix(w_)
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
003000090918     C                   z-add     *zeros        wLenDati          4 0
003100090918     C                   eval      wLenDati = %len(vgdDTA)
003200100308     C                   if        wLenDati = *zeros
003300100308     C                   eval      w_TXDATI = X'00'
003400100308     C                   else
003500090918     C                   if        wLenNum > *zeros AND wLenNum <= wLenDati
003600090918     C                   eval      w_TXDATI = %subst(vgdDTA:1:wLenNum)+X'00'
003700090918     C                   else
003800090918     C                   eval      w_TXDATI = %subst(vgdDTA:1:wLenDati)+X'00'
003900090918     C                   endif
004000100308     C                   endif
004100070622     C*
004200090910     C                   write     wp2vaw
004300000629     C*
004400090904     C                   read      tivgdtmp
004500000727     C                   enddo
004600000629     C*
004700000629     C                   eval      wrkesito = '0'
004800000629     C*
004900000727     C                   endsr
005000090325
005100090325
005200090325
005300090325     C     *pssr         BEGSR
005400090325     C*
005500090325     C                   eval      wrkesito = '2'
005600090325     C                   rolbk
005700090325     C*
005800090325     C                   seton                                        LR
005900090325     C*
006000090325     C                   ENDSR     '*CANCL'
006100090325     C***
006200090325
006300090325
006400000629
006500991027
006600991027      /TITLE Operazioni iniziali.
006700991027     C     *inzsr        BEGSR
006800991027     C*
006900991027     C     *ENTRY        PLIST
007000991027     C                   parm                    prmppt
007100991027     C     wrkesito      parm      wrkesito      prmesito
007200991027     C*
007300991027     C                   ENDSR
