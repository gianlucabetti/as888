000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300000629
000400000727     Fwp2stk00t uF   E             DISK
000500070823     Fwp2stkwwt O    E             DISK    rename(wp2stk:wp2stkw) prefix(w_)
000600070620     Fwp2stk00s O    E             DISK    rename(wp2stk:wp2stks)
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
001900000727     C                   read      wp2stk00t
002000000629     C                   dow       not %eof
002100070823     C*
002200070823     C                   movel     *zeros        wLenAlfa          4
002300070823     C                   z-add     *zeros        wLenNum           4 0
002400070823     C                   eval      wLenAlfa = %subst(prmppt:1:4)
002500070823     C*
002600070823     C* Se richiesta nei parametri gestione specifica lunghezza record out => gestisco
002700070823     C                   if        wLenAlfa <> *blanks
002800070823     C                   move(p)   wLenAlfa      wLenNum
002900070823     C                   eval      w_TXDATI =  %subst(TXDATI:1:wLenNum)+X'00'
003000070823     C                   else
003100070823     C                   eval      w_TXDATI =  TXDATI
003200070823     C                   endif
003300070823     C*
003400070620     C                   write     wp2stkw
003500070620     C                   write     wp2stks
003600000727     C                   delete    wp2stk00t
003700000629     C*
003800000727     C                   read      wp2stk00t
003900000727     C                   enddo
004000000629     C*
004100000629     C                   eval      wrkesito = '0'
004200000629     C*
004300000727     C                   endsr
004400000629
004500991027
004600991027      /TITLE Operazioni iniziali.
004700991027     C     *inzsr        BEGSR
004800991027     C*
004900991027     C     *ENTRY        PLIST
005000991027     C                   parm                    prmppt
005100991027     C     wrkesito      parm      wrkesito      prmesito
005200991027     C*
005300991027     C                   ENDSR
