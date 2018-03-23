000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400080409     FTIVGDOLD  IF   E             DISK    PREFIX(O_) RENAME(TIVGD000:TIVGD_O)
000500080409     FTIVGD00F  UF A E             DISK
000600090417
000700090417     d wCountRec       s              7  0 inz
000800991027
000900060804     C                   exsr      traduci
001000060804     C                   seton                                        LR
001100991027
001200060804
001300991027     C     traduci       BEGSR
001400991027     C*
001500080409     C                   read      TIVGDOLD
001600080409     C                   dow       not %eof(TIVGDOLD)
001700060804     C*
001800060804     C* Valorizzo il buffer del file d output
001900090416     C***                eval      VGDDTA = %subst(O_VGDDTA:1:%size(O_VGDDTA))
002000090416     C                   eval      VGDDTA = O_VGDDTA
002100080409     C                   eval      VGDTIP = O_VGDTIP
002200080409     C                   eval      VGDKSU = O_VGDKSU
002300080409     C                   eval      VGDTSC = O_VGDTSC
002400080409     C                   eval      VGDDAT = O_VGDDAT
002500080409     C                   eval      VGDPGM = O_VGDPGM
002600080409     C                   eval      VGDPRG = O_VGDPRG
002700080409     C                   eval      VGDCNT = O_VGDCNT
002800080409     C                   eval      VGDSTO = O_VGDSTO
002900080409     C                   eval      VGDFLO = O_VGDFLO
003000930409     C*
003100080409     C                   write     TIVGD000
003200090417     C*
003300090417     C                   add       1             wCountRec
003400090417     C                   if        wCountRec >= 1000
003500090417     C                   leave
003600090417     C                   endif
003700991027     C*
003800080409     C                   read      TIVGDOLD
003900080409     C                   enddo
004000991027     C*
004100910830     C                   ENDSR
