000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400080409     FTIVGDOLD  IF   E             DISK    PREFIX(O_) RENAME(TIVGD000:TIVGD_O)
000500080409     FTIVGD00F  UF A E             DISK
000600991027
000700090417     C                   do        1000
000800090417     C                   read      TIVGDOLD
000900090417     C                   enddo
001000090417     C*
001100060804     C                   exsr      traduci
001200060804     C                   seton                                        LR
001300991027
001400060804
001500991027     C     traduci       BEGSR
001600991027     C*
001700080409     C                   read      TIVGDOLD
001800080409     C                   dow       not %eof(TIVGDOLD)
001900060804     C*
002000060804     C* Valorizzo il buffer del file d output
002100090416     C***                eval      VGDDTA = %subst(O_VGDDTA:1:%size(O_VGDDTA))
002200090416     C                   eval      VGDDTA = O_VGDDTA
002300080409     C                   eval      VGDTIP = O_VGDTIP
002400080409     C                   eval      VGDKSU = O_VGDKSU
002500080409     C                   eval      VGDTSC = O_VGDTSC
002600080409     C                   eval      VGDDAT = O_VGDDAT
002700080409     C                   eval      VGDPGM = O_VGDPGM
002800080409     C                   eval      VGDPRG = O_VGDPRG
002900080409     C                   eval      VGDCNT = O_VGDCNT
003000080409     C                   eval      VGDSTO = O_VGDSTO
003100080409     C                   eval      VGDFLO = O_VGDFLO
003200930409     C*
003300080409     C                   write     TIVGD000
003400991027     C*
003500080409     C                   read      TIVGDOLD
003600080409     C                   enddo
003700991027     C*
003800910830     C                   ENDSR
