000100940224     H*--------------------------------------------------------------*
000200100623     FEDBRV00F  iF   E           K DISK
000300100623     FFIQEM01l  o    E           K DISK
000400100408      *---------------------------------------------------------------*
000500100408     D DUte01        E DS
000600100408      *---------------------------------------------------------------*
000700100416      *
000800100623     C                   read      EdBrv00F
000900100623     c                   dow       not %EoF(EdBrv00F)
001000100623     c                   clear                   fiqem000
001100100623     c                   z-add     BRVAAS        qemAAS
001200100623     c                   z-add     BRVLNP        qemLNP
001300100623     c                   z-add     BRVNRS        qemNRS
001400100623     c                   z-add     BRVNSP        qemNSP
001500100623     c                   z-add     BRVLNA        qemLNA
001600100623     c                   z-add     BRVFLS        qemFLS
001700100623     c                   z-add     BRVNSC        qemNSC
001800100623     c                   z-add     BRVNPS        qemNPS
001900100623     c                   z-add     BRVCCM        qemCCM
002000100623     c                   z-add     BRVDCS        qemDCS
002100100623     c                   z-add     BRVHCS        qemHCS
002200100623     c                   z-add     BRVZNC        qemZNC
002300100623     c                   z-add     BRVDFE        qemDFE
002400100623     c                   z-add     BRVNFE        qemNFE
002500100623     c                   movel     BRVSTS        qemSTS
002600100623     c                   movel     BRVFLG        qemFLG
002700100623     c                   movel     BRVCAN        qemCAN
002800100416      *
002900100416      *   aggiorna sempre
003000100623     c                   write     fiqem000
003100100416      *
003200100623     C                   read      EdBrv00F
003300100416     c                   end
003400100416      *
003500100408      *
003600100420     c     fine          tag
003700000000     C                   SETON                                        LR
003800030926      **-------------------------------------------------------------**
