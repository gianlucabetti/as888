000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090302     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400100316     Fwas7ifs0f o    e             disk
000500090302     F                                     infsr(*pssr)
000600991027
000700090302     D wrkesito        S              1A   inz
000800081210     D dataIso         S               D   datfmt(*iso)
000900081210     D dataEur         S               D   datfmt(*eur)
001000090302     D currKey         S              6S 0 inz
001100090302
001200081120
001300081120
001400991027
001500081120     C*
001600090324     C                   exsr      chkPAR
001700100316     C                   exsr      wriWAS7IFS
001800081210     C*
001900921023     C                   SETON                                        LR
002000090302
002100090302
002200090302
002300090324     C     chkPAR        BEGSR
002400090302     C*
002500090302     C                   if        %subst(%trim(pInPathIFS):
002600090302     C                                    %len(%trim(pInPathIFS)):1) <> '/'
002700090302     C                   eval      pInPathIFS = %trim(pInPathIFS) + '/'
002800090302     C                   endif
002900090302     C                   if        %subst(%trim(pInPathIFS):1:1) <> '/'
003000090302     C                   eval      pInPathIFS = '/' + %trim(pInPathIFS)
003100090302     C                   endif
003200090302     C*
003300090302     C                   if        %subst(%trim(pInExtIFSend):1:1) <> '.'
003400090302     C                   eval      pInExtIFSend = '.' + %trim(pInExtIFSend)
003500090302     C                   endif
003600090302     C*
003700090302     C                   ENDSR
003800090302
003900090302
004000090302
004100100316     C     wriWAS7IFS    begsr
004200090302     C*
004300100316     C                   clear                   was7ifs0
004400090306     C                   eval      ifsins    = datcor*1000000+oracor
004500090324     C                   eval      ifskey    = 'VAS_UPLOAD_' + %trim(pInHDL)
004600090324     C                   eval      ifspths   = %trim(pInPathIFS)
004700090324     C                   eval      ifsfils   = %trim(pInFilIFS)+
004800090324     C                                         %trim(pInExtIFSend)
004900170411     C                   eval      ifsopz    = 'DELN'
005000170411     C                   eval      ifstrytot = 100
005100100316     C                   write     was7ifs0
005200090302     C*
005300090302     C                   endsr
005400081210
005500081210
005600081210
005700081210     C     *pssr         BEGSR
005800081210     C*
005900081210     C                   eval      wrkesito = '2'
006000081210     C*
006100081210     C                   seton                                        LR
006200081210     C*
006300081210     C                   ENDSR     '*CANCL'
006400081210     C***
006500081210
006600070625
006700991027
006800991027      /TITLE Operazioni iniziali.
006900991027     C     *inzsr        BEGSR
007000991027     C*
007100991027     C     *ENTRY        PLIST
007200090324     C                   parm                    pInHDL           10
007300081210     C                   parm                    pInPathIFS      255
007400081210     C                   parm                    pInFilIFS       255
007500081210     C                   parm                    pInExtIFSend    255
007600081210     C     wrkesito      parm      wrkesito      pOutEsito         1
007700081210     C*
007800081210     C* Data di elaborazione
007900081210     C                   time                    w0140            14 0
008000081210     C                   move      w0140         w0080             8 0          *DATA (8) IN G/M/AA
008100090302     C                   movel     w0140         oracor            6 0          *ORA  (6) IN H/M/SS
008200081210     C     *eur          movel     w0080         dataeur
008300081210     C                   movel     dataeur       dataiso
008400090302     C     *iso          movel     dataiso       datcor            8 0
008500991027     C*
008600991027     C                   ENDSR
