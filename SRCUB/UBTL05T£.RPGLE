000100140911
000200140917 CPY  /COPY GAITRASRC/SRCUB,UBTL05D02
000300140917     D*
000400140911     D   pInNazione    S              3A
000500140911     D   pInProvincia  S              4A
000600140911     D   pInComune     S            128A
000700140911     D   pInLocalita   S            128A
000800140911     D   pInCAP        S              9A
000900140911     D   pInVia        S            128A
001000140911     D   pInCivico     S             10A
001100140918     D   pInScore      S             10A
001200140917     D   pOutEsito     S              1A
001300140917     D   pOutEsitoDes  S            128A
001400140917     D   pOutTotFound  S              4S 0
001500140917     D   pOutResults   DS                  dim(10) likeds(dsOutUBTL05R02)       10 * 630 della DS
001600140917
001700140911
001800140915
001900140915     C     *entry        plist
002000140915     C                   parm                    pInIndirizz       1
002100140918     C*
002200140915     C                   select
002300140915     C                   when      pInIndirizz ='1'
002400140915     C                   EVAL      pInNazione   = 'it'
002500140915     C                   EVAL      pInProvincia = 'bo'
002600140915     C                   EVAL      pInComune    = 'Vedegheto'
002700140915     C                   EVAL      pInLocalita  = *blanks
002800140915     C                   EVAL      pInCAP       = '40060'
002900140915     C                   EVAL      pInVia       = 'via faggeto 164/b'
003000140915     C                   EVAL      pInCivico    = *blanks
003100140918     C                   EVAL      pInScore     = *blanks
003200140915     C*
003300140915     C                   when      pInIndirizz ='2'
003400140911     C                   EVAL      pInNazione   = 'it'
003500140911     C                   EVAL      pInProvincia = 'bo'
003600140911     C                   EVAL      pInComune    = 'bologna'
003700140911     C                   EVAL      pInLocalita  = *blanks
003800140911     C                   EVAL      pInCAP       = '40100'
003900140911     C                   EVAL      pInVia       = 'via d. creti, 21'
004000140911     C                   EVAL      pInCivico    = *blanks
004100140918     C                   EVAL      pInScore     = *blanks
004200140915     C*
004300140915     C                   when      pInIndirizz ='3'
004400140915     C                   EVAL      pInNazione   = 'it'
004500140915     C                   EVAL      pInProvincia = 'rn'
004600140915     C                   EVAL      pInComune    = 'bellaria'
004700140915     C                   EVAL      pInLocalita  = *blanks
004800140915     C                   EVAL      pInCAP       = '47814'
004900140915     C                   EVAL      pInVia       = 'via del lavoro 4  '
005000140915     C                   EVAL      pInCivico    = *blanks
005100140918     C                   EVAL      pInScore     = *blanks
005200140915     C*
005300140915     C                   endsl
005400140911     C*
005500140911     C                   CALL      'UBTL05R02'
005600140911     C                   PARM                    pInNazione
005700140911     C                   PARM                    pInProvincia
005800140911     C                   PARM                    pInComune
005900140911     C                   PARM                    pInLocalita
006000140911     C                   PARM                    pInCAP
006100140911     C                   PARM                    pInVia
006200140911     C                   PARM                    pInCivico
006300140918     C                   PARM                    pInScore
006400140917     C                   PARM                    pOutEsito
006500140917     C                   PARM                    pOutEsitoDes
006600140917     C                   PARM                    pOutTotFound
006700140917     C                   PARM                    pOutResults
006800140918     C*
006900140918     C                   movel     *blanks       video1            5
007000140918     C                   movel     *blanks       video2           24
007100140918     C                   movel     *blanks       video3            1
007200140918     C                   movel     *blanks       video4           22
007300140918     C                   movel     *blanks       wOut             52
007400140918     C*
007500140918     C                   eval      wOut = *all'-'
007600140918     C     wOut          dsply
007700140918     C                   eval      video1 = 'PRV: '
007800140918     C                   eval      video2 = pInProvincia
007900140918     C                   eval      video4 = pOutResults(1).Prov
008000140918     C                   eval      wOut = video1 + video2 + video3 + video4
008100140918     C     wOut          dsply
008200140918     C                   eval      video1 = 'COM: '
008300140918     C                   eval      video2 = pInComune
008400140918     C                   eval      video4 = pOutResults(1).Comune
008500140918     C                   eval      wOut = video1 + video2 + video3 + video4
008600140918     C     wOut          dsply
008700140918     C                   eval      video1 = 'LOC: '
008800140918     C                   eval      video2 = pInLocalita
008900140918     C                   eval      video4 = pOutResults(1).Localit
009000140918     C                   eval      wOut = video1 + video2 + video3 + video4
009100140918     C     wOut          dsply
009200140918     C                   eval      video1 = 'CAP: '
009300140918     C                   eval      video2 = pInCAP
009400140918     C                   eval      video4 = pOutResults(1).CAP
009500140918     C                   eval      wOut = video1 + video2 + video3 + video4
009600140918     C     wOut          dsply
009700140918     C                   eval      video1 = 'IND: '
009800140918     C                   eval      video2 = pInVia
009900140918     C                   eval      video4 = pOutResults(1).Via
010000140918     C                   eval      wOut = video1 + video2 + video3 + video4
010100140918     C     wOut          dsply
010200140918     C                   eval      video1 = 'CIV: '
010300140918     C                   eval      video2 = pInCivico
010400140918     C                   eval      video4 = pOutResults(1).Civico
010500140918     C                   eval      wOut = video1 + video2 + video3 + video4
010600140918     C     wOut          dsply
010700140918     C                   eval      video1 = 'REG: '
010800140918     C                   eval      video2 = *blanks
010900140918     C                   eval      video4 = pOutResults(1).Regione
011000140918     C                   eval      wOut = video1 + video2 + video3 + video4
011100140918     C     wOut          dsply
011200140918     C                   eval      video1 = 'LIV: '
011300140918     C                   eval      video2 = *blanks
011400140918     C                   eval      video4 = pOutResults(1).LivNorm
011500140918     C                   eval      wOut = video1 + video2 + video3 + video4
011600140918     C     wOut          dsply
011700140918     C                   eval      video1 = 'SCO: '
011800140918     C                   eval      video2 = *blanks
011900140918     C                   eval      video4 = pOutResults(1).Score
012000140918     C                   eval      wOut = video1 + video2 + video3 + video4
012100140918     C     wOut          dsply
012200140918     C                   eval      wOut = *all'-'
012300140918     C     wOut          dsply
012400140918     C*
012500140911     C                   seton                                        lr
