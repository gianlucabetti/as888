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
004200150121     C*
004300150121     C                   when      pInIndirizz ='4'
004400150121     C                   EVAL      pInNazione   = 'it'
004500150121     C                   EVAL      pInProvincia = 'bo'
004600150121     C                   EVAL      pInComune    = 'bologna'
004700150121     C                   EVAL      pInLocalita  = *blanks
004800150121     C                   EVAL      pInCAP       = '40100'
004900150121     C                   EVAL      pInVia       = 'via marconi, 12'
005000150121     C                   EVAL      pInCivico    = *blanks
005100150121     C                   EVAL      pInScore     = *blanks
005200140915     C*
005300140915     C                   when      pInIndirizz ='3'
005400140915     C                   EVAL      pInNazione   = 'it'
005500140915     C                   EVAL      pInProvincia = 'rn'
005600140915     C                   EVAL      pInComune    = 'bellaria'
005700140915     C                   EVAL      pInLocalita  = *blanks
005800140915     C                   EVAL      pInCAP       = '47814'
005900140915     C                   EVAL      pInVia       = 'via del lavoro 4  '
006000140915     C                   EVAL      pInCivico    = *blanks
006100140918     C                   EVAL      pInScore     = *blanks
006200140915     C*
006300140915     C                   endsl
006400140911     C*
006500140911     C                   CALL      'UBTL05R02'
006600140911     C                   PARM                    pInNazione
006700140911     C                   PARM                    pInProvincia
006800140911     C                   PARM                    pInComune
006900140911     C                   PARM                    pInLocalita
007000140911     C                   PARM                    pInCAP
007100140911     C                   PARM                    pInVia
007200140911     C                   PARM                    pInCivico
007300140918     C                   PARM                    pInScore
007400140917     C                   PARM                    pOutEsito
007500140917     C                   PARM                    pOutEsitoDes
007600140917     C                   PARM                    pOutTotFound
007700140917     C                   PARM                    pOutResults
007800140918     C*
007900140918     C                   movel     *blanks       video1            5
008000140918     C                   movel     *blanks       video2           24
008100140918     C                   movel     *blanks       video3            1
008200140918     C                   movel     *blanks       video4           22
008300140918     C                   movel     *blanks       wOut             52
008400140918     C*
008500140918     C                   eval      wOut = *all'-'
008600140918     C     wOut          dsply
008700140918     C                   eval      video1 = 'PRV: '
008800140918     C                   eval      video2 = pInProvincia
008900140918     C                   eval      video4 = pOutResults(1).Prov
009000140918     C                   eval      wOut = video1 + video2 + video3 + video4
009100140918     C     wOut          dsply
009200140918     C                   eval      video1 = 'COM: '
009300140918     C                   eval      video2 = pInComune
009400140918     C                   eval      video4 = pOutResults(1).Comune
009500140918     C                   eval      wOut = video1 + video2 + video3 + video4
009600140918     C     wOut          dsply
009700140918     C                   eval      video1 = 'LOC: '
009800140918     C                   eval      video2 = pInLocalita
009900140918     C                   eval      video4 = pOutResults(1).Localit
010000140918     C                   eval      wOut = video1 + video2 + video3 + video4
010100140918     C     wOut          dsply
010200140918     C                   eval      video1 = 'CAP: '
010300140918     C                   eval      video2 = pInCAP
010400140918     C                   eval      video4 = pOutResults(1).CAP
010500140918     C                   eval      wOut = video1 + video2 + video3 + video4
010600140918     C     wOut          dsply
010700140918     C                   eval      video1 = 'IND: '
010800140918     C                   eval      video2 = pInVia
010900140918     C                   eval      video4 = pOutResults(1).Via
011000140918     C                   eval      wOut = video1 + video2 + video3 + video4
011100140918     C     wOut          dsply
011200140918     C                   eval      video1 = 'CIV: '
011300140918     C                   eval      video2 = pInCivico
011400140918     C                   eval      video4 = pOutResults(1).Civico
011500140918     C                   eval      wOut = video1 + video2 + video3 + video4
011600140918     C     wOut          dsply
011700140918     C                   eval      video1 = 'REG: '
011800140918     C                   eval      video2 = *blanks
011900140918     C                   eval      video4 = pOutResults(1).Regione
012000140918     C                   eval      wOut = video1 + video2 + video3 + video4
012100140918     C     wOut          dsply
012200140918     C                   eval      video1 = 'LIV: '
012300140918     C                   eval      video2 = *blanks
012400140918     C                   eval      video4 = pOutResults(1).LivNorm
012500140918     C                   eval      wOut = video1 + video2 + video3 + video4
012600140918     C     wOut          dsply
012700140918     C                   eval      video1 = 'SCO: '
012800140918     C                   eval      video2 = *blanks
012900140918     C                   eval      video4 = pOutResults(1).Score
013000140918     C                   eval      wOut = video1 + video2 + video3 + video4
013100140918     C     wOut          dsply
013200140918     C                   eval      wOut = *all'-'
013300140918     C     wOut          dsply
013400140918     C*
013500140911     C                   seton                                        lr
