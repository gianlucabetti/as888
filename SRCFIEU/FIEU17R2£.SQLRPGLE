000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200020409     H**********************************************************************
000300120510     FTIDP300F  uF   E             DISK    infds(DP3ds)
000400040820     H**********************************************************************
000500120508     d DP3ds           ds
000600120508     d  DP3_nrr              397    400b 0
000700160518      *
000800160518     D TIDP3DS       e DS                  extname(TIDP300F)
000900120508      *
001000160518     d  prmAAS         s                   like(Dp3AAS)
001100160518     d  prmLNP         s                   like(Dp3LNP)
001200160518     d  prmNRS         s                   like(Dp3NRS)
001300160518     d  prmNSP         s                   like(Dp3NSP)
001400160518     d  prmDEV         s                   like(Dp3DEV)
001500160518     d  prmHEV         s                   like(Dp3HEV)
001600160518     d  prmPRN         s                   like(Dp3PRN)
001700160519     d  prmEVD         s                   like(Dp3EVD)
001800040820     D*
001900040820     D WLBDAT          DS
002000040820     D  G02DAT                 1      8  0
002100040820     D  G02INV                 9     16  0
002200040820     D  G02ERR                17     17
002300040820     D  G02TGI                18     22  0
002400040820      *
002500160518     D WrkSqlCmd       S           1024
002600160518     D  data_Oggi      s                   like(G02INV) INZ(0)
002700040826     D  almeno_uno     s              1
002800040820      *
002900040820     D digits          C                   CONST('0123456789')
003000040820      *****************************************************************
003100160519      **
003200160519     C     *ENTRY        PLIST
003300160519     C                   parm                    prmAAS
003400160519     C                   parm                    prmLNP
003500160519     C                   parm                    prmNRS
003600160519     C                   parm                    prmNSP
003700160519     C                   parm                    prmDEV
003800160519     C                   parm                    prmHEV
003900160519     C                   parm                    prmPRN
004000160519     C                   parm                    prmEVD
004100160519      *
004200160519     C                   SETON                                        LR
004300160519      *
004400160519      *  Solo se richiamato da SCAN 03 o 13 MIC o DELIVERY
004500160519     c                   if        prmEVD = '03 ' or prmEVD = '13 '
004600160519     c                   else
004700160519     c                   RETURN
004800160519     c                   end
004900160519      *
005000160519      *  imposta UDATE di elaborazione
005100160519     C                   move      *date         G02DAT
005200160519     C                   MOVE      *ZEROS        G02INV
005300160519     C                   MOVE      *BLANKS       G02ERR
005400160519     C                   CALL      'XSRDA8'
005500160519     C                   PARM                    WLBDAT
005600160519     C                   Z-ADD     G02INV        data_oggi
005700160519     C                   move      data_oggi     day               8 0
005800160519     C                   TIME                    W0140            14 0
005900160519     C                   MOVEl     W0140         ore               6 0
006000160519      **
006100160519     c                   seton                                        66
006200160519     c                   clear                   prima_volta       1
006300160519     c                   clear                   almeno_uno
006400160519     c*
006500160519     c* compongo istruzione sql
006600160519     c                   exsr      istruz
006700160519     c*
006800011210     C/EXEC SQL
006900011210     C+ PREPARE S1 FROM :WrkSqlCmd
007000011210     C/END-EXEC
007100011210
007200011210     C/EXEC SQL
007300011210     C+ DECLARE A1 CURSOR FOR S1
007400011210     C/END-EXEC
007500011210
007600011210     C/EXEC SQL
007700011210     C+ OPEN A1
007800011210     C/END-EXEC
007900040820
008000040820     C                   DOU       SqlCod <> 0
008100011205     C/EXEC SQL
008200120508     C+ FETCH NEXT FROM A1 INTO :TIDP3DS, :DP3_nrr
008300011205     C/END-EXEC
008400011210     C                   SELECT
008500041007     c*
008600041007     c* a fine file Totali x rotture
008700041007     C                   WHEN      SqlCod = 100
008800120508     c*
008900041007     c                   if        almeno_uno ='S'
009000041007     c                   end
009100120510     c*
009200041007     c                   leave
009300120508      *
009400120508     **  x qualsiasi errore
009500041007     C                   WHEN      SqlCod < 0
009600120510      *
009700041007     C                   seton                                        H1
009800041007     c                   goto      fine
009900040820     c*
010000041007     C                   OTHER
010100120510      *
010200040820     c* DETTAGLIO
010300120508     c                   if        *in66
010400160518     c                   setoff                                       66
010500160518     c                   move      'S'           almeno_uno
010600040825     c                   eval      prima_volta = 'N'
010700040823     c                   end
010800120510     c*
010900160518     c*  aggiorna  lo SCAN 18
011000160518     C     DP3_nrr       chain     tiDP300f
011100160518     c                   if        %Found(tiDP300f)
011200160518     c                   eval       dp3dev = prmDEV
011300160518     c                   eval       dp3hev = prmHEV
011400160518     c                   eval       dp3prn = prmPRN
011500160518     c                   update    tidp3000
011600160518     c                   end
011700040823      *
011800011210     C                   ENDSL
011900040820
012000011205     C                   ENDDO
012100011205     C/EXEC SQL
012200011205     C+ CLOSE A1
012300011205     C/END-EXEC
012400011205     c*
012500040820     C* totali generali
012600041007     c                   if        almeno_uno ='S'
012700041007     c                   end
012800040823      *
012900041007     c     fine          tag
013000160519     c                   RETURN
013100040820     C**************************************************************************
013200040820     C* mi compongo l'istruzione sql
013300040820     C**************************************************************************
013400040820     C     istruz        BEGSR                                                  *
013500040820      **
013600120508     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file
013700120508      **
013800040820     C                   EVAL      WrkSqlCmd
013900040820     C                             =
014000120508     C                             'SELECT tidp300f.*, rrn(tidp300f) '
014100120508     c                              + ' FROM tidp300f WHERE '
014200160518     c                             + ' DP3AAS = ' + %editc(prmAAS:'X') + ' AND'
014300160518     c                             + ' DP3LNP = ' + %editc(prmLNP:'X') + ' AND'
014400160518     c                             + ' DP3NRS = ' + %editc(prmNRS:'X') + ' AND'
014500160518     c                             + ' DP3NSP = ' + %editc(prmNSP:'X') + ' AND'
014600160518     c                             + ' DP3DTR = 0 and DP3EVD =''18 '' and '
014700160518     c                             + ' DP3DEV > 20340101 AND '
014800160518     c                             + '(DP3CEV=''S25'' or DP3CEV=''E25'')'
014900160519     c                             + ' FOR READ ONLY '
015000040820      **
015100040820     C                   ENDSR
015200120510     C**************************************************************************
