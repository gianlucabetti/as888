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
001600160519     d  prmFLE         s                   like(Dp3FLE)
001700160518     d  prmPRN         s                   like(Dp3PRN)
001800160519     d  prmEVD         s                   like(Dp3EVD)
001900040820     D*
002000040820     D WLBDAT          DS
002100040820     D  G02DAT                 1      8  0
002200040820     D  G02INV                 9     16  0
002300040820     D  G02ERR                17     17
002400040820     D  G02TGI                18     22  0
002500040820      *
002600160518     D WrkSqlCmd       S           1024
002700160518     D  data_Oggi      s                   like(G02INV) INZ(0)
002800040826     D  almeno_uno     s              1
002900040820      *
003000040820     D digits          C                   CONST('0123456789')
003100040820      *****************************************************************
003200160519      **
003300160519     C     *ENTRY        PLIST
003400160519     C                   parm                    prmAAS
003500160519     C                   parm                    prmLNP
003600160519     C                   parm                    prmNRS
003700160519     C                   parm                    prmNSP
003800160519     C                   parm                    prmDEV
003900160519     C                   parm                    prmHEV
004000160519     C                   parm                    prmFLE
004100160519     C                   parm                    prmPRN
004200160519     C                   parm                    prmEVD
004300160519      *
004400160519     C                   SETON                                        LR
004500160519      *
004600160519      *  Solo se richiamato da SCAN 03 o 13 MIC o DELIVERY
004700160519     c                   if        prmEVD = '03 ' or prmEVD = '13 '
004800160519     c                   else
004900160519     c                   RETURN
005000160519     c                   end
005100160519      *
005200160519      *  imposta UDATE di elaborazione
005300160519     C                   move      *date         G02DAT
005400160519     C                   MOVE      *ZEROS        G02INV
005500160519     C                   MOVE      *BLANKS       G02ERR
005600160519     C                   CALL      'XSRDA8'
005700160519     C                   PARM                    WLBDAT
005800160519     C                   Z-ADD     G02INV        data_oggi
005900160519     C                   move      data_oggi     day               8 0
006000160519     C                   TIME                    W0140            14 0
006100160519     C                   MOVEl     W0140         ore               6 0
006200160519      **
006300160519     c                   seton                                        66
006400160519     c                   clear                   prima_volta       1
006500160519     c                   clear                   almeno_uno
006600160519     c*
006700160519     c* compongo istruzione sql
006800160519     c                   exsr      istruz
006900160519     c*
007000011210     C/EXEC SQL
007100011210     C+ PREPARE S1 FROM :WrkSqlCmd
007200011210     C/END-EXEC
007300011210
007400011210     C/EXEC SQL
007500011210     C+ DECLARE A1 CURSOR FOR S1
007600011210     C/END-EXEC
007700011210
007800011210     C/EXEC SQL
007900011210     C+ OPEN A1
008000011210     C/END-EXEC
008100040820
008200040820     C                   DOU       SqlCod <> 0
008300011205     C/EXEC SQL
008400120508     C+ FETCH NEXT FROM A1 INTO :TIDP3DS, :DP3_nrr
008500011205     C/END-EXEC
008600011210     C                   SELECT
008700041007     c*
008800041007     c* a fine file Totali x rotture
008900041007     C                   WHEN      SqlCod = 100
009000120508     c*
009100041007     c                   if        almeno_uno ='S'
009200041007     c                   end
009300120510     c*
009400041007     c                   leave
009500120508      *
009600120508     **  x qualsiasi errore
009700041007     C                   WHEN      SqlCod < 0
009800120510      *
009900041007     C                   seton                                        H1
010000041007     c                   goto      fine
010100040820     c*
010200041007     C                   OTHER
010300120510      *
010400040820     c* DETTAGLIO
010500120508     c                   if        *in66
010600160518     c                   setoff                                       66
010700160518     c                   move      'S'           almeno_uno
010800040825     c                   eval      prima_volta = 'N'
010900040823     c                   end
011000120510     c*
011100160518     c*  aggiorna  lo SCAN 18
011200160518     C     DP3_nrr       chain     tiDP300f
011300160518     c                   if        %Found(tiDP300f)
011400160519     c                   eval       dp3fle = prmFLE
011500160519     c                   eval       dp3dev = prmDEV
011600160518     c                   eval       dp3hev = prmHEV
011700160518     c                   eval       dp3prn = prmPRN
011800160518     c                   update    tidp3000
011900160518     c                   end
012000040823      *
012100011210     C                   ENDSL
012200040820
012300011205     C                   ENDDO
012400160519      *
012500160519     c     fine          tag
012600160519      *
012700011205     C/EXEC SQL
012800011205     C+ CLOSE A1
012900011205     C/END-EXEC
013000011205     c*
013100040820     C* totali generali
013200041007     c                   if        almeno_uno ='S'
013300041007     c                   end
013400040823      *
013500160519     c                   RETURN
013600040820     C**************************************************************************
013700040820     C* mi compongo l'istruzione sql
013800040820     C**************************************************************************
013900040820     C     istruz        BEGSR                                                  *
014000040820      **
014100120508     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file
014200120508      **
014300040820     C                   EVAL      WrkSqlCmd
014400040820     C                             =
014500120508     C                             'SELECT tidp300f.*, rrn(tidp300f) '
014600120508     c                              + ' FROM tidp300f WHERE '
014700160518     c                             + ' DP3AAS = ' + %editc(prmAAS:'X') + ' AND'
014800160518     c                             + ' DP3LNP = ' + %editc(prmLNP:'X') + ' AND'
014900160518     c                             + ' DP3NRS = ' + %editc(prmNRS:'X') + ' AND'
015000160518     c                             + ' DP3NSP = ' + %editc(prmNSP:'X') + ' AND'
015100160518     c                             + ' DP3DTR = 0 and DP3EVD =''18 '' and '
015200160518     c                             + ' DP3DEV > 20340101 AND '
015300160518     c                             + '(DP3CEV=''S25'' or DP3CEV=''E25'')'
015400160519     c                             + ' FOR READ ONLY '
015500040820      **
015600040820     C                   ENDSR
015700120510     C**************************************************************************
