000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200020409     H**********************************************************************
000300160519     FTIDP300F  o  a E             DISK    infds(DP3ds)
000400160519     Ffnblp01l  if   E           k DISK
000500040820     H**********************************************************************
000600120508     d DP3ds           ds
000700120508     d  DP3_nrr              397    400b 0
000800160518      *
000900160518     D TIDP3DS       e DS                  extname(TIDP300F)
001000120508      *
001100160518     d  prmAAS         s                   like(Dp3AAS)
001200160518     d  prmLNP         s                   like(Dp3LNP)
001300160518     d  prmNRS         s                   like(Dp3NRS)
001400160518     d  prmNSP         s                   like(Dp3NSP)
001500160518     d  prmDEV         s                   like(Dp3DEV)
001600160518     d  prmHEV         s                   like(Dp3HEV)
001700160519     d  prmFLE         s                   like(Dp3FLE)
001800160518     d  prmPRN         s                   like(Dp3PRN)
001900160519     d  prmEVD         s                   like(Dp3EVD)
002000040820     D*
002100040820     D WLBDAT          DS
002200040820     D  G02DAT                 1      8  0
002300040820     D  G02INV                 9     16  0
002400040820     D  G02ERR                17     17
002500040820     D  G02TGI                18     22  0
002600040820      *
002700160518     D WrkSqlCmd       S           1024
002800160518     D  data_Oggi      s                   like(G02INV) INZ(0)
002900040826     D  almeno_uno     s              1
003000040820      *
003100040820     D digits          C                   CONST('0123456789')
003200040820      *****************************************************************
003300160519      **
003400160519     C     *ENTRY        PLIST
003500160519     C                   parm                    prmAAS
003600160519     C                   parm                    prmLNP
003700160519     C                   parm                    prmNRS
003800160519     C                   parm                    prmNSP
003900160519     C                   parm                    prmDEV
004000160519     C                   parm                    prmHEV
004100160519     C                   parm                    prmFLE
004200160519     C                   parm                    prmPRN
004300160519     C                   parm                    prmEVD
004400160519      *
004500160519     c     KBLP          klist
004600160519     C                   kfld                    prmAAS
004700160519     C                   kfld                    prmLNP
004800160519     C                   kfld                    prmNRS
004900160519     C                   kfld                    prmNSP
005000160519      *
005100160519     C                   SETON                                        LR
005200160519      *
005300160519      * TRATTASI di IMPORT x segnalare se è un'ISOLA da pagare nel CELARING come tale.
005400160519      *
005500160519      *  Solo se richiamato da SCAN 03 e se su BLP è un'isola
005600160519      *    xché l'informazione deve essere mandata assieme allo SCAN 03 (MIC)
005700160519     c                   if        prmEVD = '03 '
005800160519      *
005900160519      * oppure se non trovato BLP o se pur essendoci NON è un'isola
006000160519     c     KBLP          chain     fnblp01l
006100160519     c                   if        not %found(fnblp01l) or
006200160519     c                                 %found(fnblp01l) and BLPfin<>'I'
006300160519     c                   RETURN
006400160519     c                   end
006500160519      *
006600160519     c                   else
006700160519     c                   RETURN
006800160519     c                   end
006900160519      *
007000160519      *  imposta UDATE di elaborazione
007100160519     C                   move      *date         G02DAT
007200160519     C                   MOVE      *ZEROS        G02INV
007300160519     C                   MOVE      *BLANKS       G02ERR
007400160519     C                   CALL      'XSRDA8'
007500160519     C                   PARM                    WLBDAT
007600160519     C                   Z-ADD     G02INV        data_oggi
007700160519     C                   move      data_oggi     day               8 0
007800160519     C                   TIME                    W0140            14 0
007900160519     C                   MOVEl     W0140         ore               6 0
008000160519      **
008100160519     c                   seton                                        66
008200160519     c                   clear                   prima_volta       1
008300160519     c                   clear                   almeno_uno
008400160519     c*
008500160519     c* compongo istruzione sql
008600160519     c                   exsr      istruz
008700160519     c*
008800011210     C/EXEC SQL
008900011210     C+ PREPARE S1 FROM :WrkSqlCmd
009000011210     C/END-EXEC
009100011210
009200011210     C/EXEC SQL
009300011210     C+ DECLARE A1 CURSOR FOR S1
009400011210     C/END-EXEC
009500011210
009600011210     C/EXEC SQL
009700011210     C+ OPEN A1
009800011210     C/END-EXEC
009900040820
010000040820     C                   DOU       SqlCod <> 0
010100011205     C/EXEC SQL
010200120508     C+ FETCH NEXT FROM A1 INTO :TIDP3DS, :DP3_nrr
010300011205     C/END-EXEC
010400011210     C                   SELECT
010500041007     c*
010600160519      * NON deve essere presente uno SCAN 18 con ISL già inviato
010700041007     C                   WHEN      SqlCod = 100
010800160519      *
010900160519     c                   if        almeno_uno =  *blank
011000160519      *
011100160519     c*  aggiorna  lo SCAN 18
011200160519     c                   clear                   tidp3000
011300160519     c                   eval       dp3aas = prmAAS
011400160519     c                   eval       dp3lnp = prmLNP
011500160519     c                   eval       dp3nrs = prmNRS
011600160519     c                   eval       dp3nsp = prmNSP
011700160519     c                   eval       dp3prn = prmPRN
011800160519     c                   eval       dp3evd = '18 '
011900160519     c                   eval       dp3cev = 'ISL'
012000160519     c                   eval       dp3fle = prmFLE
012100160519     c                   eval       dp3dev = prmDEV
012200160519     c                   eval       dp3hev = prmHEV
012300160519     c                   eval       DP3DTSPE = blpAAS *10000 + blpMGS
012400160519     c                   write     tidp3000
012500160519      *
012600041007     c                   end
012700160519      *
012800041007     c                   leave
012900120508      *
013000120508     **  x qualsiasi errore
013100041007     C                   WHEN      SqlCod < 0
013200120510      *
013300041007     C                   seton                                        H1
013400041007     c                   goto      fine
013500160519      *
013600041007     C                   OTHER
013700120510      *
013800160519      * Se c'è deve uscire dal programma in quanto già inviato precedentemente
013900040820     c* DETTAGLIO
014000160519     c                   RETURN
014100160519      *
014200160519      *               non lo esegue esce prima dal pgm
014300120508     c                   if        *in66
014400160518     c                   setoff                                       66
014500160518     c                   move      'S'           almeno_uno
014600040825     c                   eval      prima_volta = 'N'
014700040823     c                   end
014800120510     c*
014900011210     C                   ENDSL
015000040820
015100011205     C                   ENDDO
015200160519      *
015300160519     c     FINE          tag
015400160519      *
015500011205     C/EXEC SQL
015600011205     C+ CLOSE A1
015700011205     C/END-EXEC
015800011205     c*
015900040820     C* totali generali
016000041007     c                   if        almeno_uno ='S'
016100041007     c                   end
016200040823      *
016300160519     c                   RETURN
016400040820     C**************************************************************************
016500040820     C* mi compongo l'istruzione sql
016600040820     C**************************************************************************
016700040820     C     istruz        BEGSR                                                  *
016800040820      **
016900120508     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file
017000120508      **
017100040820     C                   EVAL      WrkSqlCmd
017200040820     C                             =
017300120508     C                             'SELECT tidp300f.*, rrn(tidp300f) '
017400120508     c                              + ' FROM tidp300f WHERE '
017500160518     c                             + ' DP3AAS = ' + %editc(prmAAS:'X') + ' AND'
017600160518     c                             + ' DP3LNP = ' + %editc(prmLNP:'X') + ' AND'
017700160518     c                             + ' DP3NRS = ' + %editc(prmNRS:'X') + ' AND'
017800160518     c                             + ' DP3NSP = ' + %editc(prmNSP:'X') + ' AND'
017900160518     c                             + ' DP3DTR = 0 and DP3EVD =''18 '' and '
018000160519     c                             + ' DP3CEV=''ISL''  FOR READ ONLY '
018100040820      **
018200040820     C                   ENDSR
018300120510     C**************************************************************************
