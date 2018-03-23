000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200020409     H**********************************************************************
000300120510     FTIDP300F  uF   E             DISK    infds(DP3ds)
000400040820     H**********************************************************************
000500040820     d KPJBA         E DS
000600120508      *
000700120508     d DP3ds           ds
000800120508     d  DP3_nrr              397    400b 0
000900120508      *
001000120508     D TIDP3DS       e DS                  extname(TIDP300F)
001100120508     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001200120508     D CNCR80        E DS
001300040820     D*
001400040820     D WLBDAT          DS
001500040820     D  G02DAT                 1      8  0
001600040820     D  G02INV                 9     16  0
001700040820     D  G02ERR                17     17
001800040820     D  G02TGI                18     22  0
001900040820      *
002000040820     D WrkSqlCmd       S           1024
002100040820     D  data_Oggi      s                   like(G02INV) INZ(0)
002200040820      *
002300120508     D  savPRN         s                   like(Dp3PRN)
002400040826     D  almeno_uno     s              1
002500120508     d  MIC_nrr        s              9s 0
002600120510     d  MIC_Updated    s              9s 0
002700120510     d  NOMIC_nrr      s              9s 0
002800040820      *
002900040820     D digits          C                   CONST('0123456789')
003000040820      *****************************************************************
003100011210     C/EXEC SQL
003200011210     C+ PREPARE S1 FROM :WrkSqlCmd
003300011210     C/END-EXEC
003400011210
003500011210     C/EXEC SQL
003600011210     C+ DECLARE A1 CURSOR FOR S1
003700011210     C/END-EXEC
003800011210
003900011210     C/EXEC SQL
004000011210     C+ OPEN A1
004100011210     C/END-EXEC
004200040820
004300040820     C                   DOU       SqlCod <> 0
004400011205     C/EXEC SQL
004500120508     C+ FETCH NEXT FROM A1 INTO :TIDP3DS, :DP3_nrr
004600011205     C/END-EXEC
004700011210     C                   SELECT
004800041007     c*
004900041007     c* a fine file Totali x rotture
005000041007     C                   WHEN      SqlCod = 100
005100120508     c*
005200041007     c                   if        almeno_uno ='S'
005300120508     c                   exsr      CHG_ParcelNR
005400041007     c                   end
005500120510     c*
005600041007     c                   leave
005700120508      *
005800120508     **  x qualsiasi errore
005900041007     C                   WHEN      SqlCod < 0
006000120510      *
006100041007     C                   seton                                        H1
006200041007     c                   goto      fine
006300040820     c*
006400041007     C                   OTHER
006500120510      *
006600120508     c                   move      'S'           almeno_uno
006700120508     c*
006800120510     c* Rottura di Livello x Parcel Nr.
006900120508     c                   exsr      CHG_ParcelNR
007000120510     c*
007100040820     c* DETTAGLIO
007200120508     c                   if        *in66
007300040823     c                   setoff                                       66
007400040825     c                   eval      prima_volta = 'N'
007500040823     c                   end
007600120510     c*
007700120510     c*  controlla l'evento ricevuto se un MIC o un NIC oppure ALTRO
007800120510     c                   exsr      CHK_EVENTO
007900120510     c*
008000040823      *
008100011210     C                   ENDSL
008200040820
008300011205     C                   ENDDO
008400011205     C/EXEC SQL
008500011205     C+ CLOSE A1
008600011205     C/END-EXEC
008700011205     c*
008800040820     C* totali generali
008900041007     c                   if        almeno_uno ='S'
009000041007     c                   end
009100040823      *
009200041007     c     fine          tag
009300020326     C                   SETON                                        LR
009400040820     C**************************************************************************
009500040820     C* INIZIALIZZAZIONE
009600040820     C**************************************************************************
009700040820     C     *inzsr        BEGSR
009800040820      **
009900040820     C     *ENTRY        PLIST
010000040820     C                   PARM                    KPJBA
010100041103      *
010200041103     C                   Z-ADD     1             CODUT
010300041103     C                   CALL      'X§PARUT'
010400041103     C                   PARM                    UT§DSE
010500041103     C                   MOVEL     REC80         CNCR80
010600041103     C                   MOVEL     RAGUT         RSUT             20
010700041103     C*
010800040820      *
010900040820      *  imposta UDATE di elaborazione
011000040820     C                   move      *date         G02DAT
011100040820     C                   MOVE      *ZEROS        G02INV
011200040820     C                   MOVE      *BLANKS       G02ERR
011300040820     C                   CALL      'XSRDA8'
011400040820     C                   PARM                    WLBDAT
011500040820     C                   Z-ADD     G02INV        data_oggi
011600120510     C                   move      data_oggi     day               8 0
011700040820     C                   TIME                    W0140            14 0
011800120510     C                   MOVEl     W0140         ore               6 0
011900040823      **
012000040823     c                   seton                                        66
012100040825     c                   clear                   prima_volta       1
012200040826     c                   clear                   almeno_uno
012300040820     c*
012400040820     c* compongo istruzione sql
012500040820     c                   exsr      istruz
012600040820     c*
012700040820     C                   ENDSR
012800040820     C**************************************************************************
012900040820     C* mi compongo l'istruzione sql
013000040820     C**************************************************************************
013100040820     C     istruz        BEGSR                                                  *
013200040820      **
013300120508     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file
013400120508      **
013500040820     C                   EVAL      WrkSqlCmd
013600040820     C                             =
013700120508     C                             'SELECT tidp300f.*, rrn(tidp300f) '
013800120508     c                              + ' FROM tidp300f WHERE '
013900120508     c                              + ' DP3DEV > 0 and DP3DTR = 0 '
014000120508     c                              + ' ORDER BY dp3prn, rrn(tidp300f) '
014100040820      **
014200040820     C                   ENDSR
014300040825     C**************************************************************************
014400040825      *
014500040825     C**************************************************************************
014600120508     C     CHG_ParcelNR  begSR
014700040826      *
014800120508     c* ROTTURA x Parcel NR.
014900120508     c                   if        dp3prn <> savprn
015000120508     c                               or
015100040825     C                             SqlCod = 100
015200120510      *
015300120510      * Salva il PARCEL x rottura di livello
015400120510     c                   eval      savprn =  dp3prn
015500120508      *
015600120508      * pulisce il puntamento del record con il MIC da trasmettere
015700120510     c                   exsr      Reset_NRR
015800120508      *
015900040826     c                   end
016000040825      *
016100040825     C                   ENDSR
016200040820     C**************************************************************************
016300120510     C     Reset_NRR     BEGSR
016400120510      *
016500120510     c                   clear                   MIC_nrr
016600120510     c                   clear                   NOMIC_nrr
016700120510     c                   clear                   MIC_updated
016800120510      *
016900120510     C                   ENDSR
017000120510     C**************************************************************************
017100120510      *
017200120510     C**************************************************************************
017300120510     C     CHK_EVENTO    BEGSR
017400120510      *
017500120511     c                   IF        DP3CEV = 'MIC' and dp3DTR=0
017600120510      * Se è un altro MIC a seguito subito di un NIC
017700120510     c                   if           NOMIC_nrr > 0
017800120510     c                   exsr      Reset_NRR
017900120510     c                   end
018000120510     c                   eval        MIC_nrr = DP3_nrr
018100120510      *
018200120511     c                   elseIF    DP3CEV = 'NIC' and dp3DTR=0 and MIC_nrr > 0
018300120510     c                   eval        NOMIC_nrr = DP3_nrr
018400120510     c                   exsr      AGGIORNA_DP3
018500120510      *
018600120510     c                   else
018700120510      * spezza il controllo e azzera i puntatori di records
018800120510     c                   exsr      Reset_NRR
018900120510     C                   END
019000120510      *
019100120510     C                   ENDSR
019200120510     C**************************************************************************
019300120510      *  Aggiorna sia il MIC che il NIC come già spediti (così non li manda più)
019400120510     C**************************************************************************
019500120510     C     AGGIORNA_DP3  BEGSR
019600120510      *
019700120510      *   Aggiorna il primo MIC davanti ai NIC
019800120510     c                   if           MIC_updated = 0
019900120510     C     MIC_nrr       chain     tiDP300f
020000120510     c                   if        %Found(tiDP300f)
020100120510     c                   eval         MIC_updated = DP3_nrr
020200120510     c                   eval       dp3ftr ='X'
020300120510     c                   eval       dp3dtr = 99999999
020400120510     c                   update    tidp3000
020500120510     c                   end
020600120510     c                   end
020700120510      *
020800120510      *   Aggiorna i NIC subito successivi al MIC
020900120510     C     NOMIC_nrr     chain     tiDP300f
021000120510     c                   if        %Found(tiDP300f)
021100120510     c                   eval       dp3ftr ='X'
021200120510     c                   eval       dp3dtr = 99999999
021300120510     c                   update    tidp3000
021400120510     c                   end
021500120510      *
021600120510     C                   ENDSR
021700120510     C**************************************************************************
