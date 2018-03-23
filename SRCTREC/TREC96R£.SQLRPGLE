000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200020409     H**********************************************************************
000300170621     fWFEVD00F  O  a E             DISK    usropn
000400170621     fWFEVDSIMF O  a E             DISK    usropn
000500170620      *
000600170619     fTREC96P   o    E             PRINTER OFLIND(*in66)
000700170620      **********************************************************************
000800040820     d KPJBA         E DS
000900170619     D trec96ds      e DS
001000170619     D*
001100041103     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001200041103     D CNCR80        E DS
001300040820     D*
001400040820     D WLBDAT          DS
001500040820     D  G02DAT                 1      8  0
001600040820     D  G02INV                 9     16  0
001700040820     D  G02ERR                17     17
001800040820     D  G02TGI                18     22  0
001900040820      *
002000040820     D WrkSqlCmd       S           1024
002100040820     D  data_Oggi      s                   like(G02INV) INZ(0)
002200170621     D  savcli         s                   like(evdcli)
002300170621     D UltimaData      s              8  0
002400170621     D  savsqlcod      s                   like(sqlcod)
002500170620      *
002600170620     D numRelR         s              9  0
002700170620     D ecevdDS       E DS                  EXTNAME(ECEVD00F)
002800040820      *
002900040820     D digits          C                   CONST('0123456789')
003000040820      *****************************************************************
003100170619      *
003200170619      *****************************************************************
003300170621      *  pulisce il file di WORK e lo apre
003400170621      *   questo file conterra i records  che sono stati estratti per
003500170621      *   l'elaborazione.
003600170620     C/EXEC SQL
003700170620     C+ delete from wfevd00f
003800170620     C/END-EXEC
003900170621      *
004000170621     C/EXEC SQL
004100170621     C+ delete from wfevdsimF
004200170621     C/END-EXEC
004300170620     c                   open      wfevd00f
004400170621     c                   open      wfevdsimF
004500170620      *
004600170620     C                   clear                   obsoleti          7 0
004700170619     C                   clear                   vecchi            7 0
004800170619     C                   clear                   cli_obsoleti      7 0
004900170619     C                   clear                   cli_vecchi        7 0
005000170620     C/EXEC SQL
005100170620     C+ PREPARE S1 FROM :WrkSqlCmd
005200170620     C/END-EXEC
005300170620
005400170620     C/EXEC SQL
005500170620     C+ DECLARE A1 CURSOR FOR S1
005600170620     C/END-EXEC
005700170620
005800170620     C/EXEC SQL
005900170620     C+ OPEN A1
006000170620     C/END-EXEC
006100040820     C                   DOU       SqlCod <> 0
006200040823     c*
006300170620     C/EXEC SQL
006400170620     C+ FETCH NEXT FROM A1 INTO :numRelR, :ecevdDS
006500170620     C/END-EXEC
006600170620     c*
006700011210     C                   SELECT
006800041007     c*
006900041007     c* a fine file Totali x rotture
007000041007     C                   WHEN      SqlCod = 100
007100170620
007200170621     c                   if        stampa = 'SI'
007300041007     c                   if        almeno_uno ='S'
007400170621
007500170620     c                   exsr      a_EOF
007600170620      *
007700041007     c                   else
007800170620     c*  se non c'era nulla
007900041007     c                   write     testa
008000041007     c                   write     NODATI
008100041007     c                   end
008200170621      *
008300170621     c                   end
008400041007     c                   leave
008500041007     **
008600041007     C                   WHEN      SqlCod < 0
008700041007     C                   seton                                        H1
008800041007     c                   goto      fine
008900040820     c*
009000041007     C                   OTHER
009100170620      **
009200170621     c                   eval      savsqlcod = sqlcod
009300170620     c                   exsr      dettaglio
009400170621     c                   eval      sqlcod = savsqlcod
009500170620      **
009600040826     c                   move      'S'           almeno_uno
009700011210     C                   ENDSL
009800040820
009900011205     C                   ENDDO
010000011205     C/EXEC SQL
010100011205     C+ CLOSE A1
010200011205     C/END-EXEC
010300040823      *
010400041007     c     fine          tag
010500020326     C                   SETON                                        LR
010600040820     C**************************************************************************
010700040820     C* INIZIALIZZAZIONE
010800040820     C**************************************************************************
010900040820     C     *inzsr        BEGSR
011000040820      **
011100040820     C     *ENTRY        PLIST
011200040820     C                   PARM                    KPJBA
011300170619     c                   movel     kpjbu         trec96ds
011400041103      *
011500041103     C                   Z-ADD     1             CODUT
011600041103     C                   CALL      'X§PARUT'
011700041103     C                   PARM                    UT§DSE
011800041103     C                   MOVEL     REC80         CNCR80
011900041103     C                   MOVEL     RAGUT         RSUT             20
012000040820      *
012100040820      *  imposta UDATE di elaborazione
012200040820     C                   move      *date         G02DAT
012300040820     C                   MOVE      *ZEROS        G02INV
012400040820     C                   MOVE      *BLANKS       G02ERR
012500040820     C                   CALL      'XSRDA8'
012600040820     C                   PARM                    WLBDAT
012700040820     C                   Z-ADD     G02INV        data_oggi
012800040820     C                   move      data_oggi     day
012900040820     C                   TIME                    W0140            14 0
013000040820     C                   MOVEl     W0140         ore
013100040823      **
013200170620      **   imposta dai parametri
013300170620     c                   move      c96dadata     dadata            8 0
013400170620     c                   move      c96adata      adata             8 0
013500170620      **
013600170620     c                   clear                   Stampa            2
013700170620     c                   if        C96PRT = 'S'
013800170620     c                   eval      Stampa = 'SI'
013900170620     c                   end
014000170620      **
014100170620     c                   clear                   Aggiorna          2
014200170620     c                   if        C96DEL = 'S'
014300170620     c                   eval      Aggiorna = 'SI'
014400170620     c                   end
014500040823      **
014600040823     c                   seton                                        66
014700040825     c                   clear                   prima_volta       1
014800170620     c                   clear                   almeno_uno        1
014900040820     c*
015000170620     c* compongo istruzione sql
015100170620     c                   exsr      ISTRUZ
015200170620     c*
015300040820     C                   ENDSR
015400170620     C**************************************************************************
015500170620     C* mi compongo l'istruzione sql
015600170620     C**************************************************************************
015700170620     C     istruz        BEGSR
015800170620      **
015900170620     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file TNECF
016000170620     C                   EVAL      WrkSqlCmd  = 'Select RRN(ecevd00F), '
016100170620     C                              + ' ecevd00F.* from ecevd00F '
016200170620     C                              + ' where EVDCLI > 0 '
016300170620     c                              + ' and EVDDST > int(curdate()) '
016400170620     c                              + ' and evdcli not in '
016500170620     c                              + ' (select tasksc from titas00f) '
016600170620     c                              + ' and evdddt < '
016700170620     c                              +     %trim(%char(adata))
016800170620     c                              + ' and evdcli not in '
016900170620     c                              + ' (select tasksc from titas10f '
017000170620     c                              + ' where tasaas * 10000 + tasmgs >='
017100170620     c                              +     %trim(%char(dadata))
017200170620     c                              + ' ) order by EVDCLI '
017300170620     C                   ENDSR
017400040820     C**************************************************************************
017500040825      *
017600040825     C**************************************************************************
017700170620     C     a_EoF         begSR
017800170621      *
017900170621     C                   eval      totRIGobs  =  obsoleti
018000170621     C                   eval      totRIGvec  =  vecchi
018100170621     C                   eval      totCLIobs  =  cli_obsoleti
018200170621     C                   eval      totCLIvec  =  cli_vecchi
018300040826      *
018400170620     c                   write     testa
018500170621     c                   write     TOTALI
018600040825      *
018700040825     C                   ENDSR
018800170620     C**************************************************************************
018900170620      *
019000170620     C**************************************************************************
019100170621     C     Dettaglio     begSR
019200170620      *
019300170621      *  memorizza il record appena letto
019400170621     C                   Exsr      Salva_WORKF
019500170621      *
019600170621      *    a rottura cliente
019700170621     C                   Exsr      ROT_Cliente
019800170621      *
019900170621      *    simula come verrà fatto scadere il record tariffa
020000170621     C                   Exsr      SIMULA
020100170621      *
020200170621      *
020300170621     C                   ENDSR
020400170621     C**************************************************************************
020500170621      *  a rottura di Cliente
020600170621     C**************************************************************************
020700170621     C     ROT_Cliente   begSR
020800170621      *
020900170621      *  a rottura di cliente:
021000170621     c                   if        evdcli <> savcli
021100170621     c                   eval       savcli = evdcli
021200170621      *
021300170621     c                   clear                   UltimaData
021400170621      *
021500170621      *  Controlla l'ultima spedizione presente su TITAS10F
021600170621     C/EXEC SQL
021700170621     C+ SELECT
021800170621     C+  case when  max(tasaas * 10000 + tasmgs) is not null
021900170621     C+    then 0 end
022000170621     C+   INTO :UltimaData
022100170621     C+ FROM   titas10f   WHERE tasKSC= :evdcli
022200170621     C/END-EXEC
022300170621     c                   if        UltimaData > 0
022400170621     C                   eval      cli_vecchi   = cli_vecchi + 1
022500170621     c                   else
022600170621     C                   eval      cli_obsoleti = cli_obsoleti + 1
022700170621     c                   end
022800170621      *
022900170621     c                   end
023000170621      *
023100170621     C                   ENDSR
023200170621     C**************************************************************************
023300170621      *  SIMULA come verrà fatta scadere la tariffa
023400170621     C**************************************************************************
023500170621     C     SIMULA        begSR
023600170621      *
023700170621     c                   clear                   WFEVDsim
023800170621     c                   eval      WFDsVOC =  evDVOC
023900170621     c                   eval      WFDsPRG =  evDPRG
024000170621     c                   eval      WFDsTTV =  evDTTV
024100170621     c                   eval      WFDsFT1 =  evDFT1
024200170621     c                   eval      WFDsFT2 =  evDFT2
024300170621     c                   eval      WFDsCLI =  evDCLI
024400170621     c                   eval      WFDsCTR =  evDCTR
024500170621     c                   eval      WFDsCTS =  evDCTS
024600170621     c                   eval      WFDsCAP =  evDCAP
024700170621     c                   eval      WFDsDDT =  evDDDT
024800170621     c                   eval      WFDsSGI =  evDSGI
024900170621     c                   eval      WFDsSGF =  evDSGF
025000170621     c                   eval      WFDsITR =  evDITR
025100170621     c                   eval      WFDsIMI =  evDIMI
025200170621     c                   eval      WFDsIMA =  evDIMA
025300170621     c                   eval      WFDsSEG =  evDSEG
025400170621     c                   eval      WFDsPER =  evDPER
025500170621     c                   eval      WFDsRPV =  evDRPV
025600170621     c                   eval      WFDsDIR =  evDDIR
025700170621     c                   eval      WFDsDUV =  evDDUV
025800170621     c                   eval      WFDsFTR =  evDFTR
025900170621     c                   eval      WFDsDTR =  evDDTR
026000170621     c                   eval      WFDsATB =  evDATB
026100170621     c                   eval      WFDsNAZ =  evDNAZ
026200170621     c                   eval      WFDsCAD =  evDCAD
026300170621      **
026400170621      *  Data Scadenza Tariffa
026500170621     c                   if        UltimaData > 0
026600170621     c                   eval      WFDsDST = UltimaData
026700170621     c                   else
026800170621     c                   eval      WFDsDST = dadata
026900170621     c                   end
027000170621      **
027100170621     c                   if        UltimaData > 0
027200170621     C                   eval      vecchi   = vecchi + 1
027300170621     c                   else
027400170621     C                   eval      obsoleti = obsoleti + 1
027500170621     c                   end
027600170621      **
027700170621     c                   write     WFEVDsim
027800170621      *
027900170621      *
028000170621     C                   ENDSR
028100170621     C**************************************************************************
028200170621      *  Salva il record letto prima di elaborarlo
028300170621     C**************************************************************************
028400170621     C     Salva_WORKF   begSR
028500170621      *
028600170620     c                   clear                   WFEVD000
028700170620     c                   eval      WFDVOC =  evDVOC
028800170620     c                   eval      WFDPRG =  evDPRG
028900170620     c                   eval      WFDTTV =  evDTTV
029000170620     c                   eval      WFDFT1 =  evDFT1
029100170620     c                   eval      WFDFT2 =  evDFT2
029200170620     c                   eval      WFDCLI =  evDCLI
029300170620     c                   eval      WFDCTR =  evDCTR
029400170620     c                   eval      WFDCTS =  evDCTS
029500170620     c                   eval      WFDCAP =  evDCAP
029600170620     c                   eval      WFDDDT =  evDDDT
029700170620     c                   eval      WFDDST =  evDDST
029800170620     c                   eval      WFDSGI =  evDSGI
029900170620     c                   eval      WFDSGF =  evDSGF
030000170620     c                   eval      WFDITR =  evDITR
030100170620     c                   eval      WFDIMI =  evDIMI
030200170620     c                   eval      WFDIMA =  evDIMA
030300170620     c                   eval      WFDSEG =  evDSEG
030400170620     c                   eval      WFDPER =  evDPER
030500170620     c                   eval      WFDRPV =  evDRPV
030600170620     c                   eval      WFDDIR =  evDDIR
030700170620     c                   eval      WFDDUV =  evDDUV
030800170620     c                   eval      WFDFTR =  evDFTR
030900170620     c                   eval      WFDDTR =  evDDTR
031000170620     c                   eval      WFDATB =  evDATB
031100170620     c                   eval      WFDNAZ =  evDNAZ
031200170620     c                   eval      WFDCAD =  evDCAD
031300170620     c                   write     WFEVD000
031400170620      *
031500170620     C                   ENDSR
031600040820     C**************************************************************************
