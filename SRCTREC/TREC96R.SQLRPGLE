000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200020409     H**********************************************************************
000300170621     fecevd00F  uf   E             DISK    usropn
000400170621     fWFEVD00F  O  a E             DISK    usropn
000500170621     fWFEVDSIMF O  a E             DISK    usropn
000600170620      *
000700170619     fTREC96P   o    E             PRINTER OFLIND(*in66)
000800170620      **********************************************************************
000900040820     d KPJBA         E DS
001000170619     D trec96ds      e DS
001100170619     D*
001200041103     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001300041103     D CNCR80        E DS
001400040820     D*
001500040820     D WLBDAT          DS
001600040820     D  G02DAT                 1      8  0
001700040820     D  G02INV                 9     16  0
001800040820     D  G02ERR                17     17
001900040820     D  G02TGI                18     22  0
002000040820      *
002100040820     D WrkSqlCmd       S           1024
002200040820     D  data_Oggi      s                   like(G02INV) INZ(0)
002300170621     D  savcli         s                   like(evdcli)
002400170621     D UltimaData      s              8  0
002500170621     D  savsqlcod      s                   like(sqlcod)
002600170621     D Solo_su_TASP0F  s              1
002700170620      *
002800170620     D numRelR         s              9  0
002900170620     D ecevdDS       E DS                  EXTNAME(ECEVD00F)
003000040820      *
003100040820     D digits          C                   CONST('0123456789')
003200040820      *****************************************************************
003300170619      *
003400170619      *****************************************************************
003500170621      *  pulisce il file di WORK e lo apre
003600170621      *   questo file conterra i records  che sono stati estratti per
003700170621      *   l'elaborazione.
003800170620     C/EXEC SQL
003900170620     C+ delete from wfevd00f
004000170620     C/END-EXEC
004100170621      *
004200170621     C/EXEC SQL
004300170621     C+ delete from wfevdsimF
004400170621     C/END-EXEC
004500170620     c                   open      wfevd00f
004600170621     c                   open      wfevdsimF
004700170620      *
004800170621     C                   clear                   anomale           7 0
004900170621      *
005000170620     C                   clear                   obsoleti          7 0
005100170619     C                   clear                   vecchi            7 0
005200170621      *
005300170619     C                   clear                   cli_obsoleti      7 0
005400170619     C                   clear                   cli_vecchi        7 0
005500170621      *
005600170621     C                   clear                   assoluti          7 0
005700170621     C                   clear                   cli_assoluti      7 0
005800170621      *
005900170620     C/EXEC SQL
006000170620     C+ PREPARE S1 FROM :WrkSqlCmd
006100170620     C/END-EXEC
006200170620
006300170620     C/EXEC SQL
006400170620     C+ DECLARE A1 CURSOR FOR S1
006500170620     C/END-EXEC
006600170620
006700170620     C/EXEC SQL
006800170620     C+ OPEN A1
006900170620     C/END-EXEC
007000040820     C                   DOU       SqlCod <> 0
007100040823     c*
007200170620     C/EXEC SQL
007300170620     C+ FETCH NEXT FROM A1 INTO :numRelR, :ecevdDS
007400170620     C/END-EXEC
007500170620     c*
007600011210     C                   SELECT
007700041007     c*
007800041007     c* a fine file Totali x rotture
007900041007     C                   WHEN      SqlCod = 100
008000170620
008100170621     c                   if        stampa = 'SI'
008200041007     c                   if        almeno_uno ='S'
008300170621
008400170620     c                   exsr      a_EOF
008500170620      *
008600041007     c                   else
008700170620     c*  se non c'era nulla
008800041007     c                   write     testa
008900041007     c                   write     NODATI
009000041007     c                   end
009100170621      *
009200170621     c                   end
009300041007     c                   leave
009400041007     **
009500041007     C                   WHEN      SqlCod < 0
009600041007     C                   seton                                        H1
009700041007     c                   goto      fine
009800040820     c*
009900041007     C                   OTHER
010000170620      **
010100170621     c                   eval      savsqlcod = sqlcod
010200170620     c                   exsr      dettaglio
010300170621     c                   eval      sqlcod = savsqlcod
010400170620      **
010500040826     c                   move      'S'           almeno_uno
010600011210     C                   ENDSL
010700040820
010800011205     C                   ENDDO
010900011205     C/EXEC SQL
011000011205     C+ CLOSE A1
011100011205     C/END-EXEC
011200040823      *
011300041007     c     fine          tag
011400020326     C                   SETON                                        LR
011500040820     C**************************************************************************
011600040820     C* INIZIALIZZAZIONE
011700040820     C**************************************************************************
011800040820     C     *inzsr        BEGSR
011900040820      **
012000040820     C     *ENTRY        PLIST
012100040820     C                   PARM                    KPJBA
012200170619     c                   movel     kpjbu         trec96ds
012300041103      *
012400041103     C                   Z-ADD     1             CODUT
012500041103     C                   CALL      'X§PARUT'
012600041103     C                   PARM                    UT§DSE
012700041103     C                   MOVEL     REC80         CNCR80
012800041103     C                   MOVEL     RAGUT         RSUT             20
012900040820      *
013000040820      *  imposta UDATE di elaborazione
013100040820     C                   move      *date         G02DAT
013200040820     C                   MOVE      *ZEROS        G02INV
013300040820     C                   MOVE      *BLANKS       G02ERR
013400040820     C                   CALL      'XSRDA8'
013500040820     C                   PARM                    WLBDAT
013600040820     C                   Z-ADD     G02INV        data_oggi
013700040820     C                   move      data_oggi     day
013800040820     C                   TIME                    W0140            14 0
013900040820     C                   MOVEl     W0140         ore
014000040823      **
014100170620      **   imposta dai parametri
014200170622     c                   move      c96dasped     dadata            8 0
014300170622     c                   move      c96dasped     pDAL
014400170622     c                   move      c96dadec      adata             8 0
014500170622     c                   move      c96dadec      pAL
014600170620      **
014700170620     c                   clear                   Stampa            2
014800170622     c                   if        C96stp = 'S'
014900170620     c                   eval      Stampa = 'SI'
015000170620     c                   end
015100170620      **
015200170620     c                   clear                   Aggiorna          2
015300170622     c                   if        C96agg = 'S'
015400170621     c                   open      ecevd00f
015500170620     c                   eval      Aggiorna = 'SI'
015600170620     c                   end
015700040823      **
015800040823     c                   seton                                        66
015900040825     c                   clear                   prima_volta       1
016000170620     c                   clear                   almeno_uno        1
016100040820     c*
016200170620     c* compongo istruzione sql
016300170620     c                   exsr      ISTRUZ
016400170620     c*
016500040820     C                   ENDSR
016600170620     C**************************************************************************
016700170620     C* mi compongo l'istruzione sql
016800170620     C**************************************************************************
016900170620     C     istruz        BEGSR
017000170620      **
017100170620     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file TNECF
017200170620     C                   EVAL      WrkSqlCmd  = 'Select RRN(ecevd00F), '
017300170620     C                              + ' ecevd00F.* from ecevd00F '
017400170620     C                              + ' where EVDCLI > 0 '
017500170620     c                              + ' and EVDDST > int(curdate()) '
017600170620     c                              + ' and evdcli not in '
017700170620     c                              + ' (select tasksc from titas00f) '
017800170620     c                              + ' and evdddt < '
017900170620     c                              +     %trim(%char(adata))
018000170620     c                              + ' and evdcli not in '
018100170620     c                              + ' (select tasksc from titas10f '
018200170620     c                              + ' where tasaas * 10000 + tasmgs >='
018300170620     c                              +     %trim(%char(dadata))
018400170620     c                              + ' ) order by EVDCLI '
018500170620     C                   ENDSR
018600040820     C**************************************************************************
018700040825      *
018800040825     C**************************************************************************
018900170620     C     a_EoF         begSR
019000170621      *
019100170621     C                   eval      totRIGobs  =  obsoleti
019200170621     C                   eval      totRIGvec  =  vecchi
019300170621     C                   eval      totRIGanm  =  anomale
019400170621      *
019500170621     C                   eval      totCLIobs  =  cli_obsoleti
019600170621     C                   eval      totCLIvec  =  cli_vecchi
019700170621      *
019800170621     C                   eval      totRIGass  =  assoluti
019900170621     C                   eval      totCLIass  =  cli_assoluti
020000040826      *
020100170620     c                   write     testa
020200170621     c                   write     TOTALI
020300040825      *
020400040825     C                   ENDSR
020500170620     C**************************************************************************
020600170620      *
020700170620     C**************************************************************************
020800170621     C     Dettaglio     begSR
020900170620      *
021000170621      *  memorizza il record appena letto
021100170621     C                   Exsr      Salva_WORKF
021200170621      *
021300170621      *    a rottura cliente
021400170621     C                   Exsr      ROT_Cliente
021500170621      *
021600170621      *    simula come verrà fatto scadere il record tariffa
021700170621     C                   Exsr      SIMULA
021800170621      *
021900170621      *    aggiorna ECEVD
022000170621     c                   if        aggiorna ='SI'
022100170621     C                   Exsr      AGGIORNA_DB
022200170621     c                   end
022300170621      *
022400170621     C                   ENDSR
022500170621     C**************************************************************************
022600170621      *  a rottura di Cliente
022700170621     C**************************************************************************
022800170621     C     ROT_Cliente   begSR
022900170621      *
023000170621      *  a rottura di cliente:
023100170621     c                   if        evdcli <> savcli
023200170621     c                   eval       savcli = evdcli
023300170621      *
023400170621     c                   clear                   UltimaData
023500170621     c                   clear                   Solo_su_TASP0F
023600170621      *
023700170621      *  Controlla l'ultima spedizione presente su TITAS10F
023800170621     C/EXEC SQL
023900170621     C+ SELECT
024000170621     C+   max(tasaas * 10000 + tasmgs)
024100170621     C+   INTO :UltimaData
024200170621     C+ FROM   titas10f   WHERE tasKSC= :evdcli
024300170621     C/END-EXEC
024400170621     c                   if        UltimaData > 0
024500170621     C                   eval      cli_vecchi   = cli_vecchi + 1
024600170621     c                   else
024700170621      **
024800170621     C                   eval      cli_obsoleti = cli_obsoleti + 1
024900170621      **
025000170621      *  Controlla allora su TASP0F in assoluto
025100170621     C/EXEC SQL
025200170621     C+ SELECT
025300170621     C+   max(tasaas * 10000 + tasmgs)
025400170621     C+   INTO :UltimaData
025500170621     C+ FROM   titasp0f   WHERE tasKSC= :evdcli
025600170621     C/END-EXEC
025700170621      **
025800170621     c                   if        UltimaData > 0
025900170621     c                   eval      Solo_su_TASP0F = 'S'
026000170621     c                   else
026100170621     C                   eval      cli_assoluti = cli_assoluti + 1
026200170621     c                   end
026300170621      **
026400170621     c                   end
026500170621      *
026600170621     c                   end
026700170621      *
026800170621     C                   ENDSR
026900170621     C**************************************************************************
027000170621      *  SIMULA come verrà fatta scadere la tariffa
027100170621     C**************************************************************************
027200170621     C     SIMULA        begSR
027300170621      *
027400170621     c                   clear                   WFEVDsim
027500170621     c                   eval      WFDsVOC =  evDVOC
027600170621     c                   eval      WFDsPRG =  evDPRG
027700170621     c                   eval      WFDsTTV =  evDTTV
027800170621     c                   eval      WFDsFT1 =  evDFT1
027900170621     c                   eval      WFDsFT2 =  evDFT2
028000170621     c                   eval      WFDsCLI =  evDCLI
028100170621     c                   eval      WFDsCTR =  evDCTR
028200170621     c                   eval      WFDsCTS =  evDCTS
028300170621     c                   eval      WFDsCAP =  evDCAP
028400170621     c                   eval      WFDsDDT =  evDDDT
028500170621     c                   eval      WFDsSGI =  evDSGI
028600170621     c                   eval      WFDsSGF =  evDSGF
028700170621     c                   eval      WFDsITR =  evDITR
028800170621     c                   eval      WFDsIMI =  evDIMI
028900170621     c                   eval      WFDsIMA =  evDIMA
029000170621     c                   eval      WFDsSEG =  evDSEG
029100170621     c                   eval      WFDsPER =  evDPER
029200170621     c                   eval      WFDsRPV =  evDRPV
029300170621     c                   eval      WFDsDIR =  evDDIR
029400170621     c                   eval      WFDsDUV =  evDDUV
029500170621     c                   eval      WFDsFTR =  evDFTR
029600170621     c                   eval      WFDsDTR =  evDDTR
029700170621     c                   eval      WFDsATB =  evDATB
029800170621     c                   eval      WFDsNAZ =  evDNAZ
029900170621     c                   eval      WFDsCAD =  evDCAD
030000170621      **
030100170621      *  Data Scadenza Tariffa
030200170621     c                   if        UltimaData > 0
030300170621     c                   eval      WFDsDST = UltimaData
030400170621     c                   else
030500170621     c                   eval      WFDsDST = dadata
030600170621     c                   end
030700170621      **
030800170621      *  Data Scadenza < di Decorrenza   allora si impostano entrambe
030900170621      **  con quella di decorrenza.
031000170621     c                   if        WFDsDST < WFDsDDT
031100170621     c                   eval        WFDsDST = WFDsDDT
031200170621     C                   eval        anomale = anomale + 1
031300170621     c                   end
031400170621      **
031500170621     c                   if        UltimaData > 0   and
031600170621     c                             Solo_su_TASP0F <>'S'
031700170621     C                   eval        vecchi =   vecchi + 1
031800170703     c                   else
031900170703     C                   eval      obsoleti = obsoleti + 1
032000170621      *
032100170703     c                   if        UltimaData > 0   and
032200170621     c                             Solo_su_TASP0F = 'S'
032300170621      **
032400170621     c                   else
032500170703     C                   eval      assoluti = assoluti + 1
032600170621     c                   end
032700170703     c                   endif
032800170621      **
032900170621     c                   write     WFEVDsim
033000170621      *
033100170621     C                   ENDSR
033200170621     C**************************************************************************
033300170621      *  Aggiorna il file ECEVD del C/E
033400170621     C**************************************************************************
033500170621     C     AGGIORNA_DB   begSR
033600170621      *
033700170621      * x nr.rel.record
033800170621     c     numRelR       chain     ecEVD00f
033900170621     c                   if         %Found(ecEVD00f)
034000170621     c                   eval        evdDST = WFDsDST
034100170703     c                   eval        evdDUV = 19000101
034200170621     c                   update    ecevd000
034300170621     c                   end
034400170621      *
034500170621     C                   ENDSR
034600170621     C**************************************************************************
034700170621      *  Salva il record letto prima di elaborarlo
034800170621     C**************************************************************************
034900170621     C     Salva_WORKF   begSR
035000170621      *
035100170620     c                   clear                   WFEVD000
035200170620     c                   eval      WFDVOC =  evDVOC
035300170620     c                   eval      WFDPRG =  evDPRG
035400170620     c                   eval      WFDTTV =  evDTTV
035500170620     c                   eval      WFDFT1 =  evDFT1
035600170620     c                   eval      WFDFT2 =  evDFT2
035700170620     c                   eval      WFDCLI =  evDCLI
035800170620     c                   eval      WFDCTR =  evDCTR
035900170620     c                   eval      WFDCTS =  evDCTS
036000170620     c                   eval      WFDCAP =  evDCAP
036100170620     c                   eval      WFDDDT =  evDDDT
036200170620     c                   eval      WFDDST =  evDDST
036300170620     c                   eval      WFDSGI =  evDSGI
036400170620     c                   eval      WFDSGF =  evDSGF
036500170620     c                   eval      WFDITR =  evDITR
036600170620     c                   eval      WFDIMI =  evDIMI
036700170620     c                   eval      WFDIMA =  evDIMA
036800170620     c                   eval      WFDSEG =  evDSEG
036900170620     c                   eval      WFDPER =  evDPER
037000170620     c                   eval      WFDRPV =  evDRPV
037100170620     c                   eval      WFDDIR =  evDDIR
037200170620     c                   eval      WFDDUV =  evDDUV
037300170620     c                   eval      WFDFTR =  evDFTR
037400170620     c                   eval      WFDDTR =  evDDTR
037500170620     c                   eval      WFDATB =  evDATB
037600170620     c                   eval      WFDNAZ =  evDNAZ
037700170620     c                   eval      WFDCAD =  evDCAD
037800170620     c                   write     WFEVD000
037900170620      *
038000170620     C                   ENDSR
038100040820     C**************************************************************************
