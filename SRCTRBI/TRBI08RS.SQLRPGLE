000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200060125      *****************************************************************
000300161025      * Il pgm scrive i record di valorizzazione autisti per giornata
000400170420      *     IL DETTAGLIO DEI RITIRI
000500060125      *****************************************************************
000600170421     feccet30c  if   e           k disk
000700170421     fecced30c  if   e           k disk
000800170420     FbiRVL00f  o    E             DISK
000900030429     F*---------------------------------------------------------------------------------
001000000607     D kpjba         e ds
001100161026     dparam            ds
001200161026     d datap                          8  0
001300170420     d datap2                         8  0
001400170420      **
001500170420     D fiftd         E DS                  extname(fiftd00f)
001600170420      **
001700060125     d dataoggi        s               d   datfmt(*eur)
001800060125     d data_gg1        s               d   datfmt(*iso)
001900161025     d tmp             s             17
002000161025     d exfttfgs        s              3  0
002100161025     d exfttddc        s              8  0
002200020205     c**********************************************************************
002300160126     C*
002400170420     c* Lettura fifTD00F con data zero quindi tutto lo sflaggato
002500161025     c                   if        datap = 0
002600161026     C/EXEC SQL
002601170927     C+ DECLARE A1 CURSOR FOR
002602170927     c+ select * from (
002603170927     c+ select fiftd00f.*
002604170927     c+   from fiftd00f join bitvl00f
002605170927     c+     on ftdpdr=tvlpdr and ftdddc=tvldfv and ftdTSR='R') a
002606170927     c+  join fiftt00f
002607170927     c+    on  ftdpdr=fttpdr and ftdddc=fttddc and fttTSR='R'
002608170927     c+ and fttftr = ' ' and ftdndc=fttndc
003100170420     C+  order by ftdpdr, ftdtsr, ftdndc, ftdddc, FTDNSS, FTDSTP
003200161026     C/END-EXEC
003300020328
003400020328     C/EXEC SQL
003500020328     C+ OPEN A1
003600020328     C/END-EXEC
003700020328     C                   DOU       SqlCod <> 0
003800020328     C/EXEC SQL
003900170420     C+ FETCH NEXT FROM A1 INTO :fiftd
004000020328     C/END-EXEC
004100020328     C                   SELECT
004200060125     c*
004300020328     C                   WHEN      SqlCod = 0
004400161025      *
004500170420     c                   exsr      scrivi_Rvl
004600060125      * ? EoF
004700020328     C                   WHEN      SqlCod = 100
004800160126     c                   leave
004900060125      * ? Errori
005000020328     C                   WHEN      SqlCod <> 0
005100160126     c                   leave
005200060125      **
005300020328     C                   ENDSL
005400060125      **
005500020328     C                   ENDDO
005600020328     C/EXEC SQL
005700020328     C+ CLOSE A1
005800020328     C/END-EXEC
005900161028     c* Lettura fifTT00F con data quindi seleziono tutte le distinte di quella data
006000161028     c* che abbiano il flag di trasmesso diverso da blank
006100170420      *
006200161026     c                   else
006300170420      *
006400170420      *  Prima di rigenerare un periodo specifico
006500170420      *    =========================================
006600170420      *      PULISCE x ciò che dovrà rigenerare.
006700170420      *    =========================================
006800170420     C/EXEC SQL
006900170420     C+    DELETE from birvl00f where
007000170420     C+         rvldfv between :datap and :datap2
007100170420     C/END-EXEC
007200170420      *    =========================================
007300170420      *     Quindi rigenera il periodo in questione
007400170420      *    =========================================
007500161026     C/EXEC SQL
007600170420     C+ DECLARE B1 CURSOR FOR
007700170420     C+ select fiftd00f.*
007800170420     C+   from fiftd00f join fiftt00f
007900170420     C+     on ftdpdr=fttpdr and ftdddc=fttddc and ftdTSR='R'
008000170420     C+  where fttfvl = 'C' and ftttsr = ' '
008100170420     C+    and fttddc between :datap and :datap2
008200170420     C+    and fttftr <> ' '
008300170420     C+  order by ftdpdr, ftdtsr, ftdndc, ftdddc, FTDNSS, FTDSTP
008400161026     C/END-EXEC
008500161026     C/EXEC SQL
008600161026     C+ OPEN B1
008700161026     C/END-EXEC
008800161026     C                   DOU       SqlCod <> 0
008900161026     C/EXEC SQL
009000170420     C+ FETCH NEXT FROM B1 INTO :fiftd
009100161026     C/END-EXEC
009200161026     C                   SELECT
009300161026     c*
009400161026     C                   WHEN      SqlCod = 0
009500161026      *
009600170420     c                   exsr      scrivi_Rvl
009700161026      * ? EoF
009800161026     C                   WHEN      SqlCod = 100
009900161026     c                   leave
010000161026      * ? Errori
010100161026     C                   WHEN      SqlCod <> 0
010200161026     c                   leave
010300161026      **
010400161026     C                   ENDSL
010500161026      **
010600161026     C                   ENDDO
010700161026     C/EXEC SQL
010800161026     C+ CLOSE B1
010900161026     C/END-EXEC
011000161026     c                   endif
011100030429      *
011200000607     c                   seton                                        lr
011300161025      * ?___________________________________________________________________
011400170420      ** scrittura testate valorizzazioni
011500170420      * ?___________________________________________________________________
011600170420     C     scrivi_Rvl    BEGSR
011700170420      *
011800170420     c                   clear                   biRvl000
011900170420      *
012000170420     c                   eval      RVLFGS   =  ftdFGS
012100170420     c                   eval      RVLDFV   =  ftdDDC
012200170420     c                   eval      RVLNFV   =  ftdNDC
012300170420     c                   eval      RVLPDR   =  ftdPDR
012400170420     c                   eval      RVLAAS   =  ftdAAS
012500170420     c                   eval      RVLLNP   =  ftdLNP
012600170420     c                   eval      RVLNRS   =  ftdNRS
012700170420     c                   eval      RVLNSP   =  ftdNSP
012800170420     c                   eval      RVLNCL   =  ftdNCL
012900170420     c                   eval      RVLPKL   =  ftdPKL
013000170420     c                   eval      RVLVLF   =  ftdVLU
013100170420     c                   eval      RVLCAP   =  ftdCAP
013200170420     c                   eval      RVLFIN   =  ftdFIN
013300170420     c                   eval      RVLCTP   =  ftdCTP
013400170420     c                   eval      RVLITT   =  ftdITT
013500170420     c                   eval      RVLITA   =  ftdITA
013600170421     c                   eval      tmp= %char(%timestamp: *iso0)
013700170421     c                   eval      RVLdatora =   Tmp
013800161027      *
013900170421      * prende le competenze BASE (001-Partenza) e ACCESSORI (035-Isole)
014000170421     c     kcet          chain     ecCET30c
014100170421     c                   if        %found(ecCET30c)
014200170421     c                   eval      RVLICE   =  cetPAR
014300170421     c                   z-add     35            CEDcmp
014400170421     c     kced          chain     ecCED30c
014500170421     c                   if        %found(ecCED30c)
014600170421     c                   eval      RVLICEa  =  ceDimp
014700170421     c                   end
014800170421     c                   endif
014900170421      *
015000170420     c                   write     biRvl000
015100170420      *
015200160125     C                   endsr
015300020205     C**-------------------------------------------------------------**
015400000607     c     *inzsr        begsr
015500060125     C**-------------------------------------------------------------**
015600000607     c     *entry        plist
015700020205     c                   parm                    kpjba
015800170420      *
015900161026     c                   if        kpjbu = *blank
016000161026     c                   clear                   param
016100161026     c                   else
016200161026     c                   movel     kpjbu         param
016300170420     c                   if        datap2 = 0
016400170420     c                   eval      datap2 = datap
016500170420     c                   endif
016600161026     c                   endif
016700030429      *----------*
016800030429     C* GIRO DATA ODIERNA
016900030429     C                   TIME                    W0140            14 0
017000030429     C                   MOVE      W0140         UDATE8            8 0
017100160126     C                   MOVEl     W0140         oracor            6 0
017200060125     C     *eur          movel     UDATE8        dataoggi
017300160201     c                   move      dataoggi      data_gg1
017400160201     c                   move      data_gg1      oggi              8 0
017500060125     C     dataoggi      subdur    1:*d          data_gg1
017600160201     c                   move      data_gg1      ieri              8 0
017700060125      *
017800160125     C* Definisco chiavi di accesso
017900170421     c     kcet          klist
018000170421     c                   kfld                    rvlaas
018100170421     c                   kfld                    rvllnp
018200170421     c                   kfld                    rvlnrs
018300170421     c                   kfld                    rvlnsp
018400170421      *
018500170421     c     kced          klist
018600170421     c                   kfld                    rvlaas
018700170421     c                   kfld                    rvllnp
018800170421     c                   kfld                    rvlnrs
018900170421     c                   kfld                    rvlnsp
019000170421     c                   kfld                    ceTTBL
019100170421     c                   kfld                    ceDCMP
019200020402     c*
019300020402     c                   endsr
019400161123      * ?------------------------------------------------------------------ */
019500161123      *?      X non bloccare in nessun caso il traduttore CLIENTI
019600161123      * ?------------------------------------------------------------------ */
019700161123     C     *pssr         BEGSR
019800161123     C
019900161123     c                   seton                                        LR
020000161123     c                   return
020100161123     C
020200161123     C                   ENDSR
020300161123      * ?------------------------------------------------------------------ */
