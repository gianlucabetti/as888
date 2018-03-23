000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200060125      *****************************************************************
000300161025      * Il pgm scrive i record di valorizzazione autisti per giornata
000400170420      *     IL DETTAGLIO DEI RITIRI
000500060125      *****************************************************************
000600170420     FbiRVL00f  o    E             DISK
000700030429     F*---------------------------------------------------------------------------------
000800000607     D kpjba         e ds
000900161026     dparam            ds
001000161026     d datap                          8  0
001100170420     d datap2                         8  0
001200170420      **
001300170420     D fiftd         E DS                  extname(fiftd00f)
001400170420      **
001500060125     d dataoggi        s               d   datfmt(*eur)
001600060125     d data_gg1        s               d   datfmt(*iso)
001700161025     d tmp             s             17
001800161025     d exfttfgs        s              3  0
001900161025     d exfttddc        s              8  0
002000020205     c**********************************************************************
002100160126     C*
002200170420     c* Lettura fifTD00F con data zero quindi tutto lo sflaggato
002300161025     c                   if        datap = 0
002400161026     C/EXEC SQL
002500170420     C+ DECLARE A1 CURSOR FOR
002600170420     C+ select fiftd00f.*
002700170420     C+   from fiftd00f join bitvl00f
002800170420     C+     on ftdpdr=tvlpdr and ftdddc=tvldfv and ftdTSR='R'
002900170420     C+  order by ftdpdr, ftdtsr, ftdndc, ftdddc, FTDNSS, FTDSTP
003000161026     C/END-EXEC
003100020328
003200020328     C/EXEC SQL
003300020328     C+ OPEN A1
003400020328     C/END-EXEC
003500020328     C                   DOU       SqlCod <> 0
003600020328     C/EXEC SQL
003700170420     C+ FETCH NEXT FROM A1 INTO :fiftd
003800020328     C/END-EXEC
003900020328     C                   SELECT
004000060125     c*
004100020328     C                   WHEN      SqlCod = 0
004200161025      *
004300170420     c                   exsr      scrivi_Rvl
004400060125      * ? EoF
004500020328     C                   WHEN      SqlCod = 100
004600160126     c                   leave
004700060125      * ? Errori
004800020328     C                   WHEN      SqlCod <> 0
004900160126     c                   leave
005000060125      **
005100020328     C                   ENDSL
005200060125      **
005300020328     C                   ENDDO
005400020328     C/EXEC SQL
005500020328     C+ CLOSE A1
005600020328     C/END-EXEC
005700161028     c* Lettura fifTT00F con data quindi seleziono tutte le distinte di quella data
005800161028     c* che abbiano il flag di trasmesso diverso da blank
005900170420      *
006000161026     c                   else
006100170420      *
006200170420      *  Prima di rigenerare un periodo specifico
006300170420      *    =========================================
006400170420      *      PULISCE x ciò che dovrà rigenerare.
006500170420      *    =========================================
006600170420     C/EXEC SQL
006700170420     C+    DELETE from birvl00f where
006800170420     C+         rvldfv between :datap and :datap2
006900170420     C/END-EXEC
007000170420      *    =========================================
007100170420      *     Quindi rigenera il periodo in questione
007200170420      *    =========================================
007300161026     C/EXEC SQL
007400170420     C+ DECLARE B1 CURSOR FOR
007500170420     C+ select fiftd00f.*
007600170420     C+   from fiftd00f join fiftt00f
007700170420     C+     on ftdpdr=fttpdr and ftdddc=fttddc and ftdTSR='R'
007800170420     C+  where fttfvl = 'C' and ftttsr = ' '
007900170420     C+    and fttddc between :datap and :datap2
008000170420     C+    and fttftr <> ' '
008100170420     C+  order by ftdpdr, ftdtsr, ftdndc, ftdddc, FTDNSS, FTDSTP
008200161026     C/END-EXEC
008300161026     C/EXEC SQL
008400161026     C+ OPEN B1
008500161026     C/END-EXEC
008600161026     C                   DOU       SqlCod <> 0
008700161026     C/EXEC SQL
008800170420     C+ FETCH NEXT FROM B1 INTO :fiftd
008900161026     C/END-EXEC
009000161026     C                   SELECT
009100161026     c*
009200161026     C                   WHEN      SqlCod = 0
009300161026      *
009400170420     c                   exsr      scrivi_Rvl
009500161026      * ? EoF
009600161026     C                   WHEN      SqlCod = 100
009700161026     c                   leave
009800161026      * ? Errori
009900161026     C                   WHEN      SqlCod <> 0
010000161026     c                   leave
010100161026      **
010200161026     C                   ENDSL
010300161026      **
010400161026     C                   ENDDO
010500161026     C/EXEC SQL
010600161026     C+ CLOSE B1
010700161026     C/END-EXEC
010800161026     c                   endif
010900030429      *
011000000607     c                   seton                                        lr
011100161025      * ?___________________________________________________________________
011200170420      ** scrittura testate valorizzazioni
011300170420      * ?___________________________________________________________________
011400170420     C     scrivi_Rvl    BEGSR
011500170420      *
011600170420     c                   clear                   biRvl000
011700170420      *
011800170420     c                   eval      RVLFGS   =  ftdFGS
011900170420     c                   eval      RVLDFV   =  ftdDDC
012000170420     c                   eval      RVLNFV   =  ftdNDC
012100170420     c                   eval      RVLPDR   =  ftdPDR
012200170420     c                   eval      RVLAAS   =  ftdAAS
012300170420     c                   eval      RVLLNP   =  ftdLNP
012400170420     c                   eval      RVLNRS   =  ftdNRS
012500170420     c                   eval      RVLNSP   =  ftdNSP
012600170420     c                   eval      RVLNCL   =  ftdNCL
012700170420     c                   eval      RVLPKL   =  ftdPKL
012800170420     c                   eval      RVLVLF   =  ftdVLU
012900170420     c                   eval      RVLCAP   =  ftdCAP
013000170420     c                   eval      RVLFIN   =  ftdFIN
013100170420     c                   eval      RVLCTP   =  ftdCTP
013200170420     c                   eval      RVLITT   =  ftdITT
013300170420     c                   eval      RVLITA   =  ftdITA
013400161027      *
013500170420     c                   write     biRvl000
013600170420      *
013700160125     C                   endsr
013800020205     C**-------------------------------------------------------------**
013900000607     c     *inzsr        begsr
014000060125     C**-------------------------------------------------------------**
014100000607     c     *entry        plist
014200020205     c                   parm                    kpjba
014300170420      *
014400161026     c                   if        kpjbu = *blank
014500161026     c                   clear                   param
014600161026     c                   else
014700161026     c                   movel     kpjbu         param
014800170420     c                   if        datap2 = 0
014900170420     c                   eval      datap2 = datap
015000170420     c                   endif
015100161026     c                   endif
015200030429      *----------*
015300030429     C* GIRO DATA ODIERNA
015400030429     C                   TIME                    W0140            14 0
015500030429     C                   MOVE      W0140         UDATE8            8 0
015600160126     C                   MOVEl     W0140         oracor            6 0
015700060125     C     *eur          movel     UDATE8        dataoggi
015800160201     c                   move      dataoggi      data_gg1
015900160201     c                   move      data_gg1      oggi              8 0
016000060125     C     dataoggi      subdur    1:*d          data_gg1
016100160201     c                   move      data_gg1      ieri              8 0
016200060125      *
016300160125     C* Definisco chiavi di accesso
016400020402     c*
016500020402     c                   endsr
016600161123      * ?------------------------------------------------------------------ */
016700161123      *?      X non bloccare in nessun caso il traduttore CLIENTI
016800161123      * ?------------------------------------------------------------------ */
016900161123     C     *pssr         BEGSR
017000161123     C
017100161123     c                   seton                                        LR
017200161123     c                   return
017300161123     C
017400161123     C                   ENDSR
017500161123      * ?------------------------------------------------------------------ */
