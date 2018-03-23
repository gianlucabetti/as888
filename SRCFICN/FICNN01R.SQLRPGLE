000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200060125      *****************************************************************
000300161025      * Il pgm scrive i record di valorizzazione autisti per giornata
000400060125      *
000500060125      *****************************************************************
000600160125     FTntbe01l  IF   E           K DISK
000700170523     Ffiftt02l  IF   E           K DISK
000800170706     Ffiftd03l  IF   E           K DISK
000900170523     Ffiapd01l  IF   E           K DISK
001000170523     Fwfftt00f  o    E             DISK
001100170706     Fwfftd00f  o    E             DISK
001200030429     F*---------------------------------------------------------------------------------
001300000607     D kpjba         e ds
001400170706     D Ficnn01DS     e ds
001500170523     D fiftt         E DS                  extname(fiftt00f) prefix(x)
001600030429      *
001700170523     d trovato         s              1
001800170523     d wtsr            s              1
001900170523     d tipo            s              1    inz('A')
002000170523     d wfvl            s              1    inz('C')
002100170706     d RagSociale      s             35
002200170706     d Filiale         s              3  0
002300170706     d dadata          s              8  0
002400170706     d  adata          s              8  0
002500170523     d codda           s              7  0
002600170523     d coda            s              7  0
002700020205     c**********************************************************************
002800170523      * ?effettua una delete preventiva
002900170420     C/EXEC SQL
003000170523     C+ delete from wfftt00f
003100170420     C/END-EXEC
003200170420      *
003300170706     C/EXEC SQL
003400170706     C+ delete from wfftd00f
003500170706     C/END-EXEC
003600170706      *
003700170706     c                   movel     N01FGS        filiale
003800170706     c                   movel     N01DADATA     dadata
003900170706     c                   movel     N01ADATA      adata
004000170706     c                   eval      codda = filiale*10000
004100170706     c                   eval      coda =  codDA + 9999
004200170706      * parametri di estrazione
004300170706      *
004400161026     C/EXEC SQL
004500170706     C+ DECLARE B1 CURSOR FOR SELECT * from fiftt00f   where
004600170706     C+  fttfvl = 'C'  and ftttsr = ' ' and fttnff > 0 and
004700170706     C+  fttddc between :dadata and :adata             and
004800170706     C+ (fttpdr between :codda and :coda or :filiale = 999)
004900170523     C+  order by fttpdr, fttddc
005000161026     C/END-EXEC
005100170706      *
005200161026     C/EXEC SQL
005300161026     C+ OPEN B1
005400161026     C/END-EXEC
005500161026     C                   DOU       SqlCod <> 0
005600161026     C/EXEC SQL
005700161026     C+ FETCH NEXT FROM B1 INTO :fiftt
005800161026     C/END-EXEC
005900161026     C                   SELECT
006000161026     c*
006100161026     C                   WHEN      SqlCod = 0
006200170523     c                   clear                   wfftt000
006300170523      * ricerca decodifica autista
006400170523     c     kapd          chain     fiapd01l
006500170523     c                   if        %found(fiapd01l)
006600170706     c                   eval      RagSociale = APDRSF
006700170523     c                   else
006800170706     c                   eval      RagSociale = *all'?'
006900170523     c                   endif
007000170523      * somma consegne per giornata autista
007100170523     c                   eval      wtsr = 'C'
007200170523     c                   exsr      ConsRitiri
007300170523      * ripulisco per ritiri
007400170523     c                   clear                   wfftt000
007500170523      * somma Ritiri per giornata autista
007600170523     c                   eval      wtsr = 'R'
007700170523     c                   exsr      ConsRitiri
007800161026      * ? EoF
007900161026     C                   WHEN      SqlCod = 100
008000161026     c                   leave
008100161026      **
008200161026      * ? Errori
008300161026     C                   WHEN      SqlCod <> 0
008400161026     c                   leave
008500161026      **
008600161026     C                   ENDSL
008700161026      **
008800161026     C                   ENDDO
008900161026     C/EXEC SQL
009000161026     C+ CLOSE B1
009100161026     C/END-EXEC
009200030429      *
009300000607     c                   seton                                        lr
009400161025      * ?___________________________________________________________________
009500170523     c     ConsRitiri    begsr
009600161025      * ?___________________________________________________________________
009700170523     c                   eval      trovato = *blank
009800170523     c     kftt          setll     fiftt02l
009900170523     c                   do        *hival
010000170523     c     kftt          reade     fiftt02l
010100170523     c                   if        %eof(fiftt02l)
010200170523     c                   leave
010300170523     c                   endif
010400170523      *totalizzo campi quantità per tipo servizio
010500170523     c                   eval      trovato = 'S'
010600170523     C                   add       FTTSET        WFTTSET
010700170523     C                   add       FTTCLA        WFTTCLA
010800170523     C                   add       FTTKFA        WFTTKFA
010900170523     C                   add       FTTTVL        WFTTTVL
011000170706     c                   exsr      scriviFTD
011100170523     c                   enddo
011200170523     c                   if        trovato = 'S'
011300170706     c                   exsr      scriviFTT
011400170523     c                   endif
011500161025      *
011600161025     c                   endsr
011700161025     C*_____________________________________________________________________
011800170523     C** scrittura  valorizzazione raggruppata per tipo servizio
011900161025     C*_____________________________________________________________________
012000170706     C     scriviFTT     BEGSR
012100170706     c                   eval      WAPDRSF  =  RagSociale
012200170523     c                   eval      WFTTPDR  =  xFTTPDR
012300170523     c                   eval      Wftttsr  =  Wtsr
012400170523     c                   eval      WFTTFGS  =  xFTTFGS
012500170523     c                   eval      WFTTDDC  =  xFTTDDC
012600170523     c                   eval      WFTTSOC  =  xFTTSOC
012700170523     c                   eval      WFTTCDF  =  xFTTCDF
012800170523     c                   eval      WFTTDFT  =  xFTTDFT
012900170523     c                   eval      wfttnff  =  xfttnff
013000161027      *
013100170523     c                   write     wfftt000
013200160125     C                   endsr
013300170706     C*_____________________________________________________________________
013400170706     C** scrittura  dettaglio raggruppata per tipo servizio
013500170706     C*_____________________________________________________________________
013600170706     C     scriviFTD     BEGSR
013700170706      *
013800170706     c                   clear                   wfftd000
013900170706     C     Kftd          setll     fiftd03l
014000170706     C     Kftd          reade     fiftd03l
014100170706     c                   dow       not %EoF(fiftd03l)
014200170706     c                   eval      WAPDRSF  =  RagSociale
014300170706     c                   eval      WFTDPDR  =  FTdPDR
014400170706     c                   eval      WFTDTSR  =  FTdTSR
014500170706     c                   eval      WFTDFGS  =  FTdFGS
014600170706     c                   eval      WFTDDDC  =  FTdDDC
014700170706     c                   eval      WFTDNDC  =  FTdNDC
014800170706      *
014900170706     c                   eval      WFTDaas  =  ftdaas
015000170706     c                   eval      WFTDlnp  =  ftdlnp
015100170706     c                   eval      WFTDnrs  =  ftdnrs
015200170706     c                   eval      WFTDnsp  =  ftdnsp
015300170706     c                   eval      WFTDRSC  =  ftdRSC
015400170706     c                   eval      WFTDNCL  =  ftdNCL
015500170706     c                   eval      wftdPKL  =  ftdPKL
015600170706     c                   eval      wftdVLU  =  ftdVLU
015700170706     c                   eval      wftdTMC  =  ftdTMC
015800170706     c                   eval      wftdCMC  =  ftdCMC
015900170706      *
016000170706     c                   write     wfftd000
016100170706     C     Kftd          reade     fiftd03l
016200170706     C                   enddo
016300170706      *
016400170706     C                   endsr
016500020205     C**-------------------------------------------------------------**
016600000607     c     *inzsr        begsr
016700060125     C**-------------------------------------------------------------**
016800000607     c     *entry        plist
016900020205     c                   parm                    kpjba
017000170706     c                   movel     kpjbu         ficnn01DS
017100060125      *
017200160125     C* Definisco chiavi di accesso
017300170523     C     Kftt          KLIST
017400170523     C                   KFLD                    xfttpdr
017500170523     C                   KFLD                    wfvl
017600170523     C                   KFLD                    xfttddc
017700170523     C                   KFLD                    wtsr
017800161025      *
017900170706     C     Kftd          KLIST
018000170706     C                   KFLD                    fttPDR
018100170706     C                   KFLD                    fttTSR
018200170706     C                   KFLD                    fttNDC
018300170706     C                   KFLD                    fttDDC
018400170706      *
018500170523     c     kapd          klist
018600170523     c                   kFLD                    Tipo
018700170523     c                   kFLD                    xfttpdr
018800161027      *
018900020402     c                   endsr
