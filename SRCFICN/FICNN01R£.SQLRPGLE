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
002100170706     d Filiale         s              3  0
002200170706     d dadata          s              8  0
002300170706     d  adata          s              8  0
002400170523     d codda           s              7  0
002500170523     d coda            s              7  0
002600020205     c**********************************************************************
002700170523      * ?effettua una delete preventiva
002800170420     C/EXEC SQL
002900170523     C+ delete from wfftt00f
003000170420     C/END-EXEC
003100170420      *
003200170706     C/EXEC SQL
003300170706     C+ delete from wfftd00f
003400170706     C/END-EXEC
003500170706      *
003600170706     c                   movel     N01FGS        filiale
003700170706     c                   movel     N01DADATA     dadata
003800170706     c                   movel     N01ADATA      adata
003900170706     c                   eval      codda = filiale*10000
004000170706     c                   eval      coda =  codDA + 9999
004100170706      * parametri di estrazione
004200170706      *
004300161026     C/EXEC SQL
004400161026     C+ DECLARE B1 CURSOR FOR SELECT * from fiftt00f where
004500170523     C+  fttfvl = 'C'  and ftttsr = ' ' and fttnff > 0  and
004600170706     C+  fttpdr between :codda and :coda          and
004700170706     C+  fttddc between :dadata and :adata
004800170523     C+  order by fttpdr, fttddc
004900161026     C/END-EXEC
005000170706      *
005100161026     C/EXEC SQL
005200161026     C+ OPEN B1
005300161026     C/END-EXEC
005400161026     C                   DOU       SqlCod <> 0
005500161026     C/EXEC SQL
005600161026     C+ FETCH NEXT FROM B1 INTO :fiftt
005700161026     C/END-EXEC
005800161026     C                   SELECT
005900161026     c*
006000161026     C                   WHEN      SqlCod = 0
006100170523     c                   clear                   wfftt000
006200170523      * ricerca decodifica autista
006300170523     c     kapd          chain     fiapd01l
006400170523     c                   if        %found(fiapd01l)
006500170523     c                   eval      WAPDRSF  =  APDRSF
006600170523     c                   else
006700170523     c                   eval      WAPDRSF  =  *all'?'
006800170523     c                   endif
006900170523      * somma consegne per giornata autista
007000170523     c                   eval      wtsr = 'C'
007100170523     c                   exsr      ConsRitiri
007200170523      * ripulisco per ritiri
007300170523     c                   clear                   wfftt000
007400170523     c                   eval      WAPDRSF  =  APDRSF
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
012100170523     c                   eval      WFTTPDR  =  xFTTPDR
012200170523     c                   eval      Wftttsr  =  Wtsr
012300170523     c                   eval      WFTTFGS  =  xFTTFGS
012400170523     c                   eval      WFTTDDC  =  xFTTDDC
012500170523     c                   eval      WFTTSOC  =  xFTTSOC
012600170523     c                   eval      WFTTCDF  =  xFTTCDF
012700170523     c                   eval      WFTTDFT  =  xFTTDFT
012800170523     c                   eval      wfttnff  =  xfttnff
012900161027      *
013000170523     c                   write     wfftt000
013100160125     C                   endsr
013200170706     C*_____________________________________________________________________
013300170706     C** scrittura  dettaglio raggruppata per tipo servizio
013400170706     C*_____________________________________________________________________
013500170706     C     scriviFTD     BEGSR
013600170706      *
013700170706     c                   clear                   wfftd000
013800170706     C     Kftd          setll     fiftd03l
013900170706     C     Kftd          reade     fiftd03l
014000170706     c                   dow       not %EoF(fiftd03l)
014100170706     c                   eval      WFTDPDR  =  FTdPDR
014200170706     c                   eval      WFTDTSR  =  FTdTSR
014300170706     c                   eval      WFTDFGS  =  FTdFGS
014400170706     c                   eval      WFTDDDC  =  FTdDDC
014500170706     c                   eval      WFTDNDC  =  FTdNDC
014600170706      *
014700170706     c                   eval      WFTDaas  =  ftdaas
014800170706     c                   eval      WFTDlnp  =  ftdlnp
014900170706     c                   eval      WFTDnrs  =  ftdnrs
015000170706     c                   eval      WFTDnsp  =  ftdnsp
015100170706     c                   eval      WFTDRSC  =  ftdRSC
015200170706     c                   eval      WFTDNCL  =  ftdNCL
015300170706     c                   eval      wftdPKL  =  ftdPKL
015400170706     c                   eval      wftdVLU  =  ftdVLU
015500170706     c                   eval      wftdTMC  =  ftdTMC
015600170706     c                   eval      wftdCMC  =  ftdCMC
015700170706      *
015800170706     c                   write     wfftd000
015900170706     C     Kftd          reade     fiftd03l
016000170706     C                   enddo
016100170706      *
016200170706     C                   endsr
016300020205     C**-------------------------------------------------------------**
016400000607     c     *inzsr        begsr
016500060125     C**-------------------------------------------------------------**
016600000607     c     *entry        plist
016700020205     c                   parm                    kpjba
016800170706     c                   movel     kpjbu         ficnn01DS
016900060125      *
017000160125     C* Definisco chiavi di accesso
017100170523     C     Kftt          KLIST
017200170523     C                   KFLD                    xfttpdr
017300170523     C                   KFLD                    wfvl
017400170523     C                   KFLD                    xfttddc
017500170523     C                   KFLD                    wtsr
017600161025      *
017700170706     C     Kftd          KLIST
017800170706     C                   KFLD                    fttPDR
017900170706     C                   KFLD                    fttTSR
018000170706     C                   KFLD                    fttNDC
018100170706     C                   KFLD                    fttDDC
018200170706      *
018300170523     c     kapd          klist
018400170523     c                   kFLD                    Tipo
018500170523     c                   kFLD                    xfttpdr
018600161027      *
018700020402     c                   endsr
