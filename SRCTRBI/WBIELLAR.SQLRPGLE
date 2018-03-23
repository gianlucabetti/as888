000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200060125      *****************************************************************
000300161025      * Il pgm scrive i record di valorizzazione autisti per giornata
000400060125      *
000500060125      *****************************************************************
000600160125     FTntbe01l  IF   E           K DISK
000700170523     Ffiftt02l  IF   E           K DISK
000800170523     Ffiapd01l  IF   E           K DISK
000900170523     Fwfftt00f  o    E             DISK
001000030429     F*---------------------------------------------------------------------------------
001100000607     D kpjba         e ds
001500170523     D fiftt         E DS                  extname(fiftt00f) prefix(x)
002000030429      *
002001170523     d trovato         s              1
002101170523     d wtsr            s              1
002102170523     d tipo            s              1    inz('A')
002103170523     d wfvl            s              1    inz('C')
002104170523     d codda           s              7  0
002105170523     d coda            s              7  0
002900020205     c**********************************************************************
007900170523      * ?effettua una delete preventiva
008000170420     C/EXEC SQL
008100170523     C+ delete from wfftt00f
008200170420     C/END-EXEC
008300170420      *
008301170523     c                   eval      codda = 0860000
008302170523     c                   eval      coda =  0869999
008303170523     c
008304170523     c
008400161026     C/EXEC SQL
008500161026     C+ DECLARE B1 CURSOR FOR SELECT * from fiftt00f where
008600170523     C+  fttfvl = 'C'  and ftttsr = ' ' and fttnff > 0  and
008700170523     C+  fttpdr between :codda and :coda
008800170523     C+  order by fttpdr, fttddc
008900161026     C/END-EXEC
009000161026     C/EXEC SQL
009100161026     C+ OPEN B1
009200161026     C/END-EXEC
009300161026     C                   DOU       SqlCod <> 0
009400161026     C/EXEC SQL
009500161026     C+ FETCH NEXT FROM B1 INTO :fiftt
009600161026     C/END-EXEC
009700161026     C                   SELECT
009800161026     c*
009900161026     C                   WHEN      SqlCod = 0
010000170523     c                   clear                   wfftt000
010001170523      * ricerca decodifica autista
010002170523     c     kapd          chain     fiapd01l
010003170523     c                   if        %found(fiapd01l)
010004170523     c                   eval      WAPDRSF  =  APDRSF
010005170523     c                   else
010006170523     c                   eval      WAPDRSF  =  *all'?'
010008170523     c                   endif
010100170523      * somma consegne per giornata autista
010101170523     c                   eval      wtsr = 'C'
010400170523     c                   exsr      ConsRitiri
010500170523      * ripulisco per ritiri
010501170523     c                   clear                   wfftt000
010502170523     c                   eval      WAPDRSF  =  APDRSF
010503170523      * somma Ritiri per giornata autista
010504170523     c                   eval      wtsr = 'R'
010505170523     c                   exsr      ConsRitiri
011300161026      * ? EoF
011400161026     C                   WHEN      SqlCod = 100
011500161026     c                   leave
011600161026      **
011700161026      * ? Errori
011800161026     C                   WHEN      SqlCod <> 0
011900161026     c                   leave
012000161026      **
012100161026     C                   ENDSL
012200161026      **
012300161026     C                   ENDDO
012400161026     C/EXEC SQL
012500161026     C+ CLOSE B1
012600161026     C/END-EXEC
012800030429      *
012900000607     c                   seton                                        lr
013000161025      * ?___________________________________________________________________
013100170523     c     ConsRitiri    begsr
013200161025      * ?___________________________________________________________________
013201170523     c                   eval      trovato = *blank
014100170523     c     kftt          setll     fiftt02l
014101170523     c                   do        *hival
014102170523     c     kftt          reade     fiftt02l
014200170523     c                   if        %eof(fiftt02l)
014300170523     c                   leave
014301170523     c                   endif
014302170523      *totalizzo campi quantità per tipo servizio
014500170523     c                   eval      trovato = 'S'
014501170523     C                   add       FTTSET        WFTTSET
014502170523     C                   add       FTTCLA        WFTTCLA
014503170523     C                   add       FTTKFA        WFTTKFA
014504170523     C                   add       FTTTVL        WFTTTVL
014900170523     c                   enddo
014901170523     c                   if        trovato = 'S'
014902170523     c                   exsr      scrivi
014903170523     c                   endif
017200161025      *
017300161025     c                   endsr
024700161025     C*_____________________________________________________________________
024800170523     C** scrittura  valorizzazione raggruppata per tipo servizio
024900161025     C*_____________________________________________________________________
025000170523     C     scrivi        BEGSR
025100170523     c                   eval      WFTTPDR  =  xFTTPDR
025300170523     c                   eval      Wftttsr  =  Wtsr
025400170523     c                   eval      WFTTFGS  =  xFTTFGS
025500170523     c                   eval      WFTTDDC  =  xFTTDDC
026000170523     c                   eval      WFTTSOC  =  xFTTSOC
026100170523     c                   eval      WFTTCDF  =  xFTTCDF
026200170523     c                   eval      WFTTDFT  =  xFTTDFT
026300170523     c                   eval      wfttnff  =  xfttnff
026600161027      *
026700170523     c                   write     wfftt000
026800160125     C                   endsr
026900020205     C**-------------------------------------------------------------**
027000000607     c     *inzsr        begsr
027100060125     C**-------------------------------------------------------------**
027200000607     c     *entry        plist
027300020205     c                   parm                    kpjba
028900060125      *
029000160125     C* Definisco chiavi di accesso
029100170523     C     Kftt          KLIST
029200170523     C                   KFLD                    xfttpdr
029300170523     C                   KFLD                    wfvl
029301170523     C                   KFLD                    xfttddc
029302170523     C                   KFLD                    wtsr
029400161025      *
029500170523     c     kapd          klist
029600170523     c                   kFLD                    Tipo
029700170523     c                   kFLD                    xfttpdr
029800161027      *
030300020402     c                   endsr
