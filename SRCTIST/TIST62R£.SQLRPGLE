000100030124     hDECEDIT('0,') DATEDIT(*DMY.)
000200030117      *****************************************************************
000300030220      *      statistica DIST.CHIUSE CON DTA CONS.ANOMALA
000400030117      *****************************************************************
000500030220     Ftitas30c  IF   E           K DISK
000600030220     Fwfdis11l  uf a E           k disk
000700030220     Fazorg01l  IF   E           K DISK
000800030220     D kpjba         e DS
000900030220     D tist60ds      e DS
001000030220     D fnevb         e DS                  extname(fnevb00f)
001100030220     D OG143         E DS
001200030220     D FIP             S              3  0 DIM(50)                              FILIALI POSTE
001300030220     D x               S              3  0 inz                                  FILIALI POSTE
001400030220     D trovato         S              1    inz                                  FILIALI POSTE
001500030220     D DISTE           S                   LIKE(TASNDC)                         FILIALI POSTE
001600030117      *****************************************************************
001700030220     C* Leggo il file degli eventi con data evento compresa nel range di
001800030220     c* date scelte a video. solo eventi MIC che hanno data immissione <>
001900030220     c* data evento
002000030220     C/EXEC SQL
002100030220     C+ DECLARE A1 CURSOR FOR SELECT * FROM fnevb00f WHERE evbdev between
002200030220     C+ :t60dti and :t60dtf and evbcev = 'MIC' and evbdev <> EVBDTV
002300030220     C/END-EXEC
002400030220      *          apertura cursore
002500030220     C/EXEC SQL
002600030220     C+ OPEN A1
002700030220     C/END-EXEC
002800030220     C                   DOU       SqlCod = 100 OR SqlCod < 0
002900030220      *          lettura cursore
003000030220     C/EXEC SQL
003100030220     C+ Fetch Next From A1 Into :fnevb
003200030220     C/END-EXEC
003300030220      *
003400030220     C                   SELECT
003500030220     C                   WHEN      SqlCod = 100
003600030220     C                   WHEN      SqlCod >= 0
003700030220     c* controllo se spediz. consegnata e nr. distinta coincide
003800030220     c* e la consegna è < della data immissione MIC
003900030220     c                   exsr      srtas
004000030220     c                   if        trovato = 'S'
004100030220     c                   exsr      aggio
004200030220     c                   end
004300030220     C                   OTHER
004400030220     C                   ENDSL
004500030220      *
004600030220     C                   ENDDO
004700030220     C/EXEC SQL
004800030220     C+ Close A1
004900030220     C/END-EXEC
005000030220     c                   seton                                        lr
005100030220     c**********************************************************************
005200030220     c     srtas         begsr
005300030220     c**********************************************************************
005400030220     c                   eval      trovato = 'N'
005500030220     c*
005600030220     c     kspe          chain     titas30c
005700030220     c                   if        %found(titas30c)
005800031113     c* controllo numero distinta e la linea d'arrivo deve essere la
005900031113     c* stessa di chi ha emesso il MIC
006000030221     c                   movel     evbnot        diste
006100030220     c                   if        tasndc = diste and tasdcm > 0 and
006200031113     c                             tasdcm < evbdtv and taslna = evbfle
006300030220     c                   eval      trovato = 'S'
006400030220     c                   end
006500030220     c                   End
006600030220     c*
006700030220     c                   endsr
006800030220     c**********************************************************************
006900030220     c     aggio         begsr
007000030220     c**********************************************************************
007100030220     c                   clear                   wfdis1
007200030220     c     kdis          chain     wfdis11l
007300030220     c                   if        not %found(wfdis11l)
007400030220     c                   eval      wf1ddc = tasddc
007500030220     c                   eval      wf1ndc = tasndc
007600030220     c                   eval      wf1fla = taslna
007700030220     c* leggo azorg per distretto area
007800030220     c                   exsr      srorg
007900030220     c                   end
008000030319     c* poste ?
008100030220     c                   z-add     1             x
008200030220     C     taslnp        LOOKUP    FIP(x)                                 65
008300030319     c* SCELTA totale
008400030319     c                   if        t60det = 'N'
008500030220     c                   if        *in65
008600030319     c                   add       1             wf1nspo
008700030220     c                   else
008800030220     c* Bartolini
008900030220     c                   add       1             wf1nsb
009000030220     c                   end
009100030319     c* SCELTA dettaglio
009200030319     c                   else
009300030319     c                   if        *in65
009400030319     c                   z-add     1             wf1nspo
009500030319     c                   z-add     0             wf1nsb
009600030319     c                   else
009700030319     c                   z-add     0             wf1nspo
009800030319     c                   z-add     1             wf1nsb
009900030319     c                   end
010000030319     c                   eval      wf1aas = tasaas
010100030319     c                   eval      wf1lnp = taslnp
010200030319     c                   eval      wf1nrs = tasnrs
010300030319     c                   eval      wf1nsp = tasnsp
010400030319     c                   end
010500030220     c* scrivo/aggiorno
010600030220     c                   if        not %found(wfdis11l)
010700030220     c                   write     wfdis1
010800030220     c                   else
010900030319     c* totale
011000030319     c                   if        t60det = 'N'
011100030220     c                   update    wfdis1
011200030319     c                   else
011300030319     c* dettaglio
011400030319     c                   write     wfdis1
011500030220     c                   end
011600030319     c                   end
011700030220     c                   endsr
011800030220     c**********************************************************************
011900030220     c     srorg         begsr
012000030220     c**********************************************************************
012100030220     c* aggancio organigramma per vedere se spedizione posta e per reperire
012200030220     c* dati distretto e area
012300030220     c     taslna        chain     azorg01l
012400030220     c                   if        %found(azorg01l)
012500030220     c                   eval      wf1div = orgfl3
012600030220     c                   eval      wf1are = orgcar
012700030220     c                   end
012800030220     c*
012900030220     c                   endsr
013000030220     c**********************************************************************
013100030220     c     *inzsr        begsr
013200030220     c**********************************************************************
013300030220     C     *ENTRY        PLIST
013400030220     C                   PARM                    KPJBA
013500030220     C                   movel     KPJBU         tist60ds
013600030220     C* CARICO DA AZORG LE FILIALI POSTE PER TEST SUCCESSIVI
013700030220     C                   CLEAR                   FIP
013800030220     C     *LOVAL        SETLL     AZORG01L
013900030220     C                   DO        *HIVAL
014000030220     C                   READ      AZORG01L                               95
014100030220     C   95              LEAVE
014200030220     C                   MOVEL     ORGDE3        OG143
014300030220     C     §OgNTW        IFEQ      'PPT'
014400030220     C                   Z-ADD     1             X
014500030220     C     *ZEROS        LOOKUP    FIP(X)                                 23
014600030220     C   23              Z-ADD     ORGFIL        FIP(X)
014700030220     C                   END
014800030220     C                   END
014900030220     c*
015000030220     c     kspe          klist
015100030220     c                   kfld                    EVBaas
015200030220     c                   kfld                    EVBlnp
015300030220     c                   kfld                    EVBnrs
015400030220     c                   kfld                    EVBnsp
015500030220     c*
015600030220     c     kdis          klist
015700030220     c                   kfld                    taslna
015800030220     c                   kfld                    tasddc
015900030220     c                   kfld                    tasndc
016000030220     c                   endsr
