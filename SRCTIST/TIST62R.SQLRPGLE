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
001100131031     D Devb01        e DS
001200030220     D OG143         E DS
001300030220     D FIP             S              3  0 DIM(50)                              FILIALI POSTE
001400030220     D x               S              3  0 inz                                  FILIALI POSTE
001500030220     D trovato         S              1    inz                                  FILIALI POSTE
001600030220     D DISTE           S                   LIKE(TASNDC)                         FILIALI POSTE
001700030117      *****************************************************************
001800030220     C* Leggo il file degli eventi con data evento compresa nel range di
001900030220     c* date scelte a video. solo eventi MIC che hanno data immissione <>
002000030220     c* data evento
002100030220     C/EXEC SQL
002200030220     C+ DECLARE A1 CURSOR FOR SELECT * FROM fnevb00f WHERE evbdev between
002300030220     C+ :t60dti and :t60dtf and evbcev = 'MIC' and evbdev <> EVBDTV
002400030220     C/END-EXEC
002500030220      *          apertura cursore
002600030220     C/EXEC SQL
002700030220     C+ OPEN A1
002800030220     C/END-EXEC
002900030220     C                   DOU       SqlCod = 100 OR SqlCod < 0
003000030220      *          lettura cursore
003100030220     C/EXEC SQL
003200030220     C+ Fetch Next From A1 Into :fnevb
003300030220     C/END-EXEC
003400030220      *
003500030220     C                   SELECT
003600030220     C                   WHEN      SqlCod = 100
003700030220     C                   WHEN      SqlCod >= 0
003800030220     c* controllo se spediz. consegnata e nr. distinta coincide
003900030220     c* e la consegna è < della data immissione MIC
004000030220     c                   exsr      srtas
004100030220     c                   if        trovato = 'S'
004200030220     c                   exsr      aggio
004300030220     c                   end
004400030220     C                   OTHER
004500030220     C                   ENDSL
004600030220      *
004700030220     C                   ENDDO
004800030220     C/EXEC SQL
004900030220     C+ Close A1
005000030220     C/END-EXEC
005100030220     c                   seton                                        lr
005200030220     c**********************************************************************
005300030220     c     srtas         begsr
005400030220     c**********************************************************************
005500030220     c                   eval      trovato = 'N'
005600030220     c*
005700030220     c     kspe          chain     titas30c
005800030220     c                   if        %found(titas30c)
005900031113     c* controllo numero distinta e la linea d'arrivo deve essere la
006000031113     c* stessa di chi ha emesso il MIC
006100131031     c                   movel     evbnot        dEVB01
006200131031     c                   if        %editc(tasndc:'X') = §NOTNDC and tasdcm > 0
006300131031     c                              and tasdcm < evbdtv and taslna = evbfle
006400030220     c                   eval      trovato = 'S'
006500030220     c                   end
006600030220     c                   End
006700030220     c*
006800030220     c                   endsr
006900030220     c**********************************************************************
007000030220     c     aggio         begsr
007100030220     c**********************************************************************
007200030220     c                   clear                   wfdis1
007300030220     c     kdis          chain     wfdis11l
007400030220     c                   if        not %found(wfdis11l)
007500030220     c                   eval      wf1ddc = tasddc
007600030220     c                   eval      wf1ndc = tasndc
007700030220     c                   eval      wf1fla = taslna
007800030220     c* leggo azorg per distretto area
007900030220     c                   exsr      srorg
008000030220     c                   end
008100030319     c* poste ?
008200030220     c                   z-add     1             x
008300030220     C     taslnp        LOOKUP    FIP(x)                                 65
008400030319     c* SCELTA totale
008500030319     c                   if        t60det = 'N'
008600030220     c                   if        *in65
008700030319     c                   add       1             wf1nspo
008800030220     c                   else
008900030220     c* Bartolini
009000030220     c                   add       1             wf1nsb
009100030220     c                   end
009200030319     c* SCELTA dettaglio
009300030319     c                   else
009400030319     c                   if        *in65
009500030319     c                   z-add     1             wf1nspo
009600030319     c                   z-add     0             wf1nsb
009700030319     c                   else
009800030319     c                   z-add     0             wf1nspo
009900030319     c                   z-add     1             wf1nsb
010000030319     c                   end
010100030319     c                   eval      wf1aas = tasaas
010200030319     c                   eval      wf1lnp = taslnp
010300030319     c                   eval      wf1nrs = tasnrs
010400030319     c                   eval      wf1nsp = tasnsp
010500030319     c                   end
010600030220     c* scrivo/aggiorno
010700030220     c                   if        not %found(wfdis11l)
010800030220     c                   write     wfdis1
010900030220     c                   else
011000030319     c* totale
011100030319     c                   if        t60det = 'N'
011200030220     c                   update    wfdis1
011300030319     c                   else
011400030319     c* dettaglio
011500030319     c                   write     wfdis1
011600030220     c                   end
011700030319     c                   end
011800030220     c                   endsr
011900030220     c**********************************************************************
012000030220     c     srorg         begsr
012100030220     c**********************************************************************
012200030220     c* aggancio organigramma per vedere se spedizione posta e per reperire
012300030220     c* dati distretto e area
012400030220     c     taslna        chain     azorg01l
012500030220     c                   if        %found(azorg01l)
012600030220     c                   eval      wf1div = orgfl3
012700030220     c                   eval      wf1are = orgcar
012800030220     c                   end
012900030220     c*
013000030220     c                   endsr
013100030220     c**********************************************************************
013200030220     c     *inzsr        begsr
013300030220     c**********************************************************************
013400030220     C     *ENTRY        PLIST
013500030220     C                   PARM                    KPJBA
013600030220     C                   movel     KPJBU         tist60ds
013700030220     C* CARICO DA AZORG LE FILIALI POSTE PER TEST SUCCESSIVI
013800030220     C                   CLEAR                   FIP
013900030220     C     *LOVAL        SETLL     AZORG01L
014000030220     C                   DO        *HIVAL
014100030220     C                   READ      AZORG01L                               95
014200030220     C   95              LEAVE
014300030220     C                   MOVEL     ORGDE3        OG143
014400030220     C     §OgNTW        IFEQ      'PPT'
014500030220     C                   Z-ADD     1             X
014600030220     C     *ZEROS        LOOKUP    FIP(X)                                 23
014700030220     C   23              Z-ADD     ORGFIL        FIP(X)
014800030220     C                   END
014900030220     C                   END
015000030220     c*
015100030220     c     kspe          klist
015200030220     c                   kfld                    EVBaas
015300030220     c                   kfld                    EVBlnp
015400030220     c                   kfld                    EVBnrs
015500030220     c                   kfld                    EVBnsp
015600030220     c*
015700030220     c     kdis          klist
015800030220     c                   kfld                    taslna
015900030220     c                   kfld                    tasddc
016000030220     c                   kfld                    tasndc
016100030220     c                   endsr
