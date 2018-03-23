000100030124     hDECEDIT('0,') DATEDIT(*DMY.)
000200030117      *****************************************************************
000300030220      *      statistica DIST.CHIUSE CON DTA CONS.ANOMALA
000400030117      *****************************************************************
000401030220     Ftitas30c  IF   E           K DISK
001400030220     Fwfdis11l  uf a E           k disk
001401030220     Fazorg01l  IF   E           K DISK
005200030220     D kpjba         e DS
005201030220     D tist60ds      e DS
005202030220     D fnevb         e DS                  extname(fnevb00f)
006102030220     D OG143         E DS
006103030220     D FIP             S              3  0 DIM(50)                              FILIALI POSTE
006104030220     D x               S              3  0 inz                                  FILIALI POSTE
006105030220     D trovato         S              1    inz                                  FILIALI POSTE
006106030220     D DISTE           S                   LIKE(TASNDC)                         FILIALI POSTE
010700030117      *****************************************************************
010701030220     C* Leggo il file degli eventi con data evento compresa nel range di
010702030220     c* date scelte a video. solo eventi MIC che hanno data immissione <>
010703030220     c* data evento
010704030220     C/EXEC SQL
010705030220     C+ DECLARE A1 CURSOR FOR SELECT * FROM fnevb00f WHERE evbdev between
010706030220     C+ :t60dti and :t60dtf and evbcev = 'MIC' and evbdev <> EVBDTV
010709030220     C/END-EXEC
010710030220      *          apertura cursore
010711030220     C/EXEC SQL
010712030220     C+ OPEN A1
010713030220     C/END-EXEC
010714030220     C                   DOU       SqlCod = 100 OR SqlCod < 0
010715030220      *          lettura cursore
010716030220     C/EXEC SQL
010717030220     C+ Fetch Next From A1 Into :fnevb
010718030220     C/END-EXEC
010719030220      *
010720030220     C                   SELECT
010721030220     C                   WHEN      SqlCod = 100
010722030220     C                   WHEN      SqlCod >= 0
010723030220     c* controllo se spediz. consegnata e nr. distinta coincide
010724030220     c* e la consegna è < della data immissione MIC
010725030220     c                   exsr      srtas
010726030220     c                   if        trovato = 'S'
010727030220     c                   exsr      aggio
010728030220     c                   end
010738030220     C                   OTHER
010739030220     C                   ENDSL
010740030220      *
010741030220     C                   ENDDO
010742030220     C/EXEC SQL
010743030220     C+ Close A1
010744030220     C/END-EXEC
014665030220     c                   seton                                        lr
014700030220     c**********************************************************************
014800030220     c     srtas         begsr
014801030220     c**********************************************************************
014803030220     c                   eval      trovato = 'N'
014804030220     c*
014805030220     c     kspe          chain     titas30c
014808030220     c                   if        %found(titas30c)
014811030220     c* controllo numero distinta
014812030221     c                   movel     evbnot        diste
014813030220     c                   if        tasndc = diste and tasdcm > 0 and
014814030220     c                             tasdcm < evbdtv
014815030220     c                   eval      trovato = 'S'
014816030220     c                   end
014818030220     c                   End
014819030220     c*
014900030220     c                   endsr
015000030220     c**********************************************************************
015100030220     c     aggio         begsr
015200030220     c**********************************************************************
015201030220     c                   clear                   wfdis1
015202030220     c     kdis          chain     wfdis11l
015203030220     c                   if        not %found(wfdis11l)
015204030220     c                   eval      wf1ddc = tasddc
015205030220     c                   eval      wf1ndc = tasndc
015206030220     c                   eval      wf1fla = taslna
015207030220     c* leggo azorg per distretto area
015208030220     c                   exsr      srorg
015209030220     c                   end
015217030319     c* poste ?
015218030220     c                   z-add     1             x
015219030220     C     taslnp        LOOKUP    FIP(x)                                 65
015220030319     c* SCELTA totale
015221030319     c                   if        t60det = 'N'
015222030220     c                   if        *in65
015223030319     c                   add       1             wf1nspo
015224030220     c                   else
015225030220     c* Bartolini
015226030220     c                   add       1             wf1nsb
015229030220     c                   end
015230030319     c* SCELTA dettaglio
015231030319     c                   else
015232030319     c                   if        *in65
015233030319     c                   z-add     1             wf1nspo
015234030319     c                   z-add     0             wf1nsb
015235030319     c                   else
015236030319     c                   z-add     0             wf1nspo
015237030319     c                   z-add     1             wf1nsb
015238030319     c                   end
015239030319     c                   eval      wf1aas = tasaas
015240030319     c                   eval      wf1lnp = taslnp
015241030319     c                   eval      wf1nrs = tasnrs
015242030319     c                   eval      wf1nsp = tasnsp
015243030319     c                   end
015244030220     c* scrivo/aggiorno
015245030220     c                   if        not %found(wfdis11l)
015246030220     c                   write     wfdis1
015247030220     c                   else
015248030319     c* totale
015249030319     c                   if        t60det = 'N'
015250030220     c                   update    wfdis1
015251030319     c                   else
015252030319     c* dettaglio
015253030319     c                   write     wfdis1
015254030220     c                   end
015255030319     c                   end
015300030220     c                   endsr
015913030220     c**********************************************************************
015914030220     c     srorg         begsr
015915030220     c**********************************************************************
015916030220     c* aggancio organigramma per vedere se spedizione posta e per reperire
015917030220     c* dati distretto e area
015918030220     c     taslna        chain     azorg01l
015919030220     c                   if        %found(azorg01l)
015920030220     c                   eval      wf1div = orgfl3
015921030220     c                   eval      wf1are = orgcar
015922030220     c                   end
015923030220     c*
015924030220     c                   endsr
015925030220     c**********************************************************************
015926030220     c     *inzsr        begsr
015927030220     c**********************************************************************
015928030220     C     *ENTRY        PLIST
015929030220     C                   PARM                    KPJBA
015930030220     C                   movel     KPJBU         tist60ds
015931030220     C* CARICO DA AZORG LE FILIALI POSTE PER TEST SUCCESSIVI
015932030220     C                   CLEAR                   FIP
015933030220     C     *LOVAL        SETLL     AZORG01L
015934030220     C                   DO        *HIVAL
015935030220     C                   READ      AZORG01L                               95
015936030220     C   95              LEAVE
015937030220     C                   MOVEL     ORGDE3        OG143
015938030220     C     §OgNTW        IFEQ      'PPT'
015939030220     C                   Z-ADD     1             X
015940030220     C     *ZEROS        LOOKUP    FIP(X)                                 23
015941030220     C   23              Z-ADD     ORGFIL        FIP(X)
015942030220     C                   END
015943030220     C                   END
015944030220     c*
015945030220     c     kspe          klist
015946030220     c                   kfld                    EVBaas
015947030220     c                   kfld                    EVBlnp
015948030220     c                   kfld                    EVBnrs
015949030220     c                   kfld                    EVBnsp
015950030220     c*
015951030220     c     kdis          klist
015952030220     c                   kfld                    taslna
015953030220     c                   kfld                    tasddc
015954030220     c                   kfld                    tasndc
016000030220     c                   endsr
