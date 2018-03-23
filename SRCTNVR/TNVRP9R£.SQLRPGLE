000100051229      *PARMS CLOSQLCSR(*ENDMOD)
000200051229      *PARMS DYNUSRPRF(*OWNER)
000300051227      *-----------------------------------------------------------------*
000400060405      *  Elabora calendario eventi particolari per scrivere FNEVB
000500051227      *-----------------------------------------------------------------*
000600060405     ffnevb04l  if a e           k disk
000700060405     fazcep02l  if   e           k disk
000800060405     ftabel00f  if   e           k disk
000900060405     ftntbe01l  uf   e           k disk
001000060405     ffnarb54l  uf   e           k disk
001100060405     ffnarb01l  uf   e           k disk    rename(fnarb000:fnarb001)
001200051227
001300060405     d
001400060405     D REC             S              2    DIM(50)
001500060405     D                 DS
001600060405     d kdata           s                   like(cepdev)
001700060405     d kdcm            s                   like(cepdev)
001800060405     d kTbeCod         s                   Like(TbeCod) inz('SDF')
001900060405     d kTbeKe1         s                   Like(TbeKe1)
002000060405     d ktbkut          s                   like(tblkut) inz(1)
002100060405     d ktbcod          s                   like(tblcod) inz('3A')
002200060405     d ktbkey          s                   like(tblkey)
002300060405     d kora            s                   like(evbhev) inz(2000)
002400051229     d wrkgetlista     s           4096    varying
002500051227
002600060405     d DSFNARB       e ds                  extname(FNARB00f)
002700051227     d kpjba         e ds
002800060405     D DSDF          E DS                  INZ
002900060405     D DS3a          E DS                  INZ
003000060405     D DS2z          E DS                  INZ
003100060405     d
003200060328     d Tibs02Ds      e ds
003300060405     d Tisi95ds      e ds
003400060830      * ds per il controllo della presenza di CA per la spedizione richiesta
003500060830     d FIDN12DS      e ds
003600060830     d  skKey                 26    305    dim(20)
003700060830     d  skAnn                306    325    dim(20)
003800060830     d  skDca                326    485  0 dim(20)
003900060830     d  skFca                486    545  0 dim(20)
004000060830     d  skDch                546    705  0 dim(20)
004100060830     d  skCch                706    745    dim(20)
004200060830
004300060830     d dsKey           ds
004400060830     d  dsaac                         4  0
004500060830     d  dsfil                         3  0
004600060830     d  dsnca                         7  0
004700060830      *
004800060405     d Dataiso         s               d   datfmt(*iso)
004900060405     d DataUltima      s               d   datfmt(*iso)
005000060830     d Datamese        s               d   datfmt(*iso)
005100051227      *-----------------------------------------------------------------*
005200060405     c* Vedo quali date elaborare
005300060405     c                   exsr      DETERMDATA
005400060405     c
005500060405     c                   dow       dataiso<=dataultima
005600060405     c                   movel     dataiso       kdata
005700060405     c     kdata         setll     azcep02l
005800060405     c     kdata         reade     azcep02l
005900060405     c                   dow       not %eof(azcep02l)
006000060405     c
006100060405     c* Elaboro solo eventi con §2zcep=N (noninseriti a mano in chiusura
006200060405     c*  distinta)
006300060405     c                   eval      ktbcod='2Z'
006400060405     c                   movel(p)  cepcev        ktbkey
006500060405     c     ke3tab        chain     tabel
006600060405     c                   if        %found(tabel00f)
006700060405     c                   movel     tbluni        ds2z
006800060405     c                   else
006900060405     c                   clear                   ds2z
007000060405     c                   endif
007100060405     c                   if        §2zCEP='N'
007200060405     c*
007300060405     c* Due cicli di elaborazione:
007400060405     c* PER CAP/località --> SQL
007500051229
007600060405     c                   if        cepfil=0
007700060104     c                   exsr      PREPASQL
007800060405     c
007900060405     c                   else
008000060405     c* Per Linea di arrivo --> leggo lf apposita
008100060405     c                   exsr      LEGPO
008200060405     c                   endif
008300060405     c                   endif
008400060405     c
008500060405     c     kdata         reade     azcep02l
008600060405     c                   enddo
008700060405     c
008800060405     c     dataiso       adddur    1:*d          dataiso
008900060405     c                   enddo
009000060405
009100060405     c* Aggiorno data ultima elaborazione
009200060405     c                   movel     dataultima    kdata
009300060405     c
009400060405     c                   Movel(p)  046           kTbeKe1
009500060405     c     Ktbe          Chain     Tntbe01l
009600060405     c                   If        %Found(Tntbe01l)
009700060405     c                   Movel     TbeUni        dSdf
009800060405     c                   If        §SdfCEP < kdata
009900060405     c                   Z-Add     kdata         §SdfCEP
010000060405     c                   EndIf
010100060405     c                   Movel     dSdf          TbeUni
010200060405     c                   Update    Tntbe000
010300060405     C                   ENDIF
010400060405     c
010500051229     c                   Eval      *InLr = *On
010600051229
010700060405      *-----------------------------------------------------------------*
010800060405      * Determino date/data da elaborare
010900060405      *-----------------------------------------------------------------*
011000060405     c     DetermData    begsr
011100060405     c                   time                    w0140            14 0
011200060405     c     *eur          move      w0140         dataultima
011300060405     c                   movel     dataultima    datacor           8 0
011400060830     c                   movel     dataultima    datamese
011500060405     c                   movel     w0140         wora              4 0
011600060405     c* Fino alle ore 18:00 elaboro fino al giorno precedente
011700060405     c* Dopo   le ore 18:00 elaboro fino al giorno stesso
011800060405     c                   if        wora<=1800
011900060405     c     dataultima    subdur    1:*d          dataultima
012000060405     c                   endif
012100060405     c
012200060405     c                   if        §sdfcep=0
012300060405     c     dataultima    subdur    5:*d          dataiso
012400060405     c                   movel     dataiso       §sdfcep
012500060405     c                   endif
012600060405     c     *iso          move      §sdfcep       dataiso
012700060405     c     dataiso       adddur    1:*d          dataiso
012800060830     c
012900060830     c* Determino oggi - 1 mese per le c.a.
013000060830     c     datamese      subdur    1:*m          datamese
013100060830     c                   movel     datamese      unmesefa          8 0
013200060405     c
013300060405     c                   endsr
013400060405      *-----------------------------------------------------------------*
013500060405      * Lettura bolle arrivi per linea di arrivo e DCM=0 o > data elab
013600060405      *-----------------------------------------------------------------*
013700060405     c     LegPO         begsr
013800060405     c
013900060405     c                   clear                   kdcm
014000060405     c     karb          setll     fnarb54l
014100060405     c     karb          reade(N)  fnarb54l
014200060830     c
014300060405    1c                   dow       not %eof(fnarb54l)
014400060830     c                   clear                   escludica         1
014500060405     c
014600060405     c* Escludo le bolle di recupero
014700060405     c* Escludo le bolle con data spedizione >= data elab.
014800060405     c     arbcbo        lookup    rec                                    30
014900060405    2c                   if        not *in30
015000060405     c                   movel     arbaas        arbdsp            8 0
015100060405     c                   move      arbmgs        arbdsp
015200060405    3c                   if        arbdsp<kdata
015300060830     c
015400060830     c* Escludo le spedizioni con C.A. da almeno 1 mese
015500060830     c                   clear                   fidn12ds
015600060830     c                   eval      i12aas = arbaas
015700060830     c                   eval      i12lnp = arblnp
015800060830     c                   eval      i12nrs = arbnrs
015900060830     c                   eval      i12nsp = arbnsp
016000060830     c                   eval      i12fel = 'E'
016100060830     c                   eval      i12fan = 'E'
016200060830      *
016300060830     c                   call      'FIDN12R'
016400060830     c                   parm                    fidn12ds
016500060830      *
016600060830      * se non ci sono errori
016700060830     c                   if        o12err = ' '
016800060830      * se numero di CA trovate maggiore di zero
016900060830     c                   if        o12nca > 0
017000060830     c                   eval      EscludiCA='S'
017100060830     c
017200060830     c                   z-add     *zeros        ii                3 0
017300060830     c     1             do        o12nca        ii
017400060830     c                   movea     skkey(ii)     dskey
017500060830     C                   Z-ADD     skdca(ii)     dctdca            8 0
017600060830     C                   Z-ADD     skdch(ii)     dctdch            8 0
017700060830     c* Se chiusa --> elaboro
017800060830     c                   if        dctdch>0
017900060830     c                   clear                   escludiCA
018000060830     c                   leave
018100060830     c                   endif
018200060830     c*
018300060830     c                   if        dctdca>unmesefa
018400060830     c                   clear                   escludiCA
018500060830     c                   leave
018600060830     c                   endif
018700060830     C                   ENDDO
018800060830      *
018900060830     c                   endif
019000060830     c                   endif
019100060405     c
019200060405     c* Elaboro le spedizioni abilitate in base a spunta part o arr
019300060830    4c                   if        EscludiCA=' ' and
019400060831    4c                             (arbama>0 or arbamp=1)
019500060405     c                   exsr      ScriviEVB
019600060405    4c                   endif
019700060405    3c                   endif
019800060405    2c                   endif
019900060405     c
020000060405     c     karb          reade(N)  fnarb54l
020100060405    1c                   enddo
020200060405     c
020300060405     c                   z-add     kdata         kdcm
020400060405     c* Leeggo bolle con data consegna > kdata
020500060405     c     karb          setgt     fnarb54l
020600060405     c     cepfil        reade     fnarb54l
020700060405    1c                   dow       not %eof(fnarb54l)
020800060405     c
020900060405     c* Escludo le bolle di recupero
021000060405     c     arbcbo        lookup    rec                                    30
021100060405    2c                   if        not *in30
021200060405     c                   movel     arbaas        arbdsp
021300060405     c                   move      arbmgs        arbdsp
021400060405    3c                   if        arbdsp<kdata
021500060405     c
021600060405     c                   exsr      ScriviEVB
021700060405     c
021800060405     c* In questo caso, se calcolata, ricalcolo affidabilità
021900060405    4c                   if        arbdce>0
022000060405     c                   movel     'S'           arbfbs
022100060405     c                   clear                   arbdti
022200060405     c                   clear                   arbhti
022300060405     c                   update    fnarb000
022400060405     c                   else
022500060405     c                   unlock    fnarb54l
022600060405    4c                   endif
022700060405    3c                   endif
022800060405     c
022900060405    2c                   endif
023000060405     c
023100060405     c     cepfil        reade     fnarb54l
023200060405    1c                   enddo
023300060405     c                   endsr
023400060405      *-----------------------------------------------------------------*
023500060405      * SCrivi record in fnevb00f
023600060405      *-----------------------------------------------------------------*
023700060405     c     ScriviEVB     begsr
023800060405     c     kevb          chain     fnevb04l
023900060405     c                   if        not %found(fnevb04l)
024000060405     c                   clear                   fnevb000
024100060405     c                   move      datacor       evbdtv
024200060405     c                   time                    evborv
024300060405     c                   movel     046           evbfle
024400060405     c                   z-add     arbaas        evbaas
024500060405     c                   z-add     arblnp        evblnp
024600060405     c                   z-add     arbnrs        evbnrs
024700060405     c                   z-add     arbnsp        evbnsp
024800060405     c                   z-add     kdata         evbdev
024900060405     c                   z-add     2000          evbhev
025000060405     c                   movel     cepcev        evbcev
025100060405     c                   write     fnevb000
025200060405     c                   endif
025300060405     c
025400060405     c                   endsr
025500051229      *-----------------------------------------------------------------*
025600060405     c* Lettura FNARB con selezione per CAP
025700051229      *-----------------------------------------------------------------*
025800060104     c     prepasql      begsr
025900051229
026000051229      *?Imposto la parte fissa dell'istruzione sql
026100051229     c                   eval      wrkgetlista =
026200060405     c                             'select *  from FNARB00F where -
026300060405     c                             ARBCAD ='''+ cepCAP +''' and  ARBNZD=''   '
026400060405     c                             + CEPNAR + ''' and (ARBDCM=0 or ARBDCM>'
026500060405     c                             +  %editc(kdata:'X') + ')'
026600060322      *?Ordinamento
026700060322     c                   eval      wrkgetlista = wrkgetlista +
026800060405     c                             ' order by arblna, arbdcm'
026900051229      *?Solo lettura
027000051229     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
027100051229
027200051229     C/EXEC SQL
027300051229     C+ PREPARE s1 FROM :wrkgetlista
027400051229     C/END-EXEC
027500051229     C
027600051229     C/EXEC SQL
027700051229     C+ DECLARE A1 CURSOR FOR S1
027800051229     C/END-EXEC
027900051229     C
028000051229     C/EXEC SQL
028100051229     C+ OPEN a1
028200051229     C/END-EXEC
028300051229
028400051229      *?Leggo il file
028500051229     c                   do        *hival
028600051229     C/EXEC SQL
028700060405     C+ FETCH NEXT FROM a1 INTO : DSFNARB
028800051229     C/END-EXEC
028900051229
029000051229     c                   select
029100051229
029200051229     c                   when      sqlcod = 100
029300051229     c                   leave
029400051229     c                   when      sqlcod < 0
029500051229     c                   seton                                        H1
029600051229     c                   exsr      srfine
029700051229     c                   other
029800060321     c
029900060321     c* Sommo e a cambio p.o. o data scrivo
030000060405     c                   EXSR      LeggiARB
030100060104     c
030200051229     c                   endsl
030300051229
030400051229     c                   enddo
030500051229     C/EXEC SQL
030600051229     C+ CLOSE a1
030700051229     C/END-EXEC
030800051229     c                   endsr
030900060321      *-----------------------------------------------------------------*
031000060405      * Leggo file FNARB
031100060321      *-----------------------------------------------------------------*
031200060405     c     LeggiARB      begsr
031300060405     c* Escludo le bolle di recupero
031400060405     c* Escludo le bolle con data spedizione >= data elab.
031500060405     c     arbcbo        lookup    rec                                    30
031600060405    1c                   if        not *in30
031700060405     c                   movel     arbaas        arbdsp
031800060405     c                   move      arbmgs        arbdsp
031900060405     c                   if        arbdsp<kdata
032000060405     c* Se c'e' località, la controllo (anche con sinonimi
032100060405     c
032200060405     c                   clear                   welab             1
032300060405    2c                   if        ceploc<>*blanks
032400060405     c* Chiamata al tisi per avere la località corretta
032500060405     c                   clear                   tisi95ds
032600060405     C                   MOVEL     '7'           I95TCN
032700060405     C                   MOVEL     arbcAd        I95CAP
032800060405     C                   MOVEL     arblod        I95LOC
032900060405     C                   MOVEL     arbprd        I95PRV
033000060405     C                   MOVEL     arbnzd        I95NAR
033100060405     C                   MOVEL     arbaas        I95DAT
033200060405     C                   MOVE      arbmgs        I95DAT
033300060405     c                   call      'TISI95R'
033400060405     c                   parm                    tisi95ds
033500060405    3c                   if        o95err=' '  and o95loc=ceploc
033600060405     c                   eval      welab='S'
033700060405    3c                   endif
033800060405   x2c                   else
033900060405     c                   eval      welab='S'
034000060405    2c                   endif
034100060405     c
034200060405     c* Elaboro se bolla abilitata
034300060405    2c                   if        welab='S'
034400060405    3c                   if        (arbdcm=0 and arbama>1) or
034500060405     c                             (arbdcm=0 and arbamp=1) or
034600060405     c                             arbdcm>0
034700060405     c                   exsr      ScriviEVB
034800060405     c
034900060405    4c                   if        arbdcm>0 and arbdce>0
035000060405     c     karb1         chain     fnarb01l
035100060405    5c                   if        %found(fnarb01l)
035200060405     c                   movel     'S'           arbfbs
035300060405     c                   clear                   arbdti
035400060405     c                   clear                   arbhti
035500060405     c                   update    fnarb001
035600060405    5c                   endif
035700060405    5c                   endif
035800060405    4c                   endif
035900060405    3c                   endif
036000060405    2c                   endif
036100060405    1c                   endif
036200060405     c*
036300060321     c                   endsr
036400051229      *-----------------------------------------------------------------*
036500051229      * Fine programmma con codice errore
036600051229      *-----------------------------------------------------------------*
036700051229     c     srfine        begsr
036800051229     c                   eval      *InLr = *On
036900051229     c                   return
037000051229     c                   endsr
037100051227
037200051227      *-----------------------------------------------------------------*
037300051227      * Routine iniziale
037400051227      *-----------------------------------------------------------------*
037500051227     c     *inzsr        begsr
037600051227
037700051227     c     *entry        plist
037800051227     c                   parm                    kpjba
037900060405     c     karb          klist
038000060405     c                   kfld                    cepfil
038100060405     c                   kfld                    kdcm
038200060405     c     Ktbe          Klist
038300060405     c                   Kfld                    kTbeCod
038400060405     c                   Kfld                    kTbeKe1
038500060405     C     KE2TAB        KLIST
038600060405     C                   KFLD                    KTBKUT
038700060405     C                   KFLD                    KTBCOD
038800060405     C     KE3TAB        KLIST
038900060405     C                   KFLD                    KTBKUT
039000060405     C                   KFLD                    KTBCOD
039100060405     C                   KFLD                    KTBkey
039200060405     c     karb1         klist
039300060405     c                   kfld                    arbaas
039400060405     c                   kfld                    arblnp
039500060405     c                   kfld                    arbnrs
039600060405     c                   kfld                    arbnsp
039700060405     c     kevb          klist
039800060405     c                   kfld                    arbaas
039900060405     c                   kfld                    arblnp
040000060405     c                   kfld                    arbnrs
040100060405     c                   kfld                    arbnsp
040200060405     c                   kfld                    cepcev
040300060405     c                   kfld                    kdata
040400060405     c                   kfld                    kora
040500060328     c
040600060328     c
040700060328     c                   Clear                   Tibs02Ds
040800060405     c                   Clear                   dsdf
040900060328     c                   Eval      T02tla = 'L'
041000060328     c                   Eval      T02Mod = 'C'
041100060328     c                   Eval      T02Sif = Knsif
041200060405     c                   Eval      T02Cod = 'SDF'
041300060405     c                   Movel(p)  '046'         T02Ke1
041400060328     c                   Call      'TIBS02R'
041500060328     c                   Parm                    Kpjba
041600060328     c                   Parm                    Tibs02Ds
041700060328     c                   If        T02Err = *Blanks
041800060405     c                   Movel     T02Uni        dsdf
041900060328     c                   EndIf
042000060405     c
042100060405     c* Carico tipi bolla di recupero
042200060405     C                   CLEAR                   xx                4 0
042300060405     C     KE2TAB        SETLL     TABEL00F
042400060405     C     KE2TAB        READE     TABEL00F                               98
042500060405     C     *IN98         DOWEQ     *OFF
042600060405     C                   MOVEL     TBLUNI        DS3a
042700060405     c                   if        §3arbl='R'
042800060405     C                   ADD       1             xx
042900060405     C                   MOVEL     TBLKEY        rec(xx)
043000060405     c                   endif
043100060405     c
043200060405     C     KE2TAB        READE     TABEL00F                               98
043300060405     C                   ENDDO
043400060405     C*
043500051229     c                   endsr
