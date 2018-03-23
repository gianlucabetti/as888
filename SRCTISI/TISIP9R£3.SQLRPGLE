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
002300061109     d kora            s                   like(evbhev) inz(2200)
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
004700070831     d  DataRielab                    8  0
004800070831     d  DataAlfa                      8
004900060830      *
005000060405     d Dataiso         s               d   datfmt(*iso)
005100060405     d DataUltima      s               d   datfmt(*iso)
005200060830     d Datamese        s               d   datfmt(*iso)
005300051227      *-----------------------------------------------------------------*
005400060405     c* Vedo quali date elaborare
005500060405     c                   exsr      DETERMDATA
005600060405     c
005700060405     c                   dow       dataiso<=dataultima
005800060405     c                   movel     dataiso       kdata
005900060405     c     kdata         setll     azcep02l
006000060405     c     kdata         reade     azcep02l
006100060405     c                   dow       not %eof(azcep02l)
006200060405     c
006300060405     c* Elaboro solo eventi con §2zcep=N (noninseriti a mano in chiusura
006400060405     c*  distinta)
006500060405     c                   eval      ktbcod='2Z'
006600060405     c                   movel(p)  cepcev        ktbkey
006700060405     c     ke3tab        chain     tabel
006800060405     c                   if        %found(tabel00f)
006900060405     c                   movel     tbluni        ds2z
007000060405     c                   else
007100060405     c                   clear                   ds2z
007200060405     c                   endif
007300060405     c                   if        §2zCEP='N'
007400060405     c*
007500060405     c* Due cicli di elaborazione:
007600060405     c* PER CAP/località --> SQL
007700051229
007800060405     c                   if        cepfil=0
007900060104     c                   exsr      PREPASQL
008000060405     c
008100060405     c                   else
008200060405     c* Per Linea di arrivo --> leggo lf apposita
008300060405     c                   exsr      LEGPO
008400060405     c                   endif
008500060405     c                   endif
008600060405     c
008700060405     c     kdata         reade     azcep02l
008800060405     c                   enddo
008900060405     c
009000060405     c     dataiso       adddur    1:*d          dataiso
009100060405     c                   enddo
009200060405
009300070831     c* Aggiorno data ultima elaborazione se non passata la data
009400070831     c                   if        DataRielab=0
009500060405     c                   movel     dataultima    kdata
009600060405     c
009700060405     c                   Movel(p)  046           kTbeKe1
009800060405     c     Ktbe          Chain     Tntbe01l
009900060405     c                   If        %Found(Tntbe01l)
010000060405     c                   Movel     TbeUni        dSdf
010100060405     c                   If        §SdfCEP < kdata
010200060405     c                   Z-Add     kdata         §SdfCEP
010300060405     c                   EndIf
010400060405     c                   Movel     dSdf          TbeUni
010500060405     c                   Update    Tntbe000
010600060405     C                   ENDIF
010700070831     C                   ENDIF
010800060405     c
010900051229     c                   Eval      *InLr = *On
011000051229
011100060405      *-----------------------------------------------------------------*
011200060405      * Determino date/data da elaborare
011300060405      *-----------------------------------------------------------------*
011400060405     c     DetermData    begsr
011500060405     c                   time                    w0140            14 0
011600060405     c     *eur          move      w0140         dataultima
011700060405     c                   movel     dataultima    datacor           8 0
011800060830     c                   movel     dataultima    datamese
011900060405     c                   movel     w0140         wora              4 0
012000070831     c
012100070831     c* Se passata elaboro solo quella data
012200070831     c                   if        DataRielab>0
012300070831     c     *iso          movel     DataRielab    dataiso
012400070831     c     *iso          movel     DataRielab    dataultima
012500070831     c     *iso          movel     DataRielab    datamese
012600070831     c
012700070831     c                   else
012800060405     c* Fino alle ore 18:00 elaboro fino al giorno precedente
012900060405     c* Dopo   le ore 18:00 elaboro fino al giorno stesso
013000060405     c                   if        wora<=1800
013100060405     c     dataultima    subdur    1:*d          dataultima
013200060405     c                   endif
013300060405     c
013400060405     c                   if        §sdfcep=0
013500060405     c     dataultima    subdur    5:*d          dataiso
013600060405     c                   movel     dataiso       §sdfcep
013700060405     c                   endif
013800060405     c     *iso          move      §sdfcep       dataiso
013900060405     c     dataiso       adddur    1:*d          dataiso
014000070831     c                   endif
014100060830     c
014200060830     c* Determino oggi - 1 mese per le c.a.
014300060830     c     datamese      subdur    1:*m          datamese
014400060830     c                   movel     datamese      unmesefa          8 0
014500060405     c
014600060405     c                   endsr
014700060405      *-----------------------------------------------------------------*
014800060405      * Lettura bolle arrivi per linea di arrivo e DCM=0 o > data elab
014900060405      *-----------------------------------------------------------------*
015000060405     c     LegPO         begsr
015100060405     c
015200060405     c                   clear                   kdcm
015300060405     c     karb          setll     fnarb54l
015400060405     c     karb          reade(N)  fnarb54l
015500060830     c
015600060405    1c                   dow       not %eof(fnarb54l)
015700060830     c                   clear                   escludica         1
015800060405     c
015900060405     c* Escludo le bolle di recupero
016000060405     c* Escludo le bolle con data spedizione >= data elab.
016100061010     c* Escludo le bolle con lna 101
016200060405     c     arbcbo        lookup    rec                                    30
016300060405    2c                   if        not *in30
016400060405     c                   movel     arbaas        arbdsp            8 0
016500060405     c                   move      arbmgs        arbdsp
016600061010     c
016700061010    3c                   if        arbdsp<kdata  and arblna<>101
016800060830     c
016900060830     c* Escludo le spedizioni con C.A. da almeno 1 mese
017000060830     c                   clear                   fidn12ds
017100060830     c                   eval      i12aas = arbaas
017200060830     c                   eval      i12lnp = arblnp
017300060830     c                   eval      i12nrs = arbnrs
017400060830     c                   eval      i12nsp = arbnsp
017500060830     c                   eval      i12fel = 'E'
017600060830     c                   eval      i12fan = 'E'
017700060830      *
017800060830     c                   call      'FIDN12R'
017900060830     c                   parm                    fidn12ds
018000060830      *
018100060830      * se non ci sono errori
018200060830     c                   if        o12err = ' '
018300060830      * se numero di CA trovate maggiore di zero
018400060830     c                   if        o12nca > 0
018500060830     c                   eval      EscludiCA='S'
018600060830     c
018700060830     c                   z-add     *zeros        ii                3 0
018800060830     c     1             do        o12nca        ii
018900060830     c                   movea     skkey(ii)     dskey
019000060830     C                   Z-ADD     skdca(ii)     dctdca            8 0
019100060830     C                   Z-ADD     skdch(ii)     dctdch            8 0
019200060830     c* Se chiusa --> elaboro
019300060830     c                   if        dctdch>0
019400060830     c                   clear                   escludiCA
019500060830     c                   leave
019600060830     c                   endif
019700060830     c*
019800060830     c                   if        dctdca>unmesefa
019900060830     c                   clear                   escludiCA
020000060830     c                   leave
020100060830     c                   endif
020200060830     C                   ENDDO
020300060830      *
020400060830     c                   endif
020500060830     c                   endif
020600060405     c
020700060405     c* Elaboro le spedizioni abilitate in base a spunta part o arr
020800060830    4c                   if        EscludiCA=' ' and
020900060831    4c                             (arbama>0 or arbamp=1)
021000060405     c                   exsr      ScriviEVB
021100060405    4c                   endif
021200060405    3c                   endif
021300060405    2c                   endif
021400060405     c
021500060405     c     karb          reade(N)  fnarb54l
021600060405    1c                   enddo
021700060405     c
021800060405     c                   z-add     kdata         kdcm
021900060405     c* Leeggo bolle con data consegna > kdata
022000060405     c     karb          setgt     fnarb54l
022100060405     c     cepfil        reade     fnarb54l
022200060405    1c                   dow       not %eof(fnarb54l)
022300060405     c
022400060405     c* Escludo le bolle di recupero
022500060405     c     arbcbo        lookup    rec                                    30
022600060405    2c                   if        not *in30
022700060405     c                   movel     arbaas        arbdsp
022800060405     c                   move      arbmgs        arbdsp
022900061010    3c                   if        arbdsp<kdata and arblna<>101
023000060405     c
023100060405     c                   exsr      ScriviEVB
023200060405     c
023300060405     c* In questo caso, se calcolata, ricalcolo affidabilità
023400060405    4c                   if        arbdce>0
023500060405     c                   movel     'S'           arbfbs
023600060405     c                   clear                   arbdti
023700060405     c                   clear                   arbhti
023800060405     c                   update    fnarb000
023900060405     c                   else
024000060405     c                   unlock    fnarb54l
024100060405    4c                   endif
024200060405    3c                   endif
024300060405     c
024400060405    2c                   endif
024500060405     c
024600060405     c     cepfil        reade     fnarb54l
024700060405    1c                   enddo
024800060405     c                   endsr
024900060405      *-----------------------------------------------------------------*
025000060405      * SCrivi record in fnevb00f
025100060405      *-----------------------------------------------------------------*
025200060405     c     ScriviEVB     begsr
025300060405     c     kevb          chain     fnevb04l
025400060405     c                   if        not %found(fnevb04l)
025500060405     c                   clear                   fnevb000
025600060405     c                   move      datacor       evbdtv
025700060405     c                   time                    evborv
025800061011     c                   movel     arblna        evbfle
025900060405     c                   z-add     arbaas        evbaas
026000060405     c                   z-add     arblnp        evblnp
026100060405     c                   z-add     arbnrs        evbnrs
026200060405     c                   z-add     arbnsp        evbnsp
026300060405     c                   z-add     kdata         evbdev
026400061109     c                   z-add     2200          evbhev
026500060405     c                   movel     cepcev        evbcev
026600060405     c                   write     fnevb000
026700060405     c                   endif
026800060405     c
026900060405     c                   endsr
027000051229      *-----------------------------------------------------------------*
027100060405     c* Lettura FNARB con selezione per CAP
027200051229      *-----------------------------------------------------------------*
027300060104     c     prepasql      begsr
027400051229
027500051229      *?Imposto la parte fissa dell'istruzione sql
027600051229     c                   eval      wrkgetlista =
027700060405     c                             'select *  from FNARB00F where -
027800060405     c                             ARBCAD ='''+ cepCAP +''' and  ARBNZD=''   '
027900060405     c                             + CEPNAR + ''' and (ARBDCM=0 or ARBDCM>'
028000060405     c                             +  %editc(kdata:'X') + ')'
028100060322      *?Ordinamento
028200060322     c                   eval      wrkgetlista = wrkgetlista +
028300060405     c                             ' order by arblna, arbdcm'
028400051229      *?Solo lettura
028500051229     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
028600051229
028700051229     C/EXEC SQL
028800051229     C+ PREPARE s1 FROM :wrkgetlista
028900051229     C/END-EXEC
029000051229     C
029100051229     C/EXEC SQL
029200051229     C+ DECLARE A1 CURSOR FOR S1
029300051229     C/END-EXEC
029400051229     C
029500051229     C/EXEC SQL
029600051229     C+ OPEN a1
029700051229     C/END-EXEC
029800051229
029900051229      *?Leggo il file
030000051229     c                   do        *hival
030100051229     C/EXEC SQL
030200060405     C+ FETCH NEXT FROM a1 INTO : DSFNARB
030300051229     C/END-EXEC
030400051229
030500051229     c                   select
030600051229
030700051229     c                   when      sqlcod = 100
030800051229     c                   leave
030900051229     c                   when      sqlcod < 0
031000051229     c                   seton                                        H1
031100051229     c                   exsr      srfine
031200051229     c                   other
031300060321     c
031400060321     c* Sommo e a cambio p.o. o data scrivo
031500060405     c                   EXSR      LeggiARB
031600060104     c
031700051229     c                   endsl
031800051229
031900051229     c                   enddo
032000051229     C/EXEC SQL
032100051229     C+ CLOSE a1
032200051229     C/END-EXEC
032300051229     c                   endsr
032400060321      *-----------------------------------------------------------------*
032500060405      * Leggo file FNARB
032600060321      *-----------------------------------------------------------------*
032700060405     c     LeggiARB      begsr
032800061016     c                   clear                   escludica         1
032900060405     c* Escludo le bolle di recupero
033000060405     c* Escludo le bolle con data spedizione >= data elab.
033100060405     c     arbcbo        lookup    rec                                    30
033200060405    1c                   if        not *in30
033300060405     c                   movel     arbaas        arbdsp
033400060405     c                   move      arbmgs        arbdsp
033500061010     c                   if        arbdsp<kdata and arblna<>101
033600060405     c* Se c'e' località, la controllo (anche con sinonimi
033700060405     c
033800060405     c                   clear                   welab             1
033900060405    2c                   if        ceploc<>*blanks
034000060405     c* Chiamata al tisi per avere la località corretta
034100060405     c                   clear                   tisi95ds
034200060405     C                   MOVEL     '7'           I95TCN
034300060405     C                   MOVEL     arbcAd        I95CAP
034400060405     C                   MOVEL     arblod        I95LOC
034500060405     C                   MOVEL     arbprd        I95PRV
034600060405     C                   MOVEL     arbnzd        I95NAR
034700060405     C                   MOVEL     arbaas        I95DAT
034800060405     C                   MOVE      arbmgs        I95DAT
034900060405     c                   call      'TISI95R'
035000060405     c                   parm                    tisi95ds
035100060405    3c                   if        o95err=' '  and o95loc=ceploc
035200060405     c                   eval      welab='S'
035300060405    3c                   endif
035400060405   x2c                   else
035500060405     c                   eval      welab='S'
035600060405    2c                   endif
035700060405     c
035800060405     c* Elaboro se bolla abilitata
035900060405    2c                   if        welab='S'
036000061005     c*
036100061005     c* Escludo le spedizioni con C.A. da almeno 1 mese
036200061005     c                   clear                   fidn12ds
036300061005     c                   eval      i12aas = arbaas
036400061005     c                   eval      i12lnp = arblnp
036500061005     c                   eval      i12nrs = arbnrs
036600061005     c                   eval      i12nsp = arbnsp
036700061005     c                   eval      i12fel = 'E'
036800061005     c                   eval      i12fan = 'E'
036900061005      *
037000061005     c                   call      'FIDN12R'
037100061005     c                   parm                    fidn12ds
037200061005      *
037300061005      * se non ci sono errori
037400061005    3c                   if        o12err = ' '
037500061005      * se numero di CA trovate maggiore di zero
037600061005    4c                   if        o12nca > 0
037700061005     c                   eval      EscludiCA='S'
037800061005     c
037900061005     c                   z-add     *zeros        ii                3 0
038000061005    5c     1             do        o12nca        ii
038100061005     c                   movea     skkey(ii)     dskey
038200061005     C                   Z-ADD     skdca(ii)     dctdca            8 0
038300061005     C                   Z-ADD     skdch(ii)     dctdch            8 0
038400061005     c* Se chiusa --> elaboro
038500061005     c                   if        dctdch>0
038600061005     c                   clear                   escludiCA
038700061005     c                   leave
038800061005     c                   endif
038900061005     c*
039000061005     c                   if        dctdca>unmesefa
039100061005     c                   clear                   escludiCA
039200061005     c                   leave
039300061005     c                   endif
039400061005    5C                   ENDDO
039500061005      *
039600061005    4c                   endif
039700061005    3c                   endif
039800061005     c
039900061005   2ac                   if        EscludiCA=' '
040000061005     c
040100060405    3c                   if        (arbdcm=0 and arbama>1) or
040200060405     c                             (arbdcm=0 and arbamp=1) or
040300060405     c                             arbdcm>0
040400060405     c                   exsr      ScriviEVB
040500060405     c
040600060405    4c                   if        arbdcm>0 and arbdce>0
040700060405     c     karb1         chain     fnarb01l
040800060405    5c                   if        %found(fnarb01l)
040900060405     c                   movel     'S'           arbfbs
041000060405     c                   clear                   arbdti
041100060405     c                   clear                   arbhti
041200060405     c                   update    fnarb001
041300060405    5c                   endif
041400060405    5c                   endif
041500060405    4c                   endif
041600060405    3c                   endif
041700061005   2ac                   endif
041800061005    2c                   endif
041900060405    1c                   endif
042000060405     c*
042100060321     c                   endsr
042200051229      *-----------------------------------------------------------------*
042300051229      * Fine programmma con codice errore
042400051229      *-----------------------------------------------------------------*
042500051229     c     srfine        begsr
042600051229     c                   eval      *InLr = *On
042700051229     c                   return
042800051229     c                   endsr
042900051227
043000051227      *-----------------------------------------------------------------*
043100051227      * Routine iniziale
043200051227      *-----------------------------------------------------------------*
043300051227     c     *inzsr        begsr
043400051227
043500051227     c     *entry        plist
043600051227     c                   parm                    kpjba
043700070831     c                   movel     kpjbu         Dataalfa
043800070831     c
043900070831     c* Passata data di rielaborazione
044000070831     c                   if        dataalfa>*zeros
044100070831     c                   movel     Dataalfa      DataRielab
044200070831     c                   else
044300070831     c                   clear                   DataRielab
044400070831     c                   endif
044500070831     c
044600060405     c     karb          klist
044700060405     c                   kfld                    cepfil
044800060405     c                   kfld                    kdcm
044900060405     c     Ktbe          Klist
045000060405     c                   Kfld                    kTbeCod
045100060405     c                   Kfld                    kTbeKe1
045200060405     C     KE2TAB        KLIST
045300060405     C                   KFLD                    KTBKUT
045400060405     C                   KFLD                    KTBCOD
045500060405     C     KE3TAB        KLIST
045600060405     C                   KFLD                    KTBKUT
045700060405     C                   KFLD                    KTBCOD
045800060405     C                   KFLD                    KTBkey
045900060405     c     karb1         klist
046000060405     c                   kfld                    arbaas
046100060405     c                   kfld                    arblnp
046200060405     c                   kfld                    arbnrs
046300060405     c                   kfld                    arbnsp
046400060405     c     kevb          klist
046500060405     c                   kfld                    arbaas
046600060405     c                   kfld                    arblnp
046700060405     c                   kfld                    arbnrs
046800060405     c                   kfld                    arbnsp
046900060405     c                   kfld                    cepcev
047000060405     c                   kfld                    kdata
047100060405     c                   kfld                    kora
047200060328     c
047300060328     c
047400060328     c                   Clear                   Tibs02Ds
047500060405     c                   Clear                   dsdf
047600060328     c                   Eval      T02tla = 'L'
047700060328     c                   Eval      T02Mod = 'C'
047800060328     c                   Eval      T02Sif = Knsif
047900060405     c                   Eval      T02Cod = 'SDF'
048000060405     c                   Movel(p)  '046'         T02Ke1
048100060328     c                   Call      'TIBS02R'
048200060328     c                   Parm                    Kpjba
048300060328     c                   Parm                    Tibs02Ds
048400060328     c                   If        T02Err = *Blanks
048500060405     c                   Movel     T02Uni        dsdf
048600060328     c                   EndIf
048700060405     c
048800060405     c* Carico tipi bolla di recupero
048900060405     C                   CLEAR                   xx                4 0
049000060405     C     KE2TAB        SETLL     TABEL00F
049100060405     C     KE2TAB        READE     TABEL00F                               98
049200060405     C     *IN98         DOWEQ     *OFF
049300060405     C                   MOVEL     TBLUNI        DS3a
049400060405     c                   if        §3arbl='R'
049500060405     C                   ADD       1             xx
049600060405     C                   MOVEL     TBLKEY        rec(xx)
049700060405     c                   endif
049800060405     c
049900060405     C     KE2TAB        READE     TABEL00F                               98
050000060405     C                   ENDDO
050100060405     C*
050200051229     c                   endsr
