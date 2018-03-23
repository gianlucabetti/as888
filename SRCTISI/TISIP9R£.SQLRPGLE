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
002300140114     d kora            s                   like(evbhev) inz(0700)
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
004900080206
005000080206     d  caleloc        s                   like(ceploc)
005100060830      *
005200060405     d Dataiso         s               d   datfmt(*iso)
005300060405     d DataUltima      s               d   datfmt(*iso)
005400060830     d Datamese        s               d   datfmt(*iso)
005500051227      *-----------------------------------------------------------------*
005600060405     c* Vedo quali date elaborare
005700060405     c                   exsr      DETERMDATA
005800060405     c
005900060405     c                   dow       dataiso<=dataultima
006000060405     c                   movel     dataiso       kdata
006100060405     c     kdata         setll     azcep02l
006200060405     c     kdata         reade     azcep02l
006300060405     c                   dow       not %eof(azcep02l)
006400060405     c
006500060405     c* Elaboro solo eventi con §2zcep=N (noninseriti a mano in chiusura
006600060405     c*  distinta)
006700060405     c                   eval      ktbcod='2Z'
006800060405     c                   movel(p)  cepcev        ktbkey
006900060405     c     ke3tab        chain     tabel
007000060405     c                   if        %found(tabel00f)
007100060405     c                   movel     tbluni        ds2z
007200060405     c                   else
007300060405     c                   clear                   ds2z
007400060405     c                   endif
007500060405     c                   if        §2zCEP='N'
007600060405     c*
007700060405     c* Due cicli di elaborazione:
007800060405     c* PER CAP/località --> SQL
007900051229
008000060405     c                   if        cepfil=0
008100060104     c                   exsr      PREPASQL
008200060405     c
008300060405     c                   else
008400060405     c* Per Linea di arrivo --> leggo lf apposita
008500060405     c                   exsr      LEGPO
008600060405     c                   endif
008700060405     c                   endif
008800060405     c
008900060405     c     kdata         reade     azcep02l
009000060405     c                   enddo
009100060405     c
009200060405     c     dataiso       adddur    1:*d          dataiso
009300060405     c                   enddo
009400060405
009500070831     c* Aggiorno data ultima elaborazione se non passata la data
009600070831     c                   if        DataRielab=0
009700060405     c                   movel     dataultima    kdata
009800060405     c
009900060405     c                   Movel(p)  046           kTbeKe1
010000060405     c     Ktbe          Chain     Tntbe01l
010100060405     c                   If        %Found(Tntbe01l)
010200060405     c                   Movel     TbeUni        dSdf
010300060405     c                   If        §SdfCEP < kdata
010400060405     c                   Z-Add     kdata         §SdfCEP
010500060405     c                   EndIf
010600060405     c                   Movel     dSdf          TbeUni
010700060405     c                   Update    Tntbe000
010800060405     C                   ENDIF
010900070831     C                   ENDIF
011000060405     c
011100051229     c                   Eval      *InLr = *On
011200051229
011300060405      *-----------------------------------------------------------------*
011400060405      * Determino date/data da elaborare
011500060405      *-----------------------------------------------------------------*
011600060405     c     DetermData    begsr
011700060405     c                   time                    w0140            14 0
011800060405     c     *eur          move      w0140         dataultima
011900060405     c                   movel     dataultima    datacor           8 0
012000060830     c                   movel     dataultima    datamese
012100060405     c                   movel     w0140         wora              4 0
012200070831     c
012300070831     c* Se passata elaboro solo quella data
012400070831     c                   if        DataRielab>0
012500070831     c     *iso          movel     DataRielab    dataiso
012600070831     c     *iso          movel     DataRielab    dataultima
012700070831     c     *iso          movel     DataRielab    datamese
012800070831     c
012900070831     c                   else
013000060405     c* Fino alle ore 18:00 elaboro fino al giorno precedente
013100060405     c* Dopo   le ore 18:00 elaboro fino al giorno stesso
013200060405     c                   if        wora<=1800
013300060405     c     dataultima    subdur    1:*d          dataultima
013400060405     c                   endif
013500060405     c
013600060405     c                   if        §sdfcep=0
013700060405     c     dataultima    subdur    5:*d          dataiso
013800060405     c                   movel     dataiso       §sdfcep
013900060405     c                   endif
014000060405     c     *iso          move      §sdfcep       dataiso
014100060405     c     dataiso       adddur    1:*d          dataiso
014200070831     c                   endif
014300060830     c
014400060830     c* Determino oggi - 1 mese per le c.a.
014500060830     c     datamese      subdur    1:*m          datamese
014600060830     c                   movel     datamese      unmesefa          8 0
014700060405     c
014800060405     c                   endsr
014900060405      *-----------------------------------------------------------------*
015000060405      * Lettura bolle arrivi per linea di arrivo e DCM=0 o > data elab
015100060405      *-----------------------------------------------------------------*
015200060405     c     LegPO         begsr
015300060405     c
015400060405     c                   clear                   kdcm
015500060405     c     karb          setll     fnarb54l
015600060405     c     karb          reade(N)  fnarb54l
015700060830     c
015800060405    1c                   dow       not %eof(fnarb54l)
015900060830     c                   clear                   escludica         1
016000060405     c
016100060405     c* Escludo le bolle di recupero
016200060405     c* Escludo le bolle con data spedizione >= data elab.
016300061010     c* Escludo le bolle con lna 101
016400060405     c     arbcbo        lookup    rec                                    30
016500060405    2c                   if        not *in30
016600060405     c                   movel     arbaas        arbdsp            8 0
016700060405     c                   move      arbmgs        arbdsp
016800061010     c
016900061010    3c                   if        arbdsp<kdata  and arblna<>101
017000060830     c
017100060830     c* Escludo le spedizioni con C.A. da almeno 1 mese
017200060830     c                   clear                   fidn12ds
017300060830     c                   eval      i12aas = arbaas
017400060830     c                   eval      i12lnp = arblnp
017500060830     c                   eval      i12nrs = arbnrs
017600060830     c                   eval      i12nsp = arbnsp
017700060830     c                   eval      i12fel = 'E'
017800060830     c                   eval      i12fan = 'E'
017900060830      *
018000060830     c                   call      'FIDN12R'
018100060830     c                   parm                    fidn12ds
018200060830      *
018300060830      * se non ci sono errori
018400060830     c                   if        o12err = ' '
018500060830      * se numero di CA trovate maggiore di zero
018600060830     c                   if        o12nca > 0
018700060830     c                   eval      EscludiCA='S'
018800060830     c
018900060830     c                   z-add     *zeros        ii                3 0
019000060830     c     1             do        o12nca        ii
019100060830     c                   movea     skkey(ii)     dskey
019200060830     C                   Z-ADD     skdca(ii)     dctdca            8 0
019300060830     C                   Z-ADD     skdch(ii)     dctdch            8 0
019400060830     c* Se chiusa --> elaboro
019500060830     c                   if        dctdch>0
019600060830     c                   clear                   escludiCA
019700060830     c                   leave
019800060830     c                   endif
019900060830     c*
020000060830     c                   if        dctdca>unmesefa
020100060830     c                   clear                   escludiCA
020200060830     c                   leave
020300060830     c                   endif
020400060830     C                   ENDDO
020500060830      *
020600060830     c                   endif
020700060830     c                   endif
020800060405     c
020900060405     c* Elaboro le spedizioni abilitate in base a spunta part o arr
021000060830    4c                   if        EscludiCA=' ' and
021100060831    4c                             (arbama>0 or arbamp=1)
021200060405     c                   exsr      ScriviEVB
021300060405    4c                   endif
021400060405    3c                   endif
021500060405    2c                   endif
021600060405     c
021700060405     c     karb          reade(N)  fnarb54l
021800060405    1c                   enddo
021900060405     c
022000060405     c                   z-add     kdata         kdcm
022100060405     c* Leeggo bolle con data consegna > kdata
022200060405     c     karb          setgt     fnarb54l
022300060405     c     cepfil        reade     fnarb54l
022400060405    1c                   dow       not %eof(fnarb54l)
022500060405     c
022600060405     c* Escludo le bolle di recupero
022700060405     c     arbcbo        lookup    rec                                    30
022800060405    2c                   if        not *in30
022900060405     c                   movel     arbaas        arbdsp
023000060405     c                   move      arbmgs        arbdsp
023100061010    3c                   if        arbdsp<kdata and arblna<>101
023200060405     c
023300060405     c                   exsr      ScriviEVB
023400060405     c
023500060405     c* In questo caso, se calcolata, ricalcolo affidabilità
023600060405    4c                   if        arbdce>0
023700060405     c                   movel     'S'           arbfbs
023800060405     c                   clear                   arbdti
023900060405     c                   clear                   arbhti
024000060405     c                   update    fnarb000
024100060405     c                   else
024200060405     c                   unlock    fnarb54l
024300060405    4c                   endif
024400060405    3c                   endif
024500060405     c
024600060405    2c                   endif
024700060405     c
024800060405     c     cepfil        reade     fnarb54l
024900060405    1c                   enddo
025000060405     c                   endsr
025100060405      *-----------------------------------------------------------------*
025200060405      * SCrivi record in fnevb00f
025300060405      *-----------------------------------------------------------------*
025400060405     c     ScriviEVB     begsr
025500060405     c     kevb          chain     fnevb04l
025600060405     c                   if        not %found(fnevb04l)
025700060405     c                   clear                   fnevb000
025800060405     c                   move      datacor       evbdtv
025900060405     c                   time                    evborv
026000061011     c                   movel     arblna        evbfle
026100060405     c                   z-add     arbaas        evbaas
026200060405     c                   z-add     arblnp        evblnp
026300060405     c                   z-add     arbnrs        evbnrs
026400060405     c                   z-add     arbnsp        evbnsp
026500060405     c                   z-add     kdata         evbdev
026600140114     c                   z-add     kora          evbhev
026700060405     c                   movel     cepcev        evbcev
026800060405     c                   write     fnevb000
026900060405     c                   endif
027000060405     c
027100060405     c                   endsr
027200051229      *-----------------------------------------------------------------*
027300060405     c* Lettura FNARB con selezione per CAP
027400051229      *-----------------------------------------------------------------*
027500060104     c     prepasql      begsr
027600051229
027700051229      *?Imposto la parte fissa dell'istruzione sql
027800051229     c                   eval      wrkgetlista =
027900060405     c                             'select *  from FNARB00F where -
028000060405     c                             ARBCAD ='''+ cepCAP +''' and  ARBNZD=''   '
028100060405     c                             + CEPNAR + ''' and (ARBDCM=0 or ARBDCM>'
028200060405     c                             +  %editc(kdata:'X') + ')'
028300060322      *?Ordinamento
028400060322     c                   eval      wrkgetlista = wrkgetlista +
028500060405     c                             ' order by arblna, arbdcm'
028600051229      *?Solo lettura
028700051229     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
028800051229
028900051229     C/EXEC SQL
029000051229     C+ PREPARE s1 FROM :wrkgetlista
029100051229     C/END-EXEC
029200051229     C
029300051229     C/EXEC SQL
029400051229     C+ DECLARE A1 CURSOR FOR S1
029500051229     C/END-EXEC
029600051229     C
029700051229     C/EXEC SQL
029800051229     C+ OPEN a1
029900051229     C/END-EXEC
030000051229
030100051229      *?Leggo il file
030200051229     c                   do        *hival
030300051229     C/EXEC SQL
030400060405     C+ FETCH NEXT FROM a1 INTO : DSFNARB
030500051229     C/END-EXEC
030600051229
030700051229     c                   select
030800051229
030900051229     c                   when      sqlcod = 100
031000051229     c                   leave
031100051229     c                   when      sqlcod < 0
031200051229     c                   seton                                        H1
031300051229     c                   exsr      srfine
031400051229     c                   other
031500060321     c
031600060321     c* Sommo e a cambio p.o. o data scrivo
031700060405     c                   EXSR      LeggiARB
031800060104     c
031900051229     c                   endsl
032000051229
032100051229     c                   enddo
032200051229     C/EXEC SQL
032300051229     C+ CLOSE a1
032400051229     C/END-EXEC
032500051229     c                   endsr
032600060321      *-----------------------------------------------------------------*
032700060405      * Leggo file FNARB
032800060321      *-----------------------------------------------------------------*
032900060405     c     LeggiARB      begsr
033000061016     c                   clear                   escludica         1
033100060405     c* Escludo le bolle di recupero
033200060405     c* Escludo le bolle con data spedizione >= data elab.
033300060405     c     arbcbo        lookup    rec                                    30
033400060405    1c                   if        not *in30
033500060405     c                   movel     arbaas        arbdsp
033600060405     c                   move      arbmgs        arbdsp
033700061010     c                   if        arbdsp<kdata and arblna<>101
033800060405     c* Se c'e' località, la controllo (anche con sinonimi
033900060405     c
034000060405     c                   clear                   welab             1
034100060405    2c                   if        ceploc<>*blanks
034200080206     c
034300080206     c* Chiamata al tisi per avere la località corretta
034400080206     c                   clear                   tisi95ds
034500080206     C                   MOVEL     '7'           I95TCN
034600080206     C                   MOVEL     cepcap        I95CAP
034700080206     C                   MOVEL     ceploc        I95LOC
034800080206     C                   MOVEL     cepprv        I95PRV
034900080206     C                   MOVEL     cepnar        I95NAR
035000080206     C                   MOVEL     arbaas        I95DAT
035100080206     C                   MOVE      arbmgs        I95DAT
035200080206     c                   call      'TISI95R'
035300080206     c                   parm                    tisi95ds
035400080206     c
035500080206    3c                   if        o95err=' '
035600080206     c                   eval      caleloc=o95loc
035700080206     c                   else
035800080206     c                   eval      caleloc=ceploc
035900080206     c                   endif
036000080206     c
036100060405     c* Chiamata al tisi per avere la località corretta
036200060405     c                   clear                   tisi95ds
036300060405     C                   MOVEL     '7'           I95TCN
036400060405     C                   MOVEL     arbcAd        I95CAP
036500060405     C                   MOVEL     arblod        I95LOC
036600060405     C                   MOVEL     arbprd        I95PRV
036700060405     C                   MOVEL     arbnzd        I95NAR
036800060405     C                   MOVEL     arbaas        I95DAT
036900060405     C                   MOVE      arbmgs        I95DAT
037000060405     c                   call      'TISI95R'
037100060405     c                   parm                    tisi95ds
037200080206    3c                   if        (o95err=' '  and o95loc=caleloc) or
037300080206     c                             (o95err<>' ' and arblod=caleloc)
037400060405     c                   eval      welab='S'
037500060405    3c                   endif
037600060405   x2c                   else
037700060405     c                   eval      welab='S'
037800060405    2c                   endif
037900060405     c
038000060405     c* Elaboro se bolla abilitata
038100060405    2c                   if        welab='S'
038200061005     c*
038300061005     c* Escludo le spedizioni con C.A. da almeno 1 mese
038400061005     c                   clear                   fidn12ds
038500061005     c                   eval      i12aas = arbaas
038600061005     c                   eval      i12lnp = arblnp
038700061005     c                   eval      i12nrs = arbnrs
038800061005     c                   eval      i12nsp = arbnsp
038900061005     c                   eval      i12fel = 'E'
039000061005     c                   eval      i12fan = 'E'
039100061005      *
039200061005     c                   call      'FIDN12R'
039300061005     c                   parm                    fidn12ds
039400061005      *
039500061005      * se non ci sono errori
039600061005    3c                   if        o12err = ' '
039700061005      * se numero di CA trovate maggiore di zero
039800061005    4c                   if        o12nca > 0
039900061005     c                   eval      EscludiCA='S'
040000061005     c
040100061005     c                   z-add     *zeros        ii                3 0
040200061005    5c     1             do        o12nca        ii
040300061005     c                   movea     skkey(ii)     dskey
040400061005     C                   Z-ADD     skdca(ii)     dctdca            8 0
040500061005     C                   Z-ADD     skdch(ii)     dctdch            8 0
040600061005     c* Se chiusa --> elaboro
040700061005     c                   if        dctdch>0
040800061005     c                   clear                   escludiCA
040900061005     c                   leave
041000061005     c                   endif
041100061005     c*
041200061005     c                   if        dctdca>unmesefa
041300061005     c                   clear                   escludiCA
041400061005     c                   leave
041500061005     c                   endif
041600061005    5C                   ENDDO
041700061005      *
041800061005    4c                   endif
041900061005    3c                   endif
042000061005     c
042100061005   2ac                   if        EscludiCA=' '
042200061005     c
042300060405    3c                   if        (arbdcm=0 and arbama>1) or
042400060405     c                             (arbdcm=0 and arbamp=1) or
042500060405     c                             arbdcm>0
042600060405     c                   exsr      ScriviEVB
042700060405     c
042800060405    4c                   if        arbdcm>0 and arbdce>0
042900060405     c     karb1         chain     fnarb01l
043000060405    5c                   if        %found(fnarb01l)
043100060405     c                   movel     'S'           arbfbs
043200060405     c                   clear                   arbdti
043300060405     c                   clear                   arbhti
043400060405     c                   update    fnarb001
043500060405    5c                   endif
043600060405    5c                   endif
043700060405    4c                   endif
043800060405    3c                   endif
043900061005   2ac                   endif
044000061005    2c                   endif
044100060405    1c                   endif
044200060405     c*
044300060321     c                   endsr
044400051229      *-----------------------------------------------------------------*
044500051229      * Fine programmma con codice errore
044600051229      *-----------------------------------------------------------------*
044700051229     c     srfine        begsr
044800051229     c                   eval      *InLr = *On
044900051229     c                   return
045000051229     c                   endsr
045100051227
045200051227      *-----------------------------------------------------------------*
045300051227      * Routine iniziale
045400051227      *-----------------------------------------------------------------*
045500051227     c     *inzsr        begsr
045600051227
045700051227     c     *entry        plist
045800051227     c                   parm                    kpjba
045900070831     c                   movel     kpjbu         Dataalfa
046000070831     c
046100070831     c* Passata data di rielaborazione
046200070831     c                   if        dataalfa>*zeros
046300070831     c                   movel     Dataalfa      DataRielab
046400070831     c                   else
046500070831     c                   clear                   DataRielab
046600070831     c                   endif
046700070831     c
046800060405     c     karb          klist
046900060405     c                   kfld                    cepfil
047000060405     c                   kfld                    kdcm
047100060405     c     Ktbe          Klist
047200060405     c                   Kfld                    kTbeCod
047300060405     c                   Kfld                    kTbeKe1
047400060405     C     KE2TAB        KLIST
047500060405     C                   KFLD                    KTBKUT
047600060405     C                   KFLD                    KTBCOD
047700060405     C     KE3TAB        KLIST
047800060405     C                   KFLD                    KTBKUT
047900060405     C                   KFLD                    KTBCOD
048000060405     C                   KFLD                    KTBkey
048100060405     c     karb1         klist
048200060405     c                   kfld                    arbaas
048300060405     c                   kfld                    arblnp
048400060405     c                   kfld                    arbnrs
048500060405     c                   kfld                    arbnsp
048600060405     c     kevb          klist
048700060405     c                   kfld                    arbaas
048800060405     c                   kfld                    arblnp
048900060405     c                   kfld                    arbnrs
049000060405     c                   kfld                    arbnsp
049100060405     c                   kfld                    cepcev
049200060405     c                   kfld                    kdata
049300060328     c
049400060328     c
049500060328     c                   Clear                   Tibs02Ds
049600060405     c                   Clear                   dsdf
049700060328     c                   Eval      T02tla = 'L'
049800060328     c                   Eval      T02Mod = 'C'
049900060328     c                   Eval      T02Sif = Knsif
050000060405     c                   Eval      T02Cod = 'SDF'
050100060405     c                   Movel(p)  '046'         T02Ke1
050200060328     c                   Call      'TIBS02R'
050300060328     c                   Parm                    Kpjba
050400060328     c                   Parm                    Tibs02Ds
050500060328     c                   If        T02Err = *Blanks
050600060405     c                   Movel     T02Uni        dsdf
050700060328     c                   EndIf
050800060405     c
050900060405     c* Carico tipi bolla di recupero
051000060405     C                   CLEAR                   xx                4 0
051100060405     C     KE2TAB        SETLL     TABEL00F
051200060405     C     KE2TAB        READE     TABEL00F                               98
051300060405     C     *IN98         DOWEQ     *OFF
051400060405     C                   MOVEL     TBLUNI        DS3a
051500060405     c                   if        §3arbl='R'
051600060405     C                   ADD       1             xx
051700060405     C                   MOVEL     TBLKEY        rec(xx)
051800060405     c                   endif
051900060405     c
052000060405     C     KE2TAB        READE     TABEL00F                               98
052100060405     C                   ENDDO
052200060405     C*
052300051229     c                   endsr
