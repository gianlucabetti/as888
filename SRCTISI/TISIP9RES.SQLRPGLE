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
006300060405     c* Elaboro solo eventi con �2zcep=N (noninseriti a mano in chiusura
006400060405     c*  distinta)
006500060405     c                   eval      ktbcod='2Z'
006600060405     c                   movel(p)  cepcev        ktbkey
006700060405     c     ke3tab        chain     tabel
006800060405     c                   if        %found(tabel00f)
006900060405     c                   movel     tbluni        ds2z
007000060405     c                   else
007100060405     c                   clear                   ds2z
007200060405     c                   endif
007300060405     c                   if        �2zCEP='N'
007400060405     c*
007500060405     c* Due cicli di elaborazione:
007600060405     c* PER CAP/localit� --> SQL
007700051229
007800071217     c                   if        cepfil=0
007900071217     c   30
008000071217     cann30              exsr      PREPASQL
008100060405     c
008200060405     c                   else
008300060405     c* Per Linea di arrivo --> leggo lf apposita
008400071217     c                   if        cepfil=105
008500060405     c                   exsr      LEGPO
008600071217     c                   endif
008700060405     c                   endif
008800060405     c                   endif
008900060405     c
009000060405     c     kdata         reade     azcep02l
009100060405     c                   enddo
009200060405     c
009300060405     c     dataiso       adddur    1:*d          dataiso
009400060405     c                   enddo
009500060405
009600070831     c* Aggiorno data ultima elaborazione se non passata la data
009700070831     c                   if        DataRielab=0
009800060405     c                   movel     dataultima    kdata
009900060405     c
010000060405     c                   Movel(p)  046           kTbeKe1
010100060405     c     Ktbe          Chain     Tntbe01l
010200060405     c                   If        %Found(Tntbe01l)
010300060405     c                   Movel     TbeUni        dSdf
010400060405     c                   If        �SdfCEP < kdata
010500060405     c                   Z-Add     kdata         �SdfCEP
010600060405     c                   EndIf
010700060405     c                   Movel     dSdf          TbeUni
010800060405     c                   Update    Tntbe000
010900060405     C                   ENDIF
011000070831     C                   ENDIF
011100060405     c
011200051229     c                   Eval      *InLr = *On
011300051229
011400060405      *-----------------------------------------------------------------*
011500060405      * Determino date/data da elaborare
011600060405      *-----------------------------------------------------------------*
011700060405     c     DetermData    begsr
011800060405     c                   time                    w0140            14 0
011900060405     c     *eur          move      w0140         dataultima
012000060405     c                   movel     dataultima    datacor           8 0
012100060830     c                   movel     dataultima    datamese
012200060405     c                   movel     w0140         wora              4 0
012300070831     c
012400070831     c* Se passata elaboro solo quella data
012500070831     c                   if        DataRielab>0
012600070831     c     *iso          movel     DataRielab    dataiso
012700070831     c     *iso          movel     DataRielab    dataultima
012800070831     c     *iso          movel     DataRielab    datamese
012900070831     c
013000070831     c                   else
013100060405     c* Fino alle ore 18:00 elaboro fino al giorno precedente
013200060405     c* Dopo   le ore 18:00 elaboro fino al giorno stesso
013300060405     c                   if        wora<=1800
013400060405     c     dataultima    subdur    1:*d          dataultima
013500060405     c                   endif
013600060405     c
013700060405     c                   if        �sdfcep=0
013800060405     c     dataultima    subdur    5:*d          dataiso
013900060405     c                   movel     dataiso       �sdfcep
014000060405     c                   endif
014100060405     c     *iso          move      �sdfcep       dataiso
014200060405     c     dataiso       adddur    1:*d          dataiso
014300070831     c                   endif
014400060830     c
014500060830     c* Determino oggi - 1 mese per le c.a.
014600060830     c     datamese      subdur    1:*m          datamese
014700060830     c                   movel     datamese      unmesefa          8 0
014800060405     c
014900060405     c                   endsr
015000060405      *-----------------------------------------------------------------*
015100060405      * Lettura bolle arrivi per linea di arrivo e DCM=0 o > data elab
015200060405      *-----------------------------------------------------------------*
015300060405     c     LegPO         begsr
015400060405     c
015500060405     c                   clear                   kdcm
015600060405     c     karb          setll     fnarb54l
015700060405     c     karb          reade(N)  fnarb54l
015800060830     c
015900060405    1c                   dow       not %eof(fnarb54l)
016000060830     c                   clear                   escludica         1
016100060405     c
016200060405     c* Escludo le bolle di recupero
016300060405     c* Escludo le bolle con data spedizione >= data elab.
016400061010     c* Escludo le bolle con lna 101
016500060405     c     arbcbo        lookup    rec                                    30
016600060405    2c                   if        not *in30
016700060405     c                   movel     arbaas        arbdsp            8 0
016800060405     c                   move      arbmgs        arbdsp
016900061010     c
017000061010    3c                   if        arbdsp<kdata  and arblna<>101
017100060830     c
017200060830     c* Escludo le spedizioni con C.A. da almeno 1 mese
017300060830     c                   clear                   fidn12ds
017400060830     c                   eval      i12aas = arbaas
017500060830     c                   eval      i12lnp = arblnp
017600060830     c                   eval      i12nrs = arbnrs
017700060830     c                   eval      i12nsp = arbnsp
017800060830     c                   eval      i12fel = 'E'
017900060830     c                   eval      i12fan = 'E'
018000060830      *
018100060830     c                   call      'FIDN12R'
018200060830     c                   parm                    fidn12ds
018300060830      *
018400060830      * se non ci sono errori
018500060830     c                   if        o12err = ' '
018600060830      * se numero di CA trovate maggiore di zero
018700060830     c                   if        o12nca > 0
018800060830     c                   eval      EscludiCA='S'
018900060830     c
019000060830     c                   z-add     *zeros        ii                3 0
019100060830     c     1             do        o12nca        ii
019200060830     c                   movea     skkey(ii)     dskey
019300060830     C                   Z-ADD     skdca(ii)     dctdca            8 0
019400060830     C                   Z-ADD     skdch(ii)     dctdch            8 0
019500060830     c* Se chiusa --> elaboro
019600060830     c                   if        dctdch>0
019700060830     c                   clear                   escludiCA
019800060830     c                   leave
019900060830     c                   endif
020000060830     c*
020100060830     c                   if        dctdca>unmesefa
020200060830     c                   clear                   escludiCA
020300060830     c                   leave
020400060830     c                   endif
020500060830     C                   ENDDO
020600060830      *
020700060830     c                   endif
020800060830     c                   endif
020900060405     c
021000060405     c* Elaboro le spedizioni abilitate in base a spunta part o arr
021100060830    4c                   if        EscludiCA=' ' and
021200060831    4c                             (arbama>0 or arbamp=1)
021300060405     c                   exsr      ScriviEVB
021400060405    4c                   endif
021500060405    3c                   endif
021600060405    2c                   endif
021700060405     c
021800060405     c     karb          reade(N)  fnarb54l
021900060405    1c                   enddo
022000060405     c
022100060405     c                   z-add     kdata         kdcm
022200060405     c* Leeggo bolle con data consegna > kdata
022300060405     c     karb          setgt     fnarb54l
022400060405     c     cepfil        reade     fnarb54l
022500060405    1c                   dow       not %eof(fnarb54l)
022600060405     c
022700060405     c* Escludo le bolle di recupero
022800060405     c     arbcbo        lookup    rec                                    30
022900060405    2c                   if        not *in30
023000060405     c                   movel     arbaas        arbdsp
023100060405     c                   move      arbmgs        arbdsp
023200061010    3c                   if        arbdsp<kdata and arblna<>101
023300060405     c
023400060405     c                   exsr      ScriviEVB
023500060405     c
023600060405     c* In questo caso, se calcolata, ricalcolo affidabilit�
023700060405    4c                   if        arbdce>0
023800060405     c                   movel     'S'           arbfbs
023900060405     c                   clear                   arbdti
024000060405     c                   clear                   arbhti
024100060405     c                   update    fnarb000
024200060405     c                   else
024300060405     c                   unlock    fnarb54l
024400060405    4c                   endif
024500060405    3c                   endif
024600060405     c
024700060405    2c                   endif
024800060405     c
024900060405     c     cepfil        reade     fnarb54l
025000060405    1c                   enddo
025100060405     c                   endsr
025200060405      *-----------------------------------------------------------------*
025300060405      * SCrivi record in fnevb00f
025400060405      *-----------------------------------------------------------------*
025500060405     c     ScriviEVB     begsr
025600060405     c     kevb          chain     fnevb04l
025700060405     c                   if        not %found(fnevb04l)
025800060405     c                   clear                   fnevb000
025900060405     c                   move      datacor       evbdtv
026000060405     c                   time                    evborv
026100061011     c                   movel     arblna        evbfle
026200060405     c                   z-add     arbaas        evbaas
026300060405     c                   z-add     arblnp        evblnp
026400060405     c                   z-add     arbnrs        evbnrs
026500060405     c                   z-add     arbnsp        evbnsp
026600060405     c                   z-add     kdata         evbdev
026700061109     c                   z-add     2200          evbhev
026800060405     c                   movel     cepcev        evbcev
026900060405     c                   write     fnevb000
027000060405     c                   endif
027100060405     c
027200060405     c                   endsr
027300051229      *-----------------------------------------------------------------*
027400060405     c* Lettura FNARB con selezione per CAP
027500051229      *-----------------------------------------------------------------*
027600060104     c     prepasql      begsr
027700051229
027800051229      *?Imposto la parte fissa dell'istruzione sql
027900051229     c                   eval      wrkgetlista =
028000060405     c                             'select *  from FNARB00F where -
028100060405     c                             ARBCAD ='''+ cepCAP +''' and  ARBNZD=''   '
028200060405     c                             + CEPNAR + ''' and (ARBDCM=0 or ARBDCM>'
028300060405     c                             +  %editc(kdata:'X') + ')'
028400060322      *?Ordinamento
028500060322     c                   eval      wrkgetlista = wrkgetlista +
028600060405     c                             ' order by arblna, arbdcm'
028700051229      *?Solo lettura
028800051229     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
028900051229
029000051229     C/EXEC SQL
029100051229     C+ PREPARE s1 FROM :wrkgetlista
029200051229     C/END-EXEC
029300051229     C
029400051229     C/EXEC SQL
029500051229     C+ DECLARE A1 CURSOR FOR S1
029600051229     C/END-EXEC
029700051229     C
029800051229     C/EXEC SQL
029900051229     C+ OPEN a1
030000051229     C/END-EXEC
030100051229
030200051229      *?Leggo il file
030300051229     c                   do        *hival
030400051229     C/EXEC SQL
030500060405     C+ FETCH NEXT FROM a1 INTO : DSFNARB
030600051229     C/END-EXEC
030700051229
030800051229     c                   select
030900051229
031000051229     c                   when      sqlcod = 100
031100051229     c                   leave
031200051229     c                   when      sqlcod < 0
031300051229     c                   seton                                        H1
031400051229     c                   exsr      srfine
031500051229     c                   other
031600060321     c
031700060321     c* Sommo e a cambio p.o. o data scrivo
031800060405     c                   EXSR      LeggiARB
031900060104     c
032000051229     c                   endsl
032100051229
032200051229     c                   enddo
032300051229     C/EXEC SQL
032400051229     C+ CLOSE a1
032500051229     C/END-EXEC
032600051229     c                   endsr
032700060321      *-----------------------------------------------------------------*
032800060405      * Leggo file FNARB
032900060321      *-----------------------------------------------------------------*
033000060405     c     LeggiARB      begsr
033100061016     c                   clear                   escludica         1
033200060405     c* Escludo le bolle di recupero
033300060405     c* Escludo le bolle con data spedizione >= data elab.
033400060405     c     arbcbo        lookup    rec                                    30
033500060405    1c                   if        not *in30
033600060405     c                   movel     arbaas        arbdsp
033700060405     c                   move      arbmgs        arbdsp
033800061010     c                   if        arbdsp<kdata and arblna<>101
033900060405     c* Se c'e' localit�, la controllo (anche con sinonimi
034000060405     c
034100060405     c                   clear                   welab             1
034200060405    2c                   if        ceploc<>*blanks
034300060405     c* Chiamata al tisi per avere la localit� corretta
034400060405     c                   clear                   tisi95ds
034500060405     C                   MOVEL     '7'           I95TCN
034600060405     C                   MOVEL     arbcAd        I95CAP
034700060405     C                   MOVEL     arblod        I95LOC
034800060405     C                   MOVEL     arbprd        I95PRV
034900060405     C                   MOVEL     arbnzd        I95NAR
035000060405     C                   MOVEL     arbaas        I95DAT
035100060405     C                   MOVE      arbmgs        I95DAT
035200060405     c                   call      'TISI95R'
035300060405     c                   parm                    tisi95ds
035400060405    3c                   if        o95err=' '  and o95loc=ceploc
035500060405     c                   eval      welab='S'
035600060405    3c                   endif
035700060405   x2c                   else
035800060405     c                   eval      welab='S'
035900060405    2c                   endif
036000060405     c
036100060405     c* Elaboro se bolla abilitata
036200060405    2c                   if        welab='S'
036300061005     c*
036400061005     c* Escludo le spedizioni con C.A. da almeno 1 mese
036500061005     c                   clear                   fidn12ds
036600061005     c                   eval      i12aas = arbaas
036700061005     c                   eval      i12lnp = arblnp
036800061005     c                   eval      i12nrs = arbnrs
036900061005     c                   eval      i12nsp = arbnsp
037000061005     c                   eval      i12fel = 'E'
037100061005     c                   eval      i12fan = 'E'
037200061005      *
037300061005     c                   call      'FIDN12R'
037400061005     c                   parm                    fidn12ds
037500061005      *
037600061005      * se non ci sono errori
037700061005    3c                   if        o12err = ' '
037800061005      * se numero di CA trovate maggiore di zero
037900061005    4c                   if        o12nca > 0
038000061005     c                   eval      EscludiCA='S'
038100061005     c
038200061005     c                   z-add     *zeros        ii                3 0
038300061005    5c     1             do        o12nca        ii
038400061005     c                   movea     skkey(ii)     dskey
038500061005     C                   Z-ADD     skdca(ii)     dctdca            8 0
038600061005     C                   Z-ADD     skdch(ii)     dctdch            8 0
038700061005     c* Se chiusa --> elaboro
038800061005     c                   if        dctdch>0
038900061005     c                   clear                   escludiCA
039000061005     c                   leave
039100061005     c                   endif
039200061005     c*
039300061005     c                   if        dctdca>unmesefa
039400061005     c                   clear                   escludiCA
039500061005     c                   leave
039600061005     c                   endif
039700061005    5C                   ENDDO
039800061005      *
039900061005    4c                   endif
040000061005    3c                   endif
040100061005     c
040200061005   2ac                   if        EscludiCA=' '
040300061005     c
040400060405    3c                   if        (arbdcm=0 and arbama>1) or
040500060405     c                             (arbdcm=0 and arbamp=1) or
040600060405     c                             arbdcm>0
040700060405     c                   exsr      ScriviEVB
040800060405     c
040900060405    4c                   if        arbdcm>0 and arbdce>0
041000060405     c     karb1         chain     fnarb01l
041100060405    5c                   if        %found(fnarb01l)
041200060405     c                   movel     'S'           arbfbs
041300060405     c                   clear                   arbdti
041400060405     c                   clear                   arbhti
041500060405     c                   update    fnarb001
041600060405    5c                   endif
041700060405    5c                   endif
041800060405    4c                   endif
041900060405    3c                   endif
042000061005   2ac                   endif
042100061005    2c                   endif
042200060405    1c                   endif
042300060405     c*
042400060321     c                   endsr
042500051229      *-----------------------------------------------------------------*
042600051229      * Fine programmma con codice errore
042700051229      *-----------------------------------------------------------------*
042800051229     c     srfine        begsr
042900051229     c                   eval      *InLr = *On
043000051229     c                   return
043100051229     c                   endsr
043200051227
043300051227      *-----------------------------------------------------------------*
043400051227      * Routine iniziale
043500051227      *-----------------------------------------------------------------*
043600051227     c     *inzsr        begsr
043700051227
043800051227     c     *entry        plist
043900051227     c                   parm                    kpjba
044000070831     c                   movel     kpjbu         Dataalfa
044100070831     c
044200070831     c* Passata data di rielaborazione
044300070831     c                   if        dataalfa>*zeros
044400070831     c                   movel     Dataalfa      DataRielab
044500070831     c                   else
044600070831     c                   clear                   DataRielab
044700070831     c                   endif
044800070831     c
044900060405     c     karb          klist
045000060405     c                   kfld                    cepfil
045100060405     c                   kfld                    kdcm
045200060405     c     Ktbe          Klist
045300060405     c                   Kfld                    kTbeCod
045400060405     c                   Kfld                    kTbeKe1
045500060405     C     KE2TAB        KLIST
045600060405     C                   KFLD                    KTBKUT
045700060405     C                   KFLD                    KTBCOD
045800060405     C     KE3TAB        KLIST
045900060405     C                   KFLD                    KTBKUT
046000060405     C                   KFLD                    KTBCOD
046100060405     C                   KFLD                    KTBkey
046200060405     c     karb1         klist
046300060405     c                   kfld                    arbaas
046400060405     c                   kfld                    arblnp
046500060405     c                   kfld                    arbnrs
046600060405     c                   kfld                    arbnsp
046700060405     c     kevb          klist
046800060405     c                   kfld                    arbaas
046900060405     c                   kfld                    arblnp
047000060405     c                   kfld                    arbnrs
047100060405     c                   kfld                    arbnsp
047200060405     c                   kfld                    cepcev
047300060405     c                   kfld                    kdata
047400060405     c                   kfld                    kora
047500060328     c
047600060328     c
047700060328     c                   Clear                   Tibs02Ds
047800060405     c                   Clear                   dsdf
047900060328     c                   Eval      T02tla = 'L'
048000060328     c                   Eval      T02Mod = 'C'
048100060328     c                   Eval      T02Sif = Knsif
048200060405     c                   Eval      T02Cod = 'SDF'
048300060405     c                   Movel(p)  '046'         T02Ke1
048400060328     c                   Call      'TIBS02R'
048500060328     c                   Parm                    Kpjba
048600060328     c                   Parm                    Tibs02Ds
048700060328     c                   If        T02Err = *Blanks
048800060405     c                   Movel     T02Uni        dsdf
048900060328     c                   EndIf
049000060405     c
049100060405     c* Carico tipi bolla di recupero
049200060405     C                   CLEAR                   xx                4 0
049300060405     C     KE2TAB        SETLL     TABEL00F
049400060405     C     KE2TAB        READE     TABEL00F                               98
049500060405     C     *IN98         DOWEQ     *OFF
049600060405     C                   MOVEL     TBLUNI        DS3a
049700060405     c                   if        �3arbl='R'
049800060405     C                   ADD       1             xx
049900060405     C                   MOVEL     TBLKEY        rec(xx)
050000060405     c                   endif
050100060405     c
050200060405     C     KE2TAB        READE     TABEL00F                               98
050300060405     C                   ENDDO
050400060405     C*
050500051229     c                   endsr
