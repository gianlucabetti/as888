000100070214     h Decedit('0,') Datedit(*dmy.) Option(*nodebugio)
000200070208
000300070208      *------------------------------------------------------------------------*
000400070208      *
000500070208      *                 STAMPA PROSPETTO ORM               ?
000600070208      *
000700070208      *------------------------------------------------------------------------*
000800080418     ffirpt01l  if   e           k disk
000801080418     ffirpd01l  if   e           k disk
000900070208     ffiapd01l  if   e           k disk
001000070208     ffnorm01l  if   e           k disk
001100070208     ffior24p   o    e             printer oflind(*in90)
001200070212
001300070212      *------------------------------------------------------------------------*
001400070212      *  RIEPILOGO INDICATORI
001500070212      *------------------------------------------------------------------------*
001700070214      * 02 - ORM manuale
001800070212      * 30 - lookup
001900070212      * 90 - salto pagina
001901080404      * 91 - almeno una riga stampata
002000070212      * 99 - comodo
002100070212      *------------------------------------------------------------------------*
002200070208
002300070212      *   V A R I A B I L I
002400070208     d kapdtip         s                   like(apdtip) inz('A')
002801080402     d wndc_ok         s              1    inz
003200070212
003300070212      *   S C H I E R E
003800070208
003900070212      *   D S   I N T E R N E / E S T E R N E
004700070212
004800070212     d                 ds
004900070212     d  dsorm                  1     14  0
005000070213     d  wormpoe                1      3  0
005100070213     d  wormnsr                4      5  0
005200070213     d  wormnor                6     12  0
005300070213     d  wormnrv               13     14  0
005400070212
005500070208     d wlbdat          ds                  inz
005600070208     d  g02dat                 1      8  0
005700070208     d  g02inv                 9     16  0
005800070208     d  g02err                17     17
005900070208     d  g02tgi                18     22  0
006000070208
006100070208     d azuteds       e ds                  extname(azute00f)
006200070208     d ddatiute      e ds
006300070208     d fior24ds      e ds
006500070208     d kpjba         e ds
006600070208     d tibs34ds      e ds
006700070208
006800070208      *------------------------------------------------------------------------*
006900070208
007000080418     c     kfirpt        setll     firpt01l
007100070213     c                   do        *hival
007200080418     c     kfirpt1       reade     firpt01l
007300070208
007400080418     c                   if        %eof(firpt01l)
007500070208     c                   leave
007600070208     c                   endif
007700071214
007800080418     c                   if        rptnfv > i24ndcal
007900071214     c                   leave
008000071214     c                   endif
008802080402
008803080402      * controllo se la distinta soddisfa le selezioni del video
008804080402     c                   exsr      sr_ctrndc
008901080402
009000080403      * se la distinta non soddisfa le  selezioni  del video leggo la successiva
009001080402     c                   If        Wndc_ok = 'N'
009002080402     c                   iter
009003080402     c                   endif
009700070212
009800080418      * elaboro dettaglio
009801080418     c                   exsr      sr_eladet
009802080421      * stampo totali distinta
009803080421     c                   exsr      sr_statot
010300070212
012400070212     c                   enddo
012500071214
012800071214
012900071214      * stampo fine
012901080422     c                   if        *in91
013000071214     c                   write     or24e1
013001080422     c                   endif
013100071214
013200071214     c                   eval      *inlr = *on
013300071214
013400071214      *------------------------------------------------------------------------*
013500080422      * STAMPA TOTALI
013600071214      *------------------------------------------------------------------------*
013700080422     c     sr_statot     begsr
013701080509      * Stampo totali sell'autotrasportatore
013702080509     c   90              write     or24t1
013703080509     c   90              write     or24t2
013704080509     c   90              eval      *in90 = *off
013705080509     c                   write     or24g1
013706080509      * pulisco i totali
013707080509     c                   clear                   tncl
013708080509     c                   clear                   tbnc
022500080509      * stampo totali da spc
022501080509     c                   eval      tsncl = rpttco
022502080509     c                   eval      tsbnc = rpttbn
022600080422
022601080509     c   90              write     or24t1
022602080509     c   90              write     or24t2
022603080509     c   90              eval      *in90 = *off
022700080509     c                   write     or24g2
022800071214
022900071214     c                   endsr
024800070213
024900070213      *------------------------------------------------------------------------*
025000070213      * CARICO I DATI DI DETTAGLIO PER LA STAMPA
025100070213      *------------------------------------------------------------------------*
025200080421     c     sr_eladet     begsr
025300070213
025302080421     c     kfirpd        setll     firpd01l
025303080421     c                   do        *hival
025304080421     c     kfirpd        reade     firpd01l
025305080421
025306080421     c                   if        %eof(firpd01l)
025307080421     c                   leave
025308080421     c                   endif
025500070213
025800080422     c                   eval      pman = rpdman
025900080422     c                   eval      prsr = rpdrsr
026000080422     c                   eval      pinr = rpdinr
026001080422      * se ORM non manuale aggancio il file degli ORM
026002080422     c                   if        pman = ' '
026003080422     c                   setoff                                       02
026004080422     c                   movel     rpdiddoc      dsorm
026005080422
026400080422     c                   eval      ppoe = wormpoe
026500080422     c                   eval      pnsr = wormnsr
026600080422     c                   eval      pnor = wormnor
026700080422     c                   eval      pnrv = wormnrv
026701080422
026702080422     c                   eval      ormpoe = wormpoe
026703080422     c                   eval      ormnsr = wormnsr
026704080422     c                   eval      ormnor = wormnor
026705080422     c                   eval      ormnrv = wormnrv
026800070213     c     kfnorm        chain     fnorm01l
026900070213     c                   if        not %found(fnorm01l)
027000070213     c                   clear                   prsr
027100070213     c                   clear                   pinr
027200070213     c                   endif
027300070213     c                   eval      prsr = ormrsr
027400070213     c                   eval      pinr = orminr
027501080422     c                   else
027502080422     c                   seton                                        02
027503080422     c                   endif
027600070213
027700070213      * colli/bancali
027800080422     c                   move      rpdncl        pncl
027900080422     c                   move      rpdbnc        pbnc
027901080509     c                   add       pncl          tncl
027902080509     c                   add       pbnc          tbnc
027903080404
027906080404      * causale mancato ritiro
027907080422     c                   eval      pcmr=rpdcmr
028000070213
028100070213      * flag immesso da SPC
028200080422     c                   eval      pflgspc = rpdflgspc
028202080422
028203080422      * inverto la data
028204080422     c                   clear                   wlbdat
028205080422     c                   movel     rpddtr        g02inv
028206080422     c                   eval      g02err = '3'
028207080422     c                   call      'XSRDA8'
028208080422     c                   parm                    wlbdat
028209080422     c                   eval      pdata = g02dat
028210080422     c                   movel     rpdorr        pora
028211080422
028212080422
028213080422      * stampo dettaglio
028214080509     c   90              write     or24t1
028215080422     c   90              write     or24t2
028216080509     c   90              eval      *in90 = *off
028217080422     c                   write     or24d1
028218080422
028219080422      * segnalo il fatto che mi ha stampato almeno una riga
028220080422     c                   eval      *in91 = *on
028221080422
028300080422     c                   enddo
028400070213     c                   endsr
028500070208
028502080402      *------------------------------------------------------------------------*
028503080402      * Controllo se la distinta soddisfa le selezioni fatte nel video di lancio
028504080402      *------------------------------------------------------------------------*
028505080402     c     SR_ctrndc     begsr
028506080402
028507080402     c                   clear                   Wndc_ok
028508080402
028516080403      * data distinta
028517080422     c                   if        rptdfv <> i24data
028518080403     c                   eval      Wndc_ok  = 'N'
028519080403     c                   leavesr
028520080403     c                   endif
028521080402      * tipo uscita
028522080422     c                   If        i24fpp <> ' ' and rptfpp <> i24fpp
028523080402     c                   eval      Wndc_ok  = 'N'
028524080402     c                   leavesr
028525080402     c                   endif
028526080402      * parcel /messaggerie
028527080422     c                   if        (i24tpm = 'P' and rpttpm <> ' ') or
028528080422     c                             (i24tpm = 'M' and rpttpm <> 'M')
028533080402     c                   eval      Wndc_ok  = 'N'
028534080402     c                   leavesr
028535080402     c                   endif
028536080422
028537080422      * se tutto ok stampo la testata
028538080422
028539080422      * data distinta
028540080422     c                   eval      g02inv = i24data
028541080422     c                   eval      g02err = '3'
028542080422     c                   call      'XSRDA8'
028543080422     c                   parm                    wlbdat
028544080422     c                   eval      pddc = g02dat
028545080430
028546080430     c                   eval      pnfv = rptnfv
028547080422
028548080422      * descrizione AUT
028549080422     c     kfiapd        chain     fiapd01l
028550080422
028551080422
028552080422      * S T A M P A
028553080422      * stampo testata
028555080422     c                   write     or24t1
028556080422     c                   write     or24t2
028578080402
028579080402     c                   ENDSR
028580080402
028600070208      *------------------------------------------------------------------------*
028700070208      * ROUTINE INIZIALE
028800070208      *------------------------------------------------------------------------*
028900070208     c     *inzsr        begsr
029000070208
029100070208     c     *entry        plist
029200070208     c                   parm                    kpjba
029300070208
029400070208     c     *dtaara       define    §azute        azuteds
029500070208     c     *dtaara       define    §datiute      ddatiute
029600070208     c                   in(e)     *dtaara
029700070208     c                   if        %error or rsut = *blanks
029800070208     c                   clear                   tibs34ds
029900070208     c                   call      'TIBS34R'
030000070208     c                   parm                    tibs34ds
030100070208     c                   in        *dtaara
030200070208     c                   endif
030300070208
030400070208     c                   eval      fior24ds = kpjbu
030800070214
030900070214     c                   time                    wtime
031000070214     c                   z-add     *date         wdate
031100070208
031200070208     c     kfiapd        klist
031300070208     c                   kfld                    kapdtip
031400080430     c                   kfld                    rptpdr
031500070208
031600080418     c     kfirpt        klist
031800070208     c                   kfld                    i24fgs
031900071214     c                   kfld                    i24ndcda
032000071214
032100080418     c     kfirpt1       klist
032300071214     c                   kfld                    i24fgs
032301080402
032302080421     c     kfirpd        klist
032303080421     c                   kfld                    rptfgs
032304080421     c                   kfld                    rptnfv
032305080421     c                   kfld                    rptpdr
032306080421
032500070208     c     kfnorm        klist
032600070208     c                   kfld                    ormpoe
032700070208     c                   kfld                    ormnsr
032800070208     c                   kfld                    ormnor
032900070208     c                   kfld                    ormnrv
033000070208
033100070208     c                   endsr
