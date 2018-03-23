000100080108     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200080326     h dftactgrp(*no) actgrp(*caller)
000300080108
000400080108      *---------------------------------------------------------------*
000500080108      *                                                               *
000600080326      *             SCRIVE/CONTROLLA FASE DISTINTA                    *
000700080108      *                                                               *
000800080108      *---------------------------------------------------------------*
000900080108
001000080328     ffidsf02l  uf a e           k disk    commit(or90comit) usropn
001100080326     ffidst01l  if   e           k disk
001200080516     fprtemail  o    f  132        printer  oflind(*inof)  usropn
001300080108
001400080108      *------------------------------------------------------------------------*
001500080108      * riepilogo indicatori
001600080108      *------------------------------------------------------------------------*
001700080108
001800080108      *------------------------------------------------------------------------*
001900080108      * campi di work
002000080108      *------------------------------------------------------------------------*
002100080326     d kdsfnpg         s                   like(dsfnpg) inz(4)
002200080326     d kdsfnfv         s                   like(dsfnfv)
002300080326     d kdsffgs         s                   like(dsffgs)
002400080326     d kdsftrd         s                   like(dsftrd)
002500080327     d wtime           s             14
002600080516
002700080516     d lenght          s             15  5
002800080516     d o_testo         s            132    inz
002900080516     d qcmd            s            512    inz
003000080516     d $invio          s              1n   inz(*off)
003100080516     d $okmra          s              1n   inz(*off)
003200080108
003300080108      *------------------------------------------------------------------------*
003400080108      * schiere
003500080108      *------------------------------------------------------------------------*
003600080108
003700080108      *------------------------------------------------------------------------*
003800080108      * ds interne/esterne
003900080108      *------------------------------------------------------------------------*
004000080326     d ddsf          e ds
004100080924     d fidsfrchds    e ds                  prefix(§RCH:4)
004200080326     d fior90ds      e ds
004300080326     d kpjba         e ds
004400080326     d tibs02ds      e ds
004500080516
004600080516     d dmradan       e ds                  inz
004700080516     d trtcm1ds      e ds                  inz
004800080516      *    .§cm1mitt = Indirizzo e-mail del mittente
004900120314     d   §cm1mitt    e                     inz('ced@brt.it')
005000080516      *    ·§cm1dst  = Indirizzo e-mail del destinatario
005100120314     d   §cm1dst     e                     inz('monica.beghelli@brt.it')
005200080516      *    ·§cm1tips = Tipo lettera via e-mail:
005300080516      *     "LET" = testo allegato in corpo con logo
005400080516      *             (richiede righe libere iniziali per il logo)
005500080516      *     "COR" = testo integrato senza logo
005600080516      *             (non consente né UNDERLINE né HIGHLIGHT)
005700080516     d   §cm1tips    e                     inz('COR')
005800080516      *    ·§cm1po   = Filiale
005900080516     d   §cm1po      e                     inz('046')
006000080516      *    ·§cm1var  = Oggetto e-mail
006100080516     d   §cm1var     e                     inz('*OBJM*+
006200080516     d                                     Fase RQE non scritta')
006300080516      *    ·§cm1sts  = Stato
006400080516     d   §cm1sts     e                     inz(*off)
006500080516      *    ·§cm1idp  = Id processo
006600080519     d   §cm1idp     e                     inz('2')
006700080516
006800080516     d psds           sds
006900080516     d   sdspgm          *proc
007000080516     d   jobuser             254    263                                         User name
007100080207
007200080207      *------------------------------------------------------------------------*
007300080207      * costanti
007400080207      *------------------------------------------------------------------------*
007500080516     d c_sede          c                   const('046')
007600080516      *  - comando di override al prtf
007700080516     d c_cmdovrprtf    c                   const('OVRPRTF +
007800080516     d                                           file(PRTEMAIL) +
007900080516     d                                           pagesize(66 132) +
008000080516     d                                           lpi(6) cpi(10) +
008100080516     d                                           ovrscope(*actgrpdfn) +
008200080516     d                                           ')
008300080516     d c_cmddltovr     c                   const('DLTOVR +
008400080516     d                                            file(PRTEMAIL) +
008500080516     d                                            lvl(*actgrpdfn)')
008600080108
008700080108      *------------------------------------------------------------------------*
008800080108
008900080326      * Controllo se ok tutti i dati passati
009000080326     c                   exsr      sr_ctrdati
009100080326
009200080326      * Devo scrivere la fase
009300080326     c                   if        or90tla = 'W'
009400080326     c                   exsr      sr_scrivi
009500080326     c                   endif
009600080326
009700080326      * Devo controllare l'esistenza della fase
009800080326     c                   if        or90tla = 'C'
009900080326     c                   exsr      sr_controlla
010000080326     c                   endif
010100080108
010200080108     c     fine          tag
010300080516
010400080516      * se errore mando mail...per capire come mai non scrive la fase RQE
010500080516     c                   if        or90err = 'E' and or90fas ='RQE' and
010600080516     c                             or90tla = 'W'
010700080516     c                   exsr      sr_mail
010800080516     c                   exsr      sr_end
010900080516     c                   endif
011000080108
011100080108     c                   eval      *inlr = *on
011200080326
011300080326      *------------------------------------------------------------------------*
011400080326      * Controllo dati passati
011500080326      *------------------------------------------------------------------------*
011600080326     c     sr_ctrdati    begsr
011700080326
011800080326     c                   select
011900080326      * tipo elaborazione
012000080326     c                   when      or90tla <> 'W' and or90tla <> 'C'
012100080326     c                   eval      or90err = 'E'
012200080326     c                   eval      or90msg = 'Tipo elaborazione errata'
012300080326     c                   goto      fine
012400080326      * fase
012500080326     c                   when      or90fas = *blanks
012600080326     c                   eval      or90err = 'E'
012700080326     c                   eval      or90msg = 'Fase non passata'
012800080326     c                   goto      fine
012900080326      * filiale distinta
013000080326     c                   when      or90fgs = *zeros
013100080326     c                   eval      or90err = 'E'
013200080326     c                   eval      or90msg = 'Filiale non passata'
013300080326     c                   goto      fine
013400080326      * distinta
013500080326     c                   when      or90ndc = *zeros
013600080326     c                   eval      or90err = 'E'
013700080326     c                   eval      or90msg = 'Distinta non passata'
013800080326     c                   goto      fine
013900080326      * flag inserimento
014000080326     c                   when      or90flgins <> 'A' and or90flgins <> 'P' and
014100080326     c                             or90tla = 'W'
014200080326     c                   eval      or90err = 'E'
014300080326     c                   eval      or90msg = 'Flag inserimento errato'
014400080326     c                   goto      fine
014500080326     c                   endsl
014600080326
014700080326      * controllo se esite la distinta
014800080326     c                   eval      kdsfnfv = or90ndc
014900080326     c                   eval      kdsffgs = or90fgs
015000080326     c     kfidst        chain     fidst01l
015100080326     c                   if        not %found(fidst01l) or dstatb <> *blanks
015200080326     c                   eval      or90err = 'E'
015300080326     c                   eval      or90msg = 'Distinta non trovata'
015400080408     c                   goto      fine
015500080328     c                   endif
015600080326
015700080326      * controllo se esite la fase in tabella
015800080326     c                   clear                   ddsf
015900080326     c                   clear                   tibs02ds
016000080326     c                   eval      t02mod = 'C'
016100080326     c                   eval      t02sif = knsif
016200080326     c                   eval      t02cod = 'DSF'
016300080326     c                   movel     or90fas       t02ke1
016400080326     c                   call      'TIBS02R'
016500080326     c                   parm                    kpjba
016600080326     c                   parm                    tibs02ds
016700080326     c                   if        t02err = *blanks
016800080326     c                   eval      ddsf = t02uni
016900080326     c                   else
017000080326     c                   eval      or90err = 'E'
017100080326     c                   eval      or90msg = 'Fase non trovata'
017200080408     c                   goto      fine
017300080326     c                   endif
017400080326
017500080326     c                   endsr
017600080326
017700080326      *------------------------------------------------------------------------*
017800080326      * Scrivo la fase
017900080326      *------------------------------------------------------------------------*
018000080326     c     sr_scrivi     begsr
018100080326
018200080326     c                   eval      kdsffgs = or90fgs
018300080326     c                   eval      kdsfnfv = or90ndc
018400080326     c                   eval      kdsftrd = or90fas
018500080328     c     kfidsf        chain     fidsf02l
018600080326      * se non esiste scrivo
018700080328     c                   if        not %found(fidsf02l)
018800080326     c                   clear                   fidsf000
018900080326     c                   eval      dsfnpg = kdsfnpg
019000080326     c                   eval      dsffgs = kdsffgs
019100080326     c                   eval      dsfnfv = kdsfnfv
019200080326     c                   eval      dsftrd = kdsftrd
019300080327     c                   eval      wtime = %char(%timestamp:*iso0)
019400080327     c                   move      wtime         dsfdtorin
019500080326     c                   eval      dsfpdaas4 = or90flgins
019600080924      * se fase 'RCH'
019700080924     c                   if        or90fas = 'RCH'
019800080924      * se viene scritta da AS400 memorizzo l'utente nel campo dati
019900080924     c                   if        or90flgins = 'A'
020000080402     c                   eval      dsfdati = knmus
020100080924      * se viene scritta da PDA memorizzo l'SPC nel campo dati
020200080924     c                   else
020300080924     c                   eval      dsfdati = fidsfrchds
020400080402     c                   endif
020500080924     c                   endif
020600080909      * se fase 'RQE' memorizzo l'utente nel campo dati
020700080909     c                   if        or90fas = 'RQE'
020800080909     c                   eval      dsfdati = knmus
020900080909     c                   endif
021000080326     c                   write     fidsf000
021100080326     c                   endif
021200080326
021300080326     c                   endsr
021400080326
021500080326      *------------------------------------------------------------------------*
021600080326      * Controllo se esiste la fase distinta
021700080326      *------------------------------------------------------------------------*
021800080326     c     sr_controlla  begsr
021900080326
022000080326     c                   eval      kdsffgs = or90fgs
022100080326     c                   eval      kdsfnfv = or90ndc
022200080326     c                   eval      kdsftrd = or90fas
022300080328     c     kfidsf        setll     fidsf02l
022400080326      * se non esiste torno errore
022500080328     c                   if        not %equal(fidsf02l)
022600080326     c                   eval      or90err = 'E'
022700080326     c                   eval      or90msg = 'Fase inesistente'
022800080328      * se esiste torno flag di fase esistente
022900080328     c                   else
023000080328     c                   eval      or90ndcok = '1'
023100080328     c                   endif
023200080326
023300080326     c                   endsr
023400080516
023500080516      *------------------------------------------------------------------------*
023600080516      * ROUTINE PER INVIARE MAIL SE ERRORE CODICE SPC
023700080516      *------------------------------------------------------------------------*
023800080516     c     sr_mail       begsr
023900080516
024000080516      * Reperimento tab. "MRA" se non già recuperata
024100080516     c                   if        not $okmra
024200080516     c                   clear                   dmradan
024300080516     c                   clear                   tibs02ds
024400080516     c                   eval      t02mod = 'C'
024500080516     c                   eval      t02sif = knsif
024600080516     c                   eval      t02cod = 'MRA'
024700080516     c                   eval      t02ke1 = sdspgm
024800080516     c                   call      'TIBS02R'
024900080516     c                   parm                    kpjba
025000080516     c                   parm                    tibs02ds
025100080516     c                   if        t02err = *blanks
025200080516     c                   eval      dmradan = t02uni
025300080516     c                   eval      $okmra = *on
025400080516     c                   endif
025500080516     c                   endif
025600080516
025700080516      * Override al file di stampa ed apertura dello stesso
025800080516     c                   if        not %open(prtemail)
025900080516     c                   exsr      sr_openprtf
026000080516     c                   endif
026100080516
026200080516     c                   eval      o_testo = t02ke1
026300080516     c                   except    prtdet
026400080516
026500080516     c                   endsr
026600080516
026700080516      *------------------------------------------------------------------------*
026800080516      * OVERRIDE AL FILE DI STAMPA PER IMPOSTARE I DATI PER L'INVIO VIA MAIL
026900080516      *  + STAMPA INIZIO MAIL
027000080516      *------------------------------------------------------------------------*
027100080516     c     sr_openprtf   begsr
027200080516
027300080516      * Override al file di stampa
027400080516     c                   exsr      sr_override
027500080516
027600080516     c                   open      prtemail
027700080516
027800080516      * Stampa una testata se NON è richiesta la e-mail
027900080516     c                   if        §mradreg = *blanks
028000080516     c                   eval      o_testo = jobuser + ' - ' + sdspgm
028100080516     c                             + ' - ' + %editc( *date : 'Y' )
028200080516     c                             + ' - *REM* ' + %subst(§cm1var : 7 : 70)
028300080516     c                   except    prtdet
028400080516     c                   clear                   o_testo
028500080516     c                   except    prtdet
028600080516     c                   except    prtdet
028700080516     c                   endif
028800080516
028900080516      * Stampa testo iniziale
029000080516     c                   eval      o_testo = 'Fase RQE non scritta per la +
029100080516     c                                        distinta ' +
029200080516     c                                        %editc(or90fgs:'X') + '-' +
029300080516     c                                        %editc(or90ndc:'X')
029400080516     c                   except    prtdet
029500080516      * riga aggiuntiva per errore
029600080516     c                   eval      o_testo = or90msg
029700080516     c                   except    prtdet
029800080516
029900080516      * Stampa una riga vuota
030000080516     c                   clear                   o_testo
030100080516     c                   except    prtdet
030200080516
030300080516     c                   endsr
030400080516
030500080516      *------------------------------------------------------------------------*
030600080516      * OVERRIDE AL FILE DI STAMPA PER IMPOSTARE I DATI PER L'INVIO VIA MAIL
030700080516      *  + STAMPA INIZIO MAIL
030800080516      *------------------------------------------------------------------------*
030900080516     c     sr_override   begsr
031000080516
031100080516     c                   reset                   $invio
031200080516     c                   reset                   trtcm1ds
031300080516
031400080516     c                   if        §mradreg <> *blanks
031500080516     c                   eval      §cm1mitt = %trim(§mradmitt)
031600080516     c                   eval      §cm1dst  = %trim(§mraddest)
031700080516     c                   eval      §cm1tips = §mradreg
031800080516     c                   eval      §cm1po   = c_sede
031900080516     c                   eval      §cm1var  = '*OBJM*' + §mraddes
032000080516     c                   eval      §cm1idp  = §mradidpro
032100080516     c                   eval      qcmd = c_cmdovrprtf
032200080516     c                             + ' outq(' + %trim(§mradoutqi) + ')'
032300080516     c                             + ' usrdfndta(''' + trtcm1ds + ''')'
032400080516     c                   else
032500080516     c                   eval      qcmd = c_cmdovrprtf
032600080516     c                   endif
032700080516
032800080516     c                   eval      lenght = %size(qcmd)
032900080516     c                   call(e)   'QCMDEXC'
033000080516     c                   parm                    qcmd
033100080516     c                   parm                    lenght
033200080516
033300080516     c                   if        §mradreg <> *blanks and not %error
033400080516     c                   eval      $invio = *on
033500080516     c                   endif
033600080516
033700080516     c                   endsr
033800080516
033900080516      *------------------------------------------------------------------------*
034000080516      * ROUTINE FINALE
034100080516      *------------------------------------------------------------------------*
034200080516     c     sr_end        begsr
034300080516
034400080516     c                   if        %open(prtemail)
034500080516
034600080516      * Chiusura dello spool?
034700080516     c                   clear                   o_testo
034800080516     c                   except    prtdet
034900080516     c                   eval      o_testo = '***   Fine Lista   ***'
035000080516     c                   except    prtdet
035100080516
035200080516     c                   close     prtemail
035300080516
035400080516      * Eliminazione overflow?
035500080516     c                   eval      lenght = %size(qcmd)
035600080516     c                   eval      qcmd = c_cmddltovr
035700080516     c                   call(e)   'QCMDEXC'
035800080516     c                   parm                    qcmd
035900080516     c                   parm                    lenght
036000080516
036100080516     c                   endif
036200080516
036300080516     c                   endsr
036400080108
036500080108      *------------------------------------------------------------------------*
036600080108      * routine iniziale
036700080108      *------------------------------------------------------------------------*
036800080108     c     *inzsr        begsr
036900080108
037000080108     c     *entry        plist
037100080326     c                   parm                    kpjba
037200080326     c                   parm                    fior90ds
037300080326
037400080328     c                   clear                   or90ndcok
037500080326     c                   clear                   or90err
037600080326     c                   clear                   or90msg
037700080326
037800080326      * disabilito comit su FIDSF a meno che non sia espressamente
037900080326      * richiesto ('1')
038000080326     c                   if        or90comit <> '1'
038100080326     c                   eval      or90comit = '0'
038200080326     c                   endif
038300080326
038400080328     c                   open      fidsf02l
038500080924
038600080924      * se richiamato per scrittura fase 'RCH' imposto la ds esterna per
038700080924      * memorizzare i dati su DSFDATI
038800080924     c                   if        or90fas = 'RCH' and or90tla = 'W'
038900080924     c                   eval      fidsfrchds = kpjbu
039000080924     c                   endif
039100080326
039200080326     c     kfidsf        klist
039300080326     c                   kfld                    kdsfnpg
039400080326     c                   kfld                    kdsfnfv
039500080326     c                   kfld                    kdsffgs
039600080326     c                   kfld                    kdsftrd
039700080326
039800080326     c     kfidst        klist
039900080326     c                   kfld                    kdsfnpg
040000080326     c                   kfld                    kdsfnfv
040100080326     c                   kfld                    kdsffgs
040200080326
040300080108     c                   endsr
040400080516
040500080516     oprtemail  e            prtdet      1
040600080516     o                       o_testo
