000100070322     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200031017
000300031017      *---------------------------------------------------------------*
000400031017      *                                                               *
000500070502      *                     PARTENZA GEO                              *
000600031017      *                                                               *
000700031017      *---------------------------------------------------------------*
000800031017
000900160905     ffidg97d   cf   e             workstn sfile(dg98s01:nrr1)
001000070424     fazorg01l  uf   e           k disk    usropn
001001160906     f                                     extfile(azorgs)
004500070308      *------------------------------------------------------------------------*
004600070308      * campi di work
004700070308      *------------------------------------------------------------------------*
004800070323     d command         s             80
004900070503     d errore          s             20
005400070323     d lung            s             15  5 inz(80)
005401160906     d tibs55ds      E DS
005500070323     d nrr1            s                   like(recsf1)
005501160905     d sav_w1cfil      s                   like(w1cfil)
006400070508     d wflag           s              1    inz(*off)
006500080118     d wforza          s               n
006600080118     d w£6             s               n
006601160905     d w0030           s                   like(orgfil)
006602160905     d w0080           s              8  0 inz
007500070308     d xx              s              2  0 inz
007501160906     d azorgs          s             21
007502160906     d azorgf          s             21
007600070328
007700070323      *------------------------------------------------------------------------*
007800070323      * schiere
007900070323      *------------------------------------------------------------------------*
008100031030
009500070424     d azuteds       e ds                  extname(azute00f)
009600070424     d ddatiute      e ds
010300070424     d fnlv55ds      e ds
010500070308     d kpjba         e ds
010600160905     d og150         e ds
010800070424     d tibs34ds      e ds
010900070308     d trul06ds      e ds                  inz
011000070308     d  lin                    1     90    dim(30)
011100070308
011200070424      *------------------------------------------------------------------------*
011300070424      * costanti
011400070424      *------------------------------------------------------------------------*
011500160905     d password        c                   const('TURNO     ')
011600070424
011700070308      *------------------------------------------------------------------------*
011800070528
011801160905     c                   exsr      sr_carvideo1
012000070424
012001160905     c     inizio        tag
012200070525
012300070424     c                   exsr      sr_video1
012400070424
012500070424     c                   exsr      sr_video2
012600080118
012700080118     c                   eval      wforza = *off
012800070308
012900070308     c     fine          tag
013000070328
013100070308     c                   eval      *inlr = *on
013200070525
013300070525      *------------------------------------------------------------------------*
013400070525      * carico i dati nella prima videata
013500070525      *------------------------------------------------------------------------*
013600070525     c     sr_carvideo1  begsr
013700070525
014200160905     c                   clear                   w1tur
014300160905     c                   clear                   w1ima
014400070525     c                   clear                   w1cpsw
014401160905     c                   eval      sav_w1cfil = w1cfil
018500070525
018600070525     c                   endsr
018700070424
018800070424      *------------------------------------------------------------------------*
018900070424      * gestione prima videata
019000070424      *------------------------------------------------------------------------*
019100070424     c     sr_video1     begsr
019200070424
019300070424     c                   do        *hival
019400070424     c                   exfmt     dg98d01
019500070424     c                   eval      *in28 = *off
019600070424     c                   clear                   w1cmsg
019700070502     c                   clear                   h1colo
019800070502     c                   clear                   h1riga
019900070424
020000070424      * f3=fine
020100070424     c                   if        *inkc
020200070424     c                   goto      fine
020300070424     c                   endif
020400070424
020401160905     C   Kd              EXSR      ric01
020500070424      * controllo
020600070424     c                   exsr      sr_ctrd01
020700070424
020800070424     c                   if        *in28
020900070424     c                   iter
021000070424     c                   endif
021100070424
021200070424      * f6=conferma
021300070424     c                   if        *inkf
021400070424     c                   leave
021500070424     c                   endif
021600070424
021700070424     c                   enddo
021800070424
021900070424     c                   endsr
022000070308
022100070424      *------------------------------------------------------------------------*
022200070424      * gestione seconda videata
022300070424      *------------------------------------------------------------------------*
022400070424     c     sr_video2     begsr
022500070424
022600070424      * cerco le filiali gestite dalla filiale da elaborare
022700070424     c                   clear                   trul06ds
022800070424     c                   eval      d06cod = '£6'
022900070424     c                   movel     w1cfil        d06key
023000070424     c                   eval      d06tla = 'L'
023100070424     c                   eval      kpjbu = trul06ds
023200070424     c                   call      'TRUL06R'
023300070424     c                   parm                    kpjba
023400070424     c                   eval      trul06ds = kpjbu
023500070424
023600070424      * visualizzo le linee di arrivo da abilitare
023700070424     c                   exsr      sr_pulsfl
023800070424     c                   exsr      sr_carsfl
023900070424     c                   exsr      sr_emisfl
024000070424
024100070424     c                   endsr
024200070424
024201160905     c**********************************************************************
024202160905     C** ricerca  1 VIDEATA
024203160905     c**********************************************************************
024204160905     C     ric01         BEGSR
024207160905     C*CODICE FILIALE
024208160905     C                   if        §fld = 'W1CFIL'
024209160905     C                   MOVEL     *BLANKS       §COD1
024210160905     C                   MOVE      *BLANKS       §DES1
024211160905     C                   CALL      'TNSD24R'
024212160905     C                   PARM                    §COD1             3
024213160905     C                   PARM                    §TIP              1
024214160905     C                   PARM                    §DES1            25
024215160905     C                   PARM                    §pra              3
024216160905     C                   MOVEL     §COD1         W1CFIL                         *CODICE
024217160905     c                   end
024218160905     c                   endsr
024300070424      *------------------------------------------------------------------------*
024400070424      * controllo la prima videata
024500070424      *------------------------------------------------------------------------*
024600070424     c     sr_ctrd01     begsr
024700070424
024800070424      * la filiale inserita deve essere una filiale valida
024900070424     c                   clear                   w1dfil
025000160905     c                   clear                   og150
025001160905      * se variata la filiale riemetto la videata
025002160905     c                   if        w1cfil <> sav_w1cfil
025004160905     c                   exsr      sr_carvideo1
025005160905     c                   endif
025100070424     c     w1cfil        chain(n)  azorg01l
025200070424     c                   if        not %found(azorg01l) or orgfva <> *blanks or
025300070424     c                             (orgfag <> 'A' and orgfag <> 'F')
025400070424     c                   eval      w1cmsg = 'Filiale errata'
025500070424     c                   eval      *in28 = *on
025600070424     c                   eval      *in40 = *on
025700070424     c                   leavesr
025800070424     c                   endif
025900070502
026000070502     c                   eval      w1dfil= orgdes
026100160905     c                   eval      og150 = orgdf0
026101160905     c                   if        §ogturno = 'S'
026102160905     c                   eval      w1tur = §ogturno
026103160905     c                   end
026104160905     c                   if        §ogimaa  = 'S'
026105160905     c                   eval      w1ima = §ogimaa
026106160905     c                   end
026200070502
026300070502      * non deve essere una filiale gestita da altra filiale
026400080118     c                   eval      w£6 = *off
026500070502     c                   clear                   fnlv55ds
026600070502     c                   movel     '6'           d55tpt
026700070502     c                   z-add     w1cfil        d55lin
026800070525     c                   z-add     *date         d55drf
026900070502     c                   call      'FNLV55R'
027000070502     c                   parm                    fnlv55ds
027100080118     c                   if        d55lin <> d55tfa
027200080118     c                   if        not wforza
027300080118     c                   eval      w1cmsg = 'Attenzione!! Filiale gestita ' +
027400080118     c                             'Enter forza!'
027500070502     c                   eval      *in28 = *on
027600070502     c                   eval      *in40 = *on
027700080118     c                   eval      wforza = *on
027800070502     c                   leavesr
027900080118     c                   endif
028000080118     c                   eval      w£6 = *on
028100070502     c                   endif
028601160905      * controllo se filiale già attivata (organigramma)
028602160905     c                   if        (§ogturno= 'S' and
028603160905     c                             §ogimaa = 'S' )
028605160905     c                   eval      w1cmsg = 'Filiale già attivata'
028606160905     c                   eval      *in28 = *on
028607160905     c                   eval      *in40 = *on
028608160905     c                   leavesr
028609160905     c                   endif
028610160905     c   kf              if        (w1tur= *blank and w1ima = *blank)
028612160905     c                   eval      w1cmsg = 'Immettere una scelta'
028613160905     c                   eval      *in28 = *on
028614160905     c                   eval      *in40 = *on
028615160905     c                   leavesr
028616160905     c                   endif
041200070424
041300070502      * no utente EDP
041400070502     c                   if        not *in02
041500070424      * controllo la password
041600070424     c                   if        w1cpsw = *blanks
041700070424     c                   eval      w1cmsg = 'Immettere la password'
041800070424     c                   eval      *in28 = *on
041900070502     c                   eval      h1riga = 11
042000070525     c                   eval      h1colo = 11
042100070424     c                   leavesr
042200070424     c                   endif
042300070424      * controllo se esatta
042400070424     c                   if        w1cpsw <> password
042500070424     c                   clear                   w1cpsw
042600070424     c                   eval      w1cmsg = 'Password errata'
042700070424     c                   eval      *in28 = *on
042800070502     c                   eval      h1riga = 11
042900070525     c                   eval      h1colo = 11
043000070424     c                   leavesr
043100070424     c                   endif
043200070502     c                   endif
043300070424
043400070424     c                   endsr
043500070424
043600070308      *------------------------------------------------------------------------*
043700070308      * pulizia del subfile
043800070308      *------------------------------------------------------------------------*
043900070308     c     sr_pulsfl     begsr
044000070308
044100070323     c                   clear                   nrr1
044200070308     c                   eval      *in20 = *off
044300070308     c                   eval      *in21 = *off
044400070323     c                   write     dg98c01
044500070308     c                   eval      *in21 = *on
044600070308     c                   eval      *in20 = *on
044700070308
044800070323     c                   eval      recsf1 = 1
044900070308     c                   eval      *in31 = *off
045000070308
045100070308     c                   endsr
045200070308
045300070308      *------------------------------------------------------------------------*
045400070308      * carico subfile con le filiali
045500070308      *------------------------------------------------------------------------*
045600070308     c     sr_carsfl     begsr
045700070308
045800070308      * leggo la schiera delle filiali
045900070308     c                   do        30            xx
046000070308     c                   if        lin(xx) = *blanks or lin(xx) = *zeros
046100070308     c                   iter
046200070308     c                   endif
046300070424     c                   move      lin(xx)       w0030
046400070424     c     w0030         chain(n)  azorg01l
046500070424     c                   eval      w1sfil = w0030
046600070424     c                   eval      w1sdfil = orgdes
046700070424     c                   eval      w1ssce = '1'
046800070323     c                   eval      nrr1 = xx
046900070323     c                   write     dg98s01
047000070308     c                   enddo
047100070308
047200070308     c                   eval      *in31 = *on
048400070308
048500070308     c                   endsr
048600070308
048700070308      *------------------------------------------------------------------------*
048800070308      * emetto il subfile
048900070308      *------------------------------------------------------------------------*
049000070308     c     sr_emisfl     begsr
049100070308
049200070308     c                   do        *hival
049300070308
049400070323     c                   if        recsf1 > nrr1
049500070323     c                   eval      recsf1 = 1
049600070308     c                   endif
049700070308
049800070323     c                   write     dg98z01
049900070323     c                   exfmt     dg98c01
050000070424
050100070308     c                   eval      *in28 = *off
050200070323     c                   clear                   w1cmsg
050300070308
050400070308      * f3=fine
050500070308     c                   if        *inkc
050600070308     c                   goto      fine
050700070308     c                   endif
050800070308
050900070308      * f6=conferma
051000070308     c                   if        *inkf
051100070308     c                   exsr      sr_contrsfl
051200070308     c                   leave
051300070308     c                   endif
051400070308
051500070308     c                   enddo
051600070308
051700070308     c                   endsr
051800070308
051900070308      *------------------------------------------------------------------------*
052000070308      *  controllo i dati del subfile
052100070308      *------------------------------------------------------------------------*
052200070308     c     sr_contrsfl   begsr
052300070308
052400070508     c                   eval      wflag = *off
052500070323     c                   clear                   nrr1
052600070308     c                   do        *hival
052700070323     c                   add       1             nrr1
052800070323     c     nrr1          chain     dg98s01                            32
052900070308      * esco se fine sfl
053000070308     c                   if        *in32
053100070308     c                   leave
053200070308     c                   endif
053201160905      * scelta = 1
053202160905     c                   if        w1ssce = '1'
053203160905      *            imposto i flag per la filiale richiesta in prima videata
053204160905     c                   if        wflag = *off
053205160905     c                   exsr      sr_flag
053206160905     c                   eval      wflag = *on
053207160905     c                   endif
053300070502
055500070323     c                   eval      recsf1 = nrr1
055600070424     c                   clear                   w1ssce
055601160905     c                   endif
055800070323     c                   update    dg98s01
055900070308     c                   enddo
056000070308
056100070308     c                   endsr
056200070424
056201160906      *-----------------------------------------------------***********
056202160906     C     TSTOBJ        BEGSR
056203160906      *-----------------------------------------------------***********
056204160906     C* Richiamo TIBS55R per conoscere librerie/S.Informativi
056205160906     C* da/a cui spedire
056206160906     C                   CLEAR                   TIBS55ds
056207160906     C                   MOVEL     'L'           I50TLA
056208160906     C                   MOVEL     '046'         I50APO
056209160906     C                   CALL      'TIBS55R'
056210160906     C                   PARM                    TIBS55ds
056211160907     c                   eval      azorgs = %trim(O50ALG) + '/AZORG01L'
056212160906     C                   CLEAR                   TIBS55ds
056213160906     C                   MOVEL     'L'           I50TLA
056214160906     C                   MOVEL     '001'         I50APO
056215160906     C                   CALL      'TIBS55R'
056216160906     C                   PARM                    TIBS55ds
056217160907     c                   eval      azorgf = %trim(O50ALG) +'/AZORG01L'
056218160906     c                   endsr
056300070424      *------------------------------------------------------------------------*
056400070508      *  imposta flag in organigramma
056500070424      *------------------------------------------------------------------------*
056600070424     c     sr_flag       begsr
056700070424
056800070424      * chiudo il file aperto
056900070424     c                   if        %open(azorg01l)
057000070424     c                   close     azorg01l
057100070424     c                   endif
057200070424
057300070424      * ovrdbf azorg01l di filiale
057400070424     c                   clear                   command
057600160906     c                   eval      command ='OVRDBF FILE(AZORG01L) +
057601160907     c                             TOFILE(' + %trim(azorgf) + ')'
057700070424     c                   call      'QCMDEXC'
057800070424     c                   parm                    command
057900070424     c                   parm                    lung
058000070424     c                   open      azorg01l
058100070424
058200070424      * imposto subito il flag in organigramma di filiale
058300160905     c                   clear                   og150
058400070508     c     w1cfil        chain     azorg01l
058500070424     c                   if        %found(azorg01l)
058600160905     c                   eval      og150 = orgdf0
058800160905     c                   if        w1ima   = 'S'
058900160905     c                   eval      §ogimaa = 'S'
059000070525     c                   endif
059200160905     c                   if        w1tur   = 'S'
059300160905     c                   eval      §ogturno= 'S'
059400070502     c                   endif
059900160905     c                   eval      orgdf0 = og150
060000070424     c                   update    azorg
060100070424     c                   endif
060200070424
060300070424      * chiudo il file aperto
060400070424     c                   if        %open(azorg01l)
060500070424     c                   close     azorg01l
060600070424     c                   endif
060700070424
060800070424      * ovrdbf azorg01l di sede
060900070424     c                   clear                   command
060901160906     c                   eval      command ='OVRDBF FILE(AZORG01L) +
060902160907     c                             TOFILE(' + %trim(azorgs) + ')'
061200070424     c                   call      'QCMDEXC'
061300070424     c                   parm                    command
061400070424     c                   parm                    lung
061500070424     c                   open      azorg01l
061600070424
061700070424      * imposto subito il flag in organigramma di filiale
061800160905     c                   clear                   og150
061900070508     c     w1cfil        chain     azorg01l
062000070424     c                   if        %found(azorg01l)
062100160905     c                   eval      og150 = orgdf0
062300160905     c                   if        w1tur   = 'S'
062400160905     c                   eval      §ogturno= 'S'
062500070525     c                   endif
062700160905     c                   if        w1ima   = 'S'
062800160905     c                   eval      §ogimaa = 'S'
062900070502     c                   endif
063400160905     c                   eval      orgdf0 = og150
063500070424     c                   update    azorg
063600070424     c                   endif
063700070424
063800070424     c                   endsr
127500070308
127600070308      *------------------------------------------------------------------------*
127700070308      * routine iniziale
127800070308      *------------------------------------------------------------------------*
127900070308     c     *inzsr        begsr
128000070308
128100070308     c     *entry        plist
128200070424     c                   parm                    kpjba
128300070424
128400070424      * ambiente di prova
128501160906     c                   exsr      tstobj
128502160906     c                   open      azorg01l
128600070502
128700070502      * utente EDP
128800070502     c                   eval      *in02 = (%subst(knmus:1:3) = 'EDP')
128900070424
129000070424     c     *dtaara       define    §azute        azuteds
129100070424     c     *dtaara       define    §datiute      ddatiute
129200070424     c                   in(e)     *dtaara
129300070424     c                   if        %error or rsut = *blanks
129400070424     c                   clear                   tibs34ds
129500070424     c                   call      'TIBS34R'
129600070424     c                   parm                    tibs34ds
129700070424     c                   in        *dtaara
129800070424     c                   endif
129900070528
130000070528      * imposto la filiale utente a video
130100070528     c                   eval      w1cfil = dutpou
130200070528     c                   eval      w1dfil = dutdpo
135400070424
135500070308     c                   endsr
