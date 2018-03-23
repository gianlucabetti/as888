000100090109     H DEBUG DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP(*CALLER)
000200121130     H BNDDIR('TIBS') EXTBININT(*YES)
000300000000     H*--------------------------------------------------------------*
000400090330     Ffitgt02l  uf   e           k disk
000500090407     Ffifgt01l  uf   e           k disk
000600090330     Ffiadt02l  uf   e           k disk
000700090511     Ffiadd01l  if   e           k disk
000800090330     Faitrs01l  uf   e           k disk
000900090330     Faitra03l  if   e           k disk
001000090427     Fansog01l  IF   E           K DISK
001100090427     Fanfrn01l  IF   E           K DISK
001200090525     Ffiapd01l  IF   E           K DISK
001300090330     Faitra05l  if   e           k disk    rename(aitra000:aitra5)
001400130116     FTNtbe01l  if   e           k disk
001500090430     Fprtf198   O    F  198        PRINTER prtctl(prtds) usropn
001600090331      * ?___________________________________________________________________
001700090331     D/COPY GAITRASRC/SRCPROTOPR,TRMZ0100R
001800090331     D prmRpyOpCode...
001900090331     D                 S             10A
002000090331     D prmRpyIdMsg...
002100090331     D                 S             10I 0
002200121130ab   ** Prototipi.
002300121130ab    **
002400090331      * ?___________________________________________________________________
002500020304     dkpjba          e ds
002600090408     D trmz0100i     E DS                  prefix(x)
002700090408     D trmz70s1ds    E DS
002800090429     D aitrs00ds     E DS                  extname(aitrs00f)
002900090429     D aitra00ds     E DS                  extname(aitra00f)
003000120606     D DSaiats       E DS                  EXTNAME(AIATS00F)
003100111205     D trul13ds      E DS
003200111205     D dapdflr       E DS
003300111205     D dtgtflr       E DS
003400111205     D dtrsflr       E DS
003500130111     D dtrsflr1      E DS
003600130116     D TNTBZ79DS     E DS
003700120222
003800120222      *
003900090330     d prtds           ds
004000090330     d   spab                         3  0
004100090330     d   spaa                         3  0
004200090330     d   skab                         3  0
004300090330     d   skaa                         3  0
004400090330     d   line                         3  0
004500090408     d paramI          ds
004600090424     d   parfilI                      3  0
004700090428     d   pardatI                      8  0
004800090428     d   paroutqI                    10
004900090407     d param           ds
005000090407     d   partip                       1
005100090407     d   parpdrda                     7  0
005200090407     d   parpdra                      7  0
005300090407     d   pardat                       8  0
005400090424     d   pardtcert                    8  0
005500090428     d   parnrc                       7s 0
005600090428     d   parfil                       3s 0
005700090428     D   paroutq                     10
005800090427     d   parerr                       1
005900090526     d   parmsg                      35
006000121130     D   pgmChiamante                10
006100111206     d   pardfa                       8  0
006200121130     d   parsocprt                    3
006300090407     d paramdef        ds
006400090407     d   parpdrd                      7  0
006500090407     d   parprgd                      3  0
006600090407     d   partipd                      1
006700090424     d   pardtcertd                   8  0
006800090428     d   parnrcd                      7s 0
006900090428     d   parfild                      3s 0
007000090428     D   paroutqd                    10
007100090427     d   parerrd                      1
007200111206     d   pardfad                      8  0
007300121130     d   parsocprtd                   3
007400120606     D partraz         DS
007500120606     D  parsoc                        3s 0
007600120606     D  pariva                       16
007700120606     D  pardtin                       8s 0
007800120606     D  pardtfi                       8s 0
007900120606     d  esito                         1
008000120606     d  errmsgt                      75
008100090331      * ?___________________________________________________________________
008200090429     d dataiso0        s               d   datfmt(*iso)
008300020305     d dataiso         s               d   datfmt(*iso)
008400020305     d dataeur         s               d   datfmt(*eur)
008500090331     d data0           s              8  0
008600090525     d kpdr            s                   like(adtpdr)
008700090525     d autistas        s                   like(traaut)
008800130117     d oggi            s              8  0
008900090331     d wdatg           s              8  0
009000090331     d wdat            s              8  0
009100090408     d*
009200090401     d No_allegato     s              1
009300121130     d filiale         s              3  0
009400090330     d TipoAut         s              1    inz('C')
009500090429     d segnala         s            165
009600120606     d segnalaT        s            120
009700090429     d nota            s             33
009800090430     D CMD             S             80    DIM(1) CTDATA PERRCD(1)              OVRPRTF
009900000000     C*---------------------------------------------------------------*
010000090330      * ?______________  main   ____________________________________________
010100090330     c* lettura allegati tariffari da stampare autisti città
010200090330     c     *loval        setll     fitgt02l
010300090109     c                   do        *hival
010400090330     c                   read      fitgt02l
010500090330     c                   if        %eof(fitgt02l)
010600090109     c                   leave
010700090109     c                   end
010800090408      * se impostata scelta singola filiale scarto le tariffe delle altre
010900090408     c                   movel     tgtpdr        pdr3              3 0
011000130116     c                   z-add     pdr3          Filiale           3 0
011100090428     c                   if        parfilI > 0 and pdr3 > parfilI
011200090428     c                   leave
011300090428     c                   end
011400090424     c                   if        parfilI > 0 and pdr3 <> parfilI
011500090408     c                   iter
011600090408     c                   end
011700090519      *non considero le tariffe con decorrenza scadenza al 01/05/2009
011800090519     c                   if        tgtddt = 20090501 and tgtdst = 20090501
011900090519     c                   iter
012000090519     c                   end
012100090617      *non considero le tariffe con decorrenza maggiore della data certa
012200120607     c*                  if        tgtddt > wdatg
012300091015     c*                  iter
012400091015     c*                  end
012500130116      ****
012600130116      *   controlla se c'è in corso un cambio societario
012700130116      *     e quindi
012800130116      *   sulla tabella "Z79" esiste un record x la filiale
012900130116     C                   move      'Z79'         tbeCOD
013000130116     C                   MOVEl(p)  Filiale       tbeKE1
013100130116     c     ktbe1         chain     tntbe01l
013200130116     c                   if        %FOUND(tntbe01l)
013300130116     c                   movel     tbeuni        TNTBZ79DS
013400130116     c                   move      z79daDATA     DataTrasco        8 0
013500130116      **  se la data decorrenza tariffa è successiva alla data di inizio validità
013600130116      *    della nuova società e non si è ancora trascodificato ossia siamo
013700130116      *     temporalemente prima della data di Trascodifica,
013800130116      *      si deve bloccare la tariffa.
013900130117     c                   if        DataTrasco >= oggi
014000130116     c                                and
014100130116     c                             DataTrasco <= tgtddt
014200130116      * salta la tariffa
014300130116     c                   iter
014400130116     c                   end
014500130116     c                   endif
014600130116      ****
014700090519      *
014800090430     c                   clear                   no_allegato
014900090330     c                   z-add     tgtpdr        kpdr
015000090429     c                   clear                   aitrs00ds
015100090429     c                   clear                   aitra00ds
015200090506      * a rottura di filiale ristampo la testata del report anomalie
015300090519     c                   if        line > 62
015400090506     c                   except    testata
015500090506     c                   endif
015600090330     c                   exsr      trovata
015700090325     c                   enddo
015800090112     c*
015900090330     c* lettura allegati tariffari da stampare autisti AFF/DEF
016000090508     c                   movel     'A'           TipoAut
016100090330     c     *loval        setll     fiadt02l
016200090330     c                   do        *hival
016300090330     c                   read      fiadt02l
016400090330     c                   if        %eof(fiadt02l)
016500090330     c                   leave
016600090330     c                   end
016700090408      * se impostata scelta singola filiale scarto le tariffe delle altre
016800090408     c                   movel     adtpdr        pdr3
016900130116     c                   z-add     pdr3          Filiale
017000090428     c                   if        parfilI > 0 and pdr3 > parfilI
017100090428     c                   leave
017200090428     c                   end
017300090424     c                   if        parfilI > 0 and pdr3 <> parfilI
017400090408     c                   iter
017500090408     c                   end
017600090519      *non considero le tariffe con decorrenza scadenza al 01/05/2009
017700090519     c                   if        adtddt = 20090501 and adtdst = 20090501
017800090519     c                   iter
017900090519     c                   end
018000090617      *non considero le tariffe con decorrenza maggiore della data certa
018100091015     c*                  if        adtddt > wdatg
018200091015     c*                  iter
018300091015     c*                  end
018400130116      ****
018500130116      *   controlla se c'è in corso un cambio societario
018600130116      *     e quindi
018700130116      *   sulla tabella "Z79" esiste un record x la filiale
018800130116     C                   move      'Z79'         tbeCOD
018900130116     C                   MOVEl(p)  Filiale       tbeKE1
019000130116     c     ktbe1         chain     tntbe01l
019100130116     c                   if        %FOUND(tntbe01l)
019200130116     c                   movel     tbeuni        TNTBZ79DS
019300130116     c                   move      z79daDATA     DataTrasco
019400130116      **  se la data decorrenza tariffa è successiva alla data di inizio validità
019500130116      *    della nuova società e non si è ancora trascodificato ossia siamo
019600130116      *     temporalemente prima della data di Trascodifica,
019700130116      *      si deve bloccare la tariffa.
019800130117     c                   if        DataTrasco >= oggi
019900130116     c                                and
020000130116     c                             DataTrasco <= adtddt
020100130116      * salta la tariffa
020200130116     c                   iter
020300130116     c                   end
020400130116     c                   endif
020500130116      ****
020600090519      *
020700090430     c                   clear                   no_allegato
020800090330     c                   z-add     adtpdr        kpdr
020900090429     c                   clear                   aitrs00ds
021000090429     c                   clear                   aitra00ds
021100090506      * a rottura di filiale ristampo la testata del report anomalie
021200090519     c                   if        line > 62
021300090506     c                   except    testata
021400090506     c                   endif
021500090330     c                   exsr      trovatab
021600090330     c                   enddo
021700090330     c*
021800090401      /FREE
021900090401       trmz0100r( 'FINALIZE'
022000090401                : prmRpyOpCode
022100090401                : prmRpyIdMsg
022200090401                );
022300090401
022400090401      /END-FREE
022500090330     c                   seton                                        lr
022600090330      * ?___________________________________________________________________
022700090330     c     trovata       begsr
022800090330      * ?___________________________________________________________________
022900090330     c* verifico se l'autista è già stato inserito nell'anagrafica autisti automezzi
023000090330      * se esiste passo alle verifiche sulla società
023100090330      * altrimenti segnalo in stampa l'anomalia
023200090330     c     kaitra        chain     aitra03l
023300090330     c                   if        %found(aitra03l)
023400090427     c                   if        tradin > tgtddt
023500120322     c     kpdr          setgt     aitra03l
023600120322     c     kpdr          readpe    aitra03l
023700120322     c                   if        not %eof(aitra03l)
023800120322     c                   if        tradin > tgtddt
023900090429     c                   exsr      componi
024000090429     c                   eval      nota ='Dt. accreditamento > decor.tariffa'
024100090424     c                   except    AccrDeco
024200090430     c                   leavesr
024300120322     c                   else
024400120322     c                   if        tradfi > tgtddt
024500120322      *imposta dati fornitore della tariffa per confronto partita iva
024600120322     c                   move      tgtsoc        socfor
024700120322     c                   move      tgtcdf        codfor
024800120322     c                   exsr      VerSocieta
024900120322     c                   leavesr
025000120322     c                   endif
025100090424     c                   endif
025200120322     c                   endif
025300120322     c                   endif
025400090427      *imposta dati fornitore della tariffa per confronto partita iva
025500090427     c                   move      tgtsoc        socfor
025600090427     c                   move      tgtcdf        codfor
025700090427      *
025800090330     c                   exsr      VerSocieta
025900090330     c                   else
026000090424     c     kpdr          setgt     aitra03l
026100090424     c     kpdr          readpe    aitra03l
026200090428     c                   if        not %eof(aitra03l)
026300090528      * se la data disaccreditamento è maggiore della decorrenza tariffa stampo ugualmente
026400090528     c                   if        tradfi > tgtddt
026500090529      *imposta dati fornitore della tariffa per confronto partita iva
026600090529     c                   move      tgtsoc        socfor
026700090529     c                   move      tgtcdf        codfor
026800090528     c                   exsr      VerSocieta
026900090528     c                   leavesr
027000090528     c                   endif
027100090429     c                   exsr      componi
027200090429     c                   eval      nota ='Disaccr. in ANAGRAFICA AUTOMEZZI'
027300090330     c                   except    NoAut
027400090430     c                   leavesr
027500090427     c                   else
027600090429     c                   exsr      componi
027700090429     c                   eval      nota ='Non ancora inserito in ANAG.Autom.'
027800090427     c                   except    NoAut
027900090430     c                   leavesr
028000090330     c                   endif
028100090424     c                   endif
028200130116      ****
028300090330     c                   endsr
028400090330      * ?___________________________________________________________________
028500090330     c     trovatab      begsr
028600090330      * ?___________________________________________________________________
028700090511     c                   clear                   addettagli
028800090511
028900090511     c     kadd          setll     fiadd01l
029000090511     c                   do        *hival
029100090511     c     kadd          reade     fiadd01l
029200090511     c                   if        %eof(fiadd01l)
029300090511     c                   leave
029400090511     c                   endif
029500090511     c                   if        addatb = *blank
029600090511     c                   move      'X'           addettagli        1
029700090511     c                   leave
029800090511     c                   endif
029900090511     c                   enddo
030000120222      **
030100090511     c                   if        addettagli = *blank
030200090511     c                   exsr      componia
030300090511     c                   eval      nota ='Non presente dettaglio tariffario'
030400090511     c                   except    NoDetTar
030500090511     c                   leavesr
030600090511     c                   endif
030700120222      **
030800120222      **  Controlla se trazionista:
030900120606     c                   exsr      veriftraz
031000120717     c                   if        esito  <> ' '
031100120606     c                   if        esito  = '1'
031200120606     c                   eval      trsfil = 046
031300120606     c                   eval      trsnrc = atsnrc
031400120222     c                   exsr      AllegatoAd
031500120606     c                   else
031600120611     c                   if        atsdec = 0
031700120611     c                   move      dataiso0      atsdecs           8 0
031800120611     c                   else
031900120611     c                   z-add     atsdec        atsdecs
032000120611     c                   endif
032100120611     c                   if        atsdfc = 0
032200120611     c                   move      dataiso0      atsdfcs           8 0
032300120611     c                   else
032400120611     c                   z-add     atsdfc        atsdfcs
032500120611     c                   endif
032600120606     c                   eval      segnalaT= 'T' + ' ' +
032700120606     c                              %editc(kpdr:'Z') +' Dec. ' +
032800120606     c                              %char(%date(adtddt):*eur) + ' Scad. ' +
032900120606     c                              %char(%date(adtdst):*eur) + ' Stampa ' +
033000120606     c                              %char(%date(adtdts):*eur) + ' Dec.c. ' +
033100120611     c                              %char(%date(atsdecs):*eur) + ' Scad.c ' +
033200120611     c                              %char(%date(atsdfcs):*eur) + ' ' +
033300120606     c                             ' ' + adtcsf + ' ' +  adtcdf + ' ' +
033400120606     c                             ' ' + atspiva + ' '+%editc(atsnrc:'Z')
033500120606     c                   except    TrazErrA
033600120606     c                   leavesr
033700120606     c                   endif
033800120222      **
033900120222     c                   eLSe
034000120222      *
034100090330     c* verifico se l'autista è già stato inserito nell'anagrafica autisti automezzi
034200090330      * se esiste passo alle verifiche sulla società
034300090330      * altrimenti segnalo in stampa l'anomalia
034400090330     c     kaitra        chain     aitra05l
034500090330     c                   if        %found(aitra05l)
034600090427     c                   if        tradin > adtddt
034700120322     c     kpdr          setgt     aitra05l
034800120322     c     kpdr          readpe    aitra05l
034900120322     c                   if        not %eof(aitra05l)
035000120322     c                   if        tradin > adtddt
035100090429     c                   exsr      componia
035200090429     c                   eval      nota ='Dt. accreditamento > decor.tariffa'
035300090427     c                   except    AccrDecoA
035400090430     c                   leavesr
035500120322     c                   else
035600120322     c                   if        tradfi > tgtddt
035700120322      *imposta dati fornitore della tariffa per confronto partita iva
035800120322     c                   move      tgtsoc        socfor
035900120322     c                   move      tgtcdf        codfor
036000120322     c                   exsr      VerSocieta
036100120322     c                   leavesr
036200120322     c                   endif
036300090427     c                   endif
036400120322     c                   endif
036500120322     c                   endif
036600090427     c                   move      adtcsf        socfor
036700090427     c                   move      adtcdf        codfor
036800090330     c                   exsr      VerSocieta
036900090330     c                   else
037000090427     c     kpdr          setgt     aitra05l
037100090427     c     kpdr          readpe    aitra05l
037200090511     c                   if        not %eof(aitra05l)
037300090528      * se la data disaccreditamento è maggiore della decorrenza tariffa stampo ugualmente
037400090528     c                   if        tradfi > adtddt
037500090529     c                   move      adtcsf        socfor
037600090529     c                   move      adtcdf        codfor
037700090528     c                   exsr      VerSocieta
037800090528     c                   leavesr
037900090528     c                   endif
038000090429     c                   exsr      componia
038100090429     c                   eval      nota ='Disaccr. in ANAGRAFICA AUTOMEZZI'
038200090427     c                   except    NoAutA
038300090430     c                   leavesr
038400090427     c                   else
038500090429     c                   exsr      componia
038600090429     c                   eval      nota ='Non ancora inserito in ANAG.Autom'
038700090427     c                   except    NoAutA
038800090430     c                   leavesr
038900090427     c                   endif
039000090330     c                   endif
039100120222      **  <CODICE>
039200120222     c                   eND
039300120222      *
039400090330     c                   endsr
039500120606     C**************************************************************************
039600120606     C*    CONTROLLo per a/d se il fornitore è un trazionista certificato
039700120606     C**************************************************************************
039800120606     C     veriftraz     BEGSR
039900120606     c     kfrn          klist
040000120606     C                   kfld                    adtcsf
040100120606     C                   kfld                    adtcdf
040200120607     c                   clear                   partraz
040300120606     c                   clear                   sogpartiva
040400120606     c     kfrn          chain     anfrn01l
040500120606     c                   if        %found(anfrn01l)
040600120606     c     frnsogg       chain     ansog01l
040700120606     c                   if        %found(ansog01l)
040800120606     c                   move      adtcsf        parsoc
040900120607     c                   move      adtddt        pardtin
041000120607     c                   move      adtdst        pardtfi
041100120606     c                   movel     sogpartiva    pariva
041200120606     c                   call      'TRULTRAZ'
041300120606     c                   parm                    partraz
041400120606     c                   parm                    dsAIATS
041500120606     c                   endif
041600120606     c                   endif
041700120606     c                   endsr
041800090330      * ?___________________________________________________________________
041900090330     c     VerSocieta    begsr
042000090330      * ?___________________________________________________________________
042100090401     c                   clear                   No_allegato
042200090427     c                   clear                   IvaForn          16
042300090427     c                   exsr      RepIvaFor
042400090330     c* verifico la società che sia stata inserita che sia ancora valida e lo stato
042500090330      * del contratto
042600090330      * se mancano i prerequisiti per la stampa dell'allegato li segnalo
042700090330     c     tranrc        chain     aitrs01l
042800090330     c                   if        %found(aitrs01l)
042900090330
043000090330     c                   select
043100110321      * Società accreditamento autista diversa da società allegato tariffario
043200110321     c                   when      trssocg <> socfor
043300110321     c                   if        tipoaut = 'C'
043400110321     c                   exsr      componi
043500110321     c                   else
043600110321     c                   exsr      componia
043700110321     c                   endif
043800110321     c                   eval      nota ='Soc. Acc.'+ trssocg +
043900110321     c                             ' diversa da Tar. '+ socfor
044000110321     c                   except    SocNoCor
044100110321     c                   leavesr
044200110321
044300090427      * Partita iva società diversa da quella del fornitore in tariffa
044400090427     c                   when      trsiva <> IvaForn
044500090428     c                   if        tipoaut = 'C'
044600090429     c                   exsr      componi
044700090428     c                   else
044800090429     c                   exsr      componia
044900090428     c                   endif
045000090429     c                   eval      nota ='P.Iva Società <> da P.Iva fornitore'
045100090427     c                   except    PivaErrore
045200090430     c                   leavesr
045300090427
045400090330      * società con contratto scaduto o annullata
045500090604     c                   when      trsdfc > 0  or trsann <> *blank
045600090604     c                   if        tipoaut = 'C'
045700090604     c                   if        tgtddt > trsdfc or trsann <> *blank
045800090429     c                   exsr      componi
045900090604     c                   eval      nota ='Società con contratto scaduto '
046000090604     c                   except    SocContSCA
046100090604     c                   leavesr
046200090623     c                   else
046300090623     c                   exsr      AllegatoCit
046400090604     c                   endif
046500090428     c                   else
046600090604     c                   if        adtddt > trsdfc or trsann <> *blank
046700090429     c                   exsr      componia
046800090604     c                   eval      nota ='Società con contratto scaduto '
046900090604     c                   except    SocContSCA
047000090604     c                   leavesr
047100090623     c                   else
047200090623     c                   exsr      AllegatoAd
047300090604     c                   endif
047400090428     c                   endif
047500090330
047600090427      * società con contratto con data inferiore al minimo ammesso
047700090407     c                   when      trsdec < 20090501
047800090428     c                   if        tipoaut = 'C'
047900090429     c                   exsr      componi
048000090428     c                   else
048100090429     c                   exsr      componia
048200090428     c                   endif
048300090429     c                   eval      nota = 'Contratto antecedente data minima'
048400090407     c                   except    SocContAnt
048500090430     c                   leavesr
048600090407
048700090427      * Autista di città con tariffa antecedente decorrenza contratto
048800090427     c                   when      Tipoaut = 'C' and trsdec > tgtddt
048900090429     c                   exsr      componi
049000090429     c                   eval      nota =
049100090429     c                             'tariffa antecedente decorrenza contratto'
049200090427     c                   except    TarAntCon
049300090430     c                   leavesr
049400090427
049500090427      * Autista Aff/Def con tariffa antecedente decorrenza contratto
049600090508     c                   when      Tipoaut = 'A' and trsdec > adtddt
049700090429     c                   exsr      componia
049800090429     c                   eval      nota=
049900090429     c                             'tariffa antecedente decorrenza contratto'
050000090427     c                   except    TarAntConA
050100090430     c                   leavesr
050200090427
050300130116      *
050400130116      * società con contratto da stampare
050500090401     c                   when      trsdsc = 0
050600130116      *
050700130116     c                   z-add     trsFIL        Filiale
050800130116      ****
050900130116      *   controlla se c'è in corso un cambio societario
051000130116      *     e quindi
051100130116      *   sulla tabella "Z79" esiste un record x la filiale
051200130116     C                   move      'Z79'         tbeCOD
051300130116     C                   MOVEl(p)  Filiale       tbeKE1
051400130116     c     ktbe1         chain     tntbe01l
051500130116     c                   if        %FOUND(tntbe01l)
051600130116     c                   movel     tbeuni        TNTBZ79DS
051700130116     c                   move      z79daDATA     DataTrasco
051800130116      **  se la data decorrenza tariffa è successiva alla data di inizio validità
051900130116      *    della nuova società e non si è ancora trascodificato ossia siamo
052000130116      *     temporalemente prima della data di Trascodifica,
052100130116      *      si deve bloccare la tariffa.
052200130117     c                   if        DataTrasco >= oggi
052300130116     c                                and
052400130116     c                             DataTrasco <= trsdec
052500130116      * salta la stampa
052600130116     c                   goto      In_trascodific
052700130116     c                   end
052800130116     c                   endif
052900130116      ****
053000090623      * ?Attenzione particolarità filiale Venezia bisogna verificare se il tipo mezzo
053100090623      * ?dell'autista è barca nel caso il contratto è gestito a mano dall'ufficio automezzi
053200130116     c                   clear                   contvenezia       1
053300090623     c                   if        Tipoaut = 'C' and pdr3 = 58
053400090623     c                   exsr      Venezia
053500090623     c                   endif
053600090623     c                   if        contvenezia = *blank
053700090330     c                   exsr      Contratto
053800090623     c                   endif
053900130116
054000090401      * problemi nella stampa contratto quindi non stampo allegato tariffario
054100090401     c                   if        No_allegato = *blank
054200090330     c                   if        TipoAut = 'C'
054300090330     c                   exsr      AllegatoCit
054400090330     c                   else
054500090330     c                   exsr      AllegatoAd
054600090330     c                   endif
054700090401     c                   endif
054800130116
054900130116      * se la Filiale è in trascodifica il contratto non è stampabile
055000130116      *  se la decorrenza è successiva alla data di trascodifica
055100130116      *   in un momento antecedente la trascodifica.
055200130116     c     In_trascodificTAG
055300130116
055400090330      * società con contratto stampato stampa solo l'allegato autista
055500090330     c                   other
055600130116      *
055700090430     c                   if        No_allegato = *blank
055800090330     c                   if        TipoAut = 'C'
055900090330     c                   exsr      AllegatoCit
056000090330     c                   else
056100090330     c                   exsr      AllegatoAd
056200090430     c                   endif
056300090330     c                   endif
056400130116      *
056500090330     c                   endsl
056600130116      *
056700090330     c                   else
056800090330      * società non presente
056900090428     c                   if        TipoAut = 'C'
057000090429     c                   exsr      componi
057100090428     c                   else
057200090429     c                   exsr      componia
057300090428     c                   endif
057400090429     c                   eval      nota =
057500090429     c                             'Società non trovata in Anagrafica società'
057600090330     c                   except    NoSoc
057700090330     c                   endif
057800090330     c                   endsr
057900090330      * ?___________________________________________________________________
058000090330     c     Contratto     begsr
058100090330      * ?___________________________________________________________________
058200090330     c* Richiamo PGM per stampa del contratto
058300090330     c                   exsr      RagSocCont
058400090330     c                   if        trovato = *on
058500090330     c                   clear                   prmRpyOpCode
058600090330     c                   clear                   prmRpyIdMsg
058700090330     c                   clear                   trmz0100i
058800090401     c                   move      trsdec        dataiso
058900090408     c                   eval      xCONTRATDAT  = dataiso
059000090408     c                   eval      xCONTRATNUM  = trsnrc
059100090408     c                   eval      xFILIALE     = trsfil
059200090408     c                   eval      xVETRAGSOC   = %trim(ragsocksc) + ' ' +
059300090407     c                                          %trim(ragsocksc1)
059400090408     c                   eval      xVETSLCOM    = %trim(localita)
059500090408     c                   eval      xVETSLIND    = %trim(indirizzo)
059600090408     c                   eval      xVETSLCAP    = %trim(cap)
059700090408     c                   eval      xVETSLPRV    = provincia
059800090408     c                   eval      xVETSLNAZ    = stato
059900090408     c                   eval      xVETCDFISCA  = CodiceFisc
060000090408     c                   eval      xVETCDIVA    = trsiva
060100090408     c                   eval      xVETRIMLUO   = %trim(trscit)
060200090408     c                   eval      xVETRIMNUM   = trsnis
060300090408     c                   eval      xVETLEGRAP   = %trim(trslra)
060400090408     c                   eval      xVETANAPRV   = trspia
060500090408     c                   eval      xVETANANUM   = trsnia
060600090424     c                   move      wdatg         dataiso
060700090424     c                   eval      xdatastampa = dataiso
060800111205     c                   eval      xdatacertap = dataiso
060900130111      *
061000121130      * Reperisce società valida da stampare sul contratto
061100130111      *  direttamente quella sul file.
061200130111     c                   eval      xSOCIETA     = trsSOCG
061300130111      *
061400111206     c* data firma (1° data certa + 1 gg lavorativo)
061500111207     C                   clear                   trul13ds
061600111207     c                   eval      i13fil       = trsfil
061700111206     C                   move      dataiso       i13din
061800111205     C                   EVAL      i13gio = 1
061900111205     C                   CALL      'TRUL13R'
062000111205     C                   PARM                    trul13ds
062100111205     C                   IF        o13err <> 'E'
062200111205     c                   move      o13dfi        dataiso
062300111205     c                   eval      xDATAFIRMAV = dataiso
062400111205     c                   end
062500111205     c
062600090401      /FREE
062700090401
062800090401       trmz0100r( 'PRTCONTRAT'
062900090401                : prmRpyOpCode
063000090401                : prmRpyIdMsg
063100090401                : 'TRMZ0100I'
063200090401                : trmz0100i
063300090401                : %SIZE(trmz0100i)
063400090401                );
063500090401
063600090401       IF prmRpyIdMsg < 0;
063700090401         // Stampa non riuscita.
063800090401       ENDIF;
063900090401
064000090401      /END-FREE
064100120326      **
064200090330      * se non c'è errore aggiorno data stampa contratto
064300090330     c                   if        prmrpyidmsg = 0
064400090330     c                   z-add     wdatg         trsdsc
064500111205     c                   movel     trsflr        dtrsflr
064600111205     c                   move      wdatg         §trsunodc
064700111205     c                   move      o13dfi        §trsdfa
064800111205     c                   eval      trsflr = dtrsflr
064900130111      **
065000130111      * imposta la società stampata
065100130115     c                   movel     trsflr1       dtrsflr1
065200130115     c                   eval      §trsSOCprt = trsSOCG
065300130115     c                   eval      trsflr1    = dtrsflr1
065400130111      **
065500090401     c                   update    aitrs000
065600090330     c                   else
065700090330      * c'è errore stampo la segnalazione
065800090428     c                   if        tipoaut = 'C'
065900090429     c                   exsr      componi
066000090428     c                   else
066100090429     c                   exsr      componia
066200090428     c                   endif
066300090430     c                   eval      nota ='Errore nella stampa del contratto'
066400090330     c                   except    ErrStampa
066500090401     c                   eval      no_allegato = '1'
066600090330     c                   endif
066700090330     c                   else
066800090428     c                   if        tipoaut = 'C'
066900090429     c                   exsr      componi
067000090429     c                   else
067100090429     c                   exsr      componia
067200090428     c                   endif
067300090430     c                   eval      nota ='Non trovato fornitore'
067400090330     c                   except    NoFornit
067500090401     c                   eval      no_allegato = '1'
067600090330     c                   endif
067700090330     c                   endsr
067800090330      * ?___________________________________________________________________
067900090330     c     AllegatoCit   begsr
068000090330      * ?___________________________________________________________________
068100090330     c* Richiamo PGM per stampa allegato autista di città
068200090428     c                   clear                   param
068300090407     c                   z-add     tgtpdr        parpdrda
068400090407     c                   z-add     tgtpdr        parpdra
068500090427     c                   z-add     tgtddt        pardat
068600090407     c                   move      'C'           partip
068700090424     c                   z-add     wdatg         pardtcert
068800090428     c                   z-add     trsfil        parfil
068900090428     c                   z-add     trsnrc        parnrc
069000090428     c                   move      paroutqI      paroutq
069100111206     c                   if        tgtddt < wdatg
069200111206     c                   move      tgtddt        pardfa
069300111206     c                   else
069400111206     c                   move      wdatg         pardfa
069500111206     c                   end
069600130111      *
069700121130      *reperisce società valida nel periodo da stampare
069800130111      *  direttamente quella sul file.
069900130111     c                   eval      parsocprt = tgtSOC
070000130111      *
070100090407     c                   movel     param         kpjbu
070200090428     c                   call      'FICN14R'
070300090330     c                   parm                    kpjba
070400090427     c                   movel     kpjbu         param
070500090427     c                   if        parerr = *blank
070600090406     c                   z-add     trsfil        tgtfil
070700090330     c                   z-add     trsnrc        tgtnrc
070800090330     c                   z-add     wdatg         tgtdcn
070900111205     c                   movel     tgtflr        dtgtflr
071000111205     c                   move      wdatg         §tgtunodc
071100111206     c                   if        tgtddt < wdatg
071200111206     c                   move      tgtddt        §tgtdfa
071300111206     c                   else
071400111206     c                   move      wdatg         §tgtdfa
071500111206     c                   end
071600111205     c                   eval      tgtflr = dtgtflr
071700090401     c                   update    fitgt000
071800090427     c                   else
071900090429     c                   exsr      componi
072000090526     c                   eval      nota = parmsg
072100090427     c                   except    Allegato
072200090427     c                   endif
072300090407      * aggiorna testate tariffe legate alla supertestata che si sta trattando
072400090424      * specifiche disabilitate per ora non sembra debba essere aggiornato da qui FGT
072500090427      * ?ATTENZIONE SE ATTIVATE QUESTE SPECIFICHE DEVONO ESSERE POSTE DOPO UPDATE DI FITGT
072600090424     c                   if        1 = 2
072700090424     c                   move      'G'           ktsr
072800090424     c                   exsr      aggiornafgt
072900090424     c                   move      'C'           ktsr
073000090424     c                   exsr      aggiornafgt
073100090424     c                   move      'R'           ktsr
073200090424     c                   exsr      aggiornafgt
073300090424     c                   endif
073400090424     c*
073500090407     c                   endsr
073600090407      * ?___________________________________________________________________
073700090407     c     aggiornafgt   begsr
073800090407      * ?___________________________________________________________________
073900090406      *aggiorna fgt legati alla supertestata con data conferma
074000090407     c     kfgt          setll     fifgt01l
074100090406     c                   do        *hival
074200090406     c     kfgt          reade     fifgt01l
074300090406     c                   if        %eof(fifgt01l)
074400090406     c                   leave
074500090406     c                   endif
074600090407     c                   if        fgtprg <> tgtprg
074700090407     c                   iter
074800090407     c                   endif
074900090406     c                   z-add     wdatg         fgtdcn
075000090406     c                   update    fifgt000
075100090406     c                   enddo
075200090407     c                   endsr
075300090330      * ?___________________________________________________________________
075400090330     c     AllegatoAd    begsr
075500090330      * ?___________________________________________________________________
075600090330     c* Richiamo PGM per stampa allegato autista di AFF/DEF
075700090428     c                   clear                   paramdef
075800090407     c                   z-add     adtpdr        parpdrd
075900090407     c                   z-add     adtprg        parprgd
076000090407     c                   move      'C'           partipd
076100090424     c                   z-add     wdatg         pardtcertd
076200120606      *fittizzi se trazionista sono forzati da aiats
076300090428     c                   z-add     trsfil        parfild
076400090428     c                   z-add     trsnrc        parnrcd
076500120606      *------
076600090428     c                   move      paroutqI      paroutqd
076700111206     c                   if        adtddt < wdatg
076800111206     c                   z-add     adtddt        pardfad
076900111206     c                   else
077000111206     c                   z-add     wdatg         pardfad
077100111206     c                   end
077200130111      *
077300121130      *reperisce società valida nel periodo da stampare
077400130111      *  direttamente quella sul file.
077500130111     c                   eval      parsocprtd = adtCSF
077600130111      *
077700090427     c                   movel     paramdef      kpjbu
077800090406     c                   call      'FICNA6C'
077900090330     c                   parm                    kpjba
078000090427     c                   movel     kpjbu         paramdef
078100090427     c                   if        parerrd = *blank
078200090406     c                   z-add     trsfil        adtfil
078300090330     c                   z-add     trsnrc        adtnrc
078400090330     c                   z-add     wdatg         adtdcn
078500111205     c                   z-add     wdatg         adtunodc
078600111205     c                   if        adtddt < wdatg
078700111205     c                   z-add     adtddt        adtdfa
078800111205     c                   else
078900111205     c                   z-add     wdatg         adtdfa
079000111205     c                   end
079100090401     c                   update    fiadt000
079200090427     c                   else
079300090429     c                   exsr      componia
079400090512     c                   select
079500090512     c                   when      parerrd = '1'
079600090512     c                   eval      nota ='Testata tariffa annullata '
079700090512     c                   when      parerrd = '2'
079800090512     c                   eval      nota ='Testata tariffa non trovata '
079900090512     c                   when      parerrd = '3'
080000090512     c                   eval      nota ='Anagrafica autista annullata '
080100090512     c                   endsl
080200090427     c                   except    AllegatoD
080300090427     c                   endif
080400090330     c                   endsr
080500090330      * ?___________________________________________________________________
080600090330     C     RagSocCont    BegSR
080700090330      * ?___________________________________________________________________
080800090330      *
080900090408     C                   clear                   Trmz70s1ds                     Output
081000090330     C                   movel(p)  TRSiva        PartitaIVA                     Input
081100090408     C                   movel(p)  'F'           SottoNatur                     Input "F/C"
081200090330     C                   movel(p)  TRSFIL        Unita                          Input/Output
081300090608     C                   move      TrsSOCg       Societa                        Input/Output
081400090526      *asteriscate nella stampa del contratto non serve
081500090526     c*                  if        tipoaut = 'C'
081600090526     C*                  move      TgtSOC        Societa                        Input/Output
081700090526     C*                  move      tgtcdf        keyksc                         Input/Output
081800090526     c*                  else
081900090526     C*                  move      adtcsf        Societa                        Input/Output
082000090526     C*                  move      adtcdf        keyksc                         Input/Output
082100090526     c*                  endif
082200090330      *
082300090330     c                   call      'TRMZ70SR1'
082400090408     C                   PARM                    trmz70s1ds
082500090430     c                   if        errore <> '0'
082600090430     c                   if        tipoaut = 'C'
082700090430     c                   exsr      componi
082800090430     c                   else
082900090430     c                   exsr      componia
083000090430     c                   endif
083100090430     c                   eval      nota = errmsg
083200090430     c                   eval      no_allegato = '1'
083300090430     c                   except    NoFornit1
083400090430     c                   endif
083500090330     c*
083600090330     C                   ENDSR
083700090427      * ?___________________________________________________________________
083800090427     C     RepIvaFor     BegSR
083900090427      * ?___________________________________________________________________
084000090427     c* reperisco la p.iva del fornitore immesso in apd
084100090427     c     krco          chain     anfrn01l
084200090427     c                   if        %found(anfrn01l)
084300090427     c     frnsogg       chain     ansog01l
084400090427     c                   if        %found(ansog01l)
084500090427     c                   movel     sogpartiva    IvaForn
084600090427     c                   end
084700090427     c                   end
084800090428     c                   if        ivaforn = *blank
084900090428     c                   if        tipoaut = 'C'
085000090429     c                   exsr      componi
085100090428     c                   else
085200090429     c                   exsr      componia
085300090428     c                   endif
085400090430     c                   eval      nota ='Non trovato fornitore per la società'
085500090430     c                   eval      no_allegato = '1'
085600090428     c                   except    NoFornit
085700090428     c                   endif
085800090427      *
085900090427     C                   ENDSR
086000090623      * ?___________________________________________________________________
086100090623     C     Venezia       BegSR
086200090623      * ?___________________________________________________________________
086300090623     c                   movel     'A'           tipapd
086400090623     c                   movel     tgtpdr        codapd
086500090623     c     kapd          chain     fiapd01l
086600090623     c                   if        %found(fiapd01l)
086700090623     c                   movel     apdflr        dapdflr
086800090623     c                   if        §apdmezzo = 'B'
086900090623     c                   z-add     wdatg         trsdsc
087000130111      **
087100130111      * imposta la società stampata
087200130115     c                   movel     trsflr1       dtrsflr1
087300130115     c                   eval      §trsSOCprt = trsSOCG
087400130115     c                   eval      trsflr1    = dtrsflr1
087500130111      **
087600090623     c                   update    aitrs000
087700090623     c                   move      'X'           contvenezia
087800090623     c                   endif
087900090623     c                   endif
088000090623     C                   endsr
088100090429      * ?___________________________________________________________________
088200090429     C     componi       BegSR
088300090429      * ?___________________________________________________________________
088400090429     c                   if        tradin = 0
088500090429     c                   move      dataiso0      tradins           8 0
088600090429     c                   else
088700090429     c                   z-add     tradin        tradins
088800090429     c                   endif
088900090429     c                   if        tradfi = 0
089000090429     c                   move      dataiso0      tradfis           8 0
089100090429     c                   else
089200090429     c                   z-add     tradfi        tradfis
089300090429     c                   endif
089400090429     c                   if        trsdec = 0
089500090429     c                   move      dataiso0      trsdecs           8 0
089600090429     c                   else
089700090429     c                   z-add     trsdec        trsdecs
089800090429     c                   endif
089900090429     c                   if        trsdfc = 0
090000090429     c                   move      dataiso0      trsdfcs           8 0
090100090429     c                   else
090200090429     c                   z-add     trsdfc        trsdfcs
090300090429     c                   endif
090400090429      *
090500090525     c                   clear                   autistas
090600090525     c                   if        traaut <> *blank
090700090525     c                   eval      autistas = traaut
090800090525     c                   else
090900090525     c                   movel     'A'           tipapd
091000090525     c                   movel     tgtpdr        codapd
091100090525     c     kapd          chain     fiapd01l
091200090525     c                   if        %found(fiapd01l)
091300090525     c                   eval      autistas = apdrsf
091400090525     c                   endif
091500090525     c                   endif
091600090429     c                   eval      segnala= tipoaut + ' ' +%editc(kpdr:'Z')+
091700090525     c                             ' ' + autistas + ' ' +
091800090429     c                              %char(%date(tgtddt):*eur) + ' ' +
091900090429     c                              %char(%date(tgtdst):*eur) + ' ' +
092000090429     c                              %char(%date(tgtdts):*eur) + ' ' +
092100090429     c                             ' ' + tgtsoc + ' ' +  tgtcdf + ' ' + trasoc +
092200090429     c                              %char(%date(tradins):*eur) + ' ' +
092300090429     c                              %char(%date(tradfis):*eur) + ' ' +
092400090429     c                              %char(%date(trsdecs):*eur) + ' ' +
092500090429     c                              %char(%date(trsdfcs):*eur) + ' ' +
092600090429     c                             ' ' + trsiva + ' '
092700090429     c                   endsr
092800090429      * ?___________________________________________________________________
092900090429     C     componia      BegSR
093000090429      * ?___________________________________________________________________
093100090429     c                   if        tradin = 0
093200090429     c                   move      dataiso0      tradins           8 0
093300090429     c                   else
093400090429     c                   z-add     tradin        tradins
093500090429     c                   endif
093600090429     c                   if        tradfi = 0
093700090429     c                   move      dataiso0      tradfis           8 0
093800090429     c                   else
093900090429     c                   z-add     tradfi        tradfis
094000090429     c                   endif
094100090429     c                   if        trsdec = 0
094200090429     c                   move      dataiso0      trsdecs           8 0
094300090429     c                   else
094400090429     c                   z-add     trsdec        trsdecs
094500090429     c                   endif
094600090429     c                   if        trsdfc = 0
094700090429     c                   move      dataiso0      trsdfcs           8 0
094800090429     c                   else
094900090429     c                   z-add     trsdfc        trsdfcs
095000090429     c                   endif
095100090525     c                   clear                   autistas
095200120222      ***
095300090525     c                   if        traaut <>*blank
095400090525     c                   eval      autistas = traaut
095500090525     c                   else
095600120222      ***   se affl.defl.
095700090525     c                   movel     'D'           tipapd
095800090525     c                   movel     adtpdr        codapd
095900090525     c     kapd          chain     fiapd01l
096000090525     c                   if        %found(fiapd01l)
096100090525     c                   eval      autistas = apdrsf
096200090525     c                   endif
096300090525     c                   endif
096400090429      *
096500090429     c                   eval      segnala= tipoaut + ' ' +%editc(kpdr:'Z')+
096600090525     c                             ' ' + autistas + ' ' +
096700090429     c                              %char(%date(adtddt):*eur) + ' ' +
096800090429     c                              %char(%date(adtdst):*eur) + ' ' +
096900090429     c                              %char(%date(adtdts):*eur) + ' ' +
097000090429     c                             ' ' + adtcsf + ' ' +  adtcdf + ' ' + trasoc +
097100090429     c                              %char(%date(tradins):*eur) + ' ' +
097200090429     c                              %char(%date(tradfis):*eur) + ' ' +
097300090429     c                              %char(%date(trsdecs):*eur) + ' ' +
097400090429     c                              %char(%date(trsdfcs):*eur) + ' ' +
097500090429     c                             trsiva + ' '
097600090429     c                   endsr
097700121130ab   C**************************************************************************
097800090330      * ?___________________________________________________________________
097900011026     c     *inzsr        begsr
098000090330      * ?___________________________________________________________________
098100020304     c     *entry        plist
098200020304     c                   parm                    kpjba
098300090408     c                   clear                   paramI
098400090408     c                   if        kpjbu <> *blank
098500090408     c                   movel     kpjbu         paramI
098600090408     c                   endif
098700020304
098800090520     c                   z-add     99            line
098900020304     c
099000090330     c     kaitra        klist
099100090331     c                   kfld                    kpdr
099200090331     c                   kfld                    data0
099300011026
099400090525     c     kapd          klist
099500090525     c                   kfld                    tipapd            1
099600090525     c                   kfld                    codapd            7 0
099700090525
099800090427     c     krco          klist
099900090427     c                   kfld                    socfor            3
100000090427     c                   kfld                    codfor            8
100100090427
100200090406     c     kfgt          klist
100300090406     c                   kfld                    tGTPDR
100400090406     c                   kfld                    tGTSML
100500090407     c                   kfld                    kTSR              1
100600090511      *
100700090511     c     kadd          klist
100800090511     c                   kfld                    adtpdr
100900090511     c                   kfld                    adtprg
101000130116     C*
101100130116     C* klist
101200130116     C     KTBE1         KLIST
101300130116     C                   KFLD                    tbeCOD
101400130116     C                   KFLD                    tbeKE1
101500130116     C*
101600020218     C                   time                    w0120            14 0
101700090508     C                   movel     w0120         times             6 0
101800090331     C                   move      w0120         wdat
101900020308      * data da impostare su data stampa e convalida se lancio = 'C'
102000020308     c                   move      wdat          dataeur
102100020308     c                   move      dataeur       dataiso
102200090331     c                   move      dataiso       wdatg
102300130117     c                   move      dataiso       oggi
102400090424      * forzo la data di stampa se mi è stata passata (data certa)
102500090424     c                   if        pardatI > 0
102600090424     c                   z-add     pardatI       wdatg
102700090424     c                   endif
102800090428      *
102900090430     c                   movel     cmd(1)        comman
103000090430     C                   Z-ADD     80            LUNG
103100090430     C                   CALL      'QCMDEXC'
103200090430     C                   PARM                    COMMAN           80
103300090430     C                   PARM                    LUNG             15 5
103400090430     C                   OPEN      prtf198
103500090424      *_______________________________________________________________
103600120326     c                   if        paroutqI <> *blank
103700120326     c                   call      'TRMZ0100CO'
103800120326     c                   parm                    paroutqI
103900120326     c                   end
104000020313
104100090401      /FREE
104200090401
104300090401       trmz0100r( 'INIT'
104400090401                : prmRpyOpCode
104500090401                : prmRpyIdMsg
104600090401                );
104700090401
104800090401       IF prmRpyIdMsg < 0;
104900090401         RETURN;
105000090401       ENDIF;
105100090401
105200090401      /END-FREE
105300090401     c*
105400090401     c                   endsr
105500090330      * ?___________________________________________________________________
105600120222     C**************************************************************************
105700090506     Oprtf198   E            testata          02
105800090428     o                                           10 'TRMZ79R1'
105900090330     o                       knmus               22
106000090506     o                                        +   2 'Elenco Anomalie riscontrat-
106100090428     o                                              e nella stampa contratti e -
106200090428     o                                              allegati'
106300090508     O                       times            +   2 '  .  .  '
106400090508     o                       udate         y  +   2
106500090508     O                       page          z  +   2
106600090506     O          E            testata     2
106700090506     o                                           78 'D A T I  T A R I F F A'
106800090506     o                                        +   6 'D A T I  C E R T I F I C A'
106900090506     o                                        +   1 'Z I O N I '
107000090506     O          E            testata     2
107100090429     o                                           15 'Nome Autista'
107200090429     o                                           46 'Decorrenza'
107300090429     o                                           57 'Scadenza  '
107400090429     o                                           68 'Convalida '
107500090429     o                                           82 'Fornitore '
107600090506     o                                          100 'Società Autista'
107700090429     o                                          118 'Accreditato'
107800090429     o                                          129 'Disaccred.'
107900090429     o                                          140 'Dec. Contr.'
108000090429     o                                          151 'Scad.Contr.'
108100090429     o                                          160 'P.I.v.A.'
108200090506     O          E            testata     0
108300090506     o                                           15 'Nome Autista'
108400090506     o                                           46 'Decorrenza'
108500090506     o                                           57 'Scadenza  '
108600090506     o                                           68 'Convalida '
108700090506     o                                           82 'Fornitore '
108800090506     o                                          100 'Società Autista'
108900090506     o                                          118 'Accreditato'
109000090506     o                                          129 'Disaccred.'
109100090506     o                                          140 'Dec. Contr.'
109200090506     o                                          151 'Scad.Contr.'
109300090506     o                                          160 'P.I.v.A.'
109400090506     O          E            testata     0
109500090506     o                                           15 'Nome Autista'
109600090506     o                                           46 'Decorrenza'
109700090506     o                                           57 'Scadenza  '
109800090506     o                                           68 'Convalida '
109900090506     o                                           82 'Fornitore '
110000090506     o                                          100 'Società Autista'
110100090506     o                                          118 'Accreditato'
110200090506     o                                          129 'Disaccred.'
110300090506     o                                          140 'Dec. Contr.'
110400090506     o                                          151 'Scad.Contr.'
110500090506     o                                          160 'P.I.v.A.'
110600120606     O          E            TrazErrA    2
110700120606     o                       segnalaT           120
110800120606     o                       errmsgt            198
110900090424     O          E            AccrDeco    2
111000090429     o                       segnala            165
111100090429     o                       nota               198
111200090427     O          E            AccrDecoA   2
111300090429     o                       segnala            165
111400090429     o                       nota               198
111500090429     O          E            Noaut       2
111600090429     o                       segnala            165
111700090429     o                       nota               198
111800090427     O          E            NoautA      2
111900090429     o                       segnala            165
112000090429     o                       nota               198
112100110321     O          E            SocNoCor    2
112200110321     o                       segnala            165
112300110321     o                       nota               198
112400090427     O          E            Pivaerrore  2
112500090429     o                       segnala            165
112600090429     o                       nota               198
112700090331     O          E            Soccontsca  2
112800090429     o                       segnala            165
112900090429     o                       nota               198
113000090407     O          E            Soccontant  2
113100090429     o                       segnala            165
113200090429     o                       nota               198
113300090427     O          E            TarAntCon   2
113400090429     o                       segnala            165
113500090429     o                       nota               198
113600090427     O          E            TarAntConA  2
113700090429     o                       segnala            165
113800090429     o                       nota               198
113900090331     O          E            NoSoc       2
114000090429     o                       segnala            165
114100090429     o                       nota               198
114200090331     O          E            ErrStampa   2
114300090429     o                       segnala            165
114400090429     o                       nota               198
114500090430     O          E            NoFornit1   2
114600090430     o                       segnala            165
114700090430     o                       nota               198
114800090331     O          E            NoFornit    2
114900090429     o                       segnala            165
115000090429     o                       nota               198
115100090427     O          E            Allegato    2
115200090429     o                       segnala            165
115300090429     o                       nota               198
115400090427     O          E            Allegatod   2
115500090429     o                       segnala            165
115600090429     o                       nota               198
115700090511     O          E            NoDetTar    2
115800090511     o                       segnala            165
115900090511     o                       nota               198
116000120222AB   O          E            NoTrazio    2
116100120222     o                       segnala            165
116200120222     o                       nota               198
116300090430**
116400090430OVRPRTF FILE(prtf198) hold(*yes) save(*yes)
