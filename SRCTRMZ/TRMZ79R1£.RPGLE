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
001400090430     Fprtf198   O    F  198        PRINTER prtctl(prtds) usropn
001500090331      * ?___________________________________________________________________
001600090331     D/COPY GAITRASRC/SRCPROTOPR,TRMZ0100R
001700090331     D prmRpyOpCode...
001800090331     D                 S             10A
001900090331     D prmRpyIdMsg...
002000090331     D                 S             10I 0
002100121130ab   ** Prototipi.
002200121130ab    **
002300090331      * ?___________________________________________________________________
002400020304     dkpjba          e ds
002500090408     D trmz0100i     E DS                  prefix(x)
002600090408     D trmz70s1ds    E DS
002700090429     D aitrs00ds     E DS                  extname(aitrs00f)
002800090429     D aitra00ds     E DS                  extname(aitra00f)
002900120606     D DSaiats       E DS                  EXTNAME(AIATS00F)
003000111205     D trul13ds      E DS
003100111205     D dapdflr       E DS
003200111205     D dtgtflr       E DS
003300111205     D dtrsflr       E DS
003400120222
003500120222      *
003600090330     d prtds           ds
003700090330     d   spab                         3  0
003800090330     d   spaa                         3  0
003900090330     d   skab                         3  0
004000090330     d   skaa                         3  0
004100090330     d   line                         3  0
004200090408     d paramI          ds
004300090424     d   parfilI                      3  0
004400090428     d   pardatI                      8  0
004500090428     d   paroutqI                    10
004600090407     d param           ds
004700090407     d   partip                       1
004800090407     d   parpdrda                     7  0
004900090407     d   parpdra                      7  0
005000090407     d   pardat                       8  0
005100090424     d   pardtcert                    8  0
005200090428     d   parnrc                       7s 0
005300090428     d   parfil                       3s 0
005400090428     D   paroutq                     10
005500090427     d   parerr                       1
005600090526     d   parmsg                      35
005700121130     D   pgmChiamante                10
005800111206     d   pardfa                       8  0
005900121130     d   parsocprt                    3
006000090407     d paramdef        ds
006100090407     d   parpdrd                      7  0
006200090407     d   parprgd                      3  0
006300090407     d   partipd                      1
006400090424     d   pardtcertd                   8  0
006500090428     d   parnrcd                      7s 0
006600090428     d   parfild                      3s 0
006700090428     D   paroutqd                    10
006800090427     d   parerrd                      1
006900111206     d   pardfad                      8  0
007000121130     d   parsocprtd                   3
007100120606     D partraz         DS
007200120606     D  parsoc                        3s 0
007300120606     D  pariva                       16
007400120606     D  pardtin                       8s 0
007500120606     D  pardtfi                       8s 0
007600120606     d  esito                         1
007700120606     d  errmsgt                      75
007800090331      * ?___________________________________________________________________
007900090429     d dataiso0        s               d   datfmt(*iso)
008000020305     d dataiso         s               d   datfmt(*iso)
008100020305     d dataeur         s               d   datfmt(*eur)
008200090331     d data0           s              8  0
008300090525     d kpdr            s                   like(adtpdr)
008400090525     d autistas        s                   like(traaut)
008500090331     d wdatg           s              8  0
008600090331     d wdat            s              8  0
008700090408     d*
008800090401     d No_allegato     s              1
008900121130     d filiale         s              3  0
009000090330     d TipoAut         s              1    inz('C')
009100090429     d segnala         s            165
009200120606     d segnalaT        s            120
009300090429     d nota            s             33
009400090430     D CMD             S             80    DIM(1) CTDATA PERRCD(1)              OVRPRTF
009500000000     C*---------------------------------------------------------------*
009600090330      * ?______________  main   ____________________________________________
009700090330     c* lettura allegati tariffari da stampare autisti città
009800090330     c     *loval        setll     fitgt02l
009900090109     c                   do        *hival
010000090330     c                   read      fitgt02l
010100090330     c                   if        %eof(fitgt02l)
010200090109     c                   leave
010300090109     c                   end
010400090408      * se impostata scelta singola filiale scarto le tariffe delle altre
010500090408     c                   movel     tgtpdr        pdr3              3 0
010600090428     c                   if        parfilI > 0 and pdr3 > parfilI
010700090428     c                   leave
010800090428     c                   end
010900090424     c                   if        parfilI > 0 and pdr3 <> parfilI
011000090408     c                   iter
011100090408     c                   end
011200090519      *non considero le tariffe con decorrenza scadenza al 01/05/2009
011300090519     c                   if        tgtddt = 20090501 and tgtdst = 20090501
011400090519     c                   iter
011500090519     c                   end
011600090617      *non considero le tariffe con decorrenza maggiore della data certa
011700120607     c*                  if        tgtddt > wdatg
011800091015     c*                  iter
011900091015     c*                  end
012000090519      *
012100090430     c                   clear                   no_allegato
012200090330     c                   z-add     tgtpdr        kpdr
012300090429     c                   clear                   aitrs00ds
012400090429     c                   clear                   aitra00ds
012500090506      * a rottura di filiale ristampo la testata del report anomalie
012600090519     c                   if        line > 62
012700090506     c                   except    testata
012800090506     c                   endif
012900090330     c                   exsr      trovata
013000090325     c                   enddo
013100090112     c*
013200090330     c* lettura allegati tariffari da stampare autisti AFF/DEF
013300090508     c                   movel     'A'           TipoAut
013400090330     c     *loval        setll     fiadt02l
013500090330     c                   do        *hival
013600090330     c                   read      fiadt02l
013700090330     c                   if        %eof(fiadt02l)
013800090330     c                   leave
013900090330     c                   end
014000090408      * se impostata scelta singola filiale scarto le tariffe delle altre
014100090408     c                   movel     adtpdr        pdr3
014200090428     c                   if        parfilI > 0 and pdr3 > parfilI
014300090428     c                   leave
014400090428     c                   end
014500090424     c                   if        parfilI > 0 and pdr3 <> parfilI
014600090408     c                   iter
014700090408     c                   end
014800090519      *non considero le tariffe con decorrenza scadenza al 01/05/2009
014900090519     c                   if        adtddt = 20090501 and adtdst = 20090501
015000090519     c                   iter
015100090519     c                   end
015200090617      *non considero le tariffe con decorrenza maggiore della data certa
015300091015     c*                  if        adtddt > wdatg
015400091015     c*                  iter
015500091015     c*                  end
015600090519      *
015700090430     c                   clear                   no_allegato
015800090330     c                   z-add     adtpdr        kpdr
015900090429     c                   clear                   aitrs00ds
016000090429     c                   clear                   aitra00ds
016100090506      * a rottura di filiale ristampo la testata del report anomalie
016200090519     c                   if        line > 62
016300090506     c                   except    testata
016400090506     c                   endif
016500090330     c                   exsr      trovatab
016600090330     c                   enddo
016700090330     c*
016800090401      /FREE
016900090401       trmz0100r( 'FINALIZE'
017000090401                : prmRpyOpCode
017100090401                : prmRpyIdMsg
017200090401                );
017300090401
017400090401      /END-FREE
017500090330     c                   seton                                        lr
017600090330      * ?___________________________________________________________________
017700090330     c     trovata       begsr
017800090330      * ?___________________________________________________________________
017900090330     c* verifico se l'autista è già stato inserito nell'anagrafica autisti automezzi
018000090330      * se esiste passo alle verifiche sulla società
018100090330      * altrimenti segnalo in stampa l'anomalia
018200090330     c     kaitra        chain     aitra03l
018300090330     c                   if        %found(aitra03l)
018400090427     c                   if        tradin > tgtddt
018500120322     c     kpdr          setgt     aitra03l
018600120322     c     kpdr          readpe    aitra03l
018700120322     c                   if        not %eof(aitra03l)
018800120322     c                   if        tradin > tgtddt
018900090429     c                   exsr      componi
019000090429     c                   eval      nota ='Dt. accreditamento > decor.tariffa'
019100090424     c                   except    AccrDeco
019200090430     c                   leavesr
019300120322     c                   else
019400120322     c                   if        tradfi > tgtddt
019500120322      *imposta dati fornitore della tariffa per confronto partita iva
019600120322     c                   move      tgtsoc        socfor
019700120322     c                   move      tgtcdf        codfor
019800120322     c                   exsr      VerSocieta
019900120322     c                   leavesr
020000120322     c                   endif
020100090424     c                   endif
020200120322     c                   endif
020300120322     c                   endif
020400090427      *imposta dati fornitore della tariffa per confronto partita iva
020500090427     c                   move      tgtsoc        socfor
020600090427     c                   move      tgtcdf        codfor
020700090427      *
020800090330     c                   exsr      VerSocieta
020900090330     c                   else
021000090424     c     kpdr          setgt     aitra03l
021100090424     c     kpdr          readpe    aitra03l
021200090428     c                   if        not %eof(aitra03l)
021300090528      * se la data disaccreditamento è maggiore della decorrenza tariffa stampo ugualmente
021400090528     c                   if        tradfi > tgtddt
021500090529      *imposta dati fornitore della tariffa per confronto partita iva
021600090529     c                   move      tgtsoc        socfor
021700090529     c                   move      tgtcdf        codfor
021800090528     c                   exsr      VerSocieta
021900090528     c                   leavesr
022000090528     c                   endif
022100090429     c                   exsr      componi
022200090429     c                   eval      nota ='Disaccr. in ANAGRAFICA AUTOMEZZI'
022300090330     c                   except    NoAut
022400090430     c                   leavesr
022500090427     c                   else
022600090429     c                   exsr      componi
022700090429     c                   eval      nota ='Non ancora inserito in ANAG.Autom.'
022800090427     c                   except    NoAut
022900090430     c                   leavesr
023000090330     c                   endif
023100090424     c                   endif
023200090330     c                   endsr
023300090330      * ?___________________________________________________________________
023400090330     c     trovatab      begsr
023500090330      * ?___________________________________________________________________
023600090511     c                   clear                   addettagli
023700090511
023800090511     c     kadd          setll     fiadd01l
023900090511     c                   do        *hival
024000090511     c     kadd          reade     fiadd01l
024100090511     c                   if        %eof(fiadd01l)
024200090511     c                   leave
024300090511     c                   endif
024400090511     c                   if        addatb = *blank
024500090511     c                   move      'X'           addettagli        1
024600090511     c                   leave
024700090511     c                   endif
024800090511     c                   enddo
024900120222      **
025000090511     c                   if        addettagli = *blank
025100090511     c                   exsr      componia
025200090511     c                   eval      nota ='Non presente dettaglio tariffario'
025300090511     c                   except    NoDetTar
025400090511     c                   leavesr
025500090511     c                   endif
025600120222      **
025700120222      **  Controlla se trazionista:
025800120606     c                   exsr      veriftraz
025900120717     c                   if        esito  <> ' '
026000120606     c                   if        esito  = '1'
026100120606     c                   eval      trsfil = 046
026200120606     c                   eval      trsnrc = atsnrc
026300120222     c                   exsr      AllegatoAd
026400120606     c                   else
026500120611     c                   if        atsdec = 0
026600120611     c                   move      dataiso0      atsdecs           8 0
026700120611     c                   else
026800120611     c                   z-add     atsdec        atsdecs
026900120611     c                   endif
027000120611     c                   if        atsdfc = 0
027100120611     c                   move      dataiso0      atsdfcs           8 0
027200120611     c                   else
027300120611     c                   z-add     atsdfc        atsdfcs
027400120611     c                   endif
027500120606     c                   eval      segnalaT= 'T' + ' ' +
027600120606     c                              %editc(kpdr:'Z') +' Dec. ' +
027700120606     c                              %char(%date(adtddt):*eur) + ' Scad. ' +
027800120606     c                              %char(%date(adtdst):*eur) + ' Stampa ' +
027900120606     c                              %char(%date(adtdts):*eur) + ' Dec.c. ' +
028000120611     c                              %char(%date(atsdecs):*eur) + ' Scad.c ' +
028100120611     c                              %char(%date(atsdfcs):*eur) + ' ' +
028200120606     c                             ' ' + adtcsf + ' ' +  adtcdf + ' ' +
028300120606     c                             ' ' + atspiva + ' '+%editc(atsnrc:'Z')
028400120606     c                   except    TrazErrA
028500120606     c                   leavesr
028600120606     c                   endif
028700120222      **
028800120222     c                   eLSe
028900120222      *
029000090330     c* verifico se l'autista è già stato inserito nell'anagrafica autisti automezzi
029100090330      * se esiste passo alle verifiche sulla società
029200090330      * altrimenti segnalo in stampa l'anomalia
029300090330     c     kaitra        chain     aitra05l
029400090330     c                   if        %found(aitra05l)
029500090427     c                   if        tradin > adtddt
029600120322     c     kpdr          setgt     aitra05l
029700120322     c     kpdr          readpe    aitra05l
029800120322     c                   if        not %eof(aitra05l)
029900120322     c                   if        tradin > adtddt
030000090429     c                   exsr      componia
030100090429     c                   eval      nota ='Dt. accreditamento > decor.tariffa'
030200090427     c                   except    AccrDecoA
030300090430     c                   leavesr
030400120322     c                   else
030500120322     c                   if        tradfi > tgtddt
030600120322      *imposta dati fornitore della tariffa per confronto partita iva
030700120322     c                   move      tgtsoc        socfor
030800120322     c                   move      tgtcdf        codfor
030900120322     c                   exsr      VerSocieta
031000120322     c                   leavesr
031100120322     c                   endif
031200090427     c                   endif
031300120322     c                   endif
031400120322     c                   endif
031500090427     c                   move      adtcsf        socfor
031600090427     c                   move      adtcdf        codfor
031700090330     c                   exsr      VerSocieta
031800090330     c                   else
031900090427     c     kpdr          setgt     aitra05l
032000090427     c     kpdr          readpe    aitra05l
032100090511     c                   if        not %eof(aitra05l)
032200090528      * se la data disaccreditamento è maggiore della decorrenza tariffa stampo ugualmente
032300090528     c                   if        tradfi > adtddt
032400090529     c                   move      adtcsf        socfor
032500090529     c                   move      adtcdf        codfor
032600090528     c                   exsr      VerSocieta
032700090528     c                   leavesr
032800090528     c                   endif
032900090429     c                   exsr      componia
033000090429     c                   eval      nota ='Disaccr. in ANAGRAFICA AUTOMEZZI'
033100090427     c                   except    NoAutA
033200090430     c                   leavesr
033300090427     c                   else
033400090429     c                   exsr      componia
033500090429     c                   eval      nota ='Non ancora inserito in ANAG.Autom'
033600090427     c                   except    NoAutA
033700090430     c                   leavesr
033800090427     c                   endif
033900090330     c                   endif
034000120222      **  <CODICE>
034100120222     c                   eND
034200120222      *
034300090330     c                   endsr
034400120606     C**************************************************************************
034500120606     C*    CONTROLLo per a/d se il fornitore è un trazionista certificato
034600120606     C**************************************************************************
034700120606     C     veriftraz     BEGSR
034800120606     c     kfrn          klist
034900120606     C                   kfld                    adtcsf
035000120606     C                   kfld                    adtcdf
035100120607     c                   clear                   partraz
035200120606     c                   clear                   sogpartiva
035300120606     c     kfrn          chain     anfrn01l
035400120606     c                   if        %found(anfrn01l)
035500120606     c     frnsogg       chain     ansog01l
035600120606     c                   if        %found(ansog01l)
035700120606     c                   move      adtcsf        parsoc
035800120607     c                   move      adtddt        pardtin
035900120607     c                   move      adtdst        pardtfi
036000120606     c                   movel     sogpartiva    pariva
036100120606     c                   call      'TRULTRAZ'
036200120606     c                   parm                    partraz
036300120606     c                   parm                    dsAIATS
036400120606     c                   endif
036500120606     c                   endif
036600120606     c                   endsr
036700090330      * ?___________________________________________________________________
036800090330     c     VerSocieta    begsr
036900090330      * ?___________________________________________________________________
037000090401     c                   clear                   No_allegato
037100090427     c                   clear                   IvaForn          16
037200090427     c                   exsr      RepIvaFor
037300090330     c* verifico la società che sia stata inserita che sia ancora valida e lo stato
037400090330      * del contratto
037500090330      * se mancano i prerequisiti per la stampa dell'allegato li segnalo
037600090330     c     tranrc        chain     aitrs01l
037700090330     c                   if        %found(aitrs01l)
037800090330
037900090330     c                   select
038000110321      * Società accreditamento autista diversa da società allegato tariffario
038100110321     c                   when      trssocg <> socfor
038200110321     c                   if        tipoaut = 'C'
038300110321     c                   exsr      componi
038400110321     c                   else
038500110321     c                   exsr      componia
038600110321     c                   endif
038700110321     c                   eval      nota ='Soc. Acc.'+ trssocg +
038800110321     c                             ' diversa da Tar. '+ socfor
038900110321     c                   except    SocNoCor
039000110321     c                   leavesr
039100110321
039200090427      * Partita iva società diversa da quella del fornitore in tariffa
039300090427     c                   when      trsiva <> IvaForn
039400090428     c                   if        tipoaut = 'C'
039500090429     c                   exsr      componi
039600090428     c                   else
039700090429     c                   exsr      componia
039800090428     c                   endif
039900090429     c                   eval      nota ='P.Iva Società <> da P.Iva fornitore'
040000090427     c                   except    PivaErrore
040100090430     c                   leavesr
040200090427
040300090330      * società con contratto scaduto o annullata
040400090604     c                   when      trsdfc > 0  or trsann <> *blank
040500090604     c                   if        tipoaut = 'C'
040600090604     c                   if        tgtddt > trsdfc or trsann <> *blank
040700090429     c                   exsr      componi
040800090604     c                   eval      nota ='Società con contratto scaduto '
040900090604     c                   except    SocContSCA
041000090604     c                   leavesr
041100090623     c                   else
041200090623     c                   exsr      AllegatoCit
041300090604     c                   endif
041400090428     c                   else
041500090604     c                   if        adtddt > trsdfc or trsann <> *blank
041600090429     c                   exsr      componia
041700090604     c                   eval      nota ='Società con contratto scaduto '
041800090604     c                   except    SocContSCA
041900090604     c                   leavesr
042000090623     c                   else
042100090623     c                   exsr      AllegatoAd
042200090604     c                   endif
042300090428     c                   endif
042400090330
042500090427      * società con contratto con data inferiore al minimo ammesso
042600090407     c                   when      trsdec < 20090501
042700090428     c                   if        tipoaut = 'C'
042800090429     c                   exsr      componi
042900090428     c                   else
043000090429     c                   exsr      componia
043100090428     c                   endif
043200090429     c                   eval      nota = 'Contratto antecedente data minima'
043300090407     c                   except    SocContAnt
043400090430     c                   leavesr
043500090407
043600090427      * Autista di città con tariffa antecedente decorrenza contratto
043700090427     c                   when      Tipoaut = 'C' and trsdec > tgtddt
043800090429     c                   exsr      componi
043900090429     c                   eval      nota =
044000090429     c                             'tariffa antecedente decorrenza contratto'
044100090427     c                   except    TarAntCon
044200090430     c                   leavesr
044300090427
044400090427      * Autista Aff/Def con tariffa antecedente decorrenza contratto
044500090508     c                   when      Tipoaut = 'A' and trsdec > adtddt
044600090429     c                   exsr      componia
044700090429     c                   eval      nota=
044800090429     c                             'tariffa antecedente decorrenza contratto'
044900090427     c                   except    TarAntConA
045000090430     c                   leavesr
045100090427
045200090330      * società con contratto da stampare
045300090401     c                   when      trsdsc = 0
045400090623     c                   clear                   contvenezia       1
045500090623      * ?Attenzione particolarità filiale Venezia bisogna verificare se il tipo mezzo
045600090623      * ?dell'autista è barca nel caso il contratto è gestito a mano dall'ufficio automezzi
045700090623     c                   if        Tipoaut = 'C' and pdr3 = 58
045800090623     c                   exsr      Venezia
045900090623     c                   endif
046000090623     c                   if        contvenezia = *blank
046100090330     c                   exsr      Contratto
046200090623     c                   endif
046300090401      * problemi nella stampa contratto quindi non stampo allegato tariffario
046400090401     c                   if        No_allegato = *blank
046500090330     c                   if        TipoAut = 'C'
046600090330     c                   exsr      AllegatoCit
046700090330     c                   else
046800090330     c                   exsr      AllegatoAd
046900090330     c                   endif
047000090401     c                   endif
047100090330
047200090330      * società con contratto stampato stampa solo l'allegato autista
047300090330     c                   other
047400090430     c                   if        No_allegato = *blank
047500090330     c                   if        TipoAut = 'C'
047600090330     c                   exsr      AllegatoCit
047700090330     c                   else
047800090330     c                   exsr      AllegatoAd
047900090430     c                   endif
048000090330     c                   endif
048100090330     c                   endsl
048200090330     c                   else
048300090330      * società non presente
048400090428     c                   if        TipoAut = 'C'
048500090429     c                   exsr      componi
048600090428     c                   else
048700090429     c                   exsr      componia
048800090428     c                   endif
048900090429     c                   eval      nota =
049000090429     c                             'Società non trovata in Anagrafica società'
049100090330     c                   except    NoSoc
049200090330     c                   endif
049300090330     c                   endsr
049400090330      * ?___________________________________________________________________
049500090330     c     Contratto     begsr
049600090330      * ?___________________________________________________________________
049700090330     c* Richiamo PGM per stampa del contratto
049800090330     c                   exsr      RagSocCont
049900090330     c                   if        trovato = *on
050000090330     c                   clear                   prmRpyOpCode
050100090330     c                   clear                   prmRpyIdMsg
050200090330     c                   clear                   trmz0100i
050300090401     c                   move      trsdec        dataiso
050400090408     c                   eval      xCONTRATDAT  = dataiso
050500090408     c                   eval      xCONTRATNUM  = trsnrc
050600090408     c                   eval      xFILIALE     = trsfil
050700090408     c                   eval      xVETRAGSOC   = %trim(ragsocksc) + ' ' +
050800090407     c                                          %trim(ragsocksc1)
050900090408     c                   eval      xVETSLCOM    = %trim(localita)
051000090408     c                   eval      xVETSLIND    = %trim(indirizzo)
051100090408     c                   eval      xVETSLCAP    = %trim(cap)
051200090408     c                   eval      xVETSLPRV    = provincia
051300090408     c                   eval      xVETSLNAZ    = stato
051400090408     c                   eval      xVETCDFISCA  = CodiceFisc
051500090408     c                   eval      xVETCDIVA    = trsiva
051600090408     c                   eval      xVETRIMLUO   = %trim(trscit)
051700090408     c                   eval      xVETRIMNUM   = trsnis
051800090408     c                   eval      xVETLEGRAP   = %trim(trslra)
051900090408     c                   eval      xVETANAPRV   = trspia
052000090408     c                   eval      xVETANANUM   = trsnia
052100090424     c                   move      wdatg         dataiso
052200090424     c                   eval      xdatastampa = dataiso
052300111205     c                   eval      xdatacertap = dataiso
052400130111      *
052500121130      * Reperisce società valida da stampare sul contratto
052600130111      *  direttamente quella sul file.
052700130111     c                   eval      xSOCIETA     = trsSOCG
052800130111      *
052900111206     c* data firma (1° data certa + 1 gg lavorativo)
053000111207     C                   clear                   trul13ds
053100111207     c                   eval      i13fil       = trsfil
053200111206     C                   move      dataiso       i13din
053300111205     C                   EVAL      i13gio = 1
053400111205     C                   CALL      'TRUL13R'
053500111205     C                   PARM                    trul13ds
053600111205     C                   IF        o13err <> 'E'
053700111205     c                   move      o13dfi        dataiso
053800111205     c                   eval      xDATAFIRMAV = dataiso
053900111205     c                   end
054000111205     c
054100090401      /FREE
054200090401
054300090401       trmz0100r( 'PRTCONTRAT'
054400090401                : prmRpyOpCode
054500090401                : prmRpyIdMsg
054600090401                : 'TRMZ0100I'
054700090401                : trmz0100i
054800090401                : %SIZE(trmz0100i)
054900090401                );
055000090401
055100090401       IF prmRpyIdMsg < 0;
055200090401         // Stampa non riuscita.
055300090401       ENDIF;
055400090401
055500090401      /END-FREE
055600120326      **
055700090330      * se non c'è errore aggiorno data stampa contratto
055800090330     c                   if        prmrpyidmsg = 0
055900090330     c                   z-add     wdatg         trsdsc
056000111205     c                   movel     trsflr        dtrsflr
056100111205     c                   move      wdatg         §trsunodc
056200111205     c                   move      o13dfi        §trsdfa
056300111205     c                   eval      trsflr = dtrsflr
056400090401     c                   update    aitrs000
056500090330     c                   else
056600090330      * c'è errore stampo la segnalazione
056700090428     c                   if        tipoaut = 'C'
056800090429     c                   exsr      componi
056900090428     c                   else
057000090429     c                   exsr      componia
057100090428     c                   endif
057200090430     c                   eval      nota ='Errore nella stampa del contratto'
057300090330     c                   except    ErrStampa
057400090401     c                   eval      no_allegato = '1'
057500090330     c                   endif
057600090330     c                   else
057700090428     c                   if        tipoaut = 'C'
057800090429     c                   exsr      componi
057900090429     c                   else
058000090429     c                   exsr      componia
058100090428     c                   endif
058200090430     c                   eval      nota ='Non trovato fornitore'
058300090330     c                   except    NoFornit
058400090401     c                   eval      no_allegato = '1'
058500090330     c                   endif
058600090330     c                   endsr
058700090330      * ?___________________________________________________________________
058800090330     c     AllegatoCit   begsr
058900090330      * ?___________________________________________________________________
059000090330     c* Richiamo PGM per stampa allegato autista di città
059100090428     c                   clear                   param
059200090407     c                   z-add     tgtpdr        parpdrda
059300090407     c                   z-add     tgtpdr        parpdra
059400090427     c                   z-add     tgtddt        pardat
059500090407     c                   move      'C'           partip
059600090424     c                   z-add     wdatg         pardtcert
059700090428     c                   z-add     trsfil        parfil
059800090428     c                   z-add     trsnrc        parnrc
059900090428     c                   move      paroutqI      paroutq
060000111206     c                   if        tgtddt < wdatg
060100111206     c                   move      tgtddt        pardfa
060200111206     c                   else
060300111206     c                   move      wdatg         pardfa
060400111206     c                   end
060500130111      *
060600121130      *reperisce società valida nel periodo da stampare
060700130111      *  direttamente quella sul file.
060800130111     c                   eval      parsocprt = tgtSOC
060900130111      *
061000090407     c                   movel     param         kpjbu
061100090428     c                   call      'FICN14R'
061200090330     c                   parm                    kpjba
061300090427     c                   movel     kpjbu         param
061400090427     c                   if        parerr = *blank
061500090406     c                   z-add     trsfil        tgtfil
061600090330     c                   z-add     trsnrc        tgtnrc
061700090330     c                   z-add     wdatg         tgtdcn
061800111205     c                   movel     tgtflr        dtgtflr
061900111205     c                   move      wdatg         §tgtunodc
062000111206     c                   if        tgtddt < wdatg
062100111206     c                   move      tgtddt        §tgtdfa
062200111206     c                   else
062300111206     c                   move      wdatg         §tgtdfa
062400111206     c                   end
062500111205     c                   eval      tgtflr = dtgtflr
062600090401     c                   update    fitgt000
062700090427     c                   else
062800090429     c                   exsr      componi
062900090526     c                   eval      nota = parmsg
063000090427     c                   except    Allegato
063100090427     c                   endif
063200090407      * aggiorna testate tariffe legate alla supertestata che si sta trattando
063300090424      * specifiche disabilitate per ora non sembra debba essere aggiornato da qui FGT
063400090427      * ?ATTENZIONE SE ATTIVATE QUESTE SPECIFICHE DEVONO ESSERE POSTE DOPO UPDATE DI FITGT
063500090424     c                   if        1 = 2
063600090424     c                   move      'G'           ktsr
063700090424     c                   exsr      aggiornafgt
063800090424     c                   move      'C'           ktsr
063900090424     c                   exsr      aggiornafgt
064000090424     c                   move      'R'           ktsr
064100090424     c                   exsr      aggiornafgt
064200090424     c                   endif
064300090424     c*
064400090407     c                   endsr
064500090407      * ?___________________________________________________________________
064600090407     c     aggiornafgt   begsr
064700090407      * ?___________________________________________________________________
064800090406      *aggiorna fgt legati alla supertestata con data conferma
064900090407     c     kfgt          setll     fifgt01l
065000090406     c                   do        *hival
065100090406     c     kfgt          reade     fifgt01l
065200090406     c                   if        %eof(fifgt01l)
065300090406     c                   leave
065400090406     c                   endif
065500090407     c                   if        fgtprg <> tgtprg
065600090407     c                   iter
065700090407     c                   endif
065800090406     c                   z-add     wdatg         fgtdcn
065900090406     c                   update    fifgt000
066000090406     c                   enddo
066100090407     c                   endsr
066200090330      * ?___________________________________________________________________
066300090330     c     AllegatoAd    begsr
066400090330      * ?___________________________________________________________________
066500090330     c* Richiamo PGM per stampa allegato autista di AFF/DEF
066600090428     c                   clear                   paramdef
066700090407     c                   z-add     adtpdr        parpdrd
066800090407     c                   z-add     adtprg        parprgd
066900090407     c                   move      'C'           partipd
067000090424     c                   z-add     wdatg         pardtcertd
067100120606      *fittizzi se trazionista sono forzati da aiats
067200090428     c                   z-add     trsfil        parfild
067300090428     c                   z-add     trsnrc        parnrcd
067400120606      *------
067500090428     c                   move      paroutqI      paroutqd
067600111206     c                   if        adtddt < wdatg
067700111206     c                   z-add     adtddt        pardfad
067800111206     c                   else
067900111206     c                   z-add     wdatg         pardfad
068000111206     c                   end
068100130111      *
068200121130      *reperisce società valida nel periodo da stampare
068300130111      *  direttamente quella sul file.
068400130111     c                   eval      parsocprtd = adtCSF
068500130111      *
068600090427     c                   movel     paramdef      kpjbu
068700090406     c                   call      'FICNA6C'
068800090330     c                   parm                    kpjba
068900090427     c                   movel     kpjbu         paramdef
069000090427     c                   if        parerrd = *blank
069100090406     c                   z-add     trsfil        adtfil
069200090330     c                   z-add     trsnrc        adtnrc
069300090330     c                   z-add     wdatg         adtdcn
069400111205     c                   z-add     wdatg         adtunodc
069500111205     c                   if        adtddt < wdatg
069600111205     c                   z-add     adtddt        adtdfa
069700111205     c                   else
069800111205     c                   z-add     wdatg         adtdfa
069900111205     c                   end
070000090401     c                   update    fiadt000
070100090427     c                   else
070200090429     c                   exsr      componia
070300090512     c                   select
070400090512     c                   when      parerrd = '1'
070500090512     c                   eval      nota ='Testata tariffa annullata '
070600090512     c                   when      parerrd = '2'
070700090512     c                   eval      nota ='Testata tariffa non trovata '
070800090512     c                   when      parerrd = '3'
070900090512     c                   eval      nota ='Anagrafica autista annullata '
071000090512     c                   endsl
071100090427     c                   except    AllegatoD
071200090427     c                   endif
071300090330     c                   endsr
071400090330      * ?___________________________________________________________________
071500090330     C     RagSocCont    BegSR
071600090330      * ?___________________________________________________________________
071700090330      *
071800090408     C                   clear                   Trmz70s1ds                     Output
071900090330     C                   movel(p)  TRSiva        PartitaIVA                     Input
072000090408     C                   movel(p)  'F'           SottoNatur                     Input "F/C"
072100090330     C                   movel(p)  TRSFIL        Unita                          Input/Output
072200090608     C                   move      TrsSOCg       Societa                        Input/Output
072300090526      *asteriscate nella stampa del contratto non serve
072400090526     c*                  if        tipoaut = 'C'
072500090526     C*                  move      TgtSOC        Societa                        Input/Output
072600090526     C*                  move      tgtcdf        keyksc                         Input/Output
072700090526     c*                  else
072800090526     C*                  move      adtcsf        Societa                        Input/Output
072900090526     C*                  move      adtcdf        keyksc                         Input/Output
073000090526     c*                  endif
073100090330      *
073200090330     c                   call      'TRMZ70SR1'
073300090408     C                   PARM                    trmz70s1ds
073400090430     c                   if        errore <> '0'
073500090430     c                   if        tipoaut = 'C'
073600090430     c                   exsr      componi
073700090430     c                   else
073800090430     c                   exsr      componia
073900090430     c                   endif
074000090430     c                   eval      nota = errmsg
074100090430     c                   eval      no_allegato = '1'
074200090430     c                   except    NoFornit1
074300090430     c                   endif
074400090330     c*
074500090330     C                   ENDSR
074600090427      * ?___________________________________________________________________
074700090427     C     RepIvaFor     BegSR
074800090427      * ?___________________________________________________________________
074900090427     c* reperisco la p.iva del fornitore immesso in apd
075000090427     c     krco          chain     anfrn01l
075100090427     c                   if        %found(anfrn01l)
075200090427     c     frnsogg       chain     ansog01l
075300090427     c                   if        %found(ansog01l)
075400090427     c                   movel     sogpartiva    IvaForn
075500090427     c                   end
075600090427     c                   end
075700090428     c                   if        ivaforn = *blank
075800090428     c                   if        tipoaut = 'C'
075900090429     c                   exsr      componi
076000090428     c                   else
076100090429     c                   exsr      componia
076200090428     c                   endif
076300090430     c                   eval      nota ='Non trovato fornitore per la società'
076400090430     c                   eval      no_allegato = '1'
076500090428     c                   except    NoFornit
076600090428     c                   endif
076700090427      *
076800090427     C                   ENDSR
076900090623      * ?___________________________________________________________________
077000090623     C     Venezia       BegSR
077100090623      * ?___________________________________________________________________
077200090623     c                   movel     'A'           tipapd
077300090623     c                   movel     tgtpdr        codapd
077400090623     c     kapd          chain     fiapd01l
077500090623     c                   if        %found(fiapd01l)
077600090623     c                   movel     apdflr        dapdflr
077700090623     c                   if        §apdmezzo = 'B'
077800090623     c                   z-add     wdatg         trsdsc
077900090623     c                   update    aitrs000
078000090623     c                   move      'X'           contvenezia
078100090623     c                   endif
078200090623     c                   endif
078300090623     C                   endsr
078400090429      * ?___________________________________________________________________
078500090429     C     componi       BegSR
078600090429      * ?___________________________________________________________________
078700090429     c                   if        tradin = 0
078800090429     c                   move      dataiso0      tradins           8 0
078900090429     c                   else
079000090429     c                   z-add     tradin        tradins
079100090429     c                   endif
079200090429     c                   if        tradfi = 0
079300090429     c                   move      dataiso0      tradfis           8 0
079400090429     c                   else
079500090429     c                   z-add     tradfi        tradfis
079600090429     c                   endif
079700090429     c                   if        trsdec = 0
079800090429     c                   move      dataiso0      trsdecs           8 0
079900090429     c                   else
080000090429     c                   z-add     trsdec        trsdecs
080100090429     c                   endif
080200090429     c                   if        trsdfc = 0
080300090429     c                   move      dataiso0      trsdfcs           8 0
080400090429     c                   else
080500090429     c                   z-add     trsdfc        trsdfcs
080600090429     c                   endif
080700090429      *
080800090525     c                   clear                   autistas
080900090525     c                   if        traaut <> *blank
081000090525     c                   eval      autistas = traaut
081100090525     c                   else
081200090525     c                   movel     'A'           tipapd
081300090525     c                   movel     tgtpdr        codapd
081400090525     c     kapd          chain     fiapd01l
081500090525     c                   if        %found(fiapd01l)
081600090525     c                   eval      autistas = apdrsf
081700090525     c                   endif
081800090525     c                   endif
081900090429     c                   eval      segnala= tipoaut + ' ' +%editc(kpdr:'Z')+
082000090525     c                             ' ' + autistas + ' ' +
082100090429     c                              %char(%date(tgtddt):*eur) + ' ' +
082200090429     c                              %char(%date(tgtdst):*eur) + ' ' +
082300090429     c                              %char(%date(tgtdts):*eur) + ' ' +
082400090429     c                             ' ' + tgtsoc + ' ' +  tgtcdf + ' ' + trasoc +
082500090429     c                              %char(%date(tradins):*eur) + ' ' +
082600090429     c                              %char(%date(tradfis):*eur) + ' ' +
082700090429     c                              %char(%date(trsdecs):*eur) + ' ' +
082800090429     c                              %char(%date(trsdfcs):*eur) + ' ' +
082900090429     c                             ' ' + trsiva + ' '
083000090429     c                   endsr
083100090429      * ?___________________________________________________________________
083200090429     C     componia      BegSR
083300090429      * ?___________________________________________________________________
083400090429     c                   if        tradin = 0
083500090429     c                   move      dataiso0      tradins           8 0
083600090429     c                   else
083700090429     c                   z-add     tradin        tradins
083800090429     c                   endif
083900090429     c                   if        tradfi = 0
084000090429     c                   move      dataiso0      tradfis           8 0
084100090429     c                   else
084200090429     c                   z-add     tradfi        tradfis
084300090429     c                   endif
084400090429     c                   if        trsdec = 0
084500090429     c                   move      dataiso0      trsdecs           8 0
084600090429     c                   else
084700090429     c                   z-add     trsdec        trsdecs
084800090429     c                   endif
084900090429     c                   if        trsdfc = 0
085000090429     c                   move      dataiso0      trsdfcs           8 0
085100090429     c                   else
085200090429     c                   z-add     trsdfc        trsdfcs
085300090429     c                   endif
085400090525     c                   clear                   autistas
085500120222      ***
085600090525     c                   if        traaut <>*blank
085700090525     c                   eval      autistas = traaut
085800090525     c                   else
085900120222      ***   se affl.defl.
086000090525     c                   movel     'D'           tipapd
086100090525     c                   movel     adtpdr        codapd
086200090525     c     kapd          chain     fiapd01l
086300090525     c                   if        %found(fiapd01l)
086400090525     c                   eval      autistas = apdrsf
086500090525     c                   endif
086600090525     c                   endif
086700090429      *
086800090429     c                   eval      segnala= tipoaut + ' ' +%editc(kpdr:'Z')+
086900090525     c                             ' ' + autistas + ' ' +
087000090429     c                              %char(%date(adtddt):*eur) + ' ' +
087100090429     c                              %char(%date(adtdst):*eur) + ' ' +
087200090429     c                              %char(%date(adtdts):*eur) + ' ' +
087300090429     c                             ' ' + adtcsf + ' ' +  adtcdf + ' ' + trasoc +
087400090429     c                              %char(%date(tradins):*eur) + ' ' +
087500090429     c                              %char(%date(tradfis):*eur) + ' ' +
087600090429     c                              %char(%date(trsdecs):*eur) + ' ' +
087700090429     c                              %char(%date(trsdfcs):*eur) + ' ' +
087800090429     c                             trsiva + ' '
087900090429     c                   endsr
088000121130ab   C**************************************************************************
088100090330      * ?___________________________________________________________________
088200011026     c     *inzsr        begsr
088300090330      * ?___________________________________________________________________
088400020304     c     *entry        plist
088500020304     c                   parm                    kpjba
088600090408     c                   clear                   paramI
088700090408     c                   if        kpjbu <> *blank
088800090408     c                   movel     kpjbu         paramI
088900090408     c                   endif
089000020304
089100090520     c                   z-add     99            line
089200020304     c
089300090330     c     kaitra        klist
089400090331     c                   kfld                    kpdr
089500090331     c                   kfld                    data0
089600011026
089700090525     c     kapd          klist
089800090525     c                   kfld                    tipapd            1
089900090525     c                   kfld                    codapd            7 0
090000090525
090100090427     c     krco          klist
090200090427     c                   kfld                    socfor            3
090300090427     c                   kfld                    codfor            8
090400090427
090500090406     c     kfgt          klist
090600090406     c                   kfld                    tGTPDR
090700090406     c                   kfld                    tGTSML
090800090407     c                   kfld                    kTSR              1
090900090511      *
091000090511     c     kadd          klist
091100090511     c                   kfld                    adtpdr
091200090511     c                   kfld                    adtprg
091300090406
091400020218     C                   time                    w0120            14 0
091500090508     C                   movel     w0120         times             6 0
091600090331     C                   move      w0120         wdat
091700020308      * data da impostare su data stampa e convalida se lancio = 'C'
091800020308     c                   move      wdat          dataeur
091900020308     c                   move      dataeur       dataiso
092000090331     c                   move      dataiso       wdatg
092100090424      * forzo la data di stampa se mi è stata passata (data certa)
092200090424     c                   if        pardatI > 0
092300090424     c                   z-add     pardatI       wdatg
092400090424     c                   endif
092500090428      *
092600090430     c                   movel     cmd(1)        comman
092700090430     C                   Z-ADD     80            LUNG
092800090430     C                   CALL      'QCMDEXC'
092900090430     C                   PARM                    COMMAN           80
093000090430     C                   PARM                    LUNG             15 5
093100090430     C                   OPEN      prtf198
093200090424      *_______________________________________________________________
093300120326     c                   if        paroutqI <> *blank
093400120326     c                   call      'TRMZ0100CO'
093500120326     c                   parm                    paroutqI
093600120326     c                   end
093700020313
093800090401      /FREE
093900090401
094000090401       trmz0100r( 'INIT'
094100090401                : prmRpyOpCode
094200090401                : prmRpyIdMsg
094300090401                );
094400090401
094500090401       IF prmRpyIdMsg < 0;
094600090401         RETURN;
094700090401       ENDIF;
094800090401
094900090401      /END-FREE
095000090401     c*
095100090401     c                   endsr
095200090330      * ?___________________________________________________________________
095300120222     C**************************************************************************
095400090506     Oprtf198   E            testata          02
095500090428     o                                           10 'TRMZ79R1'
095600090330     o                       knmus               22
095700090506     o                                        +   2 'Elenco Anomalie riscontrat-
095800090428     o                                              e nella stampa contratti e -
095900090428     o                                              allegati'
096000090508     O                       times            +   2 '  .  .  '
096100090508     o                       udate         y  +   2
096200090508     O                       page          z  +   2
096300090506     O          E            testata     2
096400090506     o                                           78 'D A T I  T A R I F F A'
096500090506     o                                        +   6 'D A T I  C E R T I F I C A'
096600090506     o                                        +   1 'Z I O N I '
096700090506     O          E            testata     2
096800090429     o                                           15 'Nome Autista'
096900090429     o                                           46 'Decorrenza'
097000090429     o                                           57 'Scadenza  '
097100090429     o                                           68 'Convalida '
097200090429     o                                           82 'Fornitore '
097300090506     o                                          100 'Società Autista'
097400090429     o                                          118 'Accreditato'
097500090429     o                                          129 'Disaccred.'
097600090429     o                                          140 'Dec. Contr.'
097700090429     o                                          151 'Scad.Contr.'
097800090429     o                                          160 'P.I.v.A.'
097900090506     O          E            testata     0
098000090506     o                                           15 'Nome Autista'
098100090506     o                                           46 'Decorrenza'
098200090506     o                                           57 'Scadenza  '
098300090506     o                                           68 'Convalida '
098400090506     o                                           82 'Fornitore '
098500090506     o                                          100 'Società Autista'
098600090506     o                                          118 'Accreditato'
098700090506     o                                          129 'Disaccred.'
098800090506     o                                          140 'Dec. Contr.'
098900090506     o                                          151 'Scad.Contr.'
099000090506     o                                          160 'P.I.v.A.'
099100090506     O          E            testata     0
099200090506     o                                           15 'Nome Autista'
099300090506     o                                           46 'Decorrenza'
099400090506     o                                           57 'Scadenza  '
099500090506     o                                           68 'Convalida '
099600090506     o                                           82 'Fornitore '
099700090506     o                                          100 'Società Autista'
099800090506     o                                          118 'Accreditato'
099900090506     o                                          129 'Disaccred.'
100000090506     o                                          140 'Dec. Contr.'
100100090506     o                                          151 'Scad.Contr.'
100200090506     o                                          160 'P.I.v.A.'
100300120606     O          E            TrazErrA    2
100400120606     o                       segnalaT           120
100500120606     o                       errmsgt            198
100600090424     O          E            AccrDeco    2
100700090429     o                       segnala            165
100800090429     o                       nota               198
100900090427     O          E            AccrDecoA   2
101000090429     o                       segnala            165
101100090429     o                       nota               198
101200090429     O          E            Noaut       2
101300090429     o                       segnala            165
101400090429     o                       nota               198
101500090427     O          E            NoautA      2
101600090429     o                       segnala            165
101700090429     o                       nota               198
101800110321     O          E            SocNoCor    2
101900110321     o                       segnala            165
102000110321     o                       nota               198
102100090427     O          E            Pivaerrore  2
102200090429     o                       segnala            165
102300090429     o                       nota               198
102400090331     O          E            Soccontsca  2
102500090429     o                       segnala            165
102600090429     o                       nota               198
102700090407     O          E            Soccontant  2
102800090429     o                       segnala            165
102900090429     o                       nota               198
103000090427     O          E            TarAntCon   2
103100090429     o                       segnala            165
103200090429     o                       nota               198
103300090427     O          E            TarAntConA  2
103400090429     o                       segnala            165
103500090429     o                       nota               198
103600090331     O          E            NoSoc       2
103700090429     o                       segnala            165
103800090429     o                       nota               198
103900090331     O          E            ErrStampa   2
104000090429     o                       segnala            165
104100090429     o                       nota               198
104200090430     O          E            NoFornit1   2
104300090430     o                       segnala            165
104400090430     o                       nota               198
104500090331     O          E            NoFornit    2
104600090429     o                       segnala            165
104700090429     o                       nota               198
104800090427     O          E            Allegato    2
104900090429     o                       segnala            165
105000090429     o                       nota               198
105100090427     O          E            Allegatod   2
105200090429     o                       segnala            165
105300090429     o                       nota               198
105400090511     O          E            NoDetTar    2
105500090511     o                       segnala            165
105600090511     o                       nota               198
105700120222AB   O          E            NoTrazio    2
105800120222     o                       segnala            165
105900120222     o                       nota               198
106000090430**
106100090430OVRPRTF FILE(prtf198) hold(*yes) save(*yes)
