000100090109     H DEBUG DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP(*CALLER)
000200000000     H*--------------------------------------------------------------*
000300090330     Ffitgt02l  uf   e           k disk
000400090330     Ffiadt02l  uf   e           k disk
000500090511     Ffiadd01l  if   e           k disk
000600090330     Faitrs01l  uf   e           k disk
000700090330     Faitra03l  if   e           k disk
000800090427     Fansog01l  IF   E           K DISK
000900090427     Fanfrn01l  IF   E           K DISK
001000090330     Faitra05l  if   e           k disk    rename(aitra000:aitra5)
001100090430     Fprtf198   O    F  198        PRINTER prtctl(prtds) usropn
001200090331      * ?___________________________________________________________________
001300090331     D/COPY GAITRASRC/SRCPROTOPR,TRMZ0100R
001400090331     D prmRpyOpCode...
001500090331     D                 S             10A
001600090331     D prmRpyIdMsg...
001700090331     D                 S             10I 0
001800090331      * ?___________________________________________________________________
001900020304     dkpjba          e ds
002000090408     D trmz0100i     E DS                  prefix(x)
002100090408     D trmz70s1ds    E DS
002200090429     D aitrs00ds     E DS                  extname(aitrs00f)
002300090429     D aitra00ds     E DS                  extname(aitra00f)
002301120606     D DSaiats       E DS                  EXTNAME(AIATS00F)
002400111205     D trul13ds      E DS
002500111205     D dtgtflr       E DS
002600111205     D dtrsflr       E DS
002700090330     d prtds           ds
002800090330     d   spab                         3  0
002900090330     d   spaa                         3  0
003000090330     d   skab                         3  0
003100090330     d   skaa                         3  0
003200090330     d   line                         3  0
003300090408     d paramI          ds
003400090424     d   parfilI                      3  0
003500090428     d   pardatI                      8  0
003600090428     d   paroutqI                    10
003700090407     d param           ds
003800090407     d   partip                       1
003900090407     d   parpdrda                     7  0
004000090407     d   parpdra                      7  0
004100090407     d   pardat                       8  0
004200090424     d   pardtcert                    8  0
004300090428     d   parnrc                       7s 0
004400090428     d   parfil                       3s 0
004500090428     D   paroutq                     10
004600090427     d   parerr                       1
004700090526     d   parmsg                      35
004800111206     D pgmChiamante                  10
004900111206     d   pardfa                       8  0
005000090407     d paramdef        ds
005100090407     d   parpdrd                      7  0
005200090407     d   parprgd                      3  0
005300090407     d   partipd                      1
005400090424     d   pardtcertd                   8  0
005500090428     d   parnrcd                      7s 0
005600090428     d   parfild                      3s 0
005700090428     D   paroutqd                    10
005800090427     d   parerrd                      1
005900111206     d   pardfad                      8  0
005901120606     D partraz         DS
005902120606     D  parsoc                        3s 0
005903120606     D  pariva                       16
005904120606     D  pardtin                       8s 0
005905120606     D  pardtfi                       8s 0
005906120606     d  esito                         1
005907120606     d  errmsgt                      75
006000090331      * ?___________________________________________________________________
006100090429     d dataiso0        s               d   datfmt(*iso)
006200020305     d dataiso         s               d   datfmt(*iso)
006300020305     d dataeur         s               d   datfmt(*eur)
006400090331     d data0           s              8  0
006500090525     d kpdr            s                   like(adtpdr)
006600090525     d autistas        s                   like(traaut)
006700090331     d wdatg           s              8  0
006800090331     d wdat            s              8  0
006900090408     d*
007000090401     d No_allegato     s              1
007100090330     d TipoAut         s              1    inz('C')
007200090429     d segnala         s            165
007201120606     d segnalaT        s            120
007300090429     d nota            s             33
007400090430     D CMD             S             80    DIM(1) CTDATA PERRCD(1)              OVRPRTF
007500000000     C*---------------------------------------------------------------*
007600090330      * ?______________  main   ____________________________________________
007700090330     c* lettura allegati tariffari da stampare autisti città
007800090330     c     *loval        setll     fitgt02l
007900090109     c                   do        *hival
008000090330     c                   read      fitgt02l
008100090330     c                   if        %eof(fitgt02l)
008200090109     c                   leave
008300090109     c                   end
008400120322      * se impostata scelta singola filiale scarto le tariffe delle altre
008500120322     c                   movel     tgtpdr        pdr3              3 0
008600120322     c                   if        parfilI > 0 and pdr3 > parfilI
008700120322     c                   leave
008800120322     c                   end
008900120322     c                   if        parfilI > 0 and pdr3 <> parfilI
009000120322     c                   iter
009100120322     c                   end
009200090519      *non considero le tariffe con decorrenza scadenza al 01/05/2009
009300090519     c                   if        tgtddt = 20090501 and tgtdst = 20090501
009400090519     c                   iter
009500090519     c                   end
009600090623      *non considero le tariffe con scadenza maggiore di udate e data decorrenza maggiore di udate
009700090623     c                   if        tgtdst > wdatg or
009800090623     c                             tgtddt > wdatg
009900090617     c                   iter
010000090617     c                   end
010100090519      *
010200090430     c                   clear                   no_allegato
010300090330     c                   z-add     tgtpdr        kpdr
010400090429     c                   clear                   aitrs00ds
010500090429     c                   clear                   aitra00ds
010600090506      * a rottura di filiale ristampo la testata del report anomalie
010700090519     c                   if        line > 62
010800090506     c                   except    testata
010900090506     c                   endif
011000090330     c                   exsr      trovata
011100090325     c                   enddo
011200090112     c*
011300090330     c* lettura allegati tariffari da stampare autisti AFF/DEF
011400090508     c                   movel     'A'           TipoAut
011500090330     c     *loval        setll     fiadt02l
011600090330     c                   do        *hival
011700090330     c                   read      fiadt02l
011800090330     c                   if        %eof(fiadt02l)
011900090330     c                   leave
012000090330     c                   end
012100120322      * se impostata scelta singola filiale scarto le tariffe delle altre
012200120322     c                   movel     adtpdr        pdr3
012300120322     c                   if        parfilI > 0 and pdr3 > parfilI
012400120322     c                   leave
012500120322     c                   end
012600120322     c                   if        parfilI > 0 and pdr3 <> parfilI
012700120322     c                   iter
012800120322     c                   end
012900090519      *non considero le tariffe con decorrenza scadenza al 01/05/2009
013000090519     c                   if        adtddt = 20090501 and adtdst = 20090501
013100090519     c                   iter
013200090519     c                   end
013300090623      *non considero le tariffe con scadenza maggiore di udate e data decorrenza maggiore di udate
013400090623     c                   if        adtdst > wdatg or  adtddt > wdatg
013500090618     c                   iter
013600090618     c                   end
013700090519      *
013800090430     c                   clear                   no_allegato
013900090330     c                   z-add     adtpdr        kpdr
014000090429     c                   clear                   aitrs00ds
014100090429     c                   clear                   aitra00ds
014200090506      * a rottura di filiale ristampo la testata del report anomalie
014300090519     c                   if        line > 62
014400090506     c                   except    testata
014500090506     c                   endif
014600090330     c                   exsr      trovatab
014700090330     c                   enddo
014800090330     c*
014900090401      /FREE
015000090401       trmz0100r( 'FINALIZE'
015100090401                : prmRpyOpCode
015200090401                : prmRpyIdMsg
015300090401                );
015400090401
015500090401      /END-FREE
015600090330     c                   seton                                        lr
015700090330      * ?___________________________________________________________________
015800090330     c     trovata       begsr
015900090330      * ?___________________________________________________________________
016000090330     c* verifico se l'autista è già stato inserito nell'anagrafica autisti automezzi
016100090330      * se esiste passo alle verifiche sulla società
016200090330      * altrimenti segnalo in stampa l'anomalia
016300090618     c     kaitra        setll     aitra03l
016400090618     c                   do        *hival
016500090624     c     kpdr          reade     aitra03l
016600090618     c                   if        %eof(aitra03l)
016700090619     c                   exsr      componi
016800090619     c                   eval      nota ='Non trovato autista in anagrafico'
016900090619     c                   except    NoAut
017000090430     c                   leavesr
017100090424     c                   endif
017200090618     c                   if        tgtddt >= tradin and
017300090623     c                             tgtddt <= tradfi
017400090427      *imposta dati fornitore della tariffa per confronto partita iva
017500090427     c                   move      tgtsoc        socfor
017600090427     c                   move      tgtcdf        codfor
017700090427      *
017800090330     c                   exsr      VerSocieta
017900090618     c                   leavesr
018000090424     c                   endif
018100090618     c                   enddo
018200090330     c                   endsr
018300090330      * ?___________________________________________________________________
018400090330     c     trovatab      begsr
018500090330      * ?___________________________________________________________________
018600090511     c                   clear                   addettagli
018700090511
018800090511     c     kadd          setll     fiadd01l
018900090511     c                   do        *hival
019000090511     c     kadd          reade     fiadd01l
019100090511     c                   if        %eof(fiadd01l)
019200090511     c                   leave
019300090511     c                   endif
019400090511     c                   if        addatb = *blank
019500090511     c                   move      'X'           addettagli        1
019600090511     c                   leave
019700090511     c                   endif
019800090511     c                   enddo
019900120222      *
020000090511     c                   if        addettagli = *blank
020100090511     c                   exsr      componia
020200090511     c                   eval      nota ='Non presente dettaglio tariffario'
020300090511     c                   except    NoDetTar
020400090511     c                   leavesr
020500090511     c                   endif
020600120222      *
020700120222      **
020800120222      **  Controlla se trazionista:
021901120606     c                   exsr      veriftraz
021902120717     c                   if        esito  <> ' '
021903120606     c                   if        esito  = '1'
021904120606     c                   eval      trsfil = 046
021905120606     c                   eval      trsnrc = atsnrc
021906120606     c                   exsr      AllegatoAd
021907120606     c                   else
021908120611     c                   if        atsdec = 0
021909120611     c                   move      dataiso0      atsdecs           8 0
021910120611     c                   else
021911120611     c                   z-add     atsdec        atsdecs
021912120611     c                   endif
021913120611     c                   if        atsdfc = 0
021914120611     c                   move      dataiso0      atsdfcs           8 0
021915120611     c                   else
021916120611     c                   z-add     atsdfc        atsdfcs
021917120611     c                   endif
021918120606     c                   eval      segnalaT= 'T' + ' ' +
021919120606     c                              %editc(kpdr:'Z') +' Dec. ' +
021920120606     c                              %char(%date(adtddt):*eur) + ' Scad. ' +
021921120606     c                              %char(%date(adtdst):*eur) + ' Stampa ' +
021922120606     c                              %char(%date(adtdts):*eur) + ' Dec.c. ' +
021923120611     c                              %char(%date(atsdecs):*eur) + ' Scad.c ' +
021924120611     c                              %char(%date(atsdfcs):*eur) + ' ' +
021925120606     c                             ' ' + adtcsf + ' ' +  adtcdf + ' ' +
021926120606     c                             ' ' + atspiva + ' '+%editc(atsnrc:'Z')
021927120606     c                   except    TrazErrA
021928120606     c                   leavesr
021929120606     c                   endif
021930120606      **
022000120222     c                   eLSe
022001120606      *
022300090330     c* verifico se l'autista è già stato inserito nell'anagrafica autisti automezzi
022400090330      * se esiste passo alle verifiche sulla società
022500090618      * altrimenti segnalo in stampa l'anomalia
022600090618     c     kaitra        setll     aitra05l
022700090618     c                   do        *hival
022800090624     c     kpdr          reade     aitra05l
022900090618     c                   if        %eof(aitra05l)
023000090619     c                   exsr      componia
023100090619     c                   eval      nota ='Non trovato autista in anagrafico'
023200090619     c                   except    NoAuta
023300090618     c                   leavesr
023400090618     c                   endif
023500090618     c                   if        adtddt >= tradin and
023600090623     c                             adtddt <= tradfi
023700090618      *imposta dati fornitore della tariffa per confronto partita iva
023800090618     c                   move      adtcsf        socfor
023900090618     c                   move      adtcdf        codfor
024000090618      *
024100090618     c                   exsr      VerSocieta
024200090618     c                   leavesr
024300090618     c                   endif
024400090618     c                   enddo
024500120222      **
024600120222      **  <CODICE>
024700120222     c                   eND
024800120222      **
024900090330     c                   endsr
024901120606     C**************************************************************************
024902120606     C*    CONTROLLo per a/d se il fornitore è un trazionista certificato
024903120606     C**************************************************************************
024904120606     C     veriftraz     BEGSR
024905120606     c     kfrn          klist
024906120606     C                   kfld                    adtcsf
024907120606     C                   kfld                    adtcdf
024908120607     c                   clear                   partraz
024909120606     c                   clear                   sogpartiva
024910120606     c     kfrn          chain     anfrn01l
024911120606     c                   if        %found(anfrn01l)
024912120606     c     frnsogg       chain     ansog01l
024913120606     c                   if        %found(ansog01l)
024914120606     c                   move      adtcsf        parsoc
024915120607     c                   move      adtddt        pardtin
024916120607     c                   move      adtdst        pardtfi
024917120606     c                   movel     sogpartiva    pariva
024918120606     c                   call      'TRULTRAZ'
024919120606     c                   parm                    partraz
024920120606     c                   parm                    dsAIATS
024921120606     c                   endif
024922120606     c                   endif
024923120606     c                   endsr
025000090330      * ?___________________________________________________________________
025100090330     c     VerSocieta    begsr
025200090330      * ?___________________________________________________________________
025300090401     c                   clear                   No_allegato
025400090427     c                   clear                   IvaForn          16
025500090427     c                   exsr      RepIvaFor
025600090330     c* verifico la società che sia stata inserita che sia ancora valida e lo stato
025700090330      * del contratto
025800090330      * se mancano i prerequisiti per la stampa dell'allegato li segnalo
025900090330     c     tranrc        chain     aitrs01l
026000090330     c                   if        %found(aitrs01l)
026100090330
026200090330     c                   select
026300090427      * Partita iva società diversa da quella del fornitore in tariffa
026400090427     c                   when      trsiva <> IvaForn
026500090428     c                   if        tipoaut = 'C'
026600090429     c                   exsr      componi
026700090428     c                   else
026800090429     c                   exsr      componia
026900090428     c                   endif
027000090429     c                   eval      nota ='P.Iva Società <> da P.Iva fornitore'
027100090427     c                   except    PivaErrore
027200090430     c                   leavesr
027300090427
027400090427      * società con contratto con data inferiore al minimo ammesso
027500090407     c                   when      trsdec < 20090501
027600090428     c                   if        tipoaut = 'C'
027700090429     c                   exsr      componi
027800090428     c                   else
027900090429     c                   exsr      componia
028000090428     c                   endif
028100090429     c                   eval      nota = 'Contratto antecedente data minima'
028200090407     c                   except    SocContAnt
028300090430     c                   leavesr
028400090407
028500090427      * Autista di città con tariffa antecedente decorrenza contratto
028600090427     c                   when      Tipoaut = 'C' and trsdec > tgtddt
028700090429     c                   exsr      componi
028800090429     c                   eval      nota =
028900090429     c                             'tariffa antecedente decorrenza contratto'
029000090427     c                   except    TarAntCon
029100090430     c                   leavesr
029200090427
029300090427      * Autista Aff/Def con tariffa antecedente decorrenza contratto
029400090508     c                   when      Tipoaut = 'A' and trsdec > adtddt
029500090429     c                   exsr      componia
029600090429     c                   eval      nota=
029700090429     c                             'tariffa antecedente decorrenza contratto'
029800090427     c                   except    TarAntConA
029900090430     c                   leavesr
030000090427
030100090330      * società con contratto da stampare
030200090401     c                   when      trsdsc = 0
030300090330     c                   exsr      Contratto
030400090401      * problemi nella stampa contratto quindi non stampo allegato tariffario
030500090401     c                   if        No_allegato = *blank
030600090330     c                   if        TipoAut = 'C'
030700090330     c                   exsr      AllegatoCit
030800090330     c                   else
030900090330     c                   exsr      AllegatoAd
031000090330     c                   endif
031100090401     c                   endif
031200090330
031300090330      * società con contratto stampato stampa solo l'allegato autista
031400090330     c                   other
031500090430     c                   if        No_allegato = *blank
031600090330     c                   if        TipoAut = 'C'
031700090330     c                   exsr      AllegatoCit
031800090330     c                   else
031900090330     c                   exsr      AllegatoAd
032000090430     c                   endif
032100090330     c                   endif
032200090330     c                   endsl
032300090330     c                   else
032400090330      * società non presente
032500090428     c                   if        TipoAut = 'C'
032600090429     c                   exsr      componi
032700090428     c                   else
032800090429     c                   exsr      componia
032900090428     c                   endif
033000090429     c                   eval      nota =
033100090429     c                             'Società non trovata in Anagrafica società'
033200090330     c                   except    NoSoc
033300090330     c                   endif
033400090330     c                   endsr
033500090330      * ?___________________________________________________________________
033600090330     c     Contratto     begsr
033700090330      * ?___________________________________________________________________
033800090330     c* Richiamo PGM per stampa del contratto
033900090330     c                   exsr      RagSocCont
034000090330     c                   if        trovato = *on
034100090330     c                   clear                   prmRpyOpCode
034200090330     c                   clear                   prmRpyIdMsg
034300090330     c                   clear                   trmz0100i
034400090401     c                   move      trsdec        dataiso
034500090408     c                   eval      xCONTRATDAT  = dataiso
034600090408     c                   eval      xCONTRATNUM  = trsnrc
034700090408     c                   eval      xSOCIETA     = trssocg
034800090408     c                   eval      xFILIALE     = trsfil
034900090408     c                   eval      xVETRAGSOC   = %trim(ragsocksc) + ' ' +
035000090407     c                                          %trim(ragsocksc1)
035100090408     c                   eval      xVETSLCOM    = %trim(localita)
035200090408     c                   eval      xVETSLIND    = %trim(indirizzo)
035300090408     c                   eval      xVETSLCAP    = %trim(cap)
035400090408     c                   eval      xVETSLPRV    = provincia
035500090408     c                   eval      xVETSLNAZ    = stato
035600090408     c                   eval      xVETCDFISCA  = CodiceFisc
035700090408     c                   eval      xVETCDIVA    = trsiva
035800090408     c                   eval      xVETRIMLUO   = %trim(trscit)
035900090408     c                   eval      xVETRIMNUM   = trsnis
036000090408     c                   eval      xVETLEGRAP   = %trim(trslra)
036100090408     c                   eval      xVETANAPRV   = trspia
036200090408     c                   eval      xVETANANUM   = trsnia
036300090424     c                   move      wdatg         dataiso
036400090424     c                   eval      xdatastampa = dataiso
036500111205     c                   eval      xdatacertap = dataiso
036600111206     c* data firma (1° data certa + 1 gg lavorativo)
036700111207     C                   clear                   trul13ds
036800111206     C                   move      dataiso       i13din
036900111207     C                   EVAL      i13fil = trsfil
037000111207     C                   EVAL      i13gio = 1
037100111205     C                   CALL      'TRUL13R'
037200111205     C                   PARM                    trul13ds
037300111205     C                   IF        o13err <> 'E'
037400111205     c                   move      o13dfi        dataiso
037500111205     c                   eval      xDATAFIRMAV = dataiso
037600111205     c                   end
037700120326     c
037800090401      /FREE
037900090401
038000090401       trmz0100r( 'PRTCONTRAT'
038100090401                : prmRpyOpCode
038200090401                : prmRpyIdMsg
038300090401                : 'TRMZ0100I'
038400090401                : trmz0100i
038500090401                : %SIZE(trmz0100i)
038600090401                );
038700090401
038800090401       IF prmRpyIdMsg < 0;
038900090401         // Stampa non riuscita.
039000090401       ENDIF;
039100090401
039200090401      /END-FREE
039300120326
039400090330      * se non c'è errore aggiorno data stampa contratto
039500090330     c                   if        prmrpyidmsg = 0
039600090330     c                   z-add     wdatg         trsdsc
039700111205     c                   movel     trsflr        dtrsflr
039800111205     c                   move      wdatg         §trsunodc
039900111205     c                   move      o13dfi        §trsdfa
040000111205     c                   eval      trsflr = dtrsflr
040100090401     c                   update    aitrs000
040200090330     c                   else
040300090330      * c'è errore stampo la segnalazione
040400090428     c                   if        tipoaut = 'C'
040500090429     c                   exsr      componi
040600090428     c                   else
040700090429     c                   exsr      componia
040800090428     c                   endif
040900090430     c                   eval      nota ='Errore nella stampa del contratto'
041000090330     c                   except    ErrStampa
041100090401     c                   eval      no_allegato = '1'
041200090330     c                   endif
041300090330     c                   else
041400090428     c                   if        tipoaut = 'C'
041500090429     c                   exsr      componi
041600090429     c                   else
041700090429     c                   exsr      componia
041800090428     c                   endif
041900090430     c                   eval      nota ='Non trovato fornitore'
042000090330     c                   except    NoFornit
042100090401     c                   eval      no_allegato = '1'
042200090330     c                   endif
042300090330     c                   endsr
042400090330      * ?___________________________________________________________________
042500090330     c     AllegatoCit   begsr
042600090330      * ?___________________________________________________________________
042700090330     c* Richiamo PGM per stampa allegato autista di città
042800090428     c                   clear                   param
042900090407     c                   z-add     tgtpdr        parpdrda
043000090407     c                   z-add     tgtpdr        parpdra
043100090427     c                   z-add     tgtddt        pardat
043200090407     c                   move      'C'           partip
043300090424     c                   z-add     wdatg         pardtcert
043400090428     c                   z-add     trsfil        parfil
043500090428     c                   z-add     trsnrc        parnrc
043600090428     c                   move      paroutqI      paroutq
043700111206     c                   if        tgtddt < wdatg
043800111206     c                   move      tgtddt        pardfa
043900111206     c                   else
044000111206     c                   move      wdatg         pardfa
044100111206     c                   end
044200090407     c                   movel     param         kpjbu
044300090428     c                   call      'FICN14R'
044400090330     c                   parm                    kpjba
044500090427     c                   movel     kpjbu         param
044600090427     c                   if        parerr = *blank
044700090406     c                   z-add     trsfil        tgtfil
044800090330     c                   z-add     trsnrc        tgtnrc
044900090330     c                   z-add     wdatg         tgtdcn
045000111205     c                   movel     tgtflr        dtgtflr
045100111205     c                   move      wdatg         §tgtunodc
045200111205     c                   if        tgtddt < wdatg
045300111205     c                   move      tgtddt        §tgtdfa
045400111205     c                   else
045500111205     c                   move      wdatg         §tgtdfa
045600111205     c                   end
045700111205     c                   eval      tgtflr = dtgtflr
045800090401     c                   update    fitgt000
045900090427     c                   else
046000090429     c                   exsr      componi
046100090526     c                   eval      nota = parmsg
046200090427     c                   except    Allegato
046300090427     c                   endif
046400090424     c*
046500090407     c                   endsr
046600090330      * ?___________________________________________________________________
046700090330     c     AllegatoAd    begsr
046800090330      * ?___________________________________________________________________
046900090330     c* Richiamo PGM per stampa allegato autista di AFF/DEF
047000090428     c                   clear                   paramdef
047100090407     c                   z-add     adtpdr        parpdrd
047200090407     c                   z-add     adtprg        parprgd
047300090407     c                   move      'C'           partipd
047400090424     c                   z-add     wdatg         pardtcertd
047401120606      *fittizzi se trazionista sono forzati da aiats
047500090428     c                   z-add     trsfil        parfild
047600090428     c                   z-add     trsnrc        parnrcd
047601120606      *------
047700090428     c                   move      paroutqI      paroutqd
047800111206     c                   if        adtddt < wdatg
047900111206     c                   z-add     adtddt        pardfad
048000111206     c                   else
048100111206     c                   z-add     wdatg         pardfad
048200111206     c                   end
048300090427     c                   movel     paramdef      kpjbu
048400090406     c                   call      'FICNA6C'
048500090330     c                   parm                    kpjba
048600090427     c                   movel     kpjbu         paramdef
048700090427     c                   if        parerrd = *blank
048800090406     c                   z-add     trsfil        adtfil
048900090330     c                   z-add     trsnrc        adtnrc
049000090330     c                   z-add     wdatg         adtdcn
049100111205     c                   z-add     wdatg         adtunodc
049200111205     c                   if        adtddt < wdatg
049300111205     c                   z-add     adtddt        adtdfa
049400111205     c                   else
049500111205     c                   z-add     wdatg         adtdfa
049600111205     c                   end
049700090401     c                   update    fiadt000
049800090427     c                   else
049900090429     c                   exsr      componia
050000090512     c                   select
050100090512     c                   when      parerrd = '1'
050200090512     c                   eval      nota ='Testata tariffa annullata '
050300090512     c                   when      parerrd = '2'
050400090512     c                   eval      nota ='Testata tariffa non trovata '
050500090512     c                   when      parerrd = '3'
050600090512     c                   eval      nota ='Anagrafica autista annullata '
050700090512     c                   endsl
050800090427     c                   except    AllegatoD
050900090427     c                   endif
051000090330     c                   endsr
051100090330      * ?___________________________________________________________________
051200090330     C     RagSocCont    BegSR
051300090330      * ?___________________________________________________________________
051400090330      *
051500090408     C                   clear                   Trmz70s1ds                     Output
051600090330     C                   movel(p)  TRSiva        PartitaIVA                     Input
051700090408     C                   movel(p)  'F'           SottoNatur                     Input "F/C"
051800090330     C                   movel(p)  TRSFIL        Unita                          Input/Output
051900090608     C                   move      TrsSOCg       Societa                        Input/Output
052000090526      *asteriscate nella stampa del contratto non serve
052100090526     c*                  if        tipoaut = 'C'
052200090526     C*                  move      TgtSOC        Societa                        Input/Output
052300090526     C*                  move      tgtcdf        keyksc                         Input/Output
052400090526     c*                  else
052500090526     C*                  move      adtcsf        Societa                        Input/Output
052600090526     C*                  move      adtcdf        keyksc                         Input/Output
052700090526     c*                  endif
052800090330      *
052900090330     c                   call      'TRMZ70SR1'
053000090408     C                   PARM                    trmz70s1ds
053100090430     c                   if        errore <> '0'
053200090430     c                   if        tipoaut = 'C'
053300090430     c                   exsr      componi
053400090430     c                   else
053500090430     c                   exsr      componia
053600090430     c                   endif
053700090430     c                   eval      nota = errmsg
053800090430     c                   eval      no_allegato = '1'
053900090430     c                   except    NoFornit1
054000090430     c                   endif
054100090330     c*
054200090330     C                   ENDSR
054300090427      * ?___________________________________________________________________
054400090427     C     RepIvaFor     BegSR
054500090427      * ?___________________________________________________________________
054600090427     c* reperisco la p.iva del fornitore immesso in apd
054700090427     c     krco          chain     anfrn01l
054800090427     c                   if        %found(anfrn01l)
054900090427     c     frnsogg       chain     ansog01l
055000090427     c                   if        %found(ansog01l)
055100090427     c                   movel     sogpartiva    IvaForn
055200090427     c                   end
055300090427     c                   end
055400090428     c                   if        ivaforn = *blank
055500090428     c                   if        tipoaut = 'C'
055600090429     c                   exsr      componi
055700090428     c                   else
055800090429     c                   exsr      componia
055900090428     c                   endif
056000090430     c                   eval      nota ='Non trovato fornitore per la società'
056100090430     c                   eval      no_allegato = '1'
056200090428     c                   except    NoFornit
056300090428     c                   endif
056400090427      *
056500090427     C                   ENDSR
056600090429      * ?___________________________________________________________________
056700090429     C     componi       BegSR
056800090429      * ?___________________________________________________________________
056900090429     c                   if        tradin = 0
057000090429     c                   move      dataiso0      tradins           8 0
057100090429     c                   else
057200090429     c                   z-add     tradin        tradins
057300090429     c                   endif
057400090429     c                   if        tradfi = 0
057500090429     c                   move      dataiso0      tradfis           8 0
057600090429     c                   else
057700090429     c                   z-add     tradfi        tradfis
057800090429     c                   endif
057900090429     c                   if        trsdec = 0
058000090429     c                   move      dataiso0      trsdecs           8 0
058100090429     c                   else
058200090429     c                   z-add     trsdec        trsdecs
058300090429     c                   endif
058400090429     c                   if        trsdfc = 0
058500090429     c                   move      dataiso0      trsdfcs           8 0
058600090429     c                   else
058700090429     c                   z-add     trsdfc        trsdfcs
058800090429     c                   endif
058900090429      *
059000090618     c                   eval      autistas = traaut
059100090429     c                   eval      segnala= tipoaut + ' ' +%editc(kpdr:'Z')+
059200090525     c                             ' ' + autistas + ' ' +
059300090429     c                              %char(%date(tgtddt):*eur) + ' ' +
059400090429     c                              %char(%date(tgtdst):*eur) + ' ' +
059500090429     c                              %char(%date(tgtdts):*eur) + ' ' +
059600090429     c                             ' ' + tgtsoc + ' ' +  tgtcdf + ' ' + trasoc +
059700090429     c                              %char(%date(tradins):*eur) + ' ' +
059800090429     c                              %char(%date(tradfis):*eur) + ' ' +
059900090429     c                              %char(%date(trsdecs):*eur) + ' ' +
060000090429     c                              %char(%date(trsdfcs):*eur) + ' ' +
060100090429     c                             ' ' + trsiva + ' '
060200090429     c                   endsr
060300090429      * ?___________________________________________________________________
060400090429     C     componia      BegSR
060500090429      * ?___________________________________________________________________
060600090429     c                   if        tradin = 0
060700090429     c                   move      dataiso0      tradins           8 0
060800090429     c                   else
060900090429     c                   z-add     tradin        tradins
061000090429     c                   endif
061100090429     c                   if        tradfi = 0
061200090429     c                   move      dataiso0      tradfis           8 0
061300090429     c                   else
061400090429     c                   z-add     tradfi        tradfis
061500090429     c                   endif
061600090429     c                   if        trsdec = 0
061700090429     c                   move      dataiso0      trsdecs           8 0
061800090429     c                   else
061900090429     c                   z-add     trsdec        trsdecs
062000090429     c                   endif
062100090429     c                   if        trsdfc = 0
062200090429     c                   move      dataiso0      trsdfcs           8 0
062300090429     c                   else
062400090429     c                   z-add     trsdfc        trsdfcs
062500090429     c                   endif
062600090525     c                   eval      autistas = traaut
062700090429      *
062800090429     c                   eval      segnala= tipoaut + ' ' +%editc(kpdr:'Z')+
062900090525     c                             ' ' + autistas + ' ' +
063000090429     c                              %char(%date(adtddt):*eur) + ' ' +
063100090429     c                              %char(%date(adtdst):*eur) + ' ' +
063200090429     c                              %char(%date(adtdts):*eur) + ' ' +
063300090429     c                             ' ' + adtcsf + ' ' +  adtcdf + ' ' + trasoc +
063400090429     c                              %char(%date(tradins):*eur) + ' ' +
063500090429     c                              %char(%date(tradfis):*eur) + ' ' +
063600090429     c                              %char(%date(trsdecs):*eur) + ' ' +
063700090429     c                              %char(%date(trsdfcs):*eur) + ' ' +
063800090429     c                             trsiva + ' '
063900090429     c                   endsr
064000090330      * ?___________________________________________________________________
064100011026     c     *inzsr        begsr
064200090330      * ?___________________________________________________________________
064300020304     c     *entry        plist
064400020304     c                   parm                    kpjba
064500090619     c                   clear                   paramI
064600090619     c                   if        kpjbu <> *blank
064700090619     c                   movel     kpjbu         paramI
064800090619     c                   endif
064900020304
065000090520     c                   z-add     99            line
065100020304     c
065200090330     c     kaitra        klist
065300090331     c                   kfld                    kpdr
065400090331     c                   kfld                    data0
065500011026
065600090525     c     kapd          klist
065700090525     c                   kfld                    tipapd            1
065800090525     c                   kfld                    codapd            7 0
065900090525
066000090427     c     krco          klist
066100090427     c                   kfld                    socfor            3
066200090427     c                   kfld                    codfor            8
066300090427
066400090406     c     kfgt          klist
066500090406     c                   kfld                    tGTPDR
066600090406     c                   kfld                    tGTSML
066700090407     c                   kfld                    kTSR              1
066800090511      *
066900090511     c     kadd          klist
067000090511     c                   kfld                    adtpdr
067100090511     c                   kfld                    adtprg
067200090406
067300020218     C                   time                    w0120            14 0
067400090508     C                   movel     w0120         times             6 0
067500090331     C                   move      w0120         wdat
067600020308      * data da impostare su data stampa e convalida se lancio = 'C'
067700020308     c                   move      wdat          dataeur
067800020308     c                   move      dataeur       dataiso
067900090331     c                   move      dataiso       wdatg
068000090619      * forzo la data di stampa se mi è stata passata (data certa)
068100090619     c                   if        pardatI > 0
068200090619     c                   z-add     pardatI       wdatg
068300090619     c                   endif
068400090428      *
068500090430     c                   movel     cmd(1)        comman
068600090430     C                   Z-ADD     80            LUNG
068700090430     C                   CALL      'QCMDEXC'
068800090430     C                   PARM                    COMMAN           80
068900090430     C                   PARM                    LUNG             15 5
069000090430     C                   OPEN      prtf198
069100090424      *_______________________________________________________________
069200120326     c                   if        paroutqI <> *blank
069300120326     c                   call      'TRMZ0100CO'
069400120326     c                   parm                    paroutqI
069500120326     c                   end
069600020313
069700090401      /FREE
069800090401
069900090401       trmz0100r( 'INIT'
070000090401                : prmRpyOpCode
070100090401                : prmRpyIdMsg
070200090401                );
070300090401
070400090401       IF prmRpyIdMsg < 0;
070500090401         RETURN;
070600090401       ENDIF;
070700090401
070800090401      /END-FREE
070900090401     c*
071000090401     c                   endsr
071100090330      * ?___________________________________________________________________
071200090506     Oprtf198   E            testata          02
071300090618     o                                           10 'TRMZ79R2'
071400090330     o                       knmus               22
071500090506     o                                        +   2 'Elenco Anomalie riscontrat-
071600090428     o                                              e nella stampa contratti e -
071700090618     o                                              allegati RECUPERI'
071800090508     O                       times            +   2 '  .  .  '
071900090508     o                       udate         y  +   2
072000090508     O                       page          z  +   2
072100090506     O          E            testata     2
072200090506     o                                           78 'D A T I  T A R I F F A'
072300090506     o                                        +   6 'D A T I  C E R T I F I C A'
072400090506     o                                        +   1 'Z I O N I '
072500090506     O          E            testata     2
072600090429     o                                           15 'Nome Autista'
072700090429     o                                           46 'Decorrenza'
072800090429     o                                           57 'Scadenza  '
072900090429     o                                           68 'Convalida '
073000090429     o                                           82 'Fornitore '
073100090506     o                                          100 'Società Autista'
073200090429     o                                          118 'Accreditato'
073300090429     o                                          129 'Disaccred.'
073400090429     o                                          140 'Dec. Contr.'
073500090429     o                                          151 'Scad.Contr.'
073600090429     o                                          160 'P.I.v.A.'
073700090506     O          E            testata     0
073800090506     o                                           15 'Nome Autista'
073900090506     o                                           46 'Decorrenza'
074000090506     o                                           57 'Scadenza  '
074100090506     o                                           68 'Convalida '
074200090506     o                                           82 'Fornitore '
074300090506     o                                          100 'Società Autista'
074400090506     o                                          118 'Accreditato'
074500090506     o                                          129 'Disaccred.'
074600090506     o                                          140 'Dec. Contr.'
074700090506     o                                          151 'Scad.Contr.'
074800090506     o                                          160 'P.I.v.A.'
074900090506     O          E            testata     0
075000090506     o                                           15 'Nome Autista'
075100090506     o                                           46 'Decorrenza'
075200090506     o                                           57 'Scadenza  '
075300090506     o                                           68 'Convalida '
075400090506     o                                           82 'Fornitore '
075500090506     o                                          100 'Società Autista'
075600090506     o                                          118 'Accreditato'
075700090506     o                                          129 'Disaccred.'
075800090506     o                                          140 'Dec. Contr.'
075900090506     o                                          151 'Scad.Contr.'
076000090506     o                                          160 'P.I.v.A.'
076001120606     O          E            TrazErrA    2
076002120606     o                       segnalaT           120
076003120606     o                       errmsgt            198
076100090424     O          E            AccrDeco    2
076200090429     o                       segnala            165
076300090429     o                       nota               198
076400090427     O          E            AccrDecoA   2
076500090429     o                       segnala            165
076600090429     o                       nota               198
076700090429     O          E            Noaut       2
076800090429     o                       segnala            165
076900090429     o                       nota               198
077000090427     O          E            NoautA      2
077100090429     o                       segnala            165
077200090429     o                       nota               198
077300090427     O          E            Pivaerrore  2
077400090429     o                       segnala            165
077500090429     o                       nota               198
077600090331     O          E            Soccontsca  2
077700090429     o                       segnala            165
077800090429     o                       nota               198
077900090407     O          E            Soccontant  2
078000090429     o                       segnala            165
078100090429     o                       nota               198
078200090427     O          E            TarAntCon   2
078300090429     o                       segnala            165
078400090429     o                       nota               198
078500090427     O          E            TarAntConA  2
078600090429     o                       segnala            165
078700090429     o                       nota               198
078800090331     O          E            NoSoc       2
078900090429     o                       segnala            165
079000090429     o                       nota               198
079100090331     O          E            ErrStampa   2
079200090429     o                       segnala            165
079300090429     o                       nota               198
079400090430     O          E            NoFornit1   2
079500090430     o                       segnala            165
079600090430     o                       nota               198
079700090331     O          E            NoFornit    2
079800090429     o                       segnala            165
079900090429     o                       nota               198
080000090427     O          E            Allegato    2
080100090429     o                       segnala            165
080200090429     o                       nota               198
080300090427     O          E            Allegatod   2
080400090429     o                       segnala            165
080500090429     o                       nota               198
080600090511     O          E            NoDetTar    2
080700090511     o                       segnala            165
080800090511     o                       nota               198
080900090430**
081000090618OVRPRTF FILE(prtf198) hold(*yes) save(*yes) FORMTYPE('RECUPERI')
