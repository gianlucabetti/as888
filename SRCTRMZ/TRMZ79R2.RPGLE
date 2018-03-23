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
001100130116     FTNtbe01l  if   e           k disk
001200090430     Fprtf198   O    F  198        PRINTER prtctl(prtds) usropn
001300090331      * ?___________________________________________________________________
001400090331     D/COPY GAITRASRC/SRCPROTOPR,TRMZ0100R
001500090331     D prmRpyOpCode...
001600090331     D                 S             10A
001700090331     D prmRpyIdMsg...
001800090331     D                 S             10I 0
001900090331      * ?___________________________________________________________________
002000020304     dkpjba          e ds
002100090408     D trmz0100i     E DS                  prefix(x)
002200090408     D trmz70s1ds    E DS
002300090429     D aitrs00ds     E DS                  extname(aitrs00f)
002400090429     D aitra00ds     E DS                  extname(aitra00f)
002500120606     D DSaiats       E DS                  EXTNAME(AIATS00F)
002600111205     D trul13ds      E DS
002700111205     D dtgtflr       E DS
002800111205     D dtrsflr       E DS
002900130115     D dtrsflr1      E DS
003000130116     D TNTBZ79DS     E DS
003100130116
003200130116      *
003300090330     d prtds           ds
003400090330     d   spab                         3  0
003500090330     d   spaa                         3  0
003600090330     d   skab                         3  0
003700090330     d   skaa                         3  0
003800090330     d   line                         3  0
003900090408     d paramI          ds
004000090424     d   parfilI                      3  0
004100090428     d   pardatI                      8  0
004200090428     d   paroutqI                    10
004300090407     d param           ds
004400090407     d   partip                       1
004500090407     d   parpdrda                     7  0
004600090407     d   parpdra                      7  0
004700090407     d   pardat                       8  0
004800090424     d   pardtcert                    8  0
004900090428     d   parnrc                       7s 0
005000090428     d   parfil                       3s 0
005100090428     D   paroutq                     10
005200090427     d   parerr                       1
005300090526     d   parmsg                      35
005400111206     D pgmChiamante                  10
005500111206     d   pardfa                       8  0
005600090407     d paramdef        ds
005700090407     d   parpdrd                      7  0
005800090407     d   parprgd                      3  0
005900090407     d   partipd                      1
006000090424     d   pardtcertd                   8  0
006100090428     d   parnrcd                      7s 0
006200090428     d   parfild                      3s 0
006300090428     D   paroutqd                    10
006400090427     d   parerrd                      1
006500111206     d   pardfad                      8  0
006600120606     D partraz         DS
006700120606     D  parsoc                        3s 0
006800120606     D  pariva                       16
006900120606     D  pardtin                       8s 0
007000120606     D  pardtfi                       8s 0
007100120606     d  esito                         1
007200120606     d  errmsgt                      75
007300090331      * ?___________________________________________________________________
007400090429     d dataiso0        s               d   datfmt(*iso)
007500020305     d dataiso         s               d   datfmt(*iso)
007600020305     d dataeur         s               d   datfmt(*eur)
007700090331     d data0           s              8  0
007800090525     d kpdr            s                   like(adtpdr)
007900090525     d autistas        s                   like(traaut)
008000130117     d oggi            s              8  0
008100090331     d wdatg           s              8  0
008200090331     d wdat            s              8  0
008300090408     d*
008400090401     d No_allegato     s              1
008500090330     d TipoAut         s              1    inz('C')
008600090429     d segnala         s            165
008700120606     d segnalaT        s            120
008800090429     d nota            s             33
008900090430     D CMD             S             80    DIM(1) CTDATA PERRCD(1)              OVRPRTF
009000000000     C*---------------------------------------------------------------*
009100090330      * ?______________  main   ____________________________________________
009200090330     c* lettura allegati tariffari da stampare autisti città
009300090330     c     *loval        setll     fitgt02l
009400090109     c                   do        *hival
009500090330     c                   read      fitgt02l
009600090330     c                   if        %eof(fitgt02l)
009700090109     c                   leave
009800090109     c                   end
009900120322      * se impostata scelta singola filiale scarto le tariffe delle altre
010000120322     c                   movel     tgtpdr        pdr3              3 0
010100130116     c                   z-add     pdr3          Filiale           3 0
010200120322     c                   if        parfilI > 0 and pdr3 > parfilI
010300120322     c                   leave
010400120322     c                   end
010500120322     c                   if        parfilI > 0 and pdr3 <> parfilI
010600120322     c                   iter
010700120322     c                   end
010800090519      *non considero le tariffe con decorrenza scadenza al 01/05/2009
010900090519     c                   if        tgtddt = 20090501 and tgtdst = 20090501
011000090519     c                   iter
011100090519     c                   end
011200090623      *non considero le tariffe con scadenza maggiore di udate e data decorrenza maggiore di udate
011300090623     c                   if        tgtdst > wdatg or
011400090623     c                             tgtddt > wdatg
011500090617     c                   iter
011600090617     c                   end
011700130116      ****
011800130116      *   controlla se c'è in corso un cambio societario
011900130116      *     e quindi
012000130116      *   sulla tabella "Z79" esiste un record x la filiale
012100130116     C                   move      'Z79'         tbeCOD
012200130116     C                   MOVEl(p)  Filiale       tbeKE1
012300130116     c     ktbe1         chain     tntbe01l
012400130116     c                   if        %FOUND(tntbe01l)
012500130116     c                   movel     tbeuni        TNTBZ79DS
012600130116     c                   move      z79daDATA     DataTrasco        8 0
012700130116      **  se la data decorrenza tariffa è successiva alla data di inizio validità
012800130116      *    della nuova società e non si è ancora trascodificato ossia siamo
012900130116      *     temporalemente prima della data di Trascodifica,
013000130116      *      si deve bloccare la tariffa.
013100130117     c                   if        DataTrasco >= oggi
013200130116     c                                and
013300130116     c                             DataTrasco <= tgtddt
013400130116      * salta la tariffa
013500130116     c                   iter
013600130116     c                   end
013700130116     c                   endif
013800130116      ****
013900090519      *
014000090430     c                   clear                   no_allegato
014100090330     c                   z-add     tgtpdr        kpdr
014200090429     c                   clear                   aitrs00ds
014300090429     c                   clear                   aitra00ds
014400090506      * a rottura di filiale ristampo la testata del report anomalie
014500090519     c                   if        line > 62
014600090506     c                   except    testata
014700090506     c                   endif
014800090330     c                   exsr      trovata
014900090325     c                   enddo
015000090112     c*
015100090330     c* lettura allegati tariffari da stampare autisti AFF/DEF
015200090508     c                   movel     'A'           TipoAut
015300090330     c     *loval        setll     fiadt02l
015400090330     c                   do        *hival
015500090330     c                   read      fiadt02l
015600090330     c                   if        %eof(fiadt02l)
015700090330     c                   leave
015800090330     c                   end
015900120322      * se impostata scelta singola filiale scarto le tariffe delle altre
016000120322     c                   movel     adtpdr        pdr3
016100130116     c                   z-add     pdr3          Filiale
016200120322     c                   if        parfilI > 0 and pdr3 > parfilI
016300120322     c                   leave
016400120322     c                   end
016500120322     c                   if        parfilI > 0 and pdr3 <> parfilI
016600120322     c                   iter
016700120322     c                   end
016800090519      *non considero le tariffe con decorrenza scadenza al 01/05/2009
016900090519     c                   if        adtddt = 20090501 and adtdst = 20090501
017000090519     c                   iter
017100090519     c                   end
017200090623      *non considero le tariffe con scadenza maggiore di udate e data decorrenza maggiore di udate
017300090623     c                   if        adtdst > wdatg or  adtddt > wdatg
017400090618     c                   iter
017500090618     c                   end
017600130116      ****
017700130116      *   controlla se c'è in corso un cambio societario
017800130116      *     e quindi
017900130116      *   sulla tabella "Z79" esiste un record x la filiale
018000130116     C                   move      'Z79'         tbeCOD
018100130116     C                   MOVEl(p)  Filiale       tbeKE1
018200130116     c     ktbe1         chain     tntbe01l
018300130116     c                   if        %FOUND(tntbe01l)
018400130116     c                   movel     tbeuni        TNTBZ79DS
018500130116     c                   move      z79daDATA     DataTrasco
018600130116      **  se la data decorrenza tariffa è successiva alla data di inizio validità
018700130116      *    della nuova società e non si è ancora trascodificato ossia siamo
018800130116      *     temporalemente prima della data di Trascodifica,
018900130116      *      si deve bloccare la tariffa.
019000130117     c                   if        DataTrasco >= oggi
019100130116     c                                and
019200130116     c                             DataTrasco <= adtddt
019300130116      * salta la tariffa
019400130116     c                   iter
019500130116     c                   end
019600130116     c                   endif
019700130116      ****
019800090519      *
019900090430     c                   clear                   no_allegato
020000090330     c                   z-add     adtpdr        kpdr
020100090429     c                   clear                   aitrs00ds
020200090429     c                   clear                   aitra00ds
020300090506      * a rottura di filiale ristampo la testata del report anomalie
020400090519     c                   if        line > 62
020500090506     c                   except    testata
020600090506     c                   endif
020700090330     c                   exsr      trovatab
020800090330     c                   enddo
020900090330     c*
021000090401      /FREE
021100090401       trmz0100r( 'FINALIZE'
021200090401                : prmRpyOpCode
021300090401                : prmRpyIdMsg
021400090401                );
021500090401
021600090401      /END-FREE
021700090330     c                   seton                                        lr
021800090330      * ?___________________________________________________________________
021900090330     c     trovata       begsr
022000090330      * ?___________________________________________________________________
022100090330     c* verifico se l'autista è già stato inserito nell'anagrafica autisti automezzi
022200090330      * se esiste passo alle verifiche sulla società
022300090330      * altrimenti segnalo in stampa l'anomalia
022400090618     c     kaitra        setll     aitra03l
022500090618     c                   do        *hival
022600090624     c     kpdr          reade     aitra03l
022700090618     c                   if        %eof(aitra03l)
022800090619     c                   exsr      componi
022900090619     c                   eval      nota ='Non trovato autista in anagrafico'
023000090619     c                   except    NoAut
023100090430     c                   leavesr
023200090424     c                   endif
023300090618     c                   if        tgtddt >= tradin and
023400090623     c                             tgtddt <= tradfi
023500090427      *imposta dati fornitore della tariffa per confronto partita iva
023600090427     c                   move      tgtsoc        socfor
023700090427     c                   move      tgtcdf        codfor
023800090427      *
023900090330     c                   exsr      VerSocieta
024000090618     c                   leavesr
024100090424     c                   endif
024200090618     c                   enddo
024300130116      ****
024400090330     c                   endsr
024500090330      * ?___________________________________________________________________
024600090330     c     trovatab      begsr
024700090330      * ?___________________________________________________________________
024800090511     c                   clear                   addettagli
024900090511
025000090511     c     kadd          setll     fiadd01l
025100090511     c                   do        *hival
025200090511     c     kadd          reade     fiadd01l
025300090511     c                   if        %eof(fiadd01l)
025400090511     c                   leave
025500090511     c                   endif
025600090511     c                   if        addatb = *blank
025700090511     c                   move      'X'           addettagli        1
025800090511     c                   leave
025900090511     c                   endif
026000090511     c                   enddo
026100120222      *
026200090511     c                   if        addettagli = *blank
026300090511     c                   exsr      componia
026400090511     c                   eval      nota ='Non presente dettaglio tariffario'
026500090511     c                   except    NoDetTar
026600090511     c                   leavesr
026700090511     c                   endif
026800120222      *
026900120222      **
027000120222      **  Controlla se trazionista:
027100120606     c                   exsr      veriftraz
027200120717     c                   if        esito  <> ' '
027300120606     c                   if        esito  = '1'
027400120606     c                   eval      trsfil = 046
027500120606     c                   eval      trsnrc = atsnrc
027600120606     c                   exsr      AllegatoAd
027700120606     c                   else
027800120611     c                   if        atsdec = 0
027900120611     c                   move      dataiso0      atsdecs           8 0
028000120611     c                   else
028100120611     c                   z-add     atsdec        atsdecs
028200120611     c                   endif
028300120611     c                   if        atsdfc = 0
028400120611     c                   move      dataiso0      atsdfcs           8 0
028500120611     c                   else
028600120611     c                   z-add     atsdfc        atsdfcs
028700120611     c                   endif
028800120606     c                   eval      segnalaT= 'T' + ' ' +
028900120606     c                              %editc(kpdr:'Z') +' Dec. ' +
029000120606     c                              %char(%date(adtddt):*eur) + ' Scad. ' +
029100120606     c                              %char(%date(adtdst):*eur) + ' Stampa ' +
029200120606     c                              %char(%date(adtdts):*eur) + ' Dec.c. ' +
029300120611     c                              %char(%date(atsdecs):*eur) + ' Scad.c ' +
029400120611     c                              %char(%date(atsdfcs):*eur) + ' ' +
029500120606     c                             ' ' + adtcsf + ' ' +  adtcdf + ' ' +
029600120606     c                             ' ' + atspiva + ' '+%editc(atsnrc:'Z')
029700120606     c                   except    TrazErrA
029800120606     c                   leavesr
029900120606     c                   endif
030000120606      **
030100120222     c                   eLSe
030200120606      *
030300090330     c* verifico se l'autista è già stato inserito nell'anagrafica autisti automezzi
030400090330      * se esiste passo alle verifiche sulla società
030500090618      * altrimenti segnalo in stampa l'anomalia
030600090618     c     kaitra        setll     aitra05l
030700090618     c                   do        *hival
030800090624     c     kpdr          reade     aitra05l
030900090618     c                   if        %eof(aitra05l)
031000090619     c                   exsr      componia
031100090619     c                   eval      nota ='Non trovato autista in anagrafico'
031200090619     c                   except    NoAuta
031300090618     c                   leavesr
031400090618     c                   endif
031500090618     c                   if        adtddt >= tradin and
031600090623     c                             adtddt <= tradfi
031700090618      *imposta dati fornitore della tariffa per confronto partita iva
031800090618     c                   move      adtcsf        socfor
031900090618     c                   move      adtcdf        codfor
032000090618      *
032100090618     c                   exsr      VerSocieta
032200090618     c                   leavesr
032300090618     c                   endif
032400090618     c                   enddo
032500120222      **
032600120222      **  <CODICE>
032700120222     c                   eND
032800120222      **
032900090330     c                   endsr
033000120606     C**************************************************************************
033100120606     C*    CONTROLLo per a/d se il fornitore è un trazionista certificato
033200120606     C**************************************************************************
033300120606     C     veriftraz     BEGSR
033400120606     c     kfrn          klist
033500120606     C                   kfld                    adtcsf
033600120606     C                   kfld                    adtcdf
033700120607     c                   clear                   partraz
033800120606     c                   clear                   sogpartiva
033900120606     c     kfrn          chain     anfrn01l
034000120606     c                   if        %found(anfrn01l)
034100120606     c     frnsogg       chain     ansog01l
034200120606     c                   if        %found(ansog01l)
034300120606     c                   move      adtcsf        parsoc
034400120607     c                   move      adtddt        pardtin
034500120607     c                   move      adtdst        pardtfi
034600120606     c                   movel     sogpartiva    pariva
034700120606     c                   call      'TRULTRAZ'
034800120606     c                   parm                    partraz
034900120606     c                   parm                    dsAIATS
035000120606     c                   endif
035100120606     c                   endif
035200120606     c                   endsr
035300090330      * ?___________________________________________________________________
035400090330     c     VerSocieta    begsr
035500090330      * ?___________________________________________________________________
035600090401     c                   clear                   No_allegato
035700090427     c                   clear                   IvaForn          16
035800090427     c                   exsr      RepIvaFor
035900090330     c* verifico la società che sia stata inserita che sia ancora valida e lo stato
036000090330      * del contratto
036100090330      * se mancano i prerequisiti per la stampa dell'allegato li segnalo
036200090330     c     tranrc        chain     aitrs01l
036300090330     c                   if        %found(aitrs01l)
036400090330
036500090330     c                   select
036600090427      * Partita iva società diversa da quella del fornitore in tariffa
036700090427     c                   when      trsiva <> IvaForn
036800090428     c                   if        tipoaut = 'C'
036900090429     c                   exsr      componi
037000090428     c                   else
037100090429     c                   exsr      componia
037200090428     c                   endif
037300090429     c                   eval      nota ='P.Iva Società <> da P.Iva fornitore'
037400090427     c                   except    PivaErrore
037500090430     c                   leavesr
037600090427
037700090427      * società con contratto con data inferiore al minimo ammesso
037800090407     c                   when      trsdec < 20090501
037900090428     c                   if        tipoaut = 'C'
038000090429     c                   exsr      componi
038100090428     c                   else
038200090429     c                   exsr      componia
038300090428     c                   endif
038400090429     c                   eval      nota = 'Contratto antecedente data minima'
038500090407     c                   except    SocContAnt
038600090430     c                   leavesr
038700090407
038800090427      * Autista di città con tariffa antecedente decorrenza contratto
038900090427     c                   when      Tipoaut = 'C' and trsdec > tgtddt
039000090429     c                   exsr      componi
039100090429     c                   eval      nota =
039200090429     c                             'tariffa antecedente decorrenza contratto'
039300090427     c                   except    TarAntCon
039400090430     c                   leavesr
039500090427
039600090427      * Autista Aff/Def con tariffa antecedente decorrenza contratto
039700090508     c                   when      Tipoaut = 'A' and trsdec > adtddt
039800090429     c                   exsr      componia
039900090429     c                   eval      nota=
040000090429     c                             'tariffa antecedente decorrenza contratto'
040100090427     c                   except    TarAntConA
040200090430     c                   leavesr
040300090427
040400090330      * società con contratto da stampare
040500090401     c                   when      trsdsc = 0
040600130116      *
040700130116     c                   z-add     trsFIL        Filiale
040800130116      ****
040900130116      *   controlla se c'è in corso un cambio societario
041000130116      *     e quindi
041100130116      *   sulla tabella "Z79" esiste un record x la filiale
041200130116     C                   move      'Z79'         tbeCOD
041300130116     C                   MOVEl(p)  Filiale       tbeKE1
041400130116     c     ktbe1         chain     tntbe01l
041500130116     c                   if        %FOUND(tntbe01l)
041600130116     c                   movel     tbeuni        TNTBZ79DS
041700130116     c                   move      z79daDATA     DataTrasco
041800130116      **  se la data decorrenza tariffa è successiva alla data di inizio validità
041900130116      *    della nuova società e non si è ancora trascodificato ossia siamo
042000130116      *     temporalemente prima della data di Trascodifica,
042100130116      *      si deve bloccare la tariffa.
042200130117     c                   if        DataTrasco >= oggi
042300130116     c                                and
042400130116     c                             DataTrasco <= trsdec
042500130116      * salta la stampa
042600130116     c                   goto      In_trascodific
042700130116     c                   end
042800130116     c                   endif
042900130116      **
043000090330     c                   exsr      Contratto
043100090401      * problemi nella stampa contratto quindi non stampo allegato tariffario
043200090401     c                   if        No_allegato = *blank
043300090330     c                   if        TipoAut = 'C'
043400090330     c                   exsr      AllegatoCit
043500090330     c                   else
043600090330     c                   exsr      AllegatoAd
043700090330     c                   endif
043800090401     c                   endif
043900130116
044000130116      * se la Filiale è in trascodifica il contratto non è stampabile
044100130116      *  se la decorrenza è successiva alla data di trascodifica
044200130116      *   in un momento antecedente la trascodifica.
044300130116     c     In_trascodificTAG
044400090330
044500090330      * società con contratto stampato stampa solo l'allegato autista
044600090330     c                   other
044700090430     c                   if        No_allegato = *blank
044800090330     c                   if        TipoAut = 'C'
044900090330     c                   exsr      AllegatoCit
045000090330     c                   else
045100090330     c                   exsr      AllegatoAd
045200090430     c                   endif
045300090330     c                   endif
045400090330     c                   endsl
045500130116      *
045600090330     c                   else
045700090330      * società non presente
045800090428     c                   if        TipoAut = 'C'
045900090429     c                   exsr      componi
046000090428     c                   else
046100090429     c                   exsr      componia
046200090428     c                   endif
046300090429     c                   eval      nota =
046400090429     c                             'Società non trovata in Anagrafica società'
046500090330     c                   except    NoSoc
046600090330     c                   endif
046700090330     c                   endsr
046800090330      * ?___________________________________________________________________
046900090330     c     Contratto     begsr
047000090330      * ?___________________________________________________________________
047100090330     c* Richiamo PGM per stampa del contratto
047200090330     c                   exsr      RagSocCont
047300090330     c                   if        trovato = *on
047400090330     c                   clear                   prmRpyOpCode
047500090330     c                   clear                   prmRpyIdMsg
047600090330     c                   clear                   trmz0100i
047700090401     c                   move      trsdec        dataiso
047800090408     c                   eval      xCONTRATDAT  = dataiso
047900090408     c                   eval      xCONTRATNUM  = trsnrc
048000090408     c                   eval      xSOCIETA     = trssocg
048100090408     c                   eval      xFILIALE     = trsfil
048200090408     c                   eval      xVETRAGSOC   = %trim(ragsocksc) + ' ' +
048300090407     c                                          %trim(ragsocksc1)
048400090408     c                   eval      xVETSLCOM    = %trim(localita)
048500090408     c                   eval      xVETSLIND    = %trim(indirizzo)
048600090408     c                   eval      xVETSLCAP    = %trim(cap)
048700090408     c                   eval      xVETSLPRV    = provincia
048800090408     c                   eval      xVETSLNAZ    = stato
048900090408     c                   eval      xVETCDFISCA  = CodiceFisc
049000090408     c                   eval      xVETCDIVA    = trsiva
049100090408     c                   eval      xVETRIMLUO   = %trim(trscit)
049200090408     c                   eval      xVETRIMNUM   = trsnis
049300090408     c                   eval      xVETLEGRAP   = %trim(trslra)
049400090408     c                   eval      xVETANAPRV   = trspia
049500090408     c                   eval      xVETANANUM   = trsnia
049600090424     c                   move      wdatg         dataiso
049700090424     c                   eval      xdatastampa = dataiso
049800111205     c                   eval      xdatacertap = dataiso
049900111206     c* data firma (1° data certa + 1 gg lavorativo)
050000111207     C                   clear                   trul13ds
050100111206     C                   move      dataiso       i13din
050200111207     C                   EVAL      i13fil = trsfil
050300111207     C                   EVAL      i13gio = 1
050400111205     C                   CALL      'TRUL13R'
050500111205     C                   PARM                    trul13ds
050600111205     C                   IF        o13err <> 'E'
050700111205     c                   move      o13dfi        dataiso
050800111205     c                   eval      xDATAFIRMAV = dataiso
050900111205     c                   end
051000120326     c
051100090401      /FREE
051200090401
051300090401       trmz0100r( 'PRTCONTRAT'
051400090401                : prmRpyOpCode
051500090401                : prmRpyIdMsg
051600090401                : 'TRMZ0100I'
051700090401                : trmz0100i
051800090401                : %SIZE(trmz0100i)
051900090401                );
052000090401
052100090401       IF prmRpyIdMsg < 0;
052200090401         // Stampa non riuscita.
052300090401       ENDIF;
052400090401
052500090401      /END-FREE
052600120326
052700090330      * se non c'è errore aggiorno data stampa contratto
052800090330     c                   if        prmrpyidmsg = 0
052900090330     c                   z-add     wdatg         trsdsc
053000111205     c                   movel     trsflr        dtrsflr
053100111205     c                   move      wdatg         §trsunodc
053200111205     c                   move      o13dfi        §trsdfa
053300111205     c                   eval      trsflr = dtrsflr
053400130115      **
053500130115      * imposta la società stampata
053600130115     c                   movel     trsflr1       dtrsflr1
053700130115     c                   eval      §trsSOCprt = trsSOCG
053800130115     c                   eval      trsflr1    = dtrsflr1
053900130115      **
054000090401     c                   update    aitrs000
054100090330     c                   else
054200090330      * c'è errore stampo la segnalazione
054300090428     c                   if        tipoaut = 'C'
054400090429     c                   exsr      componi
054500090428     c                   else
054600090429     c                   exsr      componia
054700090428     c                   endif
054800090430     c                   eval      nota ='Errore nella stampa del contratto'
054900090330     c                   except    ErrStampa
055000090401     c                   eval      no_allegato = '1'
055100090330     c                   endif
055200090330     c                   else
055300090428     c                   if        tipoaut = 'C'
055400090429     c                   exsr      componi
055500090429     c                   else
055600090429     c                   exsr      componia
055700090428     c                   endif
055800090430     c                   eval      nota ='Non trovato fornitore'
055900090330     c                   except    NoFornit
056000090401     c                   eval      no_allegato = '1'
056100090330     c                   endif
056200090330     c                   endsr
056300090330      * ?___________________________________________________________________
056400090330     c     AllegatoCit   begsr
056500090330      * ?___________________________________________________________________
056600090330     c* Richiamo PGM per stampa allegato autista di città
056700090428     c                   clear                   param
056800090407     c                   z-add     tgtpdr        parpdrda
056900090407     c                   z-add     tgtpdr        parpdra
057000090427     c                   z-add     tgtddt        pardat
057100090407     c                   move      'C'           partip
057200090424     c                   z-add     wdatg         pardtcert
057300090428     c                   z-add     trsfil        parfil
057400090428     c                   z-add     trsnrc        parnrc
057500090428     c                   move      paroutqI      paroutq
057600111206     c                   if        tgtddt < wdatg
057700111206     c                   move      tgtddt        pardfa
057800111206     c                   else
057900111206     c                   move      wdatg         pardfa
058000111206     c                   end
058100090407     c                   movel     param         kpjbu
058200090428     c                   call      'FICN14R'
058300090330     c                   parm                    kpjba
058400090427     c                   movel     kpjbu         param
058500090427     c                   if        parerr = *blank
058600090406     c                   z-add     trsfil        tgtfil
058700090330     c                   z-add     trsnrc        tgtnrc
058800090330     c                   z-add     wdatg         tgtdcn
058900111205     c                   movel     tgtflr        dtgtflr
059000111205     c                   move      wdatg         §tgtunodc
059100111205     c                   if        tgtddt < wdatg
059200111205     c                   move      tgtddt        §tgtdfa
059300111205     c                   else
059400111205     c                   move      wdatg         §tgtdfa
059500111205     c                   end
059600111205     c                   eval      tgtflr = dtgtflr
059700090401     c                   update    fitgt000
059800090427     c                   else
059900090429     c                   exsr      componi
060000090526     c                   eval      nota = parmsg
060100090427     c                   except    Allegato
060200090427     c                   endif
060300090424     c*
060400090407     c                   endsr
060500090330      * ?___________________________________________________________________
060600090330     c     AllegatoAd    begsr
060700090330      * ?___________________________________________________________________
060800090330     c* Richiamo PGM per stampa allegato autista di AFF/DEF
060900090428     c                   clear                   paramdef
061000090407     c                   z-add     adtpdr        parpdrd
061100090407     c                   z-add     adtprg        parprgd
061200090407     c                   move      'C'           partipd
061300090424     c                   z-add     wdatg         pardtcertd
061400120606      *fittizzi se trazionista sono forzati da aiats
061500090428     c                   z-add     trsfil        parfild
061600090428     c                   z-add     trsnrc        parnrcd
061700120606      *------
061800090428     c                   move      paroutqI      paroutqd
061900111206     c                   if        adtddt < wdatg
062000111206     c                   z-add     adtddt        pardfad
062100111206     c                   else
062200111206     c                   z-add     wdatg         pardfad
062300111206     c                   end
062400090427     c                   movel     paramdef      kpjbu
062500090406     c                   call      'FICNA6C'
062600090330     c                   parm                    kpjba
062700090427     c                   movel     kpjbu         paramdef
062800090427     c                   if        parerrd = *blank
062900090406     c                   z-add     trsfil        adtfil
063000090330     c                   z-add     trsnrc        adtnrc
063100090330     c                   z-add     wdatg         adtdcn
063200111205     c                   z-add     wdatg         adtunodc
063300111205     c                   if        adtddt < wdatg
063400111205     c                   z-add     adtddt        adtdfa
063500111205     c                   else
063600111205     c                   z-add     wdatg         adtdfa
063700111205     c                   end
063800090401     c                   update    fiadt000
063900090427     c                   else
064000090429     c                   exsr      componia
064100090512     c                   select
064200090512     c                   when      parerrd = '1'
064300090512     c                   eval      nota ='Testata tariffa annullata '
064400090512     c                   when      parerrd = '2'
064500090512     c                   eval      nota ='Testata tariffa non trovata '
064600090512     c                   when      parerrd = '3'
064700090512     c                   eval      nota ='Anagrafica autista annullata '
064800090512     c                   endsl
064900090427     c                   except    AllegatoD
065000090427     c                   endif
065100090330     c                   endsr
065200090330      * ?___________________________________________________________________
065300090330     C     RagSocCont    BegSR
065400090330      * ?___________________________________________________________________
065500090330      *
065600090408     C                   clear                   Trmz70s1ds                     Output
065700090330     C                   movel(p)  TRSiva        PartitaIVA                     Input
065800090408     C                   movel(p)  'F'           SottoNatur                     Input "F/C"
065900090330     C                   movel(p)  TRSFIL        Unita                          Input/Output
066000090608     C                   move      TrsSOCg       Societa                        Input/Output
066100090526      *asteriscate nella stampa del contratto non serve
066200090526     c*                  if        tipoaut = 'C'
066300090526     C*                  move      TgtSOC        Societa                        Input/Output
066400090526     C*                  move      tgtcdf        keyksc                         Input/Output
066500090526     c*                  else
066600090526     C*                  move      adtcsf        Societa                        Input/Output
066700090526     C*                  move      adtcdf        keyksc                         Input/Output
066800090526     c*                  endif
066900090330      *
067000090330     c                   call      'TRMZ70SR1'
067100090408     C                   PARM                    trmz70s1ds
067200090430     c                   if        errore <> '0'
067300090430     c                   if        tipoaut = 'C'
067400090430     c                   exsr      componi
067500090430     c                   else
067600090430     c                   exsr      componia
067700090430     c                   endif
067800090430     c                   eval      nota = errmsg
067900090430     c                   eval      no_allegato = '1'
068000090430     c                   except    NoFornit1
068100090430     c                   endif
068200090330     c*
068300090330     C                   ENDSR
068400090427      * ?___________________________________________________________________
068500090427     C     RepIvaFor     BegSR
068600090427      * ?___________________________________________________________________
068700090427     c* reperisco la p.iva del fornitore immesso in apd
068800090427     c     krco          chain     anfrn01l
068900090427     c                   if        %found(anfrn01l)
069000090427     c     frnsogg       chain     ansog01l
069100090427     c                   if        %found(ansog01l)
069200090427     c                   movel     sogpartiva    IvaForn
069300090427     c                   end
069400090427     c                   end
069500090428     c                   if        ivaforn = *blank
069600090428     c                   if        tipoaut = 'C'
069700090429     c                   exsr      componi
069800090428     c                   else
069900090429     c                   exsr      componia
070000090428     c                   endif
070100090430     c                   eval      nota ='Non trovato fornitore per la società'
070200090430     c                   eval      no_allegato = '1'
070300090428     c                   except    NoFornit
070400090428     c                   endif
070500090427      *
070600090427     C                   ENDSR
070700090429      * ?___________________________________________________________________
070800090429     C     componi       BegSR
070900090429      * ?___________________________________________________________________
071000090429     c                   if        tradin = 0
071100090429     c                   move      dataiso0      tradins           8 0
071200090429     c                   else
071300090429     c                   z-add     tradin        tradins
071400090429     c                   endif
071500090429     c                   if        tradfi = 0
071600090429     c                   move      dataiso0      tradfis           8 0
071700090429     c                   else
071800090429     c                   z-add     tradfi        tradfis
071900090429     c                   endif
072000090429     c                   if        trsdec = 0
072100090429     c                   move      dataiso0      trsdecs           8 0
072200090429     c                   else
072300090429     c                   z-add     trsdec        trsdecs
072400090429     c                   endif
072500090429     c                   if        trsdfc = 0
072600090429     c                   move      dataiso0      trsdfcs           8 0
072700090429     c                   else
072800090429     c                   z-add     trsdfc        trsdfcs
072900090429     c                   endif
073000090429      *
073100090618     c                   eval      autistas = traaut
073200090429     c                   eval      segnala= tipoaut + ' ' +%editc(kpdr:'Z')+
073300090525     c                             ' ' + autistas + ' ' +
073400090429     c                              %char(%date(tgtddt):*eur) + ' ' +
073500090429     c                              %char(%date(tgtdst):*eur) + ' ' +
073600090429     c                              %char(%date(tgtdts):*eur) + ' ' +
073700090429     c                             ' ' + tgtsoc + ' ' +  tgtcdf + ' ' + trasoc +
073800090429     c                              %char(%date(tradins):*eur) + ' ' +
073900090429     c                              %char(%date(tradfis):*eur) + ' ' +
074000090429     c                              %char(%date(trsdecs):*eur) + ' ' +
074100090429     c                              %char(%date(trsdfcs):*eur) + ' ' +
074200090429     c                             ' ' + trsiva + ' '
074300090429     c                   endsr
074400090429      * ?___________________________________________________________________
074500090429     C     componia      BegSR
074600090429      * ?___________________________________________________________________
074700090429     c                   if        tradin = 0
074800090429     c                   move      dataiso0      tradins           8 0
074900090429     c                   else
075000090429     c                   z-add     tradin        tradins
075100090429     c                   endif
075200090429     c                   if        tradfi = 0
075300090429     c                   move      dataiso0      tradfis           8 0
075400090429     c                   else
075500090429     c                   z-add     tradfi        tradfis
075600090429     c                   endif
075700090429     c                   if        trsdec = 0
075800090429     c                   move      dataiso0      trsdecs           8 0
075900090429     c                   else
076000090429     c                   z-add     trsdec        trsdecs
076100090429     c                   endif
076200090429     c                   if        trsdfc = 0
076300090429     c                   move      dataiso0      trsdfcs           8 0
076400090429     c                   else
076500090429     c                   z-add     trsdfc        trsdfcs
076600090429     c                   endif
076700090525     c                   eval      autistas = traaut
076800090429      *
076900090429     c                   eval      segnala= tipoaut + ' ' +%editc(kpdr:'Z')+
077000090525     c                             ' ' + autistas + ' ' +
077100090429     c                              %char(%date(adtddt):*eur) + ' ' +
077200090429     c                              %char(%date(adtdst):*eur) + ' ' +
077300090429     c                              %char(%date(adtdts):*eur) + ' ' +
077400090429     c                             ' ' + adtcsf + ' ' +  adtcdf + ' ' + trasoc +
077500090429     c                              %char(%date(tradins):*eur) + ' ' +
077600090429     c                              %char(%date(tradfis):*eur) + ' ' +
077700090429     c                              %char(%date(trsdecs):*eur) + ' ' +
077800090429     c                              %char(%date(trsdfcs):*eur) + ' ' +
077900090429     c                             trsiva + ' '
078000090429     c                   endsr
078100090330      * ?___________________________________________________________________
078200011026     c     *inzsr        begsr
078300090330      * ?___________________________________________________________________
078400020304     c     *entry        plist
078500020304     c                   parm                    kpjba
078600090619     c                   clear                   paramI
078700090619     c                   if        kpjbu <> *blank
078800090619     c                   movel     kpjbu         paramI
078900090619     c                   endif
079000020304
079100090520     c                   z-add     99            line
079200020304     c
079300090330     c     kaitra        klist
079400090331     c                   kfld                    kpdr
079500090331     c                   kfld                    data0
079600011026
079700090525     c     kapd          klist
079800090525     c                   kfld                    tipapd            1
079900090525     c                   kfld                    codapd            7 0
080000090525
080100090427     c     krco          klist
080200090427     c                   kfld                    socfor            3
080300090427     c                   kfld                    codfor            8
080400090427
080500090406     c     kfgt          klist
080600090406     c                   kfld                    tGTPDR
080700090406     c                   kfld                    tGTSML
080800090407     c                   kfld                    kTSR              1
080900090511      *
081000090511     c     kadd          klist
081100090511     c                   kfld                    adtpdr
081200090511     c                   kfld                    adtprg
081300130116     C*
081400130116     C* klist
081500130116     C     KTBE1         KLIST
081600130116     C                   KFLD                    tbeCOD
081700130116     C                   KFLD                    tbeKE1
081800130116     C*
081900090406
082000020218     C                   time                    w0120            14 0
082100090508     C                   movel     w0120         times             6 0
082200090331     C                   move      w0120         wdat
082300020308      * data da impostare su data stampa e convalida se lancio = 'C'
082400020308     c                   move      wdat          dataeur
082500020308     c                   move      dataeur       dataiso
082600090331     c                   move      dataiso       wdatg
082700090619      * forzo la data di stampa se mi è stata passata (data certa)
082800090619     c                   if        pardatI > 0
082900090619     c                   z-add     pardatI       wdatg
083000090619     c                   endif
083100090428      *
083200090430     c                   movel     cmd(1)        comman
083300090430     C                   Z-ADD     80            LUNG
083400090430     C                   CALL      'QCMDEXC'
083500090430     C                   PARM                    COMMAN           80
083600090430     C                   PARM                    LUNG             15 5
083700090430     C                   OPEN      prtf198
083800090424      *_______________________________________________________________
083900120326     c                   if        paroutqI <> *blank
084000120326     c                   call      'TRMZ0100CO'
084100120326     c                   parm                    paroutqI
084200120326     c                   end
084300020313
084400090401      /FREE
084500090401
084600090401       trmz0100r( 'INIT'
084700090401                : prmRpyOpCode
084800090401                : prmRpyIdMsg
084900090401                );
085000090401
085100090401       IF prmRpyIdMsg < 0;
085200090401         RETURN;
085300090401       ENDIF;
085400090401
085500090401      /END-FREE
085600090401     c*
085700090401     c                   endsr
085800090330      * ?___________________________________________________________________
085900090506     Oprtf198   E            testata          02
086000090618     o                                           10 'TRMZ79R2'
086100090330     o                       knmus               22
086200090506     o                                        +   2 'Elenco Anomalie riscontrat-
086300090428     o                                              e nella stampa contratti e -
086400090618     o                                              allegati RECUPERI'
086500090508     O                       times            +   2 '  .  .  '
086600090508     o                       udate         y  +   2
086700090508     O                       page          z  +   2
086800090506     O          E            testata     2
086900090506     o                                           78 'D A T I  T A R I F F A'
087000090506     o                                        +   6 'D A T I  C E R T I F I C A'
087100090506     o                                        +   1 'Z I O N I '
087200090506     O          E            testata     2
087300090429     o                                           15 'Nome Autista'
087400090429     o                                           46 'Decorrenza'
087500090429     o                                           57 'Scadenza  '
087600090429     o                                           68 'Convalida '
087700090429     o                                           82 'Fornitore '
087800090506     o                                          100 'Società Autista'
087900090429     o                                          118 'Accreditato'
088000090429     o                                          129 'Disaccred.'
088100090429     o                                          140 'Dec. Contr.'
088200090429     o                                          151 'Scad.Contr.'
088300090429     o                                          160 'P.I.v.A.'
088400090506     O          E            testata     0
088500090506     o                                           15 'Nome Autista'
088600090506     o                                           46 'Decorrenza'
088700090506     o                                           57 'Scadenza  '
088800090506     o                                           68 'Convalida '
088900090506     o                                           82 'Fornitore '
089000090506     o                                          100 'Società Autista'
089100090506     o                                          118 'Accreditato'
089200090506     o                                          129 'Disaccred.'
089300090506     o                                          140 'Dec. Contr.'
089400090506     o                                          151 'Scad.Contr.'
089500090506     o                                          160 'P.I.v.A.'
089600090506     O          E            testata     0
089700090506     o                                           15 'Nome Autista'
089800090506     o                                           46 'Decorrenza'
089900090506     o                                           57 'Scadenza  '
090000090506     o                                           68 'Convalida '
090100090506     o                                           82 'Fornitore '
090200090506     o                                          100 'Società Autista'
090300090506     o                                          118 'Accreditato'
090400090506     o                                          129 'Disaccred.'
090500090506     o                                          140 'Dec. Contr.'
090600090506     o                                          151 'Scad.Contr.'
090700090506     o                                          160 'P.I.v.A.'
090800120606     O          E            TrazErrA    2
090900120606     o                       segnalaT           120
091000120606     o                       errmsgt            198
091100090424     O          E            AccrDeco    2
091200090429     o                       segnala            165
091300090429     o                       nota               198
091400090427     O          E            AccrDecoA   2
091500090429     o                       segnala            165
091600090429     o                       nota               198
091700090429     O          E            Noaut       2
091800090429     o                       segnala            165
091900090429     o                       nota               198
092000090427     O          E            NoautA      2
092100090429     o                       segnala            165
092200090429     o                       nota               198
092300090427     O          E            Pivaerrore  2
092400090429     o                       segnala            165
092500090429     o                       nota               198
092600090331     O          E            Soccontsca  2
092700090429     o                       segnala            165
092800090429     o                       nota               198
092900090407     O          E            Soccontant  2
093000090429     o                       segnala            165
093100090429     o                       nota               198
093200090427     O          E            TarAntCon   2
093300090429     o                       segnala            165
093400090429     o                       nota               198
093500090427     O          E            TarAntConA  2
093600090429     o                       segnala            165
093700090429     o                       nota               198
093800090331     O          E            NoSoc       2
093900090429     o                       segnala            165
094000090429     o                       nota               198
094100090331     O          E            ErrStampa   2
094200090429     o                       segnala            165
094300090429     o                       nota               198
094400090430     O          E            NoFornit1   2
094500090430     o                       segnala            165
094600090430     o                       nota               198
094700090331     O          E            NoFornit    2
094800090429     o                       segnala            165
094900090429     o                       nota               198
095000090427     O          E            Allegato    2
095100090429     o                       segnala            165
095200090429     o                       nota               198
095300090427     O          E            Allegatod   2
095400090429     o                       segnala            165
095500090429     o                       nota               198
095600090511     O          E            NoDetTar    2
095700090511     o                       segnala            165
095800090511     o                       nota               198
095900090430**
096000090618OVRPRTF FILE(prtf198) hold(*yes) save(*yes) FORMTYPE('RECUPERI')
