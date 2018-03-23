000100090109     H DEBUG DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP(*CALLER)
000200000000     H*--------------------------------------------------------------*
000300090519     Ffitgt01l  if   e           k disk
000400090519     Ffifgt01l  if   e           k disk
000500090519     Ffiadt01l  if   e           k disk
000600090519     Ffiadd01l  if   e           k disk
000700090519     Faitrs01l  if   e           k disk
000800090330     Faitra03l  if   e           k disk
000900090427     Fansog01l  IF   E           K DISK
001000090427     Fanfrn01l  IF   E           K DISK
001100090330     Faitra05l  if   e           k disk    rename(aitra000:aitra5)
001900090331      * ?___________________________________________________________________
002000020304     dkpjba          e ds
002100090519     D trmzctrds     E DS
002200090408     D trmz70s1ds    E DS
002300090429     D aitrs00ds     E DS                  extname(aitrs00f)
002400090429     D aitra00ds     E DS                  extname(aitra00f)
005400090331      * ?___________________________________________________________________
005800090331     d data0           s              8  0
005900090331     d kpdr            s                   like(adtpdr)
006200090408     d*
006400090330     d TipoAut         s              1    inz('C')
006600090429     d nota            s             33
006800000000     C*---------------------------------------------------------------*
006801090522     c
007000090519     c* lettura allegati tariffari
007001090519     c                   if        dctrtipo <> 'A'
007002090519     c                   movel     'C'           TipoAut
007100090519     c     *loval        setll     fitgt01l
007200090109     c                   do        *hival
007300090519     c                   read      fitgt01l
007400090519     c                   if        %eof(fitgt01l)
007500090109     c                   leave
007600090109     c                   end
007601090519     c*
007602090519     c                   if        tgtsml <> ' '
007603090519     c                   iter
007604090519     c                   end
007605090519     c*
007606090519     c                   if        tgtatb <> ' '
007607090519     c                   iter
007608090519     c                   end
007609090519      * contratto obbligatorio
007610090519     c                   if        tgtnrc <> dctrnrc
007611090519     c                   iter
007612090519     c                   end
007700090519      * se singolo aut
007900090519     c                   if        tgtpdr <> dctrpdr and dctrpdr <> 0
008000090519     c                   iter
008100090428     c                   end
008101090519      * un solo allegato
008200090519     c                   if        tgtddt <> dctrddt and dctrddt <> 0
008300090408     c                   iter
008400090408     c                   end
008600090330     c                   z-add     tgtpdr        kpdr
008700090429     c                   clear                   aitrs00ds
008800090429     c                   clear                   aitra00ds
008801090522     c                   clear                   dctrerr
008802090522     c                   clear                   dctrmsg
009400090330     c                   exsr      trovata
009500090325     c                   enddo
009501090519     c                   end
009600090112     c*
009700090330     c* lettura allegati tariffari da stampare autisti AFF/DEF
009701090519     c                   if        dctrtipo <> 'C'
009800090508     c                   movel     'A'           TipoAut
009900090519     c     *loval        setll     fiadt01l
010000090330     c                   do        *hival
010100090519     c                   read      fiadt01l
010200090519     c                   if        %eof(fiadt01l)
010300090330     c                   leave
010400090330     c                   end
010405090519     c*
010406090519     c                   if        adtatb <> ' '
010407090519     c                   iter
010408090519     c                   end
010409090519      * contratto obbligatorio
010410090519     c                   if        adtnrc <> dctrnrc
010411090519     c                   iter
010412090519     c                   end
010413090519      * se singolo aut
010414090519     c                   if        adtpdr <> dctrpdr and dctrpdr <> 0
010415090519     c                   iter
010416090519     c                   end
010417090519      * un solo allegato
010418090519     c                   if        adtddt <> dctrddt and dctrddt <> 0
010419090519     c                   iter
010420090519     c                   end
011400090330     c                   z-add     adtpdr        kpdr
011500090429     c                   clear                   aitrs00ds
011600090429     c                   clear                   aitra00ds
011601090522     c                   clear                   dctrerr
011602090522     c                   clear                   dctrmsg
012200090330     c                   exsr      trovatab
012300090330     c                   enddo
012301090519     c                   end
012302090519     c                   movel     trmzctrds     kpjbu
013200090330     c                   seton                                        lr
013300090330      * ?___________________________________________________________________
013400090330     c     trovata       begsr
013500090330      * ?___________________________________________________________________
013600090330     c* verifico se l'autista è già stato inserito nell'anagrafica autisti automezzi
013700090330      * se esiste passo alle verifiche sulla società
013800090330      * altrimenti segnalo in stampa l'anomalia
013900090330     c     kaitra        chain     aitra03l
014000090608     c                   if        %found(aitra03l) and tranrc = dctrnrc
014100090427     c                   if        tradin > tgtddt
014300090429     c                   eval      nota ='Dt. accreditamento > decor.tariffa'
014400090519     c                   exsr      fine
014500090430     c                   leavesr
014600090424     c                   endif
014700090427      *imposta dati fornitore della tariffa per confronto partita iva
014800090427     c                   move      tgtsoc        socfor
014900090427     c                   move      tgtcdf        codfor
015000090427      *
015100090330     c                   exsr      VerSocieta
015200090330     c                   else
015300090424     c     kpdr          setgt     aitra03l
015400090424     c     kpdr          readpe    aitra03l
015500090529     c                   if        not %eof(aitra03l)
015501090529     c                   if        tradfi < tgtddt
015700090429     c                   eval      nota ='Disaccr. in ANAGRAFICA AUTOMEZZI'
015801090519     c                   exsr      fine
015900090430     c                   leavesr
015901090529     c                   end
016000090427     c                   else
016200090429     c                   eval      nota ='Non ancora inserito in ANAG.Autom.'
016301090519     c                   exsr      fine
016400090430     c                   leavesr
016500090330     c                   endif
016600090424     c                   endif
016700090330     c                   endsr
016800090330      * ?___________________________________________________________________
016900090330     c     trovatab      begsr
017000090330      * ?___________________________________________________________________
017100090511     c                   clear                   addettagli
017200090511
017300090511     c     kadd          setll     fiadd01l
017400090511     c                   do        *hival
017500090511     c     kadd          reade     fiadd01l
017600090511     c                   if        %eof(fiadd01l)
017700090511     c                   leave
017800090511     c                   endif
017900090511     c                   if        addatb = *blank
018000090511     c                   move      'X'           addettagli        1
018100090511     c                   leave
018200090511     c                   endif
018300090511     c                   enddo
018400090511     c                   if        addettagli = *blank
018600090511     c                   eval      nota ='Non presente dettaglio tariffario'
018601090519     c                   exsr      fine
018800090511     c                   leavesr
018900090511     c                   endif
019000090330     c* verifico se l'autista è già stato inserito nell'anagrafica autisti automezzi
019100090330      * se esiste passo alle verifiche sulla società
019200090330      * altrimenti segnalo in stampa l'anomalia
019300090330     c     kaitra        chain     aitra05l
019400090608     c                   if        %found(aitra05l) and tranrc = dctrnrc
019500090427     c                   if        tradin > adtddt
019700090429     c                   eval      nota ='Dt. accreditamento > decor.tariffa'
019801090519     c                   exsr      fine
019900090430     c                   leavesr
020000090427     c                   endif
020100090427     c                   move      adtcsf        socfor
020200090427     c                   move      adtcdf        codfor
020300090330     c                   exsr      VerSocieta
020400090330     c                   else
020500090427     c     kpdr          setgt     aitra05l
020600090427     c     kpdr          readpe    aitra05l
020700090511     c                   if        not %eof(aitra05l)
020900090429     c                   eval      nota ='Disaccr. in ANAGRAFICA AUTOMEZZI'
021001090519     c                   exsr      fine
021100090430     c                   leavesr
021200090427     c                   else
021400090429     c                   eval      nota ='Non ancora inserito in ANAG.Autom'
021501090519     c                   exsr      fine
021600090430     c                   leavesr
021700090427     c                   endif
021800090330     c                   endif
021900090330     c                   endsr
022000090330      * ?___________________________________________________________________
022100090330     c     VerSocieta    begsr
022200090330      * ?___________________________________________________________________
022400090427     c                   clear                   IvaForn          16
022500090427     c                   exsr      RepIvaFor
022600090330     c* verifico la società che sia stata inserita che sia ancora valida e lo stato
022700090330      * del contratto
022800090330      * se mancano i prerequisiti per la stampa dell'allegato li segnalo
022900090519     c     tranrc        chain     aitrs01l
023000090330     c                   if        %found(aitrs01l)
023100090330
023200090330     c                   select
023300090427      * Partita iva società diversa da quella del fornitore in tariffa
023400090427     c                   when      trsiva <> IvaForn
024000090429     c                   eval      nota ='P.Iva Società <> da P.Iva fornitore'
024100090519     c                   exsr      fine
024200090430     c                   leavesr
024300090427
024400090604      * società con contratto  annullata
024500090604     c                   when      trsann <> *blank
025100090604     c                   eval      nota ='Società con contratto annullato'
025200090519     c                   exsr      fine
025300090430     c                   leavesr
025400090330
025500090427      * società con contratto con data inferiore al minimo ammesso
025600090407     c                   when      trsdec < 20090501
026200090429     c                   eval      nota = 'Contratto antecedente data minima'
026300090519     c                   exsr      fine
026400090430     c                   leavesr
026500090407
026600090427      * Autista di città con tariffa antecedente decorrenza contratto
026700090427     c                   when      Tipoaut = 'C' and trsdec > tgtddt
026900090429     c                   eval      nota =
027000090429     c                             'tariffa antecedente decorrenza contratto'
027001090519     c                   exsr      fine
027200090430     c                   leavesr
027300090427
027400090427      * Autista Aff/Def con tariffa antecedente decorrenza contratto
027500090508     c                   when      Tipoaut = 'A' and trsdec > adtddt
027700090429     c                   eval      nota=
027800090429     c                             'tariffa antecedente decorrenza contratto'
027901090519     c                   exsr      fine
028000090430     c                   leavesr
028505090519     c                   other
028507090519     c                   exsr      RagSocCont
030400090330     c                   endsl
030500090330     c                   else
030600090330      * società non presente
031200090429     c                   eval      nota =
031300090429     c                             'Società non trovata in Anagrafica società'
031400090519     c                   exsr      fine
031500090330     c                   endif
031600090330     c                   endsr
047900090330      * ?___________________________________________________________________
048000090330     C     RagSocCont    BegSR
048100090330      * ?___________________________________________________________________
048200090330      *
048300090408     C                   clear                   Trmz70s1ds                     Output
048400090330     C                   movel(p)  TRSiva        PartitaIVA                     Input
048500090408     C                   movel(p)  'F'           SottoNatur                     Input "F/C"
048600090330     C                   movel(p)  TRSFIL        Unita                          Input/Output
048700090430     c                   if        tipoaut = 'C'
048800090430     C                   move      TgtSOC        Societa                        Input/Output
048900090430     C                   move      tgtcdf        keyksc                         Input/Output
049000090430     c                   else
049100090430     C                   move      adtcsf        Societa                        Input/Output
049200090430     C                   move      adtcdf        keyksc                         Input/Output
049300090430     c                   endif
049400090330      *
049500090330     c                   call      'TRMZ70SR1'
049600090408     C                   PARM                    trmz70s1ds
049700090430     c                   if        errore <> '0'
050300090430     c                   eval      nota = errmsg
050500090519     c                   exsr      fine
050600090430     c                   endif
050700090330     c*
050800090330     C                   ENDSR
050900090427      * ?___________________________________________________________________
051000090427     C     RepIvaFor     BegSR
051100090427      * ?___________________________________________________________________
051200090427     c* reperisco la p.iva del fornitore immesso in apd
051300090427     c     krco          chain     anfrn01l
051400090427     c                   if        %found(anfrn01l)
051500090427     c     frnsogg       chain     ansog01l
051600090427     c                   if        %found(ansog01l)
051700090427     c                   movel     sogpartiva    IvaForn
051800090427     c                   end
051900090427     c                   end
052000090428     c                   if        ivaforn = *blank
052600090430     c                   eval      nota ='Non trovato fornitore per la società'
052800090519     c                   exsr      fine
052900090428     c                   endif
053000090427      *
053100090427     C                   ENDSR
060400090330      * ?___________________________________________________________________
060500090519     c     fine          begsr
060501090519     c                   eval      dctrerr = '1'
060502090519     c                   eval      dctrmsg = nota
060503090519     c                   movel     trmzctrds     kpjbu
060504090519     c                   seton                                        lr
060505090519     c                   return
060506090519     C                   ENDSR
060507090519      * ?___________________________________________________________________
060508090519     c     *inzsr        begsr
060600090330      * ?___________________________________________________________________
060700020304     c     *entry        plist
060800020304     c                   parm                    kpjba
060900090519     c                   movel     kpjbu         trmzctrds
060901090522     c                   eval      dctrerr = '1'
060902090522     c                   eval      dctrmsg ='Allegati non trovati'
061400020304     c
061500090330     c     kaitra        klist
061600090331     c                   kfld                    kpdr
061700090331     c                   kfld                    data0
061800011026
061900090427     c     krco          klist
062000090427     c                   kfld                    socfor            3
062100090427     c                   kfld                    codfor            8
062200090427
062300090406     c     kfgt          klist
062400090406     c                   kfld                    tGTPDR
062500090406     c                   kfld                    tGTSML
062600090407     c                   kfld                    kTSR              1
062700090511      *
062800090511     c     kadd          klist
062900090511     c                   kfld                    adtpdr
063000090511     c                   kfld                    adtprg
063100090519     C                   ENDSR
