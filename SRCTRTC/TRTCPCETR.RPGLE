000100080222     h decedit('0,') option(*nodebugio)
000200080222      *---------------------------------------------------------------*
000300080227      * Da FIPRD00R scrive file FIPCT00F per chiusura distinta e log
000400080227      * non deve fare commit o rolback perchè lo fa il chianmate (TRTCPC0R)
000500080222      *---------------------------------------------------------------*
000600080331     ftabel00f  if   e           k disk
000700120328     ftntbe01l  if   e           k disk
000800120508     ffipci01l  uf a e           k disk    commit
000900080226     ffiplg00f  o    e             disk    commit
001000080805     ffipct02l  uf a e           k disk    commit
001100131209     ffipct01l  uf a e           k disk    commit rename(fipct000:fipct1)
001200080222     d esito           s              1
001300080222     d prmfgs          s              3
001400080222     d prmnfv          s              6
001500080225     d prmaut          s              7
001600110606     d prmdfv          s              8
001700080222     d prmdoc          s             16
001800080222     d prmdatora       s             14
001900080805     d codaut          s                   like(pctcodaut)
002000120328     D WLBDAT          DS                  INZ
002100120328     D  G02DAT                 1      8  0
002200120328     D  G02INV                 9     16  0
002300120328     D  G02ERR                17     17
002400120328     D  G02TGI                18     22  0
002500080222     d* output cod. errore
002600140213     d saverr          s              1
002700080222     d prmerr          s              1
002800080222     d prmmsg          s             30
002900121015
003000131114     d pctoramin       s               t   timfmt(*iso)
003100131114     d cetoramin       s               t   timfmt(*iso)
003200131210     d wcetora1        s               t   timfmt(*iso)
003300131210     d wcetora2        s               t   timfmt(*iso)
003400131114      *
003500121015     d wDataOra        s             12
003600121015     d wOra            s              4
003700080222
003800140210     D trtcp2ds      E DS
003900080222     d fiprdds       e ds                  extname(fiprd00r)
004000120328     d fipcids       e ds                  extname(fipci00f)
004100080317     d fiprdcetds    e ds
004300140214     c* cmc del PCT
004400080331     d ds2a          e ds
004500140214     c* cmc del CET
004600131114     d ds2anew       e ds                  extname(ds2a) prefix(x)
004700140214     c* cmc del CEP
004800131210     d ds2acep       e ds                  extname(ds2a) prefix(y)
004900080227     d fipctds       e ds                  extname(fipct00f)
005000080317     d fipctcetds    e ds
005100080513     d COM02           S                   LIKE(§CETnrbanr)
005200140217     d oracon          S              4  0
005300140217     d datacon         S              8  0
005900140213     d parfgs          s              3
005901180221      *______________________________________________________________________
006000080222     c                   exsr      sr_prima
006100080222
006200080225      * controlla i dati ricevuti da PDA se errore scrivo il log e vado a
006300080225      * fine
006400080222     c                   exsr      sr_ctrdati
006500080227      * scrivo file esiti per chiusura distinta
006600080227     c                   exsr      sr_write
006700080416      * se arrivo qui vuol dire nessun errore quindi scrivo file di log
006800080416     c                   exsr      sr_wrtlog
006900080227     c* fine
007000080227     c                   exsr      sr_fine
007100080227     c*
007200080222      *---------------------------------------------------------------------
007300080222      * ROUTINE CONTROLLO DATI FIPRD
007400080222      *---------------------------------------------------------------------
007500080222     c     sr_ctrdati    begsr
007600080222     c                   clear                   fiplg000
007700080222     c                   clear                   prmerr
007800080222     c                   clear                   prmmsg
007900080226      * controllo se esiste la distinta e se dell'aut.
008000080222     c                   exsr      sr_ctrdst
008100080222     c                   if        prmerr <> ' '
008200080222     c                   eval      plgmsg = prmmsg
008300080222     c                   exsr      sr_wrtlog
008400080222     c                   end
008500080317     c                   move      §cetfgs       plgfgs
008600080317     c                   move      §cetndc       plgndc
008700110328      * controllo spedizione. se distinta diversa riceviamo lo stesso
008800110328      * altrimenti perdiamo l'esito
008900080222     c                   exsr      sr_ctrdoc
009000110328     c                   if        prmerr <> ' ' and prmerr <> '3'
009100080222     c                   eval      plgmsg = prmmsg
009200080222     c                   exsr      sr_wrtlog
009300080222     c                   end
009400080222      * data ora pda conferma
009500080331     c                   eval      prmdatora = %subst(prddati:204:14)
009600080222     c                   exsr      sr_ctrdop
009700110606     c                   if        prmerr =  'E'
009800080222     c                   eval      plgmsg = prmmsg
009900080331     c                   move      'PDA'         plgmsg
010000080222     c                   exsr      sr_wrtlog
010100080222     c                   end
010200110606     c* se errore F = forzabile vuol dire che ho ricvevuto una data anteced.
010300140318     c* 10 gg o superiore di 10 gg. per cui forzo data e ora ricezione
010400110609     c                   eval      saverr = prmerr
010500110609     c                   if        saverr = 'F'
010600121015     c                   eval      wDataOra = %subst(PRDdatora:1:12)
010700121015     c                   movel     wDataOra      §CETdtorin
010800110609     c                   end
010900131126      *
011000080409     c                   if        §cetcmc <> ' '
011100080331     c                   eval      tblkey = §cetcmc
011200080331     c                   eval      tblcod = '2A'
011300080331     c     ktab          chain     tabel00f
011400080331     c                   if        not %found(tabel00f)
011500080408     c                   eval      plgmsg = 'MANCATA CONSEGNA NO IN TABEL00F'
011600080331     c                   exsr      sr_wrtlog
011700140214     c                   else
011800140214     c                   eval      ds2anew = tbluni
011900080331     c                   end
012000140214     c                   else
012100140214     c                   clear                   ds2anew
012200080331     c                   end
012300080331     c* data e ora consegna/mancata consegna
012400080331     c                   eval      prmdatora = %subst(prddati:29:14)
012500080331     c                   exsr      sr_ctrdop
012600110606     c                   if        prmerr =  'E'
012700080331     c                   eval      plgmsg = prmmsg
012800080331     c                   move      'CONSEGNA'    plgmsg
012900080331     c                   exsr      sr_wrtlog
013000080331     c                   end
013100110606     c* se errore F = forzabile vuol dire che ho ricvevuto una data anteced.
013200121015     c* 10 gg o superiore 10 gg. per cui forzo data distinta e ora ricezione
013300110606     c                   eval      saverr = prmerr
013400080331     c* data e ora consegna richiesta
013500080331     c                   if        %subst(prddati:43:14) <> ' ' and
013600080331     c                             %subst(prddati:43:14) <> *all'0'
013700080331     c                   eval      prmdatora = %subst(prddati:43:14)
013800080331     c                   exsr      sr_ctrdop
013900110609     c                   if        prmerr =  'E'
014000080331     c                   eval      plgmsg = prmmsg
014100110609     c                   movel     'CON.RIC.'    plgmsg
014200080331     c                   exsr      sr_wrtlog
014300080331     c                   end
014400080430     c                   else
014500080430     c                   clear                   §cetdcr
014600080430     c                   clear                   §cethcr
014700080331     c                   end
014800080416     c* incassi
014900080416     c                   if         §CETINCCAS <>' ' and
015000080416     c                              §CETINCCAS <>'A' and
015100080416     c                              §CETINCCAS <>'C' and
015200080416     c                              §CETINCCAS <>'M' and
015300080416     c                              §CETINCCAS <>'N'
015400080416     c                   eval      plgmsg = 'TIPO INCASSO C/A ERRATO'
015500080416     c                   exsr      sr_wrtlog
015600080416     c                   end
015700080416     c                   if        §CETINCFAT <>' ' and
015800080416     c                             §CETINCFAT <>'A' and
015900080416     c                             §CETINCFAT <>'C' and
016000080416     c                             §CETINCFAT <>'M' and
016100080416     c                             §CETINCFAT <>'N'
016200080416     c                   eval      plgmsg = 'TIPO INCASSO FATTURA ERRATO'
016300080331     c                   exsr      sr_wrtlog
016400080331     c                   end
018900080331     c* RISERVA
019000080331     c                   if        §cetriserv <> ' '
019100080331     c                   eval      tblkey = §cetriserv
019200101104     c                   exsr      srtab2a
019300080331     c                   if        not %found(tabel00f) or
019400080331     c                             §2ATIP<>'R'
019500080408     c                   eval      plgmsg = 'RISERVA NO IN TABEL00F'
019600080331     c                   exsr      sr_wrtlog
019700080331     c                   end
019800080331     c                   end
019900080331     c* consegna particolare
020000080331     c                   if        §cettc1 <> ' '
020100080331     c                   eval      tblkey = §cettc1
020200080331     c                   eval      tblcod = '1P'
020300080331     c     ktab          chain     tabel00f
020400080331     c                   if        not %found(tabel00f)
020500080408     c                   eval      plgmsg = 'CONSEGNA PART. NO IN TABEL00F'
020600080331     c                   exsr      sr_wrtlog
020700080331     c                   end
020800080331     c                   end
020900080331      * nr bancali
021000080513     c                   monitor
021100080513     c                   move      §cetnrbanr    COM02
021200080513     c                   on-error
021300080408     c                   eval      plgmsg = 'NR.BANCALI FORMALMENTE ERRATO'
021400080331     c                   exsr      sr_wrtlog
021500080513     c                   endmon
021600080331     c*
021700080331     c                   endsr
021800080222      *---------------------------------------------------------------------
021900101104      * tabella 2a
022000080222      *---------------------------------------------------------------------
022100101104     c     srtab2a       begsr
022200101104     c                   clear                   ds2a
022300101104     c                   eval      tblcod = '2A'
022400101104     c     ktab          chain     tabel00f
022500101104     c                   if        %found(tabel00f)
022600101104     c                   eval      ds2a = tbluni
022700101104     c                   end
022800101104     c                   endsr
022900131210      *---------------------------------------------------------------------
023000131210      * tabella 2aCEP
023100131210      *---------------------------------------------------------------------
023200131210     c     srtab2aCEP    begsr
023300131210     c                   clear                   ds2aCEP
023400131210     c
023500131210     c                   eval      tblcod = '2A'
023600131210     c                   eval      tblkey = §pctcmc
023700131210     c     ktab          chain     tabel00f
023800131210     c                   if        %found(tabel00f)
023900131210     c                   eval      ds2aCEP = tbluni
024000131210     c                   end
024100131210     c                   endsr
024200101104      *---------------------------------------------------------------------
024300101104      * controlla distinta
024400101104      *---------------------------------------------------------------------
024500101104     c     sr_ctrdst     begsr
024600080222     c                   eval      prmfgs = %subst(prddati:1:3)
024700080222     c                   eval      prmnfv = %subst(prddati:4:6)
024800080225     c                   eval      prmaut = %subst(prdprfc:3:7)
024900110606     c                   eval      prmdfv = *all'0'
025000080222     c                   call      'TRTCPDSTR'
025100080222     c                   parm                    prmfgs
025200080222     c                   parm                    prmnfv
025300080225     c                   parm                    prmaut
025400110606     c                   parm                    prmdfv
025500080222     c                   parm                    prmerr
025600080222     c                   parm                    prmmsg
025700080222     c                   endsr
025800080222      *---------------------------------------------------------------------
025900080222      * controlla id documento
026000080222      *---------------------------------------------------------------------
026100080222     c     sr_ctrdoc     begsr
026200080222     c                   eval      prmdoc = %subst(prddati:10:16)
026300080225     c                   eval      prmfgs = %subst(prddati:1:3)
026400080225     c                   eval      prmnfv = %subst(prddati:4:6)
026500080225     c                   call      'TRTCPDOCR'
026600080222     c                   parm                    prmdoc
026700080225     c                   parm                    prmfgs
026800080225     c                   parm                    prmnfv
026900080222     c                   parm                    prmerr
027000080222     c                   parm                    prmmsg
027100080222     c                   endsr
027200080222      *---------------------------------------------------------------------
027300080222      * controlla data ora pda conferma
027400080222      *---------------------------------------------------------------------
027500080222     c     sr_ctrdop     begsr
027600080225     c                   call      'TRTCPDOPR'
027700080222     c                   parm                    prmdatora
027800080222     c                   parm                    prmerr
027900080222     c                   parm                    prmmsg
028000080222     c                   endsr
028100080222      *---------------------------------------------------------------------
028200080222      * scrive log
028300080222      *---------------------------------------------------------------------
028400080222     c     sr_wrtlog     begsr
028500080225     c* se sto scrivendo un errore imposto anche il flag
028600080225     c                   if        plgmsg <> ' '
028700080222     c                   eval      plgerr = 'E'
028800080225     c                   else
028900080225     c                   clear                   plgerr
029000080225     c                   end
029100110607     c* segnalo se forzata data ricezione errata perchè antecedente di 30 gg
029200120924     c* variato il messaggio a 10 giorni
029300110607     c                   if        saverr = 'F' and plgmsg = ' '
029400110607     c                   if        plgmsg = ' '
029500120926     c                   eval      plgmsg = 'DATA < o > di 10 GG'
029600110607     c                   end
029700110607     c                   clear                   saverr
029800110607     c                   end
029900080222     c                   eval      plgtiporcd = prdtiporcd
030000080408     c                   eval      PLGTIPAPPL = 'C'
030100080222     c                   eval      plgcodaut = %dec(%subst(prdprfc:3:7):7:0)
030200080222     c                   eval      plgprfc = prdprfc
030300080222     c                   eval      plgiddisp = prdiddisp
030400080222     c                   eval      plgdatora = prddatora
030500080222     c                   eval      plgdati = prddati
030600080225     c                   eval      plgiddoc = %subst(prddati:10:16)
030700080222     c                   write     fiplg000
030800080225     c* se sto scrivendo un errore vado a fine
030900080225     c                   if        plgmsg <> ' '
031000080222     c                   exsr      sr_fine
031100080225     c                   end
031200080227     c                   endsr
031300080227
031400080227      *----------------------------------------------------------------
031500080227      * scrittura file esiti per chiusura distinta
031600080227      *----------------------------------------------------------------
031700080227     c     sr_write      begsr
031800080317     c                   clear                   fipctcetds
031900080227     c                   clear                   fipctds
032000131114     c                   clear                   forzawrite        1
032100131115     c                   clear                   noagg             1
032200080805     c     kpct          setgt     fipct02l
032300080805     c     kpct          readpe(e) fipct02l
032400080227     c*
032500080227     c                   if        %error
032600080227     c                   eval      esito = 'E'
032700080227     c                   exsr      sr_fine
032800080227     c                   end
032900080227     c*
033000080805     c                   if        not %eof(fipct02l)
033100080317     c                   eval      fipctcetds = pctdati
033200080227     c* controllo sequenza data ora pda, nel caso non aggiorno scrivo log
033300080304     c* se quella trasmessa è > di quella che sto ricevendo
033400080331     c                   if        pctdtorin > §cetdtorin
033500080408     c                   eval      plgmsg = 'DATA/ORA FIPCT00F > DATA RICEV.'
033600080227     c                   exsr      sr_wrtlog
033700080227     c                   end
033800101104     c* se l'evento che trovo è di tipo turno, no chiusura distinta e non
033900101104     c* l'ho memorizzato prima (TM/TP) lo memorizzo nel codice di mancata
034000101104     c* consegna precedente per poterlo proporre in chiusura distinta  e
034100101104     c* fare la variazione bolla
034200101104     c                   eval      tblkey = §pctcmc
034300101104     c                   exsr      srtab2a
034400101104     c                   if        §2ACPT = 'S' and §2ATIP = '§' and
034500101104     c                             §PCTCMCP = *blanks
034600101104     c                   movel     §pctcmc       §pctcmcp
034700131115     c                   end
034800131114     c* Se non è un turno è ho già un CET
034900131114     c*verifica se ci sono le condizioni per inserire il rekord CEP
035000131114     c                   exsr      InserCep
035100131115     c                   if        noagg = 'X'
035200131115     c                   leavesr
035300131115     c                   endif
035400101104     c                   end
035500131114      *
035600080604     c                   clear                   pctatb
035700080604     c                   eval      pctfgs = §cetfgs
035800080604     c                   eval      pctndc = §cetndc
035900080317     c                   eval      pctaas = §cetaas
036000080317     c                   eval      pctlnp = §cetlnp
036100080317     c                   eval      pctnrs = §cetnrs
036200080317     c                   eval      pctnsp = §cetnsp
036300080304     c                   eval      pcttrd = prdtiporcd
036400080604     c* flag di ricezione dati PDA
036500080304     c                   eval      PCTPDAAS4 = 'P'
036600110606     c                   eval      §PCTCMC  =     §cetCMC
036700131205     c                   eval      PCTdtorin = §cetdtorin
036800131114      *ora imposta sempre date ultimo evento quindi reali
036900131210     c                   eval      §PCTDATA =     §cetDATA
037000131210     c                   eval      §PCTORA  =     §cetORA
037001140321      * errore forzabile oppure orario esitazione minore delle 6 o delle 22 in questo caso
037002140318      * forza ora di ricezione METTE SOLO ORA E MINUTI ALTRIMENTI TRASFORMEREBBE TUTTO IN MULTICONS.
037100140318     c                   if        saverr = 'F'
037200121015     c                   eval      wDataOra = %subst(PRDdatora:1:12)
037201140318     c                   eval      wOra     = %subst(PRDdatora:9:4)
037202140318     c                   movel     wDataOra      PCTdtorin
037203140318     c                   movel     wORA          §PCTora
037204140318     c                   move      prmdfv        §pctDATA
037210140318     c                   endif
037211140321     c                   if        §pctora < 060000 or §pctora > 220000
037300121015     c                   eval      wOra     = %subst(PRDdatora:9:4)
037301140318     c                   movel     wORA          §PCTora
037302140331     c                   move      §PCTora       pctdtorin
037601140318     c                   endif
037608140324      *
037800140217     c                   movel     §pctora       oracon
037900140217     c                   eval      datacon = §pctdata
038000080430     c                   eval      §PCTDCR  =     §cetDCR
038100080430     c                   eval      §PCTHCR  =     §cetHCR
038200080430     c                   eval      §PCTnotaut =   §cetnotaut
038300080430     c                   eval      §PCTINCCAS =   §cetINCCAS
038400080430     c                   eval      §PCTINCFAT =   §cetINCFAT
038500080430     c                   eval      §PCTNOMFIR =   §cetNOMFIR
038600080430     c                   eval      §PCTRISERV =   §cetRISERV
038700080430     c                   eval      §PCTTC1    =   §cetTC1
038800080430     c                   eval      §PCTNRBANR =   §cetNRBANR
038900080430     c                   eval      §PCTFLGSPC =   §cetfLGSPC
039000101102     c                   eval      §PCTldvfir =   §cetldvfir
039100130528     c                   eval      §PCTXLO    =   §CETXLO
039200130528     c                   eval      §PCTYLA    =   §CETYLA
039300140121     c                   eval      §PCTritdoc =   §CETritdoc
039400080317     c                   eval      PCTdati = fipctcetds
039500080805     c                   eval      pctcodaut = %dec(%subst(prdprfc:3:7):7:0)
039600080227     c*
039700131114     c                   if        %eof(fipct02l) or forzawrite = 'X'
039800080227     c                   write     fipct000
039900080227     c                   else
040000080227     c                   update    fipct000
040100080227     c                   end
040200140213      * se ricevuto CET di tipo l.avviso                                    i eventuali esiti
040300140213     c                   if        x§2aftc = 'A'
040400140210     c                   exsr      srlavv
040500140213     c                   end
040600131210      *
040700120328     c     kcet          chain     fipci01l
040800120328      * nel casi in cui almeno uno dei due dati è valorizzato aggiorno o scrivo
040900120328     c                   if        §cetcodfis <> *blanks or §cetcodpiv<> *blanks
041000120328     c                   if        %found(fipci01l)
041100120328     c                   eval      pcicdf = §cetcodfis
041200120328     c                   eval      pciiso = §cetcodiso
041300120328     c                   eval      pcicpi = §cetcodpiv
041400120328     c                   clear                   pciesito
041500120328     c                   update    fipci000
041600120328     c                   else
041700120328     c                   clear                   fipci000
041800120328     c                   eval      pciaas = §cetaas
041900120328     c                   eval      pcilnp = §cetlnp
042000120328     c                   eval      pcinrs = §cetnrs
042100120328     c                   eval      pcinsp = §cetnsp
042200120328     c                   eval      pcicdf = §cetcodfis
042300120328     c                   eval      pciiso = §cetcodiso
042400120328     c                   eval      pcicpi = §cetcodpiv
042500120328     c                   write     fipci000
042600120328     c                   endif
042700120328     c                   endif
042800120328      * nel casi in cui entrambi sono a blank ed il record esiste annullo il rcd
042900120328     c                   if        §cetcodfis=*blanks and §cetcodpiv=*blanks
043000120328     c                             and %found(fipci01l)
043100120328     c                   eval      pciatb = 'A'
043200120328     c                   update    fipci000
043300120328     c                   endif
043400080222     c                   endsr
043500080222
043600131114      *----------------------------------------------------------------
043700131114     c     InserCep      begsr
043800131114      *----------------------------------------------------------------
043900131115      * calcola differenza in minuti fra il rekord ricevuto precedentemente
044000131115     c                   clear                   minuti
044100131115     c                   if        pctdtorin < §cetdtorin
044200131115     c                   move      pctdtorin     pctoramin
044300131115     c                   move      §cetdtorin    cetoramin
044400131122     c                   movel     pctdtorin     wpctdata          8 0
044500131122     c                   movel     §cetdtorin    wcetdata          8 0
044600131115     c     cetoramin     subdur    pctoramin     minuti:*mn        5 0
044700131115     c                   endif
044800131115      *
044900131114     c                   select
045000131210      * se l'aggiornamento del record arriva in meno di sei minuti capisco che è un errore
045100131210      * aggiorno record originale
045200131122     c                   when      minuti < 6 and wpctdata = wcetdata
045300140213     c* se il 1° esito era un avv ed ora lo correggo devo tentare di
045400140213     c* eliminare la spia per sms
045500140213     c                   exsr      srnoavv
045600131115     c                   leavesr
045700131210      * se consegnata aggiorna originale
045800131120     c                   when      §pctcmc = *blank
045900131120     c                   leavesr
046000131120
046100131210      * se turni del pda TM TP TT
046200131115     c                   when      §2ACPT = 'S' and §2ATIP = '§'
046300131115     c                   leavesr
046400131210      * doppio lasciato avviso mantiene il primo quindi forza non aggiornamento
046500131114     c                   when      §2aftc = 'A' and x§2aftc = 'A'
046600131115     c                   move      'X'           noagg
046700131114     c                   leavesr
046800131210      * se non fatta precedentemente o non fatta con secondo tentativo verifica se
046900131210      * esiste il CEP e lo elimina forzatamente
047000131125     c                   when      §2aftc = 'N' or x§2aftc = 'N'
047100131129     c                             and x§2antc = ' '
047200131209     c                   if        x§2aftc = 'N'
047300131209     c     kpctcep1      chain     fipct01l
047400131209     c                   if        %found(fipct01l)
047500131209     c                   delete    fipct1
047600131209     c                   endif
047700140213     c* se il 1° esito era un avv ed ora lo correggo devo tentare di
047800140213     c* eliminare la spia per sms
047900140213     c                   exsr      srnoavv
048000140213     c                   endif
048100131114     c                   leavesr
048200131210      *se forzato da SPC aggiorna esistente
048300131114     c                   when      §cetflgspc = 'S'
048400140213     c* se il 1° esito era un avv ed ora lo correggo devo tentare di
048500140213     c* eliminare la spia per sms
048600140213     c                   exsr      srnoavv
048700131114     c                   leavesr
048800131210      * negli altri casi se non è già presente aggiorna vecchio record con tipo rekord CEP
048900131210      * e scrive nuovo rekord per esito CET
049000131114     c                   other
049100131209     c     kpctcep1      setll     fipct01l                               66
049200131209     c   66              leavesr
049300131114     c                   move      'CEP'         pcttrd
049400131114     c                   update    fipct000
049500131114     c                   move      'X'           forzawrite
049600131114     c                   endsl
049700131114     c                   endsr
049800131210      *----------------------------------------------------------------
049900140210     c     srlavv        begsr
050000131210      *----------------------------------------------------------------
050100140210     c* ho ricevuto un l.avv: verifico il CEP
050200131210     c     kpctcep1      chain     fipct01l
050300131210     c                   if        %found(fipct01l)
050400131210     c                   movel     pctdati       fipctcetds
050500131210     c                   exsr      srtab2acep
050600140210     c* se CEP è l.avv, cancello quello appena ricevuto e tengo buono il 1°
050700140210     c* aggiornandolo con tipo record CET
050800131210     c                   if        y§2aftc = 'A'
050900131210     c     kpct          chain     fipct02l
051000131210     c                   if        %found(fipct02l)
051100131210     c                   delete    fipct000
051200131210     c                   move      'CET'         pcttrd
051300131210     c                   update    fipct1
051400131210     c                   endif
051500131210     c                   endif
051600131210     c                   endif
051700140210     c* se è il 1° l.avviso che ricevo, chiamo il pgm per scrivere la spia
051800140210     c* d'invio SMS/MAIL al destinatario
051900140210     c                   if        not %found(fipct01l) or
052000140210     c                             (%found(fipct01l) and
052100140210     c                             y§2aftc <>'A')
052200140210     c                   clear                   trtcp2ds
052300140217     C                   Z-ADD     DATAcon       t2DEV
052400140217     C                   z-add     ORAcon        t2HEV
052500140210     c                   exsr      srspia
052600140210     c                   end
052700131210     c                   endsr
052800140210      *----------------------------------------------------------------
052900140210     c     srnoavv       begsr
053000140210      *----------------------------------------------------------------
053100140213     c* ho ricevuto un evento che mi rettifica il l.avv. scritto prima
053200140213     c* tento di cancellarlo
053300140214     c                   if        x§2aftc <>'A'
053400140210     c                   clear                   trtcp2ds
053500140210     c                   eval      T2CHIAMATA = 'A'
053600140210     c                   exsr      srspia
053700140214     c                   end
053800140210     c                   endsr
053900140210      *----------------------------------------------------------------
054000140210     c     srspia        begsr
054100140210      *----------------------------------------------------------------
054200140218     c                   if        §cetfLGSPC = 'S'
054300140218     c                   eval      t2chi= §cetfLGSPC
054400140218     c                   else
054500140213     c                   eval      t2chi= 'P'
054600140218     c                   end
054700140213     c                   move      §CETFGS       parfgs
054800140213     c                   z-add     §CETFGS       t2fgs
054900140213     c                   z-add     §CETNDC       t2ndc
055000140213     C                   Z-ADD     §cetAAS       t2aas
055100140213     C                   Z-ADD     §cetLNP       t2lnp
055200140213     C                   Z-ADD     §cetNRS       t2nrs
055300140213     C                   Z-ADD     §cetNSP       t2nsp
056100180221     c                   call      'TRTCP2R'
056200180221     c                   parm                    trtcp2ds
056400140210     c                   endsr
056500080222      *----------------------------------------------------------------
056600080222      * ROUTINE INIZIALE
056700080222      *----------------------------------------------------------------
056800080222     c     sr_prima      begsr
056900080222     c* parametri
057000080222     c     *entry        plist
057100080222     c                   parm                    fiprdds
057200080222     c                   parm                    esito
057300080222     c                   clear                   esito
057400131127      * codice mancata consegna
057500131127      * prima di trattare il codice di mancata consegna verifico che se ho un turno
057600131127      * vario il codice di ritorno da TT a T perchè il TT è un codice fittizio che serve
057700131127      * solo per gestire i turni in maniera sequenziale sul palmare per l'autista
057800131127      * che li vedrà in maniera temporale come TM= mattino TP=pomeriggio TT= Totale
057900131127      *
058000080317     c                   eval      fiprdcetds=prddati
058100131204     c                   if        §cetcmc = 'TT'
058200131204     c                   move      'T  '         §cetcmc
058300131204     c                   endif
058400131210     c                   movel     §cetdtorin    wcetdat           8 0
058500131210     c                   if        wcetdat = §cetdata
058600131210     c                   move      §cetdtorin    wcetora1
058700131210     c                   move      §cetora       wcetora2
058800131210     c     wcetora1      subdur    wcetora2      diffe:*mn         5 0
058900131210     c                   if        diffe > 2
059000131204     c                   movel     §cetdtorin    §cetdata
059100131204     c                   move      §cetdtorin    §cetora
059200131210     c                   endif
059300131210     c                   endif
059400131204     c                   eval      prddati = fiprdcetds
059500131204      *
059600080805     c                   eval      codaut = %dec(%subst(prdprfc:3:7):7:0)
059700080331     c                   eval      tblkut = 1
059800080331     c     ktab          klist
059900080331     c                   kfld                    tblkut
060000080331     c                   kfld                    tblcod
060100080331     c                   kfld                    tblkey
060200120328     c     ktbe          klist
060300120328     c                   kfld                    tbecod
060400120328     c                   kfld                    tbeke1
060500080227     c     kpct          klist
060600080805     c                   kfld                    §cetfgs
060700080805     c                   kfld                    §cetndc
060800080805     c                   kfld                    codaut
060900080805     c                   kfld                    prdtiporcd
061000080317     c                   kfld                    §cetAAS
061100080317     c                   kfld                    §cetLNP
061200080317     c                   kfld                    §cetNRS
061300080317     c                   kfld                    §cetNSP
061400131209     c     kpctcep1      klist
061500131209     c                   kfld                    §cetAAS
061600131209     c                   kfld                    §cetLNP
061700131209     c                   kfld                    §cetNRS
061800131209     c                   kfld                    §cetNSP
061900131209     c                   kfld                    ceprcd
062000131209     c                   movel     'CEP'         ceprcd            3
062100120328     c     kcet          klist
062200120328     c                   kfld                    §cetAAS
062300120328     c                   kfld                    §cetLNP
062400120328     c                   kfld                    §cetNRS
062500120328     c                   kfld                    §cetNSP
062600120328     c*
062700120328     C                   TIME                    W0140            14 0
062800120328     C                   MOVE      W0140         WDTGIO            8 0
062900120328     C                   Z-ADD     WDTGIO        G02DAT
063000120328     C                   MOVEL     *BLANK        G02ERR
063100120328     C                   CALL      'XSRDA8'
063200120328     C                   PARM                    WLBDAT
063300120328     C                   MOVEL     G02INV        DATEU             8 0
063400080222
063500080222     c                   endsr
063600080222      *--------------------------------------------------------------
063700080222      * fine
063800080222      *--------------------------------------------------------------
063900080222     c     sr_fine       begsr
064000080222     c                   seton                                        rt
064100080222     c                   return
064200080222     c                   endsr
064300120328     c*
