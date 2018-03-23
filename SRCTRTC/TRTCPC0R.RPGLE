000100070125     h decedit('0,') option(*nodebugio)
000200070125      *---------------------------------------------------------------*
000300080220      * Da FIPRD00R scrive file di log FIPLG00F                       *
000500070125      *---------------------------------------------------------------*
000501080220     ftntbe01l  if   e           k disk
000502080222     ffiapd01l  if   e           k disk
000600080221     ffiprd01r  if   e           k disk    rename(fiprd000:fiprd01)
000700080222     f                                     infds(prd)
000800080221     ffiprd00r  uf   e             disk    commit
001600070202     ffiplg00f  o    e             disk    commit
001700080205     fazorg01l  if   e           k disk
001800070130
003200080220     d chiudi          s              1
003201080222     d esito           s              1
003202080222     d kprdprfc        s                   like(prdprfc)
003300070129     d parmpo          s              3
003301080222     d data            s               d
003302080222     d ora             s               t
005300070129
005400070130      *------------------------------------------------------------------------*
005500070129      *   D S   I N T E R N E / E S T E R N E
005600070130      *------------------------------------------------------------------------*
005700080222     d prd             ds
005800080222     d  prdnrr               397    400B 0
006600080222     d dprc          e ds
009000080205     d og148         e ds
009001080222     d fiprdds£      e ds                  extname(fiprd00r) prefix(£)
009002080222     d fiprdds       e ds                  extname(fiprd00r)
009003080513     d codautds        ds
009005080513     d  codaut                 3      9  0
009006080513     d ctrdatads       ds
009007080513     d  datac                  1      8  0
009008080513     d  orac                   9     14  0
009200070129
009300070129      *------------------------------------------------------------------------*
009301080222     c                   exsr      sr_prima
010600070129
010700080222     c     kprdprfc      setll     fiprd01r
010800070129     c                   do        *hival
010900080220     c                   read      fiprd01r
011000070129
011100080220     c                   if        %eof(fiprd01r)
011200070129     c                   leave
011300070129     c                   endif
011400070129      * stesso p.o.
011500080222     c                   if        %subst(prdprfc:3:3) <> parmpo
011600070129     c                   leave
011700070129     c                   endif
012200080222      * controlla i dati ricevuti da PDA se errore scrivo il log altrimenti
012201080222      * chiamo pgm di gestione record
012202080222     c                   exsr      sr_call
013500080222      * se ho avuto errori nei pgm chiamati faccio il rollback
013700080222    2c                   if        esito = 'E'
013900070202     c                   rolbk
014000070205      * se non ho avuto errori
014100070205   2xc                   else
014200070205      * aggancio il file fisico di ricezione
014300080222     c     prdnrr        chain(e)  fiprd00r
014400080222      * se rcd allocato rollback (non dovrebbe verificarsi mai)
014600070205    3c                   if        %error
014700070205     c                   rolbk
014800070205   3xc                   else
014801080222      * cancello il file di ricezione
014900080221    4c                   if        %found(fiprd00r)
015100080221     c                   delete    fiprd000
015200070205      * committo le modifiche
015300070205     c                   commit
015400070205    4c                   endif
015500070205    3c                   endif
015600070205    2c                   endif
016101080222      * testo la chiusura del sottosistema
016102080222     c                   shtdn                                        99
016103080222     c                   if        *in99
016104080222     c                   eval      chiudi = 'S'
016105080222     c                   exsr      sr_fine
016106080222     c                   end
016200070129     c                   enddo
016300070129
016400070205     c                   eval      *inrt = *on
016500070129
016901080222      *------------------------------------------------------------------------*
016902080222      * ROUTINE CONTROLLO DATI FIPRD e lancio
016903080222      *------------------------------------------------------------------------*
016904080222     c     sr_call       begsr
016905080222     c                   clear                   fiplg000
016906080222     c                   clear                   esito
016907080225     c                   clear                   fiprdds£
016908080222      * codice autotrasportatore
016909080222      *  deve essere numerico
016911080222     c                   monitor
016912080513     c                   movel     prdprfc       codautds
016913080513     c                   move      codaut        plgcodaut
016914080222     c                   on-error
016915080304     c                   eval      plgmsg = %subst(prdprfc:3:7) +
016916080304     c                             ' COD.AUT. NON VALIDO'
016917080222     c                   exsr      sr_wrtlog
016918080513     c                   leavesr
016919080222     c                   endmon
016928080222      *  deve esistere
016929080222     c                   if        plgcodaut <> 0
016930080222     c                   eval      apdtip = 'A'
016931080222     c     kfiapd        chain     fiapd01l
016932080222     c                   if        not %found(fiapd01l) or
016933080222     c                             apdatb <> *blanks
016934080304     c                   eval      plgmsg = %subst(prdprfc:3:7) +
016935080304     c                             ' COD.AUT. NON VALIDO'
016937080222     c                   exsr      sr_wrtlog
016938080513     c                   leavesr
016939080222     c                   endif
016940080222     c                   endif
017100080222      * ticket
017101080513     c                   move      prddatora     ctrdatads
017102080222     c                   monitor
017103080513     c                   eval      data= %date(%subst(ctrdatads: 1: 8): *iso0)
017104080513     c                   eval      ora = %time(%subst(ctrdatads: 9: 6): *iso0)
017105080222     c                   on-error
017106080304     c                   eval      plgmsg = %subst(prddatora:1:14) +
017108080304     c                             ' TICKET ERRATO'
017109080222     c                   exsr      sr_wrtlog
017110080513     c                   leavesr
017111080222     c                   endmon
017112080222     c*
017113080222      * tipo record
017200070131      * se errato non posso continuare perchè non so come leggere i dati
017201080222     c                   eval      tbeke1 = prdtiporcd
017300080222     c     ktbe          chain     tntbe01l
017301080222     c                   if        not %found(tntbe01l) or
017302080222     c                             (%found(tntbe01l) and tbeatb <> ' ')
017700080304     c                   eval      plgmsg = prdtiporcd +
017701080304     c                             ' TIPO REC. NON VALIDO'
017702080222     c                   exsr      sr_wrtlog
017703080513     c                   leavesr
017704080222     c                   else
017705080222     c                   eval      dprc=tbeuni
017706080222     c* errore sto ricevendo un esito non di consegna
017707080222     c                   if        D§PRCCR <> 'C'
017708080304     c                   eval      plgmsg = prdtiporcd +
017709080304     c                             ' ESITO NON DI CONS.'
017710080222     c                   exsr      sr_wrtlog
017711080513     c                   leavesr
017712080222     c                   else
017713080222     c* errore sto ricevendo un esito senza pgm attaccato
017714080222     c                   if        D§PRCpgm = ' '
017715080304     c                   eval      plgmsg = prdtiporcd +
017716080304     c                             ' ESITO SENZA PGM'
017717080222     c                   exsr      sr_wrtlog
017718080513     c                   leavesr
017719080222     c                   else
017720080222     c                   eval      fiprdds£=fiprdds
017721080222     c* lancio
017722080222     c                   call      D§PRCPGM
017723080222     c                   parm                    fiprdds£
017724080222     c                   parm                    esito
018000070131     c                   endif
018001080222     c                   endif
018002080222     c                   endif
021700070129
021800070129     c                   endsr
021801080222      *------------------------------------------------------------------------*
021802080222      * scrive log
021803080222      *------------------------------------------------------------------------*
021804080222     c     sr_wrtlog     begsr
021805080222     c                   eval      plgerr = 'E'
021806080222     c                   eval      plgtiporcd = prdtiporcd
021807080222     c                   eval      plgprfc = prdprfc
021808080222     c                   eval      plgiddisp = prdiddisp
021809080222     c                   eval      plgdatora = prddatora
021810080222     c                   eval      plgdati = prddati
021811080222     c                   write     fiplg000
021812080222     c                   endsr
098017080222
098018080222      *------------------------------------------------------------------------*
098019080222      * ROUTINE INIZIALE
098020080222      *------------------------------------------------------------------------*
098021080222     c     sr_prima      begsr
098022080220     c* parametri
098023080220     c     *entry        plist
098024080220     c                   parm                    parmpo
098025080220     c                   parm                    chiudi
098026080222     c     ktbe          klist
098027080222     c                   kfld                    tbecod
098028080222     c                   kfld                    tbeke1
098029080222     c     kfiapd        klist
098030080222     c                   kfld                    apdtip
098031080222     c                   kfld                    plgcodaut
098032080222      * controllo se la filiale dell'AUT esite e se è in test
098033080513     c                   setoff                                       01
098035080513     c                   testn                   parmpo               01
098036080513     c                   if        *in01
098037080513     c                   move      parmpo        orgfil
098038080513     c                   else
098042080222     c                   clear                   fiplg000
098043080304     c                   eval      plgmsg = parmpo +
098044080304     c                             ' FILIALE NON VALIDA'
098045080222     c                   exsr      sr_wrtlog
098046080407     c                   eval      chiudi = 'S'
098047080222     c                   exsr      sr_fine
098048080513     c                   end
098049080222     c*
098050080220     c                   clear                   og148
098051080222     c     orgfil        chain     azorg01l
098052080220     c                   if        %found(azorg01l)
098053080220     c                   eval      og148 = orgde8
098054080222     c                   endif
098055080222     c*
098056080222     c                   if        §OGPDACON = ' '
098057080407     c* non da nessun messaggio di errore: se esiste il profilo
098058080407     c* è giusto che il lavoro sia attivo, perchè vuol dire che il p.o.
098059080407     c* a breve partirà con la procedura
098060080407     c*                  clear                   fiplg000
098061080407     c*                  eval      plgmsg = parmpo +
098062080407     c*                            ' FILIALE NON PARTITA'
098063080407     c*                  exsr      sr_wrtlog
098064080407     c*                  eval      chiudi = 'S'
098065080222     c                   exsr      sr_fine
098066080222     c                   end
098067080222     c* flg per chiusura sottositema
098068080222     c                   eval      chiudi = 'N'
098069080222     c                   setoff                                       99
098070080222     c*
098071080222     c                   eval      kprdprfc = 'AU0000000'
098072080222     c                   eval      %subst(kprdprfc:3:3) = parmpo
098073080222     c                   eval      tbecod= 'PRC'
098100070201
098200070201     c                   endsr
098300070129
098400070129      *------------------------------------------------------------------------*
098500080222      * fine
098600070129      *------------------------------------------------------------------------*
098700080222     c     sr_fine       begsr
098701080304     c                   commit
098702080222     c                   seton                                        rt
098703080222     c                   return
098704080222     c                   endsr
