000100070125     h decedit('0,') option(*nodebugio)
000200070125      *---------------------------------------------------------------*
000300080220      * Da FIPRD00R scrive file di log FIPLG00F                       *
000500070125      *---------------------------------------------------------------*
000501080220     ftntbe01l  if   e           k disk
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
005300070129
005400070130      *------------------------------------------------------------------------*
005500070129      *   D S   I N T E R N E / E S T E R N E
005600070130      *------------------------------------------------------------------------*
005700080222     d prd             ds
005800080222     d  prdnrr               397    400B 0
006600080222     d dprc          e ds
009000080205     d og148         e ds
009001080222     d fiprdds       e ds                  extname(fiprd00r)
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
016907080222     c                   clear                   fiprdds
017100070131      * tipo record
017200070131      * se errato non posso continuare perchè non so come leggere i dati
017201080222     c                   eval      tbeke1 = prdtiporcd
017300080222     c     ktbe          chain     tntbe01l
017301080222     c                   if        not %found(tntbe01l) or
017302080222     c                             (%found(tntbe01l) and tbeatb <> ' ')
017700080222     c                   eval      plgmsg = 'TRTCPD0R:TIPO RECORD NON VALIDO'
017701080222     c                   exsr      sr_wrtlog
017702080222     c                   else
017703080222     c                   eval      tbeuni = dprc
017704080222     c* errore sto ricevendo un esito non di consegna
017705080222     c                   if        D§PRCCR <> 'C'
017706080222     c                   eval      plgmsg = 'TRTCPD0R: ESITO NON DI CONSEGNA'
017707080222     c                   exsr      sr_wrtlog
017708080222     c                   else
017709080222     c* errore sto ricevendo un esito senza pgm attaccato
017710080222     c                   if        D§PRCpgm = ' '
017711080222     c                   eval      plgmsg = 'TRTCPD0R: ESITO SENZA PROGRAMMA'
017712080222     c                   exsr      sr_wrtlog
017713080222     c                   else
017714080222     c* lancio
017716080222     c                   call      D§PRCPGM
017717080222     c                   parm                    fiprdds
017718080222     c                   parm                    esito
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
098030080222      * controllo se la filiale dell'AUT esite e se è in test
098031080222     c                   monitor
098032080222     c                   eval      orgfil = %dec(parmpo: 3: 0)
098033080222     c                   on-error
098034080222     c                   clear                   fiplg000
098035080222     c                   eval      plgmsg = 'TRTCPD0R: FILIALE NON VALIDA'
098036080222     c                   exsr      sr_wrtlog
098037080222     c                   exsr      sr_fine
098038080222     c                   endmon
098039080222     c*
098040080220     c                   clear                   og148
098041080222     c     orgfil        chain     azorg01l
098042080220     c                   if        %found(azorg01l)
098043080220     c                   eval      og148 = orgde8
098044080222     c                   endif
098045080222     c*
098046080222     c                   if        §OGPDACON = ' '
098047080222     c                   clear                   fiplg000
098048080222     c                   eval      plgmsg = 'TRTCPD0R: FILIALE NON VALIDA'
098049080222     c                   exsr      sr_wrtlog
098050080222     c                   exsr      sr_fine
098051080222     c                   end
098052080222     c* flg per chiusura sottositema
098053080222     c                   eval      chiudi = 'N'
098054080222     c                   setoff                                       99
098055080222     c*
098056080222     c                   eval      kprdprfc = 'AU0000000'
098057080222     c                   eval      %subst(kprdprfc:3:3) = parmpo
098058080222     c                   eval      tbecod= 'PRC'
098100070201
098200070201     c                   endsr
098300070129
098400070129      *------------------------------------------------------------------------*
098500080222      * fine
098600070129      *------------------------------------------------------------------------*
098700080222     c     sr_fine       begsr
098701080222     c                   seton                                        rt
098702080222     c                   return
098703080222     c                   endsr
