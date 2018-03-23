000100070125     h decedit('0,') option(*nodebugio)
000200070125      *---------------------------------------------------------------*
000300070125      * Da FIPRC00R scrive file di log FIPLG00F                       *
000400070201      *       aggiorna file effettivi se dati OK                      *
000500070125      *---------------------------------------------------------------*
000600070205     ffiprc01r  if   e           k disk    rename(fiprc000:fiprc01)
000700070205     f                                     infds(prc)
000800070205     ffiprc00r  uf   e             disk    commit
000900070129     ffiapd01l  if   e           k disk
001000070202     ffipdo01t  uf   e           k disk    commit
001100140128     ffipro11l  uf a e           k disk    commit
001200070202     ffiqdt01l  uf   e           k disk    commit
001300080226     ffidst01l  if   e           k disk
001400070202     ffnorm01l  uf   e           k disk    commit
001500080219     ffnorf01l  uf a e           k disk    commit
001600080409     ffnorn02l  if a e           k disk    commit
001700080227     ffipci02l  uf a e           k disk    commit
001800070202     ffiplg00f  o    e             disk    commit
001900080418     ffirpt01l  uf a e           k disk    commit
002000080418     ffirpd02l  uf a e           k disk    commit
002100080513     fprtemail  o    f  132        printer  oflind(*inof)  usropn
002200140207     ftntbe01l  if   e           k disk
002300070130
002400070130      *------------------------------------------------------------------------*
002500070130      *  RIEPILOGO INDICATORI
002600070130      *------------------------------------------------------------------------*
002700070130      * 01 - errore
002800070131      * 02 - testn
002900070131      * 03 - ORM manuale non aggiorno fasi ORM
003000110620      * 04 - Errore per invio mail
003100080519      * 05 - codice SPC errato
003200070129
003300070130      *------------------------------------------------------------------------*
003400070129      *   V A R I A B I L I
003500070130      *------------------------------------------------------------------------*
003600080226     d chiudi          s              1
003700070201     d dateu8          s              8  0
003800110609     d data_eur        s               d   datfmt(*eur)
003900110609     d data_iso        s               d   datfmt(*iso)
004000070201     d hhmm            s              4  0
004100070130     d kprcprfc        s                   like(prcprfc)
004200080513     d lenght          s             15  5
004300080513     d o_testo         s            132    inz
004400120920     d Oggi_10         s              8  0
004500120920     d Oggi_10o        s              8  0
004600070129     d parmpo          s              3
004700080513     d qcmd            s            512    inz
004800080212     d savdfo          s                   like(ormdfo)
004900080212     d savofo          s                   like(ormofo)
005000080320     d save_§orfass    s                   like(§orfass)
005100070201     d udate8          s              8  0
005200070201     d wbnc            s                   like(qdtnbnri)
005300070129     d wcodaut         s              7
005400070521     d wdata           s              8  0
005500080924     d wdesspc         s             15
005600080327     d wfase           s              1    inz(*off)
005700070202     d wfipdo          s              1    inz(*off)
005800070202     d wfiqdt          s              1    inz(*off)
005900080319     d wfipro          s              1    inz(*off)
006000070201     d wfgs            s                   like(§okfgs)
006100070202     d wfnorm          s              1    inz(*off)
006200080924     d widrow          s                   like(ormndc)
006300070201     d wncl            s                   like(qdtnclrit)
006400070201     d wndc            s                   like(§okndc)
006500070521     d wora            s              6  0
006600070201     d wpdandc         s                   like(ormndc)
006700080409     d wprg            s                   like(ornprg)
006800070521     d w006a           s              6
006900070521     d w008a           s              8
007000070201     d w0140           s             14  0
007100070201     d w030a           s             30
007200080417     d w_dtaorach      s             14
007300080513     d xx              s              3  0
007400080513     d $invio          s              1n   inz(*off)
007500080513     d $okmra          s              1n   inz(*off)
007600140129     d worasti         s                   like(§RORORASTI)
007700140129     d wsort           s                   like(§RORSORT)
007800070129
007900080513      *------------------------------------------------------------------------*
008000080513      *   S C H I E R E
008100080513      *------------------------------------------------------------------------*
008200090911     d codspc          s              2    dim(23)
008300090911     d desspc          s             15    dim(23)
008400080513
008500070130      *------------------------------------------------------------------------*
008600070129      *   D S   I N T E R N E / E S T E R N E
008700070130      *------------------------------------------------------------------------*
008800070205     d prc             ds
008900070205     d  prcnrr               397    400B 0
009000070205
009100070201     d wlbdat          ds
009200070201     d  g02dat                 1      8  0
009300070201     d  g02inv                 9     16  0
009400070201     d  g02err                17     17
009500070201     d  g02tgi                18     22  0
009600070201
009700070716     d dfar          e ds
009800080219     d dorf01        e ds
009900080317     d ddstflr       e ds
010000080513     d dpsp          e ds
010100090911     d dpsps         e ds
010200080924
010300080924     d fidsfrchds    e ds                  prefix(§RCH:4)
010400070716
010500140129     d fiprorords    e ds
010600140129     d fiprookds     e ds
010700140129     d fiprcrords    e ds
010800140129     d  §rorpoe               10     12
010900140129     d  §rornsr               13     14
011000140129     d  §rornor               15     21
011100140129     d  §rornrv               22     23
011200140129     d  §roraas               10     13
011300140129     d  §rorlnp               14     16
011400140129     d  §rornrs               17     18
011500140129     d  §rornsp               19     25
011600140129     d  §rordata              26     33
011700140129     d  §rorora               34     39
011800070129     d fiprcokds     e ds
011900070201     d  §okpoe                10     12
012000070201     d  §oknsr                13     14
012100070201     d  §oknor                15     21
012200070201     d  §oknrv                22     23
012300070201     d  §okaas                10     13
012400070201     d  §oklnp                14     16
012500070201     d  §oknrs                17     18
012600070201     d  §oknsp                19     25
012700070213     d  §okdata               26     33
012800070213     d  §okora                34     39
012900070129     d fiprcrchds    e ds
013000070213     d  §rchdata              20     27
013100070213     d  §rchora               28     33
013200070129     d fiprcresds    e ds
013300070213     d  §resdata              24     31
013400070213     d  §resora               32     37
013500070213     d  §resdatain           171    178
013600070213     d  §resorain            179    184
013700080317
013800080513     d dmradan       e ds                  inz
013900080327     d fior90ds      e ds
014000070130     d kpjba         e ds
014100070129     d tibs02ds      e ds
014200080513
014300080513     d trtcm1ds      e ds                  inz
014400080514      *    .§cm1mitt = Indirizzo e-mail del mittente
014500120305     d   §cm1mitt    e                     inz('ced@brt.it')
014600080514      *    ·§cm1dst  = Indirizzo e-mail del destinatario
014700120305     d   §cm1dst     e                     inz('monica.beghelli@brt.it')
014800080514      *    ·§cm1tips = Tipo lettera via e-mail:
014900080513      *     "LET" = testo allegato in corpo con logo
015000080513      *             (richiede righe libere iniziali per il logo)
015100080513      *     "COR" = testo integrato senza logo
015200080513      *             (non consente né UNDERLINE né HIGHLIGHT)
015300080514     d   §cm1tips    e                     inz('COR')
015400080514      *    ·§cm1po   = Filiale
015500080514     d   §cm1po      e                     inz('046')
015600080514      *    ·§cm1var  = Oggetto e-mail
015700080514     d   §cm1var     e                     inz('*OBJM*+
015800080521     d                                     Errore in ricezione dati da PDA')
015900080514      *    ·§cm1sts  = Stato
016000080514     d   §cm1sts     e                     inz(*off)
016100080514      *    ·§cm1idp  = Id processo
016200080521     d   §cm1idp     e                     inz('2')
016300080513
016400080513     d psds           sds
016500080513     d   sdspgm          *proc
016600080513     d   jobuser             254    263                                         User name
016700080513
016800080513      *------------------------------------------------------------------------*
016900080513      *   C O S T A N T I
017000080513      *------------------------------------------------------------------------*
017100080513     d c_sede          c                   const('046')
017200080513      *  - comando di override al prtf
017300080513     d c_cmdovrprtf    c                   const('OVRPRTF +
017400080513     d                                           file(PRTEMAIL) +
017500080513     d                                           pagesize(66 132) +
017600080513     d                                           lpi(6) cpi(10) +
017700080513     d                                           ovrscope(*actgrpdfn) +
017800080513     d                                           ')
017900080513     d c_cmddltovr     c                   const('DLTOVR +
018000080513     d                                            file(PRTEMAIL) +
018100080513     d                                            lvl(*actgrpdfn)')
018200070129
018300070129      *------------------------------------------------------------------------*
018400070607
018500070607     c                   eval      kprcprfc = 'AU0000000'
018600070607     c                   eval      %subst(kprcprfc:3:3) = parmpo
018700110609
018800110609      *?Cerco la data del giorno
018900110609     c                   time                    w0140
019000120920      *?Calcolo oggi - 10 gg.
019100110609     c                   move      w0140         data_eur
019200110609     c                   move      data_eur      data_iso
019300120920     c                   subdur    10:*d         data_iso
019400120920     c                   move      data_iso      Oggi_10
019500120920      *?Calcolo oggi + 10 gg.
019600120920     c                   move      w0140         data_eur
019700120920     c                   move      data_eur      data_iso
019800120920     c                   adddur    10:*d         data_iso
019900120920     c                   move      data_iso      Oggi_10o
020000110202
020100110202      * Recupero i codici SPC
020200110202     c                   exsr      sr_carcodspc
020300070129
020400110202      * Leggo file ritorno dati da PDA
020500070130     c     kprcprfc      setll     fiprc01r
020600070129     c                   do        *hival
020700070129     c                   read      fiprc01r
020800070129
020900070129     c                   if        %eof(fiprc01r)
021000070129     c                   leave
021100070129     c                   endif
021200070129      * stesso p.o.
021300070129     c                   if        %subst(prcprfc:3:3) <> parmpo
021400070129     c                   leave
021500070129     c                   endif
021600070129
021700070129     c                   clear                   fiplg000
021800070130     c                   eval      *in01 = *off
021900110620     c                   eval      *in04 = *off
022000070129
022100070130      * controlla i dati inviati
022200070129     c                   exsr      sr_ctrdati
022300070130
022400070130      * aggiorno i dati effettivi in base al tipo record
022500070130      * se non ci sono stati errori nei controlli
022600101104     c                   if        not *in01
022700101104      * e la distinta NON è in test per gli ORM
022800080317     c                             and §dsttstpda <> 'O' and §dsttstpda <> 'E'
022900080205     c                   exsr      sr_file
023000080205     c                   endif
023100080724
023200080724      * scrivo fase RCH SEMPRE quando ricevo rcd RCH
023300080924      * passo in kpjbu i dati da memorizzare nel campo "dati" sul file delle fasi
023400090810     c                   if        prctiporcd = 'RCH' and not *in01
023500080724     c                   exsr      sr_fase
023600080724     c                   endif
023700080404
023800080519      * se non ci sono stati errori nei controlli
023900101104      * e la distinta è in test cancello solo i dati di FIPDO e FIPRO
024000101104     c                   if        not *in01 and
024100101104     c                             (§dsttstpda = 'O' or §dsttstpda = 'E')
024200080404      * e ricevo un RCH
024300080404     c                   if        plgtiporcd = 'RCH'
024400080404      * devo cancellare i dati da FIPDO e da FIPRO
024500080404     c                   exsr      sr_fipdo
024600080404     c                   exsr      sr_fipro
024700080404     c                   endif
024800080404     c                   endif
024900070202
025000080320      * se non ci sono stati errori nei controlli
025100080320      * tipo record 'OK' solo per i ritiri
025200101104     c                   if        not *in01 and
025300140129     c                             plgtiporcd = 'OK' and §OKTPIDDOC = 'R'
025400080320     c                   exsr      sr_wrtpro
025500080320     c                   endif
025600140129      * scrittura fipro per tipo record ROR
025700140129     c                   if        plgtiporcd = 'ROR'
025800140129     c                   exsr      sr_wrtproror
025900140129     c                   endif
026000070202      * scrivo il file di LOG
026100070202     c                   exsr      sr_wrtlog
026200070202
026300080519      * se non ci sono stati errori nei controlli
026400080415      * se ricevo fase "RES" o fase "RCH" scrivo i file legati al prospetto ORM
026500080415     c                   if        not *in01 and
026600080415     c                             (plgtiporcd = 'RCH' OR plgtiporcd = 'RES')
026700080415     c                   exsr      sr_Prospetto
026800080415     c                   endif
026900080519      * se ci sono stati errori nei controlli inivio mail
027000110620     c**!!!              if        *in01 or *in05
027100110620     c                   if        *in04 or *in05
027200080519     c                   exsr      sr_mail
027300080519     c                   endif
027400080519
027500070205      * se ho avuto errori di allocazione dei file devo
027600070202      * fare il rollback
027700070205    2c                   if        wfiqdt = *on or wfipdo = *on or
027800080327     c                             wfnorm = *on or wfipro = *on or
027900080327     c                             wfase = *on
028000070202     c                   rolbk
028100070205      * se non ho avuto errori
028200070205   2xc                   else
028300070205      * aggancio il file fisico di ricezione
028400070205     c     prcnrr        chain(e)  fiprc00r
028500070205      * se rcd allocato flag di comodo per poi fare il rollback
028600070205      * per errore rollback
028700070205    3c                   if        %error
028800070205     c                   rolbk
028900070205   3xc                   else
029000070205    4c                   if        %found(fiprc00r)
029100070202      * cancello il file di ricezione
029200070202     c                   delete    fiprc000
029300070205      * committo le modifiche
029400070205     c                   commit
029500070205    4c                   endif
029600070205    3c                   endif
029700070205    2c                   endif
029800070202
029900080327     c                   eval      wfase = *off
030000070202     c                   eval      wfiqdt = *off
030100070202     c                   eval      wfipdo = *off
030200070202     c                   eval      wfnorm = *off
030300080319     c                   eval      wfipro = *off
030400070202
030500070129     c                   enddo
030600080513
030700080513     c                   exsr      sr_end
030800080227
030900080226      * testa la chiusura del sottosistema
031000080226     c                   shtdn                                        99
031100080226     c   99              eval      chiudi = 'S'
031200070129
031300070205     c                   eval      *inrt = *on
031400110202
031500110202      *------------------------------------------------------------------------*
031600110202      * RECUPERO CODICI SPC
031700110202      *------------------------------------------------------------------------*
031800110202     c     sr_carcodspc  begsr
031900110202
032000110202     c                   clear                   codspc
032100110202     c                   clear                   desspc
032200110202
032300110202      * recupero tabella codici SPC (PSP)
032400110202      * chiave 1 per le prime 11 password
032500110202     c                   clear                   tibs02ds
032600110202     c                   clear                   dpsp
032700110202     c                   eval      t02mod = 'C'
032800110202     c                   eval      t02sif = knsif
032900110202     c                   eval      t02cod = 'PSP'
033000110202     c                   eval      t02ke1 = parmpo
033100110202     c                   eval      t02ke2 = '1'
033200110202     c                   call      'TIBS02R'
033300110202     c                   parm                    kpjba
033400110202     c                   parm                    tibs02ds
033500110202     c                   if        t02err = *blanks
033600110202     c                   eval      dpsp = t02uni
033700110202     c                   endif
033800110202
033900110202     c                   if        dpsp <> *blanks
034000110202     c                   eval      codspc(01) = §pspcod01
034100110202     c                   eval      desspc(01) = §pspdes01
034200110202     c                   eval      codspc(02) = §pspcod02
034300110202     c                   eval      desspc(02) = §pspdes02
034400110202     c                   eval      codspc(03) = §pspcod03
034500110202     c                   eval      desspc(03) = §pspdes03
034600110202     c                   eval      codspc(04) = §pspcod04
034700110202     c                   eval      desspc(04) = §pspdes04
034800110202     c                   eval      codspc(05) = §pspcod05
034900110202     c                   eval      desspc(05) = §pspdes05
035000110202     c                   eval      codspc(06) = §pspcod06
035100110202     c                   eval      desspc(06) = §pspdes06
035200110202     c                   eval      codspc(07) = §pspcod07
035300110202     c                   eval      desspc(07) = §pspdes07
035400110202     c                   eval      codspc(08) = §pspcod08
035500110202     c                   eval      desspc(08) = §pspdes08
035600110202     c                   eval      codspc(09) = §pspcod09
035700110202     c                   eval      desspc(09) = §pspdes09
035800110202     c                   eval      codspc(10) = §pspcod10
035900110202     c                   eval      desspc(10) = §pspdes10
036000110202     c                   eval      codspc(11) = §pspcod11
036100110202     c                   eval      desspc(11) = §pspdes11
036200110202     c                   endif
036300110202      * chiave 2 per le altre 11 password
036400110202     c                   clear                   dpsp
036500110202     c                   eval      t02ke2 = '2'
036600110202     c                   call      'TIBS02R'
036700110202     c                   parm                    kpjba
036800110202     c                   parm                    tibs02ds
036900110202     c                   if        t02err = *blanks
037000110202     c                   eval      dpsp = t02uni
037100110202     c                   endif
037200110202
037300110202     c                   if        dpsp <> *blanks
037400110202     c                   eval      codspc(12) = §pspcod01
037500110202     c                   eval      desspc(12) = §pspdes01
037600110202     c                   eval      codspc(13) = §pspcod02
037700110202     c                   eval      desspc(13) = §pspdes02
037800110202     c                   eval      codspc(14) = §pspcod03
037900110202     c                   eval      desspc(14) = §pspdes03
038000110202     c                   eval      codspc(15) = §pspcod04
038100110202     c                   eval      desspc(15) = §pspdes04
038200110202     c                   eval      codspc(16) = §pspcod05
038300110202     c                   eval      desspc(16) = §pspdes05
038400110202     c                   eval      codspc(17) = §pspcod06
038500110202     c                   eval      desspc(17) = §pspdes06
038600110202     c                   eval      codspc(18) = §pspcod07
038700110202     c                   eval      desspc(18) = §pspdes07
038800110202     c                   eval      codspc(19) = §pspcod08
038900110202     c                   eval      desspc(19) = §pspdes08
039000110202     c                   eval      codspc(20) = §pspcod09
039100110202     c                   eval      desspc(20) = §pspdes09
039200110202     c                   eval      codspc(21) = §pspcod10
039300110202     c                   eval      desspc(21) = §pspdes10
039400110202     c                   eval      codspc(22) = §pspcod11
039500110202     c                   eval      desspc(22) = §pspdes11
039600110202     c                   endif
039700110202
039800110202      * recupero tabella codici SPC (PSP)
039900110202      * la password di sede
040000110202     c                   clear                   tibs02ds
040100110202     c                   clear                   dpsps
040200110202     c                   eval      t02mod = 'C'
040300110202     c                   eval      t02sif = knsif
040400110202     c                   eval      t02cod = 'PSP'
040500110202     c                   eval      t02ke1 = '046'
040600110202     c                   call      'TIBS02R'
040700110202     c                   parm                    kpjba
040800110202     c                   parm                    tibs02ds
040900110202     c                   if        t02err = *blanks
041000110202     c                   eval      dpsps = t02uni
041100110202     c                   endif
041200110202     c                   if        dpsps <> *blanks
041300110202     c                   eval      codspc(23) = §pspcods
041400110202     c                   eval      desspc(23) = §pspdess
041500110202     c                   endif
041600110202
041700110202     c                   endsr
041800070129
041900070129      *------------------------------------------------------------------------*
042000070129      * ROUTINE CONTROLLO DATI FIPRC
042100070129      *------------------------------------------------------------------------*
042200070129     c     sr_ctrdati    begsr
042300080924
042400080924     c                   clear                   fidsfrchds
042500081103     c                   eval      *in05 = *off
042600070129
042700070131      * tipo record
042800070131      * se errato non posso continuare perchè non so come leggere i dati
042900070201     c                   if        prctiporcd <> 'OK ' and
043000070201     c                             prctiporcd <> 'RCH' and
043100140129     c                             prctiporcd <> 'ROR' and
043200070201     c                             prctiporcd <> 'RES'
043300070131     c                   eval      plgerr = 'E'
043400070131     c                   eval      plgmsg = 'TIPO RECORD NON VALIDO'
043500070131     c                   eval      *in01 = *on
043600110620     c                   eval      *in04 = *on
043700070131     c                   leavesr
043800070131     c                   endif
043900070131     c                   eval      plgtiporcd = prctiporcd
044000070131
044100070129      * codice autotrasportatore
044200070130      * anche se errato vado avanti lo stesso con i controlli ma lascio l'errore
044300070131     c                   eval      wcodaut = %subst(prcprfc:3:7)
044400070201      *  deve essere numerico
044500070201     c                   eval      w030a = *all'0'
044600070201     c                   movel     wcodaut       w030a
044700070201     c                   exsr      sr_testn
044800070201     c                   if        not *in02
044900070201     c                   eval      plgerr = 'E'
045000070201     c                   eval      plgmsg = 'AUTOTRASPORTATORE NON VALIDO'
045100070201     c                   eval      *in01 = *on
045200110620     c                   eval      *in04 = *on
045300070201     c                   else
045400070201      *  deve esistere
045500070129     c                   eval      apdtip = 'A'
045600070202     c                   move      wcodaut       apdpdr
045700070129     c     kfiapd        chain     fiapd01l
045800070129     c                   if        not %found(fiapd01l) or
045900070129     c                             apdatb <> *blanks
046000070129     c                   eval      plgerr = 'E'
046100070129     c                   eval      plgmsg = 'AUTOTRASPORTATORE NON VALIDO'
046200070130     c                   eval      *in01 = *on
046300110620     c                   eval      *in04 = *on
046400070130     c                   endif
046500070201     c                   endif
046600070130     c  n01              move      wcodaut       plgcodaut
046700070129
046800070129      * campo dati
046900070129     c                   select
047000070129     c                   when      prctiporcd = 'OK '
047100070129     c                   exsr      sr_elabok
047200070129     c                   when      prctiporcd = 'RCH'
047300070129     c                   exsr      sr_elabrch
047400070129     c                   when      prctiporcd = 'RES'
047500070129     c                   exsr      sr_elabres
047600140129     c                   when      prctiporcd = 'ROR'
047700140129     c                   exsr      sr_elabror
047800070129     c                   endsl
047900070129
048000070129     c                   endsr
048100070129
048200070129      *------------------------------------------------------------------------*
048300070130      * ROUTINE ELABORAZIONE RCD "OK"
048400070129      *------------------------------------------------------------------------*
048500070129     c     sr_elabok     begsr
048600070129
048700070129     c                   eval      fiprcokds = prcdati
048800070129
048900070129      * p.o. gestione distinta
049000070201     c                   eval      wfgs = §okfgs
049100070201     c                   exsr      sr_ctrfgs
049200070201     c   02              move      §okfgs        plgfgs
049300070129
049400070129      * numero distinta
049500070201     c                   eval      wndc = §okndc
049600070201     c                   exsr      sr_ctrndc
049700070201     c   02              move      §okndc        plgndc
049800070201
049900070201      * controllo se esiste la distinta
050000070201     c   02              exsr      sr_ctrdist
050100070129
050200070129      * tipo applicazione
050300070130      * può essere C=consegne R=ritiri
050400070131     c                   if        §oktpiddoc <> 'C' and §oktpiddoc <> 'R'
050500070130     c  n01              eval      plgerr = 'E'
050600070130     c  n01              eval      plgmsg = 'DATI NON VALIDI'
050700070130     c  n01              eval      *in01 = *on
050800110620     c  n01              eval      *in04 = *on
050900070130     c                   else
051000070131     c                   eval      plgtipappl = §oktpiddoc
051100070130     c                   endif
051200070129
051300070129      * id documento
051400080228      *  solo ritiro
051500080228     c                   if        §oktpiddoc = 'R'
051600070201      *  deve essere numerico
051700070201     c                   eval      w030a = *all'0'
051800070201     c                   eval      %subst(w030a:1:14) = %subst(§okiddoc:1:14)
051900070201     c                   exsr      sr_testn
052000070201     c                   if        not *in02
052100070201     c  n01              eval      plgerr = 'E'
052200070201     c  n01              eval      plgmsg = 'DATI ID DOC. ORM NON VALIDI'
052300070201     c  n01              eval      *in01 = *on
052400110620     c  n01              eval      *in04 = *on
052500070201     c                   else
052600070201      *  deve essere un n. di ORM valido
052700070201     c                   move      §okpoe        ormpoe
052800070201     c                   move      §oknsr        ormnsr
052900070201     c                   move      §oknor        ormnor
053000070201     c                   move      §oknrv        ormnrv
053100070201     c     kfnorm        chain(n)  fnorm01l
053200070201     c                   if        not %found(fnorm01l)
053300070201     c  n01              eval      plgerr = 'E'
053400070201     c  n01              eval      plgmsg = 'ORM INESISTENTE'
053500070201     c  n01              eval      *in01 = *on
053600110620     c  n01              eval      *in04 = *on
053700070201     c                   endif
053800070201     c                   endif
053900070201     c  n01              eval      plgiddoc = §okiddoc
054000080228     c                   endif
054100070213
054200070213      * data conferma
054300070213      *  deve essere numerico
054400070213     c                   eval      w030a = *all'0'
054500070213     c                   movel     §okdata       w030a
054600070213     c                   exsr      sr_testn
054700070213     c                   if        not *in02
054800070213     c  n01              eval      plgerr = 'E'
054900070213     c  n01              eval      plgmsg = 'DATA CONFERMA NON VALIDA'
055000070213     c  n01              eval      *in01 = *on
055100110620     c  n01              eval      *in04 = *on
055200070213     c                   endif
055300070213      *  deve essere una data valida
055400070213     c                   if        *in02
055500070213     c                   clear                   wlbdat
055600070213     c                   move      §okdata       g02inv
055700070213     c                   eval      g02err = '3'
055800070213     c                   call      'XSRDA8'
055900070213     c                   parm                    wlbdat
056000070213     c                   if        g02err = '1'
056100070213     c  n01              eval      plgerr = 'E'
056200070213     c  n01              eval      plgmsg = 'DATA CONFERMA NON VALIDA'
056300070213     c  n01              eval      *in01 = *on
056400110620     c  n01              eval      *in04 = *on
056500070213     c                   endif
056600070213     c                   endif
056700070213
056800070213      * ora conferma
056900070213      *  deve essere numerico
057000070213     c                   eval      w030a = *all'0'
057100070213     c                   movel     §okora        w030a
057200070213     c                   exsr      sr_testn
057300070213     c                   if        not *in02
057400070213     c  n01              eval      plgerr = 'E'
057500070213     c  n01              eval      plgmsg = 'ORA CONFERMA NON VALIDA'
057600070213     c  n01              eval      *in01 = *on
057700110620     c  n01              eval      *in04 = *on
057800070213     c                   endif
057900070213      *  deve essere un'ora valida
058000070213     c                   if        *in02 and
058100070213     c                             %subst(§okora:1:2) > '23' or
058200070213     c                             %subst(§okora:3:2) > '59' or
058300070213     c                             %subst(§okora:5:2) > '59'
058400070213     c  n01              eval      plgerr = 'E'
058500070213     c  n01              eval      plgmsg = 'ORA CONFERMA NON VALIDA'
058600080319     c  n01              eval      *in01 = *on
058700110620     c  n01              eval      *in04 = *on
058800070213     c                   endif
058900110609      /free
059000110609       //?La data di conferma ORM su rcd OK non può essere inferiore a 30 gg da oggi
059100110609       //?in questo caso accetto il rcd ma imposto la data della distinta
059200110609       //?e come ora imposto l'ora di ricezione del record
059300110609       //?è capitato che PDA si è resettato ed ha riportato la data al 1980
059400120920         IF  not *in01 and §OKdata < %editc(Oggi_10:'X');
059500110609           §OKdata = %editc(DSTdfv:'X');
059600110609           §OKora  = %subst(PRCdatora:9:6);
059700121015           PLGmsg  = 'DATA < o > di 10 GG';
059800110609         ENDIF;
059900120920       //?E' capitato anche che il PDA è andato fuori di testa ed ha impostato
060000120920       //?la data a 2514
060100120920         IF  not *in01 and §OKdata > %editc(Oggi_10o:'X');
060200120920           §OKdata = %editc(DSTdfv:'X');
060300121015           %subst(§OKora:1:4)  = %subst(PRCdatora:9:4);
060400121010           PLGmsg  = 'DATA < o > di 10 GG';
060500120920         ENDIF;
060600110609      /end-free
060700070201
060800070201      * flag richiesta assistenza
060900140207     c                   if        §okflgass  <> *blank
061000140213     c                   movel     'RAS'         tbecod
061100140213     c                   movel     §okflgass     tbeke1
061200140207     c     ktbe          chain     tntbe01l
061300140207     c                   if        not %found(tntbe01l)
061400070201     c  n01              eval      plgerr = 'E'
061500070201     c  n01              eval      plgmsg = 'FLAG RICH. ASS. NON VALIDO'
061600070201     c  n01              eval      *in01 = *on
061700110620     c  n01              eval      *in04 = *on
061800070201     c                   endif
061900140225      * non memorizziamo richiesta assistenza per ORM RC
062000140225     c                   IF        ORMnsr > 0
062100140225     c  n01              eval      plgerr = 'E'
062200140225     c  n01              eval      plgmsg = 'RICH. ASSISTENZA SU RC'
062300140225     c  n01              eval      *in01 = *on
062400140225     c  n01              eval      *in04 = *on
062500140225     c                   ENDIF
062600140207     c                   endif
062700080221
062800080221      * identificativo riga
062900080319     c                   clear                   Widrow
063000080221     c                   eval      w030a = *all'0'
063100080221     c                   movel     §okidrow      w030a
063200080221     c                   exsr      sr_testn
063300080221     c                   if        not *in02
063400080221     c  n01              eval      plgerr = 'E'
063500080221     c  n01              eval      plgmsg = 'ID. RIGA OK NON VALIDO'
063600080221     c  n01              eval      *in01 = *on
063700110620     c  n01              eval      *in04 = *on
063800080319
063900080319     c                   else
064000080319     c                   move      §okidrow      Widrow
064100080221     c                   endif
064200170428
064300170428      /free
064400170428       //?Alla fine di tutti i controlli se non ho errore controllo se l'ok ricevuto
064500170428       //?è relativo ad un annullamento ORM
064600170428         IF  not *in01;
064700170428         //?Cerco l'ORM su FIPDO a parità di IDrow se Annullato imposto PLGMSG
064800170428         //?come fa il pgm ricezione OK per le consegne
064900170428           setll (PLGfgs:PLGndc:ORMpoe:ORMnsr:ORMnor:ORMnrv) FIPDO01T;
065000170428           reade (PLGfgs:PLGndc:ORMpoe:ORMnsr:ORMnor:ORMnrv) FIPDO01T;
065100170428           DOW  not %eof(FIPDO01T);
065200170428             IF  PDOidrow = wIdRow and PDOatb = 'A';
065300170428               PLGmsg = 'CONFERMA ANNULLAMENTO';
065400170428             ENDIF;
065500170502             reade (PLGfgs:PLGndc:ORMpoe:ORMnsr:ORMnor:ORMnrv) FIPDO01T;
065600170428           ENDDO;
065700170428         ENDIF;
065800170428      /end-free
065900070129
066000070129     c                   endsr
066100070130
066200070130      *------------------------------------------------------------------------*
066300070130      * ROUTINE ELABORAZIONE RCD "RCH"
066400070130      *------------------------------------------------------------------------*
066500070130     c     sr_elabrch    begsr
066600070130
066700070130     c                   eval      fiprcrchds = prcdati
066800070130
066900070130      * p.o. gestione distinta
067000070201     c                   eval      wfgs = §rchfgs
067100070201     c                   exsr      sr_ctrfgs
067200070201     c   02              move      §rchfgs       plgfgs
067300070130
067400070130      * numero distinta
067500070201     c                   eval      wndc = §rchndc
067600070201     c                   exsr      sr_ctrndc
067700070201     c   02              move      §rchndc       plgndc
067800070201
067900070201      * controllo se esiste la distinta
068000070201     c   02              exsr      sr_ctrdist
068100070201
068200070201      * controllo se stesso AUT
068300070201     c                   if        *in02 and plgcodaut <> dstpdr
068400070201     c  n01              eval      plgerr = 'E'
068500070201     c  n01              eval      plgmsg = 'AUT. INCONGRUENTE'
068600070201     c  n01              eval      *in01 = *on
068700110620     c  n01              eval      *in04 = *on
068800070201     c                   endif
068900070201
069000070201      * la distinta deve esistere anche su FIQDT
069100070201     c                   if        *in02
069200070201     c     kfiqdt        chain(n)  fiqdt01l
069300070201     c                   if        not %found(fiqdt01l) or qdtatb <> *blanks
069400070201     c  n01              eval      plgerr = 'E'
069500070201     c  n01              eval      plgmsg = 'DISTINTA INESISTENTE SU FIQDT'
069600070201     c  n01              eval      *in01 = *on
069700110620     c  n01              eval      *in04 = *on
069800070201     c                   endif
069900070201     c                   endif
070000070130
070100070130      * totale colli
070200070130      *  deve essere numerico
070300070201     c                   eval      w030a = *all'0'
070400070201     c                   movel     §rchncl       w030a
070500070201     c                   exsr      sr_testn
070600070201     c                   if        not *in02
070700070130     c  n01              eval      plgerr = 'E'
070800070201     c  n01              eval      plgmsg = 'DATI TOT. COLLI NON VALIDI'
070900070130     c  n01              eval      *in01 = *on
071000110620     c  n01              eval      *in04 = *on
071100070201     c                   else
071200070201     c                   move      §rchncl       wncl
071300070130     c                   endif
071400070130
071500070130      * totale bancali
071600070130      *  deve essere numerico
071700070201     c                   eval      w030a = *all'0'
071800070201     c                   movel     §rchbnc       w030a
071900070201     c                   exsr      sr_testn
072000070201     c                   if        not *in02
072100070130     c  n01              eval      plgerr = 'E'
072200070201     c  n01              eval      plgmsg = 'DATI TOT. BANCALI NON VALIDI'
072300070130     c  n01              eval      *in01 = *on
072400110620     c  n01              eval      *in04 = *on
072500070201     c                   else
072600070201     c                   move      §rchbnc       wbnc
072700070130     c                   endif
072800070213
072900070213      * data chiusura
073000070213      *  deve essere numerico
073100070213     c                   eval      w030a = *all'0'
073200070213     c                   movel     §rchdata      w030a
073300070213     c                   exsr      sr_testn
073400070213     c                   if        not *in02
073500070213     c  n01              eval      plgerr = 'E'
073600070213     c  n01              eval      plgmsg = 'DATA CHIUSURA NON VALIDA'
073700070213     c  n01              eval      *in01 = *on
073800110620     c  n01              eval      *in04 = *on
073900070213     c                   endif
074000070213      *  deve essere una data valida
074100070213     c                   if        *in02
074200070213     c                   clear                   wlbdat
074300070213     c                   move      §rchdata      g02inv
074400070213     c                   eval      g02err = '3'
074500070213     c                   call      'XSRDA8'
074600070213     c                   parm                    wlbdat
074700070213     c                   if        g02err = '1'
074800070213     c  n01              eval      plgerr = 'E'
074900070213     c  n01              eval      plgmsg = 'DATA CHIUSURA NON VALIDA'
075000070213     c  n01              eval      *in01 = *on
075100110620     c  n01              eval      *in04 = *on
075200070213     c                   endif
075300070213     c                   endif
075400070213
075500070213      * ora chiusura
075600070213      *  deve essere numerico
075700070213     c                   eval      w030a = *all'0'
075800070213     c                   movel     §rchora       w030a
075900070213     c                   exsr      sr_testn
076000070213     c                   if        not *in02
076100070213     c  n01              eval      plgerr = 'E'
076200070213     c  n01              eval      plgmsg = 'ORA CHIUSURA NON VALIDA'
076300070213     c  n01              eval      *in01 = *on
076400110620     c  n01              eval      *in04 = *on
076500070213     c                   endif
076600070213      *  deve essere un'ora valida
076700070213     c                   if        *in02 and
076800070213     c                             %subst(§rchora:1:2) > '23' or
076900070213     c                             %subst(§rchora:3:2) > '59' or
077000070213     c                             %subst(§rchora:5:2) > '59'
077100070213     c  n01              eval      plgerr = 'E'
077200070213     c  n01              eval      plgmsg = 'ORA CHIUSURA NON VALIDA'
077300070213     c  n01              eval      *in01 = *on
077400110620     c  n01              eval      *in04 = *on
077500070213     c                   endif
077600070514
077700110609      /free
077800110609       //?La data di chiusura distinta su rcd RCH non può essere inferiore a 30 gg da oggi
077900110609       //?in questo caso accetto il rcd ma imposto la data della distinta
078000110609       //?e come ora imposto l'ora di ricezione del record
078100110609       //?è capitato che PDA si è resettato ed ha riportato la data al 1980
078200120920         IF  not *in01 and §RCHdata < %editc(Oggi_10:'X');
078300110609           §RCHdata = %editc(DSTdfv:'X');
078400121015           %subst(§RCHora:1:4)  = %subst(PRCdatora:9:4);
078500121010           PLGmsg   = 'DATA < o > di 10 GG';
078600110609         ENDIF;
078700120920       //?E' capitato anche che il PDA è andato fuori di testa ed ha impostato
078800120920       //?la data a 2514
078900120920         IF  not *in01 and §RCHdata > %editc(Oggi_10o:'X');
079000120920           §RCHdata = %editc(DSTdfv:'X');
079100121015           %subst(§RCHora:1:4)  = %subst(PRCdatora:9:4);
079200121010           PLGmsg  = 'DATA < o > di 10 GG';
079300120920         ENDIF;
079400110609      /end-free
079500110609
079600070514      * imposto il tipo applicazione anche se non previsto dalla ds
079700070514     c                   eval      plgtipappl = 'R'
079800080513
079900080513      * controllo codice SPC
080000080721     c                   eval      *in05 = *off
080100080519     c                   if        §rchcodspc <> *blanks
080200080513     c                   exsr      sr_ctrcodspc
080300080513     c                   endif
080400070130
080500070130     c                   endsr
080600070130
080700070130      *------------------------------------------------------------------------*
080800070130      * ROUTINE ELABORAZIONE RCD "RES"
080900070130      *------------------------------------------------------------------------*
081000070130     c     sr_elabres    begsr
081100070130
081200070131     c                   eval      *in03 = *off
081300070130     c                   eval      fiprcresds = prcdati
081400070201
081500070201      * causale
081600070201      *  deve esistere
081700070201     c                   if        §rescmr <> *blanks
081800081106      * se causale 'AAA' ok...annullamento di ORM Manuale
081900081106     c                             and §rescmr <> 'AAA'
082000070201     c                   clear                   tibs02ds
082100070201     c                   eval      t02mod = 'C'
082200070201     c                   eval      t02sif = knsif
082300070201     c                   eval      t02cod = 'CMR'
082400070201     c                   eval      t02ke1 = §rescmr
082500070201     c                   call      'TIBS02R'
082600070201     c                   parm                    kpjba
082700070201     c                   parm                    tibs02ds
082800070201     c                   if        t02err <> *blanks
082900070201     c  n01              eval      plgerr = 'E'
083000070201     c  n01              eval      plgmsg = 'CAU. MANCATO RIT. INESISTENTE'
083100070201     c  n01              eval      *in01 = *on
083200110620     c  n01              eval      *in04 = *on
083300070201     c                   endif
083400070201     c                   endif
083500070130
083600070130      * p.o. gestione distinta
083700070201     c                   eval      wfgs = §resfgs
083800070201     c                   exsr      sr_ctrfgs
083900070201     c   02              move      §resfgs       plgfgs
084000070130
084100070130      * numero distinta
084200070201     c                   eval      wndc = §resndc
084300070201     c                   exsr      sr_ctrndc
084400070201     c   02              move      §resndc       plgndc
084500070201
084600070201      * controllo se esiste la distinta
084700070201     c   02              exsr      sr_ctrdist
084800070201
084900070201      * controllo se stesso AUT
085000070201     c                   if        *in02 and plgcodaut <> dstpdr
085100070201     c  n01              eval      plgerr = 'E'
085200070201     c  n01              eval      plgmsg = 'AUT. INCONGRUENTE'
085300070201     c  n01              eval      *in01 = *on
085400110620     c  n01              eval      *in04 = *on
085500070201     c                   endif
085600070201
085700070201      * numero ORM
085800070201      *  deve essere numerico
085900070201     c                   eval      w030a = *all'0'
086000070201     c                   eval      %subst(w030a:1:14) = %subst(prcdati:10:14)
086100070201     c                   exsr      sr_testn
086200070201     c                   if        not *in02
086300070201     c  n01              eval      plgerr = 'E'
086400070201     c  n01              eval      plgmsg = 'DATI ID DOC. ORM NON VALIDI'
086500070201     c  n01              eval      *in01 = *on
086600110620     c  n01              eval      *in04 = *on
086700070201     c                   else
086800070201      *  controllo se ORM manuale
086900070521     c                   if        %subst(prcdati:10:8) = *all'0'
087000070521     c                   eval      w006a = %subst(prcdati:18:6)
087100070521     c                   move      w006a         wora
087200070521     c     *eur          test(te)                wora
087300070521     c                   if        %error
087400070201     c  n01              eval      plgerr = 'E'
087500070201     c  n01              eval      plgmsg = 'DATI ID DOC. ORM NON VALIDI'
087600070201     c  n01              eval      *in01 = *on
087700110620     c  n01              eval      *in04 = *on
087800070201     c                   else
087900070201     c                   eval      *in03 = *on
088000080408     c                   eval      plgiddoc = %subst(prcdati:10:14)
088100070201     c                   endif
088200070202     c                   endif
088300070202     c                   endif
088400070130
088500070201      * controllo la chiave ORM
088600070201      * se non è un ORM manuale
088700070201     c                   if        not *in03
088800070202     c                   clear                   ormpoe
088900070202     c                   clear                   ormnsr
089000070202     c                   clear                   ormnor
089100070202     c                   clear                   ormnrv
089200070130      * p.o. emissione
089300070130      *  deve essere numerico
089400070201     c                   eval      w030a = *all'0'
089500070201     c                   movel     §respoe       w030a
089600070201     c                   exsr      sr_testn
089700070201     c                   if        not *in02
089800070130     c  n01              eval      plgerr = 'E'
089900070201     c  n01              eval      plgmsg = 'DATI P.O. EMISSIONE NON VALIDI'
090000070130     c  n01              eval      *in01 = *on
090100110620     c  n01              eval      *in04 = *on
090200070201     c                   else
090300070201     c                   move      §respoe       ormpoe
090400070130     c                   endif
090500070130
090600070130      * numero serie
090700070130      *  deve essere numerico
090800070201     c                   eval      w030a = *all'0'
090900070201     c                   movel     §resnsr       w030a
091000070201     c                   exsr      sr_testn
091100070201     c                   if        not *in02
091200070130     c  n01              eval      plgerr = 'E'
091300070201     c  n01              eval      plgmsg = 'DATI N. SERIE NON VALIDI'
091400070130     c  n01              eval      *in01 = *on
091500110620     c  n01              eval      *in04 = *on
091600070201     c                   else
091700070201     c                   move      §resnsr       ormnsr
091800070130     c                   endif
091900070130
092000070130      * numero orm
092100070130      *  deve essere numerico
092200070201     c                   eval      w030a = *all'0'
092300070201     c                   movel     §resnor       w030a
092400070201     c                   exsr      sr_testn
092500070201     c                   if        not *in02
092600070130     c  n01              eval      plgerr = 'E'
092700070201     c  n01              eval      plgmsg = 'DATI N. ORM NON VALIDI'
092800070130     c  n01              eval      *in01 = *on
092900110620     c  n01              eval      *in04 = *on
093000070201     c                   else
093100070201     c                   move      §resnor       ormnor
093200070130     c                   endif
093300070130      * numero viaggio
093400070130      *  deve essere numerico
093500070201     c                   eval      w030a = *all'0'
093600070201     c                   movel     §resnrv       w030a
093700070201     c                   exsr      sr_testn
093800070201     c                   if        not *in02
093900070130     c  n01              eval      plgerr = 'E'
094000070201     c  n01              eval      plgmsg = 'DATI N. VIAGGIO NON VALIDI'
094100070130     c  n01              eval      *in01 = *on
094200110620     c  n01              eval      *in04 = *on
094300070201     c                   else
094400070201     c                   move      §resnrv       ormnrv
094500070130     c                   endif
094600070131
094700070201      * aggancio l'ORM
094800070201     c     kfnorm        chain(n)  fnorm01l
094900070131     c                   if        %found(fnorm01l)
095000070201      * verifico se la distinta presente sull'ORM è la stessa del PDA
095100070201     c                   if        ormndc <> plgndc
095200070131     c  n01              eval      plgerr = 'E'
095300070131     c  n01              eval      plgmsg = 'DISTINTA INCONGRUENTE'
095400070131     c  n01              eval      *in01 = *on
095500070201     c                   endif
095600070201      * verifico la fase dell'ORM
095700070201     c                   if        ormfao < 400 or ormfao >= 500
095800070131     c  n01              eval      plgerr = 'E'
095900070131     c  n01              eval      plgmsg = 'ESITO/FASE INCONGRUENTI'
096000070131     c  n01              eval      *in01 = *on
096100070201     c                   endif
096200070131     c                   endif
096300070207     c  n01              eval      %subst(plgiddoc:1:3) = §respoe
096400070207     c  n01              eval      %subst(plgiddoc:4:2) = §resnsr
096500070207     c  n01              eval      %subst(plgiddoc:6:7) = §resnor
096600070207     c  n01              eval      %subst(plgiddoc:13:2) = §resnrv
096700070201     c                   endif
096800070213
096900070213      * data conferma
097000070213      *  deve essere numerico
097100070213     c                   eval      w030a = *all'0'
097200070213     c                   movel     §resdata      w030a
097300070213     c                   exsr      sr_testn
097400070213     c                   if        not *in02
097500070213     c  n01              eval      plgerr = 'E'
097600070213     c  n01              eval      plgmsg = 'DATA CONFERMA NON VALIDA'
097700070213     c  n01              eval      *in01 = *on
097800110620     c  n01              eval      *in04 = *on
097900070213     c                   endif
098000070213      *  deve essere una data valida
098100070213     c                   if        *in02
098200070213     c                   clear                   wlbdat
098300070213     c                   move      §resdata      g02inv
098400070213     c                   eval      g02err = '3'
098500070213     c                   call      'XSRDA8'
098600070213     c                   parm                    wlbdat
098700070213     c                   if        g02err = '1'
098800070213     c  n01              eval      plgerr = 'E'
098900070213     c  n01              eval      plgmsg = 'DATA CONFERMA NON VALIDA'
099000070213     c  n01              eval      *in01 = *on
099100110620     c  n01              eval      *in04 = *on
099200070213     c                   endif
099300070213     c                   endif
099400070213
099500070213      * ora conferma
099600070213      *  deve essere numerico
099700070213     c                   eval      w030a = *all'0'
099800070213     c                   movel     §resora       w030a
099900070213     c                   exsr      sr_testn
100000070213     c                   if        not *in02
100100070213     c  n01              eval      plgerr = 'E'
100200070213     c  n01              eval      plgmsg = 'ORA CONFERMA NON VALIDA'
100300070213     c  n01              eval      *in01 = *on
100400110620     c  n01              eval      *in04 = *on
100500070213     c                   endif
100600070213      *  deve essere un'ora valida
100700070213     c                   if        *in02 and
100800070213     c                             %subst(§resora:1:2) > '23' or
100900070213     c                             %subst(§resora:3:2) > '59' or
101000070213     c                             %subst(§resora:5:2) > '59'
101100070213     c  n01              eval      plgerr = 'E'
101200070213     c  n01              eval      plgmsg = 'ORA CONFERMA NON VALIDA'
101300070213     c  n01              eval      *in01 = *on
101400110620     c  n01              eval      *in04 = *on
101500070213     c                   endif
101600110609
101700110609      /free
101800110609       //?La data di immissione esito su rcd RES non può essere inferiore a 30 gg da oggi
101900110609       //?in questo caso accetto il rcd ma imposto la data della distinta
102000110609       //?e come ora imposto l'ora di ricezione del record
102100110609       //?è capitato che PDA si è resettato ed ha riportato la data al 1980
102200120920         IF  not *in01 and §RESdata < %editc(Oggi_10:'X');
102300110609           §RESdata = %editc(DSTdfv:'X');
102400121015           %subst(§RESora:1:4)  = %subst(PRCdatora:9:4);
102500121010           PLGmsg   = 'DATA < o > di 10 GG';
102600110609         ENDIF;
102700120920       //?E' capitato anche che il PDA è andato fuori di testa ed ha impostato
102800120920       //?la data a 2514
102900120920         IF  not *in01 and §RESdata > %editc(Oggi_10o:'X');
103000120920           §RESdata = %editc(DSTdfv:'X');
103100121015           %subst(§RESora:1:4)  = %subst(PRCdatora:9:4);
103200121010           PLGmsg  = 'DATA < o > di 10 GG';
103300120920         ENDIF;
103400110609      /end-free
103500070130
103600070130      * numero colli
103700070130      *  deve essere numerico
103800070201     c                   eval      w030a = *all'0'
103900070201     c                   movel     §resncl       w030a
104000070201     c                   exsr      sr_testn
104100070201     c                   if        not *in02
104200070130     c  n01              eval      plgerr = 'E'
104300070201     c  n01              eval      plgmsg = 'DATI N. COLLI NON VALIDI'
104400070130     c  n01              eval      *in01 = *on
104500110620     c  n01              eval      *in04 = *on
104600070130     c                   endif
104700070130
104800070130      * numero bancali
104900070130      *  deve essere numerico
105000070201     c                   eval      w030a = *all'0'
105100070201     c                   movel     §resbnc       w030a
105200070201     c                   exsr      sr_testn
105300070201     c                   if        not *in02
105400070130     c  n01              eval      plgerr = 'E'
105500070201     c  n01              eval      plgmsg = 'DATI N. BANCALI NON VALIDI'
105600070130     c  n01              eval      *in01 = *on
105700110620     c  n01              eval      *in04 = *on
105800070130     c                   endif
105900070213
106000070213      * data inserimento record
106100070213      *  deve essere numerico
106200070213     c                   eval      w030a = *all'0'
106300070213     c                   movel     §resdatain    w030a
106400070213     c                   exsr      sr_testn
106500070213     c                   if        not *in02
106600070213     c  n01              eval      plgerr = 'E'
106700070213     c  n01              eval      plgmsg = 'DATA INSERIMENTO NON VALIDA'
106800070213     c  n01              eval      *in01 = *on
106900110620     c  n01              eval      *in04 = *on
107000070213     c                   endif
107100070213      *  deve essere una data valida
107200070213     c                   if        *in02
107300070213     c                   clear                   wlbdat
107400070213     c                   move      §resdatain    g02inv
107500070213     c                   eval      g02err = '3'
107600070213     c                   call      'XSRDA8'
107700070213     c                   parm                    wlbdat
107800070213     c                   if        g02err = '1'
107900070213     c  n01              eval      plgerr = 'E'
108000070213     c  n01              eval      plgmsg = 'DATA INSERIMENTO NON VALIDA'
108100070213     c  n01              eval      *in01 = *on
108200110620     c  n01              eval      *in04 = *on
108300070213     c                   endif
108400070213     c                   endif
108500070213
108600070213      * ora conferma
108700070213      *  deve essere numerico
108800070213     c                   eval      w030a = *all'0'
108900070213     c                   movel     §resorain     w030a
109000070213     c                   exsr      sr_testn
109100070213     c                   if        not *in02
109200070213     c  n01              eval      plgerr = 'E'
109300070213     c  n01              eval      plgmsg = 'ORA INSERIMENTO NON VALIDA'
109400070213     c  n01              eval      *in01 = *on
109500110620     c  n01              eval      *in04 = *on
109600070213     c                   endif
109700070213      *  deve essere un'ora valida
109800070213     c                   if        *in02 and
109900070213     c                             %subst(§resorain:1:2) > '23' or
110000070213     c                             %subst(§resorain:3:2) > '59' or
110100070213     c                             %subst(§resorain:5:2) > '59'
110200070213     c  n01              eval      plgerr = 'E'
110300070213     c  n01              eval      plgmsg = 'ORA INSERIMENTO NON VALIDA'
110400070213     c  n01              eval      *in01 = *on
110500110620     c  n01              eval      *in04 = *on
110600070213     c                   endif
110700110609
110800110609      /free
110900110609       //?La data di inserimento esito su rcd RES non può essere inferiore a 30 gg da oggi
111000110609       //?in questo caso accetto il rcd ma imposto la data della distinta
111100110609       //?e come ora imposto l'ora di ricezione del record
111200110609       //?è capitato che PDA si è resettato ed ha riportato la data al 1980
111300120920         IF  not *in01 and §RESdatain < %editc(Oggi_10:'X');
111400110609           §RESdatain = %editc(DSTdfv:'X');
111500121015           %subst(§RESorain:1:4)  = %subst(PRCdatora:9:4);
111600121010           PLGmsg     = 'DATA < o > di 10 GG';
111700110609         ENDIF;
111800120920       //?E' capitato anche che il PDA è andato fuori di testa ed ha impostato
111900120920       //?la data a 2514
112000120920         IF  not *in01 and §RESdatain > %editc(Oggi_10o:'X');
112100120920           §RESdatain = %editc(DSTdfv:'X');
112200121015           %subst(§RESorain:1:4)  = %subst(PRCdatora:9:4);
112300121010           PLGmsg     = 'DATA < o > di 10 GG';
112400120920         ENDIF;
112500110609      /end-free
112600070130
112700070130      * inserito da SPC
112800070130      *  deve essere 'S' o blank
112900070130     c                   if        §resflgspc <> 'S' and
113000070130     c                             §resflgspc <> *blanks
113100070130     c  n01              eval      plgerr = 'E'
113200070201     c  n01              eval      plgmsg = 'FLAG INS. DA SPC NON VALIDO'
113300070130     c  n01              eval      *in01 = *on
113400110620     c  n01              eval      *in04 = *on
113500070130     c                   endif
113600070514
113700070514      * imposto il tipo applicazione anche se non previsto dalla ds
113800070514     c                   eval      plgtipappl = 'R'
113900070130
114000070130     c                   endsr
114100070130
114200140129      *------------------------------------------------------------------------*
114300140129      * ROUTINE ELABORAZIONE RCD "ROR"
114400140129      *------------------------------------------------------------------------*
114500140129     c     sr_elabror    begsr
114600140129
114700140129     c                   eval      fiprcrords = prcdati
114800140129
114900140129      * p.o. gestione distinta
115000140129     c                   eval      wfgs = §rorfgs
115100140129     c                   exsr      sr_ctrfgs
115200140129     c   02              move      §rorfgs       plgfgs
115300140129
115400140129      * numero distinta
115500140129     c                   eval      wndc = §rorndc
115600140129     c                   exsr      sr_ctrndc
115700140207     c   02              move      §rorndc       plgndc
115800140129
115900140129      * controllo se esiste la distinta
116000140129     c   02              exsr      sr_ctrdist
116100140129
116200140129      * tipo applicazione
116300140129      * può essere C=consegne R=ritiri
116400140207     c                   if        §rortpiddo <> 'C' and §rortpiddo <> 'R'
116500140129     c  n01              eval      plgerr = 'E'
116600140129     c  n01              eval      plgmsg = 'DATI NON VALIDI'
116700140129     c  n01              eval      *in01 = *on
116800140129     c  n01              eval      *in04 = *on
116900140129     c                   else
117000140207     c                   eval      plgtipappl = §rortpiddo
117100140129     c                   endif
117200140129
117300140129      * id documento
117400140129      *  solo ritiro
117500140207     c                   if        §rortpiddo = 'R'
117600140129      *  deve essere numerico
117700140129     c                   eval      w030a = *all'0'
117800140129     c                   eval      %subst(w030a:1:14) = %subst(§roriddoc:1:14)
117900140129     c                   exsr      sr_testn
118000140129     c                   if        not *in02
118100140129     c  n01              eval      plgerr = 'E'
118200140129     c  n01              eval      plgmsg = 'DATI ID DOC. ORM NON VALIDI'
118300140129     c  n01              eval      *in01 = *on
118400140129     c  n01              eval      *in04 = *on
118500140129     c                   else
118600140129      *  deve essere un n. di ORM valido
118700140129     c                   move      §rorpoe       ormpoe
118800140129     c                   move      §rornsr       ormnsr
118900140129     c                   move      §rornor       ormnor
119000140129     c                   move      §rornrv       ormnrv
119100140129     c     kfnorm        chain(n)  fnorm01l
119200140129     c                   if        not %found(fnorm01l)
119300140129     c  n01              eval      plgerr = 'E'
119400140129     c  n01              eval      plgmsg = 'ORM INESISTENTE'
119500140129     c  n01              eval      *in01 = *on
119600140129     c  n01              eval      *in04 = *on
119700140129     c                   endif
119800140129     c                   endif
119900140129     c  n01              eval      plgiddoc = §roriddoc
120000140129     c                   endif
120100140129
120200140129      * data conferma
120300140129      *  deve essere numerico
120400140129     c                   eval      w030a = *all'0'
120500140129     c                   movel     §rordata      w030a
120600140129     c                   exsr      sr_testn
120700140129     c                   if        not *in02
120800140129     c  n01              eval      plgerr = 'E'
120900140129     c  n01              eval      plgmsg = 'DATA CONFERMA NON VALIDA'
121000140129     c  n01              eval      *in01 = *on
121100140129     c  n01              eval      *in04 = *on
121200140129     c                   endif
121300140129      *  deve essere una data valida
121400140129     c                   if        *in02
121500140129     c                   clear                   wlbdat
121600140129     c                   move      §rordata      g02inv
121700140129     c                   eval      g02err = '3'
121800140129     c                   call      'XSRDA8'
121900140129     c                   parm                    wlbdat
122000140129     c                   if        g02err = '1'
122100140129     c  n01              eval      plgerr = 'E'
122200140129     c  n01              eval      plgmsg = 'DATA CONFERMA NON VALIDA'
122300140129     c  n01              eval      *in01 = *on
122400140129     c  n01              eval      *in04 = *on
122500140129     c                   endif
122600140129     c                   endif
122700140129
122800140129      * ora conferma
122900140129      *  deve essere numerico
123000140129     c                   eval      w030a = *all'0'
123100140129     c                   movel     §rorora       w030a
123200140129     c                   exsr      sr_testn
123300140129     c                   if        not *in02
123400140129     c  n01              eval      plgerr = 'E'
123500140129     c  n01              eval      plgmsg = 'ORA CONFERMA NON VALIDA'
123600140129     c  n01              eval      *in01 = *on
123700140129     c  n01              eval      *in04 = *on
123800140129     c                   endif
123900140129      *  deve essere un'ora valida
124000140129     c                   if        *in02 and
124100140129     c                             %subst(§rorora:1:2) > '23' or
124200140129     c                             %subst(§rorora:3:2) > '59' or
124300140129     c                             %subst(§rorora:5:2) > '59'
124400140129     c  n01              eval      plgerr = 'E'
124500140129     c  n01              eval      plgmsg = 'ORA CONFERMA NON VALIDA'
124600140129     c  n01              eval      *in01 = *on
124700140129     c  n01              eval      *in04 = *on
124800140129     c                   endif
124900140129      /free
125000140129       //?La data di conferma ORM su rcd OK non può essere inferiore a 30 gg da oggi
125100140129       //?in questo caso accetto il rcd ma imposto la data della distinta
125200140129       //?e come ora imposto l'ora di ricezione del record
125300140129       //?è capitato che PDA si è resettato ed ha riportato la data al 1980
125400140129         IF  not *in01 and §rordata < %editc(Oggi_10:'X');
125500140207           §rordata = %editc(DSTdfv:'X');
125600140129           §rorora  = %subst(PRCdatora:9:6);
125700140129           PLGmsg  = 'DATA < o > di 10 GG';
125800140129         ENDIF;
125900140129       //?E' capitato anche che il PDA è andato fuori di testa ed ha impostato
126000140129       //?la data a 2514
126100140129         IF  not *in01 and §rordata > %editc(Oggi_10o:'X');
126200140129           §rordata = %editc(DSTdfv:'X');
126300140129           %subst(§rorora:1:4)  = %subst(PRCdatora:9:4);
126400140129           PLGmsg  = 'DATA < o > di 10 GG';
126500140129         ENDIF;
126600140129      /end-free
126700140129
126800140129      * ora stimata
126900140129     c                   clear                   Worasti
127000140129     c                   eval      w030a = *all'0'
127100140129     c                   movel     §rororasti    w030a
127200140129     c                   exsr      sr_testn
127300140129     c                   if        not *in02
127400140129     c  n01              eval      plgerr = 'E'
127500140129     c  n01              eval      plgmsg = 'ORA STIMATA NON VALIDA'
127600140129     c  n01              eval      *in01 = *on
127700140129     c  n01              eval      *in04 = *on
127800140129      *
127900140129     c                   if        %subst(§rorora:1:2) > '23' or
128000140129     c                             %subst(§rorora:3:2) > '59' or
128100140129     c                             %subst(§rorora:5:2) > '59'
128200140129     c  n01              eval      plgerr = 'E'
128300140129     c  n01              eval      plgmsg = 'ORA STIMATA NON VALIDA'
128400140129     c  n01              eval      *in01 = *on
128500140129     c  n01              eval      *in04 = *on
128600140129     c                   endif
128700140129
128800140129     c                   else
128900140129     c                   move      §rororasti    Worasti
129000140129     c                   endif
129100140129
129200140129      * ordinamento
129300140129     c                   clear                   Wsort
129400140129     c                   eval      w030a = *all'0'
129500140129     c                   movel     §rorsort      w030a
129600140129     c                   exsr      sr_testn
129700140129     c                   if        not *in02
129800140129     c  n01              eval      plgerr = 'E'
129900140129     c  n01              eval      plgmsg = 'ORDINAMENTO NON VALIDO'
130000140129     c  n01              eval      *in01 = *on
130100140129     c  n01              eval      *in04 = *on
130200140129
130300140129     c                   else
130400140129     c                   move      §rorsort      Wsort
130500140129     c                   endif
130600140129
130700140129     c                   endsr
130800140129
130900070130      *------------------------------------------------------------------------*
131000070130      * ROUTINE PER AGGIORNARE I DATI EFFETTIVI
131100070130      *------------------------------------------------------------------------*
131200070130     c     sr_file       begsr
131300070130
131400070130     c                   select
131500070130      * tipo record 'RCH'
131600070130     c                   when      plgtiporcd = 'RCH'
131700080327      * aggiorno fiqdt
131800070201     c                   exsr      sr_fiqdt
131900080327      * devo cancellare i dati da FIPDO e da FIPRO
132000070201     c                   exsr      sr_fipdo
132100080319     c                   exsr      sr_fipro
132200070130
132300070130      * tipo record 'RES'
132400070131     c                   when      plgtiporcd = 'RES' and
132500070131     c                             not *in03
132600070201     c                   exsr      sr_fnorm
132700070130
132800070130     c                   endsl
132900070130
133000070130     c                   endsr
133100070202
133200070202      *------------------------------------------------------------------------*
133300070202      * ROUTINE SCRITTURA FILE LOG
133400070202      *------------------------------------------------------------------------*
133500070202     c     sr_wrtlog     begsr
133600070202
133700070202     c                   eval      plgprfc = prcprfc
133800070202     c                   eval      plgiddisp = prciddisp
133900070202     c                   eval      plgdatora = prcdatora
134000070202     c                   eval      plgdati = prcdati
134100070205     c                   write     fiplg000
134200070202
134300070202     c                   endsr
134400070201
134500080415      *------------------------------------------------------------------------*
134600080415      * ROUTINE SCRITTURA FILE PER PROSPETTO ORM
134700080415      *------------------------------------------------------------------------*
134800080415     c     sr_Prospetto  begsr
134900080415
135000080415      * se ricevo un RES
135100080417    1c                   If        plgtiporcd = 'RES'
135200080415      * cerco l'ORM da scrivere o aggiornare nel file dettaglio prospetto ORM
135300080415     c     kfirpd        chain     Firpd02l
135400080415
135500080417    2c                   if        not %found(firpd02l)
135600080415     c                   clear                   firpd000
135700080415     c                   eval      rpdfgs = plgfgs
135800080415     c                   eval      rpdnfv = plgndc
135900080415     c                   eval      rpdpdr = plgcodaut
136000080415     c                   eval      rpdiddoc = plgiddoc
136100080415      * è un manuale ???
136200080417    3c                   if        %subst(plgiddoc:1:8) = *all'0'
136300080417     c                   eval      rpdman = 'S'
136400080415     c                   eval      rpdrsr = §resrsr
136500080415     c                   eval      rpdinr = §resinr
136600080417    3c                   endif
136700080417     c                   move      §resncl       rpdncl
136800080417     c                   move      §resbnc       rpdbnc
136900080415     c                   eval      rpdcmr = §rescmr
137000080417     c                   movel     §resdata      rpddtr
137100080417     c                   move      §resora       rpdorr
137200080415     c                   eval      rpdflgspc = §resflgspc
137300080417     c                   movel     §resdtorin    rpddtorin
137400081106      * non scrivo record se orm manuale annullato
137500081106     c                   if        rpdman <> 'S' or
137600081106     c                             (rpdman = 'S' and §rescmr <> 'AAA')
137700080417     c                   write     firpd000
137800140611
137900140611      /free
138000140611       //?Scrivo, se non è già stata scritta prima, la testata FIRPT
138100140611         chain(n) (RPDfgs:RPDnfv:RPDpdr) FIRPT01L;
138200140611         IF  not %found(FIRPT01L);
138300140611           clear FIRPT000;
138400140611           RPTfgs = PLGfgs;
138500140611           RPTnfv = PLGndc;
138600140611           RPTpdr = PLGcodaut;
138700140611           RPTdfv = DSTdfv;
138800140611           RPTfpp = DSTfpp;
138900140611           RPTtpm = DSTtpm;
139000140611           write FIRPT000;
139100140611         ENDIF;
139200140611      /end-free
139300140611
139400081106     c                   endif
139500080417    2c                   endif
139600080417      * se trovato record se data ora modifica maggiore di quella registrata aggiorno
139700080417    2c                   if        %found(firpd02l) and
139800080417     c                             rpddtorin < §resdtorin
139900081106      * ma solo se non è un orm manuale annullato
140000081106     c                             and (rpdman <> 'S' or
140100081106     c                                 (rpdman = 'S' and §rescmr <> 'AAA'))
140200080417     c                   move      §resncl       rpdncl
140300080417     c                   move      §resbnc       rpdbnc
140400080417     c                   eval      rpdcmr = §rescmr
140500080505     c                   movel     §resdtorin    rpddtorin
140600080505     c                   eval      rpdflgspc = §resflgspc
140700080417     c                   update    firpd000
140800080417    2c                   endif
140900081106
141000081106      * se trovato record e orm manuale annullato cancello il record
141100081106    2c                   if        %found(firpd02l) and
141200081106     c                             rpdman = 'S' and §rescmr = 'AAA'
141300081106     c                   delete    firpd000
141400081106    2c                   endif
141500080417
141600080417    1c                   endif
141700080417
141800080417      * se ricevo un RCH
141900080417    1c                   If        plgtiporcd = 'RCH'
142000080417      * cerco se la distinta è presente
142100080417     c     kfirpt        chain     Firpt01l
142200080417
142300080417    2c                   if        not %found(firpt01l)
142400080417     c                   clear                   firpt000
142500080417     c                   eval      rptfgs = plgfgs
142600080417     c                   eval      rptnfv = plgndc
142700080417     c                   eval      rptpdr = plgcodaut
142800080421     c                   eval      rptdfv = dstdfv
142900080418     c                   eval      rptfpp = dstfpp
143000080418     c                   eval      rpttpm = dsttpm
143100080417     c                   move      §rchncl       rpttco
143200080417     c                   move      §rchbnc       rpttbn
143300080417     c                   movel     §rchdata      rptdch
143400080417     c                   move      §rchora       rptoch
143500080417     c                   write     firpt000
143600080417    2c                   endif
143700080417      * se trovato record se data ora modifica maggiore di quella registrata aggiorno
143800080417    2c                   if        %found(firpt01l)
143900080417     c                   movel     rptdch        w_dtaorach
144000080417     c                   move      rptoch        w_dtaorach
144100080417    3c                   If        w_dtaorach < §rchdatora
144200080417     c                   move      §rchncl       rpttco
144300080417     c                   move      §rchbnc       rpttbn
144400080417     c                   movel     §rchdata      rptdch
144500080417     c                   move      §rchora       rptoch
144600080417     c                   update    firpt000
144700080417    3c                   endif
144800080417    2c                   endif
144900080417
145000080417    1c                   endif
145100080415
145200080415     c                   endsr
145300080319      *------------------------------------------------------------------------*
145400080319      * ROUTINE SCRITTURA FILE OK RICEVUTI
145500080319      *------------------------------------------------------------------------*
145600080319     c     sr_wrtpro     begsr
145700080319
145800080730      * verifico se estiste già un ok per questo ORM + DISTINTA + AUT
145900080319
146000140128     c     kfipro        chain     fipro11l
146100080319
146200080319      * se il record esiste e l'ok che ho ricevuto ha un idrow minore o uguale
146300140207     c                   if        %found(fipro11l)
146400140207     c                   movel     prodati       fiprookds
146500140207     c                   endif
146600080319      * a quello del file vado a fine
146700140207     c                   if        %found(fipro11l) and widrow <= §proidrow
146800080319     c                   leavesr
146900080319     c                   endif
147000080319
147100140128     c                   eval      prodatora = §OKDATORA
147200080319      * valorizzo i campi dell'ok
147300140128     c                   eval      §proidrow = widrow
147400140128     c                   eval      §proflgass = §OKFLGASS
147500140224     c                   eval      §PROFLGVIS = ' '
147600140128     c                   eval      §pronotass = §OKnotass
147700140128     c                   eval      Prodati    = fiprookds
147800080319
147900140207     c                   if        not %found(fipro11l)
148000080319     c                   eval      profgs = plgfgs
148100080319     c                   eval      prondc = plgndc
148200080730     c                   eval      procodaut = plgcodaut
148300080319     c                   clear                   proatb
148400080319     c                   eval      propoe = ormpoe
148500080319     c                   eval      pronsr = ormnsr
148600080319     c                   eval      pronor = ormnor
148700080319     c                   eval      pronrv = ormnrv
148800140128     c                   eval      protrd = 'OK '
148900140128     c                   write     fipro100
149000080319     c                   else
149100140128     c                   update    fipro100
149200080319     c                   endif
149300080319
149400080319     c                   endsr
149500080319
149600140129      *------------------------------------------------------------------------*
149700140129      * ROUTINE SCRITTURA FILE ROR RICEVUTI
149800140129      *------------------------------------------------------------------------*
149900140129     c     sr_wrtproror  begsr
150000140129
150100140129      * verifico se estiste già un ok per questo ORM + DISTINTA + AUT
150200140129
150300140129     c     kfipro        chain     fipro11l
150400140129
150500140129     c                   eval      prodatora = §rorDATORA
150600140207      * valorizzo i campi del ror
150700140207     c                   move      §rorORASTI    §PROORASTI
150800140207     c                   move      §rorSORT      §PROSORT
150900140129     c                   eval      Prodati    = fiprorords
151000140129
151100140129     c                   if        not %found(fipro11l)
151200140129     c                   eval      profgs = plgfgs
151300140129     c                   eval      prondc = plgndc
151400140129     c                   eval      procodaut = plgcodaut
151500140129     c                   clear                   proatb
151600140129     c                   eval      propoe = ormpoe
151700140129     c                   eval      pronsr = ormnsr
151800140129     c                   eval      pronor = ormnor
151900140129     c                   eval      pronrv = ormnrv
152000140129     c                   eval      protrd = 'ROR'
152100140129     c                   write     fipro100
152200140129     c                   else
152300140129     c                   update    fipro100
152400140129     c                   endif
152500140129
152600140129     c                   endsr
152700140129
152800070201      *------------------------------------------------------------------------*
152900070201      * ROUTINE PER CONTROLLARE IL P.O. GESTIONE DISTINTA
153000070201      *------------------------------------------------------------------------*
153100070201     c     sr_ctrfgs     begsr
153200070201
153300070201     c                   eval      w030a = *all'0'
153400070201     c                   movel     wfgs          w030a
153500070201     c                   exsr      sr_testn
153600070201     c                   if        not *in02
153700070201     c  n01              eval      plgerr = 'E'
153800070201     c  n01              eval      plgmsg = 'DATI P.O. DISTINTA NON VALIDI'
153900070201     c  n01              eval      *in01 = *on
154000110620     c  n01              eval      *in04 = *on
154100070201     c                   endif
154200070201
154300070201     c                   endsr
154400070201
154500070201      *------------------------------------------------------------------------*
154600070201      * ROUTINE PER CONTROLLARE IL NUMERO DISTINTA
154700070201      *------------------------------------------------------------------------*
154800070201     c     sr_ctrndc     begsr
154900070201
155000070201     c                   eval      w030a = *all'0'
155100070201     c                   movel     wndc          w030a
155200070201     c                   exsr      sr_testn
155300070201     c                   if        not *in02
155400070201     c  n01              eval      plgerr = 'E'
155500070201     c  n01              eval      plgmsg = 'DATI NUM. DISTINTA NON VALIDI'
155600070201     c  n01              eval      *in01 = *on
155700110620     c  n01              eval      *in04 = *on
155800070201     c                   endif
155900070201
156000070201     c                   endsr
156100070201
156200070201      *------------------------------------------------------------------------*
156300070201      * ROUTINE PER CONTROLLARE L'EISISTENZA DELLA DISTINTA
156400070201      *------------------------------------------------------------------------*
156500070201     c     sr_ctrdist    begsr
156600070201
156700080226     c                   eval      dstnpg = 4
156800070201     c                   move      wndc          dstnfv
156900070201     c                   move      wfgs          dstfgs
157000080317     c                   clear                   ddstflr
157100080226     c     kfidst        chain     fidst01l
157200080226     c                   if        not %found(fidst01l) or dstatb <> *blanks
157300070201     c  n01              eval      plgerr = 'E'
157400070201     c  n01              eval      plgmsg = 'DISTINA INESISTENTE'
157500070201     c  n01              eval      *in01 = *on
157600110620     c  n01              eval      *in04 = *on
157700080317     c                   leavesr
157800070201     c                   endif
157900080317     c                   eval      ddstflr = dstflr
158000070201
158100070201     c                   endsr
158200080513
158300080513      *------------------------------------------------------------------------*
158400080513      * ROUTINE PER CONTROLLARE CODICE SPC
158500080513      *------------------------------------------------------------------------*
158600080513     c     sr_ctrcodspc  begsr
158700080513
158800080519     c                   eval      *in05 = *off
158900080924     c                   eval      xx = 1
159000080924     c     §rchcodspc    lookup    codspc(xx)                             30
159100080514     c                   if        not *in30
159200080519     c                   eval      *in05 = *on
159300080924     c                   else
159400080924     c                   eval      §rchspccod = codspc(xx)
159500080924     c                   eval      §rchspcdes = desspc(xx)
159600080514     c                   endif
159700080513
159800080513     c                   endsr
159900070201
160000070201      *------------------------------------------------------------------------*
160100070201      * ROUTINE PER CONTROLLARE SE IL CAMPO E' NUMERICO
160200070201      *------------------------------------------------------------------------*
160300070201     c     sr_testn      begsr
160400070201
160500070201     c                   eval      *in02 = *off
160600070201     c                   testn                   w030a                02
160700070201
160800070201     c                   endsr
160900070201
161000070201      *------------------------------------------------------------------------*
161100070201      * ROUTINE PER AGGIORNARE FIQDT
161200070201      *------------------------------------------------------------------------*
161300070201     c     sr_fiqdt      begsr
161400070201
161500070201      * imposto la data e ora attuale
161600070201     c                   time                    w0140
161700070201     c                   move      w0140         udate8
161800070201     c                   movel     w0140         hhmm
161900070201     c                   z-add     udate8        g02dat
162000070201     c                   move      *blank        g02err
162100070201     c                   call      'XSRDA8'
162200070201     c                   parm                    wlbdat
162300070201     c                   z-add     g02inv        dateu8
162400070201
162500070201      * aggancio FIQDT
162600070202     c     kfiqdt        chain(e)  fiqdt01l
162700070205      * se rcd allocato flag di comodo per poi fare il rollback
162800070202     c                   if        %error
162900070202     c                   eval      wfiqdt = *on
163000070202     c                   leavesr
163100070202     c                   endif
163200070201     c                   if        %found(fiqdt01l)
163300070201      * bancali ritirati
163400081106      * se distinta a PDA non in test sommo i bancali rientrati a quando già presente su
163500081106      * fiqdt
163600110316     c**!!!              if        dstpda = 'E' and §dsttstpda = *blanks
163700110316     c**!!!              add       wbnc          qdtnbnri
163800110316     c**!!!              eval      qdtorari = hhmm
163900110316     c**!!!              eval      qdtdairi = dateu8
164000110316     c**!!!              eval      qdtfnbri = 'S'
164100110316     c**!!!              endif
164200080317     c****               if        wbnc <> qdtnbnri
164300080317     c****               eval      qdtnbnri = wbnc
164400080317     c****               eval      qdtorari = hhmm
164500080317     c****               eval      qdtdairi = dateu8
164600080317     c****               endif
164700070201      * colli ritirati
164800070201     c                   if        wncl <> qdtnclrit
164900070201     c                   eval      qdtnclrit = wncl
165000081112     c                   eval      qdtfncrit = 'S'
165100070201     c                   endif
165200070201
165300070205     c                   update    fiqdt000
165400070201     c                   endif
165500070201
165600070201     c                   endsr
165700080327
165800080327      *------------------------------------------------------------------------*
165900080327      * ROUTINE PER SCRIVERE LA FASE RCH SULLA DISTINTA
166000080327      *------------------------------------------------------------------------*
166100080327     c     sr_fase       begsr
166200080327
166300080327     c                   clear                   fior90ds
166400080327     c                   eval      or90tla = 'W'
166500080327     c                   eval      or90fas = 'RCH'
166600080327     c                   eval      or90fgs = plgfgs
166700080327     c                   eval      or90ndc = plgndc
166800080327     c                   eval      or90flgins = 'P'
166900080327     c                   eval      or90comit = '1'
167000080924     c                   eval      kpjbu = fidsfrchds
167100080327     c                   call      'FIOR90R'
167200080327     c                   parm                    kpjba
167300080327     c                   parm                    fior90ds
167400080327      * se torna errore imposto flag per poi fare il rollback
167500080327     c                   if        or90err <> *blanks
167600080327     c                   eval      wfase = *on
167700080327     c                   leavesr
167800080327     c                   endif
167900080327
168000080327     c                   endsr
168100070201
168200070201      *------------------------------------------------------------------------*
168300080319      * ROUTINE PER CANCELLARE RCD DA FIPDO
168400070201      *------------------------------------------------------------------------*
168500070201     c     sr_fipdo      begsr
168600080319
168700080319      * Cancellazione PDO
168800070201     c     kfiqdt        setll     fipdo01t
168900070201     c                   do        *hival
169000070205     c     kfiqdt        reade(e)  fipdo01t
169100070205      * se rcd allocato flag di comodo per poi fare il rollback
169200070205     c                   if        %error
169300070205     c                   eval      wfipdo = *on
169400070205     c                   leave
169500070205     c                   endif
169600070201     c                   if        %eof(fipdo01t)
169700070201     c                   leave
169800070201     c                   endif
169900070205     c                   delete    fipdo000
170000070201     c                   enddo
170100080319
170200070201     c                   endsr
170300070201
170400080319      *------------------------------------------------------------------------*
170500080319      * ROUTINE PER CANCELLARE RCD DA FIPRO
170600080319      *------------------------------------------------------------------------*
170700080319     c     sr_fipro      begsr
170800080319
170900080319      * Cancellazione PRO
171000140207     c     kfiqdt        setll     fipro11l
171100080319     c                   do        *hival
171200140207     c     kfiqdt        reade(e)  fipro11l
171300080319      * se rcd allocato flag di comodo per poi fare il rollback
171400080319     c                   if        %error
171500080319     c                   eval      wfipro = *on
171600080319     c                   leave
171700080319     c                   endif
171800140207     c                   if        %eof(fipro11l)
171900080319     c                   leave
172000080319     c                   endif
172100140207     c                   delete    fipro100
172200080319     c                   enddo
172300080319
172400080319     c                   endsr
172500080319
172600070201      *------------------------------------------------------------------------*
172700070201      * ROUTINE PER AGGIORNARE FNORM/FNORF
172800070201      *------------------------------------------------------------------------*
172900070201     c     sr_fnorm      begsr
173000070201
173100070201      * imposto la data e ora attuale
173200070201     c                   time                    w0140
173300070201     c                   move      w0140         udate8
173400070201     c                   z-add     udate8        g02dat
173500070201     c                   move      *blank        g02err
173600070201     c                   call      'XSRDA8'
173700070201     c                   parm                    wlbdat
173800070201     c                   z-add     g02inv        dateu8
173900070201
174000070201      * aggancio FNORM
174100070202     c     kfnorm        chain(e)  fnorm01l
174200070205      * se rcd allocato flag di comodo per poi fare il rollback
174300070202     c                   if        %error
174400070202     c                   eval      wfnorm = *on
174500070202     c                   leavesr
174600070202     c                   endif
174700070201     c                   if        %found(fnorm01l)
174800080219      * aggancio l'ultima fase dell'ORM
174900080219      * per recuperare l'eventuale data/ora dell'ultimo esito avuto da PDA
175000080219     c                   clear                   dorf01
175100080219     c                   clear                   savdfo
175200080219     c                   clear                   savofo
175300080219     c     kfnorf        chain     fnorf01l
175400080219     c                   if        %found(fnorf01l)
175500080219     c                   eval      dorf01 = orfflo
175600080320      * mi salvo orfass della fase precedente
175700080320     c                   eval      save_§orfass = §orfass
175800080219     c                   endif
175900160324      * salvo data/ora esecuzione da PDA, ma solo se fase PDA (410/420)
176000080219     c                   if        §orfesito <> *blanks
176100160324     c                             and (ORFfar = 410 or ORFfar = 420)
176200080219     c                   eval      savdfo = %int(%subst(§orfesito:1:8))
176300080219     c                   eval      savofo = %int(%subst(§orfesito:9:6))
176400080219     c                   endif
176500080212
176600070201     c                   select
176700070201      * esito fatto
176800070201     c                   when      §rescmr = *blanks
176900070201     c                   eval      ormfao = 420
177000070201      * esito non fatto
177100070201     c                   when      §rescmr <> *blanks
177200070201     c                   eval      ormfao = 410
177300070201     c                   endsl
177400070201
177500070213     c                   move      §resdata      ormdfo
177600070213     c                   move      §resora       ormofo
177700070716      * imposto il campo ORMETI
177800070716     c                   clear                   tibs02ds
177900070716     c                   eval      t02mod = 'C'
178000070716     c                   eval      t02sif = knsif
178100070716     c                   eval      t02cod = 'FAR'
178200070716     c                   movel(p)  ormfao        t02ke1
178300070716     c                   call      'TIBS02R'
178400070716     c                   parm                    kpjba
178500070716     c                   parm                    tibs02ds
178600070716     c                   eval      dfar = t02uni
178700070716     c                   eval      ormeti = d§farass
178800070201      * scrivo fase
178900070201     c                   clear                   fnorf000
179000080219     c                   clear                   dorf01
179100070201     c                   eval      orfpoe = ormpoe
179200070201     c                   eval      orfnsr = ormnsr
179300070201     c                   eval      orfnor = ormnor
179400070201     c                   eval      orfnrv = ormnrv
179500070201     c                   eval      orfpog = plgfgs
179600070201     c                   eval      orfdae = ormdfo
179700070201     c                   eval      orfore = ormofo
179800070201     c                   eval      orffar = ormfao
179900070221     c                   eval      orfpue = prcprfc
180000070201     c                   eval      orfcar = §rescmr
180100070201     c                   eval      orffgs = plgfgs
180200070201     c                   eval      orfndc = plgndc
180300070201     c                   eval      orfddc = ormddc
180400080219     c                   eval      §orfesito = §resdtorin
180500080320      * imposto  orfass della fase precedente
180600080320     c                   eval      §orfass =  save_§orfass
180700080219     c                   eval      orfflo = dorf01
180800070205     c                   write     fnorf000
180900080409      * scrivo NOTE
181000080409      * solo se ci sono
181100080409     c                   if        §resnote <> *blanks
181200080409     c                   clear                   wprg
181300080409      * cerco ultimo progressivo relativo alla stessa fase
181400080409     c     kfnorn        setll     fnorn02l
181500080409     c                   do        *hival
181600080409     c     kfnorn        reade     fnorn02l
181700080409     c                   if        %eof(fnorn02l)
181800080409     c                   leave
181900080409     c                   endif
182000080409     c                   eval      wprg = ornprg
182100080409     c                   enddo
182200080409      * scrivo la nota
182300080409     c                   add       1             wprg
182400080409     c                   clear                   fnorn000
182500080409     c                   eval      ornpoe = ormpoe
182600080409     c                   eval      ornnsr = ormnsr
182700080409     c                   eval      ornnor = ormnor
182800080409     c                   eval      ornnrv = ormnrv
182900080409     c                   eval      ornfar = ormfao
183000080409     c                   movel     §orfesito     orndai
183100080409     c                   move      §orfesito     ornori
183200080409     c                   eval      ornprg = wprg
183300080409     c                   eval      ornpue = orfpue
183400080409     c                   eval      ornpon = plgfgs
183500080409     c                   eval      ornsns = 'I'
183600080409     c                   eval      ornnot = §resnote
183700080409     c                   eval      ornftr = 'S'
183800080409     c                   eval      orndtr = dateu8
183900080409     c                   write     fnorn000
184000080409     c                   endif
184100070202      * aggiorno ORM
184200080212      * solo se la data/ora ultima fase dell'ORM è inferiore a quella che sto per scrivere
184300080212     c                   if        ormdfo > savdfo or
184400080214     c                             (ormdfo = savdfo and ormofo >= savofo)
184500070205     c                   update    fnorm000
184600080212     c                   endif
184700080227      * scrivo la partita iva e il codice fiscale nel file di appoggio
184800080320      *
184900080320      * verifico se esiste il file di appoggio fipci02l
185000080320     c     kfnorm        chain     fipci02l
185100080320      * nel casi in cui almeno uno dei due dati è valorizzato aggiorno o scrivo
185200080227     c                   if        §rescodfis <> *blanks or §respiva <> *blanks
185300080227     c                   if        %found(fipci02l)
185400080227     c                   eval      pcicdf = §rescodfis
185500080227     c                   eval      pciiso = §resisoiva
185600080228     c                   eval      pcicpi = §respiva
185700080227     c                   clear                   pciesito
185800080227     c                   update    fipci000
185900080227     c                   else
186000080227     c                   clear                   fipci000
186100080227     c                   eval      pcipoe = ormpoe
186200080227     c                   eval      pcinsr = ormnsr
186300080227     c                   eval      pcinor = ormnor
186400080227     c                   eval      pcinrv = ormnrv
186500080227     c                   eval      pcicdf = §rescodfis
186600080227     c                   eval      pciiso = §resisoiva
186700080228     c                   eval      pcicpi = §respiva
186800080228     c                   write     fipci000
186900080227     c                   endif
187000080227     c                   endif
187100080320
187200080321      * nel casi in cui entrambi sono a blank ed il record esiste annullo il rcd
187300080320     c                   if        §rescodfis = *blanks and §respiva = *blanks
187400080320     c                             and %found(fipci02l)
187500080321     c                   eval      pciatb = 'A'
187600080321     c                   update    fipci000
187700080320     c                   endif
187800080320
187900070201     c                   endif
188000070201
188100070201     c                   endsr
188200080519
188300080519      *------------------------------------------------------------------------*
188400080519      * ROUTINE PER INVIARE MAIL SE ERRORE CODICE SPC
188500080519      *------------------------------------------------------------------------*
188600080519     c     sr_mail       begsr
188700080519
188800080519      * Reperimento tab. "MRA" se non già recuperata
188900080519     c                   if        not $okmra
189000080519     c                   clear                   dmradan
189100080519     c                   clear                   tibs02ds
189200080519     c                   eval      t02mod = 'C'
189300080519     c                   eval      t02sif = knsif
189400080519     c                   eval      t02cod = 'MRA'
189500080519     c                   eval      t02ke1 = sdspgm
189600080519     c                   call      'TIBS02R'
189700080519     c                   parm                    kpjba
189800080519     c                   parm                    tibs02ds
189900080519     c                   if        t02err = *blanks
190000080519     c                   eval      dmradan = t02uni
190100080519     c                   eval      $okmra = *on
190200080519     c                   endif
190300080519     c                   endif
190400080519
190500080519      * Override al file di stampa ed apertura dello stesso
190600080519     c                   if        not %open(prtemail)
190700080519     c                   exsr      sr_openprtf
190800080519     c                   endif
190900080522
191000080522      * Stampa errore
191100080522     c                   if        *in05
191200080522     c                   eval      o_testo = 'trovato codice SPC non valido in +
191300080522     c                                        chiusura distinta ' +
191400080522     c                                        %editc(plgfgs:'X') + '-' +
191500080522     c                                        %editc(plgndc:'X') +
191600080522     c                                        ' in data ' +
191700080522     c                                        %subst(prcdatora:7:2) + '/' +
191800080522     c                                        %subst(prcdatora:5:2) + '/' +
191900080522     c                                        %subst(prcdatora:1:4)
192000080522     c                   except    prtdet
192100080522     c                   endif
192200080522     c                   if        *in01
192300080522     c                   eval      o_testo = 'trovato errore per la distinta ' +
192400080522     c                                        %editc(plgfgs:'X') + '-' +
192500080522     c                                        %editc(plgndc:'X') +
192600080522     c                                        ' in data ' +
192700080522     c                                        %subst(prcdatora:7:2) + '/' +
192800080522     c                                        %subst(prcdatora:5:2) + '/' +
192900080522     c                                        %subst(prcdatora:1:4)
193000080522     c                   except    prtdet
193100080522     c                   eval      o_testo = plgmsg + 'Profilo ' + prcprfc
193200080522     c                   except    prtdet
193300080522     c                   endif
193400080522
193500080522      * Stampa una riga vuota
193600080522     c                   clear                   o_testo
193700080522     c                   except    prtdet
193800080519
193900080519     c                   endsr
194000080519
194100080519      *------------------------------------------------------------------------*
194200080519      * OVERRIDE AL FILE DI STAMPA PER IMPOSTARE I DATI PER L'INVIO VIA MAIL
194300080519      *  + STAMPA INIZIO MAIL
194400080519      *------------------------------------------------------------------------*
194500080519     c     sr_openprtf   begsr
194600080519
194700080519      * Override al file di stampa
194800080519     c                   exsr      sr_override
194900080519
195000080519     c                   open      prtemail
195100080519
195200080519      * Stampa una testata se NON è richiesta la e-mail
195300080519     c                   if        §mradreg = *blanks
195400080519     c                   eval      o_testo = jobuser + ' - ' + sdspgm
195500080519     c                             + ' - ' + %editc( *date : 'Y' )
195600080519     c                             + ' - *REM* ' + %subst(§cm1var : 7 : 70)
195700080519     c                   except    prtdet
195800080519     c                   clear                   o_testo
195900080519     c                   except    prtdet
196000080519     c                   except    prtdet
196100080519     c                   endif
196200080519
196300080519      * Stampa testo iniziale
196400080519     c                   eval      o_testo = 'trovato errore in ricezione dati +
196500080522     c                                        PDA '
196600080519     c                   except    prtdet
196700080519
196800080519      * Stampa una riga vuota
196900080519     c                   clear                   o_testo
197000080519     c                   except    prtdet
197100080519
197200080519     c                   endsr
197300080519
197400080519      *------------------------------------------------------------------------*
197500080519      * OVERRIDE AL FILE DI STAMPA PER IMPOSTARE I DATI PER L'INVIO VIA MAIL
197600080519      *  + STAMPA INIZIO MAIL
197700080519      *------------------------------------------------------------------------*
197800080519     c     sr_override   begsr
197900080519
198000080519     c                   reset                   $invio
198100080519     c                   reset                   trtcm1ds
198200080519
198300080519     c                   if        §mradreg <> *blanks
198400080519     c                   eval      §cm1mitt = %trim(§mradmitt)
198500080519     c                   eval      §cm1dst  = %trim(§mraddest)
198600080519     c                   eval      §cm1tips = §mradreg
198700080519     c                   eval      §cm1po   = c_sede
198800080519     c                   eval      §cm1var  = '*OBJM*' + §mraddes
198900080519     c                   eval      §cm1idp  = §mradidpro
199000080519     c                   eval      qcmd = c_cmdovrprtf
199100080519     c                             + ' outq(' + %trim(§mradoutqi) + ')'
199200080519     c                             + ' usrdfndta(''' + trtcm1ds + ''')'
199300080519     c                   else
199400080519     c                   eval      qcmd = c_cmdovrprtf
199500080519     c                   endif
199600080519
199700080519     c                   eval      lenght = %size(qcmd)
199800080519     c                   call(e)   'QCMDEXC'
199900080519     c                   parm                    qcmd
200000080519     c                   parm                    lenght
200100080519
200200080519     c                   if        §mradreg <> *blanks and not %error
200300080519     c                   eval      $invio = *on
200400080519     c                   endif
200500080519
200600080519     c                   endsr
200700080513
200800080513      *------------------------------------------------------------------------*
200900080513      * ROUTINE FINALE
201000080513      *------------------------------------------------------------------------*
201100080513     c     sr_end        begsr
201200080513
201300080513     c                   if        %open(prtemail)
201400080513
201500080513      * Chiusura dello spool?
201600080522     c                   eval      o_testo = sdspgm
201700080522     c                   except    prtdet
201800080513     c                   clear                   o_testo
201900080513     c                   except    prtdet
202000080513     c                   eval      o_testo = '***   Fine Lista   ***'
202100080513     c                   except    prtdet
202200080513
202300080513     c                   close     prtemail
202400080513
202500080513      * Eliminazione overflow?
202600080513     c                   eval      lenght = %size(qcmd)
202700080513     c                   eval      qcmd = c_cmddltovr
202800080513     c                   call(e)   'QCMDEXC'
202900080513     c                   parm                    qcmd
203000080513     c                   parm                    lenght
203100080513
203200080513     c                   endif
203300080513
203400080513     c                   endsr
203500070129
203600070129      *------------------------------------------------------------------------*
203700070129      * ROUTINE INIZIALE
203800070129      *------------------------------------------------------------------------*
203900070129     c     *inzsr        begsr
204000070129
204100070129     c     *entry        plist
204200070129     c                   parm                    parmpo
204300080226     c                   parm                    chiudi
204400070129
204500070129     c     kfiapd        klist
204600070129     c                   kfld                    apdtip
204700070129     c                   kfld                    apdpdr
204800070201
204900070201     c     kfiqdt        klist
205000070201     c                   kfld                    plgfgs
205100070201     c                   kfld                    plgndc
205200080319
205300080319     c     kfipro        klist
205400080319     c                   kfld                    plgfgs
205500080319     c                   kfld                    plgndc
205600140129     c                   kfld                    plgtiporcd
205700080730     c                   kfld                    plgcodaut
205800080319     c                   kfld                    ormpoe
205900080319     c                   kfld                    ormnsr
206000080319     c                   kfld                    ormnor
206100080319     c                   kfld                    ormnrv
206200070201
206300080226     c     kfidst        klist
206400080226     c                   kfld                    dstnpg
206500070201     c                   kfld                    dstnfv
206600070201     c                   kfld                    dstfgs
206700070131
206800070131     c     kfnorm        klist
206900070131     c                   kfld                    ormpoe
207000070131     c                   kfld                    ormnsr
207100070131     c                   kfld                    ormnor
207200070131     c                   kfld                    ormnrv
207300080219
207400080219     c     kfnorf        klist
207500080219     c                   kfld                    ormpoe
207600080219     c                   kfld                    ormnsr
207700080219     c                   kfld                    ormnor
207800080219     c                   kfld                    ormnrv
207900080219     c                   kfld                    ormdfo
208000080219     c                   kfld                    ormofo
208100080219     c                   kfld                    ormfao
208200080409
208300080409     c     kfnorn        klist
208400080409     c                   kfld                    ormpoe
208500080409     c                   kfld                    ormnsr
208600080409     c                   kfld                    ormnor
208700080409     c                   kfld                    ormnrv
208800080409     c                   kfld                    ormfao
208900080415
209000080415     c     kfirpd        klist
209100080415     c                   kfld                    plgfgs
209200080415     c                   kfld                    plgndc
209300080415     c                   kfld                    plgcodaut
209400080415     c                   kfld                    plgiddoc
209500080417
209600080417     c     kfirpt        klist
209700080417     c                   kfld                    plgfgs
209800080417     c                   kfld                    plgndc
209900080417     c                   kfld                    plgcodaut
210000140207      *
210100140207     c     ktbe          klist
210200140207     c                   kfld                    tbecod
210300140207     c                   kfld                    tbeke1
210400070129
210500070129     c                   endsr
210600080513
210700080513     oprtemail  e            prtdet      1
210800080513     o                       o_testo
