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
064200070129
064300070129     c                   endsr
064400070130
064500070130      *------------------------------------------------------------------------*
064600070130      * ROUTINE ELABORAZIONE RCD "RCH"
064700070130      *------------------------------------------------------------------------*
064800070130     c     sr_elabrch    begsr
064900070130
065000070130     c                   eval      fiprcrchds = prcdati
065100070130
065200070130      * p.o. gestione distinta
065300070201     c                   eval      wfgs = §rchfgs
065400070201     c                   exsr      sr_ctrfgs
065500070201     c   02              move      §rchfgs       plgfgs
065600070130
065700070130      * numero distinta
065800070201     c                   eval      wndc = §rchndc
065900070201     c                   exsr      sr_ctrndc
066000070201     c   02              move      §rchndc       plgndc
066100070201
066200070201      * controllo se esiste la distinta
066300070201     c   02              exsr      sr_ctrdist
066400070201
066500070201      * controllo se stesso AUT
066600070201     c                   if        *in02 and plgcodaut <> dstpdr
066700070201     c  n01              eval      plgerr = 'E'
066800070201     c  n01              eval      plgmsg = 'AUT. INCONGRUENTE'
066900070201     c  n01              eval      *in01 = *on
067000110620     c  n01              eval      *in04 = *on
067100070201     c                   endif
067200070201
067300070201      * la distinta deve esistere anche su FIQDT
067400070201     c                   if        *in02
067500070201     c     kfiqdt        chain(n)  fiqdt01l
067600070201     c                   if        not %found(fiqdt01l) or qdtatb <> *blanks
067700070201     c  n01              eval      plgerr = 'E'
067800070201     c  n01              eval      plgmsg = 'DISTINTA INESISTENTE SU FIQDT'
067900070201     c  n01              eval      *in01 = *on
068000110620     c  n01              eval      *in04 = *on
068100070201     c                   endif
068200070201     c                   endif
068300070130
068400070130      * totale colli
068500070130      *  deve essere numerico
068600070201     c                   eval      w030a = *all'0'
068700070201     c                   movel     §rchncl       w030a
068800070201     c                   exsr      sr_testn
068900070201     c                   if        not *in02
069000070130     c  n01              eval      plgerr = 'E'
069100070201     c  n01              eval      plgmsg = 'DATI TOT. COLLI NON VALIDI'
069200070130     c  n01              eval      *in01 = *on
069300110620     c  n01              eval      *in04 = *on
069400070201     c                   else
069500070201     c                   move      §rchncl       wncl
069600070130     c                   endif
069700070130
069800070130      * totale bancali
069900070130      *  deve essere numerico
070000070201     c                   eval      w030a = *all'0'
070100070201     c                   movel     §rchbnc       w030a
070200070201     c                   exsr      sr_testn
070300070201     c                   if        not *in02
070400070130     c  n01              eval      plgerr = 'E'
070500070201     c  n01              eval      plgmsg = 'DATI TOT. BANCALI NON VALIDI'
070600070130     c  n01              eval      *in01 = *on
070700110620     c  n01              eval      *in04 = *on
070800070201     c                   else
070900070201     c                   move      §rchbnc       wbnc
071000070130     c                   endif
071100070213
071200070213      * data chiusura
071300070213      *  deve essere numerico
071400070213     c                   eval      w030a = *all'0'
071500070213     c                   movel     §rchdata      w030a
071600070213     c                   exsr      sr_testn
071700070213     c                   if        not *in02
071800070213     c  n01              eval      plgerr = 'E'
071900070213     c  n01              eval      plgmsg = 'DATA CHIUSURA NON VALIDA'
072000070213     c  n01              eval      *in01 = *on
072100110620     c  n01              eval      *in04 = *on
072200070213     c                   endif
072300070213      *  deve essere una data valida
072400070213     c                   if        *in02
072500070213     c                   clear                   wlbdat
072600070213     c                   move      §rchdata      g02inv
072700070213     c                   eval      g02err = '3'
072800070213     c                   call      'XSRDA8'
072900070213     c                   parm                    wlbdat
073000070213     c                   if        g02err = '1'
073100070213     c  n01              eval      plgerr = 'E'
073200070213     c  n01              eval      plgmsg = 'DATA CHIUSURA NON VALIDA'
073300070213     c  n01              eval      *in01 = *on
073400110620     c  n01              eval      *in04 = *on
073500070213     c                   endif
073600070213     c                   endif
073700070213
073800070213      * ora chiusura
073900070213      *  deve essere numerico
074000070213     c                   eval      w030a = *all'0'
074100070213     c                   movel     §rchora       w030a
074200070213     c                   exsr      sr_testn
074300070213     c                   if        not *in02
074400070213     c  n01              eval      plgerr = 'E'
074500070213     c  n01              eval      plgmsg = 'ORA CHIUSURA NON VALIDA'
074600070213     c  n01              eval      *in01 = *on
074700110620     c  n01              eval      *in04 = *on
074800070213     c                   endif
074900070213      *  deve essere un'ora valida
075000070213     c                   if        *in02 and
075100070213     c                             %subst(§rchora:1:2) > '23' or
075200070213     c                             %subst(§rchora:3:2) > '59' or
075300070213     c                             %subst(§rchora:5:2) > '59'
075400070213     c  n01              eval      plgerr = 'E'
075500070213     c  n01              eval      plgmsg = 'ORA CHIUSURA NON VALIDA'
075600070213     c  n01              eval      *in01 = *on
075700110620     c  n01              eval      *in04 = *on
075800070213     c                   endif
075900070514
076000110609      /free
076100110609       //?La data di chiusura distinta su rcd RCH non può essere inferiore a 30 gg da oggi
076200110609       //?in questo caso accetto il rcd ma imposto la data della distinta
076300110609       //?e come ora imposto l'ora di ricezione del record
076400110609       //?è capitato che PDA si è resettato ed ha riportato la data al 1980
076500120920         IF  not *in01 and §RCHdata < %editc(Oggi_10:'X');
076600110609           §RCHdata = %editc(DSTdfv:'X');
076700121015           %subst(§RCHora:1:4)  = %subst(PRCdatora:9:4);
076800121010           PLGmsg   = 'DATA < o > di 10 GG';
076900110609         ENDIF;
077000120920       //?E' capitato anche che il PDA è andato fuori di testa ed ha impostato
077100120920       //?la data a 2514
077200120920         IF  not *in01 and §RCHdata > %editc(Oggi_10o:'X');
077300120920           §RCHdata = %editc(DSTdfv:'X');
077400121015           %subst(§RCHora:1:4)  = %subst(PRCdatora:9:4);
077500121010           PLGmsg  = 'DATA < o > di 10 GG';
077600120920         ENDIF;
077700110609      /end-free
077800110609
077900070514      * imposto il tipo applicazione anche se non previsto dalla ds
078000070514     c                   eval      plgtipappl = 'R'
078100080513
078200080513      * controllo codice SPC
078300080721     c                   eval      *in05 = *off
078400080519     c                   if        §rchcodspc <> *blanks
078500080513     c                   exsr      sr_ctrcodspc
078600080513     c                   endif
078700070130
078800070130     c                   endsr
078900070130
079000070130      *------------------------------------------------------------------------*
079100070130      * ROUTINE ELABORAZIONE RCD "RES"
079200070130      *------------------------------------------------------------------------*
079300070130     c     sr_elabres    begsr
079400070130
079500070131     c                   eval      *in03 = *off
079600070130     c                   eval      fiprcresds = prcdati
079700070201
079800070201      * causale
079900070201      *  deve esistere
080000070201     c                   if        §rescmr <> *blanks
080100081106      * se causale 'AAA' ok...annullamento di ORM Manuale
080200081106     c                             and §rescmr <> 'AAA'
080300070201     c                   clear                   tibs02ds
080400070201     c                   eval      t02mod = 'C'
080500070201     c                   eval      t02sif = knsif
080600070201     c                   eval      t02cod = 'CMR'
080700070201     c                   eval      t02ke1 = §rescmr
080800070201     c                   call      'TIBS02R'
080900070201     c                   parm                    kpjba
081000070201     c                   parm                    tibs02ds
081100070201     c                   if        t02err <> *blanks
081200070201     c  n01              eval      plgerr = 'E'
081300070201     c  n01              eval      plgmsg = 'CAU. MANCATO RIT. INESISTENTE'
081400070201     c  n01              eval      *in01 = *on
081500110620     c  n01              eval      *in04 = *on
081600070201     c                   endif
081700070201     c                   endif
081800070130
081900070130      * p.o. gestione distinta
082000070201     c                   eval      wfgs = §resfgs
082100070201     c                   exsr      sr_ctrfgs
082200070201     c   02              move      §resfgs       plgfgs
082300070130
082400070130      * numero distinta
082500070201     c                   eval      wndc = §resndc
082600070201     c                   exsr      sr_ctrndc
082700070201     c   02              move      §resndc       plgndc
082800070201
082900070201      * controllo se esiste la distinta
083000070201     c   02              exsr      sr_ctrdist
083100070201
083200070201      * controllo se stesso AUT
083300070201     c                   if        *in02 and plgcodaut <> dstpdr
083400070201     c  n01              eval      plgerr = 'E'
083500070201     c  n01              eval      plgmsg = 'AUT. INCONGRUENTE'
083600070201     c  n01              eval      *in01 = *on
083700110620     c  n01              eval      *in04 = *on
083800070201     c                   endif
083900070201
084000070201      * numero ORM
084100070201      *  deve essere numerico
084200070201     c                   eval      w030a = *all'0'
084300070201     c                   eval      %subst(w030a:1:14) = %subst(prcdati:10:14)
084400070201     c                   exsr      sr_testn
084500070201     c                   if        not *in02
084600070201     c  n01              eval      plgerr = 'E'
084700070201     c  n01              eval      plgmsg = 'DATI ID DOC. ORM NON VALIDI'
084800070201     c  n01              eval      *in01 = *on
084900110620     c  n01              eval      *in04 = *on
085000070201     c                   else
085100070201      *  controllo se ORM manuale
085200070521     c                   if        %subst(prcdati:10:8) = *all'0'
085300070521     c                   eval      w006a = %subst(prcdati:18:6)
085400070521     c                   move      w006a         wora
085500070521     c     *eur          test(te)                wora
085600070521     c                   if        %error
085700070201     c  n01              eval      plgerr = 'E'
085800070201     c  n01              eval      plgmsg = 'DATI ID DOC. ORM NON VALIDI'
085900070201     c  n01              eval      *in01 = *on
086000110620     c  n01              eval      *in04 = *on
086100070201     c                   else
086200070201     c                   eval      *in03 = *on
086300080408     c                   eval      plgiddoc = %subst(prcdati:10:14)
086400070201     c                   endif
086500070202     c                   endif
086600070202     c                   endif
086700070130
086800070201      * controllo la chiave ORM
086900070201      * se non è un ORM manuale
087000070201     c                   if        not *in03
087100070202     c                   clear                   ormpoe
087200070202     c                   clear                   ormnsr
087300070202     c                   clear                   ormnor
087400070202     c                   clear                   ormnrv
087500070130      * p.o. emissione
087600070130      *  deve essere numerico
087700070201     c                   eval      w030a = *all'0'
087800070201     c                   movel     §respoe       w030a
087900070201     c                   exsr      sr_testn
088000070201     c                   if        not *in02
088100070130     c  n01              eval      plgerr = 'E'
088200070201     c  n01              eval      plgmsg = 'DATI P.O. EMISSIONE NON VALIDI'
088300070130     c  n01              eval      *in01 = *on
088400110620     c  n01              eval      *in04 = *on
088500070201     c                   else
088600070201     c                   move      §respoe       ormpoe
088700070130     c                   endif
088800070130
088900070130      * numero serie
089000070130      *  deve essere numerico
089100070201     c                   eval      w030a = *all'0'
089200070201     c                   movel     §resnsr       w030a
089300070201     c                   exsr      sr_testn
089400070201     c                   if        not *in02
089500070130     c  n01              eval      plgerr = 'E'
089600070201     c  n01              eval      plgmsg = 'DATI N. SERIE NON VALIDI'
089700070130     c  n01              eval      *in01 = *on
089800110620     c  n01              eval      *in04 = *on
089900070201     c                   else
090000070201     c                   move      §resnsr       ormnsr
090100070130     c                   endif
090200070130
090300070130      * numero orm
090400070130      *  deve essere numerico
090500070201     c                   eval      w030a = *all'0'
090600070201     c                   movel     §resnor       w030a
090700070201     c                   exsr      sr_testn
090800070201     c                   if        not *in02
090900070130     c  n01              eval      plgerr = 'E'
091000070201     c  n01              eval      plgmsg = 'DATI N. ORM NON VALIDI'
091100070130     c  n01              eval      *in01 = *on
091200110620     c  n01              eval      *in04 = *on
091300070201     c                   else
091400070201     c                   move      §resnor       ormnor
091500070130     c                   endif
091600070130      * numero viaggio
091700070130      *  deve essere numerico
091800070201     c                   eval      w030a = *all'0'
091900070201     c                   movel     §resnrv       w030a
092000070201     c                   exsr      sr_testn
092100070201     c                   if        not *in02
092200070130     c  n01              eval      plgerr = 'E'
092300070201     c  n01              eval      plgmsg = 'DATI N. VIAGGIO NON VALIDI'
092400070130     c  n01              eval      *in01 = *on
092500110620     c  n01              eval      *in04 = *on
092600070201     c                   else
092700070201     c                   move      §resnrv       ormnrv
092800070130     c                   endif
092900070131
093000070201      * aggancio l'ORM
093100070201     c     kfnorm        chain(n)  fnorm01l
093200070131     c                   if        %found(fnorm01l)
093300070201      * verifico se la distinta presente sull'ORM è la stessa del PDA
093400070201     c                   if        ormndc <> plgndc
093500070131     c  n01              eval      plgerr = 'E'
093600070131     c  n01              eval      plgmsg = 'DISTINTA INCONGRUENTE'
093700070131     c  n01              eval      *in01 = *on
093800070201     c                   endif
093900070201      * verifico la fase dell'ORM
094000070201     c                   if        ormfao < 400 or ormfao >= 500
094100070131     c  n01              eval      plgerr = 'E'
094200070131     c  n01              eval      plgmsg = 'ESITO/FASE INCONGRUENTI'
094300070131     c  n01              eval      *in01 = *on
094400070201     c                   endif
094500070131     c                   endif
094600070207     c  n01              eval      %subst(plgiddoc:1:3) = §respoe
094700070207     c  n01              eval      %subst(plgiddoc:4:2) = §resnsr
094800070207     c  n01              eval      %subst(plgiddoc:6:7) = §resnor
094900070207     c  n01              eval      %subst(plgiddoc:13:2) = §resnrv
095000070201     c                   endif
095100070213
095200070213      * data conferma
095300070213      *  deve essere numerico
095400070213     c                   eval      w030a = *all'0'
095500070213     c                   movel     §resdata      w030a
095600070213     c                   exsr      sr_testn
095700070213     c                   if        not *in02
095800070213     c  n01              eval      plgerr = 'E'
095900070213     c  n01              eval      plgmsg = 'DATA CONFERMA NON VALIDA'
096000070213     c  n01              eval      *in01 = *on
096100110620     c  n01              eval      *in04 = *on
096200070213     c                   endif
096300070213      *  deve essere una data valida
096400070213     c                   if        *in02
096500070213     c                   clear                   wlbdat
096600070213     c                   move      §resdata      g02inv
096700070213     c                   eval      g02err = '3'
096800070213     c                   call      'XSRDA8'
096900070213     c                   parm                    wlbdat
097000070213     c                   if        g02err = '1'
097100070213     c  n01              eval      plgerr = 'E'
097200070213     c  n01              eval      plgmsg = 'DATA CONFERMA NON VALIDA'
097300070213     c  n01              eval      *in01 = *on
097400110620     c  n01              eval      *in04 = *on
097500070213     c                   endif
097600070213     c                   endif
097700070213
097800070213      * ora conferma
097900070213      *  deve essere numerico
098000070213     c                   eval      w030a = *all'0'
098100070213     c                   movel     §resora       w030a
098200070213     c                   exsr      sr_testn
098300070213     c                   if        not *in02
098400070213     c  n01              eval      plgerr = 'E'
098500070213     c  n01              eval      plgmsg = 'ORA CONFERMA NON VALIDA'
098600070213     c  n01              eval      *in01 = *on
098700110620     c  n01              eval      *in04 = *on
098800070213     c                   endif
098900070213      *  deve essere un'ora valida
099000070213     c                   if        *in02 and
099100070213     c                             %subst(§resora:1:2) > '23' or
099200070213     c                             %subst(§resora:3:2) > '59' or
099300070213     c                             %subst(§resora:5:2) > '59'
099400070213     c  n01              eval      plgerr = 'E'
099500070213     c  n01              eval      plgmsg = 'ORA CONFERMA NON VALIDA'
099600070213     c  n01              eval      *in01 = *on
099700110620     c  n01              eval      *in04 = *on
099800070213     c                   endif
099900110609
100000110609      /free
100100110609       //?La data di immissione esito su rcd RES non può essere inferiore a 30 gg da oggi
100200110609       //?in questo caso accetto il rcd ma imposto la data della distinta
100300110609       //?e come ora imposto l'ora di ricezione del record
100400110609       //?è capitato che PDA si è resettato ed ha riportato la data al 1980
100500120920         IF  not *in01 and §RESdata < %editc(Oggi_10:'X');
100600110609           §RESdata = %editc(DSTdfv:'X');
100700121015           %subst(§RESora:1:4)  = %subst(PRCdatora:9:4);
100800121010           PLGmsg   = 'DATA < o > di 10 GG';
100900110609         ENDIF;
101000120920       //?E' capitato anche che il PDA è andato fuori di testa ed ha impostato
101100120920       //?la data a 2514
101200120920         IF  not *in01 and §RESdata > %editc(Oggi_10o:'X');
101300120920           §RESdata = %editc(DSTdfv:'X');
101400121015           %subst(§RESora:1:4)  = %subst(PRCdatora:9:4);
101500121010           PLGmsg  = 'DATA < o > di 10 GG';
101600120920         ENDIF;
101700110609      /end-free
101800070130
101900070130      * numero colli
102000070130      *  deve essere numerico
102100070201     c                   eval      w030a = *all'0'
102200070201     c                   movel     §resncl       w030a
102300070201     c                   exsr      sr_testn
102400070201     c                   if        not *in02
102500070130     c  n01              eval      plgerr = 'E'
102600070201     c  n01              eval      plgmsg = 'DATI N. COLLI NON VALIDI'
102700070130     c  n01              eval      *in01 = *on
102800110620     c  n01              eval      *in04 = *on
102900070130     c                   endif
103000070130
103100070130      * numero bancali
103200070130      *  deve essere numerico
103300070201     c                   eval      w030a = *all'0'
103400070201     c                   movel     §resbnc       w030a
103500070201     c                   exsr      sr_testn
103600070201     c                   if        not *in02
103700070130     c  n01              eval      plgerr = 'E'
103800070201     c  n01              eval      plgmsg = 'DATI N. BANCALI NON VALIDI'
103900070130     c  n01              eval      *in01 = *on
104000110620     c  n01              eval      *in04 = *on
104100070130     c                   endif
104200070213
104300070213      * data inserimento record
104400070213      *  deve essere numerico
104500070213     c                   eval      w030a = *all'0'
104600070213     c                   movel     §resdatain    w030a
104700070213     c                   exsr      sr_testn
104800070213     c                   if        not *in02
104900070213     c  n01              eval      plgerr = 'E'
105000070213     c  n01              eval      plgmsg = 'DATA INSERIMENTO NON VALIDA'
105100070213     c  n01              eval      *in01 = *on
105200110620     c  n01              eval      *in04 = *on
105300070213     c                   endif
105400070213      *  deve essere una data valida
105500070213     c                   if        *in02
105600070213     c                   clear                   wlbdat
105700070213     c                   move      §resdatain    g02inv
105800070213     c                   eval      g02err = '3'
105900070213     c                   call      'XSRDA8'
106000070213     c                   parm                    wlbdat
106100070213     c                   if        g02err = '1'
106200070213     c  n01              eval      plgerr = 'E'
106300070213     c  n01              eval      plgmsg = 'DATA INSERIMENTO NON VALIDA'
106400070213     c  n01              eval      *in01 = *on
106500110620     c  n01              eval      *in04 = *on
106600070213     c                   endif
106700070213     c                   endif
106800070213
106900070213      * ora conferma
107000070213      *  deve essere numerico
107100070213     c                   eval      w030a = *all'0'
107200070213     c                   movel     §resorain     w030a
107300070213     c                   exsr      sr_testn
107400070213     c                   if        not *in02
107500070213     c  n01              eval      plgerr = 'E'
107600070213     c  n01              eval      plgmsg = 'ORA INSERIMENTO NON VALIDA'
107700070213     c  n01              eval      *in01 = *on
107800110620     c  n01              eval      *in04 = *on
107900070213     c                   endif
108000070213      *  deve essere un'ora valida
108100070213     c                   if        *in02 and
108200070213     c                             %subst(§resorain:1:2) > '23' or
108300070213     c                             %subst(§resorain:3:2) > '59' or
108400070213     c                             %subst(§resorain:5:2) > '59'
108500070213     c  n01              eval      plgerr = 'E'
108600070213     c  n01              eval      plgmsg = 'ORA INSERIMENTO NON VALIDA'
108700070213     c  n01              eval      *in01 = *on
108800110620     c  n01              eval      *in04 = *on
108900070213     c                   endif
109000110609
109100110609      /free
109200110609       //?La data di inserimento esito su rcd RES non può essere inferiore a 30 gg da oggi
109300110609       //?in questo caso accetto il rcd ma imposto la data della distinta
109400110609       //?e come ora imposto l'ora di ricezione del record
109500110609       //?è capitato che PDA si è resettato ed ha riportato la data al 1980
109600120920         IF  not *in01 and §RESdatain < %editc(Oggi_10:'X');
109700110609           §RESdatain = %editc(DSTdfv:'X');
109800121015           %subst(§RESorain:1:4)  = %subst(PRCdatora:9:4);
109900121010           PLGmsg     = 'DATA < o > di 10 GG';
110000110609         ENDIF;
110100120920       //?E' capitato anche che il PDA è andato fuori di testa ed ha impostato
110200120920       //?la data a 2514
110300120920         IF  not *in01 and §RESdatain > %editc(Oggi_10o:'X');
110400120920           §RESdatain = %editc(DSTdfv:'X');
110500121015           %subst(§RESorain:1:4)  = %subst(PRCdatora:9:4);
110600121010           PLGmsg     = 'DATA < o > di 10 GG';
110700120920         ENDIF;
110800110609      /end-free
110900070130
111000070130      * inserito da SPC
111100070130      *  deve essere 'S' o blank
111200070130     c                   if        §resflgspc <> 'S' and
111300070130     c                             §resflgspc <> *blanks
111400070130     c  n01              eval      plgerr = 'E'
111500070201     c  n01              eval      plgmsg = 'FLAG INS. DA SPC NON VALIDO'
111600070130     c  n01              eval      *in01 = *on
111700110620     c  n01              eval      *in04 = *on
111800070130     c                   endif
111900070514
112000070514      * imposto il tipo applicazione anche se non previsto dalla ds
112100070514     c                   eval      plgtipappl = 'R'
112200070130
112300070130     c                   endsr
112400070130
112500140129      *------------------------------------------------------------------------*
112600140129      * ROUTINE ELABORAZIONE RCD "ROR"
112700140129      *------------------------------------------------------------------------*
112800140129     c     sr_elabror    begsr
112900140129
113000140129     c                   eval      fiprcrords = prcdati
113100140129
113200140129      * p.o. gestione distinta
113300140129     c                   eval      wfgs = §rorfgs
113400140129     c                   exsr      sr_ctrfgs
113500140129     c   02              move      §rorfgs       plgfgs
113600140129
113700140129      * numero distinta
113800140129     c                   eval      wndc = §rorndc
113900140129     c                   exsr      sr_ctrndc
114000140207     c   02              move      §rorndc       plgndc
114100140129
114200140129      * controllo se esiste la distinta
114300140129     c   02              exsr      sr_ctrdist
114400140129
114500140129      * tipo applicazione
114600140129      * può essere C=consegne R=ritiri
114700140207     c                   if        §rortpiddo <> 'C' and §rortpiddo <> 'R'
114800140129     c  n01              eval      plgerr = 'E'
114900140129     c  n01              eval      plgmsg = 'DATI NON VALIDI'
115000140129     c  n01              eval      *in01 = *on
115100140129     c  n01              eval      *in04 = *on
115200140129     c                   else
115300140207     c                   eval      plgtipappl = §rortpiddo
115400140129     c                   endif
115500140129
115600140129      * id documento
115700140129      *  solo ritiro
115800140207     c                   if        §rortpiddo = 'R'
115900140129      *  deve essere numerico
116000140129     c                   eval      w030a = *all'0'
116100140129     c                   eval      %subst(w030a:1:14) = %subst(§roriddoc:1:14)
116200140129     c                   exsr      sr_testn
116300140129     c                   if        not *in02
116400140129     c  n01              eval      plgerr = 'E'
116500140129     c  n01              eval      plgmsg = 'DATI ID DOC. ORM NON VALIDI'
116600140129     c  n01              eval      *in01 = *on
116700140129     c  n01              eval      *in04 = *on
116800140129     c                   else
116900140129      *  deve essere un n. di ORM valido
117000140129     c                   move      §rorpoe       ormpoe
117100140129     c                   move      §rornsr       ormnsr
117200140129     c                   move      §rornor       ormnor
117300140129     c                   move      §rornrv       ormnrv
117400140129     c     kfnorm        chain(n)  fnorm01l
117500140129     c                   if        not %found(fnorm01l)
117600140129     c  n01              eval      plgerr = 'E'
117700140129     c  n01              eval      plgmsg = 'ORM INESISTENTE'
117800140129     c  n01              eval      *in01 = *on
117900140129     c  n01              eval      *in04 = *on
118000140129     c                   endif
118100140129     c                   endif
118200140129     c  n01              eval      plgiddoc = §roriddoc
118300140129     c                   endif
118400140129
118500140129      * data conferma
118600140129      *  deve essere numerico
118700140129     c                   eval      w030a = *all'0'
118800140129     c                   movel     §rordata      w030a
118900140129     c                   exsr      sr_testn
119000140129     c                   if        not *in02
119100140129     c  n01              eval      plgerr = 'E'
119200140129     c  n01              eval      plgmsg = 'DATA CONFERMA NON VALIDA'
119300140129     c  n01              eval      *in01 = *on
119400140129     c  n01              eval      *in04 = *on
119500140129     c                   endif
119600140129      *  deve essere una data valida
119700140129     c                   if        *in02
119800140129     c                   clear                   wlbdat
119900140129     c                   move      §rordata      g02inv
120000140129     c                   eval      g02err = '3'
120100140129     c                   call      'XSRDA8'
120200140129     c                   parm                    wlbdat
120300140129     c                   if        g02err = '1'
120400140129     c  n01              eval      plgerr = 'E'
120500140129     c  n01              eval      plgmsg = 'DATA CONFERMA NON VALIDA'
120600140129     c  n01              eval      *in01 = *on
120700140129     c  n01              eval      *in04 = *on
120800140129     c                   endif
120900140129     c                   endif
121000140129
121100140129      * ora conferma
121200140129      *  deve essere numerico
121300140129     c                   eval      w030a = *all'0'
121400140129     c                   movel     §rorora       w030a
121500140129     c                   exsr      sr_testn
121600140129     c                   if        not *in02
121700140129     c  n01              eval      plgerr = 'E'
121800140129     c  n01              eval      plgmsg = 'ORA CONFERMA NON VALIDA'
121900140129     c  n01              eval      *in01 = *on
122000140129     c  n01              eval      *in04 = *on
122100140129     c                   endif
122200140129      *  deve essere un'ora valida
122300140129     c                   if        *in02 and
122400140129     c                             %subst(§rorora:1:2) > '23' or
122500140129     c                             %subst(§rorora:3:2) > '59' or
122600140129     c                             %subst(§rorora:5:2) > '59'
122700140129     c  n01              eval      plgerr = 'E'
122800140129     c  n01              eval      plgmsg = 'ORA CONFERMA NON VALIDA'
122900140129     c  n01              eval      *in01 = *on
123000140129     c  n01              eval      *in04 = *on
123100140129     c                   endif
123200140129      /free
123300140129       //?La data di conferma ORM su rcd OK non può essere inferiore a 30 gg da oggi
123400140129       //?in questo caso accetto il rcd ma imposto la data della distinta
123500140129       //?e come ora imposto l'ora di ricezione del record
123600140129       //?è capitato che PDA si è resettato ed ha riportato la data al 1980
123700140129         IF  not *in01 and §rordata < %editc(Oggi_10:'X');
123800140207           §rordata = %editc(DSTdfv:'X');
123900140129           §rorora  = %subst(PRCdatora:9:6);
124000140129           PLGmsg  = 'DATA < o > di 10 GG';
124100140129         ENDIF;
124200140129       //?E' capitato anche che il PDA è andato fuori di testa ed ha impostato
124300140129       //?la data a 2514
124400140129         IF  not *in01 and §rordata > %editc(Oggi_10o:'X');
124500140129           §rordata = %editc(DSTdfv:'X');
124600140129           %subst(§rorora:1:4)  = %subst(PRCdatora:9:4);
124700140129           PLGmsg  = 'DATA < o > di 10 GG';
124800140129         ENDIF;
124900140129      /end-free
125000140129
125100140129      * ora stimata
125200140129     c                   clear                   Worasti
125300140129     c                   eval      w030a = *all'0'
125400140129     c                   movel     §rororasti    w030a
125500140129     c                   exsr      sr_testn
125600140129     c                   if        not *in02
125700140129     c  n01              eval      plgerr = 'E'
125800140129     c  n01              eval      plgmsg = 'ORA STIMATA NON VALIDA'
125900140129     c  n01              eval      *in01 = *on
126000140129     c  n01              eval      *in04 = *on
126100140129      *
126200140129     c                   if        %subst(§rorora:1:2) > '23' or
126300140129     c                             %subst(§rorora:3:2) > '59' or
126400140129     c                             %subst(§rorora:5:2) > '59'
126500140129     c  n01              eval      plgerr = 'E'
126600140129     c  n01              eval      plgmsg = 'ORA STIMATA NON VALIDA'
126700140129     c  n01              eval      *in01 = *on
126800140129     c  n01              eval      *in04 = *on
126900140129     c                   endif
127000140129
127100140129     c                   else
127200140129     c                   move      §rororasti    Worasti
127300140129     c                   endif
127400140129
127500140129      * ordinamento
127600140129     c                   clear                   Wsort
127700140129     c                   eval      w030a = *all'0'
127800140129     c                   movel     §rorsort      w030a
127900140129     c                   exsr      sr_testn
128000140129     c                   if        not *in02
128100140129     c  n01              eval      plgerr = 'E'
128200140129     c  n01              eval      plgmsg = 'ORDINAMENTO NON VALIDO'
128300140129     c  n01              eval      *in01 = *on
128400140129     c  n01              eval      *in04 = *on
128500140129
128600140129     c                   else
128700140129     c                   move      §rorsort      Wsort
128800140129     c                   endif
128900140129
129000140129     c                   endsr
129100140129
129200070130      *------------------------------------------------------------------------*
129300070130      * ROUTINE PER AGGIORNARE I DATI EFFETTIVI
129400070130      *------------------------------------------------------------------------*
129500070130     c     sr_file       begsr
129600070130
129700070130     c                   select
129800070130      * tipo record 'RCH'
129900070130     c                   when      plgtiporcd = 'RCH'
130000080327      * aggiorno fiqdt
130100070201     c                   exsr      sr_fiqdt
130200080327      * devo cancellare i dati da FIPDO e da FIPRO
130300070201     c                   exsr      sr_fipdo
130400080319     c                   exsr      sr_fipro
130500070130
130600070130      * tipo record 'RES'
130700070131     c                   when      plgtiporcd = 'RES' and
130800070131     c                             not *in03
130900070201     c                   exsr      sr_fnorm
131000070130
131100070130     c                   endsl
131200070130
131300070130     c                   endsr
131400070202
131500070202      *------------------------------------------------------------------------*
131600070202      * ROUTINE SCRITTURA FILE LOG
131700070202      *------------------------------------------------------------------------*
131800070202     c     sr_wrtlog     begsr
131900070202
132000070202     c                   eval      plgprfc = prcprfc
132100070202     c                   eval      plgiddisp = prciddisp
132200070202     c                   eval      plgdatora = prcdatora
132300070202     c                   eval      plgdati = prcdati
132400070205     c                   write     fiplg000
132500070202
132600070202     c                   endsr
132700070201
132800080415      *------------------------------------------------------------------------*
132900080415      * ROUTINE SCRITTURA FILE PER PROSPETTO ORM
133000080415      *------------------------------------------------------------------------*
133100080415     c     sr_Prospetto  begsr
133200080415
133300080415      * se ricevo un RES
133400080417    1c                   If        plgtiporcd = 'RES'
133500080415      * cerco l'ORM da scrivere o aggiornare nel file dettaglio prospetto ORM
133600080415     c     kfirpd        chain     Firpd02l
133700080415
133800080417    2c                   if        not %found(firpd02l)
133900080415     c                   clear                   firpd000
134000080415     c                   eval      rpdfgs = plgfgs
134100080415     c                   eval      rpdnfv = plgndc
134200080415     c                   eval      rpdpdr = plgcodaut
134300080415     c                   eval      rpdiddoc = plgiddoc
134400080415      * è un manuale ???
134500080417    3c                   if        %subst(plgiddoc:1:8) = *all'0'
134600080417     c                   eval      rpdman = 'S'
134700080415     c                   eval      rpdrsr = §resrsr
134800080415     c                   eval      rpdinr = §resinr
134900080417    3c                   endif
135000080417     c                   move      §resncl       rpdncl
135100080417     c                   move      §resbnc       rpdbnc
135200080415     c                   eval      rpdcmr = §rescmr
135300080417     c                   movel     §resdata      rpddtr
135400080417     c                   move      §resora       rpdorr
135500080415     c                   eval      rpdflgspc = §resflgspc
135600080417     c                   movel     §resdtorin    rpddtorin
135700081106      * non scrivo record se orm manuale annullato
135800081106     c                   if        rpdman <> 'S' or
135900081106     c                             (rpdman = 'S' and §rescmr <> 'AAA')
136000080417     c                   write     firpd000
136100140611
136200140611      /free
136300140611       //?Scrivo, se non è già stata scritta prima, la testata FIRPT
136400140611         chain(n) (RPDfgs:RPDnfv:RPDpdr) FIRPT01L;
136500140611         IF  not %found(FIRPT01L);
136600140611           clear FIRPT000;
136700140611           RPTfgs = PLGfgs;
136800140611           RPTnfv = PLGndc;
136900140611           RPTpdr = PLGcodaut;
137000140611           RPTdfv = DSTdfv;
137100140611           RPTfpp = DSTfpp;
137200140611           RPTtpm = DSTtpm;
137300140611           write FIRPT000;
137400140611         ENDIF;
137500140611      /end-free
137600140611
137700081106     c                   endif
137800080417    2c                   endif
137900080417      * se trovato record se data ora modifica maggiore di quella registrata aggiorno
138000080417    2c                   if        %found(firpd02l) and
138100080417     c                             rpddtorin < §resdtorin
138200081106      * ma solo se non è un orm manuale annullato
138300081106     c                             and (rpdman <> 'S' or
138400081106     c                                 (rpdman = 'S' and §rescmr <> 'AAA'))
138500080417     c                   move      §resncl       rpdncl
138600080417     c                   move      §resbnc       rpdbnc
138700080417     c                   eval      rpdcmr = §rescmr
138800080505     c                   movel     §resdtorin    rpddtorin
138900080505     c                   eval      rpdflgspc = §resflgspc
139000080417     c                   update    firpd000
139100080417    2c                   endif
139200081106
139300081106      * se trovato record e orm manuale annullato cancello il record
139400081106    2c                   if        %found(firpd02l) and
139500081106     c                             rpdman = 'S' and §rescmr = 'AAA'
139600081106     c                   delete    firpd000
139700081106    2c                   endif
139800080417
139900080417    1c                   endif
140000080417
140100080417      * se ricevo un RCH
140200080417    1c                   If        plgtiporcd = 'RCH'
140300080417      * cerco se la distinta è presente
140400080417     c     kfirpt        chain     Firpt01l
140500080417
140600080417    2c                   if        not %found(firpt01l)
140700080417     c                   clear                   firpt000
140800080417     c                   eval      rptfgs = plgfgs
140900080417     c                   eval      rptnfv = plgndc
141000080417     c                   eval      rptpdr = plgcodaut
141100080421     c                   eval      rptdfv = dstdfv
141200080418     c                   eval      rptfpp = dstfpp
141300080418     c                   eval      rpttpm = dsttpm
141400080417     c                   move      §rchncl       rpttco
141500080417     c                   move      §rchbnc       rpttbn
141600080417     c                   movel     §rchdata      rptdch
141700080417     c                   move      §rchora       rptoch
141800080417     c                   write     firpt000
141900080417    2c                   endif
142000080417      * se trovato record se data ora modifica maggiore di quella registrata aggiorno
142100080417    2c                   if        %found(firpt01l)
142200080417     c                   movel     rptdch        w_dtaorach
142300080417     c                   move      rptoch        w_dtaorach
142400080417    3c                   If        w_dtaorach < §rchdatora
142500080417     c                   move      §rchncl       rpttco
142600080417     c                   move      §rchbnc       rpttbn
142700080417     c                   movel     §rchdata      rptdch
142800080417     c                   move      §rchora       rptoch
142900080417     c                   update    firpt000
143000080417    3c                   endif
143100080417    2c                   endif
143200080417
143300080417    1c                   endif
143400080415
143500080415     c                   endsr
143600080319      *------------------------------------------------------------------------*
143700080319      * ROUTINE SCRITTURA FILE OK RICEVUTI
143800080319      *------------------------------------------------------------------------*
143900080319     c     sr_wrtpro     begsr
144000080319
144100080730      * verifico se estiste già un ok per questo ORM + DISTINTA + AUT
144200080319
144300140128     c     kfipro        chain     fipro11l
144400080319
144500080319      * se il record esiste e l'ok che ho ricevuto ha un idrow minore o uguale
144600140207     c                   if        %found(fipro11l)
144700140207     c                   movel     prodati       fiprookds
144800140207     c                   endif
144900080319      * a quello del file vado a fine
145000140207     c                   if        %found(fipro11l) and widrow <= §proidrow
145100080319     c                   leavesr
145200080319     c                   endif
145300080319
145400140128     c                   eval      prodatora = §OKDATORA
145500080319      * valorizzo i campi dell'ok
145600140128     c                   eval      §proidrow = widrow
145700140128     c                   eval      §proflgass = §OKFLGASS
145800140224     c                   eval      §PROFLGVIS = ' '
145900140128     c                   eval      §pronotass = §OKnotass
146000140128     c                   eval      Prodati    = fiprookds
146100080319
146200140207     c                   if        not %found(fipro11l)
146300080319     c                   eval      profgs = plgfgs
146400080319     c                   eval      prondc = plgndc
146500080730     c                   eval      procodaut = plgcodaut
146600080319     c                   clear                   proatb
146700080319     c                   eval      propoe = ormpoe
146800080319     c                   eval      pronsr = ormnsr
146900080319     c                   eval      pronor = ormnor
147000080319     c                   eval      pronrv = ormnrv
147100140128     c                   eval      protrd = 'OK '
147200140128     c                   write     fipro100
147300080319     c                   else
147400140128     c                   update    fipro100
147500080319     c                   endif
147600080319
147700080319     c                   endsr
147800080319
147900140129      *------------------------------------------------------------------------*
148000140129      * ROUTINE SCRITTURA FILE ROR RICEVUTI
148100140129      *------------------------------------------------------------------------*
148200140129     c     sr_wrtproror  begsr
148300140129
148400140129      * verifico se estiste già un ok per questo ORM + DISTINTA + AUT
148500140129
148600140129     c     kfipro        chain     fipro11l
148700140129
148800140129     c                   eval      prodatora = §rorDATORA
148900140207      * valorizzo i campi del ror
149000140207     c                   move      §rorORASTI    §PROORASTI
149100140207     c                   move      §rorSORT      §PROSORT
149200140129     c                   eval      Prodati    = fiprorords
149300140129
149400140129     c                   if        not %found(fipro11l)
149500140129     c                   eval      profgs = plgfgs
149600140129     c                   eval      prondc = plgndc
149700140129     c                   eval      procodaut = plgcodaut
149800140129     c                   clear                   proatb
149900140129     c                   eval      propoe = ormpoe
150000140129     c                   eval      pronsr = ormnsr
150100140129     c                   eval      pronor = ormnor
150200140129     c                   eval      pronrv = ormnrv
150300140129     c                   eval      protrd = 'ROR'
150400140129     c                   write     fipro100
150500140129     c                   else
150600140129     c                   update    fipro100
150700140129     c                   endif
150800140129
150900140129     c                   endsr
151000140129
151100070201      *------------------------------------------------------------------------*
151200070201      * ROUTINE PER CONTROLLARE IL P.O. GESTIONE DISTINTA
151300070201      *------------------------------------------------------------------------*
151400070201     c     sr_ctrfgs     begsr
151500070201
151600070201     c                   eval      w030a = *all'0'
151700070201     c                   movel     wfgs          w030a
151800070201     c                   exsr      sr_testn
151900070201     c                   if        not *in02
152000070201     c  n01              eval      plgerr = 'E'
152100070201     c  n01              eval      plgmsg = 'DATI P.O. DISTINTA NON VALIDI'
152200070201     c  n01              eval      *in01 = *on
152300110620     c  n01              eval      *in04 = *on
152400070201     c                   endif
152500070201
152600070201     c                   endsr
152700070201
152800070201      *------------------------------------------------------------------------*
152900070201      * ROUTINE PER CONTROLLARE IL NUMERO DISTINTA
153000070201      *------------------------------------------------------------------------*
153100070201     c     sr_ctrndc     begsr
153200070201
153300070201     c                   eval      w030a = *all'0'
153400070201     c                   movel     wndc          w030a
153500070201     c                   exsr      sr_testn
153600070201     c                   if        not *in02
153700070201     c  n01              eval      plgerr = 'E'
153800070201     c  n01              eval      plgmsg = 'DATI NUM. DISTINTA NON VALIDI'
153900070201     c  n01              eval      *in01 = *on
154000110620     c  n01              eval      *in04 = *on
154100070201     c                   endif
154200070201
154300070201     c                   endsr
154400070201
154500070201      *------------------------------------------------------------------------*
154600070201      * ROUTINE PER CONTROLLARE L'EISISTENZA DELLA DISTINTA
154700070201      *------------------------------------------------------------------------*
154800070201     c     sr_ctrdist    begsr
154900070201
155000080226     c                   eval      dstnpg = 4
155100070201     c                   move      wndc          dstnfv
155200070201     c                   move      wfgs          dstfgs
155300080317     c                   clear                   ddstflr
155400080226     c     kfidst        chain     fidst01l
155500080226     c                   if        not %found(fidst01l) or dstatb <> *blanks
155600070201     c  n01              eval      plgerr = 'E'
155700070201     c  n01              eval      plgmsg = 'DISTINA INESISTENTE'
155800070201     c  n01              eval      *in01 = *on
155900110620     c  n01              eval      *in04 = *on
156000080317     c                   leavesr
156100070201     c                   endif
156200080317     c                   eval      ddstflr = dstflr
156300070201
156400070201     c                   endsr
156500080513
156600080513      *------------------------------------------------------------------------*
156700080513      * ROUTINE PER CONTROLLARE CODICE SPC
156800080513      *------------------------------------------------------------------------*
156900080513     c     sr_ctrcodspc  begsr
157000080513
157100080519     c                   eval      *in05 = *off
157200080924     c                   eval      xx = 1
157300080924     c     §rchcodspc    lookup    codspc(xx)                             30
157400080514     c                   if        not *in30
157500080519     c                   eval      *in05 = *on
157600080924     c                   else
157700080924     c                   eval      §rchspccod = codspc(xx)
157800080924     c                   eval      §rchspcdes = desspc(xx)
157900080514     c                   endif
158000080513
158100080513     c                   endsr
158200070201
158300070201      *------------------------------------------------------------------------*
158400070201      * ROUTINE PER CONTROLLARE SE IL CAMPO E' NUMERICO
158500070201      *------------------------------------------------------------------------*
158600070201     c     sr_testn      begsr
158700070201
158800070201     c                   eval      *in02 = *off
158900070201     c                   testn                   w030a                02
159000070201
159100070201     c                   endsr
159200070201
159300070201      *------------------------------------------------------------------------*
159400070201      * ROUTINE PER AGGIORNARE FIQDT
159500070201      *------------------------------------------------------------------------*
159600070201     c     sr_fiqdt      begsr
159700070201
159800070201      * imposto la data e ora attuale
159900070201     c                   time                    w0140
160000070201     c                   move      w0140         udate8
160100070201     c                   movel     w0140         hhmm
160200070201     c                   z-add     udate8        g02dat
160300070201     c                   move      *blank        g02err
160400070201     c                   call      'XSRDA8'
160500070201     c                   parm                    wlbdat
160600070201     c                   z-add     g02inv        dateu8
160700070201
160800070201      * aggancio FIQDT
160900070202     c     kfiqdt        chain(e)  fiqdt01l
161000070205      * se rcd allocato flag di comodo per poi fare il rollback
161100070202     c                   if        %error
161200070202     c                   eval      wfiqdt = *on
161300070202     c                   leavesr
161400070202     c                   endif
161500070201     c                   if        %found(fiqdt01l)
161600070201      * bancali ritirati
161700081106      * se distinta a PDA non in test sommo i bancali rientrati a quando già presente su
161800081106      * fiqdt
161900110316     c**!!!              if        dstpda = 'E' and §dsttstpda = *blanks
162000110316     c**!!!              add       wbnc          qdtnbnri
162100110316     c**!!!              eval      qdtorari = hhmm
162200110316     c**!!!              eval      qdtdairi = dateu8
162300110316     c**!!!              eval      qdtfnbri = 'S'
162400110316     c**!!!              endif
162500080317     c****               if        wbnc <> qdtnbnri
162600080317     c****               eval      qdtnbnri = wbnc
162700080317     c****               eval      qdtorari = hhmm
162800080317     c****               eval      qdtdairi = dateu8
162900080317     c****               endif
163000070201      * colli ritirati
163100070201     c                   if        wncl <> qdtnclrit
163200070201     c                   eval      qdtnclrit = wncl
163300081112     c                   eval      qdtfncrit = 'S'
163400070201     c                   endif
163500070201
163600070205     c                   update    fiqdt000
163700070201     c                   endif
163800070201
163900070201     c                   endsr
164000080327
164100080327      *------------------------------------------------------------------------*
164200080327      * ROUTINE PER SCRIVERE LA FASE RCH SULLA DISTINTA
164300080327      *------------------------------------------------------------------------*
164400080327     c     sr_fase       begsr
164500080327
164600080327     c                   clear                   fior90ds
164700080327     c                   eval      or90tla = 'W'
164800080327     c                   eval      or90fas = 'RCH'
164900080327     c                   eval      or90fgs = plgfgs
165000080327     c                   eval      or90ndc = plgndc
165100080327     c                   eval      or90flgins = 'P'
165200080327     c                   eval      or90comit = '1'
165300080924     c                   eval      kpjbu = fidsfrchds
165400080327     c                   call      'FIOR90R'
165500080327     c                   parm                    kpjba
165600080327     c                   parm                    fior90ds
165700080327      * se torna errore imposto flag per poi fare il rollback
165800080327     c                   if        or90err <> *blanks
165900080327     c                   eval      wfase = *on
166000080327     c                   leavesr
166100080327     c                   endif
166200080327
166300080327     c                   endsr
166400070201
166500070201      *------------------------------------------------------------------------*
166600080319      * ROUTINE PER CANCELLARE RCD DA FIPDO
166700070201      *------------------------------------------------------------------------*
166800070201     c     sr_fipdo      begsr
166900080319
167000080319      * Cancellazione PDO
167100070201     c     kfiqdt        setll     fipdo01t
167200070201     c                   do        *hival
167300070205     c     kfiqdt        reade(e)  fipdo01t
167400070205      * se rcd allocato flag di comodo per poi fare il rollback
167500070205     c                   if        %error
167600070205     c                   eval      wfipdo = *on
167700070205     c                   leave
167800070205     c                   endif
167900070201     c                   if        %eof(fipdo01t)
168000070201     c                   leave
168100070201     c                   endif
168200070205     c                   delete    fipdo000
168300070201     c                   enddo
168400080319
168500070201     c                   endsr
168600070201
168700080319      *------------------------------------------------------------------------*
168800080319      * ROUTINE PER CANCELLARE RCD DA FIPRO
168900080319      *------------------------------------------------------------------------*
169000080319     c     sr_fipro      begsr
169100080319
169200080319      * Cancellazione PRO
169300140207     c     kfiqdt        setll     fipro11l
169400080319     c                   do        *hival
169500140207     c     kfiqdt        reade(e)  fipro11l
169600080319      * se rcd allocato flag di comodo per poi fare il rollback
169700080319     c                   if        %error
169800080319     c                   eval      wfipro = *on
169900080319     c                   leave
170000080319     c                   endif
170100140207     c                   if        %eof(fipro11l)
170200080319     c                   leave
170300080319     c                   endif
170400140207     c                   delete    fipro100
170500080319     c                   enddo
170600080319
170700080319     c                   endsr
170800080319
170900070201      *------------------------------------------------------------------------*
171000070201      * ROUTINE PER AGGIORNARE FNORM/FNORF
171100070201      *------------------------------------------------------------------------*
171200070201     c     sr_fnorm      begsr
171300070201
171400070201      * imposto la data e ora attuale
171500070201     c                   time                    w0140
171600070201     c                   move      w0140         udate8
171700070201     c                   z-add     udate8        g02dat
171800070201     c                   move      *blank        g02err
171900070201     c                   call      'XSRDA8'
172000070201     c                   parm                    wlbdat
172100070201     c                   z-add     g02inv        dateu8
172200070201
172300070201      * aggancio FNORM
172400070202     c     kfnorm        chain(e)  fnorm01l
172500070205      * se rcd allocato flag di comodo per poi fare il rollback
172600070202     c                   if        %error
172700070202     c                   eval      wfnorm = *on
172800070202     c                   leavesr
172900070202     c                   endif
173000070201     c                   if        %found(fnorm01l)
173100080219      * aggancio l'ultima fase dell'ORM
173200080219      * per recuperare l'eventuale data/ora dell'ultimo esito avuto da PDA
173300080219     c                   clear                   dorf01
173400080219     c                   clear                   savdfo
173500080219     c                   clear                   savofo
173600080219     c     kfnorf        chain     fnorf01l
173700080219     c                   if        %found(fnorf01l)
173800080219     c                   eval      dorf01 = orfflo
173900080320      * mi salvo orfass della fase precedente
174000080320     c                   eval      save_§orfass = §orfass
174100080219     c                   endif
174200160324      * salvo data/ora esecuzione da PDA, ma solo se fase PDA (410/420)
174300080219     c                   if        §orfesito <> *blanks
174400160324     c                             and (ORFfar = 410 or ORFfar = 420)
174500080219     c                   eval      savdfo = %int(%subst(§orfesito:1:8))
174600080219     c                   eval      savofo = %int(%subst(§orfesito:9:6))
174700080219     c                   endif
174800080212
174900070201     c                   select
175000070201      * esito fatto
175100070201     c                   when      §rescmr = *blanks
175200070201     c                   eval      ormfao = 420
175300070201      * esito non fatto
175400070201     c                   when      §rescmr <> *blanks
175500070201     c                   eval      ormfao = 410
175600070201     c                   endsl
175700070201
175800070213     c                   move      §resdata      ormdfo
175900070213     c                   move      §resora       ormofo
176000070716      * imposto il campo ORMETI
176100070716     c                   clear                   tibs02ds
176200070716     c                   eval      t02mod = 'C'
176300070716     c                   eval      t02sif = knsif
176400070716     c                   eval      t02cod = 'FAR'
176500070716     c                   movel(p)  ormfao        t02ke1
176600070716     c                   call      'TIBS02R'
176700070716     c                   parm                    kpjba
176800070716     c                   parm                    tibs02ds
176900070716     c                   eval      dfar = t02uni
177000070716     c                   eval      ormeti = d§farass
177100070201      * scrivo fase
177200070201     c                   clear                   fnorf000
177300080219     c                   clear                   dorf01
177400070201     c                   eval      orfpoe = ormpoe
177500070201     c                   eval      orfnsr = ormnsr
177600070201     c                   eval      orfnor = ormnor
177700070201     c                   eval      orfnrv = ormnrv
177800070201     c                   eval      orfpog = plgfgs
177900070201     c                   eval      orfdae = ormdfo
178000070201     c                   eval      orfore = ormofo
178100070201     c                   eval      orffar = ormfao
178200070221     c                   eval      orfpue = prcprfc
178300070201     c                   eval      orfcar = §rescmr
178400070201     c                   eval      orffgs = plgfgs
178500070201     c                   eval      orfndc = plgndc
178600070201     c                   eval      orfddc = ormddc
178700080219     c                   eval      §orfesito = §resdtorin
178800080320      * imposto  orfass della fase precedente
178900080320     c                   eval      §orfass =  save_§orfass
179000080219     c                   eval      orfflo = dorf01
179100070205     c                   write     fnorf000
179200080409      * scrivo NOTE
179300080409      * solo se ci sono
179400080409     c                   if        §resnote <> *blanks
179500080409     c                   clear                   wprg
179600080409      * cerco ultimo progressivo relativo alla stessa fase
179700080409     c     kfnorn        setll     fnorn02l
179800080409     c                   do        *hival
179900080409     c     kfnorn        reade     fnorn02l
180000080409     c                   if        %eof(fnorn02l)
180100080409     c                   leave
180200080409     c                   endif
180300080409     c                   eval      wprg = ornprg
180400080409     c                   enddo
180500080409      * scrivo la nota
180600080409     c                   add       1             wprg
180700080409     c                   clear                   fnorn000
180800080409     c                   eval      ornpoe = ormpoe
180900080409     c                   eval      ornnsr = ormnsr
181000080409     c                   eval      ornnor = ormnor
181100080409     c                   eval      ornnrv = ormnrv
181200080409     c                   eval      ornfar = ormfao
181300080409     c                   movel     §orfesito     orndai
181400080409     c                   move      §orfesito     ornori
181500080409     c                   eval      ornprg = wprg
181600080409     c                   eval      ornpue = orfpue
181700080409     c                   eval      ornpon = plgfgs
181800080409     c                   eval      ornsns = 'I'
181900080409     c                   eval      ornnot = §resnote
182000080409     c                   eval      ornftr = 'S'
182100080409     c                   eval      orndtr = dateu8
182200080409     c                   write     fnorn000
182300080409     c                   endif
182400070202      * aggiorno ORM
182500080212      * solo se la data/ora ultima fase dell'ORM è inferiore a quella che sto per scrivere
182600080212     c                   if        ormdfo > savdfo or
182700080214     c                             (ormdfo = savdfo and ormofo >= savofo)
182800070205     c                   update    fnorm000
182900080212     c                   endif
183000080227      * scrivo la partita iva e il codice fiscale nel file di appoggio
183100080320      *
183200080320      * verifico se esiste il file di appoggio fipci02l
183300080320     c     kfnorm        chain     fipci02l
183400080320      * nel casi in cui almeno uno dei due dati è valorizzato aggiorno o scrivo
183500080227     c                   if        §rescodfis <> *blanks or §respiva <> *blanks
183600080227     c                   if        %found(fipci02l)
183700080227     c                   eval      pcicdf = §rescodfis
183800080227     c                   eval      pciiso = §resisoiva
183900080228     c                   eval      pcicpi = §respiva
184000080227     c                   clear                   pciesito
184100080227     c                   update    fipci000
184200080227     c                   else
184300080227     c                   clear                   fipci000
184400080227     c                   eval      pcipoe = ormpoe
184500080227     c                   eval      pcinsr = ormnsr
184600080227     c                   eval      pcinor = ormnor
184700080227     c                   eval      pcinrv = ormnrv
184800080227     c                   eval      pcicdf = §rescodfis
184900080227     c                   eval      pciiso = §resisoiva
185000080228     c                   eval      pcicpi = §respiva
185100080228     c                   write     fipci000
185200080227     c                   endif
185300080227     c                   endif
185400080320
185500080321      * nel casi in cui entrambi sono a blank ed il record esiste annullo il rcd
185600080320     c                   if        §rescodfis = *blanks and §respiva = *blanks
185700080320     c                             and %found(fipci02l)
185800080321     c                   eval      pciatb = 'A'
185900080321     c                   update    fipci000
186000080320     c                   endif
186100080320
186200070201     c                   endif
186300070201
186400070201     c                   endsr
186500080519
186600080519      *------------------------------------------------------------------------*
186700080519      * ROUTINE PER INVIARE MAIL SE ERRORE CODICE SPC
186800080519      *------------------------------------------------------------------------*
186900080519     c     sr_mail       begsr
187000080519
187100080519      * Reperimento tab. "MRA" se non già recuperata
187200080519     c                   if        not $okmra
187300080519     c                   clear                   dmradan
187400080519     c                   clear                   tibs02ds
187500080519     c                   eval      t02mod = 'C'
187600080519     c                   eval      t02sif = knsif
187700080519     c                   eval      t02cod = 'MRA'
187800080519     c                   eval      t02ke1 = sdspgm
187900080519     c                   call      'TIBS02R'
188000080519     c                   parm                    kpjba
188100080519     c                   parm                    tibs02ds
188200080519     c                   if        t02err = *blanks
188300080519     c                   eval      dmradan = t02uni
188400080519     c                   eval      $okmra = *on
188500080519     c                   endif
188600080519     c                   endif
188700080519
188800080519      * Override al file di stampa ed apertura dello stesso
188900080519     c                   if        not %open(prtemail)
189000080519     c                   exsr      sr_openprtf
189100080519     c                   endif
189200080522
189300080522      * Stampa errore
189400080522     c                   if        *in05
189500080522     c                   eval      o_testo = 'trovato codice SPC non valido in +
189600080522     c                                        chiusura distinta ' +
189700080522     c                                        %editc(plgfgs:'X') + '-' +
189800080522     c                                        %editc(plgndc:'X') +
189900080522     c                                        ' in data ' +
190000080522     c                                        %subst(prcdatora:7:2) + '/' +
190100080522     c                                        %subst(prcdatora:5:2) + '/' +
190200080522     c                                        %subst(prcdatora:1:4)
190300080522     c                   except    prtdet
190400080522     c                   endif
190500080522     c                   if        *in01
190600080522     c                   eval      o_testo = 'trovato errore per la distinta ' +
190700080522     c                                        %editc(plgfgs:'X') + '-' +
190800080522     c                                        %editc(plgndc:'X') +
190900080522     c                                        ' in data ' +
191000080522     c                                        %subst(prcdatora:7:2) + '/' +
191100080522     c                                        %subst(prcdatora:5:2) + '/' +
191200080522     c                                        %subst(prcdatora:1:4)
191300080522     c                   except    prtdet
191400080522     c                   eval      o_testo = plgmsg + 'Profilo ' + prcprfc
191500080522     c                   except    prtdet
191600080522     c                   endif
191700080522
191800080522      * Stampa una riga vuota
191900080522     c                   clear                   o_testo
192000080522     c                   except    prtdet
192100080519
192200080519     c                   endsr
192300080519
192400080519      *------------------------------------------------------------------------*
192500080519      * OVERRIDE AL FILE DI STAMPA PER IMPOSTARE I DATI PER L'INVIO VIA MAIL
192600080519      *  + STAMPA INIZIO MAIL
192700080519      *------------------------------------------------------------------------*
192800080519     c     sr_openprtf   begsr
192900080519
193000080519      * Override al file di stampa
193100080519     c                   exsr      sr_override
193200080519
193300080519     c                   open      prtemail
193400080519
193500080519      * Stampa una testata se NON è richiesta la e-mail
193600080519     c                   if        §mradreg = *blanks
193700080519     c                   eval      o_testo = jobuser + ' - ' + sdspgm
193800080519     c                             + ' - ' + %editc( *date : 'Y' )
193900080519     c                             + ' - *REM* ' + %subst(§cm1var : 7 : 70)
194000080519     c                   except    prtdet
194100080519     c                   clear                   o_testo
194200080519     c                   except    prtdet
194300080519     c                   except    prtdet
194400080519     c                   endif
194500080519
194600080519      * Stampa testo iniziale
194700080519     c                   eval      o_testo = 'trovato errore in ricezione dati +
194800080522     c                                        PDA '
194900080519     c                   except    prtdet
195000080519
195100080519      * Stampa una riga vuota
195200080519     c                   clear                   o_testo
195300080519     c                   except    prtdet
195400080519
195500080519     c                   endsr
195600080519
195700080519      *------------------------------------------------------------------------*
195800080519      * OVERRIDE AL FILE DI STAMPA PER IMPOSTARE I DATI PER L'INVIO VIA MAIL
195900080519      *  + STAMPA INIZIO MAIL
196000080519      *------------------------------------------------------------------------*
196100080519     c     sr_override   begsr
196200080519
196300080519     c                   reset                   $invio
196400080519     c                   reset                   trtcm1ds
196500080519
196600080519     c                   if        §mradreg <> *blanks
196700080519     c                   eval      §cm1mitt = %trim(§mradmitt)
196800080519     c                   eval      §cm1dst  = %trim(§mraddest)
196900080519     c                   eval      §cm1tips = §mradreg
197000080519     c                   eval      §cm1po   = c_sede
197100080519     c                   eval      §cm1var  = '*OBJM*' + §mraddes
197200080519     c                   eval      §cm1idp  = §mradidpro
197300080519     c                   eval      qcmd = c_cmdovrprtf
197400080519     c                             + ' outq(' + %trim(§mradoutqi) + ')'
197500080519     c                             + ' usrdfndta(''' + trtcm1ds + ''')'
197600080519     c                   else
197700080519     c                   eval      qcmd = c_cmdovrprtf
197800080519     c                   endif
197900080519
198000080519     c                   eval      lenght = %size(qcmd)
198100080519     c                   call(e)   'QCMDEXC'
198200080519     c                   parm                    qcmd
198300080519     c                   parm                    lenght
198400080519
198500080519     c                   if        §mradreg <> *blanks and not %error
198600080519     c                   eval      $invio = *on
198700080519     c                   endif
198800080519
198900080519     c                   endsr
199000080513
199100080513      *------------------------------------------------------------------------*
199200080513      * ROUTINE FINALE
199300080513      *------------------------------------------------------------------------*
199400080513     c     sr_end        begsr
199500080513
199600080513     c                   if        %open(prtemail)
199700080513
199800080513      * Chiusura dello spool?
199900080522     c                   eval      o_testo = sdspgm
200000080522     c                   except    prtdet
200100080513     c                   clear                   o_testo
200200080513     c                   except    prtdet
200300080513     c                   eval      o_testo = '***   Fine Lista   ***'
200400080513     c                   except    prtdet
200500080513
200600080513     c                   close     prtemail
200700080513
200800080513      * Eliminazione overflow?
200900080513     c                   eval      lenght = %size(qcmd)
201000080513     c                   eval      qcmd = c_cmddltovr
201100080513     c                   call(e)   'QCMDEXC'
201200080513     c                   parm                    qcmd
201300080513     c                   parm                    lenght
201400080513
201500080513     c                   endif
201600080513
201700080513     c                   endsr
201800070129
201900070129      *------------------------------------------------------------------------*
202000070129      * ROUTINE INIZIALE
202100070129      *------------------------------------------------------------------------*
202200070129     c     *inzsr        begsr
202300070129
202400070129     c     *entry        plist
202500070129     c                   parm                    parmpo
202600080226     c                   parm                    chiudi
202700070129
202800070129     c     kfiapd        klist
202900070129     c                   kfld                    apdtip
203000070129     c                   kfld                    apdpdr
203100070201
203200070201     c     kfiqdt        klist
203300070201     c                   kfld                    plgfgs
203400070201     c                   kfld                    plgndc
203500080319
203600080319     c     kfipro        klist
203700080319     c                   kfld                    plgfgs
203800080319     c                   kfld                    plgndc
203900140129     c                   kfld                    plgtiporcd
204000080730     c                   kfld                    plgcodaut
204100080319     c                   kfld                    ormpoe
204200080319     c                   kfld                    ormnsr
204300080319     c                   kfld                    ormnor
204400080319     c                   kfld                    ormnrv
204500070201
204600080226     c     kfidst        klist
204700080226     c                   kfld                    dstnpg
204800070201     c                   kfld                    dstnfv
204900070201     c                   kfld                    dstfgs
205000070131
205100070131     c     kfnorm        klist
205200070131     c                   kfld                    ormpoe
205300070131     c                   kfld                    ormnsr
205400070131     c                   kfld                    ormnor
205500070131     c                   kfld                    ormnrv
205600080219
205700080219     c     kfnorf        klist
205800080219     c                   kfld                    ormpoe
205900080219     c                   kfld                    ormnsr
206000080219     c                   kfld                    ormnor
206100080219     c                   kfld                    ormnrv
206200080219     c                   kfld                    ormdfo
206300080219     c                   kfld                    ormofo
206400080219     c                   kfld                    ormfao
206500080409
206600080409     c     kfnorn        klist
206700080409     c                   kfld                    ormpoe
206800080409     c                   kfld                    ormnsr
206900080409     c                   kfld                    ormnor
207000080409     c                   kfld                    ormnrv
207100080409     c                   kfld                    ormfao
207200080415
207300080415     c     kfirpd        klist
207400080415     c                   kfld                    plgfgs
207500080415     c                   kfld                    plgndc
207600080415     c                   kfld                    plgcodaut
207700080415     c                   kfld                    plgiddoc
207800080417
207900080417     c     kfirpt        klist
208000080417     c                   kfld                    plgfgs
208100080417     c                   kfld                    plgndc
208200080417     c                   kfld                    plgcodaut
208300140207      *
208400140207     c     ktbe          klist
208500140207     c                   kfld                    tbecod
208600140207     c                   kfld                    tbeke1
208700070129
208800070129     c                   endsr
208900080513
209000080513     oprtemail  e            prtdet      1
209100080513     o                       o_testo
