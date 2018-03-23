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
007800170703     d wdatadistinta...
007900170703     d                 s                   like(QDTddc)
008000070129
008100080513      *------------------------------------------------------------------------*
008200080513      *   S C H I E R E
008300080513      *------------------------------------------------------------------------*
008400090911     d codspc          s              2    dim(23)
008500090911     d desspc          s             15    dim(23)
008600080513
008700070130      *------------------------------------------------------------------------*
008800070129      *   D S   I N T E R N E / E S T E R N E
008900070130      *------------------------------------------------------------------------*
009000070205     d prc             ds
009100070205     d  prcnrr               397    400B 0
009200070205
009300070201     d wlbdat          ds
009400070201     d  g02dat                 1      8  0
009500070201     d  g02inv                 9     16  0
009600070201     d  g02err                17     17
009700070201     d  g02tgi                18     22  0
009800070201
009900070716     d dfar          e ds
010000080219     d dorf01        e ds
010100080317     d ddstflr       e ds
010200080513     d dpsp          e ds
010300090911     d dpsps         e ds
010400080924
010500080924     d fidsfrchds    e ds                  prefix(§RCH:4)
010600070716
010700140129     d fiprorords    e ds
010800140129     d fiprookds     e ds
010900140129     d fiprcrords    e ds
011000140129     d  §rorpoe               10     12
011100140129     d  §rornsr               13     14
011200140129     d  §rornor               15     21
011300140129     d  §rornrv               22     23
011400140129     d  §roraas               10     13
011500140129     d  §rorlnp               14     16
011600140129     d  §rornrs               17     18
011700140129     d  §rornsp               19     25
011800140129     d  §rordata              26     33
011900140129     d  §rorora               34     39
012000070129     d fiprcokds     e ds
012100070201     d  §okpoe                10     12
012200070201     d  §oknsr                13     14
012300070201     d  §oknor                15     21
012400070201     d  §oknrv                22     23
012500070201     d  §okaas                10     13
012600070201     d  §oklnp                14     16
012700070201     d  §oknrs                17     18
012800070201     d  §oknsp                19     25
012900070213     d  §okdata               26     33
013000070213     d  §okora                34     39
013100070129     d fiprcrchds    e ds
013200070213     d  §rchdata              20     27
013300070213     d  §rchora               28     33
013400070129     d fiprcresds    e ds
013500070213     d  §resdata              24     31
013600070213     d  §resora               32     37
013700070213     d  §resdatain           171    178
013800070213     d  §resorain            179    184
013900080317
014000080513     d dmradan       e ds                  inz
014100080327     d fior90ds      e ds
014200070130     d kpjba         e ds
014300070129     d tibs02ds      e ds
014400080513
014500080513     d trtcm1ds      e ds                  inz
014600080514      *    .§cm1mitt = Indirizzo e-mail del mittente
014700120305     d   §cm1mitt    e                     inz('ced@brt.it')
014800080514      *    ·§cm1dst  = Indirizzo e-mail del destinatario
014900120305     d   §cm1dst     e                     inz('monica.beghelli@brt.it')
015000080514      *    ·§cm1tips = Tipo lettera via e-mail:
015100080513      *     "LET" = testo allegato in corpo con logo
015200080513      *             (richiede righe libere iniziali per il logo)
015300080513      *     "COR" = testo integrato senza logo
015400080513      *             (non consente né UNDERLINE né HIGHLIGHT)
015500080514     d   §cm1tips    e                     inz('COR')
015600080514      *    ·§cm1po   = Filiale
015700080514     d   §cm1po      e                     inz('046')
015800080514      *    ·§cm1var  = Oggetto e-mail
015900080514     d   §cm1var     e                     inz('*OBJM*+
016000080521     d                                     Errore in ricezione dati da PDA')
016100080514      *    ·§cm1sts  = Stato
016200080514     d   §cm1sts     e                     inz(*off)
016300080514      *    ·§cm1idp  = Id processo
016400080521     d   §cm1idp     e                     inz('2')
016500080513
016600080513     d psds           sds
016700080513     d   sdspgm          *proc
016800080513     d   jobuser             254    263                                         User name
016900080513
017000080513      *------------------------------------------------------------------------*
017100080513      *   C O S T A N T I
017200080513      *------------------------------------------------------------------------*
017300080513     d c_sede          c                   const('046')
017400080513      *  - comando di override al prtf
017500080513     d c_cmdovrprtf    c                   const('OVRPRTF +
017600080513     d                                           file(PRTEMAIL) +
017700080513     d                                           pagesize(66 132) +
017800080513     d                                           lpi(6) cpi(10) +
017900080513     d                                           ovrscope(*actgrpdfn) +
018000080513     d                                           ')
018100080513     d c_cmddltovr     c                   const('DLTOVR +
018200080513     d                                            file(PRTEMAIL) +
018300080513     d                                            lvl(*actgrpdfn)')
018400070129
018500070129      *------------------------------------------------------------------------*
018600070607
018700070607     c                   eval      kprcprfc = 'AU0000000'
018800070607     c                   eval      %subst(kprcprfc:3:3) = parmpo
018900110609
019000110609      *?Cerco la data del giorno
019100110609     c                   time                    w0140
019200120920      *?Calcolo oggi - 10 gg.
019300110609     c                   move      w0140         data_eur
019400110609     c                   move      data_eur      data_iso
019500120920     c                   subdur    10:*d         data_iso
019600120920     c                   move      data_iso      Oggi_10
019700120920      *?Calcolo oggi + 10 gg.
019800120920     c                   move      w0140         data_eur
019900120920     c                   move      data_eur      data_iso
020000120920     c                   adddur    10:*d         data_iso
020100120920     c                   move      data_iso      Oggi_10o
020200110202
020300110202      * Recupero i codici SPC
020400110202     c                   exsr      sr_carcodspc
020500070129
020600110202      * Leggo file ritorno dati da PDA
020700070130     c     kprcprfc      setll     fiprc01r
020800070129     c                   do        *hival
020900070129     c                   read      fiprc01r
021000070129
021100070129     c                   if        %eof(fiprc01r)
021200070129     c                   leave
021300070129     c                   endif
021400070129      * stesso p.o.
021500070129     c                   if        %subst(prcprfc:3:3) <> parmpo
021600070129     c                   leave
021700070129     c                   endif
021800070129
021900070129     c                   clear                   fiplg000
022000070130     c                   eval      *in01 = *off
022100110620     c                   eval      *in04 = *off
022200070129
022300070130      * controlla i dati inviati
022400070129     c                   exsr      sr_ctrdati
022500070130
022600070130      * aggiorno i dati effettivi in base al tipo record
022700070130      * se non ci sono stati errori nei controlli
022800101104     c                   if        not *in01
022900101104      * e la distinta NON è in test per gli ORM
023000080317     c                             and §dsttstpda <> 'O' and §dsttstpda <> 'E'
023100080205     c                   exsr      sr_file
023200080205     c                   endif
023300080724
023400080724      * scrivo fase RCH SEMPRE quando ricevo rcd RCH
023500080924      * passo in kpjbu i dati da memorizzare nel campo "dati" sul file delle fasi
023600090810     c                   if        prctiporcd = 'RCH' and not *in01
023700080724     c                   exsr      sr_fase
023800080724     c                   endif
023900080404
024000080519      * se non ci sono stati errori nei controlli
024100101104      * e la distinta è in test cancello solo i dati di FIPDO e FIPRO
024200101104     c                   if        not *in01 and
024300101104     c                             (§dsttstpda = 'O' or §dsttstpda = 'E')
024400080404      * e ricevo un RCH
024500080404     c                   if        plgtiporcd = 'RCH'
024600080404      * devo cancellare i dati da FIPDO e da FIPRO
024700080404     c                   exsr      sr_fipdo
024800080404     c                   exsr      sr_fipro
024900080404     c                   endif
025000080404     c                   endif
025100070202
025200080320      * se non ci sono stati errori nei controlli
025300080320      * tipo record 'OK' solo per i ritiri
025400101104     c                   if        not *in01 and
025500140129     c                             plgtiporcd = 'OK' and §OKTPIDDOC = 'R'
025600080320     c                   exsr      sr_wrtpro
025700080320     c                   endif
025800140129      * scrittura fipro per tipo record ROR
025900140129     c                   if        plgtiporcd = 'ROR'
026000140129     c                   exsr      sr_wrtproror
026100140129     c                   endif
026200070202      * scrivo il file di LOG
026300070202     c                   exsr      sr_wrtlog
026400070202
026500080519      * se non ci sono stati errori nei controlli
026600080415      * se ricevo fase "RES" o fase "RCH" scrivo i file legati al prospetto ORM
026700080415     c                   if        not *in01 and
026800080415     c                             (plgtiporcd = 'RCH' OR plgtiporcd = 'RES')
026900080415     c                   exsr      sr_Prospetto
027000080415     c                   endif
027100080519      * se ci sono stati errori nei controlli inivio mail
027200110620     c**!!!              if        *in01 or *in05
027300110620     c                   if        *in04 or *in05
027400080519     c                   exsr      sr_mail
027500080519     c                   endif
027600080519
027700070205      * se ho avuto errori di allocazione dei file devo
027800070202      * fare il rollback
027900070205    2c                   if        wfiqdt = *on or wfipdo = *on or
028000080327     c                             wfnorm = *on or wfipro = *on or
028100080327     c                             wfase = *on
028200070202     c                   rolbk
028300070205      * se non ho avuto errori
028400070205   2xc                   else
028500070205      * aggancio il file fisico di ricezione
028600070205     c     prcnrr        chain(e)  fiprc00r
028700070205      * se rcd allocato flag di comodo per poi fare il rollback
028800070205      * per errore rollback
028900070205    3c                   if        %error
029000070205     c                   rolbk
029100070205   3xc                   else
029200070205    4c                   if        %found(fiprc00r)
029300070202      * cancello il file di ricezione
029400070202     c                   delete    fiprc000
029500070205      * committo le modifiche
029600070205     c                   commit
029700070205    4c                   endif
029800070205    3c                   endif
029900070205    2c                   endif
030000070202
030100080327     c                   eval      wfase = *off
030200070202     c                   eval      wfiqdt = *off
030300070202     c                   eval      wfipdo = *off
030400070202     c                   eval      wfnorm = *off
030500080319     c                   eval      wfipro = *off
030600070202
030700070129     c                   enddo
030800080513
030900080513     c                   exsr      sr_end
031000080227
031100080226      * testa la chiusura del sottosistema
031200080226     c                   shtdn                                        99
031300080226     c   99              eval      chiudi = 'S'
031400070129
031500070205     c                   eval      *inrt = *on
031600110202
031700110202      *------------------------------------------------------------------------*
031800110202      * RECUPERO CODICI SPC
031900110202      *------------------------------------------------------------------------*
032000110202     c     sr_carcodspc  begsr
032100110202
032200110202     c                   clear                   codspc
032300110202     c                   clear                   desspc
032400110202
032500110202      * recupero tabella codici SPC (PSP)
032600110202      * chiave 1 per le prime 11 password
032700110202     c                   clear                   tibs02ds
032800110202     c                   clear                   dpsp
032900110202     c                   eval      t02mod = 'C'
033000110202     c                   eval      t02sif = knsif
033100110202     c                   eval      t02cod = 'PSP'
033200110202     c                   eval      t02ke1 = parmpo
033300110202     c                   eval      t02ke2 = '1'
033400110202     c                   call      'TIBS02R'
033500110202     c                   parm                    kpjba
033600110202     c                   parm                    tibs02ds
033700110202     c                   if        t02err = *blanks
033800110202     c                   eval      dpsp = t02uni
033900110202     c                   endif
034000110202
034100110202     c                   if        dpsp <> *blanks
034200110202     c                   eval      codspc(01) = §pspcod01
034300110202     c                   eval      desspc(01) = §pspdes01
034400110202     c                   eval      codspc(02) = §pspcod02
034500110202     c                   eval      desspc(02) = §pspdes02
034600110202     c                   eval      codspc(03) = §pspcod03
034700110202     c                   eval      desspc(03) = §pspdes03
034800110202     c                   eval      codspc(04) = §pspcod04
034900110202     c                   eval      desspc(04) = §pspdes04
035000110202     c                   eval      codspc(05) = §pspcod05
035100110202     c                   eval      desspc(05) = §pspdes05
035200110202     c                   eval      codspc(06) = §pspcod06
035300110202     c                   eval      desspc(06) = §pspdes06
035400110202     c                   eval      codspc(07) = §pspcod07
035500110202     c                   eval      desspc(07) = §pspdes07
035600110202     c                   eval      codspc(08) = §pspcod08
035700110202     c                   eval      desspc(08) = §pspdes08
035800110202     c                   eval      codspc(09) = §pspcod09
035900110202     c                   eval      desspc(09) = §pspdes09
036000110202     c                   eval      codspc(10) = §pspcod10
036100110202     c                   eval      desspc(10) = §pspdes10
036200110202     c                   eval      codspc(11) = §pspcod11
036300110202     c                   eval      desspc(11) = §pspdes11
036400110202     c                   endif
036500110202      * chiave 2 per le altre 11 password
036600110202     c                   clear                   dpsp
036700110202     c                   eval      t02ke2 = '2'
036800110202     c                   call      'TIBS02R'
036900110202     c                   parm                    kpjba
037000110202     c                   parm                    tibs02ds
037100110202     c                   if        t02err = *blanks
037200110202     c                   eval      dpsp = t02uni
037300110202     c                   endif
037400110202
037500110202     c                   if        dpsp <> *blanks
037600110202     c                   eval      codspc(12) = §pspcod01
037700110202     c                   eval      desspc(12) = §pspdes01
037800110202     c                   eval      codspc(13) = §pspcod02
037900110202     c                   eval      desspc(13) = §pspdes02
038000110202     c                   eval      codspc(14) = §pspcod03
038100110202     c                   eval      desspc(14) = §pspdes03
038200110202     c                   eval      codspc(15) = §pspcod04
038300110202     c                   eval      desspc(15) = §pspdes04
038400110202     c                   eval      codspc(16) = §pspcod05
038500110202     c                   eval      desspc(16) = §pspdes05
038600110202     c                   eval      codspc(17) = §pspcod06
038700110202     c                   eval      desspc(17) = §pspdes06
038800110202     c                   eval      codspc(18) = §pspcod07
038900110202     c                   eval      desspc(18) = §pspdes07
039000110202     c                   eval      codspc(19) = §pspcod08
039100110202     c                   eval      desspc(19) = §pspdes08
039200110202     c                   eval      codspc(20) = §pspcod09
039300110202     c                   eval      desspc(20) = §pspdes09
039400110202     c                   eval      codspc(21) = §pspcod10
039500110202     c                   eval      desspc(21) = §pspdes10
039600110202     c                   eval      codspc(22) = §pspcod11
039700110202     c                   eval      desspc(22) = §pspdes11
039800110202     c                   endif
039900110202
040000110202      * recupero tabella codici SPC (PSP)
040100110202      * la password di sede
040200110202     c                   clear                   tibs02ds
040300110202     c                   clear                   dpsps
040400110202     c                   eval      t02mod = 'C'
040500110202     c                   eval      t02sif = knsif
040600110202     c                   eval      t02cod = 'PSP'
040700110202     c                   eval      t02ke1 = '046'
040800110202     c                   call      'TIBS02R'
040900110202     c                   parm                    kpjba
041000110202     c                   parm                    tibs02ds
041100110202     c                   if        t02err = *blanks
041200110202     c                   eval      dpsps = t02uni
041300110202     c                   endif
041400110202     c                   if        dpsps <> *blanks
041500110202     c                   eval      codspc(23) = §pspcods
041600110202     c                   eval      desspc(23) = §pspdess
041700110202     c                   endif
041800110202
041900110202     c                   endsr
042000070129
042100070129      *------------------------------------------------------------------------*
042200070129      * ROUTINE CONTROLLO DATI FIPRC
042300070129      *------------------------------------------------------------------------*
042400070129     c     sr_ctrdati    begsr
042500080924
042600080924     c                   clear                   fidsfrchds
042700081103     c                   eval      *in05 = *off
042800070129
042900070131      * tipo record
043000070131      * se errato non posso continuare perchè non so come leggere i dati
043100070201     c                   if        prctiporcd <> 'OK ' and
043200070201     c                             prctiporcd <> 'RCH' and
043300140129     c                             prctiporcd <> 'ROR' and
043400070201     c                             prctiporcd <> 'RES'
043500070131     c                   eval      plgerr = 'E'
043600070131     c                   eval      plgmsg = 'TIPO RECORD NON VALIDO'
043700070131     c                   eval      *in01 = *on
043800110620     c                   eval      *in04 = *on
043900070131     c                   leavesr
044000070131     c                   endif
044100070131     c                   eval      plgtiporcd = prctiporcd
044200070131
044300070129      * codice autotrasportatore
044400070130      * anche se errato vado avanti lo stesso con i controlli ma lascio l'errore
044500070131     c                   eval      wcodaut = %subst(prcprfc:3:7)
044600070201      *  deve essere numerico
044700070201     c                   eval      w030a = *all'0'
044800070201     c                   movel     wcodaut       w030a
044900070201     c                   exsr      sr_testn
045000070201     c                   if        not *in02
045100070201     c                   eval      plgerr = 'E'
045200070201     c                   eval      plgmsg = 'AUTOTRASPORTATORE NON VALIDO'
045300070201     c                   eval      *in01 = *on
045400110620     c                   eval      *in04 = *on
045500070201     c                   else
045600070201      *  deve esistere
045700070129     c                   eval      apdtip = 'A'
045800070202     c                   move      wcodaut       apdpdr
045900070129     c     kfiapd        chain     fiapd01l
046000070129     c                   if        not %found(fiapd01l) or
046100070129     c                             apdatb <> *blanks
046200070129     c                   eval      plgerr = 'E'
046300070129     c                   eval      plgmsg = 'AUTOTRASPORTATORE NON VALIDO'
046400070130     c                   eval      *in01 = *on
046500110620     c                   eval      *in04 = *on
046600070130     c                   endif
046700070201     c                   endif
046800070130     c  n01              move      wcodaut       plgcodaut
046900070129
047000070129      * campo dati
047100070129     c                   select
047200070129     c                   when      prctiporcd = 'OK '
047300070129     c                   exsr      sr_elabok
047400070129     c                   when      prctiporcd = 'RCH'
047500070129     c                   exsr      sr_elabrch
047600070129     c                   when      prctiporcd = 'RES'
047700070129     c                   exsr      sr_elabres
047800140129     c                   when      prctiporcd = 'ROR'
047900140129     c                   exsr      sr_elabror
048000070129     c                   endsl
048100070129
048200070129     c                   endsr
048300070129
048400070129      *------------------------------------------------------------------------*
048500070130      * ROUTINE ELABORAZIONE RCD "OK"
048600070129      *------------------------------------------------------------------------*
048700070129     c     sr_elabok     begsr
048800070129
048900070129     c                   eval      fiprcokds = prcdati
049000070129
049100070129      * p.o. gestione distinta
049200070201     c                   eval      wfgs = §okfgs
049300070201     c                   exsr      sr_ctrfgs
049400070201     c   02              move      §okfgs        plgfgs
049500070129
049600070129      * numero distinta
049700070201     c                   eval      wndc = §okndc
049800070201     c                   exsr      sr_ctrndc
049900070201     c   02              move      §okndc        plgndc
050000070201
050100070201      * controllo se esiste la distinta
050200070201     c   02              exsr      sr_ctrdist
050300070129
050400070129      * tipo applicazione
050500070130      * può essere C=consegne R=ritiri
050600070131     c                   if        §oktpiddoc <> 'C' and §oktpiddoc <> 'R'
050700070130     c  n01              eval      plgerr = 'E'
050800070130     c  n01              eval      plgmsg = 'DATI NON VALIDI'
050900070130     c  n01              eval      *in01 = *on
051000110620     c  n01              eval      *in04 = *on
051100070130     c                   else
051200070131     c                   eval      plgtipappl = §oktpiddoc
051300070130     c                   endif
051400070129
051500070129      * id documento
051600080228      *  solo ritiro
051700080228     c                   if        §oktpiddoc = 'R'
051800070201      *  deve essere numerico
051900070201     c                   eval      w030a = *all'0'
052000070201     c                   eval      %subst(w030a:1:14) = %subst(§okiddoc:1:14)
052100070201     c                   exsr      sr_testn
052200070201     c                   if        not *in02
052300070201     c  n01              eval      plgerr = 'E'
052400070201     c  n01              eval      plgmsg = 'DATI ID DOC. ORM NON VALIDI'
052500070201     c  n01              eval      *in01 = *on
052600110620     c  n01              eval      *in04 = *on
052700070201     c                   else
052800070201      *  deve essere un n. di ORM valido
052900070201     c                   move      §okpoe        ormpoe
053000070201     c                   move      §oknsr        ormnsr
053100070201     c                   move      §oknor        ormnor
053200070201     c                   move      §oknrv        ormnrv
053300070201     c     kfnorm        chain(n)  fnorm01l
053400070201     c                   if        not %found(fnorm01l)
053500070201     c  n01              eval      plgerr = 'E'
053600070201     c  n01              eval      plgmsg = 'ORM INESISTENTE'
053700070201     c  n01              eval      *in01 = *on
053800110620     c  n01              eval      *in04 = *on
053900070201     c                   endif
054000070201     c                   endif
054100070201     c  n01              eval      plgiddoc = §okiddoc
054200080228     c                   endif
054300070213
054400070213      * data conferma
054500070213      *  deve essere numerico
054600070213     c                   eval      w030a = *all'0'
054700070213     c                   movel     §okdata       w030a
054800070213     c                   exsr      sr_testn
054900070213     c                   if        not *in02
055000070213     c  n01              eval      plgerr = 'E'
055100070213     c  n01              eval      plgmsg = 'DATA CONFERMA NON VALIDA'
055200070213     c  n01              eval      *in01 = *on
055300110620     c  n01              eval      *in04 = *on
055400070213     c                   endif
055500070213      *  deve essere una data valida
055600070213     c                   if        *in02
055700070213     c                   clear                   wlbdat
055800070213     c                   move      §okdata       g02inv
055900070213     c                   eval      g02err = '3'
056000070213     c                   call      'XSRDA8'
056100070213     c                   parm                    wlbdat
056200070213     c                   if        g02err = '1'
056300070213     c  n01              eval      plgerr = 'E'
056400070213     c  n01              eval      plgmsg = 'DATA CONFERMA NON VALIDA'
056500070213     c  n01              eval      *in01 = *on
056600110620     c  n01              eval      *in04 = *on
056700070213     c                   endif
056800070213     c                   endif
056900070213
057000070213      * ora conferma
057100070213      *  deve essere numerico
057200070213     c                   eval      w030a = *all'0'
057300070213     c                   movel     §okora        w030a
057400070213     c                   exsr      sr_testn
057500070213     c                   if        not *in02
057600070213     c  n01              eval      plgerr = 'E'
057700070213     c  n01              eval      plgmsg = 'ORA CONFERMA NON VALIDA'
057800070213     c  n01              eval      *in01 = *on
057900110620     c  n01              eval      *in04 = *on
058000070213     c                   endif
058100070213      *  deve essere un'ora valida
058200070213     c                   if        *in02 and
058300070213     c                             %subst(§okora:1:2) > '23' or
058400070213     c                             %subst(§okora:3:2) > '59' or
058500070213     c                             %subst(§okora:5:2) > '59'
058600070213     c  n01              eval      plgerr = 'E'
058700070213     c  n01              eval      plgmsg = 'ORA CONFERMA NON VALIDA'
058800080319     c  n01              eval      *in01 = *on
058900110620     c  n01              eval      *in04 = *on
059000070213     c                   endif
059100110609      /free
059200110609       //?La data di conferma ORM su rcd OK non può essere inferiore a 30 gg da oggi
059300110609       //?in questo caso accetto il rcd ma imposto la data della distinta
059400110609       //?e come ora imposto l'ora di ricezione del record
059500110609       //?è capitato che PDA si è resettato ed ha riportato la data al 1980
059600120920         IF  not *in01 and §OKdata < %editc(Oggi_10:'X');
059700110609           §OKdata = %editc(DSTdfv:'X');
059800110609           §OKora  = %subst(PRCdatora:9:6);
059900121015           PLGmsg  = 'DATA < o > di 10 GG';
060000110609         ENDIF;
060100120920       //?E' capitato anche che il PDA è andato fuori di testa ed ha impostato
060200120920       //?la data a 2514
060300120920         IF  not *in01 and §OKdata > %editc(Oggi_10o:'X');
060400120920           §OKdata = %editc(DSTdfv:'X');
060500121015           %subst(§OKora:1:4)  = %subst(PRCdatora:9:4);
060600121010           PLGmsg  = 'DATA < o > di 10 GG';
060700120920         ENDIF;
060800110609      /end-free
060900070201
061000070201      * flag richiesta assistenza
061100140207     c                   if        §okflgass  <> *blank
061200140213     c                   movel     'RAS'         tbecod
061300140213     c                   movel     §okflgass     tbeke1
061400140207     c     ktbe          chain     tntbe01l
061500140207     c                   if        not %found(tntbe01l)
061600070201     c  n01              eval      plgerr = 'E'
061700070201     c  n01              eval      plgmsg = 'FLAG RICH. ASS. NON VALIDO'
061800070201     c  n01              eval      *in01 = *on
061900110620     c  n01              eval      *in04 = *on
062000070201     c                   endif
062100140225      * non memorizziamo richiesta assistenza per ORM RC
062200140225     c                   IF        ORMnsr > 0
062300140225     c  n01              eval      plgerr = 'E'
062400140225     c  n01              eval      plgmsg = 'RICH. ASSISTENZA SU RC'
062500140225     c  n01              eval      *in01 = *on
062600140225     c  n01              eval      *in04 = *on
062700140225     c                   ENDIF
062800140207     c                   endif
062900080221
063000080221      * identificativo riga
063100080319     c                   clear                   Widrow
063200080221     c                   eval      w030a = *all'0'
063300080221     c                   movel     §okidrow      w030a
063400080221     c                   exsr      sr_testn
063500080221     c                   if        not *in02
063600080221     c  n01              eval      plgerr = 'E'
063700080221     c  n01              eval      plgmsg = 'ID. RIGA OK NON VALIDO'
063800080221     c  n01              eval      *in01 = *on
063900110620     c  n01              eval      *in04 = *on
064000080319
064100080319     c                   else
064200080319     c                   move      §okidrow      Widrow
064300080221     c                   endif
064400170428
064500170428      /free
064600170428       //?Alla fine di tutti i controlli se non ho errore controllo se l'ok ricevuto
064700170428       //?è relativo ad un annullamento ORM
064800170428         IF  not *in01;
064900170428         //?Cerco l'ORM su FIPDO a parità di IDrow se Annullato imposto PLGMSG
065000170428         //?come fa il pgm ricezione OK per le consegne
065100170428           setll (PLGfgs:PLGndc:ORMpoe:ORMnsr:ORMnor:ORMnrv) FIPDO01T;
065200170428           reade (PLGfgs:PLGndc:ORMpoe:ORMnsr:ORMnor:ORMnrv) FIPDO01T;
065300170428           DOW  not %eof(FIPDO01T);
065400170428             IF  PDOidrow = wIdRow and PDOatb = 'A';
065500170428               PLGmsg = 'CONFERMA ANNULLAMENTO';
065600170428             ENDIF;
065700170502             reade (PLGfgs:PLGndc:ORMpoe:ORMnsr:ORMnor:ORMnrv) FIPDO01T;
065800170428           ENDDO;
065900170428         ENDIF;
066000170428      /end-free
066100070129
066200070129     c                   endsr
066300070130
066400070130      *------------------------------------------------------------------------*
066500070130      * ROUTINE ELABORAZIONE RCD "RCH"
066600070130      *------------------------------------------------------------------------*
066700070130     c     sr_elabrch    begsr
066800070130
066900070130     c                   eval      fiprcrchds = prcdati
067000070130
067100070130      * p.o. gestione distinta
067200070201     c                   eval      wfgs = §rchfgs
067300070201     c                   exsr      sr_ctrfgs
067400070201     c   02              move      §rchfgs       plgfgs
067500070130
067600070130      * numero distinta
067700070201     c                   eval      wndc = §rchndc
067800070201     c                   exsr      sr_ctrndc
067900070201     c   02              move      §rchndc       plgndc
068000070201
068100070201      * controllo se esiste la distinta
068200070201     c   02              exsr      sr_ctrdist
068300070201
068400070201      * controllo se stesso AUT
068500070201     c                   if        *in02 and plgcodaut <> dstpdr
068600070201     c  n01              eval      plgerr = 'E'
068700070201     c  n01              eval      plgmsg = 'AUT. INCONGRUENTE'
068800070201     c  n01              eval      *in01 = *on
068900110620     c  n01              eval      *in04 = *on
069000070201     c                   endif
069100070201
069200070201      * la distinta deve esistere anche su FIQDT
069300070201     c                   if        *in02
069400170703     c***  kfiqdt        chain(n)  fiqdt01l
069500170703       chain (PLGfgs:PLGndc:wdatadistinta) FIQDT01L;
069600070201     c                   if        not %found(fiqdt01l) or qdtatb <> *blanks
069700070201     c  n01              eval      plgerr = 'E'
069800070201     c  n01              eval      plgmsg = 'DISTINTA INESISTENTE SU FIQDT'
069900070201     c  n01              eval      *in01 = *on
070000110620     c  n01              eval      *in04 = *on
070100070201     c                   endif
070200070201     c                   endif
070300070130
070400070130      * totale colli
070500070130      *  deve essere numerico
070600070201     c                   eval      w030a = *all'0'
070700070201     c                   movel     §rchncl       w030a
070800070201     c                   exsr      sr_testn
070900070201     c                   if        not *in02
071000070130     c  n01              eval      plgerr = 'E'
071100070201     c  n01              eval      plgmsg = 'DATI TOT. COLLI NON VALIDI'
071200070130     c  n01              eval      *in01 = *on
071300110620     c  n01              eval      *in04 = *on
071400070201     c                   else
071500070201     c                   move      §rchncl       wncl
071600070130     c                   endif
071700070130
071800070130      * totale bancali
071900070130      *  deve essere numerico
072000070201     c                   eval      w030a = *all'0'
072100070201     c                   movel     §rchbnc       w030a
072200070201     c                   exsr      sr_testn
072300070201     c                   if        not *in02
072400070130     c  n01              eval      plgerr = 'E'
072500070201     c  n01              eval      plgmsg = 'DATI TOT. BANCALI NON VALIDI'
072600070130     c  n01              eval      *in01 = *on
072700110620     c  n01              eval      *in04 = *on
072800070201     c                   else
072900070201     c                   move      §rchbnc       wbnc
073000070130     c                   endif
073100070213
073200070213      * data chiusura
073300070213      *  deve essere numerico
073400070213     c                   eval      w030a = *all'0'
073500070213     c                   movel     §rchdata      w030a
073600070213     c                   exsr      sr_testn
073700070213     c                   if        not *in02
073800070213     c  n01              eval      plgerr = 'E'
073900070213     c  n01              eval      plgmsg = 'DATA CHIUSURA NON VALIDA'
074000070213     c  n01              eval      *in01 = *on
074100110620     c  n01              eval      *in04 = *on
074200070213     c                   endif
074300070213      *  deve essere una data valida
074400070213     c                   if        *in02
074500070213     c                   clear                   wlbdat
074600070213     c                   move      §rchdata      g02inv
074700070213     c                   eval      g02err = '3'
074800070213     c                   call      'XSRDA8'
074900070213     c                   parm                    wlbdat
075000070213     c                   if        g02err = '1'
075100070213     c  n01              eval      plgerr = 'E'
075200070213     c  n01              eval      plgmsg = 'DATA CHIUSURA NON VALIDA'
075300070213     c  n01              eval      *in01 = *on
075400110620     c  n01              eval      *in04 = *on
075500070213     c                   endif
075600070213     c                   endif
075700070213
075800070213      * ora chiusura
075900070213      *  deve essere numerico
076000070213     c                   eval      w030a = *all'0'
076100070213     c                   movel     §rchora       w030a
076200070213     c                   exsr      sr_testn
076300070213     c                   if        not *in02
076400070213     c  n01              eval      plgerr = 'E'
076500070213     c  n01              eval      plgmsg = 'ORA CHIUSURA NON VALIDA'
076600070213     c  n01              eval      *in01 = *on
076700110620     c  n01              eval      *in04 = *on
076800070213     c                   endif
076900070213      *  deve essere un'ora valida
077000070213     c                   if        *in02 and
077100070213     c                             %subst(§rchora:1:2) > '23' or
077200070213     c                             %subst(§rchora:3:2) > '59' or
077300070213     c                             %subst(§rchora:5:2) > '59'
077400070213     c  n01              eval      plgerr = 'E'
077500070213     c  n01              eval      plgmsg = 'ORA CHIUSURA NON VALIDA'
077600070213     c  n01              eval      *in01 = *on
077700110620     c  n01              eval      *in04 = *on
077800070213     c                   endif
077900070514
078000110609      /free
078100110609       //?La data di chiusura distinta su rcd RCH non può essere inferiore a 30 gg da oggi
078200110609       //?in questo caso accetto il rcd ma imposto la data della distinta
078300110609       //?e come ora imposto l'ora di ricezione del record
078400110609       //?è capitato che PDA si è resettato ed ha riportato la data al 1980
078500120920         IF  not *in01 and §RCHdata < %editc(Oggi_10:'X');
078600110609           §RCHdata = %editc(DSTdfv:'X');
078700121015           %subst(§RCHora:1:4)  = %subst(PRCdatora:9:4);
078800121010           PLGmsg   = 'DATA < o > di 10 GG';
078900110609         ENDIF;
079000120920       //?E' capitato anche che il PDA è andato fuori di testa ed ha impostato
079100120920       //?la data a 2514
079200120920         IF  not *in01 and §RCHdata > %editc(Oggi_10o:'X');
079300120920           §RCHdata = %editc(DSTdfv:'X');
079400121015           %subst(§RCHora:1:4)  = %subst(PRCdatora:9:4);
079500121010           PLGmsg  = 'DATA < o > di 10 GG';
079600120920         ENDIF;
079700110609      /end-free
079800110609
079900070514      * imposto il tipo applicazione anche se non previsto dalla ds
080000070514     c                   eval      plgtipappl = 'R'
080100080513
080200080513      * controllo codice SPC
080300080721     c                   eval      *in05 = *off
080400080519     c                   if        §rchcodspc <> *blanks
080500080513     c                   exsr      sr_ctrcodspc
080600080513     c                   endif
080700070130
080800070130     c                   endsr
080900070130
081000070130      *------------------------------------------------------------------------*
081100070130      * ROUTINE ELABORAZIONE RCD "RES"
081200070130      *------------------------------------------------------------------------*
081300070130     c     sr_elabres    begsr
081400070130
081500070131     c                   eval      *in03 = *off
081600070130     c                   eval      fiprcresds = prcdati
081700070201
081800070201      * causale
081900070201      *  deve esistere
082000070201     c                   if        §rescmr <> *blanks
082100081106      * se causale 'AAA' ok...annullamento di ORM Manuale
082200081106     c                             and §rescmr <> 'AAA'
082300070201     c                   clear                   tibs02ds
082400070201     c                   eval      t02mod = 'C'
082500070201     c                   eval      t02sif = knsif
082600070201     c                   eval      t02cod = 'CMR'
082700070201     c                   eval      t02ke1 = §rescmr
082800070201     c                   call      'TIBS02R'
082900070201     c                   parm                    kpjba
083000070201     c                   parm                    tibs02ds
083100070201     c                   if        t02err <> *blanks
083200070201     c  n01              eval      plgerr = 'E'
083300070201     c  n01              eval      plgmsg = 'CAU. MANCATO RIT. INESISTENTE'
083400070201     c  n01              eval      *in01 = *on
083500110620     c  n01              eval      *in04 = *on
083600070201     c                   endif
083700070201     c                   endif
083800070130
083900070130      * p.o. gestione distinta
084000070201     c                   eval      wfgs = §resfgs
084100070201     c                   exsr      sr_ctrfgs
084200070201     c   02              move      §resfgs       plgfgs
084300070130
084400070130      * numero distinta
084500070201     c                   eval      wndc = §resndc
084600070201     c                   exsr      sr_ctrndc
084700070201     c   02              move      §resndc       plgndc
084800070201
084900070201      * controllo se esiste la distinta
085000070201     c   02              exsr      sr_ctrdist
085100070201
085200070201      * controllo se stesso AUT
085300070201     c                   if        *in02 and plgcodaut <> dstpdr
085400070201     c  n01              eval      plgerr = 'E'
085500070201     c  n01              eval      plgmsg = 'AUT. INCONGRUENTE'
085600070201     c  n01              eval      *in01 = *on
085700110620     c  n01              eval      *in04 = *on
085800070201     c                   endif
085900070201
086000070201      * numero ORM
086100070201      *  deve essere numerico
086200070201     c                   eval      w030a = *all'0'
086300070201     c                   eval      %subst(w030a:1:14) = %subst(prcdati:10:14)
086400070201     c                   exsr      sr_testn
086500070201     c                   if        not *in02
086600070201     c  n01              eval      plgerr = 'E'
086700070201     c  n01              eval      plgmsg = 'DATI ID DOC. ORM NON VALIDI'
086800070201     c  n01              eval      *in01 = *on
086900110620     c  n01              eval      *in04 = *on
087000070201     c                   else
087100070201      *  controllo se ORM manuale
087200070521     c                   if        %subst(prcdati:10:8) = *all'0'
087300070521     c                   eval      w006a = %subst(prcdati:18:6)
087400070521     c                   move      w006a         wora
087500070521     c     *eur          test(te)                wora
087600070521     c                   if        %error
087700070201     c  n01              eval      plgerr = 'E'
087800070201     c  n01              eval      plgmsg = 'DATI ID DOC. ORM NON VALIDI'
087900070201     c  n01              eval      *in01 = *on
088000110620     c  n01              eval      *in04 = *on
088100070201     c                   else
088200070201     c                   eval      *in03 = *on
088300080408     c                   eval      plgiddoc = %subst(prcdati:10:14)
088400070201     c                   endif
088500070202     c                   endif
088600070202     c                   endif
088700070130
088800070201      * controllo la chiave ORM
088900070201      * se non è un ORM manuale
089000070201     c                   if        not *in03
089100070202     c                   clear                   ormpoe
089200070202     c                   clear                   ormnsr
089300070202     c                   clear                   ormnor
089400070202     c                   clear                   ormnrv
089500070130      * p.o. emissione
089600070130      *  deve essere numerico
089700070201     c                   eval      w030a = *all'0'
089800070201     c                   movel     §respoe       w030a
089900070201     c                   exsr      sr_testn
090000070201     c                   if        not *in02
090100070130     c  n01              eval      plgerr = 'E'
090200070201     c  n01              eval      plgmsg = 'DATI P.O. EMISSIONE NON VALIDI'
090300070130     c  n01              eval      *in01 = *on
090400110620     c  n01              eval      *in04 = *on
090500070201     c                   else
090600070201     c                   move      §respoe       ormpoe
090700070130     c                   endif
090800070130
090900070130      * numero serie
091000070130      *  deve essere numerico
091100070201     c                   eval      w030a = *all'0'
091200070201     c                   movel     §resnsr       w030a
091300070201     c                   exsr      sr_testn
091400070201     c                   if        not *in02
091500070130     c  n01              eval      plgerr = 'E'
091600070201     c  n01              eval      plgmsg = 'DATI N. SERIE NON VALIDI'
091700070130     c  n01              eval      *in01 = *on
091800110620     c  n01              eval      *in04 = *on
091900070201     c                   else
092000070201     c                   move      §resnsr       ormnsr
092100070130     c                   endif
092200070130
092300070130      * numero orm
092400070130      *  deve essere numerico
092500070201     c                   eval      w030a = *all'0'
092600070201     c                   movel     §resnor       w030a
092700070201     c                   exsr      sr_testn
092800070201     c                   if        not *in02
092900070130     c  n01              eval      plgerr = 'E'
093000070201     c  n01              eval      plgmsg = 'DATI N. ORM NON VALIDI'
093100070130     c  n01              eval      *in01 = *on
093200110620     c  n01              eval      *in04 = *on
093300070201     c                   else
093400070201     c                   move      §resnor       ormnor
093500070130     c                   endif
093600070130      * numero viaggio
093700070130      *  deve essere numerico
093800070201     c                   eval      w030a = *all'0'
093900070201     c                   movel     §resnrv       w030a
094000070201     c                   exsr      sr_testn
094100070201     c                   if        not *in02
094200070130     c  n01              eval      plgerr = 'E'
094300070201     c  n01              eval      plgmsg = 'DATI N. VIAGGIO NON VALIDI'
094400070130     c  n01              eval      *in01 = *on
094500110620     c  n01              eval      *in04 = *on
094600070201     c                   else
094700070201     c                   move      §resnrv       ormnrv
094800070130     c                   endif
094900070131
095000070201      * aggancio l'ORM
095100070201     c     kfnorm        chain(n)  fnorm01l
095200070131     c                   if        %found(fnorm01l)
095300070201      * verifico se la distinta presente sull'ORM è la stessa del PDA
095400070201     c                   if        ormndc <> plgndc
095500070131     c  n01              eval      plgerr = 'E'
095600070131     c  n01              eval      plgmsg = 'DISTINTA INCONGRUENTE'
095700070131     c  n01              eval      *in01 = *on
095800070201     c                   endif
095900070201      * verifico la fase dell'ORM
096000070201     c                   if        ormfao < 400 or ormfao >= 500
096100070131     c  n01              eval      plgerr = 'E'
096200070131     c  n01              eval      plgmsg = 'ESITO/FASE INCONGRUENTI'
096300070131     c  n01              eval      *in01 = *on
096400070201     c                   endif
096500070131     c                   endif
096600070207     c  n01              eval      %subst(plgiddoc:1:3) = §respoe
096700070207     c  n01              eval      %subst(plgiddoc:4:2) = §resnsr
096800070207     c  n01              eval      %subst(plgiddoc:6:7) = §resnor
096900070207     c  n01              eval      %subst(plgiddoc:13:2) = §resnrv
097000070201     c                   endif
097100070213
097200070213      * data conferma
097300070213      *  deve essere numerico
097400070213     c                   eval      w030a = *all'0'
097500070213     c                   movel     §resdata      w030a
097600070213     c                   exsr      sr_testn
097700070213     c                   if        not *in02
097800070213     c  n01              eval      plgerr = 'E'
097900070213     c  n01              eval      plgmsg = 'DATA CONFERMA NON VALIDA'
098000070213     c  n01              eval      *in01 = *on
098100110620     c  n01              eval      *in04 = *on
098200070213     c                   endif
098300070213      *  deve essere una data valida
098400070213     c                   if        *in02
098500070213     c                   clear                   wlbdat
098600070213     c                   move      §resdata      g02inv
098700070213     c                   eval      g02err = '3'
098800070213     c                   call      'XSRDA8'
098900070213     c                   parm                    wlbdat
099000070213     c                   if        g02err = '1'
099100070213     c  n01              eval      plgerr = 'E'
099200070213     c  n01              eval      plgmsg = 'DATA CONFERMA NON VALIDA'
099300070213     c  n01              eval      *in01 = *on
099400110620     c  n01              eval      *in04 = *on
099500070213     c                   endif
099600070213     c                   endif
099700070213
099800070213      * ora conferma
099900070213      *  deve essere numerico
100000070213     c                   eval      w030a = *all'0'
100100070213     c                   movel     §resora       w030a
100200070213     c                   exsr      sr_testn
100300070213     c                   if        not *in02
100400070213     c  n01              eval      plgerr = 'E'
100500070213     c  n01              eval      plgmsg = 'ORA CONFERMA NON VALIDA'
100600070213     c  n01              eval      *in01 = *on
100700110620     c  n01              eval      *in04 = *on
100800070213     c                   endif
100900070213      *  deve essere un'ora valida
101000070213     c                   if        *in02 and
101100070213     c                             %subst(§resora:1:2) > '23' or
101200070213     c                             %subst(§resora:3:2) > '59' or
101300070213     c                             %subst(§resora:5:2) > '59'
101400070213     c  n01              eval      plgerr = 'E'
101500070213     c  n01              eval      plgmsg = 'ORA CONFERMA NON VALIDA'
101600070213     c  n01              eval      *in01 = *on
101700110620     c  n01              eval      *in04 = *on
101800070213     c                   endif
101900110609
102000110609      /free
102100110609       //?La data di immissione esito su rcd RES non può essere inferiore a 30 gg da oggi
102200110609       //?in questo caso accetto il rcd ma imposto la data della distinta
102300110609       //?e come ora imposto l'ora di ricezione del record
102400110609       //?è capitato che PDA si è resettato ed ha riportato la data al 1980
102500120920         IF  not *in01 and §RESdata < %editc(Oggi_10:'X');
102600110609           §RESdata = %editc(DSTdfv:'X');
102700121015           %subst(§RESora:1:4)  = %subst(PRCdatora:9:4);
102800121010           PLGmsg   = 'DATA < o > di 10 GG';
102900110609         ENDIF;
103000120920       //?E' capitato anche che il PDA è andato fuori di testa ed ha impostato
103100120920       //?la data a 2514
103200120920         IF  not *in01 and §RESdata > %editc(Oggi_10o:'X');
103300120920           §RESdata = %editc(DSTdfv:'X');
103400121015           %subst(§RESora:1:4)  = %subst(PRCdatora:9:4);
103500121010           PLGmsg  = 'DATA < o > di 10 GG';
103600120920         ENDIF;
103700110609      /end-free
103800070130
103900070130      * numero colli
104000070130      *  deve essere numerico
104100070201     c                   eval      w030a = *all'0'
104200070201     c                   movel     §resncl       w030a
104300070201     c                   exsr      sr_testn
104400070201     c                   if        not *in02
104500070130     c  n01              eval      plgerr = 'E'
104600070201     c  n01              eval      plgmsg = 'DATI N. COLLI NON VALIDI'
104700070130     c  n01              eval      *in01 = *on
104800110620     c  n01              eval      *in04 = *on
104900070130     c                   endif
105000070130
105100070130      * numero bancali
105200070130      *  deve essere numerico
105300070201     c                   eval      w030a = *all'0'
105400070201     c                   movel     §resbnc       w030a
105500070201     c                   exsr      sr_testn
105600070201     c                   if        not *in02
105700070130     c  n01              eval      plgerr = 'E'
105800070201     c  n01              eval      plgmsg = 'DATI N. BANCALI NON VALIDI'
105900070130     c  n01              eval      *in01 = *on
106000110620     c  n01              eval      *in04 = *on
106100070130     c                   endif
106200070213
106300070213      * data inserimento record
106400070213      *  deve essere numerico
106500070213     c                   eval      w030a = *all'0'
106600070213     c                   movel     §resdatain    w030a
106700070213     c                   exsr      sr_testn
106800070213     c                   if        not *in02
106900070213     c  n01              eval      plgerr = 'E'
107000070213     c  n01              eval      plgmsg = 'DATA INSERIMENTO NON VALIDA'
107100070213     c  n01              eval      *in01 = *on
107200110620     c  n01              eval      *in04 = *on
107300070213     c                   endif
107400070213      *  deve essere una data valida
107500070213     c                   if        *in02
107600070213     c                   clear                   wlbdat
107700070213     c                   move      §resdatain    g02inv
107800070213     c                   eval      g02err = '3'
107900070213     c                   call      'XSRDA8'
108000070213     c                   parm                    wlbdat
108100070213     c                   if        g02err = '1'
108200070213     c  n01              eval      plgerr = 'E'
108300070213     c  n01              eval      plgmsg = 'DATA INSERIMENTO NON VALIDA'
108400070213     c  n01              eval      *in01 = *on
108500110620     c  n01              eval      *in04 = *on
108600070213     c                   endif
108700070213     c                   endif
108800070213
108900070213      * ora conferma
109000070213      *  deve essere numerico
109100070213     c                   eval      w030a = *all'0'
109200070213     c                   movel     §resorain     w030a
109300070213     c                   exsr      sr_testn
109400070213     c                   if        not *in02
109500070213     c  n01              eval      plgerr = 'E'
109600070213     c  n01              eval      plgmsg = 'ORA INSERIMENTO NON VALIDA'
109700070213     c  n01              eval      *in01 = *on
109800110620     c  n01              eval      *in04 = *on
109900070213     c                   endif
110000070213      *  deve essere un'ora valida
110100070213     c                   if        *in02 and
110200070213     c                             %subst(§resorain:1:2) > '23' or
110300070213     c                             %subst(§resorain:3:2) > '59' or
110400070213     c                             %subst(§resorain:5:2) > '59'
110500070213     c  n01              eval      plgerr = 'E'
110600070213     c  n01              eval      plgmsg = 'ORA INSERIMENTO NON VALIDA'
110700070213     c  n01              eval      *in01 = *on
110800110620     c  n01              eval      *in04 = *on
110900070213     c                   endif
111000110609
111100110609      /free
111200110609       //?La data di inserimento esito su rcd RES non può essere inferiore a 30 gg da oggi
111300110609       //?in questo caso accetto il rcd ma imposto la data della distinta
111400110609       //?e come ora imposto l'ora di ricezione del record
111500110609       //?è capitato che PDA si è resettato ed ha riportato la data al 1980
111600120920         IF  not *in01 and §RESdatain < %editc(Oggi_10:'X');
111700110609           §RESdatain = %editc(DSTdfv:'X');
111800121015           %subst(§RESorain:1:4)  = %subst(PRCdatora:9:4);
111900121010           PLGmsg     = 'DATA < o > di 10 GG';
112000110609         ENDIF;
112100120920       //?E' capitato anche che il PDA è andato fuori di testa ed ha impostato
112200120920       //?la data a 2514
112300120920         IF  not *in01 and §RESdatain > %editc(Oggi_10o:'X');
112400120920           §RESdatain = %editc(DSTdfv:'X');
112500121015           %subst(§RESorain:1:4)  = %subst(PRCdatora:9:4);
112600121010           PLGmsg     = 'DATA < o > di 10 GG';
112700120920         ENDIF;
112800110609      /end-free
112900070130
113000070130      * inserito da SPC
113100070130      *  deve essere 'S' o blank
113200070130     c                   if        §resflgspc <> 'S' and
113300070130     c                             §resflgspc <> *blanks
113400070130     c  n01              eval      plgerr = 'E'
113500070201     c  n01              eval      plgmsg = 'FLAG INS. DA SPC NON VALIDO'
113600070130     c  n01              eval      *in01 = *on
113700110620     c  n01              eval      *in04 = *on
113800070130     c                   endif
113900070514
114000070514      * imposto il tipo applicazione anche se non previsto dalla ds
114100070514     c                   eval      plgtipappl = 'R'
114200070130
114300070130     c                   endsr
114400070130
114500140129      *------------------------------------------------------------------------*
114600140129      * ROUTINE ELABORAZIONE RCD "ROR"
114700140129      *------------------------------------------------------------------------*
114800140129     c     sr_elabror    begsr
114900140129
115000140129     c                   eval      fiprcrords = prcdati
115100140129
115200140129      * p.o. gestione distinta
115300140129     c                   eval      wfgs = §rorfgs
115400140129     c                   exsr      sr_ctrfgs
115500140129     c   02              move      §rorfgs       plgfgs
115600140129
115700140129      * numero distinta
115800140129     c                   eval      wndc = §rorndc
115900140129     c                   exsr      sr_ctrndc
116000140207     c   02              move      §rorndc       plgndc
116100140129
116200140129      * controllo se esiste la distinta
116300140129     c   02              exsr      sr_ctrdist
116400140129
116500140129      * tipo applicazione
116600140129      * può essere C=consegne R=ritiri
116700140207     c                   if        §rortpiddo <> 'C' and §rortpiddo <> 'R'
116800140129     c  n01              eval      plgerr = 'E'
116900140129     c  n01              eval      plgmsg = 'DATI NON VALIDI'
117000140129     c  n01              eval      *in01 = *on
117100140129     c  n01              eval      *in04 = *on
117200140129     c                   else
117300140207     c                   eval      plgtipappl = §rortpiddo
117400140129     c                   endif
117500140129
117600140129      * id documento
117700140129      *  solo ritiro
117800140207     c                   if        §rortpiddo = 'R'
117900140129      *  deve essere numerico
118000140129     c                   eval      w030a = *all'0'
118100140129     c                   eval      %subst(w030a:1:14) = %subst(§roriddoc:1:14)
118200140129     c                   exsr      sr_testn
118300140129     c                   if        not *in02
118400140129     c  n01              eval      plgerr = 'E'
118500140129     c  n01              eval      plgmsg = 'DATI ID DOC. ORM NON VALIDI'
118600140129     c  n01              eval      *in01 = *on
118700140129     c  n01              eval      *in04 = *on
118800140129     c                   else
118900140129      *  deve essere un n. di ORM valido
119000140129     c                   move      §rorpoe       ormpoe
119100140129     c                   move      §rornsr       ormnsr
119200140129     c                   move      §rornor       ormnor
119300140129     c                   move      §rornrv       ormnrv
119400140129     c     kfnorm        chain(n)  fnorm01l
119500140129     c                   if        not %found(fnorm01l)
119600140129     c  n01              eval      plgerr = 'E'
119700140129     c  n01              eval      plgmsg = 'ORM INESISTENTE'
119800140129     c  n01              eval      *in01 = *on
119900140129     c  n01              eval      *in04 = *on
120000140129     c                   endif
120100140129     c                   endif
120200140129     c  n01              eval      plgiddoc = §roriddoc
120300140129     c                   endif
120400140129
120500140129      * data conferma
120600140129      *  deve essere numerico
120700140129     c                   eval      w030a = *all'0'
120800140129     c                   movel     §rordata      w030a
120900140129     c                   exsr      sr_testn
121000140129     c                   if        not *in02
121100140129     c  n01              eval      plgerr = 'E'
121200140129     c  n01              eval      plgmsg = 'DATA CONFERMA NON VALIDA'
121300140129     c  n01              eval      *in01 = *on
121400140129     c  n01              eval      *in04 = *on
121500140129     c                   endif
121600140129      *  deve essere una data valida
121700140129     c                   if        *in02
121800140129     c                   clear                   wlbdat
121900140129     c                   move      §rordata      g02inv
122000140129     c                   eval      g02err = '3'
122100140129     c                   call      'XSRDA8'
122200140129     c                   parm                    wlbdat
122300140129     c                   if        g02err = '1'
122400140129     c  n01              eval      plgerr = 'E'
122500140129     c  n01              eval      plgmsg = 'DATA CONFERMA NON VALIDA'
122600140129     c  n01              eval      *in01 = *on
122700140129     c  n01              eval      *in04 = *on
122800140129     c                   endif
122900140129     c                   endif
123000140129
123100140129      * ora conferma
123200140129      *  deve essere numerico
123300140129     c                   eval      w030a = *all'0'
123400140129     c                   movel     §rorora       w030a
123500140129     c                   exsr      sr_testn
123600140129     c                   if        not *in02
123700140129     c  n01              eval      plgerr = 'E'
123800140129     c  n01              eval      plgmsg = 'ORA CONFERMA NON VALIDA'
123900140129     c  n01              eval      *in01 = *on
124000140129     c  n01              eval      *in04 = *on
124100140129     c                   endif
124200140129      *  deve essere un'ora valida
124300140129     c                   if        *in02 and
124400140129     c                             %subst(§rorora:1:2) > '23' or
124500140129     c                             %subst(§rorora:3:2) > '59' or
124600140129     c                             %subst(§rorora:5:2) > '59'
124700140129     c  n01              eval      plgerr = 'E'
124800140129     c  n01              eval      plgmsg = 'ORA CONFERMA NON VALIDA'
124900140129     c  n01              eval      *in01 = *on
125000140129     c  n01              eval      *in04 = *on
125100140129     c                   endif
125200140129      /free
125300140129       //?La data di conferma ORM su rcd OK non può essere inferiore a 30 gg da oggi
125400140129       //?in questo caso accetto il rcd ma imposto la data della distinta
125500140129       //?e come ora imposto l'ora di ricezione del record
125600140129       //?è capitato che PDA si è resettato ed ha riportato la data al 1980
125700140129         IF  not *in01 and §rordata < %editc(Oggi_10:'X');
125800140207           §rordata = %editc(DSTdfv:'X');
125900140129           §rorora  = %subst(PRCdatora:9:6);
126000140129           PLGmsg  = 'DATA < o > di 10 GG';
126100140129         ENDIF;
126200140129       //?E' capitato anche che il PDA è andato fuori di testa ed ha impostato
126300140129       //?la data a 2514
126400140129         IF  not *in01 and §rordata > %editc(Oggi_10o:'X');
126500140129           §rordata = %editc(DSTdfv:'X');
126600140129           %subst(§rorora:1:4)  = %subst(PRCdatora:9:4);
126700140129           PLGmsg  = 'DATA < o > di 10 GG';
126800140129         ENDIF;
126900140129      /end-free
127000140129
127100140129      * ora stimata
127200140129     c                   clear                   Worasti
127300140129     c                   eval      w030a = *all'0'
127400140129     c                   movel     §rororasti    w030a
127500140129     c                   exsr      sr_testn
127600140129     c                   if        not *in02
127700140129     c  n01              eval      plgerr = 'E'
127800140129     c  n01              eval      plgmsg = 'ORA STIMATA NON VALIDA'
127900140129     c  n01              eval      *in01 = *on
128000140129     c  n01              eval      *in04 = *on
128100140129      *
128200140129     c                   if        %subst(§rorora:1:2) > '23' or
128300140129     c                             %subst(§rorora:3:2) > '59' or
128400140129     c                             %subst(§rorora:5:2) > '59'
128500140129     c  n01              eval      plgerr = 'E'
128600140129     c  n01              eval      plgmsg = 'ORA STIMATA NON VALIDA'
128700140129     c  n01              eval      *in01 = *on
128800140129     c  n01              eval      *in04 = *on
128900140129     c                   endif
129000140129
129100140129     c                   else
129200140129     c                   move      §rororasti    Worasti
129300140129     c                   endif
129400140129
129500140129      * ordinamento
129600140129     c                   clear                   Wsort
129700140129     c                   eval      w030a = *all'0'
129800140129     c                   movel     §rorsort      w030a
129900140129     c                   exsr      sr_testn
130000140129     c                   if        not *in02
130100140129     c  n01              eval      plgerr = 'E'
130200140129     c  n01              eval      plgmsg = 'ORDINAMENTO NON VALIDO'
130300140129     c  n01              eval      *in01 = *on
130400140129     c  n01              eval      *in04 = *on
130500140129
130600140129     c                   else
130700140129     c                   move      §rorsort      Wsort
130800140129     c                   endif
130900140129
131000140129     c                   endsr
131100140129
131200070130      *------------------------------------------------------------------------*
131300070130      * ROUTINE PER AGGIORNARE I DATI EFFETTIVI
131400070130      *------------------------------------------------------------------------*
131500070130     c     sr_file       begsr
131600070130
131700070130     c                   select
131800070130      * tipo record 'RCH'
131900070130     c                   when      plgtiporcd = 'RCH'
132000080327      * aggiorno fiqdt
132100070201     c                   exsr      sr_fiqdt
132200080327      * devo cancellare i dati da FIPDO e da FIPRO
132300070201     c                   exsr      sr_fipdo
132400080319     c                   exsr      sr_fipro
132500070130
132600070130      * tipo record 'RES'
132700070131     c                   when      plgtiporcd = 'RES' and
132800070131     c                             not *in03
132900070201     c                   exsr      sr_fnorm
133000070130
133100070130     c                   endsl
133200070130
133300070130     c                   endsr
133400070202
133500070202      *------------------------------------------------------------------------*
133600070202      * ROUTINE SCRITTURA FILE LOG
133700070202      *------------------------------------------------------------------------*
133800070202     c     sr_wrtlog     begsr
133900070202
134000070202     c                   eval      plgprfc = prcprfc
134100070202     c                   eval      plgiddisp = prciddisp
134200070202     c                   eval      plgdatora = prcdatora
134300070202     c                   eval      plgdati = prcdati
134400070205     c                   write     fiplg000
134500070202
134600070202     c                   endsr
134700070201
134800080415      *------------------------------------------------------------------------*
134900080415      * ROUTINE SCRITTURA FILE PER PROSPETTO ORM
135000080415      *------------------------------------------------------------------------*
135100080415     c     sr_Prospetto  begsr
135200080415
135300080415      * se ricevo un RES
135400080417    1c                   If        plgtiporcd = 'RES'
135500080415      * cerco l'ORM da scrivere o aggiornare nel file dettaglio prospetto ORM
135600080415     c     kfirpd        chain     Firpd02l
135700080415
135800080417    2c                   if        not %found(firpd02l)
135900080415     c                   clear                   firpd000
136000080415     c                   eval      rpdfgs = plgfgs
136100080415     c                   eval      rpdnfv = plgndc
136200080415     c                   eval      rpdpdr = plgcodaut
136300080415     c                   eval      rpdiddoc = plgiddoc
136400080415      * è un manuale ???
136500080417    3c                   if        %subst(plgiddoc:1:8) = *all'0'
136600080417     c                   eval      rpdman = 'S'
136700080415     c                   eval      rpdrsr = §resrsr
136800080415     c                   eval      rpdinr = §resinr
136900080417    3c                   endif
137000080417     c                   move      §resncl       rpdncl
137100080417     c                   move      §resbnc       rpdbnc
137200080415     c                   eval      rpdcmr = §rescmr
137300080417     c                   movel     §resdata      rpddtr
137400080417     c                   move      §resora       rpdorr
137500080415     c                   eval      rpdflgspc = §resflgspc
137600080417     c                   movel     §resdtorin    rpddtorin
137700081106      * non scrivo record se orm manuale annullato
137800081106     c                   if        rpdman <> 'S' or
137900081106     c                             (rpdman = 'S' and §rescmr <> 'AAA')
138000080417     c                   write     firpd000
138100140611
138200140611      /free
138300140611       //?Scrivo, se non è già stata scritta prima, la testata FIRPT
138400140611         chain(n) (RPDfgs:RPDnfv:RPDpdr) FIRPT01L;
138500140611         IF  not %found(FIRPT01L);
138600140611           clear FIRPT000;
138700140611           RPTfgs = PLGfgs;
138800140611           RPTnfv = PLGndc;
138900140611           RPTpdr = PLGcodaut;
139000140611           RPTdfv = DSTdfv;
139100140611           RPTfpp = DSTfpp;
139200140611           RPTtpm = DSTtpm;
139300140611           write FIRPT000;
139400140611         ENDIF;
139500140611      /end-free
139600140611
139700081106     c                   endif
139800080417    2c                   endif
139900080417      * se trovato record se data ora modifica maggiore di quella registrata aggiorno
140000080417    2c                   if        %found(firpd02l) and
140100080417     c                             rpddtorin < §resdtorin
140200081106      * ma solo se non è un orm manuale annullato
140300081106     c                             and (rpdman <> 'S' or
140400081106     c                                 (rpdman = 'S' and §rescmr <> 'AAA'))
140500080417     c                   move      §resncl       rpdncl
140600080417     c                   move      §resbnc       rpdbnc
140700080417     c                   eval      rpdcmr = §rescmr
140800080505     c                   movel     §resdtorin    rpddtorin
140900080505     c                   eval      rpdflgspc = §resflgspc
141000080417     c                   update    firpd000
141100080417    2c                   endif
141200081106
141300081106      * se trovato record e orm manuale annullato cancello il record
141400081106    2c                   if        %found(firpd02l) and
141500081106     c                             rpdman = 'S' and §rescmr = 'AAA'
141600081106     c                   delete    firpd000
141700081106    2c                   endif
141800080417
141900080417    1c                   endif
142000080417
142100080417      * se ricevo un RCH
142200080417    1c                   If        plgtiporcd = 'RCH'
142300080417      * cerco se la distinta è presente
142400080417     c     kfirpt        chain     Firpt01l
142500080417
142600080417    2c                   if        not %found(firpt01l)
142700080417     c                   clear                   firpt000
142800080417     c                   eval      rptfgs = plgfgs
142900080417     c                   eval      rptnfv = plgndc
143000080417     c                   eval      rptpdr = plgcodaut
143100080421     c                   eval      rptdfv = dstdfv
143200080418     c                   eval      rptfpp = dstfpp
143300080418     c                   eval      rpttpm = dsttpm
143400080417     c                   move      §rchncl       rpttco
143500080417     c                   move      §rchbnc       rpttbn
143600080417     c                   movel     §rchdata      rptdch
143700080417     c                   move      §rchora       rptoch
143800080417     c                   write     firpt000
143900080417    2c                   endif
144000080417      * se trovato record se data ora modifica maggiore di quella registrata aggiorno
144100080417    2c                   if        %found(firpt01l)
144200080417     c                   movel     rptdch        w_dtaorach
144300080417     c                   move      rptoch        w_dtaorach
144400080417    3c                   If        w_dtaorach < §rchdatora
144500080417     c                   move      §rchncl       rpttco
144600080417     c                   move      §rchbnc       rpttbn
144700080417     c                   movel     §rchdata      rptdch
144800080417     c                   move      §rchora       rptoch
144900080417     c                   update    firpt000
145000080417    3c                   endif
145100080417    2c                   endif
145200080417
145300080417    1c                   endif
145400080415
145500080415     c                   endsr
145600080319      *------------------------------------------------------------------------*
145700080319      * ROUTINE SCRITTURA FILE OK RICEVUTI
145800080319      *------------------------------------------------------------------------*
145900080319     c     sr_wrtpro     begsr
146000080319
146100080730      * verifico se estiste già un ok per questo ORM + DISTINTA + AUT
146200080319
146300140128     c     kfipro        chain     fipro11l
146400080319
146500080319      * se il record esiste e l'ok che ho ricevuto ha un idrow minore o uguale
146600140207     c                   if        %found(fipro11l)
146700140207     c                   movel     prodati       fiprookds
146800140207     c                   endif
146900080319      * a quello del file vado a fine
147000140207     c                   if        %found(fipro11l) and widrow <= §proidrow
147100080319     c                   leavesr
147200080319     c                   endif
147300080319
147400140128     c                   eval      prodatora = §OKDATORA
147500080319      * valorizzo i campi dell'ok
147600140128     c                   eval      §proidrow = widrow
147700140128     c                   eval      §proflgass = §OKFLGASS
147800140224     c                   eval      §PROFLGVIS = ' '
147900140128     c                   eval      §pronotass = §OKnotass
148000140128     c                   eval      Prodati    = fiprookds
148100080319
148200140207     c                   if        not %found(fipro11l)
148300080319     c                   eval      profgs = plgfgs
148400080319     c                   eval      prondc = plgndc
148500080730     c                   eval      procodaut = plgcodaut
148600080319     c                   clear                   proatb
148700080319     c                   eval      propoe = ormpoe
148800080319     c                   eval      pronsr = ormnsr
148900080319     c                   eval      pronor = ormnor
149000080319     c                   eval      pronrv = ormnrv
149100140128     c                   eval      protrd = 'OK '
149200140128     c                   write     fipro100
149300080319     c                   else
149400140128     c                   update    fipro100
149500080319     c                   endif
149600080319
149700080319     c                   endsr
149800080319
149900140129      *------------------------------------------------------------------------*
150000140129      * ROUTINE SCRITTURA FILE ROR RICEVUTI
150100140129      *------------------------------------------------------------------------*
150200140129     c     sr_wrtproror  begsr
150300140129
150400140129      * verifico se estiste già un ok per questo ORM + DISTINTA + AUT
150500140129
150600140129     c     kfipro        chain     fipro11l
150700140129
150800140129     c                   eval      prodatora = §rorDATORA
150900140207      * valorizzo i campi del ror
151000140207     c                   move      §rorORASTI    §PROORASTI
151100140207     c                   move      §rorSORT      §PROSORT
151200140129     c                   eval      Prodati    = fiprorords
151300140129
151400140129     c                   if        not %found(fipro11l)
151500140129     c                   eval      profgs = plgfgs
151600140129     c                   eval      prondc = plgndc
151700140129     c                   eval      procodaut = plgcodaut
151800140129     c                   clear                   proatb
151900140129     c                   eval      propoe = ormpoe
152000140129     c                   eval      pronsr = ormnsr
152100140129     c                   eval      pronor = ormnor
152200140129     c                   eval      pronrv = ormnrv
152300140129     c                   eval      protrd = 'ROR'
152400140129     c                   write     fipro100
152500140129     c                   else
152600140129     c                   update    fipro100
152700140129     c                   endif
152800140129
152900140129     c                   endsr
153000140129
153100070201      *------------------------------------------------------------------------*
153200070201      * ROUTINE PER CONTROLLARE IL P.O. GESTIONE DISTINTA
153300070201      *------------------------------------------------------------------------*
153400070201     c     sr_ctrfgs     begsr
153500070201
153600070201     c                   eval      w030a = *all'0'
153700070201     c                   movel     wfgs          w030a
153800070201     c                   exsr      sr_testn
153900070201     c                   if        not *in02
154000070201     c  n01              eval      plgerr = 'E'
154100070201     c  n01              eval      plgmsg = 'DATI P.O. DISTINTA NON VALIDI'
154200070201     c  n01              eval      *in01 = *on
154300110620     c  n01              eval      *in04 = *on
154400070201     c                   endif
154500070201
154600070201     c                   endsr
154700070201
154800070201      *------------------------------------------------------------------------*
154900070201      * ROUTINE PER CONTROLLARE IL NUMERO DISTINTA
155000070201      *------------------------------------------------------------------------*
155100070201     c     sr_ctrndc     begsr
155200070201
155300070201     c                   eval      w030a = *all'0'
155400070201     c                   movel     wndc          w030a
155500070201     c                   exsr      sr_testn
155600070201     c                   if        not *in02
155700070201     c  n01              eval      plgerr = 'E'
155800070201     c  n01              eval      plgmsg = 'DATI NUM. DISTINTA NON VALIDI'
155900070201     c  n01              eval      *in01 = *on
156000110620     c  n01              eval      *in04 = *on
156100070201     c                   endif
156200070201
156300070201     c                   endsr
156400070201
156500070201      *------------------------------------------------------------------------*
156600070201      * ROUTINE PER CONTROLLARE L'EISISTENZA DELLA DISTINTA
156700070201      *------------------------------------------------------------------------*
156800070201     c     sr_ctrdist    begsr
156900070201
157000080226     c                   eval      dstnpg = 4
157100070201     c                   move      wndc          dstnfv
157200070201     c                   move      wfgs          dstfgs
157300080317     c                   clear                   ddstflr
157400080226     c     kfidst        chain     fidst01l
157500080226     c                   if        not %found(fidst01l) or dstatb <> *blanks
157600070201     c  n01              eval      plgerr = 'E'
157700070201     c  n01              eval      plgmsg = 'DISTINA INESISTENTE'
157800070201     c  n01              eval      *in01 = *on
157900110620     c  n01              eval      *in04 = *on
158000080317     c                   leavesr
158100070201     c                   endif
158200080317     c                   eval      ddstflr = dstflr
158300170703       wdatadistinta = DSTdfv;
158400070201
158500070201     c                   endsr
158600080513
158700080513      *------------------------------------------------------------------------*
158800080513      * ROUTINE PER CONTROLLARE CODICE SPC
158900080513      *------------------------------------------------------------------------*
159000080513     c     sr_ctrcodspc  begsr
159100080513
159200080519     c                   eval      *in05 = *off
159300080924     c                   eval      xx = 1
159400080924     c     §rchcodspc    lookup    codspc(xx)                             30
159500080514     c                   if        not *in30
159600080519     c                   eval      *in05 = *on
159700080924     c                   else
159800080924     c                   eval      §rchspccod = codspc(xx)
159900080924     c                   eval      §rchspcdes = desspc(xx)
160000080514     c                   endif
160100080513
160200080513     c                   endsr
160300070201
160400070201      *------------------------------------------------------------------------*
160500070201      * ROUTINE PER CONTROLLARE SE IL CAMPO E' NUMERICO
160600070201      *------------------------------------------------------------------------*
160700070201     c     sr_testn      begsr
160800070201
160900070201     c                   eval      *in02 = *off
161000070201     c                   testn                   w030a                02
161100070201
161200070201     c                   endsr
161300070201
161400070201      *------------------------------------------------------------------------*
161500070201      * ROUTINE PER AGGIORNARE FIQDT
161600070201      *------------------------------------------------------------------------*
161700070201     c     sr_fiqdt      begsr
161800070201
161900070201      * imposto la data e ora attuale
162000070201     c                   time                    w0140
162100070201     c                   move      w0140         udate8
162200070201     c                   movel     w0140         hhmm
162300070201     c                   z-add     udate8        g02dat
162400070201     c                   move      *blank        g02err
162500070201     c                   call      'XSRDA8'
162600070201     c                   parm                    wlbdat
162700070201     c                   z-add     g02inv        dateu8
162800070201
162900070201      * aggancio FIQDT
163000170703     c***  kfiqdt        chain(e)  fiqdt01l
163100170703       chain(e) (PLGfgs:PLGndc:wdatadistinta) FIQDT01L;
163200070205      * se rcd allocato flag di comodo per poi fare il rollback
163300070202     c                   if        %error
163400070202     c                   eval      wfiqdt = *on
163500070202     c                   leavesr
163600070202     c                   endif
163700070201     c                   if        %found(fiqdt01l)
163800070201      * bancali ritirati
163900081106      * se distinta a PDA non in test sommo i bancali rientrati a quando già presente su
164000081106      * fiqdt
164100110316     c**!!!              if        dstpda = 'E' and §dsttstpda = *blanks
164200110316     c**!!!              add       wbnc          qdtnbnri
164300110316     c**!!!              eval      qdtorari = hhmm
164400110316     c**!!!              eval      qdtdairi = dateu8
164500110316     c**!!!              eval      qdtfnbri = 'S'
164600110316     c**!!!              endif
164700080317     c****               if        wbnc <> qdtnbnri
164800080317     c****               eval      qdtnbnri = wbnc
164900080317     c****               eval      qdtorari = hhmm
165000080317     c****               eval      qdtdairi = dateu8
165100080317     c****               endif
165200070201      * colli ritirati
165300070201     c                   if        wncl <> qdtnclrit
165400070201     c                   eval      qdtnclrit = wncl
165500081112     c                   eval      qdtfncrit = 'S'
165600070201     c                   endif
165700070201
165800070205     c                   update    fiqdt000
165900070201     c                   endif
166000070201
166100070201     c                   endsr
166200080327
166300080327      *------------------------------------------------------------------------*
166400080327      * ROUTINE PER SCRIVERE LA FASE RCH SULLA DISTINTA
166500080327      *------------------------------------------------------------------------*
166600080327     c     sr_fase       begsr
166700080327
166800080327     c                   clear                   fior90ds
166900080327     c                   eval      or90tla = 'W'
167000080327     c                   eval      or90fas = 'RCH'
167100080327     c                   eval      or90fgs = plgfgs
167200080327     c                   eval      or90ndc = plgndc
167300080327     c                   eval      or90flgins = 'P'
167400080327     c                   eval      or90comit = '1'
167500080924     c                   eval      kpjbu = fidsfrchds
167600080327     c                   call      'FIOR90R'
167700080327     c                   parm                    kpjba
167800080327     c                   parm                    fior90ds
167900080327      * se torna errore imposto flag per poi fare il rollback
168000080327     c                   if        or90err <> *blanks
168100080327     c                   eval      wfase = *on
168200080327     c                   leavesr
168300080327     c                   endif
168400080327
168500080327     c                   endsr
168600070201
168700070201      *------------------------------------------------------------------------*
168800080319      * ROUTINE PER CANCELLARE RCD DA FIPDO
168900070201      *------------------------------------------------------------------------*
169000070201     c     sr_fipdo      begsr
169100080319
169200080319      * Cancellazione PDO
169300070201     c     kfiqdt        setll     fipdo01t
169400070201     c                   do        *hival
169500070205     c     kfiqdt        reade(e)  fipdo01t
169600070205      * se rcd allocato flag di comodo per poi fare il rollback
169700070205     c                   if        %error
169800070205     c                   eval      wfipdo = *on
169900070205     c                   leave
170000070205     c                   endif
170100070201     c                   if        %eof(fipdo01t)
170200070201     c                   leave
170300070201     c                   endif
170400070205     c                   delete    fipdo000
170500070201     c                   enddo
170600080319
170700070201     c                   endsr
170800070201
170900080319      *------------------------------------------------------------------------*
171000080319      * ROUTINE PER CANCELLARE RCD DA FIPRO
171100080319      *------------------------------------------------------------------------*
171200080319     c     sr_fipro      begsr
171300080319
171400080319      * Cancellazione PRO
171500140207     c     kfiqdt        setll     fipro11l
171600080319     c                   do        *hival
171700140207     c     kfiqdt        reade(e)  fipro11l
171800080319      * se rcd allocato flag di comodo per poi fare il rollback
171900080319     c                   if        %error
172000080319     c                   eval      wfipro = *on
172100080319     c                   leave
172200080319     c                   endif
172300140207     c                   if        %eof(fipro11l)
172400080319     c                   leave
172500080319     c                   endif
172600140207     c                   delete    fipro100
172700080319     c                   enddo
172800080319
172900080319     c                   endsr
173000080319
173100070201      *------------------------------------------------------------------------*
173200070201      * ROUTINE PER AGGIORNARE FNORM/FNORF
173300070201      *------------------------------------------------------------------------*
173400070201     c     sr_fnorm      begsr
173500070201
173600070201      * imposto la data e ora attuale
173700070201     c                   time                    w0140
173800070201     c                   move      w0140         udate8
173900070201     c                   z-add     udate8        g02dat
174000070201     c                   move      *blank        g02err
174100070201     c                   call      'XSRDA8'
174200070201     c                   parm                    wlbdat
174300070201     c                   z-add     g02inv        dateu8
174400070201
174500070201      * aggancio FNORM
174600070202     c     kfnorm        chain(e)  fnorm01l
174700070205      * se rcd allocato flag di comodo per poi fare il rollback
174800070202     c                   if        %error
174900070202     c                   eval      wfnorm = *on
175000070202     c                   leavesr
175100070202     c                   endif
175200070201     c                   if        %found(fnorm01l)
175300080219      * aggancio l'ultima fase dell'ORM
175400080219      * per recuperare l'eventuale data/ora dell'ultimo esito avuto da PDA
175500080219     c                   clear                   dorf01
175600080219     c                   clear                   savdfo
175700080219     c                   clear                   savofo
175800080219     c     kfnorf        chain     fnorf01l
175900080219     c                   if        %found(fnorf01l)
176000080219     c                   eval      dorf01 = orfflo
176100080320      * mi salvo orfass della fase precedente
176200080320     c                   eval      save_§orfass = §orfass
176300080219     c                   endif
176400160324      * salvo data/ora esecuzione da PDA, ma solo se fase PDA (410/420)
176500080219     c                   if        §orfesito <> *blanks
176600160324     c                             and (ORFfar = 410 or ORFfar = 420)
176700080219     c                   eval      savdfo = %int(%subst(§orfesito:1:8))
176800080219     c                   eval      savofo = %int(%subst(§orfesito:9:6))
176900080219     c                   endif
177000080212
177100070201     c                   select
177200070201      * esito fatto
177300070201     c                   when      §rescmr = *blanks
177400070201     c                   eval      ormfao = 420
177500070201      * esito non fatto
177600070201     c                   when      §rescmr <> *blanks
177700070201     c                   eval      ormfao = 410
177800070201     c                   endsl
177900070201
178000070213     c                   move      §resdata      ormdfo
178100070213     c                   move      §resora       ormofo
178200070716      * imposto il campo ORMETI
178300070716     c                   clear                   tibs02ds
178400070716     c                   eval      t02mod = 'C'
178500070716     c                   eval      t02sif = knsif
178600070716     c                   eval      t02cod = 'FAR'
178700070716     c                   movel(p)  ormfao        t02ke1
178800070716     c                   call      'TIBS02R'
178900070716     c                   parm                    kpjba
179000070716     c                   parm                    tibs02ds
179100070716     c                   eval      dfar = t02uni
179200070716     c                   eval      ormeti = d§farass
179300070201      * scrivo fase
179400070201     c                   clear                   fnorf000
179500080219     c                   clear                   dorf01
179600070201     c                   eval      orfpoe = ormpoe
179700070201     c                   eval      orfnsr = ormnsr
179800070201     c                   eval      orfnor = ormnor
179900070201     c                   eval      orfnrv = ormnrv
180000070201     c                   eval      orfpog = plgfgs
180100070201     c                   eval      orfdae = ormdfo
180200070201     c                   eval      orfore = ormofo
180300070201     c                   eval      orffar = ormfao
180400070221     c                   eval      orfpue = prcprfc
180500070201     c                   eval      orfcar = §rescmr
180600070201     c                   eval      orffgs = plgfgs
180700070201     c                   eval      orfndc = plgndc
180800070201     c                   eval      orfddc = ormddc
180900080219     c                   eval      §orfesito = §resdtorin
181000080320      * imposto  orfass della fase precedente
181100080320     c                   eval      §orfass =  save_§orfass
181200080219     c                   eval      orfflo = dorf01
181300070205     c                   write     fnorf000
181400080409      * scrivo NOTE
181500080409      * solo se ci sono
181600080409     c                   if        §resnote <> *blanks
181700080409     c                   clear                   wprg
181800080409      * cerco ultimo progressivo relativo alla stessa fase
181900080409     c     kfnorn        setll     fnorn02l
182000080409     c                   do        *hival
182100080409     c     kfnorn        reade     fnorn02l
182200080409     c                   if        %eof(fnorn02l)
182300080409     c                   leave
182400080409     c                   endif
182500080409     c                   eval      wprg = ornprg
182600080409     c                   enddo
182700080409      * scrivo la nota
182800080409     c                   add       1             wprg
182900080409     c                   clear                   fnorn000
183000080409     c                   eval      ornpoe = ormpoe
183100080409     c                   eval      ornnsr = ormnsr
183200080409     c                   eval      ornnor = ormnor
183300080409     c                   eval      ornnrv = ormnrv
183400080409     c                   eval      ornfar = ormfao
183500080409     c                   movel     §orfesito     orndai
183600080409     c                   move      §orfesito     ornori
183700080409     c                   eval      ornprg = wprg
183800080409     c                   eval      ornpue = orfpue
183900080409     c                   eval      ornpon = plgfgs
184000080409     c                   eval      ornsns = 'I'
184100080409     c                   eval      ornnot = §resnote
184200080409     c                   eval      ornftr = 'S'
184300080409     c                   eval      orndtr = dateu8
184400080409     c                   write     fnorn000
184500080409     c                   endif
184600070202      * aggiorno ORM
184700080212      * solo se la data/ora ultima fase dell'ORM è inferiore a quella che sto per scrivere
184800080212     c                   if        ormdfo > savdfo or
184900080214     c                             (ormdfo = savdfo and ormofo >= savofo)
185000070205     c                   update    fnorm000
185100080212     c                   endif
185200080227      * scrivo la partita iva e il codice fiscale nel file di appoggio
185300080320      *
185400080320      * verifico se esiste il file di appoggio fipci02l
185500080320     c     kfnorm        chain     fipci02l
185600080320      * nel casi in cui almeno uno dei due dati è valorizzato aggiorno o scrivo
185700080227     c                   if        §rescodfis <> *blanks or §respiva <> *blanks
185800080227     c                   if        %found(fipci02l)
185900080227     c                   eval      pcicdf = §rescodfis
186000080227     c                   eval      pciiso = §resisoiva
186100080228     c                   eval      pcicpi = §respiva
186200080227     c                   clear                   pciesito
186300080227     c                   update    fipci000
186400080227     c                   else
186500080227     c                   clear                   fipci000
186600080227     c                   eval      pcipoe = ormpoe
186700080227     c                   eval      pcinsr = ormnsr
186800080227     c                   eval      pcinor = ormnor
186900080227     c                   eval      pcinrv = ormnrv
187000080227     c                   eval      pcicdf = §rescodfis
187100080227     c                   eval      pciiso = §resisoiva
187200080228     c                   eval      pcicpi = §respiva
187300080228     c                   write     fipci000
187400080227     c                   endif
187500080227     c                   endif
187600080320
187700080321      * nel casi in cui entrambi sono a blank ed il record esiste annullo il rcd
187800080320     c                   if        §rescodfis = *blanks and §respiva = *blanks
187900080320     c                             and %found(fipci02l)
188000080321     c                   eval      pciatb = 'A'
188100080321     c                   update    fipci000
188200080320     c                   endif
188300080320
188400070201     c                   endif
188500070201
188600070201     c                   endsr
188700080519
188800080519      *------------------------------------------------------------------------*
188900080519      * ROUTINE PER INVIARE MAIL SE ERRORE CODICE SPC
189000080519      *------------------------------------------------------------------------*
189100080519     c     sr_mail       begsr
189200080519
189300080519      * Reperimento tab. "MRA" se non già recuperata
189400080519     c                   if        not $okmra
189500080519     c                   clear                   dmradan
189600080519     c                   clear                   tibs02ds
189700080519     c                   eval      t02mod = 'C'
189800080519     c                   eval      t02sif = knsif
189900080519     c                   eval      t02cod = 'MRA'
190000080519     c                   eval      t02ke1 = sdspgm
190100080519     c                   call      'TIBS02R'
190200080519     c                   parm                    kpjba
190300080519     c                   parm                    tibs02ds
190400080519     c                   if        t02err = *blanks
190500080519     c                   eval      dmradan = t02uni
190600080519     c                   eval      $okmra = *on
190700080519     c                   endif
190800080519     c                   endif
190900080519
191000080519      * Override al file di stampa ed apertura dello stesso
191100080519     c                   if        not %open(prtemail)
191200080519     c                   exsr      sr_openprtf
191300080519     c                   endif
191400080522
191500080522      * Stampa errore
191600080522     c                   if        *in05
191700080522     c                   eval      o_testo = 'trovato codice SPC non valido in +
191800080522     c                                        chiusura distinta ' +
191900080522     c                                        %editc(plgfgs:'X') + '-' +
192000080522     c                                        %editc(plgndc:'X') +
192100080522     c                                        ' in data ' +
192200080522     c                                        %subst(prcdatora:7:2) + '/' +
192300080522     c                                        %subst(prcdatora:5:2) + '/' +
192400080522     c                                        %subst(prcdatora:1:4)
192500080522     c                   except    prtdet
192600080522     c                   endif
192700080522     c                   if        *in01
192800080522     c                   eval      o_testo = 'trovato errore per la distinta ' +
192900080522     c                                        %editc(plgfgs:'X') + '-' +
193000080522     c                                        %editc(plgndc:'X') +
193100080522     c                                        ' in data ' +
193200080522     c                                        %subst(prcdatora:7:2) + '/' +
193300080522     c                                        %subst(prcdatora:5:2) + '/' +
193400080522     c                                        %subst(prcdatora:1:4)
193500080522     c                   except    prtdet
193600080522     c                   eval      o_testo = plgmsg + 'Profilo ' + prcprfc
193700080522     c                   except    prtdet
193800080522     c                   endif
193900080522
194000080522      * Stampa una riga vuota
194100080522     c                   clear                   o_testo
194200080522     c                   except    prtdet
194300080519
194400080519     c                   endsr
194500080519
194600080519      *------------------------------------------------------------------------*
194700080519      * OVERRIDE AL FILE DI STAMPA PER IMPOSTARE I DATI PER L'INVIO VIA MAIL
194800080519      *  + STAMPA INIZIO MAIL
194900080519      *------------------------------------------------------------------------*
195000080519     c     sr_openprtf   begsr
195100080519
195200080519      * Override al file di stampa
195300080519     c                   exsr      sr_override
195400080519
195500080519     c                   open      prtemail
195600080519
195700080519      * Stampa una testata se NON è richiesta la e-mail
195800080519     c                   if        §mradreg = *blanks
195900080519     c                   eval      o_testo = jobuser + ' - ' + sdspgm
196000080519     c                             + ' - ' + %editc( *date : 'Y' )
196100080519     c                             + ' - *REM* ' + %subst(§cm1var : 7 : 70)
196200080519     c                   except    prtdet
196300080519     c                   clear                   o_testo
196400080519     c                   except    prtdet
196500080519     c                   except    prtdet
196600080519     c                   endif
196700080519
196800080519      * Stampa testo iniziale
196900080519     c                   eval      o_testo = 'trovato errore in ricezione dati +
197000080522     c                                        PDA '
197100080519     c                   except    prtdet
197200080519
197300080519      * Stampa una riga vuota
197400080519     c                   clear                   o_testo
197500080519     c                   except    prtdet
197600080519
197700080519     c                   endsr
197800080519
197900080519      *------------------------------------------------------------------------*
198000080519      * OVERRIDE AL FILE DI STAMPA PER IMPOSTARE I DATI PER L'INVIO VIA MAIL
198100080519      *  + STAMPA INIZIO MAIL
198200080519      *------------------------------------------------------------------------*
198300080519     c     sr_override   begsr
198400080519
198500080519     c                   reset                   $invio
198600080519     c                   reset                   trtcm1ds
198700080519
198800080519     c                   if        §mradreg <> *blanks
198900080519     c                   eval      §cm1mitt = %trim(§mradmitt)
199000080519     c                   eval      §cm1dst  = %trim(§mraddest)
199100080519     c                   eval      §cm1tips = §mradreg
199200080519     c                   eval      §cm1po   = c_sede
199300080519     c                   eval      §cm1var  = '*OBJM*' + §mraddes
199400080519     c                   eval      §cm1idp  = §mradidpro
199500080519     c                   eval      qcmd = c_cmdovrprtf
199600080519     c                             + ' outq(' + %trim(§mradoutqi) + ')'
199700080519     c                             + ' usrdfndta(''' + trtcm1ds + ''')'
199800080519     c                   else
199900080519     c                   eval      qcmd = c_cmdovrprtf
200000080519     c                   endif
200100080519
200200080519     c                   eval      lenght = %size(qcmd)
200300080519     c                   call(e)   'QCMDEXC'
200400080519     c                   parm                    qcmd
200500080519     c                   parm                    lenght
200600080519
200700080519     c                   if        §mradreg <> *blanks and not %error
200800080519     c                   eval      $invio = *on
200900080519     c                   endif
201000080519
201100080519     c                   endsr
201200080513
201300080513      *------------------------------------------------------------------------*
201400080513      * ROUTINE FINALE
201500080513      *------------------------------------------------------------------------*
201600080513     c     sr_end        begsr
201700080513
201800080513     c                   if        %open(prtemail)
201900080513
202000080513      * Chiusura dello spool?
202100080522     c                   eval      o_testo = sdspgm
202200080522     c                   except    prtdet
202300080513     c                   clear                   o_testo
202400080513     c                   except    prtdet
202500080513     c                   eval      o_testo = '***   Fine Lista   ***'
202600080513     c                   except    prtdet
202700080513
202800080513     c                   close     prtemail
202900080513
203000080513      * Eliminazione overflow?
203100080513     c                   eval      lenght = %size(qcmd)
203200080513     c                   eval      qcmd = c_cmddltovr
203300080513     c                   call(e)   'QCMDEXC'
203400080513     c                   parm                    qcmd
203500080513     c                   parm                    lenght
203600080513
203700080513     c                   endif
203800080513
203900080513     c                   endsr
204000070129
204100070129      *------------------------------------------------------------------------*
204200070129      * ROUTINE INIZIALE
204300070129      *------------------------------------------------------------------------*
204400070129     c     *inzsr        begsr
204500070129
204600070129     c     *entry        plist
204700070129     c                   parm                    parmpo
204800080226     c                   parm                    chiudi
204900070129
205000070129     c     kfiapd        klist
205100070129     c                   kfld                    apdtip
205200070129     c                   kfld                    apdpdr
205300070201
205400070201     c     kfiqdt        klist
205500070201     c                   kfld                    plgfgs
205600070201     c                   kfld                    plgndc
205700080319
205800080319     c     kfipro        klist
205900080319     c                   kfld                    plgfgs
206000080319     c                   kfld                    plgndc
206100140129     c                   kfld                    plgtiporcd
206200080730     c                   kfld                    plgcodaut
206300080319     c                   kfld                    ormpoe
206400080319     c                   kfld                    ormnsr
206500080319     c                   kfld                    ormnor
206600080319     c                   kfld                    ormnrv
206700070201
206800080226     c     kfidst        klist
206900080226     c                   kfld                    dstnpg
207000070201     c                   kfld                    dstnfv
207100070201     c                   kfld                    dstfgs
207200070131
207300070131     c     kfnorm        klist
207400070131     c                   kfld                    ormpoe
207500070131     c                   kfld                    ormnsr
207600070131     c                   kfld                    ormnor
207700070131     c                   kfld                    ormnrv
207800080219
207900080219     c     kfnorf        klist
208000080219     c                   kfld                    ormpoe
208100080219     c                   kfld                    ormnsr
208200080219     c                   kfld                    ormnor
208300080219     c                   kfld                    ormnrv
208400080219     c                   kfld                    ormdfo
208500080219     c                   kfld                    ormofo
208600080219     c                   kfld                    ormfao
208700080409
208800080409     c     kfnorn        klist
208900080409     c                   kfld                    ormpoe
209000080409     c                   kfld                    ormnsr
209100080409     c                   kfld                    ormnor
209200080409     c                   kfld                    ormnrv
209300080409     c                   kfld                    ormfao
209400080415
209500080415     c     kfirpd        klist
209600080415     c                   kfld                    plgfgs
209700080415     c                   kfld                    plgndc
209800080415     c                   kfld                    plgcodaut
209900080415     c                   kfld                    plgiddoc
210000080417
210100080417     c     kfirpt        klist
210200080417     c                   kfld                    plgfgs
210300080417     c                   kfld                    plgndc
210400080417     c                   kfld                    plgcodaut
210500140207      *
210600140207     c     ktbe          klist
210700140207     c                   kfld                    tbecod
210800140207     c                   kfld                    tbeke1
210900070129
211000070129     c                   endsr
211100080513
211200080513     oprtemail  e            prtdet      1
211300080513     o                       o_testo
