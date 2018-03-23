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
002101140207     ftntbe01l  if   e           k disk
002200070130
002300070130      *------------------------------------------------------------------------*
002400070130      *  RIEPILOGO INDICATORI
002500070130      *------------------------------------------------------------------------*
002600070130      * 01 - errore
002700070131      * 02 - testn
002800070131      * 03 - ORM manuale non aggiorno fasi ORM
002900110620      * 04 - Errore per invio mail
003000080519      * 05 - codice SPC errato
003100070129
003200070130      *------------------------------------------------------------------------*
003300070129      *   V A R I A B I L I
003400070130      *------------------------------------------------------------------------*
003500080226     d chiudi          s              1
003600070201     d dateu8          s              8  0
003700110609     d data_eur        s               d   datfmt(*eur)
003800110609     d data_iso        s               d   datfmt(*iso)
003900070201     d hhmm            s              4  0
004000070130     d kprcprfc        s                   like(prcprfc)
004100080513     d lenght          s             15  5
004200080513     d o_testo         s            132    inz
004300120920     d Oggi_10         s              8  0
004400120920     d Oggi_10o        s              8  0
004500070129     d parmpo          s              3
004600080513     d qcmd            s            512    inz
004700080212     d savdfo          s                   like(ormdfo)
004800080212     d savofo          s                   like(ormofo)
004900080320     d save_§orfass    s                   like(§orfass)
005000070201     d udate8          s              8  0
005100070201     d wbnc            s                   like(qdtnbnri)
005200070129     d wcodaut         s              7
005300070521     d wdata           s              8  0
005400080924     d wdesspc         s             15
005500080327     d wfase           s              1    inz(*off)
005600070202     d wfipdo          s              1    inz(*off)
005700070202     d wfiqdt          s              1    inz(*off)
005800080319     d wfipro          s              1    inz(*off)
005900070201     d wfgs            s                   like(§okfgs)
006000070202     d wfnorm          s              1    inz(*off)
006100080924     d widrow          s                   like(ormndc)
006200070201     d wncl            s                   like(qdtnclrit)
006300070201     d wndc            s                   like(§okndc)
006400070521     d wora            s              6  0
006500070201     d wpdandc         s                   like(ormndc)
006600080409     d wprg            s                   like(ornprg)
006700070521     d w006a           s              6
006800070521     d w008a           s              8
006900070201     d w0140           s             14  0
007000070201     d w030a           s             30
007100080417     d w_dtaorach      s             14
007200080513     d xx              s              3  0
007300080513     d $invio          s              1n   inz(*off)
007400080513     d $okmra          s              1n   inz(*off)
007401140129     d worasti         s                   like(§RORORASTI)
007402140129     d wsort           s                   like(§RORSORT)
007500070129
007600080513      *------------------------------------------------------------------------*
007700080513      *   S C H I E R E
007800080513      *------------------------------------------------------------------------*
007900090911     d codspc          s              2    dim(23)
008000090911     d desspc          s             15    dim(23)
008100080513
008200070130      *------------------------------------------------------------------------*
008300070129      *   D S   I N T E R N E / E S T E R N E
008400070130      *------------------------------------------------------------------------*
008500070205     d prc             ds
008600070205     d  prcnrr               397    400B 0
008700070205
008800070201     d wlbdat          ds
008900070201     d  g02dat                 1      8  0
009000070201     d  g02inv                 9     16  0
009100070201     d  g02err                17     17
009200070201     d  g02tgi                18     22  0
009300070201
009400070716     d dfar          e ds
009500080219     d dorf01        e ds
009600080317     d ddstflr       e ds
009700080513     d dpsp          e ds
009800090911     d dpsps         e ds
009900080924
010000080924     d fidsfrchds    e ds                  prefix(§RCH:4)
010100070716
010101140129     d fiprorords    e ds
010102140129     d fiprookds     e ds
010103140129     d fiprcrords    e ds
010104140129     d  §rorpoe               10     12
010105140129     d  §rornsr               13     14
010106140129     d  §rornor               15     21
010107140129     d  §rornrv               22     23
010108140129     d  §roraas               10     13
010109140129     d  §rorlnp               14     16
010110140129     d  §rornrs               17     18
010111140129     d  §rornsp               19     25
010112140129     d  §rordata              26     33
010113140129     d  §rorora               34     39
010200070129     d fiprcokds     e ds
010300070201     d  §okpoe                10     12
010400070201     d  §oknsr                13     14
010500070201     d  §oknor                15     21
010600070201     d  §oknrv                22     23
010700070201     d  §okaas                10     13
010800070201     d  §oklnp                14     16
010900070201     d  §oknrs                17     18
011000070201     d  §oknsp                19     25
011100070213     d  §okdata               26     33
011200070213     d  §okora                34     39
011300070129     d fiprcrchds    e ds
011400070213     d  §rchdata              20     27
011500070213     d  §rchora               28     33
011600070129     d fiprcresds    e ds
011700070213     d  §resdata              24     31
011800070213     d  §resora               32     37
011900070213     d  §resdatain           171    178
012000070213     d  §resorain            179    184
012100080317
012200080513     d dmradan       e ds                  inz
012300080327     d fior90ds      e ds
012400070130     d kpjba         e ds
012500070129     d tibs02ds      e ds
012600080513
012700080513     d trtcm1ds      e ds                  inz
012800080514      *    .§cm1mitt = Indirizzo e-mail del mittente
012900120305     d   §cm1mitt    e                     inz('ced@brt.it')
013000080514      *    ·§cm1dst  = Indirizzo e-mail del destinatario
013100120305     d   §cm1dst     e                     inz('monica.beghelli@brt.it')
013200080514      *    ·§cm1tips = Tipo lettera via e-mail:
013300080513      *     "LET" = testo allegato in corpo con logo
013400080513      *             (richiede righe libere iniziali per il logo)
013500080513      *     "COR" = testo integrato senza logo
013600080513      *             (non consente né UNDERLINE né HIGHLIGHT)
013700080514     d   §cm1tips    e                     inz('COR')
013800080514      *    ·§cm1po   = Filiale
013900080514     d   §cm1po      e                     inz('046')
014000080514      *    ·§cm1var  = Oggetto e-mail
014100080514     d   §cm1var     e                     inz('*OBJM*+
014200080521     d                                     Errore in ricezione dati da PDA')
014300080514      *    ·§cm1sts  = Stato
014400080514     d   §cm1sts     e                     inz(*off)
014500080514      *    ·§cm1idp  = Id processo
014600080521     d   §cm1idp     e                     inz('2')
014700080513
014800080513     d psds           sds
014900080513     d   sdspgm          *proc
015000080513     d   jobuser             254    263                                         User name
015100080513
015200080513      *------------------------------------------------------------------------*
015300080513      *   C O S T A N T I
015400080513      *------------------------------------------------------------------------*
015500080513     d c_sede          c                   const('046')
015600080513      *  - comando di override al prtf
015700080513     d c_cmdovrprtf    c                   const('OVRPRTF +
015800080513     d                                           file(PRTEMAIL) +
015900080513     d                                           pagesize(66 132) +
016000080513     d                                           lpi(6) cpi(10) +
016100080513     d                                           ovrscope(*actgrpdfn) +
016200080513     d                                           ')
016300080513     d c_cmddltovr     c                   const('DLTOVR +
016400080513     d                                            file(PRTEMAIL) +
016500080513     d                                            lvl(*actgrpdfn)')
016600070129
016700070129      *------------------------------------------------------------------------*
016800070607
016900070607     c                   eval      kprcprfc = 'AU0000000'
017000070607     c                   eval      %subst(kprcprfc:3:3) = parmpo
017100110609
017200110609      *?Cerco la data del giorno
017300110609     c                   time                    w0140
017400120920      *?Calcolo oggi - 10 gg.
017500110609     c                   move      w0140         data_eur
017600110609     c                   move      data_eur      data_iso
017700120920     c                   subdur    10:*d         data_iso
017800120920     c                   move      data_iso      Oggi_10
017900120920      *?Calcolo oggi + 10 gg.
018000120920     c                   move      w0140         data_eur
018100120920     c                   move      data_eur      data_iso
018200120920     c                   adddur    10:*d         data_iso
018300120920     c                   move      data_iso      Oggi_10o
018400110202
018500110202      * Recupero i codici SPC
018600110202     c                   exsr      sr_carcodspc
018700070129
018800110202      * Leggo file ritorno dati da PDA
018900070130     c     kprcprfc      setll     fiprc01r
019000070129     c                   do        *hival
019100070129     c                   read      fiprc01r
019200070129
019300070129     c                   if        %eof(fiprc01r)
019400070129     c                   leave
019500070129     c                   endif
019600070129      * stesso p.o.
019700070129     c                   if        %subst(prcprfc:3:3) <> parmpo
019800070129     c                   leave
019900070129     c                   endif
020000070129
020100070129     c                   clear                   fiplg000
020200070130     c                   eval      *in01 = *off
020300110620     c                   eval      *in04 = *off
020400070129
020500070130      * controlla i dati inviati
020600070129     c                   exsr      sr_ctrdati
020700070130
020800070130      * aggiorno i dati effettivi in base al tipo record
020900070130      * se non ci sono stati errori nei controlli
021000101104     c                   if        not *in01
021100101104      * e la distinta NON è in test per gli ORM
021200080317     c                             and §dsttstpda <> 'O' and §dsttstpda <> 'E'
021300080205     c                   exsr      sr_file
021400080205     c                   endif
021500080724
021600080724      * scrivo fase RCH SEMPRE quando ricevo rcd RCH
021700080924      * passo in kpjbu i dati da memorizzare nel campo "dati" sul file delle fasi
021800090810     c                   if        prctiporcd = 'RCH' and not *in01
021900080724     c                   exsr      sr_fase
022000080724     c                   endif
022100080404
022200080519      * se non ci sono stati errori nei controlli
022300101104      * e la distinta è in test cancello solo i dati di FIPDO e FIPRO
022400101104     c                   if        not *in01 and
022500101104     c                             (§dsttstpda = 'O' or §dsttstpda = 'E')
022600080404      * e ricevo un RCH
022700080404     c                   if        plgtiporcd = 'RCH'
022800080404      * devo cancellare i dati da FIPDO e da FIPRO
022900080404     c                   exsr      sr_fipdo
023000080404     c                   exsr      sr_fipro
023100080404     c                   endif
023200080404     c                   endif
023300070202
023400080320      * se non ci sono stati errori nei controlli
023500080320      * tipo record 'OK' solo per i ritiri
023600101104     c                   if        not *in01 and
023700140129     c                             plgtiporcd = 'OK' and §OKTPIDDOC = 'R'
023800080320     c                   exsr      sr_wrtpro
023900080320     c                   endif
023901140129      * scrittura fipro per tipo record ROR
023902140129     c                   if        plgtiporcd = 'ROR'
023903140129     c                   exsr      sr_wrtproror
023904140129     c                   endif
024000070202      * scrivo il file di LOG
024100070202     c                   exsr      sr_wrtlog
024200070202
024300080519      * se non ci sono stati errori nei controlli
024400080415      * se ricevo fase "RES" o fase "RCH" scrivo i file legati al prospetto ORM
024500080415     c                   if        not *in01 and
024600080415     c                             (plgtiporcd = 'RCH' OR plgtiporcd = 'RES')
024700080415     c                   exsr      sr_Prospetto
024800080415     c                   endif
024900080519      * se ci sono stati errori nei controlli inivio mail
025000110620     c**!!!              if        *in01 or *in05
025100110620     c                   if        *in04 or *in05
025200080519     c                   exsr      sr_mail
025300080519     c                   endif
025400080519
025500070205      * se ho avuto errori di allocazione dei file devo
025600070202      * fare il rollback
025700070205    2c                   if        wfiqdt = *on or wfipdo = *on or
025800080327     c                             wfnorm = *on or wfipro = *on or
025900080327     c                             wfase = *on
026000070202     c                   rolbk
026100070205      * se non ho avuto errori
026200070205   2xc                   else
026300070205      * aggancio il file fisico di ricezione
026400070205     c     prcnrr        chain(e)  fiprc00r
026500070205      * se rcd allocato flag di comodo per poi fare il rollback
026600070205      * per errore rollback
026700070205    3c                   if        %error
026800070205     c                   rolbk
026900070205   3xc                   else
027000070205    4c                   if        %found(fiprc00r)
027100070202      * cancello il file di ricezione
027200070202     c                   delete    fiprc000
027300070205      * committo le modifiche
027400070205     c                   commit
027500070205    4c                   endif
027600070205    3c                   endif
027700070205    2c                   endif
027800070202
027900080327     c                   eval      wfase = *off
028000070202     c                   eval      wfiqdt = *off
028100070202     c                   eval      wfipdo = *off
028200070202     c                   eval      wfnorm = *off
028300080319     c                   eval      wfipro = *off
028400070202
028500070129     c                   enddo
028600080513
028700080513     c                   exsr      sr_end
028800080227
028900080226      * testa la chiusura del sottosistema
029000080226     c                   shtdn                                        99
029100080226     c   99              eval      chiudi = 'S'
029200070129
029300070205     c                   eval      *inrt = *on
029400110202
029500110202      *------------------------------------------------------------------------*
029600110202      * RECUPERO CODICI SPC
029700110202      *------------------------------------------------------------------------*
029800110202     c     sr_carcodspc  begsr
029900110202
030000110202     c                   clear                   codspc
030100110202     c                   clear                   desspc
030200110202
030300110202      * recupero tabella codici SPC (PSP)
030400110202      * chiave 1 per le prime 11 password
030500110202     c                   clear                   tibs02ds
030600110202     c                   clear                   dpsp
030700110202     c                   eval      t02mod = 'C'
030800110202     c                   eval      t02sif = knsif
030900110202     c                   eval      t02cod = 'PSP'
031000110202     c                   eval      t02ke1 = parmpo
031100110202     c                   eval      t02ke2 = '1'
031200110202     c                   call      'TIBS02R'
031300110202     c                   parm                    kpjba
031400110202     c                   parm                    tibs02ds
031500110202     c                   if        t02err = *blanks
031600110202     c                   eval      dpsp = t02uni
031700110202     c                   endif
031800110202
031900110202     c                   if        dpsp <> *blanks
032000110202     c                   eval      codspc(01) = §pspcod01
032100110202     c                   eval      desspc(01) = §pspdes01
032200110202     c                   eval      codspc(02) = §pspcod02
032300110202     c                   eval      desspc(02) = §pspdes02
032400110202     c                   eval      codspc(03) = §pspcod03
032500110202     c                   eval      desspc(03) = §pspdes03
032600110202     c                   eval      codspc(04) = §pspcod04
032700110202     c                   eval      desspc(04) = §pspdes04
032800110202     c                   eval      codspc(05) = §pspcod05
032900110202     c                   eval      desspc(05) = §pspdes05
033000110202     c                   eval      codspc(06) = §pspcod06
033100110202     c                   eval      desspc(06) = §pspdes06
033200110202     c                   eval      codspc(07) = §pspcod07
033300110202     c                   eval      desspc(07) = §pspdes07
033400110202     c                   eval      codspc(08) = §pspcod08
033500110202     c                   eval      desspc(08) = §pspdes08
033600110202     c                   eval      codspc(09) = §pspcod09
033700110202     c                   eval      desspc(09) = §pspdes09
033800110202     c                   eval      codspc(10) = §pspcod10
033900110202     c                   eval      desspc(10) = §pspdes10
034000110202     c                   eval      codspc(11) = §pspcod11
034100110202     c                   eval      desspc(11) = §pspdes11
034200110202     c                   endif
034300110202      * chiave 2 per le altre 11 password
034400110202     c                   clear                   dpsp
034500110202     c                   eval      t02ke2 = '2'
034600110202     c                   call      'TIBS02R'
034700110202     c                   parm                    kpjba
034800110202     c                   parm                    tibs02ds
034900110202     c                   if        t02err = *blanks
035000110202     c                   eval      dpsp = t02uni
035100110202     c                   endif
035200110202
035300110202     c                   if        dpsp <> *blanks
035400110202     c                   eval      codspc(12) = §pspcod01
035500110202     c                   eval      desspc(12) = §pspdes01
035600110202     c                   eval      codspc(13) = §pspcod02
035700110202     c                   eval      desspc(13) = §pspdes02
035800110202     c                   eval      codspc(14) = §pspcod03
035900110202     c                   eval      desspc(14) = §pspdes03
036000110202     c                   eval      codspc(15) = §pspcod04
036100110202     c                   eval      desspc(15) = §pspdes04
036200110202     c                   eval      codspc(16) = §pspcod05
036300110202     c                   eval      desspc(16) = §pspdes05
036400110202     c                   eval      codspc(17) = §pspcod06
036500110202     c                   eval      desspc(17) = §pspdes06
036600110202     c                   eval      codspc(18) = §pspcod07
036700110202     c                   eval      desspc(18) = §pspdes07
036800110202     c                   eval      codspc(19) = §pspcod08
036900110202     c                   eval      desspc(19) = §pspdes08
037000110202     c                   eval      codspc(20) = §pspcod09
037100110202     c                   eval      desspc(20) = §pspdes09
037200110202     c                   eval      codspc(21) = §pspcod10
037300110202     c                   eval      desspc(21) = §pspdes10
037400110202     c                   eval      codspc(22) = §pspcod11
037500110202     c                   eval      desspc(22) = §pspdes11
037600110202     c                   endif
037700110202
037800110202      * recupero tabella codici SPC (PSP)
037900110202      * la password di sede
038000110202     c                   clear                   tibs02ds
038100110202     c                   clear                   dpsps
038200110202     c                   eval      t02mod = 'C'
038300110202     c                   eval      t02sif = knsif
038400110202     c                   eval      t02cod = 'PSP'
038500110202     c                   eval      t02ke1 = '046'
038600110202     c                   call      'TIBS02R'
038700110202     c                   parm                    kpjba
038800110202     c                   parm                    tibs02ds
038900110202     c                   if        t02err = *blanks
039000110202     c                   eval      dpsps = t02uni
039100110202     c                   endif
039200110202     c                   if        dpsps <> *blanks
039300110202     c                   eval      codspc(23) = §pspcods
039400110202     c                   eval      desspc(23) = §pspdess
039500110202     c                   endif
039600110202
039700110202     c                   endsr
039800070129
039900070129      *------------------------------------------------------------------------*
040000070129      * ROUTINE CONTROLLO DATI FIPRC
040100070129      *------------------------------------------------------------------------*
040200070129     c     sr_ctrdati    begsr
040300080924
040400080924     c                   clear                   fidsfrchds
040500081103     c                   eval      *in05 = *off
040600070129
040700070131      * tipo record
040800070131      * se errato non posso continuare perchè non so come leggere i dati
040900070201     c                   if        prctiporcd <> 'OK ' and
041000070201     c                             prctiporcd <> 'RCH' and
041001140129     c                             prctiporcd <> 'ROR' and
041100070201     c                             prctiporcd <> 'RES'
041200070131     c                   eval      plgerr = 'E'
041300070131     c                   eval      plgmsg = 'TIPO RECORD NON VALIDO'
041400070131     c                   eval      *in01 = *on
041500110620     c                   eval      *in04 = *on
041600070131     c                   leavesr
041700070131     c                   endif
041800070131     c                   eval      plgtiporcd = prctiporcd
041900070131
042000070129      * codice autotrasportatore
042100070130      * anche se errato vado avanti lo stesso con i controlli ma lascio l'errore
042200070131     c                   eval      wcodaut = %subst(prcprfc:3:7)
042300070201      *  deve essere numerico
042400070201     c                   eval      w030a = *all'0'
042500070201     c                   movel     wcodaut       w030a
042600070201     c                   exsr      sr_testn
042700070201     c                   if        not *in02
042800070201     c                   eval      plgerr = 'E'
042900070201     c                   eval      plgmsg = 'AUTOTRASPORTATORE NON VALIDO'
043000070201     c                   eval      *in01 = *on
043100110620     c                   eval      *in04 = *on
043200070201     c                   else
043300070201      *  deve esistere
043400070129     c                   eval      apdtip = 'A'
043500070202     c                   move      wcodaut       apdpdr
043600070129     c     kfiapd        chain     fiapd01l
043700070129     c                   if        not %found(fiapd01l) or
043800070129     c                             apdatb <> *blanks
043900070129     c                   eval      plgerr = 'E'
044000070129     c                   eval      plgmsg = 'AUTOTRASPORTATORE NON VALIDO'
044100070130     c                   eval      *in01 = *on
044200110620     c                   eval      *in04 = *on
044300070130     c                   endif
044400070201     c                   endif
044500070130     c  n01              move      wcodaut       plgcodaut
044600070129
044700070129      * campo dati
044800070129     c                   select
044900070129     c                   when      prctiporcd = 'OK '
045000070129     c                   exsr      sr_elabok
045100070129     c                   when      prctiporcd = 'RCH'
045200070129     c                   exsr      sr_elabrch
045300070129     c                   when      prctiporcd = 'RES'
045400070129     c                   exsr      sr_elabres
045401140129     c                   when      prctiporcd = 'ROR'
045402140129     c                   exsr      sr_elabror
045500070129     c                   endsl
045600070129
045700070129     c                   endsr
045800070129
045900070129      *------------------------------------------------------------------------*
046000070130      * ROUTINE ELABORAZIONE RCD "OK"
046100070129      *------------------------------------------------------------------------*
046200070129     c     sr_elabok     begsr
046300070129
046400070129     c                   eval      fiprcokds = prcdati
046500070129
046600070129      * p.o. gestione distinta
046700070201     c                   eval      wfgs = §okfgs
046800070201     c                   exsr      sr_ctrfgs
046900070201     c   02              move      §okfgs        plgfgs
047000070129
047100070129      * numero distinta
047200070201     c                   eval      wndc = §okndc
047300070201     c                   exsr      sr_ctrndc
047400070201     c   02              move      §okndc        plgndc
047500070201
047600070201      * controllo se esiste la distinta
047700070201     c   02              exsr      sr_ctrdist
047800070129
047900070129      * tipo applicazione
048000070130      * può essere C=consegne R=ritiri
048100070131     c                   if        §oktpiddoc <> 'C' and §oktpiddoc <> 'R'
048200070130     c  n01              eval      plgerr = 'E'
048300070130     c  n01              eval      plgmsg = 'DATI NON VALIDI'
048400070130     c  n01              eval      *in01 = *on
048500110620     c  n01              eval      *in04 = *on
048600070130     c                   else
048700070131     c                   eval      plgtipappl = §oktpiddoc
048800070130     c                   endif
048900070129
049000070129      * id documento
049100080228      *  solo ritiro
049200080228     c                   if        §oktpiddoc = 'R'
049300070201      *  deve essere numerico
049400070201     c                   eval      w030a = *all'0'
049500070201     c                   eval      %subst(w030a:1:14) = %subst(§okiddoc:1:14)
049600070201     c                   exsr      sr_testn
049700070201     c                   if        not *in02
049800070201     c  n01              eval      plgerr = 'E'
049900070201     c  n01              eval      plgmsg = 'DATI ID DOC. ORM NON VALIDI'
050000070201     c  n01              eval      *in01 = *on
050100110620     c  n01              eval      *in04 = *on
050200070201     c                   else
050300070201      *  deve essere un n. di ORM valido
050400070201     c                   move      §okpoe        ormpoe
050500070201     c                   move      §oknsr        ormnsr
050600070201     c                   move      §oknor        ormnor
050700070201     c                   move      §oknrv        ormnrv
050800070201     c     kfnorm        chain(n)  fnorm01l
050900070201     c                   if        not %found(fnorm01l)
051000070201     c  n01              eval      plgerr = 'E'
051100070201     c  n01              eval      plgmsg = 'ORM INESISTENTE'
051200070201     c  n01              eval      *in01 = *on
051300110620     c  n01              eval      *in04 = *on
051400070201     c                   endif
051500070201     c                   endif
051600070201     c  n01              eval      plgiddoc = §okiddoc
051700080228     c                   endif
051800070213
051900070213      * data conferma
052000070213      *  deve essere numerico
052100070213     c                   eval      w030a = *all'0'
052200070213     c                   movel     §okdata       w030a
052300070213     c                   exsr      sr_testn
052400070213     c                   if        not *in02
052500070213     c  n01              eval      plgerr = 'E'
052600070213     c  n01              eval      plgmsg = 'DATA CONFERMA NON VALIDA'
052700070213     c  n01              eval      *in01 = *on
052800110620     c  n01              eval      *in04 = *on
052900070213     c                   endif
053000070213      *  deve essere una data valida
053100070213     c                   if        *in02
053200070213     c                   clear                   wlbdat
053300070213     c                   move      §okdata       g02inv
053400070213     c                   eval      g02err = '3'
053500070213     c                   call      'XSRDA8'
053600070213     c                   parm                    wlbdat
053700070213     c                   if        g02err = '1'
053800070213     c  n01              eval      plgerr = 'E'
053900070213     c  n01              eval      plgmsg = 'DATA CONFERMA NON VALIDA'
054000070213     c  n01              eval      *in01 = *on
054100110620     c  n01              eval      *in04 = *on
054200070213     c                   endif
054300070213     c                   endif
054400070213
054500070213      * ora conferma
054600070213      *  deve essere numerico
054700070213     c                   eval      w030a = *all'0'
054800070213     c                   movel     §okora        w030a
054900070213     c                   exsr      sr_testn
055000070213     c                   if        not *in02
055100070213     c  n01              eval      plgerr = 'E'
055200070213     c  n01              eval      plgmsg = 'ORA CONFERMA NON VALIDA'
055300070213     c  n01              eval      *in01 = *on
055400110620     c  n01              eval      *in04 = *on
055500070213     c                   endif
055600070213      *  deve essere un'ora valida
055700070213     c                   if        *in02 and
055800070213     c                             %subst(§okora:1:2) > '23' or
055900070213     c                             %subst(§okora:3:2) > '59' or
056000070213     c                             %subst(§okora:5:2) > '59'
056100070213     c  n01              eval      plgerr = 'E'
056200070213     c  n01              eval      plgmsg = 'ORA CONFERMA NON VALIDA'
056300080319     c  n01              eval      *in01 = *on
056400110620     c  n01              eval      *in04 = *on
056500070213     c                   endif
056600110609      /free
056700110609       //?La data di conferma ORM su rcd OK non può essere inferiore a 30 gg da oggi
056800110609       //?in questo caso accetto il rcd ma imposto la data della distinta
056900110609       //?e come ora imposto l'ora di ricezione del record
057000110609       //?è capitato che PDA si è resettato ed ha riportato la data al 1980
057100120920         IF  not *in01 and §OKdata < %editc(Oggi_10:'X');
057200110609           §OKdata = %editc(DSTdfv:'X');
057300110609           §OKora  = %subst(PRCdatora:9:6);
057400121015           PLGmsg  = 'DATA < o > di 10 GG';
057500110609         ENDIF;
057600120920       //?E' capitato anche che il PDA è andato fuori di testa ed ha impostato
057700120920       //?la data a 2514
057800120920         IF  not *in01 and §OKdata > %editc(Oggi_10o:'X');
057900120920           §OKdata = %editc(DSTdfv:'X');
058000121015           %subst(§OKora:1:4)  = %subst(PRCdatora:9:4);
058100121010           PLGmsg  = 'DATA < o > di 10 GG';
058200120920         ENDIF;
058300110609      /end-free
058400070201
058500070201      * flag richiesta assistenza
058501140207     c                   if        §okflgass  <> *blank
058514140213     c                   movel     'RAS'         tbecod
058515140213     c                   movel     §okflgass     tbeke1
058516140207     c     ktbe          chain     tntbe01l
058517140207     c                   if        not %found(tntbe01l)
058700070201     c  n01              eval      plgerr = 'E'
058800070201     c  n01              eval      plgmsg = 'FLAG RICH. ASS. NON VALIDO'
058900070201     c  n01              eval      *in01 = *on
059000110620     c  n01              eval      *in04 = *on
059100070201     c                   endif
059101140207     c                   endif
059200080221
059300080221      * identificativo riga
059400080319     c                   clear                   Widrow
059500080221     c                   eval      w030a = *all'0'
059600080221     c                   movel     §okidrow      w030a
059700080221     c                   exsr      sr_testn
059800080221     c                   if        not *in02
059900080221     c  n01              eval      plgerr = 'E'
060000080221     c  n01              eval      plgmsg = 'ID. RIGA OK NON VALIDO'
060100080221     c  n01              eval      *in01 = *on
060200110620     c  n01              eval      *in04 = *on
060300080319
060400080319     c                   else
060500080319     c                   move      §okidrow      Widrow
060600080221     c                   endif
060700070129
060800070129     c                   endsr
060900070130
061000070130      *------------------------------------------------------------------------*
061100070130      * ROUTINE ELABORAZIONE RCD "RCH"
061200070130      *------------------------------------------------------------------------*
061300070130     c     sr_elabrch    begsr
061400070130
061500070130     c                   eval      fiprcrchds = prcdati
061600070130
061700070130      * p.o. gestione distinta
061800070201     c                   eval      wfgs = §rchfgs
061900070201     c                   exsr      sr_ctrfgs
062000070201     c   02              move      §rchfgs       plgfgs
062100070130
062200070130      * numero distinta
062300070201     c                   eval      wndc = §rchndc
062400070201     c                   exsr      sr_ctrndc
062500070201     c   02              move      §rchndc       plgndc
062600070201
062700070201      * controllo se esiste la distinta
062800070201     c   02              exsr      sr_ctrdist
062900070201
063000070201      * controllo se stesso AUT
063100070201     c                   if        *in02 and plgcodaut <> dstpdr
063200070201     c  n01              eval      plgerr = 'E'
063300070201     c  n01              eval      plgmsg = 'AUT. INCONGRUENTE'
063400070201     c  n01              eval      *in01 = *on
063500110620     c  n01              eval      *in04 = *on
063600070201     c                   endif
063700070201
063800070201      * la distinta deve esistere anche su FIQDT
063900070201     c                   if        *in02
064000070201     c     kfiqdt        chain(n)  fiqdt01l
064100070201     c                   if        not %found(fiqdt01l) or qdtatb <> *blanks
064200070201     c  n01              eval      plgerr = 'E'
064300070201     c  n01              eval      plgmsg = 'DISTINTA INESISTENTE SU FIQDT'
064400070201     c  n01              eval      *in01 = *on
064500110620     c  n01              eval      *in04 = *on
064600070201     c                   endif
064700070201     c                   endif
064800070130
064900070130      * totale colli
065000070130      *  deve essere numerico
065100070201     c                   eval      w030a = *all'0'
065200070201     c                   movel     §rchncl       w030a
065300070201     c                   exsr      sr_testn
065400070201     c                   if        not *in02
065500070130     c  n01              eval      plgerr = 'E'
065600070201     c  n01              eval      plgmsg = 'DATI TOT. COLLI NON VALIDI'
065700070130     c  n01              eval      *in01 = *on
065800110620     c  n01              eval      *in04 = *on
065900070201     c                   else
066000070201     c                   move      §rchncl       wncl
066100070130     c                   endif
066200070130
066300070130      * totale bancali
066400070130      *  deve essere numerico
066500070201     c                   eval      w030a = *all'0'
066600070201     c                   movel     §rchbnc       w030a
066700070201     c                   exsr      sr_testn
066800070201     c                   if        not *in02
066900070130     c  n01              eval      plgerr = 'E'
067000070201     c  n01              eval      plgmsg = 'DATI TOT. BANCALI NON VALIDI'
067100070130     c  n01              eval      *in01 = *on
067200110620     c  n01              eval      *in04 = *on
067300070201     c                   else
067400070201     c                   move      §rchbnc       wbnc
067500070130     c                   endif
067600070213
067700070213      * data chiusura
067800070213      *  deve essere numerico
067900070213     c                   eval      w030a = *all'0'
068000070213     c                   movel     §rchdata      w030a
068100070213     c                   exsr      sr_testn
068200070213     c                   if        not *in02
068300070213     c  n01              eval      plgerr = 'E'
068400070213     c  n01              eval      plgmsg = 'DATA CHIUSURA NON VALIDA'
068500070213     c  n01              eval      *in01 = *on
068600110620     c  n01              eval      *in04 = *on
068700070213     c                   endif
068800070213      *  deve essere una data valida
068900070213     c                   if        *in02
069000070213     c                   clear                   wlbdat
069100070213     c                   move      §rchdata      g02inv
069200070213     c                   eval      g02err = '3'
069300070213     c                   call      'XSRDA8'
069400070213     c                   parm                    wlbdat
069500070213     c                   if        g02err = '1'
069600070213     c  n01              eval      plgerr = 'E'
069700070213     c  n01              eval      plgmsg = 'DATA CHIUSURA NON VALIDA'
069800070213     c  n01              eval      *in01 = *on
069900110620     c  n01              eval      *in04 = *on
070000070213     c                   endif
070100070213     c                   endif
070200070213
070300070213      * ora chiusura
070400070213      *  deve essere numerico
070500070213     c                   eval      w030a = *all'0'
070600070213     c                   movel     §rchora       w030a
070700070213     c                   exsr      sr_testn
070800070213     c                   if        not *in02
070900070213     c  n01              eval      plgerr = 'E'
071000070213     c  n01              eval      plgmsg = 'ORA CHIUSURA NON VALIDA'
071100070213     c  n01              eval      *in01 = *on
071200110620     c  n01              eval      *in04 = *on
071300070213     c                   endif
071400070213      *  deve essere un'ora valida
071500070213     c                   if        *in02 and
071600070213     c                             %subst(§rchora:1:2) > '23' or
071700070213     c                             %subst(§rchora:3:2) > '59' or
071800070213     c                             %subst(§rchora:5:2) > '59'
071900070213     c  n01              eval      plgerr = 'E'
072000070213     c  n01              eval      plgmsg = 'ORA CHIUSURA NON VALIDA'
072100070213     c  n01              eval      *in01 = *on
072200110620     c  n01              eval      *in04 = *on
072300070213     c                   endif
072400070514
072500110609      /free
072600110609       //?La data di chiusura distinta su rcd RCH non può essere inferiore a 30 gg da oggi
072700110609       //?in questo caso accetto il rcd ma imposto la data della distinta
072800110609       //?e come ora imposto l'ora di ricezione del record
072900110609       //?è capitato che PDA si è resettato ed ha riportato la data al 1980
073000120920         IF  not *in01 and §RCHdata < %editc(Oggi_10:'X');
073100110609           §RCHdata = %editc(DSTdfv:'X');
073200121015           %subst(§RCHora:1:4)  = %subst(PRCdatora:9:4);
073300121010           PLGmsg   = 'DATA < o > di 10 GG';
073400110609         ENDIF;
073500120920       //?E' capitato anche che il PDA è andato fuori di testa ed ha impostato
073600120920       //?la data a 2514
073700120920         IF  not *in01 and §RCHdata > %editc(Oggi_10o:'X');
073800120920           §RCHdata = %editc(DSTdfv:'X');
073900121015           %subst(§RCHora:1:4)  = %subst(PRCdatora:9:4);
074000121010           PLGmsg  = 'DATA < o > di 10 GG';
074100120920         ENDIF;
074200110609      /end-free
074300110609
074400070514      * imposto il tipo applicazione anche se non previsto dalla ds
074500070514     c                   eval      plgtipappl = 'R'
074600080513
074700080513      * controllo codice SPC
074800080721     c                   eval      *in05 = *off
074900080519     c                   if        §rchcodspc <> *blanks
075000080513     c                   exsr      sr_ctrcodspc
075100080513     c                   endif
075200070130
075300070130     c                   endsr
075400070130
075500070130      *------------------------------------------------------------------------*
075600070130      * ROUTINE ELABORAZIONE RCD "RES"
075700070130      *------------------------------------------------------------------------*
075800070130     c     sr_elabres    begsr
075900070130
076000070131     c                   eval      *in03 = *off
076100070130     c                   eval      fiprcresds = prcdati
076200070201
076300070201      * causale
076400070201      *  deve esistere
076500070201     c                   if        §rescmr <> *blanks
076600081106      * se causale 'AAA' ok...annullamento di ORM Manuale
076700081106     c                             and §rescmr <> 'AAA'
076800070201     c                   clear                   tibs02ds
076900070201     c                   eval      t02mod = 'C'
077000070201     c                   eval      t02sif = knsif
077100070201     c                   eval      t02cod = 'CMR'
077200070201     c                   eval      t02ke1 = §rescmr
077300070201     c                   call      'TIBS02R'
077400070201     c                   parm                    kpjba
077500070201     c                   parm                    tibs02ds
077600070201     c                   if        t02err <> *blanks
077700070201     c  n01              eval      plgerr = 'E'
077800070201     c  n01              eval      plgmsg = 'CAU. MANCATO RIT. INESISTENTE'
077900070201     c  n01              eval      *in01 = *on
078000110620     c  n01              eval      *in04 = *on
078100070201     c                   endif
078200070201     c                   endif
078300070130
078400070130      * p.o. gestione distinta
078500070201     c                   eval      wfgs = §resfgs
078600070201     c                   exsr      sr_ctrfgs
078700070201     c   02              move      §resfgs       plgfgs
078800070130
078900070130      * numero distinta
079000070201     c                   eval      wndc = §resndc
079100070201     c                   exsr      sr_ctrndc
079200070201     c   02              move      §resndc       plgndc
079300070201
079400070201      * controllo se esiste la distinta
079500070201     c   02              exsr      sr_ctrdist
079600070201
079700070201      * controllo se stesso AUT
079800070201     c                   if        *in02 and plgcodaut <> dstpdr
079900070201     c  n01              eval      plgerr = 'E'
080000070201     c  n01              eval      plgmsg = 'AUT. INCONGRUENTE'
080100070201     c  n01              eval      *in01 = *on
080200110620     c  n01              eval      *in04 = *on
080300070201     c                   endif
080400070201
080500070201      * numero ORM
080600070201      *  deve essere numerico
080700070201     c                   eval      w030a = *all'0'
080800070201     c                   eval      %subst(w030a:1:14) = %subst(prcdati:10:14)
080900070201     c                   exsr      sr_testn
081000070201     c                   if        not *in02
081100070201     c  n01              eval      plgerr = 'E'
081200070201     c  n01              eval      plgmsg = 'DATI ID DOC. ORM NON VALIDI'
081300070201     c  n01              eval      *in01 = *on
081400110620     c  n01              eval      *in04 = *on
081500070201     c                   else
081600070201      *  controllo se ORM manuale
081700070521     c                   if        %subst(prcdati:10:8) = *all'0'
081800070521     c                   eval      w006a = %subst(prcdati:18:6)
081900070521     c                   move      w006a         wora
082000070521     c     *eur          test(te)                wora
082100070521     c                   if        %error
082200070201     c  n01              eval      plgerr = 'E'
082300070201     c  n01              eval      plgmsg = 'DATI ID DOC. ORM NON VALIDI'
082400070201     c  n01              eval      *in01 = *on
082500110620     c  n01              eval      *in04 = *on
082600070201     c                   else
082700070201     c                   eval      *in03 = *on
082800080408     c                   eval      plgiddoc = %subst(prcdati:10:14)
082900070201     c                   endif
083000070202     c                   endif
083100070202     c                   endif
083200070130
083300070201      * controllo la chiave ORM
083400070201      * se non è un ORM manuale
083500070201     c                   if        not *in03
083600070202     c                   clear                   ormpoe
083700070202     c                   clear                   ormnsr
083800070202     c                   clear                   ormnor
083900070202     c                   clear                   ormnrv
084000070130      * p.o. emissione
084100070130      *  deve essere numerico
084200070201     c                   eval      w030a = *all'0'
084300070201     c                   movel     §respoe       w030a
084400070201     c                   exsr      sr_testn
084500070201     c                   if        not *in02
084600070130     c  n01              eval      plgerr = 'E'
084700070201     c  n01              eval      plgmsg = 'DATI P.O. EMISSIONE NON VALIDI'
084800070130     c  n01              eval      *in01 = *on
084900110620     c  n01              eval      *in04 = *on
085000070201     c                   else
085100070201     c                   move      §respoe       ormpoe
085200070130     c                   endif
085300070130
085400070130      * numero serie
085500070130      *  deve essere numerico
085600070201     c                   eval      w030a = *all'0'
085700070201     c                   movel     §resnsr       w030a
085800070201     c                   exsr      sr_testn
085900070201     c                   if        not *in02
086000070130     c  n01              eval      plgerr = 'E'
086100070201     c  n01              eval      plgmsg = 'DATI N. SERIE NON VALIDI'
086200070130     c  n01              eval      *in01 = *on
086300110620     c  n01              eval      *in04 = *on
086400070201     c                   else
086500070201     c                   move      §resnsr       ormnsr
086600070130     c                   endif
086700070130
086800070130      * numero orm
086900070130      *  deve essere numerico
087000070201     c                   eval      w030a = *all'0'
087100070201     c                   movel     §resnor       w030a
087200070201     c                   exsr      sr_testn
087300070201     c                   if        not *in02
087400070130     c  n01              eval      plgerr = 'E'
087500070201     c  n01              eval      plgmsg = 'DATI N. ORM NON VALIDI'
087600070130     c  n01              eval      *in01 = *on
087700110620     c  n01              eval      *in04 = *on
087800070201     c                   else
087900070201     c                   move      §resnor       ormnor
088000070130     c                   endif
088100070130      * numero viaggio
088200070130      *  deve essere numerico
088300070201     c                   eval      w030a = *all'0'
088400070201     c                   movel     §resnrv       w030a
088500070201     c                   exsr      sr_testn
088600070201     c                   if        not *in02
088700070130     c  n01              eval      plgerr = 'E'
088800070201     c  n01              eval      plgmsg = 'DATI N. VIAGGIO NON VALIDI'
088900070130     c  n01              eval      *in01 = *on
089000110620     c  n01              eval      *in04 = *on
089100070201     c                   else
089200070201     c                   move      §resnrv       ormnrv
089300070130     c                   endif
089400070131
089500070201      * aggancio l'ORM
089600070201     c     kfnorm        chain(n)  fnorm01l
089700070131     c                   if        %found(fnorm01l)
089800070201      * verifico se la distinta presente sull'ORM è la stessa del PDA
089900070201     c                   if        ormndc <> plgndc
090000070131     c  n01              eval      plgerr = 'E'
090100070131     c  n01              eval      plgmsg = 'DISTINTA INCONGRUENTE'
090200070131     c  n01              eval      *in01 = *on
090300070201     c                   endif
090400070201      * verifico la fase dell'ORM
090500070201     c                   if        ormfao < 400 or ormfao >= 500
090600070131     c  n01              eval      plgerr = 'E'
090700070131     c  n01              eval      plgmsg = 'ESITO/FASE INCONGRUENTI'
090800070131     c  n01              eval      *in01 = *on
090900070201     c                   endif
091000070131     c                   endif
091100070207     c  n01              eval      %subst(plgiddoc:1:3) = §respoe
091200070207     c  n01              eval      %subst(plgiddoc:4:2) = §resnsr
091300070207     c  n01              eval      %subst(plgiddoc:6:7) = §resnor
091400070207     c  n01              eval      %subst(plgiddoc:13:2) = §resnrv
091500070201     c                   endif
091600070213
091700070213      * data conferma
091800070213      *  deve essere numerico
091900070213     c                   eval      w030a = *all'0'
092000070213     c                   movel     §resdata      w030a
092100070213     c                   exsr      sr_testn
092200070213     c                   if        not *in02
092300070213     c  n01              eval      plgerr = 'E'
092400070213     c  n01              eval      plgmsg = 'DATA CONFERMA NON VALIDA'
092500070213     c  n01              eval      *in01 = *on
092600110620     c  n01              eval      *in04 = *on
092700070213     c                   endif
092800070213      *  deve essere una data valida
092900070213     c                   if        *in02
093000070213     c                   clear                   wlbdat
093100070213     c                   move      §resdata      g02inv
093200070213     c                   eval      g02err = '3'
093300070213     c                   call      'XSRDA8'
093400070213     c                   parm                    wlbdat
093500070213     c                   if        g02err = '1'
093600070213     c  n01              eval      plgerr = 'E'
093700070213     c  n01              eval      plgmsg = 'DATA CONFERMA NON VALIDA'
093800070213     c  n01              eval      *in01 = *on
093900110620     c  n01              eval      *in04 = *on
094000070213     c                   endif
094100070213     c                   endif
094200070213
094300070213      * ora conferma
094400070213      *  deve essere numerico
094500070213     c                   eval      w030a = *all'0'
094600070213     c                   movel     §resora       w030a
094700070213     c                   exsr      sr_testn
094800070213     c                   if        not *in02
094900070213     c  n01              eval      plgerr = 'E'
095000070213     c  n01              eval      plgmsg = 'ORA CONFERMA NON VALIDA'
095100070213     c  n01              eval      *in01 = *on
095200110620     c  n01              eval      *in04 = *on
095300070213     c                   endif
095400070213      *  deve essere un'ora valida
095500070213     c                   if        *in02 and
095600070213     c                             %subst(§resora:1:2) > '23' or
095700070213     c                             %subst(§resora:3:2) > '59' or
095800070213     c                             %subst(§resora:5:2) > '59'
095900070213     c  n01              eval      plgerr = 'E'
096000070213     c  n01              eval      plgmsg = 'ORA CONFERMA NON VALIDA'
096100070213     c  n01              eval      *in01 = *on
096200110620     c  n01              eval      *in04 = *on
096300070213     c                   endif
096400110609
096500110609      /free
096600110609       //?La data di immissione esito su rcd RES non può essere inferiore a 30 gg da oggi
096700110609       //?in questo caso accetto il rcd ma imposto la data della distinta
096800110609       //?e come ora imposto l'ora di ricezione del record
096900110609       //?è capitato che PDA si è resettato ed ha riportato la data al 1980
097000120920         IF  not *in01 and §RESdata < %editc(Oggi_10:'X');
097100110609           §RESdata = %editc(DSTdfv:'X');
097200121015           %subst(§RESora:1:4)  = %subst(PRCdatora:9:4);
097300121010           PLGmsg   = 'DATA < o > di 10 GG';
097400110609         ENDIF;
097500120920       //?E' capitato anche che il PDA è andato fuori di testa ed ha impostato
097600120920       //?la data a 2514
097700120920         IF  not *in01 and §RESdata > %editc(Oggi_10o:'X');
097800120920           §RESdata = %editc(DSTdfv:'X');
097900121015           %subst(§RESora:1:4)  = %subst(PRCdatora:9:4);
098000121010           PLGmsg  = 'DATA < o > di 10 GG';
098100120920         ENDIF;
098200110609      /end-free
098300070130
098400070130      * numero colli
098500070130      *  deve essere numerico
098600070201     c                   eval      w030a = *all'0'
098700070201     c                   movel     §resncl       w030a
098800070201     c                   exsr      sr_testn
098900070201     c                   if        not *in02
099000070130     c  n01              eval      plgerr = 'E'
099100070201     c  n01              eval      plgmsg = 'DATI N. COLLI NON VALIDI'
099200070130     c  n01              eval      *in01 = *on
099300110620     c  n01              eval      *in04 = *on
099400070130     c                   endif
099500070130
099600070130      * numero bancali
099700070130      *  deve essere numerico
099800070201     c                   eval      w030a = *all'0'
099900070201     c                   movel     §resbnc       w030a
100000070201     c                   exsr      sr_testn
100100070201     c                   if        not *in02
100200070130     c  n01              eval      plgerr = 'E'
100300070201     c  n01              eval      plgmsg = 'DATI N. BANCALI NON VALIDI'
100400070130     c  n01              eval      *in01 = *on
100500110620     c  n01              eval      *in04 = *on
100600070130     c                   endif
100700070213
100800070213      * data inserimento record
100900070213      *  deve essere numerico
101000070213     c                   eval      w030a = *all'0'
101100070213     c                   movel     §resdatain    w030a
101200070213     c                   exsr      sr_testn
101300070213     c                   if        not *in02
101400070213     c  n01              eval      plgerr = 'E'
101500070213     c  n01              eval      plgmsg = 'DATA INSERIMENTO NON VALIDA'
101600070213     c  n01              eval      *in01 = *on
101700110620     c  n01              eval      *in04 = *on
101800070213     c                   endif
101900070213      *  deve essere una data valida
102000070213     c                   if        *in02
102100070213     c                   clear                   wlbdat
102200070213     c                   move      §resdatain    g02inv
102300070213     c                   eval      g02err = '3'
102400070213     c                   call      'XSRDA8'
102500070213     c                   parm                    wlbdat
102600070213     c                   if        g02err = '1'
102700070213     c  n01              eval      plgerr = 'E'
102800070213     c  n01              eval      plgmsg = 'DATA INSERIMENTO NON VALIDA'
102900070213     c  n01              eval      *in01 = *on
103000110620     c  n01              eval      *in04 = *on
103100070213     c                   endif
103200070213     c                   endif
103300070213
103400070213      * ora conferma
103500070213      *  deve essere numerico
103600070213     c                   eval      w030a = *all'0'
103700070213     c                   movel     §resorain     w030a
103800070213     c                   exsr      sr_testn
103900070213     c                   if        not *in02
104000070213     c  n01              eval      plgerr = 'E'
104100070213     c  n01              eval      plgmsg = 'ORA INSERIMENTO NON VALIDA'
104200070213     c  n01              eval      *in01 = *on
104300110620     c  n01              eval      *in04 = *on
104400070213     c                   endif
104500070213      *  deve essere un'ora valida
104600070213     c                   if        *in02 and
104700070213     c                             %subst(§resorain:1:2) > '23' or
104800070213     c                             %subst(§resorain:3:2) > '59' or
104900070213     c                             %subst(§resorain:5:2) > '59'
105000070213     c  n01              eval      plgerr = 'E'
105100070213     c  n01              eval      plgmsg = 'ORA INSERIMENTO NON VALIDA'
105200070213     c  n01              eval      *in01 = *on
105300110620     c  n01              eval      *in04 = *on
105400070213     c                   endif
105500110609
105600110609      /free
105700110609       //?La data di inserimento esito su rcd RES non può essere inferiore a 30 gg da oggi
105800110609       //?in questo caso accetto il rcd ma imposto la data della distinta
105900110609       //?e come ora imposto l'ora di ricezione del record
106000110609       //?è capitato che PDA si è resettato ed ha riportato la data al 1980
106100120920         IF  not *in01 and §RESdatain < %editc(Oggi_10:'X');
106200110609           §RESdatain = %editc(DSTdfv:'X');
106300121015           %subst(§RESorain:1:4)  = %subst(PRCdatora:9:4);
106400121010           PLGmsg     = 'DATA < o > di 10 GG';
106500110609         ENDIF;
106600120920       //?E' capitato anche che il PDA è andato fuori di testa ed ha impostato
106700120920       //?la data a 2514
106800120920         IF  not *in01 and §RESdatain > %editc(Oggi_10o:'X');
106900120920           §RESdatain = %editc(DSTdfv:'X');
107000121015           %subst(§RESorain:1:4)  = %subst(PRCdatora:9:4);
107100121010           PLGmsg     = 'DATA < o > di 10 GG';
107200120920         ENDIF;
107300110609      /end-free
107400070130
107500070130      * inserito da SPC
107600070130      *  deve essere 'S' o blank
107700070130     c                   if        §resflgspc <> 'S' and
107800070130     c                             §resflgspc <> *blanks
107900070130     c  n01              eval      plgerr = 'E'
108000070201     c  n01              eval      plgmsg = 'FLAG INS. DA SPC NON VALIDO'
108100070130     c  n01              eval      *in01 = *on
108200110620     c  n01              eval      *in04 = *on
108300070130     c                   endif
108400070514
108500070514      * imposto il tipo applicazione anche se non previsto dalla ds
108600070514     c                   eval      plgtipappl = 'R'
108700070130
108800070130     c                   endsr
108900070130
108901140129      *------------------------------------------------------------------------*
108902140129      * ROUTINE ELABORAZIONE RCD "ROR"
108903140129      *------------------------------------------------------------------------*
108904140129     c     sr_elabror    begsr
108905140129
108906140129     c                   eval      fiprcrords = prcdati
108907140129
108908140129      * p.o. gestione distinta
108909140129     c                   eval      wfgs = §rorfgs
108910140129     c                   exsr      sr_ctrfgs
108911140129     c   02              move      §rorfgs       plgfgs
108912140129
108913140129      * numero distinta
108914140129     c                   eval      wndc = §rorndc
108915140129     c                   exsr      sr_ctrndc
108916140207     c   02              move      §rorndc       plgndc
108917140129
108918140129      * controllo se esiste la distinta
108919140129     c   02              exsr      sr_ctrdist
108920140129
108921140129      * tipo applicazione
108922140129      * può essere C=consegne R=ritiri
108923140207     c                   if        §rortpiddo <> 'C' and §rortpiddo <> 'R'
108924140129     c  n01              eval      plgerr = 'E'
108925140129     c  n01              eval      plgmsg = 'DATI NON VALIDI'
108926140129     c  n01              eval      *in01 = *on
108927140129     c  n01              eval      *in04 = *on
108928140129     c                   else
108929140207     c                   eval      plgtipappl = §rortpiddo
108930140129     c                   endif
108931140129
108932140129      * id documento
108933140129      *  solo ritiro
108934140207     c                   if        §rortpiddo = 'R'
108935140129      *  deve essere numerico
108936140129     c                   eval      w030a = *all'0'
108937140129     c                   eval      %subst(w030a:1:14) = %subst(§roriddoc:1:14)
108938140129     c                   exsr      sr_testn
108939140129     c                   if        not *in02
108940140129     c  n01              eval      plgerr = 'E'
108941140129     c  n01              eval      plgmsg = 'DATI ID DOC. ORM NON VALIDI'
108942140129     c  n01              eval      *in01 = *on
108943140129     c  n01              eval      *in04 = *on
108944140129     c                   else
108945140129      *  deve essere un n. di ORM valido
108946140129     c                   move      §rorpoe       ormpoe
108947140129     c                   move      §rornsr       ormnsr
108948140129     c                   move      §rornor       ormnor
108949140129     c                   move      §rornrv       ormnrv
108950140129     c     kfnorm        chain(n)  fnorm01l
108951140129     c                   if        not %found(fnorm01l)
108952140129     c  n01              eval      plgerr = 'E'
108953140129     c  n01              eval      plgmsg = 'ORM INESISTENTE'
108954140129     c  n01              eval      *in01 = *on
108955140129     c  n01              eval      *in04 = *on
108956140129     c                   endif
108957140129     c                   endif
108958140129     c  n01              eval      plgiddoc = §roriddoc
108959140129     c                   endif
108960140129
108961140129      * data conferma
108962140129      *  deve essere numerico
108963140129     c                   eval      w030a = *all'0'
108964140129     c                   movel     §rordata      w030a
108965140129     c                   exsr      sr_testn
108966140129     c                   if        not *in02
108967140129     c  n01              eval      plgerr = 'E'
108968140129     c  n01              eval      plgmsg = 'DATA CONFERMA NON VALIDA'
108969140129     c  n01              eval      *in01 = *on
108970140129     c  n01              eval      *in04 = *on
108971140129     c                   endif
108972140129      *  deve essere una data valida
108973140129     c                   if        *in02
108974140129     c                   clear                   wlbdat
108975140129     c                   move      §rordata      g02inv
108976140129     c                   eval      g02err = '3'
108977140129     c                   call      'XSRDA8'
108978140129     c                   parm                    wlbdat
108979140129     c                   if        g02err = '1'
108980140129     c  n01              eval      plgerr = 'E'
108981140129     c  n01              eval      plgmsg = 'DATA CONFERMA NON VALIDA'
108982140129     c  n01              eval      *in01 = *on
108983140129     c  n01              eval      *in04 = *on
108984140129     c                   endif
108985140129     c                   endif
108986140129
108987140129      * ora conferma
108988140129      *  deve essere numerico
108989140129     c                   eval      w030a = *all'0'
108990140129     c                   movel     §rorora       w030a
108991140129     c                   exsr      sr_testn
108992140129     c                   if        not *in02
108993140129     c  n01              eval      plgerr = 'E'
108994140129     c  n01              eval      plgmsg = 'ORA CONFERMA NON VALIDA'
108995140129     c  n01              eval      *in01 = *on
108996140129     c  n01              eval      *in04 = *on
108997140129     c                   endif
108998140129      *  deve essere un'ora valida
108999140129     c                   if        *in02 and
109000140129     c                             %subst(§rorora:1:2) > '23' or
109001140129     c                             %subst(§rorora:3:2) > '59' or
109002140129     c                             %subst(§rorora:5:2) > '59'
109003140129     c  n01              eval      plgerr = 'E'
109004140129     c  n01              eval      plgmsg = 'ORA CONFERMA NON VALIDA'
109005140129     c  n01              eval      *in01 = *on
109006140129     c  n01              eval      *in04 = *on
109007140129     c                   endif
109008140129      /free
109009140129       //?La data di conferma ORM su rcd OK non può essere inferiore a 30 gg da oggi
109010140129       //?in questo caso accetto il rcd ma imposto la data della distinta
109011140129       //?e come ora imposto l'ora di ricezione del record
109012140129       //?è capitato che PDA si è resettato ed ha riportato la data al 1980
109013140129         IF  not *in01 and §rordata < %editc(Oggi_10:'X');
109014140207           §rordata = %editc(DSTdfv:'X');
109015140129           §rorora  = %subst(PRCdatora:9:6);
109016140129           PLGmsg  = 'DATA < o > di 10 GG';
109017140129         ENDIF;
109018140129       //?E' capitato anche che il PDA è andato fuori di testa ed ha impostato
109019140129       //?la data a 2514
109020140129         IF  not *in01 and §rordata > %editc(Oggi_10o:'X');
109021140129           §rordata = %editc(DSTdfv:'X');
109022140129           %subst(§rorora:1:4)  = %subst(PRCdatora:9:4);
109023140129           PLGmsg  = 'DATA < o > di 10 GG';
109024140129         ENDIF;
109025140129      /end-free
109034140129
109035140129      * ora stimata
109036140129     c                   clear                   Worasti
109037140129     c                   eval      w030a = *all'0'
109038140129     c                   movel     §rororasti    w030a
109039140129     c                   exsr      sr_testn
109040140129     c                   if        not *in02
109041140129     c  n01              eval      plgerr = 'E'
109042140129     c  n01              eval      plgmsg = 'ORA STIMATA NON VALIDA'
109043140129     c  n01              eval      *in01 = *on
109044140129     c  n01              eval      *in04 = *on
109045140129      *
109047140129     c                   if        %subst(§rorora:1:2) > '23' or
109048140129     c                             %subst(§rorora:3:2) > '59' or
109049140129     c                             %subst(§rorora:5:2) > '59'
109050140129     c  n01              eval      plgerr = 'E'
109051140129     c  n01              eval      plgmsg = 'ORA STIMATA NON VALIDA'
109052140129     c  n01              eval      *in01 = *on
109053140129     c  n01              eval      *in04 = *on
109054140129     c                   endif
109055140129
109056140129     c                   else
109057140129     c                   move      §rororasti    Worasti
109058140129     c                   endif
109059140129
109060140129      * ordinamento
109061140129     c                   clear                   Wsort
109062140129     c                   eval      w030a = *all'0'
109063140129     c                   movel     §rorsort      w030a
109064140129     c                   exsr      sr_testn
109065140129     c                   if        not *in02
109066140129     c  n01              eval      plgerr = 'E'
109067140129     c  n01              eval      plgmsg = 'ORDINAMENTO NON VALIDO'
109068140129     c  n01              eval      *in01 = *on
109069140129     c  n01              eval      *in04 = *on
109079140129
109080140129     c                   else
109081140129     c                   move      §rorsort      Wsort
109082140129     c                   endif
109083140129
109084140129     c                   endsr
109085140129
109086070130      *------------------------------------------------------------------------*
109100070130      * ROUTINE PER AGGIORNARE I DATI EFFETTIVI
109200070130      *------------------------------------------------------------------------*
109300070130     c     sr_file       begsr
109400070130
109500070130     c                   select
109600070130      * tipo record 'RCH'
109700070130     c                   when      plgtiporcd = 'RCH'
109800080327      * aggiorno fiqdt
109900070201     c                   exsr      sr_fiqdt
110000080327      * devo cancellare i dati da FIPDO e da FIPRO
110100070201     c                   exsr      sr_fipdo
110200080319     c                   exsr      sr_fipro
110300070130
110400070130      * tipo record 'RES'
110500070131     c                   when      plgtiporcd = 'RES' and
110600070131     c                             not *in03
110700070201     c                   exsr      sr_fnorm
110800070130
110900070130     c                   endsl
111000070130
111100070130     c                   endsr
111200070202
111300070202      *------------------------------------------------------------------------*
111400070202      * ROUTINE SCRITTURA FILE LOG
111500070202      *------------------------------------------------------------------------*
111600070202     c     sr_wrtlog     begsr
111700070202
111800070202     c                   eval      plgprfc = prcprfc
111900070202     c                   eval      plgiddisp = prciddisp
112000070202     c                   eval      plgdatora = prcdatora
112100070202     c                   eval      plgdati = prcdati
112200070205     c                   write     fiplg000
112300070202
112400070202     c                   endsr
112500070201
112600080415      *------------------------------------------------------------------------*
112700080415      * ROUTINE SCRITTURA FILE PER PROSPETTO ORM
112800080415      *------------------------------------------------------------------------*
112900080415     c     sr_Prospetto  begsr
113000080415
113100080415      * se ricevo un RES
113200080417    1c                   If        plgtiporcd = 'RES'
113300080415      * cerco l'ORM da scrivere o aggiornare nel file dettaglio prospetto ORM
113400080415     c     kfirpd        chain     Firpd02l
113500080415
113600080417    2c                   if        not %found(firpd02l)
113700080415     c                   clear                   firpd000
113800080415     c                   eval      rpdfgs = plgfgs
113900080415     c                   eval      rpdnfv = plgndc
114000080415     c                   eval      rpdpdr = plgcodaut
114100080415     c                   eval      rpdiddoc = plgiddoc
114200080415      * è un manuale ???
114300080417    3c                   if        %subst(plgiddoc:1:8) = *all'0'
114400080417     c                   eval      rpdman = 'S'
114500080415     c                   eval      rpdrsr = §resrsr
114600080415     c                   eval      rpdinr = §resinr
114700080417    3c                   endif
114800080417     c                   move      §resncl       rpdncl
114900080417     c                   move      §resbnc       rpdbnc
115000080415     c                   eval      rpdcmr = §rescmr
115100080417     c                   movel     §resdata      rpddtr
115200080417     c                   move      §resora       rpdorr
115300080415     c                   eval      rpdflgspc = §resflgspc
115400080417     c                   movel     §resdtorin    rpddtorin
115500081106      * non scrivo record se orm manuale annullato
115600081106     c                   if        rpdman <> 'S' or
115700081106     c                             (rpdman = 'S' and §rescmr <> 'AAA')
115800080417     c                   write     firpd000
115900081106     c                   endif
116000080417    2c                   endif
116100080417      * se trovato record se data ora modifica maggiore di quella registrata aggiorno
116200080417    2c                   if        %found(firpd02l) and
116300080417     c                             rpddtorin < §resdtorin
116400081106      * ma solo se non è un orm manuale annullato
116500081106     c                             and (rpdman <> 'S' or
116600081106     c                                 (rpdman = 'S' and §rescmr <> 'AAA'))
116700080417     c                   move      §resncl       rpdncl
116800080417     c                   move      §resbnc       rpdbnc
116900080417     c                   eval      rpdcmr = §rescmr
117000080505     c                   movel     §resdtorin    rpddtorin
117100080505     c                   eval      rpdflgspc = §resflgspc
117200080417     c                   update    firpd000
117300080417    2c                   endif
117400081106
117500081106      * se trovato record e orm manuale annullato cancello il record
117600081106    2c                   if        %found(firpd02l) and
117700081106     c                             rpdman = 'S' and §rescmr = 'AAA'
117800081106     c                   delete    firpd000
117900081106    2c                   endif
118000080417
118100080417    1c                   endif
118200080417
118300080417      * se ricevo un RCH
118400080417    1c                   If        plgtiporcd = 'RCH'
118500080417      * cerco se la distinta è presente
118600080417     c     kfirpt        chain     Firpt01l
118700080417
118800080417    2c                   if        not %found(firpt01l)
118900080417     c                   clear                   firpt000
119000080417     c                   eval      rptfgs = plgfgs
119100080417     c                   eval      rptnfv = plgndc
119200080417     c                   eval      rptpdr = plgcodaut
119300080421     c                   eval      rptdfv = dstdfv
119400080418     c                   eval      rptfpp = dstfpp
119500080418     c                   eval      rpttpm = dsttpm
119600080417     c                   move      §rchncl       rpttco
119700080417     c                   move      §rchbnc       rpttbn
119800080417     c                   movel     §rchdata      rptdch
119900080417     c                   move      §rchora       rptoch
120000080417     c                   write     firpt000
120100080417    2c                   endif
120200080417      * se trovato record se data ora modifica maggiore di quella registrata aggiorno
120300080417    2c                   if        %found(firpt01l)
120400080417     c                   movel     rptdch        w_dtaorach
120500080417     c                   move      rptoch        w_dtaorach
120600080417    3c                   If        w_dtaorach < §rchdatora
120700080417     c                   move      §rchncl       rpttco
120800080417     c                   move      §rchbnc       rpttbn
120900080417     c                   movel     §rchdata      rptdch
121000080417     c                   move      §rchora       rptoch
121100080417     c                   update    firpt000
121200080417    3c                   endif
121300080417    2c                   endif
121400080417
121500080417    1c                   endif
121600080415
121700080415     c                   endsr
121800080319      *------------------------------------------------------------------------*
121900080319      * ROUTINE SCRITTURA FILE OK RICEVUTI
122000080319      *------------------------------------------------------------------------*
122100080319     c     sr_wrtpro     begsr
122200080319
122300080730      * verifico se estiste già un ok per questo ORM + DISTINTA + AUT
122400080319
122500140128     c     kfipro        chain     fipro11l
122600080319
122700080319      * se il record esiste e l'ok che ho ricevuto ha un idrow minore o uguale
122701140207     c                   if        %found(fipro11l)
122702140207     c                   movel     prodati       fiprookds
122703140207     c                   endif
122800080319      * a quello del file vado a fine
122900140207     c                   if        %found(fipro11l) and widrow <= §proidrow
123000080319     c                   leavesr
123100080319     c                   endif
123200080319
123201140128     c                   eval      prodatora = §OKDATORA
123300080319      * valorizzo i campi dell'ok
123400140128     c                   eval      §proidrow = widrow
123600140128     c                   eval      §proflgass = §OKFLGASS
123601140224     c                   eval      §PROFLGVIS = ' '
123602140128     c                   eval      §pronotass = §OKnotass
123603140128     c                   eval      Prodati    = fiprookds
123700080319
123800140207     c                   if        not %found(fipro11l)
123900080319     c                   eval      profgs = plgfgs
124000080319     c                   eval      prondc = plgndc
124100080730     c                   eval      procodaut = plgcodaut
124200080319     c                   clear                   proatb
124300080319     c                   eval      propoe = ormpoe
124400080319     c                   eval      pronsr = ormnsr
124500080319     c                   eval      pronor = ormnor
124600080319     c                   eval      pronrv = ormnrv
124601140128     c                   eval      protrd = 'OK '
124700140128     c                   write     fipro100
124800080319     c                   else
124900140128     c                   update    fipro100
125000080319     c                   endif
125100080319
125200080319     c                   endsr
125300080319
125301140129      *------------------------------------------------------------------------*
125302140129      * ROUTINE SCRITTURA FILE ROR RICEVUTI
125303140129      *------------------------------------------------------------------------*
125304140129     c     sr_wrtproror  begsr
125305140129
125306140129      * verifico se estiste già un ok per questo ORM + DISTINTA + AUT
125307140129
125308140129     c     kfipro        chain     fipro11l
125315140129
125316140129     c                   eval      prodatora = §rorDATORA
125317140207      * valorizzo i campi del ror
125319140207     c                   move      §rorORASTI    §PROORASTI
125320140207     c                   move      §rorSORT      §PROSORT
125321140129     c                   eval      Prodati    = fiprorords
125322140129
125323140129     c                   if        not %found(fipro11l)
125324140129     c                   eval      profgs = plgfgs
125325140129     c                   eval      prondc = plgndc
125326140129     c                   eval      procodaut = plgcodaut
125327140129     c                   clear                   proatb
125328140129     c                   eval      propoe = ormpoe
125329140129     c                   eval      pronsr = ormnsr
125330140129     c                   eval      pronor = ormnor
125331140129     c                   eval      pronrv = ormnrv
125332140129     c                   eval      protrd = 'ROR'
125333140129     c                   write     fipro100
125334140129     c                   else
125335140129     c                   update    fipro100
125336140129     c                   endif
125337140129
125338140129     c                   endsr
125339140129
125400070201      *------------------------------------------------------------------------*
125500070201      * ROUTINE PER CONTROLLARE IL P.O. GESTIONE DISTINTA
125600070201      *------------------------------------------------------------------------*
125700070201     c     sr_ctrfgs     begsr
125800070201
125900070201     c                   eval      w030a = *all'0'
126000070201     c                   movel     wfgs          w030a
126100070201     c                   exsr      sr_testn
126200070201     c                   if        not *in02
126300070201     c  n01              eval      plgerr = 'E'
126400070201     c  n01              eval      plgmsg = 'DATI P.O. DISTINTA NON VALIDI'
126500070201     c  n01              eval      *in01 = *on
126600110620     c  n01              eval      *in04 = *on
126700070201     c                   endif
126800070201
126900070201     c                   endsr
127000070201
127100070201      *------------------------------------------------------------------------*
127200070201      * ROUTINE PER CONTROLLARE IL NUMERO DISTINTA
127300070201      *------------------------------------------------------------------------*
127400070201     c     sr_ctrndc     begsr
127500070201
127600070201     c                   eval      w030a = *all'0'
127700070201     c                   movel     wndc          w030a
127800070201     c                   exsr      sr_testn
127900070201     c                   if        not *in02
128000070201     c  n01              eval      plgerr = 'E'
128100070201     c  n01              eval      plgmsg = 'DATI NUM. DISTINTA NON VALIDI'
128200070201     c  n01              eval      *in01 = *on
128300110620     c  n01              eval      *in04 = *on
128400070201     c                   endif
128500070201
128600070201     c                   endsr
128700070201
128800070201      *------------------------------------------------------------------------*
128900070201      * ROUTINE PER CONTROLLARE L'EISISTENZA DELLA DISTINTA
129000070201      *------------------------------------------------------------------------*
129100070201     c     sr_ctrdist    begsr
129200070201
129300080226     c                   eval      dstnpg = 4
129400070201     c                   move      wndc          dstnfv
129500070201     c                   move      wfgs          dstfgs
129600080317     c                   clear                   ddstflr
129700080226     c     kfidst        chain     fidst01l
129800080226     c                   if        not %found(fidst01l) or dstatb <> *blanks
129900070201     c  n01              eval      plgerr = 'E'
130000070201     c  n01              eval      plgmsg = 'DISTINA INESISTENTE'
130100070201     c  n01              eval      *in01 = *on
130200110620     c  n01              eval      *in04 = *on
130300080317     c                   leavesr
130400070201     c                   endif
130500080317     c                   eval      ddstflr = dstflr
130600070201
130700070201     c                   endsr
130800080513
130900080513      *------------------------------------------------------------------------*
131000080513      * ROUTINE PER CONTROLLARE CODICE SPC
131100080513      *------------------------------------------------------------------------*
131200080513     c     sr_ctrcodspc  begsr
131300080513
131400080519     c                   eval      *in05 = *off
131500080924     c                   eval      xx = 1
131600080924     c     §rchcodspc    lookup    codspc(xx)                             30
131700080514     c                   if        not *in30
131800080519     c                   eval      *in05 = *on
131900080924     c                   else
132000080924     c                   eval      §rchspccod = codspc(xx)
132100080924     c                   eval      §rchspcdes = desspc(xx)
132200080514     c                   endif
132300080513
132400080513     c                   endsr
132500070201
132600070201      *------------------------------------------------------------------------*
132700070201      * ROUTINE PER CONTROLLARE SE IL CAMPO E' NUMERICO
132800070201      *------------------------------------------------------------------------*
132900070201     c     sr_testn      begsr
133000070201
133100070201     c                   eval      *in02 = *off
133200070201     c                   testn                   w030a                02
133300070201
133400070201     c                   endsr
133500070201
133600070201      *------------------------------------------------------------------------*
133700070201      * ROUTINE PER AGGIORNARE FIQDT
133800070201      *------------------------------------------------------------------------*
133900070201     c     sr_fiqdt      begsr
134000070201
134100070201      * imposto la data e ora attuale
134200070201     c                   time                    w0140
134300070201     c                   move      w0140         udate8
134400070201     c                   movel     w0140         hhmm
134500070201     c                   z-add     udate8        g02dat
134600070201     c                   move      *blank        g02err
134700070201     c                   call      'XSRDA8'
134800070201     c                   parm                    wlbdat
134900070201     c                   z-add     g02inv        dateu8
135000070201
135100070201      * aggancio FIQDT
135200070202     c     kfiqdt        chain(e)  fiqdt01l
135300070205      * se rcd allocato flag di comodo per poi fare il rollback
135400070202     c                   if        %error
135500070202     c                   eval      wfiqdt = *on
135600070202     c                   leavesr
135700070202     c                   endif
135800070201     c                   if        %found(fiqdt01l)
135900070201      * bancali ritirati
136000081106      * se distinta a PDA non in test sommo i bancali rientrati a quando già presente su
136100081106      * fiqdt
136200110316     c**!!!              if        dstpda = 'E' and §dsttstpda = *blanks
136300110316     c**!!!              add       wbnc          qdtnbnri
136400110316     c**!!!              eval      qdtorari = hhmm
136500110316     c**!!!              eval      qdtdairi = dateu8
136600110316     c**!!!              eval      qdtfnbri = 'S'
136700110316     c**!!!              endif
136800080317     c****               if        wbnc <> qdtnbnri
136900080317     c****               eval      qdtnbnri = wbnc
137000080317     c****               eval      qdtorari = hhmm
137100080317     c****               eval      qdtdairi = dateu8
137200080317     c****               endif
137300070201      * colli ritirati
137400070201     c                   if        wncl <> qdtnclrit
137500070201     c                   eval      qdtnclrit = wncl
137600081112     c                   eval      qdtfncrit = 'S'
137700070201     c                   endif
137800070201
137900070205     c                   update    fiqdt000
138000070201     c                   endif
138100070201
138200070201     c                   endsr
138300080327
138400080327      *------------------------------------------------------------------------*
138500080327      * ROUTINE PER SCRIVERE LA FASE RCH SULLA DISTINTA
138600080327      *------------------------------------------------------------------------*
138700080327     c     sr_fase       begsr
138800080327
138900080327     c                   clear                   fior90ds
139000080327     c                   eval      or90tla = 'W'
139100080327     c                   eval      or90fas = 'RCH'
139200080327     c                   eval      or90fgs = plgfgs
139300080327     c                   eval      or90ndc = plgndc
139400080327     c                   eval      or90flgins = 'P'
139500080327     c                   eval      or90comit = '1'
139600080924     c                   eval      kpjbu = fidsfrchds
139700080327     c                   call      'FIOR90R'
139800080327     c                   parm                    kpjba
139900080327     c                   parm                    fior90ds
140000080327      * se torna errore imposto flag per poi fare il rollback
140100080327     c                   if        or90err <> *blanks
140200080327     c                   eval      wfase = *on
140300080327     c                   leavesr
140400080327     c                   endif
140500080327
140600080327     c                   endsr
140700070201
140800070201      *------------------------------------------------------------------------*
140900080319      * ROUTINE PER CANCELLARE RCD DA FIPDO
141000070201      *------------------------------------------------------------------------*
141100070201     c     sr_fipdo      begsr
141200080319
141300080319      * Cancellazione PDO
141400070201     c     kfiqdt        setll     fipdo01t
141500070201     c                   do        *hival
141600070205     c     kfiqdt        reade(e)  fipdo01t
141700070205      * se rcd allocato flag di comodo per poi fare il rollback
141800070205     c                   if        %error
141900070205     c                   eval      wfipdo = *on
142000070205     c                   leave
142100070205     c                   endif
142200070201     c                   if        %eof(fipdo01t)
142300070201     c                   leave
142400070201     c                   endif
142500070205     c                   delete    fipdo000
142600070201     c                   enddo
142700080319
142800070201     c                   endsr
142900070201
143000080319      *------------------------------------------------------------------------*
143100080319      * ROUTINE PER CANCELLARE RCD DA FIPRO
143200080319      *------------------------------------------------------------------------*
143300080319     c     sr_fipro      begsr
143400080319
143500080319      * Cancellazione PRO
143600140207     c     kfiqdt        setll     fipro11l
143700080319     c                   do        *hival
143800140207     c     kfiqdt        reade(e)  fipro11l
143900080319      * se rcd allocato flag di comodo per poi fare il rollback
144000080319     c                   if        %error
144100080319     c                   eval      wfipro = *on
144200080319     c                   leave
144300080319     c                   endif
144400140207     c                   if        %eof(fipro11l)
144500080319     c                   leave
144600080319     c                   endif
144700140207     c                   delete    fipro100
144800080319     c                   enddo
144900080319
145000080319     c                   endsr
145100080319
145200070201      *------------------------------------------------------------------------*
145300070201      * ROUTINE PER AGGIORNARE FNORM/FNORF
145400070201      *------------------------------------------------------------------------*
145500070201     c     sr_fnorm      begsr
145600070201
145700070201      * imposto la data e ora attuale
145800070201     c                   time                    w0140
145900070201     c                   move      w0140         udate8
146000070201     c                   z-add     udate8        g02dat
146100070201     c                   move      *blank        g02err
146200070201     c                   call      'XSRDA8'
146300070201     c                   parm                    wlbdat
146400070201     c                   z-add     g02inv        dateu8
146500070201
146600070201      * aggancio FNORM
146700070202     c     kfnorm        chain(e)  fnorm01l
146800070205      * se rcd allocato flag di comodo per poi fare il rollback
146900070202     c                   if        %error
147000070202     c                   eval      wfnorm = *on
147100070202     c                   leavesr
147200070202     c                   endif
147300070201     c                   if        %found(fnorm01l)
147400080219      * aggancio l'ultima fase dell'ORM
147500080219      * per recuperare l'eventuale data/ora dell'ultimo esito avuto da PDA
147600080219     c                   clear                   dorf01
147700080219     c                   clear                   savdfo
147800080219     c                   clear                   savofo
147900080219     c     kfnorf        chain     fnorf01l
148000080219     c                   if        %found(fnorf01l)
148100080219     c                   eval      dorf01 = orfflo
148200080320      * mi salvo orfass della fase precedente
148300080320     c                   eval      save_§orfass = §orfass
148400080320
148500080219     c                   endif
148600080219     c                   if        §orfesito <> *blanks
148700080219     c                   eval      savdfo = %int(%subst(§orfesito:1:8))
148800080219     c                   eval      savofo = %int(%subst(§orfesito:9:6))
148900080219     c                   endif
149000080212
149100070201     c                   select
149200070201      * esito fatto
149300070201     c                   when      §rescmr = *blanks
149400070201     c                   eval      ormfao = 420
149500070201      * esito non fatto
149600070201     c                   when      §rescmr <> *blanks
149700070201     c                   eval      ormfao = 410
149800070201     c                   endsl
149900070201
150000070213     c                   move      §resdata      ormdfo
150100070213     c                   move      §resora       ormofo
150200070716      * imposto il campo ORMETI
150300070716     c                   clear                   tibs02ds
150400070716     c                   eval      t02mod = 'C'
150500070716     c                   eval      t02sif = knsif
150600070716     c                   eval      t02cod = 'FAR'
150700070716     c                   movel(p)  ormfao        t02ke1
150800070716     c                   call      'TIBS02R'
150900070716     c                   parm                    kpjba
151000070716     c                   parm                    tibs02ds
151100070716     c                   eval      dfar = t02uni
151200070716     c                   eval      ormeti = d§farass
151300070201      * scrivo fase
151400070201     c                   clear                   fnorf000
151500080219     c                   clear                   dorf01
151600070201     c                   eval      orfpoe = ormpoe
151700070201     c                   eval      orfnsr = ormnsr
151800070201     c                   eval      orfnor = ormnor
151900070201     c                   eval      orfnrv = ormnrv
152000070201     c                   eval      orfpog = plgfgs
152100070201     c                   eval      orfdae = ormdfo
152200070201     c                   eval      orfore = ormofo
152300070201     c                   eval      orffar = ormfao
152400070221     c                   eval      orfpue = prcprfc
152500070201     c                   eval      orfcar = §rescmr
152600070201     c                   eval      orffgs = plgfgs
152700070201     c                   eval      orfndc = plgndc
152800070201     c                   eval      orfddc = ormddc
152900080219     c                   eval      §orfesito = §resdtorin
153000080320      * imposto  orfass della fase precedente
153100080320     c                   eval      §orfass =  save_§orfass
153200080219     c                   eval      orfflo = dorf01
153300070205     c                   write     fnorf000
153400080409      * scrivo NOTE
153500080409      * solo se ci sono
153600080409     c                   if        §resnote <> *blanks
153700080409     c                   clear                   wprg
153800080409      * cerco ultimo progressivo relativo alla stessa fase
153900080409     c     kfnorn        setll     fnorn02l
154000080409     c                   do        *hival
154100080409     c     kfnorn        reade     fnorn02l
154200080409     c                   if        %eof(fnorn02l)
154300080409     c                   leave
154400080409     c                   endif
154500080409     c                   eval      wprg = ornprg
154600080409     c                   enddo
154700080409      * scrivo la nota
154800080409     c                   add       1             wprg
154900080409     c                   clear                   fnorn000
155000080409     c                   eval      ornpoe = ormpoe
155100080409     c                   eval      ornnsr = ormnsr
155200080409     c                   eval      ornnor = ormnor
155300080409     c                   eval      ornnrv = ormnrv
155400080409     c                   eval      ornfar = ormfao
155500080409     c                   movel     §orfesito     orndai
155600080409     c                   move      §orfesito     ornori
155700080409     c                   eval      ornprg = wprg
155800080409     c                   eval      ornpue = orfpue
155900080409     c                   eval      ornpon = plgfgs
156000080409     c                   eval      ornsns = 'I'
156100080409     c                   eval      ornnot = §resnote
156200080409     c                   eval      ornftr = 'S'
156300080409     c                   eval      orndtr = dateu8
156400080409     c                   write     fnorn000
156500080409     c                   endif
156600070202      * aggiorno ORM
156700080212      * solo se la data/ora ultima fase dell'ORM è inferiore a quella che sto per scrivere
156800080212     c                   if        ormdfo > savdfo or
156900080214     c                             (ormdfo = savdfo and ormofo >= savofo)
157000070205     c                   update    fnorm000
157100080212     c                   endif
157200080227      * scrivo la partita iva e il codice fiscale nel file di appoggio
157300080320      *
157400080320      * verifico se esiste il file di appoggio fipci02l
157500080320     c     kfnorm        chain     fipci02l
157600080320      * nel casi in cui almeno uno dei due dati è valorizzato aggiorno o scrivo
157700080227     c                   if        §rescodfis <> *blanks or §respiva <> *blanks
157800080227     c                   if        %found(fipci02l)
157900080227     c                   eval      pcicdf = §rescodfis
158000080227     c                   eval      pciiso = §resisoiva
158100080228     c                   eval      pcicpi = §respiva
158200080227     c                   clear                   pciesito
158300080227     c                   update    fipci000
158400080227     c                   else
158500080227     c                   clear                   fipci000
158600080227     c                   eval      pcipoe = ormpoe
158700080227     c                   eval      pcinsr = ormnsr
158800080227     c                   eval      pcinor = ormnor
158900080227     c                   eval      pcinrv = ormnrv
159000080227     c                   eval      pcicdf = §rescodfis
159100080227     c                   eval      pciiso = §resisoiva
159200080228     c                   eval      pcicpi = §respiva
159300080228     c                   write     fipci000
159400080227     c                   endif
159500080227     c                   endif
159600080320
159700080321      * nel casi in cui entrambi sono a blank ed il record esiste annullo il rcd
159800080320     c                   if        §rescodfis = *blanks and §respiva = *blanks
159900080320     c                             and %found(fipci02l)
160000080321     c                   eval      pciatb = 'A'
160100080321     c                   update    fipci000
160200080320     c                   endif
160300080320
160400070201     c                   endif
160500070201
160600070201     c                   endsr
160700080519
160800080519      *------------------------------------------------------------------------*
160900080519      * ROUTINE PER INVIARE MAIL SE ERRORE CODICE SPC
161000080519      *------------------------------------------------------------------------*
161100080519     c     sr_mail       begsr
161200080519
161300080519      * Reperimento tab. "MRA" se non già recuperata
161400080519     c                   if        not $okmra
161500080519     c                   clear                   dmradan
161600080519     c                   clear                   tibs02ds
161700080519     c                   eval      t02mod = 'C'
161800080519     c                   eval      t02sif = knsif
161900080519     c                   eval      t02cod = 'MRA'
162000080519     c                   eval      t02ke1 = sdspgm
162100080519     c                   call      'TIBS02R'
162200080519     c                   parm                    kpjba
162300080519     c                   parm                    tibs02ds
162400080519     c                   if        t02err = *blanks
162500080519     c                   eval      dmradan = t02uni
162600080519     c                   eval      $okmra = *on
162700080519     c                   endif
162800080519     c                   endif
162900080519
163000080519      * Override al file di stampa ed apertura dello stesso
163100080519     c                   if        not %open(prtemail)
163200080519     c                   exsr      sr_openprtf
163300080519     c                   endif
163400080522
163500080522      * Stampa errore
163600080522     c                   if        *in05
163700080522     c                   eval      o_testo = 'trovato codice SPC non valido in +
163800080522     c                                        chiusura distinta ' +
163900080522     c                                        %editc(plgfgs:'X') + '-' +
164000080522     c                                        %editc(plgndc:'X') +
164100080522     c                                        ' in data ' +
164200080522     c                                        %subst(prcdatora:7:2) + '/' +
164300080522     c                                        %subst(prcdatora:5:2) + '/' +
164400080522     c                                        %subst(prcdatora:1:4)
164500080522     c                   except    prtdet
164600080522     c                   endif
164700080522     c                   if        *in01
164800080522     c                   eval      o_testo = 'trovato errore per la distinta ' +
164900080522     c                                        %editc(plgfgs:'X') + '-' +
165000080522     c                                        %editc(plgndc:'X') +
165100080522     c                                        ' in data ' +
165200080522     c                                        %subst(prcdatora:7:2) + '/' +
165300080522     c                                        %subst(prcdatora:5:2) + '/' +
165400080522     c                                        %subst(prcdatora:1:4)
165500080522     c                   except    prtdet
165600080522     c                   eval      o_testo = plgmsg + 'Profilo ' + prcprfc
165700080522     c                   except    prtdet
165800080522     c                   endif
165900080522
166000080522      * Stampa una riga vuota
166100080522     c                   clear                   o_testo
166200080522     c                   except    prtdet
166300080519
166400080519     c                   endsr
166500080519
166600080519      *------------------------------------------------------------------------*
166700080519      * OVERRIDE AL FILE DI STAMPA PER IMPOSTARE I DATI PER L'INVIO VIA MAIL
166800080519      *  + STAMPA INIZIO MAIL
166900080519      *------------------------------------------------------------------------*
167000080519     c     sr_openprtf   begsr
167100080519
167200080519      * Override al file di stampa
167300080519     c                   exsr      sr_override
167400080519
167500080519     c                   open      prtemail
167600080519
167700080519      * Stampa una testata se NON è richiesta la e-mail
167800080519     c                   if        §mradreg = *blanks
167900080519     c                   eval      o_testo = jobuser + ' - ' + sdspgm
168000080519     c                             + ' - ' + %editc( *date : 'Y' )
168100080519     c                             + ' - *REM* ' + %subst(§cm1var : 7 : 70)
168200080519     c                   except    prtdet
168300080519     c                   clear                   o_testo
168400080519     c                   except    prtdet
168500080519     c                   except    prtdet
168600080519     c                   endif
168700080519
168800080519      * Stampa testo iniziale
168900080519     c                   eval      o_testo = 'trovato errore in ricezione dati +
169000080522     c                                        PDA '
169100080519     c                   except    prtdet
169200080519
169300080519      * Stampa una riga vuota
169400080519     c                   clear                   o_testo
169500080519     c                   except    prtdet
169600080519
169700080519     c                   endsr
169800080519
169900080519      *------------------------------------------------------------------------*
170000080519      * OVERRIDE AL FILE DI STAMPA PER IMPOSTARE I DATI PER L'INVIO VIA MAIL
170100080519      *  + STAMPA INIZIO MAIL
170200080519      *------------------------------------------------------------------------*
170300080519     c     sr_override   begsr
170400080519
170500080519     c                   reset                   $invio
170600080519     c                   reset                   trtcm1ds
170700080519
170800080519     c                   if        §mradreg <> *blanks
170900080519     c                   eval      §cm1mitt = %trim(§mradmitt)
171000080519     c                   eval      §cm1dst  = %trim(§mraddest)
171100080519     c                   eval      §cm1tips = §mradreg
171200080519     c                   eval      §cm1po   = c_sede
171300080519     c                   eval      §cm1var  = '*OBJM*' + §mraddes
171400080519     c                   eval      §cm1idp  = §mradidpro
171500080519     c                   eval      qcmd = c_cmdovrprtf
171600080519     c                             + ' outq(' + %trim(§mradoutqi) + ')'
171700080519     c                             + ' usrdfndta(''' + trtcm1ds + ''')'
171800080519     c                   else
171900080519     c                   eval      qcmd = c_cmdovrprtf
172000080519     c                   endif
172100080519
172200080519     c                   eval      lenght = %size(qcmd)
172300080519     c                   call(e)   'QCMDEXC'
172400080519     c                   parm                    qcmd
172500080519     c                   parm                    lenght
172600080519
172700080519     c                   if        §mradreg <> *blanks and not %error
172800080519     c                   eval      $invio = *on
172900080519     c                   endif
173000080519
173100080519     c                   endsr
173200080513
173300080513      *------------------------------------------------------------------------*
173400080513      * ROUTINE FINALE
173500080513      *------------------------------------------------------------------------*
173600080513     c     sr_end        begsr
173700080513
173800080513     c                   if        %open(prtemail)
173900080513
174000080513      * Chiusura dello spool?
174100080522     c                   eval      o_testo = sdspgm
174200080522     c                   except    prtdet
174300080513     c                   clear                   o_testo
174400080513     c                   except    prtdet
174500080513     c                   eval      o_testo = '***   Fine Lista   ***'
174600080513     c                   except    prtdet
174700080513
174800080513     c                   close     prtemail
174900080513
175000080513      * Eliminazione overflow?
175100080513     c                   eval      lenght = %size(qcmd)
175200080513     c                   eval      qcmd = c_cmddltovr
175300080513     c                   call(e)   'QCMDEXC'
175400080513     c                   parm                    qcmd
175500080513     c                   parm                    lenght
175600080513
175700080513     c                   endif
175800080513
175900080513     c                   endsr
176000070129
176100070129      *------------------------------------------------------------------------*
176200070129      * ROUTINE INIZIALE
176300070129      *------------------------------------------------------------------------*
176400070129     c     *inzsr        begsr
176500070129
176600070129     c     *entry        plist
176700070129     c                   parm                    parmpo
176800080226     c                   parm                    chiudi
176900070129
177000070129     c     kfiapd        klist
177100070129     c                   kfld                    apdtip
177200070129     c                   kfld                    apdpdr
177300070201
177400070201     c     kfiqdt        klist
177500070201     c                   kfld                    plgfgs
177600070201     c                   kfld                    plgndc
177700080319
177800080319     c     kfipro        klist
177900080319     c                   kfld                    plgfgs
178000080319     c                   kfld                    plgndc
178002140129     c                   kfld                    plgtiporcd
178100080730     c                   kfld                    plgcodaut
178200080319     c                   kfld                    ormpoe
178300080319     c                   kfld                    ormnsr
178400080319     c                   kfld                    ormnor
178500080319     c                   kfld                    ormnrv
178600070201
178700080226     c     kfidst        klist
178800080226     c                   kfld                    dstnpg
178900070201     c                   kfld                    dstnfv
179000070201     c                   kfld                    dstfgs
179100070131
179200070131     c     kfnorm        klist
179300070131     c                   kfld                    ormpoe
179400070131     c                   kfld                    ormnsr
179500070131     c                   kfld                    ormnor
179600070131     c                   kfld                    ormnrv
179700080219
179800080219     c     kfnorf        klist
179900080219     c                   kfld                    ormpoe
180000080219     c                   kfld                    ormnsr
180100080219     c                   kfld                    ormnor
180200080219     c                   kfld                    ormnrv
180300080219     c                   kfld                    ormdfo
180400080219     c                   kfld                    ormofo
180500080219     c                   kfld                    ormfao
180600080409
180700080409     c     kfnorn        klist
180800080409     c                   kfld                    ormpoe
180900080409     c                   kfld                    ormnsr
181000080409     c                   kfld                    ormnor
181100080409     c                   kfld                    ormnrv
181200080409     c                   kfld                    ormfao
181300080415
181400080415     c     kfirpd        klist
181500080415     c                   kfld                    plgfgs
181600080415     c                   kfld                    plgndc
181700080415     c                   kfld                    plgcodaut
181800080415     c                   kfld                    plgiddoc
181900080417
182000080417     c     kfirpt        klist
182100080417     c                   kfld                    plgfgs
182200080417     c                   kfld                    plgndc
182300080417     c                   kfld                    plgcodaut
182301140207      *
182302140207     c     ktbe          klist
182303140207     c                   kfld                    tbecod
182304140207     c                   kfld                    tbeke1
182400070129
182500070129     c                   endsr
182600080513
182700080513     oprtemail  e            prtdet      1
182800080513     o                       o_testo
