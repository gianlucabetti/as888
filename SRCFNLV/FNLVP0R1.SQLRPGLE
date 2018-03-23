000100080731     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200080731      // ----------------------------------------------------------------------
000300080725      //
000400080805      //         MONITOR PER DISTINTE A PDA DETTAGLIO  ?
000500080725      //
000600080731      // ----------------------------------------------------------------------
000700080725      // ? DICHIARAZIONE DEI FILE ?
000800080731      // ----------------------------------------------------------------------
000900070209
001000101129     ftntbe01l  if   e           k disk
001100080804     ffnlvp0d1  cf   e             workstn sfile(lvp0s01:nrr)
001200070209
001300080725      // ----------------------------------------------------------------------
001400080725      // ? RIEPILOGO INDICATORI ?
001500080725      // ----------------------------------------------------------------------
001600080801      // 20 - gestione subfile
001700080801      // 21 - gestione subfile
001800080725      // 28 - errore generico
001900080725      // 30 - comodo
002000080801      // 31 - fine file per subfile
002100080806      // 40 - posizionamento cursore subfile
002200080725      // ----------------------------------------------------------------------
002300070209
002400080725      // ? V A R I A B I L I ?
002500080804     d nrr             s                   like(recsf1)
002600080725     d xx              s              3  0
002700110614     d waas            s                   like(arbaas)
002800110614     d wlnp            s                   like(arblnp)
002900110614     d wnrs            s                   like(arbnrs)
003000110614     d wnsp            s                   like(arbnsp)
003100080807     d wpoe            s                   like(orgpoe)
003200080807     d wnsr            s                   like(orgnsr)
003300080807     d wnor            s                   like(orgnor)
003400080807     d wnrv            s                   like(orgnrv)
003600101129     d WDcodInfo       s             20a
003700101129     d dataiso         s               d   datfmt(*iso)
003800101129     d dataeur         s               d   datfmt(*eur)
003900101129     d Wdatabol        s              8s 0
004000080731     d $fine           s               n
004100080806     d $finearb        s               n
004200080806     d $fineorg        s               n
004300081201     d $finepdc        s               n
004400080813     d $finepdo        s               n
004500080731     d $fv6            s              6p 0 dim(600)
004600101123     d Laser           s              1
004700070209
004800080725      // ? S C H I E R E ?
004900101124     d msg             s             78    dim(15) ctdata perrcd(1)
005000070209
005100080725      // ? D S   I N T E R N E / E S T E R N E ?
005200080806     d p07ds           ds
005300080806     d  p07poe                        3  0
005400080806     d  p07nor                        7  0
005500080806     d  p07nsr                        2  0
005600080806     d  p07nrv                        2  0
005700080806     d  p07sce                        1
005800080806     d  p07fgs                        3  0
005900080806     d  p07por                        3  0
006000080806     d  p07dtr                        8  0
006100080806     d  p07mdb                       10
006200080806     d  p07prb                       10
006300080806     d  p07dts                        8  0
006400080806     d  p07rmp                        1
006500080806     d  p07brc                        1
006600080806     d  p07ref                        2
006700080806     d  p07mio                        1
006800080806     d  p07dtv                        8  0
006900080806     d  p07orv                        6  0
007000080806     d  p07utv                       10
007100080806     d  p07ric                        1
007200101119      *
007300101119     D ds_PARA04       DS
007400101119     D  p04NFV                 1      5  0
007500101119     D  p04LNP                 6      8  0
007600101119     D  p04LNA                 9     11  0
007700101119     D  p04NRS                12     13  0
007800101119     D  p04NCD                14     20  0
007900101119     D  p04NCA                21     27  0
008000101119     D  p04NPS                28     29  0
008100101119     D  p04NPG                30     30  0
008200101119     D  p04FLG                31     31
008300101119     D  p04PGM                32     38
008400101119     D  p04FGS                39     41  0
008500101203     d* bolla
008600101203     d  p04bolp               42     44  0
008700101203     d  p04bors               45     46  0
008800101203     d  p04bosp               47     53  0
008900101203     d  p04boan               54     57  0
009000101203     D  PA4ERR                58     58
009100080806
009200080806     d p36ds           ds                  inz
009300080806     d  p36aas                14     17  0
009400080806     d  p36lnp                18     20  0
009500080806     d  p36nrs                21     22  0
009600080806     d  p36nsp                23     29  0
009700080806     d  p36f03                30     30
009800080806     d  p36flg                31     31
009900080806
010000080725     d wlbdat          ds                  inz
010100080725     d  g02dat                 1      8  0
010200080725     d  g02inv                 9     16  0
010300080725     d  g02err                17     17
010400080725     d  g02tgi                18     22  0
010500080806
010600080806     d §azute        e ds                  extname(azute00f)
010700080806     d                                     dtaara
010800080806     d §datiute      e ds                  extname(ddatiute)
010900080806     d                                     dtaara
011000080805     d fiapd00f      e ds                  extname(fiapd00f)
011100080805     d fidg30ds      e ds
011200080915     d fidg311ds     e ds
011300080804     d fidg45ds      e ds
011301140210     d fior82ds      e ds
011400101123     d fnlr59ds      e ds
011500080729     d fidstds       e ds                  extname(fidst00f)
011600080805     d fior60ds      e ds
011700080805     d fipctds       e ds                  extname(fipct00f)
011800080731     d fipdcds       e ds                  extname(fipdc00t)
011900080805     d fipdods       e ds                  extname(fipdo00t)
012000140210     d fiprods       e ds                  extname(fipro10f)
012100080805     d fnarbds       e ds                  extname(fnarb00f)
012200081201     d fnarbads      e ds                  extname(fnarb00f)
012300081201     d                                     prefix(a_)
012400080805     d fnorgds       e ds                  extname(fnorg52j)
012500090220     d fnorgads      e ds                  extname(fnorg00f)
012600080915     d                                     prefix(a_)
012700090220     d fnormads      e ds                  extname(fnorm00f)
012800090220     d                                     prefix(a_)
012900080805     d fnlvp0ds      e ds
013000070209     d kpjba         e ds
013100080806     d tibs34ds      e ds                  inz
013200110217
013300110217     D DARBT         E DS
013400110217     D DSARBK        E DS
013500110217     D DSARBD        E DS
013600110217     D DSARBG        E DS
013601150407     D DSARBN        E DS
013700110218     D* DS PER FNLR48R - VARIAZIONE BOLLA
013800110217     D DSLR48        E DS                  EXTNAME(FNLR48DS)
013900110218     D* DS PER FNLR66R - CONTROLLO E VISUALIZZAZIONE CAUSALI VARIAZIONI
014000110218     D DSLR66        E DS                  EXTNAME(FNLR66DS)
014100110217     D TRUL90ds      E DS
014200070209
014300070209     d                sds
014400070209     d  vtcpgm                 1     10
014500080728
014600080728      // ? COSTANTI ?
014700080728     d digitn          c                   const('1234567890')
014800080801
014900080801      // ? Ordinamento SBFL ?
015000080801      // ----------------------------------------------------------------------
015100080801      //?Costanti per ordinamento subfile
015200080801      // ----------------------------------------------------------------------
015300080801     d MaxKey          c                   2
015400080801     d Ascendente      c                   1
015500080801     d Discendente     c                   2
015600080801     d Carattere       c                   6
015700080801     d Numerico        c                   8
015800080801     d Put             c                   1
015900080801     d EndPut          c                   2
016000080801     d Get             c                   3
016100080801      **********************************************************************
016200080801      * Campi utili:
016300080801      *     nrr        - Numero relativo di record del Subfile
016400080801      *     SizeList   - Dimensione della lista
016500080801      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamen
016600080801      **********************************************************************
016700080801     d NotUsed         s             16A
016800080801     d ReturnSize      s              9B 0
016900080801     d SizeList        s              9B 0
017000080801     d RrnLast         s              5I 0
017100080801     d WrkSort         s              1  0 inz(0)
017200080801      **********************************************************************
017300080801      * Data Structures
017400080801      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
017500080801      *     QLGSCB     - The sort request block for the QLGSORT API
017600080801      *     QLGSCB00   - The sort request block for the QLGSRTIO API
017700080801      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
017800080801      *     QUSEC      - Error structure for the QLGSORT API
017900080801      **********************************************************************
018000080801     d sflrcd          ds
018100080805     d  v1stipo
018200080805     d  v1srag
018300080805     d  v1sind
018400080805     d  v1sloc
018500080805     d  v1spkg
018600080805     d  v1svlm
018700080805     d  v1ssca
018800080805     d  v1sconf
018900080805     d  v1sinf
019000080805     d  v1sann
019100080805     d  v1sscad
019200080805     d  v1sscao
019300080805     d  v1sinfo
019400080807     d  v1spoe
019500080807     d  v1snsr
019600080807     d  v1snor
019700080807     d  v1snrv
019701140210     d  v1sdao
019800080807     d  v1saas
019900080807     d  v1slnp
020000080807     d  v1snrs
020100080807     d  v1snsp
020200080807     d  v1sidrow
020300101122     d  v1slna
020400101122     d  v1sncd
020500101122     d  v1snca
020600101126     d  v1snotass
020700101129     d  v1smgs
020800101130     d  Hin45
020900080807     d  selected                      1A
021000080801
021100080801      /COPY QSYSINC/QRPGLESRC,QLGSORT
021200080801     d QLGKL                         16    DIM(MaxKey)
021300080801     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
021400080801     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
021500080801     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
021600080801     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
021700080801
021800080801      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
021900080801      /COPY QSYSINC/QRPGLESRC,QUSEC
022000080801
022100080725
022200080728      // ? PROTOTIPI ?
022300080805      /copy gaitrasrc/srcprotopr,fidg30c
022400080915      /copy gaitrasrc/srcprotopr,fidg311r
022500080731      /copy gaitrasrc/srcprotopr,fidg45r
022501140210      /copy gaitrasrc/srcprotopr,fior82r
022600080806      /copy gaitrasrc/srcprotopr,fior07r
022700080805      /copy gaitrasrc/srcprotopr,fior60c
022800080806      /copy gaitrasrc/srcprotopr,fnlr36r
022900110217      /copy gaitrasrc/srcprotopr,fnlr48r
023000110218      /copy gaitrasrc/srcprotopr,fnlr66r
023100080806      /copy gaitrasrc/srcprotopr,tibs34r
023200080728      /copy gaitrasrc/srcprotopr,xsrda8
023300101122      /copy gaitrasrc/srcprotopr,fnlr04r
023400101123      /copy gaitrasrc/srcprotopr,fnlrf6r1
023500101123      /copy gaitrasrc/srcprotopr,Fnlrf7c
023600080801
023700080801     d Qlgsort_pr      pr                  extpgm('QLGSORT')
023800080801     d  pr_QLGSCB                          like(Qlgscb)
023900080801     d  pr_NotUsed1                        like(NotUsed)
024000080801     d  pr_NotUsed2                        like(NotUsed)
024100080801     d  pr_SizeList                        like(SizeList)
024200080801     d  pr_ReturnSize                      like(ReturnSize)
024300080801     d  pr_QUSEC                           like(Qusec)
024400080801     d                                     options(*varsize)
024500080801
024600080801     d Qlgsrtio_pr     pr                  extpgm('QLGSRTIO')
024700080801     d  pr_QLGSCB00                        like(QLGSCB00)
024800080801     d  pr_SflRcd                          like(SflRcd)
024900080801     d  pr_NotUsed1                        like(NotUsed)
025000080801     d  pr_SizeList                        like(SizeList)
025100080801     d  pr_NotUsed2                        like(NotUsed)
025200080801     d  pr_QUSEC                           like(Qusec)
025300080801     d                                     options(*varsize)
025400080801
025500080801     d Qlgsrtio_pr2    pr                  extpgm('QLGSRTIO')
025600080801     d  pr_QLGSCB00                        like(QLGSCB00)
025700080801     d  pr_NotUsed1                        like(NotUsed)
025800080801     d  pr_SflRcd                          like(SflRcd)
025900080801     d  pr_SizeList                        like(SizeList)
026000080801     d  pr_NotUsed2                        like(NotUsed)
026100080801     d  pr_QUSEC                           like(Qusec)
026200080801     d                                     options(*varsize)
026300070209
026400080725      // ----------------------------------------------------------------------
026500080725
026600080725     c     *entry        plist
026700080725     c                   parm                    kpjba
026800080804     c                   parm                    fnlvp0ds
026900071217
027000101129     c     Ktbe          klist
027100101129     c                   kfld                    tbeCOD
027200101129     c                   kfld                    tbeKE1
027300080725      /free
027400080725
027500080731       //?operazioni iniziali
027600080725         exsr routinz;
027700080725
027800080729         dow not $fine;
027900080729
028000080805       //?gestione videata dettaglio
028100080805          exsr gess01;
028200080801
028300080731         enddo;
028400080729
028500080806         clear kpjbu;
028600080729         *inlr = *on;
028700080725
028800080731       // ----------------------------------------------------------------------
028900080731       //?Operazioni iniziali.
029000080731       // ----------------------------------------------------------------------
029100080725       begsr routinz;
029200080725
029300080725         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
029400080725
029500080804       //?controllo se passati tutti i parametri
029600080804         if %parms = 1;
029700080804          clear fnlvp0ds;
029800080804         endif;
029900080806
030000080806       //?recupero dei dati utente
030100080806          in(e) §azute;
030200080806          if not %error;
030300080806           in(e) §datiute;
030400080806          endif;
030500080806          if %error or rsut = *blanks;
030600080806           tibs34r(tibs34ds);
030700080806           in §azute;
030800080806           in §datiute;
030900080806          endif;
031000080805
031100080729         $fine =*off;
031200080806
031300080806       //?carico i dati relativi alla distinta passata
031400080806         exsr sr_carica;
031500080725
031600080725       endsr;
031700080725
031800080731       // ----------------------------------------------------------------------
031900080804       //?Gestione subfile
032000080731       // ----------------------------------------------------------------------
032100080804       begsr gess01;
032200080731
032300080731       //?ho caricato dati emetto il subfile
032400080801         if nrr > 0;
032500080804          exsr sr_sfl01;
032600080731         endif;
032700080731
032800080731       //?non ho caricato dati emetto videata di segnalazione
032900080801         if nrr = 0;
033000080804          exsr sr_d01;
033100080731         endif;
033200080731
033300080731       endsr;
033400080729
033500080731       // ----------------------------------------------------------------------
033600080731       //?Carico i dati per la seconda videata (subfile distinte)
033700080731       // ----------------------------------------------------------------------
033800080729       begsr sr_carica;
033900080730
034000080804       //?pulisco il subfile
034100080804         exsr sr_puls01;
034200080804
034300080804       //?carico la distinta da elaborare
034400080804         exsr sr_cardst;
034500080804
034600080804       //?elaboro i dati della distinta
034700080729         exsr sr_elabdst;
034800080806
034900080806       //?ordino il subfile per tipo
035000080806         exsr sr_ordina;
035100080729
035200080729       endsr;
035300080801
035400080731       // ----------------------------------------------------------------------
035500080731       //?Emetto il subfile distinte
035600080731       // ----------------------------------------------------------------------
035700080804       begsr sr_sfl01;
035800080801
035900080731       //?emetto la videata
036000080731         write lvp0t01;
036100080804         write lvp0z01;
036200080804         exfmt lvp0c01;
036300080731
036400080731       //?spengo indicatori di comodo
036500080731         *in28 = *off;
036600080731       //?pulisco campo messaggi
036700080804         clear v1cmsg;
036800080731       //?esecuzione comandi
036900080731         select;
037000080731
037100080805       //?f1=chiudi tutte info distinta
037200080805          when *inka;
037300080805           exsr sr_chiuinfo;
037400080731       //?f3=fine
037500080731          when *inkc;
037600080731           exsr sr_fine;
037700080805       //?f4=interroga distinta
037800080805          when *inkd;
037900080805           exsr sr_intdst;
038000080805       //?f7=assegnazione manuale ORM
038100080805          when *inkg;
038200080805           exsr sr_assmanorm;
038300080805       //?f8=assegnazione manuale spedizioni
038400080805          when *inkh;
038500080805           exsr sr_assmanspe;
038600101122       //?f10=quadratura distinta
038700101122          when *inkj;
038800101122           exsr sr_quadradis;
038900080731       //?f12=ritorno
039000080731          when *inkl;
039100080804           exsr sr_fine;
039200080731         other;
039300080805           exsr sr_ctrs01;
039400080731         endsl;
039500080731
039600080731       endsr;
039700080731
039800080731       // ----------------------------------------------------------------------
039900080804       //?Emetto video di segnalazione dati non caricati
040000080731       // ----------------------------------------------------------------------
040100080804       begsr sr_d01;
040200080731
040300080731       //?emetto la videata
040400080731         write lvp0t01;
040500080804         exfmt lvp0d01;
040600080731
040700080731       //?spengo indicatori di comodo
040800080731         *in28 = *off;
040900080731       //?pulisco campo messaggi
041000080804         clear v1cmsg;
041100080731       //?esecuzione comandi
041200080731         select;
041300080731
041400080731       //?f3=fine
041500080731          when *inkc;
041600080731           exsr sr_fine;
041700080912       //?f4=interroga distinta
041800080912          when *inkd;
041900080912           exsr sr_intdst;
042000080912       //?f7=assegnazione manuale ORM
042100080912          when *inkg;
042200080912           exsr sr_assmanorm;
042300080912       //?f8=assegnazione manuale spedizioni
042400080912          when *inkh;
042500080912           exsr sr_assmanspe;
042600101206       //?f10=quadratura distinta
042700101206          when *inkj;
042800101206           exsr sr_quadradis;
042900080731       //?f12=ritorno
043000080731          when *inkl;
043100080804           exsr sr_fine;
043200080813          other;
043300080731         endsl;
043400080731
043500080731       endsr;
043600080730
043700080731       // ----------------------------------------------------------------------
043800080804       //?Pulisco subfile
043900080731       // ----------------------------------------------------------------------
044000080804       begsr sr_puls01;
044100080730
044200080801         clear nrr;
044300080801
044400080730         *in20 = *off;
044500080730         *in21 = *off;
044600080804         write lvp0c01;
044700080730         *in20 = *on;
044800080730         *in21 = *on;
044900080730
045000080804         recsf1 = 1;
045100080730         *in31 = *off;
045200080730
045300080730       endsr;
045400080729
045500080731       // ----------------------------------------------------------------------
045600080804       //?Carico la distinta
045700080731       // ----------------------------------------------------------------------
045800080729       begsr sr_cardst;
045900080729
046000080805       //?imposto i dati relativi alla testata distinta
046100080729          exec sql
046200080805           select fidst01l.* into :fidstds from fidst01l
046300080804           where dstnpg = 4 and dstnfv = :lvp0ndc and
046400080813           dstfgs = :lvp0fgs;
046500080805       //?non trovo il rcd corrispondente vado a fine
046600080805          if sqlcod <> 0;
046700080805           leavesr;
046800080805          endif;
046900080804
047000080805          v1cfgs = dstfgs;
047100080805          v1cndc = dstnfv;
047200080805
047300080805       //?data distinta
047400080805         clear wlbdat;
047500080805         g02inv = dstdfv;
047600080805         g02err = '3';
047700080805         xsrda8(wlbdat);
047800080805         v1cddc = g02dat;
047900080804
048000080804       //?recupero ragione sociale AUT
048100080813         v1caut = lvp0aut;
048200080805         exec sql
048300080805          select apdrsc into :apdrsc from fiapd00f
048400080805          where apdtip = 'A' and apdatb = '' and
048500080813          apdpdr = :v1caut;
048600080805       //?non trovo il rcd corrispondente
048700080805         if sqlcod <> 0;
048800080805          clear apdrsc;
048900080805         endif;
049000080805         v1cdaut = apdrsc;
049100080813
049200080813       //?se aut diverso non carico i dati della distinta
049300080813         if dstpdr <> lvp0aut;
049400080813          clear v1cpkl;
049500080813          clear v1cvlu;
049600080813          clear v1cnftl;
049700080813          clear v1cpkla;
049800080813          clear v1cvlua;
049900080813          clear v1cnftla;
050000080813          clear v1cnbla;
050100080813          clear v1crpkl;
050200080813          clear v1crvlu;
050300080813          clear v1crnftl;
050400080813          clear v1crpkla;
050500080813          clear v1crvlua;
050600080813          clear v1crnftla;
050700080813          clear v1crnbla;
050800080813          eval dstpdr = lvp0aut;
050900080813          leavesr;
051000080813         endif;
051100080805
051200080805       //?dati relativi alle consegne
051300080805         v1cpkl = dstpkl;
051400080805         v1cvlu = dstvlu;
051500080805         v1cnftl = dstnftl;
051600080805         v1cpkla = dstpkla;
051700080805         v1cvlua = dstvlua;
051800080805         v1cnftla = dststpa;
051900080805         v1cnbla = dstnbla;
052000080805
052100080805       //?dati relativi ai ritiri
052200080805         v1crpkl = dstrpkl;
052300080805         v1crvlu = dstrvlu;
052400080805         v1crnftl = dstrnftl;
052500080805         v1crpkla = dstrpkla;
052600080805         v1crvlua = dstrvlua;
052700080805         v1crnftla = dstrstpa;
052800080805         v1crnbla = dstrnbla;
052900080729
053000080729       endsr;
053100080729
053200080731       // ----------------------------------------------------------------------
053300080804       //?Elaboro la distinta
053400080731       // ----------------------------------------------------------------------
053500080730       begsr sr_elabdst;
053600080806
053700080806         $finearb = *off;
053800080806         $fineorg = *off;
053900080729
054000080804       //?cerco cosa è in distinta come consegne
054100080806         if dstpda = 'C' or dstpda = 'E';
054200080806          exsr sr_consegne;
054300080813          exsr sr_consegnea;
054400080806         endif;
054500080805
054600080804       //?cerco cosa è in distinta come ritiri
054700080806         if dstpda = 'O' or dstpda = 'E';
054800080806          exsr sr_ritiri;
054900080813          exsr sr_ritiria;
055000080806         endif;
055100080801
055200080801       //?indicatore per fine file
055300080801         *in31 = *on;
055400080805
055500080801       //?imposto il numero di record caricati
055600080801         rrnlast = nrr;
055700080729
055800080729       endsr;
055900080805
056000080805       // ----------------------------------------------------------------------
056100080805       //?F1=chiudo tutte le info della distinta
056200080805       // ----------------------------------------------------------------------
056300080805       begsr sr_chiuinfo;
056400080805
056500080805       //?devo leggere tutto il subfile
056600080805         for xx = 1 to rrnlast;
056700080805          chain xx lvp0s01;
056800080805       //?se ha richiesta INFO
056900101125        //if v1sinf = 'SI';
057000101125          if v1sinf <>'  ';
057100080805       //?consegna
057200080805           if v1stipo = 'C';
057300080805            exsr sr_chiuinfoc;
057400080805           endif;
057500080805       //?consegna
057600080805           if v1stipo = 'R';
057700080805            exsr sr_chiuinfor;
057800080805           endif;
057900080805          endif;
058000080805         endfor;
058100080805
058200080805       //?ho aggiornato tutto quello che dovevo ricarico il subfile
058300080805         exsr sr_carica;
058400080805
058500080805       endsr;
058600080805
058700080805       // ----------------------------------------------------------------------
058800080805       //?F4=interrogazione distinta
058900080805       // ----------------------------------------------------------------------
059000080805       begsr sr_intdst;
059100080805
059200080915         clear fidg311ds;
059300080915         f31fgs = v1cfgs;
059400080915         f31npg = 4;
059500080915         f31numdis = v1cndc;
059600080915       //?giro la data distinta
059700080915         clear wlbdat;
059800080915         g02dat = v1cddc;
059900080915         xsrda8(wlbdat);
060000080915         f31datdis = g02inv;
060100080915         f31dativis = 'E';
060200080915         f31pridati = 'C';
060300080915         kpjbu = fidg311ds;
060400080915         fidg311r(kpjba);
060500080915         fidg311ds = kpjbu;
060600080805
060700080805       endsr;
060800080805
060900080805       // ----------------------------------------------------------------------
061000080805       //?F7=assegnazione manuale ORM
061100080805       // ----------------------------------------------------------------------
061200080805       begsr sr_assmanorm;
061300080805
061400080805         clear fior60ds;
061500080805         o60pgm = 'MONITOR';
061600080805         o60opz = '2';
061700080805         o60fgs = v1cfgs;
061800080805         o60nfv = v1cndc;
061900080805       //?giro la data distinta
062000080805         clear wlbdat;
062100080805         g02dat = v1cddc;
062200080805         xsrda8(wlbdat);
062300080805         o60dfv = g02inv;
062400080805         kpjbu = fior60ds;
062500080805         fior60c(kpjba);
062600080805         fior60ds = kpjbu;
062700080805
062800080805       //?dopo l'assegnazione manuale è sempre meglio ricaricare
062900080805       //?il subfile
063000080805         exsr sr_carica;
063100080805
063200080805       endsr;
063300080805
063400080805       // ----------------------------------------------------------------------
063500080805       //?F8=assegnazione manuale spedizioni
063600080805       // ----------------------------------------------------------------------
063700080805       begsr sr_assmanspe;
063800080805
063900080805         clear fidg30ds;
064000080805         g30pgm = 'MONITOR';
064100080805         g30opz = '2';
064200080805         g30fgs = v1cfgs;
064300080805         g30nfv = v1cndc;
064400080805       //?giro la data distinta
064500080805         clear wlbdat;
064600080805         g02dat = v1cddc;
064700080805         xsrda8(wlbdat);
064800080805         g30dfv = g02inv;
064900080805         kpjbu = fidg30ds;
065000080805         fidg30c(kpjba);
065100080805         fidg30ds = kpjbu;
065200080805
065300080805       //?dopo l'assegnazione manuale è sempre meglio ricaricare
065400080805       //?il subfile
065500080805         exsr sr_carica;
065600080805
065700080805       endsr;
065800080805
065900080805       // ----------------------------------------------------------------------
066000101122       //?F10=Quadratura Distinta
066100080805       // ----------------------------------------------------------------------
066200101122       begsr sr_quadradis;
066300080805
066400101122       // emetto window per chiedere la stampante  laser o ad aghi
066500101122
066600101123         clear  laser;
066700101123         FNLRF6R1(Laser);
066800101122
066900101123         if Laser <>' ';
067000101123
067100101123            clear fnlr59ds;
067200101123RDE         clear D59tpd;
067300101123            d59lsr = Laser;
067400101123
067500101123           //  STAMPA SOLO LE ANOMALIE
067600101123            eval  d59STP = 'A';
067700101123
067800101123           //  NON STAMPA ORM
067900101123            d59SOR = 'N';
068000101123            d59fgs = v1cFGS;
068100101123            d59ndd = V1CNDC;
068200101123            d59nda = V1CNDC;
068300101123            g02dat = v1cddc;
068400101123            xsrda8(wlbdat);
068500101123            d59ddt = g02inv;
068600101123            d59pdr = v1caut;
068700101123            d59dsf = v1cdaut;
068800101123
068900101123            kpjbu  = FNLR59ds;
069000101123            Fnlrf7c(kpjba);
069100101123
069200101123         endIf;
069300101122
069400101122       endsr;
069500101122
069600101122       // ----------------------------------------------------------------------
069700101122       //?Controllo opzioni immesse sul subfile
069800101122       // ----------------------------------------------------------------------
069900101122       begsr sr_ctrs01;
070000101122
070100080805       //?controllo l'opzione immessa
070200080805         for xx = 1 to rrnlast;
070300080805          chain xx lvp0s01;
070400080806          *in40 = *off;
070500080806          select;
070600080806       //?visualizza
070700080806          when v1sopz = '5';
070800080806           recsf1 = xx;
070900080806       //?spedizione
071000080806           if v1stipo = 'C';
071100080806            exsr sr_visbolla;
071200080806           endif;
071300080806       //?ritiro
071400080806           if v1stipo = 'R';
071500080806            exsr sr_visorm;
071600080806           endif;
071700080806           clear  v1sopz;
071800101126       //?visualizza  SOLO X CONSEGNE
071900101126       //? Spunte  e  INFO
072000101126       //?  altrimenti messaggio di errore
072100140210          when  (v1sopz = '9' and v1stipo<> 'C');
072300101124           v1cmsg = msg(02);
072400101124           *in28 = *on;
072500101124           *in40 = *on;
072600101124           recsf1 = xx;
072700101130          *in45 = Hin45;
072800101124           update lvp0s01;
072900101124           leavesr;
073000101126       //?  Se non ci sono delle NOTE x la INFO
073100101126       //?  altrimenti messaggio di errore
073200140210        //when  (v1sopz = '7'  and v1snotass=' '
073300110218        //       and v1sinf <> 'SI');
073400110218        // v1cmsg = msg(03);
073500110218        // *in28 = *on;
073600110218        // *in40 = *on;
073700110218        // recsf1 = xx;
073800110218        //*in45 = Hin45;
073900110218        // update lvp0s01;
074000110218        // leavesr;
074100101119       //?visualizza  spunte
074200101119          when v1sopz = '9';
074300101124            exsr sr_gesspu;
074400101119           clear  v1sopz;
074500101126       //?visualizza  Note INFO
074600101126          when v1sopz = '7';
074700101126            exsr sr_NoteInfo;
074800101126           clear  v1sopz;
074900080806       //?chiudi info errore se no info aperta
075000101125        //when  v1sopz = '8' and v1sinf <> 'SI';
075100101125          when  v1sopz = '8' and v1sinf =  '  ';
075200080806           v1cmsg = msg(01);
075300080806           *in28 = *on;
075400080806           *in40 = *on;
075500080806           recsf1 = xx;
075600101130          *in45 = Hin45;
075700080806           update lvp0s01;
075800080806           leavesr;
075900080805       //?chiudi info
076000080806          when v1sopz = '8';
076100080806           recsf1 = xx;
076200080806       //?spedizione
076300080806           if v1stipo = 'C';
076400080806            exsr sr_chiuinfoc;
076500080806           endif;
076600080806       //?ritiro
076700080806           if v1stipo = 'R';
076800080806            exsr sr_chiuinfor;
076900080806           endif;
077000080806           clear v1sinf;
077100080806           clear v1sopz;
077200110217
077300110217       //?Variazione Bolla
077400110217          when  v1sopz = '2' and v1stipo <>'C';
077500110217           v1cmsg = msg(01);
077600110217           *in28 = *on;
077700110217           *in40 = *on;
077800110217           recsf1 = xx;
077900110217          *in45 = Hin45;
078000110217           update lvp0s01;
078100110217           leavesr;
078200110217       //?variazione spedizione
078300110217          when v1sopz = '2' and v1stipo = 'C';
078400110217           recsf1 = xx;
078500110217
078600110217            exsr sr_VariaBolla;
078700110217
078800110217         // errore
078900110217          if  d48err = '1' ;
079000110217           v1cmsg = msg(05);
079100110217           *in28 = *on;
079200110217           *in40 = *on;
079300110217          *in45 = Hin45;
079400110217           update lvp0s01;
079500110217           leavesr;
079600110217          endif;
079700110217
079800080806          endsl;
079900110217
080000101130          *in45 = Hin45;
080100080806          update lvp0s01;
080200080805         endfor;
080300080805
080400080805       endsr;
080500080805
080600080805       // ----------------------------------------------------------------------
080700080805       //?Cerco dati consegne in distinta
080800080805       // ----------------------------------------------------------------------
080900080805       begsr sr_consegne;
081000080805
081100080805       //?--> FNARB leggo le consegne in distinta
081200080805         exec sql
081300080805          declare arb cursor for select fnarb00f.*
081400080805          from fnarb00f where
081500080813          arbifp = :dstfgs and arbndc = :dstnfv and
081600080813          arbpdc = :dstpdr;
081700080805         exec sql open arb;
081800080806         dow not $finearb;
081900080805          exec sql fetch next from arb into: fnarbds;
082000080805       //?fine file o errore sql esco
082100080805          if sqlcod = 100 or sqlcod < 0;
082200080806           $finearb = *on;
082300080806           leave;
082400080805          endif;
082500080805
082600080805       //?imposto i dati della spedizione
082700080805          v1stipo = 'C';
082800080805          v1saas = arbaas;
082900080805          v1slnp = arblnp;
083000080805          v1snrs = arbnrs;
083100080805          v1snsp = arbnsp;
083200080805          v1srag = arbrsd;
083300080805          v1sind = arbind;
083400080805          v1sloc = arblod;
083500080805          v1spkg = arbpkb;
083600080805          v1svlm = arbvlb;
083700101122          v1slna = arblna;
083800101122          v1sncd = arbncd;
083900101122          v1snca = arbnca;
084000101129          v1smgs = arbmgs;
084100080806
084200080806       //?pulisco i dati che possono essere anche a blank
084300080806          clear v1ssca;
084400080806          clear v1sconf;
084500080806          clear v1sinf;
084600080806          clear v1sscad;
084700080806          clear v1sscao;
084800080806          clear v1sinfo;
084900080807          clear v1sidrow;
085000081009          clear v1sann;
085100080807       //?pulisco i dati della chiave ORM
085200080807          clear v1spoe;
085300080807          clear v1snsr;
085400080807          clear v1snor;
085500080807          clear v1snrv;
085501140210          clear v1sdao;
085600080805
085700080805       //?cerco i dati relativi alla preparazione PDA
085800080805          exsr sr_scac;
085900080805
086000080805       //?se già preparato per il PDA
086100080805          if v1ssca <> 'SI';
086200080805
086300080805       //?controllo se è da confermare dal PDA
086400080805           exsr sr_confc;
086500080805
086600080805       //?controllo se richiesta INFO
086700110614           waas = arbaas;
086800110614           wlnp = arblnp;
086900110614           wnrs = arbnrs;
087000110614           wnsp = arbnsp;
087100080805           exsr sr_infoc;
087200080805          endif;
087300080805
087400080805       //?scrivo subfile
087500080805          exsr sr_cars01;
087600080805
087700080805         enddo;
087800080806
087900080806       //?chiudo il cursore
088000080806          exec sql close arb;
088100080805
088200080805       endsr;
088300080813
088400080813       // ----------------------------------------------------------------------
088500080813       //?Cerco i dati di scarico della spedizione annullati
088600080813       // ----------------------------------------------------------------------
088700080813       begsr sr_consegnea;
088800080813
088900080813       //?pulisco i dati che possono essere anche a blank
089000080813          clear v1ssca;
089100080813          clear v1sconf;
089200080813          clear v1sinf;
089300080813          clear v1sscad;
089400080813          clear v1sscao;
089500080813          clear v1sinfo;
089600080813          clear v1sidrow;
089700081009          clear v1sann;
089800080813       //?pulisco i dati della chiave ORM
089900080813          clear v1spoe;
090000080813          clear v1snsr;
090100080813          clear v1snor;
090200080813          clear v1snrv;
090201140210          clear v1sdao;
090300081201       //?indicatore di comodo
090400081201          $finepdc = *off;
090500080813
090600080813       //?--> FIPDC leggo se la spedizione è stata preparata per il PDA
090700080813       //?          solo 1 rcd prendo idrow + alto ma annullato
090800080813         exec sql
090900081201          declare pdc cursor for select fipdc00t.*
091000080813          from fipdc00t where pdcfgs = :dstfgs and
091100080813          pdcndc = :dstnfv and pdccodaut = :dstpdr and
091200080813          pdcatb <> ''
091300080813          order by pdcidrow desc;
091400081201          exec sql open pdc;
091500081201          dow not $finepdc;
091600081201          exec sql fetch next from pdc into: fipdcds;
091700081201       //?fine file o errore sql esco
091800081201          if sqlcod = 100 or sqlcod < 0;
091900081201           $finepdc = *on;
092000081201           leave;
092100080813          endif;
092200080813       //?imposto la data e ora di preparazione
092300080813          clear wlbdat;
092400080813          g02inv = %dec(%subst(pdcdatora:1:8):8:0);
092500080813          g02err = '3';
092600080813          xsrda8(wlbdat);
092700080813          v1sscad = g02dat;
092800080813          v1sscao = %dec(%subst(pdcdatora:9:4):4:0);
092900080813       //?imposto se annullato
093000080813          v1sann = pdcatb;
093100080813       //?imposto idrow
093200080813          v1sidrow = pdcidrow;
093300080813       //?imposto i dati della consegna agganciando FNARB
093400080813          exec sql
093500081201           select fnarb01l.* into :fnarbads from fnarb01l
093600080813           where arbaas = :pdcaas and arblnp = :pdclnp and
093700080813           arbnrs = :pdcnrs and arbnsp = :pdcnsp;
093800080813       //?non trovo il rcd corrispondente leggo altro record
093900080813          if sqlcod <> 0;
094000081201           iter;
094100080813          endif;
094200080813       //?imposto i dati della spedizione
094300080813          v1stipo = 'C';
094400081201          v1saas = a_arbaas;
094500081201          v1slnp = a_arblnp;
094600081201          v1snrs = a_arbnrs;
094700081201          v1snsp = a_arbnsp;
094800081201          v1srag = a_arbrsd;
094900081201          v1sind = a_arbind;
095000081201          v1sloc = a_arblod;
095100081201          v1spkg = a_arbpkb;
095200081201          v1svlm = a_arbvlb;
095300101122          v1slna = a_arblna;
095400101122          v1sncd = a_arbncd;
095500101122          v1snca = a_arbnca;
095600101129          v1smgs = a_arbmgs;
095700080813
095800081201       //?controllo se è da confermare dal PDA
095900081201           exsr sr_confc;
096000110614
096100110614       //?controllo se richiesta INFO
096200110614           waas = a_arbaas;
096300110614           wlnp = a_arblnp;
096400110614           wnrs = a_arbnrs;
096500110614           wnsp = a_arbnsp;
096600080813           exsr sr_infoc;
096700080813
096800080813       //?scrivo subfile
096900080813          exsr sr_cars01;
097000081201
097100081201         enddo;
097200081201
097300081201       //?chiudo il cursore
097400081201         exec sql close pdc;
097500080813
097600080813       endsr;
097700080805
097800080805       // ----------------------------------------------------------------------
097900080805       //?Cerco dati ritiri in distinta
098000080805       // ----------------------------------------------------------------------
098100080805       begsr sr_ritiri;
098200080805
098300080805       //?--> FNORG leggo i ritiri in distinta
098400080805         exec sql
098500080805          declare org cursor for select fnorg52j.*
098600080805          from fnorg52j where
098700080813          orgfgs = :dstfgs and orgndc = :dstnfv and
098800080813          orgpdc = :dstpdr;
098900080805         exec sql open org;
099000080806         dow not $fineorg;
099100080805          exec sql fetch next from org into: fnorgds;
099200080805       //?fine file o errore sql esco
099300080805          if sqlcod = 100 or sqlcod < 0;
099400080806           $fineorg = *on;
099500080805           leave;
099600080805          endif;
099700080805
099800080805       //?imposto i dati del ritiro
099900080805          v1stipo = 'R';
100000080805          v1spoe = orgpoe;
100100080805          v1snsr = orgnsr;
100200080805          v1snor = orgnor;
100300080805          v1snrv = orgnrv;
100301140210          v1sdao = ormdao;
100400080805          v1srag = ormrsr;
100500080805          v1sind = orminr;
100600080805          v1sloc = ormlor;
100700080915          v1spkg = orgpkg;
100800080915          v1svlm = orgvlm;
100900080806
101000080806       //?pulisco i dati che possono essere anche a blank
101100080806          clear v1ssca;
101200080806          clear v1sconf;
101300080806          clear v1sinf;
101400080806          clear v1sscad;
101500080806          clear v1sscao;
101600080806          clear v1sinfo;
101700080807          clear v1sidrow;
101800081009          clear v1sann;
101900080807       //?pulisco i dati della chiave spedizione
102000080807          clear v1saas;
102100080807          clear v1slnp;
102200080813          clear v1snrs;
102300080807          clear v1snsp;
102400101122          clear v1slna;
102500101122          clear v1sncd;
102600101122          clear v1snca;
102700101129          clear v1smgs;
102800080806
102900080805       //?cerco i dati relativi alla preparazione PDA
103000080813          exsr sr_scar;
103100080806
103200080806       //?se già preparato per il PDA
103300080806          if v1ssca <> 'SI';
103400080805
103500080805       //?controllo se è da confermare dal PDA
103600080805       //?e se richiesta INFO
103700080805           exsr sr_confr;
103800080805          endif;
103900080805
103901140210
103902140210       // controllo se richiesta INFO
103903140210           wpoe = orgpoe;
103904140210           wnsr = orgnsr;
103905140210           wnor = orgnor;
103906140210           wnrv = orgnrv;
103907140210           exsr sr_infor;
103909140210
104000080805       //?scrivo subfile
104100080805          exsr sr_cars01;
104200080805
104300080805         enddo;
104400080806
104500080806       //?chiudo il cursore
104600080813         exec sql close org;
104700080805
104800080805       endsr;
104900080813
105000080813       // ----------------------------------------------------------------------
105100080813       //?Cerco dati ritiri in distinta annullati
105200080813       // ----------------------------------------------------------------------
105300080813       begsr sr_ritiria;
105400080813
105500080813       //?pulisco i dati che possono essere anche a blank
105600080813          clear v1ssca;
105700080813          clear v1sconf;
105800080813          clear v1sinf;
105900080813          clear v1sscad;
106000080813          clear v1sscao;
106100080813          clear v1sinfo;
106200080813          clear v1sidrow;
106300081009          clear v1sann;
106400080813       //?pulisco i dati della chiave spedizione
106500080813          clear v1saas;
106600080813          clear v1slnp;
106700080813          clear v1snrs;
106800080813          clear v1snsp;
106900101122          clear v1slna;
107000101122          clear v1sncd;
107100101122          clear v1snca;
107200101129          clear v1smgs;
107300101129
107400081201       //?indicatore di comodo
107500081201          $finepdo = *off;
107600080813
107700080813       //?--> FIPDO per i dati preparati ma annullati
107800080813         exec sql
107900080813          declare pdo cursor for select fipdo00t.*
108000080813          from fipdo00t where pdofgs = :dstfgs and
108100080813          pdondc = :dstnfv and pdocodaut = :dstpdr and
108200080813          pdoatb <> '';
108300080813          exec sql open pdo;
108400080813          dow not $finepdo;
108500080813          exec sql fetch next from pdo into: fipdods;
108600080813       //?fine file o errore sql esco
108700080813          if sqlcod = 100 or sqlcod < 0;
108800080813           $finepdo = *on;
108900080813           leave;
109000080813          endif;
109100080813       //?imposto la data e ora di preparazione
109200080813          clear wlbdat;
109300080813          g02inv = %dec(%subst(pdodatora:1:8):8:0);
109400080813          g02err = '3';
109500080813          xsrda8(wlbdat);
109600080813          v1sscad = g02dat;
109700080813          v1sscao = %dec(%subst(pdodatora:9:4):4:0);
109800080813       //?imposto se annullato
109900080813          v1sann = pdoatb;
110000080813       //?imposto idrow
110100080813          v1sidrow = pdoidrow;
110200090220       //?imposto i dati dell'ORM agganciando FNORM e poi FNORG
110300080813          exec sql
110400090220           select fnorm00f.* into :fnormads from fnorm00f
110500090220           where ormpoe = :pdopoe and ormnsr = :pdonsr and
110600090220           ormnor = :pdonor and ormnrv = :pdonrv;
110700080813       //?non trovo il rcd corrispondente leggo altro record
110800080813          if sqlcod <> 0;
110900080813           iter;
111000080813          endif;
111100080813       //?imposto i dati del ritiro
111200080813          v1stipo = 'R';
111300090220          v1spoe = a_ormpoe;
111400090220          v1snsr = a_ormnsr;
111500090220          v1snor = a_ormnor;
111600090220          v1snrv = a_ormnrv;
111601140210          v1sdao = a_ormdao;
111700080915          v1srag = a_ormrsr;
111800080915          v1sind = a_orminr;
111900080915          v1sloc = a_ormlor;
112000090220       //?aggancio ORG
112100090220          exec sql
112200090220           select fnorg00f.* into :fnorgads from fnorg00f
112300090220           where orgpoe = :pdopoe and orgnsr = :pdonsr and
112400090220           orgnor = :pdonor and orgnrv = :pdonrv;
112500090220       //?non trovo il rcd corrispondente leggo altro record
112600090220          if sqlcod <> 0;
112700090220           iter;
112800090220          endif;
112900080915          v1spkg = a_orgpkg;
113000080915          v1svlm = a_orgvlm;
113100080813
113200080813       //?controllo se è da confermare dal PDA
113300080813       //?e se richiesta INFO
113400080813          exsr sr_confr;
113500080813
113600080813       //?scrivo subfile
113700080813          exsr sr_cars01;
113800080813
113900080813         enddo;
114000080813
114100080813       //?chiudo il cursore
114200080813         exec sql close pdo;
114300080813
114400080813       endsr;
114500080805
114600080805       // ----------------------------------------------------------------------
114700080805       //?Cerco i dati di scarico della spedizione
114800080805       // ----------------------------------------------------------------------
114900080805       begsr sr_scac;
115000080805
115100080805       //?--> FIPDC leggo se la spedizione è stata preparata per il PDA
115200080805       //?          solo 1 rcd prendo idrow + alto
115300080805         exec sql
115400080807          select fipdc00t.* into :fipdcds
115500080805          from fipdc00t where pdcfgs = :dstfgs and
115600080805          pdcndc = :dstnfv and pdccodaut = :dstpdr and
115700080805          pdcaas = :arbaas and pdclnp = :arblnp and
115800080807          pdcnrs = :arbnrs and pdcnsp = :arbnsp
115900080805          order by pdcidrow desc;
116000080807       //?non trovo il rcd corrispondente è da scaricare
116100080807          if sqlcod <> 0;
116200080805       //?imposto che spedizione ancora da scaricare
116300080805           v1ssca = 'SI';
116400080807           leavesr;
116500080805          endif;
116600080805       //?imposto la data e ora di preparazione
116700080807          clear wlbdat;
116800080807          g02inv = %dec(%subst(pdcdatora:1:8):8:0);
116900080807          g02err = '3';
117000080807          xsrda8(wlbdat);
117100080807          v1sscad = g02dat;
117200080807          v1sscao = %dec(%subst(pdcdatora:9:4):4:0);
117300080805       //?imposto se annullato
117400080807          v1sann = pdcatb;
117500080805       //?imposto idrow
117600080807          v1sidrow = pdcidrow;
117700080806
117800080805       endsr;
117900080805
118000080805       // ----------------------------------------------------------------------
118100080805       //?Controllo se da confermare da PDA
118200080805       // ----------------------------------------------------------------------
118300080805       begsr sr_confc;
118400080805
118500080805         clear v1sconf;
118600080805
118700080805       //?--> FIPCT leggo se ricevuto ok di conferma a parità di idrow
118800080805         exec sql
118900080805          select fipct00f.* into :fipctds from fipct00f
119000080805          where pctfgs = :pdcfgs and pctndc = :pdcndc and
119100080912           pctcodaut = :pdccodaut and
119200080805           pctaas = :pdcaas and
119300080805           pctlnp = :pdclnp and pctnrs = :pdcnrs and
119400080805           pctnsp = :pdcnsp and pcttrd = 'COK' and
119500080805           substr(pctdati, 3, 2) = :pdcidrow;
119600080805       //?non trovo il rcd corrispondente
119700080805          if sqlcod <> 0;
119800080805       //?imposto come non confermato
119900080805           v1sconf = 'SI';
120000080805          endif;
120100080805
120200080805       endsr;
120300080805
120400080805       // ----------------------------------------------------------------------
120500080805       //?Controllo se richiesta INFO
120600080805       // ----------------------------------------------------------------------
120700080805       begsr sr_infoc;
120800080805
120900080805         clear v1sinf;
121000101130         hin45 = '0';
121100080805
121200080805       //?pgm che ritorna se presente richiesta info sulla spedizione
121300080805         clear fidg45ds;
121400080912         c45fgs = dstfgs;
121500080912         c45ndc = dstnfv;
121600080912         c45codaut = dstpdr;
121700110614         c45aas = waas;
121800110614         c45lnp = wlnp;
121900110614         c45nrs = wnrs;
122000110614         c45nsp = wnsp;
122100080805         kpjbu = fidg45ds;
122200080805         fidg45r(kpjba);
122300080805         fidg45ds = kpjbu;
122400080805         if c45davisio <> '';
122500101201        if c45davisio = 'S';
122600101201          v1sinf = 'SI';  //  Adesso riceve il motivo al posto del blank o "S"
122700101201        ELSE;
122800101126          v1sinf = c45davisio;
122900101126          v1snotass = c45notass;
123000110110              hin45 = '0';
123100101130           if v1snotass <> *blanks;
123200101130              hin45 = '1';
123300101130           endif;
123400080805       //?ora della richiesta info
123500080912          v1sinfo = %dec(%subst(%editc(c45dtorino:'X'):9:4):4:0);
123600080912       // clear v1sinfo;
123700101201        ENDif;
123800080805         endif;
123900080805
124000080805       endsr;
124100080805
124101140210       // ----------------------------------------------------------------------
124102140210       //?Controllo se richiesta INFO  ritiri
124103140210       // ----------------------------------------------------------------------
124104140210       begsr sr_infor;
124105140210
124106140210         clear v1sinf;
124107140210         hin45 = '0';
124108140210
124109140210       //?pgm che ritorna se presente richiesta info sulla spedizione
124110140210         clear fior82ds;
124111140210         c82fgs = dstfgs;
124112140210         c82ndc = dstnfv;
124113140210         c82codaut = dstpdr;
124114140210         c82poe = wpoe;
124115140210         c82nsr = wnsr;
124116140210         c82nor = wnor;
124117140210         c82nrv = wnrv;
124118140210         kpjbu = fior82ds;
124119140210         fior82r(kpjba);
124120140210         fior82ds = kpjbu;
124121140210         if c82davisio <> '';
124122140210        if c82davisio = 'S';
124123140210          v1sinf = 'SI';  //  Adesso riceve il motivo al posto del blank o "S"
124124140210        ELSE;
124125140210          v1sinf = c82davisio;
124126140210          v1snotass = c82notass;
124127140210              hin45 = '0';
124128140210           if v1snotass <> *blanks;
124129140210              hin45 = '1';
124130140210           endif;
124132140210       //?ora della richiesta info
124134140210          v1sinfo = %dec(%subst(c82dtorino:9:4):4:0);
124135140210       // clear v1sinfo;
124136140210        ENDif;
124137140210         endif;
124138140210
124139140210       endsr;
124140140210
124200080805       // ----------------------------------------------------------------------
124300080805       //?Cerco i dati di scarico del ritiro
124400080805       // ----------------------------------------------------------------------
124500080805       begsr sr_scar;
124600080805
124700080805       //?--> FIPDO leggo se il ritiro è stato preparato per il PDA
124800080805         exec sql
124900080807          select fipdo00t.* into :fipdods
125000080805          from fipdo00t where pdofgs = :dstfgs and
125100080805          pdondc = :dstnfv and pdocodaut = :dstpdr and
125200080805          pdopoe = :orgpoe and pdonsr = :orgnsr and
125300081014          pdonor = :orgnor and pdonrv = :orgnrv and
125400081014          pdoatb = '';
125500080807       //?non trovo il rcd corrispondente
125600080807          if sqlcod <> 0;
125700080807       //?imposto che ritiro ancora da scaricare
125800080805           v1ssca = 'SI';
125900080807           leavesr;
126000080805          endif;
126100080805       //?imposto la data e ora di preparazione
126200080805          clear wlbdat;
126300080805          g02inv = %dec(%subst(pdodatora:1:8):8:0);
126400080805          g02err = '3';
126500080805          xsrda8(wlbdat);
126600080805          v1sscad = g02dat;
126700080805          v1sscao = %dec(%subst(pdodatora:9:4):4:0);
126800080805       //?imposto se annullato
126900080805          v1sann = pdoatb;
127000080805       //?imposto idrow
127100080813          v1sidrow = pdoidrow;
127200080805
127300080805       endsr;
127400080805
127500080805       // ----------------------------------------------------------------------
127600080805       //?Controllo se da confermare da PDA e richiesta info
127700080805       // ----------------------------------------------------------------------
127800080805       begsr sr_confr;
127900080805
128000080805         clear v1sconf;
128100080805         clear v1sinf;
128200080805         clear v1sinfo;
128300080805
128400080805       //?--> FIPRO leggo se ricevuto ok di conferma a parità di idrow
128500080805          exec sql
128600140210           select fipro10f.* into :fiprods from fipro10f
128700080805           where profgs = :pdofgs and prondc = :pdondc and
128800080805       // procodaut = :pdocodaut and
128900080805           propoe = :pdopoe and
129000080805           pronsr = :pdonsr and pronor = :pdonor and
129100140210           pronrv = :pdonrv and protrd = 'OK ' and
129101140210           substr(prodati, 3, 2) = :pdoidrow;
129200080805       //?non trovo il rcd corrispondente
129300080805          if sqlcod <> 0;
129400080805       //?imposto come non confermato
129500080805           v1sconf = 'SI';
129600080805          endif;
130600080805
130700080805       endsr;
130800080801
130900080801       // ----------------------------------------------------------------------
131000080805       //?Carico il subfile
131100080801       // ----------------------------------------------------------------------
131200080805       begsr sr_cars01;
131300080805
131400080805       //?carico solo se ha un'anomalia
131500101125       //if v1ssca = 'SI' or v1sconf = 'SI' or v1sinf = 'SI';
131600101125         if v1ssca = 'SI' or v1sconf = 'SI' or v1sinf <>'  ';
131700080805          clear v1sopz;
131800080801          nrr = nrr + 1;
131900101130          *in45 = Hin45;
132000080805          write lvp0s01;
132100080805         endif;
132200080801
132300080801       endsr;
132400080805
132500080805       // ----------------------------------------------------------------------
132600080805       //?Chiudo info per la spedizione
132700080805       // ----------------------------------------------------------------------
132800080805       begsr sr_chiuinfoc;
132900080805
133000080805         clear fidg45ds;
133100080912         c45fgs = v1cfgs;
133200080912         c45ndc = v1cndc;
133300080912         c45codaut = v1caut;
133400080805         c45aas = v1saas;
133500080805         c45lnp = v1slnp;
133600080805         c45nrs = v1snrs;
133700080805         c45nsp = v1snsp;
133800080805         c45aggvis = 'S';
133900080805         kpjbu = fidg45ds;
134000080805         fidg45r(kpjba);
134100080805         fidg45ds = kpjbu;
134200080805
134300080805       endsr;
134400080805
134500080805       // ----------------------------------------------------------------------
134600080805       //?Chiudo info per il ritiro
134700080805       // ----------------------------------------------------------------------
134800080805       begsr sr_chiuinfor;
134900080807
134901140210         clear fior82ds;
134902140210         c82fgs = v1cfgs;
134903140210         c82ndc = v1cndc;
134904140210         c82codaut = v1caut;
134905140210         c82poe = v1spoe;
134906140210         c82nsr = v1snsr;
134907140210         c82nor = v1snor;
134908140210         c82nrv = v1snrv;
134909140210         c82aggvis = 'S';
134910140210         kpjbu = fior82ds;
134911140210         fior82r(kpjba);
134912140210         fior82ds = kpjbu;
136300080805
136400080805       endsr;
136500080806
136600080806       // ----------------------------------------------------------------------
136700080806       //?Visualizzo la spedizione
136800080806       // ----------------------------------------------------------------------
136900080806       begsr sr_visbolla;
137000080806
137100080806         clear p36ds;
137200080806         p36aas = v1saas;
137300080806         p36lnp = v1slnp;
137400080806         p36nrs = v1snrs;
137500080806         p36nsp = v1snsp;
137600080806         p36flg = '1';
137700080806         clear p36f03;
137800080806         kpjbu = p36ds;
137900080806         fnlr36r(kpjba);
138000080806
138100080806       endsr;
138200080806
138300080806       // ----------------------------------------------------------------------
138400080806       //?Visualizzo l'ORM
138500080806       // ----------------------------------------------------------------------
138600080806       begsr sr_visorm;
138700080806
138800080806         clear p07ds;
138900080806         p07poe = v1spoe;
139000080806         p07nor = v1snor;
139100080806         p07nsr = v1snsr;
139200080806         p07nrv = v1snrv;
139300080806         p07sce = '5';
139400080806         p07fgs = v1cfgs;
139500080806         kpjbu = p07ds;
139600080806         fior07r(kpjba);
139700080806
139800080806       endsr;
139900110217
140000110217       // ----------------------------------------------------------------------
140100110217       //?Variazione della spedizione
140200110217       // ----------------------------------------------------------------------
140300110217       begsr sr_variabolla;
140400110218
140500110218         clear DSlr66;
140600110218         eval d66aas = v1saas;
140700110218         eval d66lnp = v1slnp;
140800110218         eval d66nrs = v1snrs;
140900110218         eval d66nsp = v1snsp;
141000110218         eval d66ges = 'V';
141100110218         eval d66tbo = 'A';
141200110218
141300110218         eval  kpjbu = DSLR66;
141400110218         fnlr66r(kpjba);
141500110218         eval DSLR66 = kpjbu;
141600110218
141700110218       // Controllo          -- //
141800110218         eval d66ges = 'C';
141900110218
142000110218         eval  kpjbu = DSLR66;
142100110218         fnlr66r(kpjba);
142200110218         eval DSLR66 = kpjbu;
142300110218
142400110218         if d66MSG <> *blank;
142500110218
142600110218         else;
142700110218
142800110218        // RICHIAMO PGM DI VARIAZIONE SE IMPOSTATO IL CODICE
142900110218        //  unlock fnarb01l;
143000110217
143100110217         clear DSlr48;
143200110217         clear dsarbd;
143300110217         clear dsarbg;
143400110217         clear dsarbk;
143500110217         clear darbt;
143600110217         clear trul90ds;
143700110218
143800110217         eval d48tla = 'L';
143900110217         eval d48aas = v1saas;
144000110217         eval d48lnp = v1slnp;
144100110217         eval d48nrs = v1snrs;
144200110217         eval d48nsp = v1snsp;
144300110217         eval d48fgs = v1cfgs;
144400110218         eval d48cvb = d66cvb;
144500110218         eval d48tbo = 'A';
144600110218         eval d48f12 = 'S';
144700110218         eval d48ffr = 'V';
144800110217
144900110217         eval  kpjbu = DSLR48;
145000110217         fnlr48r(kpjba:dsarbd:dsarbk:darbt:dsarbg:trul90ds);
145100110217         eval DsLR48 = KPJBU;
145200110217
145300110218         endif;
145400110218
145500110217       endsr;
145600101119
145700101119       // ----------------------------------------------------------------------
145800101119       //?Visualizzo le spunte
145900101119       // ----------------------------------------------------------------------
146000101124       begsr sr_gesspu;
146100101119
146200101119         clear ds_para04;
146300101122         P04NFV = *zeros;
146400101122         P04NRS = v1sNRS;
146500101122         P04lnp = v1sLNP;
146600101122         P04lna = v1sLNA;
146700101122         P04ncd = v1sNCD;
146800101122         P04nca = v1sNCA;
146900101119         P04NPS = 99;
147000101122         P04NPG = 9;
147100101203         P04FLG = '2';
147200101122         P04PGM = 'FNLVP0R';
147300101119         P04FGS = v1cFGS;
147400101203         p04bolp = V1SLNP;
147500101203         p04bors = V1SNRS;
147600101203         p04bosp = V1SNSP;
147700101203         p04boan = V1SAAS;
147800101203         PA4ERR  = *blank;
147900101119         kpjbu = ds_para04;
148000101122         FNLR04R(kpjba);
148100101119
148200101119       endsr;
148300080801
148400101126       // ----------------------------------------------------------------------
148500101126       //?Visualizzo le spunte
148600101126       // ----------------------------------------------------------------------
148700101126       begsr sr_noteinfo;
148800101126
148901140210         if v1stipo = 'C';
148902140210         clear NOTINFO;
149000101126           WSPED =  %editc(v1sAAS:'X') + '/' + %editc(v1sLNP:'X') + '/' +
149100101126                    %editc(v1sNRS:'X') + '/' + %editc(v1sNSP:'X');
149200101129
149300101129          wdatabol = v1sAAS * 10000  + v1smgs;
149400101129          dataiso = %date(wdatabol:*iso);
149500101129          dataeur = dataiso;
149600101129          WDEL   = %dec(dataeur);
149700101129
149800101126           WDEST = V1SRAG;
149900101201            if  v1sINF <> 'SI' AND v1sINF <> 'S ';
150000101201                 tbeCOD = 'ASA';
150100101201                 tbeKE1 = v1sINF;
150200101201                 chain (tbecod:tbeke1) tntbe01l;
150300101201                 clear WDcodInfo;
150400101201                   if  %found(tntbe01l);
150500101201                      WDcodInfo = tbeUNI;
150600101201                   endif;
150700101201                 WCODINFO= %trim(v1sINF)  + ': ' + WDcodInfo;
150800101201                 WNOTASS = v1snotass;
150900101201             endif;
150901140210           exfmt NOTINFO;
150902140210           else;
150903140210         clear NOTINFOr;
150904140210           WSPED =  %editc(v1spoe:'X') + '/' + %editc(v1snsr:'X') + '/' +
150905140210                    %editc(v1sNor:'X') + '/' + %editc(v1sNrv:'X');
150906140210
150907140210          dataiso = %date(v1sdao:*iso);
150908140210          dataeur = dataiso;
150909140210          WDEL   = %dec(dataeur);
150910140210
150911140210           WDEST = V1SRAG;
150912140210            if  v1sINF <> 'SI' AND v1sINF <> 'S ';
150913140210                 tbeCOD = 'RAS';
150914140210                 tbeKE1 = v1sINF;
150915140210                 chain (tbecod:tbeke1) tntbe01l;
150916140210                 clear WDcodInfo;
150917140210                   if  %found(tntbe01l);
150918140210                      WDcodInfo = tbeUNI;
150919140210                   endif;
150920140210                 WCODINFO= %trim(v1sINF)  + ': ' + WDcodInfo;
150921140210                 WNOTASS = v1snotass;
150922140210             endif;
150923140210           exfmt NOTINFOr;
150924140210             endif;
151100101126
151200101126       endsr;
151300101126
151400080801       // ----------------------------------------------------------------------
151500080804       //?Ordino il subfile prima di emetterlo
151600080801       // ----------------------------------------------------------------------
151700080804       begsr sr_ordina;
151800080801
151900080804        recsf1 = 1;
152000080801
152100080801       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
152200080801       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
152300080801       // il campo è usato per selezionare i records dando un ordine a quelli
152400080801       // selezionati davanti ai non selezionati.
152500080801         clear QLGSCB;
152600080801         clear QLGSCB00;
152700080801
152800080801       // 1 campi chiave x Distinta
152900080801         QLGNBRK = 1;
153000080801
153100080801       // imposto la posizione della distinta sul subfile e la lunghezza
153200080801       // l'ordinamento è su un campo numerico e deve essere
153300080801       // ascending
153400080805         QLGSP = 1;
153500080805         QLGSS = %SIZE(v1stipo);
153600080805         QLGDT = Carattere;
153700080801         QLGSO = Ascendente;
153800080801         QLGKL(1) = QLGSKL;
153900080801
154000080801       // Load other sort parameters.
154100080801         QLGLB = 80 + 16 * MaxKey;
154200080801         QLGRL = %SIZE(sflrcd) - 1;
154300080801         QLGRT = 8;
154400080801         QLGOKL = 80;
154500080801         QLGLKE = 16;
154600080801         QLGLSS = 290;
154700080801
154800080801       // Initialize Sort I/O API fields.
154900080801         QLGRL00 = QLGRL;
155000080801         QLGRC00 = 1;
155100080801         clear QUSEI;
155200080801         QUSBPRV = %SIZE(QUSEC);
155300080801
155400080801       // First step - Initialize the sort routine.
155500080801         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
155600080801
155700080801       // Next step - Write records to I/O routine.
155800080801         QLGRT00 = Put;
155900080801         for xx = 1 to rrnlast;
156000080805          chain xx lvp0s01;
156100080801
156200080801       // solo le righe con Selected = 'Y' sono riordinate,
156300080801       // quindi per fare un ordinamento di tutte le righe
156400080801       // metto 'Y' sempre.
156500080801          selected  = 'Y';
156600080801          clear QUSEI;
156700080801          QUSBPRV = %SIZE(QUSEC);
156800080801          QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
156900080801         endfor;
157000080801
157100080801       // Next step - Signal end of input, clear subfile for reload.
157200080801         QLGRT00 = EndPut;
157300080801         clear QUSEI;
157400080801         QUSBPRV = %SIZE(QUSEC);
157500080801         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
157600080801       // pulizia SFL
157700080801         clear nrr;
157800080801         *in20 = *off;
157900080801         *in21 = *off;
158000080805         write lvp0c01;
158100080801         *in20 = *on;
158200080801         *in21 = *on;
158300080801
158400080801       // Final step - Write the records back to the subfile.
158500080801         QLGRT00 = Get;
158600080801         for xx = 1 to rrnlast;
158700080801          clear QUSEI;
158800080801          QUSBPRV = %SIZE(QUSEC);
158900080801          QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
159000080801          nrr = xx;
159100101130          *in45 = Hin45;
159200080805          write lvp0s01;
159300080801         endfor;
159400080801
159500080801       endsr;
159600080725
159700080731       // ----------------------------------------------------------------------
159800080731       //?Fine programma
159900080731       // ----------------------------------------------------------------------
160000080725       begsr sr_fine;
160100080725
160200080725         *inlr = *on;
160300080725         return;
160400080725
160500080725       endsr;
160600070214
160700080805** MSG  Lungh. 78                                                            *
160800080806Opzione non valida                                                            01
160900101124Solo x le consegne                                                            02
161000101126Non ci sono NOTE                                                              03
161100101201Opzione non Attiva                                                            04
161200110217Errori durante la fase di variazione Bolle                                    05
