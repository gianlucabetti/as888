000100080731     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200080731      // ----------------------------------------------------------------------
000300080725      //
000400080725      //         MONITOR PER DISTINTE A PDA  ?
000500080725      //
000600080731      // ----------------------------------------------------------------------
000700080725      // ? DICHIARAZIONE DEI FILE ?
000800080731      // ----------------------------------------------------------------------
000900070209
001000070209     fazorg01l  if   e           k disk
001100070209     ffiapd01l  if   e           k disk
001200080725     ffidst08l  if   e           k disk    rename(fidst000:fidst08)
001300080728     ffidst09l  if   e           k disk    rename(fidst000:fidst09)
001400110627     ffnlvp9d   cf   e             workstn sfile(lvp0s02:nrr)
001500110617     f                                     sfile(lvp0s01:nrr)
001600070209
001700080725      // ----------------------------------------------------------------------
001800080725      // ? RIEPILOGO INDICATORI ?
001900080725      // ----------------------------------------------------------------------
002000080923      // 01 - consegne da preparare (non ho nessuna bolla in distinta)
002100080923      // 02 - ritiri da preparare (non ho nessun ORM in distinta)
002200080731      // 03 - rosso cod. AUT (AUT <> da quello in distinta)
002300080725      // 05 - sproteggo p.o. in gestione
002400080725      // 06 - non abilito f18
002500080923      // 07 - visualizzo data distinta in testata
002600080725      // 10 - parcel/messaggerie
002700080801      // 11 - ordina subfile  per distinta
002800080801      // 12 - ordina subfile  per AUT
002900080801      // 20 - gestione subfile
003000080801      // 21 - gestione subfile
003100080725      // 28 - errore generico
003200080725      // 30 - comodo
003300080801      // 31 - fine file per subfile
003400080728      // 40 - filiale gestione
003500080728      // 41 - data distinta
003600080728      // 42 - numero distinta dal
003700080728      // 43 - numero distinta al
003800080728      // 44 - autotrasportatore
003900080916      // 45 - parzializzazioni
004000080916      // 46 - parzializzazioni
004100080916      // 47 - parzializzazioni
004200080728      // 90 - riemetto videata
004300080725      // ----------------------------------------------------------------------
004400070209
004500080725      // ? V A R I A B I L I ?
004600080922     d altroaut        s                   like(pdocodaut)
004700080725     d dataiso         s               d   datfmt(*iso)
004800080725     d dataeur         s               d   datfmt(*eur)
004900080730     d contar          s              5i 0
005000080730     d contac          s              5i 0
005100080731     d contapct        s              5i 0
005200080730     d contapdc        s              5i 0
005300080730     d contapdo        s              5i 0
005400080731     d contapro        s              5i 0
005500080801     d nrr             s                   like(recsf2)
005600080731     d totconfc        s              5i 0
005700080731     d totconfr        s              5i 0
005800080731     d totscac         s              5i 0
005900080731     d totscar         s              5i 0
006000080725     d xx              s              3  0
006100080922     d yy              s              3  0
006200110616     d waut            s                   like(dstpdr)
006300110616     d conta           s              4  0
006400110616     d ora             s              4
006500080730     d wfgs            s                   like(dstfgs)
006600080725     d wdata           s              8  0
006700080730     d wndc            s                   like(dstnfv)
006800080728     d wndcdal         s                   like(dstnfv)
006900080801     d wpdr            s                   like(dstpdr)
007000080728     d w0030           s              3  0
007100120131     d comper          s              6  0
007200080729     d $chiusa         s               n
007300080729     d $chiusac        s               n
007400080729     d $chiusar        s               n
007500080731     d $cus            s               n
007600080731     d $infoc          s               n
007700080731     d $infor          s               n
007800080731     d $fine           s               n
007900110616     d $fineplg        s               n
008000110616     d $finedst        s               n
008100080813     d $finepdc        s               n
008200080813     d $finepdo        s               n
008300080731     d $fv6            s              6p 0 dim(600)
008400080731     d $noc            s               n
008500080731     d $nor            s               n
008600080804     d $recnook        s               n
008700140521     d $ricar          s               n
008800080725     d $video          s             10
008900070209
009000080725      // ? S C H I E R E ?
009100110620     d autsk           s              7  0 dim(99)
009200070214     d l1              s              3  0 dim(30)
009300080731     d msg             s             78    dim(15) ctdata perrcd(1)
009400070209
009500080725      // ? D S   I N T E R N E / E S T E R N E ?
009600080725     d wlbdat          ds                  inz
009700080725     d  g02dat                 1      8  0
009800080725     d  g02inv                 9     16  0
009900080725     d  g02err                17     17
010000080725     d  g02tgi                18     22  0
010100070209
010200110616     d fiplgds       e ds                  extname(fiplg00f)
010300110616     d §azute        e ds                  extname(azute00f)
010400080725     d                                     dtaara
010500080731     d §datiute      e ds                  extname(ddatiute)
010600080725     d                                     dtaara
010700080729     d ddstflr       e ds
010800080731     d fidg31ds      e ds
010900080923     d fidg39ds      e ds
011000080731     d fidg45ds      e ds
011100140210     d fior82ds      e ds
011200080729     d fidstds       e ds                  extname(fidst00f)
011300080731     d fior90ds      e ds
011400080731     d fipctds       e ds                  extname(fipct00f)
011500080731     d fipdcds       e ds                  extname(fipdc00t)
011600080731     d fipdods       e ds                  extname(fipdo00t)
011700140210     d fiprods       e ds                  extname(fipro10f)
011800070209     d fnlv24ds      e ds
011900080806     d fnlvp0ds      e ds
012000070209     d kpjba         e ds
012100070209     d og148         e ds
012200080728     d og150         e ds
012300070209     d tibs34ds      e ds                  inz
012400140520     d fior56ds      e ds                  inz
012500070209     d trul06ds      e ds                  extname(trul06ds)
012600070209     d  lin                    1     90  0 dim(30)
012700070209
012800070209     d                sds
012900070209     d  vtcpgm                 1     10
013000080728
013100080728      // ? COSTANTI ?
013200080728     d digitn          c                   const('1234567890')
013300080801     d up              c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
013400080801     d lo              c                   const('abcdefghijklmnopqrstuvwxyz')
013500080801
013600080801      // ? Ordinamento SBFL ?
013700080801      // ----------------------------------------------------------------------
013800080801      //?Costanti per ordinamento subfile
013900080801      // ----------------------------------------------------------------------
014000080801     d MaxKey          c                   2
014100080801     d Ascendente      c                   1
014200080801     d Discendente     c                   2
014300080801     d Carattere       c                   6
014400080801     d Numerico        c                   8
014500080801     d Put             c                   1
014600080801     d EndPut          c                   2
014700080801     d Get             c                   3
014800080801      **********************************************************************
014900080801      * Campi utili:
015000080801      *     nrr        - Numero relativo di record del Subfile
015100080801      *     SizeList   - Dimensione della lista
015200080801      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamen
015300080801      **********************************************************************
015400080801     d NotUsed         s             16A
015500080801     d ReturnSize      s              9B 0
015600080801     d SizeList        s              9B 0
015700080801     d RrnLast         s              5I 0
015800080801     d WrkSort         s              1  0 inz(0)
015900080801      **********************************************************************
016000080801      * Data Structures
016100080801      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
016200080801      *     QLGSCB     - The sort request block for the QLGSORT API
016300080801      *     QLGSCB00   - The sort request block for the QLGSRTIO API
016400080801      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
016500080801      *     QUSEC      - Error structure for the QLGSORT API
016600080801      **********************************************************************
016700080801     d sflrcd          ds
016800080801     d  v2sdaut
016900080801     d  v2sndc
017000080801     d  v2spda
017100080801     d  v2stst
017200080923     d  v2sassc
017300080923     d  v2sassr
017400080801     d  v2sscac
017500080801     d  v2sscar
017600080801     d  v2sconfc
017700080801     d  v2sconfr
017800080801     d  v2scus
017900080801     d  v2sinfoc
018000080801     d  v2sinfor
018100110617     d  v2sora
018200110617     d  v2sfgs
018300110617     d  v2saut
018400080813     d  v2sin01
018500080813     d  v2sin02
018600080813     d  v2sin03
018700110617     d  v1sdaut
018800110617     d  v1sndc
018900110617     d  v1spda
019000110617     d  v1stst
019100110617     d  v1sassc
019200110617     d  v1sassr
019300110617     d  v1sesic
019400110617     d  v1spec
019500110617     d  v1sesir
019600110617     d  v1sper
019700110617     d  v1sesnc
019800110617     d  v1sesnr
019900110617     d  v1snesic
020000110617     d  v1snesir
020100110617     d  v1sfgs
020200110617     d  v1saut
020300110617     d  v1sin01
020400110617     d  v1sin02
020500110617     d  v1sin03
020600110617     d  v1sin60
020700110617     d  v1sin61
020800110621     d  selected                      1A
020900080801
021000080801      /COPY QSYSINC/QRPGLESRC,QLGSORT
021100080801     d QLGKL                         16    DIM(MaxKey)
021200080801     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
021300080801     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
021400080801     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
021500080801     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
021600080801
021700080801      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
021800080801      /COPY QSYSINC/QRPGLESRC,QUSEC
021900080801
022000080725
022100080728      // ? PROTOTIPI ?
022200080728      /copy gaitrasrc/srcprotopr,fidg31r
022300080923      /copy gaitrasrc/srcprotopr,fidg39r
022400080731      /copy gaitrasrc/srcprotopr,fidg45r
022500140520      /copy gaitrasrc/srcprotopr,fior56r
022600140210      /copy gaitrasrc/srcprotopr,fior82r
022700080728      /copy gaitrasrc/srcprotopr,fior90r
022800080728      /copy gaitrasrc/srcprotopr,fnlv24r
022900080806      /copy gaitrasrc/srcprotopr,fnlvp0r1
023000110629      /copy gaitrasrc/srcprotopr,fnlvp0r2
023100080728      /copy gaitrasrc/srcprotopr,tibs34r
023200080728      /copy gaitrasrc/srcprotopr,trul06r
023300080728      /copy gaitrasrc/srcprotopr,xsrda8
023400080801
023500080801     d Qlgsort_pr      pr                  extpgm('QLGSORT')
023600080801     d  pr_QLGSCB                          like(Qlgscb)
023700080801     d  pr_NotUsed1                        like(NotUsed)
023800080801     d  pr_NotUsed2                        like(NotUsed)
023900080801     d  pr_SizeList                        like(SizeList)
024000080801     d  pr_ReturnSize                      like(ReturnSize)
024100080801     d  pr_QUSEC                           like(Qusec)
024200080801     d                                     options(*varsize)
024300080801
024400080801     d Qlgsrtio_pr     pr                  extpgm('QLGSRTIO')
024500080801     d  pr_QLGSCB00                        like(QLGSCB00)
024600080801     d  pr_SflRcd                          like(SflRcd)
024700080801     d  pr_NotUsed1                        like(NotUsed)
024800080801     d  pr_SizeList                        like(SizeList)
024900080801     d  pr_NotUsed2                        like(NotUsed)
025000080801     d  pr_QUSEC                           like(Qusec)
025100080801     d                                     options(*varsize)
025200080801
025300080801     d Qlgsrtio_pr2    pr                  extpgm('QLGSRTIO')
025400080801     d  pr_QLGSCB00                        like(QLGSCB00)
025500080801     d  pr_NotUsed1                        like(NotUsed)
025600080801     d  pr_SflRcd                          like(SflRcd)
025700080801     d  pr_SizeList                        like(SizeList)
025800080801     d  pr_NotUsed2                        like(NotUsed)
025900080801     d  pr_QUSEC                           like(Qusec)
026000080801     d                                     options(*varsize)
026100070209
026200080725      // ----------------------------------------------------------------------
026300080725
026400080725     c     *entry        plist
026500080725     c                   parm                    kpjba
026600071217
026700080725      /free
026800080725
026900080731       //?operazioni iniziali
027000080725         exsr routinz;
027100080725
027200080729         dow not $fine;
027300080729
027400080731       //?gestione delle videate
027500080729          select;
027600080729
027700080731       //?gestione prima videata
027800080729          when $video = 'D01';
027900080923           *in07 = *off;
028000080729           exsr gesd01;
028100080729
028200110617       //?gestione subfile
028300110617          when $video = 'S02' or $video = 'S01';
028400080923           *in07 = *on;
028500080729           exsr gess02;
028600110616
028700080729          endsl;
028800080801
028900080731         enddo;
029000080729
029100080729         *inlr = *on;
029200080725
029300110617       // ----------------------------------------------------------------------
029400110617       //?Operazioni iniziali.
029500110617       // ----------------------------------------------------------------------
029600110617       begsr routinz;
029700110617
029800110617         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
029900110617
030000110617       //?recupero dei dati utente
030100110617          in(e) §azute;
030200110617          if not %error;
030300110617           in(e) §datiute;
030400110617          endif;
030500110617          if %error or rsut = *blanks;
030600110617           tibs34r(tibs34ds);
030700110617           in §azute;
030800110617           in §datiute;
030900110617          endif;
031000110617
031100110617       //?imposto la filiale in gestione
031200110617          v1cfgs = dutpou;
031300110617       //?decodifico la filiale gestione
031400110617          v1dfgs = dutdpo;
031500110617       //?abilito f18
031600110617          *in06 = (dutlpo = '2' or dutlpo = *blanks);
031700110617
031800110617       //?carico schiera filiali gestite
031900110617          clear trul06ds;
032000110617          d06cod = '£1';
032100110617          d06key = %editc(%dec(dutpou):'X');
032200110617          kpjbu = trul06ds;
032300110617          trul06r(kpjba);
032400110617          trul06ds = kpjbu;
032500110617          l1 = lin;
032600110617
032700110617       //?controllo se PDA attivo sulla filiale in gestione
032800110617       //?                 e se gestisce parcel/messaggerie
032900110617          clear og148;
033000110617          clear og150;
033100110617          chain v1cfgs azorg01l;
033200110617          if  %found(azorg01l) and orgfva = *blanks;
033300110617           og148 = orgde8;
033400110617           og150 = orgdf0;
033500110617          endif;
033600110617          if §ogpdaorm = *blanks and §ogpdacon = *blanks;
033700110617           *in28 = *on;
033800110617           *in40 = *on;
033900110617           v1cmsg = msg(01);
034000110617          endif;
034100110617
034200110617       //?parcel messaggerie
034300110617          *in10 = (§ogpkm ='S');
034400110617
034500110617       //?imposto la data del giorno
034600110617          wdata = %dec(%date());
034700110617          dataiso = %date(wdata:*iso);
034800110617          dataeur = dataiso;
034900110617          v1cddc = %dec(dataeur);
035000110617          v1tipm = 'D';
035100110628          v1segc = '>';
035200110628          v1segr = '>';
035300110617
035400110617       //?imposto il range distinta
035500110617          v1ndcal = *all'9';
035600110617
035700110617       //?gestione video D01
035800110617         $video = 'D01';
035900110617         $fine =*off;
036000110617
036100110617       endsr;
036200110617
036300080731       // ----------------------------------------------------------------------
036400080731       //?Gestione prima videata (parametri)
036500080731       // ----------------------------------------------------------------------
036600080725       begsr gesd01;
036700080725
036800080731       //?emetto la videata
036900080725          write lvp0t01;
037000080725          exfmt lvp0d01;
037100070209
037200080731       //?spengo indicatori di comodo
037300080725          *in28 = *off;
037400080725          *in90 = *off;
037500080731       //?pulisco campo messaggi
037600080725          clear v1cmsg;
037700080725
037800080731       //?esecuzione comandi
037900080725         select;
038000080729
038100080731       //?f3=fine
038200080725          when *inkc;
038300080725           exsr sr_fine;
038400080731       //?f18=cambio filiale gestione
038500080725          when *inks;
038600080725           *in05 = *on;
038700080731           leavesr;
038800080725         other;
038900080729
039000080731       //?controllo filiale gestione se F18
039100080725          if *in05;
039200080725           exsr sr_contrfgs;
039300080725           if not *in28;
039400080725            *in05 = *off;
039500080731            leavesr;
039600080725           endif;
039700080725          endif;
039800080731       //?controllo i dati della videata
039900080725          exsr sr_contrd01;
040000080725          if *in28 or *in90;
040100080725           leavesr;
040200080725          endif;
040300080725         endsl;
040400080801
040500110616       //?preparo i dati per il subfile delle distinte o delle attività
040600080801       //?accendo indicatore per impostare subito l'ordinamento
040700080801       //?di dft del subfile (per distinta)
040800110627         *in11 = *off;
040900110627         *in12 = *on;
041000110616         exsr sr_carica;
041100110620
041200110701       //?dati
041300110616          if v1tipm = 'D';
041400080725         $video = 'S02';
041500110616          endif;
041600110701       //?operatività
041700110627          if v1tipm = 'O';
041800110616         $video = 'S01';
041900110616          endif;
042000080725
042100080725       endsr;
042200080731
042300110616       // ----------------------------------------------------------------------
042400110616       //?Gestione subfile distinte
042500110616       // ----------------------------------------------------------------------
042600110616       begsr gess02;
042700110616
042800110616       //?ho caricato dati emetto il subfile
042900110616         if nrr > 0;
043000110616          exsr sr_sfl02;
043100110616         endif;
043200110616
043300110616       //?non ho caricato dati emetto videata di segnalazione
043400110621         if nrr = 0 ;
043500110616          exsr sr_d02;
043600110616         endif;
043700110616
043800110616       endsr;
043900110616
044000080731       // ----------------------------------------------------------------------
044100080731       //?Controllo filiale gestione
044200080731       // ----------------------------------------------------------------------
044300080729       begsr sr_contrfgs;
044400080729
044500080729         *in40 = *off;
044600080729         clear v1dfgs;
044700080729         clear og148;
044800080729         clear og150;
044900080729
045000080731       //?deve essere una filiale gestita dall'utente
045100080729         if %lookup(v1cfgs:lin) = *zeros;
045200080729          *in28 = *on;
045300080729          *in40 = *on;
045400080729          v1cmsg = msg(02);
045500080729          leavesr;
045600080729         endif;
045700080729
045800080731       //?decodifico la filiale gestione
045900080731       //?controllo se PDA attivo sulla filiale in gestione
046000080731       //?                 e se gestisce parcel/messaggerie
046100080729          clear og148;
046200080729          clear og150;
046300080729          chain v1cfgs azorg01l;
046400080729          if  %found(azorg01l) and orgfva = *blanks;
046500080729           v1dfgs = orgdes;
046600080729           og148 = orgde8;
046700080729           og150 = orgdf0;
046800080729          endif;
046900080729          if §ogpdaorm = *blanks and §ogpdacon = *blanks;
047000080729           *in28 = *on;
047100080729           *in40 = *on;
047200080729           v1cmsg = msg(01);
047300080729          endif;
047400080729
047500080731       //?parcel messaggerie
047600080729          *in10 = (§ogpkm ='S');
047700080729
047800080729       endsr;
047900080729
048000080731       // ----------------------------------------------------------------------
048100080731       //?Controllo dati prima videata (parametri)
048200080731       // ----------------------------------------------------------------------
048300080729       begsr sr_contrd01;
048400080729
048500080731       //?spengo indicatori di errore
048600080729         *in41 = *off;
048700080729         *in42 = *off;
048800080729         *in43 = *off;
048900080729         *in44 = *off;
049000080916         *in45 = *off;
049100080916         *in46 = *off;
049200080916         *in47 = *off;
049300080915
049400080915       //?pulisco campi di comodo
049500080915         clear wdata;
049600080915         clear wndcdal;
049700080915         clear waut;
049800080729
049900080731       //?controllo data distinta
050000080729         exsr sr_contrddc;
050100080729         if *in28;
050200080729          leavesr;
050300080729         endif;
050400080729
050500080731       //?controllo distinta
050600080729         exsr sr_contrndc;
050700080731         if *in28 or *in90;
050800080729          leavesr;
050900080729         endif;
051000080729
051100080731       //?controllo autotrasportatore
051200080729         exsr sr_contraut;
051300080729         if *in28 or *in90;
051400080729          leavesr;
051500080729         endif;
051600080916
051700080916       //?controllo parzializzazioni
051800080916         exsr sr_contrpar;
051900080916         if *in28;
052000080916          leavesr;
052100080916         endif;
052200080729
052300080729       endsr;
052400080729
052500080731       // ----------------------------------------------------------------------
052600110616       //?Carico i dati per subfile distinte e attività
052700080731       // ----------------------------------------------------------------------
052800080729       begsr sr_carica;
052900080730
053000080731       //?pulisco il subfile distinte
053100080730         exsr sr_puls02;
053200080729
053300110616       //?pulisco il subfile attività
053400110616         exsr sr_puls01;
053500110616
053600080731       //?carico le distinte da elaborare
053700080729         exsr sr_cardst;
053800080729
053900080731       //?elaboro i dati delle distinte
054000080729         exsr sr_elabdst;
054100110620
054200110621       if nrr <> 0;
054300110620           if *in11;
054400110620            exsr sr_ordina1;
054500110620            exsr sr_ordinaA1;
054600110620           endif;
054700110620           if *in12;
054800110620            exsr sr_ordina2;
054900110620            exsr sr_ordinaA2;
055000110620           endif;
055100110621       endif;
055200080729
055300080729       endsr;
055400080801
055500080731       // ----------------------------------------------------------------------
055600080731       //?Emetto il subfile distinte
055700080731       // ----------------------------------------------------------------------
055800080731       begsr sr_sfl02;
055900080801
056000080731       //?emetto la videata
056100110617         if  $video = 'S02';
056200080731         write lvp0t01;
056300080731         write lvp0z02;
056400080731         exfmt lvp0c02;
056500110617         endif;
056600110617         if  $video = 'S01';
056700110617         write lvp0t01;
056800110617         write lvp0z01;
056900110617         exfmt lvp0c01;
057000110617         endif;
057100080731       //?spengo indicatori di comodo
057200080731         *in28 = *off;
057300080731       //?pulisco campo messaggi
057400080731         clear v2cmsg;
057500080731       //?esecuzione comandi
057600080731         select;
057700080731
057800080731       //?f3=fine
057900080731          when *inkc;
058000080731           exsr sr_fine;
058100080731       //?f5=aggiorna
058200080731          when *inke;
058300080731           exsr sr_carica;
058400080731           leavesr;
058500080801       //?f11=ordina x AUT
058600080801          when *inkk and *in11;
058700080801           *in11 = *off;
058800080801           *in12 = *on;
058900110617           clear v2cdaut;
059000110620           exsr sr_ordina2;
059100110617           exsr sr_ordinaA2;
059200080801           leavesr;
059300080801       //?f11=ordina x NUM
059400080801          when *inkk and *in12;
059500080801           *in11 = *on;
059600080801           *in12 = *off;
059700110617           clear v2cdaut;
059800080801           exsr sr_ordina1;
059900110617           exsr sr_ordinaA1;
060000080801           leavesr;
060100080731       //?f12=ritorno
060200080731          when *inkl;
060300110620           clear v2cdaut;
060400080731           $video = 'D01';
060500080731           leavesr;
060600110616       //?f8=monitor attività
060700110617          when *inkH;
060800110617         if  $video = 'S02';
060900110616           $video = 'S01';
061000110617         else;
061100110617           $video = 'S02';
061200110617         endif;
061300110616           leavesr;
061400080731         other;
061500080801       //?controllo testata subfile (posizionamento)
061600080801           exsr sr_ctrc02;
061700080806       //?controllo opzione subfile
061800080806           exsr sr_ctrs02;
061900080731         endsl;
062000080731
062100080731       endsr;
062200080731
062300110620     C/EJECT
062400080731       // ----------------------------------------------------------------------
062500080731       //?Emetto video di segnalazione distinte non caricate
062600080731       // ----------------------------------------------------------------------
062700080731       begsr sr_d02;
062800080731
062900080731       //?emetto la videata
063000080731         write lvp0t01;
063100080731         exfmt lvp0d02;
063200080731
063300080731       //?spengo indicatori di comodo
063400080731         *in28 = *off;
063500080731       //?pulisco campo messaggi
063600080731         clear v2cmsg;
063700080731       //?esecuzione comandi
063800080731         select;
063900080731
064000080731       //?f3=fine
064100080731          when *inkc;
064200080731           exsr sr_fine;
064300080731       //?f5=aggiorna
064400080731          when *inke;
064500080731           exsr sr_carica;
064600080731           leavesr;
064700080731       //?f12=ritorno
064800080731          when *inkl;
064900080731           $video = 'D01';
065000080731           leavesr;
065100080731         other;
065200080731         endsl;
065300080731
065400080731       endsr;
065500080725
065600080731       // ----------------------------------------------------------------------
065700080731       //?Controllo data distinta
065800080731       // ----------------------------------------------------------------------
065900080725       begsr sr_contrddc;
066000080725
066100080731       //?la data è obbligatoria
066200080725         if v1cddc = *zeros;
066300080725          *in28 = *on;
066400080725          *in41 = *on;
066500080725          v1cmsg = msg(03);
066600080725          leavesr;
066700080725         endif;
066800080725
066900080731       //?deve essere una data valida
067000080725         clear wlbdat;
067100080725         g02dat = v1cddc;
067200080725         xsrda8(wlbdat);
067300080725         if g02err = '1';
067400080725          *in28 = *on;
067500080725          *in41 = *on;
067600080725          v1cmsg = msg(03);
067700080725          leavesr;
067800080725         endif;
067900080725
068000080725        v1cddc = g02dat;
068100080725        wdata = g02inv;
068200080729
068300080731       //?controllo se ci sono distinte nel giorno selezionato
068400080729         dstnpg = 4;
068500080729         chain (v1cfgs:dstnpg:wdata) fidst08l;
068600080729         if not %found(fidst08l);
068700080729          *in28 = *on;
068800080922          *in41 = *on;
068900080729          v1cmsg = msg(04);
069000080729          leavesr;
069100080729         endif;
069200080725
069300080725       endsr;
069400080725
069500080731       // ----------------------------------------------------------------------
069600080731       //?Controllo distinta
069700080731       // ----------------------------------------------------------------------
069800080725       begsr sr_contrndc;
069900080801
070000080912       //?se richiesto un range di distinte non posso richiedere
070100080912       //?anche l'autotrasportatore
070200080915         if v1caut > *zeros and (v1ndcda > *zeros or
070300080915                                (v1ndcal > *zeros and v1ndcal <> 999999));
070400080912          *in28 = *on;
070500080912          *in44 = *on;
070600080912          v1cmsg = msg(14);
070700080912          leavesr;
070800080912         endif;
070900080912
071000080912       //?se impostato AUT senza distinta vado a fine dei controlli
071100080912         if v1caut <> *blanks;
071200080801          leavesr;
071300080801         endif;
071400080725
071500080731       //?numero distinta dal --> se impostato
071600080725         if v1ndcda <> *blanks;
071700080731       //?pulisco se a zero
071800080728          if v1ndcda = *zeros;
071900080728           clear v1ndcda;
072000080915           leavesr;
072100080728          endif;
072200080731       //?ricerca della distinta
072300080728          if %scan('?':v1ndcda) > *zeros;
072400080731           *in42 = *on;
072500080731           *in90 = *on;
072600080728           clear fidg31ds;
072700080728           fdgfgs = v1cfgs;
072800080728           fdggda = wdata;
072900080728           fdggal = wdata;
073000080728           fdgric = 'S';
073100080728           fdgtfv = '2';
073200080728           fdgtpv = 'P';
073300080728           fdgopz = '1';
073400080731           clear $fv6;
073500080731       //?impostato a tutti 99999
073600080731           xx = 1;
073700080731           for xx by 1 to 600;
073800080731            $fv6(xx) = 999999;
073900080728           endfor;
074000080728           kpjbu = fidg31ds;
074100080728           fidg31r(kpjba:$fv6);
074200080728           fidg31ds = kpjbu;
074300080731       //?controllo se scelta una distinta o no
074400080728           if fdgnfv = *zeros;
074500080728            clear fidg31ds;
074600080915            clear v1ndcda;
074700080915            leavesr;
074800080728           else;
074900080731            v1ndcda = %editc(%dec(fdgnfv):'X');
075000080728           endif;
075100080728          endif;
075200080731       //?controllo validità
075300080915          if %check(digitn:v1ndcda) = *zeros;
075400080915           wndcdal = %dec(v1ndcda:6:0);
075500080915           dstnpg = 4;
075600080915           chain (v1cfgs:dstnpg:wndcdal) fidst09l;
075700080915          endif;
075800080728          select;
075900080731       //?campo non numerico
076000080728          when %check(digitn:v1ndcda) > *zeros;
076100080728           *in28 = *on;
076200080728           *in42 = *on;
076300080728           v1cmsg = msg(05);
076400080728           leavesr;
076500080731       //?distinta non trovata o annullata
076600080728          when not %found(fidst09l) or dstatb <> *blanks;
076700080728           *in28 = *on;
076800080728           *in42 = *on;
076900080728           v1cmsg = msg(06);
077000080728           leavesr;
077100080731       //?distinta non della data richiesta
077200080728          when dstdfv <> wdata;
077300080728           *in28 = *on;
077400080728           *in42 = *on;
077500080728           v1cmsg = msg(04);
077600080728           leavesr;
077700080731       //?distinta non a PDA
077800080728          when dstpda = 'N';
077900080728           *in28 = *on;
078000080728           *in42 = *on;
078100080728           v1cmsg = msg(08);
078200080728           leavesr;
078300080728          other;
078400080731       //?imposto lo stesso numero nel campo 'al' se non impostato
078500080728           if v1ndcal = *zeros;
078600080728            v1ndcal = wndcdal;
078700080728           endif;
078800080728          endsl;
078900080728         endif;
079000080725
079100080731       //?numero distinta al deve essere impostato a con distinta o 999999
079200080728         if v1ndcal <= *zeros;
079300080728          *in28 = *on;
079400080728          *in43 = *on;
079500080728          v1cmsg = msg(09);
079600080728          leavesr;
079700080728         endif;
079800080725
079900080731       //?range incompatibile
080000080728         if wndcdal > v1ndcal;
080100080728          *in28 = *on;
080200080728          *in43 = *on;
080300080728          v1cmsg = msg(10);
080400080728          leavesr;
080500080728         endif;
080600080725
080700080731       //?se richiesta una sola distinta
080800080728         if wndcdal = v1ndcal;
080900080731       //?controllo se distinta chiusa per consegne e ritiri
081000080730          wfgs = v1cfgs;
081100080730          wndc = wndcdal;
081200080730          exsr sr_dstchiusa;
081300080730          if $chiusa;
081400080730           *in28 = *on;
081500080730           *in42 = *on;
081600080730           v1cmsg = msg(07);
081700080730           leavesr;
081800080730          endif;
081900080728         endif;
082000080725
082100080725       endsr;
082200080728
082300080731       // ----------------------------------------------------------------------
082400080731       //?Controllo autotrasportatore
082500080731       // ----------------------------------------------------------------------
082600080728       begsr sr_contraut;
082700080729
082800080729         clear v1daut;
082900080804       //?se autotrasportatore non impostato vado a fine controlli
083000080804         if v1caut = *blanks;
083100080804          leavesr;
083200080804         endif;
083300080804       //?se codice a zero lo metto a blank
083400080804         if v1caut = *zeros;
083500080804          clear v1caut;
083600080804          *in90 = *on;
083700080804          leavesr;
083800080804         endif;
083900080729
084000080731       //?se richiesto un range di distinte non posso richiedere
084100080731       //?anche l'autotrasportatore
084200080915         if wndcdal > *zeros or (v1ndcal > *zeros and v1ndcal < 999999);
084300080729          *in28 = *on;
084400080729          *in44 = *on;
084500080730          v1cmsg = msg(14);
084600080729          leavesr;
084700080729         endif;
084800080728
084900080731       //?ricerca dell'autotrasportatore
085000080728         if %scan('?':v1caut) > *zeros;
085100080728          *in44 = *on;
085200080728          *in90 = *on;
085300080728          clear fnlv24ds;
085400080728          d24fil = dutpou;
085500080728          d24tip = 'A';
085600080728          d24flg = 'R';
085700080728          kpjbu = fnlv24ds;
085800080728          fnlv24r(kpjba);
085900080731       //?è stato scelto un codice lo imposto a video
086000080728          fnlv24ds = kpjbu;
086100080728          if d24pdr <> *zeros;
086200080731           v1caut = %editc(%dec(d24pdr):'X');
086300080728           v1daut = d24rsc;
086400080728           leavesr;
086500080728          endif;
086600080731       //?non è stato scelto nessun codice pulisco i campi a video
086700080728          clear v1caut;
086800080728          clear v1daut;
086900080728          leavesr;
087000080728         endif;
087100080728
087200080731       //?controllo
087300080731         if v1caut <> *blanks and v1caut <> *zeros;
087400080731       //?deve essere numerico
087500080728          if %check(digitn:v1caut) > *zeros;
087600080728           *in28 = *on;
087700080728           *in44 = *on;
087800080730           v1cmsg = msg(11);
087900080728           leavesr;
088000080728          endif;
088100080731       //?deve essere un codice valido
088200080728          apdtip = 'A';
088300080801          apdpdr = %dec(v1caut:7:0);
088400080728          chain (apdtip:apdpdr) fiapd01l;
088500080728          if not %found(fiapd01l) or apdatb <> *blanks;
088600080728           *in28 = *on;
088700080728           *in44 = *on;
088800080730           v1cmsg = msg(12);
088900080728           leavesr;
089000080728          endif;
089100080728         endif;
089200080728
089300080731       //?l'autotrasportatore deve essere in gestione all'utente in base alla
089400080731       //?filiale gestione impostata a video
089500080728         w0030 = %dec(%subst(v1caut:1:3):3:0);
089600080728         if w0030 <> v1cfgs;
089700080728          *in28 = *on;
089800080728          *in44 = *on;
089900080730          v1cmsg = msg(13);
090000080728          leavesr;
090100080728         endif;
090200080729
090300080731       //?imposto campo numerico dell'AUT
090400080731           waut = %dec(v1caut:7:0);
090500080728
090600080728       endsr;
090700080916
090800080916       // ----------------------------------------------------------------------
090900080916       //?Controllo parzializzazioni
091000080916       // ----------------------------------------------------------------------
091100080916       begsr sr_contrpar;
091200080916
091300080916       //?le parzializzazioni non sono in alternativa
091400080916         if v1sca <> *blanks and (v1ok <> *blanks or v1info <> *blanks or
091500080916                                  v1cus <> *blanks);
091600080916          *in28 = *on;
091700080916          *in45 = *on;
091800080916          v1cmsg = msg(15);
091900080916          leavesr;
092000080916         endif;
092100080916         if v1ok <> *blanks and (v1info <> *blanks or v1cus <> *blanks);
092200080916          *in28 = *on;
092300080916          *in46 = *on;
092400080916          v1cmsg = msg(15);
092500080916          leavesr;
092600080916         endif;
092700080916         if v1info <> *blanks and v1cus <> *blanks;
092800080916          *in28 = *on;
092900080916          *in47 = *on;
093000080916          v1cmsg = msg(15);
093100080916          leavesr;
093200080916         endif;
093300080916
093400080916       endsr;
093500080730
093600080731       // ----------------------------------------------------------------------
093700110616       //?Pulisco subfile attività
093800080731       // ----------------------------------------------------------------------
093900110616       begsr sr_puls01;
094000080730
094100080801         clear nrr;
094200080801         clear rrnlast;
094300110616         clear V1TASSC;
094400110616         clear V1TASSR;
094500110616         clear V1TESIC;
094600110616         clear V1TPESC;
094700110616         clear V1TESIR;
094800110616         clear V1TPESR;
094900110616         clear V1TESNC;
095000110616         clear V1TESNR;
095100110616         clear V1TNESC;
095200110616         clear V1TNESR;
095300080801
095400110616         *in85 = *off;
095500110616         *in86 = *off;
095600110616         write lvp0c01;
095700110616         *in85 = *on;
095800110616         *in86 = *on;
095900080730
096000110616         recsf2 = 1;
096100110616         *in87 = *off;
096200080923
096300080923       //?imposto la data in testata
096400080923         vtcddc = v1cddc;
096500080730
096600080730       endsr;
096700080729
096800110616       // ----------------------------------------------------------------------
096900110616       //?Pulisco subfile distinte
097000110616       // ----------------------------------------------------------------------
097100110616       begsr sr_puls02;
097200110616
097300110616         clear nrr;
097400110616         clear rrnlast;
097500110617
097600110616         *in20 = *off;
097700110616         *in21 = *off;
097800110616         write lvp0c02;
097900110616         *in20 = *on;
098000110616         *in21 = *on;
098100110616
098200110616         recsf2 = 1;
098300110621         *in31 = *off;
098400110616
098500110616       //?imposto la data in testata
098600110616         vtcddc = v1cddc;
098700110616
098800110616       endsr;
098900110616
099000080731       // ----------------------------------------------------------------------
099100080731       //?Carico le distinte a PDA
099200080731       // ----------------------------------------------------------------------
099300080729       begsr sr_cardst;
099400080813
099500080813         $finedst = *off;
099600080729
099700080731       //?sql divisi in base alle selezioni fatte
099800080731       //?prepara cursore
099900080729         select;
100000080729
100100080731       //?sql per tutte le distinte del giorno dell'AUT
100200080912         when waut > *zeros;
100300080729          exec sql
100400080729           declare autdst cursor for select fidst02l.*
100500080729           from fidst02l where dstnpg = 4 and dstdfv = :wdata and
100600110621           dstpda <> 'N' and dstpdr = :waut and dstatb = '';
100700080729
100800080731       //?sql per tutte le distinte del giorno
100900080729         when wndcdal = *zeros;
101000080729          exec sql
101100080729           declare alldst cursor for select fidst02l.*
101200080729           from fidst02l where dstnpg = 4 and dstdfv = :wdata and
101300110621           dstpda <> 'N' and dstfgs = :v1cfgs and dstatb = '';
101400080729
101500080731       //?sql per range di distinte del giorno
101600080729         when wndcdal <> *zeros;
101700080729          exec sql
101800080729           declare rangedst cursor for select fidst02l.*
101900080729           from fidst02l where dstnpg = 4 and dstdfv = :wdata and
102000080729           dstpda <> 'N' and dstnfv between :wndcdal and :v1ndcal
102100110621           and dstfgs = :v1cfgs and dstatb = '';
102200080729
102300080729         endsl;
102400080729
102500080731       //?open cursori
102600080729         select;
102700080731       //?sql per tutte le distinte del giorno dell'AUT
102800080731         when waut <> *zeros;
102900080729          exec sql open autdst;
103000080731       //?sql per tutte le distinte del giorno
103100080729         when wndcdal = *zeros;
103200080729          exec sql open alldst;
103300080731       //?sql per range di distinte del giorno
103400080729         when wndcdal <> *zeros;
103500080729          exec sql open rangedst;
103600080729         endsl;
103700080729
103800080729       endsr;
103900080729
104000080731       // ----------------------------------------------------------------------
104100080731       //?Elaboro le distinte a PDA
104200080731       // ----------------------------------------------------------------------
104300080730       begsr sr_elabdst;
104400080729
104500080813         dow not $finedst;
104600080729
104700080731       //?leggo cursori
104800080730          select;
104900080731       //?sql per tutte le distinte del giorno dell'AUT
105000080731          when waut <> *zeros;
105100080730           exec sql fetch next from autdst into: fidstds;
105200080731       //?sql per tutte le distinte del giorno
105300080730          when wndcdal = *zeros;
105400080730           exec sql fetch next from alldst into: fidstds;
105500080731       //?sql per range di distinte del giorno
105600080730          when wndcdal <> *zeros;
105700080730           exec sql fetch next from rangedst into: fidstds;
105800080730          endsl;
105900080729
106000080731       //?fine file o errore sql esco
106100080730          if sqlcod = 100 or sqlcod < 0;
106200110621         *in31 = *on;
106300110621         *in87 = *on;
106400080813           $finedst = *on;
106500080730           leave;
106600080730          endif;
106700080729
106800080730          ddstflr = dstflr;
106900080813       //?spengo indicatori per richieste info
107000080813          $infoc = *off;
107100080813          $infor = *off;
107200080729
107300080731       //?controllo congruenza tipo distinta
107400120223          if v1tdis <> *blanks;
107500120223           if v1tdis = 'P' and dsttpm <> ' ';
107600080730           iter;
107700120223           endif;
107800120223           if v1tdis = 'M' and dsttpm <> v1tdis;
107900120223           iter;
108000120223           endif;
108100120223          endif;
108200080729
108300080731       //?controllo congruenza tipo uscita
108400080730          if v1tusc <> *blanks and dstfpp <> v1tusc;
108500080730           iter;
108600080730          endif;
108700080729
108800080731       //?controllo se distinta chiusa per consegne e ritiri
108900080730          wfgs = dstfgs;
109000080730          wndc = dstnfv;
109100080730          exsr sr_dstchiusa;
109200080730          if $chiusa;
109300080730           iter;
109400080730          endif;
109500080729
109600080731       //?calcolo i totali dei dati da preparare
109700080731          exsr sr_dstsca;
109800080731       //?calcolo i totali dei dati da confermare
109900080731          exsr sr_dstconf;
110000080731       //?controllo se OK ad uscire
110100080731          wfgs = dstfgs;
110200080731          wndc = dstnfv;
110300080731          exsr sr_dstcus;
110400080731       //?recupero ragione sociale AUT
110500080801          wpdr = dstpdr;
110600080731          exsr sr_dstaut;
110700110616       //?calcolo i totali dei dati esitati
110800110616          exsr sr_dstesiti;
110900110616       //?scrivo subfile distinte e attività
111000080731          *in03 = *off;
111100080813          v2sin03 = '0';
111200080730          exsr sr_cars02;
111300080731       //?leggo se ho dei dati preparati per altro AUT
111400080731       //?così lo emetto a video
111500080922
111600080922       //?carico prima gli AUT diversi in SK
111700110620         if $finedst=*off;
111800080922         exsr sr_skaut;
111900080922          xx = 1;
112000080922          for xx by 1 to 99;
112100080922           if autsk(xx) > 0;
112200080922           wpdr = autsk(xx);
112300110617           exsr sr_altroaut;
112400110617           else;
112500110617           leave;
112600080922           endif;
112700080922          endfor;
112800110620           endif;
112900080730         enddo;
113000080801
113100080801       //?indicatore per fine file
113200080801       //?imposto il numero di record caricati
113300080801         rrnlast = nrr;
113400080729
113500080731       //?chiudo i cursori
113600080730         select;
113700080731       //?sql per tutte le distinte del giorno dell'AUT
113800080731         when waut <> *zeros;
113900080730          exec sql close autdst;
114000080731       //?sql per tutte le distinte del giorno
114100080730         when wndcdal = *zeros;
114200080730          exec sql close alldst;
114300080731       //?sql per range di distinte del giorno
114400080730         when wndcdal <> *zeros;
114500080730          exec sql close rangedst;
114600080730         endsl;
114700080729
114800080729       endsr;
114900080729
115000080731       // ----------------------------------------------------------------------
115100080731       //?Controllo se distinta chiusa consegne e ritiri
115200080731       // ----------------------------------------------------------------------
115300080729       begsr sr_dstchiusa;
115400080729
115500080731         $chiusar = *off;
115600080731         $chiusac = *off;
115700080731         $chiusa = *off;
115800080729
115900080731       //?se c'è la fase RCH o la RQE è chiusa x i ritiri
116000080729         clear fior90ds;
116100080729         or90tla = 'C';
116200080729         or90fas = 'RCH';
116300080730         or90fgs = wfgs;
116400080730         or90ndc = wndc;
116500080729         fior90r(kpjba:fior90ds);
116600080731       //?se trovo la fase RCH imposto che è chiusa
116700080729         if or90ndcok <> *blanks;
116800080731          $chiusar = *on;
116900080729         endif;
117000080731       //?se non trovo la fase RCH provo con la fase RQE
117100080729         if or90ndcok = *blanks;
117200080729          clear fior90ds;
117300080729          or90tla = 'C';
117400080729          or90fas = 'RQE';
117500080730          or90fgs = wfgs;
117600080730          or90ndc = wndc;
117700080729          fior90r(kpjba:fior90ds);
117800080731       //?se trovo la fase RQE imposto che è chiusa
117900080729          if or90ndcok <> *blanks;
118000080731           $chiusar = *on;
118100080729          endif;
118200080730         endif;
118300080730
118400080731       //?se c'è la fase CRI è chiusa x le consegne
118500080730         clear fior90ds;
118600080730         or90tla = 'C';
118700080730         or90fas = 'CRI';
118800080730         or90fgs = wfgs;
118900080730         or90ndc = wndc;
119000080730         fior90r(kpjba:fior90ds);
119100080731       //?se trovo la fase CRI imposto che è chiusa
119200080730         if or90ndcok <> *blanks;
119300080731          $chiusac = *on;
119400080730         endif;
119500080731       //?se non trovo la fase CRI controllo se chiusa con flag
119600080730         if or90ndcok = *blanks and dstfcf = 'S';
119700080731          $chiusac = *on;
119800080729         endif;
119900080730
120000080731       //?la distinta è chiusa x le consegne e i ritiri imposto
120100080731       //?il flag
120200080730         if $chiusar and $chiusac;
120300080730          $chiusa = *on;
120400080730         endif;
120500080729
120600080729       endsr;
120700080731
120800080731       // ----------------------------------------------------------------------
120900080731       //?Calcolo il totale da preparare
121000080731       // ----------------------------------------------------------------------
121100080731       begsr sr_dstsca;
121200080731
121300080731       //?inizializzo i vari campi di appoggio
121400080731         clear contar;
121500080731         clear contac;
121600080731         clear contapdo;
121700080731         clear contapdc;
121800080731         clear totscar;
121900080731         clear totscac;
122000080731         $nor = *off;
122100080731         $noc = *off;
122200080731
122300080801       //?RITIRI
122400080922       //?se distinta non è già chiusa per ritiri
122500080922         if $chiusar = *off;
122600080801       //?per ogni distinta devo contare quanti ritiri
122700080731       //?sono presenti in distinta (da preparare)
122800080801       //?--> FNORG
122900080801         if dstpda = 'O' or dstpda = 'E';
123000080801          exec sql
123100080801           select count(*) into :contar from fnorg00f where
123200080801            orgfgs = :dstfgs and orgndc = :dstnfv;
123300080801       //?se ho trovato almeno un ritiro per la distinta/AUT
123400080801       //?conto quanti ritiri ho già preparato per il PDA
123500080801       //?--> FIPDO
123600080801          if contar > *zeros;
123700080801           exec sql
123800080801            select count(*) into :contapdo from fipdo00t where
123900080801            pdofgs = :dstfgs and pdondc = :dstnfv and
124000080801            pdocodaut = :dstpdr and pdoatb = '';
124100080801       //?calcolo il totale dei ritiri da preparare
124200080801           totscar = contar - contapdo;
124300080801          else;
124400080801       //?non ho neanche un ritiro sulla distinta lo evidenzio
124500080801           $nor = *on;
124600080801          endif;
124700080917         else;
124800080917       //?non sono previsti ritiri sulla distinta lo evidenzio
124900080917          $nor = *on;
125000080801         endif;
125100080922         endif;
125200080731
125300080801       //?CONSEGNE
125400080922       //?se distinta non è già chiusa per le consegne
125500080922         if $chiusac = *off;
125600080801       //?per ogni distinta devo contare quante consegne
125700080801       //?sono presenti in distinta (da preparare)
125800080801         if dstpda = 'C' or dstpda = 'E';
125900080801       //?--> FNARB
126000080801          exec sql
126100080801           select count(*) into :contac from fnarb00f where
126200080801            arbifp = :dstfgs and arbndc = :dstnfv;
126300080801       //?se ho trovato almeno una consegna per la distinta/AUT
126400080801       //?conto quante consegne ho già preparato per il PDA
126500080801       //?--> FIPDC
126600080801          if contac > *zeros;
126700080801           exec sql
126800080801            select count(*) into :contapdc from fipdc00t where
126900080801            pdcfgs = :dstfgs and pdcndc = :dstnfv and
127000080801            pdccodaut = :dstpdr and pdcatb = '';
127100080731       //?calcolo il totale delle consegne da preparare
127200080801           totscac = contac - contapdc;
127300080801          else;
127400080731       //?non ho neanche una consegna sulla distinta lo evidenzio
127500080801           $noc = *on;
127600080801          endif;
127700080917         else;
127800080917       //?non sono previste consegne sulla distinta lo evidenzio
127900080917          $noc = *on;
128000080801         endif;
128100080922         endif;
128200080731
128300080731       endsr;
128400080730
128500080731       // ----------------------------------------------------------------------
128600080731       //?Calcolo il totale da confermare
128700080731       // ----------------------------------------------------------------------
128800080731       begsr sr_dstconf;
128900080813
129000080813         $finepdc = *off;
129100080813         $finepdo = *off;
129200080731
129300080731       //?inizializzo i vari campi di appoggio
129400080731         clear contapdo;
129500080731         clear contapdc;
129600080731         clear contapro;
129700080912         clear contapct;
129800080731         clear totconfr;
129900080731         clear totconfc;
130000080730
130100080731       //?per ogni distinta devo contare quante conferme ho ricevuto
130200080731       //?RITIRI --> FIPDO leggo quello che ho preparato
130300080731       //?                 per distinta e AUT
130400080731       //?       --> FIPRO leggo le conferme ricevute
130500080731       //?                 per distinta - AUT - ORM a parità di idrow
130600080730         exec sql
130700080730          declare pdo cursor for select fipdo00t.*
130800080730          from fipdo00t where pdofgs = :dstfgs and
130900080731          pdondc = :dstnfv and pdocodaut = :dstpdr
131000080730          order by pdofgs, pdondc, pdocodaut, pdopoe, pdonsr, pdonor, pdonrv;
131100080731         exec sql open pdo;
131200080731       //?leggo il file dei dati preparati
131300080813         dow not $finepdo;
131400080730          exec sql fetch next from pdo into: fipdods;
131500080731       //?fine file o errore sql esco
131600080730          if sqlcod = 100 or sqlcod < 0;
131700080813           $finepdo = *on;
131800080730           leave;
131900080730          endif;
132000080731       //?per ogni distinta/aut/orm conto gli orm preparati
132100080731          contapdo = contapdo + 1;
132200080731       //?cerco gli ok di conferma a parità di idrow
132300080731          exec sql
132400140210           select fipro10f.* into :fiprods from fipro10f
132500080731           where profgs = :pdofgs and prondc = :pdondc and
132600080912           procodaut = :pdocodaut and
132700080731           propoe = :pdopoe and
132800080731           pronsr = :pdonsr and pronor = :pdonor and
132900140210           pronrv = :pdonrv and protrd = 'OK ' and
133000140210           substr(prodati, 3, 2) = :pdoidrow;
133100080731       //?trovo il rcd corrispondente
133200080813          if sqlcod = 0;
133300080731       //?come confermato
133400080731           contapro = contapro + 1;
133500080731       //?controllo se ha richiesta info
133600140313          if pdoATB = ' ';
133700140210           clear fior82ds;
133800140210           c82fgs = profgs;
133900140210           c82ndc = prondc;
134000140210           c82codaut = procodaut;
134100140210           c82POE = proPOE;
134200140210           c82NSR = proNSR;
134300140210           c82NOR = proNOR;
134400140210           c82NRV = proNRV;
134500140210           kpjbu = fior82ds;
134600140210           fior82r(kpjba);
134700140210           fior82ds = kpjbu;
134800140210           if c82davisio <> '';
134900140210            $infor = *on;
135000140210           endif;
135100140210           endif;
135200080731          endif;
135300080730         enddo;
135400080731       //?chiuso cursore
135500080731         exec sql close pdo;
135600080731       //?calcolo quanti sono ancora da confermare
135700080731         totconfr = contapdo - contapro;
135800080730
135900080731       //?CONSEGNE --> FIPDC leggo quello che ho preparato
136000080731       //?                   per distinta e AUT
136100080731       //?         --> FIPCT leggo le conferme ricevute
136200080731       //?                 per distinta - AUT - bolla a parità di idrow
136300080731         exec sql
136400080731          declare pdc cursor for select fipdc00t.*
136500110216          from fipdc00t where pdcfgs = :dstfgs and pdcatb = ' ' and
136600080731          pdcndc = :dstnfv and pdccodaut = :dstpdr
136700080731          order by pdcfgs, pdcndc, pdccodaut, pdcaas, pdclnp, pdcnrs, pdcnsp;
136800080731         exec sql open pdc;
136900080731       //?leggo il file dei dati preparati
137000080813         dow not $finepdc;
137100080731          exec sql fetch next from pdc into: fipdcds;
137200080731       //?fine file o errore sql esco
137300080731          if sqlcod = 100 or sqlcod < 0;
137400080813           $finepdc = *on;
137500080731           leave;
137600080731          endif;
137700080731       //?per ogni distinta/aut/bolla conto le bolle preparate
137800080731          contapdc = contapdc + 1;
137900080731       //?cerco gli ok di conferma a parità di idrow
138000080731          exec sql
138100080731           select fipct00f.* into :fipctds from fipct00f
138200080731           where pctfgs = :pdcfgs and pctndc = :pdcndc and
138300080912           pctcodaut = :pdccodaut and
138400080731           pctaas = :pdcaas and
138500080731           pctlnp = :pdclnp and pctnrs = :pdcnrs and
138600080731           pctnsp = :pdcnsp and pcttrd = 'COK' and
138700080731           substr(pctdati, 3, 2) = :pdcidrow;
138800080731       //?trovo il rcd corrispondente
138900080807          if sqlcod = 0;
139000080731       //?come confermato
139100080731           contapct = contapct + 1;
139200080731       //?controllo se ha richiesta info
139300140313          if pdcATB = ' ';
139400080731           clear fidg45ds;
139500080902           c45fgs = pctfgs;
139600080902           c45ndc = pctndc;
139700080902           c45codaut = pctcodaut;
139800080731           c45aas = pctaas;
139900080731           c45lnp = pctlnp;
140000080731           c45nrs = pctnrs;
140100080731           c45nsp = pctnsp;
140200080731           kpjbu = fidg45ds;
140300080731           fidg45r(kpjba);
140400080731           fidg45ds = kpjbu;
140500080731           if c45davisio <> '';
140600080731            $infoc = *on;
140700080731           endif;
140800110616           endif;
140900080731          endif;
141000080731         enddo;
141100080731       //?chiuso cursore
141200080731         exec sql close pdc;
141300080731       //?calcolo quanti sono ancora da confermare
141400080731         totconfc = contapdc - contapct;
141500080730
141600080730       endsr;
141700080731
141800080731       // ----------------------------------------------------------------------
141900080731       //?Controllo se c'è OK ad uscire
142000080731       // ----------------------------------------------------------------------
142100080731       begsr sr_dstcus;
142200080731
142300080731         $cus = *off;
142400080731         clear fior90ds;
142500080731         or90tla = 'C';
142600080731         or90fas = 'CUS';
142700080731         or90fgs = wfgs;
142800080731         or90ndc = wndc;
142900080731         fior90r(kpjba:fior90ds);
143000080731       //?se trovo la fase CUS imposto flag
143100080731         if or90ndcok <> *blanks;
143200080731          $cus = *on;
143300080731         endif;
143400080731
143500080731       endsr;
143600080731
143700110616       // ----------------------------------------------------------------------
143800110616       //?Controllo se c'è OK ad uscire
143900110616       // ----------------------------------------------------------------------
144000110616       begsr sr_dstora;
144100110616
144200110616         clear plgdatora;
144300110616          exec sql
144400110616           select max(plgdatora) into :plgdatora from fiplg00f
144500110616           where plgfgs = :dstfgs and plgndc = :dstnfv and
144600110616           plgcodaut = :wpdr and plgerr= ' ';
144700110617           ora = %subst(plgdatora:9:4);
144800110617       if ora <> ' ';
144900110617           v2sora = %dec(ora:4:0);
145000110617       else;
145100110617           clear v2sora;
145200110617       endif;
145300110616       endsr;
145400110616
145500080731       // ----------------------------------------------------------------------
145600080731       //?Recupero ragione sociale AUT
145700080731       // ----------------------------------------------------------------------
145800080731       begsr sr_dstaut;
145900080731
146000080731          exec sql
146100080731           select apdrsc into :apdrsc from fiapd00f
146200080731           where apdtip = 'A' and apdatb = '' and
146300080801           apdpdr = :wpdr;
146400080731       //?non trovo il rcd corrispondente
146500080731          if sqlcod <> 0;
146600080731           clear apdrsc;
146700080731          endif;
146800080731
146900080731       endsr;
147000080922
147100110616       // ----------------------------------------------------------------------
147200110616       //?Calcolo il totale degli esiti
147300110616       // ----------------------------------------------------------------------
147400110616       begsr sr_dstesiti;
147500110616
147600110616       //?inizializzo i vari campi di appoggio
147700110616         clear conta   ;
147800110616         clear V1SESIc ;
147900110616         clear V1SPEc  ;
148000110616         clear V1SESIR ;
148100110616         clear V1SPER  ;
148200110616         clear V1SESNC ;
148300110616         clear V1SESNR ;
148400110616         clear V1SNESiC;
148500110616         clear V1SNESiR;
148600110617          *in60 = *off;
148700110617          *in61 = *off;
148800110616
148900110616       //?per ogni distinta devo contare quanti esiti ritiri e consegne
149000110616       //?sono stati ricevuti
149100110616          exec sql
149200110617          select count(*) into :conta from (SELECT DISTINCT
149300110616          plgiddoc FROM fiplg00f WHERE plgfgs=:dstfgs
149400110616          and plgndc = :dstnfv and
149500110617          plgcodaut = :wpdr and plgerr=' ' and plgtiporcd = 'RES'
149600110617          group by plgiddoc ) as pippo;
149700110616       //?calcolo il totale dei ritiri esitati
149800110617         v1sesir=conta;
149900110617            if conta <> 0 and CONTAR <>0 ;
150000120131            comper=conta*100/CONTAR;
150100120131            if comper > 999;
150200120131            v1sper=999;
150300120131            else;
150400120131            v1sper=comper;
150500120131            endif;
150600110617            *in61 = *on;
150700110616            endif;
150800110617         clear conta   ;
150900110616       //?calcolo il totale delle consegne esitate
151000110617          exec sql
151100110617          select count(*) into :conta from (SELECT DISTINCT
151200110617          plgiddoc FROM fiplg00f WHERE plgfgs=:dstfgs
151300110617          and plgndc = :dstnfv and
151400110617          plgcodaut = :wpdr and plgerr=' ' and plgtiporcd = 'CET'
151500110617          group by plgiddoc ) as pippo;
151600110617       //?calcolo il totale dei ritiri esitati
151700110616           v1sesic=conta;
151800110617            if conta <> 0 and CONTAC <>0 ;
151900120131            comper=conta*100/CONTAC;
152000120131            if comper > 999;
152100120131            v1spec=999;
152200120131            else;
152300120131            v1spec=comper;
152400120131            endif;
152500110617            *in60 = *on;
152600110616            endif;
152700110617           clear conta   ;
152800110616       //?conto quanti esiti RITIRI negativi ho ricevuto
152900110616          exec sql
153000110707
153100110707          with selPLG as
153200110707          (select fiplg00f.* from fiplg00f WHERE plgfgs=:dstfgs and
153300110707          plgndc=:dstnfv and plgcodaut=:Wpdr and
153400110707          plgerr=' '     and plgtiporcd = 'RES' and PLGTIPAPPL = 'R')
153500110707
153600110707          select  count(*) into :conta
153700110707          from (SELECT DISTINCT plgiddoc, max(plgdatora) maxdta
153800110707          from selplg group by plgiddoc ) as pippo join fiplg00f a on
153900110707          pippo.plgiddoc = a.plgiddoc and pippo.maxdta = a.plgdatora and
154000110707          'RES' = a.plgtiporcd where substr(plgdati, 83, 3) <> ' ';
154100110616
154200110616           v1sesnr=conta;
154300110616           clear conta;
154400110616       //?conto quanti esiti CONSEGNE negative ho ricevuto
154500110616          exec sql
154600110707
154700110707          with selPLG as
154800110707          (select fiplg00f.* from fiplg00f WHERE plgfgs=:dstfgs and
154900110707          plgndc=:dstnfv and plgcodaut=:Wpdr and
155000110707          plgerr=' '     and plgtiporcd = 'CET' and PLGTIPAPPL = 'C')
155100110707
155200110707          select count(*) into :conta
155300110707          from (SELECT DISTINCT plgiddoc, max(plgdatora) maxdta
155400110707          from selplg group by plgiddoc ) as pippo join fiplg00f a on
155500110707          pippo.plgiddoc = a.plgiddoc and pippo.maxdta = a.plgdatora and
155600110707          'CET' = a.plgtiporcd where substr(plgdati, 26, 3)
155700110707          not in (' ', 'P');
155800110707
155900110616           v1sesnc=conta;
156000110616           clear conta;
156100110616       //?conto quanti esiti mancano
156200110617          if CONTAR > v1sesir;
156300110617           v1snesir=CONTAR-v1sesir;
156400110616          endif;
156500110617          if CONTAC > v1sesic;
156600110617           v1snesic=CONTAC-v1sesic;
156700110616          endif;
156800110616       //?conto totali filiale
156900110617         V1TASSC = v1tassc + CONTAC;
157000110617         V1TASSR = v1tassr + CONTAR;
157100110616         V1TESIC = v1tesic + v1sesic ;
157200110616            if v1tesic <> 0 and v1tassc <>0 ;
157300120131            comper=v1tesic*100/v1tassc;
157400120131            if comper > 999;
157500120131            v1tpesc=999;
157600120131            else;
157700120131            v1tpesc=comper;
157800120131            endif;
157900110616            endif;
158000110616         V1TESIR = v1tesir + v1sesir;
158100110616         clear V1TPESR;
158200110616            if v1tesir <> 0 and v1tassr <>0 ;
158300120131            comper=v1tesir*100/v1tassr;
158400120131            if comper > 999;
158500120131            v1tpesr=999;
158600120131            else;
158700120131            v1tpesr=comper;
158800120131            endif;
158900110616            endif;
159000110616         V1TESNC = v1tesnc + v1sesnc;
159100110616         V1TESNR = v1tesnr + v1sesnr;
159200110616         V1TNESC = v1tnesc + v1snesic;
159300110616         V1TNESR = v1tnesr + v1snesir;
159400110616       endsr;
159500110616
159600080922       // ----------------------------------------------------------------------
159700080922       //?Leggo se ho dati preparati per altro AUT e carico la sk
159800080922       // ----------------------------------------------------------------------
159900080922       begsr sr_skaut;
160000080922
160100080922        clear autsk;
160200080922
160300080922       //?leggo FIPDO
160400080922         exec sql
160500080922          declare pdoaut cursor for select distinct(pdocodaut)
160600080922          from fipdo00t where pdofgs = :dstfgs and
160700080922          pdondc = :dstnfv and pdocodaut <> :dstpdr
160800080922          order by pdocodaut;
160900080922         exec sql open pdoaut;
161000080922       //?leggo il file dei dati preparati
161100080922         dou sqlcod <> 0;
161200080922          exec sql fetch next from pdoaut into: altroaut;
161300080922       //?fine file o errore sql esco
161400080922          if sqlcod = 100 or sqlcod < 0;
161500080922           leave;
161600080922          endif;
161700080922       //?carico sk
161800080922          if %lookup(altroaut:autsk) = *zeros;
161900080922           xx = 1;
162000080922           yy = %lookup(*zeros:autsk:xx);
162100080922           if yy > 0;
162200080922            autsk(yy) = altroaut;
162300080922           endif;
162400080922          endif;
162500080922         enddo;
162600080922       //?chiuso cursore
162700080922         exec sql close pdoaut;
162800080922
162900080922       //?leggo FIPDC
163000080922         exec sql
163100080922          declare pdcaut cursor for select distinct(pdccodaut)
163200080922          from fipdc00t where pdcfgs = :dstfgs and
163300080922          pdcndc = :dstnfv and pdccodaut <> :dstpdr
163400080922          order by pdccodaut;
163500080922         exec sql open pdcaut;
163600080922       //?leggo il file dei dati preparati
163700080922         dou sqlcod <> 0;
163800080922          exec sql fetch next from pdcaut into: altroaut;
163900080922       //?fine file o errore sql esco
164000080922          if sqlcod = 100 or sqlcod < 0;
164100080922           leave;
164200080922          endif;
164300080922       //?carico sk
164400080922          if %lookup(altroaut:autsk) = *zeros;
164500080922           xx = 1;
164600080922           yy = %lookup(*zeros:autsk:xx);
164700080922           if yy > 0;
164800080922            autsk(yy) = altroaut;
164900080922           endif;
165000080922          endif;
165100080922         enddo;
165200080922       //?chiuso cursore
165300080922         exec sql close pdcaut;
165400080922
165500080922       endsr;
165600080731
165700080731       // ----------------------------------------------------------------------
165800080731       //?Leggo se ho dati preparati per altro AUT
165900080731       // ----------------------------------------------------------------------
166000080731       begsr sr_altroaut;
166100080731
166200080731       //?controllo se ho dei dati preparati per AUT diverso da quello
166300080731       //?impostato in distinta (caso di cambio AUT sulla stessa distinta)
166400080731
166500080731       //?inizializzo i vari campi di appoggio
166600080731         clear contapdo;
166700080731         clear contapdc;
166800080731         clear contapro;
166900080801         clear contapct;
167000080731         clear totconfr;
167100080731         clear totconfc;
167200110701         clear totscac;
167300110701         clear totscar;
167400110701         clear contac;
167500110701         clear contar;
167600110701         $finepdc = *off;
167700110701         $finepdo = *off;
167800080731
167900080731       //?RITIRI --> FIPDO leggo quello che ho preparato
168000080731       //?                 per distinta e AUT <> da FIDST
168100080731       //?       --> FIPRO leggo le conferme ricevute
168200080731       //?                 per distinta - AUT - ORM a parità di idrow
168300080731         exec sql
168400080731          declare pdoa cursor for select fipdo00t.*
168500080731          from fipdo00t where pdofgs = :dstfgs and
168600080922          pdondc = :dstnfv and pdocodaut = :wpdr
168700080731          order by pdofgs, pdondc, pdocodaut, pdopoe, pdonsr, pdonor, pdonrv;
168800080731         exec sql open pdoa;
168900080731       //?leggo il file dei dati preparati
169000110701         dow not $finepdo;
169100080813          exec sql fetch next from pdoa into: fipdods;
169200080731       //?fine file o errore sql esco
169300080731          if sqlcod = 100 or sqlcod < 0;
169400110701           $finepdo = *on;
169500080731           leave;
169600080731          endif;
169700080731       //?per ogni distinta/aut/orm conto gli orm preparati
169800080731          contapdo = contapdo + 1;
169900080731       //?cerco gli ok di conferma a parità di idrow
170000080731          exec sql
170100140210           select fipro10f.* into :fiprods from fipro10f
170200080731           where profgs = :pdofgs and prondc = :pdondc and
170300080912           procodaut = :pdocodaut and
170400080731           propoe = :pdopoe and
170500080731           pronsr = :pdonsr and pronor = :pdonor and
170600140210           pronrv = :pdonrv and protrd = 'OK ' and
170700140210           substr(prodati, 3, 2) = :pdoidrow;
170800080731       //?trovo il rcd corrispondente
170900080731          if  sqlcod = 0;
171000080731       //?come confermato
171100080731           contapro = contapro + 1;
171200080731       //?controllo se ha richiesta info
171300140210          if proATB = ' ';
171400140210           clear fior82ds;
171500140210           c82fgs = profgs;
171600140210           c82ndc = prondc;
171700140210           c82codaut = procodaut;
171800140210           c82POE = proPOE;
171900140210           c82NSR = proNSR;
172000140210           c82NOR = proNOR;
172100140210           c82NRV = proNRV;
172200140210           kpjbu = fior82ds;
172300140210           fior82r(kpjba);
172400140210           fior82ds = kpjbu;
172500140210           if c82davisio <> '';
172600140210            $infor = *on;
172700140210           endif;
172800140210           endif;
172900140210          endif;
173000080731         enddo;
173100080731       //?chiuso cursore
173200080731         exec sql close pdoa;
173300080731       //?calcolo quanti sono ancora da confermare
173400080731         totconfr = contapdo - contapro;
173500080731
173600080731       //?CONSEGNE --> FIPDC leggo quello che ho preparato
173700080731       //?                   per distinta e AUT
173800080731       //?         --> FIPCT leggo le conferme ricevute
173900080731       //?                 per distinta - AUT - bolla a parità di idrow
174000080731         exec sql
174100080731          declare pdca cursor for select fipdc00t.*
174200110513          from fipdc00t where pdcfgs = :dstfgs and
174300080922          pdcndc = :dstnfv and pdccodaut = :wpdr
174400080731          order by pdcfgs, pdcndc, pdccodaut, pdcaas, pdclnp, pdcnrs, pdcnsp;
174500080731         exec sql open pdca;
174600080731       //?leggo il file dei dati preparati
174700110701         dow not $finepdc;
174800080813          exec sql fetch next from pdca into: fipdcds;
174900080731       //?fine file o errore sql esco
175000080731          if sqlcod = 100 or sqlcod < 0;
175100110701           $finepdc = *on;
175200080731           leave;
175300080731          endif;
175400080731       //?per ogni distinta/aut/bolla conto le bolle preparate
175500080731          contapdc = contapdc + 1;
175600080731       //?cerco gli ok di conferma a parità di idrow
175700080731          exec sql
175800080731           select fipct00f.* into :fipctds from fipct00f
175900080731           where pctfgs = :pdcfgs and pctndc = :pdcndc and
176000080912           pctcodaut = :pdccodaut and
176100080731           pctaas = :pdcaas and
176200080731           pctlnp = :pdclnp and pctnrs = :pdcnrs and
176300080731           pctnsp = :pdcnsp and pcttrd = 'COK' and
176400080731           substr(pctdati, 3, 2) = :pdcidrow;
176500080731       //?trovo il rcd corrispondente
176600080731          if  sqlcod = 0;
176700080731       //?come confermato
176800080731           contapct = contapct + 1;
176900080731       //?controllo se ha richiesta info
177000110513          if pctATB = ' ';
177100080731           clear fidg45ds;
177200080902           c45fgs = pctfgs;
177300080902           c45ndc = pctndc;
177400080902           c45codaut = pctcodaut;
177500080731           c45aas = pctaas;
177600080731           c45lnp = pctlnp;
177700080731           c45nrs = pctnrs;
177800080731           c45nsp = pctnsp;
177900080731           kpjbu = fidg45ds;
178000080731           fidg45r(kpjba);
178100080731           fidg45ds = kpjbu;
178200080731           if c45davisio <> '';
178300080731            $infoc = *on;
178400080731           endif;
178500080731          endif;
178600110513          endif;
178700080912       //?imposto il codice AUT
178800080731         enddo;
178900080731       //?chiuso cursore
179000080731         exec sql close pdca;
179100080731       //?calcolo quanti sono ancora da confermare
179200080731         totconfc = contapdc - contapct;
179300080731
179400080731       //?se ho almeno un ritiro o una consegna devo scrivere il subfile
179500080731         if totconfr <> *zeros or totconfc <> *zeros;
179600110616       //?calcolo quante esitate
179700110616          exsr sr_dstesiti;
179800080731          exsr sr_dstaut;
179900080731          *in03 = *on;
180000080813          v2sin03 = '1';
180100080731          clear totscar;
180200080731          clear totscac;
180300080731          $cus = *off;
180400080813       //?imposto che non ho ritiri e consegne da preparare
180500080813          $noc = *on;
180600080813          $nor = *on;
180700110616          exsr sr_cars02;
180800080731         endif;
180900080731
181000080731       endsr;
181100080801
181200080801       // ----------------------------------------------------------------------
181300080801       //?Carico il subfile distinte
181400080801       // ----------------------------------------------------------------------
181500080801       begsr sr_cars02;
181600080804
181700080804         $recnook = *off;
181800080801
181900080801         clear v2sopz;
182000080801       //?distinta - AUT
182100080801         v2sfgs = dstfgs;
182200110617         v1sfgs = dstfgs;
182300080801         v2saut = wpdr;
182400110617         v1saut = wpdr;
182500080801         v2sdaut = apdrsc;
182600110617         v1sdaut = apdrsc;
182700080801         v2sndc = dstnfv;
182800110617         v1sndc = dstnfv;
182900080801       //?distinta a PDA
183000080801         v2spda = dstpda;
183100110617         v1spda = dstpda;
183200080801       //?distinta in test
183300080801         v2stst = %xlate(up:lo:§dsttstpda);
183400110617         v1stst = %xlate(up:lo:§dsttstpda);
183500080923       //?assegnati in distinta
183600080923         v2sassc = contac;
183700080923         v2sassr = contar;
183800110617         v1sassc = contac;
183900110617         v1sassr = contar;
184000080801       //?dati da preparare
184100080801         v2sscac = totscac;
184200080801         v2sscar = totscar;
184300080801       //?dati da confermare
184400080801         v2sconfc = totconfc;
184500080801         v2sconfr = totconfr;
184600080801       //?ok ad uscire
184700080801         clear v2scus;
184800080801         if $cus;
184900080801          v2scus = 'SI';
185000080801         endif;
185100080801       //?INFO
185200080801         clear v2sinfoc;
185300080801         if $infoc;
185400080801         v2sinfoc = 'SI';
185500080801         endif;
185600080801         clear v2sinfor;
185700080801         if $infor;
185800080801         v2sinfor = 'SI';
185900080801         endif;
186000080801       //?evidenzio se non ho nessun ORM o bolla per la distinta
186100080804         *in01 = ($noc = *on);
186200080804         *in02 = ($nor = *on);
186300080813         if *in01;
186400080813          v2sin01 = '1';
186500110617          v1sin01 = '1';
186600080813         else;
186700080813          v2sin01 = '0';
186800110617          v1sin01 = '0';
186900080813         endif;
187000080813         if *in02;
187100080813          v2sin02 = '1';
187200110617          v1sin02 = '1';
187300080813         else;
187400080813          v2sin02 = '0';
187500110617          v1sin02 = '0';
187600080813         endif;
187700080804
187800110617         if *in60;
187900110617          v1sin60 = '1';
188000110617         else;
188100110617          v1sin60 = '0';
188200110617         endif;
188300110617
188400110617         if *in61;
188500110617          v1sin61 = '1';
188600110617         else;
188700110617          v1sin61 = '0';
188800110617         endif;
188900110617
189000080804       //?controllo le parzializzazioni
189100080916         exsr sr_parzializza;
189200080916         if $recnook;
189300080804          leavesr;
189400080804         endif;
189500110616       //?controllo ultima ora ricezione
189600110616          exsr sr_dstora;
189700080801
189800080801       //?se ordinamento per AUT ed è stato immesso qualcosa nel posizionamento
189900080801       //?carico da lì in poi
190000080801         if  *in11 or
190100080801            (*in12 and v2cdaut <= v2sdaut);
190200080801
190300080801          nrr = nrr + 1;
190400080801          write lvp0s02;
190500110617          write lvp0s01;
190600110620         endif;
190700080801
190800080801       endsr;
190900080804
191000080804       // ----------------------------------------------------------------------
191100080804       //?Controllo le parzializzazioni richieste in prima videata
191200080804       // ----------------------------------------------------------------------
191300080804       begsr sr_parzializza;
191400080916
191500080916       //?solo distinte con dati non confermati
191600080916         if v1ok <> *blanks and v2sconfc = 0 and v2sconfr = 0;
191700080916          $recnook = *on;
191800080916          leavesr;
191900080916         endif;
192000080916
192100080916       //?solo distinte con richiesta assistenza
192200101126         if v1info <> *blanks and v2sinfoc = '  ' and v2sinfor = '  ';
192300080916          $recnook = *on;
192400080916          leavesr;
192500080916         endif;
192600080916
192700080916       //?solo distinte con dati ancora da preparare
192800080916         if v1sca <> *blanks and v2sscac = 0 and v2sscar = 0;
192900080916          $recnook = *on;
193000080916          leavesr;
193100080916         endif;
193200080916
193300080916       //?ok ad uscire
193400080916         if v1cus = 'S' and v2scus <> 'SI';
193500080916          $recnook = *on;
193600080916          leavesr;
193700080916         endif;
193800080916         if v1cus = 'N' and v2scus = 'SI';
193900080916          $recnook = *on;
194000080916          leavesr;
194100080916         endif;
194200110616       //?% consegne
194300110628         if v1perc <> 0;
194400110707         if v1segc = '>' and v1spec < v1perc;
194500110616          $recnook = *on;
194600110616          leavesr;
194700110616         endif;
194800110707         if v1segc = '<' and v1spec > v1perc;
194900110628          $recnook = *on;
195000110628          leavesr;
195100110628         endif;
195200110628         endif;
195300110616       //?% ritiri
195400110628         if v1perr <> 0;
195500110707         if v1segr ='>' and v1sper < v1perr;
195600110616          $recnook = *on;
195700110616          leavesr;
195800110616         endif;
195900110707         if v1segr ='<' and v1sper > v1perr;
196000110628          $recnook = *on;
196100110628          leavesr;
196200110628         endif;
196300110628         endif;
196400080804
196500080804       endsr;
196600080801
196700080801       // ----------------------------------------------------------------------
196800080801       //?Controllo la testata del subfile
196900080801       // ----------------------------------------------------------------------
197000080801       begsr sr_ctrc02;
197100080801
197200080801       //?se ordinamento per AUT salvo il campo immesso
197300080801         if *in12 and v2cdaut <> *blanks;
197400080801          exsr sr_carica;
197500080801          exsr sr_ordina2;
197600110617          exsr sr_ordinaA2;
197700080917          clear v2cdaut;
197800080801         endif;
197900080801
198000080801       endsr;
198100080806
198200080806       // ----------------------------------------------------------------------
198300110616       //?Controllo opzione subfile
198400080806       // ----------------------------------------------------------------------
198500110616       begsr sr_ctrs02;
198600080806
198700080806       //?controllo l'opzione immessa
198800080806         for xx = 1 to rrnlast;
198900110616         if $video = 'S02';
199000080806          chain xx lvp0s02;
199100110616         endif;
199200110616         if $video = 'S01';
199300110616          chain xx lvp0s01;
199400110616         endif;
199500080806       //?visualizza dettaglio
199600080806          if v2sopz = '5';
199700080806           recsf2 = xx;
199800080806           exsr sr_visdettaglio;
199900110617           dstfgs = v2sfgs;
200000110617           dstnfv = v2sndc;
200100110617           dstpdr = v2saut;
200200110617           exsr sr_dstconf;
200300110617       //?devo aggiornare l'eventuale info
200400110617         clear v2sinfoc;
200500110617         if $infoc;
200600110617         v2sinfoc = 'SI';
200700110617         endif;
200800110617         clear v2sinfor;
200900110617         if $infor;
201000110617         v2sinfor = 'SI';
201100110617         endif;
201200080806           clear v2sopz;
201300080806           endif;
201400080923       //?visualizza fasi distinta
201500080923          if v2sopz = 'F';
201600080923           recsf2 = xx;
201700080923           exsr sr_visfasi;
201800080923           clear v2sopz;
201900080923           endif;
202000110616       //?visualizza Esiti
202100110616          if v2sopz = '6';
202200110616           recsf2 = xx;
202300110616           exsr sr_visesi;
202400110616           clear v2sopz;
202500110616           endif;
202600140520       //?Preparazione ORM
202700140724          if v2sopz = 'P' and v2sscar > 0;
202800140527            recsf2 = xx;
202900140527            exsr sr_prepar;
203000140527            $ricar = *on ;
203100140527            clear v2sopz;
203200140527           else ;
203300140527            if v2sopz = 'P';
203400140527             clear v2sopz;
203500140527            endif;
203600140527          endif;
203700080813       //?reimposto gli indicatori di visualizzazione
203800080813          *in01 = (v2sin01 = '1');
203900080813          *in02 = (v2sin02 = '1');
204000080813          *in03 = (v2sin03 = '1');
204100110617          *in01 = (v1sin01 = '1');
204200110617          *in02 = (v1sin02 = '1');
204300110617          *in03 = (v1sin03 = '1');
204400110617          *in60 = (v1sin60 = '1');
204500110617          *in61 = (v1sin61 = '1');
204600110616         if $video = 'S01';
204700110616          update lvp0s01;
204800110616         endif;
204900110616         if $video = 'S02';
205000110616          update lvp0s02;
205100110616         endif;
205200080806         endfor;
205300080806
205400140521         if $ricar ;
205500140521          $ricar = *off ;
205600140521           exsr sr_carica;
205700140521         endif;
205800080806       endsr;
205900080806
206000080806       // ----------------------------------------------------------------------
206100080806       //?Visualizzo dettaglio distinta
206200080806       // ----------------------------------------------------------------------
206300080806       begsr sr_visdettaglio;
206400080806
206500080806         clear fnlvp0ds;
206600080806         clear kpjbu;
206700110627         if  $video = 'S02';
206800080806         lvp0fgs = v2sfgs;
206900080806         lvp0ndc = v2sndc;
207000080806         lvp0aut = v2saut;
207100110627         else;
207200110627         lvp0fgs = v1sfgs;
207300110627         lvp0ndc = v1sndc;
207400110627         lvp0aut = v1saut;
207500110627         endif;
207600080923         fnlvp0r1(kpjba:fnlvp0ds);
207700080806
207800080806       endsr;
207900080923
208000080923       // ----------------------------------------------------------------------
208100080923       //?Visualizzo fasi distinta
208200080923       // ----------------------------------------------------------------------
208300080923       begsr sr_visfasi;
208400080923
208500080923         write frcdta;
208600080923
208700080923         clear fidg39ds;
208800080923         clear kpjbu;
208900080923         fdg39npg = 4;
209000110627         fdg39norec = 'N';
209100110627         if  $video = 'S02';
209200080923         fdg39nfv = v2sndc;
209300080923         fdg39fgs = v2sfgs;
209400110627         else;
209500110627         fdg39nfv = v1sndc;
209600110627         fdg39fgs = v1sfgs;
209700110627         endif;
209800080923         kpjbu = fidg39ds;
209900080923         fidg39r(kpjba);
210000080923
210100080923       endsr;
210200080801
210300110616       // ----------------------------------------------------------------------
210400110616       //?Visualizzo Esiti
210500110616       // ----------------------------------------------------------------------
210600110616       begsr sr_visesi;
210700110616
210800110627         clear fnlvp0ds;
210900110627         clear kpjbu;
211000110627         if  $video = 'S02';
211100110627         lvp0fgs = v2sfgs;
211200110627         lvp0ndc = v2sndc;
211300110627         lvp0aut = v2saut;
211400110627         else;
211500110627         lvp0fgs = v1sfgs;
211600110627         lvp0ndc = v1sndc;
211700110627         lvp0aut = v1saut;
211800110627         endif;
211900110629         fnlvp0r2(kpjba:fnlvp0ds);
212000110616
212100110616       endsr;
212200110616
212300140520       // ----------------------------------------------------------------------
212400140520       //?lancia preparazione orm
212500140520       // ----------------------------------------------------------------------
212600140520       begsr sr_prepar;
212700140520
212800140520         clear fior56ds;
212900140520         clear kpjbu;
213000140520         or56fgs = v2sfgs;
213100140521         or56ddc = wdata ;
213200140520         or56ndcd= v2sndc;
213300140520         or56ndca= v2sndc;
213400140521         or56tla = 'T'   ;
213500140521         or56cmd = 'I'   ;
213600140520         kpjbu   = fior56ds;
213700140521         fior56r(kpjba:fior56ds);
213800140520
213900140520       endsr;
214000140520
214100080801       // ----------------------------------------------------------------------
214200080801       //?Ordino il subfile per distinta
214300080801       // ----------------------------------------------------------------------
214400080801       begsr sr_ordina1;
214500080801
214600080801        recsf2 = 1;
214700080801
214800080801       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
214900080801       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
215000080801       // il campo è usato per selezionare i records dando un ordine a quelli
215100080801       // selezionati davanti ai non selezionati.
215200080801         clear QLGSCB;
215300080801         clear QLGSCB00;
215400080801
215500080801       // 1 campi chiave x Distinta
215600080801         QLGNBRK = 1;
215700080801
215800080801       // imposto la posizione della distinta sul subfile e la lunghezza
215900080801       // l'ordinamento è su un campo numerico e deve essere
216000080801       // ascending
216100110621         QLGSP = 1 + %size(v2sdaut);
216200080801         QLGSS = %SIZE(v2sndc);
216300080801         QLGDT = Numerico;
216400080801         QLGSO = Ascendente;
216500080801         QLGKL(1) = QLGSKL;
216600080801
216700080801       // Load other sort parameters.
216800080801         QLGLB = 80 + 16 * MaxKey;
216900080801         QLGRL = %SIZE(sflrcd) - 1;
217000080801         QLGRT = 8;
217100080801         QLGOKL = 80;
217200080801         QLGLKE = 16;
217300080801         QLGLSS = 290;
217400080801
217500080801       // Initialize Sort I/O API fields.
217600080801         QLGRL00 = QLGRL;
217700080801         QLGRC00 = 1;
217800080801         clear QUSEI;
217900080801         QUSBPRV = %SIZE(QUSEC);
218000080801
218100080801       // First step - Initialize the sort routine.
218200080801         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
218300080801
218400080801       // Next step - Write records to I/O routine.
218500080801         QLGRT00 = Put;
218600080801         for xx = 1 to rrnlast;
218700110616          chain xx lvp0s02;
218800080801
218900080801       // solo le righe con Selected = 'Y' sono riordinate,
219000080801       // quindi per fare un ordinamento di tutte le righe
219100080801       // metto 'Y' sempre.
219200080801          selected  = 'Y';
219300080801          clear QUSEI;
219400080801          QUSBPRV = %SIZE(QUSEC);
219500080801          QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
219600080801         endfor;
219700080801
219800080801       // Next step - Signal end of input, clear subfile for reload.
219900080801         QLGRT00 = EndPut;
220000080801         clear QUSEI;
220100080801         QUSBPRV = %SIZE(QUSEC);
220200080801         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
220300080801       // pulizia SFL
220400110620         clear nrr;
220500080912         clear v2cdaut;
220600110616         *in20 = *off;
220700110616         *in21 = *off;
220800110616         write lvp0c02;
220900110616         *in20 = *on;
221000110616         *in21 = *on;
221100080801
221200080801       // Final step - Write the records back to the subfile.
221300080801         QLGRT00 = Get;
221400080801         for xx = 1 to rrnlast;
221500080801          clear QUSEI;
221600080801          QUSBPRV = %SIZE(QUSEC);
221700080801          QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
221800080801          nrr = xx;
221900080813       //?reimposto gli indicatori di visualizzazione
222000080813          *in01 = (v2sin01 = '1');
222100080813          *in02 = (v2sin02 = '1');
222200080813          *in03 = (v2sin03 = '1');
222300110616          write lvp0s02;
222400080801         endfor;
222500080801
222600080801       endsr;
222700080801
222800080801       // ----------------------------------------------------------------------
222900110621       //?Ordino il subfile per decodifica aut
223000080801       // ----------------------------------------------------------------------
223100080801       begsr sr_ordina2;
223200080801
223300080801        recsf2 = 1;
223400080801
223500080801       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
223600080801       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
223700080801       // il campo è usato per selezionare i records dando un ordine a quelli
223800080801       // selezionati davanti ai non selezionati.
223900080801         clear QLGSCB;
224000080801         clear QLGSCB00;
224100080801
224200110621       // 2 campi chiave x decodifica/Distinta
224300080801         QLGNBRK = 2;
224400080801
224500080801       // imposto la posizione dell'AUT sul subfile e la lunghezza
224600080801       // l'ordinamento è su un campo numerico e deve essere
224700080801       // ascending
224800110621         QLGSP = 1 ;
224900080801         QLGSS = %SIZE(v2sdaut);
225000080801         QLGDT = Carattere;
225100080801         QLGSO = Ascendente;
225200080801         QLGKL(1) = QLGSKL;
225300080801
225400080801       // imposto la posizione della distinta sul subfile e la lunghezza
225500080801       // l'ordinamento è su un campo numerico e deve essere
225600080801       // ascending
225700110621         QLGSP = 1 + %size(v2sdaut);
225800080801         QLGSS = %SIZE(v2sndc);
225900080801         QLGDT = Numerico;
226000080801         QLGSO = Ascendente;
226100080801         QLGKL(2) = QLGSKL;
226200080801
226300080801       // Load other sort parameters.
226400080801         QLGLB = 80 + 16 * MaxKey;
226500080801         QLGRL = %SIZE(sflrcd) - 1;
226600080801         QLGRT = 8;
226700080801         QLGOKL = 80;
226800080801         QLGLKE = 16;
226900080801         QLGLSS = 290;
227000080801
227100080801       // Initialize Sort I/O API fields.
227200080801         QLGRL00 = QLGRL;
227300080801         QLGRC00 = 1;
227400080801         clear QUSEI;
227500080801         QUSBPRV = %SIZE(QUSEC);
227600080801
227700080801       // First step - Initialize the sort routine.
227800080801         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
227900080801
228000080801       // Next step - Write records to I/O routine.
228100080801         QLGRT00 = Put;
228200080801         for xx = 1 to rrnlast;
228300110616          chain xx lvp0s02;
228400080801
228500080801       // solo le righe con Selected = 'Y' sono riordinate,
228600080801       // quindi per fare un ordinamento di tutte le righe
228700080801       // metto 'Y' sempre.
228800080801          selected  = 'Y';
228900080801          clear QUSEI;
229000080801          QUSBPRV = %SIZE(QUSEC);
229100080801          QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
229200080801         endfor;
229300080801
229400080801       // Next step - Signal end of input, clear subfile for reload.
229500080801         QLGRT00 = EndPut;
229600080801         clear QUSEI;
229700080801         QUSBPRV = %SIZE(QUSEC);
229800080801         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
229900080801       // pulizia SFL
230000080801         clear nrr;
230100110616         *in20 = *off;
230200110616         *in21 = *off;
230300110616         write lvp0c02;
230400110616         *in20 = *on;
230500110616         *in21 = *on;
230600080801
230700080801       // Final step - Write the records back to the subfile.
230800080801         QLGRT00 = Get;
230900080801         for xx = 1 to rrnlast;
231000080801          clear QUSEI;
231100080801          QUSBPRV = %SIZE(QUSEC);
231200080801          QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
231300110620          nrr = xx;
231400080813       //?reimposto gli indicatori di visualizzazione
231500080813          *in01 = (v2sin01 = '1');
231600080813          *in02 = (v2sin02 = '1');
231700080813          *in03 = (v2sin03 = '1');
231800110616          write lvp0s02;
231900080801         endfor;
232000080801
232100080801       endsr;
232200110617       // ----------------------------------------------------------------------
232300110617       //?Ordino il subfile attività  X distinte
232400110617       // ----------------------------------------------------------------------
232500110617       begsr sr_ordinaA1;
232600110617
232700110617        recsf2 = 1;
232800110617
232900110617       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
233000110617       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
233100110617       // il campo è usato per selezionare i records dando un ordine a quelli
233200110617       // selezionati davanti ai non selezionati.
233300110617         clear QLGSCB;
233400110617         clear QLGSCB00;
233500110617
233600110617       // 1 campi chiave x Distinta
233700110617         QLGNBRK = 1;
233800110617
233900110617       // imposto la posizione della distinta sul subfile e la lunghezza
234000110617       // l'ordinamento è su un campo numerico e deve essere
234100110617       // ascending
234200110621         QLGSP = 1 + %size(v1sdaut);
234300110617         QLGSS = %SIZE(v1sndc);
234400110617         QLGDT = Numerico;
234500110617         QLGSO = Ascendente;
234600110617         QLGKL(1) = QLGSKL;
234700110617
234800110617       // Load other sort parameters.
234900110617         QLGLB = 80 + 16 * MaxKey;
235000110621         QLGRL = %SIZE(sflrcd) - 1;
235100110617         QLGRT = 8;
235200110617         QLGOKL = 80;
235300110617         QLGLKE = 16;
235400110617         QLGLSS = 290;
235500110617
235600110617       // Initialize Sort I/O API fields.
235700110617         QLGRL00 = QLGRL;
235800110617         QLGRC00 = 1;
235900110617         clear QUSEI;
236000110617         QUSBPRV = %SIZE(QUSEC);
236100110617
236200110617       // First step - Initialize the sort routine.
236300110617         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
236400110617
236500110617       // Next step - Write records to I/O routine.
236600110617         QLGRT00 = Put;
236700110617         for xx = 1 to rrnlast;
236800110617          chain xx lvp0s01;
236900110617
237000110617       // solo le righe con Selected = 'Y' sono riordinate,
237100110617       // quindi per fare un ordinamento di tutte le righe
237200110617       // metto 'Y' sempre.
237300110621          selected = 'Y';
237400110617          clear QUSEI;
237500110617          QUSBPRV = %SIZE(QUSEC);
237600110621          QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
237700110617         endfor;
237800110617
237900110617       // Next step - Signal end of input, clear subfile for reload.
238000110617         QLGRT00 = EndPut;
238100110617         clear QUSEI;
238200110617         QUSBPRV = %SIZE(QUSEC);
238300110621         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
238400110617       // pulizia SFL
238500110617         clear nrr;
238600110617         clear v2cdaut;
238700110617         *in85 = *off;
238800110617         *in86 = *off;
238900110617         write lvp0c01;
239000110617         *in85 = *on;
239100110617         *in86 = *on;
239200110617
239300110617       // Final step - Write the records back to the subfile.
239400110617         QLGRT00 = Get;
239500110617         for xx = 1 to rrnlast;
239600110617          clear QUSEI;
239700110617          QUSBPRV = %SIZE(QUSEC);
239800110621          QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
239900110617          nrr = xx;
240000110617       //?reimposto gli indicatori di visualizzazione
240100110617          *in01 = (v1sin01 = '1');
240200110617          *in02 = (v1sin02 = '1');
240300110617          *in03 = (v1sin03 = '1');
240400110617          *in60 = (v1sin60 = '1');
240500110617          *in61 = (v1sin61 = '1');
240600110617          write lvp0s01;
240700110617         endfor;
240800110617
240900110617       endsr;
241000110617
241100110617       // ----------------------------------------------------------------------
241200110621       //?Ordino il subfile attività per decodifica aut
241300110617       // ----------------------------------------------------------------------
241400110617       begsr sr_ordinaA2;
241500110617
241600110617        recsf2 = 1;
241700110617
241800110617       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
241900110617       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
242000110617       // il campo è usato per selezionare i records dando un ordine a quelli
242100110617       // selezionati davanti ai non selezionati.
242200110617         clear QLGSCB;
242300110617         clear QLGSCB00;
242400110617
242500110621       // 2 campi chiave x decodifica/Distinta
242600110617         QLGNBRK = 2;
242700110617
242800110617       // imposto la posizione dell'AUT sul subfile e la lunghezza
242900110617       // l'ordinamento è su un campo numerico e deve essere
243000110617       // ascending
243100110621         QLGSP = 1;
243200110617         QLGSS = %SIZE(v1sdaut);
243300110617         QLGDT = Carattere;
243400110617         QLGSO = Ascendente;
243500110617         QLGKL(1) = QLGSKL;
243600110617
243700110617       // imposto la posizione della distinta sul subfile e la lunghezza
243800110617       // l'ordinamento è su un campo numerico e deve essere
243900110617       // ascending
244000110621         QLGSP = 1 + %size(v1sdaut);
244100110617         QLGSS = %SIZE(v1sndc);
244200110617         QLGDT = Numerico;
244300110617         QLGSO = Ascendente;
244400110617         QLGKL(2) = QLGSKL;
244500110617
244600110617       // Load other sort parameters.
244700110617         QLGLB = 80 + 16 * MaxKey;
244800110621         QLGRL = %SIZE(sflrcd) - 1;
244900110617         QLGRT = 8;
245000110617         QLGOKL = 80;
245100110617         QLGLKE = 16;
245200110617         QLGLSS = 290;
245300110617
245400110617       // Initialize Sort I/O API fields.
245500110617         QLGRL00 = QLGRL;
245600110617         QLGRC00 = 1;
245700110617         clear QUSEI;
245800110617         QUSBPRV = %SIZE(QUSEC);
245900110617
246000110617       // First step - Initialize the sort routine.
246100110617         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
246200110617
246300110617       // Next step - Write records to I/O routine.
246400110617         QLGRT00 = Put;
246500110617         for xx = 1 to rrnlast;
246600110617          chain xx lvp0s01;
246700110617
246800110617       // solo le righe con Selected = 'Y' sono riordinate,
246900110617       // quindi per fare un ordinamento di tutte le righe
247000110617       // metto 'Y' sempre.
247100110621          selected = 'Y';
247200110617          clear QUSEI;
247300110617          QUSBPRV = %SIZE(QUSEC);
247400110621          QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
247500110617         endfor;
247600110617
247700110617       // Next step - Signal end of input, clear subfile for reload.
247800110617         QLGRT00 = EndPut;
247900110617         clear QUSEI;
248000110617         QUSBPRV = %SIZE(QUSEC);
248100110621         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
248200110617       // pulizia SFL
248300110617         clear nrr;
248400110617       // clear v2cdaut;
248500110617         *in85 = *off;
248600110617         *in86 = *off;
248700110617         write lvp0c01;
248800110617         *in85 = *on;
248900110617         *in86 = *on;
249000110617
249100110617       // Final step - Write the records back to the subfile.
249200110617         QLGRT00 = Get;
249300110617         for xx = 1 to rrnlast;
249400110617          clear QUSEI;
249500110617          QUSBPRV = %SIZE(QUSEC);
249600110621          QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
249700110617          nrr = xx;
249800110617       //?reimposto gli indicatori di visualizzazione
249900110617          *in01 = (v1sin01 = '1');
250000110617          *in02 = (v1sin02 = '1');
250100110617          *in03 = (v1sin03 = '1');
250200110617          *in60 = (v1sin60 = '1');
250300110617          *in61 = (v1sin61 = '1');
250400110617          write lvp0s01;
250500110617         endfor;
250600110617
250700110617       endsr;
250800080725
250900080731       // ----------------------------------------------------------------------
251000080731       //?Fine programma
251100080731       // ----------------------------------------------------------------------
251200080725       begsr sr_fine;
251300080725
251400080725         *inlr = *on;
251500080725         return;
251600080725
251700080725       endsr;
251800070214
251900070214** MSG  Lungh. 78                                                            *
252000080725PDA non attivo per la filiale in gestione                                     01
252100080725Filiale in gestione errata                                                    02
252200080725Data errata                                                                   03
252300080725Non trovate distinte nella data indicata                                      04
252400080728Numero distinta errato: contiene caratteri NON numerici                       05
252500080728Numero distinta inesistente o annullato                                       06
252600080728Distinta già chiusa                                                           07
252700080728Distinta senza dati a PDA                                                     08
252800080728Numero distinta obbligatorio                                                  09
252900080728Limiti incompatibili: numero distinta al minore del numero distinta dal       10
253000080730Autotrasportatore errato: contine caratteri NON numerici                      11
253100080730Autotrasportatore inesistente o annullato                                     12
253200080730Autotrasportatore non in gestione all'utente                                  13
253300080730La selezione è possibile per Distinta o per AUT non entrambe!!!!              14
253400080916E' possibile selezionare solo una parzializzazione                            15
