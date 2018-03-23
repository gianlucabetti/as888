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
009300160510     d msg             s             78    dim(20) ctdata perrcd(1)
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
015600170227     d SizeList        s             10I 0
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
034400160504
034500160504       //?Turno
034600160504          *in09 = (§ogturno ='S');
034700110617
034800160511          If §ogturno ='S';
034900160511            eval v1tur1 ='1';
035000160511            eval v1tur2 ='2';
035100160511          endif;
035200160511
035300160511
035400110617       //?imposto la data del giorno
035500110617          wdata = %dec(%date());
035600110617          dataiso = %date(wdata:*iso);
035700110617          dataeur = dataiso;
035800110617          v1cddc = %dec(dataeur);
035900110617          v1tipm = 'D';
036000110628          v1segc = '>';
036100110628          v1segr = '>';
036200110617
036300110617       //?imposto il range distinta
036400110617          v1ndcal = *all'9';
036500110617
036600110617       //?gestione video D01
036700110617         $video = 'D01';
036800110617         $fine =*off;
036900110617
037000110617       endsr;
037100110617
037200080731       // ----------------------------------------------------------------------
037300080731       //?Gestione prima videata (parametri)
037400080731       // ----------------------------------------------------------------------
037500080725       begsr gesd01;
037600080725
037700080731       //?emetto la videata
037800080725          write lvp0t01;
037900080725          exfmt lvp0d01;
038000070209
038100080731       //?spengo indicatori di comodo
038200080725          *in28 = *off;
038300080725          *in90 = *off;
038400080731       //?pulisco campo messaggi
038500080725          clear v1cmsg;
038600080725
038700080731       //?esecuzione comandi
038800080725         select;
038900080729
039000080731       //?f3=fine
039100080725          when *inkc;
039200080725           exsr sr_fine;
039300080731       //?f18=cambio filiale gestione
039400080725          when *inks;
039500080725           *in05 = *on;
039600080731           leavesr;
039700080725         other;
039800080729
039900080731       //?controllo filiale gestione se F18
040000080725          if *in05;
040100080725           exsr sr_contrfgs;
040200080725           if not *in28;
040300080725            *in05 = *off;
040400080731            leavesr;
040500080725           endif;
040600080725          endif;
040700080731       //?controllo i dati della videata
040800080725          exsr sr_contrd01;
040900080725          if *in28 or *in90;
041000080725           leavesr;
041100080725          endif;
041200080725         endsl;
041300080801
041400110616       //?preparo i dati per il subfile delle distinte o delle attività
041500080801       //?accendo indicatore per impostare subito l'ordinamento
041600080801       //?di dft del subfile (per distinta)
041700110627         *in11 = *off;
041800110627         *in12 = *on;
041900110616         exsr sr_carica;
042000110620
042100110701       //?dati
042200110616          if v1tipm = 'D';
042300080725         $video = 'S02';
042400110616          endif;
042500110701       //?operatività
042600110627          if v1tipm = 'O';
042700110616         $video = 'S01';
042800110616          endif;
042900080725
043000080725       endsr;
043100080731
043200110616       // ----------------------------------------------------------------------
043300110616       //?Gestione subfile distinte
043400110616       // ----------------------------------------------------------------------
043500110616       begsr gess02;
043600110616
043700110616       //?ho caricato dati emetto il subfile
043800110616         if nrr > 0;
043900110616          exsr sr_sfl02;
044000110616         endif;
044100110616
044200110616       //?non ho caricato dati emetto videata di segnalazione
044300110621         if nrr = 0 ;
044400110616          exsr sr_d02;
044500110616         endif;
044600110616
044700110616       endsr;
044800110616
044900080731       // ----------------------------------------------------------------------
045000080731       //?Controllo filiale gestione
045100080731       // ----------------------------------------------------------------------
045200080729       begsr sr_contrfgs;
045300080729
045400080729         *in40 = *off;
045500080729         clear v1dfgs;
045600080729         clear og148;
045700080729         clear og150;
045800080729
045900080731       //?deve essere una filiale gestita dall'utente
046000080729         if %lookup(v1cfgs:lin) = *zeros;
046100080729          *in28 = *on;
046200080729          *in40 = *on;
046300080729          v1cmsg = msg(02);
046400080729          leavesr;
046500080729         endif;
046600080729
046700080731       //?decodifico la filiale gestione
046800080731       //?controllo se PDA attivo sulla filiale in gestione
046900080731       //?                 e se gestisce parcel/messaggerie
047000080729          clear og148;
047100080729          clear og150;
047200080729          chain v1cfgs azorg01l;
047300080729          if  %found(azorg01l) and orgfva = *blanks;
047400080729           v1dfgs = orgdes;
047500080729           og148 = orgde8;
047600080729           og150 = orgdf0;
047700080729          endif;
047800080729          if §ogpdaorm = *blanks and §ogpdacon = *blanks;
047900080729           *in28 = *on;
048000080729           *in40 = *on;
048100080729           v1cmsg = msg(01);
048200080729          endif;
048300080729
048400080731       //?parcel messaggerie
048500080729          *in10 = (§ogpkm ='S');
048600160504
048700160504       //?Turno
048800160504          *in09 = (§ogturno ='S');
048900160512
049000160512          If §ogturno ='S' and h1cfgs<>v1cfgs;
049100160512            eval h1cfgs =v1cfgs;
049200160512            eval v1tur1 ='1';
049300160512            eval v1tur2 ='2';
049400160512          endif;
049500160512
049600160512          If §ogturno<>'S';
049700160512            clear h1cfgs;
049800160512            clear v1tur1;
049900160512            clear v1tur2;
050000160512          endif;
050100160512
050200080729
050300080729       endsr;
050400080729
050500080731       // ----------------------------------------------------------------------
050600080731       //?Controllo dati prima videata (parametri)
050700080731       // ----------------------------------------------------------------------
050800080729       begsr sr_contrd01;
050900080729
051000080731       //?spengo indicatori di errore
051100080729         *in41 = *off;
051200080729         *in42 = *off;
051300080729         *in43 = *off;
051400080729         *in44 = *off;
051500080916         *in45 = *off;
051600080916         *in46 = *off;
051700080916         *in47 = *off;
051800160509         *in48 = *off;
051900160509         *in49 = *off;
052000080915
052100080915       //?pulisco campi di comodo
052200080915         clear wdata;
052300080915         clear wndcdal;
052400080915         clear waut;
052500080729
052600080731       //?controllo data distinta
052700080729         exsr sr_contrddc;
052800080729         if *in28;
052900080729          leavesr;
053000080729         endif;
053100160509
053200160509       //?controllo Selezioni  Tipo Distinta/Turni
053300160509         exsr sr_contrsel;
053400160509         if *in28 or *in90;
053500160509          leavesr;
053600160509         endif;
053700080729
053800080731       //?controllo distinta
053900080729         exsr sr_contrndc;
054000080731         if *in28 or *in90;
054100080729          leavesr;
054200080729         endif;
054300080729
054400080731       //?controllo autotrasportatore
054500080729         exsr sr_contraut;
054600080729         if *in28 or *in90;
054700080729          leavesr;
054800080729         endif;
054900080916
055000080916       //?controllo parzializzazioni
055100080916         exsr sr_contrpar;
055200080916         if *in28;
055300080916          leavesr;
055400080916         endif;
055500080729
055600080729       endsr;
055700080729
055800080731       // ----------------------------------------------------------------------
055900110616       //?Carico i dati per subfile distinte e attività
056000080731       // ----------------------------------------------------------------------
056100080729       begsr sr_carica;
056200080730
056300080731       //?pulisco il subfile distinte
056400080730         exsr sr_puls02;
056500080729
056600110616       //?pulisco il subfile attività
056700110616         exsr sr_puls01;
056800110616
056900080731       //?carico le distinte da elaborare
057000080729         exsr sr_cardst;
057100080729
057200080731       //?elaboro i dati delle distinte
057300080729         exsr sr_elabdst;
057400110620
057500110621       if nrr <> 0;
057600110620           if *in11;
057700110620            exsr sr_ordina1;
057800110620            exsr sr_ordinaA1;
057900110620           endif;
058000110620           if *in12;
058100110620            exsr sr_ordina2;
058200110620            exsr sr_ordinaA2;
058300110620           endif;
058400110621       endif;
058500080729
058600080729       endsr;
058700080801
058800080731       // ----------------------------------------------------------------------
058900080731       //?Emetto il subfile distinte
059000080731       // ----------------------------------------------------------------------
059100080731       begsr sr_sfl02;
059200080801
059300080731       //?emetto la videata
059400110617         if  $video = 'S02';
059500080731         write lvp0t01;
059600080731         write lvp0z02;
059700080731         exfmt lvp0c02;
059800110617         endif;
059900110617         if  $video = 'S01';
060000110617         write lvp0t01;
060100110617         write lvp0z01;
060200110617         exfmt lvp0c01;
060300110617         endif;
060400080731       //?spengo indicatori di comodo
060500080731         *in28 = *off;
060600080731       //?pulisco campo messaggi
060700080731         clear v2cmsg;
060800080731       //?esecuzione comandi
060900080731         select;
061000080731
061100080731       //?f3=fine
061200080731          when *inkc;
061300080731           exsr sr_fine;
061400080731       //?f5=aggiorna
061500080731          when *inke;
061600080731           exsr sr_carica;
061700080731           leavesr;
061800080801       //?f11=ordina x AUT
061900080801          when *inkk and *in11;
062000080801           *in11 = *off;
062100080801           *in12 = *on;
062200110617           clear v2cdaut;
062300110620           exsr sr_ordina2;
062400110617           exsr sr_ordinaA2;
062500080801           leavesr;
062600080801       //?f11=ordina x NUM
062700080801          when *inkk and *in12;
062800080801           *in11 = *on;
062900080801           *in12 = *off;
063000110617           clear v2cdaut;
063100080801           exsr sr_ordina1;
063200110617           exsr sr_ordinaA1;
063300080801           leavesr;
063400080731       //?f12=ritorno
063500080731          when *inkl;
063600110620           clear v2cdaut;
063700080731           $video = 'D01';
063800080731           leavesr;
063900110616       //?f8=monitor attività
064000110617          when *inkH;
064100110617         if  $video = 'S02';
064200110616           $video = 'S01';
064300110617         else;
064400110617           $video = 'S02';
064500110617         endif;
064600110616           leavesr;
064700080731         other;
064800080801       //?controllo testata subfile (posizionamento)
064900080801           exsr sr_ctrc02;
065000080806       //?controllo opzione subfile
065100080806           exsr sr_ctrs02;
065200080731         endsl;
065300080731
065400080731       endsr;
065500080731
065600110620     C/EJECT
065700080731       // ----------------------------------------------------------------------
065800080731       //?Emetto video di segnalazione distinte non caricate
065900080731       // ----------------------------------------------------------------------
066000080731       begsr sr_d02;
066100080731
066200080731       //?emetto la videata
066300080731         write lvp0t01;
066400080731         exfmt lvp0d02;
066500080731
066600080731       //?spengo indicatori di comodo
066700080731         *in28 = *off;
066800080731       //?pulisco campo messaggi
066900080731         clear v2cmsg;
067000080731       //?esecuzione comandi
067100080731         select;
067200080731
067300080731       //?f3=fine
067400080731          when *inkc;
067500080731           exsr sr_fine;
067600080731       //?f5=aggiorna
067700080731          when *inke;
067800080731           exsr sr_carica;
067900080731           leavesr;
068000080731       //?f12=ritorno
068100080731          when *inkl;
068200080731           $video = 'D01';
068300080731           leavesr;
068400080731         other;
068500080731         endsl;
068600080731
068700080731       endsr;
068800080725
068900080731       // ----------------------------------------------------------------------
069000080731       //?Controllo data distinta
069100080731       // ----------------------------------------------------------------------
069200080725       begsr sr_contrddc;
069300080725
069400080731       //?la data è obbligatoria
069500080725         if v1cddc = *zeros;
069600080725          *in28 = *on;
069700080725          *in41 = *on;
069800080725          v1cmsg = msg(03);
069900080725          leavesr;
070000080725         endif;
070100080725
070200080731       //?deve essere una data valida
070300080725         clear wlbdat;
070400080725         g02dat = v1cddc;
070500080725         xsrda8(wlbdat);
070600080725         if g02err = '1';
070700080725          *in28 = *on;
070800080725          *in41 = *on;
070900080725          v1cmsg = msg(03);
071000080725          leavesr;
071100080725         endif;
071200080725
071300080725        v1cddc = g02dat;
071400080725        wdata = g02inv;
071500080729
071600080731       //?controllo se ci sono distinte nel giorno selezionato
071700080729         dstnpg = 4;
071800080729         chain (v1cfgs:dstnpg:wdata) fidst08l;
071900080729         if not %found(fidst08l);
072000080729          *in28 = *on;
072100080922          *in41 = *on;
072200080729          v1cmsg = msg(04);
072300080729          leavesr;
072400080729         endif;
072500080725
072600080725       endsr;
072700080725
072800080731       // ----------------------------------------------------------------------
072900160509       //?Controllo Selezioni Tipo Distinta / Turni
073000080731       // ----------------------------------------------------------------------
073100160509       begsr sr_contrsel;
073200080801
073300160509       //?se c'è il Tipo distinta ed il turno
073400160509         if *in10 and v1tdis <> 'M';
073500160512          if §ogturno='S' and
073600160512                v1tur1 = ' ' and v1tur2 = ' ';
073700160509             *in28 = *on;
073800160509             *in48 = *on;
073900160509             *in49 = *on;
074000160511             v1cmsg = msg(17);
074100160509             leavesr;
074200160509           endif;
074300160509         endif;
074400160509
074500160509       endsr;
074600160509
074700160509       // ----------------------------------------------------------------------
074800160509       //?Controllo distinta
074900160509       // ----------------------------------------------------------------------
075000160509       begsr sr_contrndc;
075100160509
075200080912       //?se richiesto un range di distinte non posso richiedere
075300080912       //?anche l'autotrasportatore
075400080915         if v1caut > *zeros and (v1ndcda > *zeros or
075500080915                                (v1ndcal > *zeros and v1ndcal <> 999999));
075600080912          *in28 = *on;
075700080912          *in44 = *on;
075800080912          v1cmsg = msg(14);
075900080912          leavesr;
076000080912         endif;
076100080912
076200080912       //?se impostato AUT senza distinta vado a fine dei controlli
076300080912         if v1caut <> *blanks;
076400080801          leavesr;
076500080801         endif;
076600080725
076700080731       //?numero distinta dal --> se impostato
076800080725         if v1ndcda <> *blanks;
076900080731       //?pulisco se a zero
077000080728          if v1ndcda = *zeros;
077100080728           clear v1ndcda;
077200080915           leavesr;
077300080728          endif;
077400160509
077500080731       //?ricerca della distinta
077600080728          if %scan('?':v1ndcda) > *zeros;
077700080731           *in42 = *on;
077800080731           *in90 = *on;
077900080728           clear fidg31ds;
078000080728           fdgfgs = v1cfgs;
078100080728           fdggda = wdata;
078200080728           fdggal = wdata;
078300080728           fdgric = 'S';
078400080728           fdgtfv = '2';
078500080728           fdgtpv = 'P';
078600080728           fdgopz = '1';
078700160511           if §ogpkm ='S';
078800160511             fdgtdis = v1tdis;
078900160511           endif;
079000160512           if §ogturno ='S';
079100160512             fdgtur1= v1tur1;
079200160512             fdgtur2= v1tur2;
079300160509           endif;
079400080731           clear $fv6;
079500080731       //?impostato a tutti 99999
079600080731           xx = 1;
079700080731           for xx by 1 to 600;
079800080731            $fv6(xx) = 999999;
079900080728           endfor;
080000080728           kpjbu = fidg31ds;
080100080728           fidg31r(kpjba:$fv6);
080200080728           fidg31ds = kpjbu;
080300080731       //?controllo se scelta una distinta o no
080400080728           if fdgnfv = *zeros;
080500080728            clear fidg31ds;
080600080915            clear v1ndcda;
080700080915            leavesr;
080800080728           else;
080900080731            v1ndcda = %editc(%dec(fdgnfv):'X');
081000080728           endif;
081100080728          endif;
081200080731       //?controllo validità
081300080915          if %check(digitn:v1ndcda) = *zeros;
081400080915           wndcdal = %dec(v1ndcda:6:0);
081500080915           dstnpg = 4;
081600080915           chain (v1cfgs:dstnpg:wndcdal) fidst09l;
081700080915          endif;
081800080728          select;
081900080731       //?campo non numerico
082000080728          when %check(digitn:v1ndcda) > *zeros;
082100080728           *in28 = *on;
082200080728           *in42 = *on;
082300080728           v1cmsg = msg(05);
082400080728           leavesr;
082500080731       //?distinta non trovata o annullata
082600080728          when not %found(fidst09l) or dstatb <> *blanks;
082700080728           *in28 = *on;
082800080728           *in42 = *on;
082900080728           v1cmsg = msg(06);
083000080728           leavesr;
083100080731       //?distinta non della data richiesta
083200080728          when dstdfv <> wdata;
083300080728           *in28 = *on;
083400080728           *in42 = *on;
083500080728           v1cmsg = msg(04);
083600080728           leavesr;
083700080731       //?distinta non a PDA
083800080728          when dstpda = 'N';
083900080728           *in28 = *on;
084000080728           *in42 = *on;
084100080728           v1cmsg = msg(08);
084200080728           leavesr;
084300080728          other;
084400080731       //?imposto lo stesso numero nel campo 'al' se non impostato
084500080728           if v1ndcal = *zeros;
084600080728            v1ndcal = wndcdal;
084700080728           endif;
084800080728          endsl;
084900080728         endif;
085000080725
085100080731       //?numero distinta al deve essere impostato a con distinta o 999999
085200080728         if v1ndcal <= *zeros;
085300080728          *in28 = *on;
085400080728          *in43 = *on;
085500080728          v1cmsg = msg(09);
085600080728          leavesr;
085700080728         endif;
085800080725
085900080731       //?range incompatibile
086000080728         if wndcdal > v1ndcal;
086100080728          *in28 = *on;
086200080728          *in43 = *on;
086300080728          v1cmsg = msg(10);
086400080728          leavesr;
086500080728         endif;
086600080725
086700080731       //?se richiesta una sola distinta
086800080728         if wndcdal = v1ndcal;
086900080731       //?controllo se distinta chiusa per consegne e ritiri
087000080730          wfgs = v1cfgs;
087100080730          wndc = wndcdal;
087200080730          exsr sr_dstchiusa;
087300080730          if $chiusa;
087400080730           *in28 = *on;
087500080730           *in42 = *on;
087600080730           v1cmsg = msg(07);
087700080730           leavesr;
087800080730          endif;
087900080728         endif;
088000080725
088100080725       endsr;
088200080728
088300080731       // ----------------------------------------------------------------------
088400080731       //?Controllo autotrasportatore
088500080731       // ----------------------------------------------------------------------
088600080728       begsr sr_contraut;
088700080729
088800080729         clear v1daut;
088900080804       //?se autotrasportatore non impostato vado a fine controlli
089000080804         if v1caut = *blanks;
089100080804          leavesr;
089200080804         endif;
089300080804       //?se codice a zero lo metto a blank
089400080804         if v1caut = *zeros;
089500080804          clear v1caut;
089600080804          *in90 = *on;
089700080804          leavesr;
089800080804         endif;
089900080729
090000080731       //?se richiesto un range di distinte non posso richiedere
090100080731       //?anche l'autotrasportatore
090200080915         if wndcdal > *zeros or (v1ndcal > *zeros and v1ndcal < 999999);
090300080729          *in28 = *on;
090400080729          *in44 = *on;
090500080730          v1cmsg = msg(14);
090600080729          leavesr;
090700080729         endif;
090800080728
090900080731       //?ricerca dell'autotrasportatore
091000080728         if %scan('?':v1caut) > *zeros;
091100080728          *in44 = *on;
091200080728          *in90 = *on;
091300080728          clear fnlv24ds;
091400080728          d24fil = dutpou;
091500080728          d24tip = 'A';
091600080728          d24flg = 'R';
091700080728          kpjbu = fnlv24ds;
091800080728          fnlv24r(kpjba);
091900080731       //?è stato scelto un codice lo imposto a video
092000080728          fnlv24ds = kpjbu;
092100080728          if d24pdr <> *zeros;
092200080731           v1caut = %editc(%dec(d24pdr):'X');
092300080728           v1daut = d24rsc;
092400080728           leavesr;
092500080728          endif;
092600080731       //?non è stato scelto nessun codice pulisco i campi a video
092700080728          clear v1caut;
092800080728          clear v1daut;
092900080728          leavesr;
093000080728         endif;
093100080728
093200080731       //?controllo
093300080731         if v1caut <> *blanks and v1caut <> *zeros;
093400080731       //?deve essere numerico
093500080728          if %check(digitn:v1caut) > *zeros;
093600080728           *in28 = *on;
093700080728           *in44 = *on;
093800080730           v1cmsg = msg(11);
093900080728           leavesr;
094000080728          endif;
094100080731       //?deve essere un codice valido
094200080728          apdtip = 'A';
094300080801          apdpdr = %dec(v1caut:7:0);
094400080728          chain (apdtip:apdpdr) fiapd01l;
094500080728          if not %found(fiapd01l) or apdatb <> *blanks;
094600080728           *in28 = *on;
094700080728           *in44 = *on;
094800080730           v1cmsg = msg(12);
094900080728           leavesr;
095000080728          endif;
095100080728         endif;
095200080728
095300080731       //?l'autotrasportatore deve essere in gestione all'utente in base alla
095400080731       //?filiale gestione impostata a video
095500080728         w0030 = %dec(%subst(v1caut:1:3):3:0);
095600080728         if w0030 <> v1cfgs;
095700080728          *in28 = *on;
095800080728          *in44 = *on;
095900080730          v1cmsg = msg(13);
096000080728          leavesr;
096100080728         endif;
096200080729
096300080731       //?imposto campo numerico dell'AUT
096400080731           waut = %dec(v1caut:7:0);
096500080728
096600080728       endsr;
096700080916
096800080916       // ----------------------------------------------------------------------
096900080916       //?Controllo parzializzazioni
097000080916       // ----------------------------------------------------------------------
097100080916       begsr sr_contrpar;
097200080916
097300080916       //?le parzializzazioni non sono in alternativa
097400080916         if v1sca <> *blanks and (v1ok <> *blanks or v1info <> *blanks or
097500080916                                  v1cus <> *blanks);
097600080916          *in28 = *on;
097700080916          *in45 = *on;
097800080916          v1cmsg = msg(15);
097900080916          leavesr;
098000080916         endif;
098100080916         if v1ok <> *blanks and (v1info <> *blanks or v1cus <> *blanks);
098200080916          *in28 = *on;
098300080916          *in46 = *on;
098400080916          v1cmsg = msg(15);
098500080916          leavesr;
098600080916         endif;
098700080916         if v1info <> *blanks and v1cus <> *blanks;
098800080916          *in28 = *on;
098900080916          *in47 = *on;
099000080916          v1cmsg = msg(15);
099100080916          leavesr;
099200080916         endif;
099300080916
099400080916       endsr;
099500080730
099600080731       // ----------------------------------------------------------------------
099700110616       //?Pulisco subfile attività
099800080731       // ----------------------------------------------------------------------
099900110616       begsr sr_puls01;
100000080730
100100080801         clear nrr;
100200080801         clear rrnlast;
100300110616         clear V1TASSC;
100400110616         clear V1TASSR;
100500110616         clear V1TESIC;
100600110616         clear V1TPESC;
100700110616         clear V1TESIR;
100800110616         clear V1TPESR;
100900110616         clear V1TESNC;
101000110616         clear V1TESNR;
101100110616         clear V1TNESC;
101200110616         clear V1TNESR;
101300080801
101400110616         *in85 = *off;
101500110616         *in86 = *off;
101600110616         write lvp0c01;
101700110616         *in85 = *on;
101800110616         *in86 = *on;
101900080730
102000110616         recsf2 = 1;
102100110616         *in87 = *off;
102200080923
102300080923       //?imposto la data in testata
102400080923         vtcddc = v1cddc;
102500080730
102600080730       endsr;
102700080729
102800110616       // ----------------------------------------------------------------------
102900110616       //?Pulisco subfile distinte
103000110616       // ----------------------------------------------------------------------
103100110616       begsr sr_puls02;
103200110616
103300110616         clear nrr;
103400110616         clear rrnlast;
103500110617
103600110616         *in20 = *off;
103700110616         *in21 = *off;
103800110616         write lvp0c02;
103900110616         *in20 = *on;
104000110616         *in21 = *on;
104100110616
104200110616         recsf2 = 1;
104300110621         *in31 = *off;
104400110616
104500110616       //?imposto la data in testata
104600110616         vtcddc = v1cddc;
104700110616
104800110616       endsr;
104900110616
105000080731       // ----------------------------------------------------------------------
105100080731       //?Carico le distinte a PDA
105200080731       // ----------------------------------------------------------------------
105300080729       begsr sr_cardst;
105400080813
105500080813         $finedst = *off;
105600080729
105700080731       //?sql divisi in base alle selezioni fatte
105800080731       //?prepara cursore
105900080729         select;
106000080729
106100080731       //?sql per tutte le distinte del giorno dell'AUT
106200080912         when waut > *zeros;
106300080729          exec sql
106400080729           declare autdst cursor for select fidst02l.*
106500080729           from fidst02l where dstnpg = 4 and dstdfv = :wdata and
106600110621           dstpda <> 'N' and dstpdr = :waut and dstatb = '';
106700080729
106800080731       //?sql per tutte le distinte del giorno
106900080729         when wndcdal = *zeros;
107000080729          exec sql
107100080729           declare alldst cursor for select fidst02l.*
107200080729           from fidst02l where dstnpg = 4 and dstdfv = :wdata and
107300110621           dstpda <> 'N' and dstfgs = :v1cfgs and dstatb = '';
107400080729
107500080731       //?sql per range di distinte del giorno
107600080729         when wndcdal <> *zeros;
107700080729          exec sql
107800080729           declare rangedst cursor for select fidst02l.*
107900080729           from fidst02l where dstnpg = 4 and dstdfv = :wdata and
108000080729           dstpda <> 'N' and dstnfv between :wndcdal and :v1ndcal
108100110621           and dstfgs = :v1cfgs and dstatb = '';
108200080729
108300080729         endsl;
108400080729
108500080731       //?open cursori
108600080729         select;
108700080731       //?sql per tutte le distinte del giorno dell'AUT
108800080731         when waut <> *zeros;
108900080729          exec sql open autdst;
109000080731       //?sql per tutte le distinte del giorno
109100080729         when wndcdal = *zeros;
109200080729          exec sql open alldst;
109300080731       //?sql per range di distinte del giorno
109400080729         when wndcdal <> *zeros;
109500080729          exec sql open rangedst;
109600080729         endsl;
109700080729
109800080729       endsr;
109900080729
110000080731       // ----------------------------------------------------------------------
110100080731       //?Elaboro le distinte a PDA
110200080731       // ----------------------------------------------------------------------
110300080730       begsr sr_elabdst;
110400080729
110500080813         dow not $finedst;
110600080729
110700080731       //?leggo cursori
110800080730          select;
110900080731       //?sql per tutte le distinte del giorno dell'AUT
111000080731          when waut <> *zeros;
111100080730           exec sql fetch next from autdst into: fidstds;
111200080731       //?sql per tutte le distinte del giorno
111300080730          when wndcdal = *zeros;
111400080730           exec sql fetch next from alldst into: fidstds;
111500080731       //?sql per range di distinte del giorno
111600080730          when wndcdal <> *zeros;
111700080730           exec sql fetch next from rangedst into: fidstds;
111800080730          endsl;
111900080729
112000080731       //?fine file o errore sql esco
112100080730          if sqlcod = 100 or sqlcod < 0;
112200110621         *in31 = *on;
112300110621         *in87 = *on;
112400080813           $finedst = *on;
112500080730           leave;
112600080730          endif;
112700080729
112800080730          ddstflr = dstflr;
112900160512
113000160512          if §dstturno = ' ' and dstTPM <>'M' and §ogturno='S';
113100160511             §dstturno = '1';
113200160511          endif;
113300160511
113400080813       //?spengo indicatori per richieste info
113500080813          $infoc = *off;
113600080813          $infor = *off;
113700080729
113800080731       //?controllo congruenza tipo distinta
113900120223          if v1tdis <> *blanks;
114000120223           if v1tdis = 'P' and dsttpm <> ' ';
114100080730           iter;
114200120223           endif;
114300120223           if v1tdis = 'M' and dsttpm <> v1tdis;
114400120223           iter;
114500120223           endif;
114600120223          endif;
114700080729
114800080731       //?controllo congruenza tipo uscita
114900080730          if v1tusc <> *blanks and dstfpp <> v1tusc;
115000080730           iter;
115100080730          endif;
115200080729
115300160510
115400160510       //?controllo congruenza con il Turno se previsto
115500170227       //?   solo se Distinta PARCEL  (ossia blank)
115600170227          if §ogturno='S' and v1tdis <>'M' and dstTPM = ' ' ;
115700160512            if §dstturno = v1tur1 or §dstturno = v1tur2;
115800160511            else;
115900160511              iter;
116000160511            endif;
116100160511          endif;
116200160510
116300080731       //?controllo se distinta chiusa per consegne e ritiri
116400080730          wfgs = dstfgs;
116500080730          wndc = dstnfv;
116600080730          exsr sr_dstchiusa;
116700080730          if $chiusa;
116800080730           iter;
116900080730          endif;
117000080729
117100080731       //?calcolo i totali dei dati da preparare
117200080731          exsr sr_dstsca;
117300080731       //?calcolo i totali dei dati da confermare
117400080731          exsr sr_dstconf;
117500080731       //?controllo se OK ad uscire
117600080731          wfgs = dstfgs;
117700080731          wndc = dstnfv;
117800080731          exsr sr_dstcus;
117900080731       //?recupero ragione sociale AUT
118000080801          wpdr = dstpdr;
118100080731          exsr sr_dstaut;
118200110616       //?calcolo i totali dei dati esitati
118300110616          exsr sr_dstesiti;
118400110616       //?scrivo subfile distinte e attività
118500080731          *in03 = *off;
118600080813          v2sin03 = '0';
118700080730          exsr sr_cars02;
118800080731       //?leggo se ho dei dati preparati per altro AUT
118900080731       //?così lo emetto a video
119000080922
119100080922       //?carico prima gli AUT diversi in SK
119200110620         if $finedst=*off;
119300080922         exsr sr_skaut;
119400080922          xx = 1;
119500080922          for xx by 1 to 99;
119600080922           if autsk(xx) > 0;
119700080922           wpdr = autsk(xx);
119800110617           exsr sr_altroaut;
119900110617           else;
120000110617           leave;
120100080922           endif;
120200080922          endfor;
120300110620           endif;
120400080730         enddo;
120500080801
120600080801       //?indicatore per fine file
120700080801       //?imposto il numero di record caricati
120800080801         rrnlast = nrr;
120900080729
121000080731       //?chiudo i cursori
121100080730         select;
121200080731       //?sql per tutte le distinte del giorno dell'AUT
121300080731         when waut <> *zeros;
121400080730          exec sql close autdst;
121500080731       //?sql per tutte le distinte del giorno
121600080730         when wndcdal = *zeros;
121700080730          exec sql close alldst;
121800080731       //?sql per range di distinte del giorno
121900080730         when wndcdal <> *zeros;
122000080730          exec sql close rangedst;
122100080730         endsl;
122200080729
122300080729       endsr;
122400080729
122500080731       // ----------------------------------------------------------------------
122600080731       //?Controllo se distinta chiusa consegne e ritiri
122700080731       // ----------------------------------------------------------------------
122800080729       begsr sr_dstchiusa;
122900080729
123000080731         $chiusar = *off;
123100080731         $chiusac = *off;
123200080731         $chiusa = *off;
123300080729
123400080731       //?se c'è la fase RCH o la RQE è chiusa x i ritiri
123500080729         clear fior90ds;
123600080729         or90tla = 'C';
123700080729         or90fas = 'RCH';
123800080730         or90fgs = wfgs;
123900080730         or90ndc = wndc;
124000080729         fior90r(kpjba:fior90ds);
124100080731       //?se trovo la fase RCH imposto che è chiusa
124200080729         if or90ndcok <> *blanks;
124300080731          $chiusar = *on;
124400080729         endif;
124500080731       //?se non trovo la fase RCH provo con la fase RQE
124600080729         if or90ndcok = *blanks;
124700080729          clear fior90ds;
124800080729          or90tla = 'C';
124900080729          or90fas = 'RQE';
125000080730          or90fgs = wfgs;
125100080730          or90ndc = wndc;
125200080729          fior90r(kpjba:fior90ds);
125300080731       //?se trovo la fase RQE imposto che è chiusa
125400080729          if or90ndcok <> *blanks;
125500080731           $chiusar = *on;
125600080729          endif;
125700080730         endif;
125800080730
125900080731       //?se c'è la fase CRI è chiusa x le consegne
126000080730         clear fior90ds;
126100080730         or90tla = 'C';
126200080730         or90fas = 'CRI';
126300080730         or90fgs = wfgs;
126400080730         or90ndc = wndc;
126500080730         fior90r(kpjba:fior90ds);
126600080731       //?se trovo la fase CRI imposto che è chiusa
126700080730         if or90ndcok <> *blanks;
126800080731          $chiusac = *on;
126900080730         endif;
127000080731       //?se non trovo la fase CRI controllo se chiusa con flag
127100080730         if or90ndcok = *blanks and dstfcf = 'S';
127200080731          $chiusac = *on;
127300080729         endif;
127400080730
127500080731       //?la distinta è chiusa x le consegne e i ritiri imposto
127600080731       //?il flag
127700080730         if $chiusar and $chiusac;
127800080730          $chiusa = *on;
127900080730         endif;
128000080729
128100080729       endsr;
128200080731
128300080731       // ----------------------------------------------------------------------
128400080731       //?Calcolo il totale da preparare
128500080731       // ----------------------------------------------------------------------
128600080731       begsr sr_dstsca;
128700080731
128800080731       //?inizializzo i vari campi di appoggio
128900080731         clear contar;
129000080731         clear contac;
129100080731         clear contapdo;
129200080731         clear contapdc;
129300080731         clear totscar;
129400080731         clear totscac;
129500080731         $nor = *off;
129600080731         $noc = *off;
129700080731
129800080801       //?RITIRI
129900080922       //?se distinta non è già chiusa per ritiri
130000080922         if $chiusar = *off;
130100080801       //?per ogni distinta devo contare quanti ritiri
130200080731       //?sono presenti in distinta (da preparare)
130300080801       //?--> FNORG
130400080801         if dstpda = 'O' or dstpda = 'E';
130500080801          exec sql
130600080801           select count(*) into :contar from fnorg00f where
130700080801            orgfgs = :dstfgs and orgndc = :dstnfv;
130800080801       //?se ho trovato almeno un ritiro per la distinta/AUT
130900080801       //?conto quanti ritiri ho già preparato per il PDA
131000080801       //?--> FIPDO
131100080801          if contar > *zeros;
131200080801           exec sql
131300080801            select count(*) into :contapdo from fipdo00t where
131400080801            pdofgs = :dstfgs and pdondc = :dstnfv and
131500080801            pdocodaut = :dstpdr and pdoatb = '';
131600080801       //?calcolo il totale dei ritiri da preparare
131700080801           totscar = contar - contapdo;
131800080801          else;
131900080801       //?non ho neanche un ritiro sulla distinta lo evidenzio
132000080801           $nor = *on;
132100080801          endif;
132200080917         else;
132300080917       //?non sono previsti ritiri sulla distinta lo evidenzio
132400080917          $nor = *on;
132500080801         endif;
132600080922         endif;
132700080731
132800080801       //?CONSEGNE
132900080922       //?se distinta non è già chiusa per le consegne
133000080922         if $chiusac = *off;
133100080801       //?per ogni distinta devo contare quante consegne
133200080801       //?sono presenti in distinta (da preparare)
133300080801         if dstpda = 'C' or dstpda = 'E';
133400080801       //?--> FNARB
133500080801          exec sql
133600080801           select count(*) into :contac from fnarb00f where
133700080801            arbifp = :dstfgs and arbndc = :dstnfv;
133800080801       //?se ho trovato almeno una consegna per la distinta/AUT
133900080801       //?conto quante consegne ho già preparato per il PDA
134000080801       //?--> FIPDC
134100080801          if contac > *zeros;
134200080801           exec sql
134300080801            select count(*) into :contapdc from fipdc00t where
134400080801            pdcfgs = :dstfgs and pdcndc = :dstnfv and
134500080801            pdccodaut = :dstpdr and pdcatb = '';
134600080731       //?calcolo il totale delle consegne da preparare
134700080801           totscac = contac - contapdc;
134800080801          else;
134900080731       //?non ho neanche una consegna sulla distinta lo evidenzio
135000080801           $noc = *on;
135100080801          endif;
135200080917         else;
135300080917       //?non sono previste consegne sulla distinta lo evidenzio
135400080917          $noc = *on;
135500080801         endif;
135600080922         endif;
135700080731
135800080731       endsr;
135900080730
136000080731       // ----------------------------------------------------------------------
136100080731       //?Calcolo il totale da confermare
136200080731       // ----------------------------------------------------------------------
136300080731       begsr sr_dstconf;
136400080813
136500080813         $finepdc = *off;
136600080813         $finepdo = *off;
136700080731
136800080731       //?inizializzo i vari campi di appoggio
136900080731         clear contapdo;
137000080731         clear contapdc;
137100080731         clear contapro;
137200080912         clear contapct;
137300080731         clear totconfr;
137400080731         clear totconfc;
137500080730
137600080731       //?per ogni distinta devo contare quante conferme ho ricevuto
137700080731       //?RITIRI --> FIPDO leggo quello che ho preparato
137800080731       //?                 per distinta e AUT
137900080731       //?       --> FIPRO leggo le conferme ricevute
138000080731       //?                 per distinta - AUT - ORM a parità di idrow
138100080730         exec sql
138200080730          declare pdo cursor for select fipdo00t.*
138300080730          from fipdo00t where pdofgs = :dstfgs and
138400080731          pdondc = :dstnfv and pdocodaut = :dstpdr
138500080730          order by pdofgs, pdondc, pdocodaut, pdopoe, pdonsr, pdonor, pdonrv;
138600080731         exec sql open pdo;
138700080731       //?leggo il file dei dati preparati
138800080813         dow not $finepdo;
138900080730          exec sql fetch next from pdo into: fipdods;
139000080731       //?fine file o errore sql esco
139100080730          if sqlcod = 100 or sqlcod < 0;
139200080813           $finepdo = *on;
139300080730           leave;
139400080730          endif;
139500080731       //?per ogni distinta/aut/orm conto gli orm preparati
139600080731          contapdo = contapdo + 1;
139700080731       //?cerco gli ok di conferma a parità di idrow
139800080731          exec sql
139900140210           select fipro10f.* into :fiprods from fipro10f
140000080731           where profgs = :pdofgs and prondc = :pdondc and
140100080912           procodaut = :pdocodaut and
140200080731           propoe = :pdopoe and
140300080731           pronsr = :pdonsr and pronor = :pdonor and
140400140210           pronrv = :pdonrv and protrd = 'OK ' and
140500140210           substr(prodati, 3, 2) = :pdoidrow;
140600080731       //?trovo il rcd corrispondente
140700080813          if sqlcod = 0;
140800080731       //?come confermato
140900080731           contapro = contapro + 1;
141000080731       //?controllo se ha richiesta info
141100140313          if pdoATB = ' ';
141200140210           clear fior82ds;
141300140210           c82fgs = profgs;
141400140210           c82ndc = prondc;
141500140210           c82codaut = procodaut;
141600140210           c82POE = proPOE;
141700140210           c82NSR = proNSR;
141800140210           c82NOR = proNOR;
141900140210           c82NRV = proNRV;
142000140210           kpjbu = fior82ds;
142100140210           fior82r(kpjba);
142200140210           fior82ds = kpjbu;
142300140210           if c82davisio <> '';
142400140210            $infor = *on;
142500140210           endif;
142600140210           endif;
142700080731          endif;
142800080730         enddo;
142900080731       //?chiuso cursore
143000080731         exec sql close pdo;
143100080731       //?calcolo quanti sono ancora da confermare
143200080731         totconfr = contapdo - contapro;
143300080730
143400080731       //?CONSEGNE --> FIPDC leggo quello che ho preparato
143500080731       //?                   per distinta e AUT
143600080731       //?         --> FIPCT leggo le conferme ricevute
143700080731       //?                 per distinta - AUT - bolla a parità di idrow
143800080731         exec sql
143900080731          declare pdc cursor for select fipdc00t.*
144000110216          from fipdc00t where pdcfgs = :dstfgs and pdcatb = ' ' and
144100080731          pdcndc = :dstnfv and pdccodaut = :dstpdr
144200080731          order by pdcfgs, pdcndc, pdccodaut, pdcaas, pdclnp, pdcnrs, pdcnsp;
144300080731         exec sql open pdc;
144400080731       //?leggo il file dei dati preparati
144500080813         dow not $finepdc;
144600080731          exec sql fetch next from pdc into: fipdcds;
144700080731       //?fine file o errore sql esco
144800080731          if sqlcod = 100 or sqlcod < 0;
144900080813           $finepdc = *on;
145000080731           leave;
145100080731          endif;
145200080731       //?per ogni distinta/aut/bolla conto le bolle preparate
145300080731          contapdc = contapdc + 1;
145400080731       //?cerco gli ok di conferma a parità di idrow
145500080731          exec sql
145600080731           select fipct00f.* into :fipctds from fipct00f
145700080731           where pctfgs = :pdcfgs and pctndc = :pdcndc and
145800080912           pctcodaut = :pdccodaut and
145900080731           pctaas = :pdcaas and
146000080731           pctlnp = :pdclnp and pctnrs = :pdcnrs and
146100080731           pctnsp = :pdcnsp and pcttrd = 'COK' and
146200080731           substr(pctdati, 3, 2) = :pdcidrow;
146300080731       //?trovo il rcd corrispondente
146400080807          if sqlcod = 0;
146500080731       //?come confermato
146600080731           contapct = contapct + 1;
146700080731       //?controllo se ha richiesta info
146800140313          if pdcATB = ' ';
146900080731           clear fidg45ds;
147000080902           c45fgs = pctfgs;
147100080902           c45ndc = pctndc;
147200080902           c45codaut = pctcodaut;
147300080731           c45aas = pctaas;
147400080731           c45lnp = pctlnp;
147500080731           c45nrs = pctnrs;
147600080731           c45nsp = pctnsp;
147700080731           kpjbu = fidg45ds;
147800080731           fidg45r(kpjba);
147900080731           fidg45ds = kpjbu;
148000080731           if c45davisio <> '';
148100080731            $infoc = *on;
148200080731           endif;
148300110616           endif;
148400080731          endif;
148500080731         enddo;
148600080731       //?chiuso cursore
148700080731         exec sql close pdc;
148800080731       //?calcolo quanti sono ancora da confermare
148900080731         totconfc = contapdc - contapct;
149000080730
149100080730       endsr;
149200080731
149300080731       // ----------------------------------------------------------------------
149400080731       //?Controllo se c'è OK ad uscire
149500080731       // ----------------------------------------------------------------------
149600080731       begsr sr_dstcus;
149700080731
149800080731         $cus = *off;
149900080731         clear fior90ds;
150000080731         or90tla = 'C';
150100080731         or90fas = 'CUS';
150200080731         or90fgs = wfgs;
150300080731         or90ndc = wndc;
150400080731         fior90r(kpjba:fior90ds);
150500080731       //?se trovo la fase CUS imposto flag
150600080731         if or90ndcok <> *blanks;
150700080731          $cus = *on;
150800080731         endif;
150900080731
151000080731       endsr;
151100080731
151200110616       // ----------------------------------------------------------------------
151300110616       //?Controllo se c'è OK ad uscire
151400110616       // ----------------------------------------------------------------------
151500110616       begsr sr_dstora;
151600110616
151700110616         clear plgdatora;
151800110616          exec sql
151900110616           select max(plgdatora) into :plgdatora from fiplg00f
152000110616           where plgfgs = :dstfgs and plgndc = :dstnfv and
152100110616           plgcodaut = :wpdr and plgerr= ' ';
152200110617           ora = %subst(plgdatora:9:4);
152300110617       if ora <> ' ';
152400110617           v2sora = %dec(ora:4:0);
152500110617       else;
152600110617           clear v2sora;
152700110617       endif;
152800110616       endsr;
152900110616
153000080731       // ----------------------------------------------------------------------
153100080731       //?Recupero ragione sociale AUT
153200080731       // ----------------------------------------------------------------------
153300080731       begsr sr_dstaut;
153400080731
153500080731          exec sql
153600080731           select apdrsc into :apdrsc from fiapd00f
153700080731           where apdtip = 'A' and apdatb = '' and
153800080801           apdpdr = :wpdr;
153900080731       //?non trovo il rcd corrispondente
154000080731          if sqlcod <> 0;
154100080731           clear apdrsc;
154200080731          endif;
154300080731
154400080731       endsr;
154500080922
154600110616       // ----------------------------------------------------------------------
154700110616       //?Calcolo il totale degli esiti
154800110616       // ----------------------------------------------------------------------
154900110616       begsr sr_dstesiti;
155000110616
155100110616       //?inizializzo i vari campi di appoggio
155200110616         clear conta   ;
155300110616         clear V1SESIc ;
155400110616         clear V1SPEc  ;
155500110616         clear V1SESIR ;
155600110616         clear V1SPER  ;
155700110616         clear V1SESNC ;
155800110616         clear V1SESNR ;
155900110616         clear V1SNESiC;
156000110616         clear V1SNESiR;
156100110617          *in60 = *off;
156200110617          *in61 = *off;
156300110616
156400110616       //?per ogni distinta devo contare quanti esiti ritiri e consegne
156500110616       //?sono stati ricevuti
156600110616          exec sql
156700110617          select count(*) into :conta from (SELECT DISTINCT
156800110616          plgiddoc FROM fiplg00f WHERE plgfgs=:dstfgs
156900110616          and plgndc = :dstnfv and
157000110617          plgcodaut = :wpdr and plgerr=' ' and plgtiporcd = 'RES'
157100110617          group by plgiddoc ) as pippo;
157200110616       //?calcolo il totale dei ritiri esitati
157300110617         v1sesir=conta;
157400110617            if conta <> 0 and CONTAR <>0 ;
157500120131            comper=conta*100/CONTAR;
157600120131            if comper > 999;
157700120131            v1sper=999;
157800120131            else;
157900120131            v1sper=comper;
158000120131            endif;
158100110617            *in61 = *on;
158200110616            endif;
158300110617         clear conta   ;
158400110616       //?calcolo il totale delle consegne esitate
158500110617          exec sql
158600110617          select count(*) into :conta from (SELECT DISTINCT
158700110617          plgiddoc FROM fiplg00f WHERE plgfgs=:dstfgs
158800110617          and plgndc = :dstnfv and
158900110617          plgcodaut = :wpdr and plgerr=' ' and plgtiporcd = 'CET'
159000110617          group by plgiddoc ) as pippo;
159100110617       //?calcolo il totale dei ritiri esitati
159200110616           v1sesic=conta;
159300110617            if conta <> 0 and CONTAC <>0 ;
159400120131            comper=conta*100/CONTAC;
159500120131            if comper > 999;
159600120131            v1spec=999;
159700120131            else;
159800120131            v1spec=comper;
159900120131            endif;
160000110617            *in60 = *on;
160100110616            endif;
160200110617           clear conta   ;
160300110616       //?conto quanti esiti RITIRI negativi ho ricevuto
160400110616          exec sql
160500110707
160600110707          with selPLG as
160700110707          (select fiplg00f.* from fiplg00f WHERE plgfgs=:dstfgs and
160800110707          plgndc=:dstnfv and plgcodaut=:Wpdr and
160900110707          plgerr=' '     and plgtiporcd = 'RES' and PLGTIPAPPL = 'R')
161000110707
161100110707          select  count(*) into :conta
161200110707          from (SELECT DISTINCT plgiddoc, max(plgdatora) maxdta
161300110707          from selplg group by plgiddoc ) as pippo join fiplg00f a on
161400110707          pippo.plgiddoc = a.plgiddoc and pippo.maxdta = a.plgdatora and
161500110707          'RES' = a.plgtiporcd where substr(plgdati, 83, 3) <> ' ';
161600110616
161700110616           v1sesnr=conta;
161800110616           clear conta;
161900110616       //?conto quanti esiti CONSEGNE negative ho ricevuto
162000110616          exec sql
162100110707
162200110707          with selPLG as
162300110707          (select fiplg00f.* from fiplg00f WHERE plgfgs=:dstfgs and
162400110707          plgndc=:dstnfv and plgcodaut=:Wpdr and
162500110707          plgerr=' '     and plgtiporcd = 'CET' and PLGTIPAPPL = 'C')
162600110707
162700110707          select count(*) into :conta
162800110707          from (SELECT DISTINCT plgiddoc, max(plgdatora) maxdta
162900110707          from selplg group by plgiddoc ) as pippo join fiplg00f a on
163000110707          pippo.plgiddoc = a.plgiddoc and pippo.maxdta = a.plgdatora and
163100110707          'CET' = a.plgtiporcd where substr(plgdati, 26, 3)
163200110707          not in (' ', 'P');
163300110707
163400110616           v1sesnc=conta;
163500110616           clear conta;
163600110616       //?conto quanti esiti mancano
163700110617          if CONTAR > v1sesir;
163800110617           v1snesir=CONTAR-v1sesir;
163900110616          endif;
164000110617          if CONTAC > v1sesic;
164100110617           v1snesic=CONTAC-v1sesic;
164200110616          endif;
164300110616       //?conto totali filiale
164400110617         V1TASSC = v1tassc + CONTAC;
164500110617         V1TASSR = v1tassr + CONTAR;
164600110616         V1TESIC = v1tesic + v1sesic ;
164700110616            if v1tesic <> 0 and v1tassc <>0 ;
164800120131            comper=v1tesic*100/v1tassc;
164900120131            if comper > 999;
165000120131            v1tpesc=999;
165100120131            else;
165200120131            v1tpesc=comper;
165300120131            endif;
165400110616            endif;
165500110616         V1TESIR = v1tesir + v1sesir;
165600110616         clear V1TPESR;
165700110616            if v1tesir <> 0 and v1tassr <>0 ;
165800120131            comper=v1tesir*100/v1tassr;
165900120131            if comper > 999;
166000120131            v1tpesr=999;
166100120131            else;
166200120131            v1tpesr=comper;
166300120131            endif;
166400110616            endif;
166500110616         V1TESNC = v1tesnc + v1sesnc;
166600110616         V1TESNR = v1tesnr + v1sesnr;
166700110616         V1TNESC = v1tnesc + v1snesic;
166800110616         V1TNESR = v1tnesr + v1snesir;
166900110616       endsr;
167000110616
167100080922       // ----------------------------------------------------------------------
167200080922       //?Leggo se ho dati preparati per altro AUT e carico la sk
167300080922       // ----------------------------------------------------------------------
167400080922       begsr sr_skaut;
167500080922
167600080922        clear autsk;
167700080922
167800080922       //?leggo FIPDO
167900080922         exec sql
168000080922          declare pdoaut cursor for select distinct(pdocodaut)
168100080922          from fipdo00t where pdofgs = :dstfgs and
168200080922          pdondc = :dstnfv and pdocodaut <> :dstpdr
168300080922          order by pdocodaut;
168400080922         exec sql open pdoaut;
168500080922       //?leggo il file dei dati preparati
168600080922         dou sqlcod <> 0;
168700080922          exec sql fetch next from pdoaut into: altroaut;
168800080922       //?fine file o errore sql esco
168900080922          if sqlcod = 100 or sqlcod < 0;
169000080922           leave;
169100080922          endif;
169200080922       //?carico sk
169300080922          if %lookup(altroaut:autsk) = *zeros;
169400080922           xx = 1;
169500080922           yy = %lookup(*zeros:autsk:xx);
169600080922           if yy > 0;
169700080922            autsk(yy) = altroaut;
169800080922           endif;
169900080922          endif;
170000080922         enddo;
170100080922       //?chiuso cursore
170200080922         exec sql close pdoaut;
170300080922
170400080922       //?leggo FIPDC
170500080922         exec sql
170600080922          declare pdcaut cursor for select distinct(pdccodaut)
170700080922          from fipdc00t where pdcfgs = :dstfgs and
170800080922          pdcndc = :dstnfv and pdccodaut <> :dstpdr
170900080922          order by pdccodaut;
171000080922         exec sql open pdcaut;
171100080922       //?leggo il file dei dati preparati
171200080922         dou sqlcod <> 0;
171300080922          exec sql fetch next from pdcaut into: altroaut;
171400080922       //?fine file o errore sql esco
171500080922          if sqlcod = 100 or sqlcod < 0;
171600080922           leave;
171700080922          endif;
171800080922       //?carico sk
171900080922          if %lookup(altroaut:autsk) = *zeros;
172000080922           xx = 1;
172100080922           yy = %lookup(*zeros:autsk:xx);
172200080922           if yy > 0;
172300080922            autsk(yy) = altroaut;
172400080922           endif;
172500080922          endif;
172600080922         enddo;
172700080922       //?chiuso cursore
172800080922         exec sql close pdcaut;
172900080922
173000080922       endsr;
173100080731
173200080731       // ----------------------------------------------------------------------
173300080731       //?Leggo se ho dati preparati per altro AUT
173400080731       // ----------------------------------------------------------------------
173500080731       begsr sr_altroaut;
173600080731
173700080731       //?controllo se ho dei dati preparati per AUT diverso da quello
173800080731       //?impostato in distinta (caso di cambio AUT sulla stessa distinta)
173900080731
174000080731       //?inizializzo i vari campi di appoggio
174100080731         clear contapdo;
174200080731         clear contapdc;
174300080731         clear contapro;
174400080801         clear contapct;
174500080731         clear totconfr;
174600080731         clear totconfc;
174700110701         clear totscac;
174800110701         clear totscar;
174900110701         clear contac;
175000110701         clear contar;
175100110701         $finepdc = *off;
175200110701         $finepdo = *off;
175300080731
175400080731       //?RITIRI --> FIPDO leggo quello che ho preparato
175500080731       //?                 per distinta e AUT <> da FIDST
175600080731       //?       --> FIPRO leggo le conferme ricevute
175700080731       //?                 per distinta - AUT - ORM a parità di idrow
175800080731         exec sql
175900080731          declare pdoa cursor for select fipdo00t.*
176000080731          from fipdo00t where pdofgs = :dstfgs and
176100080922          pdondc = :dstnfv and pdocodaut = :wpdr
176200080731          order by pdofgs, pdondc, pdocodaut, pdopoe, pdonsr, pdonor, pdonrv;
176300080731         exec sql open pdoa;
176400080731       //?leggo il file dei dati preparati
176500110701         dow not $finepdo;
176600080813          exec sql fetch next from pdoa into: fipdods;
176700080731       //?fine file o errore sql esco
176800080731          if sqlcod = 100 or sqlcod < 0;
176900110701           $finepdo = *on;
177000080731           leave;
177100080731          endif;
177200080731       //?per ogni distinta/aut/orm conto gli orm preparati
177300080731          contapdo = contapdo + 1;
177400080731       //?cerco gli ok di conferma a parità di idrow
177500080731          exec sql
177600140210           select fipro10f.* into :fiprods from fipro10f
177700080731           where profgs = :pdofgs and prondc = :pdondc and
177800080912           procodaut = :pdocodaut and
177900080731           propoe = :pdopoe and
178000080731           pronsr = :pdonsr and pronor = :pdonor and
178100140210           pronrv = :pdonrv and protrd = 'OK ' and
178200140210           substr(prodati, 3, 2) = :pdoidrow;
178300080731       //?trovo il rcd corrispondente
178400080731          if  sqlcod = 0;
178500080731       //?come confermato
178600080731           contapro = contapro + 1;
178700080731       //?controllo se ha richiesta info
178800140210          if proATB = ' ';
178900140210           clear fior82ds;
179000140210           c82fgs = profgs;
179100140210           c82ndc = prondc;
179200140210           c82codaut = procodaut;
179300140210           c82POE = proPOE;
179400140210           c82NSR = proNSR;
179500140210           c82NOR = proNOR;
179600140210           c82NRV = proNRV;
179700140210           kpjbu = fior82ds;
179800140210           fior82r(kpjba);
179900140210           fior82ds = kpjbu;
180000140210           if c82davisio <> '';
180100140210            $infor = *on;
180200140210           endif;
180300140210           endif;
180400140210          endif;
180500080731         enddo;
180600080731       //?chiuso cursore
180700080731         exec sql close pdoa;
180800080731       //?calcolo quanti sono ancora da confermare
180900080731         totconfr = contapdo - contapro;
181000080731
181100080731       //?CONSEGNE --> FIPDC leggo quello che ho preparato
181200080731       //?                   per distinta e AUT
181300080731       //?         --> FIPCT leggo le conferme ricevute
181400080731       //?                 per distinta - AUT - bolla a parità di idrow
181500080731         exec sql
181600080731          declare pdca cursor for select fipdc00t.*
181700110513          from fipdc00t where pdcfgs = :dstfgs and
181800080922          pdcndc = :dstnfv and pdccodaut = :wpdr
181900080731          order by pdcfgs, pdcndc, pdccodaut, pdcaas, pdclnp, pdcnrs, pdcnsp;
182000080731         exec sql open pdca;
182100080731       //?leggo il file dei dati preparati
182200110701         dow not $finepdc;
182300080813          exec sql fetch next from pdca into: fipdcds;
182400080731       //?fine file o errore sql esco
182500080731          if sqlcod = 100 or sqlcod < 0;
182600110701           $finepdc = *on;
182700080731           leave;
182800080731          endif;
182900080731       //?per ogni distinta/aut/bolla conto le bolle preparate
183000080731          contapdc = contapdc + 1;
183100080731       //?cerco gli ok di conferma a parità di idrow
183200080731          exec sql
183300080731           select fipct00f.* into :fipctds from fipct00f
183400080731           where pctfgs = :pdcfgs and pctndc = :pdcndc and
183500080912           pctcodaut = :pdccodaut and
183600080731           pctaas = :pdcaas and
183700080731           pctlnp = :pdclnp and pctnrs = :pdcnrs and
183800080731           pctnsp = :pdcnsp and pcttrd = 'COK' and
183900080731           substr(pctdati, 3, 2) = :pdcidrow;
184000080731       //?trovo il rcd corrispondente
184100080731          if  sqlcod = 0;
184200080731       //?come confermato
184300080731           contapct = contapct + 1;
184400080731       //?controllo se ha richiesta info
184500110513          if pctATB = ' ';
184600080731           clear fidg45ds;
184700080902           c45fgs = pctfgs;
184800080902           c45ndc = pctndc;
184900080902           c45codaut = pctcodaut;
185000080731           c45aas = pctaas;
185100080731           c45lnp = pctlnp;
185200080731           c45nrs = pctnrs;
185300080731           c45nsp = pctnsp;
185400080731           kpjbu = fidg45ds;
185500080731           fidg45r(kpjba);
185600080731           fidg45ds = kpjbu;
185700080731           if c45davisio <> '';
185800080731            $infoc = *on;
185900080731           endif;
186000080731          endif;
186100110513          endif;
186200080912       //?imposto il codice AUT
186300080731         enddo;
186400080731       //?chiuso cursore
186500080731         exec sql close pdca;
186600080731       //?calcolo quanti sono ancora da confermare
186700080731         totconfc = contapdc - contapct;
186800080731
186900080731       //?se ho almeno un ritiro o una consegna devo scrivere il subfile
187000080731         if totconfr <> *zeros or totconfc <> *zeros;
187100110616       //?calcolo quante esitate
187200110616          exsr sr_dstesiti;
187300080731          exsr sr_dstaut;
187400080731          *in03 = *on;
187500080813          v2sin03 = '1';
187600080731          clear totscar;
187700080731          clear totscac;
187800080731          $cus = *off;
187900080813       //?imposto che non ho ritiri e consegne da preparare
188000080813          $noc = *on;
188100080813          $nor = *on;
188200110616          exsr sr_cars02;
188300080731         endif;
188400080731
188500080731       endsr;
188600080801
188700080801       // ----------------------------------------------------------------------
188800080801       //?Carico il subfile distinte
188900080801       // ----------------------------------------------------------------------
189000080801       begsr sr_cars02;
189100080804
189200080804         $recnook = *off;
189300080801
189400080801         clear v2sopz;
189500080801       //?distinta - AUT
189600080801         v2sfgs = dstfgs;
189700110617         v1sfgs = dstfgs;
189800080801         v2saut = wpdr;
189900110617         v1saut = wpdr;
190000080801         v2sdaut = apdrsc;
190100110617         v1sdaut = apdrsc;
190200080801         v2sndc = dstnfv;
190300110617         v1sndc = dstnfv;
190400080801       //?distinta a PDA
190500080801         v2spda = dstpda;
190600110617         v1spda = dstpda;
190700080801       //?distinta in test
190800080801         v2stst = %xlate(up:lo:§dsttstpda);
190900110617         v1stst = %xlate(up:lo:§dsttstpda);
191000080923       //?assegnati in distinta
191100080923         v2sassc = contac;
191200080923         v2sassr = contar;
191300110617         v1sassc = contac;
191400110617         v1sassr = contar;
191500080801       //?dati da preparare
191600080801         v2sscac = totscac;
191700080801         v2sscar = totscar;
191800080801       //?dati da confermare
191900080801         v2sconfc = totconfc;
192000080801         v2sconfr = totconfr;
192100080801       //?ok ad uscire
192200080801         clear v2scus;
192300080801         if $cus;
192400080801          v2scus = 'SI';
192500080801         endif;
192600080801       //?INFO
192700080801         clear v2sinfoc;
192800080801         if $infoc;
192900080801         v2sinfoc = 'SI';
193000080801         endif;
193100080801         clear v2sinfor;
193200080801         if $infor;
193300080801         v2sinfor = 'SI';
193400080801         endif;
193500080801       //?evidenzio se non ho nessun ORM o bolla per la distinta
193600080804         *in01 = ($noc = *on);
193700080804         *in02 = ($nor = *on);
193800080813         if *in01;
193900080813          v2sin01 = '1';
194000110617          v1sin01 = '1';
194100080813         else;
194200080813          v2sin01 = '0';
194300110617          v1sin01 = '0';
194400080813         endif;
194500080813         if *in02;
194600080813          v2sin02 = '1';
194700110617          v1sin02 = '1';
194800080813         else;
194900080813          v2sin02 = '0';
195000110617          v1sin02 = '0';
195100080813         endif;
195200080804
195300110617         if *in60;
195400110617          v1sin60 = '1';
195500110617         else;
195600110617          v1sin60 = '0';
195700110617         endif;
195800110617
195900110617         if *in61;
196000110617          v1sin61 = '1';
196100110617         else;
196200110617          v1sin61 = '0';
196300110617         endif;
196400110617
196500080804       //?controllo le parzializzazioni
196600080916         exsr sr_parzializza;
196700080916         if $recnook;
196800080804          leavesr;
196900080804         endif;
197000110616       //?controllo ultima ora ricezione
197100110616          exsr sr_dstora;
197200080801
197300080801       //?se ordinamento per AUT ed è stato immesso qualcosa nel posizionamento
197400080801       //?carico da lì in poi
197500080801         if  *in11 or
197600080801            (*in12 and v2cdaut <= v2sdaut);
197700080801
197800080801          nrr = nrr + 1;
197900080801          write lvp0s02;
198000110617          write lvp0s01;
198100110620         endif;
198200080801
198300080801       endsr;
198400080804
198500080804       // ----------------------------------------------------------------------
198600080804       //?Controllo le parzializzazioni richieste in prima videata
198700080804       // ----------------------------------------------------------------------
198800080804       begsr sr_parzializza;
198900080916
199000080916       //?solo distinte con dati non confermati
199100080916         if v1ok <> *blanks and v2sconfc = 0 and v2sconfr = 0;
199200080916          $recnook = *on;
199300080916          leavesr;
199400080916         endif;
199500080916
199600080916       //?solo distinte con richiesta assistenza
199700101126         if v1info <> *blanks and v2sinfoc = '  ' and v2sinfor = '  ';
199800080916          $recnook = *on;
199900080916          leavesr;
200000080916         endif;
200100080916
200200080916       //?solo distinte con dati ancora da preparare
200300080916         if v1sca <> *blanks and v2sscac = 0 and v2sscar = 0;
200400080916          $recnook = *on;
200500080916          leavesr;
200600080916         endif;
200700080916
200800080916       //?ok ad uscire
200900080916         if v1cus = 'S' and v2scus <> 'SI';
201000080916          $recnook = *on;
201100080916          leavesr;
201200080916         endif;
201300080916         if v1cus = 'N' and v2scus = 'SI';
201400080916          $recnook = *on;
201500080916          leavesr;
201600080916         endif;
201700110616       //?% consegne
201800110628         if v1perc <> 0;
201900110707         if v1segc = '>' and v1spec < v1perc;
202000110616          $recnook = *on;
202100110616          leavesr;
202200110616         endif;
202300110707         if v1segc = '<' and v1spec > v1perc;
202400110628          $recnook = *on;
202500110628          leavesr;
202600110628         endif;
202700110628         endif;
202800110616       //?% ritiri
202900110628         if v1perr <> 0;
203000110707         if v1segr ='>' and v1sper < v1perr;
203100110616          $recnook = *on;
203200110616          leavesr;
203300110616         endif;
203400110707         if v1segr ='<' and v1sper > v1perr;
203500110628          $recnook = *on;
203600110628          leavesr;
203700110628         endif;
203800110628         endif;
203900080804
204000080804       endsr;
204100080801
204200080801       // ----------------------------------------------------------------------
204300080801       //?Controllo la testata del subfile
204400080801       // ----------------------------------------------------------------------
204500080801       begsr sr_ctrc02;
204600080801
204700080801       //?se ordinamento per AUT salvo il campo immesso
204800080801         if *in12 and v2cdaut <> *blanks;
204900080801          exsr sr_carica;
205000080801          exsr sr_ordina2;
205100110617          exsr sr_ordinaA2;
205200080917          clear v2cdaut;
205300080801         endif;
205400080801
205500080801       endsr;
205600080806
205700080806       // ----------------------------------------------------------------------
205800110616       //?Controllo opzione subfile
205900080806       // ----------------------------------------------------------------------
206000110616       begsr sr_ctrs02;
206100080806
206200080806       //?controllo l'opzione immessa
206300080806         for xx = 1 to rrnlast;
206400110616         if $video = 'S02';
206500080806          chain xx lvp0s02;
206600110616         endif;
206700110616         if $video = 'S01';
206800110616          chain xx lvp0s01;
206900110616         endif;
207000080806       //?visualizza dettaglio
207100080806          if v2sopz = '5';
207200080806           recsf2 = xx;
207300080806           exsr sr_visdettaglio;
207400110617           dstfgs = v2sfgs;
207500110617           dstnfv = v2sndc;
207600110617           dstpdr = v2saut;
207700110617           exsr sr_dstconf;
207800110617       //?devo aggiornare l'eventuale info
207900110617         clear v2sinfoc;
208000110617         if $infoc;
208100110617         v2sinfoc = 'SI';
208200110617         endif;
208300110617         clear v2sinfor;
208400110617         if $infor;
208500110617         v2sinfor = 'SI';
208600110617         endif;
208700080806           clear v2sopz;
208800080806           endif;
208900080923       //?visualizza fasi distinta
209000080923          if v2sopz = 'F';
209100080923           recsf2 = xx;
209200080923           exsr sr_visfasi;
209300080923           clear v2sopz;
209400080923           endif;
209500110616       //?visualizza Esiti
209600110616          if v2sopz = '6';
209700110616           recsf2 = xx;
209800110616           exsr sr_visesi;
209900110616           clear v2sopz;
210000110616           endif;
210100140520       //?Preparazione ORM
210200140724          if v2sopz = 'P' and v2sscar > 0;
210300140527            recsf2 = xx;
210400140527            exsr sr_prepar;
210500140527            $ricar = *on ;
210600140527            clear v2sopz;
210700140527           else ;
210800140527            if v2sopz = 'P';
210900140527             clear v2sopz;
211000140527            endif;
211100140527          endif;
211200080813       //?reimposto gli indicatori di visualizzazione
211300080813          *in01 = (v2sin01 = '1');
211400080813          *in02 = (v2sin02 = '1');
211500080813          *in03 = (v2sin03 = '1');
211600110617          *in01 = (v1sin01 = '1');
211700110617          *in02 = (v1sin02 = '1');
211800110617          *in03 = (v1sin03 = '1');
211900110617          *in60 = (v1sin60 = '1');
212000110617          *in61 = (v1sin61 = '1');
212100110616         if $video = 'S01';
212200110616          update lvp0s01;
212300110616         endif;
212400110616         if $video = 'S02';
212500110616          update lvp0s02;
212600110616         endif;
212700080806         endfor;
212800080806
212900140521         if $ricar ;
213000140521          $ricar = *off ;
213100140521           exsr sr_carica;
213200140521         endif;
213300080806       endsr;
213400080806
213500080806       // ----------------------------------------------------------------------
213600080806       //?Visualizzo dettaglio distinta
213700080806       // ----------------------------------------------------------------------
213800080806       begsr sr_visdettaglio;
213900080806
214000080806         clear fnlvp0ds;
214100080806         clear kpjbu;
214200110627         if  $video = 'S02';
214300080806         lvp0fgs = v2sfgs;
214400080806         lvp0ndc = v2sndc;
214500080806         lvp0aut = v2saut;
214600110627         else;
214700110627         lvp0fgs = v1sfgs;
214800110627         lvp0ndc = v1sndc;
214900110627         lvp0aut = v1saut;
215000110627         endif;
215100080923         fnlvp0r1(kpjba:fnlvp0ds);
215200080806
215300080806       endsr;
215400080923
215500080923       // ----------------------------------------------------------------------
215600080923       //?Visualizzo fasi distinta
215700080923       // ----------------------------------------------------------------------
215800080923       begsr sr_visfasi;
215900080923
216000080923         write frcdta;
216100080923
216200080923         clear fidg39ds;
216300080923         clear kpjbu;
216400080923         fdg39npg = 4;
216500110627         fdg39norec = 'N';
216600110627         if  $video = 'S02';
216700080923         fdg39nfv = v2sndc;
216800080923         fdg39fgs = v2sfgs;
216900110627         else;
217000110627         fdg39nfv = v1sndc;
217100110627         fdg39fgs = v1sfgs;
217200110627         endif;
217300080923         kpjbu = fidg39ds;
217400080923         fidg39r(kpjba);
217500080923
217600080923       endsr;
217700080801
217800110616       // ----------------------------------------------------------------------
217900110616       //?Visualizzo Esiti
218000110616       // ----------------------------------------------------------------------
218100110616       begsr sr_visesi;
218200110616
218300110627         clear fnlvp0ds;
218400110627         clear kpjbu;
218500110627         if  $video = 'S02';
218600110627         lvp0fgs = v2sfgs;
218700110627         lvp0ndc = v2sndc;
218800110627         lvp0aut = v2saut;
218900110627         else;
219000110627         lvp0fgs = v1sfgs;
219100110627         lvp0ndc = v1sndc;
219200110627         lvp0aut = v1saut;
219300110627         endif;
219400110629         fnlvp0r2(kpjba:fnlvp0ds);
219500110616
219600110616       endsr;
219700110616
219800140520       // ----------------------------------------------------------------------
219900140520       //?lancia preparazione orm
220000140520       // ----------------------------------------------------------------------
220100140520       begsr sr_prepar;
220200140520
220300140520         clear fior56ds;
220400140520         clear kpjbu;
220500140520         or56fgs = v2sfgs;
220600140521         or56ddc = wdata ;
220700140520         or56ndcd= v2sndc;
220800140520         or56ndca= v2sndc;
220900140521         or56tla = 'T'   ;
221000140521         or56cmd = 'I'   ;
221100140520         kpjbu   = fior56ds;
221200140521         fior56r(kpjba:fior56ds);
221300140520
221400140520       endsr;
221500140520
221600080801       // ----------------------------------------------------------------------
221700080801       //?Ordino il subfile per distinta
221800080801       // ----------------------------------------------------------------------
221900080801       begsr sr_ordina1;
222000080801
222100080801        recsf2 = 1;
222200080801
222300080801       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
222400080801       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
222500080801       // il campo è usato per selezionare i records dando un ordine a quelli
222600080801       // selezionati davanti ai non selezionati.
222700080801         clear QLGSCB;
222800080801         clear QLGSCB00;
222900080801
223000080801       // 1 campi chiave x Distinta
223100080801         QLGNBRK = 1;
223200080801
223300080801       // imposto la posizione della distinta sul subfile e la lunghezza
223400080801       // l'ordinamento è su un campo numerico e deve essere
223500080801       // ascending
223600110621         QLGSP = 1 + %size(v2sdaut);
223700080801         QLGSS = %SIZE(v2sndc);
223800080801         QLGDT = Numerico;
223900080801         QLGSO = Ascendente;
224000080801         QLGKL(1) = QLGSKL;
224100080801
224200080801       // Load other sort parameters.
224300080801         QLGLB = 80 + 16 * MaxKey;
224400080801         QLGRL = %SIZE(sflrcd) - 1;
224500080801         QLGRT = 8;
224600080801         QLGOKL = 80;
224700080801         QLGLKE = 16;
224800080801         QLGLSS = 290;
224900080801
225000080801       // Initialize Sort I/O API fields.
225100080801         QLGRL00 = QLGRL;
225200080801         QLGRC00 = 1;
225300080801         clear QUSEI;
225400080801         QUSBPRV = %SIZE(QUSEC);
225500080801
225600080801       // First step - Initialize the sort routine.
225700080801         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
225800080801
225900080801       // Next step - Write records to I/O routine.
226000080801         QLGRT00 = Put;
226100080801         for xx = 1 to rrnlast;
226200110616          chain xx lvp0s02;
226300080801
226400080801       // solo le righe con Selected = 'Y' sono riordinate,
226500080801       // quindi per fare un ordinamento di tutte le righe
226600080801       // metto 'Y' sempre.
226700080801          selected  = 'Y';
226800080801          clear QUSEI;
226900080801          QUSBPRV = %SIZE(QUSEC);
227000080801          QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
227100080801         endfor;
227200080801
227300080801       // Next step - Signal end of input, clear subfile for reload.
227400080801         QLGRT00 = EndPut;
227500080801         clear QUSEI;
227600080801         QUSBPRV = %SIZE(QUSEC);
227700080801         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
227800080801       // pulizia SFL
227900110620         clear nrr;
228000080912         clear v2cdaut;
228100110616         *in20 = *off;
228200110616         *in21 = *off;
228300110616         write lvp0c02;
228400110616         *in20 = *on;
228500110616         *in21 = *on;
228600080801
228700080801       // Final step - Write the records back to the subfile.
228800080801         QLGRT00 = Get;
228900080801         for xx = 1 to rrnlast;
229000080801          clear QUSEI;
229100080801          QUSBPRV = %SIZE(QUSEC);
229200080801          QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
229300080801          nrr = xx;
229400080813       //?reimposto gli indicatori di visualizzazione
229500080813          *in01 = (v2sin01 = '1');
229600080813          *in02 = (v2sin02 = '1');
229700080813          *in03 = (v2sin03 = '1');
229800110616          write lvp0s02;
229900080801         endfor;
230000080801
230100080801       endsr;
230200080801
230300080801       // ----------------------------------------------------------------------
230400110621       //?Ordino il subfile per decodifica aut
230500080801       // ----------------------------------------------------------------------
230600080801       begsr sr_ordina2;
230700080801
230800080801        recsf2 = 1;
230900080801
231000080801       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
231100080801       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
231200080801       // il campo è usato per selezionare i records dando un ordine a quelli
231300080801       // selezionati davanti ai non selezionati.
231400080801         clear QLGSCB;
231500080801         clear QLGSCB00;
231600080801
231700110621       // 2 campi chiave x decodifica/Distinta
231800080801         QLGNBRK = 2;
231900080801
232000080801       // imposto la posizione dell'AUT sul subfile e la lunghezza
232100080801       // l'ordinamento è su un campo numerico e deve essere
232200080801       // ascending
232300110621         QLGSP = 1 ;
232400080801         QLGSS = %SIZE(v2sdaut);
232500080801         QLGDT = Carattere;
232600080801         QLGSO = Ascendente;
232700080801         QLGKL(1) = QLGSKL;
232800080801
232900080801       // imposto la posizione della distinta sul subfile e la lunghezza
233000080801       // l'ordinamento è su un campo numerico e deve essere
233100080801       // ascending
233200110621         QLGSP = 1 + %size(v2sdaut);
233300080801         QLGSS = %SIZE(v2sndc);
233400080801         QLGDT = Numerico;
233500080801         QLGSO = Ascendente;
233600080801         QLGKL(2) = QLGSKL;
233700080801
233800080801       // Load other sort parameters.
233900080801         QLGLB = 80 + 16 * MaxKey;
234000080801         QLGRL = %SIZE(sflrcd) - 1;
234100080801         QLGRT = 8;
234200080801         QLGOKL = 80;
234300080801         QLGLKE = 16;
234400080801         QLGLSS = 290;
234500080801
234600080801       // Initialize Sort I/O API fields.
234700080801         QLGRL00 = QLGRL;
234800080801         QLGRC00 = 1;
234900080801         clear QUSEI;
235000080801         QUSBPRV = %SIZE(QUSEC);
235100080801
235200080801       // First step - Initialize the sort routine.
235300080801         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
235400080801
235500080801       // Next step - Write records to I/O routine.
235600080801         QLGRT00 = Put;
235700080801         for xx = 1 to rrnlast;
235800110616          chain xx lvp0s02;
235900080801
236000080801       // solo le righe con Selected = 'Y' sono riordinate,
236100080801       // quindi per fare un ordinamento di tutte le righe
236200080801       // metto 'Y' sempre.
236300080801          selected  = 'Y';
236400080801          clear QUSEI;
236500080801          QUSBPRV = %SIZE(QUSEC);
236600080801          QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
236700080801         endfor;
236800080801
236900080801       // Next step - Signal end of input, clear subfile for reload.
237000080801         QLGRT00 = EndPut;
237100080801         clear QUSEI;
237200080801         QUSBPRV = %SIZE(QUSEC);
237300080801         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
237400080801       // pulizia SFL
237500080801         clear nrr;
237600110616         *in20 = *off;
237700110616         *in21 = *off;
237800110616         write lvp0c02;
237900110616         *in20 = *on;
238000110616         *in21 = *on;
238100080801
238200080801       // Final step - Write the records back to the subfile.
238300080801         QLGRT00 = Get;
238400080801         for xx = 1 to rrnlast;
238500080801          clear QUSEI;
238600080801          QUSBPRV = %SIZE(QUSEC);
238700080801          QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
238800110620          nrr = xx;
238900080813       //?reimposto gli indicatori di visualizzazione
239000080813          *in01 = (v2sin01 = '1');
239100080813          *in02 = (v2sin02 = '1');
239200080813          *in03 = (v2sin03 = '1');
239300110616          write lvp0s02;
239400080801         endfor;
239500080801
239600080801       endsr;
239700110617       // ----------------------------------------------------------------------
239800110617       //?Ordino il subfile attività  X distinte
239900110617       // ----------------------------------------------------------------------
240000110617       begsr sr_ordinaA1;
240100110617
240200110617        recsf2 = 1;
240300110617
240400110617       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
240500110617       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
240600110617       // il campo è usato per selezionare i records dando un ordine a quelli
240700110617       // selezionati davanti ai non selezionati.
240800110617         clear QLGSCB;
240900110617         clear QLGSCB00;
241000110617
241100110617       // 1 campi chiave x Distinta
241200110617         QLGNBRK = 1;
241300110617
241400110617       // imposto la posizione della distinta sul subfile e la lunghezza
241500110617       // l'ordinamento è su un campo numerico e deve essere
241600110617       // ascending
241700110621         QLGSP = 1 + %size(v1sdaut);
241800110617         QLGSS = %SIZE(v1sndc);
241900110617         QLGDT = Numerico;
242000110617         QLGSO = Ascendente;
242100110617         QLGKL(1) = QLGSKL;
242200110617
242300110617       // Load other sort parameters.
242400110617         QLGLB = 80 + 16 * MaxKey;
242500110621         QLGRL = %SIZE(sflrcd) - 1;
242600110617         QLGRT = 8;
242700110617         QLGOKL = 80;
242800110617         QLGLKE = 16;
242900110617         QLGLSS = 290;
243000110617
243100110617       // Initialize Sort I/O API fields.
243200110617         QLGRL00 = QLGRL;
243300110617         QLGRC00 = 1;
243400110617         clear QUSEI;
243500110617         QUSBPRV = %SIZE(QUSEC);
243600110617
243700110617       // First step - Initialize the sort routine.
243800110617         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
243900110617
244000110617       // Next step - Write records to I/O routine.
244100110617         QLGRT00 = Put;
244200110617         for xx = 1 to rrnlast;
244300110617          chain xx lvp0s01;
244400110617
244500110617       // solo le righe con Selected = 'Y' sono riordinate,
244600110617       // quindi per fare un ordinamento di tutte le righe
244700110617       // metto 'Y' sempre.
244800110621          selected = 'Y';
244900110617          clear QUSEI;
245000110617          QUSBPRV = %SIZE(QUSEC);
245100110621          QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
245200110617         endfor;
245300110617
245400110617       // Next step - Signal end of input, clear subfile for reload.
245500110617         QLGRT00 = EndPut;
245600110617         clear QUSEI;
245700110617         QUSBPRV = %SIZE(QUSEC);
245800110621         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
245900110617       // pulizia SFL
246000110617         clear nrr;
246100110617         clear v2cdaut;
246200110617         *in85 = *off;
246300110617         *in86 = *off;
246400110617         write lvp0c01;
246500110617         *in85 = *on;
246600110617         *in86 = *on;
246700110617
246800110617       // Final step - Write the records back to the subfile.
246900110617         QLGRT00 = Get;
247000110617         for xx = 1 to rrnlast;
247100110617          clear QUSEI;
247200110617          QUSBPRV = %SIZE(QUSEC);
247300110621          QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
247400110617          nrr = xx;
247500110617       //?reimposto gli indicatori di visualizzazione
247600110617          *in01 = (v1sin01 = '1');
247700110617          *in02 = (v1sin02 = '1');
247800110617          *in03 = (v1sin03 = '1');
247900110617          *in60 = (v1sin60 = '1');
248000110617          *in61 = (v1sin61 = '1');
248100110617          write lvp0s01;
248200110617         endfor;
248300110617
248400110617       endsr;
248500110617
248600110617       // ----------------------------------------------------------------------
248700110621       //?Ordino il subfile attività per decodifica aut
248800110617       // ----------------------------------------------------------------------
248900110617       begsr sr_ordinaA2;
249000110617
249100110617        recsf2 = 1;
249200110617
249300110617       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
249400110617       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
249500110617       // il campo è usato per selezionare i records dando un ordine a quelli
249600110617       // selezionati davanti ai non selezionati.
249700110617         clear QLGSCB;
249800110617         clear QLGSCB00;
249900110617
250000110621       // 2 campi chiave x decodifica/Distinta
250100110617         QLGNBRK = 2;
250200110617
250300110617       // imposto la posizione dell'AUT sul subfile e la lunghezza
250400110617       // l'ordinamento è su un campo numerico e deve essere
250500110617       // ascending
250600110621         QLGSP = 1;
250700110617         QLGSS = %SIZE(v1sdaut);
250800110617         QLGDT = Carattere;
250900110617         QLGSO = Ascendente;
251000110617         QLGKL(1) = QLGSKL;
251100110617
251200110617       // imposto la posizione della distinta sul subfile e la lunghezza
251300110617       // l'ordinamento è su un campo numerico e deve essere
251400110617       // ascending
251500110621         QLGSP = 1 + %size(v1sdaut);
251600110617         QLGSS = %SIZE(v1sndc);
251700110617         QLGDT = Numerico;
251800110617         QLGSO = Ascendente;
251900110617         QLGKL(2) = QLGSKL;
252000110617
252100110617       // Load other sort parameters.
252200110617         QLGLB = 80 + 16 * MaxKey;
252300110621         QLGRL = %SIZE(sflrcd) - 1;
252400110617         QLGRT = 8;
252500110617         QLGOKL = 80;
252600110617         QLGLKE = 16;
252700110617         QLGLSS = 290;
252800110617
252900110617       // Initialize Sort I/O API fields.
253000110617         QLGRL00 = QLGRL;
253100110617         QLGRC00 = 1;
253200110617         clear QUSEI;
253300110617         QUSBPRV = %SIZE(QUSEC);
253400110617
253500110617       // First step - Initialize the sort routine.
253600110617         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
253700110617
253800110617       // Next step - Write records to I/O routine.
253900110617         QLGRT00 = Put;
254000110617         for xx = 1 to rrnlast;
254100110617          chain xx lvp0s01;
254200110617
254300110617       // solo le righe con Selected = 'Y' sono riordinate,
254400110617       // quindi per fare un ordinamento di tutte le righe
254500110617       // metto 'Y' sempre.
254600110621          selected = 'Y';
254700110617          clear QUSEI;
254800110617          QUSBPRV = %SIZE(QUSEC);
254900110621          QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
255000110617         endfor;
255100110617
255200110617       // Next step - Signal end of input, clear subfile for reload.
255300110617         QLGRT00 = EndPut;
255400110617         clear QUSEI;
255500110617         QUSBPRV = %SIZE(QUSEC);
255600110621         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
255700110617       // pulizia SFL
255800110617         clear nrr;
255900110617       // clear v2cdaut;
256000110617         *in85 = *off;
256100110617         *in86 = *off;
256200110617         write lvp0c01;
256300110617         *in85 = *on;
256400110617         *in86 = *on;
256500110617
256600110617       // Final step - Write the records back to the subfile.
256700110617         QLGRT00 = Get;
256800110617         for xx = 1 to rrnlast;
256900110617          clear QUSEI;
257000110617          QUSBPRV = %SIZE(QUSEC);
257100110621          QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
257200110617          nrr = xx;
257300110617       //?reimposto gli indicatori di visualizzazione
257400110617          *in01 = (v1sin01 = '1');
257500110617          *in02 = (v1sin02 = '1');
257600110617          *in03 = (v1sin03 = '1');
257700110617          *in60 = (v1sin60 = '1');
257800110617          *in61 = (v1sin61 = '1');
257900110617          write lvp0s01;
258000110617         endfor;
258100110617
258200110617       endsr;
258300080725
258400080731       // ----------------------------------------------------------------------
258500080731       //?Fine programma
258600080731       // ----------------------------------------------------------------------
258700080725       begsr sr_fine;
258800080725
258900080725         *inlr = *on;
259000080725         return;
259100080725
259200080725       endsr;
259300070214
259400070214** MSG  Lungh. 78                                                            *
259500080725PDA non attivo per la filiale in gestione                                     01
259600080725Filiale in gestione errata                                                    02
259700080725Data errata                                                                   03
259800080725Non trovate distinte nella data indicata                                      04
259900080728Numero distinta errato: contiene caratteri NON numerici                       05
260000080728Numero distinta inesistente o annullato                                       06
260100080728Distinta già chiusa                                                           07
260200080728Distinta senza dati a PDA                                                     08
260300080728Numero distinta obbligatorio                                                  09
260400080728Limiti incompatibili: numero distinta al minore del numero distinta dal       10
260500080730Autotrasportatore errato: contine caratteri NON numerici                      11
260600080730Autotrasportatore inesistente o annullato                                     12
260700080730Autotrasportatore non in gestione all'utente                                  13
260800080730La selezione è possibile per Distinta o per AUT non entrambe!!!!              14
260900080916E' possibile selezionare solo una parzializzazione                            15
261000160511Il valore immesso per il campo non è valido.                                  16
261100160511Se scelto Tipo DIST.PARCEL occorre inserire almeno un Turno!!!!               17
