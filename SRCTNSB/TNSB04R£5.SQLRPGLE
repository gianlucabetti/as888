000100090324     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200090324      // ----------------------------------------------------------------------
000300090324      //
000400090326      //         Pod Image su CD  ?
000500090324      //
000600090324      // ----------------------------------------------------------------------
000700090324      // ? DICHIARAZIONE DEI FILE ?
000800090324      // ----------------------------------------------------------------------
000801171229     fTILAC05L  if   e           k disk
000900090324
001000090326     ftnsb04d   cf   e             workstn sfile(sb04s02:nrr2)
001100090326     f                                     sfile(sb04s03:nrr3)
001200090324
001300090324      // ----------------------------------------------------------------------
001400090324      // ? RIEPILOGO INDICATORI ?
001500090324      // ----------------------------------------------------------------------
001600090324      // 20 - gestione subfile
001700090324      // 21 - gestione subfile
001800090327      // 22 - gestione subfile
001900090327      // 23 - gestione subfile
002000090324      // 28 - errore generico
002100090324      // 30 - comodo
002200090324      // 31 - fine file per subfile
002300090327      // 32 - fine file per subfile
002400090326      // 41 - data dal
002500090326      // 42 - data  al
002600090326      // 43 - codice padre
002700090330      // 44 - parzializzazioni
002800090330      // 45 - parzializzazioni
002900090330      // 46 - parzializzazioni
003000090327      // 50 - HI Elaborate SF02
003100090327      // 51 - HI Tas SF02
003200090327      // 52 - HI Elaborate SF03
003300090327      // 53 - HI Tas SF03
003400090330      // 54 - RED codice + rag.sociale SF02
003500090330      // 55 - RED codice + rag.sociale SF03
003600110303      // 56 - riga di TOTALE SF03
003700090324      // 90 - riemetto videata
003800090324      // ----------------------------------------------------------------------
003900090324
004000090324      // ? V A R I A B I L I ?
004100090324     d dataiso         s               d   datfmt(*iso)
004200090324     d dataeur         s               d   datfmt(*eur)
004300090326     d nrr2            s                   like(recsf2)
004400090326     d nrr3            s                   like(recsf3)
004500141201     d P01opz3         s              1    inz('R')
004600090324     d rrnlast         s                   like(recsf2)
004700090327     d s02dir          s                   like(lacdir)
004800110714     d s02ela          s                   like(lacela)
004900090327     d s02idjob        s                   like(lacidjob)
005000090327     d s02ksu          s                   like(lacksu)
005100090402     d s02nela         s             10i 0
005200090402     d s02nerr         s             10i 0
005300090402     d s02nest         s             10i 0
005400090402     d s02ntas         s             10i 0
005500090402     d s02tad          s                   like(lactad)
005600090327     d s02tim          s                   like(lactim)
005700090327     d s02utecd        s                   like(lacutecd)
005800090327     d s03dir          s                   like(lacdir)
005900090327     d s03idjob        s                   like(lacidjob)
006000090327     d s03ksc          s                   like(lacksc)
006100090327     d s03ksu          s                   like(lacksu)
006200090402     d s03nela         s             10i 0
006300090402     d s03nerr         s             10i 0
006400090402     d s03nest         s             10i 0
006500090402     d s03ntas         s             10i 0
006600090402     d s03tad          s                   like(lactad)
006700090327     d s03tim          s                   like(lactim)
006800110303     d T03nela         s             10i 0
006900110303     d T03nest         s             10i 0
007000090324     d wdata           s              8  0
007100090326     d wdatada         s              8  0
007200090326     d wdataal         s              8  0
007300090327     d wtimestp        s             14
007400090324     d xx              s              3  0
007500090327     d $ends02         s               n
007600090327     d $ends03         s               n
007700090330     d $endw02         s               n
007800110714     d $endw03         s               n
007900090324     d $fine           s               n
008000090330     d $okcd           s               n
008100110714     d $okfile         s               n
008200090327     d $recnook        s               n
008300090324     d $video          s             10
008301171229     d sav_LACnim      s                   like(LACnim) inz
008400090324
008500090324      // ? S C H I E R E ?
008600090324     d msg             s             78    dim(15) ctdata perrcd(1)
008700090324
008800090324      // ? D S   I N T E R N E / E S T E R N E ?
008900090324     d wlbdat          ds                  inz
009000090324     d  g02dat                 1      8  0
009100090324     d  g02inv                 9     16  0
009200090324     d  g02err                17     17
009300090324     d  g02tgi                18     22  0
009400090324
009500090324     d §azute        e ds                  extname(azute00f)
009600090324     d                                     dtaara
009700090324     d §datiute      e ds                  extname(ddatiute)
009800090324     d                                     dtaara
009900090324     d dlac          e ds
010000090324     d kpjba         e ds
010100090324     d tilacds       e ds                  extname(tilac00f)
010200090324     d tibs34ds      e ds                  inz
010300090324     d tntbeds       e ds                  extname(tntbe00f)
010400110721     d tnsb06ds      e ds
010500090327
010600090327      // - Interrogazione tabella LAC dettaglio
010700090327     d tntb46ds      e ds                  inz
010800090327     d   b46opz      e                     inz('5')
010900090327     d   b46err      e                     inz(*off)
011000090324
011100090324       // - Struttura per passaggio dati ad interrogazione tabella
011200090324     d param01         ds
011300090324     d  p01cod                        7  0 inz
011400090324     d  p01ord                        1
011500090324     d  p01ksu                        7  0 inz
011600090324     d  p01ke1                        7
011700090324     d  p01ke2                       15
011800090324     d  p01rit                        1
011900090324
012000090324     d                sds
012100090324     d  vtcpgm                 1     10
012200090324
012300090324      // ? COSTANTI ?
012400090324     d digitn          c                   const('1234567890')
012500090324
012600090324      // ? PROTOTIPI ?
012700090324      /copy gaitrasrc/srcprotopr,tibs34r
012800090324      /copy gaitrasrc/srcprotopr,xsrda8
012900090324
013000090327       // - Visualizzazione tabella LAC
013100090327     d tntb46r         pr                  extpgm('TNTB46R')
013200090327     d  kpjba                              likeds(KPJBA)
013300090327
013400090324       // - Interrogazione tabella LAC
013500090324     d tntb461r        pr                  extpgm('TNTB461R')
013600090324     d  kpjba                              likeds(KPJBA)
013700141201     d  P01opz3                       1a
013800110721
013900110721       // - Pgm creazione file di work per elenco Pod Image
014000110721     d tnsb06r         pr                  extpgm('TNSB06R')
014100110721     d  kpjba                              likeds(KPJBA)
014200110721     d  tnsb06ds                           likeds(TNSB06DS)
014300090324
014400090324      // ----------------------------------------------------------------------
014500090324
014600090324     c     *entry        plist
014700090324     c                   parm                    kpjba
014800090324
014900090324      /free
015000090324
015100090324       //?operazioni iniziali
015200090326          exsr routinz;
015300090326
015400090326       //?imposto i dati di dft nella prima videata
015500090326       //?per ora solo all'inizio del programma poi vediamo
015600090326          exsr inzd01;
015700090324
015800090326          dow not $fine;
015900090324
016000090326         //?gestione delle videate
016100090326            select;
016200090324
016300090326         //?gestione prima videata
016400090326             when $video = 'D01';
016500090326               exsr gesd01;
016600090324
016700090326         //?gestione subfile
016800090326             when $video = 'S02';
016900090326               exsr gess02;
017000090327
017100090327         //?gestione subfile
017200090327             when $video = 'S03';
017300090327               exsr gess03;
017400090327
017500090327         //?gestione visualizzazione tabella LAC
017600090327             when $video = 'LAC';
017700090327               exsr geslac;
017800090324
017900090326            endsl;
018000090326          enddo;
018100090324
018200090326          *inlr = *on;
018300090324
018400090324       // ----------------------------------------------------------------------
018500090324       //?Operazioni iniziali.
018600090324       // ----------------------------------------------------------------------
018700090324       begsr routinz;
018800090324
018900090324         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
019000090324
019100090324       //?recupero dei dati utente
019200090324          in(e) §azute;
019300090324          if not %error;
019400090326            in(e) §datiute;
019500090324          endif;
019600090324          if %error or rsut = *blanks;
019700090326            tibs34r(tibs34ds);
019800090326            in §azute;
019900090326            in §datiute;
020000090324          endif;
020100090324
020200090324       //?imposto la data del giorno
020300090324          wdata = %dec(%date());
020400090324          dataiso = %date(wdata:*iso);
020500090324          dataeur = dataiso;
020600090327
020700090327       //?imposto il time del job
020800090327          wtimestp = %char(%timestamp:*iso0);
020900090324
021000090324       //?gestione video D01
021100090326          $video = 'D01';
021200090326          $fine =*off;
021300090324
021400090324       endsr;
021500090326
021600090326       // ----------------------------------------------------------------------
021700090326       //?Imposto i dati di dflt nella prima videata.
021800090326       // ----------------------------------------------------------------------
021900090326        begsr inzd01;
022000090326
022100090326          clear v1datada;
022200090326          v1dataal = %dec(dataeur);
022300090330          v1cd = 'X';
022400090326
022500090326        endsr;
022600090324
022700090324       // ----------------------------------------------------------------------
022800090324       //?Gestione prima videata (parametri)
022900090324       // ----------------------------------------------------------------------
023000090326        begsr gesd01;
023100090324
023200090324       //?pulisco la videata
023300090324          if not *in28 and not *in90;
023400090330            exsr inzd01;
023500090326            exsr sr_puld01;
023600090324          endif;
023700090324
023800090324       //?emetto la videata
023900090324          write sb04t01;
024000090324          exfmt sb04d01;
024100090324
024200090324       //?spengo indicatori di comodo
024300090324          *in28 = *off;
024400090324          *in90 = *off;
024500090324       //?pulisco campo messaggi
024600090324          clear v1cmsg;
024700090324
024800090324       //?esecuzione comandi
024900090326          select;
025000090324
025100090326         //?f3=fine
025200090326            when *inkc;
025300090326              exsr sr_fine;
025400090326            other;
025500090324
025600090326           //?controllo i dati della videata
025700090326              exsr sr_contrd01;
025800090326              if *in28 or *in90;
025900090326                leavesr;
026000090326              endif;
026100090326          endsl;
026200090324
026300090326       //?passo al subfile
026400090326          exsr sr_s02;
026500090326          $video = 'S02';
026600090324
026700090326        endsr;
026800090324
026900090324       // ----------------------------------------------------------------------
027000090324       //?Gestione subfile
027100090324       // ----------------------------------------------------------------------
027200090326        begsr gess02;
027300090324
027400090324       //?ho caricato dati emetto il subfile
027500090326          if nrr2 > 0;
027600090326            exsr sr_sfl02;
027700090326          endif;
027800090324
027900090324       //?non ho caricato dati emetto videata di segnalazione
028000090326          if nrr2 = 0;
028100090326            exsr sr_d02;
028200090326          endif;
028300090324
028400090326        endsr;
028500090324
028600090324       // ----------------------------------------------------------------------
028700090324       //?Pulizia prima videata (parametri)
028800090324       // ----------------------------------------------------------------------
028900090326        begsr sr_puld01;
029000090324
029100090326          clear v1cksu;
029200090326          clear v1dksu;
029300090326          clear v1estr;
029400090326          clear v1elab;
029500090326          clear v1tas;
029600090324
029700090326        endsr;
029800090324
029900090324       // ----------------------------------------------------------------------
030000090324       //?Controllo dati prima videata (parametri)
030100090324       // ----------------------------------------------------------------------
030200090326        begsr sr_contrd01;
030300090324
030400090324       //?spengo indicatori di errore
030500090326          *in41 = *off;
030600090326          *in42 = *off;
030700090326          *in43 = *off;
030800090330          *in44 = *off;
030900090330          *in45 = *off;
031000090330          *in46 = *off;
031100090324
031200090324       //?pulisco campi di comodo
031300090326          clear wdatada;
031400090326          clear wdataal;
031500090324
031600090324       //?controllo data
031700090326          exsr sr_contrdata;
031800090326          if *in28;
031900090326            leavesr;
032000090326          endif;
032100090324
032200090326       //?controllo padre
032300090326          if v1cksu <> *blanks;
032400090326            exsr sr_contrksu;
032500090326            if *in28 or *in90;
032600090326              leavesr;
032700090326            endif;
032800090326          endif;
032900090330
033000090330       //?controllo parzializzazioni
033100090330          exsr sr_contrpar;
033200090330          if *in28;
033300090330            leavesr;
033400090330          endif;
033500090324
033600090326        endsr;
033700090324
033800090324       // ----------------------------------------------------------------------
033900090326       //?Subfile
034000090324       // ----------------------------------------------------------------------
034100090326        begsr sr_s02;
034200090324
034300090324       //?pulisco il subfile
034400090326          exsr sr_puls02;
034500090324
034600090327          $ends02 = *off;
034700090326
034800110714       //?leggo tilac00f solo rcd con lacela = '33' o '34' Job sottomesso
034900171009          SELECT;
035000090326       //?tutti i papà
035100171009       //?tutti i rcd
035200171009          WHEN v1cksu = *blanks and V1cd = *blanks;
035300090326            exec sql
035400090327              declare ksu cursor for
035500110714              select lacidjob, lactim, lacksu, lacutecd, lacdir, lactad,
035600110714                     lacela
035700110714              from tilac00f where lacela in('33', '34') and
035800090326              substr(lactim, 1, 8) between digits(:wdatada)
035900090326              and digits(:wdataal)
036000090326              order by lactim, lacksu, lacidjob;
036100090326
036200090326            exec sql
036300090327              open ksu;
036400090326              if sqlcode < 0;
036500090327                $ends02 = *on;
036600090326              ENDIF;
036700171009       //?tutti i papà
036800171009       //?solo cd ancora da fare
036900171009          WHEN  v1cksu = *blanks and V1cd <> *blanks;
037000171009            exec sql
037100171009              declare ksucd cursor for
037200171009              select lacidjob, lactim, lacksu, lacutecd, lacdir, lactad,
037300171009                     lacela
037400171009              from tilac00f where lacela in('33', '34') and
037500171009              substr(lactim, 1, 8) between digits(:wdatada)
037600171009              and digits(:wdataal) and lacutecd = ' '
037700171009              order by lactim, lacksu, lacidjob;
037800171009
037900171009            exec sql
038000171009              open ksucd;
038100171009              if sqlcode < 0;
038200171009                $ends02 = *on;
038300171009              ENDIF;
038400090326
038500090326       //?leggo tilac00f raggruppato per id job e codice estrazione
038600090326       //?un solo codice raggruppamento chiesto
038700171009          WHEN  v1cksu > *zeros;
038800090326            exec sql
038900090327              declare ksu1 cursor for
039000110714              select lacidjob, lactim, lacksu, lacutecd, lacdir, lactad,
039100110714                     lacela
039200110714              from tilac00f where lacela in('33', '34') and
039300090326              substr(lactim, 1, 8) between digits(:wdatada) and
039400090326              digits(:wdataal) and digits(lacksu) = :v1cksu
039500090326              order by lactim, lacksu, lacidjob;
039600090326
039700090326            exec sql
039800090327              open ksu1;
039900090326              if sqlcode < 0;
040000090327                $ends02 = *on;
040100090326              ENDIF;
040200171009          endSL;
040300090326
040400090324       //?carico il subfile
040500090326          exsr sr_cars02;
040600090324
040700090326        endsr;
040800090324
040900090324       // ----------------------------------------------------------------------
041000090326       //?Emetto il subfile
041100090324       // ----------------------------------------------------------------------
041200090326        begsr sr_sfl02;
041300090324
041400090330       //?flag creazione cd
041500090330          $okcd = *off;
041600110714       //?flag creazione file
041700110714          $okfile = *off;
041800090330
041900090324       //?emetto la videata
042000090326          write sb04t01;
042100090326          write sb04z02;
042200090326          exfmt sb04c02;
042300090324
042400090324       //?spengo indicatori di comodo
042500090326          *in28 = *off;
042600090324       //?pulisco campo messaggi
042700090326          clear v2cmsg;
042800090324       //?esecuzione comandi
042900090326          select;
043000090324
043100090326         //?f3=fine
043200090326            when *inkc;
043300110301              exsr sr_fine;
043400110301         //?f5=refresh
043500110301            when *inke;
043600110301              exsr sr_s02;
043700090326         //?f12=ritorno
043800090326            when *inkl;
043900090326              $video = 'D01';
044000090326              leavesr;
044100090326            other;
044200090326         //?controllo opzione subfile
044300090326            exsr sr_ctrs02;
044400090330         //?se creati CD ricarico il subfile
044500110714         //?anche se creato il file
044600110714            if $okcd or $okfile;
044700090330              exsr sr_s02;
044800090330            endif;
044900090326          endsl;
045000090324
045100090326        endsr;
045200090324
045300090324       // ----------------------------------------------------------------------
045400090324       //?Emetto video di segnalazione dati non caricati
045500090324       // ----------------------------------------------------------------------
045600090326        begsr sr_d02;
045700090324
045800090324       //?emetto la videata
045900090326          write sb04t01;
046000090326          exfmt sb04d02;
046100090324
046200090324       //?spengo indicatori di comodo
046300090326          *in28 = *off;
046400090326          *in90 = *off;
046500090324       //?pulisco campo messaggi
046600090326          clear v2cmsg;
046700090324       //?esecuzione comandi
046800090326          select;
046900090324
047000090326         //?f3=fine
047100090326            when *inkc;
047200090326              exsr sr_fine;
047300090326         //?f12=ritorno
047400090326            when *inkl;
047500090326              $video = 'D01';
047600090326              leavesr;
047700090326            other;
047800090326          endsl;
047900090324
048000090326        endsr;
048100090324
048200090324       // ----------------------------------------------------------------------
048300090324       //?Controllo data
048400090324       // ----------------------------------------------------------------------
048500090326        begsr sr_contrdata;
048600090324
048700090324       //?la data è obbligatoria
048800090326          if v1datada = *zeros and v1dataal = *zeros;
048900090326            *in28 = *on;
049000090326            *in41 = *on;
049100090326            v1cmsg = msg(01);
049200090326            leavesr;
049300090326          endif;
049400090324
049500090324       //?deve essere una data valida
049600090326       //?data da
049700090326          if v1datada <> *zeros;
049800090326            clear wlbdat;
049900090326            g02dat = v1datada;
050000090326            xsrda8(wlbdat);
050100090326            if g02err = '1';
050200090326              *in28 = *on;
050300090326              *in41 = *on;
050400090326              v1cmsg = msg(02);
050500090326              leavesr;
050600090326            endif;
050700090326            v1datada = g02dat;
050800090326            wdatada  = g02inv;
050900090326          endif;
051000090326       //?data al
051100090326          if v1dataal <> *zeros;
051200090326            clear wlbdat;
051300090326            g02dat = v1dataal;
051400090326            xsrda8(wlbdat);
051500090326            if g02err = '1';
051600090326              *in28 = *on;
051700090326              *in42 = *on;
051800090326              v1cmsg = msg(02);
051900090326              leavesr;
052000090326            endif;
052100090326            v1dataal = g02dat;
052200090326            wdataal  = g02inv;
052300090326          endif;
052400090324
052500090326       //?imposto in automatico la data mancante
052600090326          if v1dataal = *zeros;
052700090326            v1dataal = v1datada;
052800090326            wdataal  = wdatada;
052900090326          endif;
053000090326       //?controllo range date
053100110502          if wdatada > wdataal;
053200090326            *in28 = *on;
053300090326            *in41 = *on;
053400090326            v1cmsg = msg(03);
053500090326            leavesr;
053600090326          endif;
053700090324
053800090326        endsr;
053900090324
054000090324       // ----------------------------------------------------------------------
054100090326       //?Controllo padre
054200090324       // ----------------------------------------------------------------------
054300090326        begsr sr_contrksu;
054400090324
054500090326       //?Ricerca codice cliente
054600090326          if %scan('?':v1cksu) > *zeros;
054700090326            clear  param01;
054800090326            p01ord = '0';
054900090326            kpjbu = param01;
055000141201            tntb461r (kpjba:P01opz3);
055100090326            param01 = kpjbu;
055200090324
055300090326            if p01ke1 <> *zeros;
055400090326              v1cksu = p01ke1;
055500090326              *in43 = *on;
055600090326              *in90 = *on;
055700090326            endif;
055800090326          endif;
055900090324
056000090326       //?Controllo immissione solo caratteri numerici
056100090326          if %check(digitn:v1cksu) > *zeros;
056200090326            *in28 = *on;
056300090326            *in43 = *on;
056400090326            v1cmsg = msg(04);
056500090326            leavesr;
056600090326          endif;
056700090324
056800090326       //?Controllo esistenza codice cliente in tabella LAC
056900090326          exec sql
057000090326            select tbeuni into :tbeuni from tntbe00f
057100090326            where tbecod = 'LAC' and tbeatb = '' and
057200090326            substr(tbeke1, 1, 7) = :v1cksu;
057300090326       //?non trovo il rcd corrispondente
057400090326          if sqlcod <> 0;
057500090326            clear tbeuni;
057600090326            *in28 = *on;
057700090326            *in43 = *on;
057800090326            v1cmsg = msg(05);
057900090326            leavesr;
058000090326          endif;
058100090324
058200090326          dlac = tbeuni;
058300090326          v1dksu = §lacrag;
058400090324
058500090326        endsr;
058600090330
058700090330       // ----------------------------------------------------------------------
058800090330       //?Controllo parzializzazioni
058900090330       // ----------------------------------------------------------------------
059000090330        begsr sr_contrpar;
059100090330
059200090330       //?le parzializzazioni non sono in alternativa
059300090330          if v1cd <> *blanks and (v1estr <> *blanks or v1elab <> *blanks or
059400090330                                  v1tas <> *blanks);
059500090330            *in28 = *on;
059600090330            *in44 = *on;
059700090330            v1cmsg = msg(06);
059800090330            leavesr;
059900090330          endif;
060000090330          if v1estr <> *blanks and (v1elab <> *blanks or v1tas <> *blanks);
060100090330            *in28 = *on;
060200090330            *in45 = *on;
060300090330            v1cmsg = msg(06);
060400090330            leavesr;
060500090330          endif;
060600090330          if v1elab <> *blanks and v1tas <> *blanks;
060700090330            *in28 = *on;
060800090330            *in46 = *on;
060900090330            v1cmsg = msg(06);
061000090330            leavesr;
061100090330          endif;
061200090330
061300090330       endsr;
061400090324
061500090324       // ----------------------------------------------------------------------
061600090324       //?Pulisco il subfile
061700090324       // ----------------------------------------------------------------------
061800090326        begsr sr_puls02;
061900090324
062000090326          clear nrr2;
062100090326          clear rrnlast;
062200090324
062300090326          *in20 = *off;
062400090326          *in21 = *off;
062500090326          write sb04c02;
062600090326          *in20 = *on;
062700090326          *in21 = *on;
062800090324
062900090326          recsf2 = 1;
063000090326          *in31 = *on;
063100090324
063200090326        endsr;
063300090324
063400090324       // ----------------------------------------------------------------------
063500090324       //?Carico il subfile
063600090324       // ----------------------------------------------------------------------
063700090326        begsr sr_cars02;
063800090324
063900090327          dow not $ends02;
064000090324
064100090324       //?leggo i job
064200171009            SELECT;
064300171009       //?tutti i codici raggruppamento tutti i record
064400171009            WHEN v1cksu = *blanks and V1cd = *blanks;
064500090326              exec sql
064600090327                fetch next from ksu into :s02idjob, :s02tim,
064700090327                                         :s02ksu, :s02utecd,
064800110714                                         :s02dir, :s02tad,
064900110714                                         :s02ela;
065000171009
065100171009       //?tutti i codici raggruppamento solo i cd da fare
065200171009            WHEN v1cksu = *blanks and V1cd <> *blanks;
065300171009              exec sql
065400171009                fetch next from ksucd into :s02idjob, :s02tim,
065500171009                                         :s02ksu, :s02utecd,
065600171009                                         :s02dir, :s02tad,
065700171009                                         :s02ela;
065800171009
065900090325       //?un solo codice raggruppamento chiesto
066000171009            WHEN  v1cksu > *zeros;
066100090326              exec sql
066200090327                fetch next from ksu1 into :s02idjob, :s02tim,
066300090327                                          :s02ksu, :s02utecd,
066400110714                                          :s02dir, :s02tad,
066500110714                                          :s02ela;
066600171009            endSL;
066700090324
066800090324       //?fine file o errore sql esco
066900090326            if sqlcod = 100 or sqlcod < 0;
067000090327              $ends02 = *on;
067100090326              leave;
067200090326            endif;
067300090324
067400090326       //?conto per idjob e papà
067500090327          exsr sr_contas02;
067600090326
067700090324       //?scrivo subfile
067800090326          exsr sr_wrts02;
067900090324
068000090326          enddo;
068100090324
068200090324       //?indicatore per fine file
068300090326          *in31 = *on;
068400090324       //?imposto il numero di record caricati
068500090326          rrnlast = nrr2;
068600090324
068700090325       //?chiudo il cursore
068800171009          if v1cksu = *blanks and v1cd = *blanks;
068900090327            exec sql close ksu;
069000090326          endif;
069100171009          if v1cksu = *blanks and v1cd <> *blanks;
069200171009            exec sql close ksucd;
069300171009          endif;
069400090326          if v1cksu > *zeros;
069500090327            exec sql close ksu1;
069600090326          endif;
069700090324
069800090326        endsr;
069900090324
070000090326       // ----------------------------------------------------------------------
070100090326       //?Conto
070200090326       // ----------------------------------------------------------------------
070300090327        begsr sr_contas02;
070301171229
070400171229         clear S02nest;
070401171229         clear S02nela;
070402171229         clear S02ntas;
070403171229         clear S02nerr;
070404171229         clear sav_LACnim;
070405171229
070406171229       // Conto x IDJob
070407171229         setll (S02idjob:S02ksu) TILAC05L;
070408171229         reade (S02idjob:S02ksu) TILAC05L;
070409171229         DOW not %eof(TILAC05L);
070410171229           IF  LACela <> '33' and LACela <> '34';
070411171229             IF  LACnim <> sav_LACnim;
070413171229             // Immagini estratte
070414171229             S02nest += 1;
070415171229             // Immagini elaborate
070416171229             IF  LACela <> '00';
070417171229               S02nela += 1;
070418171229             ENDIF;
070419171229             // Addebiti
070420171229             IF  S02tad <> 'N' and LACela = '11';
070421171229               S02ntas += 1;
070422171229             ENDIF;
070423171229             // controllo errori (rcd con 'x2' o '9x' in LACELA')
070424171229             IF  %subst(LACela:2:1) = '2' or
070425171229                 %subst(LACela:1:1) = '9';
070426171229               S02nerr += 1;
070427171229             ENDIF;
070428171229             sav_LACnim = LACnim;
070429171229             ENDIF;
070430171229           ENDIF;
070431171229           reade (S02idjob:S02ksu) TILAC05L;
070432171229         ENDDO;
074400090326
074500090326        endsr;
074600090326
074700090324       // ----------------------------------------------------------------------
074800090326       //?Scrivo il subfile
074900090324       // ----------------------------------------------------------------------
075000090326        begsr sr_wrts02;
075100090327
075200090327          $recnook = *off;
075300090327
075400090327       //?controllo le parzializzazioni
075500090327          exsr sr_parzializza;
075600090327          if $recnook;
075700090327            leavesr;
075800090327          endif;
075900090324
076000090326       //?imposto i campi del subfile
076100090327       //?idjob
076200090327          v2sidjob = s02idjob;
076300090326       //?data
076400090327          v2sdata = %subst(s02tim:7:2) + '/' +
076500090327                    %subst(s02tim:5:2) + '/' +
076600090327                    %subst(s02tim:1:4);
076700090326       //?codice papà e cerco la ragione sociale da tabella LAC
076800090327          v2sksu = s02ksu;
076900090326       //?ragione sociale da tabella LAC
077000090326          exec sql
077100090326            select tbeuni into :tbeuni from tntbe00f
077200090326              where tbecod = 'LAC' and tbeatb = ' ' and
077300090327                    substr(tbeke1, 1, 7) = digits(:s02ksu);
077400090324       //?non trovo il rcd corrispondente
077500090324          if sqlcod <> 0;
077600090326            clear tbeuni;
077700090324          endif;
077800090326          dlac = tbeuni;
077900090326          v2srag = §lacrag;
078000090326       //?nr. immagini estratte
078100090327          v2sestr = s02nest;
078200090326       //?nr. immagini elaborate
078300090327          v2selab = s02nela;
078400090330       //?codice + ragione sociale in rosso se non elaborazione non terminata
078500090330          *in54 = (s02nest <> s02nela);
078600090326       //?se ho trovato dei rcd a 'x2' o a '9x'
078700090327          *in50 = (s02nerr > *zeros);
078800110714       //?Creare file di riepilogo
078900110714          IF  §LACpgm <> *blanks and s02ela = '33';
079000110714            v2sfile = 'SI';
079100110714          ENDIF;
079200110714          IF  §LACpgm <> *blanks and s02ela = '34';
079300110714            v2sfile = 'OK';
079400110714          ENDIF;
079500110714          IF  §LACpgm = *blanks;
079600110714            v2sfile = 'NO';
079700110714          ENDIF;
079800110714          v2lacela = s02ela;
079900090326       //?CD si/no
080000090327          if s02utecd <> *blanks;
080100090326            v2scd = 'SI';
080200090326          else;
080300090326            v2scd = 'NO';
080400090326          endif;
080500090326       //?TAS si/no
080600090326          select;
080700090402            when s02tad = 'N';
080800090402              v2stas = 'NP';
080900090327            when s02ntas = *zeros;
081000090327              v2stas = 'NO';
081100090330              *in51 = *off;
081200090327            when s02ntas = s02nest;
081300090326              v2stas = 'SI';
081400090330              *in51 = *off;
081500090327            when s02ntas <> s02nest;
081600090326              v2stas = 'SI';
081700090330              *in51 = *on;
081800090326          endsl;
081900090327       //?directory
082000090327          v2sdir = s02dir;
082100090330       //?indicatori del subfile
082200090330          v2sin50 = '0';
082300090330          v2sin51 = '0';
082400090330          v2sin54 = '0';
082500090330          if *in50;
082600090330            v2sin50 = '1';
082700090330          endif;
082800090330          if *in51;
082900090330            v2sin51 = '1';
083000090330          endif;
083100090330          if *in54;
083200090330            v2sin54 = '1';
083300090330          endif;
083400090326
083500090326          nrr2 = nrr2 + 1;
083600090324          write sb04s02;
083700090324
083800090326        endsr;
083900090327
084000090327       // ----------------------------------------------------------------------
084100090327       //?Controllo le parzializzazioni richieste in prima videata
084200090327       // ----------------------------------------------------------------------
084300090327        begsr sr_parzializza;
084400090327
084500090327       //?solo NON estratti
084600090327          if v1estr <> *blanks and s02nest <> *zeros;
084700090327            $recnook = *on;
084800090327            leavesr;
084900090327          endif;
085000090327
085100090327       //?solo elaborazioni NON terminate
085200090327          if v1elab <> *blanks and s02nest = s02nela;
085300090327            $recnook = *on;
085400090327            leavesr;
085500090327          endif;
085600090327
085700090327       //?solo CD NON completati
085800090327          if v1cd <> *blanks and s02utecd <> *blanks;
085900090327            $recnook = *on;
086000090327            leavesr;
086100090327          endif;
086200090327
086300090327       //?solo NON addebitati
086400090327          if v1tas <> *blanks and s02ntas <> *zeros;
086500090327            $recnook = *on;
086600090327            leavesr;
086700090327          endif;
086800090327
086900090327       endsr;
087000090324
087100090324       // ----------------------------------------------------------------------
087200090324       //?Controllo opzione subfile
087300090324       // ----------------------------------------------------------------------
087400090326        begsr sr_ctrs02;
087500090324
087600090324       //?controllo l'opzione immessa
087700090326          for xx = 1 to rrnlast;
087800090326            chain xx sb04s02;
087900090330            $endw02 = *off;
088000110714            $endw03 = *off;
088100090327            recsf2 = xx;
088200090327            select;
088300090327           //?visualizza dettaglio
088400090327              when v2sopz = '5';
088500090909             //?errore se non esistono dati estratti
088600090909                if v2sestr = 0 and v2selab = 0;
088700090909                  *in28 = *on;
088800090909                  v2cmsg = 'Dati non estratti!!';
088900090909                else;
089000090909                  $video = 'S03';
089100090909                  exsr gess03;
089200090909                  clear v2sopz;
089300090909                endif;
089400110714           //?Creazione file riepilogo
089500110714              when v2sopz = 'F';
089600110714             //?creo file se richiesto
089700110714                if  v2sfile = 'NO';
089800110714                  *in28 = *on;
089900110714                  v2cmsg = 'Opzione errata';
090000110714                endif;
090100110714                if  not *in28;
090200110714                  exsr gesw03;
090300110714                  clear v2sopz;
090400110714                endif;
090500090327           //?OK creato CD
090600090327              when v2sopz = 'C';
090700110714             //?errore se file da creare e non ancora creato
090800110714                if v2sfile = 'SI' and v2lacela = '33';
090900110714                  *in28 = *on;
091000110714                  v2cmsg = 'Prima creare il file!';
091100110714                endif;
091200110714                if  not *in28;
091300090327             //?errore se cd già creato
091400090327                if v2scd = 'SI';
091500090327                  *in28 = *on;
091600090327                  v2cmsg = 'CD già creato!!';
091700090327                else;
091800090327             //?creo cd
091900090327                  exsr gesw02;
092000090327                  clear v2sopz;
092100090327                endif;
092200110714                endif;
092300090327           //?visualizzo tabella LAC
092400090327              when v2sopz = 'T';
092500090327                $video = 'LAC';
092600090327                exsr geslac;
092700090327                clear v2sopz;
092800090327            endsl;
092900090327
093000090330            *in50 = (v2sin50 = '1');
093100090330            *in51 = (v2sin51 = '1');
093200090330            *in54 = (v2sin54 = '1');
093300090327            update sb04s02;
093400090326          endfor;
093500090324
093600090326        endsr;
093700090324
093800090324       // ----------------------------------------------------------------------
093900090327       //?Gestione subfile
094000090324       // ----------------------------------------------------------------------
094100090327        begsr gess03;
094200090327
094300090327       //?subfile
094400090327          exsr sr_s03;
094500090327
094600090327       //?emetto il subfile
094700090327          if nrr3 > 0;
094800090327            exsr sr_sfl03;
094900090327          endif;
095000090324
095100090326        endsr;
095200090327
095300090327       // ----------------------------------------------------------------------
095400090327       //?Gestione window
095500090327       // ----------------------------------------------------------------------
095600090327        begsr gesw02;
095700090327
095800090327       //?imposto i campi
095900090327          w2idjob = v2sidjob;
096000090327          w2cksu = v2sksu;
096100090327          w2crag = v2srag;
096200090327
096300090327       //?emetto la videata
096400090330          dow not $endw02;
096500090330            exfmt sb04w02;
096600090327
096700090330         //?spengo indicatori di comodo
096800090330            *in28 = *off;
096900090330         //?pulisco campo messaggi
097000090330            clear w2cmsg;
097100090330         //?esecuzione comandi
097200090330            select;
097300090327
097400090330           //?f6=conferma
097500090330              when *inkf;
097600090330                exsr sr_okcd;
097700090330             //?imposto flag per ricaricare il subfile
097800090330                $okcd = *on;
097900090330                $endw02 = *on;
098000090330           //?f12=ritorno
098100090330              when *inkl;
098200090330                $video = 'S02';
098300090330                $endw02 = *on;
098400090330            endsl;
098500090330          enddo;
098600090327
098700090327        endsr;
098800110714
098900110714       // ----------------------------------------------------------------------
099000110714       //?Gestione window
099100110714       // ----------------------------------------------------------------------
099200110714        begsr gesw03;
099300110714
099400110714       //?imposto i campi
099500110714          w3idjob = v2sidjob;
099600110714          w3cksu = v2sksu;
099700110714          w3crag = v2srag;
099800110714
099900110714       //?emetto la videata
100000110714          dow not $endw03;
100100110714            exfmt sb04w03;
100200110714
100300110714         //?spengo indicatori di comodo
100400110714            *in28 = *off;
100500110714         //?pulisco campo messaggi
100600110714            clear w3cmsg;
100700110721            *in28 = *off;
100800110714         //?esecuzione comandi
100900110714            select;
101000110714
101100110714           //?f6=conferma
101200110714              when *inkf;
101300110714                exsr sr_okfile;
101400110721                IF  OSB06esito <> *zeros;
101500110721                  w3cmsg = 'File CSV non creato';
101600110721                  *in28 = *on;
101700110721                ELSE;
101800110714             //?imposto flag per ricaricare il subfile
101900110721                  $okfile = *on;
102000110721                  $endw03 = *on;
102100110721                ENDIF;
102200110714           //?f12=ritorno
102300110714              when *inkl;
102400110714                $video = 'S02';
102500110714                $endw03 = *on;
102600110714            endsl;
102700110714          enddo;
102800110714
102900110714        endsr;
103000090327
103100090327       // ----------------------------------------------------------------------
103200090327       //?Gestione visualizzazione tabella LAC
103300090327       // ----------------------------------------------------------------------
103400090327        begsr geslac;
103500090327
103600090327       //?richiamo pgm TNTB46R
103700090327          reset tntb46ds;
103800090327          b46ke1 = %trim( %editw( v2sksu : '0       ' ) );
103900090327
104000090327          kpjbu = tntb46ds;
104100090327          tntb46r(kpjba);
104200090327          tntb46ds = kpjbu;
104300090327          $video = 'S02';
104400090327          clear v2sopz;
104500090327
104600090327        endsr;
104700090327
104800090327       // ----------------------------------------------------------------------
104900090327       //?Subfile
105000090327       // ----------------------------------------------------------------------
105100090327        begsr sr_s03;
105200090327
105300090327       //?pulisco il subfile
105400090327          exsr sr_puls03;
105500090327
105600090327          $ends03 = *off;
105700090327
105800090327       //?leggo tilac00f per Job Time Papà e Figlio
105900090327          exec sql
106000090327            declare ksc cursor for select distinct
106100090402            lacidjob, lactim, lacksu, lacksc, lacdir, lactad
106200090327            from tilac00f where lacidjob = :v2sidjob and
106300140509            lacksu = :v2sksu and lacela <> '33' and lacela <> '34'
106400090402            group by lacidjob, lactim, lacksu, lacksc, lacdir, lactad;
106500090327
106600090327          exec sql
106700090327            open ksc;
106800090327            if sqlcode < 0;
106900090327              $ends03 = *on;
107000090327            ENDIF;
107100090327
107200090327       //?carico il subfile
107300090327          exsr sr_cars03;
107400090327
107500090327        endsr;
107600090327
107700090327       // ----------------------------------------------------------------------
107800090327       //?Emetto il subfile
107900090327       // ----------------------------------------------------------------------
108000090327        begsr sr_sfl03;
108100090327
108200090327       //?emetto la videata
108300090327          write sb04t01;
108400090327          write sb04z03;
108500090327          exfmt sb04c03;
108600090327
108700090327       //?spengo indicatori di comodo
108800090327          *in28 = *off;
108900090327       //?pulisco campo messaggi
109000090327          clear v3cmsg;
109100090327       //?esecuzione comandi
109200090327          select;
109300090327
109400090327         //?f3=fine
109500090327            when *inkc;
109600090327              exsr sr_fine;
109700090327         //?f12=ritorno
109800090327            when *inkl;
109900090327              $video = 'S02';
110000090327              leavesr;
110100090327          endsl;
110200090327
110300090327        endsr;
110400090324
110500090327       // ----------------------------------------------------------------------
110600090327       //?Pulisco il subfile
110700090327       // ----------------------------------------------------------------------
110800090327        begsr sr_puls03;
110900090327
111000090327          clear nrr3;
111100110303          clear T03nest;
111200110303          clear T03nela;
111300090327
111400090327          *in22 = *off;
111500090327          *in23 = *off;
111600090327          write sb04c03;
111700090327          *in22 = *on;
111800090327          *in23 = *on;
111900090327
112000090327          recsf3 = 1;
112100090327          *in32 = *on;
112200090327
112300090327        endsr;
112400090327
112500090327       // ----------------------------------------------------------------------
112600090327       //?Carico il subfile
112700090327       // ----------------------------------------------------------------------
112800090327        begsr sr_cars03;
112900110303
113000110303          *in56 = *off;
113100090327
113200090327          dow not $ends03;
113300090327
113400090327       //?leggo i ksc
113500090327            exec sql
113600090327              fetch next from ksc into :s03idjob, :s03tim,
113700090327                                       :s03ksu, :s03ksc,
113800090402                                       :s03dir, :s03tad;
113900090327
114000090327       //?fine file o errore sql esco
114100090327            if sqlcod = 100 or sqlcod < 0;
114200090327              $ends03 = *on;
114300090909              leave;
114400090327            endif;
114500090327
114600090327       //?conto per idjob e papà
114700090327          exsr sr_contas03;
114800090327
114900090327       //?scrivo subfile
115000090327          exsr sr_wrts03;
115100090327
115200090327          enddo;
115300110303
115400110303       //?scrivo riga di totale nel subfile se ci sono più figli
115500110303          IF  nrr3 > 1;
115600110303            *in56 = *on;
115700110303            exsr sr_wrtT03;
115800110303            *in56 = *off;
115900110303          ENDIF;
116000090327
116100090327       //?indicatore per fine file
116200090327          *in32 = *on;
116300090327
116400090327          exec sql close ksc;
116500090327
116600090327        endsr;
116700090327
116800090327       // ----------------------------------------------------------------------
116900090327       //?Conto
117000090327       // ----------------------------------------------------------------------
117100090327        begsr sr_contas03;
117200090327
117300090327       //?immagini estratte
117400090327          exec sql
117500110303            select count(distinct lacnim) into :s03nest
117600090327              from tilac00f where lacidjob = :s03idjob and
117700090327              lacksu = :s03ksu and lacksc = :s03ksc and
117800140509              lacela <> '33' and lacela <> '34';
117900090327          if sqlcod <> 0;
118000090327            clear s03nest;
118100090327          endif;
118200090327       //?immagini elaborate
118300090327          exec sql
118400110303            select count(distinct lacnim) into :s03nela
118500090327              from tilac00f where lacidjob = :s03idjob and
118600090327              lacksu = :s03ksu and lacksc = :s03ksc and
118700140509              lacela <> '33' and lacela <> '34' and lacela <> '00';
118800090327          if sqlcod <> 0;
118900090327            clear s03nela;
119000090327          endif;
119100090327       //?addebiti
119200090402          if s03tad <> 'N';
119300090402            exec sql
119400110303              select count(distinct lacnim) into :s03ntas
119500090402                from tilac00f where lacidjob = :s03idjob and
119600090402                lacksu = :s03ksu and lacksc = :s03ksc and
119700140509                lacela <> '33' and lacela <> '34' and lacela = '11';
119800090402            if sqlcod <> 0;
119900090402              clear s03ntas;
120000090402            endif;
120100090402          else;
120200090402            clear s03ntas;
120300090402          endif;
120400090327       //?controllo se ci sono dei rcd a 'x2' o a '9x'
120500090327          exec sql
120600110303            select count(distinct lacnim) into :s03nerr
120700090327              from tilac00f where lacidjob = :s03idjob and
120800090327              lacksu = :s03ksu and lacksc = :s03ksc and
120900090327              (substr(lacela, 2, 1) = '2' or
121000090327               substr(lacela, 1, 1) = '9');
121100090327          if sqlcod <> 0;
121200090327            clear s03nerr;
121300090327          endif;
121400090327
121500090327        endsr;
121600090327
121700090327       // ----------------------------------------------------------------------
121800090327       //?Scrivo il subfile
121900090327       // ----------------------------------------------------------------------
122000090327        begsr sr_wrts03;
122100090327
122200090327       //?imposto i campi del control
122300090327          v3cksu = s03ksu;
122400090327       //?ragione sociale da tabella LAC
122500090327          exec sql
122600090327            select tbeuni into :tbeuni from tntbe00f
122700090327              where tbecod = 'LAC' and tbeatb = ' ' and
122800090327                    substr(tbeke1, 1, 7) = digits(:s03ksu);
122900090327       //?non trovo il rcd corrispondente
123000090327          if sqlcod <> 0;
123100090327            clear tbeuni;
123200090327          endif;
123300090327          dlac = tbeuni;
123400090327          v3crag = §lacrag;
123500090327
123600090327       //?imposto i campi del subfile
123700090327       //?data
123800090327          v3sdata = %subst(s03tim:7:2) + '/' +
123900090327                    %subst(s03tim:5:2) + '/' +
124000090327                    %subst(s03tim:1:4);
124100090327       //?codice estrazione
124200090327          v3sksc = s03ksc;
124300090327       //?ragione sociale da tabella LAC
124400090327          exec sql
124500090327            select tbeuni into :tbeuni from tntbe00f
124600090327              where tbecod = 'LAC' and tbeatb = ' ' and
124700090327                    substr(tbeke1, 1, 7) = digits(:s03ksc);
124800090327       //?non trovo il rcd corrispondente
124900090327          if sqlcod <> 0;
125000090327            clear tbeuni;
125100090327          endif;
125200090327          dlac = tbeuni;
125300090327          v3srag = §lacrag;
125400090327       //?nr. immagini estratte
125500090327          v3sestr = s03nest;
125600090327       //?nr. immagini elaborate
125700090327          v3selab = s03nela;
125800090330       //?codice + ragione sociale in rosso se non elaborazione non terminata
125900090330          *in55 = (s03nest <> s03nela);
126000090327       //?se ho trovato dei rcd a 'x2' o a '9x'
126100090327          *in52 = (s03nerr > *zeros);
126200090327       //?CD si/no
126300090327          v3scd = v2scd;
126400090327       //?TAS si/no
126500090327          select;
126600090402            when s03tad = 'N';
126700090402              v3stas = 'NP';
126800090327            when s03ntas = *zeros;
126900090327              v3stas = 'NO';
127000090327              eval *in53 = *off;
127100090327            when s03ntas = s03nest;
127200090327              v3stas = 'SI';
127300090327              eval *in53 = *off;
127400090327            when s03ntas <> s03nest;
127500090327              v3stas = 'SI';
127600090327              eval *in53 = *on;
127700090327          endsl;
127800090327       //?directory
127900090327          v3sdir = s03dir;
128000110303
128100110303       //?sommo le immagini estratte e le elaborate
128200110303          T03nest += S03nest;
128300110303          T03nela += S03nela;
128400090327
128500090327          nrr3 = nrr3 + 1;
128600090327          write sb04s03;
128700090327
128800090327        endsr;
128900110303
129000110303       // ----------------------------------------------------------------------
129100110303       //?Scrivo riga di totale nel subfile.
129200110303       // ----------------------------------------------------------------------
129300110303        begsr sr_wrtT03;
129400110303
129500110303          *in52 = *off;
129600110303          *in53 = *off;
129700110303          *in55 = *off;
129800110303          clear v3sdata;
129900110303          clear v3sksc;
130000110303          v3srag = 'T O T A L E ';
130100110303          v3sestr = T03nest;
130200110303          v3selab = T03nela;
130300110303          clear v3scd;
130400110303          clear v3stas;
130500110303          clear v3stas;
130600110303          clear v3sdir;
130700110303          nrr3 = nrr3 + 1;
130800110303          write sb04s03;
130900110303
131000110303        endsr;
131100090327
131200090327       // ----------------------------------------------------------------------
131300090327       //?Ok fatto cd aggiorno TILAC
131400090327       // ----------------------------------------------------------------------
131500090327        begsr sr_okcd;
131600090327
131700090327          exec sql
131800090327            update tilac00f
131900090327            set lacutecd = :knmus, lactimecd = :wtimestp
132000090327            where
132100090327            lacidjob = :w2idjob and
132200110714              lacksu = :w2cksu  and lacela in('33', '34');
132300090327
132400090327        endsr;
132500110714
132600110714       // ----------------------------------------------------------------------
132700110714       //?Ok faccio il file e aggiorno TILAC
132800110714       // ----------------------------------------------------------------------
132900110714        begsr sr_okfile;
133000110714
133100110714       //?richiamo pgm per creare il FILE CSV da aggiungere al CD
133200110721          clear TNSB06DS;
133300110721          ISB06ksu   = V2Sksu;
133400110721          ISB06idjob = V2Sidjob;
133500110721          ISB06dir   = V2Sdir;
133600110721          tnsb06r (kpjba : TNSB06DS);
133700110721          IF  OSB06esito <> *zeros;
133800110721            leavesr;
133900110721          ENDIF;
134000110714
134100110714          exec sql
134200110714            update tilac00f
134300110714            set lacela = '34'
134400110714            where
134500110714            lacidjob = :w3idjob and
134600110714              lacksu = :w3cksu  and lacela = '33';
134700110714
134800110714        endsr;
134900090327
135000090324       // ----------------------------------------------------------------------
135100090324       //?Fine programma
135200090324       // ----------------------------------------------------------------------
135300090327        begsr sr_fine;
135400090324
135500090327          *inlr = *on;
135600090327          return;
135700090324
135800090327        endsr;
135900090324
136000090324** MSG  Lungh. 78                                                            *
136100090326Immettere almeno una delle due date                                           01
136200090326Data errata                                                                   02
136300090326Range di date errato                                                          03
136400090326Immettere SOLO caratteri numerici                                             04
136500090326Codice errato                                                                 05
136600090330E' possibile selezionare solo una parzializzazione                            06
