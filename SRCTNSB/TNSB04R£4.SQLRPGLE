000100090324     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200090324      // ----------------------------------------------------------------------
000300090324      //
000400090326      //         Pod Image su CD  ?
000500090324      //
000600090324      // ----------------------------------------------------------------------
000700090324      // ? DICHIARAZIONE DEI FILE ?
000800090324      // ----------------------------------------------------------------------
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
034900090326       //?tutti i papà
035000090326          if v1cksu = *blanks;
035100090326            exec sql
035200090327              declare ksu cursor for
035300110714              select lacidjob, lactim, lacksu, lacutecd, lacdir, lactad,
035400110714                     lacela
035500110714              from tilac00f where lacela in('33', '34') and
035600090326              substr(lactim, 1, 8) between digits(:wdatada)
035700090326              and digits(:wdataal)
035800090326              order by lactim, lacksu, lacidjob;
035900090326
036000090326            exec sql
036100090327              open ksu;
036200090326              if sqlcode < 0;
036300090327                $ends02 = *on;
036400090326              ENDIF;
036500090326          endif;
036600090326
036700090326       //?leggo tilac00f raggruppato per id job e codice estrazione
036800090326       //?un solo codice raggruppamento chiesto
036900090326          if v1cksu > *zeros;
037000090326            exec sql
037100090327              declare ksu1 cursor for
037200110714              select lacidjob, lactim, lacksu, lacutecd, lacdir, lactad,
037300110714                     lacela
037400110714              from tilac00f where lacela in('33', '34') and
037500090326              substr(lactim, 1, 8) between digits(:wdatada) and
037600090326              digits(:wdataal) and digits(lacksu) = :v1cksu
037700090326              order by lactim, lacksu, lacidjob;
037800090326
037900090326            exec sql
038000090327              open ksu1;
038100090326              if sqlcode < 0;
038200090327                $ends02 = *on;
038300090326              ENDIF;
038400090326          endif;
038500090326
038600090324       //?carico il subfile
038700090326          exsr sr_cars02;
038800090324
038900090326        endsr;
039000090324
039100090324       // ----------------------------------------------------------------------
039200090326       //?Emetto il subfile
039300090324       // ----------------------------------------------------------------------
039400090326        begsr sr_sfl02;
039500090324
039600090330       //?flag creazione cd
039700090330          $okcd = *off;
039800110714       //?flag creazione file
039900110714          $okfile = *off;
040000090330
040100090324       //?emetto la videata
040200090326          write sb04t01;
040300090326          write sb04z02;
040400090326          exfmt sb04c02;
040500090324
040600090324       //?spengo indicatori di comodo
040700090326          *in28 = *off;
040800090324       //?pulisco campo messaggi
040900090326          clear v2cmsg;
041000090324       //?esecuzione comandi
041100090326          select;
041200090324
041300090326         //?f3=fine
041400090326            when *inkc;
041500110301              exsr sr_fine;
041600110301         //?f5=refresh
041700110301            when *inke;
041800110301              exsr sr_s02;
041900090326         //?f12=ritorno
042000090326            when *inkl;
042100090326              $video = 'D01';
042200090326              leavesr;
042300090326            other;
042400090326         //?controllo opzione subfile
042500090326            exsr sr_ctrs02;
042600090330         //?se creati CD ricarico il subfile
042700110714         //?anche se creato il file
042800110714            if $okcd or $okfile;
042900090330              exsr sr_s02;
043000090330            endif;
043100090326          endsl;
043200090324
043300090326        endsr;
043400090324
043500090324       // ----------------------------------------------------------------------
043600090324       //?Emetto video di segnalazione dati non caricati
043700090324       // ----------------------------------------------------------------------
043800090326        begsr sr_d02;
043900090324
044000090324       //?emetto la videata
044100090326          write sb04t01;
044200090326          exfmt sb04d02;
044300090324
044400090324       //?spengo indicatori di comodo
044500090326          *in28 = *off;
044600090326          *in90 = *off;
044700090324       //?pulisco campo messaggi
044800090326          clear v2cmsg;
044900090324       //?esecuzione comandi
045000090326          select;
045100090324
045200090326         //?f3=fine
045300090326            when *inkc;
045400090326              exsr sr_fine;
045500090326         //?f12=ritorno
045600090326            when *inkl;
045700090326              $video = 'D01';
045800090326              leavesr;
045900090326            other;
046000090326          endsl;
046100090324
046200090326        endsr;
046300090324
046400090324       // ----------------------------------------------------------------------
046500090324       //?Controllo data
046600090324       // ----------------------------------------------------------------------
046700090326        begsr sr_contrdata;
046800090324
046900090324       //?la data è obbligatoria
047000090326          if v1datada = *zeros and v1dataal = *zeros;
047100090326            *in28 = *on;
047200090326            *in41 = *on;
047300090326            v1cmsg = msg(01);
047400090326            leavesr;
047500090326          endif;
047600090324
047700090324       //?deve essere una data valida
047800090326       //?data da
047900090326          if v1datada <> *zeros;
048000090326            clear wlbdat;
048100090326            g02dat = v1datada;
048200090326            xsrda8(wlbdat);
048300090326            if g02err = '1';
048400090326              *in28 = *on;
048500090326              *in41 = *on;
048600090326              v1cmsg = msg(02);
048700090326              leavesr;
048800090326            endif;
048900090326            v1datada = g02dat;
049000090326            wdatada  = g02inv;
049100090326          endif;
049200090326       //?data al
049300090326          if v1dataal <> *zeros;
049400090326            clear wlbdat;
049500090326            g02dat = v1dataal;
049600090326            xsrda8(wlbdat);
049700090326            if g02err = '1';
049800090326              *in28 = *on;
049900090326              *in42 = *on;
050000090326              v1cmsg = msg(02);
050100090326              leavesr;
050200090326            endif;
050300090326            v1dataal = g02dat;
050400090326            wdataal  = g02inv;
050500090326          endif;
050600090324
050700090326       //?imposto in automatico la data mancante
050800090326          if v1dataal = *zeros;
050900090326            v1dataal = v1datada;
051000090326            wdataal  = wdatada;
051100090326          endif;
051200090326       //?controllo range date
051300110502          if wdatada > wdataal;
051400090326            *in28 = *on;
051500090326            *in41 = *on;
051600090326            v1cmsg = msg(03);
051700090326            leavesr;
051800090326          endif;
051900090324
052000090326        endsr;
052100090324
052200090324       // ----------------------------------------------------------------------
052300090326       //?Controllo padre
052400090324       // ----------------------------------------------------------------------
052500090326        begsr sr_contrksu;
052600090324
052700090326       //?Ricerca codice cliente
052800090326          if %scan('?':v1cksu) > *zeros;
052900090326            clear  param01;
053000090326            p01ord = '0';
053100090326            kpjbu = param01;
053200141201            tntb461r (kpjba:P01opz3);
053300090326            param01 = kpjbu;
053400090324
053500090326            if p01ke1 <> *zeros;
053600090326              v1cksu = p01ke1;
053700090326              *in43 = *on;
053800090326              *in90 = *on;
053900090326            endif;
054000090326          endif;
054100090324
054200090326       //?Controllo immissione solo caratteri numerici
054300090326          if %check(digitn:v1cksu) > *zeros;
054400090326            *in28 = *on;
054500090326            *in43 = *on;
054600090326            v1cmsg = msg(04);
054700090326            leavesr;
054800090326          endif;
054900090324
055000090326       //?Controllo esistenza codice cliente in tabella LAC
055100090326          exec sql
055200090326            select tbeuni into :tbeuni from tntbe00f
055300090326            where tbecod = 'LAC' and tbeatb = '' and
055400090326            substr(tbeke1, 1, 7) = :v1cksu;
055500090326       //?non trovo il rcd corrispondente
055600090326          if sqlcod <> 0;
055700090326            clear tbeuni;
055800090326            *in28 = *on;
055900090326            *in43 = *on;
056000090326            v1cmsg = msg(05);
056100090326            leavesr;
056200090326          endif;
056300090324
056400090326          dlac = tbeuni;
056500090326          v1dksu = §lacrag;
056600090324
056700090326        endsr;
056800090330
056900090330       // ----------------------------------------------------------------------
057000090330       //?Controllo parzializzazioni
057100090330       // ----------------------------------------------------------------------
057200090330        begsr sr_contrpar;
057300090330
057400090330       //?le parzializzazioni non sono in alternativa
057500090330          if v1cd <> *blanks and (v1estr <> *blanks or v1elab <> *blanks or
057600090330                                  v1tas <> *blanks);
057700090330            *in28 = *on;
057800090330            *in44 = *on;
057900090330            v1cmsg = msg(06);
058000090330            leavesr;
058100090330          endif;
058200090330          if v1estr <> *blanks and (v1elab <> *blanks or v1tas <> *blanks);
058300090330            *in28 = *on;
058400090330            *in45 = *on;
058500090330            v1cmsg = msg(06);
058600090330            leavesr;
058700090330          endif;
058800090330          if v1elab <> *blanks and v1tas <> *blanks;
058900090330            *in28 = *on;
059000090330            *in46 = *on;
059100090330            v1cmsg = msg(06);
059200090330            leavesr;
059300090330          endif;
059400090330
059500090330       endsr;
059600090324
059700090324       // ----------------------------------------------------------------------
059800090324       //?Pulisco il subfile
059900090324       // ----------------------------------------------------------------------
060000090326        begsr sr_puls02;
060100090324
060200090326          clear nrr2;
060300090326          clear rrnlast;
060400090324
060500090326          *in20 = *off;
060600090326          *in21 = *off;
060700090326          write sb04c02;
060800090326          *in20 = *on;
060900090326          *in21 = *on;
061000090324
061100090326          recsf2 = 1;
061200090326          *in31 = *on;
061300090324
061400090326        endsr;
061500090324
061600090324       // ----------------------------------------------------------------------
061700090324       //?Carico il subfile
061800090324       // ----------------------------------------------------------------------
061900090326        begsr sr_cars02;
062000090324
062100090327          dow not $ends02;
062200090324
062300090324       //?leggo i job
062400090325       //?tutti i codici raggruppamento
062500090326            if v1cksu = *blanks;
062600090326              exec sql
062700090327                fetch next from ksu into :s02idjob, :s02tim,
062800090327                                         :s02ksu, :s02utecd,
062900110714                                         :s02dir, :s02tad,
063000110714                                         :s02ela;
063100090326            endif;
063200090325       //?un solo codice raggruppamento chiesto
063300090326            if v1cksu > *zeros;
063400090326              exec sql
063500090327                fetch next from ksu1 into :s02idjob, :s02tim,
063600090327                                          :s02ksu, :s02utecd,
063700110714                                          :s02dir, :s02tad,
063800110714                                          :s02ela;
063900090326            endif;
064000090324
064100090324       //?fine file o errore sql esco
064200090326            if sqlcod = 100 or sqlcod < 0;
064300090327              $ends02 = *on;
064400090326              leave;
064500090326            endif;
064600090324
064700090326       //?conto per idjob e papà
064800090327          exsr sr_contas02;
064900090326
065000090324       //?scrivo subfile
065100090326          exsr sr_wrts02;
065200090324
065300090326          enddo;
065400090324
065500090324       //?indicatore per fine file
065600090326          *in31 = *on;
065700090324       //?imposto il numero di record caricati
065800090326          rrnlast = nrr2;
065900090324
066000090325       //?chiudo il cursore
066100090326          if v1cksu = *blanks;
066200090327            exec sql close ksu;
066300090326          endif;
066400090326          if v1cksu > *zeros;
066500090327            exec sql close ksu1;
066600090326          endif;
066700090324
066800090326        endsr;
066900090324
067000090326       // ----------------------------------------------------------------------
067100090326       //?Conto
067200090326       // ----------------------------------------------------------------------
067300090327        begsr sr_contas02;
067400090326
067500090326       //?immagini estratte
067600090326          exec sql
067700090716            select count(distinct lacnim) into :s02nest
067800090327              from tilac00f where lacidjob = :s02idjob and
067900110714              lacksu = :s02ksu and lacela not in('33', '34');
068000090326          if sqlcod <> 0;
068100090327            clear s02nest;
068200090326          endif;
068300090326       //?immagini elaborate
068400090326          exec sql
068500090716            select count(distinct lacnim) into :s02nela
068600090327              from tilac00f where lacidjob = :s02idjob and
068700110714              lacksu = :s02ksu and lacela not in('33', '34', '00');
068800090326          if sqlcod <> 0;
068900090327            clear s02nela;
069000090326          endif;
069100090326       //?addebiti
069200090402          if s02tad <> 'N';
069300090402            exec sql
069400090716              select count(distinct lacnim) into :s02ntas
069500090402                from tilac00f where lacidjob = :s02idjob and
069600110714                lacksu = :s02ksu and lacela not in('33', '34') and
069700090402                lacela = '11';
069800090402            if sqlcod <> 0;
069900090402              clear s02ntas;
070000090402            endif;
070100090402          else;
070200090402            clear s02ntas;
070300090402          endif;
070400090326       //?controllo se ci sono dei rcd a 'x2' o a '9x'
070500090326          exec sql
070600090327            select count(*) into :s02nerr
070700090327              from tilac00f where lacidjob = :s02idjob and
070800090327              lacksu = :s02ksu and
070900090326              (substr(lacela, 2, 1) = '2' or
071000090326               substr(lacela, 1, 1) = '9');
071100090326          if sqlcod <> 0;
071200090327            clear s02nerr;
071300090326          endif;
071400090326
071500090326        endsr;
071600090326
071700090324       // ----------------------------------------------------------------------
071800090326       //?Scrivo il subfile
071900090324       // ----------------------------------------------------------------------
072000090326        begsr sr_wrts02;
072100090327
072200090327          $recnook = *off;
072300090327
072400090327       //?controllo le parzializzazioni
072500090327          exsr sr_parzializza;
072600090327          if $recnook;
072700090327            leavesr;
072800090327          endif;
072900090324
073000090326       //?imposto i campi del subfile
073100090327       //?idjob
073200090327          v2sidjob = s02idjob;
073300090326       //?data
073400090327          v2sdata = %subst(s02tim:7:2) + '/' +
073500090327                    %subst(s02tim:5:2) + '/' +
073600090327                    %subst(s02tim:1:4);
073700090326       //?codice papà e cerco la ragione sociale da tabella LAC
073800090327          v2sksu = s02ksu;
073900090326       //?ragione sociale da tabella LAC
074000090326          exec sql
074100090326            select tbeuni into :tbeuni from tntbe00f
074200090326              where tbecod = 'LAC' and tbeatb = ' ' and
074300090327                    substr(tbeke1, 1, 7) = digits(:s02ksu);
074400090324       //?non trovo il rcd corrispondente
074500090324          if sqlcod <> 0;
074600090326            clear tbeuni;
074700090324          endif;
074800090326          dlac = tbeuni;
074900090326          v2srag = §lacrag;
075000090326       //?nr. immagini estratte
075100090327          v2sestr = s02nest;
075200090326       //?nr. immagini elaborate
075300090327          v2selab = s02nela;
075400090330       //?codice + ragione sociale in rosso se non elaborazione non terminata
075500090330          *in54 = (s02nest <> s02nela);
075600090326       //?se ho trovato dei rcd a 'x2' o a '9x'
075700090327          *in50 = (s02nerr > *zeros);
075800110714       //?Creare file di riepilogo
075900110714          IF  §LACpgm <> *blanks and s02ela = '33';
076000110714            v2sfile = 'SI';
076100110714          ENDIF;
076200110714          IF  §LACpgm <> *blanks and s02ela = '34';
076300110714            v2sfile = 'OK';
076400110714          ENDIF;
076500110714          IF  §LACpgm = *blanks;
076600110714            v2sfile = 'NO';
076700110714          ENDIF;
076800110714          v2lacela = s02ela;
076900090326       //?CD si/no
077000090327          if s02utecd <> *blanks;
077100090326            v2scd = 'SI';
077200090326          else;
077300090326            v2scd = 'NO';
077400090326          endif;
077500090326       //?TAS si/no
077600090326          select;
077700090402            when s02tad = 'N';
077800090402              v2stas = 'NP';
077900090327            when s02ntas = *zeros;
078000090327              v2stas = 'NO';
078100090330              *in51 = *off;
078200090327            when s02ntas = s02nest;
078300090326              v2stas = 'SI';
078400090330              *in51 = *off;
078500090327            when s02ntas <> s02nest;
078600090326              v2stas = 'SI';
078700090330              *in51 = *on;
078800090326          endsl;
078900090327       //?directory
079000090327          v2sdir = s02dir;
079100090330       //?indicatori del subfile
079200090330          v2sin50 = '0';
079300090330          v2sin51 = '0';
079400090330          v2sin54 = '0';
079500090330          if *in50;
079600090330            v2sin50 = '1';
079700090330          endif;
079800090330          if *in51;
079900090330            v2sin51 = '1';
080000090330          endif;
080100090330          if *in54;
080200090330            v2sin54 = '1';
080300090330          endif;
080400090326
080500090326          nrr2 = nrr2 + 1;
080600090324          write sb04s02;
080700090324
080800090326        endsr;
080900090327
081000090327       // ----------------------------------------------------------------------
081100090327       //?Controllo le parzializzazioni richieste in prima videata
081200090327       // ----------------------------------------------------------------------
081300090327        begsr sr_parzializza;
081400090327
081500090327       //?solo NON estratti
081600090327          if v1estr <> *blanks and s02nest <> *zeros;
081700090327            $recnook = *on;
081800090327            leavesr;
081900090327          endif;
082000090327
082100090327       //?solo elaborazioni NON terminate
082200090327          if v1elab <> *blanks and s02nest = s02nela;
082300090327            $recnook = *on;
082400090327            leavesr;
082500090327          endif;
082600090327
082700090327       //?solo CD NON completati
082800090327          if v1cd <> *blanks and s02utecd <> *blanks;
082900090327            $recnook = *on;
083000090327            leavesr;
083100090327          endif;
083200090327
083300090327       //?solo NON addebitati
083400090327          if v1tas <> *blanks and s02ntas <> *zeros;
083500090327            $recnook = *on;
083600090327            leavesr;
083700090327          endif;
083800090327
083900090327       endsr;
084000090324
084100090324       // ----------------------------------------------------------------------
084200090324       //?Controllo opzione subfile
084300090324       // ----------------------------------------------------------------------
084400090326        begsr sr_ctrs02;
084500090324
084600090324       //?controllo l'opzione immessa
084700090326          for xx = 1 to rrnlast;
084800090326            chain xx sb04s02;
084900090330            $endw02 = *off;
085000110714            $endw03 = *off;
085100090327            recsf2 = xx;
085200090327            select;
085300090327           //?visualizza dettaglio
085400090327              when v2sopz = '5';
085500090909             //?errore se non esistono dati estratti
085600090909                if v2sestr = 0 and v2selab = 0;
085700090909                  *in28 = *on;
085800090909                  v2cmsg = 'Dati non estratti!!';
085900090909                else;
086000090909                  $video = 'S03';
086100090909                  exsr gess03;
086200090909                  clear v2sopz;
086300090909                endif;
086400110714           //?Creazione file riepilogo
086500110714              when v2sopz = 'F';
086600110714             //?creo file se richiesto
086700110714                if  v2sfile = 'NO';
086800110714                  *in28 = *on;
086900110714                  v2cmsg = 'Opzione errata';
087000110714                endif;
087100110714                if  not *in28;
087200110714                  exsr gesw03;
087300110714                  clear v2sopz;
087400110714                endif;
087500090327           //?OK creato CD
087600090327              when v2sopz = 'C';
087700110714             //?errore se file da creare e non ancora creato
087800110714                if v2sfile = 'SI' and v2lacela = '33';
087900110714                  *in28 = *on;
088000110714                  v2cmsg = 'Prima creare il file!';
088100110714                endif;
088200110714                if  not *in28;
088300090327             //?errore se cd già creato
088400090327                if v2scd = 'SI';
088500090327                  *in28 = *on;
088600090327                  v2cmsg = 'CD già creato!!';
088700090327                else;
088800090327             //?creo cd
088900090327                  exsr gesw02;
089000090327                  clear v2sopz;
089100090327                endif;
089200110714                endif;
089300090327           //?visualizzo tabella LAC
089400090327              when v2sopz = 'T';
089500090327                $video = 'LAC';
089600090327                exsr geslac;
089700090327                clear v2sopz;
089800090327            endsl;
089900090327
090000090330            *in50 = (v2sin50 = '1');
090100090330            *in51 = (v2sin51 = '1');
090200090330            *in54 = (v2sin54 = '1');
090300090327            update sb04s02;
090400090326          endfor;
090500090324
090600090326        endsr;
090700090324
090800090324       // ----------------------------------------------------------------------
090900090327       //?Gestione subfile
091000090324       // ----------------------------------------------------------------------
091100090327        begsr gess03;
091200090327
091300090327       //?subfile
091400090327          exsr sr_s03;
091500090327
091600090327       //?emetto il subfile
091700090327          if nrr3 > 0;
091800090327            exsr sr_sfl03;
091900090327          endif;
092000090324
092100090326        endsr;
092200090327
092300090327       // ----------------------------------------------------------------------
092400090327       //?Gestione window
092500090327       // ----------------------------------------------------------------------
092600090327        begsr gesw02;
092700090327
092800090327       //?imposto i campi
092900090327          w2idjob = v2sidjob;
093000090327          w2cksu = v2sksu;
093100090327          w2crag = v2srag;
093200090327
093300090327       //?emetto la videata
093400090330          dow not $endw02;
093500090330            exfmt sb04w02;
093600090327
093700090330         //?spengo indicatori di comodo
093800090330            *in28 = *off;
093900090330         //?pulisco campo messaggi
094000090330            clear w2cmsg;
094100090330         //?esecuzione comandi
094200090330            select;
094300090327
094400090330           //?f6=conferma
094500090330              when *inkf;
094600090330                exsr sr_okcd;
094700090330             //?imposto flag per ricaricare il subfile
094800090330                $okcd = *on;
094900090330                $endw02 = *on;
095000090330           //?f12=ritorno
095100090330              when *inkl;
095200090330                $video = 'S02';
095300090330                $endw02 = *on;
095400090330            endsl;
095500090330          enddo;
095600090327
095700090327        endsr;
095800110714
095900110714       // ----------------------------------------------------------------------
096000110714       //?Gestione window
096100110714       // ----------------------------------------------------------------------
096200110714        begsr gesw03;
096300110714
096400110714       //?imposto i campi
096500110714          w3idjob = v2sidjob;
096600110714          w3cksu = v2sksu;
096700110714          w3crag = v2srag;
096800110714
096900110714       //?emetto la videata
097000110714          dow not $endw03;
097100110714            exfmt sb04w03;
097200110714
097300110714         //?spengo indicatori di comodo
097400110714            *in28 = *off;
097500110714         //?pulisco campo messaggi
097600110714            clear w3cmsg;
097700110721            *in28 = *off;
097800110714         //?esecuzione comandi
097900110714            select;
098000110714
098100110714           //?f6=conferma
098200110714              when *inkf;
098300110714                exsr sr_okfile;
098400110721                IF  OSB06esito <> *zeros;
098500110721                  w3cmsg = 'File CSV non creato';
098600110721                  *in28 = *on;
098700110721                ELSE;
098800110714             //?imposto flag per ricaricare il subfile
098900110721                  $okfile = *on;
099000110721                  $endw03 = *on;
099100110721                ENDIF;
099200110714           //?f12=ritorno
099300110714              when *inkl;
099400110714                $video = 'S02';
099500110714                $endw03 = *on;
099600110714            endsl;
099700110714          enddo;
099800110714
099900110714        endsr;
100000090327
100100090327       // ----------------------------------------------------------------------
100200090327       //?Gestione visualizzazione tabella LAC
100300090327       // ----------------------------------------------------------------------
100400090327        begsr geslac;
100500090327
100600090327       //?richiamo pgm TNTB46R
100700090327          reset tntb46ds;
100800090327          b46ke1 = %trim( %editw( v2sksu : '0       ' ) );
100900090327
101000090327          kpjbu = tntb46ds;
101100090327          tntb46r(kpjba);
101200090327          tntb46ds = kpjbu;
101300090327          $video = 'S02';
101400090327          clear v2sopz;
101500090327
101600090327        endsr;
101700090327
101800090327       // ----------------------------------------------------------------------
101900090327       //?Subfile
102000090327       // ----------------------------------------------------------------------
102100090327        begsr sr_s03;
102200090327
102300090327       //?pulisco il subfile
102400090327          exsr sr_puls03;
102500090327
102600090327          $ends03 = *off;
102700090327
102800090327       //?leggo tilac00f per Job Time Papà e Figlio
102900090327          exec sql
103000090327            declare ksc cursor for select distinct
103100090402            lacidjob, lactim, lacksu, lacksc, lacdir, lactad
103200090327            from tilac00f where lacidjob = :v2sidjob and
103300140509            lacksu = :v2sksu and lacela <> '33' and lacela <> '34'
103400090402            group by lacidjob, lactim, lacksu, lacksc, lacdir, lactad;
103500090327
103600090327          exec sql
103700090327            open ksc;
103800090327            if sqlcode < 0;
103900090327              $ends03 = *on;
104000090327            ENDIF;
104100090327
104200090327       //?carico il subfile
104300090327          exsr sr_cars03;
104400090327
104500090327        endsr;
104600090327
104700090327       // ----------------------------------------------------------------------
104800090327       //?Emetto il subfile
104900090327       // ----------------------------------------------------------------------
105000090327        begsr sr_sfl03;
105100090327
105200090327       //?emetto la videata
105300090327          write sb04t01;
105400090327          write sb04z03;
105500090327          exfmt sb04c03;
105600090327
105700090327       //?spengo indicatori di comodo
105800090327          *in28 = *off;
105900090327       //?pulisco campo messaggi
106000090327          clear v3cmsg;
106100090327       //?esecuzione comandi
106200090327          select;
106300090327
106400090327         //?f3=fine
106500090327            when *inkc;
106600090327              exsr sr_fine;
106700090327         //?f12=ritorno
106800090327            when *inkl;
106900090327              $video = 'S02';
107000090327              leavesr;
107100090327          endsl;
107200090327
107300090327        endsr;
107400090324
107500090327       // ----------------------------------------------------------------------
107600090327       //?Pulisco il subfile
107700090327       // ----------------------------------------------------------------------
107800090327        begsr sr_puls03;
107900090327
108000090327          clear nrr3;
108100110303          clear T03nest;
108200110303          clear T03nela;
108300090327
108400090327          *in22 = *off;
108500090327          *in23 = *off;
108600090327          write sb04c03;
108700090327          *in22 = *on;
108800090327          *in23 = *on;
108900090327
109000090327          recsf3 = 1;
109100090327          *in32 = *on;
109200090327
109300090327        endsr;
109400090327
109500090327       // ----------------------------------------------------------------------
109600090327       //?Carico il subfile
109700090327       // ----------------------------------------------------------------------
109800090327        begsr sr_cars03;
109900110303
110000110303          *in56 = *off;
110100090327
110200090327          dow not $ends03;
110300090327
110400090327       //?leggo i ksc
110500090327            exec sql
110600090327              fetch next from ksc into :s03idjob, :s03tim,
110700090327                                       :s03ksu, :s03ksc,
110800090402                                       :s03dir, :s03tad;
110900090327
111000090327       //?fine file o errore sql esco
111100090327            if sqlcod = 100 or sqlcod < 0;
111200090327              $ends03 = *on;
111300090909              leave;
111400090327            endif;
111500090327
111600090327       //?conto per idjob e papà
111700090327          exsr sr_contas03;
111800090327
111900090327       //?scrivo subfile
112000090327          exsr sr_wrts03;
112100090327
112200090327          enddo;
112300110303
112400110303       //?scrivo riga di totale nel subfile se ci sono più figli
112500110303          IF  nrr3 > 1;
112600110303            *in56 = *on;
112700110303            exsr sr_wrtT03;
112800110303            *in56 = *off;
112900110303          ENDIF;
113000090327
113100090327       //?indicatore per fine file
113200090327          *in32 = *on;
113300090327
113400090327          exec sql close ksc;
113500090327
113600090327        endsr;
113700090327
113800090327       // ----------------------------------------------------------------------
113900090327       //?Conto
114000090327       // ----------------------------------------------------------------------
114100090327        begsr sr_contas03;
114200090327
114300090327       //?immagini estratte
114400090327          exec sql
114500110303            select count(distinct lacnim) into :s03nest
114600090327              from tilac00f where lacidjob = :s03idjob and
114700090327              lacksu = :s03ksu and lacksc = :s03ksc and
114800140509              lacela <> '33' and lacela <> '34';
114900090327          if sqlcod <> 0;
115000090327            clear s03nest;
115100090327          endif;
115200090327       //?immagini elaborate
115300090327          exec sql
115400110303            select count(distinct lacnim) into :s03nela
115500090327              from tilac00f where lacidjob = :s03idjob and
115600090327              lacksu = :s03ksu and lacksc = :s03ksc and
115700140509              lacela <> '33' and lacela <> '34' and lacela <> '00';
115800090327          if sqlcod <> 0;
115900090327            clear s03nela;
116000090327          endif;
116100090327       //?addebiti
116200090402          if s03tad <> 'N';
116300090402            exec sql
116400110303              select count(distinct lacnim) into :s03ntas
116500090402                from tilac00f where lacidjob = :s03idjob and
116600090402                lacksu = :s03ksu and lacksc = :s03ksc and
116700140509                lacela <> '33' and lacela <> '34' and lacela = '11';
116800090402            if sqlcod <> 0;
116900090402              clear s03ntas;
117000090402            endif;
117100090402          else;
117200090402            clear s03ntas;
117300090402          endif;
117400090327       //?controllo se ci sono dei rcd a 'x2' o a '9x'
117500090327          exec sql
117600110303            select count(distinct lacnim) into :s03nerr
117700090327              from tilac00f where lacidjob = :s03idjob and
117800090327              lacksu = :s03ksu and lacksc = :s03ksc and
117900090327              (substr(lacela, 2, 1) = '2' or
118000090327               substr(lacela, 1, 1) = '9');
118100090327          if sqlcod <> 0;
118200090327            clear s03nerr;
118300090327          endif;
118400090327
118500090327        endsr;
118600090327
118700090327       // ----------------------------------------------------------------------
118800090327       //?Scrivo il subfile
118900090327       // ----------------------------------------------------------------------
119000090327        begsr sr_wrts03;
119100090327
119200090327       //?imposto i campi del control
119300090327          v3cksu = s03ksu;
119400090327       //?ragione sociale da tabella LAC
119500090327          exec sql
119600090327            select tbeuni into :tbeuni from tntbe00f
119700090327              where tbecod = 'LAC' and tbeatb = ' ' and
119800090327                    substr(tbeke1, 1, 7) = digits(:s03ksu);
119900090327       //?non trovo il rcd corrispondente
120000090327          if sqlcod <> 0;
120100090327            clear tbeuni;
120200090327          endif;
120300090327          dlac = tbeuni;
120400090327          v3crag = §lacrag;
120500090327
120600090327       //?imposto i campi del subfile
120700090327       //?data
120800090327          v3sdata = %subst(s03tim:7:2) + '/' +
120900090327                    %subst(s03tim:5:2) + '/' +
121000090327                    %subst(s03tim:1:4);
121100090327       //?codice estrazione
121200090327          v3sksc = s03ksc;
121300090327       //?ragione sociale da tabella LAC
121400090327          exec sql
121500090327            select tbeuni into :tbeuni from tntbe00f
121600090327              where tbecod = 'LAC' and tbeatb = ' ' and
121700090327                    substr(tbeke1, 1, 7) = digits(:s03ksc);
121800090327       //?non trovo il rcd corrispondente
121900090327          if sqlcod <> 0;
122000090327            clear tbeuni;
122100090327          endif;
122200090327          dlac = tbeuni;
122300090327          v3srag = §lacrag;
122400090327       //?nr. immagini estratte
122500090327          v3sestr = s03nest;
122600090327       //?nr. immagini elaborate
122700090327          v3selab = s03nela;
122800090330       //?codice + ragione sociale in rosso se non elaborazione non terminata
122900090330          *in55 = (s03nest <> s03nela);
123000090327       //?se ho trovato dei rcd a 'x2' o a '9x'
123100090327          *in52 = (s03nerr > *zeros);
123200090327       //?CD si/no
123300090327          v3scd = v2scd;
123400090327       //?TAS si/no
123500090327          select;
123600090402            when s03tad = 'N';
123700090402              v3stas = 'NP';
123800090327            when s03ntas = *zeros;
123900090327              v3stas = 'NO';
124000090327              eval *in53 = *off;
124100090327            when s03ntas = s03nest;
124200090327              v3stas = 'SI';
124300090327              eval *in53 = *off;
124400090327            when s03ntas <> s03nest;
124500090327              v3stas = 'SI';
124600090327              eval *in53 = *on;
124700090327          endsl;
124800090327       //?directory
124900090327          v3sdir = s03dir;
125000110303
125100110303       //?sommo le immagini estratte e le elaborate
125200110303          T03nest += S03nest;
125300110303          T03nela += S03nela;
125400090327
125500090327          nrr3 = nrr3 + 1;
125600090327          write sb04s03;
125700090327
125800090327        endsr;
125900110303
126000110303       // ----------------------------------------------------------------------
126100110303       //?Scrivo riga di totale nel subfile.
126200110303       // ----------------------------------------------------------------------
126300110303        begsr sr_wrtT03;
126400110303
126500110303          *in52 = *off;
126600110303          *in53 = *off;
126700110303          *in55 = *off;
126800110303          clear v3sdata;
126900110303          clear v3sksc;
127000110303          v3srag = 'T O T A L E ';
127100110303          v3sestr = T03nest;
127200110303          v3selab = T03nela;
127300110303          clear v3scd;
127400110303          clear v3stas;
127500110303          clear v3stas;
127600110303          clear v3sdir;
127700110303          nrr3 = nrr3 + 1;
127800110303          write sb04s03;
127900110303
128000110303        endsr;
128100090327
128200090327       // ----------------------------------------------------------------------
128300090327       //?Ok fatto cd aggiorno TILAC
128400090327       // ----------------------------------------------------------------------
128500090327        begsr sr_okcd;
128600090327
128700090327          exec sql
128800090327            update tilac00f
128900090327            set lacutecd = :knmus, lactimecd = :wtimestp
129000090327            where
129100090327            lacidjob = :w2idjob and
129200110714              lacksu = :w2cksu  and lacela in('33', '34');
129300090327
129400090327        endsr;
129500110714
129600110714       // ----------------------------------------------------------------------
129700110714       //?Ok faccio il file e aggiorno TILAC
129800110714       // ----------------------------------------------------------------------
129900110714        begsr sr_okfile;
130000110714
130100110714       //?richiamo pgm per creare il FILE CSV da aggiungere al CD
130200110721          clear TNSB06DS;
130300110721          ISB06ksu   = V2Sksu;
130400110721          ISB06idjob = V2Sidjob;
130500110721          ISB06dir   = V2Sdir;
130600110721          tnsb06r (kpjba : TNSB06DS);
130700110721          IF  OSB06esito <> *zeros;
130800110721            leavesr;
130900110721          ENDIF;
131000110714
131100110714          exec sql
131200110714            update tilac00f
131300110714            set lacela = '34'
131400110714            where
131500110714            lacidjob = :w3idjob and
131600110714              lacksu = :w3cksu  and lacela = '33';
131700110714
131800110714        endsr;
131900090327
132000090324       // ----------------------------------------------------------------------
132100090324       //?Fine programma
132200090324       // ----------------------------------------------------------------------
132300090327        begsr sr_fine;
132400090324
132500090327          *inlr = *on;
132600090327          return;
132700090324
132800090327        endsr;
132900090324
133000090324** MSG  Lungh. 78                                                            *
133100090326Immettere almeno una delle due date                                           01
133200090326Data errata                                                                   02
133300090326Range di date errato                                                          03
133400090326Immettere SOLO caratteri numerici                                             04
133500090326Codice errato                                                                 05
133600090330E' possibile selezionare solo una parzializzazione                            06
