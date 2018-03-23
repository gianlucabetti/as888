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
004500090324     d rrnlast         s                   like(recsf2)
004600090327     d s02dir          s                   like(lacdir)
004700110714     d s02ela          s                   like(lacela)
004800090327     d s02idjob        s                   like(lacidjob)
004900090327     d s02ksu          s                   like(lacksu)
005000090402     d s02nela         s             10i 0
005100090402     d s02nerr         s             10i 0
005200090402     d s02nest         s             10i 0
005300090402     d s02ntas         s             10i 0
005400090402     d s02tad          s                   like(lactad)
005500090327     d s02tim          s                   like(lactim)
005600090327     d s02utecd        s                   like(lacutecd)
005700090327     d s03dir          s                   like(lacdir)
005800090327     d s03idjob        s                   like(lacidjob)
005900090327     d s03ksc          s                   like(lacksc)
006000090327     d s03ksu          s                   like(lacksu)
006100090402     d s03nela         s             10i 0
006200090402     d s03nerr         s             10i 0
006300090402     d s03nest         s             10i 0
006400090402     d s03ntas         s             10i 0
006500090402     d s03tad          s                   like(lactad)
006600090327     d s03tim          s                   like(lactim)
006700110303     d T03nela         s             10i 0
006800110303     d T03nest         s             10i 0
006900090324     d wdata           s              8  0
007000090326     d wdatada         s              8  0
007100090326     d wdataal         s              8  0
007200090327     d wtimestp        s             14
007300090324     d xx              s              3  0
007400090327     d $ends02         s               n
007500090327     d $ends03         s               n
007600090330     d $endw02         s               n
007700110714     d $endw03         s               n
007800090324     d $fine           s               n
007900090330     d $okcd           s               n
008000110714     d $okfile         s               n
008100090327     d $recnook        s               n
008200090324     d $video          s             10
008300090324
008400090324      // ? S C H I E R E ?
008500090324     d msg             s             78    dim(15) ctdata perrcd(1)
008600090324
008700090324      // ? D S   I N T E R N E / E S T E R N E ?
008800090324     d wlbdat          ds                  inz
008900090324     d  g02dat                 1      8  0
009000090324     d  g02inv                 9     16  0
009100090324     d  g02err                17     17
009200090324     d  g02tgi                18     22  0
009300090324
009400090324     d §azute        e ds                  extname(azute00f)
009500090324     d                                     dtaara
009600090324     d §datiute      e ds                  extname(ddatiute)
009700090324     d                                     dtaara
009800090324     d dlac          e ds
009900090324     d kpjba         e ds
010000090324     d tilacds       e ds                  extname(tilac00f)
010100090324     d tibs34ds      e ds                  inz
010200090324     d tntbeds       e ds                  extname(tntbe00f)
010300110721     d tnsb06ds      e ds
010400090327
010500090327      // - Interrogazione tabella LAC dettaglio
010600090327     d tntb46ds      e ds                  inz
010700090327     d   b46opz      e                     inz('5')
010800090327     d   b46err      e                     inz(*off)
010900090324
011000090324       // - Struttura per passaggio dati ad interrogazione tabella
011100090324     d param01         ds
011200090324     d  p01cod                        7  0 inz
011300090324     d  p01ord                        1
011400090324     d  p01ksu                        7  0 inz
011500090324     d  p01ke1                        7
011600090324     d  p01ke2                       15
011700090324     d  p01rit                        1
011800090324
011900090324     d                sds
012000090324     d  vtcpgm                 1     10
012100090324
012200090324      // ? COSTANTI ?
012300090324     d digitn          c                   const('1234567890')
012400090324
012500090324      // ? PROTOTIPI ?
012600090324      /copy gaitrasrc/srcprotopr,tibs34r
012700090324      /copy gaitrasrc/srcprotopr,xsrda8
012800090324
012900090327       // - Visualizzazione tabella LAC
013000090327     d tntb46r         pr                  extpgm('TNTB46R')
013100090327     d  kpjba                              likeds(KPJBA)
013200090327
013300090324       // - Interrogazione tabella LAC
013400090324     d tntb461r        pr                  extpgm('TNTB461R')
013500090324     d  kpjba                              likeds(KPJBA)
013600110721
013700110721       // - Pgm creazione file di work per elenco Pod Image
013800110721     d tnsb06r         pr                  extpgm('TNSB06R')
013900110721     d  kpjba                              likeds(KPJBA)
014000110721     d  tnsb06ds                           likeds(TNSB06DS)
014100090324
014200090324      // ----------------------------------------------------------------------
014300090324
014400090324     c     *entry        plist
014500090324     c                   parm                    kpjba
014600090324
014700090324      /free
014800090324
014900090324       //?operazioni iniziali
015000090326          exsr routinz;
015100090326
015200090326       //?imposto i dati di dft nella prima videata
015300090326       //?per ora solo all'inizio del programma poi vediamo
015400090326          exsr inzd01;
015500090324
015600090326          dow not $fine;
015700090324
015800090326         //?gestione delle videate
015900090326            select;
016000090324
016100090326         //?gestione prima videata
016200090326             when $video = 'D01';
016300090326               exsr gesd01;
016400090324
016500090326         //?gestione subfile
016600090326             when $video = 'S02';
016700090326               exsr gess02;
016800090327
016900090327         //?gestione subfile
017000090327             when $video = 'S03';
017100090327               exsr gess03;
017200090327
017300090327         //?gestione visualizzazione tabella LAC
017400090327             when $video = 'LAC';
017500090327               exsr geslac;
017600090324
017700090326            endsl;
017800090326          enddo;
017900090324
018000090326          *inlr = *on;
018100090324
018200090324       // ----------------------------------------------------------------------
018300090324       //?Operazioni iniziali.
018400090324       // ----------------------------------------------------------------------
018500090324       begsr routinz;
018600090324
018700090324         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
018800090324
018900090324       //?recupero dei dati utente
019000090324          in(e) §azute;
019100090324          if not %error;
019200090326            in(e) §datiute;
019300090324          endif;
019400090324          if %error or rsut = *blanks;
019500090326            tibs34r(tibs34ds);
019600090326            in §azute;
019700090326            in §datiute;
019800090324          endif;
019900090324
020000090324       //?imposto la data del giorno
020100090324          wdata = %dec(%date());
020200090324          dataiso = %date(wdata:*iso);
020300090324          dataeur = dataiso;
020400090327
020500090327       //?imposto il time del job
020600090327          wtimestp = %char(%timestamp:*iso0);
020700090324
020800090324       //?gestione video D01
020900090326          $video = 'D01';
021000090326          $fine =*off;
021100090324
021200090324       endsr;
021300090326
021400090326       // ----------------------------------------------------------------------
021500090326       //?Imposto i dati di dflt nella prima videata.
021600090326       // ----------------------------------------------------------------------
021700090326        begsr inzd01;
021800090326
021900090326          clear v1datada;
022000090326          v1dataal = %dec(dataeur);
022100090330          v1cd = 'X';
022200090326
022300090326        endsr;
022400090324
022500090324       // ----------------------------------------------------------------------
022600090324       //?Gestione prima videata (parametri)
022700090324       // ----------------------------------------------------------------------
022800090326        begsr gesd01;
022900090324
023000090324       //?pulisco la videata
023100090324          if not *in28 and not *in90;
023200090330            exsr inzd01;
023300090326            exsr sr_puld01;
023400090324          endif;
023500090324
023600090324       //?emetto la videata
023700090324          write sb04t01;
023800090324          exfmt sb04d01;
023900090324
024000090324       //?spengo indicatori di comodo
024100090324          *in28 = *off;
024200090324          *in90 = *off;
024300090324       //?pulisco campo messaggi
024400090324          clear v1cmsg;
024500090324
024600090324       //?esecuzione comandi
024700090326          select;
024800090324
024900090326         //?f3=fine
025000090326            when *inkc;
025100090326              exsr sr_fine;
025200090326            other;
025300090324
025400090326           //?controllo i dati della videata
025500090326              exsr sr_contrd01;
025600090326              if *in28 or *in90;
025700090326                leavesr;
025800090326              endif;
025900090326          endsl;
026000090324
026100090326       //?passo al subfile
026200090326          exsr sr_s02;
026300090326          $video = 'S02';
026400090324
026500090326        endsr;
026600090324
026700090324       // ----------------------------------------------------------------------
026800090324       //?Gestione subfile
026900090324       // ----------------------------------------------------------------------
027000090326        begsr gess02;
027100090324
027200090324       //?ho caricato dati emetto il subfile
027300090326          if nrr2 > 0;
027400090326            exsr sr_sfl02;
027500090326          endif;
027600090324
027700090324       //?non ho caricato dati emetto videata di segnalazione
027800090326          if nrr2 = 0;
027900090326            exsr sr_d02;
028000090326          endif;
028100090324
028200090326        endsr;
028300090324
028400090324       // ----------------------------------------------------------------------
028500090324       //?Pulizia prima videata (parametri)
028600090324       // ----------------------------------------------------------------------
028700090326        begsr sr_puld01;
028800090324
028900090326          clear v1cksu;
029000090326          clear v1dksu;
029100090326          clear v1estr;
029200090326          clear v1elab;
029300090326          clear v1tas;
029400090324
029500090326        endsr;
029600090324
029700090324       // ----------------------------------------------------------------------
029800090324       //?Controllo dati prima videata (parametri)
029900090324       // ----------------------------------------------------------------------
030000090326        begsr sr_contrd01;
030100090324
030200090324       //?spengo indicatori di errore
030300090326          *in41 = *off;
030400090326          *in42 = *off;
030500090326          *in43 = *off;
030600090330          *in44 = *off;
030700090330          *in45 = *off;
030800090330          *in46 = *off;
030900090324
031000090324       //?pulisco campi di comodo
031100090326          clear wdatada;
031200090326          clear wdataal;
031300090324
031400090324       //?controllo data
031500090326          exsr sr_contrdata;
031600090326          if *in28;
031700090326            leavesr;
031800090326          endif;
031900090324
032000090326       //?controllo padre
032100090326          if v1cksu <> *blanks;
032200090326            exsr sr_contrksu;
032300090326            if *in28 or *in90;
032400090326              leavesr;
032500090326            endif;
032600090326          endif;
032700090330
032800090330       //?controllo parzializzazioni
032900090330          exsr sr_contrpar;
033000090330          if *in28;
033100090330            leavesr;
033200090330          endif;
033300090324
033400090326        endsr;
033500090324
033600090324       // ----------------------------------------------------------------------
033700090326       //?Subfile
033800090324       // ----------------------------------------------------------------------
033900090326        begsr sr_s02;
034000090324
034100090324       //?pulisco il subfile
034200090326          exsr sr_puls02;
034300090324
034400090327          $ends02 = *off;
034500090326
034600110714       //?leggo tilac00f solo rcd con lacela = '33' o '34' Job sottomesso
034700090326       //?tutti i papà
034800090326          if v1cksu = *blanks;
034900090326            exec sql
035000090327              declare ksu cursor for
035100110714              select lacidjob, lactim, lacksu, lacutecd, lacdir, lactad,
035200110714                     lacela
035300110714              from tilac00f where lacela in('33', '34') and
035400090326              substr(lactim, 1, 8) between digits(:wdatada)
035500090326              and digits(:wdataal)
035600090326              order by lactim, lacksu, lacidjob;
035700090326
035800090326            exec sql
035900090327              open ksu;
036000090326              if sqlcode < 0;
036100090327                $ends02 = *on;
036200090326              ENDIF;
036300090326          endif;
036400090326
036500090326       //?leggo tilac00f raggruppato per id job e codice estrazione
036600090326       //?un solo codice raggruppamento chiesto
036700090326          if v1cksu > *zeros;
036800090326            exec sql
036900090327              declare ksu1 cursor for
037000110714              select lacidjob, lactim, lacksu, lacutecd, lacdir, lactad,
037100110714                     lacela
037200110714              from tilac00f where lacela in('33', '34') and
037300090326              substr(lactim, 1, 8) between digits(:wdatada) and
037400090326              digits(:wdataal) and digits(lacksu) = :v1cksu
037500090326              order by lactim, lacksu, lacidjob;
037600090326
037700090326            exec sql
037800090327              open ksu1;
037900090326              if sqlcode < 0;
038000090327                $ends02 = *on;
038100090326              ENDIF;
038200090326          endif;
038300090326
038400090324       //?carico il subfile
038500090326          exsr sr_cars02;
038600090324
038700090326        endsr;
038800090324
038900090324       // ----------------------------------------------------------------------
039000090326       //?Emetto il subfile
039100090324       // ----------------------------------------------------------------------
039200090326        begsr sr_sfl02;
039300090324
039400090330       //?flag creazione cd
039500090330          $okcd = *off;
039600110714       //?flag creazione file
039700110714          $okfile = *off;
039800090330
039900090324       //?emetto la videata
040000090326          write sb04t01;
040100090326          write sb04z02;
040200090326          exfmt sb04c02;
040300090324
040400090324       //?spengo indicatori di comodo
040500090326          *in28 = *off;
040600090324       //?pulisco campo messaggi
040700090326          clear v2cmsg;
040800090324       //?esecuzione comandi
040900090326          select;
041000090324
041100090326         //?f3=fine
041200090326            when *inkc;
041300110301              exsr sr_fine;
041400110301         //?f5=refresh
041500110301            when *inke;
041600110301              exsr sr_s02;
041700090326         //?f12=ritorno
041800090326            when *inkl;
041900090326              $video = 'D01';
042000090326              leavesr;
042100090326            other;
042200090326         //?controllo opzione subfile
042300090326            exsr sr_ctrs02;
042400090330         //?se creati CD ricarico il subfile
042500110714         //?anche se creato il file
042600110714            if $okcd or $okfile;
042700090330              exsr sr_s02;
042800090330            endif;
042900090326          endsl;
043000090324
043100090326        endsr;
043200090324
043300090324       // ----------------------------------------------------------------------
043400090324       //?Emetto video di segnalazione dati non caricati
043500090324       // ----------------------------------------------------------------------
043600090326        begsr sr_d02;
043700090324
043800090324       //?emetto la videata
043900090326          write sb04t01;
044000090326          exfmt sb04d02;
044100090324
044200090324       //?spengo indicatori di comodo
044300090326          *in28 = *off;
044400090326          *in90 = *off;
044500090324       //?pulisco campo messaggi
044600090326          clear v2cmsg;
044700090324       //?esecuzione comandi
044800090326          select;
044900090324
045000090326         //?f3=fine
045100090326            when *inkc;
045200090326              exsr sr_fine;
045300090326         //?f12=ritorno
045400090326            when *inkl;
045500090326              $video = 'D01';
045600090326              leavesr;
045700090326            other;
045800090326          endsl;
045900090324
046000090326        endsr;
046100090324
046200090324       // ----------------------------------------------------------------------
046300090324       //?Controllo data
046400090324       // ----------------------------------------------------------------------
046500090326        begsr sr_contrdata;
046600090324
046700090324       //?la data è obbligatoria
046800090326          if v1datada = *zeros and v1dataal = *zeros;
046900090326            *in28 = *on;
047000090326            *in41 = *on;
047100090326            v1cmsg = msg(01);
047200090326            leavesr;
047300090326          endif;
047400090324
047500090324       //?deve essere una data valida
047600090326       //?data da
047700090326          if v1datada <> *zeros;
047800090326            clear wlbdat;
047900090326            g02dat = v1datada;
048000090326            xsrda8(wlbdat);
048100090326            if g02err = '1';
048200090326              *in28 = *on;
048300090326              *in41 = *on;
048400090326              v1cmsg = msg(02);
048500090326              leavesr;
048600090326            endif;
048700090326            v1datada = g02dat;
048800090326            wdatada  = g02inv;
048900090326          endif;
049000090326       //?data al
049100090326          if v1dataal <> *zeros;
049200090326            clear wlbdat;
049300090326            g02dat = v1dataal;
049400090326            xsrda8(wlbdat);
049500090326            if g02err = '1';
049600090326              *in28 = *on;
049700090326              *in42 = *on;
049800090326              v1cmsg = msg(02);
049900090326              leavesr;
050000090326            endif;
050100090326            v1dataal = g02dat;
050200090326            wdataal  = g02inv;
050300090326          endif;
050400090324
050500090326       //?imposto in automatico la data mancante
050600090326          if v1dataal = *zeros;
050700090326            v1dataal = v1datada;
050800090326            wdataal  = wdatada;
050900090326          endif;
051000090326       //?controllo range date
051100110502          if wdatada > wdataal;
051200090326            *in28 = *on;
051300090326            *in41 = *on;
051400090326            v1cmsg = msg(03);
051500090326            leavesr;
051600090326          endif;
051700090324
051800090326        endsr;
051900090324
052000090324       // ----------------------------------------------------------------------
052100090326       //?Controllo padre
052200090324       // ----------------------------------------------------------------------
052300090326        begsr sr_contrksu;
052400090324
052500090326       //?Ricerca codice cliente
052600090326          if %scan('?':v1cksu) > *zeros;
052700090326            clear  param01;
052800090326            p01ord = '0';
052900090326            kpjbu = param01;
053000090326            tntb461r (kpjba);
053100090326            param01 = kpjbu;
053200090324
053300090326            if p01ke1 <> *zeros;
053400090326              v1cksu = p01ke1;
053500090326              *in43 = *on;
053600090326              *in90 = *on;
053700090326            endif;
053800090326          endif;
053900090324
054000090326       //?Controllo immissione solo caratteri numerici
054100090326          if %check(digitn:v1cksu) > *zeros;
054200090326            *in28 = *on;
054300090326            *in43 = *on;
054400090326            v1cmsg = msg(04);
054500090326            leavesr;
054600090326          endif;
054700090324
054800090326       //?Controllo esistenza codice cliente in tabella LAC
054900090326          exec sql
055000090326            select tbeuni into :tbeuni from tntbe00f
055100090326            where tbecod = 'LAC' and tbeatb = '' and
055200090326            substr(tbeke1, 1, 7) = :v1cksu;
055300090326       //?non trovo il rcd corrispondente
055400090326          if sqlcod <> 0;
055500090326            clear tbeuni;
055600090326            *in28 = *on;
055700090326            *in43 = *on;
055800090326            v1cmsg = msg(05);
055900090326            leavesr;
056000090326          endif;
056100090324
056200090326          dlac = tbeuni;
056300090326          v1dksu = §lacrag;
056400090324
056500090326        endsr;
056600090330
056700090330       // ----------------------------------------------------------------------
056800090330       //?Controllo parzializzazioni
056900090330       // ----------------------------------------------------------------------
057000090330        begsr sr_contrpar;
057100090330
057200090330       //?le parzializzazioni non sono in alternativa
057300090330          if v1cd <> *blanks and (v1estr <> *blanks or v1elab <> *blanks or
057400090330                                  v1tas <> *blanks);
057500090330            *in28 = *on;
057600090330            *in44 = *on;
057700090330            v1cmsg = msg(06);
057800090330            leavesr;
057900090330          endif;
058000090330          if v1estr <> *blanks and (v1elab <> *blanks or v1tas <> *blanks);
058100090330            *in28 = *on;
058200090330            *in45 = *on;
058300090330            v1cmsg = msg(06);
058400090330            leavesr;
058500090330          endif;
058600090330          if v1elab <> *blanks and v1tas <> *blanks;
058700090330            *in28 = *on;
058800090330            *in46 = *on;
058900090330            v1cmsg = msg(06);
059000090330            leavesr;
059100090330          endif;
059200090330
059300090330       endsr;
059400090324
059500090324       // ----------------------------------------------------------------------
059600090324       //?Pulisco il subfile
059700090324       // ----------------------------------------------------------------------
059800090326        begsr sr_puls02;
059900090324
060000090326          clear nrr2;
060100090326          clear rrnlast;
060200090324
060300090326          *in20 = *off;
060400090326          *in21 = *off;
060500090326          write sb04c02;
060600090326          *in20 = *on;
060700090326          *in21 = *on;
060800090324
060900090326          recsf2 = 1;
061000090326          *in31 = *on;
061100090324
061200090326        endsr;
061300090324
061400090324       // ----------------------------------------------------------------------
061500090324       //?Carico il subfile
061600090324       // ----------------------------------------------------------------------
061700090326        begsr sr_cars02;
061800090324
061900090327          dow not $ends02;
062000090324
062100090324       //?leggo i job
062200090325       //?tutti i codici raggruppamento
062300090326            if v1cksu = *blanks;
062400090326              exec sql
062500090327                fetch next from ksu into :s02idjob, :s02tim,
062600090327                                         :s02ksu, :s02utecd,
062700110714                                         :s02dir, :s02tad,
062800110714                                         :s02ela;
062900090326            endif;
063000090325       //?un solo codice raggruppamento chiesto
063100090326            if v1cksu > *zeros;
063200090326              exec sql
063300090327                fetch next from ksu1 into :s02idjob, :s02tim,
063400090327                                          :s02ksu, :s02utecd,
063500110714                                          :s02dir, :s02tad,
063600110714                                          :s02ela;
063700090326            endif;
063800090324
063900090324       //?fine file o errore sql esco
064000090326            if sqlcod = 100 or sqlcod < 0;
064100090327              $ends02 = *on;
064200090326              leave;
064300090326            endif;
064400090324
064500090326       //?conto per idjob e papà
064600090327          exsr sr_contas02;
064700090326
064800090324       //?scrivo subfile
064900090326          exsr sr_wrts02;
065000090324
065100090326          enddo;
065200090324
065300090324       //?indicatore per fine file
065400090326          *in31 = *on;
065500090324       //?imposto il numero di record caricati
065600090326          rrnlast = nrr2;
065700090324
065800090325       //?chiudo il cursore
065900090326          if v1cksu = *blanks;
066000090327            exec sql close ksu;
066100090326          endif;
066200090326          if v1cksu > *zeros;
066300090327            exec sql close ksu1;
066400090326          endif;
066500090324
066600090326        endsr;
066700090324
066800090326       // ----------------------------------------------------------------------
066900090326       //?Conto
067000090326       // ----------------------------------------------------------------------
067100090327        begsr sr_contas02;
067200090326
067300090326       //?immagini estratte
067400090326          exec sql
067500090716            select count(distinct lacnim) into :s02nest
067600090327              from tilac00f where lacidjob = :s02idjob and
067700110714              lacksu = :s02ksu and lacela not in('33', '34');
067800090326          if sqlcod <> 0;
067900090327            clear s02nest;
068000090326          endif;
068100090326       //?immagini elaborate
068200090326          exec sql
068300090716            select count(distinct lacnim) into :s02nela
068400090327              from tilac00f where lacidjob = :s02idjob and
068500110714              lacksu = :s02ksu and lacela not in('33', '34', '00');
068600090326          if sqlcod <> 0;
068700090327            clear s02nela;
068800090326          endif;
068900090326       //?addebiti
069000090402          if s02tad <> 'N';
069100090402            exec sql
069200090716              select count(distinct lacnim) into :s02ntas
069300090402                from tilac00f where lacidjob = :s02idjob and
069400110714                lacksu = :s02ksu and lacela not in('33', '34') and
069500090402                lacela = '11';
069600090402            if sqlcod <> 0;
069700090402              clear s02ntas;
069800090402            endif;
069900090402          else;
070000090402            clear s02ntas;
070100090402          endif;
070200090326       //?controllo se ci sono dei rcd a 'x2' o a '9x'
070300090326          exec sql
070400090327            select count(*) into :s02nerr
070500090327              from tilac00f where lacidjob = :s02idjob and
070600090327              lacksu = :s02ksu and
070700090326              (substr(lacela, 2, 1) = '2' or
070800090326               substr(lacela, 1, 1) = '9');
070900090326          if sqlcod <> 0;
071000090327            clear s02nerr;
071100090326          endif;
071200090326
071300090326        endsr;
071400090326
071500090324       // ----------------------------------------------------------------------
071600090326       //?Scrivo il subfile
071700090324       // ----------------------------------------------------------------------
071800090326        begsr sr_wrts02;
071900090327
072000090327          $recnook = *off;
072100090327
072200090327       //?controllo le parzializzazioni
072300090327          exsr sr_parzializza;
072400090327          if $recnook;
072500090327            leavesr;
072600090327          endif;
072700090324
072800090326       //?imposto i campi del subfile
072900090327       //?idjob
073000090327          v2sidjob = s02idjob;
073100090326       //?data
073200090327          v2sdata = %subst(s02tim:7:2) + '/' +
073300090327                    %subst(s02tim:5:2) + '/' +
073400090327                    %subst(s02tim:1:4);
073500090326       //?codice papà e cerco la ragione sociale da tabella LAC
073600090327          v2sksu = s02ksu;
073700090326       //?ragione sociale da tabella LAC
073800090326          exec sql
073900090326            select tbeuni into :tbeuni from tntbe00f
074000090326              where tbecod = 'LAC' and tbeatb = ' ' and
074100090327                    substr(tbeke1, 1, 7) = digits(:s02ksu);
074200090324       //?non trovo il rcd corrispondente
074300090324          if sqlcod <> 0;
074400090326            clear tbeuni;
074500090324          endif;
074600090326          dlac = tbeuni;
074700090326          v2srag = §lacrag;
074800090326       //?nr. immagini estratte
074900090327          v2sestr = s02nest;
075000090326       //?nr. immagini elaborate
075100090327          v2selab = s02nela;
075200090330       //?codice + ragione sociale in rosso se non elaborazione non terminata
075300090330          *in54 = (s02nest <> s02nela);
075400090326       //?se ho trovato dei rcd a 'x2' o a '9x'
075500090327          *in50 = (s02nerr > *zeros);
075600110714       //?Creare file di riepilogo
075700110714          IF  §LACpgm <> *blanks and s02ela = '33';
075800110714            v2sfile = 'SI';
075900110714          ENDIF;
076000110714          IF  §LACpgm <> *blanks and s02ela = '34';
076100110714            v2sfile = 'OK';
076200110714          ENDIF;
076300110714          IF  §LACpgm = *blanks;
076400110714            v2sfile = 'NO';
076500110714          ENDIF;
076600110714          v2lacela = s02ela;
076700090326       //?CD si/no
076800090327          if s02utecd <> *blanks;
076900090326            v2scd = 'SI';
077000090326          else;
077100090326            v2scd = 'NO';
077200090326          endif;
077300090326       //?TAS si/no
077400090326          select;
077500090402            when s02tad = 'N';
077600090402              v2stas = 'NP';
077700090327            when s02ntas = *zeros;
077800090327              v2stas = 'NO';
077900090330              *in51 = *off;
078000090327            when s02ntas = s02nest;
078100090326              v2stas = 'SI';
078200090330              *in51 = *off;
078300090327            when s02ntas <> s02nest;
078400090326              v2stas = 'SI';
078500090330              *in51 = *on;
078600090326          endsl;
078700090327       //?directory
078800090327          v2sdir = s02dir;
078900090330       //?indicatori del subfile
079000090330          v2sin50 = '0';
079100090330          v2sin51 = '0';
079200090330          v2sin54 = '0';
079300090330          if *in50;
079400090330            v2sin50 = '1';
079500090330          endif;
079600090330          if *in51;
079700090330            v2sin51 = '1';
079800090330          endif;
079900090330          if *in54;
080000090330            v2sin54 = '1';
080100090330          endif;
080200090326
080300090326          nrr2 = nrr2 + 1;
080400090324          write sb04s02;
080500090324
080600090326        endsr;
080700090327
080800090327       // ----------------------------------------------------------------------
080900090327       //?Controllo le parzializzazioni richieste in prima videata
081000090327       // ----------------------------------------------------------------------
081100090327        begsr sr_parzializza;
081200090327
081300090327       //?solo NON estratti
081400090327          if v1estr <> *blanks and s02nest <> *zeros;
081500090327            $recnook = *on;
081600090327            leavesr;
081700090327          endif;
081800090327
081900090327       //?solo elaborazioni NON terminate
082000090327          if v1elab <> *blanks and s02nest = s02nela;
082100090327            $recnook = *on;
082200090327            leavesr;
082300090327          endif;
082400090327
082500090327       //?solo CD NON completati
082600090327          if v1cd <> *blanks and s02utecd <> *blanks;
082700090327            $recnook = *on;
082800090327            leavesr;
082900090327          endif;
083000090327
083100090327       //?solo NON addebitati
083200090327          if v1tas <> *blanks and s02ntas <> *zeros;
083300090327            $recnook = *on;
083400090327            leavesr;
083500090327          endif;
083600090327
083700090327       endsr;
083800090324
083900090324       // ----------------------------------------------------------------------
084000090324       //?Controllo opzione subfile
084100090324       // ----------------------------------------------------------------------
084200090326        begsr sr_ctrs02;
084300090324
084400090324       //?controllo l'opzione immessa
084500090326          for xx = 1 to rrnlast;
084600090326            chain xx sb04s02;
084700090330            $endw02 = *off;
084800110714            $endw03 = *off;
084900090327            recsf2 = xx;
085000090327            select;
085100090327           //?visualizza dettaglio
085200090327              when v2sopz = '5';
085300090909             //?errore se non esistono dati estratti
085400090909                if v2sestr = 0 and v2selab = 0;
085500090909                  *in28 = *on;
085600090909                  v2cmsg = 'Dati non estratti!!';
085700090909                else;
085800090909                  $video = 'S03';
085900090909                  exsr gess03;
086000090909                  clear v2sopz;
086100090909                endif;
086200110714           //?Creazione file riepilogo
086300110714              when v2sopz = 'F';
086400110714             //?creo file se richiesto
086500110714                if  v2sfile = 'NO';
086600110714                  *in28 = *on;
086700110714                  v2cmsg = 'Opzione errata';
086800110714                endif;
086900110714                if  not *in28;
087000110714                  exsr gesw03;
087100110714                  clear v2sopz;
087200110714                endif;
087300090327           //?OK creato CD
087400090327              when v2sopz = 'C';
087500110714             //?errore se file da creare e non ancora creato
087600110714                if v2sfile = 'SI' and v2lacela = '33';
087700110714                  *in28 = *on;
087800110714                  v2cmsg = 'Prima creare il file!';
087900110714                endif;
088000110714                if  not *in28;
088100090327             //?errore se cd già creato
088200090327                if v2scd = 'SI';
088300090327                  *in28 = *on;
088400090327                  v2cmsg = 'CD già creato!!';
088500090327                else;
088600090327             //?creo cd
088700090327                  exsr gesw02;
088800090327                  clear v2sopz;
088900090327                endif;
089000110714                endif;
089100090327           //?visualizzo tabella LAC
089200090327              when v2sopz = 'T';
089300090327                $video = 'LAC';
089400090327                exsr geslac;
089500090327                clear v2sopz;
089600090327            endsl;
089700090327
089800090330            *in50 = (v2sin50 = '1');
089900090330            *in51 = (v2sin51 = '1');
090000090330            *in54 = (v2sin54 = '1');
090100090327            update sb04s02;
090200090326          endfor;
090300090324
090400090326        endsr;
090500090324
090600090324       // ----------------------------------------------------------------------
090700090327       //?Gestione subfile
090800090324       // ----------------------------------------------------------------------
090900090327        begsr gess03;
091000090327
091100090327       //?subfile
091200090327          exsr sr_s03;
091300090327
091400090327       //?emetto il subfile
091500090327          if nrr3 > 0;
091600090327            exsr sr_sfl03;
091700090327          endif;
091800090324
091900090326        endsr;
092000090327
092100090327       // ----------------------------------------------------------------------
092200090327       //?Gestione window
092300090327       // ----------------------------------------------------------------------
092400090327        begsr gesw02;
092500090327
092600090327       //?imposto i campi
092700090327          w2idjob = v2sidjob;
092800090327          w2cksu = v2sksu;
092900090327          w2crag = v2srag;
093000090327
093100090327       //?emetto la videata
093200090330          dow not $endw02;
093300090330            exfmt sb04w02;
093400090327
093500090330         //?spengo indicatori di comodo
093600090330            *in28 = *off;
093700090330         //?pulisco campo messaggi
093800090330            clear w2cmsg;
093900090330         //?esecuzione comandi
094000090330            select;
094100090327
094200090330           //?f6=conferma
094300090330              when *inkf;
094400090330                exsr sr_okcd;
094500090330             //?imposto flag per ricaricare il subfile
094600090330                $okcd = *on;
094700090330                $endw02 = *on;
094800090330           //?f12=ritorno
094900090330              when *inkl;
095000090330                $video = 'S02';
095100090330                $endw02 = *on;
095200090330            endsl;
095300090330          enddo;
095400090327
095500090327        endsr;
095600110714
095700110714       // ----------------------------------------------------------------------
095800110714       //?Gestione window
095900110714       // ----------------------------------------------------------------------
096000110714        begsr gesw03;
096100110714
096200110714       //?imposto i campi
096300110714          w3idjob = v2sidjob;
096400110714          w3cksu = v2sksu;
096500110714          w3crag = v2srag;
096600110714
096700110714       //?emetto la videata
096800110714          dow not $endw03;
096900110714            exfmt sb04w03;
097000110714
097100110714         //?spengo indicatori di comodo
097200110714            *in28 = *off;
097300110714         //?pulisco campo messaggi
097400110714            clear w3cmsg;
097500110721            *in28 = *off;
097600110714         //?esecuzione comandi
097700110714            select;
097800110714
097900110714           //?f6=conferma
098000110714              when *inkf;
098100110714                exsr sr_okfile;
098200110721                IF  OSB06esito <> *zeros;
098300110721                  w3cmsg = 'File CSV non creato';
098400110721                  *in28 = *on;
098500110721                ELSE;
098600110714             //?imposto flag per ricaricare il subfile
098700110721                  $okfile = *on;
098800110721                  $endw03 = *on;
098900110721                ENDIF;
099000110714           //?f12=ritorno
099100110714              when *inkl;
099200110714                $video = 'S02';
099300110714                $endw03 = *on;
099400110714            endsl;
099500110714          enddo;
099600110714
099700110714        endsr;
099800090327
099900090327       // ----------------------------------------------------------------------
100000090327       //?Gestione visualizzazione tabella LAC
100100090327       // ----------------------------------------------------------------------
100200090327        begsr geslac;
100300090327
100400090327       //?richiamo pgm TNTB46R
100500090327          reset tntb46ds;
100600090327          b46ke1 = %trim( %editw( v2sksu : '0       ' ) );
100700090327
100800090327          kpjbu = tntb46ds;
100900090327          tntb46r(kpjba);
101000090327          tntb46ds = kpjbu;
101100090327          $video = 'S02';
101200090327          clear v2sopz;
101300090327
101400090327        endsr;
101500090327
101600090327       // ----------------------------------------------------------------------
101700090327       //?Subfile
101800090327       // ----------------------------------------------------------------------
101900090327        begsr sr_s03;
102000090327
102100090327       //?pulisco il subfile
102200090327          exsr sr_puls03;
102300090327
102400090327          $ends03 = *off;
102500090327
102600090327       //?leggo tilac00f per Job Time Papà e Figlio
102700090327          exec sql
102800090327            declare ksc cursor for select distinct
102900090402            lacidjob, lactim, lacksu, lacksc, lacdir, lactad
103000090327            from tilac00f where lacidjob = :v2sidjob and
103100090327            lacksu = :v2sksu and lacela <> '33'
103200090402            group by lacidjob, lactim, lacksu, lacksc, lacdir, lactad;
103300090327
103400090327          exec sql
103500090327            open ksc;
103600090327            if sqlcode < 0;
103700090327              $ends03 = *on;
103800090327            ENDIF;
103900090327
104000090327       //?carico il subfile
104100090327          exsr sr_cars03;
104200090327
104300090327        endsr;
104400090327
104500090327       // ----------------------------------------------------------------------
104600090327       //?Emetto il subfile
104700090327       // ----------------------------------------------------------------------
104800090327        begsr sr_sfl03;
104900090327
105000090327       //?emetto la videata
105100090327          write sb04t01;
105200090327          write sb04z03;
105300090327          exfmt sb04c03;
105400090327
105500090327       //?spengo indicatori di comodo
105600090327          *in28 = *off;
105700090327       //?pulisco campo messaggi
105800090327          clear v3cmsg;
105900090327       //?esecuzione comandi
106000090327          select;
106100090327
106200090327         //?f3=fine
106300090327            when *inkc;
106400090327              exsr sr_fine;
106500090327         //?f12=ritorno
106600090327            when *inkl;
106700090327              $video = 'S02';
106800090327              leavesr;
106900090327          endsl;
107000090327
107100090327        endsr;
107200090324
107300090327       // ----------------------------------------------------------------------
107400090327       //?Pulisco il subfile
107500090327       // ----------------------------------------------------------------------
107600090327        begsr sr_puls03;
107700090327
107800090327          clear nrr3;
107900110303          clear T03nest;
108000110303          clear T03nela;
108100090327
108200090327          *in22 = *off;
108300090327          *in23 = *off;
108400090327          write sb04c03;
108500090327          *in22 = *on;
108600090327          *in23 = *on;
108700090327
108800090327          recsf3 = 1;
108900090327          *in32 = *on;
109000090327
109100090327        endsr;
109200090327
109300090327       // ----------------------------------------------------------------------
109400090327       //?Carico il subfile
109500090327       // ----------------------------------------------------------------------
109600090327        begsr sr_cars03;
109700110303
109800110303          *in56 = *off;
109900090327
110000090327          dow not $ends03;
110100090327
110200090327       //?leggo i ksc
110300090327            exec sql
110400090327              fetch next from ksc into :s03idjob, :s03tim,
110500090327                                       :s03ksu, :s03ksc,
110600090402                                       :s03dir, :s03tad;
110700090327
110800090327       //?fine file o errore sql esco
110900090327            if sqlcod = 100 or sqlcod < 0;
111000090327              $ends03 = *on;
111100090909              leave;
111200090327            endif;
111300090327
111400090327       //?conto per idjob e papà
111500090327          exsr sr_contas03;
111600090327
111700090327       //?scrivo subfile
111800090327          exsr sr_wrts03;
111900090327
112000090327          enddo;
112100110303
112200110303       //?scrivo riga di totale nel subfile se ci sono più figli
112300110303          IF  nrr3 > 1;
112400110303            *in56 = *on;
112500110303            exsr sr_wrtT03;
112600110303            *in56 = *off;
112700110303          ENDIF;
112800090327
112900090327       //?indicatore per fine file
113000090327          *in32 = *on;
113100090327
113200090327          exec sql close ksc;
113300090327
113400090327        endsr;
113500090327
113600090327       // ----------------------------------------------------------------------
113700090327       //?Conto
113800090327       // ----------------------------------------------------------------------
113900090327        begsr sr_contas03;
114000090327
114100090327       //?immagini estratte
114200090327          exec sql
114300110303            select count(distinct lacnim) into :s03nest
114400090327              from tilac00f where lacidjob = :s03idjob and
114500090327              lacksu = :s03ksu and lacksc = :s03ksc and
114600090327              lacela <> '33';
114700090327          if sqlcod <> 0;
114800090327            clear s03nest;
114900090327          endif;
115000090327       //?immagini elaborate
115100090327          exec sql
115200110303            select count(distinct lacnim) into :s03nela
115300090327              from tilac00f where lacidjob = :s03idjob and
115400090327              lacksu = :s03ksu and lacksc = :s03ksc and
115500090327              lacela <> '33' and lacela <> '00';
115600090327          if sqlcod <> 0;
115700090327            clear s03nela;
115800090327          endif;
115900090327       //?addebiti
116000090402          if s03tad <> 'N';
116100090402            exec sql
116200110303              select count(distinct lacnim) into :s03ntas
116300090402                from tilac00f where lacidjob = :s03idjob and
116400090402                lacksu = :s03ksu and lacksc = :s03ksc and
116500090402                lacela <> '33' and lacela = '11';
116600090402            if sqlcod <> 0;
116700090402              clear s03ntas;
116800090402            endif;
116900090402          else;
117000090402            clear s03ntas;
117100090402          endif;
117200090327       //?controllo se ci sono dei rcd a 'x2' o a '9x'
117300090327          exec sql
117400110303            select count(distinct lacnim) into :s03nerr
117500090327              from tilac00f where lacidjob = :s03idjob and
117600090327              lacksu = :s03ksu and lacksc = :s03ksc and
117700090327              (substr(lacela, 2, 1) = '2' or
117800090327               substr(lacela, 1, 1) = '9');
117900090327          if sqlcod <> 0;
118000090327            clear s03nerr;
118100090327          endif;
118200090327
118300090327        endsr;
118400090327
118500090327       // ----------------------------------------------------------------------
118600090327       //?Scrivo il subfile
118700090327       // ----------------------------------------------------------------------
118800090327        begsr sr_wrts03;
118900090327
119000090327       //?imposto i campi del control
119100090327          v3cksu = s03ksu;
119200090327       //?ragione sociale da tabella LAC
119300090327          exec sql
119400090327            select tbeuni into :tbeuni from tntbe00f
119500090327              where tbecod = 'LAC' and tbeatb = ' ' and
119600090327                    substr(tbeke1, 1, 7) = digits(:s03ksu);
119700090327       //?non trovo il rcd corrispondente
119800090327          if sqlcod <> 0;
119900090327            clear tbeuni;
120000090327          endif;
120100090327          dlac = tbeuni;
120200090327          v3crag = §lacrag;
120300090327
120400090327       //?imposto i campi del subfile
120500090327       //?data
120600090327          v3sdata = %subst(s03tim:7:2) + '/' +
120700090327                    %subst(s03tim:5:2) + '/' +
120800090327                    %subst(s03tim:1:4);
120900090327       //?codice estrazione
121000090327          v3sksc = s03ksc;
121100090327       //?ragione sociale da tabella LAC
121200090327          exec sql
121300090327            select tbeuni into :tbeuni from tntbe00f
121400090327              where tbecod = 'LAC' and tbeatb = ' ' and
121500090327                    substr(tbeke1, 1, 7) = digits(:s03ksc);
121600090327       //?non trovo il rcd corrispondente
121700090327          if sqlcod <> 0;
121800090327            clear tbeuni;
121900090327          endif;
122000090327          dlac = tbeuni;
122100090327          v3srag = §lacrag;
122200090327       //?nr. immagini estratte
122300090327          v3sestr = s03nest;
122400090327       //?nr. immagini elaborate
122500090327          v3selab = s03nela;
122600090330       //?codice + ragione sociale in rosso se non elaborazione non terminata
122700090330          *in55 = (s03nest <> s03nela);
122800090327       //?se ho trovato dei rcd a 'x2' o a '9x'
122900090327          *in52 = (s03nerr > *zeros);
123000090327       //?CD si/no
123100090327          v3scd = v2scd;
123200090327       //?TAS si/no
123300090327          select;
123400090402            when s03tad = 'N';
123500090402              v3stas = 'NP';
123600090327            when s03ntas = *zeros;
123700090327              v3stas = 'NO';
123800090327              eval *in53 = *off;
123900090327            when s03ntas = s03nest;
124000090327              v3stas = 'SI';
124100090327              eval *in53 = *off;
124200090327            when s03ntas <> s03nest;
124300090327              v3stas = 'SI';
124400090327              eval *in53 = *on;
124500090327          endsl;
124600090327       //?directory
124700090327          v3sdir = s03dir;
124800110303
124900110303       //?sommo le immagini estratte e le elaborate
125000110303          T03nest += S03nest;
125100110303          T03nela += S03nela;
125200090327
125300090327          nrr3 = nrr3 + 1;
125400090327          write sb04s03;
125500090327
125600090327        endsr;
125700110303
125800110303       // ----------------------------------------------------------------------
125900110303       //?Scrivo riga di totale nel subfile.
126000110303       // ----------------------------------------------------------------------
126100110303        begsr sr_wrtT03;
126200110303
126300110303          *in52 = *off;
126400110303          *in53 = *off;
126500110303          *in55 = *off;
126600110303          clear v3sdata;
126700110303          clear v3sksc;
126800110303          v3srag = 'T O T A L E ';
126900110303          v3sestr = T03nest;
127000110303          v3selab = T03nela;
127100110303          clear v3scd;
127200110303          clear v3stas;
127300110303          clear v3stas;
127400110303          clear v3sdir;
127500110303          nrr3 = nrr3 + 1;
127600110303          write sb04s03;
127700110303
127800110303        endsr;
127900090327
128000090327       // ----------------------------------------------------------------------
128100090327       //?Ok fatto cd aggiorno TILAC
128200090327       // ----------------------------------------------------------------------
128300090327        begsr sr_okcd;
128400090327
128500090327          exec sql
128600090327            update tilac00f
128700090327            set lacutecd = :knmus, lactimecd = :wtimestp
128800090327            where
128900090327            lacidjob = :w2idjob and
129000110714              lacksu = :w2cksu  and lacela in('33', '34');
129100090327
129200090327        endsr;
129300110714
129400110714       // ----------------------------------------------------------------------
129500110714       //?Ok faccio il file e aggiorno TILAC
129600110714       // ----------------------------------------------------------------------
129700110714        begsr sr_okfile;
129800110714
129900110714       //?richiamo pgm per creare il FILE CSV da aggiungere al CD
130000110721          clear TNSB06DS;
130100110721          ISB06ksu   = V2Sksu;
130200110721          ISB06idjob = V2Sidjob;
130300110721          ISB06dir   = V2Sdir;
130400110721          tnsb06r (kpjba : TNSB06DS);
130500110721          IF  OSB06esito <> *zeros;
130600110721            leavesr;
130700110721          ENDIF;
130800110714
130900110714          exec sql
131000110714            update tilac00f
131100110714            set lacela = '34'
131200110714            where
131300110714            lacidjob = :w3idjob and
131400110714              lacksu = :w3cksu  and lacela = '33';
131500110714
131600110714        endsr;
131700090327
131800090324       // ----------------------------------------------------------------------
131900090324       //?Fine programma
132000090324       // ----------------------------------------------------------------------
132100090327        begsr sr_fine;
132200090324
132300090327          *inlr = *on;
132400090327          return;
132500090324
132600090327        endsr;
132700090324
132800090324** MSG  Lungh. 78                                                            *
132900090326Immettere almeno una delle due date                                           01
133000090326Data errata                                                                   02
133100090326Range di date errato                                                          03
133200090326Immettere SOLO caratteri numerici                                             04
133300090326Codice errato                                                                 05
133400090330E' possibile selezionare solo una parzializzazione                            06
