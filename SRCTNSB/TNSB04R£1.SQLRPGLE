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
004700090327     d s02idjob        s                   like(lacidjob)
004800090327     d s02ksu          s                   like(lacksu)
004900090402     d s02nela         s             10i 0
005000090402     d s02nerr         s             10i 0
005100090402     d s02nest         s             10i 0
005200090402     d s02ntas         s             10i 0
005300090402     d s02tad          s                   like(lactad)
005400090327     d s02tim          s                   like(lactim)
005500090327     d s02utecd        s                   like(lacutecd)
005600090327     d s03dir          s                   like(lacdir)
005700090327     d s03idjob        s                   like(lacidjob)
005800090327     d s03ksc          s                   like(lacksc)
005900090327     d s03ksu          s                   like(lacksu)
006000090402     d s03nela         s             10i 0
006100090402     d s03nerr         s             10i 0
006200090402     d s03nest         s             10i 0
006300090402     d s03ntas         s             10i 0
006400090402     d s03tad          s                   like(lactad)
006500090327     d s03tim          s                   like(lactim)
006600110303     d T03nela         s             10i 0
006700110303     d T03nest         s             10i 0
006800090324     d wdata           s              8  0
006900090326     d wdatada         s              8  0
007000090326     d wdataal         s              8  0
007100090327     d wtimestp        s             14
007200090324     d xx              s              3  0
007300090327     d $ends02         s               n
007400090327     d $ends03         s               n
007500090330     d $endw02         s               n
007600090324     d $fine           s               n
007700090330     d $okcd           s               n
007800090327     d $recnook        s               n
007900090324     d $video          s             10
008000090324
008100090324      // ? S C H I E R E ?
008200090324     d msg             s             78    dim(15) ctdata perrcd(1)
008300090324
008400090324      // ? D S   I N T E R N E / E S T E R N E ?
008500090324     d wlbdat          ds                  inz
008600090324     d  g02dat                 1      8  0
008700090324     d  g02inv                 9     16  0
008800090324     d  g02err                17     17
008900090324     d  g02tgi                18     22  0
009000090324
009100090324     d §azute        e ds                  extname(azute00f)
009200090324     d                                     dtaara
009300090324     d §datiute      e ds                  extname(ddatiute)
009400090324     d                                     dtaara
009500090324     d dlac          e ds
009600090324     d kpjba         e ds
009700090324     d tilacds       e ds                  extname(tilac00f)
009800090324     d tibs34ds      e ds                  inz
009900090324     d tntbeds       e ds                  extname(tntbe00f)
010000090327
010100090327      // - Interrogazione tabella LAC dettaglio
010200090327     d tntb46ds      e ds                  inz
010300090327     d   b46opz      e                     inz('5')
010400090327     d   b46err      e                     inz(*off)
010500090324
010600090324       // - Struttura per passaggio dati ad interrogazione tabella
010700090324     d param01         ds
010800090324     d  p01cod                        7  0 inz
010900090324     d  p01ord                        1
011000090324     d  p01ksu                        7  0 inz
011100090324     d  p01ke1                        7
011200090324     d  p01ke2                       15
011300090324     d  p01rit                        1
011400090324
011500090324     d                sds
011600090324     d  vtcpgm                 1     10
011700090324
011800090324      // ? COSTANTI ?
011900090324     d digitn          c                   const('1234567890')
012000090324
012100090324      // ? PROTOTIPI ?
012200090324      /copy gaitrasrc/srcprotopr,tibs34r
012300090324      /copy gaitrasrc/srcprotopr,xsrda8
012400090324
012500090327       // - Visualizzazione tabella LAC
012600090327     d tntb46r         pr                  extpgm('TNTB46R')
012700090327     d  kpjba                              likeds(KPJBA)
012800090327
012900090324       // - Interrogazione tabella LAC
013000090324     d tntb461r        pr                  extpgm('TNTB461R')
013100090324     d  kpjba                              likeds(KPJBA)
013200090324
013300090324      // ----------------------------------------------------------------------
013400090324
013500090324     c     *entry        plist
013600090324     c                   parm                    kpjba
013700090324
013800090324      /free
013900090324
014000090324       //?operazioni iniziali
014100090326          exsr routinz;
014200090326
014300090326       //?imposto i dati di dft nella prima videata
014400090326       //?per ora solo all'inizio del programma poi vediamo
014500090326          exsr inzd01;
014600090324
014700090326          dow not $fine;
014800090324
014900090326         //?gestione delle videate
015000090326            select;
015100090324
015200090326         //?gestione prima videata
015300090326             when $video = 'D01';
015400090326               exsr gesd01;
015500090324
015600090326         //?gestione subfile
015700090326             when $video = 'S02';
015800090326               exsr gess02;
015900090327
016000090327         //?gestione subfile
016100090327             when $video = 'S03';
016200090327               exsr gess03;
016300090327
016400090327         //?gestione visualizzazione tabella LAC
016500090327             when $video = 'LAC';
016600090327               exsr geslac;
016700090324
016800090326            endsl;
016900090326          enddo;
017000090324
017100090326          *inlr = *on;
017200090324
017300090324       // ----------------------------------------------------------------------
017400090324       //?Operazioni iniziali.
017500090324       // ----------------------------------------------------------------------
017600090324       begsr routinz;
017700090324
017800090324         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
017900090324
018000090324       //?recupero dei dati utente
018100090324          in(e) §azute;
018200090324          if not %error;
018300090326            in(e) §datiute;
018400090324          endif;
018500090324          if %error or rsut = *blanks;
018600090326            tibs34r(tibs34ds);
018700090326            in §azute;
018800090326            in §datiute;
018900090324          endif;
019000090324
019100090324       //?imposto la data del giorno
019200090324          wdata = %dec(%date());
019300090324          dataiso = %date(wdata:*iso);
019400090324          dataeur = dataiso;
019500090327
019600090327       //?imposto il time del job
019700090327          wtimestp = %char(%timestamp:*iso0);
019800090324
019900090324       //?gestione video D01
020000090326          $video = 'D01';
020100090326          $fine =*off;
020200090324
020300090324       endsr;
020400090326
020500090326       // ----------------------------------------------------------------------
020600090326       //?Imposto i dati di dflt nella prima videata.
020700090326       // ----------------------------------------------------------------------
020800090326        begsr inzd01;
020900090326
021000090326          clear v1datada;
021100090326          v1dataal = %dec(dataeur);
021200090330          v1cd = 'X';
021300090326
021400090326        endsr;
021500090324
021600090324       // ----------------------------------------------------------------------
021700090324       //?Gestione prima videata (parametri)
021800090324       // ----------------------------------------------------------------------
021900090326        begsr gesd01;
022000090324
022100090324       //?pulisco la videata
022200090324          if not *in28 and not *in90;
022300090330            exsr inzd01;
022400090326            exsr sr_puld01;
022500090324          endif;
022600090324
022700090324       //?emetto la videata
022800090324          write sb04t01;
022900090324          exfmt sb04d01;
023000090324
023100090324       //?spengo indicatori di comodo
023200090324          *in28 = *off;
023300090324          *in90 = *off;
023400090324       //?pulisco campo messaggi
023500090324          clear v1cmsg;
023600090324
023700090324       //?esecuzione comandi
023800090326          select;
023900090324
024000090326         //?f3=fine
024100090326            when *inkc;
024200090326              exsr sr_fine;
024300090326            other;
024400090324
024500090326           //?controllo i dati della videata
024600090326              exsr sr_contrd01;
024700090326              if *in28 or *in90;
024800090326                leavesr;
024900090326              endif;
025000090326          endsl;
025100090324
025200090326       //?passo al subfile
025300090326          exsr sr_s02;
025400090326          $video = 'S02';
025500090324
025600090326        endsr;
025700090324
025800090324       // ----------------------------------------------------------------------
025900090324       //?Gestione subfile
026000090324       // ----------------------------------------------------------------------
026100090326        begsr gess02;
026200090324
026300090324       //?ho caricato dati emetto il subfile
026400090326          if nrr2 > 0;
026500090326            exsr sr_sfl02;
026600090326          endif;
026700090324
026800090324       //?non ho caricato dati emetto videata di segnalazione
026900090326          if nrr2 = 0;
027000090326            exsr sr_d02;
027100090326          endif;
027200090324
027300090326        endsr;
027400090324
027500090324       // ----------------------------------------------------------------------
027600090324       //?Pulizia prima videata (parametri)
027700090324       // ----------------------------------------------------------------------
027800090326        begsr sr_puld01;
027900090324
028000090326          clear v1cksu;
028100090326          clear v1dksu;
028200090326          clear v1estr;
028300090326          clear v1elab;
028400090326          clear v1tas;
028500090324
028600090326        endsr;
028700090324
028800090324       // ----------------------------------------------------------------------
028900090324       //?Controllo dati prima videata (parametri)
029000090324       // ----------------------------------------------------------------------
029100090326        begsr sr_contrd01;
029200090324
029300090324       //?spengo indicatori di errore
029400090326          *in41 = *off;
029500090326          *in42 = *off;
029600090326          *in43 = *off;
029700090330          *in44 = *off;
029800090330          *in45 = *off;
029900090330          *in46 = *off;
030000090324
030100090324       //?pulisco campi di comodo
030200090326          clear wdatada;
030300090326          clear wdataal;
030400090324
030500090324       //?controllo data
030600090326          exsr sr_contrdata;
030700090326          if *in28;
030800090326            leavesr;
030900090326          endif;
031000090324
031100090326       //?controllo padre
031200090326          if v1cksu <> *blanks;
031300090326            exsr sr_contrksu;
031400090326            if *in28 or *in90;
031500090326              leavesr;
031600090326            endif;
031700090326          endif;
031800090330
031900090330       //?controllo parzializzazioni
032000090330          exsr sr_contrpar;
032100090330          if *in28;
032200090330            leavesr;
032300090330          endif;
032400090324
032500090326        endsr;
032600090324
032700090324       // ----------------------------------------------------------------------
032800090326       //?Subfile
032900090324       // ----------------------------------------------------------------------
033000090326        begsr sr_s02;
033100090324
033200090324       //?pulisco il subfile
033300090326          exsr sr_puls02;
033400090324
033500090327          $ends02 = *off;
033600090326
033700090326       //?leggo tilac00f solo rcd con lacela = '33' Job sottomesso
033800090326       //?tutti i papà
033900090326          if v1cksu = *blanks;
034000090326            exec sql
034100090327              declare ksu cursor for
034200090402              select lacidjob, lactim, lacksu, lacutecd, lacdir, lactad
034300090326              from tilac00f where lacela = '33' and
034400090326              substr(lactim, 1, 8) between digits(:wdatada)
034500090326              and digits(:wdataal)
034600090326              order by lactim, lacksu, lacidjob;
034700090326
034800090326            exec sql
034900090327              open ksu;
035000090326              if sqlcode < 0;
035100090327                $ends02 = *on;
035200090326              ENDIF;
035300090326          endif;
035400090326
035500090326       //?leggo tilac00f raggruppato per id job e codice estrazione
035600090326       //?un solo codice raggruppamento chiesto
035700090326          if v1cksu > *zeros;
035800090326            exec sql
035900090327              declare ksu1 cursor for
036000090402              select lacidjob, lactim, lacksu, lacutecd, lacdir, lactad
036100090326              from tilac00f where lacela = '33' and
036200090326              substr(lactim, 1, 8) between digits(:wdatada) and
036300090326              digits(:wdataal) and digits(lacksu) = :v1cksu
036400090326              order by lactim, lacksu, lacidjob;
036500090326
036600090326            exec sql
036700090327              open ksu1;
036800090326              if sqlcode < 0;
036900090327                $ends02 = *on;
037000090326              ENDIF;
037100090326          endif;
037200090326
037300090324       //?carico il subfile
037400090326          exsr sr_cars02;
037500090324
037600090326        endsr;
037700090324
037800090324       // ----------------------------------------------------------------------
037900090326       //?Emetto il subfile
038000090324       // ----------------------------------------------------------------------
038100090326        begsr sr_sfl02;
038200090324
038300090330       //?flag creazione cd
038400090330          $okcd = *off;
038500090330
038600090324       //?emetto la videata
038700090326          write sb04t01;
038800090326          write sb04z02;
038900090326          exfmt sb04c02;
039000090324
039100090324       //?spengo indicatori di comodo
039200090326          *in28 = *off;
039300090324       //?pulisco campo messaggi
039400090326          clear v2cmsg;
039500090324       //?esecuzione comandi
039600090326          select;
039700090324
039800090326         //?f3=fine
039900090326            when *inkc;
040000110301              exsr sr_fine;
040100110301         //?f5=refresh
040200110301            when *inke;
040300110301              exsr sr_s02;
040400090326         //?f12=ritorno
040500090326            when *inkl;
040600090326              $video = 'D01';
040700090326              leavesr;
040800090326            other;
040900090326         //?controllo opzione subfile
041000090326            exsr sr_ctrs02;
041100090330         //?se creati CD ricarico il subfile
041200090330            if $okcd;
041300090330              exsr sr_s02;
041400090330            endif;
041500090326          endsl;
041600090324
041700090326        endsr;
041800090324
041900090324       // ----------------------------------------------------------------------
042000090324       //?Emetto video di segnalazione dati non caricati
042100090324       // ----------------------------------------------------------------------
042200090326        begsr sr_d02;
042300090324
042400090324       //?emetto la videata
042500090326          write sb04t01;
042600090326          exfmt sb04d02;
042700090324
042800090324       //?spengo indicatori di comodo
042900090326          *in28 = *off;
043000090326          *in90 = *off;
043100090324       //?pulisco campo messaggi
043200090326          clear v2cmsg;
043300090324       //?esecuzione comandi
043400090326          select;
043500090324
043600090326         //?f3=fine
043700090326            when *inkc;
043800090326              exsr sr_fine;
043900090326         //?f12=ritorno
044000090326            when *inkl;
044100090326              $video = 'D01';
044200090326              leavesr;
044300090326            other;
044400090326          endsl;
044500090324
044600090326        endsr;
044700090324
044800090324       // ----------------------------------------------------------------------
044900090324       //?Controllo data
045000090324       // ----------------------------------------------------------------------
045100090326        begsr sr_contrdata;
045200090324
045300090324       //?la data è obbligatoria
045400090326          if v1datada = *zeros and v1dataal = *zeros;
045500090326            *in28 = *on;
045600090326            *in41 = *on;
045700090326            v1cmsg = msg(01);
045800090326            leavesr;
045900090326          endif;
046000090324
046100090324       //?deve essere una data valida
046200090326       //?data da
046300090326          if v1datada <> *zeros;
046400090326            clear wlbdat;
046500090326            g02dat = v1datada;
046600090326            xsrda8(wlbdat);
046700090326            if g02err = '1';
046800090326              *in28 = *on;
046900090326              *in41 = *on;
047000090326              v1cmsg = msg(02);
047100090326              leavesr;
047200090326            endif;
047300090326            v1datada = g02dat;
047400090326            wdatada  = g02inv;
047500090326          endif;
047600090326       //?data al
047700090326          if v1dataal <> *zeros;
047800090326            clear wlbdat;
047900090326            g02dat = v1dataal;
048000090326            xsrda8(wlbdat);
048100090326            if g02err = '1';
048200090326              *in28 = *on;
048300090326              *in42 = *on;
048400090326              v1cmsg = msg(02);
048500090326              leavesr;
048600090326            endif;
048700090326            v1dataal = g02dat;
048800090326            wdataal  = g02inv;
048900090326          endif;
049000090324
049100090326       //?imposto in automatico la data mancante
049200090326          if v1dataal = *zeros;
049300090326            v1dataal = v1datada;
049400090326            wdataal  = wdatada;
049500090326          endif;
049600090326       //?controllo range date
049700110502          if wdatada > wdataal;
049800090326            *in28 = *on;
049900090326            *in41 = *on;
050000090326            v1cmsg = msg(03);
050100090326            leavesr;
050200090326          endif;
050300090324
050400090326        endsr;
050500090324
050600090324       // ----------------------------------------------------------------------
050700090326       //?Controllo padre
050800090324       // ----------------------------------------------------------------------
050900090326        begsr sr_contrksu;
051000090324
051100090326       //?Ricerca codice cliente
051200090326          if %scan('?':v1cksu) > *zeros;
051300090326            clear  param01;
051400090326            p01ord = '0';
051500090326            kpjbu = param01;
051600090326            tntb461r (kpjba);
051700090326            param01 = kpjbu;
051800090324
051900090326            if p01ke1 <> *zeros;
052000090326              v1cksu = p01ke1;
052100090326              *in43 = *on;
052200090326              *in90 = *on;
052300090326            endif;
052400090326          endif;
052500090324
052600090326       //?Controllo immissione solo caratteri numerici
052700090326          if %check(digitn:v1cksu) > *zeros;
052800090326            *in28 = *on;
052900090326            *in43 = *on;
053000090326            v1cmsg = msg(04);
053100090326            leavesr;
053200090326          endif;
053300090324
053400090326       //?Controllo esistenza codice cliente in tabella LAC
053500090326          exec sql
053600090326            select tbeuni into :tbeuni from tntbe00f
053700090326            where tbecod = 'LAC' and tbeatb = '' and
053800090326            substr(tbeke1, 1, 7) = :v1cksu;
053900090326       //?non trovo il rcd corrispondente
054000090326          if sqlcod <> 0;
054100090326            clear tbeuni;
054200090326            *in28 = *on;
054300090326            *in43 = *on;
054400090326            v1cmsg = msg(05);
054500090326            leavesr;
054600090326          endif;
054700090324
054800090326          dlac = tbeuni;
054900090326          v1dksu = §lacrag;
055000090324
055100090326        endsr;
055200090330
055300090330       // ----------------------------------------------------------------------
055400090330       //?Controllo parzializzazioni
055500090330       // ----------------------------------------------------------------------
055600090330        begsr sr_contrpar;
055700090330
055800090330       //?le parzializzazioni non sono in alternativa
055900090330          if v1cd <> *blanks and (v1estr <> *blanks or v1elab <> *blanks or
056000090330                                  v1tas <> *blanks);
056100090330            *in28 = *on;
056200090330            *in44 = *on;
056300090330            v1cmsg = msg(06);
056400090330            leavesr;
056500090330          endif;
056600090330          if v1estr <> *blanks and (v1elab <> *blanks or v1tas <> *blanks);
056700090330            *in28 = *on;
056800090330            *in45 = *on;
056900090330            v1cmsg = msg(06);
057000090330            leavesr;
057100090330          endif;
057200090330          if v1elab <> *blanks and v1tas <> *blanks;
057300090330            *in28 = *on;
057400090330            *in46 = *on;
057500090330            v1cmsg = msg(06);
057600090330            leavesr;
057700090330          endif;
057800090330
057900090330       endsr;
058000090324
058100090324       // ----------------------------------------------------------------------
058200090324       //?Pulisco il subfile
058300090324       // ----------------------------------------------------------------------
058400090326        begsr sr_puls02;
058500090324
058600090326          clear nrr2;
058700090326          clear rrnlast;
058800090324
058900090326          *in20 = *off;
059000090326          *in21 = *off;
059100090326          write sb04c02;
059200090326          *in20 = *on;
059300090326          *in21 = *on;
059400090324
059500090326          recsf2 = 1;
059600090326          *in31 = *on;
059700090324
059800090326        endsr;
059900090324
060000090324       // ----------------------------------------------------------------------
060100090324       //?Carico il subfile
060200090324       // ----------------------------------------------------------------------
060300090326        begsr sr_cars02;
060400090324
060500090327          dow not $ends02;
060600090324
060700090324       //?leggo i job
060800090325       //?tutti i codici raggruppamento
060900090326            if v1cksu = *blanks;
061000090326              exec sql
061100090327                fetch next from ksu into :s02idjob, :s02tim,
061200090327                                         :s02ksu, :s02utecd,
061300090402                                         :s02dir, :s02tad;
061400090326            endif;
061500090325       //?un solo codice raggruppamento chiesto
061600090326            if v1cksu > *zeros;
061700090326              exec sql
061800090327                fetch next from ksu1 into :s02idjob, :s02tim,
061900090327                                          :s02ksu, :s02utecd,
062000090402                                          :s02dir, :s02tad;
062100090326            endif;
062200090324
062300090324       //?fine file o errore sql esco
062400090326            if sqlcod = 100 or sqlcod < 0;
062500090327              $ends02 = *on;
062600090326              leave;
062700090326            endif;
062800090324
062900090326       //?conto per idjob e papà
063000090327          exsr sr_contas02;
063100090326
063200090324       //?scrivo subfile
063300090326          exsr sr_wrts02;
063400090324
063500090326          enddo;
063600090324
063700090324       //?indicatore per fine file
063800090326          *in31 = *on;
063900090324       //?imposto il numero di record caricati
064000090326          rrnlast = nrr2;
064100090324
064200090325       //?chiudo il cursore
064300090326          if v1cksu = *blanks;
064400090327            exec sql close ksu;
064500090326          endif;
064600090326          if v1cksu > *zeros;
064700090327            exec sql close ksu1;
064800090326          endif;
064900090324
065000090326        endsr;
065100090324
065200090326       // ----------------------------------------------------------------------
065300090326       //?Conto
065400090326       // ----------------------------------------------------------------------
065500090327        begsr sr_contas02;
065600090326
065700090326       //?immagini estratte
065800090326          exec sql
065900090716            select count(distinct lacnim) into :s02nest
066000090327              from tilac00f where lacidjob = :s02idjob and
066100090327              lacksu = :s02ksu and lacela <> '33';
066200090326          if sqlcod <> 0;
066300090327            clear s02nest;
066400090326          endif;
066500090326       //?immagini elaborate
066600090326          exec sql
066700090716            select count(distinct lacnim) into :s02nela
066800090327              from tilac00f where lacidjob = :s02idjob and
066900090327              lacksu = :s02ksu and lacela <> '33' and
067000090326              lacela <> '00';
067100090326          if sqlcod <> 0;
067200090327            clear s02nela;
067300090326          endif;
067400090326       //?addebiti
067500090402          if s02tad <> 'N';
067600090402            exec sql
067700090716              select count(distinct lacnim) into :s02ntas
067800090402                from tilac00f where lacidjob = :s02idjob and
067900090402                lacksu = :s02ksu and lacela <> '33' and
068000090402                lacela = '11';
068100090402            if sqlcod <> 0;
068200090402              clear s02ntas;
068300090402            endif;
068400090402          else;
068500090402            clear s02ntas;
068600090402          endif;
068700090326       //?controllo se ci sono dei rcd a 'x2' o a '9x'
068800090326          exec sql
068900090327            select count(*) into :s02nerr
069000090327              from tilac00f where lacidjob = :s02idjob and
069100090327              lacksu = :s02ksu and
069200090326              (substr(lacela, 2, 1) = '2' or
069300090326               substr(lacela, 1, 1) = '9');
069400090326          if sqlcod <> 0;
069500090327            clear s02nerr;
069600090326          endif;
069700090326
069800090326        endsr;
069900090326
070000090324       // ----------------------------------------------------------------------
070100090326       //?Scrivo il subfile
070200090324       // ----------------------------------------------------------------------
070300090326        begsr sr_wrts02;
070400090327
070500090327          $recnook = *off;
070600090327
070700090327       //?controllo le parzializzazioni
070800090327          exsr sr_parzializza;
070900090327          if $recnook;
071000090327            leavesr;
071100090327          endif;
071200090324
071300090326       //?imposto i campi del subfile
071400090327       //?idjob
071500090327          v2sidjob = s02idjob;
071600090326       //?data
071700090327          v2sdata = %subst(s02tim:7:2) + '/' +
071800090327                    %subst(s02tim:5:2) + '/' +
071900090327                    %subst(s02tim:1:4);
072000090326       //?codice papà e cerco la ragione sociale da tabella LAC
072100090327          v2sksu = s02ksu;
072200090326       //?ragione sociale da tabella LAC
072300090326          exec sql
072400090326            select tbeuni into :tbeuni from tntbe00f
072500090326              where tbecod = 'LAC' and tbeatb = ' ' and
072600090327                    substr(tbeke1, 1, 7) = digits(:s02ksu);
072700090324       //?non trovo il rcd corrispondente
072800090324          if sqlcod <> 0;
072900090326            clear tbeuni;
073000090324          endif;
073100090326          dlac = tbeuni;
073200090326          v2srag = §lacrag;
073300090326       //?nr. immagini estratte
073400090327          v2sestr = s02nest;
073500090326       //?nr. immagini elaborate
073600090327          v2selab = s02nela;
073700090330       //?codice + ragione sociale in rosso se non elaborazione non terminata
073800090330          *in54 = (s02nest <> s02nela);
073900090326       //?se ho trovato dei rcd a 'x2' o a '9x'
074000090327          *in50 = (s02nerr > *zeros);
074100090326       //?CD si/no
074200090327          if s02utecd <> *blanks;
074300090326            v2scd = 'SI';
074400090326          else;
074500090326            v2scd = 'NO';
074600090326          endif;
074700090326       //?TAS si/no
074800090326          select;
074900090402            when s02tad = 'N';
075000090402              v2stas = 'NP';
075100090327            when s02ntas = *zeros;
075200090327              v2stas = 'NO';
075300090330              *in51 = *off;
075400090327            when s02ntas = s02nest;
075500090326              v2stas = 'SI';
075600090330              *in51 = *off;
075700090327            when s02ntas <> s02nest;
075800090326              v2stas = 'SI';
075900090330              *in51 = *on;
076000090326          endsl;
076100090327       //?directory
076200090327          v2sdir = s02dir;
076300090330       //?indicatori del subfile
076400090330          v2sin50 = '0';
076500090330          v2sin51 = '0';
076600090330          v2sin54 = '0';
076700090330          if *in50;
076800090330            v2sin50 = '1';
076900090330          endif;
077000090330          if *in51;
077100090330            v2sin51 = '1';
077200090330          endif;
077300090330          if *in54;
077400090330            v2sin54 = '1';
077500090330          endif;
077600090326
077700090326          nrr2 = nrr2 + 1;
077800090324          write sb04s02;
077900090324
078000090326        endsr;
078100090327
078200090327       // ----------------------------------------------------------------------
078300090327       //?Controllo le parzializzazioni richieste in prima videata
078400090327       // ----------------------------------------------------------------------
078500090327        begsr sr_parzializza;
078600090327
078700090327       //?solo NON estratti
078800090327          if v1estr <> *blanks and s02nest <> *zeros;
078900090327            $recnook = *on;
079000090327            leavesr;
079100090327          endif;
079200090327
079300090327       //?solo elaborazioni NON terminate
079400090327          if v1elab <> *blanks and s02nest = s02nela;
079500090327            $recnook = *on;
079600090327            leavesr;
079700090327          endif;
079800090327
079900090327       //?solo CD NON completati
080000090327          if v1cd <> *blanks and s02utecd <> *blanks;
080100090327            $recnook = *on;
080200090327            leavesr;
080300090327          endif;
080400090327
080500090327       //?solo NON addebitati
080600090327          if v1tas <> *blanks and s02ntas <> *zeros;
080700090327            $recnook = *on;
080800090327            leavesr;
080900090327          endif;
081000090327
081100090327       endsr;
081200090324
081300090324       // ----------------------------------------------------------------------
081400090324       //?Controllo opzione subfile
081500090324       // ----------------------------------------------------------------------
081600090326        begsr sr_ctrs02;
081700090324
081800090324       //?controllo l'opzione immessa
081900090326          for xx = 1 to rrnlast;
082000090326            chain xx sb04s02;
082100090330            $endw02 = *off;
082200090327            recsf2 = xx;
082300090327            select;
082400090327           //?visualizza dettaglio
082500090327              when v2sopz = '5';
082600090909             //?errore se non esistono dati estratti
082700090909                if v2sestr = 0 and v2selab = 0;
082800090909                  *in28 = *on;
082900090909                  v2cmsg = 'Dati non estratti!!';
083000090909                else;
083100090909                  $video = 'S03';
083200090909                  exsr gess03;
083300090909                  clear v2sopz;
083400090909                endif;
083500090327           //?OK creato CD
083600090327              when v2sopz = 'C';
083700090327             //?errore se cd già creato
083800090327                if v2scd = 'SI';
083900090327                  *in28 = *on;
084000090327                  v2cmsg = 'CD già creato!!';
084100090327                else;
084200090327             //?creo cd
084300090327                  exsr gesw02;
084400090327                  clear v2sopz;
084500090327                endif;
084600090327           //?visualizzo tabella LAC
084700090327              when v2sopz = 'T';
084800090327                $video = 'LAC';
084900090327                exsr geslac;
085000090327                clear v2sopz;
085100090327            endsl;
085200090327
085300090330            *in50 = (v2sin50 = '1');
085400090330            *in51 = (v2sin51 = '1');
085500090330            *in54 = (v2sin54 = '1');
085600090327            update sb04s02;
085700090326          endfor;
085800090324
085900090326        endsr;
086000090324
086100090324       // ----------------------------------------------------------------------
086200090327       //?Gestione subfile
086300090324       // ----------------------------------------------------------------------
086400090327        begsr gess03;
086500090327
086600090327       //?subfile
086700090327          exsr sr_s03;
086800090327
086900090327       //?emetto il subfile
087000090327          if nrr3 > 0;
087100090327            exsr sr_sfl03;
087200090327          endif;
087300090324
087400090326        endsr;
087500090327
087600090327       // ----------------------------------------------------------------------
087700090327       //?Gestione window
087800090327       // ----------------------------------------------------------------------
087900090327        begsr gesw02;
088000090327
088100090327       //?imposto i campi
088200090327          w2idjob = v2sidjob;
088300090327          w2cksu = v2sksu;
088400090327          w2crag = v2srag;
088500090327
088600090327       //?emetto la videata
088700090330          dow not $endw02;
088800090330            exfmt sb04w02;
088900090327
089000090330         //?spengo indicatori di comodo
089100090330            *in28 = *off;
089200090330         //?pulisco campo messaggi
089300090330            clear w2cmsg;
089400090330         //?esecuzione comandi
089500090330            select;
089600090327
089700090330           //?f6=conferma
089800090330              when *inkf;
089900090330                exsr sr_okcd;
090000090330             //?imposto flag per ricaricare il subfile
090100090330                $okcd = *on;
090200090330                $endw02 = *on;
090300090330           //?f12=ritorno
090400090330              when *inkl;
090500090330                $video = 'S02';
090600090330                $endw02 = *on;
090700090330            endsl;
090800090330          enddo;
090900090327
091000090327        endsr;
091100090327
091200090327       // ----------------------------------------------------------------------
091300090327       //?Gestione visualizzazione tabella LAC
091400090327       // ----------------------------------------------------------------------
091500090327        begsr geslac;
091600090327
091700090327       //?richiamo pgm TNTB46R
091800090327          reset tntb46ds;
091900090327          b46ke1 = %trim( %editw( v2sksu : '0       ' ) );
092000090327
092100090327          kpjbu = tntb46ds;
092200090327          tntb46r(kpjba);
092300090327          tntb46ds = kpjbu;
092400090327          $video = 'S02';
092500090327          clear v2sopz;
092600090327
092700090327        endsr;
092800090327
092900090327       // ----------------------------------------------------------------------
093000090327       //?Subfile
093100090327       // ----------------------------------------------------------------------
093200090327        begsr sr_s03;
093300090327
093400090327       //?pulisco il subfile
093500090327          exsr sr_puls03;
093600090327
093700090327          $ends03 = *off;
093800090327
093900090327       //?leggo tilac00f per Job Time Papà e Figlio
094000090327          exec sql
094100090327            declare ksc cursor for select distinct
094200090402            lacidjob, lactim, lacksu, lacksc, lacdir, lactad
094300090327            from tilac00f where lacidjob = :v2sidjob and
094400090327            lacksu = :v2sksu and lacela <> '33'
094500090402            group by lacidjob, lactim, lacksu, lacksc, lacdir, lactad;
094600090327
094700090327          exec sql
094800090327            open ksc;
094900090327            if sqlcode < 0;
095000090327              $ends03 = *on;
095100090327            ENDIF;
095200090327
095300090327       //?carico il subfile
095400090327          exsr sr_cars03;
095500090327
095600090327        endsr;
095700090327
095800090327       // ----------------------------------------------------------------------
095900090327       //?Emetto il subfile
096000090327       // ----------------------------------------------------------------------
096100090327        begsr sr_sfl03;
096200090327
096300090327       //?emetto la videata
096400090327          write sb04t01;
096500090327          write sb04z03;
096600090327          exfmt sb04c03;
096700090327
096800090327       //?spengo indicatori di comodo
096900090327          *in28 = *off;
097000090327       //?pulisco campo messaggi
097100090327          clear v3cmsg;
097200090327       //?esecuzione comandi
097300090327          select;
097400090327
097500090327         //?f3=fine
097600090327            when *inkc;
097700090327              exsr sr_fine;
097800090327         //?f12=ritorno
097900090327            when *inkl;
098000090327              $video = 'S02';
098100090327              leavesr;
098200090327          endsl;
098300090327
098400090327        endsr;
098500090324
098600090327       // ----------------------------------------------------------------------
098700090327       //?Pulisco il subfile
098800090327       // ----------------------------------------------------------------------
098900090327        begsr sr_puls03;
099000090327
099100090327          clear nrr3;
099200110303          clear T03nest;
099300110303          clear T03nela;
099400090327
099500090327          *in22 = *off;
099600090327          *in23 = *off;
099700090327          write sb04c03;
099800090327          *in22 = *on;
099900090327          *in23 = *on;
100000090327
100100090327          recsf3 = 1;
100200090327          *in32 = *on;
100300090327
100400090327        endsr;
100500090327
100600090327       // ----------------------------------------------------------------------
100700090327       //?Carico il subfile
100800090327       // ----------------------------------------------------------------------
100900090327        begsr sr_cars03;
101000110303
101100110303          *in56 = *off;
101200090327
101300090327          dow not $ends03;
101400090327
101500090327       //?leggo i ksc
101600090327            exec sql
101700090327              fetch next from ksc into :s03idjob, :s03tim,
101800090327                                       :s03ksu, :s03ksc,
101900090402                                       :s03dir, :s03tad;
102000090327
102100090327       //?fine file o errore sql esco
102200090327            if sqlcod = 100 or sqlcod < 0;
102300090327              $ends03 = *on;
102400090909              leave;
102500090327            endif;
102600090327
102700090327       //?conto per idjob e papà
102800090327          exsr sr_contas03;
102900090327
103000090327       //?scrivo subfile
103100090327          exsr sr_wrts03;
103200090327
103300090327          enddo;
103400110303
103500110303       //?scrivo riga di totale nel subfile se ci sono più figli
103600110303          IF  nrr3 > 1;
103700110303            *in56 = *on;
103800110303            exsr sr_wrtT03;
103900110303            *in56 = *off;
104000110303          ENDIF;
104100090327
104200090327       //?indicatore per fine file
104300090327          *in32 = *on;
104400090327
104500090327          exec sql close ksc;
104600090327
104700090327        endsr;
104800090327
104900090327       // ----------------------------------------------------------------------
105000090327       //?Conto
105100090327       // ----------------------------------------------------------------------
105200090327        begsr sr_contas03;
105300090327
105400090327       //?immagini estratte
105500090327          exec sql
105600110303            select count(distinct lacnim) into :s03nest
105700090327              from tilac00f where lacidjob = :s03idjob and
105800090327              lacksu = :s03ksu and lacksc = :s03ksc and
105900090327              lacela <> '33';
106000090327          if sqlcod <> 0;
106100090327            clear s03nest;
106200090327          endif;
106300090327       //?immagini elaborate
106400090327          exec sql
106500110303            select count(distinct lacnim) into :s03nela
106600090327              from tilac00f where lacidjob = :s03idjob and
106700090327              lacksu = :s03ksu and lacksc = :s03ksc and
106800090327              lacela <> '33' and lacela <> '00';
106900090327          if sqlcod <> 0;
107000090327            clear s03nela;
107100090327          endif;
107200090327       //?addebiti
107300090402          if s03tad <> 'N';
107400090402            exec sql
107500110303              select count(distinct lacnim) into :s03ntas
107600090402                from tilac00f where lacidjob = :s03idjob and
107700090402                lacksu = :s03ksu and lacksc = :s03ksc and
107800090402                lacela <> '33' and lacela = '11';
107900090402            if sqlcod <> 0;
108000090402              clear s03ntas;
108100090402            endif;
108200090402          else;
108300090402            clear s03ntas;
108400090402          endif;
108500090327       //?controllo se ci sono dei rcd a 'x2' o a '9x'
108600090327          exec sql
108700110303            select count(distinct lacnim) into :s03nerr
108800090327              from tilac00f where lacidjob = :s03idjob and
108900090327              lacksu = :s03ksu and lacksc = :s03ksc and
109000090327              (substr(lacela, 2, 1) = '2' or
109100090327               substr(lacela, 1, 1) = '9');
109200090327          if sqlcod <> 0;
109300090327            clear s03nerr;
109400090327          endif;
109500090327
109600090327        endsr;
109700090327
109800090327       // ----------------------------------------------------------------------
109900090327       //?Scrivo il subfile
110000090327       // ----------------------------------------------------------------------
110100090327        begsr sr_wrts03;
110200090327
110300090327       //?imposto i campi del control
110400090327          v3cksu = s03ksu;
110500090327       //?ragione sociale da tabella LAC
110600090327          exec sql
110700090327            select tbeuni into :tbeuni from tntbe00f
110800090327              where tbecod = 'LAC' and tbeatb = ' ' and
110900090327                    substr(tbeke1, 1, 7) = digits(:s03ksu);
111000090327       //?non trovo il rcd corrispondente
111100090327          if sqlcod <> 0;
111200090327            clear tbeuni;
111300090327          endif;
111400090327          dlac = tbeuni;
111500090327          v3crag = §lacrag;
111600090327
111700090327       //?imposto i campi del subfile
111800090327       //?data
111900090327          v3sdata = %subst(s03tim:7:2) + '/' +
112000090327                    %subst(s03tim:5:2) + '/' +
112100090327                    %subst(s03tim:1:4);
112200090327       //?codice estrazione
112300090327          v3sksc = s03ksc;
112400090327       //?ragione sociale da tabella LAC
112500090327          exec sql
112600090327            select tbeuni into :tbeuni from tntbe00f
112700090327              where tbecod = 'LAC' and tbeatb = ' ' and
112800090327                    substr(tbeke1, 1, 7) = digits(:s03ksc);
112900090327       //?non trovo il rcd corrispondente
113000090327          if sqlcod <> 0;
113100090327            clear tbeuni;
113200090327          endif;
113300090327          dlac = tbeuni;
113400090327          v3srag = §lacrag;
113500090327       //?nr. immagini estratte
113600090327          v3sestr = s03nest;
113700090327       //?nr. immagini elaborate
113800090327          v3selab = s03nela;
113900090330       //?codice + ragione sociale in rosso se non elaborazione non terminata
114000090330          *in55 = (s03nest <> s03nela);
114100090327       //?se ho trovato dei rcd a 'x2' o a '9x'
114200090327          *in52 = (s03nerr > *zeros);
114300090327       //?CD si/no
114400090327          v3scd = v2scd;
114500090327       //?TAS si/no
114600090327          select;
114700090402            when s03tad = 'N';
114800090402              v3stas = 'NP';
114900090327            when s03ntas = *zeros;
115000090327              v3stas = 'NO';
115100090327              eval *in53 = *off;
115200090327            when s03ntas = s03nest;
115300090327              v3stas = 'SI';
115400090327              eval *in53 = *off;
115500090327            when s03ntas <> s03nest;
115600090327              v3stas = 'SI';
115700090327              eval *in53 = *on;
115800090327          endsl;
115900090327       //?directory
116000090327          v3sdir = s03dir;
116100110303
116200110303       //?sommo le immagini estratte e le elaborate
116300110303          T03nest += S03nest;
116400110303          T03nela += S03nela;
116500090327
116600090327          nrr3 = nrr3 + 1;
116700090327          write sb04s03;
116800090327
116900090327        endsr;
117000110303
117100110303       // ----------------------------------------------------------------------
117200110303       //?Scrivo riga di totale nel subfile.
117300110303       // ----------------------------------------------------------------------
117400110303        begsr sr_wrtT03;
117500110303
117600110303          *in52 = *off;
117700110303          *in53 = *off;
117800110303          *in55 = *off;
117900110303          clear v3sdata;
118000110303          clear v3sksc;
118100110303          v3srag = 'T O T A L E ';
118200110303          v3sestr = T03nest;
118300110303          v3selab = T03nela;
118400110303          clear v3scd;
118500110303          clear v3stas;
118600110303          clear v3stas;
118700110303          clear v3sdir;
118800110303          nrr3 = nrr3 + 1;
118900110303          write sb04s03;
119000110303
119100110303        endsr;
119200090327
119300090327       // ----------------------------------------------------------------------
119400090327       //?Ok fatto cd aggiorno TILAC
119500090327       // ----------------------------------------------------------------------
119600090327        begsr sr_okcd;
119700090327
119800090327          exec sql
119900090327            update tilac00f
120000090327            set lacutecd = :knmus, lactimecd = :wtimestp
120100090327            where
120200090327            lacidjob = :w2idjob and
120300090327              lacksu = :w2cksu  and lacela = '33';
120400090327
120500090327        endsr;
120600090327
120700090324       // ----------------------------------------------------------------------
120800090324       //?Fine programma
120900090324       // ----------------------------------------------------------------------
121000090327        begsr sr_fine;
121100090324
121200090327          *inlr = *on;
121300090327          return;
121400090324
121500090327        endsr;
121600090324
121700090324** MSG  Lungh. 78                                                            *
121800090326Immettere almeno una delle due date                                           01
121900090326Data errata                                                                   02
122000090326Range di date errato                                                          03
122100090326Immettere SOLO caratteri numerici                                             04
122200090326Codice errato                                                                 05
122300090330E' possibile selezionare solo una parzializzazione                            06
