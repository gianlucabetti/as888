000100080206      //--------------------------------------------------------------
000200101110      //?TIST41R - controllo per cancellazione CAP obsoleti
000300080206      //--------------------------------------------------------------
000400080206
000500080206     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600080206
000700080206      //---------------------------------------------------------------
000800080206      //?Dichiarazione file.
000900080206      //---------------------------------------------------------------
001000100312     fazorg01l  if   e           k disk
001100140213     ftiala03l  if   e           k disk
001200101115     fazcpl01l  uf   e           k disk
001300101115     fazcpc01l  uf   e           k disk
001400101115     fazcps01l  uf   e           k disk
001500101111     f
001600101115     fWFALO00F  iF a e             disk
001700080206      //---------------------------------------------------------------
001800080206      //?Definizione schiere.
001900080206      //---------------------------------------------------------------
002000080206      //---------------------------------------------------------------
002100080206      //?Definizione aree dati.
002200080206      //---------------------------------------------------------------
002300080206      // - Dati utente
002400080206     d §AzUte        e ds                  extname(AZUTE00F)
002500080206     d                                     dtaara
002600080206     d §DatiUte      e ds                  extname(dDatiUte)
002700080206     d                                     dtaara
002800080206
002900080206      //---------------------------------------------------------------
003000080206      //?Definizione strutture dati.
003100080206      //---------------------------------------------------------------
003200080206
003300080206      // - Parametri ricevuti
003400080206     d KPJBA         e ds
003500080206
003600080206      // - Reperimento dati utente
003700080206     d TIBS34ds      e ds
003800080206     d dUte01        e ds
003900080702     d DLAT          e ds
004000080206
004100080206      // - Ricerca/Controllo tabelle
004200080206     d TIBS02ds      e ds                  inz
004300100316
004400100316     d wlbdat          ds                  inz
004500100316     d  g02dat                 1      8  0
004600100316     d  g02inv                 9     16  0
004700100316     d  g02err                17     17
004800100316     d  g02tgi                18     22  0
004900101110     d
005000101110     d TIST41DS        ds
005100101110     d  ST41VERO               1      5
005200101110     d  ST41VERR               6     10
005300101115     d  ST41VERA              11     15
005400101115     d  ST41max               16     20
005500101115     d  ST41aggio             21     21
005600100316
005700080206      //---------------------------------------------------------------
005800080206      //?Definizione variabili globali.
005900080206      //---------------------------------------------------------------
006000080206
006100080206      // - Flags booleani
006200080208     d $Fine           s               n   inz(*off)
006300100902     d ErrGenerico     s               n   inz(*off)
006400080206
006500100901     d Indx            s              3  0 inz
006600101111     d kverO           s              5  0
006700101111     d kverR           s              5  0
006800101115     d kverA           s              5  0
006900101115     d kmin            s              5  0
007000101011     d PosM            s              2  0
007100101011     d LungM           s              2  0
007200101111     d i96datarif      s              8  0
007300101111     d o96ver          s              5  0
007400101111     d dataoggi        s              8  0
007500101115     d savnaz          s                   like(alonaz)
007600101115     d savcap          s                   like(alocap)
007700101115     d i_naz           s                   like(cplnar)
007800101111     d i_cap           s                   like(cplcap)
007900101111     d i_loc           s                   like(cplloc)
008000101111     d i_sum           s              9  0
008100080414
008200100901     d wrkgetlista     s           4096    varying
008300100901
008400100315     d Dataeur         s               d   datfmt(*eur)
008500100315     d Dataiso         s               d   datfmt(*iso)
008600080605     d Dataymd         s               d   datfmt(*ymd)
008700080605     d Datadmy         s               d   datfmt(*dmy)
008800100901     d Datasys         s               d   inz(*sys)
008900080604     d
009000080605     d                 DS
009100080605     d  AA                     1      4  0
009200080605     d  MM                     5      6  0
009300080605     d  GG                     7      8  0
009400080605     d DATA                    1      8  0
009500101111      // -Reperisce versione cappario
009600101111     d tisi96r         pr                  extpgm('TISI96R')
009700101111     d  i96datrif                     8  0
009800101111     d  o96ver                        5  0
009900101111
010000081009     d
010100080206      //---------------------------------------------------------------
010200080206      //?Definizione procedure usate.
010300080206      //---------------------------------------------------------------
010400080414      /copy gaitrasrc/srcprotopr,tibs34r
010500080206
010600080206      //---------------------------------------------------------------
010700080206      //?Riepilogo indicatori.
010800080206      //---------------------------------------------------------------
010900080207      // - Comuni
011000080207      // 28    : Emissione messaggio di errore a video
011100080207      // - C01/S01
011200080207      // 30    : Condiziona SFLDSP    (*not)
011300080207      // 31    : Condiziona SFLDSPCTL (*not)
011400080207      // 30+31 : Condiziona SFLCLR
011500100824      // 32    : Condiziona SFLNXTCHG
011600080207      // 50    : Errore: Opzione errata
011700080207      // - D01
011800080207      // - Comuni
011900080207      // 99    : Generico di Errore
012000080206      //---------------------------------------------------------------
012100080206
012200080206      //---------------------------------------------------------------
012300080206      //?M A I N - L I N E
012400080206      //---------------------------------------------------------------
012500080206
012600080627     c     *Entry        plist
012700080206     c                   parm                    KPJBA
012800101110     c                   movel     kpjbu         tist41ds
012900080702
013000080206      /free
013100080206
013200080206       // Operazioni iniziali
013300080206       exsr RoutInz;
013400081215
013500101111       // Prepara SQL con loc obsolete SENZA spedizioni
013600101111       exsr PrepaSQL1   ;
013700080206
013800100902       // Lettura record selezionati
013900101111       exsr LeggiSQL1   ;
014000100902
014100101111       // Prepara SQL con loc obsolete CON spedizioni
014200101111       exsr PrepaSQL2   ;
014300101111
014400101111       // Lettura record selezionati
014500101111       exsr LeggiSQL2   ;
014600101111
014700140128       // Prepara SQL con loc obsolete versione attuale non in WFALO
014800140128       exsr PrepaSQL3   ;
014900140128
015000140128       // Lettura record selezionati
015100140128       exsr LeggiSQL3   ;
015200140128
015300101115       // Se richiesto aggiorno versione in kverA
015400101115       if st41aggio='S'   ;
015500101115        exsr DelCAP   ;
015600101115       endif  ;
015700101115
015800080206       // Operazioni finali
015900080206       exsr RoutEnd;
016000080206
016100080206       //--------------------------------------------------------------
016200080206       //?Operazioni iniziali.
016300080206       //--------------------------------------------------------------
016400080206       BEGSR RoutInz;
016500100902       *in90  =*on;
016600080206
016700080206         // Reperimento dati job
016800100902         exsr DatiJob;
016900100826
017000100902       // Udate
017100101112       Dataoggi=%dec(datasys) ;
017200100917
017300101110       // Se versione riferimento CAP obsoleti vuota, uso la "29"
017400101110       if st41verO<=*zeros   ;
017500101110          KVERO=29  ;
017600101111          else ;
017700101111          kverO=%int(st41verO)     ;
017800101110       endif   ;
017900101111
018000101110       // Se versione riferimento attuale  vuota, uso quella decorrente a oggi
018100101110       if st41verR<=*zeros   ;
018200101111         i96datarif=dataoggi   ;
018300101111         clear o96ver   ;
018400101111         callp tisi96r (i96datarif:o96ver)  ;
018500101111          kverR=o96ver   ;
018600101111       else  ;
018700101111          kverR=%int(st41verR) ;
018800101111       endif   ;
018900101115
019000101115       // Se versione da aggioranre vuota, uso la "37"
019100101115       if st41verA<=*zeros   ;
019200101115          KVERA=37  ;
019300101115          else ;
019400101115          kverA=%int(st41verA)     ;
019500101115       endif   ;
019600101111
019700101115       // Se non impostato minimo spedizioni, imposto di default 20
019800101115       if st41max<=*zeros   ;
019900101115        kmin=20   ;
020000101115       else ;
020100101115       kmin=%int(st41max) ;
020200101115       endif  ;
020300101115
020400101112
020500101112         exec sql   DELETE from wfalo00f        ;
020600101112
020700080627         ENDSR;
020800080206
020900080206       //--------------------------------------------------------------
021000080206       //?Reperimento Dati del job (Utente/Operativi).
021100080206       //--------------------------------------------------------------
021200080206       BEGSR DatiJob;
021300080206
021400080206         in(E) §AzUte;
021500080206         if NOT %error;
021600080206           in(E) §DatiUte;
021700080206         endif;
021800080206         if %error or RSut = *blanks;
021900080206           clear TIBS34ds;
022000080206           tibs34r(tibs34ds);
022100080206           in §AzUte;
022200080206           in §DatiUte;
022300080206         endif;
022400080206
022500080206       ENDSR;
022600100901       //--------------------------------------------------------------
022700101111       //?Prepara SQL per loc obsolete  SENZA spedizioni
022800100901       //--------------------------------------------------------------
022900101111       BEGSR PrepaSQL1 ;
023000100901
023100101112       wrkgetlista='select cplnar, cplcap, cplloc  +
023200121217          from azcpl00f exception join tiala00f on cplcap = alacap and +
023300101112          cplnar=alanaz and +
023400101111          cplloc = alaloc where cplfi1 = ''O'' and cplver ='   +
023500101112          %editc(kverO:'X') + ' order by cplnar, cplcap, cplloc'        ;
023600101111
023700100901         exec sql   PREPARE s1 FROM :wrkgetlista   ;
023800101111
023900100901         exec sql   DECLARE a1 cursor FOR  s1   ;
024000100901
024100100901         exec sql OPEN a1   ;
024200100901
024300100901         ENDSR    ;
024400100902       //--------------------------------------------------------------
024500101111       //?Leggi record selezionati con SQL 1
024600100902       //--------------------------------------------------------------
024700101111       BEGSR LeggiSQL1 ;
024800100902
024900100902         clear sqlcod   ;
025000100902
025100100902         dow sqlcod=0   ;
025200100902
025300101112         exec sql FETCH NEXT  FROM a1 INTO: i_naz, :i_cap, :i_loc  ;
025400100902
025500100902         select ;
025600100902         when sqlcod=100 ;
025700100902
025800100902         when sqlcod<0   ;
025900100902         *inh1=*on   ;
026000100902
026100100902         other   ;
026200101111         clear i_sum    ;
026300101111         exsr Scrivifile  ;
026400100902         endsl  ;
026500100902
026600100902         enddo   ;
026700101111
026800101111       exec sql CLOSE a1   ;
026900100902
027000100902       ENDSR    ;
027100101111       //--------------------------------------------------------------
027200101111       //?Prepara SQL per loc obsolete  CON  spedizioni
027300101111       //--------------------------------------------------------------
027400101111       BEGSR PrepaSQL2 ;
027500101112       clear wrkgetlista   ;
027600101112          wrkgetlista='select alanaz, alacap, alaloc,  sum(alatos) +
027700121217              from azcpl00f inner join tiala00f on cplcap = alacap and +
027800101112              cplnar=alanaz and +
027900101111              cplloc = alaloc where cplfi1 = ''O'' and cplver = ' +
028000101112              %editc(kverO:'X') + ' group by alanaz, alacap, alaloc +
028100101112              order by alanaz, alacap, alaloc  ' ;
028200101111
028300101111         exec sql   PREPARE s2 FROM :wrkgetlista   ;
028400101111
028500101111         exec sql   DECLARE a2 cursor FOR  s2   ;
028600101111
028700101111         exec sql OPEN a2   ;
028800101111
028900101111         ENDSR    ;
029000101111       //--------------------------------------------------------------
029100101111       //?Leggi record selezionati con SQL 2
029200101111       //--------------------------------------------------------------
029300101111       BEGSR LeggiSQL2 ;
029400101111
029500101111         clear sqlcod   ;
029600101111
029700101111         dow sqlcod=0   ;
029800101111
029900101112         exec sql FETCH NEXT  FROM a2 INTO: i_naz, :i_cap, :i_loc,
030000101111                                           :I_sum ;
030100101111
030200101111         select ;
030300101111         when sqlcod=100 ;
030400101111
030500101111         when sqlcod<0   ;
030600101111         *inh1=*on   ;
030700101111
030800101111         other   ;
030900101111         exsr Scrivifile  ;
031000101111         endsl  ;
031100101111
031200101111         enddo   ;
031300101111
031400101111       exec sql CLOSE a2   ;
031500101111
031600101111       ENDSR    ;
031700140128       //--------------------------------------------------------------
031800140128       //?Prepara SQL per loc obsolete versione attuale non in WFALO
031900140128       //--------------------------------------------------------------
032000140128       BEGSR PrepaSQL3 ;
032100140128
032200140128       wrkgetlista='select cplnar, cplcap, cplloc  +
032300140128          from azcpl00f exception join WFALO00f on cplcap = alocap and +
032400140128          cplnar=alonaz and +
032500140128          cplloc = aloloc where cplfi1 = ''O'' and cplver ='   +
032600140128          %editc(kverR:'X') + ' order by cplnar, cplcap, cplloc'        ;
032700140128
032800140128         exec sql   PREPARE s3 FROM :wrkgetlista   ;
032900140128
033000140128         exec sql   DECLARE a3 cursor FOR  s3   ;
033100140128
033200140128         exec sql OPEN a3   ;
033300140128
033400140128         ENDSR    ;
033500140128       //--------------------------------------------------------------
033600140128       //?Leggi record selezionati con SQL 3
033700140128       //--------------------------------------------------------------
033800140128       BEGSR LeggiSQL3 ;
033900140128
034000140128         clear sqlcod   ;
034100140128
034200140128         dow sqlcod=0   ;
034300140128
034400140128         exec sql FETCH NEXT  FROM a3 INTO: i_naz, :i_cap, :i_loc  ;
034500140128
034600140128         select ;
034700140128         when sqlcod=100 ;
034800140128
034900140128         when sqlcod<0   ;
035000140128         *inh1=*on   ;
035100140128
035200140128         other   ;
035300140128         clear i_sum    ;
035400140213         // se non presenete in TIALA --> scrivo il file
035500140213         chain (i_naz:i_cap:i_loc) tiala03l  ;
035600140213         if not %found(tiala03l)  ;
035700140128         exsr Scrivifile  ;
035800140213         endif  ;
035900140128         endsl  ;
036000140128
036100140128         enddo   ;
036200140128
036300140128       exec sql CLOSE a3   ;
036400140128
036500140128       ENDSR    ;
036600100902       //--------------------------------------------------------------
036700101111       //?Scrivo file di work per controllo CAP e/o località obsolete
036800100902       //--------------------------------------------------------------
036900101111       BEGSR Scrivifile            ;
037000101111
037100101111       clear wfalo000   ;
037200101111
037300101111       aloverO=kverO ;
037400101111       alocap=i_cap ;
037500101111       alonaz=i_naz ;
037600101111       aloloc= i_loc  ;
037700101111       alotnsp= i_sum ;
037800101111       aloverR=kverR ;
037900101111
038000101111       // Verifico se obsoleto anche nella versione di riferimento
038100101115       chain(n) (kverR:alonaz:alocap:aloloc)  azcpl01l   ;
038200101111
038300101111       if %found(azcpl01l) and cplfi1='O'   ;
038400101111         aloobsr='O'   ;
038500101111
038600101111         // Verifico se CAP obsoleto  nella versione di riferimento
038700101115         chain(n) (kverR:alonaz:alocap)  azcpc01l   ;
038800101111
038900101111         if %found(azcpc01l) and cpcfi1='O'   ;
039000101111           alocapo='O'   ;
039100101111         endif  ;
039200101112       else  ;
039300101112         if not %found(azcpl01l) ;
039400101112         aloobsr='N'   ;
039500101112         endif  ;
039600101111       endif    ;
039700101111
039800101111       write wfalo000   ;
039900100902
040000100902       ENDSR    ;
040100101115       //--------------------------------------------------------------
040200101115       //?Deleto CAP obsoleti, impostando una "X" nel falg annullamento
040300101115       //--------------------------------------------------------------
040400101115       BEGSR DelCAP ;
040500101115
040600101115       setll *start  wfalo00f   ;
040700101115       read          wfalo00f   ;
040800101115
040900101115 1     dow  not %eof(wfalo00f)  ;
041000101115
041100101115       // Elaboro   solo le località che sono ancora obsolete con spedizioni < del minimo
041200101115 2     if aloobsr='O'  and alotnsp<20  ;
041300101115       chain    (kverA:alonaz:alocap:aloloc)  azcpl01l   ;
041400101115 3     if %found(azcpl01l)  and  cplfi1='O'  ;
041500101115
041600101115       // Se esistono sinonimi li cancello   ;
041700101115 4        if cpless='S'   ;
041800101115             setll    (kverA:alonaz:alocap:aloloc)  azcps01l   ;
041900101115             reade    (kverA:alonaz:alocap:aloloc)  azcps01l   ;
042000101115 5           dow  not %eof(azcps01l)  ;
042100101115             cpsatb='X'  ;
042200101115             update azcps000  ;
042300101115
042400101115             reade    (kverA:alonaz:alocap:aloloc)  azcps01l   ;
042500101115 5           enddo   ;
042600101115 4        endif    ;
042700101115
042800101115         cplatb='X'  ;
042900101115         update azcpl000  ;
043000101115 3     endif  ;
043100101115 2     endif  ;
043200101115
043300101115       read          wfalo00f   ;
043400101115 1     enddo   ;
043500101115
043600101115
043700101115       // Secondo giro: dei cap cpmpletamente obsoleti, cancello solo se ho eliminsato
043800101115       //  tutte le locolità (con la "X")
043900101115       clear savcap   ;
044000101115       clear savnaz   ;
044100101115       setll *start  wfalo00f   ;
044200101115       read          wfalo00f   ;
044300101115
044400101115 1     dow  not %eof(wfalo00f)  ;
044500101115 2     if alocapo='O'  and (alocap<>savcap  or alonaz<>savnaz);
044600101115          chain    (kverA:alonaz:alocap)  azcpc01l   ;
044700101115 3        if %found(azcpc01l) and cpcatb<>'X'  ;
044800101115       // col cap leggo tutte le locaità
044900101115             setll    (kverA:alonaz:alocap)  azcpl01l   ;
045000101115             reade    (kverA:alonaz:alocap)  azcpl01l   ;
045100101115 4           dow  not %eof(azcpl01l)  and cplatb='X' ;
045200101115             reade    (kverA:alonaz:alocap)  azcpl01l   ;
045300101115 4           enddo   ;
045400101115
045500101115 4     if cplatb='X'  ;
045600101115             cpcatb='X'   ;
045700101115             update azcpc000 ;
045800101115 4           endif  ;
045900101115 3     endif  ;
046000101115
046100101115       savcap=alocap  ;
046200101115       savnaz=alonaz  ;
046300101115 2     endif  ;
046400101115
046500101115       read          wfalo00f   ;
046600101115 1     enddo   ;
046700101115
046800101115       ENDSR    ;
046900100902       //--------------------------------------------------------------
047000100902       //?Operazioni finali.
047100100902       //--------------------------------------------------------------
047200100902       BEGSR RoutEnd;
047300100902
047400100920
047500100902         *inLR = *on;
047600100902
047700100902         return;
047800100902
047900100902       ENDSR;
048000100902
048100100902      /end-free
048200100902
