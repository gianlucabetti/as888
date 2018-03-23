000100080530      *PARMS DFTACTGRP(*NO) ACTGRP(QILE) OPTION(*NOXREF) DATEDIT(*yMd.)
000200080206      //--------------------------------------------------------------
000300101110      //?TIST41R - controllo per cancellazione CAP obsoleti
000400080206      //--------------------------------------------------------------
000500080206
000600080206     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000700080206
000800080206      //---------------------------------------------------------------
000900080206      //?Dichiarazione file.
001000080206      //---------------------------------------------------------------
001100100312     fazorg01l  if   e           k disk
001200140213     ftiala03l  if   e           k disk
001300101115     fazcpl01l  uf   e           k disk
001400101115     fazcpc01l  uf   e           k disk
001500101115     fazcps01l  uf   e           k disk
001600101111     f
001700101115     fWFALO00F  iF a e             disk
001800080206      //---------------------------------------------------------------
001900080206      //?Definizione schiere.
002000080206      //---------------------------------------------------------------
002100080206      //---------------------------------------------------------------
002200080206      //?Definizione aree dati.
002300080206      //---------------------------------------------------------------
002400080206      // - Dati utente
002500080206     d §AzUte        e ds                  extname(AZUTE00F)
002600080206     d                                     dtaara
002700080206     d §DatiUte      e ds                  extname(dDatiUte)
002800080206     d                                     dtaara
002900080206
003000080206      //---------------------------------------------------------------
003100080206      //?Definizione strutture dati.
003200080206      //---------------------------------------------------------------
003300080206
003400080206      // - Parametri ricevuti
003500080206     d KPJBA         e ds
003600080206
003700080206      // - Reperimento dati utente
003800080206     d TIBS34ds      e ds
003900080206     d dUte01        e ds
004000080702     d DLAT          e ds
004100080206
004200080206      // - Ricerca/Controllo tabelle
004300080206     d TIBS02ds      e ds                  inz
004400100316
004500100316     d wlbdat          ds                  inz
004600100316     d  g02dat                 1      8  0
004700100316     d  g02inv                 9     16  0
004800100316     d  g02err                17     17
004900100316     d  g02tgi                18     22  0
005000101110     d
005100101110     d TIST41DS        ds
005200101110     d  ST41VERO               1      5
005300101110     d  ST41VERR               6     10
005400101115     d  ST41VERA              11     15
005500101115     d  ST41max               16     20
005600101115     d  ST41aggio             21     21
005700100316
005800080206      //---------------------------------------------------------------
005900080206      //?Definizione variabili globali.
006000080206      //---------------------------------------------------------------
006100080206
006200080206      // - Flags booleani
006300080208     d $Fine           s               n   inz(*off)
006400100902     d ErrGenerico     s               n   inz(*off)
006500080206
006600100901     d Indx            s              3  0 inz
006700101111     d kverO           s              5  0
006800101111     d kverR           s              5  0
006900101115     d kverA           s              5  0
007000101115     d kmin            s              5  0
007100101011     d PosM            s              2  0
007200101011     d LungM           s              2  0
007300101111     d i96datarif      s              8  0
007400101111     d o96ver          s              5  0
007500101111     d dataoggi        s              8  0
007600101115     d savnaz          s                   like(alonaz)
007700101115     d savcap          s                   like(alocap)
007800101115     d i_naz           s                   like(cplnar)
007900101111     d i_cap           s                   like(cplcap)
008000101111     d i_loc           s                   like(cplloc)
008100101111     d i_sum           s              9  0
008200080414
008300100901     d wrkgetlista     s           4096    varying
008400100901
008500100315     d Dataeur         s               d   datfmt(*eur)
008600100315     d Dataiso         s               d   datfmt(*iso)
008700080605     d Dataymd         s               d   datfmt(*ymd)
008800080605     d Datadmy         s               d   datfmt(*dmy)
008900100901     d Datasys         s               d   inz(*sys)
009000080604     d
009100080605     d                 DS
009200080605     d  AA                     1      4  0
009300080605     d  MM                     5      6  0
009400080605     d  GG                     7      8  0
009500080605     d DATA                    1      8  0
009600101111      // -Reperisce versione cappario
009700101111     d tisi96r         pr                  extpgm('TISI96R')
009800101111     d  i96datrif                     8  0
009900101111     d  o96ver                        5  0
010000101111
010100081009     d
010200080206      //---------------------------------------------------------------
010300080206      //?Definizione procedure usate.
010400080206      //---------------------------------------------------------------
010500080414      /copy gaitrasrc/srcprotopr,tibs34r
010600080206
010700080206      //---------------------------------------------------------------
010800080206      //?Riepilogo indicatori.
010900080206      //---------------------------------------------------------------
011000080207      // - Comuni
011100080207      // 28    : Emissione messaggio di errore a video
011200080207      // - C01/S01
011300080207      // 30    : Condiziona SFLDSP    (*not)
011400080207      // 31    : Condiziona SFLDSPCTL (*not)
011500080207      // 30+31 : Condiziona SFLCLR
011600100824      // 32    : Condiziona SFLNXTCHG
011700080207      // 50    : Errore: Opzione errata
011800080207      // - D01
011900080207      // - Comuni
012000080207      // 99    : Generico di Errore
012100080206      //---------------------------------------------------------------
012200080206
012300080206      //---------------------------------------------------------------
012400080206      //?M A I N - L I N E
012500080206      //---------------------------------------------------------------
012600080206
012700080627     c     *Entry        plist
012800080206     c                   parm                    KPJBA
012900101110     c                   movel     kpjbu         tist41ds
013000080702
013100080206      /free
013200080206
013300080206       // Operazioni iniziali
013400080206       exsr RoutInz;
013500081215
013600101111       // Prepara SQL con loc obsolete SENZA spedizioni
013700101111       exsr PrepaSQL1   ;
013800080206
013900100902       // Lettura record selezionati
014000101111       exsr LeggiSQL1   ;
014100100902
014200101111       // Prepara SQL con loc obsolete CON spedizioni
014300101111       exsr PrepaSQL2   ;
014400101111
014500101111       // Lettura record selezionati
014600101111       exsr LeggiSQL2   ;
014700101111
014800140128       // Prepara SQL con loc obsolete versione attuale non in WFALO
014900140128       exsr PrepaSQL3   ;
015000140128
015100140128       // Lettura record selezionati
015200140128       exsr LeggiSQL3   ;
015300140128
015400101115       // Se richiesto aggiorno versione in kverA
015500101115       if st41aggio='S'   ;
015600101115        exsr DelCAP   ;
015700101115       endif  ;
015800101115
015900080206       // Operazioni finali
016000080206       exsr RoutEnd;
016100080206
016200080206       //--------------------------------------------------------------
016300080206       //?Operazioni iniziali.
016400080206       //--------------------------------------------------------------
016500080206       BEGSR RoutInz;
016600100902       *in90  =*on;
016700080206
016800080206         // Reperimento dati job
016900100902         exsr DatiJob;
017000100826
017100100902       // Udate
017200101112       Dataoggi=%dec(datasys) ;
017300100917
017400101110       // Se versione riferimento CAP obsoleti vuota, uso la "29"
017500101110       if st41verO<=*zeros   ;
017600101110          KVERO=29  ;
017700101111          else ;
017800101111          kverO=%int(st41verO)     ;
017900101110       endif   ;
018000101111
018100101110       // Se versione riferimento attuale  vuota, uso quella decorrente a oggi
018200101110       if st41verR<=*zeros   ;
018300101111         i96datarif=dataoggi   ;
018400101111         clear o96ver   ;
018500101111         callp tisi96r (i96datarif:o96ver)  ;
018600101111          kverR=o96ver   ;
018700101111       else  ;
018800101111          kverR=%int(st41verR) ;
018900101111       endif   ;
019000101115
019100101115       // Se versione da aggioranre vuota, uso la "37"
019200101115       if st41verA<=*zeros   ;
019300101115          KVERA=37  ;
019400101115          else ;
019500101115          kverA=%int(st41verA)     ;
019600101115       endif   ;
019700101111
019800101115       // Se non impostato minimo spedizioni, imposto di default 20
019900101115       if st41max<=*zeros   ;
020000101115        kmin=20   ;
020100101115       else ;
020200101115       kmin=%int(st41max) ;
020300101115       endif  ;
020400101115
020500101112
020600101112         exec sql   DELETE from wfalo00f        ;
020700101112
020800080627         ENDSR;
020900080206
021000080206       //--------------------------------------------------------------
021100080206       //?Reperimento Dati del job (Utente/Operativi).
021200080206       //--------------------------------------------------------------
021300080206       BEGSR DatiJob;
021400080206
021500080206         in(E) §AzUte;
021600080206         if NOT %error;
021700080206           in(E) §DatiUte;
021800080206         endif;
021900080206         if %error or RSut = *blanks;
022000080206           clear TIBS34ds;
022100080206           tibs34r(tibs34ds);
022200080206           in §AzUte;
022300080206           in §DatiUte;
022400080206         endif;
022500080206
022600080206       ENDSR;
022700100901       //--------------------------------------------------------------
022800101111       //?Prepara SQL per loc obsolete  SENZA spedizioni
022900100901       //--------------------------------------------------------------
023000101111       BEGSR PrepaSQL1 ;
023100100901
023200101112       wrkgetlista='select cplnar, cplcap, cplloc  +
023300121217          from azcpl00f exception join tiala00f on cplcap = alacap and +
023400101112          cplnar=alanaz and +
023500101111          cplloc = alaloc where cplfi1 = ''O'' and cplver ='   +
023600101112          %editc(kverO:'X') + ' order by cplnar, cplcap, cplloc'        ;
023700101111
023800100901         exec sql   PREPARE s1 FROM :wrkgetlista   ;
023900101111
024000100901         exec sql   DECLARE a1 cursor FOR  s1   ;
024100100901
024200100901         exec sql OPEN a1   ;
024300100901
024400100901         ENDSR    ;
024500100902       //--------------------------------------------------------------
024600101111       //?Leggi record selezionati con SQL 1
024700100902       //--------------------------------------------------------------
024800101111       BEGSR LeggiSQL1 ;
024900100902
025000100902         clear sqlcod   ;
025100100902
025200100902         dow sqlcod=0   ;
025300100902
025400101112         exec sql FETCH NEXT  FROM a1 INTO: i_naz, :i_cap, :i_loc  ;
025500100902
025600100902         select ;
025700100902         when sqlcod=100 ;
025800100902
025900100902         when sqlcod<0   ;
026000100902         *inh1=*on   ;
026100100902
026200100902         other   ;
026300101111         clear i_sum    ;
026400101111         exsr Scrivifile  ;
026500100902         endsl  ;
026600100902
026700100902         enddo   ;
026800101111
026900101111       exec sql CLOSE a1   ;
027000100902
027100100902       ENDSR    ;
027200101111       //--------------------------------------------------------------
027300101111       //?Prepara SQL per loc obsolete  CON  spedizioni
027400101111       //--------------------------------------------------------------
027500101111       BEGSR PrepaSQL2 ;
027600101112       clear wrkgetlista   ;
027700101112          wrkgetlista='select alanaz, alacap, alaloc,  sum(alatos) +
027800121217              from azcpl00f inner join tiala00f on cplcap = alacap and +
027900101112              cplnar=alanaz and +
028000101111              cplloc = alaloc where cplfi1 = ''O'' and cplver = ' +
028100101112              %editc(kverO:'X') + ' group by alanaz, alacap, alaloc +
028200101112              order by alanaz, alacap, alaloc  ' ;
028300101111
028400101111         exec sql   PREPARE s2 FROM :wrkgetlista   ;
028500101111
028600101111         exec sql   DECLARE a2 cursor FOR  s2   ;
028700101111
028800101111         exec sql OPEN a2   ;
028900101111
029000101111         ENDSR    ;
029100101111       //--------------------------------------------------------------
029200101111       //?Leggi record selezionati con SQL 2
029300101111       //--------------------------------------------------------------
029400101111       BEGSR LeggiSQL2 ;
029500101111
029600101111         clear sqlcod   ;
029700101111
029800101111         dow sqlcod=0   ;
029900101111
030000101112         exec sql FETCH NEXT  FROM a2 INTO: i_naz, :i_cap, :i_loc,
030100101111                                           :I_sum ;
030200101111
030300101111         select ;
030400101111         when sqlcod=100 ;
030500101111
030600101111         when sqlcod<0   ;
030700101111         *inh1=*on   ;
030800101111
030900101111         other   ;
031000101111         exsr Scrivifile  ;
031100101111         endsl  ;
031200101111
031300101111         enddo   ;
031400101111
031500101111       exec sql CLOSE a2   ;
031600101111
031700101111       ENDSR    ;
031800140128       //--------------------------------------------------------------
031900140128       //?Prepara SQL per loc obsolete versione attuale non in WFALO
032000140128       //--------------------------------------------------------------
032100140128       BEGSR PrepaSQL3 ;
032200140128
032300140128       wrkgetlista='select cplnar, cplcap, cplloc  +
032400140128          from azcpl00f exception join WFALO00f on cplcap = alocap and +
032500140128          cplnar=alonaz and +
032600140128          cplloc = aloloc where cplfi1 = ''O'' and cplver ='   +
032700140128          %editc(kverR:'X') + ' order by cplnar, cplcap, cplloc'        ;
032800140128
032900140128         exec sql   PREPARE s3 FROM :wrkgetlista   ;
033000140128
033100140128         exec sql   DECLARE a3 cursor FOR  s3   ;
033200140128
033300140128         exec sql OPEN a3   ;
033400140128
033500140128         ENDSR    ;
033600140128       //--------------------------------------------------------------
033700140128       //?Leggi record selezionati con SQL 3
033800140128       //--------------------------------------------------------------
033900140128       BEGSR LeggiSQL3 ;
034000140128
034100140128         clear sqlcod   ;
034200140128
034300140128         dow sqlcod=0   ;
034400140128
034500140128         exec sql FETCH NEXT  FROM a3 INTO: i_naz, :i_cap, :i_loc  ;
034600140128
034700140128         select ;
034800140128         when sqlcod=100 ;
034900140128
035000140128         when sqlcod<0   ;
035100140128         *inh1=*on   ;
035200140128
035300140128         other   ;
035400140128         clear i_sum    ;
035500140213         // se non presenete in TIALA --> scrivo il file
035600140213         chain (i_naz:i_cap:i_loc) tiala03l  ;
035700140213         if not %found(tiala03l)  ;
035800140128         exsr Scrivifile  ;
035900140213         endif  ;
036000140128         endsl  ;
036100140128
036200140128         enddo   ;
036300140128
036400140128       exec sql CLOSE a3   ;
036500140128
036600140128       ENDSR    ;
036700100902       //--------------------------------------------------------------
036800101111       //?Scrivo file di work per controllo CAP e/o località obsolete
036900100902       //--------------------------------------------------------------
037000101111       BEGSR Scrivifile            ;
037100101111
037200101111       clear wfalo000   ;
037300101111
037400101111       aloverO=kverO ;
037500101111       alocap=i_cap ;
037600101111       alonaz=i_naz ;
037700101111       aloloc= i_loc  ;
037800101111       alotnsp= i_sum ;
037900101111       aloverR=kverR ;
038000101111
038100101111       // Verifico se obsoleto anche nella versione di riferimento
038200101115       chain(n) (kverR:alonaz:alocap:aloloc)  azcpl01l   ;
038300101111
038400101111       if %found(azcpl01l) and cplfi1='O'   ;
038500101111         aloobsr='O'   ;
038600101111
038700101111         // Verifico se CAP obsoleto  nella versione di riferimento
038800101115         chain(n) (kverR:alonaz:alocap)  azcpc01l   ;
038900101111
039000101111         if %found(azcpc01l) and cpcfi1='O'   ;
039100101111           alocapo='O'   ;
039200101111         endif  ;
039300101112       else  ;
039400101112         if not %found(azcpl01l) ;
039500101112         aloobsr='N'   ;
039600101112         endif  ;
039700101111       endif    ;
039800101111
039900101111       write wfalo000   ;
040000100902
040100100902       ENDSR    ;
040200101115       //--------------------------------------------------------------
040300101115       //?Deleto CAP obsoleti, impostando una "X" nel falg annullamento
040400101115       //--------------------------------------------------------------
040500101115       BEGSR DelCAP ;
040600101115
040700101115       setll *start  wfalo00f   ;
040800101115       read          wfalo00f   ;
040900101115
041000101115 1     dow  not %eof(wfalo00f)  ;
041100101115
041200101115       // Elaboro   solo le località che sono ancora obsolete con spedizioni < del minimo
041300101115 2     if aloobsr='O'  and alotnsp<20  ;
041400101115       chain    (kverA:alonaz:alocap:aloloc)  azcpl01l   ;
041500101115 3     if %found(azcpl01l)  and  cplfi1='O'  ;
041600101115
041700101115       // Se esistono sinonimi li cancello   ;
041800101115 4        if cpless='S'   ;
041900101115             setll    (kverA:alonaz:alocap:aloloc)  azcps01l   ;
042000101115             reade    (kverA:alonaz:alocap:aloloc)  azcps01l   ;
042100101115 5           dow  not %eof(azcps01l)  ;
042200101115             cpsatb='X'  ;
042300101115             update azcps000  ;
042400101115
042500101115             reade    (kverA:alonaz:alocap:aloloc)  azcps01l   ;
042600101115 5           enddo   ;
042700101115 4        endif    ;
042800101115
042900101115         cplatb='X'  ;
043000101115         update azcpl000  ;
043100101115 3     endif  ;
043200101115 2     endif  ;
043300101115
043400101115       read          wfalo00f   ;
043500101115 1     enddo   ;
043600101115
043700101115
043800101115       // Secondo giro: dei cap cpmpletamente obsoleti, cancello solo se ho eliminsato
043900101115       //  tutte le locolità (con la "X")
044000101115       clear savcap   ;
044100101115       clear savnaz   ;
044200101115       setll *start  wfalo00f   ;
044300101115       read          wfalo00f   ;
044400101115
044500101115 1     dow  not %eof(wfalo00f)  ;
044600101115 2     if alocapo='O'  and (alocap<>savcap  or alonaz<>savnaz);
044700101115          chain    (kverA:alonaz:alocap)  azcpc01l   ;
044800101115 3        if %found(azcpc01l) and cpcatb<>'X'  ;
044900101115       // col cap leggo tutte le locaità
045000101115             setll    (kverA:alonaz:alocap)  azcpl01l   ;
045100101115             reade    (kverA:alonaz:alocap)  azcpl01l   ;
045200101115 4           dow  not %eof(azcpl01l)  and cplatb='X' ;
045300101115             reade    (kverA:alonaz:alocap)  azcpl01l   ;
045400101115 4           enddo   ;
045500101115
045600101115 4     if cplatb='X'  ;
045700101115             cpcatb='X'   ;
045800101115             update azcpc000 ;
045900101115 4           endif  ;
046000101115 3     endif  ;
046100101115
046200101115       savcap=alocap  ;
046300101115       savnaz=alonaz  ;
046400101115 2     endif  ;
046500101115
046600101115       read          wfalo00f   ;
046700101115 1     enddo   ;
046800101115
046900101115       ENDSR    ;
047000100902       //--------------------------------------------------------------
047100100902       //?Operazioni finali.
047200100902       //--------------------------------------------------------------
047300100902       BEGSR RoutEnd;
047400100902
047500100920
047600100902         *inLR = *on;
047700100902
047800100902         return;
047900100902
048000100902       ENDSR;
048100100902
048200100902      /end-free
048300100902
