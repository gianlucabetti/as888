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
014700101115       // Se richiesto aggiorno versione in kverA
014800101115       if st41aggio='S'   ;
014900101115        exsr DelCAP   ;
015000101115       endif  ;
015100101115
015200080206       // Operazioni finali
015300080206       exsr RoutEnd;
015400080206
015500080206       //--------------------------------------------------------------
015600080206       //?Operazioni iniziali.
015700080206       //--------------------------------------------------------------
015800080206       BEGSR RoutInz;
015900100902       *in90  =*on;
016000080206
016100080206         // Reperimento dati job
016200100902         exsr DatiJob;
016300100826
016400100902       // Udate
016500101112       Dataoggi=%dec(datasys) ;
016600100917
016700101110       // Se versione riferimento CAP obsoleti vuota, uso la "29"
016800101110       if st41verO<=*zeros   ;
016900101110          KVERO=29  ;
017000101111          else ;
017100101111          kverO=%int(st41verO)     ;
017200101110       endif   ;
017300101111
017400101110       // Se versione riferimento attuale  vuota, uso quella decorrente a oggi
017500101110       if st41verR<=*zeros   ;
017600101111         i96datarif=dataoggi   ;
017700101111         clear o96ver   ;
017800101111         callp tisi96r (i96datarif:o96ver)  ;
017900101111          kverR=o96ver   ;
018000101111       else  ;
018100101111          kverR=%int(st41verR) ;
018200101111       endif   ;
018300101115
018400101115       // Se versione da aggioranre vuota, uso la "37"
018500101115       if st41verA<=*zeros   ;
018600101115          KVERA=37  ;
018700101115          else ;
018800101115          kverA=%int(st41verA)     ;
018900101115       endif   ;
019000101111
019100101115       // Se non impostato minimo spedizioni, imposto di default 20
019200101115       if st41max<=*zeros   ;
019300101115        kmin=20   ;
019400101115       else ;
019500101115       kmin=%int(st41max) ;
019600101115       endif  ;
019700101115
019800101112
019900101112         exec sql   DELETE from wfalo00f        ;
020000101112
020100080627         ENDSR;
020200080206
020300080206       //--------------------------------------------------------------
020400080206       //?Reperimento Dati del job (Utente/Operativi).
020500080206       //--------------------------------------------------------------
020600080206       BEGSR DatiJob;
020700080206
020800080206         in(E) §AzUte;
020900080206         if NOT %error;
021000080206           in(E) §DatiUte;
021100080206         endif;
021200080206         if %error or RSut = *blanks;
021300080206           clear TIBS34ds;
021400080206           tibs34r(tibs34ds);
021500080206           in §AzUte;
021600080206           in §DatiUte;
021700080206         endif;
021800080206
021900080206       ENDSR;
022000100901       //--------------------------------------------------------------
022100101111       //?Prepara SQL per loc obsolete  SENZA spedizioni
022200100901       //--------------------------------------------------------------
022300101111       BEGSR PrepaSQL1 ;
022400100901
022500101112       wrkgetlista='select cplnar, cplcap, cplloc  +
022600121217          from azcpl00f exception join tiala00f on cplcap = alacap and +
022700101112          cplnar=alanaz and +
022800101111          cplloc = alaloc where cplfi1 = ''O'' and cplver ='   +
022900101112          %editc(kverO:'X') + ' order by cplnar, cplcap, cplloc'        ;
023000101111
023100100901         exec sql   PREPARE s1 FROM :wrkgetlista   ;
023200101111
023300100901         exec sql   DECLARE a1 cursor FOR  s1   ;
023400100901
023500100901         exec sql OPEN a1   ;
023600100901
023700100901         ENDSR    ;
023800100902       //--------------------------------------------------------------
023900101111       //?Leggi record selezionati con SQL 1
024000100902       //--------------------------------------------------------------
024100101111       BEGSR LeggiSQL1 ;
024200100902
024300100902         clear sqlcod   ;
024400100902
024500100902         dow sqlcod=0   ;
024600100902
024700101112         exec sql FETCH NEXT  FROM a1 INTO: i_naz, :i_cap, :i_loc  ;
024800100902
024900100902         select ;
025000100902         when sqlcod=100 ;
025100100902
025200100902         when sqlcod<0   ;
025300100902         *inh1=*on   ;
025400100902
025500100902         other   ;
025600101111         clear i_sum    ;
025700101111         exsr Scrivifile  ;
025800100902         endsl  ;
025900100902
026000100902         enddo   ;
026100101111
026200101111       exec sql CLOSE a1   ;
026300100902
026400100902       ENDSR    ;
026500101111       //--------------------------------------------------------------
026600101111       //?Prepara SQL per loc obsolete  CON  spedizioni
026700101111       //--------------------------------------------------------------
026800101111       BEGSR PrepaSQL2 ;
026900101112       clear wrkgetlista   ;
027000101112          wrkgetlista='select alanaz, alacap, alaloc,  sum(alatos) +
027100121217              from azcpl00f inner join tiala00f on cplcap = alacap and +
027200101112              cplnar=alanaz and +
027300101111              cplloc = alaloc where cplfi1 = ''O'' and cplver = ' +
027400101112              %editc(kverO:'X') + ' group by alanaz, alacap, alaloc +
027500101112              order by alanaz, alacap, alaloc  ' ;
027600101111
027700101111         exec sql   PREPARE s2 FROM :wrkgetlista   ;
027800101111
027900101111         exec sql   DECLARE a2 cursor FOR  s2   ;
028000101111
028100101111         exec sql OPEN a2   ;
028200101111
028300101111         ENDSR    ;
028400101111       //--------------------------------------------------------------
028500101111       //?Leggi record selezionati con SQL 2
028600101111       //--------------------------------------------------------------
028700101111       BEGSR LeggiSQL2 ;
028800101111
028900101111         clear sqlcod   ;
029000101111
029100101111         dow sqlcod=0   ;
029200101111
029300101112         exec sql FETCH NEXT  FROM a2 INTO: i_naz, :i_cap, :i_loc,
029400101111                                           :I_sum ;
029500101111
029600101111         select ;
029700101111         when sqlcod=100 ;
029800101111
029900101111         when sqlcod<0   ;
030000101111         *inh1=*on   ;
030100101111
030200101111         other   ;
030300101111         exsr Scrivifile  ;
030400101111         endsl  ;
030500101111
030600101111         enddo   ;
030700101111
030800101111       exec sql CLOSE a2   ;
030900101111
031000101111       ENDSR    ;
031100100902       //--------------------------------------------------------------
031200101111       //?Scrivo file di work per controllo CAP e/o località obsolete
031300100902       //--------------------------------------------------------------
031400101111       BEGSR Scrivifile            ;
031500101111
031600101111       clear wfalo000   ;
031700101111
031800101111       aloverO=kverO ;
031900101111       alocap=i_cap ;
032000101111       alonaz=i_naz ;
032100101111       aloloc= i_loc  ;
032200101111       alotnsp= i_sum ;
032300101111       aloverR=kverR ;
032400101111
032500101111       // Verifico se obsoleto anche nella versione di riferimento
032600101115       chain(n) (kverR:alonaz:alocap:aloloc)  azcpl01l   ;
032700101111
032800101111       if %found(azcpl01l) and cplfi1='O'   ;
032900101111         aloobsr='O'   ;
033000101111
033100101111         // Verifico se CAP obsoleto  nella versione di riferimento
033200101115         chain(n) (kverR:alonaz:alocap)  azcpc01l   ;
033300101111
033400101111         if %found(azcpc01l) and cpcfi1='O'   ;
033500101111           alocapo='O'   ;
033600101111         endif  ;
033700101112       else  ;
033800101112         if not %found(azcpl01l) ;
033900101112         aloobsr='N'   ;
034000101112         endif  ;
034100101111       endif    ;
034200101111
034300101111       write wfalo000   ;
034400100902
034500100902       ENDSR    ;
034600101115       //--------------------------------------------------------------
034700101115       //?Deleto CAP obsoleti, impostando una "X" nel falg annullamento
034800101115       //--------------------------------------------------------------
034900101115       BEGSR DelCAP ;
035000101115
035100101115       setll *start  wfalo00f   ;
035200101115       read          wfalo00f   ;
035300101115
035400101115 1     dow  not %eof(wfalo00f)  ;
035500101115
035600101115       // Elaboro   solo le località che sono ancora obsolete con spedizioni < del minimo
035700101115 2     if aloobsr='O'  and alotnsp<20  ;
035800101115       chain    (kverA:alonaz:alocap:aloloc)  azcpl01l   ;
035900101115 3     if %found(azcpl01l)  and  cplfi1='O'  ;
036000101115
036100101115       // Se esistono sinonimi li cancello   ;
036200101115 4        if cpless='S'   ;
036300101115             setll    (kverA:alonaz:alocap:aloloc)  azcps01l   ;
036400101115             reade    (kverA:alonaz:alocap:aloloc)  azcps01l   ;
036500101115 5           dow  not %eof(azcps01l)  ;
036600101115             cpsatb='X'  ;
036700101115             update azcps000  ;
036800101115
036900101115             reade    (kverA:alonaz:alocap:aloloc)  azcps01l   ;
037000101115 5           enddo   ;
037100101115 4        endif    ;
037200101115
037300101115         cplatb='X'  ;
037400101115         update azcpl000  ;
037500101115 3     endif  ;
037600101115 2     endif  ;
037700101115
037800101115       read          wfalo00f   ;
037900101115 1     enddo   ;
038000101115
038100101115
038200101115       // Secondo giro: dei cap cpmpletamente obsoleti, cancello solo se ho eliminsato
038300101115       //  tutte le locolità (con la "X")
038400101115       clear savcap   ;
038500101115       clear savnaz   ;
038600101115       setll *start  wfalo00f   ;
038700101115       read          wfalo00f   ;
038800101115
038900101115 1     dow  not %eof(wfalo00f)  ;
039000101115 2     if alocapo='O'  and (alocap<>savcap  or alonaz<>savnaz);
039100101115          chain    (kverA:alonaz:alocap)  azcpc01l   ;
039200101115 3        if %found(azcpc01l) and cpcatb<>'X'  ;
039300101115       // col cap leggo tutte le locaità
039400101115             setll    (kverA:alonaz:alocap)  azcpl01l   ;
039500101115             reade    (kverA:alonaz:alocap)  azcpl01l   ;
039600101115 4           dow  not %eof(azcpl01l)  and cplatb='X' ;
039700101115             reade    (kverA:alonaz:alocap)  azcpl01l   ;
039800101115 4           enddo   ;
039900101115
040000101115 4     if cplatb='X'  ;
040100101115             cpcatb='X'   ;
040200101115             update azcpc000 ;
040300101115 4           endif  ;
040400101115 3     endif  ;
040500101115
040600101115       savcap=alocap  ;
040700101115       savnaz=alonaz  ;
040800101115 2     endif  ;
040900101115
041000101115       read          wfalo00f   ;
041100101115 1     enddo   ;
041200101115
041300101115       ENDSR    ;
041400100902       //--------------------------------------------------------------
041500100902       //?Operazioni finali.
041600100902       //--------------------------------------------------------------
041700100902       BEGSR RoutEnd;
041800100902
041900100920
042000100902         *inLR = *on;
042100100902
042200100902         return;
042300100902
042400100902       ENDSR;
042500100902
042600100902      /end-free
042700100902
