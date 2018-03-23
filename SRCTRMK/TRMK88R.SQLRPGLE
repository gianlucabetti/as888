000100080206      //--------------------------------------------------------------
000200100426      //?TRMK88R - Report Commerciale
000300080206      //--------------------------------------------------------------
000400080206
000500090407     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600090807     h dftactgrp(*no) actgrp(*caller)
000700080206
000800080206      //---------------------------------------------------------------
000900080206      //?Dichiarazione file.
001000080206      //---------------------------------------------------------------
001100100325     fTABEL00F  if   e           k disk
001200100519     fTNCPO01L  if   e           k disk
001300100517     fTIVIS05L  if   e           k disk
001400100519     fCNCLP00F  if   e           k disk
001500100519     fAZORG01L  if   e           k disk
001600130917     fAZCMM01L  if   e           k disk
001700100423     fWFRCA00F  o    e             disk
001800100519     fWFRPA00F  o    e             disk
001900100310
002000080206      //---------------------------------------------------------------
002100090406      //?Definizione costanti.
002200080206      //---------------------------------------------------------------
002300080207
002400080206
002500080206      //---------------------------------------------------------------
002600080206      //?Definizione schiere.
002700080206      //---------------------------------------------------------------
002800100325
002900100325      // - Agenti
003000100325     d $Age            s              7  0 dim(3500)
003100100325
003200100325      // - Agenti Unificanti
003300100325     d $AgeU           s              7  0 dim(3500)
003400100519
003500110421     d RCACaaids       ds            70    inz
003600110421     d   $CAAI                        2    inz  dim(35)
003700100519
003800100520     d RCANraids       ds                  inz
003900110421     d   $NRAI                        5s 0 inz  dim(35)
004000100604
004100110421     d RCACaapmds      ds            70    inz
004200110421     d   $CAAPM                       2    inz  dim(35)
004300100604
004400110421     d RCANrapmds      ds                  inz
004500110421     d   $NRAPM                       5s 0 inz  dim(35)
004600100608
004700110421     d RCACaapgds      ds            70    inz
004800110421     d   $CAAPG                       2    inz  dim(35)
004900100608
005000110421     d RCANrapgds      ds                  inz
005100110421     d   $NRAPG                       5s 0 inz  dim(35)
005200100608
005300110421     d RCACaaptds      ds            70    inz
005400110421     d   $CAAPT                       2    inz  dim(35)
005500100608
005600100608     d RCANraptds      ds                  inz
005700110421     d   $NRAPT                       5s 0 inz  dim(35)
005800100608
005900110421     d RCACaacfds      ds            70    inz
006000110421     d   $CAACF                       2    inz  dim(35)
006100100608
006200100608     d RCANracfds      ds                  inz
006300110421     d   $NRACF                       5s 0 inz  dim(35)
006400100608
006500110421     d RCACaactds      ds            70    inz
006600110421     d   $CAACT                       2    inz  dim(35)
006700100608
006800100608     d RCANractds      ds                  inz
006900110421     d   $NRACT                       5s 0 inz  dim(35)
007000110421
007100110421     d RCACaappds      ds            70    inz
007200110421     d   $CAAPP                       2    inz  dim(35)
007300110421
007400110421     d RCANrappds      ds                  inz
007500110421     d   $NRAPP                       5s 0 inz  dim(35)
007600100610
007700110421     d RCACaapptds     ds            70    inz
007800110421     d   $CAAPPT                      2    inz  dim(35)
007900110421
008000110421     d RCANrapptds     ds                  inz
008100110421     d   $NRAPPT                      5s 0 inz  dim(35)
008200110421
008300100610      // causali da stampare
008400110421     d $CTE            s              2    dim(35) ctdata perrcd(35)
008500080206      //---------------------------------------------------------------
008600080206      //?Definizione aree dati.
008700080206      //---------------------------------------------------------------
008800080206
008900080206      // - Dati utente
009000080206     d §AzUte        e ds                  extname(AZUTE00F)
009100080206     d                                     dtaara
009200080206     d §DatiUte      e ds                  extname(dDatiUte)
009300080206     d                                     dtaara
009400100423
009500100423     d tiatcDS       e ds                  ExtName(tiatc00f)
009600080206
009700080206      //---------------------------------------------------------------
009800080206      //?Definizione strutture dati.
009900080206      //---------------------------------------------------------------
010000080206
010100080206      // - Status
010200080206     d Psds           sds
010300080206     d   SDSpgm          *proc
010400080206
010500080206      // - Parametri ricevuti
010600080206     d KPJBA         e ds
010700100423     d Trmk88DS      e ds
010800100308
010900080206      // - Reperimento dati utente
011000100305     d TIBS34DS      e ds
011100100326
011200100326      // - controllo abilitazioni
011300100326     d TNTAA1DS      e ds                  inz
011400100325
011500100325      // - Carico Filiali abilitate all'utente
011600100325     d TRUL31DS      e ds
011700100325     d POG                    10    759    DIM(250)
011800110120
011900110120      // - Carico Aree abilitate all'utente
012000110120     d TRUL31DS2     e ds
012100110120
012200110120      // - Tabella 05 = Codici Aree
012300110120     d ds05          e ds                  inz
012400090807
012500100519      // - Tabella 17 = Distretto
012600100519     d ds17          e ds                  inz
012700100519
012800100819      // - Flag operativi tiatc
012900100819     d datc01        e ds                  inz
013000100519
013100090629
013200080206      //---------------------------------------------------------------
013300080206      //?Definizione variabili globali.
013400080206      //---------------------------------------------------------------
013500080206
013600080206      // - Flags booleani
013700100305     d $End            s               n   inz(*off)
013800100325     d $EndAge         s               n   inz(*off)
013900100305     d $RcdOk          s               n   inz(*off)
014000100623     d $Scrivic        s               n   inz(*off)
014100100623     d $Scrivip        s               n   inz(*off)
014200100325
014300100325      // - Indici di schiera
014400100326     d xx              s              4  0 inz
014500100326     d yy              s              4  0 inz
014600110421     d zz              s              4  0 inz
014700110421     d xxi             s              4  0 inz
014800110421     d wwi             s              4  0 inz
014900110421     d xpm             s              4  0 inz
015000110421     d wpm             s              4  0 inz
015100110421     d xpg             s              4  0 inz
015200110421     d wpg             s              4  0 inz
015300100608     d xpt             s              4  0 inz
015400100608     d wpt             s              4  0 inz
015500100608     d xcf             s              4  0 inz
015600100608     d wcf             s              4  0 inz
015700100608     d xct             s              4  0 inz
015800100608     d wct             s              4  0 inz
015900110421     d xpp             s              4  0 inz
016000110421     d wpp             s              4  0 inz
016100110421     d xppt            s              4  0 inz
016200110421     d wppt            s              4  0 inz
016300080206
016400090806       // - Stringa SQL da eseguire
016500111020     d wSQL            s          30000A   Varying        inz
016600080206
016700090401      // - Campi di comodo data
016800100305     d wDate_EUR       s               d   datfmt(*eur)
016900100305     d wDate_ISO       s               d   datfmt(*iso)
017000100305
017100100305      // - Campi di comodo
017200100519     d watcoii         s              6  0 inz
017300100519     d watcofi         s              6  0 inz
017400110421     d w_catepo        s              1    inz
017500100329     d wOggi           s              8  0 inz
017600100518     d w_Iso_oiF       s               t   inz
017700100518     d w_Iso_oiI       s               t   inz
017800100518     d wTot_M          s              5  0 inz
017900100519     d wTot_H          s              5  0 inz
018000100519     d wTot_R          s              5  0 inz
018100100519     d WTot_M_FP       s              7  0 inz
018200100518     d WTot_M_RA       s              7  0 inz
018300100518     d WTot_M_RC       s              7  0 inz
018400100518     d WTot_M_UF       s              7  0 inz
018500100519     d WTot_M_FEP      s              7  0 inz
018600100519     d WTot_M_UFP      s              7  0 inz
018700100325
018800090508      //---------------------------------------------------------------
018900090807      //?Definizione procedure esterne.
019000090508      //---------------------------------------------------------------
019100100120
019200100120
019300100120      //---------------------------------------------------------------
019400100120      //?prototipi
019500100120      //---------------------------------------------------------------
019600090807
019700080626      /copy gaitrasrc/srcprotopr,tibs34r
019800100326      /copy gaitrasrc/srcprotopr,tntaa1c
019900110120
020000110120      // - Filiali e aree abilitate
020100110120     d trul31r         pr                  extpgm('TRUL31R')
020200110120     d  kpjba                              likeds(KPJBA)
020300110120     d  trul31ds                           likeds(trul31ds)
020400110120     d  trul31ds2                          likeds(trul31ds2)
020500090807
020600080206      //---------------------------------------------------------------
020700080206      //?Definizione key-list.
020800080206      //---------------------------------------------------------------
020900100325
021000080206
021100080206      //---------------------------------------------------------------
021200080206      //?Riepilogo indicatori.
021300080206      //---------------------------------------------------------------
021400100326
021500090807
021600080206      //---------------------------------------------------------------
021700080206
021800080206      //---------------------------------------------------------------
021900080206      //?M A I N - L I N E
022000080206      //---------------------------------------------------------------
022100080206
022200080206     c     *Entry        plist
022300080206     c                   parm                    KPJBA
022400080206
022500080206      /free
022600080206
022700090807       //?Operazioni iniziali
022800080206       exsr RoutInz;
022900100325
023000100423       //?Ciclo per sk unificanti
023100100326       zz = 1;
023200100326       FOR zz by 1 to %elem($AgeU);
023300100326         IF  $AgeU(zz) <> *zeros;
023400100326
023500100326         //?Carico sk agenti dell'unificante
023600100326           exsr sr_CarAge;
023700100305
023800100423         //?Leggo le attività per commerciale unificante
023900100423               exsr sr_Attivita ;
024000100329             ENDIF;
024100100329           ENDFOR;
024200100324
024300100325
024400090807       //?Operazioni finali
024500080206       exsr RoutEnd;
024600080206
024700080206       //--------------------------------------------------------------
024800080206       //?Operazioni iniziali.
024900080206       //--------------------------------------------------------------
025000080206       BEGSR RoutInz;
025100100329
025200100329         //?Imposto la ds con i dati della KPJBU
025300100423         Trmk88ds = kpjbu;
025400090807
025500090807         //?Reperimento dati job
025600090807         exsr DatiJob;
025700090807
025800090807         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
025900100305
026000100305         //?Imposto data e ora elaborazione
026100100330         wOggi     = %dec(%date());
026200100330         wDate_ISO = %date(wOggi:*ISO);
026300100330         wDate_EUR = wDate_ISO;
026400100325
026500100325         clear $AgeU;
026600100325
026700100325         //?Carico in sk l'unificante richiesto
026800100519         IF i88cmmu > 0;
026900100519           $AgeU(01) = i88cmmu;
027000100325         ENDIF;
027100100325
027200100325         //?Carico gli unificanti delle filiali gestite dall'utente
027300110120       //?o dell'area se richiesta
027400100519         IF i88cmmu = 0;
027500100325           exsr sr_CarAgeUni;
027600100325           sorta $AgeU;
027700100325         ENDIF;
027800100610
027900080206       ENDSR;
028000100326
028100100326       //--------------------------------------------------------------
028200100326       //?Carico Sk Agenti.
028300100326       //--------------------------------------------------------------
028400100326       BEGSR sr_CarAge;
028500100326
028600100326         clear xx;
028700100326         clear $Age;
028800100326         $EndAge = *off;
028900100326
029000100326         exec sql
029100100326          declare AGE cursor for
029200130917          select CMMcod, CMMuni
029300130917          from AZCMM00F
029400130917          order by CMMcod;
029500100326
029600100326         exec sql
029700100326           open AGE;
029800100326           IF sqlcode < 0;
029900100326             $EndAge = *on;
030000100326           ENDIF;
030100100326
030200100326         DOW not $EndAge;
030300100326           exec sql
030400130917             fetch next from AGE into :CMMcod, :CMMuni;
030500100326             IF sqlcod = 100 or sqlcod < 0;
030600100326               $EndAge = *on;
030700100326               leave;
030800100326             ENDIF;
030900130917           IF  CMMuni = $AgeU(zz);
031000130917             xx += 1;
031100130917             $Age(xx) = CMMcod;
031200130917           ENDIF;
031300100326         ENDDO;
031400100326
031500100326         exec sql close AGE;
031600100326
031700100326         //?Se non ho cricato niente memorizzo almeno il codice richiesto
031800100326         IF $Age(01) = 0;
031900100326           $Age(01) = $AgeU(zz);
032000100326         ENDIF;
032100100326
032200100326       ENDSR;
032300100324
032400100305       //--------------------------------------------------------------
032500100423       //?Leggo le attività per tutti i commerciali legati all'unificante
032600100305       //--------------------------------------------------------------
032700100423       BEGSR sr_Attivita ;
032800100426
032900100623
033000100623         $scrivic  = *off ;
033100100623         $scrivip  = *off ;
033200100517       // pulisco il record  di wfrca / wfrpa
033300100519       If i88tcpe = 'C' or i88tcpe = 'E' ;
033400100517         clear wfrca000 ;
033500100518         Clear WTot_M_Fp ;
033600100518         Clear WTot_M_Ra ;
033700100518         Clear WTot_M_Rc ;
033800100518         Clear WTot_M_Uf ;
033900100519         Clear WTot_M_Fep;
034000100519         Clear WTot_M_Ufp;
034100100520         CLEAR RCACaaids  ;
034200100520         CLEAR RCANraids  ;
034300110421         CLEAR RCACaapmds  ;
034400110421         CLEAR RCANrapmds  ;
034500110421         CLEAR RCACaapgds  ;
034600110421         CLEAR RCANrapgds  ;
034700100610         CLEAR RCACaaptds  ;
034800100610         CLEAR RCANraptds  ;
034900100610         CLEAR RCACaacfds  ;
035000100610         CLEAR RCANracfds  ;
035100100610         CLEAR RCACaactds  ;
035200110421         CLEAR RCANractds  ;
035300110421         CLEAR RCACaappds  ;
035400110421         CLEAR RCANrappds  ;
035500110421         CLEAR RCACaapptds  ;
035600110421         CLEAR RCANrapptds  ;
035700110421         clear wwi;
035800110421         clear wpm;
035900110421         clear wpg;
036000100610         clear wpt;
036100100610         clear wcf;
036200100610         clear wct;
036300110421         clear wpp;
036400110421         clear wppt;
036500110421          $CAAI  = $CTE ;
036600110421          $CAAPM = $CTE ;
036700110421          $CAAPG = $CTE ;
036800100610          $CAAPT = $CTE ;
036900100610          $CAACF = $CTE ;
037000100610          $CAACT = $CTE ;
037100110421          $CAAPP = $CTE ;
037200110421          $CAAPPT= $CTE ;
037300100517       endif ;
037400100519       If i88tcpe = 'P' or i88tcpe = 'E' ;
037500100517         clear wfrpa000 ;
037600101008         Clear WTot_M_Fp ;
037700101008         Clear WTot_M_Ra ;
037800101008         Clear WTot_M_Rc ;
037900101008         Clear WTot_M_Uf ;
038000101008         Clear WTot_M_Fep;
038100101008         Clear WTot_M_Ufp;
038200100517       endif ;
038300100305
038400100305         $End = *off;
038500100305
038600100305         //?Imposto la stringa per SQL
038700100305         exsr  sr_PrepSQL;
038800100305
038900100305         //?Dichiarazione cursore
039000100305         exec sql
039100100305           prepare S1   from :wSQL;
039200100305         exec sql
039300100423           declare c1   cursor for S1;
039400100305
039500100305         //?Apertura del cursore
039600100305         exec sql
039700100423           open c1 ;
039800100305
039900100305         IF sqlcode < 0;
040000100305           $End = *on;
040100100305         ENDIF;
040200100305
040300100305         DOW not $End;
040400100305           exec sql
040500100423             fetch next from C1  into :tiatcds;
040600100305           IF sqlcod = 100 or sqlcod < 0;
040700100305             $End = *on;
040800100305             leave;
040900100305           ENDIF;
041000100305
041100100324         //?Controllo se ok il rcd che sto leggendo
041200100526           exsr sr_CtrRcdOk;
041300100526           IF  not $RcdOk;
041400100526             iter;
041500100526           ENDIF;
041600100526
041700100517           exsr sr_Eseguo   ;
041800100305         ENDDO;
041900100305
042000100305         exec sql
042100100423           close c1 ;
042200100426
042300100519               If i88tcpe = 'C' or i88tcpe = 'E' ;
042400100623         // se contata almeno un'attività da conteggiare  scrivo
042500100623         // un record altrimenti passo all'unificante successivo
042600100623                  If $scrivic = *on ;
042700100623                     exsr SR_writeRCA ;
042800100623                  endif;
042900100517               endif ;
043000100519               If i88tcpe = 'P' or i88tcpe = 'E' ;
043100100623         // se contata almeno un'attività da conteggiare  scrivo
043200100623         // un record altrimenti passo all'unificante successivo
043300100623                  If $scrivip = *on ;
043400100623                     exsr SR_writeRPA ;
043500100623                  endif;
043600100517               endif ;
043700100520         // endif ;
043800100305
043900100305       ENDSR;
044000100330
044100100526       //--------------------------------------------------------------
044200100526       //?Controllo se il rcd che sto leggendo è OK per la statistica
044300100526       //--------------------------------------------------------------
044400100526       BEGSR Sr_CtrRcdOk;
044500100526
044600100526         $RcdOk = *on;
044700100526         //?CHIODO
044800100526         //?Se attvità qualsiasi chiusa con attività 'AA'
044900100528         //?non devo caricare in statistica
045000100526         IF  ATCcac = 'AA';
045100100526           $RcdOk = *off;
045200100526         ENDIF;
045300100526
045400100526       ENDSR;
045500100526
045600100326       //--------------------------------------------------------------
045700100423       // Eseguo i conteggi
045800100326       //--------------------------------------------------------------
045900100423       BEGSR sr_Eseguo     ;
046000100819
046100100819         // recupero i dati dei flag operativi
046200100819           datc01 = atcflo ;
046300110421
046400110421         // recupero dal flag operativi la categioria del potenziale
046500110421         // Per adesso Cuffaro vuole la categoria al momento della creazione dell'attività
046600110421
046700110421           clear W_catepo ;
046800110421           If §atccapo1 = ' ' ;
046900110421              If §atccapo2 <> ' ' ;
047000110421                  W_catepo  = §atccapo2  ;
047100110421              Endif ;
047200110421           Else ;
047300110421              W_catepo  = §atccapo1  ;
047400110421           Endif ;
047500100819
047600100519         // imposto  ore inizio e fine impegno nel formato che voglio
047700100519           watcoii = atcoii * 100;
047800100519           watcofi = atcofi * 100;
047900100819
048000100819         // verifico se attività legata a trattativa aggancio trattativa e se fittizia
048100100819         // o non esiste passo al record successivo
048200100819         If atcnra > 0 ;
048300100819            chain atcnra tivis05l ;
048400100819            If (%found(tivis05l) and visffz = 'S') or not %found(tivis05l);
048500100819               leavesr ;
048600100819            endif ;
048700100819         endif ;
048800100819
048900100819         If atcnrv > 0 ;
049000100819            chain atcnrv tivis05l ;
049100100819            If (%found(tivis05l) and visffz = 'S') or not %found(tivis05l);
049200100819               leavesr ;
049300100819            endif ;
049400100819         endif ;
049500100819
049600100517         // imposto i vari valori in base all'attività che leggo  CONSUNTIVO
049700100517
049800100519       If i88tcpe = 'C' or i88tcpe = 'E' ;
049900100426
050000100605         // Conto le TELEFONATE effettuate da un'agente unificante ed effettuate nel periodo
050100100605         If ATCTAT = 'T' and %lookup(atccco:$age) > 0  and
050200100605            atcdco >= i88deci and atcdco <= i88decf  ;
050300100604
050400100604         // potenziali mai trattati
050500100819         // codice cliente zero e flag mai trattato uguale a "S"
050600110421            If atcksc = *zeros and §ATCmaitr = 'S' and W_catepo = 'M' ;
050700100426               Rcatepm+= 1 ;
050800100623               $scrivic  = *on ;
050900100604            // attribuisco ad ogni causale il numero delle telefonate effettuate
051000100604            // cerco di caricare le causali nella schiera
051100110421               xpm =  %lookup(atccac: $Caapm);
051200110421               If xpm = 0 ;
051300110421                  wpm +=1 ;
051400110421                  $Caapm(wpm) = Atccac ;
051500110421                  xpm = wpm ;
051600100604               endif ;
051700110421               If xpm > 0 ;
051800110421                  $Nrapm(xpm) +=1 ;
051900100623                  $scrivic  = *on ;
052000100604               Endif ;
052100100605
052200100610               If atccac <> '91' ;
052300100610                  leavesr ;
052400100610               Endif ;
052500100426            Endif;
052600100426
052700110117         // caldi
052800110117         // data scadenza attività > zero e
052900110117         // causale 31   o causale 44
053000110117            If atcdad > *zeros and (atccad = '31' or atccad = '44');
053100110117
053200100426
053300100426            // Verifico se la trattativa è mia oppure no
053400110421              If atccad = '31' ;
053500100519               if %lookup(viscmm:$age) > 0 ;
053600100517                  Rcatepc31m += 1 ;
053700100623                  $scrivic  = *on ;
053800100517               else ;
053900100517                  Rcatepc31a += 1 ;
054000100623                  $scrivic  = *on ;
054100100517               endif ;
054200110421              endif ;
054300110421
054400110421              If atccad = '44' ;
054500110421               if %lookup(viscmm:$age) > 0 ;
054600110421                  Rcatepc44m += 1 ;
054700110421                  $scrivic  = *on ;
054800110421               else ;
054900110421                  Rcatepc44a += 1 ;
055000110421                  $scrivic  = *on ;
055100110421               endif ;
055200110421              endif ;
055300100605
055400100610               If atccac <> '91' ;
055500100610                  leavesr ;
055600100610               Endif ;
055700100426            Endif;
055800100517
055900100517         // post Vendita
056000100605         // data scadenza attività > zero , codice cliente > zero e
056100100517         // causale 32
056200100605            If atcdad > *zeros and atcksc <> *zeros and atccad = '32' ;
056300100517
056400100517            // Verifico se cliente mio oppure no
056500100517             clpkut = 1 ;
056600100517             clpkcc = dutkci ;
056700100517             clpksc = atcksc ;
056800100519             chain (clpkut:clpkcc:clpksc) Cnclp00f ;
056900100517               if %lookup(clpage:$age) > 0 ;
057000100517                  Rcatepv32m += 1 ;
057100100623                  $scrivic  = *on ;
057200100517               else ;
057300100517                  Rcatepv32a += 1 ;
057400100623                  $scrivic  = *on ;
057500100517               endif ;
057600100605
057700100610               If atccac <> '91' ;
057800100610                  leavesr ;
057900100610               Endif ;
058000100605
058100100517            Endif;
058200100517
058300100605         // Mantenimenti telefonici clienti
058400100605         // codice cliente > zero e
058500100517         // causale varie mie e di altri positive e negative
058600110929            If  atcnrv = 0 and atccac = '40' ;
058700100517
058800100517            // Verifico se cliente mio oppure no
058900100517             clpkut = 1 ;
059000100517             clpkcc = dutkci ;
059100100517             clpksc = atcksc ;
059200100519             chain (clpkut:clpkcc:clpksc) Cnclp00f ;
059300100517               if %lookup(clpage:$age) > 0 ;
059400100517
059500100517                     Rcatepc40m += 1 ;
059600100623                     $scrivic  = *on ;
059700100605                     leavesr ;
059800100517
059900100517             // di altri
060000100517               Else ;
060100100517
060200100517                     Rcatepc40a += 1 ;
060300100623                     $scrivic  = *on ;
060400100605                     leavesr ;
060500100517
060600100517               endif ;
060700100517            Endif;
060800100517
060900100517         // richiami post offerta
061000100605         // data scadenza attività > zero , trattativa >  zero e
061100100605         // causale 30/33
061200100605            If atcdad > *zeros and atcnrv > *zeros ;
061300100517
061400100517            // Verifico se la trattativa è mia oppure no
061500100519               if %lookup(viscmm:$age) > 0 ;
061600100517
061700100517            // causale 30
061800100521                  If atccad = '30' ;
061900100517                     Rcatepo30m += 1 ;
062000100623                     $scrivic  = *on ;
062100100610                     If atccac <> '91' ;
062200100610                        leavesr ;
062300100610                     Endif ;
062400100517                  endif ;
062500100517            // causale 33
062600100521                  If atccad = '33' ;
062700100517                     Rcatepo33m += 1 ;
062800100623                     $scrivic  = *on ;
062900100610                     If atccac <> '91' ;
063000100610                        leavesr ;
063100100610                     Endif ;
063200100517                  endif ;
063300100517               else ;
063400100517
063500100517            // causale 30
063600100521                  If atccad = '30' ;
063700100517                     Rcatepo30a += 1 ;
063800100623                     $scrivic  = *on ;
063900100610                     If atccac <> '91' ;
064000100610                        leavesr ;
064100100610                     Endif ;
064200100517                  endif ;
064300100517            // causale 33
064400100521                  If atccad = '33' ;
064500100517                     Rcatepo33a += 1 ;
064600100623                     $scrivic  = *on ;
064700100610                     If atccac <> '91' ;
064800100610                        leavesr ;
064900100610                     Endif ;
065000100517                  endif ;
065100100517               endif ;
065200100517            Endif;
065300100605
065400100605          // Tutto quello che rimane fuori come telefonate le conteggio nelle varie schiere
065500100610          // carico le causali
065600100605
065700100623          // Telefonate su potenziale fuori trattattiva già trattati ;
065800110427            If atcnrv=0 and (W_catepo = 'E' or
065900110427                      (§ATCmaitr=' ' and W_catepo='M'));
066000100608            // attribuisco ad ogni causale il numero delle telefonate effettuate
066100100608            // cerco di caricare le causali nella schiera
066200100702
066300110421                  xpg =  %lookup(atccac: $Caapg);
066400110421               If xpg > 0 ;
066500110421                  $Nrapg(xpg) +=1 ;
066600100623                  $scrivic  = *on ;
066700100608               Endif ;
066800100608            endif ;
066900110421
067000110421          // Telefonate su potenziale persi fuori trattattiva ;
067100110421            If atcnrv=0  and  W_catepo='P' ;
067200110421            // attribuisco ad ogni causale il numero delle telefonate effettuate
067300110421            // cerco di caricare le causali nella schiera
067400110421
067500110421                  xpp =  %lookup(atccac: $Caapp);
067600110421               If xpp > 0 ;
067700110421                  $Nrapp(xpp) +=1 ;
067800110421                  $scrivic  = *on ;
067900110421               Endif ;
068000110421            endif ;
068100100605
068200100608          // Telefonate su potenziale dentro trattattiva
068300110421            If atcnrv > 0 and (W_catepo='M'or W_catepo='E') and atccad <> '44'
068400110421               and atccad <> '30' and atccad <> '31' and atccad <> '33' ;
068500100608            // attribuisco ad ogni causale il numero delle telefonate effettuate
068600100608            // cerco di caricare le causali nella schiera
068700100702
068800100702                  xpt =  %lookup(atccac: $Caapt);
068900100608               If xpt > 0 ;
069000100608                  $Nrapt(xpt) +=1 ;
069100100623                  $scrivic  = *on ;
069200100608               Endif ;
069300100608            endif ;
069400100608
069500110421          // Telefonate su potenziale perso dentro trattattiva
069600110421            If atcnrv > 0 and W_catepo='P' and atccad <> '44'
069700110421               and atccad <> '30' and atccad <> '31' and atccad <> '33' ;
069800110421            // attribuisco ad ogni causale il numero delle telefonate effettuate
069900110421            // cerco di caricare le causali nella schiera
070000110421
070100110421                  xppt =  %lookup(atccac: $Caappt);
070200110421               If xppt > 0 ;
070300110929                  $Nrappt(xppt) +=1 ;
070400110421                  $scrivic  = *on ;
070500110421               Endif ;
070600110421            endif ;
070700110421
070800100608          // Telefonate su clienti fuori trattattiva
070900110421            If atcnrv = 0 and atccad <> '32' and W_catepo = 'C' ;
071000100608            // attribuisco ad ogni causale il numero delle telefonate effettuate
071100100608            // cerco di caricare le causali nella schiera
071200100702
071300100702                  xcf =  %lookup(atccac: $Caacf);
071400100608               If xcf > 0 ;
071500100608                  $Nracf(xcf) +=1 ;
071600100623                  $scrivic  = *on ;
071700100608               Endif ;
071800100608            endif ;
071900100608
072000100608          // Telefonate su clienti dentro trattattiva
072100110421            If W_catepo = 'C' and atcnrv > 0  and
072200110117               atccad <> '30' and atccad <> '31' and atccad <> '33' and
072300110117               atccad <> '44' ;
072400100608            // attribuisco ad ogni causale il numero delle telefonate effettuate
072500100608            // cerco di caricare le causali nella schiera
072600100702
072700100702                  xct =  %lookup(atccac: $Caact);
072800100608               If xct > 0 ;
072900100608                  $Nract(xct) +=1 ;
073000100623                  $scrivic  = *on ;
073100100608               Endif ;
073200100608            endif ;
073300100608
073400100608         ENDIF ;
073500100517
073600100622         // Conto gli APPUNTAMENTI presi da un'agente unificante ed effettuate nel periodo
073700100622         If atccac = '91' and %lookup(atccco:$age) > 0  and
073800100622            atcdco >= i88deci and atcdco <= i88decf  ;
073900100622
074000100622         // 1° appuntamento preso
074100100622         // trattativa aperta maggiore di zero
074200100622            If atcnra > *zeros ;
074300100622
074400100622            // Verifico se la trattativa è mantenimento o incremento
074500100623                  $scrivic  = *on ;
074600100622                     If VisTpv = 'N' ;
074700100622                        Rcapatn += 1 ;
074800100622                     endif ;
074900100622                     If VisTpv = 'M' ;
075000100622                        Rcapatm += 1 ;
075100100622                     endif ;
075200100622                     If VisTpv = 'A' ;
075300100622                        Rcapata += 1 ;
075400100622                     endif ;
075500100622                     If VisTpv = 'C' ;
075600100622                        Rcapatc += 1 ;
075700100622                     endif ;
075800110118                     If VisTpv = 'I' ;
075900110118                        Rcapati += 1 ;
076000110118                     endif ;
076100110118                     If VisTpv = 'R' ;
076200110118                        Rcapatr += 1 ;
076300110118                     endif ;
076400100622             endif ;
076500100623         ENDIF ;
076600100622
076700100622         // appuntamenti presi esclusi il primo appuntamento
076800100623         // Conto gli APPUNTAMENTI presi da un'agente unificante ed effettuate nel periodo
076900100623         If atccad = '91' and %lookup(atcco3:$age) > 0  and
077000100629            atcdim >= i88deci and atcdim <= i88decf  ;
077100100623
077200100622         // trattativa  maggiore di zero
077300100622            If atcnrv > *zeros ;
077400100622
077500100622            // Verifico e suddivido per tipo trattativa
077600100623                  $scrivic  = *on ;
077700100622                     If VisTpv = 'N' ;
077800100622                        Rcaapptn += 1 ;
077900100622                     endif ;
078000100622                     If VisTpv = 'M' ;
078100100622                        Rcaapptm += 1 ;
078200100622                     endif ;
078300100622                     If VisTpv = 'A' ;
078400100622                        Rcaappta += 1 ;
078500100622                     endif ;
078600100622                     If VisTpv = 'C' ;
078700100622                        Rcaapptc += 1 ;
078800100622                     endif ;
078900110118                     If VisTpv = 'I' ;
079000110118                        Rcaappti += 1 ;
079100110118                     endif ;
079200110118                     If VisTpv = 'R' ;
079300110118                        Rcaapptr += 1 ;
079400110118                     endif ;
079500100622            endif ;
079600100622         ENDIF ;
079700100518
079800100518         // appuntamenti effettuati nel periodo suddivisi per tipo trattativa
079900100518         // trattativa  maggiore di zero e attività 91 eseguita nel periodo
080000100608         If Atctat = 'A' and atccad = '91' and atcnrv > *zeros and
080100100608               atcdco >= i88deci and  atcdco <= i88decf and  atcest = 'S' and
080200100518            // verifico se effettuata da un'agente dell'unificante
080300100518               %lookup(atccco:$age) > 0 ;
080400100518
080500100518            // Verifico se la trattativa è mantenimento o incremento o incasso
080600100623                  $scrivic  = *on ;
080700100518                     If VisTpv = 'M' ;
080800100518                        Rcaaptm += 1 ;
080900100518                     endif ;
081000100518                     If VisTpv = 'A' ;
081100100518                        Rcaapta += 1 ;
081200100518                     endif ;
081300100518                     If VisTpv = 'N' ;
081400100518                        Rcaaptn += 1 ;
081500100518                     endif ;
081600100518                     If VisTpv = 'C' ;
081700100518                        Rcaaptc += 1 ;
081800100518                     endif ;
081900110118                     If VisTpv = 'I' ;
082000110118                        Rcaapti += 1 ;
082100110118                     endif ;
082200110118                     If VisTpv = 'R' ;
082300110118                        Rcaaptr += 1 ;
082400110118                     endif ;
082500100608            leavesr ;
082600100608         ENDIF ;
082700100518
082800100518         // affiancamenti effettuati nel periodo suddivisi per tipo trattativa
082900100519         // trattativa  maggiore di zero e attività 92 eseguita nel periodo
083000100608         If Atctat = 'A' and atccad = '92' and atcnrv > *zeros and
083100100608               atcdco >= i88deci and  atcdco <= i88decf and  atcest = 'S' and
083200100518            // verifico se effettuata da un'agente dell'unificante
083300100518               %lookup(atccco:$age) > 0 ;
083400100518
083500100518            // Verifico se la trattativa è mantenimento o incremento o incasso
083600100623                  $scrivic  = *on ;
083700100518                     If VisTpv = 'M' ;
083800100519                        Rcaaftm += 1 ;
083900100518                     endif ;
084000100518                     If VisTpv = 'A' ;
084100100519                        Rcaafta += 1 ;
084200100518                     endif ;
084300100518                     If VisTpv = 'N' ;
084400100519                        Rcaaftn += 1 ;
084500100518                     endif ;
084600100518                     If VisTpv = 'C' ;
084700100519                        Rcaaftc += 1 ;
084800100518                     endif ;
084900110118                     If VisTpv = 'I' ;
085000110118                        Rcaafti += 1 ;
085100110118                     endif ;
085200110118                     If VisTpv = 'R' ;
085300110118                        Rcaaftr += 1 ;
085400110118                     endif ;
085500100518            endif ;
085600100518           // conto appuntamenti disdetti con la causale 24
085700100519            If atccad = '91' and  atcdco >= i88deci and
085800100519               atcdco <= i88decf and
085900100518            // verifico se effettuata da un'agente dell'unificante
086000100601               %lookup(atcco3:$age) > 0 and
086100100518            // verifico se chiusa con causale 24
086200100518               atccac = '24' ;
086300100518                  Rcaap24 += 1 ;
086400100518            Endif ;
086500100518
086600100518          // calcolo le attività ufficio
086700100518
086800100518          // ferie parziali in ore e minuti
086900100519            If atctat = 'F' and atccad = 'FP' and atcdad >= i88deci and
087000100519               atcdad <= i88decf and
087100100518            // verifico se da effettuare  da un'agente dell'unificante
087200100518               %lookup(atccmm:$age) > 0  ;
087300100518            // trasformo in minuti ed al momento della scrittura imposto
087400100524            // il campo totale in ore e minuti
087500100519               W_iso_oii = %time(watcoii);
087600100519               W_iso_oif = %time(Watcofi);
087700100518               wTot_M = %diff(w_ISO_oif : w_ISO_oiI : *minutes);
087800100518               WTot_M_FP   +=Wtot_m ;
087900100623                  $scrivic  = *on ;
088000100518            Endif ;
088100100518
088200100518          // ferie totali in giorni
088300100519            If atctat = 'F' and atccad = 'FT' and atcdad >= i88deci and
088400100519               atcdad <= i88decf and
088500100518            // verifico se da effettuare  da un'agente dell'unificante
088600100518               %lookup(atccmm:$age) > 0  ;
088700100518                  Rcaferiegg += 1 ;
088800100623                  $scrivic  = *on ;
088900100518            Endif ;
089000100518
089100100518          // Riunione d'area
089200100519            If atctat = 'U' and atccad = 'RA' and atcdad >= i88deci and
089300100519               atcdad <= i88decf and
089400100518            // verifico se da effettuare  da un'agente dell'unificante
089500100518               %lookup(atccmm:$age) > 0  ;
089600100518            // trasformo in minuti ed al momento della scrittura imposto
089700100518            // il campo totale inore e minuti
089800100519               W_iso_oii = %time(watcoii);
089900100519               W_iso_oif = %time(Watcofi);
090000100518               wTot_M = %diff(w_ISO_oif : w_ISO_oiI : *minutes);
090100100518               WTot_M_RA   +=Wtot_m ;
090200100623               $scrivic  = *on ;
090300100518            Endif ;
090400100518
090500100518          // Riunione commerciale
090600100519            If atctat = 'U' and atccad = 'RC' and atcdad >= i88deci and
090700100519               atcdad <= i88decf and
090800100518            // verifico se da effettuare  da un'agente dell'unificante
090900100518               %lookup(atccmm:$age) > 0  ;
091000100518            // trasformo in minuti ed al momento della scrittura imposto
091100100518            // il campo totale inore e minuti
091200100519               W_iso_oii = %time(watcoii);
091300100519               W_iso_oif = %time(Watcofi);
091400100518               wTot_M = %diff(w_ISO_oif : w_ISO_oiI : *minutes);
091500100518               WTot_M_RC   +=Wtot_m ;
091600100623               $scrivic  = *on ;
091700100518            Endif ;
091800100518
091900100518          // Attività ufficio
092000100519            If atctat = 'U' and atccad = 'UF' and atcdad >= i88deci and
092100100519               atcdad <= i88decf and
092200100518            // verifico se da effettuare  da un'agente dell'unificante
092300100518               %lookup(atccmm:$age) > 0  ;
092400100518            // trasformo in minuti ed al momento della scrittura imposto
092500100518            // il campo totale inore e minuti
092600100519               W_iso_oii = %time(watcoii);
092700100519               W_iso_oif = %time(Watcofi);
092800100518               wTot_M = %diff(w_ISO_oif : w_ISO_oiI : *minutes);
092900100518               WTot_M_UF   +=Wtot_m ;
093000100623               $scrivic  = *on ;
093100100518            Endif ;
093200100518
093300100518          // calcolo le attività INEVASE
093400100518
093500100518          // Attività da eseguire nel periodo e mai eseguite oppure eseguite in ritardo
093600100621            If atctat <>'F' and atctat <>'U' and
093700100519               atcdad <= i88decf and
093800100518            // verifico se da effettuare  da un'agente dell'unificante
093900100518               %lookup(atccmm:$age) > 0  and
094000100518            // data esecuzione a zero oppure maggiore del limite massimo consuntivo
094100100519               (atcdco = 0 or atcdco > i88decf) ;
094200100518            // cerco di caricare le causali nella schiera
094300110421               xxi =  %lookup(atccad: $Caai);
094400110421               If xxi = 0 ;
094500110421                  wwi +=1 ;
094600110421                  $Caai(wwi) = Atccad ;
094700110421                  xxi = wwi ;
094800100519               endif ;
094900110421               If xxi > 0 ;
095000110421                  $Nrai(xxi) +=1 ;
095100100623                  $scrivic  = *on ;
095200100519               Endif ;
095300100519            Endif ;
095400100518
095500100519       Endif ;
095600100519
095700100519         // imposto i vari valori in base all'attività che leggo  PREVENTIVO
095800100519
095900100519       If i88tcpe = 'P' or i88tcpe = 'E' ;
096000100519
096100110121          If atcdco = 0 ;
096200100519         // appuntamenti da effettuare nel periodo suddivisi per tipo trattativa
096300100519         // trattativa  maggiore di zero e attività 91 da effettuare nel periodo
096400100519            If atccad = '91' and atcnrv > *zeros and atcdad >= i88depi and
096500100528               atcdad <= i88depf and atcest <> 'N' and atccac <> '24' and
096600100519            // verifico da effettuare da un'agente dell'unificante
096700100519               %lookup(atccmm:$age) > 0 ;
096800100519
096900100519            // Verifico se la trattativa è mantenimento o incremento o incasso
097000100623                  $scrivip  = *on ;
097100100519                     If VisTpv = 'M' ;
097200100519                        Rpaappm += 1 ;
097300100519                     endif ;
097400100519                     If VisTpv = 'A' ;
097500100519                        Rpaappa += 1 ;
097600100519                     endif ;
097700100519                     If VisTpv = 'N' ;
097800100519                        Rpaappn += 1 ;
097900100519                     endif ;
098000100519                     If VisTpv = 'C' ;
098100100519                        Rpaappc += 1 ;
098200100519                     endif ;
098300110118                     If VisTpv = 'I' ;
098400110118                        Rpaappi += 1 ;
098500110118                     endif ;
098600110118                     If VisTpv = 'R' ;
098700110118                        Rpaappr += 1 ;
098800110118                     endif ;
098900100519            endif ;
099000100519
099100100519         // affiancamenti da effettuare nel periodo suddivisi per tipo trattativa
099200100519         // trattativa  maggiore di zero e attività 92 da eseguire nel periodo
099300100519            If atccad = '92' and atcnrv > *zeros and atcdad >= i88depi and
099400100528               atcdad <= i88depf and atccac <> 'NO' and atcest <> 'N' and
099500100519            // verifico da effettuare da un'agente dell'unificante
099600100519               %lookup(atccmm:$age) > 0 ;
099700100519
099800100519            // Verifico se la trattativa è mantenimento o incremento o incasso
099900100623                  $scrivip  = *on ;
100000100519                     If VisTpv = 'M' ;
100100100519                        Rpaaffm += 1 ;
100200100519                     endif ;
100300100519                     If VisTpv = 'A' ;
100400100519                        Rpaaffa += 1 ;
100500100519                     endif ;
100600100519                     If VisTpv = 'N' ;
100700100519                        Rpaaffn += 1 ;
100800100519                     endif ;
100900100519                     If VisTpv = 'C' ;
101000100519                        Rpaaffc += 1 ;
101100100519                     endif ;
101200110118                     If VisTpv = 'I' ;
101300110118                        Rpaaffi += 1 ;
101400110118                     endif ;
101500110118                     If VisTpv = 'R' ;
101600110118                        Rpaaffr += 1 ;
101700110118                     endif ;
101800100519            endif ;
101900100519
102000100519         // telefonate da effettuare nel periodo suddivisi per causale
102100100519            If atctat = 'T' and atcdad >= i88depi and
102200100519               atcdad <= i88depf and
102300100519            // verifico da effettuare da un'agente dell'unificante
102400100519               %lookup(atccmm:$age) > 0 ;
102500100519
102600100519            // Suddivido per causale
102700100519
102800100519            // causale  19
102900100519                  If atccad = '19'    ;
103000100519                     Rpatc19 += 1 ;
103100100623                     $scrivip  = *on ;
103200100519                  endif ;
103300100519            // causale  20
103400100519                  If atccad = '20'    ;
103500100519                     Rpatc20 += 1 ;
103600100623                     $scrivip  = *on ;
103700100519                  endif ;
103800100519            // causale  21
103900100519                  If atccad = '21'    ;
104000100519                     Rpatc21 += 1 ;
104100100623                     $scrivip  = *on ;
104200100519                  endif ;
104300100519            // causale  22
104400100519                  If atccad = '22'    ;
104500100519                     Rpatc22 += 1 ;
104600100623                     $scrivip  = *on ;
104700100519                  endif ;
104800100519            // causale  24
104900100519                  If atccad = '24'    ;
105000100519                     Rpatc24 += 1 ;
105100100623                     $scrivip  = *on ;
105200100519                  endif ;
105300100519            // causale  25
105400100519                  If atccad = '25'    ;
105500100519                     Rpatc25 += 1 ;
105600100623                     $scrivip  = *on ;
105700100519                  endif ;
105800100519            // causale  26
105900100519                  If atccad = '26'    ;
106000100519                     Rpatc26 += 1 ;
106100100623                     $scrivip  = *on ;
106200100519                  endif ;
106300100519            // causale  27
106400100519                  If atccad = '27'    ;
106500100519                     Rpatc27 += 1 ;
106600100623                     $scrivip  = *on ;
106700100519                  endif ;
106800100519            // causale  28
106900100519                  If atccad = '28'    ;
107000100519                     Rpatc28 += 1 ;
107100100623                     $scrivip  = *on ;
107200100519                  endif ;
107300100519            // causale  30
107400100519                  If atccad = '30'    ;
107500100519                     Rpatc30 += 1 ;
107600100623                     $scrivip  = *on ;
107700100519                  endif ;
107800100519            // causale  31
107900100519                  If atccad = '31'    ;
108000100519                     Rpatc31 += 1 ;
108100100623                     $scrivip  = *on ;
108200100519                  endif ;
108300100519            // causale  32
108400100519                  If atccad = '32'    ;
108500100519                     Rpatc32 += 1 ;
108600100623                     $scrivip  = *on ;
108700100519                  endif ;
108800100519            // causale  33
108900100519                  If atccad = '33'    ;
109000100519                     Rpatc33 += 1 ;
109100100623                     $scrivip  = *on ;
109200100519                  endif ;
109300110117            // causale  44
109400110117                  If atccad = '44'    ;
109500110117                     Rpatc44 += 1 ;
109600110117                     $scrivip  = *on ;
109700110117                  endif ;
109800100519            // causale  60
109900100519                  If atccad = '60'    ;
110000100519                     Rpatc60 += 1 ;
110100100623                     $scrivip  = *on ;
110200100519                  endif ;
110300110421            // causale  70
110400110421                  If atccad = '70'    ;
110500110421                     Rpatc70 += 1 ;
110600110421                     $scrivip  = *on ;
110700110421                  endif ;
110800110421            // causale  71
110900110421                  If atccad = '71'    ;
111000110421                     Rpatc71 += 1 ;
111100110421                     $scrivip  = *on ;
111200110421                  endif ;
111300110421            // causale  73
111400110421                  If atccad = '73'    ;
111500110421                     Rpatc73 += 1 ;
111600110421                     $scrivip  = *on ;
111700110421                  endif ;
111800110421            // causale  74
111900110421                  If atccad = '74'    ;
112000110421                     Rpatc74 += 1 ;
112100110421                     $scrivip  = *on ;
112200110421                  endif ;
112300110421            // causale  75
112400110421                  If atccad = '75'    ;
112500110421                     Rpatc75 += 1 ;
112600110421                     $scrivip  = *on ;
112700110421                  endif ;
112800100519
112900100519            endif ;
113000100518
113100100519         // OFFERTE    da effettuare nel periodo suddivisi per causale
113200100519            If atctat = 'O' and atcdad >= i88depi and
113300100519               atcdad <= i88depf and
113400100519            // verifico da effettuare da un'agente dell'unificante
113500100519               %lookup(atccmm:$age) > 0 ;
113600100519
113700100519                     Rpaoff  += 1 ;
113800100623                     $scrivip  = *on ;
113900100519            endif ;
114000100519
114100100519          // ferie parziali e totali in ore e minuti
114200100519            If atctat = 'F' and atcdad >= i88depi and
114300100519               atcdad <= i88depf and
114400100519            // verifico se da effettuare  da un'agente dell'unificante
114500100519               %lookup(atccmm:$age) > 0  ;
114600100519            // trasformo in minuti ed al momento della scrittura imposto
114700100519            // il campo totale inore e minuti
114800100519            // se ferie totali aggiungo 8 h * 60 m
114900100623               $scrivip  = *on ;
115000100519               If atccad = 'FT';
115100100526                  RPAFEGG += 1 ;
115200100519               else ;
115300100519                  W_iso_oii = %time(watcoii);
115400100519                  W_iso_oif = %time(Watcofi);
115500100519                  wTot_M = %diff(w_ISO_oif : w_ISO_oiI : *minutes);
115600100519                  WTot_M_FEP  +=Wtot_m ;
115700100519               endif;
115800100519            Endif ;
115900100519
116000100519          // Attività ufficio
116100100519            If atctat = 'U'  and atcdad >= i88depi and
116200100519               atcdad <= i88depf and
116300100519            // verifico se da effettuare  da un'agente dell'unificante
116400100519               %lookup(atccmm:$age) > 0  ;
116500100519            // trasformo in minuti ed al momento della scrittura imposto
116600100519            // il campo totale inore e minuti
116700100519               W_iso_oii = %time(watcoii);
116800100519               W_iso_oif = %time(Watcofi);
116900100519               wTot_M = %diff(w_ISO_oif : w_ISO_oiI : *minutes);
117000100519               WTot_M_UFP  +=Wtot_m ;
117100100623               $scrivip  = *on ;
117200100519            Endif ;
117300100519
117400110121          Endif ;
117500100519         Endif ;
117600100519
117700100326       ENDSR;
117800100519
117900100519       //--------------------------------------------------------------
118000100519       //?Scrittura record statitistica Consuntiva
118100100519       //--------------------------------------------------------------
118200100519       BEGSR SR_writeRCA ;
118300100519
118400100519         // decodifico agente
118500100519         exsr sr_deco_age ;
118600100519
118700100519         rcargf = $ageu(zz) ;
118800130917         rcadrg = CMMdes ;
118900130917         rcafun = CMMfun ;
119000100519
119100100519         rcafil = %dec(%subst(%editc(rcargf: 'X'):1:3):3:0) ;
119200100519
119300100519         // Distretto e area unificante
119400100519         chain rcafil azorg01l ;
119500100519
119600100519         exsr SR_deco_dst ;
119700100519         exsr SR_deco_are ;
119800100519
119900100519         rcafid = orgdes ;
120000100519         rcadiv = orgfl3 ;
120100100519         rcadid = §17des ;
120200100519         rcaare = orgcar ;
120300100519         rcaard = §05des ;
120400100519
120500100519         // imposto i periodi
120600100519         rcapda = i88deci ;
120700100519         rcapal = i88decf ;
120800100519         // trasformo minuti in ore
120900100519         // ferie
121000100519             wTot_H = %div(wTot_M_FP : 60);
121100100519             wTot_R = %rem(wTot_M_FP : 60);
121200100519             Rcaferiehh = wtot_h;
121300100519             Rcaferiehh += (Wtot_r / 100);
121400100519         // riunioni d'area
121500100519             wTot_H = %div(wTot_M_RA : 60);
121600100519             wTot_R = %rem(wTot_M_RA : 60);
121700100519             Rcaufra    = wtot_h;
121800100519             Rcaufra    += (Wtot_r / 100);
121900100519         // riunioni commerciale
122000100519             wTot_H = %div(wTot_M_RC : 60);
122100100519             wTot_R = %rem(wTot_M_RC : 60);
122200100519             Rcaufrc    = wtot_h;
122300100519             Rcaufrc    += (Wtot_r / 100);
122400100519         // attività ufficio
122500100519             wTot_H = %div(wTot_M_UF : 60);
122600100519             wTot_R = %rem(wTot_M_UF : 60);
122700100519             Rcaufuf    = wtot_h;
122800100519             Rcaufuf    += (Wtot_r / 100);
122900100522         // attività inevase
123000100522             Rcacaai = Rcacaaids ;
123100100522             Rcanrai = Rcanraids ;
123200110427             Rcainevase = %xfoot($nrai);
123300100519
123400100604         // telefonate mai trattate
123500110421             Rcacaapm = Rcacaapmds ;
123600110421             Rcanrapm = Rcanrapmds ;
123700100604
123800110421         // telefonate effettuate su potenziali fuori trattativa già trattati
123900110421             Rcacaapg = Rcacaapgds ;
124000110421             Rcanrapg = Rcanrapgds ;
124100110427             Rcatepg = %xfoot($nrapg);
124200110421
124300110421         // telefonate effettuate su potenziali PERSI fuori trattativa
124400110421             Rcacaapp = Rcacaappds ;
124500110421             Rcanrapp = Rcanrappds ;
124600110421             Rcatepp = %xfoot($nrapp);
124700100610
124800100610         // telefonate effettuate su potenziali dentro trattativa
124900110421             Rcacaapt = Rcacaaptds ;
125000110421             Rcanrapt = Rcanraptds ;
125100110421             Rcatept  = %xfoot($nrapt);
125200110421
125300110421         // telefonate effettuate su potenziali PERSI dentro trattativa
125400110421             Rcacaappt = Rcacaapptds ;
125500110421             Rcanrappt = Rcanrapptds ;
125600110428             Rcateppt = %xfoot($nrappt);
125700100610
125800100610         // telefonate effettuate su clienti fuori trattativa
125900100610             Rcacaacf = Rcacaacfds ;
126000100610             Rcanracf = Rcanracfds ;
126100110421             Rcatecft = %xfoot($nracf);
126200100610
126300100610         // telefonate effettuate su clienti dentro trattativa
126400100610             Rcacaact = Rcacaactds ;
126500100610             Rcanract = Rcanractds ;
126600110421             Rcatect  = %xfoot($nract);
126700110421
126800110421         // telefonate post offerta
126900110421             Rcatepost = rcatepo30m + rcatepo30a + rcatepo33m + rcatepo33a ;
127000110421
127100110421         // telefonate caldi
127200110421             Rcatecaldi = rcatepc31m + rcatepc31a + rcatepc44m + rcatepc44a ;
127300110421
127400110421         // telefonate post vendita
127500110421             Rcateposve = rcatepv32m + rcatepv32a  ;
127600110421
127700110421         // telefonate mantenimento telefonico
127800110421             Rcatemante = rcatepc40m + rcatepc40a  ;
127900110421
128000110421         // telefonate incremento/aumento telefonico
128100110518         //  Rcateincre = rcatepc50m + rcatepc50a  ;
128200110421
128300110421         // TOTALE telefonate eseguite
128400110421             Rcatelese = rcatepm + rcatepg + rcatepp + rcatecft + rcatept +
128500110421                         rcatect + rcateppt + rcatepost + rcatecaldi +
128600110421                         rcateposve + rcatemante + rcateincre ;
128700100610
128800100519             rcadat = Woggi ;
128900100519             rcapru = knmus ;
129000100519         write wfrca000 ;
129100100519
129200100519       ENDSR;
129300100519
129400100519       //--------------------------------------------------------------
129500100519       //?Scrittura record statitistica Preventiva
129600100519       //--------------------------------------------------------------
129700100519       BEGSR SR_writeRPA ;
129800100519
129900100519         // decodifico agente
130000100519         exsr sr_deco_age ;
130100100519
130200100519         rpargf = $ageu(zz) ;
130300130917         rpadrg = CMMdes ;
130400130917         rpafun = CMMfun ;
130500100519
130600100519         rpafil = %dec(%subst(%editc(rpargf: 'X'):1:3):3:0) ;
130700100519
130800100519         // Distretto e area unificante
130900100519         chain rpafil azorg01l ;
131000100519
131100100519         exsr SR_deco_dst ;
131200100519         exsr SR_deco_are ;
131300100519
131400100519         rpafid = orgdes ;
131500100519         rpadiv = orgfl3 ;
131600100519         rpadid = §17des ;
131700100519         rpaare = orgcar ;
131800100519         rpaard = §05des ;
131900100519
132000100519         // imposto i periodi
132100100519         rpapda = i88depi ;
132200100519         rpapal = i88depf ;
132300100519         // trasformo minuti in ore
132400100519         // ferie
132500100519             wTot_H = %div(wTot_M_FEP : 60);
132600100519             wTot_R = %rem(wTot_M_FEP : 60);
132700100519             Rpaferie = wtot_h;
132800100519             Rpaferie += (Wtot_r / 100);
132900100519         // attività ufficio
133000100519             wTot_H = %div(wTot_M_UFP : 60);
133100100519             wTot_R = %rem(wTot_M_UFP : 60);
133200100519             Rpauff     = wtot_h;
133300100519             Rpauff     += (Wtot_r / 100);
133400100519
133500100519
133600100519             rpadat = Woggi ;
133700100519             rpapru = knmus ;
133800100519
133900100519         write wfrpa000 ;
134000100519
134100100519       ENDSR;
134200100519
134300100519       //--------------------------------------------------------------
134400100519       //?Decodifico i dati relativi all'agente unificante
134500100519       //--------------------------------------------------------------
134600100519       BEGSR Sr_deco_age ;
134700100519
134800130917         // reperimento dei dati dell'unificante
134900130917         chain  ($AgeU(zz))  AZCMM000;
135000130917         if  NOT  %found(AZCMM01L);
135100130917           clear  CMMdes;
135200130917           clear  CMMfun;
135300130917         endif;
135400130917
135500100519       Endsr ;
135600100519       //--------------------------------------------------------------
135700100519       //?Decodifico distretto
135800100519       //--------------------------------------------------------------
135900100519       BEGSR Sr_deco_dst ;
136000100519
136100100519        // reperimento dei dati distretto
136200100519                  clear  ds17;
136300100520                  TBLkut = 1;
136400100519                  TBLcod = '17';
136500100519                  TBLkey = orgfl3 ;
136600100519                  chain (TBLkut : TBLcod : TBLkey) TABEL;
136700100519                  if   %found(TABEL00F);
136800100519                       ds17    = TBLuni;
136900100519                  endif;
137000100519       Endsr ;
137100100519
137200100519       //--------------------------------------------------------------
137300100519       //?Decodifico area
137400100519       //--------------------------------------------------------------
137500100519       BEGSR Sr_deco_are ;
137600100519
137700100519        // reperimento dei dati area
137800100519                  clear  ds05;
137900100520                  TBLkut = 1;
138000100519                  TBLcod = '05';
138100100519                  TBLkey = %editc(orgcar : 'X') ;
138200100519                  chain (TBLkut : TBLcod : TBLkey) TABEL;
138300100519                  if   %found(TABEL00F);
138400100519                       ds05    = TBLuni;
138500100519                  endif;
138600100519       Endsr ;
138700100519
138800080206       //--------------------------------------------------------------
138900080206       //?Reperimento Dati del job (Utente/Operativi).
139000080206       //--------------------------------------------------------------
139100080206       BEGSR DatiJob;
139200080206
139300080206         in(E) §AzUte;
139400080206         if NOT %error;
139500080206           in(E) §DatiUte;
139600080206         endif;
139700080206         if %error or RSut = *blanks;
139800080206           clear TIBS34ds;
139900080206           tibs34r(tibs34ds);
140000080206           in §AzUte;
140100080206           in §DatiUte;
140200080206         endif;
140300100325
140400100325         //?Richiamo pgm di controllo autorizzazioni utente così mi torna
140500100325         //?il codice autorizzazione relativo alle tariffe clienti
140600100325         //?Non controllo se abilitato o meno, lo ha già fatto il chiamante
140700100325         reset TNTAA1DS;
140800100325         ITAA1caut = '§utegtc';
140900100325         kpjbu     = TNTAA1DS;
141000100325         tntaa1c (kpjba);
141100100325         TNTAA1DS = kpjbu;
141200100325
141300100325         //?Se utente abilitato come 'AZ' mi carico la sk delle filiali di area
141400100325         IF  OTAA1cabi = 'AZ';
141500100325           OTAA1cabi = 'RA';
141600100325         ENDIF;
141700100325
141800100325         //?Carico filiali abilitate all'utente
141900100325         //?Non controllo se caricate o no le filiali
142000100325         //?tanto se non ho caricato nessuna filiale e la richiesta
142100100325         //?è per tutti i commerciali, non stampo niente
142200110120         //?Se richiesta un'area carico SOLO le filiali di quell'area
142300110120         IF  I88car > 0;
142400110120             clear TRUL31DS;
142500110120             clear TRUL31DS2;
142600110120             I31abi = 'AO';
142700110120             I31car = I88car ;
142800110120             trul31r (kpjba : trul31ds : trul31ds2);
142900110120         Else ;
143000110120             clear TRUL31DS;
143100110120             clear TRUL31DS2;
143200110328             if Dutpou = 046 ;
143300110328                I31abi = 'AZ'     ;
143400110328             else ;
143500110328                I31abi = OTAA1cabi;
143600110328                I31cdi = DUTdis;
143700110328                I31car = DUTare;
143800110328                I31cpo = DUTpou;
143900110328             Endif;
144000110120             trul31r (kpjba : trul31ds : trul31ds2);
144100110120         Endif;
144200080206
144300080206       ENDSR;
144400100325
144500100325       //--------------------------------------------------------------
144600100325       //?Carico sk agenti unificanti.
144700100325       //--------------------------------------------------------------
144800100326       BEGSR sr_CarAgeUni;
144900100325
145000100325         //?Carico gli agenti unificanti delle filiali gestite dall'utente
145100110120         //?o dell'area se richiesti
145200100326         clear xx;
145300100326         clear $AGEU;
145400100326         $EndAge = *off;
145500100325
145600100325         //?Imposto la stringa per SQL
145700130917         wSQL = 'select CMMcod +
145800130917                 from AZCMM00F +
145900130917                 where substr(digits(CMMuni), 1, 3) in (';
146000100325
146100100325         yy = 0;
146200100325         xx = 1;
146300100325         FOR xx by 1 to %elem(pog);
146400100325           IF pog(xx) <> *zeros and pog(xx) <> *blanks;
146500100325             IF yy > 0;
146600100325               wSQL += ', ';
146700100325             ELSE;
146800100325               yy = 1;
146900100325             ENDIF;
147000100325             wSQL += '''';
147100100325             wSQL += pog(xx);
147200100325             wSQL += '''';
147300100325           ENDIF;
147400100325         ENDFOR;
147500100325
147600130917         wSQL += ') and CMMcod = CMMuni +
147700130917                    and CMMpar = '' ''';
147800100325
147900130917         wSQL += ' order by CMMcod +
148000100325                   for fetch only';
148100100325
148200100325         //?Dichiarazione cursore
148300100325         exec sql
148400100325           prepare A1   from :wSQL;
148500100325         exec sql
148600100325           declare AGEU cursor for A1;
148700100325
148800100325         //?Apertura del cursore
148900100325         exec sql
149000100325           open AGEU;
149100100325
149200100325         IF sqlcode < 0;
149300100325           $EndAge = *on;
149400100325         ENDIF;
149500100325
149600100325         yy = 0;
149700100325         DOW not $EndAge;
149800100325           exec sql
149900130917             fetch next from AGEU into :CMMcod;
150000100325           IF sqlcod = 100 or sqlcod < 0;
150100100325             $EndAge = *on;
150200100325             leave;
150300100325           ENDIF;
150400100325
150500100325         yy += 1;
150600130917         $AGEU(yy) = CMMcod;
150700100325
150800100325         ENDDO;
150900100325
151000100325         exec sql
151100100325           close AGEU;
151200100325
151300100325       ENDSR;
151400100305
151500100305       //--------------------------------------------------------------
151600100305       //?Preparazione stringa SQL.
151700100305       //--------------------------------------------------------------
151800100305       BEGSR sr_PrepSQL;
151900100305
152000100517       // se desidero la sola statistica consuntiva o entrambe
152100100519       If i88tcpe = 'C' or i88tcpe = 'E' ;
152200100423         wSQL = 'select * from TIATC00F +
152300100423                 where ( atccco in(';
152400100423       // agente che ha eseguito l'attivita
152500100325         yy = 0;
152600100325         xx = 1;
152700100325         FOR xx by 1 to %elem($Age);
152800100325           IF $Age(xx) <> *zeros;
152900100325             IF yy > 0;
153000100325               wSQL += ', ';
153100100325             ELSE;
153200100325               yy = 1;
153300100325             ENDIF;
153400100325             wSQL += %editc($Age(xx):'X');
153500100325           ENDIF;
153600100325         ENDFOR;
153700100423
153800100423         // nel limite di date impostate
153900100519           wSQL += ') and atcdco between ' + %editc(i88deci:'X') +
154000100519                   ' and ' + %editc(i88decf:'X');
154100100329
154200130924           wSQL += ') union select * from TIATC00F where (atcco3 in (';
154300100423       // agente che ha inserito l'attivita
154400100423         yy = 0;
154500100423         xx = 1;
154600100423         FOR xx by 1 to %elem($Age);
154700100423           IF $Age(xx) <> *zeros;
154800100423             IF yy > 0;
154900100423               wSQL += ', ';
155000100423             ELSE;
155100100423               yy = 1;
155200100423             ENDIF;
155300100423             wSQL += %editc($Age(xx):'X');
155400100423           ENDIF;
155500100423         ENDFOR;
155600100423
155700100423         // nel limite di date impostate
155800100629           wSQL += ') and atcdim between ' + %editc(i88deci:'X') +
155900100519                   ' and ' + %editc(i88decf:'X');
156000100423
156100130924           wSQL += ') union select * from TIATC00F where (atccmm in (';
156200100517       // agente che deve eseguire l'attività
156300100423         yy = 0;
156400100423         xx = 1;
156500100423         FOR xx by 1 to %elem($Age);
156600100423           IF $Age(xx) <> *zeros;
156700100423             IF yy > 0;
156800100423               wSQL += ', ';
156900100423             ELSE;
157000100423               yy = 1;
157100100423             ENDIF;
157200100423             wSQL += %editc($Age(xx):'X');
157300100423           ENDIF;
157400100423         ENDFOR;
157500100517         endif ;
157600100423
157700100517       // se desidero la sola statistica preventiva
157800100519       If i88tcpe = 'P' ;
157900100517         wSQL = 'select * from TIATC00F +
158000100517                 where ( atccmm in(';
158100100517       // agente che ha eseguito l'attivita
158200100517         yy = 0;
158300100517         xx = 1;
158400100517         FOR xx by 1 to %elem($Age);
158500100517           IF $Age(xx) <> *zeros;
158600100517             IF yy > 0;
158700100517               wSQL += ', ';
158800100517             ELSE;
158900100517               yy = 1;
159000100517             ENDIF;
159100100517             wSQL += %editc($Age(xx):'X');
159200100517           ENDIF;
159300100517         ENDFOR;
159400100517       Endif ;
159500100517
159600100517         // se desidero sia la statistica preventiva che consuntiva
159700100519         If i88tcpe = 'E' ;
159800100517         // nel limite di date impostate sia consuntive o preventive
159900100623           wSQL += ') and ((atcdad <= '+ %editc(i88decf:'X') +
160000100623                    ') or (atcdad between ' +
160100100519                   %editc(i88depi:'X') + ' and ' + %editc(i88depf:'X');
160200100423
160300100611           wSQL += ')))';
160400100517         endif ;
160500100517
160600100517         // se desidero solo la statistica preventiva
160700100519         If i88tcpe = 'P' ;
160800100517         // nel limite di date impostate come preventive
160900100519           wSQL += ') and (atcdad between ' + %editc(i88depi:'X') +
161000100519                   ' and ' + %editc(i88depf:'X') ;
161100100517
161200100520           wSQL += '))';
161300100517         endif ;
161400100329
161500100517         // se desidero solo la statistica consuntiva
161600100519         If i88tcpe = 'C' ;
161700100517         // nel limite di date impostate consuntive
161800100623           wSQL += ') and (atcdad <= ' + %editc(i88decf:'X') ;
161900100520
162000100520       //  wSQL += ') and (atcdad between ' + %editc(i88deci:'X') +
162100100520       //          ' and ' + %editc(i88decf:'X') + ') r (atcdad betwee n ' +
162200100520       //          %editc(i88deci:'X') + ' and ' + %editc(i88decf:'X');
162300100520         //        ' and ' + %editc(i88decf:'X') + ') or (atcdad between ' +
162400100520         //        %editc(i88deci:'X') + ' and ' + %editc(i88decf:'X');
162500100517
162600100517           wSQL += '))';
162700100517         endif ;
162800100517
162900100520         wSQL += '  for fetch only ' ;
163000100305
163100100305       ENDSR;
163200100305
163300080206       //--------------------------------------------------------------
163400080206       //?Operazioni finali.
163500080206       //--------------------------------------------------------------
163600080206       BEGSR RoutEnd;
163700090521
163800100522         kpjbu = trmk88ds ;
163900080206         *inLR = *on;
164000080206         return;
164100080206
164200080206       ENDSR;
164300080206
164400080206      /end-free
164500100610** causali telefonate   CTE
1646001105311415192021222425262728303132334142446070717273747580819192
