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
001300100329     fTNTBE01L  if   e           k disk
001400100517     fTIVIS05L  if   e           k disk
001500100519     fCNCLP00F  if   e           k disk
001600100519     fAZORG01L  if   e           k disk
001700100423     fWFRCA00F  o    e             disk
001800100519     fWFRPA00F  o    e             disk
001900100310
002000080206      //---------------------------------------------------------------
002100090406      //?Definizione costanti.
002200080206      //---------------------------------------------------------------
002300080207
002400100329      // - Tabella tipo trattativa
002500100329     d C_tab           c                   const('TTR')
002600080206
002700080206      //---------------------------------------------------------------
002800080206      //?Definizione schiere.
002900080206      //---------------------------------------------------------------
003000100325
003100100325      // - Agenti
003200100325     d $Age            s              7  0 dim(3500)
003300100325
003400100325      // - Agenti Unificanti
003500100325     d $AgeU           s              7  0 dim(3500)
003600100329
003700100329      // - Tipo trattative
003800100329     d $Ttr            s              3    dim(10)
003900100519
004000110421     d RCACaaids       ds            70    inz
004100110421     d   $CAAI                        2    inz  dim(35)
004200100519
004300100520     d RCANraids       ds                  inz
004400110421     d   $NRAI                        5s 0 inz  dim(35)
004500100604
004600110421     d RCACaapmds      ds            70    inz
004700110421     d   $CAAPM                       2    inz  dim(35)
004800100604
004900110421     d RCANrapmds      ds                  inz
005000110421     d   $NRAPM                       5s 0 inz  dim(35)
005100100608
005200110421     d RCACaapgds      ds            70    inz
005300110421     d   $CAAPG                       2    inz  dim(35)
005400100608
005500110421     d RCANrapgds      ds                  inz
005600110421     d   $NRAPG                       5s 0 inz  dim(35)
005700100608
005800110421     d RCACaaptds      ds            70    inz
005900110421     d   $CAAPT                       2    inz  dim(35)
006000100608
006100100608     d RCANraptds      ds                  inz
006200110421     d   $NRAPT                       5s 0 inz  dim(35)
006300100608
006400110421     d RCACaacfds      ds            70    inz
006500110421     d   $CAACF                       2    inz  dim(35)
006600100608
006700100608     d RCANracfds      ds                  inz
006800110421     d   $NRACF                       5s 0 inz  dim(35)
006900100608
007000110421     d RCACaactds      ds            70    inz
007100110421     d   $CAACT                       2    inz  dim(35)
007200100608
007300100608     d RCANractds      ds                  inz
007400110421     d   $NRACT                       5s 0 inz  dim(35)
007500110421
007600110421     d RCACaappds      ds            70    inz
007700110421     d   $CAAPP                       2    inz  dim(35)
007800110421
007900110421     d RCANrappds      ds                  inz
008000110421     d   $NRAPP                       5s 0 inz  dim(35)
008100100610
008200110421     d RCACaapptds     ds            70    inz
008300110421     d   $CAAPPT                      2    inz  dim(35)
008400110421
008500110421     d RCANrapptds     ds                  inz
008600110421     d   $NRAPPT                      5s 0 inz  dim(35)
008700110421
008800100610      // causali da stampare
008900110421     d $CTE            s              2    dim(35) ctdata perrcd(35)
009000080206      //---------------------------------------------------------------
009100080206      //?Definizione aree dati.
009200080206      //---------------------------------------------------------------
009300080206
009400080206      // - Dati utente
009500080206     d §AzUte        e ds                  extname(AZUTE00F)
009600080206     d                                     dtaara
009700080206     d §DatiUte      e ds                  extname(dDatiUte)
009800080206     d                                     dtaara
009900100423
010000100423     d tiatcDS       e ds                  ExtName(tiatc00f)
010100080206
010200080206      //---------------------------------------------------------------
010300080206      //?Definizione strutture dati.
010400080206      //---------------------------------------------------------------
010500100310
010600100310     d PrtINFO         ds
010700100310     d  CurLine              367    368i 0
010800080206
010900080206      // - Status
011000080206     d Psds           sds
011100080206     d   SDSpgm          *proc
011200080206
011300080206      // - Parametri ricevuti
011400080206     d KPJBA         e ds
011500100423     d Trmk88DS      e ds
011600080206
011700100308      // - Ricerca/Controllo tabelle
011800100308     d TIBS02ds      e ds                  inz
011900100308
012000080206      // - Reperimento dati utente
012100100305     d TIBS34DS      e ds
012200100308
012300100308       // - Reperimento dati anagrafici
012400100308     d TIBS69ds      e ds
012500100308     d DS_cnaco      e ds                  inz extname(CNACO00F)
012600100308     d DS_cnind      e ds                  inz extname(CNIND00F)
012700100308     d DS_cnclp      e ds                  inz extname(CNCLP00F)
012800100308     d DS_fncls      e ds                  inz extname(FNCLS00F)
012900100326
013000100326      // - controllo abilitazioni
013100100326     d TNTAA1DS      e ds                  inz
013200100325
013300100325      // - Carico Filiali abilitate all'utente
013400100325     d TRUL31DS      e ds
013500100325     d POG                    10    759    DIM(250)
013600110120
013700110120      // - Carico Aree abilitate all'utente
013800110120     d TRUL31DS2     e ds
013900110120     d CAR                    22    171    DIM(50)
014000100308
014100100329      // - Tabella TTR = Tipo trattativa
014200100329     d dTTR          e ds                  inz
014300100325
014400100325      // - Tabella 01 = Codici Commerciali
014500100325     d ds01          e ds                  inz
014600110120
014700110120      // - Tabella 05 = Codici Aree
014800110120     d ds05          e ds                  inz
014900090807
015000100519      // - Tabella 17 = Distretto
015100100519     d ds17          e ds                  inz
015200100519
015300100819      // - Flag operativi tiatc
015400100819     d datc01        e ds                  inz
015500100519
015600090629
015700080206      //---------------------------------------------------------------
015800080206      //?Definizione variabili globali.
015900080206      //---------------------------------------------------------------
016000080206
016100080206      // - Flags booleani
016200100305     d $End            s               n   inz(*off)
016300100325     d $EndAge         s               n   inz(*off)
016400100326     d $EndAtt         s               n   inz(*off)
016500100305     d $RcdOk          s               n   inz(*off)
016600100426     d $Scrivi         s               n   inz(*off)
016700100623     d $Scrivic        s               n   inz(*off)
016800100623     d $Scrivip        s               n   inz(*off)
016900100325
017000100325      // - Indici di schiera
017100100326     d xx              s              4  0 inz
017200100326     d yy              s              4  0 inz
017300110421     d zz              s              4  0 inz
017400110421     d xxi             s              4  0 inz
017500110421     d wwi             s              4  0 inz
017600110421     d xpm             s              4  0 inz
017700110421     d wpm             s              4  0 inz
017800110421     d xpg             s              4  0 inz
017900110421     d wpg             s              4  0 inz
018000100608     d xpt             s              4  0 inz
018100100608     d wpt             s              4  0 inz
018200100608     d xcf             s              4  0 inz
018300100608     d wcf             s              4  0 inz
018400100608     d xct             s              4  0 inz
018500100608     d wct             s              4  0 inz
018600110421     d xpp             s              4  0 inz
018700110421     d wpp             s              4  0 inz
018800110421     d xppt            s              4  0 inz
018900110421     d wppt            s              4  0 inz
019000080206
019100090806       // - Stringa SQL da eseguire
019200111020     d wSQL            s          30000A   Varying        inz
019300080206
019400090401      // - Campi di comodo data
019500100305     d wDate_EUR       s               d   datfmt(*eur)
019600100305     d wDate_ISO       s               d   datfmt(*iso)
019700100305
019800100305      // - Campi di comodo
019900100519     d watcoii         s              6  0 inz
020000100519     d watcofi         s              6  0 inz
020100110421     d w_catepo        s              1    inz
020200100326     d wConc           s              1  0 inz
020300100329     d wConta          s              2  0 inz
020400100329     d wOggi           s              8  0 inz
020500100305     d wTrat           s            196    inz(*all'-')
020600100518     d w_Iso_oiF       s               t   inz
020700100518     d w_Iso_oiI       s               t   inz
020800100518     d wTot_M          s              5  0 inz
020900100519     d wTot_H          s              5  0 inz
021000100519     d wTot_R          s              5  0 inz
021100100519     d WTot_M_FP       s              7  0 inz
021200100518     d WTot_M_RA       s              7  0 inz
021300100518     d WTot_M_RC       s              7  0 inz
021400100518     d WTot_M_UF       s              7  0 inz
021500100519     d WTot_M_FEP      s              7  0 inz
021600100519     d WTot_M_UFP      s              7  0 inz
021700100325
021800090508      //---------------------------------------------------------------
021900090807      //?Definizione procedure esterne.
022000090508      //---------------------------------------------------------------
022100100120
022200100120
022300100120      //---------------------------------------------------------------
022400100120      //?prototipi
022500100120      //---------------------------------------------------------------
022600090807
022700080626      /copy gaitrasrc/srcprotopr,tibs02r
022800080626      /copy gaitrasrc/srcprotopr,tibs34r
022900100308      /copy gaitrasrc/srcprotopr,tibs69r
023000100326      /copy gaitrasrc/srcprotopr,tntaa1c
023100110120
023200110120      // - Filiali e aree abilitate
023300110120     d trul31r         pr                  extpgm('TRUL31R')
023400110120     d  kpjba                              likeds(KPJBA)
023500110120     d  trul31ds                           likeds(trul31ds)
023600110120     d  trul31ds2                          likeds(trul31ds2)
023700090807
023800080206      //---------------------------------------------------------------
023900080206      //?Definizione key-list.
024000080206      //---------------------------------------------------------------
024100100325
024200100325       // - File TABEL00F
024300100325     d k03tabel      e ds                  extname(TABEL00F:*key)
024400100325     d                                     prefix(k_)
024500100325     d                                     inz
024600080206
024700080206      //---------------------------------------------------------------
024800080206      //?Riepilogo indicatori.
024900080206      //---------------------------------------------------------------
025000100326
025100090807
025200080206      //---------------------------------------------------------------
025300080206
025400080206      //---------------------------------------------------------------
025500080206      //?M A I N - L I N E
025600080206      //---------------------------------------------------------------
025700080206
025800080206     c     *Entry        plist
025900080206     c                   parm                    KPJBA
026000080206
026100080206      /free
026200080206
026300090807       //?Operazioni iniziali
026400080206       exsr RoutInz;
026500100325
026600100423       //?Ciclo per sk unificanti
026700100326       zz = 1;
026800100326       FOR zz by 1 to %elem($AgeU);
026900100326         IF  $AgeU(zz) <> *zeros;
027000100326
027100100326         //?Carico sk agenti dell'unificante
027200100326           exsr sr_CarAge;
027300100305
027400100423         //?Leggo le attività per commerciale unificante
027500100423               exsr sr_Attivita ;
027600100329             ENDIF;
027700100329           ENDFOR;
027800100324
027900100325
028000090807       //?Operazioni finali
028100080206       exsr RoutEnd;
028200080206
028300080206       //--------------------------------------------------------------
028400080206       //?Operazioni iniziali.
028500080206       //--------------------------------------------------------------
028600080206       BEGSR RoutInz;
028700100329
028800100329         //?Imposto la ds con i dati della KPJBU
028900100423         Trmk88ds = kpjbu;
029000090807
029100090807         //?Reperimento dati job
029200090807         exsr DatiJob;
029300090807
029400090807         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
029500100305
029600100305         //?Imposto data e ora elaborazione
029700100330         wOggi     = %dec(%date());
029800100330         wDate_ISO = %date(wOggi:*ISO);
029900100330         wDate_EUR = wDate_ISO;
030000100325
030100100325         clear $AgeU;
030200100325
030300100325         //?Carico in sk l'unificante richiesto
030400100519         IF i88cmmu > 0;
030500100519           $AgeU(01) = i88cmmu;
030600100325         ENDIF;
030700100325
030800100325         //?Carico gli unificanti delle filiali gestite dall'utente
030900110120       //?o dell'area se richiesta
031000100519         IF i88cmmu = 0;
031100100325           exsr sr_CarAgeUni;
031200100325           sorta $AgeU;
031300100325         ENDIF;
031400100610
031500080206       ENDSR;
031600100326
031700100326       //--------------------------------------------------------------
031800100326       //?Carico Sk Agenti.
031900100326       //--------------------------------------------------------------
032000100326       BEGSR sr_CarAge;
032100100326
032200100326         clear xx;
032300100326         clear $Age;
032400100326         $EndAge = *off;
032500100326
032600100326         exec sql
032700100326          declare AGE cursor for
032800100326          select tblkey, tbluni
032900100326          from tabel00f where tblkut = '1' and tblcod = '01' and
033000100326          tblflg = ' ' order by tblkut, tblcod, tblkey;
033100100326
033200100326         exec sql
033300100326           open AGE;
033400100326           IF sqlcode < 0;
033500100326             $EndAge = *on;
033600100326           ENDIF;
033700100326
033800100326         DOW not $EndAge;
033900100326           exec sql
034000100326             fetch next from AGE into :tblkey, :tbluni;
034100100326             IF sqlcod = 100 or sqlcod < 0;
034200100326               $EndAge = *on;
034300100326               leave;
034400100326             ENDIF;
034500100326             ds01 = Tbluni;
034600100326             IF  §01rgf = $AgeU(zz);
034700100326               xx += 1;
034800100326               $Age(xx) = %dec(%subst(tblkey:1:7):7:0);
034900100326             ENDIF;
035000100326         ENDDO;
035100100326
035200100326         exec sql close AGE;
035300100326
035400100326         //?Se non ho cricato niente memorizzo almeno il codice richiesto
035500100326         IF $Age(01) = 0;
035600100326           $Age(01) = $AgeU(zz);
035700100326         ENDIF;
035800100326
035900100326       ENDSR;
036000100324
036100100305       //--------------------------------------------------------------
036200100423       //?Leggo le attività per tutti i commerciali legati all'unificante
036300100305       //--------------------------------------------------------------
036400100423       BEGSR sr_Attivita ;
036500100426
036600100623
036700100623         $scrivic  = *off ;
036800100623         $scrivip  = *off ;
036900100517       // pulisco il record  di wfrca / wfrpa
037000100519       If i88tcpe = 'C' or i88tcpe = 'E' ;
037100100517         clear wfrca000 ;
037200100518         Clear WTot_M_Fp ;
037300100518         Clear WTot_M_Ra ;
037400100518         Clear WTot_M_Rc ;
037500100518         Clear WTot_M_Uf ;
037600100519         Clear WTot_M_Fep;
037700100519         Clear WTot_M_Ufp;
037800100520         CLEAR RCACaaids  ;
037900100520         CLEAR RCANraids  ;
038000110421         CLEAR RCACaapmds  ;
038100110421         CLEAR RCANrapmds  ;
038200110421         CLEAR RCACaapgds  ;
038300110421         CLEAR RCANrapgds  ;
038400100610         CLEAR RCACaaptds  ;
038500100610         CLEAR RCANraptds  ;
038600100610         CLEAR RCACaacfds  ;
038700100610         CLEAR RCANracfds  ;
038800100610         CLEAR RCACaactds  ;
038900110421         CLEAR RCANractds  ;
039000110421         CLEAR RCACaappds  ;
039100110421         CLEAR RCANrappds  ;
039200110421         CLEAR RCACaapptds  ;
039300110421         CLEAR RCANrapptds  ;
039400110421         clear wwi;
039500110421         clear wpm;
039600110421         clear wpg;
039700100610         clear wpt;
039800100610         clear wcf;
039900100610         clear wct;
040000110421         clear wpp;
040100110421         clear wppt;
040200110421          $CAAI  = $CTE ;
040300110421          $CAAPM = $CTE ;
040400110421          $CAAPG = $CTE ;
040500100610          $CAAPT = $CTE ;
040600100610          $CAACF = $CTE ;
040700100610          $CAACT = $CTE ;
040800110421          $CAAPP = $CTE ;
040900110421          $CAAPPT= $CTE ;
041000100517       endif ;
041100100519       If i88tcpe = 'P' or i88tcpe = 'E' ;
041200100517         clear wfrpa000 ;
041300101008         Clear WTot_M_Fp ;
041400101008         Clear WTot_M_Ra ;
041500101008         Clear WTot_M_Rc ;
041600101008         Clear WTot_M_Uf ;
041700101008         Clear WTot_M_Fep;
041800101008         Clear WTot_M_Ufp;
041900100517       endif ;
042000100305
042100100305         $End = *off;
042200100305
042300100305         //?Imposto la stringa per SQL
042400100305         exsr  sr_PrepSQL;
042500100305
042600100305         //?Dichiarazione cursore
042700100305         exec sql
042800100305           prepare S1   from :wSQL;
042900100305         exec sql
043000100423           declare c1   cursor for S1;
043100100305
043200100305         //?Apertura del cursore
043300100305         exec sql
043400100423           open c1 ;
043500100305
043600100305         IF sqlcode < 0;
043700100305           $End = *on;
043800100305         ENDIF;
043900100305
044000100305         DOW not $End;
044100100305           exec sql
044200100423             fetch next from C1  into :tiatcds;
044300100305           IF sqlcod = 100 or sqlcod < 0;
044400100305             $End = *on;
044500100305             leave;
044600100305           ENDIF;
044700100305
044800100324         //?Controllo se ok il rcd che sto leggendo
044900100526           exsr sr_CtrRcdOk;
045000100526           IF  not $RcdOk;
045100100526             iter;
045200100526           ENDIF;
045300100526
045400100517           exsr sr_Eseguo   ;
045500100305         ENDDO;
045600100305
045700100305         exec sql
045800100423           close c1 ;
045900100426
046000100519               If i88tcpe = 'C' or i88tcpe = 'E' ;
046100100623         // se contata almeno un'attività da conteggiare  scrivo
046200100623         // un record altrimenti passo all'unificante successivo
046300100623                  If $scrivic = *on ;
046400100623                     exsr SR_writeRCA ;
046500100623                  endif;
046600100517               endif ;
046700100519               If i88tcpe = 'P' or i88tcpe = 'E' ;
046800100623         // se contata almeno un'attività da conteggiare  scrivo
046900100623         // un record altrimenti passo all'unificante successivo
047000100623                  If $scrivip = *on ;
047100100623                     exsr SR_writeRPA ;
047200100623                  endif;
047300100517               endif ;
047400100520         // endif ;
047500100305
047600100305       ENDSR;
047700100330
047800100526       //--------------------------------------------------------------
047900100526       //?Controllo se il rcd che sto leggendo è OK per la statistica
048000100526       //--------------------------------------------------------------
048100100526       BEGSR Sr_CtrRcdOk;
048200100526
048300100526         $RcdOk = *on;
048400100526         //?CHIODO
048500100526         //?Se attvità qualsiasi chiusa con attività 'AA'
048600100528         //?non devo caricare in statistica
048700100526         IF  ATCcac = 'AA';
048800100526           $RcdOk = *off;
048900100526         ENDIF;
049000100526
049100100526       ENDSR;
049200100526
049300100326       //--------------------------------------------------------------
049400100423       // Eseguo i conteggi
049500100326       //--------------------------------------------------------------
049600100423       BEGSR sr_Eseguo     ;
049700100819
049800100819         // recupero i dati dei flag operativi
049900100819           datc01 = atcflo ;
050000110421
050100110421         // recupero dal flag operativi la categioria del potenziale
050200110421         // Per adesso Cuffaro vuole la categoria al momento della creazione dell'attività
050300110421
050400110421           clear W_catepo ;
050500110421           If §atccapo1 = ' ' ;
050600110421              If §atccapo2 <> ' ' ;
050700110421                  W_catepo  = §atccapo2  ;
050800110421              Endif ;
050900110421           Else ;
051000110421              W_catepo  = §atccapo1  ;
051100110421           Endif ;
051200100819
051300100519         // imposto  ore inizio e fine impegno nel formato che voglio
051400100519           watcoii = atcoii * 100;
051500100519           watcofi = atcofi * 100;
051600100819
051700100819         // verifico se attività legata a trattativa aggancio trattativa e se fittizia
051800100819         // o non esiste passo al record successivo
051900100819         If atcnra > 0 ;
052000100819            chain atcnra tivis05l ;
052100100819            If (%found(tivis05l) and visffz = 'S') or not %found(tivis05l);
052200100819               leavesr ;
052300100819            endif ;
052400100819         endif ;
052500100819
052600100819         If atcnrv > 0 ;
052700100819            chain atcnrv tivis05l ;
052800100819            If (%found(tivis05l) and visffz = 'S') or not %found(tivis05l);
052900100819               leavesr ;
053000100819            endif ;
053100100819         endif ;
053200100819
053300100517         // imposto i vari valori in base all'attività che leggo  CONSUNTIVO
053400100517
053500100519       If i88tcpe = 'C' or i88tcpe = 'E' ;
053600100426
053700100605         // Conto le TELEFONATE effettuate da un'agente unificante ed effettuate nel periodo
053800100605         If ATCTAT = 'T' and %lookup(atccco:$age) > 0  and
053900100605            atcdco >= i88deci and atcdco <= i88decf  ;
054000100604
054100100604         // potenziali mai trattati
054200100819         // codice cliente zero e flag mai trattato uguale a "S"
054300110421            If atcksc = *zeros and §ATCmaitr = 'S' and W_catepo = 'M' ;
054400100426               Rcatepm+= 1 ;
054500100623               $scrivic  = *on ;
054600100604            // attribuisco ad ogni causale il numero delle telefonate effettuate
054700100604            // cerco di caricare le causali nella schiera
054800110421               xpm =  %lookup(atccac: $Caapm);
054900110421               If xpm = 0 ;
055000110421                  wpm +=1 ;
055100110421                  $Caapm(wpm) = Atccac ;
055200110421                  xpm = wpm ;
055300100604               endif ;
055400110421               If xpm > 0 ;
055500110421                  $Nrapm(xpm) +=1 ;
055600100623                  $scrivic  = *on ;
055700100604               Endif ;
055800100605
055900100610               If atccac <> '91' ;
056000100610                  leavesr ;
056100100610               Endif ;
056200100426            Endif;
056300100426
056400110117         // caldi
056500110117         // data scadenza attività > zero e
056600110117         // causale 31   o causale 44
056700110117            If atcdad > *zeros and (atccad = '31' or atccad = '44');
056800110117
056900100426
057000100426            // Verifico se la trattativa è mia oppure no
057100110421              If atccad = '31' ;
057200100519               if %lookup(viscmm:$age) > 0 ;
057300100517                  Rcatepc31m += 1 ;
057400100623                  $scrivic  = *on ;
057500100517               else ;
057600100517                  Rcatepc31a += 1 ;
057700100623                  $scrivic  = *on ;
057800100517               endif ;
057900110421              endif ;
058000110421
058100110421              If atccad = '44' ;
058200110421               if %lookup(viscmm:$age) > 0 ;
058300110421                  Rcatepc44m += 1 ;
058400110421                  $scrivic  = *on ;
058500110421               else ;
058600110421                  Rcatepc44a += 1 ;
058700110421                  $scrivic  = *on ;
058800110421               endif ;
058900110421              endif ;
059000100605
059100100610               If atccac <> '91' ;
059200100610                  leavesr ;
059300100610               Endif ;
059400100426            Endif;
059500100517
059600100517         // post Vendita
059700100605         // data scadenza attività > zero , codice cliente > zero e
059800100517         // causale 32
059900100605            If atcdad > *zeros and atcksc <> *zeros and atccad = '32' ;
060000100517
060100100517            // Verifico se cliente mio oppure no
060200100517             clpkut = 1 ;
060300100517             clpkcc = dutkci ;
060400100517             clpksc = atcksc ;
060500100519             chain (clpkut:clpkcc:clpksc) Cnclp00f ;
060600100517               if %lookup(clpage:$age) > 0 ;
060700100517                  Rcatepv32m += 1 ;
060800100623                  $scrivic  = *on ;
060900100517               else ;
061000100517                  Rcatepv32a += 1 ;
061100100623                  $scrivic  = *on ;
061200100517               endif ;
061300100605
061400100610               If atccac <> '91' ;
061500100610                  leavesr ;
061600100610               Endif ;
061700100605
061800100517            Endif;
061900100517
062000100605         // Mantenimenti telefonici clienti
062100100605         // codice cliente > zero e
062200100517         // causale varie mie e di altri positive e negative
062300110929            If  atcnrv = 0 and atccac = '40' ;
062400100517
062500100517            // Verifico se cliente mio oppure no
062600100517             clpkut = 1 ;
062700100517             clpkcc = dutkci ;
062800100517             clpksc = atcksc ;
062900100519             chain (clpkut:clpkcc:clpksc) Cnclp00f ;
063000100517               if %lookup(clpage:$age) > 0 ;
063100100517
063200100517                     Rcatepc40m += 1 ;
063300100623                     $scrivic  = *on ;
063400100605                     leavesr ;
063500100517
063600100517             // di altri
063700100517               Else ;
063800100517
063900100517                     Rcatepc40a += 1 ;
064000100623                     $scrivic  = *on ;
064100100605                     leavesr ;
064200100517
064300100517               endif ;
064400100517            Endif;
064500100517
064600100517         // richiami post offerta
064700100605         // data scadenza attività > zero , trattativa >  zero e
064800100605         // causale 30/33
064900100605            If atcdad > *zeros and atcnrv > *zeros ;
065000100517
065100100517            // Verifico se la trattativa è mia oppure no
065200100519               if %lookup(viscmm:$age) > 0 ;
065300100517
065400100517            // causale 30
065500100521                  If atccad = '30' ;
065600100517                     Rcatepo30m += 1 ;
065700100623                     $scrivic  = *on ;
065800100610                     If atccac <> '91' ;
065900100610                        leavesr ;
066000100610                     Endif ;
066100100517                  endif ;
066200100517            // causale 33
066300100521                  If atccad = '33' ;
066400100517                     Rcatepo33m += 1 ;
066500100623                     $scrivic  = *on ;
066600100610                     If atccac <> '91' ;
066700100610                        leavesr ;
066800100610                     Endif ;
066900100517                  endif ;
067000100517               else ;
067100100517
067200100517            // causale 30
067300100521                  If atccad = '30' ;
067400100517                     Rcatepo30a += 1 ;
067500100623                     $scrivic  = *on ;
067600100610                     If atccac <> '91' ;
067700100610                        leavesr ;
067800100610                     Endif ;
067900100517                  endif ;
068000100517            // causale 33
068100100521                  If atccad = '33' ;
068200100517                     Rcatepo33a += 1 ;
068300100623                     $scrivic  = *on ;
068400100610                     If atccac <> '91' ;
068500100610                        leavesr ;
068600100610                     Endif ;
068700100517                  endif ;
068800100517               endif ;
068900100517            Endif;
069000100605
069100100605          // Tutto quello che rimane fuori come telefonate le conteggio nelle varie schiere
069200100610          // carico le causali
069300100605
069400100623          // Telefonate su potenziale fuori trattattiva già trattati ;
069500110427            If atcnrv=0 and (W_catepo = 'E' or
069600110427                      (§ATCmaitr=' ' and W_catepo='M'));
069700100608            // attribuisco ad ogni causale il numero delle telefonate effettuate
069800100608            // cerco di caricare le causali nella schiera
069900100702
070000110421                  xpg =  %lookup(atccac: $Caapg);
070100110421               If xpg > 0 ;
070200110421                  $Nrapg(xpg) +=1 ;
070300100623                  $scrivic  = *on ;
070400100608               Endif ;
070500100608            endif ;
070600110421
070700110421          // Telefonate su potenziale persi fuori trattattiva ;
070800110421            If atcnrv=0  and  W_catepo='P' ;
070900110421            // attribuisco ad ogni causale il numero delle telefonate effettuate
071000110421            // cerco di caricare le causali nella schiera
071100110421
071200110421                  xpp =  %lookup(atccac: $Caapp);
071300110421               If xpp > 0 ;
071400110421                  $Nrapp(xpp) +=1 ;
071500110421                  $scrivic  = *on ;
071600110421               Endif ;
071700110421            endif ;
071800100605
071900100608          // Telefonate su potenziale dentro trattattiva
072000110421            If atcnrv > 0 and (W_catepo='M'or W_catepo='E') and atccad <> '44'
072100110421               and atccad <> '30' and atccad <> '31' and atccad <> '33' ;
072200100608            // attribuisco ad ogni causale il numero delle telefonate effettuate
072300100608            // cerco di caricare le causali nella schiera
072400100702
072500100702                  xpt =  %lookup(atccac: $Caapt);
072600100608               If xpt > 0 ;
072700100608                  $Nrapt(xpt) +=1 ;
072800100623                  $scrivic  = *on ;
072900100608               Endif ;
073000100608            endif ;
073100100608
073200110421          // Telefonate su potenziale perso dentro trattattiva
073300110421            If atcnrv > 0 and W_catepo='P' and atccad <> '44'
073400110421               and atccad <> '30' and atccad <> '31' and atccad <> '33' ;
073500110421            // attribuisco ad ogni causale il numero delle telefonate effettuate
073600110421            // cerco di caricare le causali nella schiera
073700110421
073800110421                  xppt =  %lookup(atccac: $Caappt);
073900110421               If xppt > 0 ;
074000110929                  $Nrappt(xppt) +=1 ;
074100110421                  $scrivic  = *on ;
074200110421               Endif ;
074300110421            endif ;
074400110421
074500100608          // Telefonate su clienti fuori trattattiva
074600110421            If atcnrv = 0 and atccad <> '32' and W_catepo = 'C' ;
074700100608            // attribuisco ad ogni causale il numero delle telefonate effettuate
074800100608            // cerco di caricare le causali nella schiera
074900100702
075000100702                  xcf =  %lookup(atccac: $Caacf);
075100100608               If xcf > 0 ;
075200100608                  $Nracf(xcf) +=1 ;
075300100623                  $scrivic  = *on ;
075400100608               Endif ;
075500100608            endif ;
075600100608
075700100608          // Telefonate su clienti dentro trattattiva
075800110421            If W_catepo = 'C' and atcnrv > 0  and
075900110117               atccad <> '30' and atccad <> '31' and atccad <> '33' and
076000110117               atccad <> '44' ;
076100100608            // attribuisco ad ogni causale il numero delle telefonate effettuate
076200100608            // cerco di caricare le causali nella schiera
076300100702
076400100702                  xct =  %lookup(atccac: $Caact);
076500100608               If xct > 0 ;
076600100608                  $Nract(xct) +=1 ;
076700100623                  $scrivic  = *on ;
076800100608               Endif ;
076900100608            endif ;
077000100608
077100100608         ENDIF ;
077200100517
077300100622         // Conto gli APPUNTAMENTI presi da un'agente unificante ed effettuate nel periodo
077400100622         If atccac = '91' and %lookup(atccco:$age) > 0  and
077500100622            atcdco >= i88deci and atcdco <= i88decf  ;
077600100622
077700100622         // 1° appuntamento preso
077800100622         // trattativa aperta maggiore di zero
077900100622            If atcnra > *zeros ;
078000100622
078100100622            // Verifico se la trattativa è mantenimento o incremento
078200100623                  $scrivic  = *on ;
078300100622                     If VisTpv = 'N' ;
078400100622                        Rcapatn += 1 ;
078500100622                     endif ;
078600100622                     If VisTpv = 'M' ;
078700100622                        Rcapatm += 1 ;
078800100622                     endif ;
078900100622                     If VisTpv = 'A' ;
079000100622                        Rcapata += 1 ;
079100100622                     endif ;
079200100622                     If VisTpv = 'C' ;
079300100622                        Rcapatc += 1 ;
079400100622                     endif ;
079500110118                     If VisTpv = 'I' ;
079600110118                        Rcapati += 1 ;
079700110118                     endif ;
079800110118                     If VisTpv = 'R' ;
079900110118                        Rcapatr += 1 ;
080000110118                     endif ;
080100100622             endif ;
080200100623         ENDIF ;
080300100622
080400100622         // appuntamenti presi esclusi il primo appuntamento
080500100623         // Conto gli APPUNTAMENTI presi da un'agente unificante ed effettuate nel periodo
080600100623         If atccad = '91' and %lookup(atcco3:$age) > 0  and
080700100629            atcdim >= i88deci and atcdim <= i88decf  ;
080800100623
080900100622         // trattativa  maggiore di zero
081000100622            If atcnrv > *zeros ;
081100100622
081200100622            // Verifico e suddivido per tipo trattativa
081300100623                  $scrivic  = *on ;
081400100622                     If VisTpv = 'N' ;
081500100622                        Rcaapptn += 1 ;
081600100622                     endif ;
081700100622                     If VisTpv = 'M' ;
081800100622                        Rcaapptm += 1 ;
081900100622                     endif ;
082000100622                     If VisTpv = 'A' ;
082100100622                        Rcaappta += 1 ;
082200100622                     endif ;
082300100622                     If VisTpv = 'C' ;
082400100622                        Rcaapptc += 1 ;
082500100622                     endif ;
082600110118                     If VisTpv = 'I' ;
082700110118                        Rcaappti += 1 ;
082800110118                     endif ;
082900110118                     If VisTpv = 'R' ;
083000110118                        Rcaapptr += 1 ;
083100110118                     endif ;
083200100622            endif ;
083300100622         ENDIF ;
083400100518
083500100518         // appuntamenti effettuati nel periodo suddivisi per tipo trattativa
083600100518         // trattativa  maggiore di zero e attività 91 eseguita nel periodo
083700100608         If Atctat = 'A' and atccad = '91' and atcnrv > *zeros and
083800100608               atcdco >= i88deci and  atcdco <= i88decf and  atcest = 'S' and
083900100518            // verifico se effettuata da un'agente dell'unificante
084000100518               %lookup(atccco:$age) > 0 ;
084100100518
084200100518            // Verifico se la trattativa è mantenimento o incremento o incasso
084300100623                  $scrivic  = *on ;
084400100518                     If VisTpv = 'M' ;
084500100518                        Rcaaptm += 1 ;
084600100518                     endif ;
084700100518                     If VisTpv = 'A' ;
084800100518                        Rcaapta += 1 ;
084900100518                     endif ;
085000100518                     If VisTpv = 'N' ;
085100100518                        Rcaaptn += 1 ;
085200100518                     endif ;
085300100518                     If VisTpv = 'C' ;
085400100518                        Rcaaptc += 1 ;
085500100518                     endif ;
085600110118                     If VisTpv = 'I' ;
085700110118                        Rcaapti += 1 ;
085800110118                     endif ;
085900110118                     If VisTpv = 'R' ;
086000110118                        Rcaaptr += 1 ;
086100110118                     endif ;
086200100608            leavesr ;
086300100608         ENDIF ;
086400100518
086500100518         // affiancamenti effettuati nel periodo suddivisi per tipo trattativa
086600100519         // trattativa  maggiore di zero e attività 92 eseguita nel periodo
086700100608         If Atctat = 'A' and atccad = '92' and atcnrv > *zeros and
086800100608               atcdco >= i88deci and  atcdco <= i88decf and  atcest = 'S' and
086900100518            // verifico se effettuata da un'agente dell'unificante
087000100518               %lookup(atccco:$age) > 0 ;
087100100518
087200100518            // Verifico se la trattativa è mantenimento o incremento o incasso
087300100623                  $scrivic  = *on ;
087400100518                     If VisTpv = 'M' ;
087500100519                        Rcaaftm += 1 ;
087600100518                     endif ;
087700100518                     If VisTpv = 'A' ;
087800100519                        Rcaafta += 1 ;
087900100518                     endif ;
088000100518                     If VisTpv = 'N' ;
088100100519                        Rcaaftn += 1 ;
088200100518                     endif ;
088300100518                     If VisTpv = 'C' ;
088400100519                        Rcaaftc += 1 ;
088500100518                     endif ;
088600110118                     If VisTpv = 'I' ;
088700110118                        Rcaafti += 1 ;
088800110118                     endif ;
088900110118                     If VisTpv = 'R' ;
089000110118                        Rcaaftr += 1 ;
089100110118                     endif ;
089200100518            endif ;
089300100518           // conto appuntamenti disdetti con la causale 24
089400100519            If atccad = '91' and  atcdco >= i88deci and
089500100519               atcdco <= i88decf and
089600100518            // verifico se effettuata da un'agente dell'unificante
089700100601               %lookup(atcco3:$age) > 0 and
089800100518            // verifico se chiusa con causale 24
089900100518               atccac = '24' ;
090000100518                  Rcaap24 += 1 ;
090100100518            Endif ;
090200100518
090300100518          // calcolo le attività ufficio
090400100518
090500100518          // ferie parziali in ore e minuti
090600100519            If atctat = 'F' and atccad = 'FP' and atcdad >= i88deci and
090700100519               atcdad <= i88decf and
090800100518            // verifico se da effettuare  da un'agente dell'unificante
090900100518               %lookup(atccmm:$age) > 0  ;
091000100518            // trasformo in minuti ed al momento della scrittura imposto
091100100524            // il campo totale in ore e minuti
091200100519               W_iso_oii = %time(watcoii);
091300100519               W_iso_oif = %time(Watcofi);
091400100518               wTot_M = %diff(w_ISO_oif : w_ISO_oiI : *minutes);
091500100518               WTot_M_FP   +=Wtot_m ;
091600100623                  $scrivic  = *on ;
091700100518            Endif ;
091800100518
091900100518          // ferie totali in giorni
092000100519            If atctat = 'F' and atccad = 'FT' and atcdad >= i88deci and
092100100519               atcdad <= i88decf and
092200100518            // verifico se da effettuare  da un'agente dell'unificante
092300100518               %lookup(atccmm:$age) > 0  ;
092400100518                  Rcaferiegg += 1 ;
092500100623                  $scrivic  = *on ;
092600100518            Endif ;
092700100518
092800100518          // Riunione d'area
092900100519            If atctat = 'U' and atccad = 'RA' and atcdad >= i88deci and
093000100519               atcdad <= i88decf and
093100100518            // verifico se da effettuare  da un'agente dell'unificante
093200100518               %lookup(atccmm:$age) > 0  ;
093300100518            // trasformo in minuti ed al momento della scrittura imposto
093400100518            // il campo totale inore e minuti
093500100519               W_iso_oii = %time(watcoii);
093600100519               W_iso_oif = %time(Watcofi);
093700100518               wTot_M = %diff(w_ISO_oif : w_ISO_oiI : *minutes);
093800100518               WTot_M_RA   +=Wtot_m ;
093900100623               $scrivic  = *on ;
094000100518            Endif ;
094100100518
094200100518          // Riunione commerciale
094300100519            If atctat = 'U' and atccad = 'RC' and atcdad >= i88deci and
094400100519               atcdad <= i88decf and
094500100518            // verifico se da effettuare  da un'agente dell'unificante
094600100518               %lookup(atccmm:$age) > 0  ;
094700100518            // trasformo in minuti ed al momento della scrittura imposto
094800100518            // il campo totale inore e minuti
094900100519               W_iso_oii = %time(watcoii);
095000100519               W_iso_oif = %time(Watcofi);
095100100518               wTot_M = %diff(w_ISO_oif : w_ISO_oiI : *minutes);
095200100518               WTot_M_RC   +=Wtot_m ;
095300100623               $scrivic  = *on ;
095400100518            Endif ;
095500100518
095600100518          // Attività ufficio
095700100519            If atctat = 'U' and atccad = 'UF' and atcdad >= i88deci and
095800100519               atcdad <= i88decf and
095900100518            // verifico se da effettuare  da un'agente dell'unificante
096000100518               %lookup(atccmm:$age) > 0  ;
096100100518            // trasformo in minuti ed al momento della scrittura imposto
096200100518            // il campo totale inore e minuti
096300100519               W_iso_oii = %time(watcoii);
096400100519               W_iso_oif = %time(Watcofi);
096500100518               wTot_M = %diff(w_ISO_oif : w_ISO_oiI : *minutes);
096600100518               WTot_M_UF   +=Wtot_m ;
096700100623               $scrivic  = *on ;
096800100518            Endif ;
096900100518
097000100518          // calcolo le attività INEVASE
097100100518
097200100518          // Attività da eseguire nel periodo e mai eseguite oppure eseguite in ritardo
097300100621            If atctat <>'F' and atctat <>'U' and
097400100519               atcdad <= i88decf and
097500100518            // verifico se da effettuare  da un'agente dell'unificante
097600100518               %lookup(atccmm:$age) > 0  and
097700100518            // data esecuzione a zero oppure maggiore del limite massimo consuntivo
097800100519               (atcdco = 0 or atcdco > i88decf) ;
097900100518            // cerco di caricare le causali nella schiera
098000110421               xxi =  %lookup(atccad: $Caai);
098100110421               If xxi = 0 ;
098200110421                  wwi +=1 ;
098300110421                  $Caai(wwi) = Atccad ;
098400110421                  xxi = wwi ;
098500100519               endif ;
098600110421               If xxi > 0 ;
098700110421                  $Nrai(xxi) +=1 ;
098800100623                  $scrivic  = *on ;
098900100519               Endif ;
099000100519            Endif ;
099100100518
099200100519       Endif ;
099300100519
099400100519         // imposto i vari valori in base all'attività che leggo  PREVENTIVO
099500100519
099600100519       If i88tcpe = 'P' or i88tcpe = 'E' ;
099700100519
099800110121          If atcdco = 0 ;
099900100519         // appuntamenti da effettuare nel periodo suddivisi per tipo trattativa
100000100519         // trattativa  maggiore di zero e attività 91 da effettuare nel periodo
100100100519            If atccad = '91' and atcnrv > *zeros and atcdad >= i88depi and
100200100528               atcdad <= i88depf and atcest <> 'N' and atccac <> '24' and
100300100519            // verifico da effettuare da un'agente dell'unificante
100400100519               %lookup(atccmm:$age) > 0 ;
100500100519
100600100519            // Verifico se la trattativa è mantenimento o incremento o incasso
100700100623                  $scrivip  = *on ;
100800100519                     If VisTpv = 'M' ;
100900100519                        Rpaappm += 1 ;
101000100519                     endif ;
101100100519                     If VisTpv = 'A' ;
101200100519                        Rpaappa += 1 ;
101300100519                     endif ;
101400100519                     If VisTpv = 'N' ;
101500100519                        Rpaappn += 1 ;
101600100519                     endif ;
101700100519                     If VisTpv = 'C' ;
101800100519                        Rpaappc += 1 ;
101900100519                     endif ;
102000110118                     If VisTpv = 'I' ;
102100110118                        Rpaappi += 1 ;
102200110118                     endif ;
102300110118                     If VisTpv = 'R' ;
102400110118                        Rpaappr += 1 ;
102500110118                     endif ;
102600100519            endif ;
102700100519
102800100519         // affiancamenti da effettuare nel periodo suddivisi per tipo trattativa
102900100519         // trattativa  maggiore di zero e attività 92 da eseguire nel periodo
103000100519            If atccad = '92' and atcnrv > *zeros and atcdad >= i88depi and
103100100528               atcdad <= i88depf and atccac <> 'NO' and atcest <> 'N' and
103200100519            // verifico da effettuare da un'agente dell'unificante
103300100519               %lookup(atccmm:$age) > 0 ;
103400100519
103500100519            // Verifico se la trattativa è mantenimento o incremento o incasso
103600100623                  $scrivip  = *on ;
103700100519                     If VisTpv = 'M' ;
103800100519                        Rpaaffm += 1 ;
103900100519                     endif ;
104000100519                     If VisTpv = 'A' ;
104100100519                        Rpaaffa += 1 ;
104200100519                     endif ;
104300100519                     If VisTpv = 'N' ;
104400100519                        Rpaaffn += 1 ;
104500100519                     endif ;
104600100519                     If VisTpv = 'C' ;
104700100519                        Rpaaffc += 1 ;
104800100519                     endif ;
104900110118                     If VisTpv = 'I' ;
105000110118                        Rpaaffi += 1 ;
105100110118                     endif ;
105200110118                     If VisTpv = 'R' ;
105300110118                        Rpaaffr += 1 ;
105400110118                     endif ;
105500100519            endif ;
105600100519
105700100519         // telefonate da effettuare nel periodo suddivisi per causale
105800100519            If atctat = 'T' and atcdad >= i88depi and
105900100519               atcdad <= i88depf and
106000100519            // verifico da effettuare da un'agente dell'unificante
106100100519               %lookup(atccmm:$age) > 0 ;
106200100519
106300100519            // Suddivido per causale
106400100519
106500100519            // causale  19
106600100519                  If atccad = '19'    ;
106700100519                     Rpatc19 += 1 ;
106800100623                     $scrivip  = *on ;
106900100519                  endif ;
107000100519            // causale  20
107100100519                  If atccad = '20'    ;
107200100519                     Rpatc20 += 1 ;
107300100623                     $scrivip  = *on ;
107400100519                  endif ;
107500100519            // causale  21
107600100519                  If atccad = '21'    ;
107700100519                     Rpatc21 += 1 ;
107800100623                     $scrivip  = *on ;
107900100519                  endif ;
108000100519            // causale  22
108100100519                  If atccad = '22'    ;
108200100519                     Rpatc22 += 1 ;
108300100623                     $scrivip  = *on ;
108400100519                  endif ;
108500100519            // causale  24
108600100519                  If atccad = '24'    ;
108700100519                     Rpatc24 += 1 ;
108800100623                     $scrivip  = *on ;
108900100519                  endif ;
109000100519            // causale  25
109100100519                  If atccad = '25'    ;
109200100519                     Rpatc25 += 1 ;
109300100623                     $scrivip  = *on ;
109400100519                  endif ;
109500100519            // causale  26
109600100519                  If atccad = '26'    ;
109700100519                     Rpatc26 += 1 ;
109800100623                     $scrivip  = *on ;
109900100519                  endif ;
110000100519            // causale  27
110100100519                  If atccad = '27'    ;
110200100519                     Rpatc27 += 1 ;
110300100623                     $scrivip  = *on ;
110400100519                  endif ;
110500100519            // causale  28
110600100519                  If atccad = '28'    ;
110700100519                     Rpatc28 += 1 ;
110800100623                     $scrivip  = *on ;
110900100519                  endif ;
111000100519            // causale  30
111100100519                  If atccad = '30'    ;
111200100519                     Rpatc30 += 1 ;
111300100623                     $scrivip  = *on ;
111400100519                  endif ;
111500100519            // causale  31
111600100519                  If atccad = '31'    ;
111700100519                     Rpatc31 += 1 ;
111800100623                     $scrivip  = *on ;
111900100519                  endif ;
112000100519            // causale  32
112100100519                  If atccad = '32'    ;
112200100519                     Rpatc32 += 1 ;
112300100623                     $scrivip  = *on ;
112400100519                  endif ;
112500100519            // causale  33
112600100519                  If atccad = '33'    ;
112700100519                     Rpatc33 += 1 ;
112800100623                     $scrivip  = *on ;
112900100519                  endif ;
113000110117            // causale  44
113100110117                  If atccad = '44'    ;
113200110117                     Rpatc44 += 1 ;
113300110117                     $scrivip  = *on ;
113400110117                  endif ;
113500100519            // causale  60
113600100519                  If atccad = '60'    ;
113700100519                     Rpatc60 += 1 ;
113800100623                     $scrivip  = *on ;
113900100519                  endif ;
114000110421            // causale  70
114100110421                  If atccad = '70'    ;
114200110421                     Rpatc70 += 1 ;
114300110421                     $scrivip  = *on ;
114400110421                  endif ;
114500110421            // causale  71
114600110421                  If atccad = '71'    ;
114700110421                     Rpatc71 += 1 ;
114800110421                     $scrivip  = *on ;
114900110421                  endif ;
115000110421            // causale  73
115100110421                  If atccad = '73'    ;
115200110421                     Rpatc73 += 1 ;
115300110421                     $scrivip  = *on ;
115400110421                  endif ;
115500110421            // causale  74
115600110421                  If atccad = '74'    ;
115700110421                     Rpatc74 += 1 ;
115800110421                     $scrivip  = *on ;
115900110421                  endif ;
116000110421            // causale  75
116100110421                  If atccad = '75'    ;
116200110421                     Rpatc75 += 1 ;
116300110421                     $scrivip  = *on ;
116400110421                  endif ;
116500100519
116600100519            endif ;
116700100518
116800100519         // OFFERTE    da effettuare nel periodo suddivisi per causale
116900100519            If atctat = 'O' and atcdad >= i88depi and
117000100519               atcdad <= i88depf and
117100100519            // verifico da effettuare da un'agente dell'unificante
117200100519               %lookup(atccmm:$age) > 0 ;
117300100519
117400100519                     Rpaoff  += 1 ;
117500100623                     $scrivip  = *on ;
117600100519            endif ;
117700100519
117800100519          // ferie parziali e totali in ore e minuti
117900100519            If atctat = 'F' and atcdad >= i88depi and
118000100519               atcdad <= i88depf and
118100100519            // verifico se da effettuare  da un'agente dell'unificante
118200100519               %lookup(atccmm:$age) > 0  ;
118300100519            // trasformo in minuti ed al momento della scrittura imposto
118400100519            // il campo totale inore e minuti
118500100519            // se ferie totali aggiungo 8 h * 60 m
118600100623               $scrivip  = *on ;
118700100519               If atccad = 'FT';
118800100526                  RPAFEGG += 1 ;
118900100519               else ;
119000100519                  W_iso_oii = %time(watcoii);
119100100519                  W_iso_oif = %time(Watcofi);
119200100519                  wTot_M = %diff(w_ISO_oif : w_ISO_oiI : *minutes);
119300100519                  WTot_M_FEP  +=Wtot_m ;
119400100519               endif;
119500100519            Endif ;
119600100519
119700100519          // Attività ufficio
119800100519            If atctat = 'U'  and atcdad >= i88depi and
119900100519               atcdad <= i88depf and
120000100519            // verifico se da effettuare  da un'agente dell'unificante
120100100519               %lookup(atccmm:$age) > 0  ;
120200100519            // trasformo in minuti ed al momento della scrittura imposto
120300100519            // il campo totale inore e minuti
120400100519               W_iso_oii = %time(watcoii);
120500100519               W_iso_oif = %time(Watcofi);
120600100519               wTot_M = %diff(w_ISO_oif : w_ISO_oiI : *minutes);
120700100519               WTot_M_UFP  +=Wtot_m ;
120800100623               $scrivip  = *on ;
120900100519            Endif ;
121000100519
121100110121          Endif ;
121200100519         Endif ;
121300100519
121400100326       ENDSR;
121500100519
121600100519       //--------------------------------------------------------------
121700100519       //?Scrittura record statitistica Consuntiva
121800100519       //--------------------------------------------------------------
121900100519       BEGSR SR_writeRCA ;
122000100519
122100100519         // decodifico agente
122200100519         exsr sr_deco_age ;
122300100519
122400100519         rcargf = $ageu(zz) ;
122500100519         rcadrg = §01age ;
122600100519         rcafun = §01fun ;
122700100519
122800100519         rcafil = %dec(%subst(%editc(rcargf: 'X'):1:3):3:0) ;
122900100519
123000100519         // Distretto e area unificante
123100100519         chain rcafil azorg01l ;
123200100519
123300100519         exsr SR_deco_dst ;
123400100519         exsr SR_deco_are ;
123500100519
123600100519         rcafid = orgdes ;
123700100519         rcadiv = orgfl3 ;
123800100519         rcadid = §17des ;
123900100519         rcaare = orgcar ;
124000100519         rcaard = §05des ;
124100100519
124200100519         // imposto i periodi
124300100519         rcapda = i88deci ;
124400100519         rcapal = i88decf ;
124500100519         // trasformo minuti in ore
124600100519         // ferie
124700100519             wTot_H = %div(wTot_M_FP : 60);
124800100519             wTot_R = %rem(wTot_M_FP : 60);
124900100519             Rcaferiehh = wtot_h;
125000100519             Rcaferiehh += (Wtot_r / 100);
125100100519         // riunioni d'area
125200100519             wTot_H = %div(wTot_M_RA : 60);
125300100519             wTot_R = %rem(wTot_M_RA : 60);
125400100519             Rcaufra    = wtot_h;
125500100519             Rcaufra    += (Wtot_r / 100);
125600100519         // riunioni commerciale
125700100519             wTot_H = %div(wTot_M_RC : 60);
125800100519             wTot_R = %rem(wTot_M_RC : 60);
125900100519             Rcaufrc    = wtot_h;
126000100519             Rcaufrc    += (Wtot_r / 100);
126100100519         // attività ufficio
126200100519             wTot_H = %div(wTot_M_UF : 60);
126300100519             wTot_R = %rem(wTot_M_UF : 60);
126400100519             Rcaufuf    = wtot_h;
126500100519             Rcaufuf    += (Wtot_r / 100);
126600100522         // attività inevase
126700100522             Rcacaai = Rcacaaids ;
126800100522             Rcanrai = Rcanraids ;
126900110427             Rcainevase = %xfoot($nrai);
127000100519
127100100604         // telefonate mai trattate
127200110421             Rcacaapm = Rcacaapmds ;
127300110421             Rcanrapm = Rcanrapmds ;
127400100604
127500110421         // telefonate effettuate su potenziali fuori trattativa già trattati
127600110421             Rcacaapg = Rcacaapgds ;
127700110421             Rcanrapg = Rcanrapgds ;
127800110427             Rcatepg = %xfoot($nrapg);
127900110421
128000110421         // telefonate effettuate su potenziali PERSI fuori trattativa
128100110421             Rcacaapp = Rcacaappds ;
128200110421             Rcanrapp = Rcanrappds ;
128300110421             Rcatepp = %xfoot($nrapp);
128400100610
128500100610         // telefonate effettuate su potenziali dentro trattativa
128600110421             Rcacaapt = Rcacaaptds ;
128700110421             Rcanrapt = Rcanraptds ;
128800110421             Rcatept  = %xfoot($nrapt);
128900110421
129000110421         // telefonate effettuate su potenziali PERSI dentro trattativa
129100110421             Rcacaappt = Rcacaapptds ;
129200110421             Rcanrappt = Rcanrapptds ;
129300110428             Rcateppt = %xfoot($nrappt);
129400100610
129500100610         // telefonate effettuate su clienti fuori trattativa
129600100610             Rcacaacf = Rcacaacfds ;
129700100610             Rcanracf = Rcanracfds ;
129800110421             Rcatecft = %xfoot($nracf);
129900100610
130000100610         // telefonate effettuate su clienti dentro trattativa
130100100610             Rcacaact = Rcacaactds ;
130200100610             Rcanract = Rcanractds ;
130300110421             Rcatect  = %xfoot($nract);
130400110421
130500110421         // telefonate post offerta
130600110421             Rcatepost = rcatepo30m + rcatepo30a + rcatepo33m + rcatepo33a ;
130700110421
130800110421         // telefonate caldi
130900110421             Rcatecaldi = rcatepc31m + rcatepc31a + rcatepc44m + rcatepc44a ;
131000110421
131100110421         // telefonate post vendita
131200110421             Rcateposve = rcatepv32m + rcatepv32a  ;
131300110421
131400110421         // telefonate mantenimento telefonico
131500110421             Rcatemante = rcatepc40m + rcatepc40a  ;
131600110421
131700110421         // telefonate incremento/aumento telefonico
131800110518         //  Rcateincre = rcatepc50m + rcatepc50a  ;
131900110421
132000110421         // TOTALE telefonate eseguite
132100110421             Rcatelese = rcatepm + rcatepg + rcatepp + rcatecft + rcatept +
132200110421                         rcatect + rcateppt + rcatepost + rcatecaldi +
132300110421                         rcateposve + rcatemante + rcateincre ;
132400100610
132500100519             rcadat = Woggi ;
132600100519             rcapru = knmus ;
132700100519         write wfrca000 ;
132800100519
132900100519       ENDSR;
133000100519
133100100519       //--------------------------------------------------------------
133200100519       //?Scrittura record statitistica Preventiva
133300100519       //--------------------------------------------------------------
133400100519       BEGSR SR_writeRPA ;
133500100519
133600100519         // decodifico agente
133700100519         exsr sr_deco_age ;
133800100519
133900100519         rpargf = $ageu(zz) ;
134000100519         rpadrg = §01age ;
134100100519         rpafun = §01fun ;
134200100519
134300100519         rpafil = %dec(%subst(%editc(rpargf: 'X'):1:3):3:0) ;
134400100519
134500100519         // Distretto e area unificante
134600100519         chain rpafil azorg01l ;
134700100519
134800100519         exsr SR_deco_dst ;
134900100519         exsr SR_deco_are ;
135000100519
135100100519         rpafid = orgdes ;
135200100519         rpadiv = orgfl3 ;
135300100519         rpadid = §17des ;
135400100519         rpaare = orgcar ;
135500100519         rpaard = §05des ;
135600100519
135700100519         // imposto i periodi
135800100519         rpapda = i88depi ;
135900100519         rpapal = i88depf ;
136000100519         // trasformo minuti in ore
136100100519         // ferie
136200100519             wTot_H = %div(wTot_M_FEP : 60);
136300100519             wTot_R = %rem(wTot_M_FEP : 60);
136400100519             Rpaferie = wtot_h;
136500100519             Rpaferie += (Wtot_r / 100);
136600100519         // attività ufficio
136700100519             wTot_H = %div(wTot_M_UFP : 60);
136800100519             wTot_R = %rem(wTot_M_UFP : 60);
136900100519             Rpauff     = wtot_h;
137000100519             Rpauff     += (Wtot_r / 100);
137100100519
137200100519
137300100519             rpadat = Woggi ;
137400100519             rpapru = knmus ;
137500100519
137600100519         write wfrpa000 ;
137700100519
137800100519       ENDSR;
137900100519
138000100519       //--------------------------------------------------------------
138100100519       //?Decodifico i dati relativi all'agente unificante
138200100519       //--------------------------------------------------------------
138300100519       BEGSR Sr_deco_age ;
138400100519
138500100519        // reperimento dei dati dell'unificante
138600100519                  clear  ds01;
138700100520                  TBLkut = 1;
138800100519                  TBLcod = '01';
138900100519                  TBLkey = %editc($ageu(zz): 'X');
139000100519                  chain (TBLkut : TBLcod : TBLkey) TABEL;
139100100519                  if   %found(TABEL00F);
139200100519                       ds01    = TBLuni;
139300100519                  endif;
139400100519       Endsr ;
139500100519       //--------------------------------------------------------------
139600100519       //?Decodifico distretto
139700100519       //--------------------------------------------------------------
139800100519       BEGSR Sr_deco_dst ;
139900100519
140000100519        // reperimento dei dati distretto
140100100519                  clear  ds17;
140200100520                  TBLkut = 1;
140300100519                  TBLcod = '17';
140400100519                  TBLkey = orgfl3 ;
140500100519                  chain (TBLkut : TBLcod : TBLkey) TABEL;
140600100519                  if   %found(TABEL00F);
140700100519                       ds17    = TBLuni;
140800100519                  endif;
140900100519       Endsr ;
141000100519
141100100519       //--------------------------------------------------------------
141200100519       //?Decodifico area
141300100519       //--------------------------------------------------------------
141400100519       BEGSR Sr_deco_are ;
141500100519
141600100519        // reperimento dei dati area
141700100519                  clear  ds05;
141800100520                  TBLkut = 1;
141900100519                  TBLcod = '05';
142000100519                  TBLkey = %editc(orgcar : 'X') ;
142100100519                  chain (TBLkut : TBLcod : TBLkey) TABEL;
142200100519                  if   %found(TABEL00F);
142300100519                       ds05    = TBLuni;
142400100519                  endif;
142500100519       Endsr ;
142600100519
142700080206       //--------------------------------------------------------------
142800080206       //?Reperimento Dati del job (Utente/Operativi).
142900080206       //--------------------------------------------------------------
143000080206       BEGSR DatiJob;
143100080206
143200080206         in(E) §AzUte;
143300080206         if NOT %error;
143400080206           in(E) §DatiUte;
143500080206         endif;
143600080206         if %error or RSut = *blanks;
143700080206           clear TIBS34ds;
143800080206           tibs34r(tibs34ds);
143900080206           in §AzUte;
144000080206           in §DatiUte;
144100080206         endif;
144200100325
144300100325         //?Richiamo pgm di controllo autorizzazioni utente così mi torna
144400100325         //?il codice autorizzazione relativo alle tariffe clienti
144500100325         //?Non controllo se abilitato o meno, lo ha già fatto il chiamante
144600100325         reset TNTAA1DS;
144700100325         ITAA1caut = '§utegtc';
144800100325         kpjbu     = TNTAA1DS;
144900100325         tntaa1c (kpjba);
145000100325         TNTAA1DS = kpjbu;
145100100325
145200100325         //?Se utente abilitato come 'AZ' mi carico la sk delle filiali di area
145300100325         IF  OTAA1cabi = 'AZ';
145400100325           OTAA1cabi = 'RA';
145500100325         ENDIF;
145600100325
145700100325         //?Carico filiali abilitate all'utente
145800100325         //?Non controllo se caricate o no le filiali
145900100325         //?tanto se non ho caricato nessuna filiale e la richiesta
146000100325         //?è per tutti i commerciali, non stampo niente
146100110120         //?Se richiesta un'area carico SOLO le filiali di quell'area
146200110120         IF  I88car > 0;
146300110120             clear TRUL31DS;
146400110120             clear TRUL31DS2;
146500110120             I31abi = 'AO';
146600110120             I31car = I88car ;
146700110120             trul31r (kpjba : trul31ds : trul31ds2);
146800110120         Else ;
146900110120             clear TRUL31DS;
147000110120             clear TRUL31DS2;
147100110328             if Dutpou = 046 ;
147200110328                I31abi = 'AZ'     ;
147300110328             else ;
147400110328                I31abi = OTAA1cabi;
147500110328                I31cdi = DUTdis;
147600110328                I31car = DUTare;
147700110328                I31cpo = DUTpou;
147800110328             Endif;
147900110120             trul31r (kpjba : trul31ds : trul31ds2);
148000110120         Endif;
148100080206
148200080206       ENDSR;
148300100325
148400100325       //--------------------------------------------------------------
148500100325       //?Carico sk agenti unificanti.
148600100325       //--------------------------------------------------------------
148700100326       BEGSR sr_CarAgeUni;
148800100325
148900100325         //?Carico gli agenti unificanti delle filiali gestite dall'utente
149000110120         //?o dell'area se richiesti
149100100326         clear xx;
149200100326         clear $AGEU;
149300100326         $EndAge = *off;
149400100325
149500100325         //?Imposto la stringa per SQL
149600100325         wSQL = 'select TBLKEY +
149700100325                 from TABEL00F +
149800100325                 where TBLCOD = ''01'' and TBLFLG = '' '' and +
149900100325                       substr(TBLUNI, 26, 3) in (';
150000100325
150100100325         yy = 0;
150200100325         xx = 1;
150300100325         FOR xx by 1 to %elem(pog);
150400100325           IF pog(xx) <> *zeros and pog(xx) <> *blanks;
150500100325             IF yy > 0;
150600100325               wSQL += ', ';
150700100325             ELSE;
150800100325               yy = 1;
150900100325             ENDIF;
151000100325             wSQL += '''';
151100100325             wSQL += pog(xx);
151200100325             wSQL += '''';
151300100325           ENDIF;
151400100325         ENDFOR;
151500100325
151600100325         wSQL += ') and TBLKEY = substr(TBLUNI, 26, 7) and +
151700110614                    substr(TBLUNI, 78, 1) = '' ''';
151800100325
151900100325         wSQL += ' order by TBLKEY +
152000100325                   for fetch only';
152100100325
152200100325         //?Dichiarazione cursore
152300100325         exec sql
152400100325           prepare A1   from :wSQL;
152500100325         exec sql
152600100325           declare AGEU cursor for A1;
152700100325
152800100325         //?Apertura del cursore
152900100325         exec sql
153000100325           open AGEU;
153100100325
153200100325         IF sqlcode < 0;
153300100325           $EndAge = *on;
153400100325         ENDIF;
153500100325
153600100325         yy = 0;
153700100325         DOW not $EndAge;
153800100325           exec sql
153900100325             fetch next from AGEU into :TBLKEY;
154000100325           IF sqlcod = 100 or sqlcod < 0;
154100100325             $EndAge = *on;
154200100325             leave;
154300100325           ENDIF;
154400100325
154500100325         yy += 1;
154600100326         $AGEU(yy) = %dec(%subst(TBLKEY:1:7):7:0);
154700100325
154800100325         ENDDO;
154900100325
155000100325         exec sql
155100100325           close AGEU;
155200100325
155300100325       ENDSR;
155400100305
155500100305       //--------------------------------------------------------------
155600100305       //?Preparazione stringa SQL.
155700100305       //--------------------------------------------------------------
155800100305       BEGSR sr_PrepSQL;
155900100305
156000100517       // se desidero la sola statistica consuntiva o entrambe
156100100519       If i88tcpe = 'C' or i88tcpe = 'E' ;
156200100423         wSQL = 'select * from TIATC00F +
156300100423                 where ( atccco in(';
156400100423       // agente che ha eseguito l'attivita
156500100325         yy = 0;
156600100325         xx = 1;
156700100325         FOR xx by 1 to %elem($Age);
156800100325           IF $Age(xx) <> *zeros;
156900100325             IF yy > 0;
157000100325               wSQL += ', ';
157100100325             ELSE;
157200100325               yy = 1;
157300100325             ENDIF;
157400100325             wSQL += %editc($Age(xx):'X');
157500100325           ENDIF;
157600100325         ENDFOR;
157700100423
157800100423         // nel limite di date impostate
157900100519           wSQL += ') and atcdco between ' + %editc(i88deci:'X') +
158000100519                   ' and ' + %editc(i88decf:'X');
158100100329
158200100520           wSQL += ') or ( atcco3 in (';
158300100423       // agente che ha inserito l'attivita
158400100423         yy = 0;
158500100423         xx = 1;
158600100423         FOR xx by 1 to %elem($Age);
158700100423           IF $Age(xx) <> *zeros;
158800100423             IF yy > 0;
158900100423               wSQL += ', ';
159000100423             ELSE;
159100100423               yy = 1;
159200100423             ENDIF;
159300100423             wSQL += %editc($Age(xx):'X');
159400100423           ENDIF;
159500100423         ENDFOR;
159600100423
159700100423         // nel limite di date impostate
159800100629           wSQL += ') and atcdim between ' + %editc(i88deci:'X') +
159900100519                   ' and ' + %editc(i88decf:'X');
160000100423
160100100520           wSQL += ') or ( atccmm in (';
160200100517       // agente che deve eseguire l'attività
160300100423         yy = 0;
160400100423         xx = 1;
160500100423         FOR xx by 1 to %elem($Age);
160600100423           IF $Age(xx) <> *zeros;
160700100423             IF yy > 0;
160800100423               wSQL += ', ';
160900100423             ELSE;
161000100423               yy = 1;
161100100423             ENDIF;
161200100423             wSQL += %editc($Age(xx):'X');
161300100423           ENDIF;
161400100423         ENDFOR;
161500100517         endif ;
161600100423
161700100517       // se desidero la sola statistica preventiva
161800100519       If i88tcpe = 'P' ;
161900100517         wSQL = 'select * from TIATC00F +
162000100517                 where ( atccmm in(';
162100100517       // agente che ha eseguito l'attivita
162200100517         yy = 0;
162300100517         xx = 1;
162400100517         FOR xx by 1 to %elem($Age);
162500100517           IF $Age(xx) <> *zeros;
162600100517             IF yy > 0;
162700100517               wSQL += ', ';
162800100517             ELSE;
162900100517               yy = 1;
163000100517             ENDIF;
163100100517             wSQL += %editc($Age(xx):'X');
163200100517           ENDIF;
163300100517         ENDFOR;
163400100517       Endif ;
163500100517
163600100517         // se desidero sia la statistica preventiva che consuntiva
163700100519         If i88tcpe = 'E' ;
163800100517         // nel limite di date impostate sia consuntive o preventive
163900100623           wSQL += ') and ((atcdad <= '+ %editc(i88decf:'X') +
164000100623                    ') or (atcdad between ' +
164100100519                   %editc(i88depi:'X') + ' and ' + %editc(i88depf:'X');
164200100423
164300100611           wSQL += ')))';
164400100517         endif ;
164500100517
164600100517         // se desidero solo la statistica preventiva
164700100519         If i88tcpe = 'P' ;
164800100517         // nel limite di date impostate come preventive
164900100519           wSQL += ') and (atcdad between ' + %editc(i88depi:'X') +
165000100519                   ' and ' + %editc(i88depf:'X') ;
165100100517
165200100520           wSQL += '))';
165300100517         endif ;
165400100329
165500100517         // se desidero solo la statistica consuntiva
165600100519         If i88tcpe = 'C' ;
165700100517         // nel limite di date impostate consuntive
165800100623           wSQL += ') and (atcdad <= ' + %editc(i88decf:'X') ;
165900100520
166000100520       //  wSQL += ') and (atcdad between ' + %editc(i88deci:'X') +
166100100520       //          ' and ' + %editc(i88decf:'X') + ') r (atcdad betwee n ' +
166200100520       //          %editc(i88deci:'X') + ' and ' + %editc(i88decf:'X');
166300100520         //        ' and ' + %editc(i88decf:'X') + ') or (atcdad between ' +
166400100520         //        %editc(i88deci:'X') + ' and ' + %editc(i88decf:'X');
166500100517
166600100517           wSQL += '))';
166700100517         endif ;
166800100517
166900100520         wSQL += '  for fetch only ' ;
167000100305
167100100305       ENDSR;
167200100305
167300080206       //--------------------------------------------------------------
167400080206       //?Operazioni finali.
167500080206       //--------------------------------------------------------------
167600080206       BEGSR RoutEnd;
167700090521
167800100522         kpjbu = trmk88ds ;
167900080206         *inLR = *on;
168000080206         return;
168100080206
168200080206       ENDSR;
168300080206
168400080206      /end-free
168500100610** causali telefonate   CTE
1686001105311415192021222425262728303132334142446070717273747580819192
