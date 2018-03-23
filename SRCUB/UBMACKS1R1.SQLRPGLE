000100121217       //==============================================================
000200161115       // Invio email push per differenza 1 TIMAC
000300121217       //==============================================================
000400141008
000500121217       //--------------------------------------------------------------
000600161115       // Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700121217       //--------------------------------------------------------------
000800121217
000900121217     /*PRM dbgview(*source)
001000121217     /*PRM commit(*none)
001100121217     /*END
001200161115     **
001300161115     ** ISTRUZIONI PER LA COMPILAZIONE
001400161115     **
001500161115     ** 1. Creare il pgm UBMACKS1R1 (Opz 14 PDM)
001600161115     **
001700121217
001800121217       //--------------------------------------------------------------
001900170127       // Spec
002000121217       //--------------------------------------------------------------
002100121217
002200121217     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002300161115     h dftactgrp(*no) actgrp('QILE')
002400121217     h alwnull(*inputonly)
002500121217     h bnddir('UBBNDDIR')
002600121217
002700121217       //--------------------------------------------------------------
002800161115       // Dichiarazione file.                                          ?
002900121217       //--------------------------------------------------------------
003000121217
003100121217       //--------------------------------------------------------------
003200161115       // Definizione costanti.                                        ?
003300121217       //--------------------------------------------------------------
003400121217
003500161115       // - Codice utente conti?
003600121217     d c_Kut           c                   const(1)
003700121218
003800161115       // - Costanti per la definizione delle schiere con i nomi?
003900161115       //   degli iSeries da elaborare e delle relative librerie?
004000140925     d c_NrSyst        c                   const(2)
004100140925     d c_NrLibr        c                   const(2)
004200121217
004300121217       //--------------------------------------------------------------
004400161115       // Definizione schiere.                                         ?
004500121217       //--------------------------------------------------------------
004600121217
004700161115       // - iSeries  &  Librerie con entrambi i file tabelle?
004800121218     d $iSystem        s                   like(currSysNetA)
004900121218     d                                     dim(c_NrSyst)
005000121218     d                                     ctdata   perrcd( 1)
005100121218     d $Libraries      s                   like(ds_Libr)
005200121218     d                                     dim(c_NrSyst)
005300121218     d                                     alt($iSystem)
005400161219
005500161219       // -?Ricerca/Controllo tabelle (TNTBE)?
005600161219     d TIBS02ds      e ds                  inz
005700161219     d   T02mod      e                     inz('C')
005800161219     d   T02cod      e                     inz('VPR')
005900121217
006000121217       //--------------------------------------------------------------
006100161115       // Definizione aree dati.                                       ?
006200121217       //--------------------------------------------------------------
006300121217
006400161115       // - Dati utente?
006500121218     d §AzUte        e ds                  extname(AZUTE00F)
006600121218     d                                     dtaara
006700121218     d §DatiUte      e ds                  extname(dDatiUte)
006800121218     d                                     dtaara
006900121217
007000121217       //--------------------------------------------------------------
007100161115       // Definizione strutture dati.                                  ?
007200121217       //--------------------------------------------------------------
007300121217
007400161115       // - Parametri in input?
007500121218     d KPJBA         e ds
007600121218
007700161115       // - Ridefinizione elenco librerie in cui elaborare le tabelle?
007800140925     d ds_Libr         ds                  inz
007900121218     d   $Libr                       10    dim(c_NrLibr) inz
008000121217
008100161115       // - Status?
008200121217     d Psds           sds
008300121217     d   SDSpgm          *proc
008400121217     d*//JobName             244    253                                         Job name
008500121217     d*//JobUser             254    263                                         User name
008600121217     d*//JobNumber           264    269s 0                                      Job number
008700121217
008800121217       //--------------------------------------------------------------
008900161115       // Definizione variabili.                                       ?
009000121217       //--------------------------------------------------------------
009100121217
009200161115       // - Indici di schiera?
009300121217     d xx              s              5  0 inz
009400121217     d yy              s              5  0 inz
009500121217     d ww              s              5  0 inz
009600140925     d wX              s              5  0 inz
009700140925     d wY              s              5  0 inz
009800121218
009900161115       // - Nome esteso Libreria/File dei 2 file tabelle?
010000121220     d wLibTABEL       s             21a   inz
010100121218     d wLibTNTBE       s             21a   inz
010200121217
010300161115       // - Campi di comodo?
010400121218     d w_iSystem       s              1  0 inz
010500121218     d w_SisInf        s              3  0 inz
010600141028     d wDataDa         s              8  0 inz
010700141028     d wDataA          s              8  0 inz
010800141028     d wDay            s              8  0 inz
010900141028     d dateISO         s               d
011000121218     d wErr            s             10i 0 inz
011100161219     d dayNbr          s             11p 0
011200141021     D pMsgV           s           5000    inz varying
011300141028     d wVideo          s               n
011400161117     D wAlert1         s               n
011500161117     D wAlert2         s               n
011600161219     D wAlert3         s               n
011700170127     D wAlert4         s               n
011800161219     d wAvgSendEmail   s             12s 3
011900161116
012000161116     D uMACDtChk       s               d
012100161116     D uMACTmChk       s               t
012200161116     d uMACDif1        s             12s 3
012300170127     d*Test4DS         ds                  occurs(5)
012400170127     d*                                    qualified
012500170127     d*Test4DS         ds                  qualified dim(5)
012600170127     d Test4DS         ds
012700170127     D  DtChk                          d
012800170127     D  TmChk                          t
012900170127     d  Server                       10a
013000170127     d  Cnt1                          6s 0
013100170127     d Test4Dim        ds                  dim(5) qualified
013200170127     D  DtChk                          d
013300170127     D  TmChk                          t
013400170127     d  Server                       10a
013500170127     d  Cnt1                          6s 0
013600161116
013700161116     d wDtChk          s               d
013800161116     d wDatePrevious   s               d
013900161116     d wTimePrevious   s               t
014000161116     d wDC_Day_NaI     s              9a
014100161117     d wCntOggi        s              7s 0
014200161117     d wCntPrevious    s              7s 0
014300161116     d wTrovato        s               n   inz(*off)
014400161116     d wNoncene        s               n   inz(*off)
014500170117     d wTS             s               z
014600170117     d wDt             s               d
014700170117     d wHours          s              2s 0
014800170127     d serverInErrore  s             10a
014900170127     d cntInErrore     s              6s 0
015000170127     d cntPrev         s              6s 0
015100121218
015200161115       // - Dati di  dialogo
015300161115     D MACKSDS1        DS
015400141218     D  IndEMail                    126a
015500141027
015600161115       // - Stringa SQL da eseguire?
015700121217     d wSQL            s           2048    inz  varying
015800121217
015900121217       //--------------------------------------------------------------
016000161115       // Definizione prototipi procedure.                             ?
016100121217       //--------------------------------------------------------------
016200121218
016300161115       // - Reperimento NETA sistema AS/400 corrente?
016400121218     d currSysNeta     s              8a   inz
016500121218      /copy gaitrasrc/srcProtoPR,UBRTVNETA
016600121218
016700161115       // - Reperimento dati utente?
016800121218     d TIBS34ds      e ds
016900121218      /copy gaitrasrc/srcProtoPR,TIBS34R
017000121217
017100161115       // - Parametri API QCAPCMD (Process Commands)?
017200121217     d Qcmd            s           2048    inz  varying
017300161219
017400161219       // - Reperimento dati tabelle
017500161219      /copy gaitrasrc/srcProtoPR,TIBS02R
017600121217
017700121217       //--------------------------------------------------------------
017800161115       // Definizione key-list.                                        ?
017900121217       //--------------------------------------------------------------
018000121217
018100121217
018200121217       //--------------------------------------------------------------
018300161115       // Definizione parametri procedura.                             ?
018400121217       //--------------------------------------------------------------
018500121217
018600121217     c     *Entry        plist
018700121217     c                   parm                    KPJBA
018800121217
018900121217       //--------------------------------------------------------------
019000161115       // M A I N - L I N E                                            ?
019100121217       //--------------------------------------------------------------
019200121217
019300161115       // - Operazioni iniziali?
019400121217       exsr sr_RoutInz;
019500161116
019600161116       // - Eseguo
019700161116       exsr  sr_Esegui;
019800140925
019900161116       // - viene mandata una e-mail a chi è preposto?
020000161116       //   se non si deve passare il risultato al chiamante
020100161116       if wVideo = *off
020200170127       and (wAlert1 = *on OR  wAlert2 = *on OR wAlert3 = *on OR
020300170127            wAlert4 = *on);
020400161201         // memorizzo che fascia in errore
020500161201         exsr  sr_MemoErr;
020600161201         // spedisco email
020700161201         exsr  sr_SendEmail;
020800141028       endif;
020900121217
021000161116       // - Operazioni finali?
021100121217       exsr sr_RoutEnd;
021200121217
021300121217       //--------------------------------------------------------------
021400161115       // Operazioni iniziali.                                         ?
021500121217       //--------------------------------------------------------------
021600121217       BEGSR  sr_RoutInz;
021700141217
021800161115         MACKSDS1 = kpjbu;
021900121217
022000121217         exec sql  set option  dynusrprf = *owner,
022100121217                               closqlcsr = *endmod;
022200121218
022300161115         // - Verifica del sistema AS/400 corrente?
022400121218         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
022500161117           exsr sr_RoutEnd;
022600121218         endif;
022700121219
022800161115         // - Reperimento dati job?
022900121219         exsr  sr_DatiJob;
023000140925
023100141028         // il risultato va inviato via email
023200141217         wVideo = *off;
023300141217         // se è stato passato un indirizzo email specifico, uso quello altrimenti uso i default
023400141217         if IndEMail <> *blank;
023500141217           peml = %trim(IndEMail);
023600141217         else;
023700170920           // a seconda del sistema AS/400 corrente imposto il ricevente
023800170920           if %subst (currSysNeta:1:6) = 'SETRAS';
023900170920             peml = 'cedalert@brt.it';
024000170920             pcceml = *blanks;
024100170920           else;
024200170920             peml = 'cedvas@brt.it';
024300170920             pcceml = 'luciano.carini@brt.it';
024400170920           endif;
024500141217         endif;
024600140925
024700161117         pogg = 'BARTMAIL - RITARDO INVIO EMAIL';
024800141217         pMsgV = *blank;
024900140925
025000140925         clear  wX;
025100140925         clear  wY;
025200130208
025300140925       ENDSR;
025400121219
025500121219       //--------------------------------------------------------------
025600161115       // Reperimento Dati del job (Utente/Operativi).                 ?
025700121219       //--------------------------------------------------------------
025800121219       BEGSR  sr_DatiJob;
025900141028
026000121219         in(E) §AzUte;
026100121219         if NOT %error;
026200121219           in(E) §DatiUte;
026300121219         endif;
026400121219         if %error or RSut = *blanks;
026500121219           clear TIBS34ds;
026600121219           tibs34r ( tibs34ds );
026700121219           in §AzUte;
026800121219           in §DatiUte;
026900121219         endif;
027000140925
027100121219       ENDSR;
027200141028
027300141028       //--------------------------------------------------------------
027400161116       // Esecuzione del test
027500141028       //--------------------------------------------------------------
027600161116       BEGSR  sr_Esegui;
027700161219
027800161219         // reperisco da tabella VPR il tempo medio da non superare (non superare il doppio)
027900161219         reset  TIBS02ds;
028000161219         //T02mod = 'C';       ? (già così)?
028100161219         //T02cod = 'VPS';      ?(già così)?
028200161219         T02ke1 = 'AVG_SEND_EMAIL';
028300161219         T02sif = knsif;
028400161219         TNTBE_RicercaControllo ( kpjba : TIBS02ds );
028500161219         wAvgSendEmail = 0;
028600161219         if  T02err = *blank;
028700161219           wAvgSendEmail = %dec(%subst(T02uni:1:13):12:3);
028800161219         endif;
028900170117
029000170127         // reperisco i dati dell'ultima fascia oraria scritta raggruppando tutti i server
029100161116         exec sql
029200161201           select macdtchk, mactmchk, avg(macdif1)
029300161116           into
029400161116           :umacdtchk, :umactmchk, :umacdif1
029500161116           from timac00f
029600161117           where char(macdtchk) concat char(mactmchk) =
029700161117           (select max(
029800161201           char(macdtchk) concat char(mactmchk)) from timac00f)
029900161201           group by macdtchk, mactmchk;
030000170117
030100170117         // I TEST 1 E 2 NON VENGONO PIU' ESEGUITI
030200170117         if 1<>1;
030300161116
030400161116         // il test che mi determina se devo mandare una email di avviso è:
030500161116         // se media tempo invio > doppio della media tempo invio, in questi 2 casi:
030600161116         // 1- stessa fascia oraria ma giorno precedente, se giorno precedente no sabato no domenica
030700161116         //    e che il nr.totale di email inviate è almeno il 70% di quello di oggi
030800161201         //    (per escludere i festivi o i giorni non utili per confronti)
030900161116         exsr RepTIMACGgPrec;
031000161116         // se ho trovato il giorno precedente giusto
031100161116         if wTrovato = *on;
031200161116           // eseguo test 1
031300161116           exec sql
031400161116           select
031500161201           (case when :umacdif1 > avg(macdif1) * 2 then '1'
031600161116           else '0' end)
031700161116           into
031800161116           :wAlert1
031900161116           from timac00f
032000161116           where
032100161116           macdtchk = :wDatePrevious
032200161116           and
032300161201           mactmchk = :umactmchk
032400161201           group by macdtchk, mactmchk;
032500161116           // controllo il risultato
032600161116           select;
032700161116            when sqlcod < 0;
032800161116             // errore SQL (non mando alert)
032900161116             clear wAlert1;
033000161116            when sqlcod = 100;
033100161116             // non trovato (non mando alert)
033200161116             clear wAlert1;
033300161116            other;
033400161116             // proseguo
033500161116           endsl;
033600161116         endif;
033700161116
033800161116         // 2- fascia oraria precedente ma stesso giorno, se fascia oraria corrente è compresa tra
033900161116         //    le 16 e le 21
034000161116           // eseguo test 2
034100161116           exec sql
034200161116           select
034300161201           (case when :umacdif1 > avg(macdif1) * 2 then '1'
034400161116           else '0' end)
034500161116           into
034600161116           :wAlert2
034700161116           from timac00f
034800161116           where
034900161117           dec(substr(char(:umactmchk) , 1 , 2) , 2 , 0) between 14 and 21
035000161116           and
035100161116           macdtchk = :wDtChk
035200161116           and
035300161117           mactmchk =
035400161117           time(
035500161117           trim(char(dec(substr(char(:umactmchk) , 1 , 2) , 2 , 0) - 1))
035600161201           concat '.00.00')
035700161201           group by macdtchk, mactmchk;
035800161116           // controllo il risultato
035900161116           select;
036000161116            when sqlcod < 0;
036100161116             // errore SQL (non mando alert)
036200161116             clear wAlert2;
036300161116            when sqlcod = 100;
036400161116             // non trovato (non mando alert)
036500161116             clear wAlert2;
036600161116            other;
036700161116             // proseguo
036800161116           endsl;
036900161219
037000170117         endif;
037100170117
037200170117         // 3- fascia oraria precedente maggiore del doppio del tempo medio di invio
037300170117         // la precedente perché è già completa rispetto l'attuale
037400170117         // per cui mi calcolo qual è la fascia precedente (l'unico problema è se l'attuale è la 0
037500170117         // perché bisogna andare indietro di 1 giorno)
037600170117         wTS = %timestamp(%char(umacdtchk) + '-' +
037700170117                          %char(umactmchk) + '.000000')
037800170117               - %hours(1);
037900170117         // estraggo la fascia oraria
038000170117         wHours = %subdt(wTS:*H);
038100170117         // recupero la data ottenuta
038200170117         wDt = %date(wTS);
038300170117
038400170117         // reperisco i dati dell'ultima fascia oraria scritta - 1
038500170117         exec sql
038600170117           select macdtchk, mactmchk, avg(macdif1)
038700170117           into
038800170117           :umacdtchk, :umactmchk, :umacdif1
038900170117           from timac00f
039000170117           where macdtchk = :wDt and
039100170117                 substr(char(mactmchk) , 1 , 2) = char(:wHours)
039200170117           group by macdtchk, mactmchk;
039300170117           // uso sysdummy1 perché in realtà non devo estrarre dati (li ho già estratti)
039400161219           // eseguo solo il test
039500161219
039600170127         // eseguo test 3
039700170127         exec sql
039800170127         select
039900170127         (case when :umacdif1 > :wAvgSendEmail then '1'
040000170127         else '0' end)
040100170127         into
040200170127         :wAlert3
040300170127         from sysibm/sysdummy1;
040400170127         // controllo il risultato
040500170127         select;
040600170127          when sqlcod < 0;
040700170127           // errore SQL (non mando alert)
040800170127           clear wAlert3;
040900170127          when sqlcod = 100;
041000170127           // non trovato (non mando alert)
041100170127           clear wAlert3;
041200170127          other;
041300170127           // proseguo
041400170127         endsl;
041500141020
041600170127         // eseguo test 4
041700170127         exsr sr_Test4;
041800170127
041900141020       ENDSR;
042000161116       //----------------------------------------------------------------------*
042100161116       // Reperisco rcd TIMAC stesso orario ma giorno precedente, no sabato no domenica
042200161116       // e che il nr.totale di email inviate è almeno il 70% di quello di oggi
042300161116       // (per escludere i festivi)
042400161116       //----------------------------------------------------------------------*
042500161116         begsr RepTIMACGgPrec;
042600161116
042700161116           // ciclo finché non ho trovato il giorno precedente oppure è chiaro che
042800161116           // non ce ne è
042900161117           wDtChk = umacdtchk;
043000161116           wTrovato = *off;
043100161116           wNoncene = *off;
043200161116           dou wTrovato = *on or
043300161116               wNoncene = *on;
043400161116             exsr RepGgPrec;
043500161116           enddo;
043600161116
043700161116
043800161116         endsr;
043900161116       //----------------------------------------------------------------------*
044000161116       // Reperisco il giorno precedente di una data passata, no sabato no domenica
044100161116       // e che il nr.totale di email inviate è almeno il 70% di quello di oggi
044200161116       // (per escludere i festivi)
044300161116       //----------------------------------------------------------------------*
044400161116         begsr RepGgPrec;
044500161116
044600161116         // controllo che giorno è quello in ingresso
044700161116         exec sql
044800161116         set :wDC_DAY_NAI = (select DC_DAY_NAI from DATECNV0F
044900161116                             where DC_Date = :wDtChk);
045000161116
045100161116         select;
045200161116          // se è lunedì, come precedente devo andare indietro di 3 gg (a venerdì)
045300161116          when wDC_DAY_NAI = 'Lunedì';
045400161116           wDatePrevious = wDtChk - %days(3);
045500161116          // se è domenica, come precedente devo andare indietro di 2 gg (a venerdì)
045600161117          when wDC_DAY_NAI = 'Domenica';
045700161116           wDatePrevious = wDtChk - %days(2);
045800161117          // se NON è né lunedì né domenica, vado indietro di 1 gg
045900161116          other;
046000161116           wDatePrevious = wDtChk - %days(1);
046100161116         endsl;
046200161116
046300161116         // controllo il totale differenza media del giorno definito
046400161116         // sia almeno il 70% di quella di oggi
046500161116         // (per escludere i festivi)
046600161116         exec sql
046700161116         set :wCntOggi = (
046800161116                   select sum(maccnt1)
046900161116                   from timac00f
047000161116                   where macdtchk = :wDtChk
047100161116                   group by macdtchk);
047200161116
047300161116         // controllo il risultato
047400161116         select;
047500161116          when sqlcod < 0;
047600161116           // errore SQL
047700161116           wNoncene = *on;
047800161116           leavesr;
047900161116          when sqlcod = 100;
048000161116           // non c'è questo precedente per cui non ci sono rcd
048100161116           wNoncene = *on;
048200161116           leavesr;
048300161116          other;
048400161116           // proseguo
048500161116         endsl;
048600161116
048700161116         exec sql
048800161116         set :wCntPrevious = (
048900161116                   select sum(maccnt1)
049000161116                   from timac00f
049100161116                   where macdtchk = :wDatePrevious
049200161116                   group by macdtchk);
049300161116
049400161116         // controllo il risultato
049500161116         select;
049600161116          when sqlcod < 0;
049700161116           // errore SQL
049800161116           wNoncene = *on;
049900161116           leavesr;
050000161116          when sqlcod = 100;
050100161116           // non c'è questo precedente per cui non ci sono rcd
050200161116           wNoncene = *on;
050300161116           leavesr;
050400161116          other;
050500161116           // proseguo
050600161116         endsl;
050700161116
050800161116         if wCntPrevious >= wCntOggi * 70/100;
050900161116           // questa data va bene e leggo il rcd di TIMAC00F
051000161116           wTrovato = *on;
051100161116         else;
051200161116           // devo leggere un nuovo precedente
051300161116           wDtChk = wDatePrevious;
051400161116         endif;
051500161116
051600121217       ENDSR;
051700121217
051800121217       //--------------------------------------------------------------
051900161201       // Memorizzo che la fascia è in errore.                               ?
052000121217       //--------------------------------------------------------------
052100161201       BEGSR  sr_MemoErr;
052200161116
052300161201         // eseguo memorizzazione per giorno-fascia oraria, tralasciando i server
052400161201         // quindi le righe aggiornate saranno più d'una
052500161201         exec sql
052600161201         update timac00f
052700161201         set macerr1 = 'E'
052800161201         where
052900161201         macdtchk = :umacdtchk
053000161201         and
053100161201         mactmchk = :umactmchk;
053200161201
053300161201       ENDSR;
053400161201
053500170127       //--------------------------------------------------------------
053600170127       // Test 4
053700170127       //--------------------------------------------------------------
053800170127       BEGSR  sr_Test4;
053900170127
054000170127         wAlert4 = *off;
054100170127         serverInErrore = *blank;
054200170127         cntInErrore = 0;
054300170127         cntPrev     = 0;
054400170127
054500170127         // siccome devo confrontare le email prese in considerazione dai vari server per quella
054600170127         // fascia oraria, devo leggere più righe, per cui uso una DS multipla
054700170127         clear Test4DS;
054800170127         // - Creazione stringa SQL da eseguire
054900170127         wSQL =
055000170127          'select macdtchk, mactmchk, macserver, avg(maccnt1) +
055100170127           from timac00f +
055200170127           where macdtchk = ''' + %char(wDT:*iso) + ''' and +
055300170127           substr(char(mactmchk) , 1 , 2) = ' + %char(wHours) +
055400170127          ' group by macdtchk, mactmchk, macserver';
055500170127         // - Dichiarazione del cursore
055600170127         exec sql  prepare S1  from :wSQL;
055700170127         exec sql  declare C1  cursor for S1;
055800170127         // - Apertura cursore
055900170127         exec sql   open C1;
056000170127         // punto all'occorrenza da valorizzare
056100170127         wX = 0;
056200170127         //exec sql  fetch next from C1 for 5 rows into :Test4DS;
056300170127         DoU  SQLCode = 100 or SQLCode < 0;
056400170127           exec sql  fetch next from C1 into :Test4DS;
056500170127           select;
056600170127             // - Fine lettura
056700170127            when  sqlCode = 100;
056800170127             // esco dal ciclo
056900170127             leave;
057000170127            // - Errore
057100170127            when  sqlCode < *zero;
057200170127             // esco dal ciclo
057300170127             leave;
057400170127            // - Tutto ok
057500170127            other;
057600170127             // proseguo
057700170127             wX += 1;
057800170127             Test4Dim(wX) = Test4DS;
057900170127           endsl;
058000170127         enddo;
058100170127         // - Errore
058200170127         //if sqlCode < *zero;
058300170127         //  // esco dalla routine
058400170127         //  leavesr;
058500170127         //endif;
058600170127
058700170127         // - Chiusura cursore
058800170127         exec sql  close C1;
058900170127         for wX=1 to 5;
059000170127           // se il server è valorizzato
059100170127           if Test4Dim(wX).server <> *blank;
059200170127             // controllo se il valore delle email è meno della 1/2 del precedente
059300170127             if Test4Dim(wX).cnt1 < cntPrev     / 2;
059400170127               serverInErrore = Test4Dim(wX).server;
059500170127               cntInErrore = Test4Dim(wX).cnt1;
059600170127             endif;
059700170127             cntPrev     = Test4Dim(wX).cnt1;
059800170127           endif;
059900170127         endfor;
060000170127         if serverInErrore <> *blank;
060100170127           wAlert4 = *on;
060200170127         endif;
060300170127
060400170127       endsr;
060500170127
060600161201       //--------------------------------------------------------------
060700161201       // Invio e-mail di avviso errore.                               ?
060800161201       //--------------------------------------------------------------
060900161201       BEGSR  sr_SendEmail;
061000161201
061100161117         // preparo il msg
061200161117         pMsgV =
061300161117         ':/N' +
061400161117         'Durante l''invio email di oggi ' +
061500161117         %subst(%char(umacdtchk):9:2) + '/' + %subst(%char(umacdtchk):6:2) +
061600161117         ' nella fascia oraria ' +
061700161117         %subst(%char(umactmchk):1:2) + '-' +
061800161117         %char(%dec(%subst(%char(umactmchk):1:2):2:0) + 1) +
061900161117         ' è stato rilevato un sensibile aumento del tempo medio di invio +
062000161219           email:' +
062100161219         ':/N';
062200161219         if wAlert1 = *on;
062300161219           pMsgV = %trim(pMsgV) +
062400161219           '  - rispetto alla stessa fascia oraria +
062500161219            del giorno precedente lavorativo;' +
062600161219           ':/N';
062700161219         endif;
062800161219         if wAlert2 = *on;
062900161219           pMsgV = %trim(pMsgV) +
063000161219           '  - rispetto alla fascia oraria precedente +
063100161219            dello stesso giorno;' +
063200161219           ':/N';
063300161219         endif;
063400161220         if wAlert3 = *on;
063500161220           pMsgV = %trim(pMsgV) +
063600161220           '  - rispetto al Tempo Medio di Riferimento;' +
063700161220           ':/N';
063800161220         endif;
063900170127         if wAlert4 = *on;
064000170127           pMsgV = %trim(pMsgV) +
064100170127           '  - dovuto a problemi ai server, probabilmente: ' +
064200170127           serverInErrore +
064300170127           ':/N';
064400170127         endif;
064500161219         pMsgV = %trim(pMsgV) +
064600161219         ':/N' +
064700161117         'Verificare il corretto funzionamento dell''intera infrastruttura +
064800161117          (specialmente lato server di posta InRete) e delle applicazioni.' +
064900161117         ':/N';
065000161116
065100161117         // lo aggiungo nella email
065200161117         exsr AggEmail;
065300140926
065400141021         pmsg = pMsgV;
065500141021         // se sono qui, qualcosa ho scritto
065600141021         exsr EmailInvia;
065700121217
065800121217       ENDSR;
065900121217
066000121217       //--------------------------------------------------------------
066100161116       // Operazioni finali.                                           ?
066200121217       //--------------------------------------------------------------
066300121217       BEGSR  sr_RoutEnd;
066400141028
066500161117         // - valorizza i parametri di output?
066600161117         // kpjbu =          ;   NON NE HO
066700161219
066800161219         // Chiusura applicazioni precedentemente aperte?
066900161219         if  T02ke1 <> *blank;
067000161219           clear  TIBS02ds;
067100161219           T02tla = 'C';
067200161219           TNTBE_RicercaControllo ( kpjba : TIBS02ds );
067300161219         endif;
067400121217
067500161116         // - Chiusura pgm?
067600141029         *inlr = *on;
067700121217         return;
067800121217
067900121217       ENDSR;
068000121218
068100140925     C*------------------------------------------------------------------------*
068200140925     C* AggEmail: aggiunge rcd all'e-mail
068300140925     C*------------------------------------------------------------------------*
068400141217     C     AggEmail      BEGSR
068500140925     C*
068600141021     C* questa routine non fa niente perché pMsgV è già stato valorizzato prima
068700140925     C*
068800140925     C                   ENDSR
068900140925     C*------------------------------------------------------------------------*
069000140925     C* EmailInvia - Invio e-mail di conferma forzatura
069100140925     C*------------------------------------------------------------------------*
069200140925     C     EmailInvia    BEGSR
069300140925     C*
069400140925     C* invio e-mail
069500140925     C                   call      'TIS701C1'
069600140925     C                   parm                    peml            253
069700140925     C                   parm                    pcceml          253
069800140925     C                   parm                    pogg             44
069900140925     C                   parm                    pmsg           5000
070000140925     C                   parm                    pesito            1
070100140925     C*
070200140925     C                   ENDSR
070300121218** -?$iSystem / $Libraries:?Sistemi AS/400 & Librerie con entrambi i file?
070400121218SETRAS  GAITRAGRU FILTRAGRU
070500121218AS888   GAITRAGRPSFILTRAGRPF
