000100091228       //==============================================================
000200160601       // Driver ctrl/conteggio ORM
000300091228       //==============================================================
000400091228
000500091228       //--------------------------------------------------------------
000600121106       // Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700091228       //--------------------------------------------------------------
000800091228
000900100325     /*PRM dbgview(*source)
001000091223     /*END
001100160202     **
001200160202     ** ISTRUZIONI PER LA COMPILAZIONE
001300160202     **
001400160601     ** 1. Creare il modulo UBORMCNTR (Opz 15 PDM)
001500160601     ** 2. Creare/Aggiornare il programma di servizio UBORMCNTR (CRTSRVPGM / UPDSRVPGM)
001600160601     **    con ACTGRP(*caller), la BNDDIR non è importante tanto non chiama niente
001700160601     ** 3. Creare/Aggiornare il programma UBORMCNTR (CRTPGM / UPDPGM)
001800160601     **    con ACTGRP(*caller), la BNDDIR non è importante tanto non chiama niente
001900160202     ** 4. Cancellare il modulo.
002000160202     **
002100091228
002200091228       //--------------------------------------------------------------
002300121106       // Specifiche di controllo.
002400091228       //--------------------------------------------------------------
002500091223
002600091223     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002700160601     h alwnull(*inputonly)
002800091223
002900091223       //--------------------------------------------------------------
003000121106       // Dichiarazione file.
003100091223       //--------------------------------------------------------------
003200160218
003300160531
003400091223       //--------------------------------------------------------------
003500121106       // Definizione costanti.
003600091223       //--------------------------------------------------------------
003700100302
003800091223
003900091223       //--------------------------------------------------------------
004000121106       // Definizione schiere.
004100091223       //--------------------------------------------------------------
004200091223
004300091223
004400091223       //--------------------------------------------------------------
004500121106       // Definizione aree dati.
004600091223       //--------------------------------------------------------------
004700091223
004800091223
004900140724       //--------------------------------------------------------------
005000140724       // Definizione strutture dati.
005100140724       //--------------------------------------------------------------
005200140724
005300121106       // - Status
005400110516     d Psds           sds
005500110516     d   SDSpgm          *proc
005600121106     d   JobName             244    253                                         Job name
005700121106     d   JobUser             254    263                                         User name
005800121106     d   JobNumber           264    269s 0                                      Job number
005900091223
006000091223       //--------------------------------------------------------------
006100121106       // Definizione variabili.
006200091223       //--------------------------------------------------------------
006300091223
006400121106       // - Parametri ricevuti:
006500160531       // opz: 1=elabora  C=chiudi
006600160531     d pIn_Opz         s              1a
006700160531     d pIn_DS_FNVAO  e ds                  extname(FNVAOEIIR:FNVAO000)
006800160531     d                                     qualified
006900160608     d DS_FNVAO_save e ds                  extname(FNVAOEIIR:FNVAO000)
007000160531       // - Parametri in uscita:
007100160531     d pOut_Count      s              3s 0
007200160531       // esito: 1=OK  E=Err input  2=KO
007300160531     d pOut_Esito      s              1a
007400121106
007500121106       // Campi di comodo
007600160531     d wSQL            s           2048    inz  varying
007700160601     d wSQL_save       s                   like(wSQL)
007800160601     d wCount_save     s                   like(pOut_Count)
007900091223
008000091223       //--------------------------------------------------------------
008100160603       // Definizione costanti.
008200091223       //--------------------------------------------------------------
008300130307
008400160603     d wApice          c                   const('''')
008500160603     d wApice2         c                   const('''''')
008600160531
008700160603       //--------------------------------------------------------------
008800160603       // Definizione prototipi procedure.
008900160603       //--------------------------------------------------------------
009000160603
009100160603
009200091223       //--------------------------------------------------------------
009300121106       // Definizione key-list.
009400091223       //--------------------------------------------------------------
009500091223
009600091223
009700091223       //--------------------------------------------------------------
009800121107       // Definizione parametri procedura.
009900091223       //--------------------------------------------------------------
010000091223
010100091223     c     *Entry        plist
010200160217     c                   parm                    pIn_Opz
010300160531     c                   parm                    pIn_DS_FNVAO
010400160531     c                   parm                    pOut_Count
010500160531     c                   parm                    pOut_Esito
010600091223
010700130307      /free
010800091223
010900091223       //--------------------------------------------------------------
011000121106       // M A I N - L I N E
011100091223       //--------------------------------------------------------------
011200140806
011300130307       // Operazioni iniziali?
011400130307       exsr sr_RoutInz;
011500130307
011600140724       // attività richiesta dal chiamante se non chiamato per chiusura
011700160531       if pIn_Opz = '1';
011800160217         exsr esegui;
011900140724       endif;
012000091223
012100121106       // Operazioni finali?
012200091223       exsr sr_RoutEnd;
012300160601
012400160601       //--------------------------------------------------------------
012500160601       //
012600160601       //--------------------------------------------------------------
012700160601       BEGSR  *inzsr;
012800160601
012900160601         // Inizializzazioni parametri esecuzione comandi sql
013000160601         exec sql  set option  dynusrprf = *owner, closqlcsr = *endmod;
013100160601
013200160601       ENDSR;
013300091223
013400091223       //--------------------------------------------------------------
013500121107       // Operazioni iniziali.
013600091223       //--------------------------------------------------------------
013700091223       BEGSR  sr_RoutInz;
013800160601
013900160608         // Salvo i dati in ingresso (perché faccio una scanrpl per modificare gli apici
014000160608         DS_FNVAO_save = pIn_DS_FNVAO;
014100160608
014200160608         // Controllo i parametri di input
014300160608         exsr sr_CtrlPIn;
014400160219
014500160219         // se devo eseguire e non chiudere
014600160531         if pIn_Opz = '1';
014700160219           // Inizializzo risultato a errore
014800160531           pOut_Esito = '2';
014900160219         endif;
015000160202
015100160202       ENDSR;
015200160202
015300160202       //--------------------------------------------------------------
015400160202       // Controllo i parametri di input
015500160202       //--------------------------------------------------------------
015600160202       BEGSR  sr_CtrlPIn;
015700140724
015800160601         // valorizzo a 0 i campi alfa che vengono poi cercati come numerici se sono a rischio
015900160601         // di essere valorizzati a *blank
016000160603         if pIn_DS_FNVAO.vaopoe = *blank;
016100160603           pIn_DS_FNVAO.vaopoe = *all'0';
016200160603         endif;
016300160603         if pIn_DS_FNVAO.vaocrc = *blank;
016400160603           pIn_DS_FNVAO.vaocrc = *all'0';
016500160603         endif;
016600160603         if pIn_DS_FNVAO.vaopkg = *blank;
016700160603           pIn_DS_FNVAO.vaopkg = *all'0';
016800160603         endif;
016900160603         if pIn_DS_FNVAO.vaodao = *blank;
017000160603           pIn_DS_FNVAO.vaodao = *all'0';
017100160603         endif;
017200160603         if pIn_DS_FNVAO.vaooao = *blank;
017300160603           pIn_DS_FNVAO.vaooao = *all'0';
017400160603         endif;
017500160603         if pIn_DS_FNVAO.vaocor = *blank;
017600160603           pIn_DS_FNVAO.vaocor = *all'0';
017700160603         endif;
017800160603         if pIn_DS_FNVAO.vaocra = *blank;
017900160603           pIn_DS_FNVAO.vaocra = *all'0';
018000160603         endif;
018100160603         if pIn_DS_FNVAO.vaodar = *blank;
018200160603           pIn_DS_FNVAO.vaodar = *all'0';
018300160603         endif;
018400160603         if pIn_DS_FNVAO.vaoorr = *blank;
018500160603           pIn_DS_FNVAO.vaoorr = *all'0';
018600160603         endif;
018700160603         if pIn_DS_FNVAO.vaoncl = *blank;
018800160603           pIn_DS_FNVAO.vaoncl = *all'0';
018900160603         endif;
019000160601         if pIn_DS_FNVAO.vaoksc = *blank;
019100160601           pIn_DS_FNVAO.vaoksc = *all'0';
019200160601         endif;
019300160603
019400160603         // trasformo il possibile singolo apice (') in doppio per i campi
019500160603         // ragione sociale, indirizzo, località
019600160603         pIn_DS_FNVAO.vaorso = %scanrpl(wApice:wApice2:pIn_DS_FNVAO.vaorso);
019700160603         pIn_DS_FNVAO.vaoino = %scanrpl(wApice:wApice2:pIn_DS_FNVAO.vaoino);
019800160603         pIn_DS_FNVAO.vaoloo = %scanrpl(wApice:wApice2:pIn_DS_FNVAO.vaoloo);
019900160603         pIn_DS_FNVAO.vaorsr = %scanrpl(wApice:wApice2:pIn_DS_FNVAO.vaorsr);
020000160603         pIn_DS_FNVAO.vaoinr = %scanrpl(wApice:wApice2:pIn_DS_FNVAO.vaoinr);
020100160603         pIn_DS_FNVAO.vaolor = %scanrpl(wApice:wApice2:pIn_DS_FNVAO.vaolor);
020200160603         pIn_DS_FNVAO.vaorsc = %scanrpl(wApice:wApice2:pIn_DS_FNVAO.vaorsc);
020300160603         pIn_DS_FNVAO.vaoinc = %scanrpl(wApice:wApice2:pIn_DS_FNVAO.vaoinc);
020400160603         pIn_DS_FNVAO.vaoloc = %scanrpl(wApice:wApice2:pIn_DS_FNVAO.vaoloc);
020500091223
020600091223       ENDSR;
020700121106
020800091223       //--------------------------------------------------------------
020900160217       // Esecuzione
021000091223       //--------------------------------------------------------------
021100160217       BEGSR  esegui;
021200160531
021300160607          // vaodao =' + %trim(pIn_DS_FNVAO.vaodao) + ' and +
021400160607          // vaooao =' + %trim(pIn_DS_FNVAO.vaooao) + ' and +
021500160607
021600160531         wSQL = 'select count(*) from tiori00f +
021700160531          where +
021800160531          vaopoe =' + %trim(pIn_DS_FNVAO.vaopoe) + ' and +
021900160601          vaotor =''' + pIn_DS_FNVAO.vaotor + ''' and +
022000160601          vaotco =''' + pIn_DS_FNVAO.vaotco + ''' and +
022100160607          timestamp( +
022200160607          date( +
022300160607           (substr(char(vaodao) , 1 , 4) concat ''-'' concat +
022400160607            substr(char(vaodao) , 5 , 2) concat ''-'' concat +
022500160607            substr(char(vaodao) , 7 , 2))), +
022600160607          time( +
022700160607           (substr(digits(vaooao) , 1 , 2) concat '':'' concat +
022800160607            substr(digits(vaooao) , 3 , 2) concat '':'' concat +
022900160607            substr(digits(vaooao) , 5 , 2)))) +
023000160607          between +
023100160607          timestamp( +
023200160607          date( +
023300160607           (substr(char(' + pIn_DS_FNVAO.vaodao + ') , 1 , 4) +
023400160607           concat ''-'' concat +
023500160607            substr(char(' + pIn_DS_FNVAO.vaodao + ') , 5 , 2) +
023600160607           concat ''-'' concat +
023700160607            substr(char(' + pIn_DS_FNVAO.vaodao + ') , 7 , 2))), +
023800160607          time( +
023900160607           (substr(digits(' + pIn_DS_FNVAO.vaooao + ') , 1 , 2) +
024000160607           concat '':'' concat +
024100160607            substr(digits(' + pIn_DS_FNVAO.vaooao + ') , 3 , 2) +
024200160607           concat '':'' concat +
024300160607            substr(digits(' + pIn_DS_FNVAO.vaooao + ') , 5 , 2)))) +
024400160607           - 30 second and +
024500160607          timestamp( +
024600160607          date( +
024700160607           (substr(char(' + pIn_DS_FNVAO.vaodao + ') , 1 , 4) +
024800160607           concat ''-'' concat +
024900160607            substr(char(' + pIn_DS_FNVAO.vaodao + ') , 5 , 2) +
025000160607           concat ''-'' concat +
025100160607            substr(char(' + pIn_DS_FNVAO.vaodao + ') , 7 , 2))), +
025200160607          time( +
025300160607           (substr(digits(' + pIn_DS_FNVAO.vaooao + ') , 1 , 2) +
025400160607           concat '':'' concat +
025500160607            substr(digits(' + pIn_DS_FNVAO.vaooao + ') , 3 , 2) +
025600160607           concat '':'' concat +
025700160607            substr(digits(' + pIn_DS_FNVAO.vaooao + ') , 5 , 2)))) +
025800160607           + 30 second and +
025900160601          vaocor =' + %trim(pIn_DS_FNVAO.vaocor) + ' and +
026000160601          vaorso =''' + pIn_DS_FNVAO.vaorso + ''' and +
026100160601          vaoino =''' + pIn_DS_FNVAO.vaoino + ''' and +
026200160601          vaocao =''' + pIn_DS_FNVAO.vaocao + ''' and +
026300160601          vaoloo =''' + pIn_DS_FNVAO.vaoloo + ''' and +
026400160601          vaopro =''' + pIn_DS_FNVAO.vaopro + ''' and +
026500160601          vaonao =''' + pIn_DS_FNVAO.vaonao + ''' and +
026600160601          vaocra =' + %trim(pIn_DS_FNVAO.vaocra) + ' and +
026700160601          vaorsr =''' + pIn_DS_FNVAO.vaorsr + ''' and +
026800160601          vaoinr =''' + pIn_DS_FNVAO.vaoinr + ''' and +
026900160601          vaocar =''' + pIn_DS_FNVAO.vaocar + ''' and +
027000160601          vaolor =''' + pIn_DS_FNVAO.vaolor + ''' and +
027100160601          vaoprr =''' + pIn_DS_FNVAO.vaoprr + ''' and +
027200160601          vaonar =''' + pIn_DS_FNVAO.vaonar + ''' and +
027300160601          vaorer =''' + pIn_DS_FNVAO.vaorer + ''' and +
027400160601          vaoter =''' + pIn_DS_FNVAO.vaoter + ''' and +
027500160601          vaocrc =' + %trim(pIn_DS_FNVAO.vaocrc) + ' and +
027600160601          vaorsc =''' + pIn_DS_FNVAO.vaorsc + ''' and +
027700160601          vaoinc =''' + pIn_DS_FNVAO.vaoinc + ''' and +
027800160601          vaocac =''' + pIn_DS_FNVAO.vaocac + ''' and +
027900160601          vaoloc =''' + pIn_DS_FNVAO.vaoloc + ''' and +
028000160601          vaoprc =''' + pIn_DS_FNVAO.vaoprc + ''' and +
028100160601          vaonac =''' + pIn_DS_FNVAO.vaonac + ''' and +
028200160601          vaoffd =''' + pIn_DS_FNVAO.vaoffd + ''' and +
028300160601          vaodar =' + %trim(pIn_DS_FNVAO.vaodar) + ' and +
028400160601          vaoorr =' + %trim(pIn_DS_FNVAO.vaoorr) + ' and +
028500160601          vaormp =''' + pIn_DS_FNVAO.vaormp + ''' and +
028600160601          vaoncl =' + %trim(pIn_DS_FNVAO.vaoncl) + ' and +
028700160601          vaopkg =' + %trim(pIn_DS_FNVAO.vaopkg) + ' and +
028800160601          vaopag =''' + pIn_DS_FNVAO.vaopag + ''' and +
028900160601          vaoksc =' + %trim(pIn_DS_FNVAO.vaoksc) +
029000160531          ' for read only';
029100140724
029200160601         // se la stringa SQL è uguale a quella della precedente chiamata
029300160601         // e già la precedente chiamata aveva dato un valore del conteggio > 0
029400160601         // questa darebbe senz'altro un valore del conteggio > 1, per cui è inutile farla
029500160601         if wSQL = wSQL_save and
029600160601            wCount_save > 0;
029700160601           pOut_Count = wCount_save;
029800160601           pOut_Esito = 'E';
029900160601           exsr sr_RoutEnd;
030000160601         endif;
030100160601
030200160531         // Dichiarazione cursore
030300160531         exec sql   prepare S0   from :wSQL;
030400160531         exec sql   declare C0   cursor   for S0;
030500160531         // Apro il cursore
030600160531         exec sql open C0;
030700160531         // Leggo i dati
030800160531         exec sql
030900160531             fetch next from C0 into :pOut_Count;
031000160531
031100160531         select;
031200160531          // nessun rcd trovato
031300160531          when sqlCode = 100;
031400160531            exec sql close C0;
031500160531            exsr sr_RoutEnd;
031600160531          // errore SQL
031700160531          when sqlCode < 0;
031800160531            exec sql close C0;
031900160531            exsr sr_RoutEnd;
032000160531         endsl;
032100160531
032200160531         // Quindi chiuso il cursore
032300160531         exec sql close C0;
032400160531
032500160531         // Se sono qui tutto OK
032600160531         pOut_Esito = '1';
032700160217
032800160217       ENDSR;
032900160601
033000160601       //--------------------------------------------------------------
033100160601       // Operazioni finali.
033200160601       //--------------------------------------------------------------
033300160601       BEGSR  sr_RoutEnd;
033400160608
033500160608         // Ripristino i dati in ingresso
033600160608         pIn_DS_FNVAO = DS_FNVAO_save;
033700160601
033800160601         // Chiusura pgm
033900160601         if pIn_Opz = 'C';
034000160601           *inlr = *on;
034100160601         else;
034200160601           // memorizzo i dati dell'SQL (se chiudessi in LR non avrebbe senso)
034300160601           wCount_save = pOut_Count;
034400160601           wSQL_save = wSQL;
034500160601
034600160601           *inrt = *on;
034700160601         endif;
034800160601         return;
034900160601
035000160601       ENDSR;
035100091223
035200091223      /end-free
