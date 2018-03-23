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
007900160609     D Dt20A30         s             20s 0
008000160609     D Dt20S30         s             20s 0
008100091223
008200091223       //--------------------------------------------------------------
008300160603       // Definizione costanti.
008400091223       //--------------------------------------------------------------
008500130307
008600160603     d wApice          c                   const('''')
008700160603     d wApice2         c                   const('''''')
008800160531
008900160603       //--------------------------------------------------------------
009000160603       // Definizione prototipi procedure.
009100160603       //--------------------------------------------------------------
009200160603
009300160603
009400091223       //--------------------------------------------------------------
009500121106       // Definizione key-list.
009600091223       //--------------------------------------------------------------
009700091223
009800091223
009900091223       //--------------------------------------------------------------
010000121107       // Definizione parametri procedura.
010100091223       //--------------------------------------------------------------
010200091223
010300091223     c     *Entry        plist
010400160217     c                   parm                    pIn_Opz
010500160531     c                   parm                    pIn_DS_FNVAO
010600160531     c                   parm                    pOut_Count
010700160531     c                   parm                    pOut_Esito
010800091223
010900130307      /free
011000091223
011100091223       //--------------------------------------------------------------
011200121106       // M A I N - L I N E
011300091223       //--------------------------------------------------------------
011400140806
011500130307       // Operazioni iniziali?
011600130307       exsr sr_RoutInz;
011700130307
011800140724       // attività richiesta dal chiamante se non chiamato per chiusura
011900160531       if pIn_Opz = '1';
012000160217         exsr esegui;
012100140724       endif;
012200091223
012300121106       // Operazioni finali?
012400091223       exsr sr_RoutEnd;
012500160601
012600160601       //--------------------------------------------------------------
012700160601       //
012800160601       //--------------------------------------------------------------
012900160601       BEGSR  *inzsr;
013000160601
013100160601         // Inizializzazioni parametri esecuzione comandi sql
013200160601         exec sql  set option  dynusrprf = *owner, closqlcsr = *endmod;
013300160601
013400160601       ENDSR;
013500091223
013600091223       //--------------------------------------------------------------
013700121107       // Operazioni iniziali.
013800091223       //--------------------------------------------------------------
013900091223       BEGSR  sr_RoutInz;
014000160601
014100160608         // Salvo i dati in ingresso (perché faccio una scanrpl per modificare gli apici
014200160608         DS_FNVAO_save = pIn_DS_FNVAO;
014300160608
014400160608         // Controllo i parametri di input
014500160608         exsr sr_CtrlPIn;
014600160219
014700160219         // se devo eseguire e non chiudere
014800160531         if pIn_Opz = '1';
014900160219           // Inizializzo risultato a errore
015000160531           pOut_Esito = '2';
015100160219         endif;
015200160202
015300160202       ENDSR;
015400160202
015500160202       //--------------------------------------------------------------
015600160202       // Controllo i parametri di input
015700160202       //--------------------------------------------------------------
015800160202       BEGSR  sr_CtrlPIn;
015900140724
016000160601         // valorizzo a 0 i campi alfa che vengono poi cercati come numerici se sono a rischio
016100160601         // di essere valorizzati a *blank
016200160603         if pIn_DS_FNVAO.vaopoe = *blank;
016300160603           pIn_DS_FNVAO.vaopoe = *all'0';
016400160603         endif;
016500160603         if pIn_DS_FNVAO.vaocrc = *blank;
016600160603           pIn_DS_FNVAO.vaocrc = *all'0';
016700160603         endif;
016800160603         if pIn_DS_FNVAO.vaopkg = *blank;
016900160603           pIn_DS_FNVAO.vaopkg = *all'0';
017000160603         endif;
017100160603         if pIn_DS_FNVAO.vaodao = *blank;
017200160603           pIn_DS_FNVAO.vaodao = *all'0';
017300160603         endif;
017400160603         if pIn_DS_FNVAO.vaooao = *blank;
017500160603           pIn_DS_FNVAO.vaooao = *all'0';
017600160603         endif;
017700160603         if pIn_DS_FNVAO.vaocor = *blank;
017800160603           pIn_DS_FNVAO.vaocor = *all'0';
017900160603         endif;
018000160603         if pIn_DS_FNVAO.vaocra = *blank;
018100160603           pIn_DS_FNVAO.vaocra = *all'0';
018200160603         endif;
018300160603         if pIn_DS_FNVAO.vaodar = *blank;
018400160603           pIn_DS_FNVAO.vaodar = *all'0';
018500160603         endif;
018600160603         if pIn_DS_FNVAO.vaoorr = *blank;
018700160603           pIn_DS_FNVAO.vaoorr = *all'0';
018800160603         endif;
018900160603         if pIn_DS_FNVAO.vaoncl = *blank;
019000160603           pIn_DS_FNVAO.vaoncl = *all'0';
019100160603         endif;
019200160601         if pIn_DS_FNVAO.vaoksc = *blank;
019300160601           pIn_DS_FNVAO.vaoksc = *all'0';
019400160601         endif;
019500160603
019600160603         // trasformo il possibile singolo apice (') in doppio per i campi
019700160603         // ragione sociale, indirizzo, località
019800160603         pIn_DS_FNVAO.vaorso = %scanrpl(wApice:wApice2:pIn_DS_FNVAO.vaorso);
019900160603         pIn_DS_FNVAO.vaoino = %scanrpl(wApice:wApice2:pIn_DS_FNVAO.vaoino);
020000160603         pIn_DS_FNVAO.vaoloo = %scanrpl(wApice:wApice2:pIn_DS_FNVAO.vaoloo);
020100160603         pIn_DS_FNVAO.vaorsr = %scanrpl(wApice:wApice2:pIn_DS_FNVAO.vaorsr);
020200160603         pIn_DS_FNVAO.vaoinr = %scanrpl(wApice:wApice2:pIn_DS_FNVAO.vaoinr);
020300160603         pIn_DS_FNVAO.vaolor = %scanrpl(wApice:wApice2:pIn_DS_FNVAO.vaolor);
020400160603         pIn_DS_FNVAO.vaorsc = %scanrpl(wApice:wApice2:pIn_DS_FNVAO.vaorsc);
020500160603         pIn_DS_FNVAO.vaoinc = %scanrpl(wApice:wApice2:pIn_DS_FNVAO.vaoinc);
020600160603         pIn_DS_FNVAO.vaoloc = %scanrpl(wApice:wApice2:pIn_DS_FNVAO.vaoloc);
020700091223
020800091223       ENDSR;
020900121106
021000091223       //--------------------------------------------------------------
021100160217       // Esecuzione
021200091223       //--------------------------------------------------------------
021300160217       BEGSR  esegui;
021400160531
021500160607          // vaodao =' + %trim(pIn_DS_FNVAO.vaodao) + ' and +
021600160609       Dt20A30 = %dec( %timestamp( %dec(
021700160609                     pIn_DS_FNVAO.vaodao +
021800160609                     pIn_DS_FNVAO.vaooao +
021900160609                               '000000'
022000160609                  : 20 : 0 )) + %seconds(30));
022100160609       Dt20S30 = %dec( %timestamp( %dec(
022200160609                     pIn_DS_FNVAO.vaodao +
022300160609                     pIn_DS_FNVAO.vaooao +
022400160609                               '000000'
022500160609                  : 20 : 0 )) - %seconds(30));
022600160607          // vaooao =' + %trim(pIn_DS_FNVAO.vaooao) + ' and +
022700160607
022800160531         wSQL = 'select count(*) from tiori00f +
022900160531          where +
023000160531          vaopoe =' + %trim(pIn_DS_FNVAO.vaopoe) + ' and +
023100160601          vaotor =''' + pIn_DS_FNVAO.vaotor + ''' and +
023200160601          vaotco =''' + pIn_DS_FNVAO.vaotco + ''' and +
023300160609          vaodao * 1000000000000 + vaooao * 1000000 between ' +
023400160609          %char(Dt20S30) + ' and ' + %char(Dt20A30) +
023500160610          ' and +
023600160610          vaocor =' + %trim(pIn_DS_FNVAO.vaocor) + ' and +
023700160601          vaorso =''' + pIn_DS_FNVAO.vaorso + ''' and +
023800160601          vaoino =''' + pIn_DS_FNVAO.vaoino + ''' and +
023900160601          vaocao =''' + pIn_DS_FNVAO.vaocao + ''' and +
024000160601          vaoloo =''' + pIn_DS_FNVAO.vaoloo + ''' and +
024100160601          vaopro =''' + pIn_DS_FNVAO.vaopro + ''' and +
024200160601          vaonao =''' + pIn_DS_FNVAO.vaonao + ''' and +
024300160601          vaocra =' + %trim(pIn_DS_FNVAO.vaocra) + ' and +
024400160601          vaorsr =''' + pIn_DS_FNVAO.vaorsr + ''' and +
024500160601          vaoinr =''' + pIn_DS_FNVAO.vaoinr + ''' and +
024600160601          vaocar =''' + pIn_DS_FNVAO.vaocar + ''' and +
024700160601          vaolor =''' + pIn_DS_FNVAO.vaolor + ''' and +
024800160601          vaoprr =''' + pIn_DS_FNVAO.vaoprr + ''' and +
024900160601          vaonar =''' + pIn_DS_FNVAO.vaonar + ''' and +
025000160601          vaorer =''' + pIn_DS_FNVAO.vaorer + ''' and +
025100160601          vaoter =''' + pIn_DS_FNVAO.vaoter + ''' and +
025200160601          vaocrc =' + %trim(pIn_DS_FNVAO.vaocrc) + ' and +
025300160601          vaorsc =''' + pIn_DS_FNVAO.vaorsc + ''' and +
025400160601          vaoinc =''' + pIn_DS_FNVAO.vaoinc + ''' and +
025500160601          vaocac =''' + pIn_DS_FNVAO.vaocac + ''' and +
025600160601          vaoloc =''' + pIn_DS_FNVAO.vaoloc + ''' and +
025700160601          vaoprc =''' + pIn_DS_FNVAO.vaoprc + ''' and +
025800160601          vaonac =''' + pIn_DS_FNVAO.vaonac + ''' and +
025900160601          vaoffd =''' + pIn_DS_FNVAO.vaoffd + ''' and +
026000160601          vaodar =' + %trim(pIn_DS_FNVAO.vaodar) + ' and +
026100160601          vaoorr =' + %trim(pIn_DS_FNVAO.vaoorr) + ' and +
026200160601          vaormp =''' + pIn_DS_FNVAO.vaormp + ''' and +
026300160601          vaoncl =' + %trim(pIn_DS_FNVAO.vaoncl) + ' and +
026400160601          vaopkg =' + %trim(pIn_DS_FNVAO.vaopkg) + ' and +
026500160601          vaopag =''' + pIn_DS_FNVAO.vaopag + ''' and +
026600160601          vaoksc =' + %trim(pIn_DS_FNVAO.vaoksc) +
026700160531          ' for read only';
026800140724
026900160601         // se la stringa SQL è uguale a quella della precedente chiamata
027000160601         // e già la precedente chiamata aveva dato un valore del conteggio > 0
027100160601         // questa darebbe senz'altro un valore del conteggio > 1, per cui è inutile farla
027200160601         if wSQL = wSQL_save and
027300160601            wCount_save > 0;
027400160601           pOut_Count = wCount_save;
027500160601           pOut_Esito = 'E';
027600160601           exsr sr_RoutEnd;
027700160601         endif;
027800160601
027900160531         // Dichiarazione cursore
028000160531         exec sql   prepare S0   from :wSQL;
028100160531         exec sql   declare C0   cursor   for S0;
028200160531         // Apro il cursore
028300160531         exec sql open C0;
028400160531         // Leggo i dati
028500160531         exec sql
028600160531             fetch next from C0 into :pOut_Count;
028700160531
028800160531         select;
028900160531          // nessun rcd trovato
029000160531          when sqlCode = 100;
029100160531            exec sql close C0;
029200160531            exsr sr_RoutEnd;
029300160531          // errore SQL
029400160531          when sqlCode < 0;
029500160531            exec sql close C0;
029600160531            exsr sr_RoutEnd;
029700160531         endsl;
029800160531
029900160531         // Quindi chiuso il cursore
030000160531         exec sql close C0;
030100160531
030200160531         // Se sono qui tutto OK
030300160531         pOut_Esito = '1';
030400160217
030500160217       ENDSR;
030600160601
030700160601       //--------------------------------------------------------------
030800160601       // Operazioni finali.
030900160601       //--------------------------------------------------------------
031000160601       BEGSR  sr_RoutEnd;
031100160608
031200160608         // Ripristino i dati in ingresso
031300160608         pIn_DS_FNVAO = DS_FNVAO_save;
031400160601
031500160601         // Chiusura pgm
031600160601         if pIn_Opz = 'C';
031700160601           *inlr = *on;
031800160601         else;
031900160601           // memorizzo i dati dell'SQL (se chiudessi in LR non avrebbe senso)
032000160601           wCount_save = pOut_Count;
032100160601           wSQL_save = wSQL;
032200160601
032300160601           *inrt = *on;
032400160601         endif;
032500160601         return;
032600160601
032700160601       ENDSR;
032800091223
032900091223      /end-free
