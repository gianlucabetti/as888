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
007700160610     d wSQL_1          s           2048    inz  varying
007800160601     d wSQL_save       s                   like(wSQL)
007900160601     d wCount_save     s                   like(pOut_Count)
008000160610     D Dt20A30         s             20s 0
008100160610     D Dt20S30         s             20s 0
008200160610     D wDAOA           s              8a
008300160610     D wDAOS           s              8a
008400160610     D wOAOA_Da        s              6a
008500160610     D wOAOA_A         s              6a
008600160610     D wOAOS_Da        s              6a
008700160610     D wOAOS_A         s              6a
008800091223
008900091223       //--------------------------------------------------------------
009000160603       // Definizione costanti.
009100091223       //--------------------------------------------------------------
009200130307
009300160603     d wApice          c                   const('''')
009400160603     d wApice2         c                   const('''''')
009500160531
009600160603       //--------------------------------------------------------------
009700160603       // Definizione prototipi procedure.
009800160603       //--------------------------------------------------------------
009900160603
010000160603
010100091223       //--------------------------------------------------------------
010200121106       // Definizione key-list.
010300091223       //--------------------------------------------------------------
010400091223
010500091223
010600091223       //--------------------------------------------------------------
010700121107       // Definizione parametri procedura.
010800091223       //--------------------------------------------------------------
010900091223
011000091223     c     *Entry        plist
011100160217     c                   parm                    pIn_Opz
011200160531     c                   parm                    pIn_DS_FNVAO
011300160531     c                   parm                    pOut_Count
011400160531     c                   parm                    pOut_Esito
011500091223
011600130307      /free
011700091223
011800091223       //--------------------------------------------------------------
011900121106       // M A I N - L I N E
012000091223       //--------------------------------------------------------------
012100140806
012200130307       // Operazioni iniziali?
012300130307       exsr sr_RoutInz;
012400130307
012500140724       // attività richiesta dal chiamante se non chiamato per chiusura
012600160531       if pIn_Opz = '1';
012700160217         exsr esegui;
012800140724       endif;
012900091223
013000121106       // Operazioni finali?
013100091223       exsr sr_RoutEnd;
013200160601
013300160601       //--------------------------------------------------------------
013400160601       //
013500160601       //--------------------------------------------------------------
013600160601       BEGSR  *inzsr;
013700160601
013800160601         // Inizializzazioni parametri esecuzione comandi sql
013900160601         exec sql  set option  dynusrprf = *owner, closqlcsr = *endmod;
014000160601
014100160601       ENDSR;
014200091223
014300091223       //--------------------------------------------------------------
014400121107       // Operazioni iniziali.
014500091223       //--------------------------------------------------------------
014600091223       BEGSR  sr_RoutInz;
014700160601
014800160608         // Salvo i dati in ingresso (perché faccio una scanrpl per modificare gli apici
014900160608         DS_FNVAO_save = pIn_DS_FNVAO;
015000160608
015100160608         // Controllo i parametri di input
015200160608         exsr sr_CtrlPIn;
015300160219
015400160219         // se devo eseguire e non chiudere
015500160531         if pIn_Opz = '1';
015600160219           // Inizializzo risultato a errore
015700160531           pOut_Esito = '2';
015800160219         endif;
015900160202
016000160202       ENDSR;
016100160202
016200160202       //--------------------------------------------------------------
016300160202       // Controllo i parametri di input
016400160202       //--------------------------------------------------------------
016500160202       BEGSR  sr_CtrlPIn;
016600140724
016700160601         // valorizzo a 0 i campi alfa che vengono poi cercati come numerici se sono a rischio
016800160601         // di essere valorizzati a *blank
016900160603         if pIn_DS_FNVAO.vaopoe = *blank;
017000160603           pIn_DS_FNVAO.vaopoe = *all'0';
017100160603         endif;
017200160603         if pIn_DS_FNVAO.vaocrc = *blank;
017300160603           pIn_DS_FNVAO.vaocrc = *all'0';
017400160603         endif;
017500160603         if pIn_DS_FNVAO.vaopkg = *blank;
017600160603           pIn_DS_FNVAO.vaopkg = *all'0';
017700160603         endif;
017800160603         if pIn_DS_FNVAO.vaodao = *blank;
017900160603           pIn_DS_FNVAO.vaodao = *all'0';
018000160603         endif;
018100160603         if pIn_DS_FNVAO.vaooao = *blank;
018200160603           pIn_DS_FNVAO.vaooao = *all'0';
018300160603         endif;
018400160603         if pIn_DS_FNVAO.vaocor = *blank;
018500160603           pIn_DS_FNVAO.vaocor = *all'0';
018600160603         endif;
018700160603         if pIn_DS_FNVAO.vaocra = *blank;
018800160603           pIn_DS_FNVAO.vaocra = *all'0';
018900160603         endif;
019000160603         if pIn_DS_FNVAO.vaodar = *blank;
019100160603           pIn_DS_FNVAO.vaodar = *all'0';
019200160603         endif;
019300160603         if pIn_DS_FNVAO.vaoorr = *blank;
019400160603           pIn_DS_FNVAO.vaoorr = *all'0';
019500160603         endif;
019600160603         if pIn_DS_FNVAO.vaoncl = *blank;
019700160603           pIn_DS_FNVAO.vaoncl = *all'0';
019800160603         endif;
019900160601         if pIn_DS_FNVAO.vaoksc = *blank;
020000160601           pIn_DS_FNVAO.vaoksc = *all'0';
020100160601         endif;
020200160603
020300160603         // trasformo il possibile singolo apice (') in doppio per i campi
020400160603         // ragione sociale, indirizzo, località
020500160603         pIn_DS_FNVAO.vaorso = %scanrpl(wApice:wApice2:pIn_DS_FNVAO.vaorso);
020600160603         pIn_DS_FNVAO.vaoino = %scanrpl(wApice:wApice2:pIn_DS_FNVAO.vaoino);
020700160603         pIn_DS_FNVAO.vaoloo = %scanrpl(wApice:wApice2:pIn_DS_FNVAO.vaoloo);
020800160603         pIn_DS_FNVAO.vaorsr = %scanrpl(wApice:wApice2:pIn_DS_FNVAO.vaorsr);
020900160603         pIn_DS_FNVAO.vaoinr = %scanrpl(wApice:wApice2:pIn_DS_FNVAO.vaoinr);
021000160603         pIn_DS_FNVAO.vaolor = %scanrpl(wApice:wApice2:pIn_DS_FNVAO.vaolor);
021100160603         pIn_DS_FNVAO.vaorsc = %scanrpl(wApice:wApice2:pIn_DS_FNVAO.vaorsc);
021200160603         pIn_DS_FNVAO.vaoinc = %scanrpl(wApice:wApice2:pIn_DS_FNVAO.vaoinc);
021300160603         pIn_DS_FNVAO.vaoloc = %scanrpl(wApice:wApice2:pIn_DS_FNVAO.vaoloc);
021400091223
021500091223       ENDSR;
021600121106
021700091223       //--------------------------------------------------------------
021800160217       // Esecuzione
021900091223       //--------------------------------------------------------------
022000160217       BEGSR  esegui;
022100160531
022200160607          // vaodao =' + %trim(pIn_DS_FNVAO.vaodao) + ' and +
022300160610       Dt20A30 = %dec( %timestamp( %dec(
022400160609                     pIn_DS_FNVAO.vaodao +
022500160609                     pIn_DS_FNVAO.vaooao +
022600160609                               '000000'
022700160609                  : 20 : 0 )) + %seconds(30));
022800160610       Dt20S30 = %dec( %timestamp( %dec(
022900160609                     pIn_DS_FNVAO.vaodao +
023000160609                     pIn_DS_FNVAO.vaooao +
023100160609                               '000000'
023200160609                  : 20 : 0 )) - %seconds(30));
023300160607          // vaooao =' + %trim(pIn_DS_FNVAO.vaooao) + ' and +
023400160610        wDAOA = %subst(%char(Dt20A30) : 1 : 8);
023500160610        wDAOS = %subst(%char(Dt20S30) : 1 : 8);
023600160610        // se le due date risultano diverse, allora o la somma o la sottrazione hanno fatto
023700160610        // cambiare il giorno
023800160610        if wDAOA <> wDAOS;
023900160610          // la between sull'ora rispetto alla data inferiore va
024000160610          // dall'ora trovata con - alle 23:59:59
024100160610          // la between sull'ora rispetto alla data superiore va
024200160610          // dalle 00:00:00 all'ora trovata con +
024300160610          wOAOS_Da = %subst(%char(Dt20S30) : 9 : 6);
024400160610          wOAOS_A = '235959';
024500160610          wOAOA_Da= '000000';
024600160610          wOAOA_A  = %subst(%char(Dt20A30) : 9 : 6);
024700160610        wSQL_1 =
024800160610        '(vaodao =' + wDAOS + ' and +
024900160610          vaooao between ' + wOAOS_Da + ' and ' + wOAOS_A +
025000160610          ' OR vaodao =' + wDAOA + ' and +
025100160610          vaooao between ' + wOAOA_Da + ' and ' + wOAOA_A +
025200160610          ')' ;
025300160610        else;
025400160610          // la between sull'ora (dato che la data è una sola) va
025500160610          // dall'ora trovata con -
025600160610          // all'ora trovata con +
025700160610          wOAOS_Da = %subst(%char(Dt20S30) : 9 : 6);
025800160610          wOAOA_A  = %subst(%char(Dt20A30) : 9 : 6);
025900160610        wSQL_1 =
026000160610        ' vaodao =' + wDAOS + ' and +
026100160610          vaooao between ' + wOAOS_Da +
026200160610          ' and ' + wOAOA_A ;
026300160610        endif;
026400160607
026500160531         wSQL = 'select count(*) from tiori00f +
026600160531          where +
026700160531          vaopoe =' + %trim(pIn_DS_FNVAO.vaopoe) + ' and +
026800160601          vaotor =''' + pIn_DS_FNVAO.vaotor + ''' and +
026900160610          vaotco =''' + pIn_DS_FNVAO.vaotco + ''' and ' +
027000160610          wSQL_1 +
027100160610          ' and +
027200160610          vaocor =' + %trim(pIn_DS_FNVAO.vaocor) + ' and +
027300160601          vaorso =''' + pIn_DS_FNVAO.vaorso + ''' and +
027400160601          vaoino =''' + pIn_DS_FNVAO.vaoino + ''' and +
027500160601          vaocao =''' + pIn_DS_FNVAO.vaocao + ''' and +
027600160601          vaoloo =''' + pIn_DS_FNVAO.vaoloo + ''' and +
027700160601          vaopro =''' + pIn_DS_FNVAO.vaopro + ''' and +
027800160601          vaonao =''' + pIn_DS_FNVAO.vaonao + ''' and +
027900160601          vaocra =' + %trim(pIn_DS_FNVAO.vaocra) + ' and +
028000160601          vaorsr =''' + pIn_DS_FNVAO.vaorsr + ''' and +
028100160601          vaoinr =''' + pIn_DS_FNVAO.vaoinr + ''' and +
028200160601          vaocar =''' + pIn_DS_FNVAO.vaocar + ''' and +
028300160601          vaolor =''' + pIn_DS_FNVAO.vaolor + ''' and +
028400160601          vaoprr =''' + pIn_DS_FNVAO.vaoprr + ''' and +
028500160601          vaonar =''' + pIn_DS_FNVAO.vaonar + ''' and +
028600160601          vaorer =''' + pIn_DS_FNVAO.vaorer + ''' and +
028700160601          vaoter =''' + pIn_DS_FNVAO.vaoter + ''' and +
028800160601          vaocrc =' + %trim(pIn_DS_FNVAO.vaocrc) + ' and +
028900160601          vaorsc =''' + pIn_DS_FNVAO.vaorsc + ''' and +
029000160601          vaoinc =''' + pIn_DS_FNVAO.vaoinc + ''' and +
029100160601          vaocac =''' + pIn_DS_FNVAO.vaocac + ''' and +
029200160601          vaoloc =''' + pIn_DS_FNVAO.vaoloc + ''' and +
029300160601          vaoprc =''' + pIn_DS_FNVAO.vaoprc + ''' and +
029400160601          vaonac =''' + pIn_DS_FNVAO.vaonac + ''' and +
029500160601          vaoffd =''' + pIn_DS_FNVAO.vaoffd + ''' and +
029600160601          vaodar =' + %trim(pIn_DS_FNVAO.vaodar) + ' and +
029700160601          vaoorr =' + %trim(pIn_DS_FNVAO.vaoorr) + ' and +
029800160601          vaormp =''' + pIn_DS_FNVAO.vaormp + ''' and +
029900160601          vaoncl =' + %trim(pIn_DS_FNVAO.vaoncl) + ' and +
030000160601          vaopkg =' + %trim(pIn_DS_FNVAO.vaopkg) + ' and +
030100160601          vaopag =''' + pIn_DS_FNVAO.vaopag + ''' and +
030200160601          vaoksc =' + %trim(pIn_DS_FNVAO.vaoksc) +
030300160531          ' for read only';
030400140724
030500160601         // se la stringa SQL è uguale a quella della precedente chiamata
030600160601         // e già la precedente chiamata aveva dato un valore del conteggio > 0
030700160601         // questa darebbe senz'altro un valore del conteggio > 1, per cui è inutile farla
030800160601         if wSQL = wSQL_save and
030900160601            wCount_save > 0;
031000160601           pOut_Count = wCount_save;
031100160601           pOut_Esito = 'E';
031200160601           exsr sr_RoutEnd;
031300160601         endif;
031400160601
031500160531         // Dichiarazione cursore
031600160531         exec sql   prepare S0   from :wSQL;
031700160531         exec sql   declare C0   cursor   for S0;
031800160531         // Apro il cursore
031900160531         exec sql open C0;
032000160531         // Leggo i dati
032100160531         exec sql
032200160531             fetch next from C0 into :pOut_Count;
032300160531
032400160531         select;
032500160531          // nessun rcd trovato
032600160531          when sqlCode = 100;
032700160531            exec sql close C0;
032800160531            exsr sr_RoutEnd;
032900160531          // errore SQL
033000160531          when sqlCode < 0;
033100160531            exec sql close C0;
033200160531            exsr sr_RoutEnd;
033300160531         endsl;
033400160531
033500160531         // Quindi chiuso il cursore
033600160531         exec sql close C0;
033700160531
033800160531         // Se sono qui tutto OK
033900160531         pOut_Esito = '1';
034000160217
034100160217       ENDSR;
034200160601
034300160601       //--------------------------------------------------------------
034400160601       // Operazioni finali.
034500160601       //--------------------------------------------------------------
034600160601       BEGSR  sr_RoutEnd;
034700160608
034800160608         // Ripristino i dati in ingresso
034900160608         pIn_DS_FNVAO = DS_FNVAO_save;
035000160601
035100160601         // Chiusura pgm
035200160601         if pIn_Opz = 'C';
035300160601           *inlr = *on;
035400160601         else;
035500160601           // memorizzo i dati dell'SQL (se chiudessi in LR non avrebbe senso)
035600160601           wCount_save = pOut_Count;
035700160601           wSQL_save = wSQL;
035800160601
035900160601           *inrt = *on;
036000160601         endif;
036100160601         return;
036200160601
036300160601       ENDSR;
036400091223
036500091223      /end-free
