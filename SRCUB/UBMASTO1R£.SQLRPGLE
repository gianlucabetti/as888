000100121217       //==============================================================
000200161103       // Valorizzazione file TIMAC00F per Check date
000300121217       //==============================================================
000400141008
000500121217       //--------------------------------------------------------------
000600161103       // Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700121217       //--------------------------------------------------------------
000800121217
000900121217     /*PRM dbgview(*source)
001000121217     /*PRM commit(*none)
001100121217     /*END
001200161108     **
001300161108     ** ISTRUZIONI PER LA COMPILAZIONE
001400161108     **
001500161108     ** 1. Creare il modulo UBMACHK1R (Opz 15 PDM)
001600161108     ** 2. Creare/Aggiornare il programma di servizio UBMACHK1R (CRTSRVPGM / UPDSRVPGM)
001700161108     **    con ACTGRP(*caller)
001800161108     **    aggiungere nella BNDDIR(UBBNDDIR)
001900161108     ** 3. Visto chi userà questo servizio, non serve l'oggetto programma
002000161108     ** 4. Cancellare il modulo.
002100161108     **
002200121217
002300121217       //--------------------------------------------------------------
002400161103       // Specifiche di controllo.                                     ?
002500121217       //--------------------------------------------------------------
002600121217
002700121217     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002800161103     h alwnull(*inputonly)
002900161104
003000141029     h expropts(*resdecpos)
003100121217
003200121217       //--------------------------------------------------------------
003300161103       // Dichiarazione file.                                          ?
003400121217       //--------------------------------------------------------------
003500121217
003600121217       //--------------------------------------------------------------
003700161103       // Definizione costanti.                                        ?
003800121217       //--------------------------------------------------------------
003900121217
004000161103       // parte fissa della where SQL
004100161103     d cWhere          c                   'substr(MALDUEX, 31, 121)<>'' '' +
004200161103     d                                     and MALATT=MALTOTATT +
004300161109     d                                     and MALSTS <> ''2'''
004400121217
004500121217       //--------------------------------------------------------------
004600161103       // Definizione schiere.                                         ?
004700121217       //--------------------------------------------------------------
004800121217
004900121217
005000121217       //--------------------------------------------------------------
005100161103       // Definizione aree dati.                                       ?
005200121217       //--------------------------------------------------------------
005300121217
005400121217
005500121217       //--------------------------------------------------------------
005600161103       // Definizione strutture dati.                                  ?
005700121217       //--------------------------------------------------------------
005800121217
005900161103       // - Risultati fetch
006000161103     d DS_res          ds
006100161103     d  Fascia                       13
006200161104     d  NrEmail                       5s 0
006300161108     d  Avg_Diff_Sec                 12s 3
006400121218
006500161103       // - Status?
006600121217     d Psds           sds
006700121217     d   SDSpgm          *proc
006800121217     d*//JobName             244    253                                         Job name
006900161103     d*//JobUser             254    263
007000121217     d*//JobNumber           264    269s 0                                      Job number
007100121217
007200121217       //--------------------------------------------------------------
007300161103       // Definizione variabili.                                       ?
007400121217       //--------------------------------------------------------------
007500121217
007600161103       // - Campi di comodo?
007700161104     d fineDati        s               n
007800161107       // Periodo DA
007900161107     d MACHKTSDa       s               z
008000161107       // Periodo AL
008100161107     d MACHKTSAl       s               z
008200121218
008300161103       // - Stringa SQL da eseguire?
008400161014     d wSQL            s           2048    inz  varying
008500121217
008600121217       //--------------------------------------------------------------
008700161103       // Definizione prototipi procedure.                             ?
008800121217       //--------------------------------------------------------------
008900121217
009000161104       /copy gaitrasrc/srcProtoPI,UBMACHK1R
009100121217
009200121217       //--------------------------------------------------------------
009300161103       // Definizione key-list.                                        ?
009400121217       //--------------------------------------------------------------
009500161104
009600121217
009700121217       //--------------------------------------------------------------
009800161103       // Definizione parametri procedura.                             ?
009900121217       //--------------------------------------------------------------
010000121217
010100161103
010200121217       //--------------------------------------------------------------
010300121217       //?M A I N - L I N E                                            ?
010400121217       //--------------------------------------------------------------
010500121217
010600161014       // Operazioni iniziali?
010700121217       exsr sr_RoutInz;
010800121217
010900161014       // Esecuzione pareggio
011000161103       exsr  sr_WriteTIMAC;
011100121217
011200161103       // - Operazioni finali?
011300121217       exsr sr_RoutEnd;
011400121217
011500121217       //--------------------------------------------------------------
011600161103       // Operazioni iniziali.                                         ?
011700121217       //--------------------------------------------------------------
011800121217       BEGSR  sr_RoutInz;
011900121217
012000121217         exec sql  set option  dynusrprf = *owner,
012100121217                               closqlcsr = *endmod;
012200121217
012300161103         // - Controllo parametri d'ingresso
012400161103         exsr  sr_CtrlParms;
012500161103
012600161103         // - Preparazione stringa SQL?
012700161103         exsr  sr_PrepSQL;
012800121217
012900161103         // - Apertura cursore SQL?
013000121217         exsr  sr_OpenCursor;
013100130208
013200140925       ENDSR;
013300141028
013400141028       //--------------------------------------------------------------
013500161103       // Controllo parametri
013600141028       //--------------------------------------------------------------
013700161103       BEGSR  sr_CtrlParms;
013800161014
013900161109         // Se Periodo DA vuoto, allora la data di partenza limite (03/11/2016) alle 00
014000161109         // prima del 03/11/2016, la data in MALMSG ha delle posizioni diverse e quando è
014100161109         // corretta (da 12/10/2016 14:10:45) comunque ci sono stati errori negli invii di posta
014200161107         MACHKTSDa = iMACHKTSDa;
014300161104         if iMACHKTSDa = *loval;
014400161109           MACHKTSDa = %timestamp(%date('2016-11-03'));
014500161104         endif;
014600161104
014700161104         // Se Periodo AL vuoto, allora imposto oggi a quest'ora
014800161107         MACHKTSAl = iMACHKTSAl;
014900161104         if iMACHKTSAl = *loval;
015000161107           MACHKTSAl = %timestamp();
015100161104         endif;
015200161104
015300161109         // Periodo DA non può essere inferiore alla data di partenza limite (03/11/2016)
015400161109         if %date(MACHKTSDa) < d'2016-11-03';
015500161107           oMACHKEsito = 'I';
015600161103           exsr sr_RoutEnd;
015700161103         endif;
015800161103
015900161103         // Periodo AL deve essere >= a DA
016000161107         if MACHKTSAl < MACHKTSDa;
016100161103           oMACHKEsito = 'I';
016200161103           exsr sr_RoutEnd;
016300161103         endif;
016400161103
016500161103       ENDSR;
016600161103
016700161103       //--------------------------------------------------------------
016800161103       // Preparazione stringa SQL                                     ?
016900161103       //--------------------------------------------------------------
017000161103       BEGSR  sr_PrepSQL;
017100161103
017200161103         // SQL che ricava la differenza in millisecondi tra la data di inserimento (MALDAOR) e
017300161103         // la data di passaggio email al server di posta
017400161109
017500161103         wSQL ='with +
017600161109                malridotto as ( +
017700161109                SELECT +
017800161109                MALDAOR, MALMSG +
017900161109                FROM timal00f +
018000161109                WHERE timestamp(MALDAOR) between';
018100161109         wSQL = %trim(wSQL) +
018200161109                ' ''' + %subst(%char(MACHKTSDa):1:23) +
018300161109                ''' and ''' +
018400161109                %subst(%char(MACHKTSAl):1:23) + '''';
018500161109         wSQL = %trim(wSQL) + ' and ' +
018600161109                cWhere +
018700161109               ' ), +
018800161109                maljoin as (';
018900161109         wSQL = %trim(wSQL) +
019000161109               'SELECT * from malridotto +
019100161109                exception join timac00f on +
019200161109                date(substr(maldaor, 1 , 10)) = macdtchk and +
019300161109                hour(timestamp(maldaor))= hour(mactmchk) and +
019400161109                macsto1 = ''S'' +
019500161109                 ), +
019600161109                base as (';
019700161108         // reperisco i rcd con la data di invio valorizzata
019800161103         wSQL = %trim(wSQL) +
019900161103               'SELECT +
020000161103                substr(char(maldaor) , 1 , 13) as fascia, +
020100161104                char( +
020200161104                timestamp(substr(MALMSG , 9 , 14)) +
020300161104                - timestamp(MALDAOR) +
020400161104                ), +
020500161104                hour( +
020600161104                timestamp(substr(MALMSG , 9 , 14)) +
020700161104                - timestamp(MALDAOR) +
020800161104                ) * 3600 +
020900161104                + minute( +
021000161104                timestamp(substr(MALMSG , 9 , 14)) +
021100161104                - timestamp(MALDAOR) +
021200161104                ) * 60 +
021300161104                + second( +
021400161104                timestamp(substr(MALMSG , 9 , 14)) +
021500161104                - timestamp(MALDAOR) +
021600161104                ) +
021700161104                // ore, minuti e secondi sono tutti valori interi, mentre microsecondi va diviso
021800161104                // e si ottiene un valore con decimali, decimali che defivisco con scalr function
021900161104                // dec
022000161104                + dec(microsecond( +
022100161104                timestamp(substr(MALMSG , 9 , 14)) +
022200161104                - timestamp(MALDAOR) +
022300161104                ) , 13 , 6) / 1000000 +
022400161104                as seconds +
022500161109                FROM maljoin +
022600161109                WHERE +
022700161109                 substr(MALMSG , 1 , 3)=''SBM'' +
022800161109                and substr(MALMSG , 9 , 2)=''20''';
022900161109         // aggiungo i rcd con la data di invio ancora non valorizzata
023000161108         wSQL = %trim(wSQL) +
023100161108               ' UNION ALL +
023200161108                SELECT +
023300161108                substr(char(maldaor) , 1 , 13) as fascia, +
023400161109                // quando la seconda data non è valorizzata dò come valore medio di differenza
023500161109                // quello di cristallizzazione, e cioè 4 ore
023600161109                // quindi la differenza in char non mi serve
023700161109                '' '', +
023800161109                4 * 3600 +
023900161108                as seconds +
024000161109                FROM malridotto +
024100161109                WHERE +
024200161109                substr(MALMSG , 1 , 3)<>''SBM'' +
024300161108                 )';
024400161108
024500161108         // creo i dati raggruppati
024600161103         wSQL = %trim(wSQL) +
024700161104               ' SELECT +
024800161103                fascia , +
024900161104                count(*) , +
025000161104         // sommo le differenze in sec. calcolate
025100161108         //     sum(seconds) , +
025200161104         // calcolo la media delle differenze in sec. calcolate
025300161104                avg(seconds) +
025400161103                from base +
025500161103                group by fascia';
025600161014
025700141020       ENDSR;
025800121217
025900121217       //--------------------------------------------------------------
026000161103       // Apertura cursore.                                            ?
026100121217       //--------------------------------------------------------------
026200121217       BEGSR  sr_OpenCursor;
026300121217
026400161103         // Dichiarazione cursore
026500161103         exec sql   prepare S0   from :wSQL;
026600161103         exec sql   declare C0   cursor   for S0;
026700161103         // Apertura del cursore
026800161103         exec sql   open C0;
026900121217
027000121217       ENDSR;
027100121217
027200121217       //--------------------------------------------------------------
027300161103       // Lettura cursore.                                             ?
027400121217       //--------------------------------------------------------------
027500161103       BEGSR  sr_WriteTIMAC;
027600121217
027700161104         fineDati = *off;
027800161104
027900161108         // cicla finché non finiscono i dati
028000161108         dou fineDati = *on;
028100161103           // Lettura cursore
028200161103           exec sql   fetch next   from C0   into :DS_Res;
028300161103
028400161103           select;
028500161103            // nessun rcd trovato
028600161103            when sqlCode = 100;
028700161103             // ho finito i dati, esco dal ciclo
028800161104             fineDati = *on;
028900161103             exec sql close C0;
029000161103            // errore SQL
029100161103            when sqlCode < 0;
029200161103             exec sql close C0;
029300161103             // esco con errore SQL
029400161103             oMACHKEsito = '2';
029500161103             exsr sr_RoutEnd;
029600161103            // c'è il rcd, allora scrivo TIMAC00F
029700161103            other;
029800161108             exsr sr_UpdateTIMAC;
029900161103           endsl;
030000121217
030100161103         enddo;
030200161108
030300161108         // - Chiusura cursore SQL?
030400161108         exsr  sr_CloseCursor;
030500161103
030600121217       ENDSR;
030700121217
030800121217       //--------------------------------------------------------------
030900161104       // Chiusura cursore.                                            ?
031000121217       //--------------------------------------------------------------
031100121217       BEGSR  sr_CloseCursor;
031200121217
031300161103         exec sql close C0;
031400121217
031500121217       ENDSR;
031600161108
031700161108       //--------------------------------------------------------------
031800161108       // aggiorna TIMAC00F
031900161108       //--------------------------------------------------------------
032000161108       BEGSR  sr_UpdateTIMAC;
032100161108
032200161108         // provo a vedere se la fascia calcolata esiste già su TIMAC00F
032300161108         // in questo caso aggiorno
032400161108         exec sql update TIMAC00F
032500161108                  set MACCnt1 = :NrEmail ,
032600161108                      MACDif1 = :Avg_Diff_Sec
032700161108                  where MACDtChk = date(substr(:Fascia , 1 , 10)) and
032800161108                        MACTmChk = time(substr(:Fascia , 12 , 2) concat
032900161108                                        '.00.00');
033000161108         select;
033100161108          // nessun rcd trovato
033200161108          when sqlCode = 100;
033300161108         // in questo caso scrivo
033400161108           exsr sr_InsertTIMAC;
033500161108          // errore SQL
033600161108          when sqlCode < 0;
033700161108           exec sql close C0;
033800161108           // esco con errore SQL
033900161108           oMACHKEsito = '2';
034000161108           exsr sr_RoutEnd;
034100161108           // in tutti gli altri casi andava bene l'update
034200161108         endsl;
034300161108
034400161108       ENDSR;
034500161108
034600161108       //--------------------------------------------------------------
034700161108       // scrive TIMAC00F
034800161108       //--------------------------------------------------------------
034900161108       BEGSR  sr_InsertTIMAC;
035000161108
035100161108           exec sql insert into TIMAC00F
035200161108                    (MACDtChk, MACTmChk, MACCnt1, MACDif1)
035300161108                    values(date(substr(:Fascia , 1 , 10)) ,
035400161108                           time(substr(:Fascia , 12 , 2) concat  '.00.00') ,
035500161108                           :NrEmail, :Avg_Diff_Sec);
035600161108         select;
035700161108          // nessun rcd trovato
035800161108          when sqlCode = 100;
035900161108          // errore SQL
036000161108          when sqlCode < 0;
036100161108           exec sql close C0;
036200161108           // esco con errore SQL
036300161108           oMACHKEsito = '2';
036400161108           exsr sr_RoutEnd;
036500161108           // in tutti gli altri casi andava bene l'insert
036600161108         endsl;
036700161108
036800161108       ENDSR;
036900121217
037000121217       //--------------------------------------------------------------
037100161104       // Operazioni finali.                                           ?
037200121217       //--------------------------------------------------------------
037300121217       BEGSR  sr_RoutEnd;
037400161103
037500161103         // - Chiusura pgm?
037600121217         return;
037700121217
037800121217       ENDSR;
037900121217
