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
004300161219     d                                     and MALSTS not in (''2'',''0'')'
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
006200161130     d  Server                       10
006300161104     d  NrEmail                       5s 0
006400161108     d  Avg_Diff_Sec                 12s 3
006500121218
006600161103       // - Status?
006700121217     d Psds           sds
006800121217     d   SDSpgm          *proc
006900121217     d*//JobName             244    253                                         Job name
007000161103     d*//JobUser             254    263
007100121217     d*//JobNumber           264    269s 0                                      Job number
007200121217
007300121217       //--------------------------------------------------------------
007400161103       // Definizione variabili.                                       ?
007500121217       //--------------------------------------------------------------
007600121217
007700161103       // - Campi di comodo?
007800161104     d fineDati        s               n
007900161107       // Periodo DA
008000161107     d MACHKTSDa       s               z
008100161107       // Periodo AL
008200161107     d MACHKTSAl       s               z
008300121218
008400161103       // - Stringa SQL da eseguire?
008500161014     d wSQL            s           2048    inz  varying
008600161111     d wSQL_D          s           2048    inz  varying
008700121217
008800121217       //--------------------------------------------------------------
008900161103       // Definizione prototipi procedure.                             ?
009000121217       //--------------------------------------------------------------
009100121217
009200161104       /copy gaitrasrc/srcProtoPI,UBMACHK1R
009300121217
009400121217       //--------------------------------------------------------------
009500161103       // Definizione key-list.                                        ?
009600121217       //--------------------------------------------------------------
009700161104
009800121217
009900121217       //--------------------------------------------------------------
010000161103       // Definizione parametri procedura.                             ?
010100121217       //--------------------------------------------------------------
010200121217
010300161103
010400121217       //--------------------------------------------------------------
010500121217       //?M A I N - L I N E                                            ?
010600121217       //--------------------------------------------------------------
010700121217
010800161014       // Operazioni iniziali?
010900121217       exsr sr_RoutInz;
011000161111
011100161111       // Scrittura TIMAC
011200161111       exsr  sr_WriteTIMAC;
011300121217
011400161103       // - Operazioni finali?
011500121217       exsr sr_RoutEnd;
011600121217
011700121217       //--------------------------------------------------------------
011800161103       // Operazioni iniziali.                                         ?
011900121217       //--------------------------------------------------------------
012000121217       BEGSR  sr_RoutInz;
012100121217
012200121217         exec sql  set option  dynusrprf = *owner,
012300121217                               closqlcsr = *endmod;
012400121217
012500161103         // - Controllo parametri d'ingresso
012600161103         exsr  sr_CtrlParms;
012700161111
012800161111         // Pulizia TIMAC
012900161111         exsr  sr_DeleteTIMAC;
013000161103
013100161103         // - Preparazione stringa SQL?
013200161103         exsr  sr_PrepSQL;
013300121217
013400161103         // - Apertura cursore SQL?
013500121217         exsr  sr_OpenCursor;
013600130208
013700140925       ENDSR;
013800141028
013900141028       //--------------------------------------------------------------
014000161103       // Controllo parametri
014100141028       //--------------------------------------------------------------
014200161103       BEGSR  sr_CtrlParms;
014300161014
014400161109         // Se Periodo DA vuoto, allora la data di partenza limite (03/11/2016) alle 00
014500161109         // prima del 03/11/2016, la data in MALMSG ha delle posizioni diverse e quando è
014600161109         // corretta (da 12/10/2016 14:10:45) comunque ci sono stati errori negli invii di posta
014700161107         MACHKTSDa = iMACHKTSDa;
014800161104         if iMACHKTSDa = *loval;
014900161109           MACHKTSDa = %timestamp(%date('2016-11-03'));
015000161104         endif;
015100161104
015200161104         // Se Periodo AL vuoto, allora imposto oggi a quest'ora
015300161107         MACHKTSAl = iMACHKTSAl;
015400161104         if iMACHKTSAl = *loval;
015500161107           MACHKTSAl = %timestamp();
015600161104         endif;
015700161104
015800161109         // Periodo DA non può essere inferiore alla data di partenza limite (03/11/2016)
015900161109         if %date(MACHKTSDa) < d'2016-11-03';
016000161107           oMACHKEsito = 'I';
016100161103           exsr sr_RoutEnd;
016200161103         endif;
016300161103
016400161103         // Periodo AL deve essere >= a DA
016500161107         if MACHKTSAl < MACHKTSDa;
016600161103           oMACHKEsito = 'I';
016700161103           exsr sr_RoutEnd;
016800161103         endif;
016900161111
017000161111         // Opzione al momento può essere o blank o D
017100161111         if iMACHKOpz <> *blank and
017200161111            iMACHKOpz <> 'D';
017300161111           oMACHKEsito = 'I';
017400161111           exsr sr_RoutEnd;
017500161111         endif;
017600161103
017700161103       ENDSR;
017800161103
017900161103       //--------------------------------------------------------------
018000161103       // Preparazione stringa SQL                                     ?
018100161103       //--------------------------------------------------------------
018200161103       BEGSR  sr_PrepSQL;
018300161103
018400161103         // SQL che ricava la differenza in millisecondi tra la data di inserimento (MALDAOR) e
018500161103         // la data di passaggio email al server di posta
018600161109
018700161103         wSQL ='with +
018800161109                malridotto as ( +
018900161109                SELECT +
019000161109                MALDAOR, MALMSG +
019100161109                FROM timal00f +
019200161109                WHERE timestamp(MALDAOR) between';
019300161109         wSQL = %trim(wSQL) +
019400161109                ' ''' + %subst(%char(MACHKTSDa):1:23) +
019500161109                ''' and ''' +
019600161109                %subst(%char(MACHKTSAl):1:23) + '''';
019700161109         wSQL = %trim(wSQL) + ' and ' +
019800161109                cWhere +
019900161109               ' ), +
020000161109                maljoin as (';
020100161109         wSQL = %trim(wSQL) +
020200161109               'SELECT * from malridotto +
020300161109                exception join timac00f on +
020400161109                date(substr(maldaor, 1 , 10)) = macdtchk and +
020500161109                hour(timestamp(maldaor))= hour(mactmchk) and +
020600161130                substr(malmsg , 5 , 3)= macserver and +
020700161109                macsto1 = ''S'' +
020800161109                 ), +
020900161109                base as (';
021000161108         // reperisco i rcd con la data di invio valorizzata
021100161103         wSQL = %trim(wSQL) +
021200161103               'SELECT +
021300161103                substr(char(maldaor) , 1 , 13) as fascia, +
021400161130                substr(MALMSG , 5 , 3) as server, +
021500161104                char( +
021600161104                timestamp(substr(MALMSG , 9 , 14)) +
021700161104                - timestamp(MALDAOR) +
021800161104                ), +
021900161104                hour( +
022000161104                timestamp(substr(MALMSG , 9 , 14)) +
022100161104                - timestamp(MALDAOR) +
022200161104                ) * 3600 +
022300161104                + minute( +
022400161104                timestamp(substr(MALMSG , 9 , 14)) +
022500161104                - timestamp(MALDAOR) +
022600161104                ) * 60 +
022700161104                + second( +
022800161104                timestamp(substr(MALMSG , 9 , 14)) +
022900161104                - timestamp(MALDAOR) +
023000161104                ) +
023100161104                // ore, minuti e secondi sono tutti valori interi, mentre microsecondi va diviso
023200161104                // e si ottiene un valore con decimali, decimali che defivisco con scalr function
023300161104                // dec
023400161104                + dec(microsecond( +
023500161104                timestamp(substr(MALMSG , 9 , 14)) +
023600161104                - timestamp(MALDAOR) +
023700161104                ) , 13 , 6) / 1000000 +
023800161104                as seconds +
023900161109                FROM maljoin +
024000161109                WHERE +
024100161109                 substr(MALMSG , 1 , 3)=''SBM'' +
024200161109                and substr(MALMSG , 9 , 2)=''20''';
024300161109         // aggiungo i rcd con la data di invio ancora non valorizzata
024400161108         wSQL = %trim(wSQL) +
024500161108               ' UNION ALL +
024600161108                SELECT +
024700161108                substr(char(maldaor) , 1 , 13) as fascia, +
024800161130                '' '' as server, +
024900161114                // la differenza in char non mi serve
025000161114                '' '', +
025100161114                // quando in MALMSG non c'è ancora la data di invio, la differenza la calcolo
025200161114                // in base alla data/ora attuale, ma se questa supera il tempo di cristallizzazione
025300161114                // (4 h = 3600 sec X 4 = 14400 sec.) mi fermo a questo tempo.
025400161114                // per farlo controllo se il timestamp risultante tra adesso meno (-) MALDAOR ha
025500161114                // almeno un anno, o un mese, o un giorno di differenza, oppre se questi sono =, se
025600161114                // ci sono almeno 5 ore di differenza, oppre se ci sono 4 ore di differenza, se c'è
025700161114                // almeno un minuto, o un secondo, o un microdecondo di differenza
025800161114                case +
025900161114                when +
026000161114                year( +
026100161114                current_timestamp +
026200161114                - timestamp(MALDAOR) +
026300161114                ) > 0 +
026400161114                then +
026500161114                4 * 3600 +
026600161114                when +
026700161114                month( +
026800161114                current_timestamp +
026900161114                - timestamp(MALDAOR) +
027000161114                ) > 0 +
027100161114                then +
027200161114                4 * 3600 +
027300161114                when +
027400161114                day( +
027500161114                current_timestamp +
027600161114                - timestamp(MALDAOR) +
027700161114                ) > 0 +
027800161114                then +
027900161114                4 * 3600 +
028000161114                when +
028100161114                hour( +
028200161114                current_timestamp +
028300161114                - timestamp(MALDAOR) +
028400161114                ) > 4 +
028500161114                then +
028600161114                4 * 3600 +
028700161114                when +
028800161114                hour(current_timestamp - timestamp(MALDAOR)) = 4 +
028900161114                and ( +
029000161114                minute(current_timestamp - timestamp(MALDAOR)) > 0 or +
029100161114                second(current_timestamp - timestamp(MALDAOR)) > 0 or +
029200161114                microsecond(current_timestamp - timestamp(MALDAOR)) > 0 ) +
029300161114                then +
029400161114                4 * 3600 +
029500161114                else +
029600161114                hour( +
029700161114                current_timestamp +
029800161114                - timestamp(MALDAOR) +
029900161114                ) * 3600 +
030000161114                + minute( +
030100161114                current_timestamp +
030200161114                - timestamp(MALDAOR) +
030300161114                ) * 60 +
030400161114                + second( +
030500161114                current_timestamp +
030600161114                - timestamp(MALDAOR) +
030700161114                ) +
030800161114                // ore, minuti e secondi sono tutti valori interi, mentre microsecondi va diviso
030900161114                // e si ottiene un valore con decimali, decimali che defivisco con scalr function
031000161114                // dec
031100161114                + dec(microsecond( +
031200161114                current_timestamp +
031300161114                - timestamp(MALDAOR) +
031400161114                ) , 13 , 6) / 1000000 +
031500161114                end +
031600161114                as seconds +
031700161111                FROM maljoin +
031800161109                WHERE +
031900161109                substr(MALMSG , 1 , 3)<>''SBM'' +
032000161114                 )';
032100161108
032200161108         // creo i dati raggruppati
032300161103         wSQL = %trim(wSQL) +
032400161104               ' SELECT +
032500161103                fascia , +
032600161130                server, +
032700161104                count(*) , +
032800161104         // sommo le differenze in sec. calcolate
032900161108         //     sum(seconds) , +
033000161104         // calcolo la media delle differenze in sec. calcolate
033100161104                avg(seconds) +
033200161103                from base +
033300161130                group by fascia, server';
033400161014
033500141020       ENDSR;
033600121217
033700121217       //--------------------------------------------------------------
033800161103       // Apertura cursore.                                            ?
033900121217       //--------------------------------------------------------------
034000121217       BEGSR  sr_OpenCursor;
034100121217
034200161103         // Dichiarazione cursore
034300161103         exec sql   prepare S0   from :wSQL;
034400161103         exec sql   declare C0   cursor   for S0;
034500161103         // Apertura del cursore
034600161103         exec sql   open C0;
034700121217
034800121217       ENDSR;
034900121217
035000121217       //--------------------------------------------------------------
035100161111       // Pulizia TIMAC nel periodo da aggiornare                       ?
035200121217       //--------------------------------------------------------------
035300161111       BEGSR  sr_DeleteTIMAC;
035400121217
035500161111         // se l'opzione prevede la pulizia prima di TIMAC00F nel periodo
035600161111         if iMACHKOpz = 'D';
035700161111           wSQL_D =
035800161111               'DELETE +
035900161111                FROM timac00f +
036000161111                WHERE +
036100161111                macdtchk between +
036200161111                date(''' + %char(machktsda) + ''') and +
036300161111                date(''' + %char(machktsal) + ''')';
036400161111           exec sql   execute immediate :wSQL_D;
036500161111         endif;
036600161111
036700161111       ENDSR;
036800161111
036900161111       //--------------------------------------------------------------
037000161111       // Aggiornamento TIMAC                                          ?
037100161111       //--------------------------------------------------------------
037200161111       BEGSR  sr_WriteTIMAC;
037300161111
037400161104         fineDati = *off;
037500161104
037600161108         // cicla finché non finiscono i dati
037700161108         dou fineDati = *on;
037800161103           // Lettura cursore
037900161103           exec sql   fetch next   from C0   into :DS_Res;
038000161103
038100161103           select;
038200161103            // nessun rcd trovato
038300161103            when sqlCode = 100;
038400161103             // ho finito i dati, esco dal ciclo
038500161104             fineDati = *on;
038600161103             exec sql close C0;
038700161103            // errore SQL
038800161103            when sqlCode < 0;
038900161103             exec sql close C0;
039000161103             // esco con errore SQL
039100161103             oMACHKEsito = '2';
039200161103             exsr sr_RoutEnd;
039300161103            // c'è il rcd, allora scrivo TIMAC00F
039400161103            other;
039500161108             exsr sr_UpdateTIMAC;
039600161103           endsl;
039700121217
039800161103         enddo;
039900161110
040000161110         // se sono qui, vuol dire che è anmdato tutto bene
040100161110         oMACHKEsito = '1';
040200161108
040300161108         // - Chiusura cursore SQL?
040400161108         exsr  sr_CloseCursor;
040500161103
040600121217       ENDSR;
040700121217
040800121217       //--------------------------------------------------------------
040900161104       // Chiusura cursore.                                            ?
041000121217       //--------------------------------------------------------------
041100121217       BEGSR  sr_CloseCursor;
041200121217
041300161103         exec sql close C0;
041400121217
041500121217       ENDSR;
041600161108
041700161108       //--------------------------------------------------------------
041800161108       // aggiorna TIMAC00F
041900161108       //--------------------------------------------------------------
042000161108       BEGSR  sr_UpdateTIMAC;
042100161108
042200161108         // provo a vedere se la fascia calcolata esiste già su TIMAC00F
042300161108         // in questo caso aggiorno
042400161130         exec sql update TIMAC00F
042500161130                  set MACCnt1 = :NrEmail ,
042600161130                      MACDif1 = :Avg_Diff_Sec
042700161108                  where MACDtChk = date(substr(:Fascia , 1 , 10)) and
042800161108                        MACTmChk = time(substr(:Fascia , 12 , 2) concat
042900161130                                        '.00.00') and
043000161130                        MACServer= :Server;
043100161130         select;
043200161108          // nessun rcd trovato
043300161108          when sqlCode = 100;
043400161108         // in questo caso scrivo
043500161108           exsr sr_InsertTIMAC;
043600161108          // errore SQL
043700161108          when sqlCode < 0;
043800161108           exec sql close C0;
043900161108           // esco con errore SQL
044000161108           oMACHKEsito = '2';
044100161108           exsr sr_RoutEnd;
044200161108           // in tutti gli altri casi andava bene l'update
044300161108         endsl;
044400161108
044500161108       ENDSR;
044600161108
044700161108       //--------------------------------------------------------------
044800161108       // scrive TIMAC00F
044900161108       //--------------------------------------------------------------
045000161108       BEGSR  sr_InsertTIMAC;
045100161108
045200161108           exec sql insert into TIMAC00F
045300161130                    (MACDtChk, MACTmChk, MACServer, MACCnt1, MACDif1)
045400161108                    values(date(substr(:Fascia , 1 , 10)) ,
045500161108                           time(substr(:Fascia , 12 , 2) concat  '.00.00') ,
045600161130                           :Server, :NrEmail, :Avg_Diff_Sec);
045700161108         select;
045800161108          // nessun rcd trovato
045900161108          when sqlCode = 100;
046000161108          // errore SQL
046100161108          when sqlCode < 0;
046200161108           exec sql close C0;
046300161108           // esco con errore SQL
046400161108           oMACHKEsito = '2';
046500161108           exsr sr_RoutEnd;
046600161108           // in tutti gli altri casi andava bene l'insert
046700161108         endsl;
046800161108
046900161108       ENDSR;
047000121217
047100121217       //--------------------------------------------------------------
047200161104       // Operazioni finali.                                           ?
047300121217       //--------------------------------------------------------------
047400121217       BEGSR  sr_RoutEnd;
047500161103
047600161103         // - Chiusura pgm?
047700121217         return;
047800121217
047900121217       ENDSR;
048000121217
