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
008500161111     d wSQL_D          s           2048    inz  varying
008600121217
008700121217       //--------------------------------------------------------------
008800161103       // Definizione prototipi procedure.                             ?
008900121217       //--------------------------------------------------------------
009000121217
009100161104       /copy gaitrasrc/srcProtoPI,UBMACHK1R
009200121217
009300121217       //--------------------------------------------------------------
009400161103       // Definizione key-list.                                        ?
009500121217       //--------------------------------------------------------------
009600161104
009700121217
009800121217       //--------------------------------------------------------------
009900161103       // Definizione parametri procedura.                             ?
010000121217       //--------------------------------------------------------------
010100121217
010200161103
010300121217       //--------------------------------------------------------------
010400121217       //?M A I N - L I N E                                            ?
010500121217       //--------------------------------------------------------------
010600121217
010700161014       // Operazioni iniziali?
010800121217       exsr sr_RoutInz;
010900161111
011000161111       // Scrittura TIMAC
011100161111       exsr  sr_WriteTIMAC;
011200121217
011300161103       // - Operazioni finali?
011400121217       exsr sr_RoutEnd;
011500121217
011600121217       //--------------------------------------------------------------
011700161103       // Operazioni iniziali.                                         ?
011800121217       //--------------------------------------------------------------
011900121217       BEGSR  sr_RoutInz;
012000121217
012100121217         exec sql  set option  dynusrprf = *owner,
012200121217                               closqlcsr = *endmod;
012300121217
012400161103         // - Controllo parametri d'ingresso
012500161103         exsr  sr_CtrlParms;
012600161111
012700161111         // Pulizia TIMAC
012800161111         exsr  sr_DeleteTIMAC;
012900161103
013000161103         // - Preparazione stringa SQL?
013100161103         exsr  sr_PrepSQL;
013200121217
013300161103         // - Apertura cursore SQL?
013400121217         exsr  sr_OpenCursor;
013500130208
013600140925       ENDSR;
013700141028
013800141028       //--------------------------------------------------------------
013900161103       // Controllo parametri
014000141028       //--------------------------------------------------------------
014100161103       BEGSR  sr_CtrlParms;
014200161014
014300161109         // Se Periodo DA vuoto, allora la data di partenza limite (03/11/2016) alle 00
014400161109         // prima del 03/11/2016, la data in MALMSG ha delle posizioni diverse e quando è
014500161109         // corretta (da 12/10/2016 14:10:45) comunque ci sono stati errori negli invii di posta
014600161107         MACHKTSDa = iMACHKTSDa;
014700161104         if iMACHKTSDa = *loval;
014800161109           MACHKTSDa = %timestamp(%date('2016-11-03'));
014900161104         endif;
015000161104
015100161104         // Se Periodo AL vuoto, allora imposto oggi a quest'ora
015200161107         MACHKTSAl = iMACHKTSAl;
015300161104         if iMACHKTSAl = *loval;
015400161107           MACHKTSAl = %timestamp();
015500161104         endif;
015600161104
015700161109         // Periodo DA non può essere inferiore alla data di partenza limite (03/11/2016)
015800161109         if %date(MACHKTSDa) < d'2016-11-03';
015900161107           oMACHKEsito = 'I';
016000161103           exsr sr_RoutEnd;
016100161103         endif;
016200161103
016300161103         // Periodo AL deve essere >= a DA
016400161107         if MACHKTSAl < MACHKTSDa;
016500161103           oMACHKEsito = 'I';
016600161103           exsr sr_RoutEnd;
016700161103         endif;
016800161111
016900161111         // Opzione al momento può essere o blank o D
017000161111         if iMACHKOpz <> *blank and
017100161111            iMACHKOpz <> 'D';
017200161111           oMACHKEsito = 'I';
017300161111           exsr sr_RoutEnd;
017400161111         endif;
017500161103
017600161103       ENDSR;
017700161103
017800161103       //--------------------------------------------------------------
017900161103       // Preparazione stringa SQL                                     ?
018000161103       //--------------------------------------------------------------
018100161103       BEGSR  sr_PrepSQL;
018200161103
018300161103         // SQL che ricava la differenza in millisecondi tra la data di inserimento (MALDAOR) e
018400161103         // la data di passaggio email al server di posta
018500161109
018600161103         wSQL ='with +
018700161109                malridotto as ( +
018800161109                SELECT +
018900161109                MALDAOR, MALMSG +
019000161109                FROM timal00f +
019100161109                WHERE timestamp(MALDAOR) between';
019200161109         wSQL = %trim(wSQL) +
019300161109                ' ''' + %subst(%char(MACHKTSDa):1:23) +
019400161109                ''' and ''' +
019500161109                %subst(%char(MACHKTSAl):1:23) + '''';
019600161109         wSQL = %trim(wSQL) + ' and ' +
019700161109                cWhere +
019800161109               ' ), +
019900161109                maljoin as (';
020000161109         wSQL = %trim(wSQL) +
020100161109               'SELECT * from malridotto +
020200161109                exception join timac00f on +
020300161109                date(substr(maldaor, 1 , 10)) = macdtchk and +
020400161109                hour(timestamp(maldaor))= hour(mactmchk) and +
020500161109                macsto1 = ''S'' +
020600161109                 ), +
020700161109                base as (';
020800161108         // reperisco i rcd con la data di invio valorizzata
020900161103         wSQL = %trim(wSQL) +
021000161103               'SELECT +
021100161103                substr(char(maldaor) , 1 , 13) as fascia, +
021200161104                char( +
021300161104                timestamp(substr(MALMSG , 9 , 14)) +
021400161104                - timestamp(MALDAOR) +
021500161104                ), +
021600161104                hour( +
021700161104                timestamp(substr(MALMSG , 9 , 14)) +
021800161104                - timestamp(MALDAOR) +
021900161104                ) * 3600 +
022000161104                + minute( +
022100161104                timestamp(substr(MALMSG , 9 , 14)) +
022200161104                - timestamp(MALDAOR) +
022300161104                ) * 60 +
022400161104                + second( +
022500161104                timestamp(substr(MALMSG , 9 , 14)) +
022600161104                - timestamp(MALDAOR) +
022700161104                ) +
022800161104                // ore, minuti e secondi sono tutti valori interi, mentre microsecondi va diviso
022900161104                // e si ottiene un valore con decimali, decimali che defivisco con scalr function
023000161104                // dec
023100161104                + dec(microsecond( +
023200161104                timestamp(substr(MALMSG , 9 , 14)) +
023300161104                - timestamp(MALDAOR) +
023400161104                ) , 13 , 6) / 1000000 +
023500161104                as seconds +
023600161109                FROM maljoin +
023700161109                WHERE +
023800161109                 substr(MALMSG , 1 , 3)=''SBM'' +
023900161109                and substr(MALMSG , 9 , 2)=''20''';
024000161109         // aggiungo i rcd con la data di invio ancora non valorizzata
024100161108         wSQL = %trim(wSQL) +
024200161108               ' UNION ALL +
024300161108                SELECT +
024400161108                substr(char(maldaor) , 1 , 13) as fascia, +
024500161114                // la differenza in char non mi serve
024600161114                '' '', +
024700161114                // quando in MALMSG non c'è ancora la data di invio, la differenza la calcolo
024800161114                // in base alla data/ora attuale, ma se questa supera il tempo di cristallizzazione
024900161114                // (4 h = 3600 sec X 4 = 14400 sec.) mi fermo a questo tempo.
025000161114                // per farlo controllo se il timestamp risultante tra adesso meno (-) MALDAOR ha
025100161114                // almeno un anno, o un mese, o un giorno di differenza, oppre se questi sono =, se
025200161114                // ci sono almeno 5 ore di differenza, oppre se ci sono 4 ore di differenza, se c'è
025300161114                // almeno un minuto, o un secondo, o un microdecondo di differenza
025400161114                case +
025500161114                when +
025600161114                year( +
025700161114                current_timestamp +
025800161114                - timestamp(MALDAOR) +
025900161114                ) > 0 +
026000161114                then +
026100161114                4 * 3600 +
026200161114                when +
026300161114                month( +
026400161114                current_timestamp +
026500161114                - timestamp(MALDAOR) +
026600161114                ) > 0 +
026700161114                then +
026800161114                4 * 3600 +
026900161114                when +
027000161114                day( +
027100161114                current_timestamp +
027200161114                - timestamp(MALDAOR) +
027300161114                ) > 0 +
027400161114                then +
027500161114                4 * 3600 +
027600161114                when +
027700161114                hour( +
027800161114                current_timestamp +
027900161114                - timestamp(MALDAOR) +
028000161114                ) > 4 +
028100161114                then +
028200161114                4 * 3600 +
028300161114                when +
028400161114                hour(current_timestamp - timestamp(MALDAOR)) = 4 +
028500161114                and ( +
028600161114                minute(current_timestamp - timestamp(MALDAOR)) > 0 or +
028700161114                second(current_timestamp - timestamp(MALDAOR)) > 0 or +
028800161114                microsecond(current_timestamp - timestamp(MALDAOR)) > 0 ) +
028900161114                then +
029000161114                4 * 3600 +
029100161114                else +
029200161114                hour( +
029300161114                current_timestamp +
029400161114                - timestamp(MALDAOR) +
029500161114                ) * 3600 +
029600161114                + minute( +
029700161114                current_timestamp +
029800161114                - timestamp(MALDAOR) +
029900161114                ) * 60 +
030000161114                + second( +
030100161114                current_timestamp +
030200161114                - timestamp(MALDAOR) +
030300161114                ) +
030400161114                // ore, minuti e secondi sono tutti valori interi, mentre microsecondi va diviso
030500161114                // e si ottiene un valore con decimali, decimali che defivisco con scalr function
030600161114                // dec
030700161114                + dec(microsecond( +
030800161114                current_timestamp +
030900161114                - timestamp(MALDAOR) +
031000161114                ) , 13 , 6) / 1000000 +
031100161114                end +
031200161114                as seconds +
031300161111                FROM maljoin +
031400161109                WHERE +
031500161109                substr(MALMSG , 1 , 3)<>''SBM'' +
031600161114                 )';
031700161108
031800161108         // creo i dati raggruppati
031900161103         wSQL = %trim(wSQL) +
032000161104               ' SELECT +
032100161103                fascia , +
032200161104                count(*) , +
032300161104         // sommo le differenze in sec. calcolate
032400161108         //     sum(seconds) , +
032500161104         // calcolo la media delle differenze in sec. calcolate
032600161104                avg(seconds) +
032700161103                from base +
032800161103                group by fascia';
032900161014
033000141020       ENDSR;
033100121217
033200121217       //--------------------------------------------------------------
033300161103       // Apertura cursore.                                            ?
033400121217       //--------------------------------------------------------------
033500121217       BEGSR  sr_OpenCursor;
033600121217
033700161103         // Dichiarazione cursore
033800161103         exec sql   prepare S0   from :wSQL;
033900161103         exec sql   declare C0   cursor   for S0;
034000161103         // Apertura del cursore
034100161103         exec sql   open C0;
034200121217
034300121217       ENDSR;
034400121217
034500121217       //--------------------------------------------------------------
034600161111       // Pulizia TIMAC nel periodo da aggiornare                       ?
034700121217       //--------------------------------------------------------------
034800161111       BEGSR  sr_DeleteTIMAC;
034900121217
035000161111         // se l'opzione prevede la pulizia prima di TIMAC00F nel periodo
035100161111         if iMACHKOpz = 'D';
035200161111           wSQL_D =
035300161111               'DELETE +
035400161111                FROM timac00f +
035500161111                WHERE +
035600161111                macdtchk between +
035700161111                date(''' + %char(machktsda) + ''') and +
035800161111                date(''' + %char(machktsal) + ''')';
035900161111           exec sql   execute immediate :wSQL_D;
036000161111         endif;
036100161111
036200161111       ENDSR;
036300161111
036400161111       //--------------------------------------------------------------
036500161111       // Aggiornamento TIMAC                                          ?
036600161111       //--------------------------------------------------------------
036700161111       BEGSR  sr_WriteTIMAC;
036800161111
036900161104         fineDati = *off;
037000161104
037100161108         // cicla finché non finiscono i dati
037200161108         dou fineDati = *on;
037300161103           // Lettura cursore
037400161103           exec sql   fetch next   from C0   into :DS_Res;
037500161103
037600161103           select;
037700161103            // nessun rcd trovato
037800161103            when sqlCode = 100;
037900161103             // ho finito i dati, esco dal ciclo
038000161104             fineDati = *on;
038100161103             exec sql close C0;
038200161103            // errore SQL
038300161103            when sqlCode < 0;
038400161103             exec sql close C0;
038500161103             // esco con errore SQL
038600161103             oMACHKEsito = '2';
038700161103             exsr sr_RoutEnd;
038800161103            // c'è il rcd, allora scrivo TIMAC00F
038900161103            other;
039000161108             exsr sr_UpdateTIMAC;
039100161103           endsl;
039200121217
039300161103         enddo;
039400161110
039500161110         // se sono qui, vuol dire che è anmdato tutto bene
039600161110         oMACHKEsito = '1';
039700161108
039800161108         // - Chiusura cursore SQL?
039900161108         exsr  sr_CloseCursor;
040000161103
040100121217       ENDSR;
040200121217
040300121217       //--------------------------------------------------------------
040400161104       // Chiusura cursore.                                            ?
040500121217       //--------------------------------------------------------------
040600121217       BEGSR  sr_CloseCursor;
040700121217
040800161103         exec sql close C0;
040900121217
041000121217       ENDSR;
041100161108
041200161108       //--------------------------------------------------------------
041300161108       // aggiorna TIMAC00F
041400161108       //--------------------------------------------------------------
041500161108       BEGSR  sr_UpdateTIMAC;
041600161108
041700161108         // provo a vedere se la fascia calcolata esiste già su TIMAC00F
041800161108         // in questo caso aggiorno
041900161108         exec sql update TIMAC00F
042000161108                  set MACCnt1 = :NrEmail ,
042100161108                      MACDif1 = :Avg_Diff_Sec
042200161108                  where MACDtChk = date(substr(:Fascia , 1 , 10)) and
042300161108                        MACTmChk = time(substr(:Fascia , 12 , 2) concat
042400161108                                        '.00.00');
042500161108         select;
042600161108          // nessun rcd trovato
042700161108          when sqlCode = 100;
042800161108         // in questo caso scrivo
042900161108           exsr sr_InsertTIMAC;
043000161108          // errore SQL
043100161108          when sqlCode < 0;
043200161108           exec sql close C0;
043300161108           // esco con errore SQL
043400161108           oMACHKEsito = '2';
043500161108           exsr sr_RoutEnd;
043600161108           // in tutti gli altri casi andava bene l'update
043700161108         endsl;
043800161108
043900161108       ENDSR;
044000161108
044100161108       //--------------------------------------------------------------
044200161108       // scrive TIMAC00F
044300161108       //--------------------------------------------------------------
044400161108       BEGSR  sr_InsertTIMAC;
044500161108
044600161108           exec sql insert into TIMAC00F
044700161108                    (MACDtChk, MACTmChk, MACCnt1, MACDif1)
044800161108                    values(date(substr(:Fascia , 1 , 10)) ,
044900161108                           time(substr(:Fascia , 12 , 2) concat  '.00.00') ,
045000161108                           :NrEmail, :Avg_Diff_Sec);
045100161108         select;
045200161108          // nessun rcd trovato
045300161108          when sqlCode = 100;
045400161108          // errore SQL
045500161108          when sqlCode < 0;
045600161108           exec sql close C0;
045700161108           // esco con errore SQL
045800161108           oMACHKEsito = '2';
045900161108           exsr sr_RoutEnd;
046000161108           // in tutti gli altri casi andava bene l'insert
046100161108         endsl;
046200161108
046300161108       ENDSR;
046400121217
046500121217       //--------------------------------------------------------------
046600161104       // Operazioni finali.                                           ?
046700121217       //--------------------------------------------------------------
046800121217       BEGSR  sr_RoutEnd;
046900161103
047000161103         // - Chiusura pgm?
047100121217         return;
047200121217
047300121217       ENDSR;
047400121217
