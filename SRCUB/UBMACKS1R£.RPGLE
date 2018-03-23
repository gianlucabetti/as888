000100121217       //==============================================================
000200161110       // Wrapper per TIMAC00F - differenza e storico 1
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
001500161110     ** 1. Creare il pgm UBMACKS1R (Opz 14 PDM)
001600161108     **
001700121217
001800121217       //--------------------------------------------------------------
001900161103       // Specifiche di controllo.                                     ?
002000121217       //--------------------------------------------------------------
002100121217
002200121217     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002300161110     h dftactgrp(*no) actgrp('QILE')
002400161110     h bnddir('UBBNDDIR')
002500121217
002600121217       //--------------------------------------------------------------
002700161103       // Dichiarazione file.                                          ?
002800121217       //--------------------------------------------------------------
002900121217
003000121217       //--------------------------------------------------------------
003100161103       // Definizione costanti.                                        ?
003200121217       //--------------------------------------------------------------
003300121217
003400121217
003500121217       //--------------------------------------------------------------
003600161103       // Definizione schiere.                                         ?
003700121217       //--------------------------------------------------------------
003800121217
003900121217
004000121217       //--------------------------------------------------------------
004100161103       // Definizione aree dati.                                       ?
004200121217       //--------------------------------------------------------------
004300121217
004400121217
004500121217       //--------------------------------------------------------------
004600161103       // Definizione strutture dati.                                  ?
004700121217       //--------------------------------------------------------------
004800121217
004900161110       // - Architettura
005000161110     d kpjba         e ds
005100121218
005200161103       // - Status?
005300121217     d Psds           sds
005400121217     d   SDSpgm          *proc
005500121217     d*//JobName             244    253                                         Job name
005600161103     d*//JobUser             254    263
005700121217     d*//JobNumber           264    269s 0                                      Job number
005800121217
005900121217       //--------------------------------------------------------------
006000161103       // Definizione variabili.                                       ?
006100121217       //--------------------------------------------------------------
006200121217
006300161107       // Periodo DA
006400161107     d MACHKTSDa       s               z
006500161107       // Periodo AL
006600161107     d MACHKTSAl       s               z
006700121218
006800121217       //--------------------------------------------------------------
006900161103       // Definizione prototipi procedure.                             ?
007000121217       //--------------------------------------------------------------
007100121217
007200161110       /copy gaitrasrc/srcProtoPR,UBMACHK1R
007300161110       /copy gaitrasrc/srcProtoPR,UBMASTO1R
007400121217
007500121217       //--------------------------------------------------------------
007600161103       // Definizione key-list.                                        ?
007700121217       //--------------------------------------------------------------
007800161104
007900121217
008000121217       //--------------------------------------------------------------
008100161103       // Definizione parametri procedura.                             ?
008200121217       //--------------------------------------------------------------
008300121217
008400161103
008500121217       //--------------------------------------------------------------
008600121217       //?M A I N - L I N E                                            ?
008700121217       //--------------------------------------------------------------
008800121217
008900161014       // Operazioni iniziali?
009000121217       exsr sr_RoutInz;
009100121217
009200161110       // Esecuzione
009300161110       exsr  sr_Esegui;
009400121217
009500161103       // - Operazioni finali?
009600121217       exsr sr_RoutEnd;
009700121217
009800121217       //--------------------------------------------------------------
009900161103       // Operazioni iniziali.                                         ?
010000121217       //--------------------------------------------------------------
010100121217       BEGSR  sr_RoutInz;
010200121217
010300161103         // - Controllo parametri d'ingresso
010400161103         exsr  sr_CtrlParms;
010500161103
010600161103         // - Preparazione stringa SQL?
010700161103         exsr  sr_PrepSQL;
010800121217
010900161103         // - Apertura cursore SQL?
011000121217         exsr  sr_OpenCursor;
011100130208
011200140925       ENDSR;
011300141028
011400141028       //--------------------------------------------------------------
011500161103       // Controllo parametri
011600141028       //--------------------------------------------------------------
011700161103       BEGSR  sr_CtrlParms;
011800161014
011900161109         // Se Periodo DA vuoto, allora la data di partenza limite (03/11/2016) alle 00
012000161109         // prima del 03/11/2016, la data in MALMSG ha delle posizioni diverse e quando è
012100161109         // corretta (da 12/10/2016 14:10:45) comunque ci sono stati errori negli invii di posta
012200161107         MACHKTSDa = iMACHKTSDa;
012300161104         if iMACHKTSDa = *loval;
012400161109           MACHKTSDa = %timestamp(%date('2016-11-03'));
012500161104         endif;
012600161104
012700161104         // Se Periodo AL vuoto, allora imposto oggi a quest'ora
012800161107         MACHKTSAl = iMACHKTSAl;
012900161104         if iMACHKTSAl = *loval;
013000161107           MACHKTSAl = %timestamp();
013100161104         endif;
013200161104
013300161109         // Periodo DA non può essere inferiore alla data di partenza limite (03/11/2016)
013400161109         if %date(MACHKTSDa) < d'2016-11-03';
013500161107           oMACHKEsito = 'I';
013600161103           exsr sr_RoutEnd;
013700161103         endif;
013800161103
013900161103         // Periodo AL deve essere >= a DA
014000161107         if MACHKTSAl < MACHKTSDa;
014100161103           oMACHKEsito = 'I';
014200161103           exsr sr_RoutEnd;
014300161103         endif;
014400161103
014500161103       ENDSR;
014600161103
014700161103       //--------------------------------------------------------------
014800161103       // Preparazione stringa SQL                                     ?
014900161103       //--------------------------------------------------------------
015000161103       BEGSR  sr_PrepSQL;
015100161103
015200161103         // SQL che ricava la differenza in millisecondi tra la data di inserimento (MALDAOR) e
015300161103         // la data di passaggio email al server di posta
015400161109
015500161103         wSQL ='with +
015600161109                malridotto as ( +
015700161109                SELECT +
015800161109                MALDAOR, MALMSG +
015900161109                FROM timal00f +
016000161109                WHERE timestamp(MALDAOR) between';
016100161109         wSQL = %trim(wSQL) +
016200161109                ' ''' + %subst(%char(MACHKTSDa):1:23) +
016300161109                ''' and ''' +
016400161109                %subst(%char(MACHKTSAl):1:23) + '''';
016500161109         wSQL = %trim(wSQL) + ' and ' +
016600161109                cWhere +
016700161109               ' ), +
016800161109                maljoin as (';
016900161109         wSQL = %trim(wSQL) +
017000161109               'SELECT * from malridotto +
017100161109                exception join timac00f on +
017200161109                date(substr(maldaor, 1 , 10)) = macdtchk and +
017300161109                hour(timestamp(maldaor))= hour(mactmchk) and +
017400161109                macsto1 = ''S'' +
017500161109                 ), +
017600161109                base as (';
017700161108         // reperisco i rcd con la data di invio valorizzata
017800161103         wSQL = %trim(wSQL) +
017900161103               'SELECT +
018000161103                substr(char(maldaor) , 1 , 13) as fascia, +
018100161104                char( +
018200161104                timestamp(substr(MALMSG , 9 , 14)) +
018300161104                - timestamp(MALDAOR) +
018400161104                ), +
018500161104                hour( +
018600161104                timestamp(substr(MALMSG , 9 , 14)) +
018700161104                - timestamp(MALDAOR) +
018800161104                ) * 3600 +
018900161104                + minute( +
019000161104                timestamp(substr(MALMSG , 9 , 14)) +
019100161104                - timestamp(MALDAOR) +
019200161104                ) * 60 +
019300161104                + second( +
019400161104                timestamp(substr(MALMSG , 9 , 14)) +
019500161104                - timestamp(MALDAOR) +
019600161104                ) +
019700161104                // ore, minuti e secondi sono tutti valori interi, mentre microsecondi va diviso
019800161104                // e si ottiene un valore con decimali, decimali che defivisco con scalr function
019900161104                // dec
020000161104                + dec(microsecond( +
020100161104                timestamp(substr(MALMSG , 9 , 14)) +
020200161104                - timestamp(MALDAOR) +
020300161104                ) , 13 , 6) / 1000000 +
020400161104                as seconds +
020500161109                FROM maljoin +
020600161109                WHERE +
020700161109                 substr(MALMSG , 1 , 3)=''SBM'' +
020800161109                and substr(MALMSG , 9 , 2)=''20''';
020900161109         // aggiungo i rcd con la data di invio ancora non valorizzata
021000161108         wSQL = %trim(wSQL) +
021100161108               ' UNION ALL +
021200161108                SELECT +
021300161108                substr(char(maldaor) , 1 , 13) as fascia, +
021400161109                // quando la seconda data non è valorizzata dò come valore medio di differenza
021500161109                // quello di cristallizzazione, e cioè 4 ore
021600161109                // quindi la differenza in char non mi serve
021700161109                '' '', +
021800161109                4 * 3600 +
021900161108                as seconds +
022000161109                FROM malridotto +
022100161109                WHERE +
022200161109                substr(MALMSG , 1 , 3)<>''SBM'' +
022300161108                 )';
022400161108
022500161108         // creo i dati raggruppati
022600161103         wSQL = %trim(wSQL) +
022700161104               ' SELECT +
022800161103                fascia , +
022900161104                count(*) , +
023000161104         // sommo le differenze in sec. calcolate
023100161108         //     sum(seconds) , +
023200161104         // calcolo la media delle differenze in sec. calcolate
023300161104                avg(seconds) +
023400161103                from base +
023500161103                group by fascia';
023600161014
023700141020       ENDSR;
023800121217
023900121217       //--------------------------------------------------------------
024000161103       // Apertura cursore.                                            ?
024100121217       //--------------------------------------------------------------
024200121217       BEGSR  sr_OpenCursor;
024300121217
024400161103         // Dichiarazione cursore
024500161103         exec sql   prepare S0   from :wSQL;
024600161103         exec sql   declare C0   cursor   for S0;
024700161103         // Apertura del cursore
024800161103         exec sql   open C0;
024900121217
025000121217       ENDSR;
025100121217
025200121217       //--------------------------------------------------------------
025300161103       // Lettura cursore.                                             ?
025400121217       //--------------------------------------------------------------
025500161103       BEGSR  sr_WriteTIMAC;
025600121217
025700161104         fineDati = *off;
025800161104
025900161108         // cicla finché non finiscono i dati
026000161108         dou fineDati = *on;
026100161103           // Lettura cursore
026200161103           exec sql   fetch next   from C0   into :DS_Res;
026300161103
026400161103           select;
026500161103            // nessun rcd trovato
026600161103            when sqlCode = 100;
026700161103             // ho finito i dati, esco dal ciclo
026800161104             fineDati = *on;
026900161103             exec sql close C0;
027000161103            // errore SQL
027100161103            when sqlCode < 0;
027200161103             exec sql close C0;
027300161103             // esco con errore SQL
027400161103             oMACHKEsito = '2';
027500161103             exsr sr_RoutEnd;
027600161103            // c'è il rcd, allora scrivo TIMAC00F
027700161103            other;
027800161108             exsr sr_UpdateTIMAC;
027900161103           endsl;
028000121217
028100161103         enddo;
028200161110
028300161110         // se sono qui, vuol dire che è anmdato tutto bene
028400161110         oMACHKEsito = '1';
028500161108
028600161108         // - Chiusura cursore SQL?
028700161108         exsr  sr_CloseCursor;
028800161103
028900121217       ENDSR;
029000121217
029100121217       //--------------------------------------------------------------
029200161104       // Chiusura cursore.                                            ?
029300121217       //--------------------------------------------------------------
029400121217       BEGSR  sr_CloseCursor;
029500121217
029600161103         exec sql close C0;
029700121217
029800121217       ENDSR;
029900161108
030000161108       //--------------------------------------------------------------
030100161108       // aggiorna TIMAC00F
030200161108       //--------------------------------------------------------------
030300161108       BEGSR  sr_UpdateTIMAC;
030400161108
030500161108         // provo a vedere se la fascia calcolata esiste già su TIMAC00F
030600161108         // in questo caso aggiorno
030700161108         exec sql update TIMAC00F
030800161108                  set MACCnt1 = :NrEmail ,
030900161108                      MACDif1 = :Avg_Diff_Sec
031000161108                  where MACDtChk = date(substr(:Fascia , 1 , 10)) and
031100161108                        MACTmChk = time(substr(:Fascia , 12 , 2) concat
031200161108                                        '.00.00');
031300161108         select;
031400161108          // nessun rcd trovato
031500161108          when sqlCode = 100;
031600161108         // in questo caso scrivo
031700161108           exsr sr_InsertTIMAC;
031800161108          // errore SQL
031900161108          when sqlCode < 0;
032000161108           exec sql close C0;
032100161108           // esco con errore SQL
032200161108           oMACHKEsito = '2';
032300161108           exsr sr_RoutEnd;
032400161108           // in tutti gli altri casi andava bene l'update
032500161108         endsl;
032600161108
032700161108       ENDSR;
032800161108
032900161108       //--------------------------------------------------------------
033000161108       // scrive TIMAC00F
033100161108       //--------------------------------------------------------------
033200161108       BEGSR  sr_InsertTIMAC;
033300161108
033400161108           exec sql insert into TIMAC00F
033500161108                    (MACDtChk, MACTmChk, MACCnt1, MACDif1)
033600161108                    values(date(substr(:Fascia , 1 , 10)) ,
033700161108                           time(substr(:Fascia , 12 , 2) concat  '.00.00') ,
033800161108                           :NrEmail, :Avg_Diff_Sec);
033900161108         select;
034000161108          // nessun rcd trovato
034100161108          when sqlCode = 100;
034200161108          // errore SQL
034300161108          when sqlCode < 0;
034400161108           exec sql close C0;
034500161108           // esco con errore SQL
034600161108           oMACHKEsito = '2';
034700161108           exsr sr_RoutEnd;
034800161108           // in tutti gli altri casi andava bene l'insert
034900161108         endsl;
035000161108
035100161108       ENDSR;
035200121217
035300121217       //--------------------------------------------------------------
035400161104       // Operazioni finali.                                           ?
035500121217       //--------------------------------------------------------------
035600121217       BEGSR  sr_RoutEnd;
035700161103
035800161103         // - Chiusura pgm?
035900121217         return;
036000121217
036100121217       ENDSR;
036200121217
