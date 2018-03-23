000100120606     H NOMAIN
000200071128     ***********************************************************************************************
000300071128     **
000400071128     ** ISTRUZIONI PER LA COMPILAZIONE
000500071128     **
000600111221     ** 1. Creare il modulo UBFMTDATE (Opz 15 PDM)
000700111221     ** 2. Creare/Aggiornare il programma di servizio UBFMTDATE (CRTSRVPGM / UPDSRVPGM)
000800080610     ** 4. Cancellare il modulo.
000900071128     **
001000071128     ***********************************************************************************************
001100080612
001200080612
001300080612      //********************************************************************************************
001400080612      //
001500080612      // Definizione file (globali)
001600080612      //
001700080612      //********************************************************************************************
001800071128
001900080612
002000071128      //********************************************************************************************
002100071128      //
002200080612      // Definizione prototipi procedure (globali)
002300071128      //
002400090710      //********************************************************************************************
002500111221     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
002600111222     D/COPY GAITRASRC/SRCPROTOPR,CEEDATM
002700111222     D/COPY GAITRASRC/SRCPROTOPR,CEESECS
002800160519     D/COPY GAITRASRC/SRCPROTOPR,CEEUTCO
002900080612
003000080612
003100071128      //********************************************************************************************
003200071128      //
003300080612      // Definizione variabili work (globali)
003400071128      //
003500071128      //********************************************************************************************
003600090710
003700090710
003800090710
003900090710     P*
004000111221     P UBFMTDATE_Convert...
004100090710     P                 B                   EXPORT
004200090710     D*
004300111221     D UBFMTDATE_Convert...
004400120102     D                 PI           100
004500111227     D pInDateIn                  32767A   CONST VARYING OPTIONS(*VARSIZE)
004600111227     D pInMaskDateIn              32767A   CONST VARYING OPTIONS(*VARSIZE)
004700111227     D pInMaskDateOut             32767A   CONST VARYING OPTIONS(*VARSIZE)
004800120103     D pInFormat                      2A   CONST OPTIONS(*OMIT:*NOPASS)
004900120102     D*
005000120103     D* DS di ritorno per formato chiamata RE
005100120103     D pOutREDS        DS
005200120103     D  pOutREDateOut                64A
005300120103     D  pOutREError                   2A
005400120103     D  pOutREDescr                  34A
005500111221     **
005600111221     **
005700111221     ** ESEMPI DI CHIAMATA
005800111221     **
005900120103     ** dataISO = UBFMTDATE_Convert('20120211:'YYYYMMDD':'YYYY-MM-DD')
006000090710     D*
006100090710      //********************************************************************************************
006200090710      //
006300090710      // Definizione variabili work (locali)
006400111221      //
006500090710      //********************************************************************************************
006600111222     D* timestamp in uscita per API
006700120102     D timestamp_out   s             64A   VARYING
006800111222     D* secondi
006900111222     D secondi         s              8F
007000111222     D* esito API
007100120103     D fc              ds            12
007200120103     D  Tutto                  1     12
007300120103     D  MsgSev                 1      2I 0
007400120104     D  MsgSevB                1      2B 0
007500120103     D  MsgNo                  3      4A
007600120103     D  Vari                   5      5I 0
007700120103     D  Facility_ID            6      8A
007800120103     D  ISINFO                 9     12I 0
007900111221
008000160519       // parametri per offset
008100160519     D  offset_hours...
008200160519     D                 s             10I 0
008300160519     D  offset_minutes...
008400160519     D                 s             10I 0
008500160519     D  offset_seconds...
008600160519     D                 s              8F
008700160519
008800160519     D wpInMaskDateOut...
008900160519     D                 s          32767A   VARYING
009000120103     D condition       s              4A
009100160519     D wUTC            s               n
009200160524     D wZuluZ          s               n
009300160524     D wZuluN          s               n
009400160524     D w20S            s             20s 0
009500160524     D w14S            s             14s 0
009600120103
009700111221      /FREE
009800111222
009900111222       //Picture String                           Example                       Note
010000111222       //o maschere riconosciute
010100111222
010200111222       //  Tipiche
010300111222       //YYMMDD                                   880516
010400111222       //YYYYMMDD                                 19880516
010500111222       //YYYY-MM-DD                               1988-05-16
010600111222       //MMDDYY                                   050688                        anche con DD primo
010700111222       //MM/DD/YY                                 05/06/88
010800111222       //ZM/ZD/YY                                 5/6/88                        Z=soppressione 0
010900111222       //MM/DD/YY                                 05/06/1988
011000111222       //MM/DD/Y                                  05/06/8
011100111222       //DD.MM.YY                                 09.06.88
011200111222       //DD-RRRR-YY                               09-  VI-88                    RRRR=nr.latino
011300111222       //DD MMM YY                                09 JUN 88
011400111222       //DD Mmmmmmmmmm YY                         09 June       88
011500111222       //ZD Mmmmmmmmmz YY                         9 June 88                     Z=soppressione 0
011600111222       //Mmmmmmmmmz ZD, YYYY                      June 9, 1988
011700111222       //ZDMMMMMMMMZYY                            9JUNE88                       Z=soppressione 0
011800111222       //  Giuliani
011900111222       //YY.DDD                                   88.137
012000111222       //YYDDD                                    88137
012100111222       //YYYY/DDD                                 1988/137
012200111222       //  Timestamp
012300111222       //YYMMDDHHMISS                             880516204229
012400111222       //YYYYMMDDHHMISS                           19880516204229
012500111222       //YYYY-MM-DD HH:MI:SS.999                  1988-05-16 20:42:29.046
012600111222       //WWW, ZM/ZD/YY HH:MI AP                   MON, 5/16/88 08:42 PM
012700111222       //Wwwwwwwwwz, DD Mmm YYYY, ZH:MI AP        Monday, 16 May 1988, 8:42 PM
012800160519       //
012900160523       // i seguenti formati di output:
013000160519       //YYYY-MM-DDTHH:MI:SS UTC                     2003-10-10T20:34:00-08:00
013100160523       //YYYY-MM-DDTHH:MI:SSZ                        2003-10-10T18:34:00Z
013200160524       //YYYYMMDDHHMISSZ                             19880516204229
013300160523       // siccome non sono gestiti automaticamente dall'API CEEDATM, vengono gestito dal srvpgm
013400160523       // con un triplo passaggio
013500160519       // 1. prima la data d'ingresso trasformata in secondi, viene convertita nel formato:
013600160519         //YYYY-MM-DD HH:MI:SS.999                  1988-05-16 20:42:29.046
013700160519       // 2. poi viene recuperato l'offset UTC corrente (ATTENZIONE! quello corrente NON della data)
013800160519       //    mediante l'API CEEUTCO
013900160523       // 3. YYYY-MM-DDTHH:MI:SS UTC  infine alla data recuperata viene:
014000160519         // 3.1 aggiunta la T di TIME
014100160519         // 3.2 sostituiti i millisecondi (.999) con l'offset (-hh:mm)
014200160523       // 3. YYYY-MM-DDTHH:MI:SSZ     infine alla data recuperata viene:
014300160523         // 3.1 aggiunta la T di TIME
014400160523         // 3.2 puliti i millisecondi (.999)
014500160523         // 3.3 sottratto l'offset recuperato
014600160524       // 3. YYYYMMDDHHMISSZ          infine alla data recuperata viene:
014700160524         // 3.1 aggiunta la T di TIME
014800160524         // 3.2 puliti i millisecondi (.999)
014900160524         // 3.3 sottratto l'offset recuperato
015000111222
015100120103        //senza ricevere il formato chiamata, o ricevendolo blanks, verrà eseguito il driver e
015200120103        //verrà restituita solo una stringa di 64A la quale conterrà la data di output o,
015300120103        //qualunque errore si verifichi, la data di input
015400120103
015500120103        //il formato chiamata RE prevede la restituzione di una DS contenente:
015600120103        // - la data formattata (se non ci sono errori)
015700120103        // - il tipo di errore (DE = errore formale data input; FE = errore formato di input;
015800120103        //                      OE = altri errori, anche sul formato di output)
015900120103        // - la descrizione estesa dell'errore
016000120102        // la data restituita è di dimensione 64A VARYING, perché non essendo possibile indicare
016100120102        // che un valore di ritorno è di DIMENSIONE variabile (*varsize), impostarla 32767A VARYING
016200120102        // occuperebbe troppa memoria e la data massima che si può restituire rispetto ai picture
016300120102        // è di 33 char, quindi 64 per sicurezza.
016400111222
016500111222        //converto la data nel formato di partenza in secondi
016600111222        ConvertTimestampToNumberOfSeconds
016700120103         (pInDateIn : pInMaskDateIn : secondi : fc);
016800111222
016900120103        //se la severità è di errore, passo in output la stessa stringa ricevuta
017000120103        if MsgSev>=3;
017100120103          timestamp_out = pInDateIn;
017200120103
017300120103          //se è stato richiesto l'indicazione del tipo errore
017400120103          if %parms > 3 and pInFormat='RE';
017500120103            pOutREDateOut = pInDateIn;
017600120103            //recupero il codice errore (che è in formato esadecimale)
017700120103            exec sql
017800120103              set :condition = hex(:MsgNo);
017900120103
018000120103            select;
018100120103              //errori sulla data di input
018200120103              when condition='2508'
018300120103                or condition='2509'
018400120103                or condition='2510'
018500120103                or condition='2513'
018600120103                or condition='2515'
018700120103                or condition='2516'
018800120103                or condition='2517'
018900120103                or condition='2519'
019000120103                or condition='2521';
019100120103                pOutREError = 'DE';
019200120103                pOutREDescr = 'Data errata';
019300120103              //errori sul formato di input
019400120103              when condition='2518'
019500120103                or condition='2525';
019600120103                pOutREError = 'FE';
019700120103                pOutREDescr = 'Formato di partenza errato';
019800120103              //altri errori
019900120103              other;
020000120103                pOutREError = 'OE';
020100120103                pOutREDescr = 'Altri errori sui dati di partenza';
020200120103            endsl;
020300120103
020400120103            return pOutREDS;
020500120103          endif;
020600111222
020700120103          return timestamp_out;
020800120103        endif;
020900111222
021000111222        //converto i secondi in una data nel formato di arrivo
021100160523          // prima controllo se il formato di arrivo è uno di questi
021200160519          //YYYY-MM-DDTHH:MI:SS UTC
021300160523          //YYYY-MM-DDTHH:MI:SSZ
021400160519          // in questo caso devo:
021500160519          // 1. prima la data d'ingresso trasformata in secondi, viene convertita nel formato:
021600160519            //YYYY-MM-DD HH:MI:SS.999                  1988-05-16 20:42:29.046
021700160519          // 2. poi viene recuperato l'offset UTC corrente
021800160519          //    mediante l'API CEEUTCO
021900160519        wUTC = *off;
022000160524        wZuluZ = *off;
022100160524        wZuluN = *off;
022200160519        wpInMaskDateOut = pInMaskDateOut;
022300160523        select;
022400160523        when pInMaskDateOut = 'YYYY-MM-DDTHH:MI:SS UTC';
022500160523          wpInMaskDateOut = 'YYYY-MM-DD HH:MI:SS.999';
022600160523          wUTC = *on;
022700160523        when pInMaskDateOut = 'YYYY-MM-DDTHH:MI:SSZ';
022800160523          wpInMaskDateOut = 'YYYY-MM-DD-HH.MI.SS.999';
022900160524          wZuluZ = *on;
023000160524        // timestamp numerico senza millisecondi in formato ZULU
023100160524        when pInMaskDateOut = 'YYYYMMDDHHMISSZ';
023200160524          // prima richiedo il timestamp completo
023300160524          wpInMaskDateOut = 'YYYY-MM-DD-HH.MI.SS.999';
023400160524          wZuluN = *on;
023500160523        endsl;
023600160523        if wUTC = *on or
023700160524           wZuluZ = *on or
023800160524           wZuluN = *on;
023900160519          // recupero l'offset UTC corrente (ATTENZIONE! quello corrente)
024000160519          GetOffsetFromUniversalTimeCoordinatedToLocalTime
024100160519           (offset_hours : offset_minutes : offset_seconds : fc);
024200160519        endif;
024300160519
024400111222        ConvertSecondsToCharacterTimestamp
024500160519         (secondi : wpInMaskDateOut : timestamp_out : fc);
024600111222
024700111222        //se l'esito è di errore passo in output la stessa stringa ricevuta
024800120103        if MsgSev>=3;
024900120103          timestamp_out = pInDateIn;
025000120103
025100120103          //se è stato richiesto l'indicazione del tipo errore
025200120103          if %parms > 3 and pInFormat='RE';
025300120103            pOutREDateOut = pInDateIn;
025400120103            //imposto sempre altri errori
025500120103            pOutREError = 'OE';
025600120103            pOutREDescr = 'Formato di arrivo errato o altro';
025700120103
025800120103            return pOutREDS;
025900120103          endif;
026000120103
026100120103          return timestamp_out;
026200111222        endif;
026300111221
026400160523        select;
026500160519        // se il formato di arrivo era quello col valore dell'offset UTC
026600160523        when wUTC = *on;
026700160523          // 3.1 aggiunta la T di TIME
026800160523          %subst(timestamp_out : 11 : 1) = 'T';
026900160523          // 3.2 sostituiti i millisecondi (.999) con l'offset (-hh:mm)
027000160523            // devo mettere anche il segno dell'offset, sia che sia + o -
027100160523            // siccome la var. timestamp_out è a lunghezza variabile e col formato
027200160523            // 'YYYY-MM-DD HH:MI:SS.999'
027300160523            // è lunga 23 char, non posso sostituire i millisecondi con l'offset UTC
027400160523            // perchè questo è più lungo, per cui prima sbianco i millisec. poi aggiungo l'offset
027500160523          %subst(timestamp_out : 20 : 4) = *blank;
027600160523          if offset_hours < 0;
027700160523            %subst(timestamp_out : 20 : 1) = '-';
027800160523          else;
027900160523            %subst(timestamp_out : 20 : 1) = '+';
028000160523          endif;
028100160523          timestamp_out = %trim(timestamp_out) +
028200160523           %editc(%dec(%abs(offset_hours) : 2 : 0) : 'X') +
028300160523           ':' +
028400160523           %editc(%dec(%abs(offset_minutes) : 2 : 0) : 'X');
028500160523
028600160523        // se il formato di arrivo era quello con la data ZULU
028700160524        when wZuluZ = *on;
028800160523          // trasformo il risultato da 'YYYY-MM-DD HH:MI:SS.999' a come deve essere un
028900160523          // timestamp per l'RPG
029000160523          // 3.1 aggiunta il - tra giorni e ore
029100160523          // aggiungo il - tra giorni e ore
029200160523          %subst(timestamp_out : 11 : 1) = '-';
029300160523          // sostituisco i : col . tra ore e min e tra min e sec
029400160523          %subst(timestamp_out : 14 : 1) = '.';
029500160523          %subst(timestamp_out : 17 : 1) = '.';
029600160523          // allungo a 6 cifre i millisecondi
029700160523          timestamp_out = %trim(timestamp_out) + '000';
029800160523          // 3.2 sottraggo l'offset recuperato
029900160523          if offset_hours < 0;
030000160523            timestamp_out = %char(%timestamp(timestamp_out:*ISO)
030100160523                            + %minutes(%abs(offset_minutes))
030200160523                            + %hours(%abs(offset_hours)));
030300160523          else;
030400160523            timestamp_out = %char(%timestamp(timestamp_out:*ISO)
030500160523                            - %minutes(offset_minutes)
030600160523                            - %hours(offset_hours));
030700160523          endif;
030800160523          // 3.3 a questo punto trasformo il risultato a come dev'essere una data Zulu
030900160523          %subst(timestamp_out : 11 : 1) = 'T';
031000160523          %subst(timestamp_out : 14 : 1) = ':';
031100160523          %subst(timestamp_out : 17 : 1) = ':';
031200160523          %subst(timestamp_out : 20 : 7) = 'Z';
031300160524
031400160524        // se il formato di arrivo è quello con la data ZULU ma numerica
031500160524        when wZuluN = *on;
031600160524          // trasformo il risultato da 'YYYY-MM-DD HH:MI:SS.999' a come deve essere un
031700160524          // timestamp per l'RPG
031800160524          // 3.1 aggiunta il - tra giorni e ore
031900160524          // aggiungo il - tra giorni e ore
032000160524          %subst(timestamp_out : 11 : 1) = '-';
032100160524          // sostituisco i : col . tra ore e min e tra min e sec
032200160524          %subst(timestamp_out : 14 : 1) = '.';
032300160524          %subst(timestamp_out : 17 : 1) = '.';
032400160524          // allungo a 6 cifre i millisecondi
032500160524          timestamp_out = %trim(timestamp_out) + '000';
032600160524          // 3.2 sottraggo l'offset recuperato
032700160524          if offset_hours < 0;
032800160524            timestamp_out = %char(%timestamp(timestamp_out:*ISO)
032900160524                            + %minutes(%abs(offset_minutes))
033000160524                            + %hours(%abs(offset_hours)));
033100160524          else;
033200160524            timestamp_out = %char(%timestamp(timestamp_out:*ISO)
033300160524                            - %minutes(offset_minutes)
033400160524                            - %hours(offset_hours));
033500160524          endif;
033600160524          // 3.3 a questo punto trasformo il risultato in un valore numerico seza millisec.
033700160524          w20S = %dec(%timestamp(timestamp_out:*ISO));
033800160524          w14S = w20S/1000000;
033900160524          timestamp_out = %char(w14S);
034000160523        endsl;
034100111221
034200120102        //ritorno corretto a seconda del formato richiesto
034300120104        select;
034400120424          when %parms > 3 and pInFormat='RE';
034500120104            pOutREDateOut = timestamp_out;
034600120104            pOutREError = *blank;
034700120104            pOutREDescr = *blank;
034800120104            return pOutREDS;
034900120104          other;
035000120104          return timestamp_out;
035100120104        endsl;
035200090710
035300090710      /END-FREE
035400090710
035500111221     P UBFMTDATE_Convert...
035600090710     P                 E
