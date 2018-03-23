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
009600160929     D wDate           s               d
009700160929     D wOfsCESTinMin   s              4s 0
009800120103
009900111221      /FREE
010000111222
010100111222       //Picture String                           Example                       Note
010200111222       //o maschere riconosciute
010300111222
010400111222       //  Tipiche
010500111222       //YYMMDD                                   880516
010600111222       //YYYYMMDD                                 19880516
010700111222       //YYYY-MM-DD                               1988-05-16
010800111222       //MMDDYY                                   050688                        anche con DD primo
010900111222       //MM/DD/YY                                 05/06/88
011000111222       //ZM/ZD/YY                                 5/6/88                        Z=soppressione 0
011100111222       //MM/DD/YY                                 05/06/1988
011200111222       //MM/DD/Y                                  05/06/8
011300111222       //DD.MM.YY                                 09.06.88
011400111222       //DD-RRRR-YY                               09-  VI-88                    RRRR=nr.latino
011500111222       //DD MMM YY                                09 JUN 88
011600111222       //DD Mmmmmmmmmm YY                         09 June       88
011700111222       //ZD Mmmmmmmmmz YY                         9 June 88                     Z=soppressione 0
011800111222       //Mmmmmmmmmz ZD, YYYY                      June 9, 1988
011900111222       //ZDMMMMMMMMZYY                            9JUNE88                       Z=soppressione 0
012000111222       //  Giuliani
012100111222       //YY.DDD                                   88.137
012200111222       //YYDDD                                    88137
012300111222       //YYYY/DDD                                 1988/137
012400111222       //  Timestamp
012500111222       //YYMMDDHHMISS                             880516204229
012600111222       //YYYYMMDDHHMISS                           19880516204229
012700111222       //YYYY-MM-DD HH:MI:SS.999                  1988-05-16 20:42:29.046
012800111222       //WWW, ZM/ZD/YY HH:MI AP                   MON, 5/16/88 08:42 PM
012900111222       //Wwwwwwwwwz, DD Mmm YYYY, ZH:MI AP        Monday, 16 May 1988, 8:42 PM
013000160519       //
013100160523       // i seguenti formati di output:
013200160519       //YYYY-MM-DDTHH:MI:SS UTC                     2003-10-10T20:34:00-08:00
013300160523       //YYYY-MM-DDTHH:MI:SSZ                        2003-10-10T18:34:00Z
013400160524       //YYYYMMDDHHMISSZ                             19880516204229
013500160523       // siccome non sono gestiti automaticamente dall'API CEEDATM, vengono gestito dal srvpgm
013600160523       // con un triplo passaggio
013700160519       // 1. prima la data d'ingresso trasformata in secondi, viene convertita nel formato:
013800160519         //YYYY-MM-DD HH:MI:SS.999                  1988-05-16 20:42:29.046
013900160519       // 2. poi viene recuperato l'offset UTC corrente (ATTENZIONE! quello corrente NON della data)
014000160519       //    mediante l'API CEEUTCO
014100160523       // 3. YYYY-MM-DDTHH:MI:SS UTC  infine alla data recuperata viene:
014200160519         // 3.1 aggiunta la T di TIME
014300160519         // 3.2 sostituiti i millisecondi (.999) con l'offset (-hh:mm)
014400160523       // 3. YYYY-MM-DDTHH:MI:SSZ     infine alla data recuperata viene:
014500160523         // 3.1 aggiunta la T di TIME
014600160523         // 3.2 puliti i millisecondi (.999)
014700160523         // 3.3 sottratto l'offset recuperato
014800160524       // 3. YYYYMMDDHHMISSZ          infine alla data recuperata viene:
014900160524         // 3.1 aggiunta la T di TIME
015000160524         // 3.2 puliti i millisecondi (.999)
015100160524         // 3.3 sottratto l'offset recuperato
015200111222
015300120103        //senza ricevere il formato chiamata, o ricevendolo blanks, verrà eseguito il driver e
015400120103        //verrà restituita solo una stringa di 64A la quale conterrà la data di output o,
015500120103        //qualunque errore si verifichi, la data di input
015600120103
015700120103        //il formato chiamata RE prevede la restituzione di una DS contenente:
015800120103        // - la data formattata (se non ci sono errori)
015900120103        // - il tipo di errore (DE = errore formale data input; FE = errore formato di input;
016000120103        //                      OE = altri errori, anche sul formato di output)
016100120103        // - la descrizione estesa dell'errore
016200120102        // la data restituita è di dimensione 64A VARYING, perché non essendo possibile indicare
016300120102        // che un valore di ritorno è di DIMENSIONE variabile (*varsize), impostarla 32767A VARYING
016400120102        // occuperebbe troppa memoria e la data massima che si può restituire rispetto ai picture
016500120102        // è di 33 char, quindi 64 per sicurezza.
016600111222
016700111222        //converto la data nel formato di partenza in secondi
016800111222        ConvertTimestampToNumberOfSeconds
016900120103         (pInDateIn : pInMaskDateIn : secondi : fc);
017000111222
017100120103        //se la severità è di errore, passo in output la stessa stringa ricevuta
017200120103        if MsgSev>=3;
017300120103          timestamp_out = pInDateIn;
017400120103
017500120103          //se è stato richiesto l'indicazione del tipo errore
017600120103          if %parms > 3 and pInFormat='RE';
017700120103            pOutREDateOut = pInDateIn;
017800120103            //recupero il codice errore (che è in formato esadecimale)
017900120103            exec sql
018000120103              set :condition = hex(:MsgNo);
018100120103
018200120103            select;
018300120103              //errori sulla data di input
018400120103              when condition='2508'
018500120103                or condition='2509'
018600120103                or condition='2510'
018700120103                or condition='2513'
018800120103                or condition='2515'
018900120103                or condition='2516'
019000120103                or condition='2517'
019100120103                or condition='2519'
019200120103                or condition='2521';
019300120103                pOutREError = 'DE';
019400120103                pOutREDescr = 'Data errata';
019500120103              //errori sul formato di input
019600120103              when condition='2518'
019700120103                or condition='2525';
019800120103                pOutREError = 'FE';
019900120103                pOutREDescr = 'Formato di partenza errato';
020000120103              //altri errori
020100120103              other;
020200120103                pOutREError = 'OE';
020300120103                pOutREDescr = 'Altri errori sui dati di partenza';
020400120103            endsl;
020500120103
020600120103            return pOutREDS;
020700120103          endif;
020800111222
020900120103          return timestamp_out;
021000120103        endif;
021100111222
021200111222        //converto i secondi in una data nel formato di arrivo
021300160523          // prima controllo se il formato di arrivo è uno di questi
021400160519          //YYYY-MM-DDTHH:MI:SS UTC
021500160523          //YYYY-MM-DDTHH:MI:SSZ
021600160519          // in questo caso devo:
021700160519          // 1. prima la data d'ingresso trasformata in secondi, viene convertita nel formato:
021800160519            //YYYY-MM-DD HH:MI:SS.999                  1988-05-16 20:42:29.046
021900160519          // 2. poi viene recuperato l'offset UTC corrente
022000160519          //    mediante l'API CEEUTCO
022100160519        wUTC = *off;
022200160524        wZuluZ = *off;
022300160524        wZuluN = *off;
022400160519        wpInMaskDateOut = pInMaskDateOut;
022500160523        select;
022600160523        when pInMaskDateOut = 'YYYY-MM-DDTHH:MI:SS UTC';
022700160523          wpInMaskDateOut = 'YYYY-MM-DD HH:MI:SS.999';
022800160523          wUTC = *on;
022900160523        when pInMaskDateOut = 'YYYY-MM-DDTHH:MI:SSZ';
023000160523          wpInMaskDateOut = 'YYYY-MM-DD-HH.MI.SS.999';
023100160524          wZuluZ = *on;
023200160524        // timestamp numerico senza millisecondi in formato ZULU
023300160524        when pInMaskDateOut = 'YYYYMMDDHHMISSZ';
023400160524          // prima richiedo il timestamp completo
023500160524          wpInMaskDateOut = 'YYYY-MM-DD-HH.MI.SS.999';
023600160524          wZuluN = *on;
023700160523        endsl;
023800160929        // questo pezzo non serce più perché l'offset UTC CEST lo recupero per la data passata su
023900160929        // DATECNV0F e non più per la data corrente
024000160929        //if wUTC = *on or
024100160929          //wZuluZ = *on or
024200160929          //wZuluN = *on;
024300160519          // recupero l'offset UTC corrente (ATTENZIONE! quello corrente)
024400160929          //GetOffsetFromUniversalTimeCoordinatedToLocalTime
024500160929          // (offset_hours : offset_minutes : offset_seconds : fc);
024600160929        //endif;
024700160519
024800111222        ConvertSecondsToCharacterTimestamp
024900160519         (secondi : wpInMaskDateOut : timestamp_out : fc);
025000111222
025100111222        //se l'esito è di errore passo in output la stessa stringa ricevuta
025200120103        if MsgSev>=3;
025300120103          timestamp_out = pInDateIn;
025400120103
025500120103          //se è stato richiesto l'indicazione del tipo errore
025600120103          if %parms > 3 and pInFormat='RE';
025700120103            pOutREDateOut = pInDateIn;
025800120103            //imposto sempre altri errori
025900120103            pOutREError = 'OE';
026000120103            pOutREDescr = 'Formato di arrivo errato o altro';
026100120103
026200120103            return pOutREDS;
026300120103          endif;
026400120103
026500120103          return timestamp_out;
026600111222        endif;
026700111221
026800160523        select;
026900160519        // se il formato di arrivo era quello col valore dell'offset UTC
027000160523        when wUTC = *on;
027100160929          // recupero l'offset UTC della data corrente leggendo il file DATECNV0F
027200160929          // siccome la data ne campo timestamp_out ha questo formato: 'YYYY-MM-DDTHH:MI:SS UTC'
027300160929          // e per reperire l'offset UTC CEST su DATECNV0F mi serve la data in formato date
027400160929          // lo estraggo da questo
027500160929          wDate = %date(%subst(timestamp_out:1:10):*ISO);
027600160929          exsr RepUTCOfsCEST;
027700160523          // 3.1 aggiunta la T di TIME
027800160523          %subst(timestamp_out : 11 : 1) = 'T';
027900160523          // 3.2 sostituiti i millisecondi (.999) con l'offset (-hh:mm)
028000160523            // devo mettere anche il segno dell'offset, sia che sia + o -
028100160523            // siccome la var. timestamp_out è a lunghezza variabile e col formato
028200160523            // 'YYYY-MM-DD HH:MI:SS.999'
028300160523            // è lunga 23 char, non posso sostituire i millisecondi con l'offset UTC
028400160523            // perchè questo è più lungo, per cui prima sbianco i millisec. poi aggiungo l'offset
028500160523          %subst(timestamp_out : 20 : 4) = *blank;
028600160523          if offset_hours < 0;
028700160523            %subst(timestamp_out : 20 : 1) = '-';
028800160523          else;
028900160523            %subst(timestamp_out : 20 : 1) = '+';
029000160523          endif;
029100160523          timestamp_out = %trim(timestamp_out) +
029200160523           %editc(%dec(%abs(offset_hours) : 2 : 0) : 'X') +
029300160523           ':' +
029400160523           %editc(%dec(%abs(offset_minutes) : 2 : 0) : 'X');
029500160523
029600160523        // se il formato di arrivo era quello con la data ZULU
029700160524        when wZuluZ = *on;
029800160929          // recupero l'offset UTC della data corrente leggendo il file DATECNV0F
029900160929          // siccome la data ne campo timestamp_out ha questo formato: 'YYYY-MM-DDTHH:MI:SSZ''
030000160929          // e per reperire l'offset UTC CEST su DATECNV0F mi serve la data in formato date
030100160929          // lo estraggo da questo
030200160929          wDate = %date(%subst(timestamp_out:1:10):*ISO);
030300160929          exsr RepUTCOfsCEST;
030400160523          // trasformo il risultato da 'YYYY-MM-DD HH:MI:SS.999' a come deve essere un
030500160523          // timestamp per l'RPG
030600160523          // 3.1 aggiunta il - tra giorni e ore
030700160523          // aggiungo il - tra giorni e ore
030800160523          %subst(timestamp_out : 11 : 1) = '-';
030900160523          // sostituisco i : col . tra ore e min e tra min e sec
031000160523          %subst(timestamp_out : 14 : 1) = '.';
031100160523          %subst(timestamp_out : 17 : 1) = '.';
031200160523          // allungo a 6 cifre i millisecondi
031300160523          timestamp_out = %trim(timestamp_out) + '000';
031400160523          // 3.2 sottraggo l'offset recuperato
031500160523          if offset_hours < 0;
031600160523            timestamp_out = %char(%timestamp(timestamp_out:*ISO)
031700160523                            + %minutes(%abs(offset_minutes))
031800160523                            + %hours(%abs(offset_hours)));
031900160523          else;
032000160523            timestamp_out = %char(%timestamp(timestamp_out:*ISO)
032100160523                            - %minutes(offset_minutes)
032200160523                            - %hours(offset_hours));
032300160523          endif;
032400160523          // 3.3 a questo punto trasformo il risultato a come dev'essere una data Zulu
032500160523          %subst(timestamp_out : 11 : 1) = 'T';
032600160523          %subst(timestamp_out : 14 : 1) = ':';
032700160523          %subst(timestamp_out : 17 : 1) = ':';
032800160523          %subst(timestamp_out : 20 : 7) = 'Z';
032900160524
033000160524        // se il formato di arrivo è quello con la data ZULU ma numerica
033100160524        when wZuluN = *on;
033200160929          // recupero l'offset UTC della data corrente leggendo il file DATECNV0F
033300160929          // siccome la data ne campo timestamp_out ha questo formato: 'YYYYMMDDHHMISSZ'
033400160929          // e per reperire l'offset UTC CEST su DATECNV0F mi serve la data in formato date
033500160929          // lo estraggo da questo
033600160929          wDate = %date(%subst(timestamp_out:1:10):*ISO);
033700160929          exsr RepUTCOfsCEST;
033800160524          // trasformo il risultato da 'YYYY-MM-DD HH:MI:SS.999' a come deve essere un
033900160524          // timestamp per l'RPG
034000160524          // 3.1 aggiunta il - tra giorni e ore
034100160524          // aggiungo il - tra giorni e ore
034200160524          %subst(timestamp_out : 11 : 1) = '-';
034300160524          // sostituisco i : col . tra ore e min e tra min e sec
034400160524          %subst(timestamp_out : 14 : 1) = '.';
034500160524          %subst(timestamp_out : 17 : 1) = '.';
034600160524          // allungo a 6 cifre i millisecondi
034700160524          timestamp_out = %trim(timestamp_out) + '000';
034800160524          // 3.2 sottraggo l'offset recuperato
034900160524          if offset_hours < 0;
035000160524            timestamp_out = %char(%timestamp(timestamp_out:*ISO)
035100160524                            + %minutes(%abs(offset_minutes))
035200160524                            + %hours(%abs(offset_hours)));
035300160524          else;
035400160524            timestamp_out = %char(%timestamp(timestamp_out:*ISO)
035500160524                            - %minutes(offset_minutes)
035600160524                            - %hours(offset_hours));
035700160524          endif;
035800160524          // 3.3 a questo punto trasformo il risultato in un valore numerico seza millisec.
035900160524          w20S = %dec(%timestamp(timestamp_out:*ISO));
036000160524          w14S = w20S/1000000;
036100160524          timestamp_out = %char(w14S);
036200160523        endsl;
036300111221
036400120102        //ritorno corretto a seconda del formato richiesto
036500120104        select;
036600120424          when %parms > 3 and pInFormat='RE';
036700120104            pOutREDateOut = timestamp_out;
036800120104            pOutREError = *blank;
036900120104            pOutREDescr = *blank;
037000120104            return pOutREDS;
037100120104          other;
037200120104          return timestamp_out;
037300120104        endsl;
037400160929
037500160929       //--------------------------------------------------------------
037600160929       // Operazioni iniziali.
037700160929       //--------------------------------------------------------------
037800160929       BEGSR  RepUTCOfsCEST;
037900160929
038000160929         exec sql
038100160929         set :wOfsCESTinMin = (select DC_OFSCEST from DATECNV0F
038200160929                              where DC_Date = :wDate);
038300160929         //il dato è in minuti, reperisco ore e minuti rimanenti
038400160929         offset_hours = %div(wOfsCESTinMin:60);
038500160929         offset_minutes = %rem(wOfsCESTinMin:60);
038600160929
038700160929       ENDSR;
038800090710
038900090710      /END-FREE
039000090710
039100111221     P UBFMTDATE_Convert...
039200090710     P                 E
