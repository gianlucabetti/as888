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
002800080612
002900080612
003000071128      //********************************************************************************************
003100071128      //
003200080612      // Definizione variabili work (globali)
003300071128      //
003400071128      //********************************************************************************************
003500090710
003600090710
003700090710
003800090710     P*
003900111221     P UBFMTDATE_Convert...
004000090710     P                 B                   EXPORT
004100090710     D*
004200111221     D UBFMTDATE_Convert...
004300120102     D                 PI           100
004400111227     D pInDateIn                  32767A   CONST VARYING OPTIONS(*VARSIZE)
004500111227     D pInMaskDateIn              32767A   CONST VARYING OPTIONS(*VARSIZE)
004600111227     D pInMaskDateOut             32767A   CONST VARYING OPTIONS(*VARSIZE)
004700120103     D pInFormat                      2A   CONST OPTIONS(*OMIT:*NOPASS)
004800120102     D*
004900120103     D* DS di ritorno per formato chiamata RE
005000120103     D pOutREDS        DS
005100120103     D  pOutREDateOut                64A
005200120103     D  pOutREError                   2A
005300120103     D  pOutREDescr                  34A
005400111221     **
005500111221     **
005600111221     ** ESEMPI DI CHIAMATA
005700111221     **
005800120103     ** dataISO = UBFMTDATE_Convert('20120211:'YYYYMMDD':'YYYY-MM-DD')
005900090710     D*
006000090710      //********************************************************************************************
006100090710      //
006200090710      // Definizione variabili work (locali)
006300111221      //
006400090710      //********************************************************************************************
006500111222     D* timestamp in uscita per API
006600120102     D timestamp_out   s             64A   VARYING
006700111222     D* secondi
006800111222     D secondi         s              8F
006900111222     D* esito API
007000120103     D fc              ds            12
007100120103     D  Tutto                  1     12
007200120103     D  MsgSev                 1      2I 0
007300120104     D  MsgSevB                1      2B 0
007400120103     D  MsgNo                  3      4A
007500120103     D  Vari                   5      5I 0
007600120103     D  Facility_ID            6      8A
007700120103     D  ISINFO                 9     12I 0
007800111221
007900120103     D condition       s              4A
008000120103
008100111221      /FREE
008200111222
008300111222       //Picture String                           Example                       Note
008400111222       //o maschere riconosciute
008500111222
008600111222       //  Tipiche
008700111222       //YYMMDD                                   880516
008800111222       //YYYYMMDD                                 19880516
008900111222       //YYYY-MM-DD                               1988-05-16
009000111222       //MMDDYY                                   050688                        anche con DD primo
009100111222       //MM/DD/YY                                 05/06/88
009200111222       //ZM/ZD/YY                                 5/6/88                        Z=soppressione 0
009300111222       //MM/DD/YY                                 05/06/1988
009400111222       //MM/DD/Y                                  05/06/8
009500111222       //DD.MM.YY                                 09.06.88
009600111222       //DD-RRRR-YY                               09-  VI-88                    RRRR=nr.latino
009700111222       //DD MMM YY                                09 JUN 88
009800111222       //DD Mmmmmmmmmm YY                         09 June       88
009900111222       //ZD Mmmmmmmmmz YY                         9 June 88                     Z=soppressione 0
010000111222       //Mmmmmmmmmz ZD, YYYY                      June 9, 1988
010100111222       //ZDMMMMMMMMZYY                            9JUNE88                       Z=soppressione 0
010200111222       //  Giuliani
010300111222       //YY.DDD                                   88.137
010400111222       //YYDDD                                    88137
010500111222       //YYYY/DDD                                 1988/137
010600111222       //  Timestamp
010700111222       //YYMMDDHHMISS                             880516204229
010800111222       //YYYYMMDDHHMISS                           19880516204229
010900111222       //YYYY-MM-DD HH:MI:SS.999                  1988-05-16 20:42:29.046
011000111222       //WWW, ZM/ZD/YY HH:MI AP                   MON, 5/16/88 08:42 PM
011100111222       //Wwwwwwwwwz, DD Mmm YYYY, ZH:MI AP        Monday, 16 May 1988, 8:42 PM
011200111222
011300120103        //senza ricevere il formato chiamata, o ricevendolo blanks, verrà eseguito il driver e
011400120103        //verrà restituita solo una stringa di 64A la quale conterrà la data di output o,
011500120103        //qualunque errore si verifichi, la data di input
011600120103
011700120103        //il formato chiamata RE prevede la restituzione di una DS contenente:
011800120103        // - la data formattata (se non ci sono errori)
011900120103        // - il tipo di errore (DE = errore formale data input; FE = errore formato di input;
012000120103        //                      OE = altri errori, anche sul formato di output)
012100120103        // - la descrizione estesa dell'errore
012200120102        // la data restituita è di dimensione 64A VARYING, perché non essendo possibile indicare
012300120102        // che un valore di ritorno è di DIMENSIONE variabile (*varsize), impostarla 32767A VARYING
012400120102        // occuperebbe troppa memoria e la data massima che si può restituire rispetto ai picture
012500120102        // è di 33 char, quindi 64 per sicurezza.
012600111222
012700111222        //converto la data nel formato di partenza in secondi
012800111222        ConvertTimestampToNumberOfSeconds
012900120103         (pInDateIn : pInMaskDateIn : secondi : fc);
013000111222
013100120103        //se la severità è di errore, passo in output la stessa stringa ricevuta
013200120103        if MsgSev>=3;
013300120103          timestamp_out = pInDateIn;
013400120103
013500120103          //se è stato richiesto l'indicazione del tipo errore
013600120103          if %parms > 3 and pInFormat='RE';
013700120103            pOutREDateOut = pInDateIn;
013800120103            //recupero il codice errore (che è in formato esadecimale)
013900120103            exec sql
014000120103              set :condition = hex(:MsgNo);
014100120103
014200120103            select;
014300120103              //errori sulla data di input
014400120103              when condition='2508'
014500120103                or condition='2509'
014600120103                or condition='2510'
014700120103                or condition='2513'
014800120103                or condition='2515'
014900120103                or condition='2516'
015000120103                or condition='2517'
015100120103                or condition='2519'
015200120103                or condition='2521';
015300120103                pOutREError = 'DE';
015400120103                pOutREDescr = 'Data errata';
015500120103              //errori sul formato di input
015600120103              when condition='2518'
015700120103                or condition='2525';
015800120103                pOutREError = 'FE';
015900120103                pOutREDescr = 'Formato di partenza errato';
016000120103              //altri errori
016100120103              other;
016200120103                pOutREError = 'OE';
016300120103                pOutREDescr = 'Altri errori sui dati di partenza';
016400120103            endsl;
016500120103
016600120103            return pOutREDS;
016700120103          endif;
016800111222
016900120103          return timestamp_out;
017000120103        endif;
017100111222
017200111222        //converto i secondi in una data nel formato di arrivo
017300111222        ConvertSecondsToCharacterTimestamp
017400120103         (secondi : pInMaskDateOut : timestamp_out : fc);
017500111222
017600111222        //se l'esito è di errore passo in output la stessa stringa ricevuta
017700120103        if MsgSev>=3;
017800120103          timestamp_out = pInDateIn;
017900120103
018000120103          //se è stato richiesto l'indicazione del tipo errore
018100120103          if %parms > 3 and pInFormat='RE';
018200120103            pOutREDateOut = pInDateIn;
018300120103            //imposto sempre altri errori
018400120103            pOutREError = 'OE';
018500120103            pOutREDescr = 'Formato di arrivo errato o altro';
018600120103
018700120103            return pOutREDS;
018800120103          endif;
018900120103
019000120103          return timestamp_out;
019100111222        endif;
019200111221
019300111221
019400120102        //ritorno corretto a seconda del formato richiesto
019500120104        select;
019600120424          when %parms > 3 and pInFormat='RE';
019700120104            pOutREDateOut = timestamp_out;
019800120104            pOutREError = *blank;
019900120104            pOutREDescr = *blank;
020000120104            return pOutREDS;
020100120104          other;
020200120104          return timestamp_out;
020300120104        endsl;
020400090710
020500090710      /END-FREE
020600090710
020700111221     P UBFMTDATE_Convert...
020800090710     P                 E
