000100090710     H NOMAIN
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
005300111221     **
005400111221     **
005500111221     ** ESEMPI DI CHIAMATA
005600111221     **
005700111222     ** dataISO = UBFMTDATE_Convert('190211:'DDMMYY':'YYYY-MM-DD')
005800090710     D*
005900090710      //********************************************************************************************
006000090710      //
006100090710      // Definizione variabili work (locali)
006200111221      //
006300090710      //********************************************************************************************
006400111222     D* timestamp in uscita per API
006500120102     D timestamp_out   s             64A   VARYING
006600111222     D* secondi
006700111222     D secondi         s              8F
006800111222     D* esito API
006900120103     D fc              ds            12
007000120103     D  Tutto                  1     12
007100120103     D  MsgSev                 1      2I 0
007200120103     D  MsgNo                  3      4I 0
007300120103     D  Vari                   5      5I 0
007400120103     D  Facility_ID            6      8A
007500120103     D  ISINFO                 9     12I 0
007600111221
007700111221      /FREE
007800111222
007900111222       //Picture String                           Example                       Note
008000111222       //o maschere riconosciute
008100111222
008200111222       //  Tipiche
008300111222       //YYMMDD                                   880516
008400111222       //YYYYMMDD                                 19880516
008500111222       //YYYY-MM-DD                               1988-05-16
008600111222       //MMDDYY                                   050688                        anche con DD primo
008700111222       //MM/DD/YY                                 05/06/88
008800111222       //ZM/ZD/YY                                 5/6/88                        Z=soppressione 0
008900111222       //MM/DD/YY                                 05/06/1988
009000111222       //MM/DD/Y                                  05/06/8
009100111222       //DD.MM.YY                                 09.06.88
009200111222       //DD-RRRR-YY                               09-  VI-88                    RRRR=nr.latino
009300111222       //DD MMM YY                                09 JUN 88
009400111222       //DD Mmmmmmmmmm YY                         09 June       88
009500111222       //ZD Mmmmmmmmmz YY                         9 June 88                     Z=soppressione 0
009600111222       //Mmmmmmmmmz ZD, YYYY                      June 9, 1988
009700111222       //ZDMMMMMMMMZYY                            9JUNE88                       Z=soppressione 0
009800111222       //  Giuliani
009900111222       //YY.DDD                                   88.137
010000111222       //YYDDD                                    88137
010100111222       //YYYY/DDD                                 1988/137
010200111222       //  Timestamp
010300111222       //YYMMDDHHMISS                             880516204229
010400111222       //YYYYMMDDHHMISS                           19880516204229
010500111222       //YYYY-MM-DD HH:MI:SS.999                  1988-05-16 20:42:29.046
010600111222       //WWW, ZM/ZD/YY HH:MI AP                   MON, 5/16/88 08:42 PM
010700111222       //Wwwwwwwwwz, DD Mmm YYYY, ZH:MI AP        Monday, 16 May 1988, 8:42 PM
010800111222
010900120103        //senza ricevere il formato chiamata, o ricevendolo blanks, verrà eseguito il driver e
011000120103        //verrà restituita solo una stringa di 64A la quale conterrà la data di output o,
011100120103        //qualunque errore si verifichi, la data di input
011200120103
011300120103        //il formato chiamata RE prevede la restituzione di una DS contenente:
011400120103        // - la data formattata (se non ci sono errori)
011500120103        // - il tipo di errore (DE = errore formale data input; FE = errore formato di input;
011600120103        //                      OE = altri errori, anche sul formato di output)
011700120103        // - la descrizione estesa dell'errore
011800120102        // la data restituita è di dimensione 64A VARYING, perché non essendo possibile indicare
011900120102        // che un valore di ritorno è di DIMENSIONE variabile (*varsize), impostarla 32767A VARYING
012000120102        // occuperebbe troppa memoria e la data massima che si può restituire rispetto ai picture
012100120102        // è di 33 char, quindi 64 per sicurezza.
012200111222
012300111222        //converto la data nel formato di partenza in secondi
012400111222        ConvertTimestampToNumberOfSeconds
012500120103         (pInDateIn : pInMaskDateIn : secondi : fc);
012600111222
012700120103        //se il feedback è di errore passo in output la stessa stringa ricevuta
012800120103        if MsgSev>=3;
012900120103          pOutREDateOut = pInDateIn;
013000120103          if pInFormat = 'RE';
013100120103            pOutREError = 'XX';
013200120103            return pOutREDS;
013300120102          endif;
013400120102        endif;
013500111222
013600111222
013700111222        //converto i secondi in una data nel formato di arrivo
013800111222        ConvertSecondsToCharacterTimestamp
013900120103         (secondi : pInMaskDateOut : timestamp_out : fc);
014000111222
014100111222        //se l'esito è di errore passo in output la stessa stringa ricevuta
014200120103        if MsgSev>=3;
014300120103          if pInFormat = 'RE';
014400120103            pOutREDateOut = pInDateIn;
014500120103            pOutREError = 'XX';
014600120103            return pOutREDS;
014700120102          endif;
014800111222        endif;
014900111221
015000111221
015100120102        //ritorno corretto a seconda del formato richiesto
015200120103        if pInFormat = 'RE';
015300120103          pOutREDateOut = timestamp_out;
015400120103          pOutREError = 'YY';
015500120103          return pOutREDS;
015600120102        endif;
015700090710
015800090710      /END-FREE
015900090710
016000111221     P UBFMTDATE_Convert...
016100090710     P                 E
