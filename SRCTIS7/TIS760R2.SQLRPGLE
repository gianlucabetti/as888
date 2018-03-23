000100091228       //==============================================================
000200140630       // Estrazione Log Upload in errore
000300091228       //==============================================================
000400140703
000500140703      * per compilare questo pgm bisogna avere in lista di librerie il file WFLUE00F che viene,
000600140703      * però, creato e distrutto da questi pgm (è una table SQL).
000700140703      * quindi crearlo secondo lo statement in SRCTNDB nella propria libreria e mettere questa
000800140703      * in lista temporaneamente, ricordandosi di toglierla prima di lanciare l'esecuzione.
000900091228
001000091228       //--------------------------------------------------------------
001100121106       // Specifiche di controllo.
001200091228       //--------------------------------------------------------------
001300091223
001400091223     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001500100325     h dftactgrp(*no)
001600091223     h alwnull(*inputonly)
001700091223
001800091223       //--------------------------------------------------------------
001900121106       // Dichiarazione file.
002000091223       //--------------------------------------------------------------
002100100112
002200091223
002300091223       //--------------------------------------------------------------
002400121106       // Definizione costanti.
002500091223       //--------------------------------------------------------------
002600100302
002700091223
002800091223       //--------------------------------------------------------------
002900121106       // Definizione schiere.
003000091223       //--------------------------------------------------------------
003100091223
003200091223
003300091223       //--------------------------------------------------------------
003400121106       // Definizione aree dati.
003500091223       //--------------------------------------------------------------
003600091223
003700091223
003800091223       //--------------------------------------------------------------
003900121106       // Definizione strutture dati.
004000091223       //--------------------------------------------------------------
004100110516
004200121106       // - Status
004300110516     d Psds           sds
004400110516     d   SDSpgm          *proc
004500121106     d   JobName             244    253                                         Job name
004600121106     d   JobUser             254    263                                         User name
004700121106     d   JobNumber           264    269s 0                                      Job number
004800121106
004900091223       //--------------------------------------------------------------
005000121106       // Definizione variabili.
005100091223       //--------------------------------------------------------------
005200091223
005300121106       // - Parametri ricevuti:
005400140630     d pIn_DatDa       s              8s 0
005500140630     d pIn_DatA        s              8s 0
005600140630     d pIn_Ksc         s              8
005700140630     d pIn_Tip         s              2
005800140630     d pIn_Perc        s              3s 0
005900140702     d pIn_Log         s              3s 0
006000140702     d pIn_Modo        s              1
006100140630     d pOut_Esito      s              2  0
006200091223
006300121106       // Stringa SQL da eseguire
006400100325     d wSQL            s           2048    inz  varying
006500121106
006600121106       // Campi di comodo
006700140630     d wDate           s              8  0 inz
006800140630     d wWhere          s              1    inz
006900091223
007000091223       //--------------------------------------------------------------
007100121106       // Definizione prototipi procedure.
007200091223       //--------------------------------------------------------------
007300091223
007400140630
007500091223       //--------------------------------------------------------------
007600121106       // Definizione key-list.
007700091223       //--------------------------------------------------------------
007800091223
007900091223
008000091223       //--------------------------------------------------------------
008100121107       // Definizione parametri procedura.
008200091223       //--------------------------------------------------------------
008300091223
008400091223     c     *Entry        plist
008500140630     c                   parm                    pIn_DatDa
008600140630     c                   parm                    pIn_DatA
008700140630     c                   parm                    pIn_Ksc
008800140630     c                   parm                    pIn_Tip
008900140630     c                   parm                    pIn_Perc
009000140702     c                   parm                    pIn_Log
009100140702     c                   parm                    pIn_Modo
009200140630     c                   parm                    pOut_Esito
009300091223
009400091223      /free
009500091223
009600091223       //--------------------------------------------------------------
009700121106       // M A I N - L I N E
009800091223       //--------------------------------------------------------------
009900091223
010000121106       // Operazioni iniziali?
010100091223       exsr sr_RoutInz;
010200140630
010300140630       // Estrazione dati
010400140630       exsr exeExtract;
010500091223
010600121106       // Operazioni finali?
010700091223       exsr sr_RoutEnd;
010800091223
010900091223       //--------------------------------------------------------------
011000121107       // Operazioni iniziali.
011100091223       //--------------------------------------------------------------
011200091223       BEGSR  sr_RoutInz;
011300091223
011400091223         exec sql  set option  dynusrprf = *owner,
011500121107                               closqlcsr = *endmod;
011600091223
011700091223         *inLR = *on;
011800100112
011900121108         // inizializzo i dati di output
012000121108         clear pOut_Esito;
012100121108
012200121108         // Reperimento data odierna (fmt aaaa/mm/gg)
012300121108         wDate = %dec( %date() );
012400091223
012500091223       ENDSR;
012600110523
012700110523       //--------------------------------------------------------------
012800140630       // Estrazione
012900110523       //--------------------------------------------------------------
013000140630       BEGSR  ExeExtract;
013100110523
013200140701         // cancello i file di appoggio in QTEMP (potrebbero non esistere, per cui non testo
013300140701         // l'SQLCOD
013400140630         wSQL = 'drop   table QTEMP/WFLUE00F';
013500121107         exec sql   execute immediate  :wSQL;
013600140630
013700140701         // creo il file di appoggio in QTEMP
013800140630         wSQL = 'CREATE TABLE QTEMP/WFLUE00F +
013900140630                 (LUEKSC CHAR (8 ) NOT NULL WITH DEFAULT, +
014000140630                 LUETIP CHAR (2 ) NOT NULL WITH DEFAULT, +
014100140707                 LUECNTLOG INTEGER NOT NULL WITH DEFAULT, +
014200140707                 LUECNTERR INTEGER NOT NULL WITH DEFAULT) +
014300140701                 RCDFMT WFLUE000';
014400140630         exec sql   execute immediate  :wSQL;
014500121107
014600140630         // preparazione estrazione
014700140630         // base è l'SQL su TIVLR che estrae per i parametri indicati
014800140630         wSQL = 'with base as ( +
014900140630                 select +
015000140630                 * +
015100140630                 from TIVLR00F ';
015200140630         // aggiungo le eventuali WHERE
015300140630         wWhere = *off;
015400140630         //periodo
015500140630         if pIn_DatDa <> 0;
015600140630           if wWhere = *off;
015700140701             wSQL = %trimr(wSQL) + ' where';
015800140630             wWhere = *oN;
015900140701           else;
016000140701             wSQL = %trimr(wSQL) + ' and';
016100140701           endif;
016200140630           wSQL = %trimr(wSQL) + ' VLRDAT between ' +
016300140630                  %char(pIn_DatDa) + ' and ' + %char(pIn_DatA);
016400140630         endif;
016500140630         //cliente
016600140630         if pIn_Ksc <> *blank;
016700140630           if wWhere = *off;
016800140630             wSQL = %trimr(wSQL) + '  where';
016900140630             wWhere = *oN;
017000140701           else;
017100140701             wSQL = %trimr(wSQL) + ' and';
017200140630           endif;
017300140630           wSQL = %trimr(wSQL) + ' VLRKSC = ''' +
017400140630                  pIn_Ksc + '''';
017500140630         endif;
017600140630         //tipo file
017700140630         if pIn_Tip <> *blank;
017800140630           if wWhere = *off;
017900140630             wSQL = %trimr(wSQL) + '  where';
018000140630             wWhere = *oN;
018100140701           else;
018200140701             wSQL = %trimr(wSQL) + ' and';
018300140630           endif;
018400140630           wSQL = %trimr(wSQL) + ' VLRTIP = ''' +
018500140630                  pIn_Tip + '''';
018600140630         endif;
018700140630         wSQL = %trimr(wSQL) + ' ),';
018800140630         // totale è il conteggio di tutti i log raggruppati per cliente/tipo file
018900140630         wSQL = %trimr(wSQL) + ' totale as (+
019000140630                select VLRKSC, VLRTIP, count(*), 0 from base +
019100140630                group by VLRKSC, VLRTIP';
019200140630         wSQL = %trimr(wSQL) + ' ),';
019300140630         // errati è il conteggio dei log errati raggruppati per cliente/tipo file
019400140630         wSQL = %trimr(wSQL) + ' errati as (+
019500140630                select VLRKSC, VLRTIP, 0, count(*) from base +
019600140701                where VLRFLG=''2'' and substr(VLRMSG , 1 , 5) <> ''Error'' +
019700140630                group by VLRKSC, VLRTIP';
019800140701         wSQL = %trimr(wSQL) + ' ) ';
019900140630         // inserisco risultati nel file di appoggio
020000140701         wSQL = 'insert into WFLUE00F ' +
020100140701                %trimr(wSQL) +
020200140701                ' select * from totale +
020300140630                union +
020400140630                select * from errati';
020500140630         exec sql   execute immediate  :wSQL;
020600121106
020700121106       ENDSR;
020800121106
020900091223       //--------------------------------------------------------------
021000121107       // Operazioni finali.
021100091223       //--------------------------------------------------------------
021200091223       BEGSR  sr_RoutEnd;
021300091223
021400121107         // Chiusura pgm
021500091223         return;
021600091223
021700091223       ENDSR;
021800091223
021900091223      /end-free
