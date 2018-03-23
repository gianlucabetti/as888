000100091228       //==============================================================
000200140630       // Estrazione Log Upload in errore
000300091228       //==============================================================
000400140703
000500140904      * per compilare questo pgm bisogna avere in lista di librerie il file WFLDE00F che viene,
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
005800140904     d pIn_ISV         s              2
005900140702     d pIn_Modo        s              1
006000141015     d pOut_Esito      s              2s 0
006100091223
006200121106       // Stringa SQL da eseguire
006300100325     d wSQL            s           2048    inz  varying
006400121106
006500121106       // Campi di comodo
006600140630     d wDate           s              8  0 inz
006700140630     d wWhere          s              1    inz
006800091223
006900091223       //--------------------------------------------------------------
007000121106       // Definizione prototipi procedure.
007100091223       //--------------------------------------------------------------
007200091223
007300140630
007400091223       //--------------------------------------------------------------
007500121106       // Definizione key-list.
007600091223       //--------------------------------------------------------------
007700091223
007800091223
007900091223       //--------------------------------------------------------------
008000121107       // Definizione parametri procedura.
008100091223       //--------------------------------------------------------------
008200091223
008300091223     c     *Entry        plist
008400140630     c                   parm                    pIn_DatDa
008500140630     c                   parm                    pIn_DatA
008600140630     c                   parm                    pIn_Ksc
008700140630     c                   parm                    pIn_Tip
008800140904     c                   parm                    pIn_ISV
008900140702     c                   parm                    pIn_Modo
009000140630     c                   parm                    pOut_Esito
009100091223
009200091223      /free
009300091223
009400091223       //--------------------------------------------------------------
009500121106       // M A I N - L I N E
009600091223       //--------------------------------------------------------------
009700091223
009800121106       // Operazioni iniziali?
009900091223       exsr sr_RoutInz;
010000140630
010100140630       // Estrazione dati
010200140630       exsr exeExtract;
010300091223
010400121106       // Operazioni finali?
010500091223       exsr sr_RoutEnd;
010600091223
010700091223       //--------------------------------------------------------------
010800121107       // Operazioni iniziali.
010900091223       //--------------------------------------------------------------
011000091223       BEGSR  sr_RoutInz;
011100091223
011200091223         exec sql  set option  dynusrprf = *owner,
011300121107                               closqlcsr = *endmod;
011400091223
011500091223         *inLR = *on;
011600100112
011700121108         // inizializzo i dati di output
011800121108         clear pOut_Esito;
011900121108
012000121108         // Reperimento data odierna (fmt aaaa/mm/gg)
012100121108         wDate = %dec( %date() );
012200091223
012300091223       ENDSR;
012400110523
012500110523       //--------------------------------------------------------------
012600140630       // Estrazione
012700110523       //--------------------------------------------------------------
012800140630       BEGSR  ExeExtract;
012900110523
013000140701         // cancello i file di appoggio in QTEMP (potrebbero non esistere, per cui non testo
013100140701         // l'SQLCOD
013200140904         wSQL = 'drop   table QTEMP/WFLDE00F';
013300121107         exec sql   execute immediate  :wSQL;
013400140630
013500140701         // creo il file di appoggio in QTEMP
013600140904         wSQL = 'CREATE TABLE QTEMP/WFLDE00F +
013700140904                 (LDEKSC CHAR (8 ) NOT NULL WITH DEFAULT, +
013800140904                 LDEISV CHAR (2 ) NOT NULL WITH DEFAULT, +
013900140908                 LDETIP CHAR (2 ) NOT NULL WITH DEFAULT, +
014000140904                 LDETIPERR CHAR (1 ) NOT NULL WITH DEFAULT, +
014100140904                 LDECNTERR INTEGER NOT NULL WITH DEFAULT) +
014200140904                 RCDFMT WFLDE000';
014300140630         exec sql   execute immediate  :wSQL;
014400121107
014500140630         // preparazione estrazione
014600140904         // base è l'SQL su TIVLT che estrae per i parametri indicati
014700140630         wSQL = 'with base as ( +
014800140630                 select +
014900140630                 * +
015000140904                 from TIVLT00F ';
015100140630         // aggiungo le eventuali WHERE
015200140630         wWhere = *off;
015300140630         //periodo
015400140630         if pIn_DatDa <> 0;
015500140630           if wWhere = *off;
015600140701             wSQL = %trimr(wSQL) + ' where';
015700140630             wWhere = *oN;
015800140701           else;
015900140701             wSQL = %trimr(wSQL) + ' and';
016000140701           endif;
016100140904           wSQL = %trimr(wSQL) + ' VLTDAT between ' +
016200140630                  %char(pIn_DatDa) + ' and ' + %char(pIn_DatA);
016300140630         endif;
016400140630         //cliente
016500140630         if pIn_Ksc <> *blank;
016600140630           if wWhere = *off;
016700140630             wSQL = %trimr(wSQL) + '  where';
016800140630             wWhere = *oN;
016900140701           else;
017000140701             wSQL = %trimr(wSQL) + ' and';
017100140630           endif;
017200140904           wSQL = %trimr(wSQL) + ' VLTKSC = ''' +
017300140630                  pIn_Ksc + '''';
017400140630         endif;
017500140630         //tipo file
017600140630         if pIn_Tip <> *blank;
017700140630           if wWhere = *off;
017800140630             wSQL = %trimr(wSQL) + '  where';
017900140630             wWhere = *oN;
018000140701           else;
018100140701             wSQL = %trimr(wSQL) + ' and';
018200140630           endif;
018300140904           wSQL = %trimr(wSQL) + ' VLTTIP = ''' +
018400140630                  pIn_Tip + '''';
018500140630         endif;
018600140904         //tipo servizio
018700140904         if pIn_ISV <> *blank;
018800140904           if wWhere = *off;
018900140904             wSQL = %trimr(wSQL) + '  where';
019000140904             wWhere = *oN;
019100140904           else;
019200140904             wSQL = %trimr(wSQL) + ' and';
019300140904           endif;
019400140904           wSQL = %trimr(wSQL) + ' VLTISV = ''' +
019500140904                  pIn_ISV + '''';
019600140904         endif;
019700140904         wSQL = %trimr(wSQL) + ' ),';
019800140904
019900140904         // errati1 è il conteggio dei log errati raggruppati per cliente/tipo servizio
020000140904         // per l'errore 1
020100140904         wSQL = %trimr(wSQL) + ' errati1 as (+
020200140910                select VLTKSC, VLTTIP, VLTISV, ''1'', count(*) from base +
020300140904                where VLTSTS in (''2'', ''4'') +
020400140908                and (VLTMSG like ''%Cliente non%'') +
020500140910                group by VLTKSC, VLTTIP, VLTISV';
020600140904         wSQL = %trimr(wSQL) + ' ),';
020700140904
020800140904         // errati2 è il conteggio dei log errati raggruppati per cliente/tipo file
020900140904         // per l'errore 2
021000140904         wSQL = %trimr(wSQL) + ' errati2 as (+
021100140910                select VLTKSC, VLTTIP, VLTISV, ''2'', count(*) from base +
021200140904                where VLTSTS in (''2'', ''4'') +
021300140908                and (VLTMSG like ''%Tipo file non%'') +
021400140910                group by VLTKSC, VLTTIP, VLTISV';
021500140904         wSQL = %trimr(wSQL) + ' ) ';
021600140904
021700140630         // inserisco risultati nel file di appoggio
021800140904         wSQL = 'insert into WFLDE00F ' +
021900140701                %trimr(wSQL) +
022000140904                ' select * from errati1 +
022100140904                union +
022200140904                select * from errati2';
022300140630         exec sql   execute immediate  :wSQL;
022400121106
022500121106       ENDSR;
022600121106
022700091223       //--------------------------------------------------------------
022800121107       // Operazioni finali.
022900091223       //--------------------------------------------------------------
023000091223       BEGSR  sr_RoutEnd;
023100091223
023200121107         // Chiusura pgm
023300091223         return;
023400091223
023500091223       ENDSR;
023600091223
023700091223      /end-free
