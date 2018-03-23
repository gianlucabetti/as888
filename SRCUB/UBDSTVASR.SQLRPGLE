000100091228       //==============================================================
000200121108       // Pgm di utility per singolarizzare rcd Nuplicati su file *VAB/D/T/X
000300091228       //==============================================================
000400091228
000500091228       //--------------------------------------------------------------
000600121106       // Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700091228       //--------------------------------------------------------------
000800091228
000900100325     /*PRM dbgview(*source)
001000091223     /*END
001100091228
001200091228       //--------------------------------------------------------------
001300121106       // Specifiche di controllo.
001400091228       //--------------------------------------------------------------
001500091223
001600091223     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700100325     h dftactgrp(*no)
001800091223     h alwnull(*inputonly)
001900100325     h bnddir('UBBNDDIR')
002000091223
002100091223       //--------------------------------------------------------------
002200121106       // Dichiarazione file.
002300091223       //--------------------------------------------------------------
002400100112
002500091223
002600091223       //--------------------------------------------------------------
002700121106       // Definizione costanti.
002800091223       //--------------------------------------------------------------
002900100302
003000091223
003100091223       //--------------------------------------------------------------
003200121106       // Definizione schiere.
003300091223       //--------------------------------------------------------------
003400091223
003500091223
003600091223       //--------------------------------------------------------------
003700121106       // Definizione aree dati.
003800091223       //--------------------------------------------------------------
003900091223
004000091223
004100091223       //--------------------------------------------------------------
004200121106       // Definizione strutture dati.
004300091223       //--------------------------------------------------------------
004400110516
004500121106       // - Status
004600110516     d Psds           sds
004700110516     d   SDSpgm          *proc
004800121106     d   JobName             244    253                                         Job name
004900121106     d   JobUser             254    263                                         User name
005000121106     d   JobNumber           264    269s 0                                      Job number
005100121106
005200121106       // - Status
005300121106     d EDIVABDs      e ds                  extname(EDIVAB0F)
005400091223
005500091223       //--------------------------------------------------------------
005600121106       // Definizione variabili.
005700091223       //--------------------------------------------------------------
005800091223
005900121106       // - Parametri ricevuti:
006000121106     d pIn_FGS         s                   like(VABFGS)
006100121106     d pIn_CCM         s                   like(VABCCM)
006200121106     d pIn_CMR         s                   like(VABCMR)
006300121106     d pIn_CNT         s                   like(VABCNT)
006400121106       // sono 4 flag in un solo campo
006500121108       // 1° char = 'X' = singolarizzo *VAB
006600121108       // 2° char = 'X' = singolarizzo *VAD
006700121108       // 3° char = 'X' = singolarizzo *VAT
006800121108       // 4° char = 'X' = singolarizzo *VAX
006900121106     d pIn_Flags       s              4a
007000121107       // 0 = tutto bene
007100121107       // 1 = CREATE X in errore
007200121107       // 2 = CREATE Y in errore
007300121107       // 3 = DELETE in errore
007400121107       // 4 = INSERT in errore
007500121107       // 5 = CPYF   in errore
007600121108       // 6 = non ci sono rcd per i filtri scritti
007700121108     d pIn_Lib         s             10a
007800121108     d pIn_TpFile      s              5a
007900121107     d pOut_Esito      s              2s 0
008000121107     d pOut_MotivoErr  s              5s 0
008100091223
008200121106       // Stringa SQL da eseguire
008300100325     d wSQL            s           2048    inz  varying
008400121106
008500121106       // Campi di comodo
008600121106     d wTable          s             10
008700121106     d wInit           s              3
008800121106     d wDate           s              8  0 inz
008900121107     d wStart          s             10
009000121107     d wEnd            s             10
009100121107     d wErroreC5       s              1
009200121107     d count           s              7s 0 inz
009300121108     d wLib            s             11
009400091223
009500091223       //--------------------------------------------------------------
009600121106       // Definizione prototipi procedure.
009700091223       //--------------------------------------------------------------
009800100325
009900121108     d UBDSTVASC       pr                  extpgm('UBDSTVASC')
010000121107     d wStart                        10
010100121107     d wEnd                          10
010200121107     d wErroreC5                      1
010300091223
010400091223       //--------------------------------------------------------------
010500121106       // Definizione key-list.
010600091223       //--------------------------------------------------------------
010700091223
010800091223
010900091223       //--------------------------------------------------------------
011000121107       // Definizione parametri procedura.
011100091223       //--------------------------------------------------------------
011200091223
011300091223     c     *Entry        plist
011400121106     c                   parm                    pIn_FGS
011500121106     c                   parm                    pIn_CCM
011600121106     c                   parm                    pIn_CMR
011700121106     c                   parm                    pIn_CNT
011800121106     c                   parm                    pIn_Flags
011900121108     c                   parm                    pIn_Lib
012000121108     c                   parm                    pIn_TpFile
012100121107     c                   parm                    pOut_Esito
012200121107     c                   parm                    pOut_MotivoErr
012300091223
012400091223      /free
012500091223
012600091223       //--------------------------------------------------------------
012700121106       // M A I N - L I N E
012800091223       //--------------------------------------------------------------
012900091223
013000121106       // Operazioni iniziali?
013100091223       exsr sr_RoutInz;
013200091223
013300121106       // Singolarizzazioni richieste
013400121107
013500121106       // VAB
013600121106       if %subst(pIn_Flags : 1 :1)='X';
013700121108         // scelgo tra i file EDIVA* e quelli FIVA*
013800121108         select;
013900121108           when pIn_TpFile = 'EDIVA';
014000121108             wTable = 'EDIVAB0F';
014100121108           when pIn_TpFile = 'FIVA';
014200121108             wTable = 'FIVAB00F';
014300121108         endsl;
014400121106         wInit  = 'VAB';
014500121106         exsr  GesDistinct;
014600121106       endif;
014700121107       // VAD
014800121107       if %subst(pIn_Flags : 2 :1)='X';
014900121108         // scelgo tra i file EDIVA* e quelli FIVA*
015000121108         select;
015100121108           when pIn_TpFile = 'EDIVA';
015200121108             wTable = 'EDIVAD0F';
015300121108           when pIn_TpFile = 'FIVA';
015400121108             wTable = 'FIVAD00F';
015500121108         endsl;
015600121107         wInit  = 'VAD';
015700121107         exsr  GesDistinct;
015800121107       endif;
015900121107       // VAT
016000121107       if %subst(pIn_Flags : 3 :1)='X';
016100121108         // scelgo tra i file EDIVA* e quelli FIVA*
016200121108         select;
016300121108           when pIn_TpFile = 'EDIVA';
016400121108             wTable = 'EDIVAT0F';
016500121108           when pIn_TpFile = 'FIVA';
016600121108             wTable = 'FIVAT00F';
016700121108         endsl;
016800121107         wInit  = 'VAT';
016900121107         exsr  GesDistinct;
017000121107       endif;
017100121107       // VAX
017200121107       if %subst(pIn_Flags : 4 :1)='X';
017300121108         // scelgo tra i file EDIVA* e quelli FIVA*
017400121108         select;
017500121108           when pIn_TpFile = 'EDIVA';
017600121108             wTable = 'EDIVAX0F';
017700121108           when pIn_TpFile = 'FIVA';
017800121108             wTable = 'FIVAX00F';
017900121108         endsl;
018000121107         wInit  = 'VAX';
018100121107         exsr  GesDistinct;
018200121107       endif;
018300091223
018400121106       // Operazioni finali?
018500091223       exsr sr_RoutEnd;
018600091223
018700091223       //--------------------------------------------------------------
018800121107       // Operazioni iniziali.
018900091223       //--------------------------------------------------------------
019000091223       BEGSR  sr_RoutInz;
019100091223
019200091223         exec sql  set option  dynusrprf = *owner,
019300121107                               closqlcsr = *endmod;
019400091223
019500091223         *inLR = *on;
019600100112
019700121108         // inizializzo i dati di output
019800121108         clear pOut_Esito;
019900121108         clear pOut_MotivoErr;
020000121108
020100121108         // Reperimento data odierna (fmt aaaa/mm/gg)
020200121108         wDate = %dec( %date() );
020300121108
020400121108         // se mi è stata passata una libreria compongo '<libreria>/'
020500121108         wLib = *blank;
020600121108         if pIn_Lib <> *blank;
020700121108           wLib = %trim(pIn_Lib) + '/';
020800121108         endif;
020900091223
021000091223       ENDSR;
021100110523
021200110523       //--------------------------------------------------------------
021300121107       // Gestione della singolarizzazione generica per i 4 files
021400110523       //--------------------------------------------------------------
021500121106       BEGSR  GesDistinct;
021600110523
021700121107         // cancello i file di appoggio in QTEMP (potrebbero non esistere, per cui non testo
021800121107         // l'SQLCOD
021900121107         wSQL = 'drop   table QTEMP/X' + %trim(wTable);
022000121107         exec sql   execute immediate  :wSQL;
022100121107         wSQL = 'drop   table QTEMP/Y' + %trim(wTable);
022200121107         exec sql   execute immediate  :wSQL;
022300121107
022400121107         // creo copia del file in QTEMP con i dati filtrati come richiesto
022500121107         wSQL = 'create table QTEMP/X' + %trim(wTable) + ' as +
022600121107                 (select ' + %trim(wTable) + '.* , rrn(' + %trim(wTable) + ') +
022700121108                  as NRR from ' + %trim(wLib) + %trim(wTable) + ' where ' +
022800121107                  %trim(wInit) + 'FGS=' + %char(pIn_FGS) + ' and ' +
022900121108                  %trim(wInit) + 'CCM=' + %char(pIn_CCM);
023000121108         if pIn_TpFile = 'EDIVA';
023100130107           //wSQL = %trim(wSQL) +
023200130107           //       ' and ' + %trim(wInit) + 'CMR=''' + pIn_CMR +  ''' and ' +
023300130107           //       %trim(wInit) + 'CNT=' + %char(pIn_CNT);
023400130107           wSQL = %trim(wSQL) +
023500130107                  ' and ' + %trim(wInit) + 'CNT=' + %char(pIn_CNT);
023600130107           if pIn_CMR <> *blank;
023700130107             wSQL = %trim(wSQL) +
023800130531                    ' and ' + %trim(wInit) + 'CMR=''' + pIn_CMR + '''';
023900130107           endif;
024000130107         endif;
024100121108         wSQL = %trim(wSQL) +
024200121106                ') with data';
024300121106         exec sql   execute immediate  :wSQL;
024400121107         // testo il risultato
024500121107         if sqlcod <> 0;
024600121107           // non c'è bisogno di rollback perché la create non è sotto transazione
024700121107           pOut_Esito = 1;
024800121107           pOut_MotivoErr = sqlcod;
024900121107           exsr sr_RoutEnd;
025000121107         endif;
025100121107
025200121107         // controllo che il file X abbia dei rcd, altrimenti non serve continuare
025300121107         wSQL = 'select count(*) from QTEMP/X' + %trim(wTable);
025400121107         // Dichiarazione cursore
025500121107         exec sql   prepare S0   from :wSQL;
025600121107         exec sql   declare C0   cursor   for S0;
025700121107         // Apertura del cursore
025800121107         exec sql   open C0;
025900121107         // Lettura cursore
026000121107         exec sql   fetch next   from C0   into :Count;
026100121107         // Chiusura del cursore
026200121107         exec sql   close C0;
026300121108         // se il conteggio da 0, esco dalla routine, ma non dal pgm (ci potrebbero essere altri
026400121108         // file da elaborare
026500121107         if count = 0;
026600121107           leavesr;
026700121107         endif;
026800121107
026900121107         // creo copia del file in QTEMP, dove scaricherò i dati senza RRN
027000121107         wSQL = 'create table QTEMP/Y' + %trim(wTable) + ' as +
027100121109                 (select ' + %trim(wTable) + '.* from ' + %trim(wLib) +
027200121108                 %trim(wTable) +
027300121107                 ') with no data';
027400121107         exec sql   execute immediate  :wSQL;
027500121107         // testo il risultato
027600121107         if sqlcod <> 0;
027700121107           // non c'è bisogno di rollback perché la create non è sotto transazione
027800121107           pOut_Esito = 2;
027900121107           pOut_MotivoErr = sqlcod;
028000121107           exsr sr_RoutEnd;
028100121107         endif;
028200121106
028300121107         // copio il file X nel file Y senza il RRN
028400121107         // (chiamo CL che esegue un CPYF *NOCHK)
028500121107         wStart = 'X' + %trim(wTable);
028600121107         wEnd   = 'Y' + %trim(wTable);
028700121108         UBDSTVASC (wStart : wEnd : wErroreC5);
028800121107         // testo il risultato
028900121107         if wErroreC5 = '1';
029000121107           // non c'è bisogno di rollback perché non c'è nulla sotto transazione
029100121107           pOut_Esito = 5;
029200121107           pOut_MotivoErr = 0;
029300121107           exsr sr_RoutEnd;
029400121107         endif;
029500121106
029600121106         // elimino sul file in buona i rcd copiati in QTEMP
029700121108         wSQL = 'delete from ' + %trim(wLib) + %trim(wTable) +
029800121108                ' where rrn(' + %trim(wtable) +
029900121107                ') in (select nrr from QTEMP/X' + %trim(wTable) + ') with RS';
030000121107         exec sql   execute immediate  :wSQL;
030100121107         // testo il risultato
030200121107         if sqlcod <> 0;
030300121107           wSQL = 'rollback';
030400121107           exec sql   execute immediate  :wSQL;
030500121107           pOut_Esito = 3;
030600121107           pOut_MotivoErr = sqlcod;
030700121107           exsr sr_RoutEnd;
030800121107         endif;
030900121106
031000121107         // scrivo sul file la distinct del file di appoggio in QTEMP senza RRN
031100121108         wSQL = 'insert into ' + %trim(wLib) + %trim(wTable) +
031200121107                ' select distinct * from QTEMP/Y' + %trim(wtable) +
031300121107                ' with RS';
031400121107         exec sql   execute immediate  :wSQL;
031500121107         // testo il risultato
031600121107         if sqlcod <> 0;
031700121107           wSQL = 'rollback';
031800121107           exec sql   execute immediate  :wSQL;
031900121107           pOut_Esito = 4;
032000121107           pOut_MotivoErr = sqlcod;
032100121107           exsr sr_RoutEnd;
032200121107         // essendo l'ultima operazione, se andata bene confermo il tutto
032300121107         else;
032400121107           wSQL = 'commit';
032500121107           exec sql   execute immediate  :wSQL;
032600121107         endif;
032700121106
032800121106       ENDSR;
032900121106
033000091223       //--------------------------------------------------------------
033100121107       // Operazioni finali.
033200091223       //--------------------------------------------------------------
033300091223       BEGSR  sr_RoutEnd;
033400091223
033500121107         // Chiusura pgm
033600091223         return;
033700091223
033800091223       ENDSR;
033900091223
034000091223      /end-free
