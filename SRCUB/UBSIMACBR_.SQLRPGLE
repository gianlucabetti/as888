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
005200130108       // - Risultati
005300130108     d Results         ds
005400130109     d  a_cost                        1
005500130109     d  a_0NRSPE                           like(pOut_0NRSPE)
005600130108     d  a_0MDCOL                           like(pOut_0MDCOL)
005700130108     d  a_0MDPES                           like(pOut_0MDPES)
005800130108     d  a_0MDVOL                           like(pOut_0MDVOL)
005900130109
006000130109      *Ds X campi null
006100130109     D BinaDS          S              4B 0 Dim(24)
006200091223
006300091223       //--------------------------------------------------------------
006400121106       // Definizione variabili.
006500091223       //--------------------------------------------------------------
006600091223
006700121106       // - Parametri ricevuti:
006800130108     d pIn_CCM         s              7s 0
006900130108     d pIn_DA          s              8s 0
007000130108     d pIn_A           s              8s 0
007100130108     d pIn_BSE         s              1
007200130108     d pIn_Stp         s              1
007300130109     d pOut_Esito      s              9b 0
007400130108     d pOut_0NRSPE     s              8s 0
007500130108     d pOut_0MDCOL     s              7s 2
007600130108     d pOut_0MDPES     s              9s 3
007700130108     d pOut_0MDVOL     s              5s 3
007800130108     d pOut_1NRSPE     s              8s 0
007900130108     d pOut_1MDCOL     s              7s 2
008000130108     d pOut_1MDPES     s              9s 3
008100130108     d pOut_1MDVOL     s              5s 3
008200130108     d pOut_2NRSPE     s              8s 0
008300130108     d pOut_2MDCOL     s              7s 2
008400130108     d pOut_2MDPES     s              9s 3
008500130108     d pOut_2MDVOL     s              5s 3
008600091223
008700121106       // Stringa SQL da eseguire
008800130108     d wSQL            s           5000    inz  varying
008900121106
009000121106       // Campi di comodo
009100121106     d wTable          s             10
009200121106     d wInit           s              3
009300121106     d wDate           s              8  0 inz
009400121107     d wStart          s             10
009500121107     d wEnd            s             10
009600121107     d wErroreC5       s              1
009700121107     d count           s              7s 0 inz
009800121108     d wLib            s             11
009900130109     d i               s              3s 0
010000091223
010100091223       //--------------------------------------------------------------
010200121106       // Definizione prototipi procedure.
010300091223       //--------------------------------------------------------------
010400091223
010500091223       //--------------------------------------------------------------
010600121106       // Definizione key-list.
010700091223       //--------------------------------------------------------------
010800091223
010900091223
011000091223       //--------------------------------------------------------------
011100121107       // Definizione parametri procedura.
011200091223       //--------------------------------------------------------------
011300091223
011400091223     c     *Entry        plist
011500130108     c                   parm                    pIn_CCM
011600130108     c                   parm                    pIn_DA
011700130108     c                   parm                    pIn_A
011800130108     c                   parm                    pIn_BSE
011900130108     c                   parm                    pIn_Stp
012000130108     c                   parm                    pOut_Esito
012100130108     c                   parm                    pOut_0NRSPE
012200130108     c                   parm                    pOut_0MDCOL
012300130108     c                   parm                    pOut_0MDPES
012400130108     c                   parm                    pOut_0MDVOL
012500130108     c                   parm                    pOut_1NRSPE
012600130108     c                   parm                    pOut_1MDCOL
012700130108     c                   parm                    pOut_1MDPES
012800130108     c                   parm                    pOut_1MDVOL
012900130108     c                   parm                    pOut_2NRSPE
013000130108     c                   parm                    pOut_2MDCOL
013100130108     c                   parm                    pOut_2MDPES
013200130108     c                   parm                    pOut_2MDVOL
013300091223
013400091223      /free
013500091223
013600091223       //--------------------------------------------------------------
013700121106       // M A I N - L I N E
013800091223       //--------------------------------------------------------------
013900091223
014000121106       // Operazioni iniziali?
014100091223       exsr sr_RoutInz;
014200091223
014300130108       // Simulazione
014400130108       exsr  GesSimulazioni;
014500091223
014600121106       // Operazioni finali?
014700091223       exsr sr_RoutEnd;
014800091223
014900091223       //--------------------------------------------------------------
015000121107       // Operazioni iniziali.
015100091223       //--------------------------------------------------------------
015200091223       BEGSR  sr_RoutInz;
015300091223
015400091223         exec sql  set option  dynusrprf = *owner,
015500121107                               closqlcsr = *endmod;
015600091223
015700091223         *inLR = *on;
015800100112
015900121108         // inizializzo i dati di output
016000121108         clear pOut_Esito;
016100130108         clear pOut_0NrSpe;
016200130108         clear pOut_0MdCol;
016300130108         clear pOut_0MdPes;
016400130108         clear pOut_0MdVol;
016500130108         clear pOut_1NrSpe;
016600130108         clear pOut_1MdCol;
016700130108         clear pOut_1MdPes;
016800130108         clear pOut_1MdVol;
016900130108         clear pOut_2NrSpe;
017000130108         clear pOut_2MdCol;
017100130108         clear pOut_2MdPes;
017200130108         clear pOut_2MdVol;
017300121108
017400121108         // Reperimento data odierna (fmt aaaa/mm/gg)
017500121108         wDate = %dec( %date() );
017600091223
017700091223       ENDSR;
017800110523
017900110523       //--------------------------------------------------------------
018000121107       // Gestione della singolarizzazione generica per i 4 files
018100110523       //--------------------------------------------------------------
018200130108       BEGSR  GesSimulazioni;
018300110523
018400130108         wSQL =
018500130108         'with +
018600130108         base as +
018700130108         (SELECT +
018800130108         * +
018900130108         from TITAS10F +
019000130108         where tasccm=' + %char(pIn_CCM) +
019100130108         ' and tasaas*10000+tasmgs between ' +
019200130108         %char(pIn_Da) + ' and ' + %char(pIn_A) +
019300130109         ' and tasXCO = '' ''),  +
019400130108         punt as +
019500130108         (select * +
019600130108         from base +
019700130109         where tascbo in (''1'', ''2'', ''4'', ''6'') +
019800130109         ), +
019900130109         sim2p1 as ( +
020000130109         select 1 as count, +
020100130109         sum(tasncl) as ncl, sum(taspkb) as pkb, sum(tasvlb) as vlb +
020200130109         from titas10f +
020300130109         where tasccm=' + %char(pIn_CCM) +
020400130109         ' and tasaas*10000+tasmgs between ' +
020500130109         %char(pIn_Da) + ' and ' + %char(pIn_A) +
020600130109         ' and tasXCO = '' '' +
020700130109         and tascbo in (''1'', ''2'') +
020800130109         group by tasaas*10000+tasmgs, +
020900130109         tasCCM, tasRSD, tasIND, tasCAD, tasLOD, tasPRD, tasNZD, tasLNA, +
021000130109         tasZNC, tascbo, tasTSP, tasCTR, tasGC1, tasGC2, tasFFD, tasDCR, +
021100130109         tasTCR, tasHCR, tasGMA, tasGGA, tasGVA, tasFTC, tasTC2, tasVAS +
021200130109         ), +
021300130109         sim2p2 as ( +
021400130109         select 1 as count, +
021500130109         tasncl as ncl, taspkb as pkb, tasvlb as vlb +
021600130109         from titas10f +
021700130109         where tasccm=' + %char(pIn_CCM) +
021800130109         ' and tasaas*10000+tasmgs between ' +
021900130109         %char(pIn_Da) + ' and ' + %char(pIn_A) +
022000130109         ' and tasXCO = '' '' +
022100130109         and tascbo in (''4'', ''6'') +
022200130109         ), +
022300130109         sim2 as ( +
022400130109         SELECT sum(COUNT) as count, +
022500130109         sum(NCL) as NCL, +
022600130109         sum(PKB) as pkb, +
022700130109         sum(VLB) as vlb FROM sim2p1 +
022800130109         union +
022900130109         SELECT sum(COUNT) as count, +
023000130109         sum(NCL) as NCL, +
023100130109         sum(PKB) as pkb, +
023200130109         sum(VLB) as vlb FROM sim2p2 +
023300130109         ), +
023400130109         sim4 as +
023500130109         (select +
023600130109         sum(tasncl) as ncl, sum(taspkb) as pkb, sum(tasvlb) as vlb +
023700130109         from base +
023800130109         where tascbo in (''1'', ''2'', ''4'', ''6'') +
023900130109         group by tasaas*10000+tasmgs, +
024000130109         tasCCM, tasRSD, tasIND, tasCAD, tasLOD, tasPRD, tasNZD, tasLNA, +
024100130109         tasZNC, tascbo, tasTSP, tasCTR, tasGC1, tasGC2, tasFFD, tasDCR, +
024200130109         tasTCR, tasHCR, tasGMA, tasGGA, tasGVA, tasFTC, tasTC2, tasVAS +
024300130109         )';
024400130109
024500130109         wSQL = %trim(wSQL) +
024600130109         'select +
024700130109         ''1'', count(*), +
024800130108         dec( dec(sum(punt.tasncl), 10 , 3)/dec(count(*) , 10 , 3) , 10 , 3), +
024900130108         dec( dec(sum(punt.taspkb), 10 , 3)/dec(count(*) , 10 , 3) , 10 , 3), +
025000130108         dec( dec(sum(punt.tasvlb), 10 , 3)/dec(count(*) , 10 , 3) , 10 , 3) +
025100130109         from punt  +
025200130109         union +
025300130109         select +
025400130109         ''2'', sum(count), +
025500130109         dec( dec(sum(sim2.ncl) , 10 , 3)/dec(sum(count) , 10 , 3) , 10 , 3), +
025600130109         dec( dec(sum(sim2.pkb) , 10 , 3)/dec(sum(count) , 10 , 3) , 10 , 3), +
025700130109         dec( dec(sum(sim2.vlb) , 10 , 3)/dec(sum(count) , 10 , 3) , 10 , 3) +
025800130109         from sim2 +
025900130109         union +
026000130109         select +
026100130109         ''3'', count(*), +
026200130109         dec( dec(sum(sim4.ncl) , 10 , 3)/dec(count(*) , 10 , 3) , 10 , 3), +
026300130109         dec( dec(sum(sim4.pkb) , 10 , 3)/dec(count(*) , 10 , 3) , 10 , 3), +
026400130109         dec( dec(sum(sim4.vlb) , 10 , 3)/dec(count(*) , 10 , 3) , 10 , 3) +
026500130109         from sim4' ;
026600130108
026700121107         // Dichiarazione cursore
026800121107         exec sql   prepare S0   from :wSQL;
026900121107         exec sql   declare C0   cursor   for S0;
027000121107         // Apertura del cursore
027100121107         exec sql   open C0;
027200130109         // Lettura cursore
027300130109         // ciclo per 3 volte dato che sono 3 le union
027400130109         for i = 1 to 3;
027500130109           exec sql   fetch next   from C0   into :Results :BinaDS;
027600130109           pOut_Esito = SQLCOD;
027700130109           // se c'è un errore (negativo) o non c'è nulla (100), fine ciclo
027800130109           if pOut_Esito < 0 or pOut_Esito = 100;
027900130109             leave;
028000130109           else;
028100130109             // sposto i risultati nei parametri di output a seconda del ciclo che sto facendo
028200130109             select;
028300130109             when i = 1;
028400130109               pOut_0NrSpe = a_0NrSpe;
028500130109               pOut_0MdCol = a_0MdCol;
028600130109               pOut_0MdPes = a_0MdPes;
028700130109               pOut_0MdVol = a_0MdVol;
028800130109             when i = 2;
028900130109               pOut_1NrSpe = a_0NrSpe;
029000130109               pOut_1MdCol = a_0MdCol;
029100130109               pOut_1MdPes = a_0MdPes;
029200130109               pOut_1MdVol = a_0MdVol;
029300130109             when i = 3;
029400130109               pOut_2NrSpe = a_0NrSpe;
029500130109               pOut_2MdCol = a_0MdCol;
029600130109               pOut_2MdPes = a_0MdPes;
029700130109               pOut_2MdVol = a_0MdVol;
029800130109             endsl;
029900130109           endif;
030000130109         endfor;
030100130109         // Chiusura del cursore
030200121107         exec sql   close C0;
030300121106
030400121106       ENDSR;
030500121106
030600091223       //--------------------------------------------------------------
030700121107       // Operazioni finali.
030800091223       //--------------------------------------------------------------
030900091223       BEGSR  sr_RoutEnd;
031000130109
031100130109         // Chiusura pgm
031200130109         return;
031300091223
031400091223       ENDSR;
031500091223
031600091223      /end-free
