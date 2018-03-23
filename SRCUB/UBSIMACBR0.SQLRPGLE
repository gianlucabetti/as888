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
005400130109     d  a_0NRSPE                           like(pOut_0NRSPE)
005500130108     d  a_0MDCOL                           like(pOut_0MDCOL)
005600130108     d  a_0MDPES                           like(pOut_0MDPES)
005700130108     d  a_0MDVOL                           like(pOut_0MDVOL)
005800130109
005900130109      *Ds X campi null
006000130109     D BinaDS          S              4B 0 Dim(24)
006100091223
006200091223       //--------------------------------------------------------------
006300121106       // Definizione variabili.
006400091223       //--------------------------------------------------------------
006500091223
006600121106       // - Parametri ricevuti:
006700130108     d pIn_CCM         s              7s 0
006800130108     d pIn_DA          s              8s 0
006900130108     d pIn_A           s              8s 0
007000130108     d pIn_BSE         s              1
007100130108     d pIn_Stp         s              1
007200130109     d pOut_Esito      s              9b 0
007300130108     d pOut_0NRSPE     s              8s 0
007400130108     d pOut_0MDCOL     s              7s 2
007500130108     d pOut_0MDPES     s              9s 3
007600130108     d pOut_0MDVOL     s              5s 3
007700130108     d pOut_1NRSPE     s              8s 0
007800130108     d pOut_1MDCOL     s              7s 2
007900130108     d pOut_1MDPES     s              9s 3
008000130108     d pOut_1MDVOL     s              5s 3
008100130108     d pOut_2NRSPE     s              8s 0
008200130108     d pOut_2MDCOL     s              7s 2
008300130108     d pOut_2MDPES     s              9s 3
008400130108     d pOut_2MDVOL     s              5s 3
008500091223
008600121106       // Stringa SQL da eseguire
008700130108     d wSQL            s           5000    inz  varying
008800121106
008900121106       // Campi di comodo
009000121106     d wTable          s             10
009100121106     d wInit           s              3
009200121106     d wDate           s              8  0 inz
009300121107     d wStart          s             10
009400121107     d wEnd            s             10
009500121107     d wErroreC5       s              1
009600121107     d count           s              7s 0 inz
009700121108     d wLib            s             11
009800130109     d i               s              3s 0
009900091223
010000091223       //--------------------------------------------------------------
010100121106       // Definizione prototipi procedure.
010200091223       //--------------------------------------------------------------
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
011400130108     c                   parm                    pIn_CCM
011500130108     c                   parm                    pIn_DA
011600130108     c                   parm                    pIn_A
011700130108     c                   parm                    pIn_BSE
011800130108     c                   parm                    pIn_Stp
011900130108     c                   parm                    pOut_Esito
012000130108     c                   parm                    pOut_0NRSPE
012100130108     c                   parm                    pOut_0MDCOL
012200130108     c                   parm                    pOut_0MDPES
012300130108     c                   parm                    pOut_0MDVOL
012400130108     c                   parm                    pOut_1NRSPE
012500130108     c                   parm                    pOut_1MDCOL
012600130108     c                   parm                    pOut_1MDPES
012700130108     c                   parm                    pOut_1MDVOL
012800130108     c                   parm                    pOut_2NRSPE
012900130108     c                   parm                    pOut_2MDCOL
013000130108     c                   parm                    pOut_2MDPES
013100130108     c                   parm                    pOut_2MDVOL
013200091223
013300091223      /free
013400091223
013500091223       //--------------------------------------------------------------
013600121106       // M A I N - L I N E
013700091223       //--------------------------------------------------------------
013800091223
013900121106       // Operazioni iniziali?
014000091223       exsr sr_RoutInz;
014100091223
014200130108       // Simulazione
014300130108       exsr  GesSimulazioni;
014400091223
014500121106       // Operazioni finali?
014600091223       exsr sr_RoutEnd;
014700091223
014800091223       //--------------------------------------------------------------
014900121107       // Operazioni iniziali.
015000091223       //--------------------------------------------------------------
015100091223       BEGSR  sr_RoutInz;
015200091223
015300091223         exec sql  set option  dynusrprf = *owner,
015400121107                               closqlcsr = *endmod;
015500091223
015600091223         *inLR = *on;
015700100112
015800121108         // inizializzo i dati di output
015900121108         clear pOut_Esito;
016000130108         clear pOut_0NrSpe;
016100130108         clear pOut_0MdCol;
016200130108         clear pOut_0MdPes;
016300130108         clear pOut_0MdVol;
016400130108         clear pOut_1NrSpe;
016500130108         clear pOut_1MdCol;
016600130108         clear pOut_1MdPes;
016700130108         clear pOut_1MdVol;
016800130108         clear pOut_2NrSpe;
016900130108         clear pOut_2MdCol;
017000130108         clear pOut_2MdPes;
017100130108         clear pOut_2MdVol;
017200121108
017300121108         // Reperimento data odierna (fmt aaaa/mm/gg)
017400121108         wDate = %dec( %date() );
017500091223
017600091223       ENDSR;
017700110523
017800110523       //--------------------------------------------------------------
017900121107       // Gestione della singolarizzazione generica per i 4 files
018000110523       //--------------------------------------------------------------
018100130108       BEGSR  GesSimulazioni;
018200110523
018300130109         // ciclo per 3 volte dato che sono 3 gli statement da eseguire
018400130109         for i = 1 to 3;
018500130109           select;
018600130109           when i=1;
018700130109             wSQL =
018800130109             'with +
018900130109             base as +
019000130109             (SELECT +
019100130109             * +
019200130109             from TITAS10F +
019300130109             where tasccm=' + %char(pIn_CCM) +
019400130109             ' and tasaas*10000+tasmgs between ' +
019500130109             %char(pIn_Da) + ' and ' + %char(pIn_A) +
019600130109             ' and tasXCO = '' ''),  +
019700130109             punt as +
019800130109             (select * +
019900130109             from base +
020000130109             where tascbo in (''1'', ''2'', ''4'', ''6'') +
020100130109             ) +
020200130109             select +
020300130109             count(*), +
020400130109             dec( dec(sum(punt.tasncl) , 10 , 3)/+
020500130109             dec(count(*) , 10 , 3) , 10 , 3), +
020600130109             dec( dec(sum(punt.taspkb), 10 , 3)/+
020700130109             dec(count(*) , 10 , 3) , 10 , 3), +
020800130109             dec( dec(sum(punt.tasvlb) , 10 , 3)/+
020900130109             dec(count(*) , 10 , 3) , 10 , 3) +
021000130109             from punt';
021100130109
021200130109           when i=2;
021300130109             wSQL =
021400130109             'with +
021500130109             base as +
021600130109             (SELECT +
021700130109             * +
021800130109             from TITAS10F +
021900130109             where tasccm=' + %char(pIn_CCM) +
022000130109             ' and tasaas*10000+tasmgs between ' +
022100130109             %char(pIn_Da) + ' and ' + %char(pIn_A) +
022200130109             ' and tasXCO = '' ''),  +
022300130109             sim2p1 as ( +
022400130109             select 1 as count, +
022500130109             sum(tasncl) as ncl, sum(taspkb) as pkb, sum(tasvlb) as vlb +
022600130109             from titas10f +
022700130109             where tasccm=' + %char(pIn_CCM) +
022800130109             ' and tasaas*10000+tasmgs between ' +
022900130109             %char(pIn_Da) + ' and ' + %char(pIn_A) +
023000130109             ' and tasXCO = '' '' +
023100130109             and tascbo in (''1'', ''2'') +
023200130109             group by tasaas*10000+tasmgs, +
023300130109             tasCCM, tasRSD, tasIND, tasCAD, tasLOD, tasPRD, tasNZD, tasLNA, +
023400130109             tasZNC, tascbo, tasTSP, tasCTR, tasGC1, tasGC2, tasFFD, tasDCR, +
023500130109             tasTCR, tasHCR, tasGMA, tasGGA, tasGVA, tasFTC, tasTC2, tasVAS +
023600130109             ), +
023700130109             sim2p2 as ( +
023800130109             select 1 as count, +
023900130109             tasncl as ncl, taspkb as pkb, tasvlb as vlb +
024000130109             from titas10f +
024100130109             where tasccm=' + %char(pIn_CCM) +
024200130109             ' and tasaas*10000+tasmgs between ' +
024300130109             %char(pIn_Da) + ' and ' + %char(pIn_A) +
024400130109             ' and tasXCO = '' '' +
024500130109             and tascbo in (''4'', ''6'') +
024600130109             ), +
024700130109             sim2 as ( +
024800130109             SELECT sum(COUNT) as count, +
024900130109             sum(NCL) as NCL, +
025000130109             sum(PKB) as pkb, +
025100130109             sum(VLB) as vlb FROM sim2p1 +
025200130109             union all +
025300130109             SELECT sum(COUNT) as count, +
025400130109             sum(NCL) as NCL, +
025500130109             sum(PKB) as pkb, +
025600130109             sum(VLB) as vlb FROM sim2p2 +
025700130109             )  +
025800130109             select +
025900130109             sum(count), +
026000130109             dec( dec(sum(sim2.ncl) , 10 , 3)/+
026100130109             dec(sum(count) , 10 , 3) , 10 , 3), +
026200130109             dec( dec(sum(sim2.pkb) , 10 , 3)/+
026300130109             dec(sum(count) , 10 , 3) , 10 , 3), +
026400130109             dec( dec(sum(sim2.vlb) , 10 , 3)/+
026500130109             dec(sum(count) , 10 , 3) , 10 , 3) +
026600130109             from sim2' ;
026700130109
026800130109           when i=3;
026900130109             wSQL =
027000130109             'with +
027100130109             base as +
027200130109             (SELECT +
027300130109             * +
027400130109             from TITAS10F +
027500130109             where tasccm=' + %char(pIn_CCM) +
027600130109             ' and tasaas*10000+tasmgs between ' +
027700130109             %char(pIn_Da) + ' and ' + %char(pIn_A) +
027800130109             ' and tasXCO = '' ''),  +
027900130109             sim4 as +
028000130109             (select +
028100130109             sum(tasncl) as ncl, sum(taspkb) as pkb, sum(tasvlb) as vlb +
028200130109             from base +
028300130109             where tascbo in (''1'', ''2'', ''4'', ''6'') +
028400130109             group by tasaas*10000+tasmgs, +
028500130109             tasCCM, tasRSD, tasIND, tasCAD, tasLOD, tasPRD, tasNZD, tasLNA, +
028600130109             tasZNC, tascbo, tasTSP, tasCTR, tasGC1, tasGC2, tasFFD, tasDCR, +
028700130109             tasTCR, tasHCR, tasGMA, tasGGA, tasGVA, tasFTC, tasTC2, tasVAS +
028800130109             ) +
028900130109             select +
029000130109             count(*), +
029100130109             dec( dec(sum(sim4.ncl) , 10 , 3)/+
029200130109             dec(count(*) , 10 , 3) , 10 , 3), +
029300130109             dec( dec(sum(sim4.pkb) , 10 , 3)/+
029400130109             dec(count(*) , 10 , 3) , 10 , 3), +
029500130109             dec( dec(sum(sim4.vlb) , 10 , 3)/+
029600130109             dec(count(*) , 10 , 3) , 10 , 3) +
029700130109             from sim4' ;
029800130109           endsl;
029900130108
030000130109           // Dichiarazione cursore
030100130109           exec sql   prepare S0   from :wSQL;
030200130109           exec sql   declare C0   cursor   for S0;
030300130109           // Apertura del cursore
030400130109           exec sql   open C0;
030500130109           // Lettura cursore
030600130109           exec sql   fetch next   from C0   into :Results;
030700130109           pOut_Esito = SQLCOD;
030800130109           // se c'è un errore (negativo) o non c'è nulla (100), fine ciclo
030900130109           if pOut_Esito < 0 or pOut_Esito = 100;
031000130109           //leave;
031100130109           else;
031200130109             // sposto i risultati nei parametri di output a seconda del ciclo che sto facendo
031300130109             select;
031400130109             when i = 1;
031500130109               pOut_0NrSpe = a_0NrSpe;
031600130109               pOut_0MdCol = a_0MdCol;
031700130109               pOut_0MdPes = a_0MdPes;
031800130109               pOut_0MdVol = a_0MdVol;
031900130109             when i = 2;
032000130109               pOut_1NrSpe = a_0NrSpe;
032100130109               pOut_1MdCol = a_0MdCol;
032200130109               pOut_1MdPes = a_0MdPes;
032300130109               pOut_1MdVol = a_0MdVol;
032400130109             when i = 3;
032500130109               pOut_2NrSpe = a_0NrSpe;
032600130109               pOut_2MdCol = a_0MdCol;
032700130109               pOut_2MdPes = a_0MdPes;
032800130109               pOut_2MdVol = a_0MdVol;
032900130109             endsl;
033000130109           endif;
033100130109           // Chiusura del cursore
033200130109           exec sql   close C0;
033300130109         endfor;
033400121106
033500121106       ENDSR;
033600121106
033700091223       //--------------------------------------------------------------
033800121107       // Operazioni finali.
033900091223       //--------------------------------------------------------------
034000091223       BEGSR  sr_RoutEnd;
034100130109
034200130109         // Chiusura pgm
034300130109         return;
034400091223
034500091223       ENDSR;
034600091223
034700091223      /end-free
