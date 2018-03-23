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
007100130110     d pOut_Esito0     s              9b 0
007200130110     d pOut_Esito1     s              9b 0
007300130110     d pOut_Esito2     s              9b 0
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
011900130110     c                   parm                    pOut_Esito0
012000130110     c                   parm                    pOut_Esito1
012100130110     c                   parm                    pOut_Esito2
012200130108     c                   parm                    pOut_0NRSPE
012300130108     c                   parm                    pOut_0MDCOL
012400130108     c                   parm                    pOut_0MDPES
012500130108     c                   parm                    pOut_0MDVOL
012600130108     c                   parm                    pOut_1NRSPE
012700130108     c                   parm                    pOut_1MDCOL
012800130108     c                   parm                    pOut_1MDPES
012900130108     c                   parm                    pOut_1MDVOL
013000130108     c                   parm                    pOut_2NRSPE
013100130108     c                   parm                    pOut_2MDCOL
013200130108     c                   parm                    pOut_2MDPES
013300130108     c                   parm                    pOut_2MDVOL
013400091223
013500091223      /free
013600091223
013700091223       //--------------------------------------------------------------
013800121106       // M A I N - L I N E
013900091223       //--------------------------------------------------------------
014000091223
014100121106       // Operazioni iniziali?
014200091223       exsr sr_RoutInz;
014300091223
014400130108       // Simulazione
014500130108       exsr  GesSimulazioni;
014600091223
014700121106       // Operazioni finali?
014800091223       exsr sr_RoutEnd;
014900091223
015000091223       //--------------------------------------------------------------
015100121107       // Operazioni iniziali.
015200091223       //--------------------------------------------------------------
015300091223       BEGSR  sr_RoutInz;
015400091223
015500091223         exec sql  set option  dynusrprf = *owner,
015600121107                               closqlcsr = *endmod;
015700091223
015800091223         *inLR = *on;
015900100112
016000121108         // inizializzo i dati di output
016100130110         clear pOut_Esito0;
016200130110         clear pOut_Esito1;
016300130110         clear pOut_Esito2;
016400130108         clear pOut_0NrSpe;
016500130108         clear pOut_0MdCol;
016600130108         clear pOut_0MdPes;
016700130108         clear pOut_0MdVol;
016800130108         clear pOut_1NrSpe;
016900130108         clear pOut_1MdCol;
017000130108         clear pOut_1MdPes;
017100130108         clear pOut_1MdVol;
017200130108         clear pOut_2NrSpe;
017300130108         clear pOut_2MdCol;
017400130108         clear pOut_2MdPes;
017500130108         clear pOut_2MdVol;
017600121108
017700121108         // Reperimento data odierna (fmt aaaa/mm/gg)
017800121108         wDate = %dec( %date() );
017900091223
018000091223       ENDSR;
018100110523
018200110523       //--------------------------------------------------------------
018300121107       // Gestione della singolarizzazione generica per i 4 files
018400110523       //--------------------------------------------------------------
018500130108       BEGSR  GesSimulazioni;
018600110523
018700130109         // ciclo per 3 volte dato che sono 3 gli statement da eseguire
018800130109         for i = 1 to 3;
018900130109           select;
019000130109           when i=1;
019100130109             wSQL =
019200130109             'with +
019300130109             base as +
019400130109             (SELECT +
019500130109             * +
019600130109             from TITAS10F +
019700130109             where tasccm=' + %char(pIn_CCM) +
019800130109             ' and tasaas*10000+tasmgs between ' +
019900130109             %char(pIn_Da) + ' and ' + %char(pIn_A) +
020000130109             ' and tasXCO = '' '' ';
020100130109             if pIn_BSE = 'S';
020200130109               wSQL = %trim(wSQL) + ' and TASNRS > 0';
020300130109             endif;
020400130109             wSQL = %trim(wSQL) + ' +
020500130109             ),  +
020600130109             punt as +
020700130109             (select * +
020800130109             from base +
020900130109             where tascbo in (''1'', ''2'', ''4'', ''6'') +
021000130109             ) +
021100130109             select +
021200130109             count(*), +
021300130109             dec( dec(sum(punt.tasncl) , 10 , 3)/+
021400130109             dec(count(*) , 10 , 3) , 10 , 3), +
021500130109             dec( dec(sum(punt.taspkb), 10 , 3)/+
021600130109             dec(count(*) , 10 , 3) , 10 , 3), +
021700130109             dec( dec(sum(punt.tasvlb) , 10 , 3)/+
021800130109             dec(count(*) , 10 , 3) , 10 , 3) +
021900130109             from punt';
022000130109
022100130109           when i=2;
022200130109             wSQL =
022300130109             'with +
022400130109             base as +
022500130109             (SELECT +
022600130109             * +
022700130109             from TITAS10F +
022800130109             where tasccm=' + %char(pIn_CCM) +
022900130109             ' and tasaas*10000+tasmgs between ' +
023000130109             %char(pIn_Da) + ' and ' + %char(pIn_A) +
023100130109             ' and tasXCO = '' '' ';
023200130109             if pIn_BSE = 'S';
023300130109               wSQL = %trim(wSQL) + ' and TASNRS > 0';
023400130109             endif;
023500130109             wSQL = %trim(wSQL) + ' +
023600130109             ),  +
023700130109             sim2 as ( +
023800130109             select 1 as count, +
023900130109             sum(tasncl) as ncl, sum(taspkb) as pkb, sum(tasvlb) as vlb +
024000130109             from base +
024100130109             and tascbo in (''1'', ''2'') +
024200130109             group by tasaas*10000+tasmgs, +
024300130109             tasCCM, tasRSD, tasIND, tasCAD, tasLOD, tasPRD, tasNZD, tasLNA, +
024400130109             tasZNC, tascbo, tasTSP, tasCTR, tasGC1, tasGC2, tasFFD, tasDCR, +
024500130109             tasTCR, tasHCR, tasGMA, tasGGA, tasGVA, tasFTC, tasTC2, tasVAS +
024600130109             ), +
024700130109             union all +
024800130109             select 1 as count, +
024900130109             tasncl as ncl, taspkb as pkb, tasvlb as vlb +
025000130109             from base +
025100130109             )  +
025200130109             select +
025300130109             sum(count), +
025400130109             dec( dec(sum(sim2.ncl) , 10 , 3)/+
025500130109             dec(sum(count) , 10 , 3) , 10 , 3), +
025600130109             dec( dec(sum(sim2.pkb) , 10 , 3)/+
025700130109             dec(sum(count) , 10 , 3) , 10 , 3), +
025800130109             dec( dec(sum(sim2.vlb) , 10 , 3)/+
025900130109             dec(sum(count) , 10 , 3) , 10 , 3) +
026000130109             from sim2' ;
026100130109
026200130109           when i=3;
026300130109             wSQL =
026400130109             'with +
026500130109             base as +
026600130109             (SELECT +
026700130109             * +
026800130109             from TITAS10F +
026900130109             where tasccm=' + %char(pIn_CCM) +
027000130109             ' and tasaas*10000+tasmgs between ' +
027100130109             %char(pIn_Da) + ' and ' + %char(pIn_A) +
027200130109             ' and tasXCO = '' '' ';
027300130109             if pIn_BSE = 'S';
027400130109               wSQL = %trim(wSQL) + ' and TASNRS > 0';
027500130109             endif;
027600130109             wSQL = %trim(wSQL) + ' +
027700130109             ),  +
027800130109             sim4 as +
027900130109             (select +
028000130109             sum(tasncl) as ncl, sum(taspkb) as pkb, sum(tasvlb) as vlb +
028100130109             from base +
028200130109             where tascbo in (''1'', ''2'', ''4'', ''6'') +
028300130109             group by tasaas*10000+tasmgs, +
028400130109             tasCCM, tasRSD, tasIND, tasCAD, tasLOD, tasPRD, tasNZD, tasLNA, +
028500130109             tasZNC, tascbo, tasTSP, tasCTR, tasGC1, tasGC2, tasFFD, tasDCR, +
028600130109             tasTCR, tasHCR, tasGMA, tasGGA, tasGVA, tasFTC, tasTC2, tasVAS +
028700130109             ) +
028800130109             select +
028900130109             count(*), +
029000130109             dec( dec(sum(sim4.ncl) , 10 , 3)/+
029100130109             dec(count(*) , 10 , 3) , 10 , 3), +
029200130109             dec( dec(sum(sim4.pkb) , 10 , 3)/+
029300130109             dec(count(*) , 10 , 3) , 10 , 3), +
029400130109             dec( dec(sum(sim4.vlb) , 10 , 3)/+
029500130109             dec(count(*) , 10 , 3) , 10 , 3) +
029600130109             from sim4' ;
029700130109           endsl;
029800130108
029900130109           // Dichiarazione cursore
030000130109           exec sql   prepare S0   from :wSQL;
030100130109           exec sql   declare C0   cursor   for S0;
030200130109           // Apertura del cursore
030300130109           exec sql   open C0;
030400130109           // Lettura cursore
030500130109           exec sql   fetch next   from C0   into :Results;
030600130110           select;
030700130110           when i=1;
030800130110             pOut_Esito0 = SQLCOD;
030900130110             // se NON c'è un errore (non negativo) e c'è qualcosa (non 100)
031000130110             if SQLCOD >= 0 and SQLCOD <> 100;
031100130110             // sposto i risultati nei parametri di output a seconda del ciclo che sto facendo
031200130110               pOut_0NrSpe = a_0NrSpe;
031300130110               pOut_0MdCol = a_0MdCol;
031400130110               pOut_0MdPes = a_0MdPes;
031500130110               pOut_0MdVol = a_0MdVol;
031600130110             endif;
031700130110           when i=2;
031800130110             pOut_Esito1 = SQLCOD;
031900130110             // se NON c'è un errore (non negativo) e c'è qualcosa (non 100)
032000130110             if SQLCOD >= 0 and SQLCOD <> 100;
032100130110             // sposto i risultati nei parametri di output a seconda del ciclo che sto facendo
032200130110               pOut_1NrSpe = a_0NrSpe;
032300130110               pOut_1MdCol = a_0MdCol;
032400130110               pOut_1MdPes = a_0MdPes;
032500130110               pOut_1MdVol = a_0MdVol;
032600130110             endif;
032700130110           when i=3;
032800130110             pOut_Esito2 = SQLCOD;
032900130110             // se NON c'è un errore (non negativo) e c'è qualcosa (non 100)
033000130110             if SQLCOD >= 0 and SQLCOD <> 100;
033100130110             // sposto i risultati nei parametri di output a seconda del ciclo che sto facendo
033200130110               pOut_2NrSpe = a_0NrSpe;
033300130110               pOut_2MdCol = a_0MdCol;
033400130110               pOut_2MdPes = a_0MdPes;
033500130110               pOut_2MdVol = a_0MdVol;
033600130110             endif;
033700130110           endsl;
033800130109           // Chiusura del cursore
033900130109           exec sql   close C0;
034000130109         endfor;
034100121106
034200121106       ENDSR;
034300121106
034400091223       //--------------------------------------------------------------
034500121107       // Operazioni finali.
034600091223       //--------------------------------------------------------------
034700091223       BEGSR  sr_RoutEnd;
034800130109
034900130109         // Chiusura pgm
035000130109         return;
035100091223
035200091223       ENDSR;
035300091223
035400091223      /end-free
