000100091228       //==============================================================
000200130123       // Pgm di utility per simulazione accorpamento bolle
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
006700130123     d pIn_CCM1        s              7s 0
006800130123     d pIn_CCM2        s              7s 0
006900130123     d pIn_CCM3        s              7s 0
007000130123     d pIn_CCM4        s              7s 0
007100130123     d pIn_CCM5        s              7s 0
007200130108     d pIn_DA          s              8s 0
007300130108     d pIn_A           s              8s 0
007400130108     d pIn_BSE         s              1
007500130110     d pOut_Esito0     s              9b 0
007600130110     d pOut_Esito1     s              9b 0
007700130110     d pOut_Esito2     s              9b 0
007800130108     d pOut_0NRSPE     s              8s 0
007900130108     d pOut_0MDCOL     s              7s 2
008000130108     d pOut_0MDPES     s              9s 3
008100130108     d pOut_0MDVOL     s              5s 3
008200130108     d pOut_1NRSPE     s              8s 0
008300130108     d pOut_1MDCOL     s              7s 2
008400130108     d pOut_1MDPES     s              9s 3
008500130108     d pOut_1MDVOL     s              5s 3
008600130108     d pOut_2NRSPE     s              8s 0
008700130108     d pOut_2MDCOL     s              7s 2
008800130108     d pOut_2MDPES     s              9s 3
008900130108     d pOut_2MDVOL     s              5s 3
009000091223
009100121106       // Stringa SQL da eseguire
009200130108     d wSQL            s           5000    inz  varying
009300121106
009400121106       // Campi di comodo
009500121106     d wTable          s             10
009600121106     d wInit           s              3
009700121106     d wDate           s              8  0 inz
009800121107     d wStart          s             10
009900121107     d wEnd            s             10
010000121107     d wErroreC5       s              1
010100121107     d count           s              7s 0 inz
010200121108     d wLib            s             11
010300130109     d i               s              3s 0
010400130123     d listaClienti    s             60
010500091223
010600091223       //--------------------------------------------------------------
010700121106       // Definizione prototipi procedure.
010800091223       //--------------------------------------------------------------
010900091223
011000091223       //--------------------------------------------------------------
011100121106       // Definizione key-list.
011200091223       //--------------------------------------------------------------
011300091223
011400091223
011500091223       //--------------------------------------------------------------
011600121107       // Definizione parametri procedura.
011700091223       //--------------------------------------------------------------
011800091223
011900091223     c     *Entry        plist
012000130123     c                   parm                    pIn_CCM1
012100130123     c                   parm                    pIn_CCM2
012200130123     c                   parm                    pIn_CCM3
012300130123     c                   parm                    pIn_CCM4
012400130123     c                   parm                    pIn_CCM5
012500130108     c                   parm                    pIn_DA
012600130108     c                   parm                    pIn_A
012700130108     c                   parm                    pIn_BSE
012800130110     c                   parm                    pOut_Esito0
012900130110     c                   parm                    pOut_Esito1
013000130110     c                   parm                    pOut_Esito2
013100130108     c                   parm                    pOut_0NRSPE
013200130108     c                   parm                    pOut_0MDCOL
013300130108     c                   parm                    pOut_0MDPES
013400130108     c                   parm                    pOut_0MDVOL
013500130108     c                   parm                    pOut_1NRSPE
013600130108     c                   parm                    pOut_1MDCOL
013700130108     c                   parm                    pOut_1MDPES
013800130108     c                   parm                    pOut_1MDVOL
013900130108     c                   parm                    pOut_2NRSPE
014000130108     c                   parm                    pOut_2MDCOL
014100130108     c                   parm                    pOut_2MDPES
014200130108     c                   parm                    pOut_2MDVOL
014300091223
014400091223      /free
014500091223
014600091223       //--------------------------------------------------------------
014700121106       // M A I N - L I N E
014800091223       //--------------------------------------------------------------
014900091223
015000121106       // Operazioni iniziali?
015100091223       exsr sr_RoutInz;
015200091223
015300130108       // Simulazione
015400130108       exsr  GesSimulazioni;
015500091223
015600121106       // Operazioni finali?
015700091223       exsr sr_RoutEnd;
015800091223
015900091223       //--------------------------------------------------------------
016000121107       // Operazioni iniziali.
016100091223       //--------------------------------------------------------------
016200091223       BEGSR  sr_RoutInz;
016300091223
016400091223         exec sql  set option  dynusrprf = *owner,
016500121107                               closqlcsr = *endmod;
016600091223
016700091223         *inLR = *on;
016800100112
016900121108         // inizializzo i dati di output
017000130110         clear pOut_Esito0;
017100130110         clear pOut_Esito1;
017200130110         clear pOut_Esito2;
017300130108         clear pOut_0NrSpe;
017400130108         clear pOut_0MdCol;
017500130108         clear pOut_0MdPes;
017600130108         clear pOut_0MdVol;
017700130108         clear pOut_1NrSpe;
017800130108         clear pOut_1MdCol;
017900130108         clear pOut_1MdPes;
018000130108         clear pOut_1MdVol;
018100130108         clear pOut_2NrSpe;
018200130108         clear pOut_2MdCol;
018300130108         clear pOut_2MdPes;
018400130108         clear pOut_2MdVol;
018500121108
018600121108         // Reperimento data odierna (fmt aaaa/mm/gg)
018700121108         wDate = %dec( %date() );
018800091223
018900091223       ENDSR;
019000110523
019100110523       //--------------------------------------------------------------
019200130123       // Gestione della simulazione
019300110523       //--------------------------------------------------------------
019400130108       BEGSR  GesSimulazioni;
019500110523
019600130123         // creo lista clienti
019700130123         listaClienti = '(' ;
019800130123         if pIn_CCM1 > 0 ;
019900130123           listaClienti = %trim(listaClienti) + ' ' + %char(pIn_CCM1) ;
020000130123           if pIn_CCM2 > 0 or
020100130123              pIn_CCM3 > 0 or
020200130123              pIn_CCM4 > 0 or
020300130123              pIn_CCM5 > 0    ;
020400130123             listaClienti = %trim(listaClienti) + ' ,' ;
020500130123           endif;
020600130123         endif;
020700130123         if pIn_CCM2 > 0 ;
020800130123           listaClienti = %trim(listaClienti) + ' ' + %char(pIn_CCM2) ;
020900130123           if pIn_CCM3 > 0 or
021000130123              pIn_CCM4 > 0 or
021100130123              pIn_CCM5 > 0    ;
021200130123             listaClienti = %trim(listaClienti) + ' ,' ;
021300130123           endif;
021400130123         endif;
021500130123         if pIn_CCM3 > 0 ;
021600130123           listaClienti = %trim(listaClienti) + ' ' + %char(pIn_CCM3) ;
021700130123           if pIn_CCM4 > 0 or
021800130123              pIn_CCM5 > 0    ;
021900130123             listaClienti = %trim(listaClienti) + ' ,' ;
022000130123           endif;
022100130123         endif;
022200130123         if pIn_CCM4 > 0 ;
022300130123           listaClienti = %trim(listaClienti) + ' ' + %char(pIn_CCM4) ;
022400130123           if pIn_CCM5 > 0    ;
022500130123             listaClienti = %trim(listaClienti) + ' ,' ;
022600130123           endif;
022700130123         endif;
022800130123         if pIn_CCM5 > 0 ;
022900130123           listaClienti = %trim(listaClienti) + ' ' + %char(pIn_CCM5) ;
023000130123         endif;
023100130123         listaClienti = %trim(listaClienti) + ')' ;
023200130123
023300130123         // ciclo per 3 volte dato che sono 3 gli statement da eseguire
023400130109         for i = 1 to 3;
023500130123           select;
023600130123           // medie sui dati dallo storico e dall'attuale, con CBO 1/2/4/6, NON raggruppati
023700130109           when i=1;
023800130109             wSQL =
023900130109             'with +
024000130109             base as +
024100130109             (SELECT +
024200130109             * +
024300130109             from TITAS10F +
024400130123             where tasccm in ' + %trim(listaClienti) +
024500130109             ' and tasaas*10000+tasmgs between ' +
024600130109             %char(pIn_Da) + ' and ' + %char(pIn_A) +
024700130109             ' and tasXCO = '' '' ';
024800130109             if pIn_BSE = 'S';
024900130109               wSQL = %trim(wSQL) + ' and TASNRS > 0';
025000130109             endif;
025100130123             wSQL = %trim(wSQL) + ' UNION ALL +
025200130123             SELECT +
025300130123             * +
025400130123             from TITAS00F +
025500130123             where tasccm in ' + %trim(listaClienti) +
025600130123             ' and tasaas*10000+tasmgs between ' +
025700130123             %char(pIn_Da) + ' and ' + %char(pIn_A) +
025800130123             ' and tasXCO = '' '' ';
025900130123             if pIn_BSE = 'S';
026000130123               wSQL = %trim(wSQL) + ' and TASNRS > 0';
026100130123             endif;
026200130109             wSQL = %trim(wSQL) + ' +
026300130109             ),  +
026400130109             punt as +
026500130109             (select * +
026600130109             from base +
026700130109             where tascbo in (''1'', ''2'', ''4'', ''6'') +
026800130109             ) +
026900130109             select +
027000130109             count(*), +
027100130109             dec( dec(sum(punt.tasncl) , 10 , 3)/+
027200130109             dec(count(*) , 10 , 3) , 10 , 3), +
027300130109             dec( dec(sum(punt.taspkb), 10 , 3)/+
027400130109             dec(count(*) , 10 , 3) , 10 , 3), +
027500130109             dec( dec(sum(punt.tasvlb) , 10 , 3)/+
027600130109             dec(count(*) , 10 , 3) , 10 , 3) +
027700130109             from punt';
027800130109
027900130123           // medie sui dati dallo storico e dall'attuale, con CBO 1/2, raggruppati
028000130123           // in union coi dati dallo storico e dall'attuale, con CBO 4/6, NON raggruppati
028100130109           when i=2;
028200130109             wSQL =
028300130109             'with +
028400130109             base as +
028500130109             (SELECT +
028600130109             * +
028700130109             from TITAS10F +
028800130123             where tasccm in ' + %trim(listaClienti) +
028900130109             ' and tasaas*10000+tasmgs between ' +
029000130109             %char(pIn_Da) + ' and ' + %char(pIn_A) +
029100130109             ' and tasXCO = '' '' ';
029200130109             if pIn_BSE = 'S';
029300130109               wSQL = %trim(wSQL) + ' and TASNRS > 0';
029400130109             endif;
029500130123             wSQL = %trim(wSQL) + ' UNION ALL +
029600130123             SELECT +
029700130123             * +
029800130123             from TITAS00F +
029900130123             where tasccm in ' + %trim(listaClienti) +
030000130123             ' and tasaas*10000+tasmgs between ' +
030100130123             %char(pIn_Da) + ' and ' + %char(pIn_A) +
030200130123             ' and tasXCO = '' '' ';
030300130123             if pIn_BSE = 'S';
030400130123               wSQL = %trim(wSQL) + ' and TASNRS > 0';
030500130123             endif;
030600130109             wSQL = %trim(wSQL) + ' +
030700130109             ),  +
030800130109             sim2 as ( +
030900130109             select 1 as count, +
031000130109             sum(tasncl) as ncl, sum(taspkb) as pkb, sum(tasvlb) as vlb +
031100130109             from base +
031200130123             where tascbo in (''1'', ''2'') +
031300130109             group by tasaas*10000+tasmgs, +
031400130123                     tasRSD, tasIND, tasCAD, tasLOD, tasPRD, tasNZD, tasLNA, +
031500130109             tasZNC, tascbo, tasTSP, tasCTR, tasGC1, tasGC2, tasFFD, tasDCR, +
031600130109             tasTCR, tasHCR, tasGMA, tasGGA, tasGVA, tasFTC, tasTC2, tasVAS +
031700130109             union all +
031800130109             select 1 as count, +
031900130109             tasncl as ncl, taspkb as pkb, tasvlb as vlb +
032000130109             from base +
032100130123             where tascbo in (''4'', ''6'') +
032200130109             )  +
032300130109             select +
032400130109             sum(count), +
032500130109             dec( dec(sum(sim2.ncl) , 10 , 3)/+
032600130109             dec(sum(count) , 10 , 3) , 10 , 3), +
032700130109             dec( dec(sum(sim2.pkb) , 10 , 3)/+
032800130109             dec(sum(count) , 10 , 3) , 10 , 3), +
032900130109             dec( dec(sum(sim2.vlb) , 10 , 3)/+
033000130109             dec(sum(count) , 10 , 3) , 10 , 3) +
033100130109             from sim2' ;
033200130109
033300130123           // medie sui dati dallo storico e dall'attuale, con CBO 1/2/4/6, raggruppati
033400130109           when i=3;
033500130109             wSQL =
033600130109             'with +
033700130109             base as +
033800130109             (SELECT +
033900130109             * +
034000130109             from TITAS10F +
034100130123             where tasccm in ' + %trim(listaClienti) +
034200130109             ' and tasaas*10000+tasmgs between ' +
034300130109             %char(pIn_Da) + ' and ' + %char(pIn_A) +
034400130109             ' and tasXCO = '' '' ';
034500130109             if pIn_BSE = 'S';
034600130109               wSQL = %trim(wSQL) + ' and TASNRS > 0';
034700130109             endif;
034800130123             wSQL = %trim(wSQL) + ' UNION ALL +
034900130123             SELECT +
035000130123             * +
035100130123             from TITAS00F +
035200130123             where tasccm in ' + %trim(listaClienti) +
035300130123             ' and tasaas*10000+tasmgs between ' +
035400130123             %char(pIn_Da) + ' and ' + %char(pIn_A) +
035500130123             ' and tasXCO = '' '' ';
035600130123             if pIn_BSE = 'S';
035700130123               wSQL = %trim(wSQL) + ' and TASNRS > 0';
035800130123             endif;
035900130109             wSQL = %trim(wSQL) + ' +
036000130109             ),  +
036100130109             sim4 as +
036200130109             (select +
036300130109             sum(tasncl) as ncl, sum(taspkb) as pkb, sum(tasvlb) as vlb +
036400130109             from base +
036500130109             where tascbo in (''1'', ''2'', ''4'', ''6'') +
036600130109             group by tasaas*10000+tasmgs, +
036700130123                     tasRSD, tasIND, tasCAD, tasLOD, tasPRD, tasNZD, tasLNA, +
036800130109             tasZNC, tascbo, tasTSP, tasCTR, tasGC1, tasGC2, tasFFD, tasDCR, +
036900130109             tasTCR, tasHCR, tasGMA, tasGGA, tasGVA, tasFTC, tasTC2, tasVAS +
037000130109             ) +
037100130109             select +
037200130109             count(*), +
037300130109             dec( dec(sum(sim4.ncl) , 10 , 3)/+
037400130109             dec(count(*) , 10 , 3) , 10 , 3), +
037500130109             dec( dec(sum(sim4.pkb) , 10 , 3)/+
037600130109             dec(count(*) , 10 , 3) , 10 , 3), +
037700130109             dec( dec(sum(sim4.vlb) , 10 , 3)/+
037800130109             dec(count(*) , 10 , 3) , 10 , 3) +
037900130109             from sim4' ;
038000130109           endsl;
038100130108
038200130109           // Dichiarazione cursore
038300130109           exec sql   prepare S0   from :wSQL;
038400130109           exec sql   declare C0   cursor   for S0;
038500130109           // Apertura del cursore
038600130109           exec sql   open C0;
038700130109           // Lettura cursore
038800130109           exec sql   fetch next   from C0   into :Results;
038900130110           select;
039000130110           when i=1;
039100130110             pOut_Esito0 = SQLCOD;
039200130110             // se NON c'è un errore (non negativo) e c'è qualcosa (non 100)
039300130110             if SQLCOD >= 0 and SQLCOD <> 100;
039400130110             // sposto i risultati nei parametri di output a seconda del ciclo che sto facendo
039500130110               pOut_0NrSpe = a_0NrSpe;
039600130110               pOut_0MdCol = a_0MdCol;
039700130110               pOut_0MdPes = a_0MdPes;
039800130110               pOut_0MdVol = a_0MdVol;
039900130110             endif;
040000130110           when i=2;
040100130110             pOut_Esito1 = SQLCOD;
040200130110             // se NON c'è un errore (non negativo) e c'è qualcosa (non 100)
040300130110             if SQLCOD >= 0 and SQLCOD <> 100;
040400130110             // sposto i risultati nei parametri di output a seconda del ciclo che sto facendo
040500130110               pOut_1NrSpe = a_0NrSpe;
040600130110               pOut_1MdCol = a_0MdCol;
040700130110               pOut_1MdPes = a_0MdPes;
040800130110               pOut_1MdVol = a_0MdVol;
040900130110             endif;
041000130110           when i=3;
041100130110             pOut_Esito2 = SQLCOD;
041200130110             // se NON c'è un errore (non negativo) e c'è qualcosa (non 100)
041300130110             if SQLCOD >= 0 and SQLCOD <> 100;
041400130110             // sposto i risultati nei parametri di output a seconda del ciclo che sto facendo
041500130110               pOut_2NrSpe = a_0NrSpe;
041600130110               pOut_2MdCol = a_0MdCol;
041700130110               pOut_2MdPes = a_0MdPes;
041800130110               pOut_2MdVol = a_0MdVol;
041900130110             endif;
042000130110           endsl;
042100130109           // Chiusura del cursore
042200130109           exec sql   close C0;
042300130109         endfor;
042400121106
042500121106       ENDSR;
042600121106
042700091223       //--------------------------------------------------------------
042800121107       // Operazioni finali.
042900091223       //--------------------------------------------------------------
043000091223       BEGSR  sr_RoutEnd;
043100130109
043200130109         // Chiusura pgm
043300130109         return;
043400091223
043500091223       ENDSR;
043600091223
043700091223      /end-free
