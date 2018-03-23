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
023500130109           select;
023600130109           when i=1;
023700130109             wSQL =
023800130109             'with +
023900130109             base as +
024000130109             (SELECT +
024100130109             * +
024200130109             from TITAS10F +
024300130123             where tasccm in ' + %trim(listaClienti) +
024400130109             ' and tasaas*10000+tasmgs between ' +
024500130109             %char(pIn_Da) + ' and ' + %char(pIn_A) +
024600130109             ' and tasXCO = '' '' ';
024700130109             if pIn_BSE = 'S';
024800130109               wSQL = %trim(wSQL) + ' and TASNRS > 0';
024900130109             endif;
025000130109             wSQL = %trim(wSQL) + ' +
025100130109             ),  +
025200130109             punt as +
025300130109             (select * +
025400130109             from base +
025500130109             where tascbo in (''1'', ''2'', ''4'', ''6'') +
025600130109             ) +
025700130109             select +
025800130109             count(*), +
025900130109             dec( dec(sum(punt.tasncl) , 10 , 3)/+
026000130109             dec(count(*) , 10 , 3) , 10 , 3), +
026100130109             dec( dec(sum(punt.taspkb), 10 , 3)/+
026200130109             dec(count(*) , 10 , 3) , 10 , 3), +
026300130109             dec( dec(sum(punt.tasvlb) , 10 , 3)/+
026400130109             dec(count(*) , 10 , 3) , 10 , 3) +
026500130109             from punt';
026600130109
026700130109           when i=2;
026800130109             wSQL =
026900130109             'with +
027000130109             base as +
027100130109             (SELECT +
027200130109             * +
027300130109             from TITAS10F +
027400130123             where tasccm in ' + %trim(listaClienti) +
027500130109             ' and tasaas*10000+tasmgs between ' +
027600130109             %char(pIn_Da) + ' and ' + %char(pIn_A) +
027700130109             ' and tasXCO = '' '' ';
027800130109             if pIn_BSE = 'S';
027900130109               wSQL = %trim(wSQL) + ' and TASNRS > 0';
028000130109             endif;
028100130109             wSQL = %trim(wSQL) + ' +
028200130109             ),  +
028300130109             sim2 as ( +
028400130109             select 1 as count, +
028500130109             sum(tasncl) as ncl, sum(taspkb) as pkb, sum(tasvlb) as vlb +
028600130109             from base +
028700130109             and tascbo in (''1'', ''2'') +
028800130109             group by tasaas*10000+tasmgs, +
028900130109             tasCCM, tasRSD, tasIND, tasCAD, tasLOD, tasPRD, tasNZD, tasLNA, +
029000130109             tasZNC, tascbo, tasTSP, tasCTR, tasGC1, tasGC2, tasFFD, tasDCR, +
029100130109             tasTCR, tasHCR, tasGMA, tasGGA, tasGVA, tasFTC, tasTC2, tasVAS +
029200130109             ), +
029300130109             union all +
029400130109             select 1 as count, +
029500130109             tasncl as ncl, taspkb as pkb, tasvlb as vlb +
029600130109             from base +
029700130109             )  +
029800130109             select +
029900130109             sum(count), +
030000130109             dec( dec(sum(sim2.ncl) , 10 , 3)/+
030100130109             dec(sum(count) , 10 , 3) , 10 , 3), +
030200130109             dec( dec(sum(sim2.pkb) , 10 , 3)/+
030300130109             dec(sum(count) , 10 , 3) , 10 , 3), +
030400130109             dec( dec(sum(sim2.vlb) , 10 , 3)/+
030500130109             dec(sum(count) , 10 , 3) , 10 , 3) +
030600130109             from sim2' ;
030700130109
030800130109           when i=3;
030900130109             wSQL =
031000130109             'with +
031100130109             base as +
031200130109             (SELECT +
031300130109             * +
031400130109             from TITAS10F +
031500130123             where tasccm in ' + %trim(listaClienti) +
031600130109             ' and tasaas*10000+tasmgs between ' +
031700130109             %char(pIn_Da) + ' and ' + %char(pIn_A) +
031800130109             ' and tasXCO = '' '' ';
031900130109             if pIn_BSE = 'S';
032000130109               wSQL = %trim(wSQL) + ' and TASNRS > 0';
032100130109             endif;
032200130109             wSQL = %trim(wSQL) + ' +
032300130109             ),  +
032400130109             sim4 as +
032500130109             (select +
032600130109             sum(tasncl) as ncl, sum(taspkb) as pkb, sum(tasvlb) as vlb +
032700130109             from base +
032800130109             where tascbo in (''1'', ''2'', ''4'', ''6'') +
032900130109             group by tasaas*10000+tasmgs, +
033000130109             tasCCM, tasRSD, tasIND, tasCAD, tasLOD, tasPRD, tasNZD, tasLNA, +
033100130109             tasZNC, tascbo, tasTSP, tasCTR, tasGC1, tasGC2, tasFFD, tasDCR, +
033200130109             tasTCR, tasHCR, tasGMA, tasGGA, tasGVA, tasFTC, tasTC2, tasVAS +
033300130109             ) +
033400130109             select +
033500130109             count(*), +
033600130109             dec( dec(sum(sim4.ncl) , 10 , 3)/+
033700130109             dec(count(*) , 10 , 3) , 10 , 3), +
033800130109             dec( dec(sum(sim4.pkb) , 10 , 3)/+
033900130109             dec(count(*) , 10 , 3) , 10 , 3), +
034000130109             dec( dec(sum(sim4.vlb) , 10 , 3)/+
034100130109             dec(count(*) , 10 , 3) , 10 , 3) +
034200130109             from sim4' ;
034300130109           endsl;
034400130108
034500130109           // Dichiarazione cursore
034600130109           exec sql   prepare S0   from :wSQL;
034700130109           exec sql   declare C0   cursor   for S0;
034800130109           // Apertura del cursore
034900130109           exec sql   open C0;
035000130109           // Lettura cursore
035100130109           exec sql   fetch next   from C0   into :Results;
035200130110           select;
035300130110           when i=1;
035400130110             pOut_Esito0 = SQLCOD;
035500130110             // se NON c'è un errore (non negativo) e c'è qualcosa (non 100)
035600130110             if SQLCOD >= 0 and SQLCOD <> 100;
035700130110             // sposto i risultati nei parametri di output a seconda del ciclo che sto facendo
035800130110               pOut_0NrSpe = a_0NrSpe;
035900130110               pOut_0MdCol = a_0MdCol;
036000130110               pOut_0MdPes = a_0MdPes;
036100130110               pOut_0MdVol = a_0MdVol;
036200130110             endif;
036300130110           when i=2;
036400130110             pOut_Esito1 = SQLCOD;
036500130110             // se NON c'è un errore (non negativo) e c'è qualcosa (non 100)
036600130110             if SQLCOD >= 0 and SQLCOD <> 100;
036700130110             // sposto i risultati nei parametri di output a seconda del ciclo che sto facendo
036800130110               pOut_1NrSpe = a_0NrSpe;
036900130110               pOut_1MdCol = a_0MdCol;
037000130110               pOut_1MdPes = a_0MdPes;
037100130110               pOut_1MdVol = a_0MdVol;
037200130110             endif;
037300130110           when i=3;
037400130110             pOut_Esito2 = SQLCOD;
037500130110             // se NON c'è un errore (non negativo) e c'è qualcosa (non 100)
037600130110             if SQLCOD >= 0 and SQLCOD <> 100;
037700130110             // sposto i risultati nei parametri di output a seconda del ciclo che sto facendo
037800130110               pOut_2NrSpe = a_0NrSpe;
037900130110               pOut_2MdCol = a_0MdCol;
038000130110               pOut_2MdPes = a_0MdPes;
038100130110               pOut_2MdVol = a_0MdVol;
038200130110             endif;
038300130110           endsl;
038400130109           // Chiusura del cursore
038500130109           exec sql   close C0;
038600130109         endfor;
038700121106
038800121106       ENDSR;
038900121106
039000091223       //--------------------------------------------------------------
039100121107       // Operazioni finali.
039200091223       //--------------------------------------------------------------
039300091223       BEGSR  sr_RoutEnd;
039400130109
039500130109         // Chiusura pgm
039600130109         return;
039700091223
039800091223       ENDSR;
039900091223
040000091223      /end-free
