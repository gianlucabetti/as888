000100090120     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200090120     h dftactgrp(*NO) actgrp(*caller) bnddir('UBBNDDIR':'UBRTVNETA')
000300090120      **********************************************************************************************
000400090120      * Dichiarazione file
000500090120      **********************************************************************************************
000600090120     FFIARS02L  IF   E           K DISK    extfile(LibFile01)        usropn
000700090120     FTIV1M101I UF A E           K DISK    extfile(LibFile02)        usropn
000800090121     FTIVGD00F  UF A E             DISK    extfile(LibFile03)        usropn
000900090120
001000090120      **********************************************************************************************
001100090120      *  Definizione esterne
001200090120      **********************************************************************************************
001300090120     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
001400090120     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
001500090120     D/COPY GAITRASRC/SRCPROTOPR,UBUTISQL
001600090120     D/COPY GAITRASRC/SRCPROTOPI,UBUTISQL
001700090120
001800090120      **********************************************************************************************
001900090120      *  Definizione data structures
002000090120      **********************************************************************************************
002100090120     D dsw001        E DS                  extname(fltr700f) qualified
002200090120
002300090120      **********************************************************************************************
002400090120      *  Definizione variabili work
002500090120      **********************************************************************************************
002600090120     D PgmCurrSts      S               N   INZ(*on)
002700090120     D LibFile01       S             21A   INZ
002800090120     D LibFile02       S             21A   INZ
002900090121     D LibFile03       S             21A   INZ
003000090120     D LibFiliale      S             10A   INZ
003100090120     D DataDa          S               D   INZ(*sys)
003200090120     D DataA           S               D   INZ(*sys)
003300090120     D DataDaNum       S              8S 0 INZ
003400090120     D DataANum        S              8S 0 INZ
003500090120     D TimDaAlfa       S             14A   INZ
003600090120     D TimAAlfa        S             14A   INZ
003700071129
003800080612
003900080612      /FREE
004000090120
004100090120        // Imposto l'environment sql
004200090121        exec sql
004300090121             set option DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD;
004400090120
004500090121
004600090121        // Operazioni iniziali
004700090121        exsr opeIni;
004800090121
004900090121
005000090121        // Se tutto ok => Calcolo gli estremi del perido da elaborare
005100090121        if PgmCurrSts = *on;
005200090121           exsr calDate;
005300090121        endif;
005400090121
005500090121
005600090121        // Se tutto ok => Genero il recordset dei dati da elaborare
005700090121        if PgmCurrSts = *on;
005800090121           exsr makRecordset;
005900090121        endif;
006000090121
006100090121
006200090121        // Se tutto ok => Scorro il recordset ed elaboro
006300090121        if PgmCurrSts = *on;
006400090121           exsr exeElab;
006500090121        endif;
006600090121
006700090121
006800090121        // Scrittura TIVGD
006900090121        exsr wriVGD;
007000090121
007100090121
007200090121        // Operazioni Finali
007300090121        exsr opeFin;
007400090120
007500090120
007600090121        // ...and that's all folks...
007700090120        *inlr = *on;
007800090120
007900090120
008000090121
008100090121       // -------------------------------- SUB ROUTINE -------------------------------------
008200090121
008300090121       // ********** Operazioni iniziali
008400090121       begsr opeIni;
008500090121
008600090121         // Verifico il sistema AS/400 corrente
008700090121         if UBRTVNETA_Rtv(PiOutSysNeta) = 0;
008800090121            if %subst(PiOutSysNeta:1:6) = 'SETRAS';
008900090121               LibFile01  = 'FILTRA201/FIARS02L';
009000090121               LibFile02  = 'UNITRAGRU/TIV1M101I';
009100090121               LibFile03  = 'UNITRAGRU/TIVGD00F';
009200090121               LibFiliale = 'FILTRA201';
009300090121            else;
009400090121               LibFile01  = 'FILTRAPRD/FIARS02L';
009500090121               LibFile02  = 'UNITRAGRP/TIV1M101I';
009600090121               LibFile03  = 'UNITRAGRP/TIVGD00F';
009700090121               LibFiliale = 'FILTRAPRD';
009800090121            endif;
009900090121
010000090121            // Apertura files
010100090121            if not %open(fiars02l);
010200090121               open fiars02l;
010300090121            endif;
010400090121            if not %open(tiv1m101i);
010500090121               open tiv1m101i;
010600090121            endif;
010700090121            if not %open(tivgd00f);
010800090121               open tivgd00f;
010900090121            endif;
011000090121            SQL_DropAlias('FLTR700F':'QTEMP');
011100090121            if SQL_CreaAlias('FLTR700F':'QTEMP':'FLTR700F':
011200090121                              LibFiliale:' ':piOutEsito) <> *zeros;
011300090121               PgmCurrSts = *off;
011400090121            endif;
011500090121         else;
011600090121            PgmCurrSts = *off;
011700090121         endif;
011800090121
011900090121       endsr;
012000090121       // ******************************************
012100090121
012200090121
012300090121
012400090121       // ********** Calcolo gli estremi del perido da elaborare
012500090121       begsr calDate;
012600090121
012700090121           DataDa = %date() - %Days(15+1);
012800090121           DataA  = %date() - %Days(1);
012900090121           DataDaNum = %dec(DataDa);
013000090121           DataANum = %dec(DataA);
013100090121           TimDaAlfa = %char(DataDaNum)+'000000';
013200090121           TimAAlfa  = %char(DataANum)+'999999';
013300090121
013400090121       endsr;
013500090121       // ******************************************
013600090121
013700090121
013800090121
013900090121       // ********** Genero il recordset dei dati da elaborare e
014000090121       begsr makRecordset;
014100090121
014200090121           exec sql
014300090121              declare elenco_spunte no scroll cursor for
014400090121                  select tr7pd2 from fltr700f
014500090121                  where
014600090121                      tr7fil = '043' and
014700090121                      tr7tim between :TimDaAlfa and :TimAAlfa and
014800090121                      tr7ctl = '5'   and (
014900090202                      tr7pd2 like '               18594%' or
015000090202                      tr7pd2 like '               18594%' or
015100090202                      tr7pd2 like ' ??????????????18594%' or
015200090202                      tr7pd2 like ' ??????????????18594%')
015300090121                      and tr7bar = '00000000000000000';
015400090121
015500090121           exec sql
015600090121              open elenco_spunte;
015700090121
015800090121           if sqlcode < *zeros;
015900090121              PgmCurrSts = *off;
016000090121           endif;
016100090121
016200090121       endsr;
016300090121       // ******************************************
016400090121
016500090121
016600090121
016700090121       // ********** Scorro il recordset ed elaboro
016800090121       begsr exeElab;
016900090121
017000090121             // Prima lettura fuori ciclo
017100090121             exec sql
017200090121                fetch next from elenco_spunte
017300090121                into :dsw001.tr7pd2;
017400090121
017500090121             // Normalizzo la lettura pistola
017600090121             dsw001.tr7pd2 = %trim(%subst(dsw001.tr7pd2:
017700090205             //              %size(dsw001.tr7pd2)-9:9));
017800090205                             %size(dsw001.tr7pd2)-14:14));
017900090121
018000090121             dow sqlcod >= *zeros and sqlcod <> 100;
018100090121
018200090121                 // Verifico bolla eventualmente nel frattempo confermata
018300090121                 chain   (dsw001.tr7pd2:'C') fiars02l;
018400090121                 if %found(fiars02l);
018500090121                 else;
018600090121
018700090121                    // Verifico se segnacollo corrente già restituito al cliente
018800090121                    chain   ('VAS':'0433996#'+dsw001.tr7pd2) tiv1m101i;
018900090121                    if %found(tiv1m101i);
019000090121                       // Se già presente sfleggo
019100090121                       exsr exeUpdTIV1M1;
019200090121                    else;
019300090121                       // Se nn già presente inserisco
019400090121                       exsr exeWriTIV1M1;
019500090121                    endif;
019600090121                 endif;
019700090121
019800090121                 // Proseguo scorrimento recordset
019900090121                 exec sql
020000090121                      fetch next from elenco_spunte
020100090121                      into :dsw001.tr7pd2;
020200090121
020300090121                 // Normalizzo la lettura pistola
020400090121                 dsw001.tr7pd2 = %trim(%subst(dsw001.tr7pd2:
020500090205                 //              %size(dsw001.tr7pd2)-9:9));
020600090205                                 %size(dsw001.tr7pd2)-14:14));
020700090121             enddo;
020800090121
020900090121       endsr;
021000090121       // ******************************************
021100090121
021200090121
021300090121
021400090121       // ********** Update record TIV1M1
021500090121       begsr exeUpdTIV1M1;
021600090121
021700090121             if v1m1ftr = 'T';
021800090121                v1m1ftr = *blanks;
021900090121                update tiv1m100f;
022000090121             endif;
022100090121
022200090121       endsr;
022300090121       // ******************************************
022400090121
022500090121
022600090121
022700090121       // ********** Write record TIV1M1
022800090121       begsr exeWriTIV1M1;
022900090121
023000090121             clear tiv1m100f;
023100090121             v1m1app = 'VAS';
023200090121             v1m1key = '0433996#'+dsw001.tr7pd2;
023300090121             v1m1dta = dsw001.tr7pd2;
023400090121             write tiv1m100f;
023500090121
023600090121       endsr;
023700090121       // ******************************************
023800090121
023900090121
024000090121
024100090121       // ********** Scrittura file TIVGD
024200090121       begsr wriVGD;
024300090121
024400090121             clear tivgd000;
024500090121             if PgmCurrSts = *on;
024600090121                vgddta = 'OK';
024700090121             else;
024800090121                vgddta = 'KO';
024900090121             endif;
025000090121             vgdtip = 'TE';
025100090121             vgdksu = '00433996';
025200090121             vgdtsc = 'WW';
025300090121             vgddat = %dec(%date());
025400090121             vgdpgm = 'TITV1M1R';
025500090121             write tivgd000;
025600090121
025700090121       endsr;
025800090121       // ******************************************
025900090121
026000090121
026100090121
026200090121       // ********** Operazioni finali
026300090121       begsr opeFin;
026400090121
026500090126            exec sql
026600090126               close elenco_spunte;
026700090126
026800090121             if %open(fiars02l);
026900090121                close fiars02l;
027000090121             endif;
027100090121             if %open(tiv1m101i);
027200090121                close tiv1m101i;
027300090121             endif;
027400090121             if %open(tivgd00f);
027500090121                close tivgd00f;
027600090121             endif;
027700090126
027800090121             SQL_DropAlias('FLTR700F':'QTEMP');
027900090121
028000090121       endsr;
028100090121       // ******************************************
028200090121
028300080612
028400080612      /END-FREE
