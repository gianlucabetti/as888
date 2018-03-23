000100110203       //==============================================================
000200111223       //?TNVRSM012R - Verifica sovrapposizioni range segnacolli in    ?
000300110914       //?             tabelle "3CP" e "3EW" (o solo "3C" - non parz.).?
000400110203       //==============================================================
000500110203
000600110203       //--------------------------------------------------------------
000700110203       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800110203       //--------------------------------------------------------------
000900110203
001000110203     /*PRM  dbgview(*source)
001100110914     /*CMD  ovrdbf file(TITAS33C) tofile(GAITRAGRPS/TITAS33C) +
001200110914     /*CMD         ovrscope(*calllvl)
001300110914     /*END  dltovr file(TITAS33C) lvl(*)
001400110203     /*END
001500110203
001600110203       //--------------------------------------------------------------
001700110203       //?Specifiche di controllo.                                     ?
001800110203       //--------------------------------------------------------------
001900110203
002000110203     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002100110914     h dftactgrp(*no) bnddir('UBBNDDIR':'TRUL')
002200110203     h alwnull(*inputonly)
002300110203
002400110203       //--------------------------------------------------------------
002500110203       //?Dichiarazione file.                                          ?
002600110203       //--------------------------------------------------------------
002700110914
002800110914       // -?Bolle Sede?
002900110914     fTITAS33C  if   e           k disk    usropn
003000110914     f                                     extfile(wLibFileSede)
003100110203
003200110203       // -?Printer File?
003300110209     fQSYSPRT   o    f  132        printer usropn oflind(*inOF)
003400110203
003500110203       //--------------------------------------------------------------
003600110203       //?Definizione costanti.                                        ?
003700110203       //--------------------------------------------------------------
003800110203
003900110203     d c_OvrPrtF       c                   const('OVRPRTF +
004000110203     d                                            file(QSYSPRT) +
004100110203     d                                            pagesize(66 132 *rowcol) +
004200110203     d                                            lpi(6) cpi(10) +
004300110203     d                                            usrdta(''SovrRngScl'')')
004400110203     d c_DltOvr        c                   const('DLTOVR +
004500110203     d                                            file(QSYSPRT)')
004600110203
004700110203       //--------------------------------------------------------------
004800110203       //?Definizione schiere.                                         ?
004900110203       //--------------------------------------------------------------
005000110203
005100110913       // -?Note in stampa?
005200111223     d sk_Note1        s                   like(s_Note)  dim(3)  inz
005300111223     d sk_Note2        s                   like(s_Note)  dim(3)  inz
005400111223
005500111223       // -?Clienti già elaborati per filiale/serie (lung. max 32.767)?
005600111223     d sk_CliElab      s                   like(wSQLrecC1.sql_Padre)
005700111223     d                                     dim(4681)   inz
005800110203
005900110203       //--------------------------------------------------------------
006000110203       //?Definizione aree dati.                                       ?
006100110203       //--------------------------------------------------------------
006200110203
006300110203
006400110203       //--------------------------------------------------------------
006500110203       //?Definizione strutture dati.                                  ?
006600110203       //--------------------------------------------------------------
006700110203
006800110203       // -?Status?
006900110203     d Status         sds
007000110203     d  SDSpgm           *proc
007100110203     d  SDSusr               254    263
007200110203
007300110203       // -?Dati estratti via SQL?
007400110203     d wSQLrecC1       ds                  inz  qualified
007500110203     d   sql_Filiale                  3    inz
007600110203     d   sql_Cliente                  4    inz
007700111215     d   sql_Padre                    7    inz
007800120209     d   sql_FilSgnC                  3  0 inz
007900110203     d   sql_Serie                    2  0 inz
008000110203     d   sql_SgnCl_I                  7  0 inz
008100110203     d   sql_SgnCl_F                  7  0 inz
008200110203
008300111223     d wSQLrecC2       ds                  inz  likeds(wSQLrecC1)
008400111223
008500111223     d SavSQLrec       ds                  inz  likeds(wSQLrecC1)
008600110203
008700110203       // -?Tab. "3C"?
008800110203     d ds3Ca         e ds                  inz  extname(ds3C)
008900110203     d                                          qualified
009000110203     d ds3Cb         e ds                  inz  extname(ds3C)
009100110203     d                                          qualified
009200110203
009300110203       //--------------------------------------------------------------
009400110203       //?Definizione variabili globali.                               ?
009500110203       //--------------------------------------------------------------
009600120105
009700120105       // -?Flags booleani?
009800120105     d*// $FirstRow       s               n   inz(*on)
009900110913
010000110913       // -?Indici di schiera?
010100111223     d ww              s              3  0 inz
010200110913     d xx              s              3  0 inz
010300110913     d xy              s              3  0 inz
010400110913     d yy              s              3  0 inz
010500110914
010600110914       // -?Nome libreria del file di sede?
010700110914     d wLibFileSede    s             21    inz
010800110203
010900110203       // -?Stringa SQL da eseguire?
011000110916     d wSQL_C1         s           5000    inz  Varying
011100111223     d wSQL_C2         s           7500    inz  Varying
011200120103       // -?(*TEMP x ctrl SQL)?
011300120105     d*// wSQL_Cx_ds      ds          7500    inz
011400120105     d*//  wSQL_Cx                      60    inz  dim(125)
011500110203
011600110203       // -?Orario?
011700110203     d w_Time          s             10    inz
011800111223
011900111223       // -?Campi di controllo rotture di livello?
012000111223     d wCli_L1         s                   like(wSQLrecC1.sql_Padre)
012100111223     d                                     inz
012200111223     d wCli_L2         s                   like(wSQLrecC1.sql_Padre)
012300111223     d                                     inz
012400110203
012500110203       // -?Campi di stampa?
012600110913     d s_Filiale       s                   like(wSQLrecC1.sql_Filiale)
012700110203     d                                     inz
012800110913     d s_Cliente       s                   like(wSQLrecC1.sql_Cliente)
012900110203     d                                     inz
013000120209     d s_FilSgnC       s                   like(wSQLrecC1.sql_FilSgnC)
013100120209     d                                     inz
013200110913     d s_Serie         s                   like(wSQLrecC1.sql_Serie)
013300110203     d                                     inz
013400110913     d s_SgnCl_I       s                   like(wSQLrecC1.sql_SgnCl_I)
013500110203     d                                     inz
013600110913     d s_SgnCl_F       s                   like(wSQLrecC1.sql_SgnCl_F)
013700110203     d                                     inz
013800110914     d s_BloAnn        s              9    inz
013900110913     d s_RagSoc        s             23    inz
014000110913     d s_Padre         s                   like(ds3Ca.§3Ccks)
014100110209     d                                     inz
014200120105     d s_NrFigli       s              4  0 inz
014300110914     d s_DtUltSpe      s              8  0 inz
014400120209     d s_Note          s             38    inz
014500110203     d w_TotSovrapp    s              5  0 inz
014600110914
014700110914       // -?Campi di comodo?
014800110914     d wDate_EUR       s               d   datfmt(*eur)  inz
014900110914     d wNRS            s                   like(ds3Ca.§3Cnrs)
015000110914     d                                     inz
015100110203
015200110203       //--------------------------------------------------------------
015300110203       //?Definizione procedure usate.                                 ?
015400110203       //--------------------------------------------------------------
015500110203
015600110203       // -?Reperimento dati tabelle?
015700110203      /copy gaitrasrc/srcProtoPI,TRULTAB
015800110203      /copy gaitrasrc/srcProtoPR,TRULTAB
015900110204
016000110204       // -?Controllo/Decodifica cliente?
016100110204      /copy gaitrasrc/srcProtoPI,TIBS69R
016200110204      /copy gaitrasrc/srcProtoPR,TIBS69R
016300120105
016400120105       // -?Reperimento legami in tab. "3C"?
016500120105      /copy gaitrasrc/srcProtoPI,UBLEG3C
016600120105      /copy gaitrasrc/srcProtoPR,UBLEG3C
016700110203
016800110203       // -?Parametri API QCAPCMD (Process Commands)?
016900110203     d Qcmd            s           1024    inz  varying
017000110203      /copy qSysInc/qRpgleSrc,QCAPCMD
017100110203      /copy gaitrasrc/srcProtoPR,QCAPCMD
017200110203
017300110203       // -?Parametri gestione errori API.?
017400110203      /copy qSysInc/qRpgleSrc,QUSEC
017500110914
017600110914       // -?Reperimento NETA sistema AS/400 corrente?
017700110914     d currSysNeta     s              8a   inz
017800110914      /copy gaitrasrc/srcProtoPr,UBRTVNETA
017900110203
018000110203       //--------------------------------------------------------------
018100110203       //?Definizione key-list.                                        ?
018200110203       //--------------------------------------------------------------
018300110203
018400110914       // -?File TITAS33C?
018500110914     d k03titas33    e ds                  extname(TITAS33C : *key)
018600110914     d                                     prefix(k_)
018700110914     d                                     inz
018800110203
018900110203       //--------------------------------------------------------------
019000110203       //?Riepilogo indicatori.                                        ?
019100110203       //--------------------------------------------------------------
019200110203
019300110203
019400110203       //--------------------------------------------------------------
019500110203       //?M A I N - L I N E                                            ?
019600110203       //--------------------------------------------------------------
019700110203
019800110203      /free
019900110203
020000110203       // -?Operazioni iniziali?
020100110203       exsr  sr_RoutInz;
020200110203
020300111223       // -?Estrazione range segnacolli per filiale/serie?
020400110203       DoU  sqlCode <> *zero;
020500110203         exsr  sr_ReadCursor;
020600110203       EndDo;
020700110203
020800110203       // -?Operazioni finali?
020900110203       exsr  sr_RoutEnd;
021000110203
021100110203       //--------------------------------------------------------------
021200110203       //?Operazioni iniziali.                                         ?
021300110203       //--------------------------------------------------------------
021400110203       BEGSR sr_RoutInz;
021500110203
021600110203         *inLR = *on;
021700110203
021800110203         // -?Impostazione opzioni per SQL?
021900110203         exec SQL   set option   DynUsrPrf = *owner,
022000110203                                 CloSqlCsr = *endmod;
022100110914
022200110914         // -?Verifica del sistema AS/400 corrente?
022300110914         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
022400110914           exsr  sr_RoutEnd;
022500110914         endif;
022600110203
022700110203         // -?Creazione stringa SQL da eseguire?
022800110203         wSQL_C1 = 'WITH +
022900110913
023000110203                    Tab_3CP as +
023100111215                    (select substr(TBEke1,  1, 3) as Filiale, +
023200111215                            substr(TBEke1,  4, 4) as Cliente, +
023300111215                            substr(TBLuni, 79, 7) as Padre, +
023400120209                            substr(TBLuni, 36, 3) as FilSgnC, +
023500111215                            substr(TBEke2,  1, 2) as Serie, +
023600111215                            substr(TBEke2,  3, 7) as SgnCl_i, +
023700111215                            substr(TBEuni,  1, 7) as SgnCl_f +
023800111215                       from TNTBE00F inner join TABEL00F +
023900111215                         on TBEcod = ''3CP'' and TBLcod = ''3C'' +
024000111215                        and TBEatb = '' '' and TBLflg = '' '' +
024100111215                        and TBEke1 = TBLkey), +
024200110913
024300110203                    Tab_3EW as +
024400110203                    (select substr(TBEke1,  2, 3) as Filiale, +
024500110203                            substr(TBEke1,  5, 4) as Cliente, +
024600111215                            substr(TBLuni, 79, 7) as Padre, +
024700120209                            substr(TBEuni,  4, 3) as FilSgnC, +
024800110203                            substr(TBEuni,  7, 2) as Serie, +
024900110203                            substr(TBEuni,  9, 7) as SgnCl_i, +
025000110203                            substr(TBEuni, 16, 7) as SgnCl_f +
025100111215                       from TNTBE00F inner join TABEL00F +
025200111215                         on TBEcod = ''3EW'' and TBLcod = ''3C'' +
025300111215                        and TBEatb = '' '' and TBLflg = '' '' +
025400111215                        and TBEke1 = ''0'' concat TBLkey),' ;
025500111215
025600111215         wSQL_C1 += ' +
025700111215                     Tab_3C as +
025800111215                    (select substr(TBLkey,  1, 3) as Filiale, +
025900111215                            substr(TBLkey,  4, 4) as Cliente, +
026000111215                            substr(TBLuni, 79, 7) as Padre, +
026100120209                            substr(TBLuni, 36, 3) as FilSgnC, +
026200111215                            substr(TBLuni,  1, 2) as Serie, +
026300111215                                                1 as SgnCl_i, +
026400111215                                          9999999 as SgnCl_f +
026500110913                       from TABEL00F +
026600110913                      where TBLcod = ''3C'' +
026700110913                        and TBLflg = '' '' +
026800111215                        and substr(TBLuni,  1, 2) > ''00'' +
026900111215                        and substr(TBLuni, 44, 5) <> ''ESWEB'' +
027000111215                        and substr(TBLkey,  1, 7) not in +
027100110913                            (select Tab_3CP.Filiale concat Tab_3CP.Cliente +
027200111215                               from Tab_3CP))';
027300111215
027400111215         wSQL_C1 += ' +
027500111215                     select * +
027600111215                       from Tab_3C +
027700111215
027800111215                      union +
027900111215
028000111215                     select * +
028100111215                       from Tab_3CP +
028200111215
028300111215                      union +
028400111215
028500111215                     select * +
028600111215                       from Tab_3EW +
028700110913
028800120209                     order by FilSgnC, Serie, SgnCl_i, SgnCl_f, +
028900120209                              Padre, Filiale, Cliente';
029000120209                     //order by Filiale, Serie, SgnCl_i, SgnCl_f, +
029100120209                     //         Padre, Cliente';
029200110203
029300110203         // -?Apertura cursore?
029400110203         exsr  sr_OpenCursor;
029500110914
029600110914         // -?Apertura file TITAS33C?
029700110914         if  %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
029800110914           wLibFileSede = 'GAITRAGRU/TITAS33C';
029900110914         else ;
030000110914           wLibFileSede = 'GAITRAGRPS/TITAS33C';
030100110914         endif;
030200110914         open  TITAS33C;
030300110203
030400110203       ENDSR;
030500110203
030600110203       //--------------------------------------------------------------
030700110203       //?Esecuzione del comando (già impostato).                      ?
030800110203       //--------------------------------------------------------------
030900110203       BEGSR  sr_ExecCmd;
031000110203
031100110203         clear Qcap0100;
031200110203         Qcabcsdh = *off;
031300110203         Qcapa    = *off;
031400110203         Qcacmdss = *off;
031500110203         Qcaerved = *allX'00';
031600110203
031700110203         clear Qusec;
031800110203         Qusbprv  = %size(Qusec);
031900110203
032000110203         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
032100110203                           %size(Qcap0100) : 'CPOP0100' : *omit :
032200110203                           0 : 0 : Qusec);
032300110203
032400110203         if  Qusei <> *blank;
032500110203           exsr  sr_PrintErr;
032600110203         endif;
032700110203
032800110203       ENDSR;
032900110203
033000110203       //--------------------------------------------------------------
033100110203       //?Stampa segnalazione dell'errore rilevato.                    ?
033200110203       //--------------------------------------------------------------
033300110203       BEGSR  sr_PrintErr;
033400110203
033500110203         // -?Stampa del Dump?
033600110203         Dump(A);
033700110203
033800110203         // -?Stampa del Job-Log?
033900110203         Qcmd = 'DSPJOBLOG job(*) output(*print)';
034000110203         exsr  sr_ExecCmd;
034100110203
034200110203         // -?Chiusura del programma?
034300110203         exsr  sr_RoutEnd;
034400110203
034500110203       ENDSR;
034600110203
034700110203       //--------------------------------------------------------------
034800110203       //?Apertura cursore C1.                                         ?
034900110203       //--------------------------------------------------------------
035000110203       BEGSR  sr_OpenCursor;
035100110203
035200110203         // -?Dichiarazione del cursore?
035300110203         exec sql  prepare S1  from :wSQL_C1;
035400110203         exec sql  declare C1  cursor for S1;
035500110203
035600110203         // -?Apertura del cursore?
035700110203         exec sql   open C1;
035800110203
035900110203       ENDSR;
036000110203
036100110203       //--------------------------------------------------------------
036200110203       //?Lettura cursore C1.                                          ?
036300110203       //--------------------------------------------------------------
036400110203       BEGSR  sr_ReadCursor;
036500110203
036600110203         clear  wSQLrecC1;
036700110203
036800110203         exec SQL  fetch next  from C1  into :wSQLrecC1;
036900110203
037000110203         select;
037100110203           // -?Fine lettura?
037200110203           when  sqlCode = 100;
037300110203             exsr  sr_RoutEnd;
037400110203           // -?Errore?
037500110203           when  sqlCode < *zero;
037600110203             exsr  sr_PrintErr;
037700110203           // -?Elaborazione?
037800110203           other;
037900110203             exsr  sr_ElabRec;
038000111223             SavSQLrec = wSQLrecC1;
038100110203         endsl;
038200110203
038300110203       ENDSR;
038400110203
038500110203       //--------------------------------------------------------------
038600110203       //?Chiusura cursore C1.                                         ?
038700110203       //--------------------------------------------------------------
038800110203       BEGSR  sr_CloseCursor;
038900110203
039000110203         // -?Chiusura del cursore?
039100110203         exec sql   close C1;
039200110203
039300110203       ENDSR;
039400110203
039500110203       //--------------------------------------------------------------
039600110203       //?Elaborazione singolo record estratto.                        ?
039700110203       //--------------------------------------------------------------
039800111223       BEGSR  sr_ElabRec;
039900111223
040000111223         // -?Rottura filiale cliente o numero serie?
040100111223         if  wSQLrecC1.sql_Filiale <> SavSQLrec.sql_Filiale  or
040200111223             wSQLrecC1.sql_Serie   <> SavSQLrec.sql_Serie;
040300111223           clear  ww;
040400111223           clear  sk_CliElab;
040500111223         endif;
040600111223
040700111223         // -?Verifica se cliente già elaborato?
040800111223         if  %lookup( wSQLrecC1.sql_Filiale + wSQLrecC1.sql_Cliente :
040900111223                      sk_CliElab ) > *zero;
041000111223           leavesr;
041100111223         endif;
041200111223
041300111223         // -?Aggiornamento elenco clienti elaborati?
041400111223         ww += 1;
041500111223         sk_CliElab(ww) = wSQLrecC1.sql_Filiale + wSQLrecC1.sql_Cliente;
041600111223
041700111223         // -?Creazione stringa SQL da eseguire?
041800111223         wSQL_C2 = 'WITH +
041900111223
042000111223                    Tab_3CP as +
042100111223                    (select substr(TBEke1,  1, 3) as Filiale, +
042200111223                            substr(TBEke1,  4, 4) as Cliente, +
042300111223                            substr(TBLuni, 79, 7) as Padre, +
042400120209                            substr(TBLuni, 36, 3) as FilSgnC, +
042500111223                            substr(TBEke2,  1, 2) as Serie, +
042600111223                            substr(TBEke2,  3, 7) as SgnCl_i, +
042700111223                            substr(TBEuni,  1, 7) as SgnCl_f +
042800111223                       from TNTBE00F inner join TABEL00F +
042900111223                         on TBEcod = ''3CP'' and TBLcod = ''3C'' +
043000111223                        and TBEatb = '' '' and TBLflg = '' '' +
043100111223                        and TBEke1 = TBLkey +
043200111223                      where substr(TBLuni, 79, 7) <> ''' +
043300111223                            ( wSQLrecC1.sql_Filiale +
043400111223                              wSQLrecC1.sql_Cliente ) + ''' +
043500120209                        and substr(TBLuni, 36, 3) = ''' +
043600120209                              %editc( wSQLrecC1.sql_FilSgnC : 'X' ) + ''' +
043700111223                        and substr(TBEke2,  1, 2) = ''' +
043800111223                              %editc( wSQLrecC1.sql_Serie : 'X' ) +
043900111223                    '''),';
044000111223
044100111223         wSQL_C2 += ' +
044200111223                    Tab_3EW as +
044300111223                    (select substr(TBEuni,  4, 3) as Filiale, +
044400111223                            substr(TBEke1,  5, 4) as Cliente, +
044500111223                            substr(TBLuni, 79, 7) as Padre, +
044600120209                            substr(TBEuni,  4, 3) as FilSgnC, +
044700111223                            substr(TBEuni,  7, 2) as Serie, +
044800111223                            substr(TBEuni,  9, 7) as SgnCl_i, +
044900111223                            substr(TBEuni, 16, 7) as SgnCl_f +
045000111223                       from TNTBE00F inner join TABEL00F +
045100111223                         on TBEcod = ''3EW'' and TBLcod = ''3C'' +
045200111223                        and TBEatb = '' '' and TBLflg = '' '' +
045300111223                        and TBEke1 = ''0'' concat TBLkey +
045400111223                      where substr(TBLuni, 79, 7) <> ''' +
045500111223                            ( wSQLrecC1.sql_Filiale +
045600111223                              wSQLrecC1.sql_Cliente ) + ''' +
045700111223                        and substr(TBEuni,  4, 3) = ''' +
045800120209                              %editc( wSQLrecC1.sql_FilSgnC : 'X' ) + ''' +
045900111223                        and substr(TBEuni,  7, 2) = ''' +
046000111223                              %editc( wSQLrecC1.sql_Serie : 'X' ) +
046100111223                    '''),';
046200111223
046300111223         wSQL_C2 += ' +
046400111223                     Tab_3C as +
046500111223                    (select substr(TBLuni, 36, 3) as Filiale, +
046600111223                            substr(TBLkey,  4, 4) as Cliente, +
046700111223                            substr(TBLuni, 79, 7) as Padre, +
046800120209                            substr(TBLuni, 36, 3) as FilSgnC, +
046900111223                            substr(TBLuni,  1, 2) as Serie, +
047000111223                                                1 as SgnCl_i, +
047100111223                                          9999999 as SgnCl_f +
047200111223                       from TABEL00F +
047300111223                      where TBLcod = ''3C'' +
047400111223                        and TBLflg = '' '' +
047500111223                        and substr(TBLuni,  1, 2) > ''00'' +
047600111223                        and substr(TBLuni, 44, 5) <> ''ESWEB'' +
047700111223                        and substr(TBLkey,  1, 7) not in +
047800111223                            (select Tab_3CP.Filiale concat Tab_3CP.Cliente +
047900111223                               from Tab_3CP) +
048000111223                        and substr(TBLuni, 79, 7) <> ''' +
048100111223                            ( wSQLrecC1.sql_Filiale +
048200111223                              wSQLrecC1.sql_Cliente ) + ''' +
048300111223                        and substr(TBLuni, 36, 3) = ''' +
048400120209                              %editc( wSQLrecC1.sql_FilSgnC : 'X' ) + ''' +
048500120103                        and substr(TBLuni,  1, 2) = ''' +
048600111223                              %editc( wSQLrecC1.sql_Serie : 'X' ) +
048700111223                    ''')';
048800111223
048900111223         wSQL_C2 += ' +
049000111223                     select * +
049100111223                       from Tab_3C +
049200111223
049300111223                      union +
049400111223
049500111223                     select * +
049600111223                       from Tab_3CP +
049700111223
049800111223                      union +
049900111223
050000111223                     select * +
050100111223                       from Tab_3EW +
050200111223
050300111223                     order by Filiale, Serie, SgnCl_i, SgnCl_f, +
050400111223                              Padre, Cliente';
050500120103
050600120103         // -?*-=»> *TEMP x *TEST <«=-*?
050700120105         //wSQL_Cx_ds = wSQL_C2;
050800111223
050900111223         // -?Apertura cursore?
051000111223         exsr  sr_OpenCursor2;
051100111223
051200111223         // -?Estrazione range segnacolli per filiale/serie?
051300111223         DoU  sqlCode <> *zero;
051400111223           exsr  sr_ReadCursor2;
051500111223         EndDo;
051600111223
051700111223         // -?Chiusura cursore?
051800111223         exsr  sr_CloseCursor2;
051900120103
052000120103         clear  sqlCode;
052100111223
052200111223       ENDSR;
052300111223
052400111223       //--------------------------------------------------------------
052500111223       //?Apertura cursore C2.                                         ?
052600111223       //--------------------------------------------------------------
052700111223       BEGSR  sr_OpenCursor2;
052800111223
052900111223         // -?Dichiarazione del cursore?
053000111223         exec sql  prepare S2  from :wSQL_C2;
053100111223         exec sql  declare C2  cursor for S2;
053200111223
053300111223         // -?Apertura del cursore?
053400111223         exec sql   open C2;
053500111223
053600111223       ENDSR;
053700111223
053800111223       //--------------------------------------------------------------
053900111223       //?Lettura cursore C2.                                          ?
054000111223       //--------------------------------------------------------------
054100111223       BEGSR  sr_ReadCursor2;
054200111223
054300111223         clear  wSQLrecC2;
054400111223
054500111223         exec SQL  fetch next  from C2  into :wSQLrecC2;
054600111223
054700111223         select;
054800111223           // -?Fine lettura?
054900111223           when  sqlCode = 100;
055000111223           // -?Errore?
055100111223           when  sqlCode < *zero;
055200111223             exsr  sr_PrintErr;
055300111223           // -?Elaborazione?
055400111223           other;
055500111223             exsr  sr_ElabRec2;
055600111223         endsl;
055700111223
055800111223       ENDSR;
055900111223
056000111223       //--------------------------------------------------------------
056100111223       //?Chiusura cursore C2.                                         ?
056200111223       //--------------------------------------------------------------
056300111223       BEGSR  sr_CloseCursor2;
056400111223
056500111223         // -?Chiusura del cursore?
056600111223         exec sql   close C2;
056700111223
056800111223       ENDSR;
056900111223
057000111223       //--------------------------------------------------------------
057100111223       //?Elaborazione singolo record estratto (e sovrapposto).        ?
057200111223       //--------------------------------------------------------------
057300111223       BEGSR  sr_ElabRec2;
057400110203
057500110203         select;
057600110203
057700110203           // -?Controllo sovrapposizione range?
057800111223           when  (wSQLrecC2.sql_SgnCl_I < wSQLrecC1.sql_SgnCl_I   and
057900120111                  wSQLrecC2.sql_SgnCl_F < wSQLrecC1.sql_SgnCl_I)  or
058000120111                 (wSQLrecC2.sql_SgnCl_I > wSQLrecC1.sql_SgnCl_F   and
058100111223                  wSQLrecC2.sql_SgnCl_F > wSQLrecC1.sql_SgnCl_F);
058200110203             leavesr;
058300120105
058400120111           // -?Verifica se cliente già segnalato?
058500120111           //when  %lookup( wSQLrecC2.sql_Filiale + wSQLrecC2.sql_Cliente :
058600120111           //               sk_CliElab ) > *zero;
058700120111           //  leavesr;
058800110203
058900110203         endsl;
059000111223
059100111223         // -?Preparaz. dati del cliente dal 1° ciclo di lettura?
059200111223         //  ?della tab. "3C"?
059300111223         IF  wSQLrecC1.sql_Filiale + wSQLrecC1.sql_Cliente <> wCli_L1;
059400111223
059500111223           wCli_L1 = wSQLrecC1.sql_Filiale + wSQLrecC1.sql_Cliente;
059600111223
059700111223           clear  xx;
059800111223           clear  sk_Note1;
059900111223           clear  ds3Ca;
060000110203
060100111223           // -?Reperimento dati 1° cliente da tab. "3C"?
060200111223           If  getTabella ( c_Tabel : '3C' : wSQLrecC1.sql_Filiale +
060300111223                                             wSQLrecC1.sql_Cliente :
060400111223                            *omit : *omit : *omit :
060500111223                            *omit : *omit :
060600111223                            *omit : *omit : *omit : *omit :
060700111223                            *omit : *omit : *omit : *omit :
060800111223                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
060900111223                          < *zero       OR
061000111223               ds_TNTBE.TBEatb <> *blank;
061100111223
061200111223             // -?Preparazione segnalazione cliente non in "3C"?
061300111223             xx += 1;
061400111223             sk_Note1(xx) = 'Cliente ' + wSQLrecC1.sql_Filiale +
061500111223                                         wSQLrecC1.sql_Cliente +
061600120209                            ' non trovato in tab. 3C';
061700111223
061800111223           EndIf;
061900111223
062000111223           ds3Ca = ds_TNTBE.TBEuni;
062100111223
062200111223           // -?Reperimento "3C" x padre 1° cliente?
062300111223           If  %editc( ds3Ca.§3Ccks : 'X' ) <>
062400111223                 wSQLrecC1.sql_Filiale + wSQLrecC1.sql_Cliente;
062500111223
062600111223             if  getTabella ( c_Tabel : '3C' : %editc( ds3Ca.§3Ccks : 'X' ) :
062700111223                              *omit : *omit : *omit :
062800111223                              *omit : *omit :
062900111223                              *omit : *omit : *omit : *omit :
063000111223                              *omit : *omit : *omit : *omit :
063100111223                              ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
063200111223                            < *zero       OR
063300111223                 ds_TNTBE.TBEatb <> *blank;
063400111223
063500111223               // -?Preparazione segnalazione unificante non in "3C"?
063600111223               xx += 1;
063700120209               sk_Note1(xx) = 'Unific. ' + %editc( ds3Ca.§3Ccks : 'X' ) +
063800120209                              ' non trovato in tab. 3C';
063900111223
064000111223             endif;
064100111223
064200111223           EndIf;
064300111223
064400111223         ENDIF;
064500111223
064600111223         // -?Preparaz. dati del cliente dal 2° ciclo di lettura?
064700111223         //  ?della tab. "3C"?
064800111223         clear  yy;
064900111223         clear  sk_Note2;
065000111223         clear  ds3Cb;
065100110913
065200110913         // -?Reperimento dati 2° cliente da tab. "3C"?
065300120103         If  getTabella ( c_Tabel : '3C' : wSQLrecC2.sql_Filiale +
065400120103                                           wSQLrecC2.sql_Cliente :
065500110913                          *omit : *omit : *omit :
065600110913                          *omit : *omit :
065700110913                          *omit : *omit : *omit : *omit :
065800110913                          *omit : *omit : *omit : *omit :
065900110913                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
066000110913                        < *zero       OR
066100110913             ds_TNTBE.TBEatb <> *blank;
066200110913
066300111223           // -?Preparazione segnalazione cliente non in "3C"?
066400110913           yy += 1;
066500120103           sk_Note2(yy) = 'Cliente ' + wSQLrecC2.sql_Filiale +
066600120103                                       wSQLrecC2.sql_Cliente +
066700120209                          ' non trovato in tab. 3C';
066800110913
066900110913         EndIf;
067000110913
067100110913         ds3Cb = ds_TNTBE.TBEuni;
067200110913
067300110913         // -?Reperimento "3C" x padre 2° cliente?
067400110913         If  %editc( ds3Cb.§3Ccks : 'X' ) <>
067500120105               wSQLrecC2.sql_Filiale + wSQLrecC2.sql_Cliente;
067600110913
067700110916           if  getTabella ( c_Tabel : '3C' : %editc( ds3Cb.§3Ccks : 'X' ) :
067800110913                            *omit : *omit : *omit :
067900110913                            *omit : *omit :
068000110913                            *omit : *omit : *omit : *omit :
068100110913                            *omit : *omit : *omit : *omit :
068200110913                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
068300110913                          < *zero       OR
068400110913               ds_TNTBE.TBEatb <> *blank;
068500110913
068600110913             // -?Stampa segnalazione unificante non in "3C"?
068700110913             yy += 1;
068800120209             sk_Note2(yy) = 'Unific. ' + %editc( ds3Cb.§3Ccks : 'X' ) +
068900120209                            ' non trovato in tab. 3C';
069000110913
069100110913           endif;
069200110913
069300110913         EndIf;
069400110913
069500111223         // -?Stampa segnalazione range sovrapposto e/o?
069600110913         //                      ?padre non corrispondente?
069700111223         If  ds3Ca.§3Ccks <> ds3Cb.§3Ccks  or
069800111223                       xx  > *zero         or
069900111223                       yy  > *zero;
070000110913
070100110913           exsr  sr_Stampa;
070200110913
070300110913         EndIf;
070400110203
070500110203       ENDSR;
070600110203
070700110203       //--------------------------------------------------------------
070800110203       //?Stampa segnalazione segmento di pagina.                      ?
070900110203       //--------------------------------------------------------------
071000110203       BEGSR  sr_Stampa;
071100120111
071200120111         // -?Incremento n° totale sovrapposizioni?
071300120111         w_TotSovrapp += 1;
071400110203
071500110203         // -?Test OverFlow e stampa testata?
071600110203         exsr  sr_TestOF;
071700110204
071800111223         // -?DATI DEL CLIENTE ESTRATTO DAL 1° SQL:?
071900111223         IF  wSQLrecC1.sql_Filiale + wSQLrecC1.sql_Cliente <> wCli_L2;
072000111223
072100111223           wCli_L2 = wSQLrecC1.sql_Filiale + wSQLrecC1.sql_Cliente;
072200111223
072300120105           //if  not  $FirstRow;
072400120105             except  PrtSpace;
072500120105           //endif;
072600120105           //$FirstRow = *off;
072700111223
072800111223           // -?Decodifica 1° cliente?
072900111223           clear  TIBS69ds;
073000111223           //I69sif = knsif;
073100111223           I69kcc = 151;
073200111223           I69kac = %int( wSQLrecC1.sql_Filiale + wSQLrecC1.sql_Cliente );
073300111223           tibs69r( tibs69ds :
073400111223                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
073500111223
073600111223           // -?Reperimento data ultima spedizione con questa serie?
073700111223           //  ?per il 1° cliente?
073800111223           wNRS = %int(wSQLrecC1.sql_Serie);
073900111223           exsr  sr_GetLastSpedDta;
074000120105
074100120105           // -?Reperimento famiglia completa per padre (1° cliente)?
074200120105           If  %editc( ds3Ca.§3Ccks : 'X' ) = wSQLrecC1.sql_Filiale +
074300120105                                              wSQLrecC1.sql_Cliente;
074400120105             ubLeg3C_Rtv ( ds3Ca.§3Ccks :
074500120105                           ubLeg3C_FlgPF :
074600120105                           ubLeg3C_Padre :
074700120105                           ubLeg3C_SKC :
074800120105                           ubLeg3C_SKEW );
074900120105           EndIf;
075000111223
075100111223           // -?Stampa 1ª riga di dettaglio?
075200111223           s_Filiale = wSQLrecC1.sql_Filiale;
075300111223           s_Cliente = wSQLrecC1.sql_Cliente;
075400120209           s_FilSgnC = wSQLrecC1.sql_FilSgnC;
075500111223           s_Serie   = wSQLrecC1.sql_Serie;
075600111223           s_SgnCl_I = wSQLrecC1.sql_SgnCl_I;
075700111223           s_SgnCl_F = wSQLrecC1.sql_SgnCl_F;
075800111223           s_RagSoc  = ACOrag;
075900111223           s_Padre   = ds3Ca.§3Ccks;
076000120105           if  %editc( ds3Ca.§3Ccks : 'X' ) = wSQLrecC1.sql_Filiale +
076100120105                                              wSQLrecC1.sql_Cliente;
076200120105             s_NrFigli = %lookup( *zero : sch_SKC );
076300120105             select;
076400120105               when  s_NrFigli = *zero;
076500120105                 s_NrFigli = *hival;
076600120105               when  s_NrFigli <= 2;
076700120105                 clear  s_NrFigli;
076800120105               other;
076900120105                 s_NrFigli -= 2;
077000120105             endsl;
077100120105           else;
077200120105             clear  s_NrFigli;
077300120105           endif;
077400111223           clear  s_BloAnn;
077500111223           select;
077600111223             when  ACOflg <> *blank;
077700111223               s_BloAnn = 'ANNULLATO';
077800111223             when  ACOabl =  '8';
077900111223               s_BloAnn = 'BLOCCATO ';
078000111223           endsl;
078100111223           if  s_DtUltSpe > *zero;
078200111223             wDate_Eur = %date( s_DtUltSpe : *iso );
078300111223             s_DtUltSpe = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
078400111223                            ( %subdt(wDate_Eur : *months) * 10000 ) +
078500111223                            ( %subdt(wDate_Eur : *years) ) );
078600111223           endif;
078700111223           clear  s_Note;
078800111223           clear  xy;
078900111223           if  xx > *zero;
079000111223             xy = 1;
079100111223             s_Note = sk_Note1(xy);
079200111223           endif;
079300111223           except  PRTdet;
079400111223           DoW  xy < xx;
079500111223             xy += 1;
079600111223             s_Note = sk_Note1(xy);
079700111223             except  PRTnot;
079800111223           EndDo;
079900111223
080000111223         ENDIF;
080100110204
080200111223         // -?DATI DEL CLIENTE ESTRATTO DAL 2° SQL:?
080300120111
080400120111         // -?Verifica se 2° cliente già segnalato?
080500120111         if  %lookup( wSQLrecC2.sql_Filiale + wSQLrecC2.sql_Cliente :
080600120111                      sk_CliElab ) > *zero;
080700120111           leavesr;
080800120111         endif;
080900111223
081000120111         // -?Aggiornamento elenco clienti segnalati?
081100120111         ////  ?(se NON di serie completa)?
081200120111         //if  wSQLrecC2.sql_SgnCl_I > 1  and
081300120111         //    wSQLrecC2.sql_SgnCl_F < *hival;
081400120111         //  ww += 1;
081500120111         //  sk_CliElab(ww) = wSQLrecC2.sql_Filiale + wSQLrecC2.sql_Cliente;
081600120111         //endif;
081700120111         ww += 1;
081800120111         sk_CliElab(ww) = wSQLrecC2.sql_Filiale + wSQLrecC2.sql_Cliente;
081900111223
082000110204         // -?Decodifica 2° cliente?
082100110204         clear  TIBS69ds;
082200110204         //I69sif = knsif;
082300110204         I69kcc = 151;
082400111223         I69kac = %int( wSQLrecC2.sql_Filiale + wSQLrecC2.sql_Cliente );
082500110204         tibs69r( tibs69ds :
082600110204                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
082700110914
082800110914         // -?Reperimento data ultima spedizione con questa serie?
082900110914         //  ?per il 2° cliente?
083000111223         wNRS = %int(wSQLrecC2.sql_Serie);
083100110914         exsr  sr_GetLastSpedDta;
083200120105
083300120105         // -?Reperimento famiglia completa per padre (2° ciente)?
083400120105         If  %editc( ds3Cb.§3Ccks : 'X' ) = wSQLrecC2.sql_Filiale +
083500120105                                            wSQLrecC2.sql_Cliente;
083600120105           ubLeg3C_Rtv ( ds3Cb.§3Ccks :
083700120105                         ubLeg3C_FlgPF :
083800120105                         ubLeg3C_Padre :
083900120105                         ubLeg3C_SKC :
084000120105                         ubLeg3C_SKEW );
084100120105         EndIf;
084200110204
084300110204         // -?Stampa 2ª riga di dettaglio?
084400111223         s_Filiale = wSQLrecC2.sql_Filiale;
084500111223         s_Cliente = wSQLrecC2.sql_Cliente;
084600120209         s_FilSgnC = wSQLrecC2.sql_FilSgnC;
084700111223         s_Serie   = wSQLrecC2.sql_Serie;
084800111223         s_SgnCl_I = wSQLrecC2.sql_SgnCl_I;
084900111223         s_SgnCl_F = wSQLrecC2.sql_SgnCl_F;
085000110913         s_RagSoc  = ACOrag;
085100110913         s_Padre   = ds3Cb.§3Ccks;
085200120105         if  %editc( ds3Cb.§3Ccks : 'X' ) = wSQLrecC2.sql_Filiale +
085300120105                                            wSQLrecC2.sql_Cliente;
085400120105           s_NrFigli = %lookup( *zero : sch_SKC );
085500120105           select;
085600120105             when  s_NrFigli = *zero;
085700120105               s_NrFigli = *hival;
085800120105             when  s_NrFigli <= 2;
085900120105               clear  s_NrFigli;
086000120105             other;
086100120105               s_NrFigli -= 2;
086200120105           endsl;
086300120105         else;
086400120105           clear  s_NrFigli;
086500120105         endif;
086600110913         clear  s_BloAnn;
086700110204         select;
086800110204           when  ACOflg <> *blank;
086900110914             s_BloAnn = 'ANNULLATO';
087000110204           when  ACOabl =  '8';
087100110914             s_BloAnn = 'BLOCCATO ';
087200110204         endsl;
087300110914         if  s_DtUltSpe > *zero;
087400110914           wDate_Eur = %date( s_DtUltSpe : *iso );
087500110914           s_DtUltSpe = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
087600110914                          ( %subdt(wDate_Eur : *months) * 10000 ) +
087700110914                          ( %subdt(wDate_Eur : *years) ) );
087800110914         endif;
087900110913         clear  s_Note;
088000110914         clear  xy;
088100110913         if  yy > *zero;
088200110913           xy = 1;
088300111223           s_Note = sk_Note2(xy);
088400110913         endif;
088500110913         except  PRTdet;
088600110913         DoW  xy < yy;
088700110913           xy += 1;
088800111223           s_Note = sk_Note2(xy);
088900110913           except  PRTnot;
089000110913         EndDo;
089100110203
089200110203       ENDSR;
089300110914
089400110914       //--------------------------------------------------------------
089500110914       //?Reperimento data ultima spedizione per il cliente.           ?
089600110914       //--------------------------------------------------------------
089700110914       BEGSR  sr_GetLastSpedDta;
089800110914
089900110914         clear  s_DtUltSpe;
090000110914         k_TASccm = I69kac;
090100110914         k_TASaas = *year;
090200110914         k_TASmgs = *hival;
090300110914         setll   %kds(k03titas33)  TITAS33C;
090400110914         readPe  %kds(k03titas33 : 1)  TITAS33C;
090500110914
090600110914do  1    DoW  not %eof(TITAS33C);
090700110914
090800110914sel 2      select;
090900110914             // -?Spedizione senza segnacolli: da scartare?
091000110914w   2        when  TASncd <= *zeros  and  TASfns <> 'S';
091100110914             // -?Spedizione con serie diversa: da scartare?
091200110914w   2        when  TASnrs <> wNRS;
091300110914             // -?Ultima spedizione valida?
091400110914x   2        other;
091500110914               s_DtUltSpe = (TASaas * 10000) + TASmgs;
091600110914               leave;
091700110914e   2      endsl;
091800110914
091900110914           readPe  %kds(k03titas33 : 1)  TITAS33C;
092000110914
092100110914e   1    EndDo;
092200110914
092300110914       ENDSR;
092400110203
092500110203       //--------------------------------------------------------------
092600110203       //?Test OverFlow in stampa.                                     ?
092700110203       //--------------------------------------------------------------
092800110203       BEGSR  sr_TestOF;
092900120105
093000120105         // -?Stampa "Segue..."?
093100120105         if  *inOF;
093200120105           except PRTsegue;
093300120105         endif;
093400110203
093500110203         // -?Apertura file di stampa?
093600110203         if  not %open (QSYSPRT);
093700110203           Qcmd = c_OvrPrtF;
093800110203           exsr  sr_ExecCmd;
093900110203           open  QSYSPRT;
094000110203           *inOF = *on;
094100110203         endif;
094200110203
094300110203         // -?Test OverFlow e stampa testata?
094400110203         if  *inOF;
094500110203           w_Time = %char( %time() );
094600110203           except PRTtxt;
094700110203           *inOF  = *off;
094800120105           //reset  $FirstRow;
094900110203         endif;
095000110203
095100110203       ENDSR;
095200110203
095300110203       //--------------------------------------------------------------
095400110203       //?Operazioni finali.                                           ?
095500110203       //--------------------------------------------------------------
095600110203       BEGSR  sr_RoutEnd;
095700110203
095800110203         // -?Chiusura cursore?
095900110203         exsr  sr_CloseCursor;
096000110203
096100110203         // -?Stampa "fine-lista"?
096200120105         exsr  sr_TestOF;
096300110203         except  PRTend;
096400110203
096500110203         // -?Chiusura printer-file?
096600110203         close  QSYSPRT;
096700110203
096800110203         // -?Cancellazione override?
096900110203         Qcmd = c_DltOvr;
097000110203         exsr  sr_ExecCmd;
097100110204
097200110204         // -?Chiusura applicazioni?
097300110204         cloTabella();
097400120105         ubLeg3C_Close ();
097500110204         clear  TIBS69ds;
097600110204         I69tla = 'C';
097700110204         tibs69r( tibs69ds :
097800110204                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
097900110203
098000110203         // -?Uscita?
098100110203         return;
098200110203
098300110203       ENDSR;
098400110203
098500110203      /end-free
098600110203
098700110203       //--------------------------------------------------------------
098800110203       //?P R I N T E R - F I L E S                                    ?
098900110203       //--------------------------------------------------------------
099000110203
099100110203     oQSYSPRT   e            PRTtxt            2
099200110913     o                                              'ELENCO SOVRAPPOSI-
099300110913     o                                              Z. RANGE SEGNACOLL-
099400110913     o                                              I IN TAB. "3CP"/"3-
099500110913     o                                              EW"/"3C"'
099600110203     o                       SDSpgm           +   5
099700110203     o                       SDSusr           +   5
099800110913     o                       *date         y  +   3
099900110203     o                       w_Time           +   5
100000110203     o                                        +   5 'Pag.'
100100110203     o                       Page          z  +   0
100200110203     o          e            PRTtxt      0  0
100300110203     o                                              'ELENCO SOVRAPPOSI-
100400110913     o                                              Z. RANGE SEGNACOLL-
100500110913     o                                              I IN TAB. "3CP"/"3-
100600110913     o                                              EW"/"3C"'
100700110203     o          e            PRTtxt      2
100800110914     o                                              'Fil'
100900110914     o                                        +   1 'Cliente'
101000120105     o                                        +   0 '                     '
101100120105     o                                        +   1 '  Padre'
101200120105     o                                        +   1 'Figl'
101300120209     o                                        +   1 'FlS'
101400120105     o                                        +   1 'Sr'
101500120105     o                                        +   1 '  SgnCl_I'
101600120105     o                                        +   1 '  SgnCl_F'
101700120105     o                                        +   1 'Cl.Bloc. '
101800110914     o                                        +   1 'Dt.Ult.Sp.'
101900110203     o          e            PRTtxt      0  0
102000110914     o                                              'Fil'
102100110914     o                                        +   1 'Cliente'
102200120105     o                                        +   0 '                     '
102300120105     o                                        +   1 '  Padre'
102400120105     o                                        +   1 'Figl'
102500120209     o                                        +   1 'FlS'
102600120105     o                                        +   1 'Sr'
102700120105     o                                        +   1 '  SgnCl_I'
102800120105     o                                        +   1 '  SgnCl_F'
102900120105     o                                        +   1 'Cl.Bloc. '
103000110914     o                                        +   1 'Dt.Ult.Sp.'
103100110203     o          e            PRTtxt      1
103200110914     o                                              '==='
103300120105     o                                        +   1 '======='
103400120105     o                                        +   0 '====================='
103500120105     o                                        +   1 '======='
103600120105     o                                        +   1 '===='
103700120209     o                                        +   1 '==='
103800120105     o                                        +   1 '=='
103900120105     o                                        +   1 '========='
104000120105     o                                        +   1 '========='
104100120105     o                                        +   1 '======== '
104200110914     o                                        +   1 '=========='
104300120105     o*//       e            PRTtxt      0  1
104400120105     o          e            PRTtxt      0  0
104500110914     o                                              '==='
104600110914     o                                        +   1 '======='
104700120105     o                                        +   0 '====================='
104800120105     o                                        +   1 '======='
104900120105     o                                        +   1 '===='
105000120209     o                                        +   1 '==='
105100120105     o                                        +   1 '=='
105200120105     o                                        +   1 '========='
105300120105     o                                        +   1 '========='
105400120105     o                                        +   1 '======== '
105500110914     o                                        +   1 '=========='
105600120105      *
105700120105     o          e            PRTspace    1
105800110203      *
105900110913     o          e            PRTdet      1
106000110914     o                       s_Filiale
106100110914     o                       s_Cliente        +   1
106200110914     o                       s_RagSoc         +   1
106300110914     o                       s_Padre          +   1
106400120105     o                       s_NrFigli     z  +   1
106500120209     o                       s_FilSgnC     1  +   1
106600120105     o                       s_Serie       1  +   1
106700120105     o                       s_SgnCl_I     1  +   1
106800120105     o                       s_SgnCl_F     1  +   1
106900120105     o                       s_BloAnn         +   1
107000110914     o                       s_DtUltSpe       +   1 '  /  /    '
107100120105     o                       s_Note           +   1
107200110913      *
107300110913     o          e            PRTnot      1
107400120105     o                       s_Note           +  90
107500120105      *
107600120105     o          e            PRTsegue    1
107700120105     o                                        +  90 'Segue...'
107800110203      *
107900110203     o          e            PRTend      2
108000110203     o                                        +  10 '***  Fine Lista  +
108100110203     o                                               ***'
108200110203     o                                        +   3 'Tot. sovrapposizioni = '
108300110203     o                       w_TotSovrapp  k  +   1
108400110203     o          e            PRTend      0  0
108500110203     o                                        +  10 '***  Fine Lista  +
108600110203     o                                               ***'
