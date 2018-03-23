000100120323       //==============================================================
000200120323       //?Creazione anagrafica clienti collegati a potenziali          ?
000300120323       //==============================================================
000400120323
000500120323       //--------------------------------------------------------------
000600120323       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700120323       //--------------------------------------------------------------
000800120323
000900120326     /*MODULE *DELETE
001000120326     /*CRTPGM bnddir(TRUL)
001100120326     /*PRM output(*print) commit(*none) dbgview(*source)
001200120323     /*END
001300120323
001400120323       //--------------------------------------------------------------
001500120323       //?Specifiche di controllo.                                     ?
001600120323       //--------------------------------------------------------------
001700120323
001800120323     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001900120326     h*// dftactgrp(*no)
002000120323     h alwnull(*inputonly)
002100120323
002200120323       //--------------------------------------------------------------
002300120323       //?Dichiarazione file.                                          ?
002400120323       //--------------------------------------------------------------
002500120323
002600120323       // -?Organigramma filiale/aziendale?
002700120323     fAZORG01L  if   e           k disk
002800130918
002900130918       // -?Anagrafica commerciali?
003000130918     fAZCMM01L  if   e           k disk
003100120327
003200120327       // -?File Variazioni anagrafiche?
003300120327     fCNACVT1L  if   e           k disk
003400120323
003500120323       // -?Anagrafica clienti collegate a potenziale?
003600120323     fTIACP00F  O    e             disk    usropn
003700120323
003800120323       //--------------------------------------------------------------
003900120323       //?Definizione costanti.                                        ?
004000120323       //--------------------------------------------------------------
004100120323
004200120323       // -?Codice utente conti?
004300120323     d c_Kut           c                   const(1)
004400120323
004500120323       // -?Comandi di override al PrtF?
004600120323     d c_CmdOvrPrtF    c                   const('OVRPRTF +
004700120323     d                                           file(PRTEMAIL) +
004800120323     d                                           pagesize(66 132) +
004900120323     d                                           lpi(6) cpi(10) +
005000120323     d                                           ovrscope(*actgrpdfn) +
005100120323     d                                           ')
005200120323     d c_CmdDltOvr     c                   const('DLTOVR +
005300120323     d                                            file(PRTEMAIL) +
005400120323     d                                            lvl(*actgrpdfn)')
005500120329
005600120329       // -?Costante per controllo "caratteri solo numerici"?
005700120329     d c_Digits        c                   const('0123456789')
005800120323
005900120323       //--------------------------------------------------------------
006000120323       //?Definizione schiere.                                         ?
006100120323       //--------------------------------------------------------------
006200120323
006300120323
006400120323       //--------------------------------------------------------------
006500120323       //?Definizione aree dati.                                       ?
006600120323       //--------------------------------------------------------------
006700120323
006800120323       // -?Dati utente?
006900120323     d §AzUte        e ds                  extname(AZUTE00F)
007000120323     d                                     dtaara
007100120323     d §DatiUte      e ds                  extname(dDatiUte)
007200120323     d                                     dtaara
007300120323
007400120323       //--------------------------------------------------------------
007500120323       //?Definizione strutture dati.                                  ?
007600120323       //--------------------------------------------------------------
007700120323
007800120329       // -?Parametri ricevuti?
007900120329     d kpjba         e ds
008000120329     d trmkQ6ds        ds                  inz
008100120329     d   pIn_AaM                      6s 0 inz
008200120323
008300120323       // -?Comodo per la definizione di campi?
008400120323     d CNACOds       e ds                  extname(CNACO00F)
008500120323     d                                     qualified  based(nullPtr)
008600120323     d CNINDds       e ds                  extname(CNIND00F)
008700120323     d                                     qualified  based(nullPtr)
008800120323     d CNCLPds       e ds                  extname(CNCLP00F)
008900120323     d                                     qualified  based(nullPtr)
009000120323     d TNCPOds       e ds                  extname(TNCPO00F)
009100120323     d                                     qualified  based(nullPtr)
009200120323     d TABELds       e ds                  extname(TABEL00F)
009300120323     d                                     qualified  based(nullPtr)
009400120323     d TFNTCds       e ds                  extname(TFNTC00F)
009500120323     d                                     qualified  based(nullPtr)
009600130214
009700130214       // -?DS pgm ricerca data blocco cliente
009800130214     d TRULBLCDS     e ds
009900120323
010000120323       // -?Tabelle usate:?
010100120323       // ·?Tab. "BI" = Causali Blocco Cliente?
010200120323     d dsBI          e ds                  inz  qualified
010300120323
010400120323       // -?Dati estratti via SQL?
010500120326     d wSQL_dati       ds                  inz
010600120323     d   wSQL_kcc                          inz  like(cnACOds.acoKCC)
010700120323     d   wSQL_ksc                          inz  like(cnACOds.acoKSC)
010800120323     d   wSQL_rag                          inz  like(cnACOds.acoRAG)
010900120323     d   wSQL_lib                          inz  like(cnACOds.acoLIB)
011000120323     d   wSQL_abl                          inz  like(cnACOds.acoABL)
011100120323     d   wSQL_itc                          inz  like(cnACOds.acoITC)
011200120323     d   wSQL_dps                          inz  like(cnCLPds.clpDPS)
011300120323     d   wSQL_dus                          inz  like(cnCLPds.clpDUS)
011400120323     d   wSQL_clv                          inz  like(cnCLPds.clpCLV)
011500120323     d   wSQL_nar                          inz  like(cnCLPds.clpNAR)
011600120323     d   wSQL_age                          inz  like(cnCLPds.clpAGE)
011700120323     d   wSQL_via                          inz  like(cnINDds.indVIA)
011800120328     d   wSQL_cap                          inz  like(cnINDds.indCAE)
011900120323     d   wSQL_cit                          inz  like(cnINDds.indCIT)
012000120323     d   wSQL_prv                          inz  like(cnINDds.indPRV)
012100120323     d   wSQL_sta                          inz  like(cnINDds.indSTA)
012200120323     d   wSQL_tel                          inz  like(cnINDds.indTEL)
012300120323     d   wSQL_cdf                          inz  like(cnINDds.indCDF)
012400120323     d   wSQL_iva                          inz  like(cnINDds.indIVA)
012500120323     d   wSQL_cpoRag                       inz  like(tnCPOds.cpoRAG)
012600130918     d*//wSQL_CMMcod                       inz  like(CMMcod)
012700130918     d   wSQL_CMMdes                       inz  like(CMMdes)
012800130918     d   wSQL_CMMuni                       inz  like(CMMuni)
012900120323     d*//wSQL_kt_BI                        inz  like(TABELds.tblKEY)
013000120323     d   wSQL_dt_BI                        inz  like(TABELds.tblUNI)
013100120323     d*//wSQL_NTC5nk1                      inz  like(TFNTCds.ntcNK1)
013200120323     d   wSQL_NTC5rnt                      inz  like(TFNTCds.ntcRNT)
013300120323     d*//wSQL_NTC6nk1                      inz  like(TFNTCds.ntcNK1)
013400120323     d   wSQL_NTC6rnt                      inz  like(TFNTCds.ntcRNT)
013500120326
013600120326       // -?Comodo: Stringa Sql?
013700120326     d ds_SQL          ds          4140    inz
013800120326     d   $ds_SQL                     60    inz  dim(69)
013900120323
014000120323       //--------------------------------------------------------------
014100120323       //?Definizione variabili.                                       ?
014200120323       //--------------------------------------------------------------
014300120323
014400120323       // -?Flags booleani?
014500120323     d $Fine           s               n   inz
014600120323
014700120323       // -?Campi di comodo?
014800120323     d wDate           s              8  0 inz
014900120329     d wDate_Iso       s               d   inz  datfmt(*iso)
015000120323
015100120323       // -?Stringa SQL da eseguire?
015200120323     d wSQL            s           4096    inz  varying
015300120323
015400120323       //--------------------------------------------------------------
015500120323       //?Definizione prototipi procedure.                             ?
015600120323       //--------------------------------------------------------------
015700120323
015800120323       // -?Reperimento dati utente?
015900120323     d TIBS34ds      e ds                  inz
016000120323      /copy gaitrasrc/srcProtoPR,TIBS34R
016100120323
016200120323       // -?Recupero codici clienti ed unificanti "TIBS10R"?
016300120323      /copy gaitrasrc/srcProtoPI,TIBS10R
016400120323      /copy gaitrasrc/srcProtoPR,TIBS10R
016500120323
016600120323       // -?Controllo/Decodifica cliente?
016700120323      /copy gaitrasrc/srcProtoPI,TIBS69R
016800120323      /copy gaitrasrc/srcProtoPR,TIBS69R
016900120326
017000120326       // -?Reperimento dati tabelle?
017100120326      /copy gaitrasrc/srcProtoPI,TRULTAB
017200120326      /copy gaitrasrc/srcProtoPR,TRULTAB
017300120403
017400120403       // -?Controllo ed inversione date?
017500120403     d WLBdat          ds                  inz
017600120403     d  G08dat                        8  0 inz
017700120403     d  G08inv                        8  0 inz
017800120403     d  G08err                        1    inz('3')
017900120403     d  G08tgi                        5  0 inz
018000120403      /copy gaitrasrc/srcProtoPr,XSRDA8
018100120323
018200120323       // -?Parametri API QCAPCMD (Process Commands)?
018300120323     d Qcmd            s           2048    inz  varying
018400120323      /copy qSysInc/qRpgleSrc,QCAPCMD
018500120323       // -?API QCAPCMD (Process Commands)?
018600120323      /copy gaitrasrc/srcProtoPR,QCAPCMD
018700120323
018800120323       // -?Parametri gestione errori API.?
018900120323      /copy qsysinc/qrpglesrc,QUSEC
019000130214
019100130214       // -?PGM ricerca data blocco cliente
019200130214     d TRULBLCR        pr                  extpgm('TRULBLCR')
019300130214     d  KPJBA                              likeds(KPJBA)
019400130214     d  TRULBLCDS                          likeds(TRULBLCDS)
019500120323
019600120323       //--------------------------------------------------------------
019700120323       //?Definizione key-list.                                        ?
019800120323       //--------------------------------------------------------------
019900120323
020000120327       // -?Dettaglio variazioi anagrafiche?
020100120327     d k04cnacvT1    e ds                  extname(CNACVT1L : *key)
020200120327     d                                     prefix(k_)
020300120327     d                                     inz
020400120323
020500120323       //--------------------------------------------------------------
020600120323       //?Definizione parametri procedura.                             ?
020700120323       //--------------------------------------------------------------
020800120323
020900120329     c     *Entry        plist
021000120329     c                   parm                    kpjba
021100120323
021200120323      /free
021300120323
021400120323       //--------------------------------------------------------------
021500120323       //?M A I N - L I N E                                            ?
021600120323       //--------------------------------------------------------------
021700120323
021800120323       // -?Operazioni iniziali?
021900120323       exsr sr_RoutInz;
022000120323
022100120323       // -?Ciclo di reperimento clienti in tab. "3C"?
022200120323       DoW  Not $Fine;
022300120323         exsr  sr_ReadCursor;
022400120323       EndDo;
022500120323
022600120323       // -?Operazioni finali?
022700120323       exsr sr_RoutEnd;
022800120323
022900120323       //--------------------------------------------------------------
023000120323       //?Operazioni iniziali.                                         ?
023100120323       //--------------------------------------------------------------
023200120323       BEGSR  sr_RoutInz;
023300120323
023400120323         exec sql  set option  dynusrprf = *owner,
023500120323                               closqlcsr = *endmod;
023600120323
023700120323         *inLR = *on;
023800120323
023900120323         // -?Reperimento dati job?
024000120323         exsr  sr_DatiJob;
024100120323
024200120323         // -?Reperimento data odierna (fmt aaaa/mm/gg)?
024300120329         if  %parms() > *zero  and
024400120329             %check( c_Digits : %subst( kpjbu : 1 : 6 ) ) = *zero;
024500120329           TRMKQ6ds  = kpjbu;
024600120329           wDate     = ( pIn_AaM * 100 ) + 01 ;
024700120329           wDate_Iso = %date( wDate : *iso ) + %months(1) - %days(1);
024800120329           wDate     = %dec( wDate_Iso );
024900120329         else;
025000120329           wDate = %dec( %date() );
025100120329         endif;
025200120323
025300120323         // -?Pulizia archivio?
025400120405         Qcmd = 'CLRPFM file(*libl/TIACP00F)';
025500120323         exsr  sr_ExecCmd;
025600120326         if  Qusei <> *blank;
025700120326           exsr  sr_PrintErr;
025800120326           $Fine = *on;
025900120326           leavesr;
026000120326         endif;
026100120323
026200120323         // -?Apertura archivio?
026300120323         open  TIACP00F;
026400120323
026500120323         // -?Preparazione stringa SQL?
026600120323         exsr  sr_PrepSQL;
026700120323
026800120323         // -?Apertura cursore SQL?
026900120323         exsr  sr_OpenCursor;
027000120323
027100120323       ENDSR;
027200120323
027300120323       //--------------------------------------------------------------
027400120323       //?Reperimento Dati del job (Utente/Operativi).                 ?
027500120323       //--------------------------------------------------------------
027600120323       BEGSR  sr_DatiJob;
027700120323
027800120323         in(e) §AzUte;
027900120323         if NOT %error;
028000120323           in(e) §DatiUte;
028100120323         endif;
028200120323         if %error or RSut = *blank;
028300120323           tibs34r ( tibs34ds );
028400120323           in §AzUte;
028500120323           in §DatiUte;
028600120323         endif;
028700120323
028800120323       ENDSR;
028900120323
029000120323       //--------------------------------------------------------------
029100120323       //?Preparazione stringa SQL                                     ?
029200120323       //--------------------------------------------------------------
029300120323       BEGSR  sr_PrepSQL;
029400120323
029500120323         // -?Parte generale?
029600120405         //  ?Estrae anche clienti:?
029700120405         //  ?· annullati [acoFLG = ' ', acoFLG = clpFLG, acoFLG = indFLG]?
029800120405         //  ?· di fililale NON più operativa [substr(ORGde8, 1, 1) <> ' ']?
029900120405         //  ?ma scarta quelli di Sede [ORGfl3 <> '9']?
030000120323         wSQL = 'with +
030100120323
030200120323                   CNACO as (select acoKCC, acoKSC, acoRAG, acoLIB, acoABL, +
030300120323                                    acoITC, +
030400120323                                    clpDPS, clpDUS, clpCLV, clpNAR, clpAGE, +
030500120328                                    indVIA, indCAE, indCIT, indPRV, indSTA, +
030600120323                                    indTEL, indCDF, indIVA, +
030700120326                                    case when cpoRAG > '''' then cpoRAG +
030800120405                                                            else '''' end +
030900120323
031000120323                               from CNACO00F +
031100120323
031200120323                                             inner join CNCLP00F +
031300120405                                 on acoKUT = clpKUT and +
031400120323                                    acoKCC = clpKCC and +
031500120323                                    acoKSC = clpKSC +
031600120323
031700120323                                             inner join CNIND00F +
031800120405                                 on acoKUT = indKUT and +
031900120323                                    acoKCC = indKCC and +
032000120323                                    acoKSC = indKSC +
032100120323
032200120323                                             left outer join TNCPO00F +
032300120323                                 on acoLIB = cpoCPO +
032400120323
032500120405                              where acoKUT = ' + %char(c_Kut) +
032600120326                              ' and acoKCC = ' +
032700120405                                    %editc( DUTkci : '3' ) + '), +
032800120323
032900120323                   AZORG as (select ORGfil +
033000120405                               from AZORG00F +
033100120405                              where ORGfl3 <> ''9''), +
033200130918
033300130918                   AZCMM as (select CMMcod, CMMdes, CMMuni +
033400130918                               from AZCMM00F), +
033500120323
033600120323                  Tab_BI as (select TBLkey, TBLuni +
033700120323                               from TABEL00F +
033800120326                              where TBLkut = ' + %char(c_Kut) +
033900120326                              ' and TBLcod = ''BI''), +
034000120323
034100120323                 TFNTC05 as (select ntcNK1, ntcRNT +
034200120323                               from TFNTC00F +
034300120323                              where ntcAPL = ''C'' +
034400120323                                and ntcTNT = ''05'' +
034500120323                                and ntcNK1 like ''' +
034600120323                                    %editc(DUTkci : 'X') + '%''), +
034700120323
034800120323                 TFNTC06 as (select ntcNK1, ntcRNT +
034900120323                               from TFNTC00F +
035000120323                              where ntcAPL = ''C'' +
035100120323                                and ntcTNT = ''06'' +
035200120323                                and ntcNK1 like ''' +
035300120323                                    %editc(DUTkci : 'X') + '%'')  +
035400120323
035500120326                 select CNACO.*, +
035600130918                        case when AZCMM.CMMdes > '''' then AZCMM.CMMdes +
035700130918                                                      else '''' end, +
035800130918                        case when AZCMM.CMMuni > 0    then AZCMM.CMMuni +
035900130918                                                      else 0 end, +
036000120323                        case when Tab_BI.TBLuni > '''' then Tab_BI.TBLuni +
036100120323                                                       else '''' end, +
036200120323                        case when TFNTC05.NTCrnt > '''' then TFNTC05.NTCrnt +
036300120323                                                        else '''' end, +
036400120323                        case when TFNTC06.NTCrnt > '''' then TFNTC06.NTCrnt +
036500120323                                                        else '''' end +
036600120323
036700120323                   from CNACO +
036800120323
036900120323                              inner join AZORG +
037000120323                     on substr(digits(CNACO.ACOksc), 1, 3) = digits(ORGfil) +
037100130918
037200130918                              left outer join AZCMM +
037300130918                     on CNACO.clpAGE = AZCMM.CMMcod +
037400120323
037500120323                              left outer join Tab_BI +
037600120323                     on CNACO.clpNAR = Tab_BI.TBLkey +
037700120323
037800120323                              left outer join TFNTC05 +
037900120323                     on digits(CNACO.acoKCC) concat digits(CNACO.acoKSC) = +
038000120323                        TFNTC05.ntcNK1 +
038100120323
038200120323                              left outer join TFNTC06 +
038300120323                     on digits(CNACO.acoKCC) concat digits(CNACO.acoKSC) = +
038400120323                        TFNTC06.ntcNK1 +
038500120323
038600120323                  order by CNACO.acoKSC +
038700120323
038800120323                    for fetch only';
038900120326
039000120326         ds_Sql = wSql;
039100120323
039200120323       ENDSR;
039300120323
039400120323       //--------------------------------------------------------------
039500120323       //?Apertura cursore.                                            ?
039600120323       //--------------------------------------------------------------
039700120323       BEGSR  sr_OpenCursor;
039800120323
039900120323         // -?Dichiarazione cursore?
040000120323         exec sql   prepare S1   from :wSQL;
040100120323         exec sql   declare C1   cursor for S1;
040200120323
040300120323         // -?Apertura del cursore?
040400120323         exec sql   open C1;
040500120323
040600120323         if  SQLcode < *zero;
040700120326           exsr  sr_PrintErr;
040800120323           $Fine = *on;
040900120323         endif;
041000120323
041100120323       ENDSR;
041200120323
041300120323       //--------------------------------------------------------------
041400120323       //?Lettura cursore.                                             ?
041500120323       //--------------------------------------------------------------
041600120323       BEGSR  sr_ReadCursor;
041700120323
041800120323         clear  wSQL_dati;
041900120323
042000120323         exec sql   fetch next   from C1   into :wSQL_dati;
042100120323
042200120323         select;
042300120323
042400120323           when  SQLcode < *zero;
042500120326             exsr  sr_PrintErr;
042600120323             $Fine = *on;
042700120323
042800120323           when  SQLcode = 100;
042900120323             $Fine = *on;
043000120323
043100120323           other;
043200120323             exsr  sr_ElabCliente;
043300120323
043400120323         endsl;
043500120323
043600120323       ENDSR;
043700120323
043800120323       //--------------------------------------------------------------
043900120323       //?Chiusura cursore.                                            ?
044000120323       //--------------------------------------------------------------
044100120323       BEGSR  sr_CloseCursor;
044200120323
044300120323         // -?Chiusura del cursore?
044400120323         exec sql   close C1;
044500120323
044600120323       ENDSR;
044700120323
044800120323       //--------------------------------------------------------------
044900120323       //?Elaborazione singolo cliente.                                ?
045000120323       //--------------------------------------------------------------
045100120323       BEGSR  sr_ElabCliente;
045200120329
045300120329         // -?Letto un secondo record da TFNTC: scartare?
045400120329         //  ?(è già stato registrato il primo record)?
045500120329         if  wSQL_ksc = acpKSC;
045600120329           leavesr;
045700120329         endif;
045800120323
045900120329         // -?PREPARAZIONE nuovo record?
046000120323         clear  TIACP000;
046100120323
046200120326         clear  dsBI;
046300120326         if  wSQL_dt_BI <> *blank;
046400120326           dsBI = wSQL_dt_BI;
046500120326         endif;
046600130918         if  wSQL_CMMuni  > *zero  and
046700130918             wSQL_CMMuni <> CLPage;
046800130918           chain  (wSQL_CMMuni)  AZCMM000;
046900130918           if  %found(AZCMM01L);
047000130918             wSQL_CMMdes = CMMdes;
047100130918           else;
047200130918             clear  wSQL_CMMdes;
047300120326           endif;
047400120326         endif;
047500120323
047600120323         // -?Codice Cliente          (Figlio)?
047700120323         acpKSC = wSQL_ksc;
047800120323         // -?Ragione Sociale Cliente (Figlio)?
047900120323         acpRSC = wSQL_rag;
048000120323
048100120323         // -?Reperimento codice Padre?
048200120323         clear tibs10ds;
048300120323         TIBS10ds.D10drf = wDate;
048400120323         TIBS10ds.D10tle = 'ST';
048500120323         TIBS10ds.D10paf = 'P';
048600120323         TIBS10ds.D10cod = wSQL_ksc;
048700120323         tibs10r ( TIBS10ds );
048800120323         if  TIBS10ds.D10err  = *blank   and
048900120323             TIBS10ds.D10cod <> TIBS10ds.D10cop;
049000120323           acpKSU = TIBS10ds.D10cop;
049100120323         else;
049200120323           acpKSU = TIBS10ds.D10cod;
049300120323         endif;
049400120323
049500120323         // -?Decodifica Padre?
049600120323         clear  TIBS69ds;
049700120403         if  %parms() > *zero;
049800120403           I69sif = knsif;
049900120403         endif;
050000120323         I69kcc = DUTkci;
050100120323         I69kac = ACPksu;
050200120323         tibs69r( tibs69ds :
050300120323                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
050400120323         if  O69err = *on;
050500120323           acpRSU = *all'? ';
050600120323         else;
050700120323           acpRSU = %subst( ACOrag : 1 : %len(acpRSU) );
050800120323         endif;
050900120323
051000120323         // -?Codice Potenziale?
051100120323         acpCPO = wSQL_lib;
051200120323         // -?Ragione Sociale Potenziale?
051300120323         acpRSP = wSQL_cpoRag;
051400120323
051500120323         // -?Tipo Cliente?
051600120326         acpCLV = wSQL_clv;
051700120323
051800120323         // -?Codice Causale Blocco?
051900120323         acpCBC = wSQL_nar;
052000120323         // -?Descrizione Causale Blocco?
052100120323         acpCBD = dsBI.§BIdes;
052200120327         // -?Data Blocco?
052300120327         If  wSQL_abl <> *blank;
052400120327           acpDBC = 00010101;
052500130214
052600130214           //?Richiamo pgm x ricerca data blocco cliente?
052700130214           clear TRULBLCDS;
052800130214           IBLCkcc = wSQL_kcc;
052900130214           IBLCksc = wSQL_ksc;
053000130214           trulblcr (KPJBA:TRULBLCDS);
053100130214           IF  OBLCdav > 0;
053200130214             ACPdbc = OBLCdav;
053300130214           ENDIF;
053400130214
053500130214           //clear  k04cnacvT1;
053600130214           //k_ACVkcc = wSQL_kcc;
053700130214           //k_ACVksc = wSQL_ksc;
053800130214           //setll  %kds(k04cnacvT1 : 2)  CNACVT00;
053900130214           //reade  %kds(k04cnacvT1 : 2)  CNACVT00;
054000130214           //DoW  not  %eof(CNACVT1L);
054100130214           //  if  ACVcta = 'M'  and  %scan('B' : ACVflo) > *zero;
054200130214           //    acpDBC = ACVdav;
054300130214           //    leave;
054400130214           //  endif;
054500130214           //  reade  %kds(k04cnacvT1 : 2)  CNACVT00;
054600130214           //EndDo;
054700130214
054800120327         EndIf;
054900120327
055000120327         // -?Data Prima Spedizione?
055100120403         acpDPS = wSQL_dps;
055200120403         reset  WLBdat;
055300120403         if  wSQL_dps <> *zero;
055400120403           G08inv = wSQL_dps;
055500120403           xsrda8 ( WLBdat );
055600120403           if  G08err = *off;
055700120403             acpDPS = G08inv;
055800120403           endif;
055900120403         endif;
056000120327         // -?Data Ultima Spedizione?
056100120403         acpDUS = wSQL_dus;
056200120403         reset  WLBdat;
056300120403         if  wSQL_dus <> *zero;
056400120403           G08inv = wSQL_dus;
056500120403           xsrda8 ( WLBdat );
056600120403           if  G08err = *off;
056700120403             acpDUS = G08inv;
056800120403           endif;
056900120403         endif;
057000120323
057100120323         // -?Nazione Partita IVA  &  Partita IVA  Cliente?
057200120323         if  %subst( wSQL_iva : 1 : 2 ) >= '00';
057300120323           acpCPI = wSQL_iva;
057400120323         else;
057500120323           acpNPI = %subst( wSQL_iva : 1 : %len( acpNPI ) ) ;
057600120326           acpCPI = %subst( wSQL_iva : %len( acpNPI ) + 1 );
057700120323         endif;
057800120323         // -?Codice Fiscale Cliente?
057900120323         acpCFI = wSQL_cdf;
058000120323
058100120323         // -?Codice Commerciale (Unificante)?
058200130918         if  wSQL_CMMuni > *zero;
058300130918           acpCMM = wSQL_CMMuni;
058400120326         else;
058500120326           acpCMM = wSQL_age;
058600120326         endif;
058700130918         // -?Descrizione Commerciale (Unificante)?
058800130918         acpCMD = wSQL_CMMdes;
058900120323
059000120323         // -?Codice Area Commerciale?
059100130918         if  wSQL_CMMuni > *zero;
059200130918           orgFIL = wSQL_CMMuni / 10000;
059300120326           chain  (orgFIL)  AZORG;
059400120326           if  %found(AZORG01L)  and  ORGfva = *blank;
059500120326             acpCAR = orgCAR;
059600120326           endif;
059700120326         endif;
059800120323
059900120323         // -?Categoria Merceologica?
060000120323         acpCME = wSQL_itc;
060100120323
060200120323         // -?Indirizzo?
060300120323         acpIND = wSQL_via;
060400120323         // -?C.A.P.?
060500120323         acpCAP = wSQL_cap;
060600120323         // -?Località?
060700120323         acpLOC = wSQL_cit;
060800120323         // -?Provincia?
060900120323         acpPRO = wSQL_prv;
061000120323         // -?Nazione?
061100120323         acpNAZ = wSQL_sta;
061200120323         // -?Telefono?
061300120323         acpTEL = wSQL_tel;
061400120323
061500120323         // -?Responsabile Trasporti?
061600120323         acpRST = wSQL_NTC5rnt;
061700120323         // -?e-Mail Responsabile Trasporti?
061800120323         acpMAI = wSQL_NTC6rnt;
061900120323
062000120323
062100120323         // -?Scrittura record?
062200120323         //______________
062300120323         write  TIACP000;
062400120323         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
062500120323
062600120323       ENDSR;
062700120323
062800120323       //--------------------------------------------------------------
062900120323       //?Esecuzione del comando (già impostato).                      ?
063000120323       //--------------------------------------------------------------
063100120323       BEGSR  sr_ExecCmd;
063200120323
063300120323         clear Qcap0100;
063400120323         Qcabcsdh = *off;
063500120323         Qcapa    = *off;
063600120323         Qcacmdss = *off;
063700120323         Qcaerved = *allX'00';
063800120323
063900120323         clear Qusec;
064000120323         Qusbprv  = %size(Qusec);
064100120323
064200120323         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
064300120323                           %size(Qcap0100) : 'CPOP0100' : *omit :
064400120323                           0 : 0 : Qusec);
064500120323
064600120326         //if  Qusei <> *blank;
064700120326         //  ...;
064800120326         //endif;
064900120323
065000120323       ENDSR;
065100120323
065200120323       //--------------------------------------------------------------
065300120326       //?Stampa segnalazione dell'errore rilevato.                    ?
065400120323       //--------------------------------------------------------------
065500120326       BEGSR  sr_PrintErr;
065600120323
065700120323         // -?Stampa del Dump?
065800120323         Dump(A);
065900120323
066000120323         // -?Stampa del Job-Log?
066100120323         Qcmd = 'DSPJOBLOG job(*) output(*print)';
066200120326         exsr  sr_ExecCmd;
066300120323
066400120323       ENDSR;
066500120323
066600120323       //--------------------------------------------------------------
066700120323       //?Operazioni finali.                                           ?
066800120323       //--------------------------------------------------------------
066900120323       BEGSR  sr_RoutEnd;
067000120323
067100120323         // -?Chiusura cursore SQL?
067200120323         exsr  sr_CloseCursor;
067300120323
067400120323         // -?Chiusura archibio TIACP00F?
067500120323         close  TIACP00F;
067600120323
067700120323         // -?Chiusura pgm?
067800120323         return;
067900120323
068000120323       ENDSR;
068100120323
068200120323      /end-free
