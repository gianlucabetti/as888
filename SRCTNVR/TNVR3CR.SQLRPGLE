000100101018       //==============================================================
000200101018       //?Controllo serie-segnacollo ("3C") utilizzate dal cliente     ?
000300101018       //==============================================================
000400101018
000500101018       //--------------------------------------------------------------
000600101018       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700101018       //--------------------------------------------------------------
000800101018
000900110418     /*PRM dbgview(*source)
001000101018     /*PRM commit(*none)
001100110418     /*CMD ovrdbf file(TITAS31C) tofile(GAITRAGRPS/TITAS31C) +
001200110418     /*CMD        ovrscope(*calllvl)
001300110418     /*CMD ovrdbf file(WFX3C00F) tofile(GAITRAAZP/WFX3C00F) +
001400110418     /*CMD        ovrscope(*calllvl)
001500110418     /*END dltovr file(TITAS31C WFX3C00F) lvl(*)
001600110418     /*END
001700101018
001800101018       //--------------------------------------------------------------
001900101018       //?Specifiche di controllo.                                     ?
002000101018       //--------------------------------------------------------------
002100101018
002200101018     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002300101018     h dftactgrp(*no)
002400101018     h alwnull(*inputonly)
002500101018
002600101018       //--------------------------------------------------------------
002700101018       //?Dichiarazione file.                                          ?
002800101018       //--------------------------------------------------------------
002900101018
003000101018       // -?Tabelle "3CP" e "3EW"?
003100101018     fTNTBE01L  if   e           k disk
003200101018       // -?Bolle SEDE?
003300101018     fTITAS31C  if   e           k disk
003400101018
003500101018       // -?Work-File con dati estratti?
003600101018     fWFx3C00F  o    e             disk    usropn
003700101018
003800101018       //--------------------------------------------------------------
003900101018       //?Definizione costanti.                                        ?
004000101018       //--------------------------------------------------------------
004100101018
004200110225       // -?Costante Supporto "EasyWEB"?
004300110225     d c_EasyWEB       c                   const('ESWEB')
004400101018
004500101018       //--------------------------------------------------------------
004600101018       //?Definizione schiere.                                         ?
004700101018       //--------------------------------------------------------------
004800101018
004900110801       // -?Numero massimo di elementi intabellabili nelle schiere?
005000170810     d c_MaxElem       c                   const(49999)
005100110801
005200110801       // -?Unificanti da tab. "3C" e relativi supporti Cliente => BRT?
005300110801     d sch_3Ccks       s                   like(§3Ccks)  inz
005400110801     d                                     dim(c_MaxElem)
005500110801     d sch_3Ccba       s                   like(§3Ccba)  inz
005600110801     d                                     dim(c_MaxElem)
005700110801
005800110801       // -?Clienti abilitati al VAB?
005900110801     d sch_VIRksc      s                   like(wVIRksc) inz
006000110801     d                                     dim(c_MaxElem)
006100101018
006200101018       //--------------------------------------------------------------
006300101018       //?Definizione aree dati.                                       ?
006400101018       //--------------------------------------------------------------
006500101018
006600101018       // -?Dati utente?
006700101018     d §AzUte        e ds                  extname(AZUTE00F)
006800101018     d                                     dtaara
006900101018     d §DatiUte      e ds                  extname(dDatiUte)
007000101018     d                                     dtaara
007100101018
007200101018       //--------------------------------------------------------------
007300101018       //?Definizione strutture dati.                                  ?
007400101018       //--------------------------------------------------------------
007500101018
007600101018       // -?Status ds?
007700101018     d Status         sds           333
007800101018     d   SDSpgm          *proc
007900101018
008000101018       // -?Parametri ricevuti?
008100101018     d KPJBA         e ds
008200101018
008300101018       // -?Tabelle usate:?
008400101018       // ·?"3C" = Clienti che inviano bolle partenza?
008500101018     d ds3C          e ds                  inz
008600101018       // ·?"3CP" = Serie parziali assegnate a clienti?
008700101018     d d3CP          e ds                  inz
008800101018       // ·?"3EW" = Dati assegnati alla postazione EasyWeb?
008900101018     d d3EW          e ds                  inz
009000101018
009100101018       // -?Salvataggio dati anagrafici per cliente unificante?
009200110816     d ds_CNACOuni   e ds                  extname(CNACO00F) inz
009300110729     d                                     qualified
009400110816     d wSaveVBU        s                   like(W3Cvbu)      inz
009500101018
009600101018       // -?Dati estratti via SQL?
009700110729     d TABEL00F      e ds                  based(nullPtr)
009800101018     d ds_TAB          ds                  inz
009900110322     d  SQL_key                            like(TBLkey)      inz                  7=>  1-  7
010000110322     d  SQL_uni                            like(TBLuni)      inz                 89=>  8- 96
010100110322     d  SQL_dtr                            like(TBLdtr)      inz                 97=> 97-100
010200110315     d wCountKscU      s              5  0 inz
010300110928     d wCountTIVSS     s              5  0 inz
010400110816     d w3RstpComod_F   s             39    inz
010500110816     d w3RstpComod_P   s             39    inz
010600101018
010700101018       //--------------------------------------------------------------
010800101018       //?Definizione variabili globali.                               ?
010900101018       //--------------------------------------------------------------
011000101018
011100101018       // -?Flags booleani?
011200101018     d $Fine           s               n   inz
011300101018     d $GiaFatt        s               n   inz
011400110729     d $AbilitVAB      s               n   inz
011500110729
011600110729       // -?Indici di schiera?
011700110729     d xx              s              5  0 inz
011800101018
011900101018       // -?Stringa SQL da eseguire?
012000110801     d wSql            s           4096    inz  varying
012100110801     d wSql2           s           2048    inz  varying
012200101018
012300110418       // -?Conteggio dei figli bloccati?
012400110418     d wCountKscBlo    s              5  0 inz
012500110418
012600101018       // -?Conteggio dei rec. in tab. "3EW" per l'unficante?
012700110315     d wCount_3EW      s              5  0 inz
012800110801
012900110801       // -?Codice cliente estratto da TIVIR00F?
013000110801     d wVIRksc         s              7    inz
013100110729
013200110729       // -?Campi data?
013300110729     d wDate           s              8  0 inz
013400110729     d wDate_Iso       s               d   inz  datfmt(*iso)
013500101018
013600101018       //--------------------------------------------------------------
013700101018       //?Definizione prototipi procedure.                             ?
013800101018       //--------------------------------------------------------------
013900101018
014000101018       // -?Reperimento dati utente?
014100101018     d TIBS34ds      e ds                  inz
014200101018      /copy gaitrasrc/srcProtoPR,TIBS34R
014300101018
014400101018       // -?Controllo/Decodifica cliente?
014500101018      /copy gaitrasrc/srcProtoPI,TIBS69R
014600101018      /copy gaitrasrc/srcProtoPR,TIBS69R
014700101018
014800101018       // -?Parametri API QCAPCMD (Process Commands)?
014900101018     d Qcmd            s           2048    inz  varying
015000101018      /copy qSysInc/qRpgleSrc,QCAPCMD
015100101018       // -?API QCAPCMD (Process Commands)?
015200101018      /copy gaitrasrc/srcProtoPR,QCAPCMD
015300101018
015400101018       // -?Parametri gestione errori API.?
015500101018      /copy qsysinc/qrpglesrc,QUSEC
015600101018
015700101018       //--------------------------------------------------------------
015800101018       //?Definizione key-list.                                        ?
015900101018       //--------------------------------------------------------------
016000101018
016100101018       // -?File TNTBE01F?
016200101018     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
016300101018     d                                     prefix(k_)   inz
016400101018
016500101018       // -?File TITAS31C?
016600101018     d k03titas31    e ds                  extname(TITAS31C : *key)
016700101018     d                                     prefix(k_)   inz
016800101018
016900101018       //--------------------------------------------------------------
017000101018       //?M A I N - L I N E                                            ?
017100101018       //--------------------------------------------------------------
017200101018
017300101018     c     *Entry        plist
017400101018     c                   parm                    KPJBA
017500101018
017600101018      /free
017700101018
017800101018       // -?Operazioni iniziali?
017900101018       exsr sr_RoutInz;
018000101018
018100101018       // -?Ciclo di lettura tab. "3C"?
018200101018       DoU  $Fine;
018300110418         exsr  sr_ExecSql;
018400101018       EndDo;
018500101018
018600101018       // -?Operazioni finali?
018700101018       exsr sr_RoutEnd;
018800101018
018900101018       //--------------------------------------------------------------
019000101018       //?Operazioni iniziali.                                         ?
019100101018       //--------------------------------------------------------------
019200101018       BEGSR  sr_RoutInz;
019300101018
019400101018         *inLR = *on;
019500110418
019600110418         // -?Impostazione opzioni per SQL?
019700110418         exec sql  set  option  DynUsrPrf = *Owner,
019800110418                                CloSqlCsr = *EndMod;
019900110801
020000110801         // -?Reperimento data odierna?
020100110801         wDate = %int( %subst( %char( %dec( %timestamp() ) )
020200110801                               : 1 : 8 ) );
020300101018
020400101018         // -?Reperimento dati job?
020500101018         exsr  sr_DatiJob;
020600110801
020700110801         // -?Intabellam. abilitazioni al VAB?
020800110801         exsr  sr_MemoAbilitVAB;
020900110729
021000110729         // -?Intabellam. unificanti in tab. "3C" e relativo supporto?
021100110729         exsr  sr_Unif_SuppCliBrt;
021200101018
021300101018         // -?Pulizia ed apertura WrkF?
021400110322         Qcmd = 'CLRPFM file(*libl/WFX3C00F)';
021500101018         exsr  sr_ExecCmd;
021600101018         open  WFX3C00F;
021700101018
021800101018         // -?Compilazione stringa SQL?
021900110418         // -?- selezione da tab. "3C"?
022000110418         //    ?Tab_3C_C estrae il n° di clienti della famiglia in tab.??
022100110418         //             ?"3C" per ogni padre (escluso il padre stesso).??
022200110418         //    ?Tab_3C_D estrae i dati di dettaglio dalla tab. "3C" per??
022300110418         //             ?ogni singolo cliente.??
022400110816         //    ?Tab_3R_P estrae eventuali stampanti in comodato dalla??
022500110816         //             ?tab. "3R" per ogni cliente padre della famiglia.??
022600110816         //    ?Tab_3R_F estrae eventuali stampanti in comodato dalla??
022700110816         //             ?tab. "3R" per ogni cliente figlio della famiglia.??
022800110801         wSql = 'with +
022900110322
023000110322                 Tab_3C_D as (select TBLkey, TBLuni, TBLdtr +
023100110315                                from TABEL00F +
023200110315                               where TBLkut = 1 +
023300110315                                 and TBLcod = ''3C'' +
023400110315                                 and TBLflg = '' '' +
023500110315                               order by substr(TBLuni, 79, 7), +
023600110817                                        TBLkey), +
023700110817
023800110817                 Tab_3C_C as (select substr(TBLuni, 79, 7) as §3Cksu, +
023900110817                                     count(*) as TotKsc +
024000110817                                from TABEL00F +
024100110817                               where TBLkut = 1 +
024200110817                                 and TBLcod = ''3C'' +
024300110817                                 and TBLflg = '' '' +
024400110817                                 and trim(TBLkey) <> +
024500110817                                     substr(TBLuni, 79, 7) +
024600110817                               group by substr(TBLuni, 79, 7) +
024700110817                               order by substr(TBLuni, 79, 7)),';
024800110816
024900110816         wSql += ' +
025000110816                 Tab_3R_P as (select §3R.TBLkey, +
025100110323                                     substr(§3R.TBLuni, 51) as +
025200110322                                                         §3RstpComod, +
025300110322                                     substr(§3C.TBLuni, 79, 7) as +
025400110322                                                         §3Cksu +
025500110322                                from TABEL00F §3R inner join +
025600110322                                     TABEL00F §3C +
025700110322                                  on §3R.TBLkut = 1 +
025800110322                                 and §3R.TBLcod = ''3R'' +
025900110322                                 and §3C.TBLcod = ''3C'' +
026000110322                                 and §3R.TBLkut = §3C.TBLkut +
026100110322                                 and §3R.TBLkey = §3C.TBLkey +
026200110816                                 and §3R.TBLkey = substr(§3C.TBLuni, 79, 7) +
026300110816                               where §3R.TBLflg = '' '' +
026400110323                                 and substr(§3R.TBLuni, 51) <> '' '' +
026500110816                               order by §3R.TBLkey), +
026600110816
026700110816                 Tab_3R_F as (select §3R.TBLkey, +
026800110816                                     substr(§3R.TBLuni, 51) as +
026900110816                                                         §3RstpComod, +
027000110816                                     substr(§3C.TBLuni, 79, 7) as +
027100110816                                                         §3Cksu +
027200110816                                from TABEL00F §3R inner join +
027300110816                                     TABEL00F §3C +
027400110816                                  on §3R.TBLkut = 1 +
027500110816                                 and §3R.TBLcod = ''3R'' +
027600110816                                 and §3C.TBLcod = ''3C'' +
027700110816                                 and §3R.TBLkut = §3C.TBLkut +
027800110816                                 and §3R.TBLkey = §3C.TBLkey +
027900110816                                 and §3C.TBLkey <> substr(§3C.TBLuni, 79, 7) +
028000110816                               where §3R.TBLflg = '' '' +
028100110816                                 and substr(§3R.TBLuni, 51) <> '' '' +
028200110816                               order by §3R.TBLkey)';
028300110322
028400110817         wSql += ' select Tab_3C_D.*, +
028500110817                          ifnull(Tab_3C_C.TotKsc, 0), +
028600110816                          ifnull(Tab_3R_F.§3RstpComod, '' ''), +
028700110816                          ifnull(Tab_3R_P.§3RstpComod, '' '') +
028800110322
028900110817                     from Tab_3C_D left outer join Tab_3C_C +
029000110322                       on substr(Tab_3C_D.TBLuni, 79, 7) = +
029100110322                                 Tab_3C_C.§3Cksu +
029200110322
029300110816                                   left outer join Tab_3R_P +
029400110816                       on substr(Tab_3C_D.TBLuni, 79, 7) = Tab_3R_P.§3Cksu +
029500110816
029600110816                                   left outer join Tab_3R_F +
029700110816                       on Tab_3C_D.TBLkey = Tab_3R_F.TBLkey +
029800110322
029900110322                    order by substr(Tab_3C_D.TBLuni, 79, 7), +
030000110322                                    Tab_3C_D.TBLkey +
030100110322
030200110322                      for fetch only';
030300101018
030400101018         // -?Start SQL?
030500110801         exec sql   prepare S1   from :wSql;
030600101018         exec sql   declare C1   cursor for S1;
030700101018         exec sql   open C1;
030800101018
030900101018       ENDSR;
031000101018
031100101018       //--------------------------------------------------------------
031200101018       //?Reperimento Dati del job (Utente/Operativi).                 ?
031300101018       //--------------------------------------------------------------
031400101018       BEGSR  sr_DatiJob;
031500101018
031600101018         in(e) §AzUte;
031700101018         if NOT %error;
031800101018           in(e) §DatiUte;
031900101018         endif;
032000101018         if %error or RSut = *blank;
032100101018           tibs34r ( tibs34ds );
032200101018           in §AzUte;
032300101018           in §DatiUte;
032400101018         endif;
032500101018
032600101018       ENDSR;
032700110801
032800110801       //--------------------------------------------------------------
032900110801       //?Intabellamento abilitazioni al VAB.                          ?
033000110801       //--------------------------------------------------------------
033100110801       BEGSR  sr_MemoAbilitVAB;
033200110801
033300110801         wSql = 'select distinct substr(VIRksc, 2, 7) +
033400110801                   from TIVIR00F +
033500110801                  where ' + %char( wDate ) + ' between VIRdti and VIRdtf +
033600110801                    and (VIRfit in (''FIVABWWR'', ''EDIVABWR'') or +
033700110801                         VIRtip in +
033800110801                        (select VTFtip +
033900110801                           from TIVTF00F +
034000110801                          where VTFud = ''U'' +
034100110801                            and VTFfit in (''FIVABWWR'', ''EDIVABWR''))) +
034200110801                  order by substr(VIRksc, 2, 7)';
034300110801
034400110801         // -?Start SQL-4?
034500110801         exec sql   prepare S4   from :wSql;
034600110801         exec sql   declare C4   cursor for S4;
034700110801         exec sql   open C4;
034800110801
034900110801         // -?Exec SQL-4?
035000110801         clear  xx;
035100110801         clear  sch_VIRksc;
035200110801         exec sql   fetch next   from C4   into :wVIRksc;
035300110801
035400110801         DoW  SQLcode <> 100;
035500110801
035600110801           if  SQLcode < *zero;
035700110801             exsr  sr_PrintErr;
035800110801             //$Fine  = *on;
035900110801             //leavesr;
036000110801             exsr  sr_RoutEnd;
036100110801           endif;
036200110801
036300110801           xx += 1;
036400110801           sch_VIRksc(xx) = wVIRksc;
036500110801
036600110801           clear  wVIRksc;
036700110801           exec sql   fetch next   from C4   into :wVIRksc;
036800110801
036900110801         EndDo;
037000110801
037100110801         // -?End SQL-3?
037200110801         exec SQL   close C4;
037300110801
037400110801       ENDSR;
037500110729
037600110729       //--------------------------------------------------------------
037700110729       //?Reperimento padri in tab. "3C" e relativi supporti Cli=>BRT. ?
037800110729       //--------------------------------------------------------------
037900110729       BEGSR  sr_Unif_SuppCliBrt;
038000110729
038100110801         wSql = 'select TBLuni +
038200110801                   from TABEL00F +
038300110801                  where TBLkut = 1 +
038400110801                    and TBLcod = ''3C'' +
038500110801                    and TBLflg = '' '' +
038600110801                    and trim( TBLkey ) = substr( TBLuni, 79, 7 )';
038700110729
038800110729         // -?Start SQL-3?
038900110801         exec sql   prepare S3   from :wSql;
039000110729         exec sql   declare C3   cursor for S3;
039100110729         exec sql   open C3;
039200110729
039300110729         // -?Exec SQL-3?
039400110801         clear  xx;
039500110801         clear  sch_3Ccks;
039600110801         clear  sch_3Ccba;
039700110729         clear  ds3C;
039800110801         clear  SQL_uni;
039900110801         exec sql   fetch next   from C3   into :SQL_uni;
040000110729
040100110729         DoW  SQLcode <> 100;
040200110729
040300110729           if  SQLcode < *zero;
040400110729             exsr  sr_PrintErr;
040500110801             //$Fine  = *on;
040600110801             //leavesr;
040700110801             exsr  sr_RoutEnd;
040800110729           endif;
040900110729
041000110801           ds3C = SQL_uni;
041100110729           xx += 1;
041200110729           sch_3Ccks(xx) = §3Ccks;
041300110729           sch_3Ccba(xx) = §3Ccba;
041400110729
041500110729           clear  ds3C;
041600110801           exec sql   fetch next   from C3   into :SQL_uni;
041700110729
041800110729         EndDo;
041900110729
042000110729         // -?End SQL-3?
042100110729         exec SQL   close C3;
042200110729
042300110729         clear  ds3C;
042400110729
042500110729       ENDSR;
042600110418
042700110418       //--------------------------------------------------------------
042800110418       //?Esecuzione singola lettura via SQL dalla tab. "3C".          ?
042900110418       //--------------------------------------------------------------
043000110418       BEGSR  sr_ExecSql;
043100110418
043200110418         clear  ds_TAB;
043300110418         clear  wCountKscU;
043400110816         clear  w3RstpComod_F;
043500110816         clear  w3RstpComod_P;
043600110418
043700110418         exec sql   fetch next   from C1   into :ds_TAB,
043800110418                                                :wCountKscU,
043900110816                                                :w3RstpComod_F,
044000110816                                                :w3RstpComod_P;
044100110418
044200110418         select;
044300110418           when  SQLcode = 100;
044400110418             $Fine = *on;
044500110418           when  SQLcode < *zero;
044600110418             exsr  sr_PrintErr;
044700110418             $Fine  = *on;
044800110418           other;
044900110418             exsr  sr_Elab3C;
045000110418         endsl;
045100110418
045200110418       ENDSR;
045300101018
045400101018       //--------------------------------------------------------------
045500101018       //?Elaborazione singolo cliente dalla tabella "3C".             ?
045600101018       //--------------------------------------------------------------
045700101018       BEGSR  sr_Elab3C;
045800101018
045900101018         clear  d3EW;
046000101018         clear  d3CP;
046100101018         ds3C = SQL_uni;
046200101018
046300101018         clear  WFX3C000;
046400101018
046500101018         W3Cksu = §3Ccks;
046600110418         W3Cnrf = wCountKscU;            // -?escluso l'unificante?
046700101018         W3Cksc = %int( %trim( SQL_key ) );
046800101018         W3Ccba = §3Ccba;
046900110310         if  §3Cnrs > *zero;
047000120119           //W3Cfls = %int( %subst( SQL_key : 1 : 3 ) );
047100120119           W3Cfls = §3Cfls;
047200110310         endif;
047300101018         W3Cnrs = §3Cnrs;
047400101018         W3Cabd = §3Cabd;
047500110816         if  w3RstpComod_F = *blank  and  w3RstpComod_P = *blank;
047600110322           W3Cf3R = 'N';
047700110322         else;
047800110322           W3Cf3R = 'S';
047900110322         endif;
048000110322         monitor;
048100110322           W3Cdtr = %dec( %date( SQL_dtr : *ymd ) );
048200110322         on-error;
048300110322         endmon;
048400101018
048500101018         // -?Decodifica cliente unificante?
048600110729         if  W3Cksu <> ds_CNACOuni.ACOksc;
048700101018           clear  ds_CNACOuni;
048800101018           clear  TIBS69ds;
048900101018           I69kac = W3Cksu;
049000101018           exsr  sr_CallTIBS69;
049100101018           if  O69err <> *on;
049200101018             ds_cnacoUNI = ds_cnaco;
049300101018           endif;
049400110418           // -?Reperimento n° figli bloccati?
049500110418           exsr  sr_NrChldrnLocked;
049600110729           // -?Reperimento abilitazione all'invio bolle (se NON EasySpedWEB)?
049700110816           clear  wSaveVBU;
049800110729           xx = %lookup( W3Cksu : sch_3Ccks );
049900110729           if  xx > *zero  and  sch_3Ccba(xx) <> c_EasyWEB;
050000110801             wVIRksc = %editc( W3Cksu : 'X' );
050100110801             $AbilitVAB = ( %lookup( wVIRksc : sch_VIRksc ) > *zero );
050200110801             if  $AbilitVAB = *on;
050300110816               wSaveVBU = 'S';
050400110801             else;
050500110816               wSaveVBU = 'N';
050600110801             endif;
050700110729           endif;
050800101018         endif;
050900110816         W3Cvbu = wSaveVBU;
051000110729         W3Crsu = ds_CNACOuni.ACOrag;
051100110729         W3Cblu = ds_CNACOuni.ACOabl;
051200110418         W3Cblf = wCountKscBlo;
051300101018
051400101018         // -?Decodifica cliente?
051500101018         select;
051600101018           when  W3Cksc =  W3Cksu;
051700101018             ds_cnaco = ds_cnacoUNI;
051800101018           when  W3Cksc <> I69kac;
051900101018             clear  TIBS69ds;
052000101018             I69kac = W3Cksc;
052100101018             exsr  sr_CallTIBS69;
052200101018         endsl;
052300101018         W3Crsc = ACOrag;
052400101018         W3Cblc = ACOabl;
052500110729         // -?Reperimento abilitazione all'invio bolle (se NON EasySpedWEB)?
052600110729         if  §3Ccba <> c_EasyWEB;
052700110801           wVIRksc = %editc( W3Cksc : 'X' );
052800110801           $AbilitVAB = ( %lookup( wVIRksc : sch_VIRksc ) > *zero );
052900110801           if  $AbilitVAB = *on;
053000110801             W3Cvbc = 'S';
053100110801           else;
053200110801             W3Cvbc = 'N';
053300110801           endif;
053400110729         endif;
053500101018
053600101018         // -?Reperimento dati aggiuntivi (serie / range segnacolli)?
053700101018         select;
053800101018           // ->?da tab. "3EW"?
053900110225           when  §3Ccba = c_EasyWEB;
054000101018             exsr  sr_Tab3EW;
054100101018           // ->?da tab. "3CP"?
054200101018           when  §3Cnrs > *zero;
054300101018             exsr  sr_Tab3CP;
054400101018         endsl;
054500110310         // -?Se range segnacolli totale (non reperito): imposto 1-9999999?
054600110310         if  (§3Ccba = c_EasyWEB   or
054700110310              §3Cnrs > *zero)     and
054800110310              W3Cncd = *zero      and
054900110310              W3Cnca = *zero;
055000110310           W3Cncd = 1;
055100110310           W3Cnca = *hival;
055200110310         endif;
055300101018
055400101018         // -?Reperimento dati ultime spedizioni per cliente su TITAS?
055500101018         exsr  sr_TITAS;
055600110928
055700110928         // -?Reperimento abilitazioni ad internet?
055800110928         exsr  sr_AbilInternet;
055900110928         if  wCountTIVSS > *zero;
056000110928           W3Cvss = 'S';
056100110928         else;
056200110928           W3Cvss = 'N';
056300110928         endif;
056400101018
056500101018         // -?Scrittura record nel work-file?
056600101018         //______________
056700101018         write  WFx3C000;
056800101018         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
056900101018
057000101018       ENDSR;
057100101018
057200101018       //--------------------------------------------------------------
057300110322       //?Decodifica cliente.                                          ?
057400101018       //--------------------------------------------------------------
057500101018       BEGSR  sr_CallTIBS69;
057600101018
057700101018         clear  ds_CNACO;
057800101018
057900101018         //clear  TIBS69ds;    ?(già fatto)?
058000101018         I69sif = knsif;
058100101018         I69kcc = DUTkci;
058200101018         //I69kac = W3Cks?;    ?(già fatto)?
058300101018
058400101018         tibs69r( tibs69ds :
058500101018                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
058600101018
058700101018       ENDSR;
058800110418
058900110418       //--------------------------------------------------------------
059000110418       //?Reperimento n° figli bloccati del padre in esame.            ?
059100110418       //--------------------------------------------------------------
059200110418       BEGSR  sr_NrChldrnLocked;
059300110418
059400110801         wSql2 = 'With +
059500110418
059600110801                  Ksc_3C as (select char( trim( TBLkey ) ) +
059700110801                               from TABEL00F +
059800110801                              where TBLkut = 1 +
059900110801                                and TBLcod = ''3C'' +
060000110801                                and TBLflg = '' '' +
060100110801                                and trim( TBLkey ) <> +
060200110801                                    substr( TBLuni, 79, 7 ) +
060300110801                                and substr( TBLuni, 79, 7 ) = ''' +
060400110801                                    %editc( §3Ccks : 'X' ) + ''') +
060500110418
060600110801                  select count(*) +
060700110801                    from CNACO00F +
060800110801                   where ACOkut = 1 +
060900110816                     and ACOkcc = ' + %editc( DUTkci : '3' ) + ' +
061000110801                     and ACOksc in (select * from Ksc_3C) +
061100110801                     and ACOabl <> '' ''';
061200110418
061300110418         // -?Start SQL-2?
061400110418         exec sql   prepare S2   from :wSql2;
061500110418         exec sql   declare C2   cursor for S2;
061600110418         exec sql   open C2;
061700110418
061800110418         // -?Exec SQL-2?
061900110418         clear  wCountKscBlo;
062000110418         exec sql   fetch next   from C2   into :wCountKscBlo;
062100110418
062200110418         if  SQLcode < *zero;
062300110418           exsr  sr_PrintErr;
062400110418           $Fine  = *on;
062500110418         endif;
062600110418
062700110418         // -?End SQL-2?
062800110418         exec SQL   close C2;
062900110418
063000110418       ENDSR;
063100110729
063200110928
063300110928       //--------------------------------------------------------------
063400110928       //?Reperimento abilitaz. internet attive del cliente in esame.  ?
063500110928       //--------------------------------------------------------------
063600110928       BEGSR  sr_AbilInternet;
063700110928
063800110928         wSql2 = 'select count(*) +
063900110928                    from TIVSS00F +
064000110928                   where TIVSS00F.VSSksu = ''0' + %editc(W3Cksc:'X') + ''' +
064100110928                     and TIVSS00F.VSSvat = '' '' +
064200110928                     and ' + %char(wDate) + ' between +
064300110928                         TIVSS00F.VSSdde and TIVSS00F.VSSdsc';
064400110928
064500110928         // -?Start SQL-5?
064600110928         exec sql   prepare S5   from :wSql2;
064700110928         exec sql   declare C5   cursor for S5;
064800110928         exec sql   open C5;
064900110928
065000110928         // -?Exec SQL-5?
065100110928         clear  wCountTIVSS;
065200110928         exec sql   fetch next   from C5   into :wCountTIVSS;
065300110928
065400110928         if  SQLcode < *zero;
065500110928           exsr  sr_PrintErr;
065600110928           $Fine  = *on;
065700110928         endif;
065800110928
065900110928         // -?End SQL-5?
066000110928         exec SQL   close C5;
066100110928
066200110928       ENDSR;
066300101018
066400101018       //--------------------------------------------------------------
066500101018       //?Reperimento dati da tab. "3CP" per cliente in esame.         ?
066600101018       //--------------------------------------------------------------
066700101018       BEGSR  sr_Tab3CP;
066800101018
066900101018         k_TBEcod = '3CP';
067000101018         k_TBEke1 = SQL_key;
067100101018
067200101018         setll  %kds( k05tntbe01 : 2 )  TNTBE000;
067300101018         reade  %kds( k05tntbe01 : 2 )  TNTBE000;
067400101018
067500101018         DoW  Not %eof(TNTBE01L);
067600101018
067700101018           if  %subst( TBEke2 : 1 : 2 ) = %editc( §3Cnrs : 'X' )   AND
067800101018               TBEatb = *blank;
067900101018             d3CP = TBEuni;
068000101018             W3Cnrs = %int( %subst( TBEke2 : 1 : 2 ) );
068100101018             W3Cncd = %int( %subst( TBEke2 : 3 : 7 ) );
068200101018             W3Cnca = §3CPal;
068300101018             leave;
068400101018           endif;
068500101018
068600101018           reade  %kds( k05tntbe01 : 2 )  TNTBE000;
068700101018
068800101018         EndDo;
068900101018
069000101018       ENDSR;
069100101018
069200101018       //--------------------------------------------------------------
069300101018       //?Reperimento dati da tab. "3EW" per cliente in esame.         ?
069400101018       //--------------------------------------------------------------
069500101018       BEGSR  sr_Tab3EW;
069600101018
069700101018         clear  wCount_3EW;
069800101018
069900101018         k_TBEcod = '3EW';
070000120117         k_TBEke1 = '0' + %editc( W3Cksu : 'X' );
070100101018
070200101018         setll  %kds( k05tntbe01 : 2 )  TNTBE000;
070300101018         reade  %kds( k05tntbe01 : 2 )  TNTBE000;
070400101018
070500101018         DoW  Not %eof(TNTBE01L);
070600101018
070700101018           if  TBEatb = *blank;
070800101018             wCount_3EW += 1;
070900101018             d3EW = TBEuni;
071000141002             if  §3EWfaa = *blank  or  wCount_3EW = 1;
071100141002               W3Cfls = §3EWfls;
071200141002               W3Cnrs = §3EWnrs;
071300141002               W3Cncd = §3EWnsi;
071400141002               W3Cnca = §3EWnsf;
071500141002             endif;
071600101018           endif;
071700101018
071800101018           reade  %kds( k05tntbe01 : 2 )  TNTBE000;
071900101018
072000101018         EndDo;
072100101018
072200101018       ENDSR;
072300101018
072400101018       //--------------------------------------------------------------
072500101018       //?Reperimento dati ultime spedizioni per cliente su TITAS.     ?
072600101018       //--------------------------------------------------------------
072700101018       BEGSR  sr_TITAS;
072800101018
072900101018         // -?Il file combinato in input è ordinato (ascend) per       ?
073000101018         //  ?· KSC = Codice cliente                                   ?
073100101018         //  ?· DFT = Data fattura                                     ?
073200101018         //  ?· NFT = Numero fattura                                   ?
073300101018         //  ?IPRIMI?record letti per il cliente saranno relativi     ?
073400101018         //  ?  alle spedizioni NON ancora fatturate (DFT = *zero);    ?
073500101018         //  POI?seguiranno quelli relativi alle spedizioni già       ?
073600101018         //  ?  fatturate (partendo dalla fattura più vecchia).        ?
073700101018         //  QUINDI:?occorre leggere PRIMA le spedizioni non ancora   ?
073800101018         //  ?fatturate, POI quelle già fatturate, partendo però dalla ?
073900101018         //  ?fattura più recente (cioè l'ultima).                     ?
074000101018
074100101018         reset  $GiaFatt;
074200101018
074300101018         k_TASksc = W3Cksc;
074400101018         clear  k_TASdft;
074500101018         clear  k_TASnft;
074600101018         setll  %kds(k03titas31)      TITAS31C;
074700101018         reade  %kds(k03titas31 : 1)  TITAS31C;
074800101018         if  NOT %eof(TITAS31C)   and
074900120117            (TASdft <> *zero  or  TASnft <> *zero);
075000101018           $GiaFatt =  *on;
075100101018           setgt   %kds(k03titas31 : 1)  TITAS31C;
075200101018           readpe  %kds(k03titas31 : 1)  TITAS31C;
075300101018         endif;
075400101018
075500101018         DoW  NOT %eof(TITAS31C);
075600101018
075700101018           if  TAStbl = 'F1';
075800101018             if  W3Cdus < (TASaas * 10000) + TASmgs;
075900101018               W3Cdus = (TASaas * 10000) + TASmgs;
076000101018             endif;
076100110310             if  TASnrs > *zero     and
076200110310                (TASaas * 10000) + TASmgs >= W3Cduss;
076300110310               if  (TASaas * 10000) + TASmgs > W3Cduss   or
076400110310                  ((TASaas * 10000) + TASmgs = W3Cduss   and
076500110310                                      TASncd > W3Csuss);
076600110310                   W3Csuss  = TASncd;
076700110310               endif;
076800110310               W3Cduss  = (TASaas * 10000) + TASmgs;
076900110310               //leave;     ?     (cerco la spedizione più recente)?
077000110310             endif;
077100101018           endif;
077200101018
077300101018           if  Not $GiaFatt;
077400101018             reade  %kds(k03titas31 : 1)  TITAS31C;
077500101018             if  NOT %eof(TITAS31C)   and
077600120117                (TASdft <> *zero  or  TASnft <> *zero);
077700101018               // -?Spedizione già reperita?
077800101018               if  W3Cdus <> *zero;
077900101018                 leave;
078000101018               endif;
078100101018               $GiaFatt =  *on;
078200101018               setgt   %kds(k03titas31 : 1)  TITAS31C;
078300101018               readpe  %kds(k03titas31 : 1)  TITAS31C;
078400101018             endif;
078500101018           else;
078600101018             readpe  %kds(k03titas31 : 1)  TITAS31C;
078700101018             if  NOT %eof(TITAS31C)   and
078800120117                 ((TASdft = *zero  and  TASnft = *zero)  OR
078900120117                  (TASnrs > *zero  and  (TASaas*10000)+TASmgs < W3Cduss));
079000101018               leave;
079100101018             endif;
079200101018           endif;
079300101018
079400101018         EndDo;
079500101018
079600101018       ENDSR;
079700101018
079800101018       //--------------------------------------------------------------
079900101018       //?Esecuzione del comando (già impostato).                      ?
080000101018       //--------------------------------------------------------------
080100101018       BEGSR  sr_ExecCmd;
080200101018
080300101018         clear Qcap0100;
080400101018         Qcabcsdh = *off;
080500101018         Qcapa    = *off;
080600101018         Qcacmdss = *off;
080700101018         Qcaerved = *allX'00';
080800101018
080900101018         clear Qusec;
081000101018         Qusbprv  = %size(Qusec);
081100101018
081200101018         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
081300101018                           %size(Qcap0100) : 'CPOP0100' : *omit :
081400101018                           0 : 0 : Qusec);
081500101018
081600101018         if  Qusei <> *blank;
081700101018           exsr  sr_PrintErr;
081800101018           exsr  sr_RoutEnd;
081900101018         endif;
082000101018
082100101018       ENDSR;
082200101018
082300101018       //--------------------------------------------------------------
082400101018       //?Stampa segnalazioni dell'errore rilevato.                    ?
082500101018       //--------------------------------------------------------------
082600101018       BEGSR  sr_PrintErr;
082700101018
082800101018         // -?Stampa Dump?
082900101018         Dump(A);
083000101018
083100101018         // -?Stampa Job-Log?
083200101018         Qcmd = 'DSPJOBLOG job(*) output(*print)';
083300101018         clear Qcap0100;
083400101018         Qcabcsdh = *off;
083500101018         Qcapa    = *off;
083600101018         Qcacmdss = *off;
083700101018         Qcaerved = *allX'00';
083800101018         clear Qusec;
083900101018         Qusbprv  = %size(Qusec);
084000101018         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
084100101018                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
084200101018                           0 : 0 : Qusec);
084300101018
084400101018       ENDSR;
084500101018
084600101018       //--------------------------------------------------------------
084700101018       //?Operazioni finali.                                           ?
084800101018       //--------------------------------------------------------------
084900101018       BEGSR  sr_RoutEnd;
085000101018
085100101018         // -?End SQL?
085200101018         exec SQL   close C1;
085300101018
085400101018         // -?Chiusura applicazioni aperte?
085500101018         clear  TIBS69ds;
085600101018         I69tla = 'C';
085700101018         tibs69r( tibs69ds :
085800101018                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
085900101018
086000101018         // -?Uscita dal *pgm?
086100101018         return;
086200101018
086300101018       ENDSR;
086400101018
086500101018      /end-free
