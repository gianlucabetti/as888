000100130116       //==============================================================
000200130116       //?Elenco clienti in tab. "3NN", ma SENZA spedizioni.           ?
000300130116       //==============================================================
000400130116
000500130116       //--------------------------------------------------------------
000600130116       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700130116       //--------------------------------------------------------------
000800130116
000900130116     /*PRM  dbgview(*source)
001000130116     /*END
001100130116
001200130116       //--------------------------------------------------------------
001300130116       //?Specifiche di controllo.                                     ?
001400130116       //--------------------------------------------------------------
001500130116
001600130116     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700130116     h dftactgrp(*no)
001800130116     h alwnull(*inputonly)
001900130116
002000130116       //--------------------------------------------------------------
002100130116       //?Dichiarazione file.                                          ?
002200130116       //--------------------------------------------------------------
002300130116
002400130116       // -?Tabelle "3NN"?
002500130116     f*//TNTBE01L  if   e           k disk
002600130116
002700130117       // -?Bolle Sede?
002800130116     fTITAS33C  if   e           k disk
002900130117
003000130117       // -?Eccezioni terminal Part/Arr?
003100130117     fAZCAE03L  if   e           k disk
003200130116
003300130116       // -?File di Stampa?
003400130116     fQSYSPRT   o    f  132        printer  oflind(*inOF)  usropn
003500130118     fQSYSPRT1  o    f  132        printer  oflind(*inOA)  usropn
003600130116
003700130116       //--------------------------------------------------------------
003800130116       //?Definizione costanti.                                        ?
003900130116       //--------------------------------------------------------------
004000130116
004100130116       // -?Comando di override al PrtF?
004200130116     d c_CmdOvrPrtF    c                   const('OVRPRTF +
004300130116     d                                           file(QSYSPRT) +
004400130116     d                                           pagesize(66 132) +
004500130116     d                                           lpi(6) cpi(10) +
004600130116     d                                           usrdta(''3NN_NoSped'') +
004700130116     d                                           ovrscope(*actgrpdfn) +
004800130116     d                                           ')
004900130118     d c_CmdOvrPrtF1   c                   const('OVRPRTF +
005000130118     d                                           file(QSYSPRT1) +
005100130118     d                                           pagesize(66 132) +
005200130118     d                                           lpi(6) cpi(10) +
005300130118     d                                           usrdta(''3NN+AZCAE'') +
005400130118     d                                           ovrscope(*actgrpdfn) +
005500130118     d                                           ')
005600130116
005700130116       // -?Comando di cellazione override?
005800130116     d c_CmdDltOvr     c                   const('DLTOVR +
005900130116     d                                            file(QSYSPRT) +
006000130116     d                                            lvl(*actgrpdfn)')
006100130118     d c_CmdDltOvr1    c                   const('DLTOVR +
006200130118     d                                            file(QSYSPRT1 +
006300130118     d                                            lvl(*actgrpdfn)')
006400130116
006500130116       //--------------------------------------------------------------
006600130116       //?Definizione schiere.                                         ?
006700130116       //--------------------------------------------------------------
006800130116
006900130116       // -?Testate in stampa?
007000130116     d sk_Txt          s             66    dim( 6)  ctdata  perrcd( 1)
007100130116
007200130116       //--------------------------------------------------------------
007300130116       //?Definizione aree dati.                                       ?
007400130116       //--------------------------------------------------------------
007500130116
007600130116       // -?Dati utente?
007700130116     d §AzUte        e ds                  extname(AZUTE00F)
007800130116     d                                     dtaara
007900130116     d §DatiUte      e ds                  extname(dDatiUte)
008000130116     d                                     dtaara
008100130116
008200130116       //--------------------------------------------------------------
008300130116       //?Definizione strutture dati.                                  ?
008400130116       //--------------------------------------------------------------
008500130116
008600130116       // -?Status ds?
008700130116     d Status         sds
008800130116     d   SDSpgm          *proc
008900130122
009000130122       // -?Parametri ricevuti?
009100130122     d TNSB28ds        ds                  inz
009200130122     d   D28ggl                       3s 0 inz
009300130122     d   D28dtl                       8s 0 inz
009400130116
009500130116       // -?Dati estratti via SQL?
009600130116     d TNTBE00F      e ds                  based(nullPtr)
009700130116     d                                     prefix(ds_)
009800130116     d wSQL_ds         ds                  inz  qualified
009900130116     d   wSQLcod                           inz  like(ds_TBEcod)
010000130116     d   wSQLke1                           inz  like(ds_TBEke1)
010100130116     d   wSQLke2                           inz  like(ds_TBEke2)
010200130116     d   wSQLuni                           inz  like(ds_TBEuni)
010300130116
010400130116       // -?Parametri ricevuti?
010500130116     d KPJBA         e ds
010600130116
010700130116       // -?Tabelle usate:?
010800130116       // ·?"3NN" = Conferma disk su altro AS?
010900130116     d d3NN          e ds                  inz  qualified
011000130116
011100130116       //--------------------------------------------------------------
011200130116       //?Definizione variabili globali.                               ?
011300130116       //--------------------------------------------------------------
011400130116
011500130116       // -?Flags booleani?
011600130116     d $Fine           s               n   inz
011700130117     d $Same£1         s               n   inz
011800130116
011900130116       // -?Stringa SQL da eseguire?
012000130116     d wSQL            s           2048    Varying        inz
012100130116
012200130116       // -?Campi di stampa?
012300130122     d s_TotCli        s              5  0 inz
012400130122     d s1_TotCli       s              5  0 inz
012500130118     d s1_Txt1         s             50    inz
012600130118     d s1_Txt2         s             50    inz
012700130116     d PrtDET_ds       ds                  inz
012800130116     d   s_CodKsc                     7a   inz
012900130116     d   s_DesKsc                    35a   inz
013000130116     d   s_LnP                        3a   inz
013100130116     d   s_CodCC                      7a   inz
013200130116     d   s_DesCC                     35a   inz
013300130116     d   s_DtUltS                     8  0 inz
013400130118     d Prt1DET_ds      ds                  inz
013500130118     d   s1_CodKsc                    7a   inz
013600130118     d   s1_DesKsc                   35a   inz
013700130118     d   s1_LnP                       3a   inz
013800130116
013900130116       // -?Campi per controllo rotture di livello?
014000130117     d SaveKSC         s                   inz  like(s_CodKSC)
014100130117     d SaveCC          s                   inz  like(d3NN.§3NNcc)
014200130118     d*// SaveFIL         s              3    inz
014300130118     d Save0KSC        s                   inz  like(s_CodKSC)
014400130118     d Save1KSC        s                   inz  like(s1_CodKSC)
014500130116
014600130116       // -?Campi di comodo?
014700130116     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
014800130122     d w28dtl          s              8  0 inz
014900130117     d wSaveLNP        s                   inz  like(CAEtfe)
015000130116     d wSaveRAG        s                   inz  like(ACOrag)
015100130117     d wSaveDesCC      s                   inz  like(ACOrag)
015200130116
015300130116       //--------------------------------------------------------------
015400130116       //?Definizione prototipi procedure.                             ?
015500130116       //--------------------------------------------------------------
015600130116
015700130116       // -?Reperimento dati utente?
015800130116     d TIBS34ds      e ds
015900130116      /copy gaitrasrc/srcProtoPR,TIBS34R
016000130117
016100130117       // -?Caricamento filiali con lo stesso terminal?
016200130117     d TRUL06ds      e ds                  inz
016300130117     d   D06cod      e                     inz('PP')
016400130118     d   sk_D06lin                    3    overlay(D06lia)
016500130117     d                                     dim(30)
016600130117      /copy gaitrasrc/srcProtoPR,TRUL06R
016700130116
016800130116       // -?Controllo/Decodifica cliente?
016900130116      /copy gaitrasrc/srcProtoPI,TIBS69R
017000130116      /copy gaitrasrc/srcProtoPR,TIBS69R
017100130116
017200130116       // -?Parametri API QCAPCMD (Process Commands)?
017300130116     d Qcmd            s           2048    inz  varying
017400130116      /copy qSysInc/qRpgleSrc,QCAPCMD
017500130116       // -?API QCAPCMD (Process Commands)?
017600130116      /copy gaitrasrc/srcProtoPR,QCAPCMD
017700130116
017800130116       // -?Parametri gestione errori API.?
017900130116      /copy qsysinc/qrpglesrc,QUSEC
018000130116
018100130116       //--------------------------------------------------------------
018200130116       //?Definizione key-list.                                        ?
018300130116       //--------------------------------------------------------------
018400130116
018500130116       // -?File TITAS33C?
018600130116     d k03titas33    e ds                  extname(TITAS33C : *key)
018700130116     d                                     prefix(k_)   inz
018800130117
018900130117       // -?File AZCAE03L?
019000130117     d k04azcae03    e ds                  extname(AZCAE03L : *key)
019100130117     d                                     prefix(k_)   inz
019200130116
019300130116       //--------------------------------------------------------------
019400130116       //?M A I N - L I N E                                            ?
019500130116       //--------------------------------------------------------------
019600130116
019700130116     c     *Entry        plist
019800130116     c                   parm                    KPJBA
019900130116
020000130116      /free
020100130116
020200130116       // -?Operazioni iniziali?
020300130116       exsr  sr_RoutInz;
020400130116
020500130122       // -?Estrazione dati da tab. "3NN" e dal file TITAS?
020600130122       exsr  sr_ReadTab;
020700130116
020800130116       // -?Operazioni finali?
020900130116       exsr  sr_RoutEnd;
021000130116
021100130116       //--------------------------------------------------------------
021200130116       //?Operazioni iniziali.                                         ?
021300130116       //--------------------------------------------------------------
021400130116       BEGSR  sr_RoutInz;
021500130116
021600130116         *inLR = *on;
021700130122
021800130122         // -?Reperimento parametri?
021900130122         TNSB28ds = kpjbu;
022000130116
022100130116         // -?Reperimento dati job?
022200130116         exsr  sr_DatiJob;
022300130116
022400130118         // -?Override ai printer-files?
022500130116         Qcmd = c_CmdOvrPrtF;
022600130116         exsr  sr_ExecCmd;
022700130116         //if  Qusei <> *blank;
022800130116         //  ...;
022900130116         //endif;
023000130122
023100130118         Qcmd = c_CmdOvrPrtF1;
023200130118         exsr  sr_ExecCmd;
023300130118         //if  Qusei <> *blank;
023400130118         //  ...;
023500130118         //endif;
023600130122
023700130122         wDate_Eur = %date( D28dtl : *iso );
023800130122         w28dtl    = %dec( wDate_Eur );
023900130122
024000130122         // -?Apertura printer-file?
024100130122         open  QSYSPRT;
024200130122         clear Page;
024300130122         except  PrtTXT;
024400130122
024500130122         open  QSYSPRT1;
024600130122         clear Page1;
024700130122         s1_Txt1 = %subst( sk_Txt(1) : 1 : 50 );
024800130122         s1_Txt2 = %subst( sk_Txt(2) : 1 : 50 );
024900130122         except  Prt1TXT;
025000130116
025100130116       ENDSR;
025200130116
025300130116       //--------------------------------------------------------------
025400130116       //?Reperimento Dati del job (Utente/Operativi).                 ?
025500130116       //--------------------------------------------------------------
025600130116       BEGSR  sr_DatiJob;
025700130116
025800130116         in(E) §AzUte;
025900130116         if NOT %error;
026000130116           in(E) §DatiUte;
026100130116         endif;
026200130116         if %error or RSut = *blanks;
026300130116           clear TIBS34ds;
026400130116           tibs34r ( tibs34ds );
026500130116           in §AzUte;
026600130116           in §DatiUte;
026700130116         endif;
026800130116
026900130116       ENDSR;
027000130116
027100130116       //--------------------------------------------------------------
027200130116       //?Estrazione dati da tab. "3NN" e dal file TITAS.              ?
027300130116       //--------------------------------------------------------------
027400130116       BEGSR  sr_ReadTab;
027500130116
027600130116         // -?Preparazione SQL?
027700130116         wSQL = 'select TBEcod, TBEke1, TBEke2, TBEuni +
027800130116                   from TNTBE00F +
027900130116                  where TBEcod = ''3NN'' +
028000130116                    and TBEatb = '' '' +
028100130116                  order by TBEcod, TBEke1, TBEke2 +
028200130116                    for fetch only';
028300130116
028400130116         exec sql   prepare S1   from :wSQL;
028500130116
028600130116         // -?Dichiarazione cursore SQL?
028700130116         exec sql   declare C1   cursor for S1;
028800130116
028900130116         // -?Apertura cursore SQL?
029000130116         exec sql   open C1;
029100130116
029200130116         // -?Rilevato errore SQL?
029300130116         if  SQLcode < *zero;
029400130116           exsr  sr_PrintErr;
029500130116           exsr  sr_RoutEnd;
029600130116         endif;
029700130116
029800130116         // -?Ciclo esecuzione SQL?
029900130116         exec sql   fetch next   from C1   into :wSQL_ds;
030000130116
030100130116         DoW  SQLcode <> 100;
030200130116
030300130116           // ·?Rilevato errore SQL?
030400130116           if  SQLcode < *zero;
030500130116             exsr  sr_PrintErr;
030600130116             exsr  sr_RoutEnd;
030700130116           endif;
030800130117
030900130117           // ·?Impostazione dati della tabella reperita?
031000130117           clear  d3NN;
031100130117           if  wSQL_ds.wSQLuni <> *blank;
031200130117             d3NN = wSQL_ds.wSQLuni;
031300130117           endif;
031400130117
031500130117           // ·?Verifica se LNP e FIL.KSC sono nella stessa £1?
031600130117           //  ?(SE NON è stato inserito un cliente con cui confermare?
031700130117           //  ?le bolle)?
031800130117           $Same£1 = *off;
031900130117           if  d3NN.§3NNcc <= *zero;
032000130117             exsr  sr_Ctrl£1;
032100130117           endif;
032200130116
032300130116           // ·?Estrazione ultima spedizione da TITAS?
032400130118           //  ?(SE LNP e FIL.KSC NON sono nella stessa £1, o?
032500130118           //  ?SE è stato inserito un cliente con cui confermare le?
032600130118           //  ?bolle)?
032700130117           if  Not $Same£1;
032800130117             exsr  sr_CtrlSped;
032900130117           endif;
033000130116
033100130116           // ·?Lettura cliente successivo?
033200130116           exec sql   fetch next   from C1   into :wSQL_ds;
033300130116
033400130116         EndDo;
033500130116
033600130116         // -?Chiusura cursore SQL?
033700130116         exec SQL   close C1;
033800130116
033900130116       ENDSR;
034000130117
034100130117       //--------------------------------------------------------------
034200130117       //?Controllo se LNP e filiale del cliente KE1 sono nella stessa ?
034300130117       //?"£1".                                                        ?
034400130117       //--------------------------------------------------------------
034500130117       BEGSR  sr_Ctrl£1;
034600130117
034700130117         //$Same£1 = *off;    ?(già così)?
034800130118
034900130118         // -?Se per la filiale è stato specificato un altro codice?
035000130118         //  ?cliente da utilizzare per la conferma => va lasciato?
035100130118         if  d3NN.§3NNcc > *zero;
035200130118           leavesr;
035300130118         endif;
035400130117
035500130117         // -?Reperimento elenco delle filiali - "£1"?
035600130117         //  ?(da quella del cliente - in base al suo codice XXXyyyy)?
035700130118         If  D06key <> %subst( wSQL_ds.wSQLke1 : 1 : 3 );
035800130118           reset  TRUL06ds;
035900130118           //D06cod = 'PP';        ?(già così)?
036000130118           //D06tla = *blank;      ?(già così)?
036100130118           D06key = %subst( wSQL_ds.wSQLke1 : 1 : 3 );
036200130118           kpjbu  = TRUL06ds;
036300130118           TRUL06R( kpjba );
036400130118           TRUL06ds = kpjbu;
036500130118           if  D06err <> *blank;
036600130118             leavesr;
036700130118           endif;
036800130118         EndIf;
036900130117
037000130118         // -?Se LNP non è nella £1 della fil. del cliente => va lasciato?
037100130118         if  %lookup( %subst( wSQL_ds.wSQLke2 : 1 : 3 ) : sk_D06lin ) = *zero;
037200130117           leavesr;
037300130117         endif;
037400130117
037500130117         // -?Verifica se cambiato il terminal per la LNP nell'ultimo?
037600130118         //  ?anno: se cambiato => va lasciato?
037700130117         clear  wSaveLNP;
037800130117         clear  k04azcae03;
037900130117         k_CAEepa = 'P';
038000130117         k_CAEtfp = d3NN.§3NNcc;
038100130117         setll  %kds(k04azcae03)  AZCAE000;
038200130117         reade  %kds(k04azcae03 : 3)  AZCAE000;
038300130117         DoW  Not %eof(AZCAE03L);
038400130117           if  CAEdde >= %dec( %date() - %years(1) );
038500130117             if  CAEtfe <> wSaveLNP  and  wSaveLNP > *zero;
038600130117               leavesr;
038700130117             endif;
038800130117             wSaveLNP = CAEtfe;
038900130117           endif;
039000130117           reade  %kds(k04azcae03 : 3)  AZCAE000;
039100130117         EndDo;
039200130117
039300130117         // -?Impostazione flag che indica che le due filiali sono?
039400130117         //  ?della stessa £1?
039500130117         $Same£1 = *on;
039600130118
039700130118         // -?Stampa cliente in esame SE legato a filiale già nella sua?
039800130118         //  ?"£1" (senza cambiamenti di terminal & senza altro codice?
039900130118         //  ?cliente da usare per la conferma)?
040000130118         exsr  sr_PrintCli;
040100130117
040200130117       ENDSR;
040300130116
040400130116       //--------------------------------------------------------------
040500130116       //?Controllo ultima spedizione su TITAS (con tipo bolla "F1").  ?
040600130116       //--------------------------------------------------------------
040700130116       BEGSR  sr_CtrlSped;
040800130116
040900130122         // -?Controllo ultima spedizione per  il cod. cliente con cui?
041000130122         //  ?confermare le bolle  e  la LNP in tab. "3NN"?
041100130116         clear  k03titas33;
041200130117         if  d3NN.§3NNcc > *zero;
041300130117           k_TASccm = d3NN.§3NNcc;
041400130117         else;
041500130117           k_TASccm = %int( %trim( wSQL_ds.wSQLke1 ) );
041600130117         endif;
041700130117
041800130123         // =>?Il reperimento della data dell'ultima spedizione va?
041900130123         //   ?comunque SEMPRE fatta: il cliente potrebbe essere sempre?
042000130123         //   ?lo stesso, ma NON la LNP.?
042100130123         //If  k_TASccm <> TASccm;
042200130117           setgt  %kds( k03titas33 : 1 )  TITAS33C;
042300130117           readpe  %kds( k03titas33 : 1 )  TITAS33C;
042400130122           DoW  Not %eof(TITAS33C)  and  ( TAStbl <> 'F1'  or
042500130122                TASlnp <> %int( %subst( wSQL_ds.wSQLke2 : 1 : 3 ) ) );
042600130117             readpe  %kds( k03titas33 : 1 )  TITAS33C;
042700130117           EndDo;
042800130123         //EndIf;
042900130116
043000130122         // -?Stampa Cliente/LNP in esame SE non trovate spedizioni?
043100130116         //  ?negli ultimi 120 gg. (inseriti a video)?
043200130116         //if  %eof(TITAS33C);
043300130116         if  %eof(TITAS33C)  or
043400130122             (TASaas * 10000 + TASmgs) < D28dtl;
043500130116           exsr  sr_PrintCli;
043600130116         endif;
043700130116
043800130116       ENDSR;
043900130116
044000130116       //--------------------------------------------------------------
044100130118       //?Stampa cliente in tab. "3NN", ma senza spedizioni;           ?
044200130118       //?               oppure         con LNP già nella sua "£1" e   ?
044300130118       //?                              senza cambiamenti di terminal e?
044400130118       //?                              senza altro codice per la conf.?
044500130116       //--------------------------------------------------------------
044600130116       BEGSR  sr_PrintCli;
044700130116
044800130116         clear  PrtDET_ds;
044900130118         clear  Prt1DET_ds;
045000130116
045100130116         // -?Decodifica cliente (tbeKE1) - alla rottura?
045200130116         If  %trimr( wSQL_ds.wSQLke1 ) <> SaveKSC;
045300130116           clear  TIBS69ds;
045400130116           I69sif = knsif;
045500130116           I69kcc = DUTkci;
045600130116           I69kac = %int( %trim( wSQL_ds.wSQLke1 ) );
045700130116           clear  ds_CNACO;
045800130116           tibs69r( tibs69ds :
045900130116                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
046000130116           if  O69err = *on;
046100130116             ACOrag = *all'? ';
046200130116           endif;
046300130116           wSaveRAG = ACOrag;
046400130116         EndIf;
046500130116
046600130116         // -?Decodifica cliente (§3NNcc)?
046700130117         If  d3NN.§3NNcc > *zero  and  d3NN.§3NNcc <> SaveCC;
046800130116           clear  TIBS69ds;
046900130116           I69sif = knsif;
047000130116           I69kcc = DUTkci;
047100130116           I69kac = d3NN.§3NNcc;
047200130116           clear  ds_CNACO;
047300130116           tibs69r( tibs69ds :
047400130116                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
047500130116           if  O69err = *on;
047600130116             ACOrag = *all'? ';
047700130116           endif;
047800130117           wSaveDesCC = ACOrag;
047900130116         EndIf;
048000130116
048100130116         // -?Impostazione campi di output?
048200130118         IF  Not $Same£1;
048300130118
048400130118           If  %trimr( wSQL_ds.wSQLke1 ) <> Save0KSC;
048500130118             s_CodKsc = %trimr( wSQL_ds.wSQLke1 );
048600130118             s_DesKsc = %subst( wSaveRAG : 1 : %len(s_DesKsc) );
048700130118             Save0KSC = %trimr( wSQL_ds.wSQLke1 );
048800130118           Else;
048900130118             s_CodKsc = '   "   ';
049000130118           EndIf;
049100130118           s_LnP    = %trimr( wSQL_ds.wSQLke2 );
049200130118           If  d3NN.§3NNcc > *zero;
049300130118             if  d3NN.§3NNcc <> SaveCC;
049400130118               s_CodCC  = %editc( d3NN.§3NNcc : 'X' );
049500130118               s_DesCC  = wSaveDesCC;
049600130118             else;
049700130118               s_CodCC  = '   "   ';
049800130118             endif;
049900130118           EndIf;
050000130118           if  Not  %eof(TITAS33C);
050100130118             wDate_Eur = %date( TASaas * 10000 + TASmgs : *iso );
050200130118             s_DtUltS  = %dec( wDate_Eur );
050300130118           endif;
050400130118
050500130118         ELSE;
050600130118
050700130118           If  %trimr( wSQL_ds.wSQLke1 ) <> Save1KSC;
050800130118             s1_CodKsc = %trimr( wSQL_ds.wSQLke1 );
050900130118             s1_DesKsc = %subst( wSaveRAG : 1 : %len(s_DesKsc) );
051000130118             Save1KSC  = %trimr( wSQL_ds.wSQLke1 );
051100130118           Else;
051200130118             s1_CodKsc = '   "   ';
051300130118           EndIf;
051400130118           s1_LnP    = %trimr( wSQL_ds.wSQLke2 );
051500130118
051600130118         ENDIF;
051700130116
051800130116         // -?Stampa riga di dettaglio?
051900130118         IF  Not $Same£1;
052000130118           //if  SaveFIL <> %subst( SaveKSC : 1 : 3 );
052100130118           //  except  PrtSPACE;
052200130118           //endif;
052300130118           if  *inOF;
052400130118             //if  SaveFIL <> %subst( SaveKSC : 1 : 3 );
052500130118             //  except  PrtSPACE;
052600130118             //endif;
052700130118             except  PrtTXT;
052800130118             *inOF = *off;
052900130118           endif;
053000130118           except  PrtDET;
053100130118           s_TotCli += 1;
053200130118         ELSE;
053300130118           if  *inOA;
053400130118             except  Prt1TXT;
053500130118             *inOA = *off;
053600130118           endif;
053700130118           except  Prt1DET;
053800130118           s1_TotCli += 1;
053900130118         ENDIF;
054000130118
054100130116         SaveKSC = %trimr( wSQL_ds.wSQLke1 );
054200130117         SaveCC  = d3NN.§3NNcc;
054300130118         //SaveFIL = %subst( SaveKSC : 1 : 3 );
054400130116
054500130116       ENDSR;
054600130116
054700130116       //--------------------------------------------------------------
054800130116       //?Stampa segnalazione errore.                                  ?
054900130116       //--------------------------------------------------------------
055000130116       BEGSR  sr_PrintErr;
055100130116
055200130116         // -?Stampa del Dump?
055300130116         Dump(A);
055400130116
055500130116         // -?Stampa del Job-Log?
055600130116         Qcmd = 'DSPJOBLOG job(*) output(*print)';
055700130116         exsr  sr_ExecCmd;
055800130116
055900130116         // -?Stampa segnalazione nel file di stampa?
056000130116         except  PrtERR;
056100130116         except  PrtEND;
056200130116         close  QSYSPRT;
056300130116
056400130116       ENDSR;
056500130116
056600130116       //--------------------------------------------------------------
056700130116       //?Esecuzione del comando (già impostato).                      ?
056800130116       //--------------------------------------------------------------
056900130116       BEGSR  sr_ExecCmd;
057000130116
057100130116         clear Qcap0100;
057200130116         Qcabcsdh = *off;
057300130116         Qcapa    = *off;
057400130116         Qcacmdss = *off;
057500130116         Qcaerved = *allX'00';
057600130116
057700130116         clear Qusec;
057800130116         Qusbprv  = %size(Qusec);
057900130116
058000130116         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
058100130116                           %size(Qcap0100) : 'CPOP0100' : *omit :
058200130116                           0 : 0 : Qusec);
058300130116
058400130116         //if  Qusei <> *blank;
058500130116         //  ...;
058600130116         //endif;
058700130116
058800130116       ENDSR;
058900130116
059000130116       //--------------------------------------------------------------
059100130116       //?Operazioni finali.                                           ?
059200130116       //--------------------------------------------------------------
059300130116       BEGSR  sr_RoutEnd;
059400130122
059500130122         // -?Chiusura printer-files?
059600130122         except  PrtEND;
059700130122         close  QSYSPRT;
059800130122
059900130122         except  Prt1END;
060000130122         close  QSYSPRT1;
060100130116
060200130116         // -?Chiusura applicazioni precedentemente aperte?
060300130116         reset  TIBS69ds;
060400130116         tibs69r( tibs69ds :
060500130116                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
060600130116
060700130116         // -?Chiusura pgm?
060800130116         return;
060900130116
061000130116       ENDSR;
061100130116
061200130116      /end-free
061300130116
061400130116       //--------------------------------------------------------------
061500130116       //?Definizione file di stampa in output.                        ?
061600130116       //--------------------------------------------------------------
061700130116
061800130116     oQSYSPRT   e            PrtTXT            3
061900130116     o                       RsUt
062000130116     o                                         +  3 'CLIENTI IN TAB. "-
062100130116     o                                              3NN", MA SENZA SPE-
062200130116     o                                              DIZIONI'
062300130122     o                       SDSpgm            + 10
062400130116     o                       *date         y   +  5
062500130116     o                                         +  5 'Pag.'
062600130116     o                       Page          z   +  0
062700130116     o          e            PrtTXT      0
062800130116     o                                         + 23 'CLIENTI IN TAB. "-
062900130116     o                                              3NN", MA SENZA SPE-
063000130116     o                                              DIZIONI'
063100130116     o          e            PrtTXT      1
063200130116     o                                         + 23 'NEGLI ULTIMI'
063300130122     o                       D28ggl        1   +  1
063400130116     o                                         +  1 'GIORNI (DAL'
063500130122     o                       W28dtl        y   +  1
063600130116     o                                         +  0 ')'
063700130116     o          e            PrtTXT      0
063800130116     o                                         + 23 'NEGLI ULTIMI'
063900130122     o                       D28ggl        1   +  1
064000130116     o                                         +  1 'GIORNI (DAL'
064100130122     o                       W28dtl        y   +  1
064200130116     o                                         +  0 ')'
064300130116     o          e            PrtTXT      2
064400130116     o                       sk_Txt(1)
064500130116     o                       sK_Txt(4)         +  0
064600130116     o          e            PrtTXT      1
064700130116     o                       sk_Txt(2)
064800130116     o                       sk_Txt(5)         +  0
064900130116      *
065000130116     o          e            PrtERR      2
065100130116     o                                              'RILEVATO ERRORE: -
065200130116     o                                              consultare le stam-
065300130116     o                                              pe del DUMP e del -
065400130116     o                                              JOBLOG.'
065500130116      *
065600130116     o*//       e            PrtSPACE    1
065700130116      *
065800130116     o          e            PrtDET      1
065900130116     o                       s_CodKsc
066000130116     o                       s_DesKsc          +  1
066100130116     o                       s_LnP             +  2
066200130116     o                       s_CodCC           +  2
066300130116     o                       s_DesCC           +  1
066400130116     o                       s_DtUltS          +  2 '  /  /    '
066500130116      *
066600130116     o          e            PrtEND      2
066700130116     o                                         + 24 '***  Fine Stampa  ***'
066800130116     o                                         +  3 'Stampati'
066900130116     o                       s_TotCli      1   +  1
067000130116     o                                         +  1 'clienti/filiali.'
067100130118
067200130118       //--------------------------------------------------------------
067300130118
067400130118     oQSYSPRT1  e            Prt1TXT           3
067500130118     o                       RsUt
067600130118     o                                         +  3 'CLIENTI IN TAB. "-
067700130118     o                                              3NN" PER FILIALE I-
067800130118     o                                              N "£1" '
067900130122     o                       SDSpgm            + 10
068000130118     o                       *date         y   +  5
068100130118     o                                         +  5 'Pag.'
068200130118     o                       Page1         z   +  0
068300130118     o          e            Prt1TXT     0
068400130118     o                                         + 23 'CLIENTI IN TAB. "-
068500130118     o                                              3NN" PER FILIALE I-
068600130118     o                                              N "£1" '
068700130118     o          e            Prt1TXT     1
068800130118     o                                         + 23 '(SENZA MODIFICHE -
068900130118     o                                              NELL''ULTIMO ANNO)'
069000130118     o          e            Prt1TXT     0
069100130118     o                                         + 23 '(SENZA MODIFICHE -
069200130118     o                                              NELL''ULTIMO ANNO)'
069300130118     o          e            Prt1TXT     2
069400130118     o                       s1_Txt1
069500130118     o          e            Prt1TXT     1
069600130118     o                       s1_Txt2
069700130118      *
069800130118     o          e            PrtERR      2
069900130118     o                                              'RILEVATO ERRORE: -
070000130118     o                                              consultare le stam-
070100130118     o                                              pe del DUMP e del -
070200130118     o                                              JOBLOG.'
070300130118      *
070400130118     o*//       e            Prt1SPACE   1
070500130118      *
070600130118     o          e            Prt1DET     1
070700130118     o                       s1_CodKsc
070800130118     o                       s1_DesKsc         +  1
070900130118     o                       s1_LnP            +  2
071000130118      *
071100130118     o          e            Prt1END     2
071200130118     o                                         + 24 '***  Fine Stampa  ***'
071300130118     o                                         +  3 'Stampati'
071400130118     o                       s1_TotCli     1   +  1
071500130118     o                                         +  1 'clienti/filiali.'
071600130116
071700130116       //--------------------------------------------------------------
071800130116       //?Definizione schiere a tempo di compilazione                  ?
071900130116       //--------------------------------------------------------------
072000130116
072100130116** -?sk_Txt:?Testata di stampa?...+....4....+....5....+....6....+*
072200130116Cliente KE1                                  LNP  Nuovo codice       1
072300130116-------------------------------------------  ---  ----------------   2
0724001301161234567 *...+....1....+....2....+....3....+  123  1234567 *...+...   3
072500130116                             Dt.UltSped                              4
072600130116---------------------------  ----------                              5
072700130116.1....+....2....+....3....+  12/34/5678                              6
