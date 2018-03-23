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
003300130117       // -?Video?
003400130116     fTNSB27D   cf   e             workstn
003500130116     f                                     indds(IndDspF)
003600130116     f                                     infds(InfDspF)
003700130116
003800130116       // -?File di Stampa?
003900130116     fQSYSPRT   o    f  132        printer  oflind(*inOF)  usropn
004000130118     fQSYSPRT1  o    f  132        printer  oflind(*inOA)  usropn
004100130116
004200130116       //--------------------------------------------------------------
004300130116       //?Definizione costanti.                                        ?
004400130116       //--------------------------------------------------------------
004500130116
004600130116       // -?Comando di override al PrtF?
004700130116     d c_CmdOvrPrtF    c                   const('OVRPRTF +
004800130116     d                                           file(QSYSPRT) +
004900130116     d                                           pagesize(66 132) +
005000130116     d                                           lpi(6) cpi(10) +
005100130116     d                                           usrdta(''3NN_NoSped'') +
005200130116     d                                           ovrscope(*actgrpdfn) +
005300130116     d                                           ')
005400130118     d c_CmdOvrPrtF1   c                   const('OVRPRTF +
005500130118     d                                           file(QSYSPRT1) +
005600130118     d                                           pagesize(66 132) +
005700130118     d                                           lpi(6) cpi(10) +
005800130118     d                                           usrdta(''3NN+AZCAE'') +
005900130118     d                                           ovrscope(*actgrpdfn) +
006000130118     d                                           ')
006100130116
006200130116       // -?Comando di cellazione override?
006300130116     d c_CmdDltOvr     c                   const('DLTOVR +
006400130116     d                                            file(QSYSPRT) +
006500130116     d                                            lvl(*actgrpdfn)')
006600130118     d c_CmdDltOvr1    c                   const('DLTOVR +
006700130118     d                                            file(QSYSPRT1 +
006800130118     d                                            lvl(*actgrpdfn)')
006900130116
007000130116       // -?Tasti funzionali a video?
007100130116     d c_F01           c                   const(x'31')
007200130116     d c_F02           c                   const(x'32')
007300130116     d c_F03           c                   const(x'33')
007400130116     d c_F04           c                   const(x'34')
007500130116     d c_F05           c                   const(x'35')
007600130116     d c_F06           c                   const(x'36')
007700130116     d c_F07           c                   const(x'37')
007800130116     d c_F08           c                   const(x'38')
007900130116     d c_F09           c                   const(x'39')
008000130116     d c_F10           c                   const(x'3A')
008100130116     d c_F11           c                   const(x'3B')
008200130116     d c_F12           c                   const(x'3C')
008300130116     d c_F13           c                   const(x'B1')
008400130116     d c_F14           c                   const(x'B2')
008500130116     d c_F15           c                   const(x'B3')
008600130116     d c_F16           c                   const(x'B4')
008700130116     d c_F17           c                   const(x'B5')
008800130116     d c_F18           c                   const(x'B6')
008900130116     d c_F19           c                   const(x'B7')
009000130116     d c_F20           c                   const(x'B8')
009100130116     d c_F21           c                   const(x'B9')
009200130116     d c_F22           c                   const(x'BA')
009300130116     d c_F23           c                   const(x'BB')
009400130116     d c_F24           c                   const(x'BC')
009500130116     d c_Enter         c                   const(x'F1')
009600130116     d c_RollDown      c                   const(x'F4')
009700130116     d c_RollUp        c                   const(x'F5')
009800130116
009900130116       // -?Costante per controllo "caratteri solo numerici"?
010000130116     d c_Digits        c                   const('0123456789')
010100130116
010200130116       //--------------------------------------------------------------
010300130116       //?Definizione schiere.                                         ?
010400130116       //--------------------------------------------------------------
010500130116
010600130116       // -?Testate in stampa?
010700130116     d sk_Txt          s             66    dim( 6)  ctdata  perrcd( 1)
010800130116
010900130116       // -?Messaggi di errore?
011000130116     d sk_Msg          s             78    dim( 1)  ctdata  perrcd( 1)
011100130116
011200130116       //--------------------------------------------------------------
011300130116       //?Definizione aree dati.                                       ?
011400130116       //--------------------------------------------------------------
011500130116
011600130116       // -?Dati utente?
011700130116     d §AzUte        e ds                  extname(AZUTE00F)
011800130116     d                                     dtaara
011900130116     d §DatiUte      e ds                  extname(dDatiUte)
012000130116     d                                     dtaara
012100130116
012200130116       //--------------------------------------------------------------
012300130116       //?Definizione strutture dati.                                  ?
012400130116       //--------------------------------------------------------------
012500130116
012600130116       // -?Status ds?
012700130116     d Status         sds
012800130116     d   SDSpgm          *proc
012900130116
013000130116       // -?InfDS?
013100130116     d InfDspF         ds
013200130116     d   dsp_aid             369    369a
013300130116
013400130116       // -?Indicatori su DspF?
013500130116     d IndDspF         ds                  inz
013600130116         // -?Emissione messaggio di errore?
013700130116     d   ErrMessage                    n   overlay(IndDspF : 28)
013800130116         // -?Posizionamento cursore & segnalazione errore?
013900130116     d   PosCurGGL                     n   overlay(IndDspF : 51)
014000130116         //   -?Riemissione videata?
014100130116     d   ErrGenerico                   n   overlay(IndDspF : 99)
014200130116
014300130116       // -?Dati estratti via SQL?
014400130116     d TNTBE00F      e ds                  based(nullPtr)
014500130116     d                                     prefix(ds_)
014600130116     d wSQL_ds         ds                  inz  qualified
014700130116     d   wSQLcod                           inz  like(ds_TBEcod)
014800130116     d   wSQLke1                           inz  like(ds_TBEke1)
014900130116     d   wSQLke2                           inz  like(ds_TBEke2)
015000130116     d   wSQLuni                           inz  like(ds_TBEuni)
015100130116
015200130116       // -?Parametri ricevuti?
015300130116     d KPJBA         e ds
015400130116
015500130116       // -?Tabelle usate:?
015600130116       // ·?"3NN" = Conferma disk su altro AS?
015700130116     d d3NN          e ds                  inz  qualified
015800130116
015900130116       //--------------------------------------------------------------
016000130116       //?Definizione variabili globali.                               ?
016100130116       //--------------------------------------------------------------
016200130116
016300130116       // -?Flags booleani?
016400130116     d $Fine           s               n   inz
016500130116     d $InzD01         s               n   inz(*on)
016600130116     d $InzD02         s               n   inz(*on)
016700130117     d $Same£1         s               n   inz
016800130116
016900130116       // -?Indici di schiera?
017000130116     d xx              s              3  0  inz
017100130116
017200130116       // -?Variabili per la gestione del video?
017300130116     d $Video          s              2    inz('D1')
017400130116
017500130116       // -?Stringa SQL da eseguire?
017600130116     d wSQL            s           2048    Varying        inz
017700130116
017800130116       // -?Campi di stampa?
017900130116     d s_TotCli        s                   inz  like(VD3ncl)
018000130118     d s1_TotCli       s                   inz  like(VD3ncl)
018100130118     d s1_Txt1         s             50    inz
018200130118     d s1_Txt2         s             50    inz
018300130116     d PrtDET_ds       ds                  inz
018400130116     d   s_CodKsc                     7a   inz
018500130116     d   s_DesKsc                    35a   inz
018600130116     d   s_LnP                        3a   inz
018700130116     d   s_CodCC                      7a   inz
018800130116     d   s_DesCC                     35a   inz
018900130116     d   s_DtUltS                     8  0 inz
019000130118     d Prt1DET_ds      ds                  inz
019100130118     d   s1_CodKsc                    7a   inz
019200130118     d   s1_DesKsc                   35a   inz
019300130118     d   s1_LnP                       3a   inz
019400130116
019500130116       // -?Campi per controllo rotture di livello?
019600130117     d SaveKSC         s                   inz  like(s_CodKSC)
019700130117     d SaveCC          s                   inz  like(d3NN.§3NNcc)
019800130118     d*// SaveFIL         s              3    inz
019900130118     d Save0KSC        s                   inz  like(s_CodKSC)
020000130118     d Save1KSC        s                   inz  like(s1_CodKSC)
020100130116
020200130116       // -?Campi di comodo?
020300130116     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
020400130116     d wDate_ISO       s               d   datfmt(*iso)   inz(*loval)
020500130116     d wDate           s              8  0 inz
020600130116     d WD1dtl          s                   inz  like(VD1dtl)
020700130117     d wSaveLNP        s                   inz  like(CAEtfe)
020800130116     d wSaveRAG        s                   inz  like(ACOrag)
020900130117     d wSaveDesCC      s                   inz  like(ACOrag)
021000130116
021100130116       //--------------------------------------------------------------
021200130116       //?Definizione prototipi procedure.                             ?
021300130116       //--------------------------------------------------------------
021400130116
021500130116       // -?Reperimento dati utente?
021600130116     d TIBS34ds      e ds
021700130116      /copy gaitrasrc/srcProtoPR,TIBS34R
021800130117
021900130117       // -?Caricamento filiali con lo stesso terminal?
022000130117     d TRUL06ds      e ds                  inz
022100130117     d   D06cod      e                     inz('PP')
022200130118     d   sk_D06lin                    3    overlay(D06lia)
022300130117     d                                     dim(30)
022400130117      /copy gaitrasrc/srcProtoPR,TRUL06R
022500130116
022600130116       // -?Controllo/Decodifica cliente?
022700130116      /copy gaitrasrc/srcProtoPI,TIBS69R
022800130116      /copy gaitrasrc/srcProtoPR,TIBS69R
022900130116
023000130116       // -?Parametri API QCAPCMD (Process Commands)?
023100130116     d Qcmd            s           2048    inz  varying
023200130116      /copy qSysInc/qRpgleSrc,QCAPCMD
023300130116       // -?API QCAPCMD (Process Commands)?
023400130116      /copy gaitrasrc/srcProtoPR,QCAPCMD
023500130116
023600130116       // -?Parametri gestione errori API.?
023700130116      /copy qsysinc/qrpglesrc,QUSEC
023800130116
023900130116       //--------------------------------------------------------------
024000130116       //?Definizione key-list.                                        ?
024100130116       //--------------------------------------------------------------
024200130116
024300130116       // -?File TITAS33C?
024400130116     d k03titas33    e ds                  extname(TITAS33C : *key)
024500130116     d                                     prefix(k_)   inz
024600130117
024700130117       // -?File AZCAE03L?
024800130117     d k04azcae03    e ds                  extname(AZCAE03L : *key)
024900130117     d                                     prefix(k_)   inz
025000130116
025100130116       //--------------------------------------------------------------
025200130116       //?M A I N - L I N E                                            ?
025300130116       //--------------------------------------------------------------
025400130116
025500130116     c     *Entry        plist
025600130116     c                   parm                    KPJBA
025700130116
025800130116      /free
025900130116
026000130116       // -?Operazioni iniziali?
026100130116       exsr  sr_RoutInz;
026200130116
026300130116       // -?Ciclo di gestione del file video?
026400130116       DOW  $Fine = *off;
026500130116
026600130116         select;
026700130116
026800130116           // -?Filtro di lancio?
026900130116           when $Video = 'D1';
027000130116             exsr  sr_GesD01;
027100130116
027200130116           // -?Visualizzazione risultato?
027300130116           when $Video = 'D2';
027400130116             exsr  sr_GesD023;
027500130116
027600130116           // -?? ? ??
027700130116           other;
027800130116             $Fine = *on;
027900130116
028000130116         endsl;
028100130116
028200130116       ENDDO;
028300130116
028400130116       // -?Operazioni finali?
028500130116       exsr  sr_RoutEnd;
028600130116
028700130116       //--------------------------------------------------------------
028800130116       //?Operazioni iniziali.                                         ?
028900130116       //--------------------------------------------------------------
029000130116       BEGSR  sr_RoutInz;
029100130116
029200130116         *inLR = *on;
029300130116
029400130116         // -?Reperimento dati job?
029500130116         exsr  sr_DatiJob;
029600130116
029700130116         // -?Impostazione nome programma a video?
029800130116         VT1pgm = SDSpgm;
029900130116
030000130116         // -?Reperimento data odierna?
030100130116         wDate = %int( %subst( %char( %dec( %timestamp() ) )
030200130116                               : 1 : 8 ) );
030300130116
030400130118         // -?Override ai printer-files?
030500130116         Qcmd = c_CmdOvrPrtF;
030600130116         exsr  sr_ExecCmd;
030700130116         //if  Qusei <> *blank;
030800130116         //  ...;
030900130116         //endif;
031000130118         Qcmd = c_CmdOvrPrtF1;
031100130118         exsr  sr_ExecCmd;
031200130118         //if  Qusei <> *blank;
031300130118         //  ...;
031400130118         //endif;
031500130116
031600130116       ENDSR;
031700130116
031800130116       //--------------------------------------------------------------
031900130116       //?Reperimento Dati del job (Utente/Operativi).                 ?
032000130116       //--------------------------------------------------------------
032100130116       BEGSR  sr_DatiJob;
032200130116
032300130116         in(E) §AzUte;
032400130116         if NOT %error;
032500130116           in(E) §DatiUte;
032600130116         endif;
032700130116         if %error or RSut = *blanks;
032800130116           clear TIBS34ds;
032900130116           tibs34r ( tibs34ds );
033000130116           in §AzUte;
033100130116           in §DatiUte;
033200130116         endif;
033300130116
033400130116       ENDSR;
033500130116
033600130116       //--------------------------------------------------------------
033700130116       //?Gestione videata D01.                                        ?
033800130116       //--------------------------------------------------------------
033900130116       BEGSR  sr_GesD01;
034000130116
034100130116         // -?Inizializzazione videata?
034200130116         if  $InzD01 = *on;
034300130116           exsr  sr_InzD01;
034400130116           $InzD01 = *off;
034500130116         endif;
034600130116
034700130116         // -?Emissione videata?
034800130116         write  SB27T01;
034900130116         write  SB27P01;
035000130116         exfmt  SB27D01;
035100130116
035200130116         clear  VIDmsg;
035300130116         reset  ErrMessage;
035400130116         reset  ErrGenerico;
035500130116
035600130116         SELECT;
035700130116
035800130116           // -?F3=Fine?
035900130116           WHEN  dsp_aid = c_F03;
036000130116             $Fine = *on;
036100130116
036200130116           // -?Invio, F6=Conferma?
036300130116           OTHER;
036400130116             exsr  sr_CtrD01;
036500130116             if  ErrGenerico = *off  and  dsp_aid = c_F06;
036600130116               $Video    = 'D2';
036700130116               $InzD02   = *on;
036800130116             endif;
036900130116
037000130116         ENDSL;
037100130116
037200130116       ENDSR;
037300130116
037400130116       //--------------------------------------------------------------
037500130116       //?Inizializzazione videata D01.                                ?
037600130116       //--------------------------------------------------------------
037700130116       BEGSR  sr_InzD01;
037800130116
037900130116         clear  SB27D01;
038000130116
038100130116         VD1ggl = 120;
038200130116
038300130118         exsr  sr_CtrD01;
038400130118         clear  VIDmsg;
038500130118         reset  ErrMessage;
038600130118         reset  ErrGenerico;
038700130116
038800130116       ENDSR;
038900130116
039000130116       //--------------------------------------------------------------
039100130116       //?Controllo dati immessi nella videata D01.                    ?
039200130116       //--------------------------------------------------------------
039300130116       BEGSR  sr_CtrD01;
039400130116
039500130116         // -?Spegnimento indicatori di posizionamento cursore?
039600130116         %subst(IndDspF : 28) = *off;
039700130116
039800130116         // -?Numero giorni inferiori a 1?
039900130116         if  VD1ggl <= 1;
040000130116           VIDmsg = sk_Msg(01);
040100130116           PosCurGGL   = *on;
040200130116           ErrMessage  = *on;
040300130116           ErrGenerico = *on;
040400130116           leavesr;
040500130116         endif;
040600130116
040700130116         wDate_Eur = %date( wDate : *iso ) - %days( VD1ggl );
040800130116         wDate_Iso = wDate_Eur;
040900130116         VD1dtl = %dec( wDate_Eur );
041000130116         WD1dtl = %dec( wDate_Iso );
041100130116
041200130116       ENDSR;
041300130116
041400130116       //--------------------------------------------------------------
041500130116       //?Gestione videate D02/D03.                                    ?
041600130116       //--------------------------------------------------------------
041700130116       BEGSR  sr_GesD023;
041800130116
041900130116         // -?Inizializzazione videata?
042000130116         if  $InzD02 = *on;
042100130116           exsr  sr_InzD023;
042200130116           $InzD02 = *off;
042300130116         endif;
042400130116
042500130116         // -?Emissione videata?
042600130116         write  SB27T01;
042700130116         write  SB27D01;
042800130116         write  Protect;
042900130116         exfmt  SB27D03;
043000130116
043100130116         clear  VIDmsg;
043200130116         reset  ErrMessage;
043300130116         reset  ErrGenerico;
043400130116
043500130116         SELECT;
043600130116
043700130116           // -?F3=Fine?
043800130116           WHEN  dsp_aid = c_F03;
043900130116             $Fine = *on;
044000130116
044100130116           // -?F12=Ritorno?
044200130116           WHEN  dsp_aid = c_F12;
044300130116             $Video = 'D1';
044400130116
044500130116         ENDSL;
044600130116
044700130116       ENDSR;
044800130116
044900130116       //--------------------------------------------------------------
045000130116       //?Inizializzazione videata D02.                                ?
045100130116       //--------------------------------------------------------------
045200130116       BEGSR  sr_InzD023;
045300130116
045400130116         // -?Spegnimento degli indicatori di errore?
045500130116         %subst(IndDspF : 50) = *off;
045600130116
045700130116         // -?Emissione videata di avviso dei lavori in corso?
045800130116         write  SB27T01;
045900130116         write  SB27D01;
046000130116         write  Protect;
046100130116         write  SB27D02;
046200130116
046300130116         // -?Pulizia videata?
046400130116         clear  SB27D03;
046500130116
046600130116         // -?Estrazione e stampa dei clienti?
046700130116         exsr  sr_ReadTab;
046800130116
046900130116       ENDSR;
047000130116
047100130116       //--------------------------------------------------------------
047200130116       //?Estrazione dati da tab. "3NN" e dal file TITAS.              ?
047300130116       //--------------------------------------------------------------
047400130116       BEGSR  sr_ReadTab;
047500130116
047600130116         // -?Apertura printer-file?
047700130116         open  QSYSPRT;
047800130117         clear Page;
047900130116         except  PrtTXT;
048000130118
048100130118         open  QSYSPRT1;
048200130118         clear Page1;
048300130118         s1_Txt1 = %subst( sk_Txt(1) : 1 : 50 );
048400130118         s1_Txt2 = %subst( sk_Txt(2) : 1 : 50 );
048500130118         except  Prt1TXT;
048600130116
048700130116         // -?Preparazione SQL?
048800130116         wSQL = 'select TBEcod, TBEke1, TBEke2, TBEuni +
048900130116                   from TNTBE00F +
049000130116                  where TBEcod = ''3NN'' +
049100130116                    and TBEatb = '' '' +
049200130116                  order by TBEcod, TBEke1, TBEke2 +
049300130116                    for fetch only';
049400130116
049500130116         exec sql   prepare S1   from :wSQL;
049600130116
049700130116         // -?Dichiarazione cursore SQL?
049800130116         exec sql   declare C1   cursor for S1;
049900130116
050000130116         // -?Apertura cursore SQL?
050100130116         exec sql   open C1;
050200130116
050300130116         // -?Rilevato errore SQL?
050400130116         if  SQLcode < *zero;
050500130116           exsr  sr_PrintErr;
050600130116           exsr  sr_RoutEnd;
050700130116         endif;
050800130116
050900130116         // -?Ciclo esecuzione SQL?
051000130116         exec sql   fetch next   from C1   into :wSQL_ds;
051100130116
051200130116         DoW  SQLcode <> 100;
051300130116
051400130116           // ·?Rilevato errore SQL?
051500130116           if  SQLcode < *zero;
051600130116             exsr  sr_PrintErr;
051700130116             exsr  sr_RoutEnd;
051800130116           endif;
051900130117
052000130117           // ·?Impostazione dati della tabella reperita?
052100130117           clear  d3NN;
052200130117           if  wSQL_ds.wSQLuni <> *blank;
052300130117             d3NN = wSQL_ds.wSQLuni;
052400130117           endif;
052500130117
052600130117           // ·?Verifica se LNP e FIL.KSC sono nella stessa £1?
052700130117           //  ?(SE NON è stato inserito un cliente con cui confermare?
052800130117           //  ?le bolle)?
052900130117           $Same£1 = *off;
053000130117           if  d3NN.§3NNcc <= *zero;
053100130117             exsr  sr_Ctrl£1;
053200130117           endif;
053300130116
053400130116           // ·?Estrazione ultima spedizione da TITAS?
053500130118           //  ?(SE LNP e FIL.KSC NON sono nella stessa £1, o?
053600130118           //  ?SE è stato inserito un cliente con cui confermare le?
053700130118           //  ?bolle)?
053800130117           if  Not $Same£1;
053900130117             exsr  sr_CtrlSped;
054000130117           endif;
054100130116
054200130116           // ·?Lettura cliente successivo?
054300130116           exec sql   fetch next   from C1   into :wSQL_ds;
054400130116
054500130116         EndDo;
054600130116
054700130116         // -?Chiusura cursore SQL?
054800130116         exec SQL   close C1;
054900130116
055000130118         // -?Chiusura printer-files?
055100130116         except  PrtEND;
055200130116         close  QSYSPRT;
055300130118
055400130118         except  Prt1END;
055500130118         close  QSYSPRT1;
055600130116
055700130116       ENDSR;
055800130117
055900130117       //--------------------------------------------------------------
056000130117       //?Controllo se LNP e filiale del cliente KE1 sono nella stessa ?
056100130117       //?"£1".                                                        ?
056200130117       //--------------------------------------------------------------
056300130117       BEGSR  sr_Ctrl£1;
056400130117
056500130117         //$Same£1 = *off;    ?(già così)?
056600130118
056700130118         // -?Se per la filiale è stato specificato un altro codice?
056800130118         //  ?cliente da utilizzare per la conferma => va lasciato?
056900130118         if  d3NN.§3NNcc > *zero;
057000130118           leavesr;
057100130118         endif;
057200130117
057300130117         // -?Reperimento elenco delle filiali - "£1"?
057400130117         //  ?(da quella del cliente - in base al suo codice XXXyyyy)?
057500130118         If  D06key <> %subst( wSQL_ds.wSQLke1 : 1 : 3 );
057600130118           reset  TRUL06ds;
057700130118           //D06cod = 'PP';        ?(già così)?
057800130118           //D06tla = *blank;      ?(già così)?
057900130118           D06key = %subst( wSQL_ds.wSQLke1 : 1 : 3 );
058000130118           kpjbu  = TRUL06ds;
058100130118           TRUL06R( kpjba );
058200130118           TRUL06ds = kpjbu;
058300130118           if  D06err <> *blank;
058400130118             leavesr;
058500130118           endif;
058600130118         EndIf;
058700130117
058800130118         // -?Se LNP non è nella £1 della fil. del cliente => va lasciato?
058900130118         if  %lookup( %subst( wSQL_ds.wSQLke2 : 1 : 3 ) : sk_D06lin ) = *zero;
059000130117           leavesr;
059100130117         endif;
059200130117
059300130117         // -?Verifica se cambiato il terminal per la LNP nell'ultimo?
059400130118         //  ?anno: se cambiato => va lasciato?
059500130117         clear  wSaveLNP;
059600130117         clear  k04azcae03;
059700130117         k_CAEepa = 'P';
059800130117         k_CAEtfp = d3NN.§3NNcc;
059900130117         setll  %kds(k04azcae03)  AZCAE000;
060000130117         reade  %kds(k04azcae03 : 3)  AZCAE000;
060100130117         DoW  Not %eof(AZCAE03L);
060200130117           if  CAEdde >= %dec( %date() - %years(1) );
060300130117             if  CAEtfe <> wSaveLNP  and  wSaveLNP > *zero;
060400130117               leavesr;
060500130117             endif;
060600130117             wSaveLNP = CAEtfe;
060700130117           endif;
060800130117           reade  %kds(k04azcae03 : 3)  AZCAE000;
060900130117         EndDo;
061000130117
061100130117         // -?Impostazione flag che indica che le due filiali sono?
061200130117         //  ?della stessa £1?
061300130117         $Same£1 = *on;
061400130118
061500130118         // -?Stampa cliente in esame SE legato a filiale già nella sua?
061600130118         //  ?"£1" (senza cambiamenti di terminal & senza altro codice?
061700130118         //  ?cliente da usare per la conferma)?
061800130118         exsr  sr_PrintCli;
061900130117
062000130117       ENDSR;
062100130116
062200130116       //--------------------------------------------------------------
062300130116       //?Controllo ultima spedizione su TITAS (con tipo bolla "F1").  ?
062400130116       //--------------------------------------------------------------
062500130116       BEGSR  sr_CtrlSped;
062600130116
062700130122         // -?Controllo ultima spedizione per  il cod. cliente con cui?
062800130122         //  ?confermare le bolle  e  la LNP in tab. "3NN"?
062900130116         clear  k03titas33;
063000130117         if  d3NN.§3NNcc > *zero;
063100130117           k_TASccm = d3NN.§3NNcc;
063200130117         else;
063300130117           k_TASccm = %int( %trim( wSQL_ds.wSQLke1 ) );
063400130117         endif;
063500130117
063600130117         If  k_TASccm <> TASccm;
063700130117           setgt  %kds( k03titas33 : 1 )  TITAS33C;
063800130117           readpe  %kds( k03titas33 : 1 )  TITAS33C;
063900130122           DoW  Not %eof(TITAS33C)  and  ( TAStbl <> 'F1'  or
064000130122                TASlnp <> %int( %subst( wSQL_ds.wSQLke2 : 1 : 3 ) ) );
064100130117             readpe  %kds( k03titas33 : 1 )  TITAS33C;
064200130117           EndDo;
064300130117         EndIf;
064400130116
064500130122         // -?Stampa Cliente/LNP in esame SE non trovate spedizioni?
064600130116         //  ?negli ultimi 120 gg. (inseriti a video)?
064700130116         //if  %eof(TITAS33C);
064800130116         if  %eof(TITAS33C)  or
064900130116             (TASaas * 10000 + TASmgs) < WD1dtl;
065000130116           exsr  sr_PrintCli;
065100130116         endif;
065200130116
065300130116       ENDSR;
065400130116
065500130116       //--------------------------------------------------------------
065600130118       //?Stampa cliente in tab. "3NN", ma senza spedizioni;           ?
065700130118       //?               oppure         con LNP già nella sua "£1" e   ?
065800130118       //?                              senza cambiamenti di terminal e?
065900130118       //?                              senza altro codice per la conf.?
066000130116       //--------------------------------------------------------------
066100130116       BEGSR  sr_PrintCli;
066200130116
066300130116         clear  PrtDET_ds;
066400130118         clear  Prt1DET_ds;
066500130116
066600130116         // -?Decodifica cliente (tbeKE1) - alla rottura?
066700130116         If  %trimr( wSQL_ds.wSQLke1 ) <> SaveKSC;
066800130116           clear  TIBS69ds;
066900130116           I69sif = knsif;
067000130116           I69kcc = DUTkci;
067100130116           I69kac = %int( %trim( wSQL_ds.wSQLke1 ) );
067200130116           clear  ds_CNACO;
067300130116           tibs69r( tibs69ds :
067400130116                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
067500130116           if  O69err = *on;
067600130116             ACOrag = *all'? ';
067700130116           endif;
067800130116           wSaveRAG = ACOrag;
067900130116         EndIf;
068000130116
068100130116         // -?Decodifica cliente (§3NNcc)?
068200130117         If  d3NN.§3NNcc > *zero  and  d3NN.§3NNcc <> SaveCC;
068300130116           clear  TIBS69ds;
068400130116           I69sif = knsif;
068500130116           I69kcc = DUTkci;
068600130116           I69kac = d3NN.§3NNcc;
068700130116           clear  ds_CNACO;
068800130116           tibs69r( tibs69ds :
068900130116                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
069000130116           if  O69err = *on;
069100130116             ACOrag = *all'? ';
069200130116           endif;
069300130117           wSaveDesCC = ACOrag;
069400130116         EndIf;
069500130116
069600130116         // -?Impostazione campi di output?
069700130118         IF  Not $Same£1;
069800130118
069900130118           If  %trimr( wSQL_ds.wSQLke1 ) <> Save0KSC;
070000130118             s_CodKsc = %trimr( wSQL_ds.wSQLke1 );
070100130118             s_DesKsc = %subst( wSaveRAG : 1 : %len(s_DesKsc) );
070200130118             Save0KSC = %trimr( wSQL_ds.wSQLke1 );
070300130118           Else;
070400130118             s_CodKsc = '   "   ';
070500130118           EndIf;
070600130118           s_LnP    = %trimr( wSQL_ds.wSQLke2 );
070700130118           If  d3NN.§3NNcc > *zero;
070800130118             if  d3NN.§3NNcc <> SaveCC;
070900130118               s_CodCC  = %editc( d3NN.§3NNcc : 'X' );
071000130118               s_DesCC  = wSaveDesCC;
071100130118             else;
071200130118               s_CodCC  = '   "   ';
071300130118             endif;
071400130118           EndIf;
071500130118           if  Not  %eof(TITAS33C);
071600130118             wDate_Eur = %date( TASaas * 10000 + TASmgs : *iso );
071700130118             s_DtUltS  = %dec( wDate_Eur );
071800130118           endif;
071900130118
072000130118         ELSE;
072100130118
072200130118           If  %trimr( wSQL_ds.wSQLke1 ) <> Save1KSC;
072300130118             s1_CodKsc = %trimr( wSQL_ds.wSQLke1 );
072400130118             s1_DesKsc = %subst( wSaveRAG : 1 : %len(s_DesKsc) );
072500130118             Save1KSC  = %trimr( wSQL_ds.wSQLke1 );
072600130118           Else;
072700130118             s1_CodKsc = '   "   ';
072800130118           EndIf;
072900130118           s1_LnP    = %trimr( wSQL_ds.wSQLke2 );
073000130118
073100130118         ENDIF;
073200130116
073300130116         // -?Stampa riga di dettaglio?
073400130118         IF  Not $Same£1;
073500130118           //if  SaveFIL <> %subst( SaveKSC : 1 : 3 );
073600130118           //  except  PrtSPACE;
073700130118           //endif;
073800130118           if  *inOF;
073900130118             //if  SaveFIL <> %subst( SaveKSC : 1 : 3 );
074000130118             //  except  PrtSPACE;
074100130118             //endif;
074200130118             except  PrtTXT;
074300130118             *inOF = *off;
074400130118           endif;
074500130118           except  PrtDET;
074600130118           s_TotCli += 1;
074700130118         ELSE;
074800130118           if  *inOA;
074900130118             except  Prt1TXT;
075000130118             *inOA = *off;
075100130118           endif;
075200130118           except  Prt1DET;
075300130118           s1_TotCli += 1;
075400130118         ENDIF;
075500130118
075600130116         SaveKSC = %trimr( wSQL_ds.wSQLke1 );
075700130117         SaveCC  = d3NN.§3NNcc;
075800130118         //SaveFIL = %subst( SaveKSC : 1 : 3 );
075900130116
076000130116         // -?Incremento numero clienti stampati?
076100130116         VD3ncl   += 1;
076200130116
076300130116       ENDSR;
076400130116
076500130116       //--------------------------------------------------------------
076600130116       //?Stampa segnalazione errore.                                  ?
076700130116       //--------------------------------------------------------------
076800130116       BEGSR  sr_PrintErr;
076900130116
077000130116         // -?Stampa del Dump?
077100130116         Dump(A);
077200130116
077300130116         // -?Stampa del Job-Log?
077400130116         Qcmd = 'DSPJOBLOG job(*) output(*print)';
077500130116         exsr  sr_ExecCmd;
077600130116
077700130116         // -?Stampa segnalazione nel file di stampa?
077800130116         except  PrtERR;
077900130116         except  PrtEND;
078000130116         close  QSYSPRT;
078100130116
078200130116       ENDSR;
078300130116
078400130116       //--------------------------------------------------------------
078500130116       //?Esecuzione del comando (già impostato).                      ?
078600130116       //--------------------------------------------------------------
078700130116       BEGSR  sr_ExecCmd;
078800130116
078900130116         clear Qcap0100;
079000130116         Qcabcsdh = *off;
079100130116         Qcapa    = *off;
079200130116         Qcacmdss = *off;
079300130116         Qcaerved = *allX'00';
079400130116
079500130116         clear Qusec;
079600130116         Qusbprv  = %size(Qusec);
079700130116
079800130116         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
079900130116                           %size(Qcap0100) : 'CPOP0100' : *omit :
080000130116                           0 : 0 : Qusec);
080100130116
080200130116         //if  Qusei <> *blank;
080300130116         //  ...;
080400130116         //endif;
080500130116
080600130116       ENDSR;
080700130116
080800130116       //--------------------------------------------------------------
080900130116       //?Operazioni finali.                                           ?
081000130116       //--------------------------------------------------------------
081100130116       BEGSR  sr_RoutEnd;
081200130116
081300130116         // -?Chiusura applicazioni precedentemente aperte?
081400130116         reset  TIBS69ds;
081500130116         tibs69r( tibs69ds :
081600130116                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
081700130116
081800130116         // -?Chiusura pgm?
081900130116         return;
082000130116
082100130116       ENDSR;
082200130116
082300130116      /end-free
082400130116
082500130116       //--------------------------------------------------------------
082600130116       //?Definizione file di stampa in output.                        ?
082700130116       //--------------------------------------------------------------
082800130116
082900130116     oQSYSPRT   e            PrtTXT            3
083000130116     o                       RsUt
083100130116     o                                         +  3 'CLIENTI IN TAB. "-
083200130116     o                                              3NN", MA SENZA SPE-
083300130116     o                                              DIZIONI'
083400130116     o                       VT1pgm            + 10
083500130116     o                       *date         y   +  5
083600130116     o                                         +  5 'Pag.'
083700130116     o                       Page          z   +  0
083800130116     o          e            PrtTXT      0
083900130116     o                                         + 23 'CLIENTI IN TAB. "-
084000130116     o                                              3NN", MA SENZA SPE-
084100130116     o                                              DIZIONI'
084200130116     o          e            PrtTXT      1
084300130116     o                                         + 23 'NEGLI ULTIMI'
084400130116     o                       VD1ggl        1   +  1
084500130116     o                                         +  1 'GIORNI (DAL'
084600130116     o                       VD1dtl        y   +  1
084700130116     o                                         +  0 ')'
084800130116     o          e            PrtTXT      0
084900130116     o                                         + 23 'NEGLI ULTIMI'
085000130116     o                       VD1ggl        1   +  1
085100130116     o                                         +  1 'GIORNI (DAL'
085200130116     o                       VD1dtl        y   +  1
085300130116     o                                         +  0 ')'
085400130116     o          e            PrtTXT      2
085500130116     o                       sk_Txt(1)
085600130116     o                       sK_Txt(4)         +  0
085700130116     o          e            PrtTXT      1
085800130116     o                       sk_Txt(2)
085900130116     o                       sk_Txt(5)         +  0
086000130116      *
086100130116     o          e            PrtERR      2
086200130116     o                                              'RILEVATO ERRORE: -
086300130116     o                                              consultare le stam-
086400130116     o                                              pe del DUMP e del -
086500130116     o                                              JOBLOG.'
086600130116      *
086700130116     o*//       e            PrtSPACE    1
086800130116      *
086900130116     o          e            PrtDET      1
087000130116     o                       s_CodKsc
087100130116     o                       s_DesKsc          +  1
087200130116     o                       s_LnP             +  2
087300130116     o                       s_CodCC           +  2
087400130116     o                       s_DesCC           +  1
087500130116     o                       s_DtUltS          +  2 '  /  /    '
087600130116      *
087700130116     o          e            PrtEND      2
087800130116     o                                         + 24 '***  Fine Stampa  ***'
087900130116     o                                         +  3 'Stampati'
088000130116     o                       s_TotCli      1   +  1
088100130116     o                                         +  1 'clienti/filiali.'
088200130118
088300130118       //--------------------------------------------------------------
088400130118
088500130118     oQSYSPRT1  e            Prt1TXT           3
088600130118     o                       RsUt
088700130118     o                                         +  3 'CLIENTI IN TAB. "-
088800130118     o                                              3NN" PER FILIALE I-
088900130118     o                                              N "£1" '
089000130118     o                       VT1pgm            + 10
089100130118     o                       *date         y   +  5
089200130118     o                                         +  5 'Pag.'
089300130118     o                       Page1         z   +  0
089400130118     o          e            Prt1TXT     0
089500130118     o                                         + 23 'CLIENTI IN TAB. "-
089600130118     o                                              3NN" PER FILIALE I-
089700130118     o                                              N "£1" '
089800130118     o          e            Prt1TXT     1
089900130118     o                                         + 23 '(SENZA MODIFICHE -
090000130118     o                                              NELL''ULTIMO ANNO)'
090100130118     o          e            Prt1TXT     0
090200130118     o                                         + 23 '(SENZA MODIFICHE -
090300130118     o                                              NELL''ULTIMO ANNO)'
090400130118     o          e            Prt1TXT     2
090500130118     o                       s1_Txt1
090600130118     o          e            Prt1TXT     1
090700130118     o                       s1_Txt2
090800130118      *
090900130118     o          e            PrtERR      2
091000130118     o                                              'RILEVATO ERRORE: -
091100130118     o                                              consultare le stam-
091200130118     o                                              pe del DUMP e del -
091300130118     o                                              JOBLOG.'
091400130118      *
091500130118     o*//       e            Prt1SPACE   1
091600130118      *
091700130118     o          e            Prt1DET     1
091800130118     o                       s1_CodKsc
091900130118     o                       s1_DesKsc         +  1
092000130118     o                       s1_LnP            +  2
092100130118      *
092200130118     o          e            Prt1END     2
092300130118     o                                         + 24 '***  Fine Stampa  ***'
092400130118     o                                         +  3 'Stampati'
092500130118     o                       s1_TotCli     1   +  1
092600130118     o                                         +  1 'clienti/filiali.'
092700130116
092800130116       //--------------------------------------------------------------
092900130116       //?Definizione schiere a tempo di compilazione                  ?
093000130116       //--------------------------------------------------------------
093100130116
093200130116** -?sk_Txt:?Testata di stampa?...+....4....+....5....+....6....+*
093300130116Cliente KE1                                  LNP  Nuovo codice       1
093400130116-------------------------------------------  ---  ----------------   2
0935001301161234567 *...+....1....+....2....+....3....+  123  1234567 *...+...   3
093600130116                             Dt.UltSped                              4
093700130116---------------------------  ----------                              5
093800130116.1....+....2....+....3....+  12/34/5678                              6
093900130116** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
094000130116Il numero di giorni DEVE essere maggiore di 0 (zero)                            1
