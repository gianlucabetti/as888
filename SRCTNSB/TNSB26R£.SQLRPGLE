000100130103       //==============================================================
000200130103       //?Elenco clienti CON ritorno immagini, ma SENZA spedizioni.    ?
000300130103       //==============================================================
000400130103
000500130103       //--------------------------------------------------------------
000600130103       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700130103       //--------------------------------------------------------------
000800130103
000900130103     /*PRM  dbgview(*source)
001000130103     /*END
001100130103
001200130103       //--------------------------------------------------------------
001300130103       //?Specifiche di controllo.                                     ?
001400130103       //--------------------------------------------------------------
001500130103
001600130103     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700130103     h dftactgrp(*no)
001800130103     h alwnull(*inputonly)
001900130103
002000130103       //--------------------------------------------------------------
002100130103       //?Dichiarazione file.                                          ?
002200130103       //--------------------------------------------------------------
002300130103
002400130103       // -?Tabelle "LAC"/"JDC"?
002500130103     f*//TNTBE01L  if   e           k disk
002600130103
002700130103       // -?Bolle Sede
002800130103     fTITAS33C  if   e           k disk
002900130103
003000130103       // -?Video
003100130103     fTNSB26D   cf   e             workstn
003200130103     f                                     indds(IndDspF)
003300130103     f                                     infds(InfDspF)
003400130103
003500130103       // -?File di Stampa?
003600130103     fQSYSPRT   o    f  132        printer  oflind(*inOF)  usropn
003700130103
003800130103       //--------------------------------------------------------------
003900130103       //?Definizione costanti.                                        ?
004000130103       //--------------------------------------------------------------
004100130104
004200130104       // -?Comando di override al PrtF?
004300130104     d c_CmdOvrPrtF    c                   const('OVRPRTF +
004400130104     d                                           file(QSYSPRT) +
004500130104     d                                           pagesize(66 132) +
004600130104     d                                           lpi(6) cpi(10) +
004700130104     d                                           usrdta(''LAC_NoSped'') +
004800130104     d                                           ovrscope(*actgrpdfn) +
004900130104     d                                           ')
005000130104
005100130104       // -?Comando di cellazione override?
005200130104     d c_CmdDltOvr     c                   const('DLTOVR +
005300130104     d                                            file(QSYSPRT) +
005400130104     d                                            lvl(*actgrpdfn)')
005500130103
005600130103       // -?Tasti funzionali a video?
005700130103     d c_F01           c                   const(x'31')
005800130103     d c_F02           c                   const(x'32')
005900130103     d c_F03           c                   const(x'33')
006000130103     d c_F04           c                   const(x'34')
006100130103     d c_F05           c                   const(x'35')
006200130103     d c_F06           c                   const(x'36')
006300130103     d c_F07           c                   const(x'37')
006400130103     d c_F08           c                   const(x'38')
006500130103     d c_F09           c                   const(x'39')
006600130103     d c_F10           c                   const(x'3A')
006700130103     d c_F11           c                   const(x'3B')
006800130103     d c_F12           c                   const(x'3C')
006900130103     d c_F13           c                   const(x'B1')
007000130103     d c_F14           c                   const(x'B2')
007100130103     d c_F15           c                   const(x'B3')
007200130103     d c_F16           c                   const(x'B4')
007300130103     d c_F17           c                   const(x'B5')
007400130103     d c_F18           c                   const(x'B6')
007500130103     d c_F19           c                   const(x'B7')
007600130103     d c_F20           c                   const(x'B8')
007700130103     d c_F21           c                   const(x'B9')
007800130103     d c_F22           c                   const(x'BA')
007900130103     d c_F23           c                   const(x'BB')
008000130103     d c_F24           c                   const(x'BC')
008100130103     d c_Enter         c                   const(x'F1')
008200130103     d c_RollDown      c                   const(x'F4')
008300130103     d c_RollUp        c                   const(x'F5')
008400130103
008500130103       // -?Costante per controllo "caratteri solo numerici"?
008600130103     d c_Digits        c                   const('0123456789')
008700130103
008800130104       //--------------------------------------------------------------
008900130103       //?Definizione schiere.                                         ?
009000130104       //--------------------------------------------------------------
009100130104
009200130104       // -?Testate in stampa?
009300130104     d sk_Txt          s             66    dim( 6)  ctdata  perrcd( 1)
009400130104
009500130104       // -?Tipi elaborazione (A=FTP, N=No, S=CD)?
009600130104     d sk_Aut          s              1    dim( 3)  ctdata  perrcd( 1)
009700130104     d sk_AutD         s             15    dim( 3)  alt(sk_Aut)
009800130103
009900130103       // -?Messaggi di errore?
010000130104     d sk_Msg          s             78    dim( 1)  ctdata  perrcd( 1)
010100130103
010200130103       //--------------------------------------------------------------
010300130103       //?Definizione aree dati.                                       ?
010400130103       //--------------------------------------------------------------
010500130103
010600130103       // -?Dati utente?
010700130103     d §AzUte        e ds                  extname(AZUTE00F)
010800130103     d                                     dtaara
010900130103     d §DatiUte      e ds                  extname(dDatiUte)
011000130103     d                                     dtaara
011100130103
011200130103       //--------------------------------------------------------------
011300130103       //?Definizione strutture dati.                                  ?
011400130103       //--------------------------------------------------------------
011500130103
011600130103       // -?Status ds?
011700130103     d Status         sds
011800130116     d   SDSpgm          *proc
011900130103
012000130103       // -?InfDS?
012100130103     d InfDspF         ds
012200130103     d   dsp_aid             369    369a
012300130103
012400130103       // -?Indicatori su DspF?
012500130103     d IndDspF         ds                  inz
012600130103         // -?Emissione messaggio di errore?
012700130103     d   ErrMessage                    n   overlay(IndDspF : 28)
012800130103         // -?Posizionamento cursore & segnalazione errore?
012900130103     d   PosCurGGL                     n   overlay(IndDspF : 51)
013000130103         //   -?Riemissione videata?
013100130103     d   ErrGenerico                   n   overlay(IndDspF : 99)
013200130104
013300130104       // -?Dati estratti via SQL?
013400130104     d TNTBE00F      e ds                  based(nullPtr)
013500130104     d                                     prefix(ds_)
013600130104     d wSQL_ds         ds                  inz  qualified
013700130104     d   wSQLcod                           inz  like(ds_TBEcod)
013800130107     d   wSQLksu                      7a   inz
013900130104     d   wSQLke1                           inz  like(ds_TBEke1)
014000130104     d   wSQLuni                           inz  like(ds_TBEuni)
014100130103
014200130103       // -?Parametri ricevuti?
014300130103     d KPJBA         e ds
014400130103
014500130103       // -?Tabelle usate:?
014600130103       // ·?"LAC" = Clienti per Ritorno Immagini?
014700130103     d dLAC          e ds                  inz  qualified
014800130103       // ·?"JDC" = Clienti per Ritorno Immagini "DOC"?
014900130103     d dJDC          e ds                  inz  qualified
015000130103
015100130103       //--------------------------------------------------------------
015200130103       //?Definizione variabili globali.                               ?
015300130103       //--------------------------------------------------------------
015400130103
015500130103       // -?Flags booleani?
015600130103     d $Fine           s               n   inz
015700130103     d $InzD01         s               n   inz(*on)
015800130104     d $InzD02         s               n   inz(*on)
015900130107     d $SameUnif       s               n   inz
016000130104
016100130104       // -?Indici di schiera?
016200130104     d xx              s              3  0  inz
016300130103
016400130103       // -?Variabili per la gestione del video?
016500130103     d $Video          s              2    inz('D1')
016600130104
016700130104       // -?Stringa SQL da eseguire?
016800130104     d wSQL            s           2048    Varying        inz
016900130104
017000130104       // -?Campi di stampa?
017100130107     d s_Tab           s              3    inz
017200130107     d s_TotCli        s                   inz  like(VD3ncl)
017300130104     d PrtDET_ds       ds                  inz
017400130107     d   s_CodKsu                          inz  like(s_CodCli)
017500130107     d   s_DesKsu                    35    inz
017600130104     d   s_CodCli                     7    inz
017700130104     d   s_DesCli                          inz  like(dLAC.§LACrag)
017800130104     d   s_LACaut                          inz  like(dLAC.§LACaut)
017900130104     d   s_DesAut                    15    inz
018000130104     d   s_Merge                           inz  like(dLAC.§LACmerge)
018100130104     d   s_LA2                             inz  like(dLAC.§LACla2)
018200130104     d   s_DtUltS                     8  0 inz
018300130104
018400130104       // -?Campi per controllo rotture di livello?
018500130107     d SaveFIL         s              3    inz
018600130103
018700130103       // -?Campi di comodo?
018800130104     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
018900130104     d wDate_ISO       s               d   datfmt(*iso)   inz(*loval)
019000130104     d wDate           s              8  0 inz
019100130104     d WD1dtl          s                   inz  like(VD1dtl)
019200130103
019300130103       //--------------------------------------------------------------
019400130103       //?Definizione prototipi procedure.                             ?
019500130103       //--------------------------------------------------------------
019600130103
019700130103       // -?Reperimento dati utente?
019800130103     d TIBS34ds      e ds
019900130103      /copy gaitrasrc/srcProtoPR,TIBS34R
020000130103
020100130103       // -?Controllo/Decodifica cliente?
020200130103      /copy gaitrasrc/srcProtoPI,TIBS69R
020300130103      /copy gaitrasrc/srcProtoPR,TIBS69R
020400130104
020500130104       // -?Parametri API QCAPCMD (Process Commands)?
020600130104     d Qcmd            s           2048    inz  varying
020700130104      /copy qSysInc/qRpgleSrc,QCAPCMD
020800130104       // -?API QCAPCMD (Process Commands)?
020900130104      /copy gaitrasrc/srcProtoPR,QCAPCMD
021000130104
021100130104       // -?Parametri gestione errori API.?
021200130104      /copy qsysinc/qrpglesrc,QUSEC
021300130103
021400130103       //--------------------------------------------------------------
021500130103       //?Definizione key-list.                                        ?
021600130103       //--------------------------------------------------------------
021700130103
021800130103       // -?File TITAS33C?
021900130103     d k03titas33    e ds                  extname(TITAS33C : *key)
022000130103     d                                     prefix(k_)   inz
022100130103
022200130103       //--------------------------------------------------------------
022300130103       //?M A I N - L I N E                                            ?
022400130103       //--------------------------------------------------------------
022500130103
022600130103     c     *Entry        plist
022700130103     c                   parm                    KPJBA
022800130103
022900130103      /free
023000130103
023100130103       // -?Operazioni iniziali?
023200130103       exsr  sr_RoutInz;
023300130103
023400130103       // -?Ciclo di gestione del file video?
023500130103       DOW  $Fine = *off;
023600130103
023700130103         select;
023800130103
023900130103           // -?Filtro di lancio?
024000130103           when $Video = 'D1';
024100130103             exsr  sr_GesD01;
024200130103
024300130103           // -?Visualizzazione risultato?
024400130103           when $Video = 'D2';
024500130104             exsr  sr_GesD023;
024600130103
024700130103           // -?? ? ??
024800130103           other;
024900130103             $Fine = *on;
025000130103
025100130103         endsl;
025200130103
025300130103       ENDDO;
025400130103
025500130103       // -?Operazioni finali?
025600130103       exsr  sr_RoutEnd;
025700130103
025800130103       //--------------------------------------------------------------
025900130103       //?Operazioni iniziali.                                         ?
026000130103       //--------------------------------------------------------------
026100130103       BEGSR  sr_RoutInz;
026200130103
026300130103         *inLR = *on;
026400130103
026500130103         // -?Reperimento dati job?
026600130103         exsr  sr_DatiJob;
026700130103
026800130103         // -?Impostazione nome programma a video?
026900130103         VT1pgm = SDSpgm;
027000130103
027100130103         // -?Reperimento data odierna?
027200130103         wDate = %int( %subst( %char( %dec( %timestamp() ) )
027300130103                               : 1 : 8 ) );
027400130104
027500130116         // -?Override al printer-file?
027600130104         Qcmd = c_CmdOvrPrtF;
027700130104         exsr  sr_ExecCmd;
027800130104         //if  Qusei <> *blank;
027900130104         //  ...;
028000130104         //endif;
028100130103
028200130103       ENDSR;
028300130103
028400130103       //--------------------------------------------------------------
028500130103       //?Reperimento Dati del job (Utente/Operativi).                 ?
028600130103       //--------------------------------------------------------------
028700130103       BEGSR  sr_DatiJob;
028800130103
028900130103         in(E) §AzUte;
029000130103         if NOT %error;
029100130103           in(E) §DatiUte;
029200130103         endif;
029300130103         if %error or RSut = *blanks;
029400130103           clear TIBS34ds;
029500130103           tibs34r ( tibs34ds );
029600130103           in §AzUte;
029700130103           in §DatiUte;
029800130103         endif;
029900130103
030000130103       ENDSR;
030100130103
030200130103       //--------------------------------------------------------------
030300130103       //?Gestione videata D01.                                        ?
030400130103       //--------------------------------------------------------------
030500130103       BEGSR  sr_GesD01;
030600130103
030700130103         // -?Inizializzazione videata?
030800130103         if  $InzD01 = *on;
030900130103           exsr  sr_InzD01;
031000130103           $InzD01 = *off;
031100130103         endif;
031200130103
031300130103         // -?Emissione videata?
031400130103         write  SB26T01;
031500130107         write  SB26P01;
031600130103         exfmt  SB26D01;
031700130103
031800130104         clear  VIDmsg;
031900130103         reset  ErrMessage;
032000130103         reset  ErrGenerico;
032100130103
032200130103         SELECT;
032300130103
032400130103           // -?F3=Fine?
032500130103           WHEN  dsp_aid = c_F03;
032600130103             $Fine = *on;
032700130103
032800130103           // -?Invio, F6=Conferma?
032900130103           OTHER;
033000130103             exsr  sr_CtrD01;
033100130103             if  ErrGenerico = *off  and  dsp_aid = c_F06;
033200130103               $Video    = 'D2';
033300130103               $InzD02   = *on;
033400130103             endif;
033500130103
033600130103         ENDSL;
033700130103
033800130103       ENDSR;
033900130103
034000130103       //--------------------------------------------------------------
034100130103       //?Inizializzazione videata D01.                                ?
034200130103       //--------------------------------------------------------------
034300130103       BEGSR  sr_InzD01;
034400130103
034500130103         clear  SB26D01;
034600130103
034700130103         VD1ggl = 120;
034800130104
034900130104         wDate_Eur = %date( wDate : *iso ) - %days( VD1ggl );
035000130104         wDate_Iso = wDate_Eur;
035100130104         VD1dtl = %dec( wDate_Eur );
035200130104         WD1dtl = %dec( wDate_Iso );
035300130103
035400130103       ENDSR;
035500130103
035600130103       //--------------------------------------------------------------
035700130103       //?Controllo dati immessi nella videata D01.                    ?
035800130103       //--------------------------------------------------------------
035900130103       BEGSR  sr_CtrD01;
036000130103
036100130103         // -?Spegnimento indicatori di posizionamento cursore?
036200130103         %subst(IndDspF : 28) = *off;
036300130103
036400130103         // -?Numero giorni inferiori a 1?
036500130103         if  VD1ggl <= 1;
036600130104           VIDmsg = sk_Msg(01);
036700130103           PosCurGGL   = *on;
036800130103           ErrMessage  = *on;
036900130103           ErrGenerico = *on;
037000130103           leavesr;
037100130103         endif;
037200130103
037300130104         wDate_Eur = %date( wDate : *iso ) - %days( VD1ggl );
037400130104         wDate_Iso = wDate_Eur;
037500130104         VD1dtl = %dec( wDate_Eur );
037600130104         WD1dtl = %dec( wDate_Iso );
037700130103
037800130103       ENDSR;
037900130103
038000130103       //--------------------------------------------------------------
038100130104       //?Gestione videate D02/D03.                                    ?
038200130103       //--------------------------------------------------------------
038300130104       BEGSR  sr_GesD023;
038400130103
038500130103         // -?Inizializzazione videata?
038600130104         if  $InzD02 = *on;
038700130104           exsr  sr_InzD023;
038800130103           $InzD02 = *off;
038900130103         endif;
039000130103
039100130103         // -?Emissione videata?
039200130103         write  SB26T01;
039300130104         write  SB26D01;
039400130104         write  Protect;
039500130104         exfmt  SB26D03;
039600130103
039700130104         clear  VIDmsg;
039800130103         reset  ErrMessage;
039900130103         reset  ErrGenerico;
040000130103
040100130103         SELECT;
040200130103
040300130103           // -?F3=Fine?
040400130103           WHEN  dsp_aid = c_F03;
040500130103             $Fine = *on;
040600130103
040700130103           // -?F12=Ritorno?
040800130103           WHEN  dsp_aid = c_F12;
040900130104             $Video = 'D1';
041000130103
041100130103         ENDSL;
041200130103
041300130103       ENDSR;
041400130103
041500130103       //--------------------------------------------------------------
041600130103       //?Inizializzazione videata D02.                                ?
041700130103       //--------------------------------------------------------------
041800130104       BEGSR  sr_InzD023;
041900130103
042000130103         // -?Spegnimento degli indicatori di errore?
042100130103         %subst(IndDspF : 50) = *off;
042200130103
042300130103         // -?Emissione videata di avviso dei lavori in corso?
042400130103         write  SB26T01;
042500130103         write  SB26D01;
042600130104         write  Protect;
042700130103         write  SB26D02;
042800130103
042900130103         // -?Pulizia videata?
043000130103         clear  SB26D03;
043100130103
043200130103         // -?Estrazione e stampa dei clienti?
043300130104         exsr  sr_ReadTab;
043400130103
043500130103       ENDSR;
043600130103
043700130103       //--------------------------------------------------------------
043800130103       //?Estrazione dati da tab. "LAC" e "JDC" e dal file TITAS.      ?
043900130103       //--------------------------------------------------------------
044000130104       BEGSR  sr_ReadTab;
044100130104
044200130104         // -?Apertura printer-file?
044300130104         open  QSYSPRT;
044400130107         except  PrtTXT1;
044500130103
044600130103         // -?Preparazione SQL?
044700130107         wSQL = 'select TBEcod, substr(TBEuni, 156, 7), TBEke1, TBEuni +
044800130103                   from TNTBE00F +
044900130104                  where TBEcod in (''JDC'', ''LAC'') +
045000130103                    and TBEatb = '' '' +
045100130107                  order by TBEcod desc, substr(TBEuni, 156, 7), TBEke1 +
045200130103                    for fetch only';
045300130103
045400130104         exec sql   prepare S1   from :wSQL;
045500130103
045600130103         // -?Dichiarazione cursore SQL?
045700130103         exec sql   declare C1   cursor for S1;
045800130103
045900130103         // -?Apertura cursore SQL?
046000130103         exec sql   open C1;
046100130104
046200130104         // -?Rilevato errore SQL?
046300130104         if  SQLcode < *zero;
046400130104           exsr  sr_PrintErr;
046500130104           exsr  sr_RoutEnd;
046600130104         endif;
046700130103
046800130103         // -?Ciclo esecuzione SQL?
046900130104         exec sql   fetch next   from C1   into :wSQL_ds;
047000130103
047100130103         DoW  SQLcode <> 100;
047200130103
047300130103           // ·?Rilevato errore SQL?
047400130103           if  SQLcode < *zero;
047500130103             exsr  sr_PrintErr;
047600130103             exsr  sr_RoutEnd;
047700130103           endif;
047800130103
047900130103           // ·?Estrazione ultima spedizione da TITAS?
048000130104           exsr  sr_CtrlSped;
048100130103
048200130103           // ·?Lettura cliente successivo?
048300130104           exec sql   fetch next   from C1   into :wSQL_ds;
048400130103
048500130103         EndDo;
048600130103
048700130103         // -?Chiusura cursore SQL?
048800130103         exec SQL   close C1;
048900130104
049000130104         // -?Chiusura printer-file?
049100130107         if  s_Tab <> 'JDC';
049200130107           if  s_Tab = *blank;
049300130107             s_Tab   = 'LAC';
049400130107             except  PrtTAB;
049500130107             except  PrtTXT2;
049600130107             except  PrtEND;
049700130107           else;
049800130107             except  PrtEND;
049900130107             clear  s_TotCli;
050000130107           endif;
050100130107           s_Tab    = 'JDC';
050200130107           except  PrtTXT1;
050300130107           except  PrtTAB;
050400130107           except  PrtTXT2;
050500130107         endif;
050600130104         except  PrtEND;
050700130104         close  QSYSPRT;
050800130103
050900130103       ENDSR;
051000130103
051100130103       //--------------------------------------------------------------
051200130103       //?Controllo ultima spedizione su TITAS.                        ?
051300130103       //--------------------------------------------------------------
051400130104       BEGSR  sr_CtrlSped;
051500130103
051600130103         clear  k03titas33;
051700130104         k_TASccm = %int( %trim( wSQL_ds.wSQLke1 ) );
051800130104         //k_TASaas = WD1dtl / 10000;
051900130104         //k_TASmgs = WD1dtl - (k_tasaas * 10000);
052000130104         //setll  %kds( k03titas33 )  TITAS33C;
052100130104         //reade  %kds( k03titas33 : 1 )  TITAS33C;
052200130104         setgt  %kds( k03titas33 : 1 )  TITAS33C;
052300130104         readpe  %kds( k03titas33 : 1 )  TITAS33C;
052400130103
052500130104         // -?Stampa cliente in esame SE non trovate spedizioni?
052600130104         //  ?negli ultimi 120 gg. (inseriti a video)?
052700130104         //if  %eof(TITAS33C);
052800130104         if  %eof(TITAS33C)  or
052900130104             (TASaas * 10000 + TASmgs) < WD1dtl;
053000130104           exsr  sr_PrintCli;
053100130103         endif;
053200130103
053300130103       ENDSR;
053400130103
053500130103       //--------------------------------------------------------------
053600130103       //?Stampa cliente in tab. "LAC"/"JDC" senza spedizioni.         ?
053700130103       //--------------------------------------------------------------
053800130104       BEGSR  sr_PrintCli;
053900130107
054000130107         if  s_Tab <> wSQL_ds.wSQLcod;
054100130107           if  s_Tab <> *blank;
054200130107             except  PrtEND;
054300130107             clear  s_TotCli;
054400130107           endif;
054500130107           s_Tab    = wSQL_ds.wSQLcod;
054600130107           except  PrtTAB;
054700130107           except  PrtTXT2;
054800130107         endif;
054900130107
055000130107         clear  PrtDET_ds;
055100130103
055200130104         // -?Impostazione dati della tabella reperita?
055300130104         clear  dLAC;
055400130104         clear  dJDC;
055500130104         select;
055600130104           when  wSQL_ds.wSQLcod = 'LAC';
055700130104             dLAC = wSQL_ds.wSQLuni;
055800130104           when  wSQL_ds.wSQLcod = 'JDC';
055900130104             dJDC = wSQL_ds.wSQLuni;
056000130104         endsl;
056100130107
056200130107         // -?Verifica se stesso unificante del cliente appena?
056300130107         //  ?elaborato prima di decodificarlo?
056400130107         $SameUnif = ( (wSQL_ds.wSQLcod = 'LAC'  and
056500130107                        I69kac = dLAC.§LACksu)   or
056600130107                       (wSQL_ds.wSQLcod = 'JDC'  and
056700130107                        I69kac = dJDC.§JDCksu));
056800130104
056900130104         // -?Decodifica cliente raggruppamento?
057000130104         If  Not  $SameUnif;
057100130104           clear  TIBS69ds;
057200130104           I69sif = knsif;
057300130104           I69kcc = DUTkci;
057400130104           select;
057500130104             when  wSQL_ds.wSQLcod = 'LAC';
057600130104               I69kac = dLAC.§LACksu;
057700130104             when  wSQL_ds.wSQLcod = 'JDC';
057800130104               I69kac = dJDC.§JDCksu;
057900130104           endsl;
058000130104           clear  ds_CNACO;
058100130104           tibs69r( tibs69ds :
058200130104                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
058300130104           if  O69err = *on;
058400130104             ACOrag = *all'? ';
058500130104           endif;
058600130104         EndIf;
058700130104
058800130104         // -?Impostazione campi di output?
058900130107         if  Not  $SameUnif;
059000130107           s_DesKsu = %subst( ACOrag : 1 : %len(s_DesKsu) );
059100130107         endif;
059200130107
059300130107         s_CodCli = %trim( wSQL_ds.wSQLke1 );
059400130104
059500130104         select;
059600130104           when  wSQL_ds.wSQLcod = 'LAC';
059700130104             if  Not  $SameUnif;
059800130104               s_CodKsu = %editc( dLAC.§LACksu : 'X' );
059900130107             else;
060000130107               s_CodKsu = '   "   ';
060100130104             endif;
060200130107             s_DesCli = dLAC.§LACrag;
060300130104             s_LACaut = dLAC.§LACaut;
060400130104             s_Merge  = dLAC.§LACmerge;
060500130104             s_LA2    = dLAC.§LACla2;
060600130104           when  wSQL_ds.wSQLcod = 'JDC';
060700130104             if  Not  $SameUnif;
060800130104               s_CodKsu = %editc( dJDC.§JDCksu : 'X' );
060900130107             else;
061000130107               s_CodKsu = '   "   ';
061100130104             endif;
061200130107             s_DesCli = dJDC.§JDCrag;
061300130104             s_LACaut = dJDC.§JDCaut;
061400130104             s_Merge  = dJDC.§JDCmerge;
061500130104         endsl;
061600130104
061700130104         xx = %lookup( s_LACaut : sk_Aut );
061800130104         if  xx > *zero;
061900130104           s_DesAut = sk_AutD(xx);
062000130104         else;
062100130104           s_DesAut = *all'? ';
062200130104         endif;
062300130104
062400130104         if  Not  %eof(TITAS33C);
062500130104           wDate_Eur = %date( TASaas * 10000 + TASmgs : *iso );
062600130104           s_DtUltS  = %dec( wDate_Eur );
062700130104         endif;
062800130104
062900130104         // -?Stampa riga di dettaglio?
063000130107         //if  SaveFIL <> %subst(wSQL_ds.wSQLksu : 1 : 3);
063100130107         //  except  PrtSPACE;
063200130107         //endif;
063300130104         if  *inOF;
063400130107           except  PrtTXT1;
063500130107           except  PrtTAB;
063600130107           except  PrtTXT2;
063700130107           //if  SaveFIL <> %subst(wSQL_ds.wSQLksu : 1 : 3);
063800130107           //  except  PrtSPACE;
063900130107           //endif;
064000130104           *inOF = *off;
064100130104         endif;
064200130104         except  PrtDET;
064300130107         SaveFIL = %subst(wSQL_ds.wSQLksu : 1 : 3);
064400130104
064500130104         // -?Incremento numero clienti stampati?
064600130107         VD3ncl   += 1;
064700130107         s_TotCli += 1;
064800130103
064900130103       ENDSR;
065000130104
065100130104       //--------------------------------------------------------------
065200130104       //?Stampa segnalazione errore.                                  ?
065300130104       //--------------------------------------------------------------
065400130104       BEGSR  sr_PrintErr;
065500130104
065600130104         // -?Stampa del Dump?
065700130104         Dump(A);
065800130104
065900130104         // -?Stampa del Job-Log?
066000130104         Qcmd = 'DSPJOBLOG job(*) output(*print)';
066100130104         exsr  sr_ExecCmd;
066200130104
066300130104         // -?Stampa segnalazione nel file di stampa?
066400130104         except  PrtERR;
066500130104         except  PrtEND;
066600130104         close  QSYSPRT;
066700130104
066800130104       ENDSR;
066900130104
067000130104       //--------------------------------------------------------------
067100130104       //?Esecuzione del comando (già impostato).                      ?
067200130104       //--------------------------------------------------------------
067300130104       BEGSR  sr_ExecCmd;
067400130104
067500130104         clear Qcap0100;
067600130104         Qcabcsdh = *off;
067700130104         Qcapa    = *off;
067800130104         Qcacmdss = *off;
067900130104         Qcaerved = *allX'00';
068000130104
068100130104         clear Qusec;
068200130104         Qusbprv  = %size(Qusec);
068300130104
068400130104         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
068500130104                           %size(Qcap0100) : 'CPOP0100' : *omit :
068600130104                           0 : 0 : Qusec);
068700130104
068800130104         //if  Qusei <> *blank;
068900130104         //  ...;
069000130104         //endif;
069100130104
069200130104       ENDSR;
069300130103
069400130103       //--------------------------------------------------------------
069500130103       //?Operazioni finali.                                           ?
069600130103       //--------------------------------------------------------------
069700130103       BEGSR  sr_RoutEnd;
069800130103
069900130104         // -?Chiusura applicazioni precedentemente aperte?
070000130104         reset  TIBS69ds;
070100130104         tibs69r( tibs69ds :
070200130104                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
070300130104
070400130103         // -?Chiusura pgm?
070500130103         return;
070600130103
070700130103       ENDSR;
070800130103
070900130103      /end-free
071000130104
071100130104       //--------------------------------------------------------------
071200130104       //?Definizione file di stampa in output.                        ?
071300130104       //--------------------------------------------------------------
071400130104
071500130107     oQSYSPRT   e            PrtTXT1           3
071600130104     o                       RsUt
071700130104     o                                         +  3 'CLIENTI CON RITOR-
071800130107     o                                              NO POD/DOC, MA SEN-
071900130107     o                                              ZA SPEDIZIONI'
072000130104     o                       VT1pgm            + 10
072100130104     o                       *date         y   +  5
072200130104     o                                         +  5 'Pag.'
072300130104     o                       Page          z   +  0
072400130107     o          e            PrtTXT1     0
072500130104     o                                         + 23 'CLIENTI CON RITOR-
072600130107     o                                              NO POD/DOC, MA SEN-
072700130107     o                                              ZA SPEDIZIONI'
072800130107     o          e            PrtTXT1     1
072900130104     o                                         + 23 'NEGLI ULTIMI'
073000130104     o                       VD1ggl        1   +  1
073100130104     o                                         +  1 'GIORNI (DAL'
073200130104     o                       VD1dtl        y   +  1
073300130104     o                                         +  0 ')'
073400130107     o          e            PrtTXT1     0
073500130104     o                                         + 23 'NEGLI ULTIMI'
073600130104     o                       VD1ggl        1   +  1
073700130104     o                                         +  1 'GIORNI (DAL'
073800130104     o                       VD1dtl        y   +  1
073900130104     o                                         +  0 ')'
074000130107      *
074100130107     o          e            PrtTAB      2
074200130107     o                                              'Clienti da tab. "'
074300130107     o                       s_Tab             +  0
074400130107     o                                         +  0 '"'
074500130107     o          e            PrtTAB      0
074600130107     o                                              'Clienti da tab. "'
074700130107     o                       s_Tab             +  0
074800130107     o                                         +  0 '"'
074900130107      *
075000130107     o          e            PrtTXT2     2
075100130104     o                       sk_Txt(1)
075200130104     o                       sK_Txt(4)         +  0
075300130107     o          e            PrtTXT2     1
075400130104     o                       sk_Txt(2)
075500130104     o                       sk_Txt(5)         +  0
075600130104      *
075700130107     o          e            PrtERR      2
075800130107     o                                              'RILEVATO ERRORE: -
075900130104     o                                              consultare le stam-
076000130104     o                                              pe del DUMP e del -
076100130104     o                                              JOBLOG.'
076200130107      *
076300130107     o*//       e            PrtSPACE    1
076400130104      *
076500130104     o          e            PrtDET      1
076600130107     o                       s_CodKsu
076700130107     o                       s_DesKsu          +  1
076800130107     o                       s_CodCli          +  1
076900130104     o                       s_DesCli          +  1
077000130107     o                       s_LACaut          +  2
077100130104     o                       s_DesAut          +  0
077200130107     o                       s_Merge           +  8
077300130107     o                       s_LA2             + 11
077400130107     o                       s_DtUltS          +  5 '  /  /    '
077500130104      *
077600130104     o          e            PrtEND      2
077700130104     o                                         + 24 '***  Fine Stampa  ***'
077800130107     o                                         +  3 'Stampati'
077900130107     o                       s_TotCli      1   +  1
078000130107     o                                         +  1 'clienti.'
078100130103
078200130103       //--------------------------------------------------------------
078300130103       //?Definizione schiere a tempo di compilazione                  ?
078400130103       //--------------------------------------------------------------
078500130103
078600130104** -?sk_Txt:?Testata di stampa?...+....4....+....5....+....6....+*
078700130107Cliente raggruppamento                      Cliente estrazione       1
078800130107------------------------------------------- ----------------------   2
0789001301071234567 *...+....1....+....2....+....3....+ 1234567 *...+....1....   3
079000130107             Tipo elaboraz.    Img con merge  Tab.LA2  Dt.UltSped    4
079100130107-----------  ----------------  -------------  -------  ----------    5
079200130107+....2....+  A-Automatica/FTP        S           S     12/34/5678    6
079300130104** -?sk_Aut/sk_AutD:?Tipi estrazione?
079400130104A-Automatica/FTP     1
079500130104N-Nessuna            2
079600130104S-Schedulata/CD      3
079700130104** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
079800130103Il numero di giorni DEVE essere maggiore di 0 (zero)                            1
