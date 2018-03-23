000100150330       //==============================================================
000200150331       //?TRKC31R * Invio @-Mail a clienti in Campagna                 ?
000300150330       //==============================================================
000400150330
000500150330       //--------------------------------------------------------------
000600150330       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700150330       //--------------------------------------------------------------
000800150330
000900150330     /*PRM  dbgview(*source)
001000150330     /*END
001100150330
001200150330       //--------------------------------------------------------------
001300150330       //?Specifiche di controllo.                                     ?
001400150330       //--------------------------------------------------------------
001500150330
001600150330     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700150330     h dftactgrp(*no)
001800150330
001900150330       //--------------------------------------------------------------
002000150330       //?Dichiarazione file.                                          ?
002100150330       //--------------------------------------------------------------
002200150330
002300150330       // -?Campagne Commerciali?
002400150330     fTICMP01L  if   e           k disk
002500150330
002600150330       // -?Anagrafica Commerciali?
002700150330     fAZCMM01L  if   e           k disk
002800150330
002900150330       // -?Organigramma filiale/aziendale?
003000150330     fAZORG01L  if   e           k disk
003100150330
003200150330       // -?Video Gestione?
003300150330     fTRKC31D   cf   e             workstn
003400150330     f                                     indds( IndDspF )
003500150330     f                                     infds( InfDspF )
003600150330
003700150330       //--------------------------------------------------------------
003800150330       //?Definizione costanti.                                        ?
003900150330       //--------------------------------------------------------------
004000150330
004100150330       // -?Codice autorizzazione profilo Tariffe Clienti?
004200150330     d c_Aut_TC        c                   const('§utegtc')
004300150330
004400150330       // -?Tasti funzionali a video?
004500150330     d c_F01           c                   const(x'31')
004600150330     d c_F02           c                   const(x'32')
004700150330     d c_F03           c                   const(x'33')
004800150330     d c_F04           c                   const(x'34')
004900150330     d c_F05           c                   const(x'35')
005000150330     d c_F06           c                   const(x'36')
005100150330     d c_F07           c                   const(x'37')
005200150330     d c_F08           c                   const(x'38')
005300150330     d c_F09           c                   const(x'39')
005400150330     d c_F10           c                   const(x'3A')
005500150330     d c_F11           c                   const(x'3B')
005600150330     d c_F12           c                   const(x'3C')
005700150330     d c_F13           c                   const(x'B1')
005800150330     d c_F14           c                   const(x'B2')
005900150330     d c_F15           c                   const(x'B3')
006000150330     d c_F16           c                   const(x'B4')
006100150330     d c_F17           c                   const(x'B5')
006200150330     d c_F18           c                   const(x'B6')
006300150330     d c_F19           c                   const(x'B7')
006400150330     d c_F20           c                   const(x'B8')
006500150330     d c_F21           c                   const(x'B9')
006600150330     d c_F22           c                   const(x'BA')
006700150330     d c_F23           c                   const(x'BB')
006800150330     d c_F24           c                   const(x'BC')
006900150330     d c_Enter         c                   const(x'F1')
007000150330     d c_RollDown      c                   const(x'F4')
007100150330     d c_RollUp        c                   const(x'F5')
007200150330
007300150330       // -?Costante per controllo "caratteri solo numerici"?
007400150330     d c_Digits        c                   const('0123456789')
007500150330
007600150330       //--------------------------------------------------------------
007700150330       //?Definizione schiere.                                         ?
007800150330       //--------------------------------------------------------------
007900150330
008000150330       // -?Messaggi di errore?
008100150408     d sk_Msg          s             78    dim( 7)  ctdata  perrcd( 1)
008200150330
008300150330       //--------------------------------------------------------------
008400150330       //?Definizione aree dati.                                       ?
008500150330       //--------------------------------------------------------------
008600150330
008700150330       // -?Dati utente?
008800150330     d §AzUte        e ds                  extname(AZUTE00F)
008900150330     d                                     dtaara
009000150330     d §DatiUte      e ds                  extname(dDatiUte)
009100150330     d                                     dtaara
009200150330
009300150330       //--------------------------------------------------------------
009400150330       //?Definizione strutture dati.                                  ?
009500150330       //--------------------------------------------------------------
009600150330
009700150330       // -?Status ds?
009800150330     d Status         sds
009900150330     d   SDSpgm          *proc
010000150330
010100150330       // -?InfDS?
010200150330     d InfDspF         ds
010300150330     d   dsp_aid             369    369a
010400150330     d*//sfl_rrn             376    377i 0
010500150330     d*//min_nrr             378    379i 0
010600150330     d*//num_rcds            380    381i 0
010700150330
010800150330       // -?Indicatori su DspF?
010900150330     d IndDspF         ds
011000150330         // -?Emissione messaggio di errore?
011100150331     d   ErrMessage                    n   overlay( IndDspF : 28 )
011200150330         // -?Posizionamento cursore & segnalazione errore?
011300150331     d   PosCurFSR                     n   overlay( IndDspF : 50 )
011400150331     d   PosCurNCM                     n   overlay( IndDspF : 51 )
011500150331     d   PosCurCMU                     n   overlay( IndDspF : 52 )
011600150331     d   PosCurFIL                     n   overlay( IndDspF : 53 )
011700150330         // -?Riemissione videata?
011800150330     d   errGenerico                   n   overlay( IndDspF : 99 )
011900150330
012000150330       // -?Parametri ricevuti?
012100150330     d KPJBA         e ds
012200150330
012300150330       //--------------------------------------------------------------
012400150330       //?Definizione variabili globali.                               ?
012500150330       //--------------------------------------------------------------
012600150330
012700150330       // -?Flags booleani?
012800150330     d $Fine           s               n   inz
012900150330     d $InzD01         s               n   inz(*on)
013000150330
013100150330       // -?Indici di schiera?
013200150330     d xx              s              4  0 inz
013300150330
013400150330       // -?Campi associati al video?
013500150330     d $Video          s              2    inz('D1')
013600150331
013700150331       // -?Abilitazioni dell'utente?
013800150331     d wABI            s                   like(UTEaut)  inz
013900150330
014000150330       //--------------------------------------------------------------
014100150330       //?Definizione prototipi procedure usate.                       ?
014200150330       //--------------------------------------------------------------
014300150331
014400150331       // -?Sottomissione lavoro batch?
014500150331      /copy gaitrasrc/srcProtoPR,BCH10
014600150330
014700150330       // -?Reperimento dati utente?
014800150330     d TIBS34ds      e ds                  inz
014900150330      /copy gaitrasrc/srcProtoPR,TIBS34R
015000150330
015100150330       // -?Controllo abilitazioni utente?
015200150330     d TNTAA1ds      e ds                  inz
015300150330      /copy gaitrasrc/srcProtoPR,TNTAA1C
015400150330
015500150331       // -?Selezione di una Campagna?
015600150330     d TRKC02ds      e ds                  inz
015700150330     d trkc02r         pr                  extpgm('TRKC02R')
015800150330     d   kpjba                             likeds(kpjba)
015900150330     d   trkc02ds                          likeds(TRKC02ds)
016000150331
016100150331       // -?Invio Mail a clienti in Campagne (Batch)?
016200150331     d TRKC32ds      e ds                  inz
016300150331     d trkc32r         pr                  extpgm('TRKC32R')
016400150331     d   kpjba                             likeds(kpjba)
016500150331
016600150331       // -?Selezione di un Commerciale (unificante e non)?
016700150331      /copy gaitrasrc/srcprotoPI,TRMK43R_1
016800150331      /copy gaitrasrc/srcprotoPR,TRMK43R
016900150331
017000150331       // -?Caricamento Filiali gestite?
017100150331     d TRUL31ds      e ds                  inz
017200150331     d   sk_POG                       3    overlay( O31pog )  dim(250)
017300150331      /copy gaitrasrc/srcProtoPR,TRUL31R
017400150330
017500150330       //--------------------------------------------------------------
017600150330       //?Definizione key-list.                                        ?
017700150330       //--------------------------------------------------------------
017800150330
017900150330       // -?File TICMP01L?
018000150330     d keyTICMP01    e ds                  extname( TICMP01L : *key )
018100150330     d                                     prefix(k_)  inz
018200150330
018300150330       // -?File AZCMM01L?
018400150330     d keyAZCMM01    e ds                  extname( AZCMM01L : *key )
018500150330     d                                     prefix(k_)  inz
018600150330
018700150330       //--------------------------------------------------------------
018800150330       //?M A I N - L I N E                                            ?
018900150330       //--------------------------------------------------------------
019000150330
019100150330     c     *Entry        plist
019200150330     c                   parm                    KPJBA
019300150330
019400150330      /free
019500150330
019600150330       // -?Operazioni iniziali?
019700150330       exsr  sr_RoutInz;
019800150330
019900150330       // -?Ciclo di gestione del file video?
020000150330       DoW  $Fine = *off;
020100150330
020200150330         select;
020300150330
020400150330           // -?Gestione videata D1 (parzializzazioni)?
020500150330           when  $Video = 'D1';
020600150330             exsr  sr_GesD01;
020700150330
020800150330           // -?? ? ??
020900150330           other;
021000150330             $Fine = *on;
021100150330
021200150330         endsl;
021300150330
021400150330       EndDo;
021500150330
021600150330       // -?Operazioni finali?
021700150330       exsr  sr_RoutEnd;
021800150330
021900150330       //--------------------------------------------------------------
022000150330       //?Operazioni iniziali.                                         ?
022100150330       //--------------------------------------------------------------
022200150330       BEGSR  sr_RoutInz;
022300150330
022400150330         // -?Impostazione chiusura?
022500150330         *inLR = *on;
022600150330
022700150330         // -?Impostazione nome programma a video?
022800150330         T01pgm = SDSpgm;
022900150330
023000150330         // -?Reperimento dati job?
023100150330         exsr  sr_DatiJob;
023200150331         wABI = UTEaut;
023300150408
023400150408         // -?Controllo Sistema Informativo?
023500150408         If  DUTlpo <> 'S';
023600150408           //$Fine = *on;
023700150408           leavesr;
023800150408         EndIf;
023900150330
024000150330         // -?Controllo Autorizzazione dell'Utente alle Tariffe Clienti?
024100150331         reset  TNTAA1ds;
024200150331         ITAA1caut = c_Aut_TC;
024300150331         kPJbU = TNTAA1ds;
024400150331         TNTAA1C ( kPJbA );
024500150331         TNTAA1ds = kPJbU;
024600150331         //If  oTAA1fabi = 'N';
024700150331         //  $Fine = *on;
024800150331         //  leavesr;
024900150331         //EndIf;
025000150330
025100150330         // -?Caricamento filiali abilitate al profilo con autorizzaz.?
025200150330         //  ?a gestione Tariffe Clienti (ricevuta dal *pgm chiamante)?
025300150330         clear  TRUL31ds;
025400150331         I31abi = wABI;
025500150330         I31cdi = DUTdis;
025600150330         I31car = DUTare;
025700150330         I31cpo = DUTpou;
025800150330         TRUL31R ( kpjba : TRUL31ds );
025900150331         //If  O31pog <= *zero;
026000150331         //  $Fine = *on;
026100150331         //  leavesr;
026200150331         //EndIf;
026300150330
026400150330       ENDSR;
026500150330
026600150330       //--------------------------------------------------------------
026700150330       //?Reperimento Dati del job (Utente/Operativi).                 ?
026800150330       //--------------------------------------------------------------
026900150330       BEGSR  sr_DatiJob;
027000150330
027100150330         in(e) §AzUte;
027200150330         if NOT %error;
027300150330           in(e) §DatiUte;
027400150330         endif;
027500150330         if %error or RSut = *blanks;
027600150330           clear TIBS34ds;
027700150330           tibs34r ( tibs34ds );
027800150330           in §AzUte;
027900150330           in §DatiUte;
028000150330         endif;
028100150330
028200150330       ENDSR;
028300150330
028400150330       //--------------------------------------------------------------
028500150330       //?Gestione videata D01.                                        ?
028600150330       //--------------------------------------------------------------
028700150330       BEGSR  sr_GesD01;
028800150330
028900150330         // -?Inizializzazione videata?
029000150331         if  $InzD01 = *on;
029100150331           exsr  sr_InzD01;
029200150331           $InzD01 = *off;
029300150331         endif;
029400150330
029500150330         // -?Emissione testata?
029600150331         write  KC31T01;
029700150408
029800150408         select;
029900150408
030000150408           // -?Utente NON di Sede?
030100150408           when  DUTlpo <> 'S';
030200150408             errGenerico = *on;
030300150408             errMessage  = *on;
030400150408             PosCurFSR   = *on;
030500150408             V01msg = sk_Msg(01);
030600150408             $Fine  = *on;
030700150331
030800150408           // -?Utente NON abilitato?
030900150408           when  oTAA1fabi = 'N'  or
031000150408                 o31pog   <= *zero;
031100150408             errGenerico = *on;
031200150408             errMessage  = *on;
031300150408             PosCurFSR   = *on;
031400150408             V01msg = sk_Msg(02);
031500150408             $Fine  = *on;
031600150408
031700150408         endsl;
031800150330
031900150330         // -?Emissione videata?
032000150331         exfmt  KC31D01;
032100150330
032200150330         clear  errGenerico;
032300150330         clear  errMessage;
032400150331         clear  V01msg;
032500150330
032600150330         Select;
032700150331
032800150331           // -?Utente NON abilitato alla funzione?
032900150331           when  oTAA1fabi = 'N';
033000150331             $Fine = *on;
033100150331
033200150331           // -?Utente NON abilitato a nessuna filiale?
033300150331           when  O31pog <= *zero;
033400150331             $Fine = *on;
033500150330
033600150330           // -?F3=Fine?
033700150330           when  dsp_aid = c_F03;
033800150331             $Fine = *on;
033900150330
034000150330           // -?Invio?
034100150330           Other;
034200150330             exsr  sr_CtrD01;
034300150331             if  errGenerico  or  dsp_aid <> c_F06;
034400150330               leavesr;
034500150330             endif;
034600150331             // ·?F6=Sottomissione batch?
034700150331             exsr  sr_SbmJob;
034800150402             $Fine = (errGenerico = *off);
034900150330
035000150330         EndSl;
035100150330
035200150330       ENDSR;
035300150330
035400150330       //--------------------------------------------------------------
035500150330       //?Inizializzazione videata D01.                                ?
035600150330       //--------------------------------------------------------------
035700150330       BEGSR  sr_InzD01;
035800150330
035900150331         clear  KC31D01;
036000150330
036100150330       ENDSR;
036200150330
036300150330       //--------------------------------------------------------------
036400150330       //?Controllo dati in videata D01.                               ?
036500150330       //--------------------------------------------------------------
036600150330       BEGSR  sr_CtrD01;
036700150330
036800150330         // -?Spegnimento degli indicatori di errore?
036900150330         %subst(IndDspF : 50) = *off;
037000150331
037100150331         // -?Flag Stampa / Ristampa?
037200150414         if  V01fsr <> *blank  and  V01fsr <> 'R'
037300150414                               and  V01fsr <> 'M';
037400150331           errGenerico = *on;
037500150331           errMessage  = *on;
037600150402           PosCurFSR   = *on;
037700150408           V01msg = sk_Msg(03);
037800150331           leavesr;
037900150331         endif;
038000150330
038100150330         // -?Campagna Commerciale?
038200150331         clear  V01ncmD;
038300150331         if  V01ncm = *zero;
038400150331           clear  V01ncm;
038500150331         endif;
038600150330
038700150331         Select ;
038800150331
038900150331           // ·?Non inserito?
039000150331           When  V01ncm = *blank  or  V01ncm = *zero;
039100161114             errGenerico = *on;
039200161114             errMessage  = *on;
039300161114             PosCurNCM   = *on;
039400161114             V01msg = %replace( 'obbligatoria'
039500161114                              : sk_Msg(04)
039600161114                              : %scan('errata      ' : sk_Msg(04) ) );
039700161114             leavesr;
039800150331
039900150331           // ·?Ricerca?
040000150331           When  %scan( '?' : V01ncm ) > *zero;
040100150331             clear  V01ncm;
040200150331             clear  TRKC02ds;
040300150331             iKC02ric = 'R';
040400150331             trkc02r ( kpjba : TRKC02ds );
040500150331             if  oKC02ncm <> *zero;
040600150331               V01ncm = %editc( oKC02ncm : '3' );
040700150331             endif;
040800150331             PosCurNCM   = *on;
040900150331             errGenerico = *on;
041000150331             if  oKC02err = *on;
041100150331               ErrMessage = *on;
041200150331               V01msg = oKC02msg;
041300150331               leavesr;
041400150331             endif;
041500150330
041600150331           // ·?Controllo numericità?
041700150331           When  %check( c_Digits : %trim( V01ncm ) ) > *zero;
041800150330             errGenerico = *on;
041900150330             errMessage  = *on;
042000150330             PosCurNCM   = *on;
042100150408             V01msg = sk_Msg(04);
042200150330             leavesr;
042300150330
042400150331           // ·?Controllo validità?
042500150331           Other;
042600150330             If  k_CMPncm <> %int( %trim( V01ncm ) );
042700150330               clear  keyTICMP01;
042800150330               k_CMPncm = %int( %trim( V01ncm ) );
042900150330               chain  %kds( keyTICMP01 )  TICMP000;
043000150330               if  Not %found( TICMP01L );
043100150331                 clear  k_CMPncm;
043200150330                 errGenerico = *on;
043300150330                 errMessage  = *on;
043400150330                 PosCurNCM   = *on;
043500150408                 V01msg = sk_Msg(04);
043600150330                 leavesr;
043700150330               endif;
043800150331             EndIf;
043900150331             V01ncmD = CMPdes;
044000150331             Select;
044100150331               When  CMPtpc <> 'L';
044200150331                 errMessage  = *on;
044300150331                 errGenerico = *on;
044400150331                 PosCurNCM   = *on;
044500150408                 V01msg = %trimR( sK_Msg(04) ) + ': non è di tipo "Lettera"';
044600150331                 leavesr;
044700150331               When  %dec( %date() ) < CMPdic;
044800150331                 errMessage  = *on;
044900150331                 errGenerico = *on;
045000150331                 PosCurNCM   = *on;
045100150408                 V01msg = %trimR( sK_Msg(04) ) + ': non è ancora attiva';
045200150331                 leavesr;
045300150331               When  %dec( %date() ) > CMPdfc;
045400150331                 errMessage  = *on;
045500150331                 errGenerico = *on;
045600150331                 PosCurNCM   = *on;
045700150408                 V01msg = %trimR( sK_Msg(04) ) + ': è già scaduta';
045800150331                 leavesr;
045900150331             EndSl;
046000150330
046100150331         EndSl;
046200150331
046300150331         // -?Commerciale Unificante?
046400150331         clear  V01cmmD;
046500150331         if  V01cmm = *zero;
046600150331           clear  V01cmm;
046700150331         endif;
046800150331         Select ;
046900150331
047000150331           // ·?Non inserito?
047100150331           When  V01cmm = *blank  or  V01cmm = *zero;
047200150331
047300150331           // ·?Ricerca?
047400150331           When  %scan( '?' : V01cmm ) > *zero;
047500150331             clear  V01cmm;
047600150331             clear  TRMK43ds;
047700150331             iMK43solU = 'S';
047800150331             iMK43fil  = DUTpou;
047900150331             TRMK43R ( kpjba : TRMK43ds );
048000150331             if  oMK43err = *off  and  oMK43fxx = *blank;
048100150331               V01cmm  = %editc( oMK43cmm : 'X' );
048200150331               V01cmmD = oMK43des;
048300150331             endif;
048400150331             ErrGenerico = *on;
048500150331             PosCurCMU   = *on;
048600150331             if  oMK43err = *on;
048700150331               ErrMessage = *on;
048800150331               V01msg = oMK43msg;
048900150331             endif;
049000150331             leavesr;
049100150331
049200150331           // ·?Controllo numericità?
049300150331           When  %check( c_Digits : V01cmm ) > *zero;
049400150331             ErrMessage  = *on;
049500150331             ErrGenerico = *on;
049600150331             PosCurCMU   = *on;
049700150408             V01msg = sk_Msg(05);
049800150331             leavesr;
049900150331
050000150331           // ·?Controllo validità?
050100150331           Other;
050200150331             If  k_CMMcod <> %int( %trim( V01cmm ) );
050300150331               k_CMMcod = %int( %trim( V01cmm ) );
050400150331               chain  %kds(keyAZCMM01)  AZCMM000;
050500150415               if  Not %found( AZCMM01L );
050600150331                 clear  k_CMMcod;
050700150331                 errGenerico = *on;
050800150331                 errMessage  = *on;
050900150331                 PosCurCMU   = *on;
051000150408                 V01msg = sk_Msg(05);
051100150331                 leavesr;
051200150331               endif;
051300150331             EndIf;
051400150331             V01cmmD = CMMdes;
051500150331             Select;
051600150331               When  CMMpar <> *blank;   // ·?Verifica SE codice attivo?
051700150331                 errGenerico = *on;
051800150331                 errMessage  = *on;
051900150331                 PosCurCMU   = *on;
052000150408                 V01msg = sk_Msg(05);
052100150331                 leavesr;
052200150331               When  %dec( %date() ) < CMMdtIni;
052300150331                 errMessage  = *on;
052400150331                 errGenerico = *on;
052500150331                 PosCurCMU   = *on;
052600150408                 V01msg = %trimR( sK_Msg(05) ) + ': non è ancora attivo';
052700150331                 leavesr;
052800150331               When  %dec( %date() ) > CMMdtFin;
052900150331                 errMessage  = *on;
053000150331                 errGenerico = *on;
053100150331                 PosCurCMU   = *on;
053200150408                 V01msg = %trimR( sK_Msg(05) ) + ': è già scaduto';
053300150331                 leavesr;
053400150331             EndSl;
053500150331
053600150331         EndSl;
053700150331
053800150331         If  V01cmm > *zero;
053900150331
054000150331           // -?Abilitazione utente al commerciale?
054100150415           reset  TNTAA1ds;
054200150415           ITAA1caut = c_Aut_TC;
054300150415           ITAA1cmm  = %int( V01cmm );
054400150415           kpjbu     = TNTAA1ds;
054500150415           TNTAA1C ( kpjba );
054600150415           TNTAA1ds = kpjbu;
054700150415           if  oTAA1fabi = 'N';
054800150415             errGenerico = *on;
054900150415             errMessage  = *on;
055000150415             PosCurCMU   = *on;
055100150415             V01msg = sk_Msg(05);
055200150415             leavesr;
055300150415           endif;
055400150331
055500150331           // -?Selezionabili SOLO commerciali unificanti?
055600150331           if  %int( V01cmm ) <> CMMuni;
055700150331             errGenerico = *on;
055800150331             errMessage  = *on;
055900150331             PosCurCMU   = *on;
056000150408             V01msg = sk_Msg(06);
056100150331             leavesr;
056200150331           endif;
056300150331
056400150331         EndIf;
056500150331
056600150331         // -?Filiale Appartenenza?
056700150331         clear  V01fcmD;
056800150331         If  V01fcm > *zero;
056900150331           chain  ( V01fcm )  AZORG;
057000150331           if  Not %found(AZORG01L)  or  ORGfva <> *blank;
057100150331             errGenerico = *on;
057200150331             errMessage  = *on;
057300150331             PosCurFIL   = *on;
057400150408             V01msg = sk_Msg(07);
057500150331             leavesr;
057600150331           endif;
057700150331           V01fcmD = ORGdes;
057800150331           // ·?Verifica se fil. abilitata all'utente?
057900150331           if  %lookup( %editc( V01fcm : 'X' ) : sk_POG ) = *zero;
058000150331             errGenerico = *on;
058100150331             errMessage  = *on;
058200150331             PosCurFIL   = *on;
058300150408             V01msg = sk_Msg(07);
058400150331             leavesr;
058500150331           endif;
058600150331         EndIf;
058700150330
058800150330       ENDSR;
058900150330
059000150330       //--------------------------------------------------------------
059100150331       //?Sottomissione lavoro Batch (TRKC32R).                        ?
059200150330       //--------------------------------------------------------------
059300150331       BEGSR  sr_SbmJob;
059400150330
059500150331         clear  TRKC32ds;
059600150331         iKC32fsr = V01fsr;
059700150402         if  V01ncm <> *blank  and  V01ncm <> *zero;
059800150402           iKC32ncm = %int( %trim( V01ncm ) );
059900150402         endif;
060000150402         if  V01cmm <> *blank  and  V01cmm <> *zero;
060100150402           iKC32cmm = %int( %trim( V01cmm ) );
060200150402         endif;
060300150331         iKC32fcm = V01fcm;
060400150331
060500150331         kpjbu = TRKC32ds;
060600150331         kcoaz = 'KC32';
060700150331         clear KBUFF;
060800150331
060900150331         if  knmus = *all'1';
061000150331           // -?Richiamo interattivo?
061100150331           TRKC32R ( kpjba );
061200150331         else;
061300150331           // -?Sottomissione batch?
061400150331           BCH10 ( kpjba );
061500150331         endif;
061600150330
061700150330       ENDSR;
061800150330
061900150330       //--------------------------------------------------------------
062000150330       //?Operazioni finali.                                           ?
062100150330       //--------------------------------------------------------------
062200150330       BEGSR  sr_RoutEnd;
062300150330
062400150330         // -?Chiusura applicazioni precedentemente aperte?
062500150330         clear  TNTAA1ds;
062600150330         iTAA1tla = 'C';
062700150330         clear  KPJBU;
062800150330         KPJBU = TNTAA1ds;
062900150330         TNTAA1C ( kpjba );
063000150330
063100150330         // -?Chiusura *pgm?
063200150330         return;
063300150330
063400150330       ENDSR;
063500150330
063600150330      /end-free
063700150330
063800150330       //--------------------------------------------------------------
063900150330       //?Schiere a tempo di compilazione.
064000150330       //--------------------------------------------------------------
064100150330
064200150331** --?sk_Msg:?Messaggi di Errore?--------------------------------------------*
064300150408Funzione eseguibile solo dalla SEDE                                            1
064400150414Utente non abilitato alla funzione                                             2
064500150414Flag "Tipo elaborazione" errato                                                3
064600150414Campagna Commerciale errata                                                    4
064700150414Codice commerciale errato  o  non in gestione all'utente                       5
064800150414Richiedere solo commerciali UNIFICANTI                                         6
064900150414Filiale errata                                                                 7
