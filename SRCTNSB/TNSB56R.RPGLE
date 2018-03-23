000100151019       //==============================================================
000200151019       //?Visualizzazione Pesi usati in Tassazione (da Bolle Sede).    ?
000300151019       //==============================================================
000400151019
000500151019       //--------------------------------------------------------------
000600151019       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700151019       //--------------------------------------------------------------
000800151019
000900151019     /*PRM  dbgview(*source)
001000151019     /*END
001100151019
001200151019       //--------------------------------------------------------------
001300151019       //?Specifiche di controllo.                                     ?
001400151019       //--------------------------------------------------------------
001500151019
001600151019     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700151019     h dftactgrp(*no) actgrp(*caller)
001800151022     h bnddir('TRUL')
001900151019
002000151019       //--------------------------------------------------------------
002100151019       //?Dichiarazione file.                                          ?
002200151019       //--------------------------------------------------------------
002300151019
002400151019       // -?Bolle Fil./Sede: Estensione Testata Bolle?
002500151019     fFIAR531C  if   e           k disk
002600151019
002700151019       // -?Bolle Sede?
002800151019     fTITAS30C  if   e           k disk    usropn
002900151019
003000151019       // -?Video?
003100151019     fTNSB56D   cf   e             workstn
003200151019     f                                     indds(IndDspF)
003300151019     f                                     infds(InfDspF)
003400151019
003500151019       //--------------------------------------------------------------
003600151019       //?Definizione costanti.                                        ?
003700151019       //--------------------------------------------------------------
003800151019
003900151019       // -?Tasti funzionali a video?
004000151019     d c_F01           c                   const(x'31')
004100151019     d c_F02           c                   const(x'32')
004200151019     d c_F03           c                   const(x'33')
004300151019     d c_F04           c                   const(x'34')
004400151019     d c_F05           c                   const(x'35')
004500151019     d c_F06           c                   const(x'36')
004600151019     d c_F07           c                   const(x'37')
004700151019     d c_F08           c                   const(x'38')
004800151019     d c_F09           c                   const(x'39')
004900151019     d c_F10           c                   const(x'3A')
005000151019     d c_F11           c                   const(x'3B')
005100151019     d c_F12           c                   const(x'3C')
005200151019     d c_F13           c                   const(x'B1')
005300151019     d c_F14           c                   const(x'B2')
005400151019     d c_F15           c                   const(x'B3')
005500151019     d c_F16           c                   const(x'B4')
005600151019     d c_F17           c                   const(x'B5')
005700151019     d c_F18           c                   const(x'B6')
005800151019     d c_F19           c                   const(x'B7')
005900151019     d c_F20           c                   const(x'B8')
006000151019     d c_F21           c                   const(x'B9')
006100151019     d c_F22           c                   const(x'BA')
006200151019     d c_F23           c                   const(x'BB')
006300151019     d c_F24           c                   const(x'BC')
006400151019     d c_Enter         c                   const(x'F1')
006500151019     d c_RollDown      c                   const(x'F4')
006600151019     d c_RollUp        c                   const(x'F5')
006700151019
006800151019       //--------------------------------------------------------------
006900151019       //?Definizione schiere.                                         ?
007000151019       //--------------------------------------------------------------
007100151019
007200151019
007300151019       //--------------------------------------------------------------
007400151019       //?Definizione aree dati.                                       ?
007500151019       //--------------------------------------------------------------
007600151019
007700151019       // -?Dati utente?
007800151019     d §AzUte        e ds                  extname(AZUTE00F)
007900151019     d                                     dtaara
008000151019     d §DatiUte      e ds                  extname(dDatiUte)
008100151019     d                                     dtaara
008200151019
008300151019       //--------------------------------------------------------------
008400151019       //?Definizione strutture dati.                                  ?
008500151019       //--------------------------------------------------------------
008600151019
008700151019       // -?Status ds?
008800151019     d Status         sds
008900151019     d   SDSpgm          *proc
009000151019
009100151019       // -?InfDS?
009200151019     d InfDspF         ds
009300151019     d   dsp_aid             369    369a
009400151019
009500151019       // -?Indicatori su DspF?
009600151019     d IndDspF         ds                  inz
009700151019         // -?Emissione messaggio di errore?
009800151019     d   ErrMessage                    n   overlay(IndDspF : 28)
009900151021         // -?Indicatori per Attibuti di visualizzazione?
010000160505     d   VisStaPesi                    n   overlay(IndDspF : 42)
010100160505     d   VisStaVolumi                  n   overlay(IndDspF : 43)
010200160505     d   F02Sta                        n   overlay(IndDspF : 51)
010300160506     d   F08Stavolume                  n   overlay(IndDspF : 52)
010400160506     d   F02Stapeso                    n   overlay(IndDspF : 53)
010500151019         //   -?Riemissione videata?
010600151019     d   ErrGenerico                   n   overlay(IndDspF : 99)
010700151019
010800151019       // -?Parametri ricevuti?
010900151020     d kpjba         e ds
011000151020     d TNSB56ds      e ds                  inz
011100151019
011200151019       // -?Estensione FIAR500F: trk "FAT"?
011300151019     d dAR5fat       e ds                  inz  qualified
011400151022
011500151022       // -?Tabella "TR" = Tipi Tariffa?
011600151022     d dsTR          e ds                  inz  qualified
011700151019
011800151019       //--------------------------------------------------------------
011900151019       //?Definizione variabili globali.                               ?
012000151019       //--------------------------------------------------------------
012100151019
012200151019       // -?Flags booleani?
012300151019     d $Fine           s               n   inz
012400151019     d $InzD01         s               n   inz(*on)
012500151020
012600151020       // -?Campi "data"?
012700151020     d wDate_Eur       s               d   inz  datfmt(*EUR)
012800151019
012900151019       //--------------------------------------------------------------
013000151020       //?Definizione prototipi/procedure.                             ?
013100151019       //--------------------------------------------------------------
013200151019
013300151019       // -?Reperimento dati utente?
013400151019     d TIBS34ds      e ds
013500151019      /copy gaitrasrc/srcProtoPR,TIBS34R
013600151019
013700151019       // -?Controllo/Decodifica cliente?
013800151019      /copy gaitrasrc/srcProtoPI,TIBS69R
013900151019      /copy gaitrasrc/srcProtoPR,TIBS69R
014000151022
014100151022       // -?Reperimento dati tabelle?
014200151022      /copy gaitrasrc/srcProtoPI,TRULTAB
014300151022      /copy gaitrasrc/srcProtoPR,TRULTAB
014400151020
014500151020       // -?Interrogazione Statistica cliente?
014600151020     d TISE60ds      e ds                  inz
014700151020     d   D60op0      e                     inz('VIS')
014800151020     d   D60op1      e                     inz('F06')
014900151020     d   D60f03      e                     inz(*off)
015000151020     d   D60err      e                     inz(*off)
015100151020     d   D60sce      e                     inz('1')
015200151020     d   D60stp      e                     inz('S')
015300151020     d   D60dpv      e                     inz('N')
015400151020     d   D60st1      e                     inz('N')
015500151020     d   D60st2      e                     inz('N')
015600151020     d   D60st3      e                     inz('N')
015700151020     d   D60st4      e                     inz('N')
015800151020     d   D60st5      e                     inz('N')
015900151020     d   D60st6      e                     inz('N')
016000151020     d   D60st7      e                     inz('N')
016100160801     d   D60clA      e                     inz
016200160801     d   D60clB      e                     inz
016300160801     d   D60clC      e                     inz
016400160801     d   D60clT      e                     inz
016500160801     d   D60clD      e                     inz
016600151020     d   sk_KSC                       7p 0 overlay( D60ksc :  1 )
016700151020     d                                     dim( 28 )
016800151020     d                                     inz
016900151020     d tise65r         pr                  extpgm('TISE65R')
017000151020     d   kpjba                             likeds(KPJBA)
017100151019
017200151019       //--------------------------------------------------------------
017300151019       //?Definizione key-list.                                        ?
017400151019       //--------------------------------------------------------------
017500151019
017600151019       // -?File FIAR531L?
017700151019     d keyFIAR531    e ds                  extname( FIAR531C : *key )
017800151019     d                                     prefix( k_ )   inz
017900151019
018000151019       // -?File TITAS30C?
018100151019     d keyTITAS30    e ds                  extname( TITAS30C : *key )
018200151019     d                                     prefix( k_ )   inz
018300151019
018400151019       //--------------------------------------------------------------
018500151019       //?M A I N - L I N E                                            ?
018600151019       //--------------------------------------------------------------
018700151019
018800151019     c     *Entry        plist
018900151020     c                   parm                    KPJBA
019000151019
019100151019      /free
019200151019
019300151019       // -?Operazioni iniziali?
019400151019       exsr  sr_RoutInz;
019500151019
019600151019
019700151019       // -?Gestione Video?
019800151019       DoW  Not $Fine;
019900151019         exsr  sr_GesD01;
020000151019       EndDo;
020100151019
020200151019
020300151019       // -?Operazioni finali?
020400151019       exsr  sr_RoutEnd;
020500151019
020600151019       //--------------------------------------------------------------
020700151019       //?Operazioni iniziali.                                         ?
020800151019       //--------------------------------------------------------------
020900151019       BEGSR  sr_RoutInz;
021000151019
021100151019         // -?Impostazione chiusura?
021200151019         *inLR = *on;
021300151019
021400151019         // -?Reperimento dati job?
021500151019         exsr  sr_DatiJob;
021600151019
021700151019         // -?Eseguibile solo da Sede?
021800151019         if  DUTlpo <> 'S';
021900151019           exsr  sr_RoutEnd;
022000151019         endif;
022100151020
022200151020         T1Cpgm = SDSpgm;
022300151019
022400151019         // -?Verifica dei parametri ricevuti?
022500151019         //  ?(Numero spedizione mancante)?
022600151020         TNSB56ds = KPJBU;
022700151019         if  d56aas = *zero  or  d56lnp = *zero  or  d56nsp = *zero;
022800151019           exsr  sr_RoutEnd;
022900151019         endif;
023000151019
023100151019         // -?Reperimento dati da FIAR5/"FAT"?
023200151022         clear  dAR5fat;
023300151019         clear  keyFIAR531;
023400151019         k_AR5aas = d56aas;
023500151019         k_AR5lnp = d56lnp;
023600151019         k_AR5nrs = d56nrs;
023700151019         k_AR5nsp = d56nsp;
023800151019         k_AR5trd = 'FAT';
023900151019         chain  %kds( keyFIAR531 : 5 )  FIAR531C;
024000151019         if  Not %found(FIAR531C);
024100151019           exsr  sr_RoutEnd;
024200151019         endif;
024300151019
024400151019         dAR5fat = AR5uni;
024500151019
024600151019       ENDSR;
024700151019
024800151019       //--------------------------------------------------------------
024900151019       //?Reperimento Dati del job (Utente/Operativi).                 ?
025000151019       //--------------------------------------------------------------
025100151019       BEGSR  sr_DatiJob;
025200151019
025300151019         in(E) §AzUte;
025400151019         if NOT %error;
025500151019           in(E) §DatiUte;
025600151019         endif;
025700151019         if %error or RSut = *blanks;
025800151019           clear TIBS34ds;
025900151019           tibs34r ( tibs34ds );
026000151019           in §AzUte;
026100151019           in §DatiUte;
026200151019         endif;
026300151019
026400151019       ENDSR;
026500151019
026600151019       //--------------------------------------------------------------
026700151019       //?Richiesta / Controllo "BRTcode".                             ?
026800151019       //--------------------------------------------------------------
026900151019       BEGSR  sr_GesD01;
027000151019
027100151019         // -?Inizializzazione videata?
027200151019         if  $InzD01 = *on;
027300151019           exsr  sr_InzD01;
027400151019           $InzD01 = *off;
027500151019         endif;
027600151019
027700151019         // -?Emissione videata?
027800151019         write  SB56W01;
027900151019         write  SB56T01;
028000151019         exfmt  SB56D01;
028100151019
028200151019         clear  V1Dmsg;
028300151019         reset  ErrMessage;
028400151019         reset  ErrGenerico;
028500151019
028600151019         Select;
028700151020
028800151020           // -?F2=Statistiche cliente?
028900160506           When  dsp_aid = c_F02 or dsp_aid = c_f08;
029000151020             exsr  sr_TISE65R;
029100151019
029200151020           // -?F12=Ritorno?
029300151020           When  dsp_aid = c_F12;
029400151019             $Fine = *on;
029500151019
029600151019         EndSl;
029700151019
029800151019       ENDSR;
029900151019
030000151019       //--------------------------------------------------------------
030100151019       //?Inizializzazione videata D01.                                ?
030200151019       //--------------------------------------------------------------
030300151019       BEGSR  sr_InzD01;
030400151020
030500151020         // -?Posizionamento verticale window?
030600151022         clear  SB56W01;
030700151022         W1row  = 2;
030800151019
030900151020         // -?Inizializzazione videata principale (D01)?
031000151019         clear  SB56D01;
031100151019
031200151022         // ·?Impostazione dati ricevuti, relativi alla spedizione?
031300151019         D1Clnp = D56lnp;
031400151019         D1Cnrs = D56nrs;
031500151019         D1Cnsp = D56nsp;
031600151019         D1Caas = D56aas;
031700151019
031800151022         // ·?Impostazione codice Cliente?
031900151019         If  D56ksc <> *zero;
032000151019           D1Cksc = D56ksc;
032100151019         Else;
032200151020           if  not %open(TITAS30C);
032300151020             open  TITAS30C;
032400151022           endif;
032500151019           clear  keyTITAS30;
032600151019           k_TASaas = D56aas;
032700151019           k_TASlnp = D56lnp;
032800151019           k_TASnrs = D56nrs;
032900151019           k_TASnsp = D56nsp;
033000151019           k_TAStbl = D56tbl;
033100151019           if  D56tbl <> *blank;
033200151019             chain  %kds( keyTITAS30 : 5 )  TITAS30C;
033300151019           else;
033400151019             chain  %kds( keyTITAS30 : 4 )  TITAS30C;
033500151019           endif;
033600151019           if  %found(TITAS30C);
033700151019             D1Cksc = TASksc;
033800151019           endif;
033900151019         EndIf;
034000151019
034100151022         // ·?Decodifica Cliente?
034200151020         clear  TIBS69ds;
034300151020         I69sif = KNSIF;
034400151020         I69kcc = DUTkci;
034500151020         I69kac = D1Cksc;
034600151020         tibs69r( tibs69ds :
034700151020                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
034800151020         if  O69err <> *on;
034900151020           D1Drsc = ACOrag;
035000151020         endif;
035100151022
035200151022         // ·?Impostazione codice Tariffa?
035300151022         If  D56ctr <> *zero;
035400151022           D1Ctar = D56ctr;
035500151022         Else;
035600151022           if  not %open(TITAS30C);
035700151022             open  TITAS30C;
035800151022           endif;
035900151022           clear  keyTITAS30;
036000151022           k_TASaas = D56aas;
036100151022           k_TASlnp = D56lnp;
036200151022           k_TASnrs = D56nrs;
036300151022           k_TASnsp = D56nsp;
036400151022           k_TAStbl = D56tbl;
036500151022           if  D56tbl <> *blank;
036600151022             chain  %kds( keyTITAS30 : 5 )  TITAS30C;
036700151022           else;
036800151022             chain  %kds( keyTITAS30 : 4 )  TITAS30C;
036900151022           endif;
037000151022           if  %found(TITAS30C);
037100151022             D1Ctar = TASctr;
037200151022           endif;
037300151022         EndIf;
037400151022
037500151022         // ·?Decodifica Tariffa?
037600151022         If  D56ctr <> *zero;
037700151022           D1Dtar = D56dtr;
037800151022         Else;
037900151022           clear  dsTR;
038000151022           ds_TNTBE.TBEke1 = %subst( %editc( D1Ctar : 'X' ) : 1 : 1 );
038100151022           if  getTabella ( c_Tabel : 'TR'  : ds_TNTBE.TBEke1 :
038200151022                            *omit : *omit : *omit :
038300151022                            *omit : *omit :
038400151022                            *omit : *omit : *omit : *omit :
038500151022                            *omit : *omit : *omit : *omit :
038600151022                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
038700151022                          = *zero      AND
038800151022               ds_TNTBE.TBEatb = *blank;
038900151022             dsTR   = ds_TNTBE.TBEuni;
039000151022           endif;
039100151022           D1Dtar = dsTR.§TRdes;
039200151022         EndIf;
039300151019
039400151020         // -?Reperimento dati da FIAR5/"FAT"?
039500151022         //  ?(già fatto nella subr. "sr_RoutInz")?
039600151022         //clear  dAR5fat;
039700151022         //clear  keyFIAR531;
039800151022         //k_AR5aas = D56aas;
039900151022         //k_AR5lnp = D56lnp;
040000151022         //k_AR5nrs = D56nrs;
040100151022         //k_AR5nsp = D56nsp;
040200151022         //k_AR5trd = 'FAT';
040300151022         //chain  %kds( keyFIAR531 : 5 )  FIAR531C;
040400151022         //if  NOT %found( FIAR531C );
040500151022         //  leavesr;
040600151022         //endif;
040700151022         //dAR5fat = AR5uni;
040800151020
040900151021         // -?Impostazione dati di FIAR5/"FAT" a video?
041000151020         // ·?Peso Fatturato?
041100151020         D1CfpTas = dAR5fat.§AR5fpTas;
041200151020         D1CpkTas = dAR5fat.§AR5pkTas;
041300151021         // ·?Volume Fatturato?
041400151021         D1CfvTas = dAR5fat.§AR5fvTas;
041500151021         D1CvlTas = dAR5fat.§AR5vlTas;
041600151020         // ·?Data/Ora Tassazione?
041700151020         wDate_Eur = %date( dAR5fat.§AR5daTas : *iso );
041800151020         D1CdaTas  = %dec( wDate_Eur );
041900151020         D1ChmTas  = dAR5fat.§AR5hmTas;
042000160505         // ·?Anno/Mese Statistica Pesi  Solo se peso desunto
042100160505         If dAR5fat.§AR5fpTas = 'D' ;
042200160505            D1CstMAA  = ( dAR5fat.§AR5stAmm * 10000 ) + dAR5fat.§AR5stAaa;
042300160505         Endif;
042400160505         // ·?Anno/Mese Statistica Volumi  Solo se volume desunto
042500160505         If dAR5fat.§AR5fvTas = 'D' ;
042600160505            D1CstMAV  = ( dAR5fat.§AR5svAmm * 10000 ) + dAR5fat.§AR5svAaa;
042700160505         Endif;
042800151020         // ·?Profilo Tassazione?
042900151020         D1CprTas  = dAR5fat.§AR5prTas;
043000151020         // ·?PC Tassazione?
043100151020         D1CnjTas  = dAR5fat.§AR5njTas;
043200151021
043300151021         // -?Impostazione indicatori per attributi di visualizzazione?
043400160505         // - anno e mese statistica pesi se peso desunto
043500160505         VisStaPesi =    dAR5fat.§AR5fpTas = 'D';
043600160505         // - anno e mese statistica volumi se volume desunto
043700160505         VisStaVolumi  = dAR5fat.§AR5fvTas = 'D';
043800160506         // -?Impostazione indicatori per attributi di visualizzazione tasti funzionali
043900160506         // volume desunto e peso non desunto
044000160506         F08stavolume = dAR5fat.§AR5fvTas = 'D' and dAR5fat.§AR5fpTas <>'D';
044100160506         // volume non desunto e peso desunto
044200160506         F02stapeso   = dAR5fat.§AR5fvTas <>'D' and dAR5fat.§AR5fpTas = 'D';
044300160506         // volume desunto e peso desunto e anno e mese statistica uguali
044400160506         F02sta = dAR5fat.§AR5fvTas ='D' and dAR5fat.§AR5fpTas = 'D'
044500160506                  and D1CstMAA =  D1CstMAV;
044600160506         // volume desunto e peso desunto e anno e mese statistica diversi
044700160506         If  dAR5fat.§AR5fvTas ='D' and dAR5fat.§AR5fpTas = 'D'
044800160506             and D1CstMAA <> D1CstMAV;
044900160506             F02sta = *off ;
045000160506             F02stapeso = *on ;
045100160506             F08stavolume = *on ;
045200160506         Endif ;
045300151019       ENDSR;
045400151019
045500151019       //--------------------------------------------------------------
045600151020       //?Visualizzazione Statistiche Cliente.                         ?
045700151019       //--------------------------------------------------------------
045800151020       BEGSR  sr_TISE65R;
045900151019
046000151020         reset  TISE60ds;
046100151020         clear  sk_Ksc;
046200160506         sk_Ksc(01) = D1Cksc;
046300160506         if dsp_aid = c_F02 ;
046400160506            D60da2 = ( dAR5fat.§AR5stAaa * 100 ) + dAR5fat.§AR5stAmm;
046500160506         endif ;
046600160506         if dsp_aid = c_F08 ;
046700160506            D60da2 = ( dAR5fat.§AR5svAaa * 100 ) + dAR5fat.§AR5svAmm;
046800160506         endif ;
046900151020         D60dat = D60da2;
047000151022         D60ct1 = %editc( D1Ctar : 'X' );
047100151020
047200151020         KPJBU    = TISE60ds;
047300151020
047400151020         tise65r ( kpjba );
047500151020
047600151020         TISE60ds = KPJBU;
047700151020
047800151020         if  D60f03 = *on;
047900151020           $Fine = *on;
048000151020         endif;
048100151019
048200151019       ENDSR;
048300151020
048400151020       //--------------------------------------------------------------
048500151020       //?Operazioni finali.                                           ?
048600151020       //--------------------------------------------------------------
048700151020       BEGSR  sr_RoutEnd;
048800151022
048900151022         // -?Chiusura funzioni precedentemente aperte?
049000151022         cloTabella ( c_Tabel );
049100151020
049200151022         // -?Chiusura pgm?
049300151020         return;
049400151020
049500151020       ENDSR;
049600151019
049700151019      /end-free
