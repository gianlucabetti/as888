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
010001160505     d   VisStaPesi                    n   overlay(IndDspF : 42)
010002160505     d   VisStaVolumi                  n   overlay(IndDspF : 43)
010003160505     d   F02Sta                        n   overlay(IndDspF : 51)
010004160506     d   F08Stavolume                  n   overlay(IndDspF : 52)
010005160506     d   F02Stapeso                    n   overlay(IndDspF : 53)
010100151019         //   -?Riemissione videata?
010200151019     d   ErrGenerico                   n   overlay(IndDspF : 99)
010300151019
010400151019       // -?Parametri ricevuti?
010500151020     d kpjba         e ds
010600151020     d TNSB56ds      e ds                  inz
010700151019
010800151019       // -?Estensione FIAR500F: trk "FAT"?
010900151019     d dAR5fat       e ds                  inz  qualified
011000151022
011100151022       // -?Tabella "TR" = Tipi Tariffa?
011200151022     d dsTR          e ds                  inz  qualified
011300151019
011400151019       //--------------------------------------------------------------
011500151019       //?Definizione variabili globali.                               ?
011600151019       //--------------------------------------------------------------
011700151019
011800151019       // -?Flags booleani?
011900151019     d $Fine           s               n   inz
012000151019     d $InzD01         s               n   inz(*on)
012100151020
012200151020       // -?Campi "data"?
012300151020     d wDate_Eur       s               d   inz  datfmt(*EUR)
012400151019
012500151019       //--------------------------------------------------------------
012600151020       //?Definizione prototipi/procedure.                             ?
012700151019       //--------------------------------------------------------------
012800151019
012900151019       // -?Reperimento dati utente?
013000151019     d TIBS34ds      e ds
013100151019      /copy gaitrasrc/srcProtoPR,TIBS34R
013200151019
013300151019       // -?Controllo/Decodifica cliente?
013400151019      /copy gaitrasrc/srcProtoPI,TIBS69R
013500151019      /copy gaitrasrc/srcProtoPR,TIBS69R
013600151022
013700151022       // -?Reperimento dati tabelle?
013800151022      /copy gaitrasrc/srcProtoPI,TRULTAB
013900151022      /copy gaitrasrc/srcProtoPR,TRULTAB
014000151020
014100151020       // -?Interrogazione Statistica cliente?
014200151020     d TISE60ds      e ds                  inz
014300151020     d   D60op0      e                     inz('VIS')
014400151020     d   D60op1      e                     inz('F06')
014500151020     d   D60f03      e                     inz(*off)
014600151020     d   D60err      e                     inz(*off)
014700151020     d   D60sce      e                     inz('1')
014800151020     d   D60stp      e                     inz('S')
014900151020     d   D60dpv      e                     inz('N')
015000151020     d   D60st1      e                     inz('N')
015100151020     d   D60st2      e                     inz('N')
015200151020     d   D60st3      e                     inz('N')
015300151020     d   D60st4      e                     inz('N')
015400151020     d   D60st5      e                     inz('N')
015500151020     d   D60st6      e                     inz('N')
015600151020     d   D60st7      e                     inz('N')
015700151020     d   D60clA      e                     inz('A')
015800151020     d   D60clB      e                     inz('B')
015900151020     d   D60clC      e                     inz('C')
016000151020     d   D60clT      e                     inz('T')
016100151020     d   D60clD      e                     inz('D')
016200151020     d   sk_KSC                       7p 0 overlay( D60ksc :  1 )
016300151020     d                                     dim( 28 )
016400151020     d                                     inz
016500151020     d tise65r         pr                  extpgm('TISE65R')
016600151020     d   kpjba                             likeds(KPJBA)
016700151019
016800151019       //--------------------------------------------------------------
016900151019       //?Definizione key-list.                                        ?
017000151019       //--------------------------------------------------------------
017100151019
017200151019       // -?File FIAR531L?
017300151019     d keyFIAR531    e ds                  extname( FIAR531C : *key )
017400151019     d                                     prefix( k_ )   inz
017500151019
017600151019       // -?File TITAS30C?
017700151019     d keyTITAS30    e ds                  extname( TITAS30C : *key )
017800151019     d                                     prefix( k_ )   inz
017900151019
018000151019       //--------------------------------------------------------------
018100151019       //?M A I N - L I N E                                            ?
018200151019       //--------------------------------------------------------------
018300151019
018400151019     c     *Entry        plist
018500151020     c                   parm                    KPJBA
018600151019
018700151019      /free
018800151019
018900151019       // -?Operazioni iniziali?
019000151019       exsr  sr_RoutInz;
019100151019
019200151019
019300151019       // -?Gestione Video?
019400151019       DoW  Not $Fine;
019500151019         exsr  sr_GesD01;
019600151019       EndDo;
019700151019
019800151019
019900151019       // -?Operazioni finali?
020000151019       exsr  sr_RoutEnd;
020100151019
020200151019       //--------------------------------------------------------------
020300151019       //?Operazioni iniziali.                                         ?
020400151019       //--------------------------------------------------------------
020500151019       BEGSR  sr_RoutInz;
020600151019
020700151019         // -?Impostazione chiusura?
020800151019         *inLR = *on;
020900151019
021000151019         // -?Reperimento dati job?
021100151019         exsr  sr_DatiJob;
021200151019
021300151019         // -?Eseguibile solo da Sede?
021400151019         if  DUTlpo <> 'S';
021500151019           exsr  sr_RoutEnd;
021600151019         endif;
021700151020
021800151020         T1Cpgm = SDSpgm;
021900151019
022000151019         // -?Verifica dei parametri ricevuti?
022100151019         //  ?(Numero spedizione mancante)?
022200151020         TNSB56ds = KPJBU;
022300151019         if  d56aas = *zero  or  d56lnp = *zero  or  d56nsp = *zero;
022400151019           exsr  sr_RoutEnd;
022500151019         endif;
022600151019
022700151019         // -?Reperimento dati da FIAR5/"FAT"?
022800151022         clear  dAR5fat;
022900151019         clear  keyFIAR531;
023000151019         k_AR5aas = d56aas;
023100151019         k_AR5lnp = d56lnp;
023200151019         k_AR5nrs = d56nrs;
023300151019         k_AR5nsp = d56nsp;
023400151019         k_AR5trd = 'FAT';
023500151019         chain  %kds( keyFIAR531 : 5 )  FIAR531C;
023600151019         if  Not %found(FIAR531C);
023700151019           exsr  sr_RoutEnd;
023800151019         endif;
023900151019
024000151019         dAR5fat = AR5uni;
024100151019
024200151019       ENDSR;
024300151019
024400151019       //--------------------------------------------------------------
024500151019       //?Reperimento Dati del job (Utente/Operativi).                 ?
024600151019       //--------------------------------------------------------------
024700151019       BEGSR  sr_DatiJob;
024800151019
024900151019         in(E) §AzUte;
025000151019         if NOT %error;
025100151019           in(E) §DatiUte;
025200151019         endif;
025300151019         if %error or RSut = *blanks;
025400151019           clear TIBS34ds;
025500151019           tibs34r ( tibs34ds );
025600151019           in §AzUte;
025700151019           in §DatiUte;
025800151019         endif;
025900151019
026000151019       ENDSR;
026100151019
026200151019       //--------------------------------------------------------------
026300151019       //?Richiesta / Controllo "BRTcode".                             ?
026400151019       //--------------------------------------------------------------
026500151019       BEGSR  sr_GesD01;
026600151019
026700151019         // -?Inizializzazione videata?
026800151019         if  $InzD01 = *on;
026900151019           exsr  sr_InzD01;
027000151019           $InzD01 = *off;
027100151019         endif;
027200151019
027300151019         // -?Emissione videata?
027400151019         write  SB56W01;
027500151019         write  SB56T01;
027600151019         exfmt  SB56D01;
027700151019
027800151019         clear  V1Dmsg;
027900151019         reset  ErrMessage;
028000151019         reset  ErrGenerico;
028100151019
028200151019         Select;
028300151020
028400151020           // -?F2=Statistiche cliente?
028500160506           When  dsp_aid = c_F02 or dsp_aid = c_f08;
028600151020             exsr  sr_TISE65R;
028700151019
028800151020           // -?F12=Ritorno?
028900151020           When  dsp_aid = c_F12;
029000151019             $Fine = *on;
029100151019
029200151019         EndSl;
029300151019
029400151019       ENDSR;
029500151019
029600151019       //--------------------------------------------------------------
029700151019       //?Inizializzazione videata D01.                                ?
029800151019       //--------------------------------------------------------------
029900151019       BEGSR  sr_InzD01;
030000151020
030100151020         // -?Posizionamento verticale window?
030200151022         clear  SB56W01;
030300151022         W1row  = 2;
030400151019
030500151020         // -?Inizializzazione videata principale (D01)?
030600151019         clear  SB56D01;
030700151019
030800151022         // ·?Impostazione dati ricevuti, relativi alla spedizione?
030900151019         D1Clnp = D56lnp;
031000151019         D1Cnrs = D56nrs;
031100151019         D1Cnsp = D56nsp;
031200151019         D1Caas = D56aas;
031300151019
031400151022         // ·?Impostazione codice Cliente?
031500151019         If  D56ksc <> *zero;
031600151019           D1Cksc = D56ksc;
031700151019         Else;
031800151020           if  not %open(TITAS30C);
031900151020             open  TITAS30C;
032000151022           endif;
032100151019           clear  keyTITAS30;
032200151019           k_TASaas = D56aas;
032300151019           k_TASlnp = D56lnp;
032400151019           k_TASnrs = D56nrs;
032500151019           k_TASnsp = D56nsp;
032600151019           k_TAStbl = D56tbl;
032700151019           if  D56tbl <> *blank;
032800151019             chain  %kds( keyTITAS30 : 5 )  TITAS30C;
032900151019           else;
033000151019             chain  %kds( keyTITAS30 : 4 )  TITAS30C;
033100151019           endif;
033200151019           if  %found(TITAS30C);
033300151019             D1Cksc = TASksc;
033400151019           endif;
033500151019         EndIf;
033600151019
033700151022         // ·?Decodifica Cliente?
033800151020         clear  TIBS69ds;
033900151020         I69sif = KNSIF;
034000151020         I69kcc = DUTkci;
034100151020         I69kac = D1Cksc;
034200151020         tibs69r( tibs69ds :
034300151020                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
034400151020         if  O69err <> *on;
034500151020           D1Drsc = ACOrag;
034600151020         endif;
034700151022
034800151022         // ·?Impostazione codice Tariffa?
034900151022         If  D56ctr <> *zero;
035000151022           D1Ctar = D56ctr;
035100151022         Else;
035200151022           if  not %open(TITAS30C);
035300151022             open  TITAS30C;
035400151022           endif;
035500151022           clear  keyTITAS30;
035600151022           k_TASaas = D56aas;
035700151022           k_TASlnp = D56lnp;
035800151022           k_TASnrs = D56nrs;
035900151022           k_TASnsp = D56nsp;
036000151022           k_TAStbl = D56tbl;
036100151022           if  D56tbl <> *blank;
036200151022             chain  %kds( keyTITAS30 : 5 )  TITAS30C;
036300151022           else;
036400151022             chain  %kds( keyTITAS30 : 4 )  TITAS30C;
036500151022           endif;
036600151022           if  %found(TITAS30C);
036700151022             D1Ctar = TASctr;
036800151022           endif;
036900151022         EndIf;
037000151022
037100151022         // ·?Decodifica Tariffa?
037200151022         If  D56ctr <> *zero;
037300151022           D1Dtar = D56dtr;
037400151022         Else;
037500151022           clear  dsTR;
037600151022           ds_TNTBE.TBEke1 = %subst( %editc( D1Ctar : 'X' ) : 1 : 1 );
037700151022           if  getTabella ( c_Tabel : 'TR'  : ds_TNTBE.TBEke1 :
037800151022                            *omit : *omit : *omit :
037900151022                            *omit : *omit :
038000151022                            *omit : *omit : *omit : *omit :
038100151022                            *omit : *omit : *omit : *omit :
038200151022                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
038300151022                          = *zero      AND
038400151022               ds_TNTBE.TBEatb = *blank;
038500151022             dsTR   = ds_TNTBE.TBEuni;
038600151022           endif;
038700151022           D1Dtar = dsTR.§TRdes;
038800151022         EndIf;
038900151019
039000151020         // -?Reperimento dati da FIAR5/"FAT"?
039100151022         //  ?(già fatto nella subr. "sr_RoutInz")?
039200151022         //clear  dAR5fat;
039300151022         //clear  keyFIAR531;
039400151022         //k_AR5aas = D56aas;
039500151022         //k_AR5lnp = D56lnp;
039600151022         //k_AR5nrs = D56nrs;
039700151022         //k_AR5nsp = D56nsp;
039800151022         //k_AR5trd = 'FAT';
039900151022         //chain  %kds( keyFIAR531 : 5 )  FIAR531C;
040000151022         //if  NOT %found( FIAR531C );
040100151022         //  leavesr;
040200151022         //endif;
040300151022         //dAR5fat = AR5uni;
040400151020
040500151021         // -?Impostazione dati di FIAR5/"FAT" a video?
040600151020         // ·?Peso Fatturato?
040700151020         D1CfpTas = dAR5fat.§AR5fpTas;
040800151020         D1CpkTas = dAR5fat.§AR5pkTas;
040900151021         // ·?Volume Fatturato?
041000151021         D1CfvTas = dAR5fat.§AR5fvTas;
041100151021         D1CvlTas = dAR5fat.§AR5vlTas;
041200151020         // ·?Data/Ora Tassazione?
041300151020         wDate_Eur = %date( dAR5fat.§AR5daTas : *iso );
041400151020         D1CdaTas  = %dec( wDate_Eur );
041500151020         D1ChmTas  = dAR5fat.§AR5hmTas;
041600160505         // ·?Anno/Mese Statistica Pesi  Solo se peso desunto
041601160505         If dAR5fat.§AR5fpTas = 'D' ;
041700160505            D1CstMAA  = ( dAR5fat.§AR5stAmm * 10000 ) + dAR5fat.§AR5stAaa;
041701160505         Endif;
041702160505         // ·?Anno/Mese Statistica Volumi  Solo se volume desunto
041703160505         If dAR5fat.§AR5fvTas = 'D' ;
041704160505            D1CstMAV  = ( dAR5fat.§AR5svAmm * 10000 ) + dAR5fat.§AR5svAaa;
041705160505         Endif;
041800151020         // ·?Profilo Tassazione?
041900151020         D1CprTas  = dAR5fat.§AR5prTas;
042000151020         // ·?PC Tassazione?
042100151020         D1CnjTas  = dAR5fat.§AR5njTas;
042200151021
042300151021         // -?Impostazione indicatori per attributi di visualizzazione?
042301160505         // - anno e mese statistica pesi se peso desunto
042400160505         VisStaPesi =    dAR5fat.§AR5fpTas = 'D';
042402160505         // - anno e mese statistica volumi se volume desunto
042403160505         VisStaVolumi  = dAR5fat.§AR5fvTas = 'D';
042404160506         // -?Impostazione indicatori per attributi di visualizzazione tasti funzionali
042405160506         // volume desunto e peso non desunto
042406160506         F08stavolume = dAR5fat.§AR5fvTas = 'D' and dAR5fat.§AR5fpTas <>'D';
042407160506         // volume non desunto e peso desunto
042408160506         F02stapeso   = dAR5fat.§AR5fvTas <>'D' and dAR5fat.§AR5fpTas = 'D';
042409160506         // volume desunto e peso desunto e anno e mese statistica uguali
042410160506         F02sta = dAR5fat.§AR5fvTas ='D' and dAR5fat.§AR5fpTas = 'D'
042500160506                  and D1CstMAA =  D1CstMAV;
042501160506         // volume desunto e peso desunto e anno e mese statistica diversi
042502160506         If  dAR5fat.§AR5fvTas ='D' and dAR5fat.§AR5fpTas = 'D'
042503160506             and D1CstMAA <> D1CstMAV;
042504160506             F02sta = *off ;
042505160506             F02stapeso = *on ;
042506160506             F08stavolume = *on ;
042507160506         Endif ;
042600151019       ENDSR;
042700151019
042800151019       //--------------------------------------------------------------
042900151020       //?Visualizzazione Statistiche Cliente.                         ?
043000151019       //--------------------------------------------------------------
043100151020       BEGSR  sr_TISE65R;
043200151019
043300151020         reset  TISE60ds;
043400151020         clear  sk_Ksc;
043500160506         sk_Ksc(01) = D1Cksc;
043501160506         if dsp_aid = c_F02 ;
043600160506            D60da2 = ( dAR5fat.§AR5stAaa * 100 ) + dAR5fat.§AR5stAmm;
043601160506         endif ;
043602160506         if dsp_aid = c_F08 ;
043603160506            D60da2 = ( dAR5fat.§AR5svAaa * 100 ) + dAR5fat.§AR5svAmm;
043604160506         endif ;
043700151020         D60dat = D60da2;
043800151022         D60ct1 = %editc( D1Ctar : 'X' );
043900151020
044000151020         KPJBU    = TISE60ds;
044100151020
044200151020         tise65r ( kpjba );
044300151020
044400151020         TISE60ds = KPJBU;
044500151020
044600151020         if  D60f03 = *on;
044700151020           $Fine = *on;
044800151020         endif;
044900151019
045000151019       ENDSR;
045100151020
045200151020       //--------------------------------------------------------------
045300151020       //?Operazioni finali.                                           ?
045400151020       //--------------------------------------------------------------
045500151020       BEGSR  sr_RoutEnd;
045600151022
045700151022         // -?Chiusura funzioni precedentemente aperte?
045800151022         cloTabella ( c_Tabel );
045900151020
046000151022         // -?Chiusura pgm?
046100151020         return;
046200151020
046300151020       ENDSR;
046400151019
046500151019      /end-free
