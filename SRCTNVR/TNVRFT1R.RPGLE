000100160422       //==============================================================
000200160422       //?Elenco Numeri Fattura MANCANTI - per Registro Iva.           ?
000300160422       //==============================================================
000400160422
000500160422       //--------------------------------------------------------------
000600160422       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700160422       //--------------------------------------------------------------
000800160422
000900160422     /*PRM dbgview(*source)
001000160422     /*END
001100160422
001200160422       //--------------------------------------------------------------
001300160422       //?Specifiche di controllo.                                     ?
001400160422       //--------------------------------------------------------------
001500160422
001600160422     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700160422     h dftactgrp(*no)
001800160422
001900160422       //--------------------------------------------------------------
002000160422       //?Dichiarazione file.                                          ?
002100160422       //--------------------------------------------------------------
002200160517
002300160517       // -?Organigramma?
002400160517     fAZORG01L  if   e           k disk
002500160422
002600160422       // -?Video?
002700160422     fTNVRFT1D  cf   e             workstn
002800160422     f                                     indds(IndDspF)
002900160422     f                                     infds(InfDspF)
003000160422
003100160422       //--------------------------------------------------------------
003200160422       //?Definizione costanti.                                        ?
003300160422       //--------------------------------------------------------------
003400160422
003500160422       // -?Tasti funzionali a video?
003600160422     d c_F01           c                   const(x'31')
003700160422     d c_F02           c                   const(x'32')
003800160422     d c_F03           c                   const(x'33')
003900160422     d c_F04           c                   const(x'34')
004000160422     d c_F05           c                   const(x'35')
004100160422     d c_F06           c                   const(x'36')
004200160422     d c_F07           c                   const(x'37')
004300160422     d c_F08           c                   const(x'38')
004400160422     d c_F09           c                   const(x'39')
004500160422     d c_F10           c                   const(x'3A')
004600160422     d c_F11           c                   const(x'3B')
004700160422     d c_F12           c                   const(x'3C')
004800160422     d c_F13           c                   const(x'B1')
004900160422     d c_F14           c                   const(x'B2')
005000160422     d c_F15           c                   const(x'B3')
005100160422     d c_F16           c                   const(x'B4')
005200160422     d c_F17           c                   const(x'B5')
005300160422     d c_F18           c                   const(x'B6')
005400160422     d c_F19           c                   const(x'B7')
005500160422     d c_F20           c                   const(x'B8')
005600160422     d c_F21           c                   const(x'B9')
005700160422     d c_F22           c                   const(x'BA')
005800160422     d c_F23           c                   const(x'BB')
005900160422     d c_F24           c                   const(x'BC')
006000160422     d c_Enter         c                   const(x'F1')
006100160422     d c_RollDown      c                   const(x'F4')
006200160422     d c_RollUp        c                   const(x'F5')
006300160422
006400160422       //--------------------------------------------------------------
006500160422       //?Definizione schiere.                                         ?
006600160422       //--------------------------------------------------------------
006700160422
006800160422       // -?Messaggi di errore?
006900160517     d sk_Msg          s             78    dim( 8)  ctdata  perrcd( 1)
007000160422
007100160422       //--------------------------------------------------------------
007200160422       //?Definizione aree dati.                                       ?
007300160422       //--------------------------------------------------------------
007400160422
007500160422       // -?Dati utente?
007600160422     d §AzUte        e ds                  extname(AZUTE00F)
007700160422     d                                     dtaara
007800160422     d §DatiUte      e ds                  extname(dDatiUte)
007900160422     d                                     dtaara
008000160422
008100160422       //--------------------------------------------------------------
008200160422       //?Definizione strutture dati.                                  ?
008300160422       //--------------------------------------------------------------
008400160422
008500160422       // -?InfDS?
008600160422     d InfDspF         ds
008700160422     d   dsp_aid             369    369a
008800160422
008900160422       // -?Indicatori su DspF?
009000160422     d IndDspF         ds                  inz
009100160422         // -?Emissione messaggio di errore?
009200160512     d   $ErrMessage                   n   overlay( IndDspF : 28 )
009300160422         // -?Posizionamento cursore & segnalazione errore?
009400160512     d   $PosCurDTI                    n   overlay( IndDspF : 51 )
009500160512     d   $PosCurDTF                    n   overlay( IndDspF : 52 )
009600160517     d   $PosCurLII                    n   overlay( IndDspF : 53 )
009700160517     d   $PosCurLIF                    n   overlay( IndDspF : 54 )
009800160517     d   $PosCurSST                    n   overlay( IndDspF : 55 )
009900160422         //   -?Riemissione videata?
010000160512     d   $ErrGenerico                  n   overlay( IndDspF : 99 )
010100160422
010200160422       // -?Status ds?
010300160422     d Status         sds
010400160422     d   SDSpgmName      *proc
010500160422
010600160422       // -?Parametri ricevuti?
010700160422     d KPJBA         e ds
010800160422
010900160422       //--------------------------------------------------------------
011000160422       //?Definizione variabili globali.                               ?
011100160422       //--------------------------------------------------------------
011200160422
011300160422       // -?Flags booleani?
011400160422     d $Fine           s               n   inz(*off)
011500160422     d $InzD01         s               n   inz(*on)
011600160422
011700160422       // -?Variabili per la gestione del video?
011800160422     d $Video          s              2    inz('D1')
011900160422
012000160422       //--------------------------------------------------------------
012100160422       //?Definizione prototipi procedure.                             ?
012200160422       //--------------------------------------------------------------
012300160422
012400160422       // -?Reperimento dati utente?
012500160422     d TIBS34ds      e ds                  inz
012600160422      /copy gaitrasrc/srcProtoPR,TIBS34R
012700160422
012800160422       // -?Controllo ed inversione date?
012900160422     d WLBdat          ds                  inz
013000160422     d   G08dat                       8  0 inz
013100160422     d   G08inv                       8  0 inz
013200160422     d   G08err                       1    inz('3')
013300160422     d   G08tgi                       5  0 inz
013400160422      /copy gaitrasrc/srcProtoPr,XSRDA8
013500160422
013600160422       // -?Richiamo diretto lavoro batch?
013700160422     d TNVRFT2ds       ds                  inz   qualified
013800160517     d   VRFTdti                      8s 0 inz
013900160422     d   VRFTdtf                      8s 0 inz
014000160517     d   VRFTlii                      3s 0 inz
014100160517     d   VRFTlif                      3s 0 inz
014200160517     d   VRFTsst                      1    inz
014300160422     d tnvrFT2r        pr                  extpgm('TNVRFT2R')
014400160422     d   kpjba                             likeds(KPJBA)
014500160422
014600160422       // -?Personalizzazione lavoro batch?
014700160422     d*// Bch09           pr                  extpgm('BCH09')
014800160422     d*//   kpjba                             likeds(KPJBA)
014900160422
015000160422       // -?Sottomissione lavoro batch?
015100160422      /copy gaitrasrc/srcProtoPR,BCH10
015200160422
015300160422       //--------------------------------------------------------------
015400160422       //?Definizione key-list.                                        ?
015500160422       //--------------------------------------------------------------
015600160422
015700160422
015800160422       //--------------------------------------------------------------
015900160422       //?Riepilogo indicatori utilizzati.                             ?
016000160422       //--------------------------------------------------------------
016100160422
016200160422
016300160422       //--------------------------------------------------------------
016400160422       //?M A I N - L I N E                                            ?
016500160422       //--------------------------------------------------------------
016600160422
016700160422     c     *Entry        plist
016800160422     c                   parm                    KPJBA
016900160422
017000160422      /free
017100160422
017200160422       // -?Operazioni iniziali?
017300160422       exsr  sr_RoutInz;
017400160422
017500160422       // -?Ciclo di gestione del file video?
017600160422       DoW  $Fine = *off;
017700160422
017800160422         select;
017900160422
018000160422           // -?Richiesta parametri iniziali?
018100160422           when $Video = 'D1';
018200160422             exsr  sr_GesD01;
018300160422
018400160422           // -?? ? ??
018500160422           other;
018600160422             $Fine = *on;
018700160422
018800160422         endsl;
018900160422
019000160422       EndDo;
019100160422
019200160422       // -?Operazioni finali?
019300160422       exsr sr_RoutEnd;
019400160422
019500160422       //--------------------------------------------------------------
019600160422       //?Operazioni iniziali.                                         ?
019700160422       //--------------------------------------------------------------
019800160422       BEGSR  sr_RoutInz;
019900160422
020000160422         // -?Impostazione chiusura?
020100160422         *inLR = *on;
020200160422
020300160422         // -?Reperimento dati job?
020400160422         exsr  sr_DatiJob;
020500160422
020600160422         // -?Impostazione nome programma a video?
020700160422         V1Tpgm = SDSpgmName;
020800160422
020900160422         // -?Pulizia parametri per Batch?
021000160422         clear  TNVRFT2ds;
021100160422
021200160422       ENDSR;
021300160422
021400160422       //--------------------------------------------------------------
021500160422       //?Reperimento Dati del job (Utente/Operativi).                 ?
021600160422       //--------------------------------------------------------------
021700160422       BEGSR  sr_DatiJob;
021800160422
021900160422         in(e) §AzUte;
022000160422         if NOT %error;
022100160422           in(e) §DatiUte;
022200160422         endif;
022300160422         if %error or RSut = *blank;
022400160422           tibs34r ( tibs34ds );
022500160422           in §AzUte;
022600160422           in §DatiUte;
022700160422         endif;
022800160422
022900160422       ENDSR;
023000160422
023100160422       //--------------------------------------------------------------
023200160422       //?Gestione videata D01.                                        ?
023300160422       //--------------------------------------------------------------
023400160422       BEGSR  sr_GesD01;
023500160422
023600160422         // -?Inizializzazione videata?
023700160422         if  $InzD01 = *on;
023800160422           exsr  sr_InzD01;
023900160422           $InzD01 = *off;
024000160422         endif;
024100160422
024200160422         // -?Emissione videata?
024300160422         write  VRFT1T01;
024400160422         exfmt  VRFT1D01;
024500160422
024600160422         clear  VIDmsg;
024700160512         reset  $ErrMessage;
024800160512         reset  $ErrGenerico;
024900160422
025000160512         Select;
025100160512
025200160512           // -?Errore: sistema informativo errato?
025300160512           When  %subst( KNSIF : 1 : 3 ) <> 'GAI';
025400160512             $Fine  = *on;
025500160422
025600160422           // -?F3=Fine?
025700160512           When  dsp_aid = c_F03;
025800160422             $Fine = *on;
025900160422
026000160422           // -?Invio / F6=Conferma?
026100160512           Other;
026200160422             exsr  sr_CtrD01;
026300160512             if  $ErrGenerico = *on;
026400160422               leavesr;
026500160422             endif;
026600160422             if  dsp_aid = c_F06;
026700160422               exsr  sr_SbmJob;
026800160422               $Fine = *on;
026900160422             endif;
027000160422
027100160422         ENDSL;
027200160422
027300160422       ENDSR;
027400160422
027500160422       //--------------------------------------------------------------
027600160422       //?Inizializzazione videata D01.                                ?
027700160422       //--------------------------------------------------------------
027800160422       BEGSR  sr_InzD01;
027900160422
028000160422         clear  VRFT1D01;
028100160512
028200160512         // -?Controllo del sistema informativo?
028300160512         if  %subst( KNSIF : 1 : 3 ) <> 'GAI';
028400160512           VIDmsg = sk_Msg(01);
028500160512           $ErrMessage  = *on;
028600160512           $ErrGenerico = *on;
028700160512           leavesr;
028800160512         endif;
028900160422
029000160422       ENDSR;
029100160422
029200160422       //--------------------------------------------------------------
029300160422       //?Controllo dati nella videata D01.                            ?
029400160422       //--------------------------------------------------------------
029500160422       BEGSR  sr_CtrD01;
029600160422
029700160422         // -?Spegnimento indicatori di posizionamento cursore?
029800160422         %subst( IndDspF : 50 ) = *off;
029900160422
030000160422         // -?Date spedizioni?
030100160422         if  VD1dti = *zero  and  VD1dtf = *zero;
030200160512           $PosCurDTI   = *on;
030300160512           $ErrMessage  = *on;
030400160512           $ErrGenerico = *on;
030500160512           VIDmsg = sk_Msg(02);
030600160422           leavesr;
030700160422         endif;
030800160422
030900160422         // -?Data spedizione iniziale?
031000160422         clear  WLBdat;
031100160422         if  VD1dti = *zero;
031200160512           $PosCurDTI   = *on;
031300160512           $ErrMessage  = *on;
031400160512           $ErrGenerico = *on;
031500160512           VIDmsg = sk_Msg(03);
031600160422           leavesr;
031700160422         endif;
031800160422         G08dat = VD1dti;
031900160422         xsrda8 ( WLBdat );
032000160422         if  G08err =  *on;
032100160512           $PosCurDTI   = *on;
032200160512           $ErrMessage  = *on;
032300160512           $ErrGenerico = *on;
032400160512           VIDmsg = sk_Msg(04);
032500160422           leavesr;
032600160422         endif;
032700160422         VD1dti  = G08dat;
032800160422         TNVRFT2ds.VRFTdti = G08inv;
032900160422
033000160422         // -?Data spedizione finale?
033100160422         clear  WLBdat;
033200160422         if  VD1dtf = *zero;
033300160512           $PosCurDTF   = *on;
033400160512           $ErrMessage  = *on;
033500160512           $ErrGenerico = *on;
033600160512           VIDmsg = sk_Msg(03);
033700160422           leavesr;
033800160422         endif;
033900160422         G08dat = VD1dtf;
034000160422         xsrda8 ( WLBdat );
034100160422         if  G08err =  *on;
034200160512           $PosCurDTF   = *on;
034300160512           $ErrMessage  = *on;
034400160512           $ErrGenerico = *on;
034500160512           VIDmsg = sk_Msg(04);
034600160422           leavesr;
034700160422         endif;
034800160422         VD1dtf  = G08dat;
034900160422         TNVRFT2ds.VRFTdtf = G08inv;
035000160422
035100160422         // -?Range date spedizioni?
035200160422         if  TNVRFT2ds.VRFTdti > TNVRFT2ds.VRFTdtf;
035300160512           $PosCurDTI   = *on;
035400160512           $ErrMessage  = *on;
035500160512           $ErrGenerico = *on;
035600160512           VIDmsg = sk_Msg(05);
035700160422           leavesr;
035800160422         endif;
035900160517
036000160517         // -?Libro Iva iniziale?
036100160517         clear  VD1liiD;
036200160517         if  VD1lii <> *zero;
036300160517           chain  ( VD1lii )  AZORG;
036400160517           if  %found(AZORG01L)   and   ORGfva = *blank;
036500160517             VD1liiD = ORGdes;
036600160517           else;
036700160517             $PosCurLII   = *on;
036800160517             $ErrMessage  = *on;
036900160517             $ErrGenerico = *on;
037000160517             VIDmsg = sk_Msg(06);
037100160517             leavesr;
037200160517           endif;
037300160517         endif;
037400160517
037500160517         // -?Libro Iva finale?
037600160517         clear  VD1lifD;
037700160517         if  VD1lif <> *zero;
037800160517           chain  ( VD1lif )  AZORG;
037900160517           if  %found(AZORG01L)   and   ORGfva = *blank;
038000160517             VD1lifD = ORGdes;
038100160517           else;
038200160517             $PosCurLIF   = *on;
038300160517             $ErrMessage  = *on;
038400160517             $ErrGenerico = *on;
038500160517             VIDmsg = sk_Msg(06);
038600160517             leavesr;
038700160517           endif;
038800160517         endif;
038900160517
039000160517         // -?Range libro Iva?
039100160517         if  VD1lii > VD1lif;
039200160517           $PosCurLII   = *on;
039300160517           $ErrMessage  = *on;
039400160517           $ErrGenerico = *on;
039500160517           VIDmsg = sk_Msg(07);
039600160517           leavesr;
039700160517         endif;
039800160517
039900160517         // -?Flag "Solo Totali"?
040000160517         if  VD1sst <> *blank  and  VD1sst <> 'S';
040100160517           $PosCurSST   = *on;
040200160517           $ErrMessage  = *on;
040300160517           $ErrGenerico = *on;
040400160517           VIDmsg = sk_Msg(08);
040500160517           leavesr;
040600160517         endif;
040700160422
040800160422       ENDSR;
040900160422
041000160422       //--------------------------------------------------------------
041100160422       //?Sottomissione lavoro batch                                   ?
041200160422       //--------------------------------------------------------------
041300160422       BEGSR  sr_SbmJob;
041400160422
041500160422         // -?Lancio?
041600160517         TNVRFT2ds.VRFTlii = VD1lii;
041700160517         TNVRFT2ds.VRFTlif = VD1lif;
041800160517         TNVRFT2ds.VRFTsst = VD1sst;
041900160422         kpjbu = TNVRFT2ds;
042000160422         kcoaz = 'VRF2';
042100160422         if  KNMUS = *all'1';
042200160422           tnvrFT2r ( kpjba );
042300160422         else;
042400160422           bch10 ( kpjba );
042500160422         endif;
042600160422
042700160422       ENDSR;
042800160422
042900160422       //--------------------------------------------------------------
043000160422       //?Operazioni finali.                                           ?
043100160422       //--------------------------------------------------------------
043200160422       BEGSR  sr_RoutEnd;
043300160422
043400160422         return;
043500160422
043600160422       ENDSR;
043700160422
043800160422      /end-free
043900160422
044000160422       //--------------------------------------------------------------
044100160422       //?Schiere a tempo di compilazione.
044200160422       //--------------------------------------------------------------
044300160422
044400160422** --?sk_Msg:?Messaggi di Errore?--------------------------------------------*
044500160512Funzione eseguibile SOLO in ambiente di SEDE: premere "Invio" per uscire       1
044600160512Impostare il periodo da elaborare                                              2
044700160512Data obbligatoria                                                              3
044800160512Data formalmente errata                                                        4
044900160512Data iniziale incongruente con data finale                                     5
045000160517Libro Iva inesistente in organigramma                                          6
045100160517Libro Iva iniziale incongruente con Libro Iva finale                           7
045200160517Immesso un valore errato - ammessi: " " = No, "S" = Sì                         8
