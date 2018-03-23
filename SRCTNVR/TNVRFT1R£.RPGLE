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
002200160422
002300160422       // -?Video?
002400160422     fTNVRFT1D  cf   e             workstn
002500160422     f                                     indds(IndDspF)
002600160422     f                                     infds(InfDspF)
002700160422
002800160422       //--------------------------------------------------------------
002900160422       //?Definizione costanti.                                        ?
003000160422       //--------------------------------------------------------------
003100160422
003200160422       // -?Tasti funzionali a video?
003300160422     d c_F01           c                   const(x'31')
003400160422     d c_F02           c                   const(x'32')
003500160422     d c_F03           c                   const(x'33')
003600160422     d c_F04           c                   const(x'34')
003700160422     d c_F05           c                   const(x'35')
003800160422     d c_F06           c                   const(x'36')
003900160422     d c_F07           c                   const(x'37')
004000160422     d c_F08           c                   const(x'38')
004100160422     d c_F09           c                   const(x'39')
004200160422     d c_F10           c                   const(x'3A')
004300160422     d c_F11           c                   const(x'3B')
004400160422     d c_F12           c                   const(x'3C')
004500160422     d c_F13           c                   const(x'B1')
004600160422     d c_F14           c                   const(x'B2')
004700160422     d c_F15           c                   const(x'B3')
004800160422     d c_F16           c                   const(x'B4')
004900160422     d c_F17           c                   const(x'B5')
005000160422     d c_F18           c                   const(x'B6')
005100160422     d c_F19           c                   const(x'B7')
005200160422     d c_F20           c                   const(x'B8')
005300160422     d c_F21           c                   const(x'B9')
005400160422     d c_F22           c                   const(x'BA')
005500160422     d c_F23           c                   const(x'BB')
005600160422     d c_F24           c                   const(x'BC')
005700160422     d c_Enter         c                   const(x'F1')
005800160422     d c_RollDown      c                   const(x'F4')
005900160422     d c_RollUp        c                   const(x'F5')
006000160422
006100160422       //--------------------------------------------------------------
006200160422       //?Definizione schiere.                                         ?
006300160422       //--------------------------------------------------------------
006400160422
006500160422       // -?Messaggi di errore?
006600160512     d sk_Msg          s             78    dim( 5)  ctdata  perrcd( 1)
006700160422
006800160422       //--------------------------------------------------------------
006900160422       //?Definizione aree dati.                                       ?
007000160422       //--------------------------------------------------------------
007100160422
007200160422       // -?Dati utente?
007300160422     d §AzUte        e ds                  extname(AZUTE00F)
007400160422     d                                     dtaara
007500160422     d §DatiUte      e ds                  extname(dDatiUte)
007600160422     d                                     dtaara
007700160422
007800160422       //--------------------------------------------------------------
007900160422       //?Definizione strutture dati.                                  ?
008000160422       //--------------------------------------------------------------
008100160422
008200160422       // -?InfDS?
008300160422     d InfDspF         ds
008400160422     d   dsp_aid             369    369a
008500160422
008600160422       // -?Indicatori su DspF?
008700160422     d IndDspF         ds                  inz
008800160422         // -?Emissione messaggio di errore?
008900160512     d   $ErrMessage                   n   overlay( IndDspF : 28 )
009000160422         // -?Posizionamento cursore & segnalazione errore?
009100160512     d   $PosCurDTI                    n   overlay( IndDspF : 51 )
009200160512     d   $PosCurDTF                    n   overlay( IndDspF : 52 )
009300160422         //   -?Riemissione videata?
009400160512     d   $ErrGenerico                  n   overlay( IndDspF : 99 )
009500160422
009600160422       // -?Status ds?
009700160422     d Status         sds
009800160422     d   SDSpgmName      *proc
009900160422
010000160422       // -?Parametri ricevuti?
010100160422     d KPJBA         e ds
010200160422
010300160422       //--------------------------------------------------------------
010400160422       //?Definizione variabili globali.                               ?
010500160422       //--------------------------------------------------------------
010600160422
010700160422       // -?Flags booleani?
010800160422     d $Fine           s               n   inz(*off)
010900160422     d $InzD01         s               n   inz(*on)
011000160422
011100160422       // -?Variabili per la gestione del video?
011200160422     d $Video          s              2    inz('D1')
011300160422
011400160422       //--------------------------------------------------------------
011500160422       //?Definizione prototipi procedure.                             ?
011600160422       //--------------------------------------------------------------
011700160422
011800160422       // -?Reperimento dati utente?
011900160422     d TIBS34ds      e ds                  inz
012000160422      /copy gaitrasrc/srcProtoPR,TIBS34R
012100160422
012200160422       // -?Controllo ed inversione date?
012300160422     d WLBdat          ds                  inz
012400160422     d   G08dat                       8  0 inz
012500160422     d   G08inv                       8  0 inz
012600160422     d   G08err                       1    inz('3')
012700160422     d   G08tgi                       5  0 inz
012800160422      /copy gaitrasrc/srcProtoPr,XSRDA8
012900160422
013000160422       // -?Richiamo diretto lavoro batch?
013100160422     d TNVRFT2ds       ds                  inz   qualified
013200160422     d   VRFTdti                      8s 0 inz
013300160422     d   VRFTdtf                      8s 0 inz
013400160422     d tnvrFT2r        pr                  extpgm('TNVRFT2R')
013500160422     d   kpjba                             likeds(KPJBA)
013600160422
013700160422       // -?Personalizzazione lavoro batch?
013800160422     d*// Bch09           pr                  extpgm('BCH09')
013900160422     d*//   kpjba                             likeds(KPJBA)
014000160422
014100160422       // -?Sottomissione lavoro batch?
014200160422      /copy gaitrasrc/srcProtoPR,BCH10
014300160422
014400160422       //--------------------------------------------------------------
014500160422       //?Definizione key-list.                                        ?
014600160422       //--------------------------------------------------------------
014700160422
014800160422
014900160422       //--------------------------------------------------------------
015000160422       //?Riepilogo indicatori utilizzati.                             ?
015100160422       //--------------------------------------------------------------
015200160422
015300160422
015400160422       //--------------------------------------------------------------
015500160422       //?M A I N - L I N E                                            ?
015600160422       //--------------------------------------------------------------
015700160422
015800160422     c     *Entry        plist
015900160422     c                   parm                    KPJBA
016000160422
016100160422      /free
016200160422
016300160422       // -?Operazioni iniziali?
016400160422       exsr  sr_RoutInz;
016500160422
016600160422       // -?Ciclo di gestione del file video?
016700160422       DoW  $Fine = *off;
016800160422
016900160422         select;
017000160422
017100160422           // -?Richiesta parametri iniziali?
017200160422           when $Video = 'D1';
017300160422             exsr  sr_GesD01;
017400160422
017500160422           // -?? ? ??
017600160422           other;
017700160422             $Fine = *on;
017800160422
017900160422         endsl;
018000160422
018100160422       EndDo;
018200160422
018300160422       // -?Operazioni finali?
018400160422       exsr sr_RoutEnd;
018500160422
018600160422       //--------------------------------------------------------------
018700160422       //?Operazioni iniziali.                                         ?
018800160422       //--------------------------------------------------------------
018900160422       BEGSR  sr_RoutInz;
019000160422
019100160422         // -?Impostazione chiusura?
019200160422         *inLR = *on;
019300160422
019400160422         // -?Reperimento dati job?
019500160422         exsr  sr_DatiJob;
019600160422
019700160422         // -?Impostazione nome programma a video?
019800160422         V1Tpgm = SDSpgmName;
019900160422
020000160422         // -?Pulizia parametri per Batch?
020100160422         clear  TNVRFT2ds;
020200160422
020300160422       ENDSR;
020400160422
020500160422       //--------------------------------------------------------------
020600160422       //?Reperimento Dati del job (Utente/Operativi).                 ?
020700160422       //--------------------------------------------------------------
020800160422       BEGSR  sr_DatiJob;
020900160422
021000160422         in(e) §AzUte;
021100160422         if NOT %error;
021200160422           in(e) §DatiUte;
021300160422         endif;
021400160422         if %error or RSut = *blank;
021500160422           tibs34r ( tibs34ds );
021600160422           in §AzUte;
021700160422           in §DatiUte;
021800160422         endif;
021900160422
022000160422       ENDSR;
022100160422
022200160422       //--------------------------------------------------------------
022300160422       //?Gestione videata D01.                                        ?
022400160422       //--------------------------------------------------------------
022500160422       BEGSR  sr_GesD01;
022600160422
022700160422         // -?Inizializzazione videata?
022800160422         if  $InzD01 = *on;
022900160422           exsr  sr_InzD01;
023000160422           $InzD01 = *off;
023100160422         endif;
023200160422
023300160422         // -?Emissione videata?
023400160422         write  VRFT1T01;
023500160422         exfmt  VRFT1D01;
023600160422
023700160422         clear  VIDmsg;
023800160512         reset  $ErrMessage;
023900160512         reset  $ErrGenerico;
024000160422
024100160512         Select;
024200160512
024300160512           // -?Errore: sistema informativo errato?
024400160512           When  %subst( KNSIF : 1 : 3 ) <> 'GAI';
024500160512             $Fine  = *on;
024600160422
024700160422           // -?F3=Fine?
024800160512           When  dsp_aid = c_F03;
024900160422             $Fine = *on;
025000160422
025100160422           // -?Invio / F6=Conferma?
025200160512           Other;
025300160422             exsr  sr_CtrD01;
025400160512             if  $ErrGenerico = *on;
025500160422               leavesr;
025600160422             endif;
025700160422             if  dsp_aid = c_F06;
025800160422               exsr  sr_SbmJob;
025900160422               $Fine = *on;
026000160422             endif;
026100160422
026200160422         ENDSL;
026300160422
026400160422       ENDSR;
026500160422
026600160422       //--------------------------------------------------------------
026700160422       //?Inizializzazione videata D01.                                ?
026800160422       //--------------------------------------------------------------
026900160422       BEGSR  sr_InzD01;
027000160422
027100160422         clear  VRFT1D01;
027200160512
027300160512         // -?Controllo del sistema informativo?
027400160512         if  %subst( KNSIF : 1 : 3 ) <> 'GAI';
027500160512           VIDmsg = sk_Msg(01);
027600160512           $ErrMessage  = *on;
027700160512           $ErrGenerico = *on;
027800160512           leavesr;
027900160512         endif;
028000160422
028100160422       ENDSR;
028200160422
028300160422       //--------------------------------------------------------------
028400160422       //?Controllo dati nella videata D01.                            ?
028500160422       //--------------------------------------------------------------
028600160422       BEGSR  sr_CtrD01;
028700160422
028800160422         // -?Spegnimento indicatori di posizionamento cursore?
028900160422         %subst( IndDspF : 50 ) = *off;
029000160422
029100160422         // -?Date spedizioni?
029200160422         if  VD1dti = *zero  and  VD1dtf = *zero;
029300160512           $PosCurDTI   = *on;
029400160512           $ErrMessage  = *on;
029500160512           $ErrGenerico = *on;
029600160512           VIDmsg = sk_Msg(02);
029700160422           leavesr;
029800160422         endif;
029900160422
030000160422         // -?Data spedizione iniziale?
030100160422         clear  WLBdat;
030200160422         if  VD1dti = *zero;
030300160512           $PosCurDTI   = *on;
030400160512           $ErrMessage  = *on;
030500160512           $ErrGenerico = *on;
030600160512           VIDmsg = sk_Msg(03);
030700160422           leavesr;
030800160422         endif;
030900160422         G08dat = VD1dti;
031000160422         xsrda8 ( WLBdat );
031100160422         if  G08err =  *on;
031200160512           $PosCurDTI   = *on;
031300160512           $ErrMessage  = *on;
031400160512           $ErrGenerico = *on;
031500160512           VIDmsg = sk_Msg(04);
031600160422           leavesr;
031700160422         endif;
031800160422         VD1dti  = G08dat;
031900160422         TNVRFT2ds.VRFTdti = G08inv;
032000160422
032100160422         // -?Data spedizione finale?
032200160422         clear  WLBdat;
032300160422         if  VD1dtf = *zero;
032400160512           $PosCurDTF   = *on;
032500160512           $ErrMessage  = *on;
032600160512           $ErrGenerico = *on;
032700160512           VIDmsg = sk_Msg(03);
032800160422           leavesr;
032900160422         endif;
033000160422         G08dat = VD1dtf;
033100160422         xsrda8 ( WLBdat );
033200160422         if  G08err =  *on;
033300160512           $PosCurDTF   = *on;
033400160512           $ErrMessage  = *on;
033500160512           $ErrGenerico = *on;
033600160512           VIDmsg = sk_Msg(04);
033700160422           leavesr;
033800160422         endif;
033900160422         VD1dtf  = G08dat;
034000160422         TNVRFT2ds.VRFTdtf = G08inv;
034100160422
034200160422         // -?Range date spedizioni?
034300160422         if  TNVRFT2ds.VRFTdti > TNVRFT2ds.VRFTdtf;
034400160512           $PosCurDTI   = *on;
034500160512           $ErrMessage  = *on;
034600160512           $ErrGenerico = *on;
034700160512           VIDmsg = sk_Msg(05);
034800160422           leavesr;
034900160422         endif;
035000160422
035100160422       ENDSR;
035200160422
035300160422       //--------------------------------------------------------------
035400160422       //?Sottomissione lavoro batch                                   ?
035500160422       //--------------------------------------------------------------
035600160422       BEGSR  sr_SbmJob;
035700160422
035800160422         // -?Lancio?
035900160422         kpjbu = TNVRFT2ds;
036000160422         kcoaz = 'VRF2';
036100160422         if  KNMUS = *all'1';
036200160422           tnvrFT2r ( kpjba );
036300160422         else;
036400160422           bch10 ( kpjba );
036500160422         endif;
036600160422
036700160422       ENDSR;
036800160422
036900160422       //--------------------------------------------------------------
037000160422       //?Operazioni finali.                                           ?
037100160422       //--------------------------------------------------------------
037200160422       BEGSR  sr_RoutEnd;
037300160422
037400160422         return;
037500160422
037600160422       ENDSR;
037700160422
037800160422      /end-free
037900160422
038000160422       //--------------------------------------------------------------
038100160422       //?Schiere a tempo di compilazione.
038200160422       //--------------------------------------------------------------
038300160422
038400160422** --?sk_Msg:?Messaggi di Errore?--------------------------------------------*
038500160512Funzione eseguibile SOLO in ambiente di SEDE: premere "Invio" per uscire       1
038600160512Impostare il periodo da elaborare                                              2
038700160512Data obbligatoria                                                              3
038800160512Data formalmente errata                                                        4
038900160512Data iniziale incongruente con data finale                                     5
