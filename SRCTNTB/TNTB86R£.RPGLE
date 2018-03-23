000100110421       //==============================================================
000200110421       //?Gestione tabella "KB4" (Clienti invio BL4 a Sede)            ?
000300110421       //==============================================================
000400110420
000500110420       //--------------------------------------------------------------
000600110420       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700110420       //--------------------------------------------------------------
000800110420
000900110420     /*PRM dbgview(*source)
001000110420     /*END
001100110420
001200110420       //--------------------------------------------------------------
001300110420       //?Specifiche di controllo.                                     ?
001400110420       //--------------------------------------------------------------
001500110420
001600110420     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700110420     h dftactgrp(*no) actgrp('QILE')
001800110420
001900110420       //--------------------------------------------------------------
002000110420       //?Dichiarazione file.                                          ?
002100110420       //--------------------------------------------------------------
002200110420
002300110420       // -?Tabella "KB4"?
002400110420     fTNTBE01L  Uf A e           k disk
002500110420
002600110420       // -?Video Gestione?
002700110420     fTNTB86D   cf   e             workstn
002800110420     f                                     indds(IndDspF)
002900110420     f                                     infds(InfDspF)
003000110420
003100110420       //--------------------------------------------------------------
003200110420       //?Definizione costanti.                                        ?
003300110420       //--------------------------------------------------------------
003400110420
003500110420       // -?Codice tabella in gestione?
003600110420     d c_Tab           c                   const('KB4')
003700110420
003800110420       // -?Costante per controllo "caratteri solo numerici"?
003900110421     d c_Digits        c                   const('0123456789')
004000110420
004100110420       // -?Tasti funzionali a video?
004200110420     d c_F01           c                   const(x'31')
004300110420     d c_F02           c                   const(x'32')
004400110420     d c_F03           c                   const(x'33')
004500110420     d c_F04           c                   const(x'34')
004600110420     d c_F05           c                   const(x'35')
004700110420     d c_F06           c                   const(x'36')
004800110420     d c_F07           c                   const(x'37')
004900110420     d c_F08           c                   const(x'38')
005000110420     d c_F09           c                   const(x'39')
005100110420     d c_F10           c                   const(x'3A')
005200110420     d c_F11           c                   const(x'3B')
005300110420     d c_F12           c                   const(x'3C')
005400110420     d c_F13           c                   const(x'B1')
005500110420     d c_F14           c                   const(x'B2')
005600110420     d c_F15           c                   const(x'B3')
005700110420     d c_F16           c                   const(x'B4')
005800110420     d c_F17           c                   const(x'B5')
005900110420     d c_F18           c                   const(x'B6')
006000110420     d c_F19           c                   const(x'B7')
006100110420     d c_F20           c                   const(x'B8')
006200110420     d c_F21           c                   const(x'B9')
006300110420     d c_F22           c                   const(x'BA')
006400110420     d c_F23           c                   const(x'BB')
006500110420     d c_F24           c                   const(x'BC')
006600110420     d c_Enter         c                   const(x'F1')
006700110420     d c_RollDown      c                   const(x'F4')
006800110420     d c_RollUp        c                   const(x'F5')
006900110420
007000110420       //--------------------------------------------------------------
007100110420       //?Definizione schiere.                                         ?
007200110420       //--------------------------------------------------------------
007300110420
007400110420       // -?Messaggi di errore?
007500110421     d $Msg            s             78    dim( 7)  ctdata  perrcd(01)
007600110420
007700110420       //--------------------------------------------------------------
007800110420       //?Definizione aree dati.                                       ?
007900110420       //--------------------------------------------------------------
008000110420
008100110420       // -?Dati utente?
008200110420     d §AzUte        e ds                  extname(AZUTE00F)
008300110420     d                                     dtaara
008400110420     d §DatiUte      e ds                  extname(dDatiUte)
008500110420     d                                     dtaara
008600110420
008700110420       //--------------------------------------------------------------
008800110420       //?Definizione strutture dati.                                  ?
008900110420       //--------------------------------------------------------------
009000110420
009100110420       // -?Status ds?
009200110420     d Status         sds
009300110420     d  SDSpgm           *proc
009400110420
009500110420       // -?InfDS?
009600110420     d InfDspF         ds
009700110420     d   dsp_aid             369    369a
009800110420
009900110420       // -?Indicatori su DspF?
010000110420     d IndDspF         ds                  inz
010100110420         // -?Abilitazione tasti funzionali?
010200110421     d   F3Attivo                      n   overlay(IndDspF : 03)
010300110420     d   F5Attivo                      n   overlay(IndDspF : 05)
010400110420     d   F6Attivo                      n   overlay(IndDspF : 06)
010500110420     d   F10Attivo                     n   overlay(IndDspF : 10)
010600110420     d   F16Attivo                     n   overlay(IndDspF : 16)
010700110420         // -?Emissione messaggio di errore?
010800110420     d   ErrMessage                    n   overlay(IndDspF : 28)
010900110420         // -?Indicatori per Attibuti di visualizzazione?
011000110420         // -?Posizionamento cursore & Segnalazione errore?
011100110421     d   PosCurKSC                     n   overlay(IndDspF : 51)
011200110421     d   PosCurTRK                     n   overlay(IndDspF : 52)
011300110421     d   PosCurRAG                     n   overlay(IndDspF : 53)
011400110421     d   PosCurTRD                     n   overlay(IndDspF : 54)
011500110420         // -?Riemissione videata?
011600110420     d   ErrGenerico                   n   overlay(IndDspF : 99)
011700110420
011800110420       // -?Parametri ricevuti?
011900110420     d KPJBA         e ds
012000110420
012100110420       // -?Dati record principale della tabella?
012200110420     d TNTBEds       e ds                  inz  extname(TNTBE00F)
012300110420     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
012400110420     d                                          prefix(TBX:3)
012500110421
012600110421       // -?Tabella in manutenzione?
012700110421     d dKB4          e ds                  inz
012800110420
012900110420       //--------------------------------------------------------------
013000110420       //?Definizione variabili globali.                               ?
013100110420       //--------------------------------------------------------------
013200110420
013300110420       // -?Flags booleani?
013400110420     d $Fine           s               n   inz
013500110420     d $InzD01         s               n   inz(*on)
013600110420     d $InzD02         s               n   inz(*on)
013700110420     d $InzD03         s               n   inz(*on)
013800110420     d $InzW01         s               n   inz(*on)
013900110420     d $Annullamento   s               n   inz
014000110420     d $Immissione     s               n   inz
014100110420     d $Ripristino     s               n   inz
014200110421     d $Called         s               n   inz
014300110420
014400110420       // -?Variabili per la gestione del video?
014500110420     d $Video          s              2    inz('D1')
014600110421     d $VideoPrec      s                   like($Video)  inz
014700110421
014800110421       // -?Codice cliente eventualmente ricevuto?
014900110421     d W0Cksc          s                   like(V1Cksc)  inz
015000110421       // -?Trk eventualmente ricevuto?
015100110421     d W0Ctrk          s                   like(V1Ctrk)  inz
015200110420
015300110420       // -?Campi di comodo?
015400110420     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
015500110420
015600110420       //--------------------------------------------------------------
015700110420       //?Definizione prototipi procedure.                             ?
015800110420       //--------------------------------------------------------------
015900110420
016000110420       // -?Reperimento dati utente?
016100110420     d TIBS34ds      e ds                  inz
016200110420      /copy gaitrasrc/srcProtoPR,TIBS34R
016300110420
016400110420       // -?Ricerca/Controllo tabelle (TNTBE)?
016500110420     d TIBS02ds      e ds
016600110420     d   T02mod      e                     inz('R')
016700110420     d   T02cod      e                     inz(c_Tab)
016800110420      /copy gaitrasrc/srcProtoPR,TIBS02R
016900110420
017000110420       // -?Parametri per pgm controllo/decodifica cliente?
017100110420      /copy gaitrasrc/srcProtoPI,TIBS69R
017200110420       // -?Controllo/Decodifica cliente?
017300110420      /copy gaitrasrc/srcProtoPR,TIBS69R
017400110420
017500110420       //--------------------------------------------------------------
017600110420       //?Definizione key-list.                                        ?
017700110420       //--------------------------------------------------------------
017800110420
017900110420       // -?File TNTBE01L?
018000110420     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
018100110420     d                                     prefix(k_)   inz
018200110420
018300110420       //--------------------------------------------------------------
018400110420       //?Riepilogo indicatori utilizzati.                             ?
018500110420       //--------------------------------------------------------------
018600110420       //--------------------------------------------------------------
018700110420
018800110420       //--------------------------------------------------------------
018900110420       //?M A I N - L I N E                                            ?
019000110420       //--------------------------------------------------------------
019100110420
019200110420     c     *Entry        plist
019300110420     c                   parm                    KPJBA
019400110420
019500110420      /free
019600110420
019700110420       // -?Operazioni iniziali?
019800110420       exsr sr_RoutInz;
019900110420
020000110420       // -?Gestione videate?
020100110420       DOW  $Fine = *off;
020200110420
020300110420         select;
020400110420
020500110420           // -?Gestione videata D1?
020600110420           //  ?(richiesta chiave tabella)?
020700110420           when $Video = 'D1';
020800110420             exsr sr_GesD01;
020900110420
021000110420           // -?Gestione videata D2?
021100110420           //  ?(manutenzione dati tabella "KB4")?
021200110420           when $Video = 'D2';
021300110420             exsr sr_GesD02;
021400110420
021500110420           // -?Gestione videata D3?
021600110420           //  ?(manutenzione dati da COPIARE in tab. "KB4")?
021700110420           when $Video = 'D3';
021800110420             exsr sr_GesD03;
021900110420
022000110420           // -?Gestione videata W1?
022100110420           //  ?(richiesta dati per trasmissione record)?
022200110420           when $Video = 'W1';
022300110420             exsr sr_GesW01;
022400110420
022500110420           // -?Gestione videata ????
022600110420           other;
022700110420             $Fine = *on;
022800110420
022900110420         endsl;
023000110420
023100110420       ENDDO;
023200110420
023300110420       // -?Operazioni finali?
023400110420       exsr sr_RoutEnd;
023500110420
023600110420       //--------------------------------------------------------------
023700110420       //?Operazioni iniziali.                                         ?
023800110420       //--------------------------------------------------------------
023900110420       BEGSR  sr_RoutInz;
024000110420
024100110420         *inLR = *on;
024200110420
024300110420         // -?Reperimento dati job?
024400110420         exsr  sr_DatiJob;
024500110420
024600110420         // -?Impostazione nome programma a video?
024700110420         V1Tpgm = SDSpgm;
024800110420
024900110420         // -?Aggancio dati generali della tabella in esame?
025000110420         clear  k_TBEcod;
025100110420         k_TBEke1 = *zero;
025200110420         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
025300110420                = c_Tab;
025400110420         clear  k_TBEke2;
025500110420         clear  k_TBElin;
025600110420         k_TBEsif = KNSIF;
025700110420         chain(n)  %kds(k05tntbe01)  TNTBE000;
025800110420         if  not  %found(TNTBE01L);
025900110420           clear  k_TBEsif;
026000110420           chain(n)  %kds(k05tntbe01)  TNTBE000;
026100110420         endif;
026200110420         if  %found(TNTBE01L);
026300110420           xTNTBEds = TNTBEds;
026400110420         else;
026500110420           clear  xTNTBEds;
026600110420         endif;
026700110420
026800110420         // -?Impostazione iniziale / pulizia dei campi chiave?
026900110420         k_TBEcod = c_Tab;
027000110420         clear  k_TBEke1;
027100110420         clear  k_TBEke2;
027200110420         clear  k_TBElin;
027300110420         clear  k_TBEsif;
027400110421
027500110421         // -?Verifica se *pgm richiamato:?
027600110421         //  ?test delle prime 8 posizioni della KPJBA per sapere se il?
027700110421         //  ?pgm è stato richiamato da un altro pgm o da menù tabelle.?
027800110421         $Called = (%subst( KPJBU : 1 : %len(V1Cksc) + %len(V1Ctrk) )
027900110421                   <> *blank);
028000110421         clear  W0Cksc;
028100110421         clear  W0Ctrk;
028200110421         if  $Called;
028300110421           if  %check( c_Digits : %subst( KPJBU : 1 :
028400110421                                          %len(V1Cksc) ) ) = *zero;
028500110421             W0Cksc = %subst( KPJBU : 1 : %len(V1Cksc) );
028600110421           endif;
028700110421           if  %subst( KPJBU : %len(V1Cksc) + 1 :
028800110421                                            %len(V1Ctrk) )  <> *blank;
028900110421             W0Ctrk = %subst( KPJBU : %len(V1Cksc)+1 : %len(V1Ctrk) );
029000110421           endif;
029100110421         endif;
029200110420
029300110420       ENDSR;
029400110420
029500110420       //--------------------------------------------------------------
029600110420       //?Reperimento Dati del job (Utente/Operativi).                 ?
029700110420       //--------------------------------------------------------------
029800110420       BEGSR  sr_DatiJob;
029900110420
030000110420         in(E) §AzUte;
030100110420         if NOT %error;
030200110420           in(E) §DatiUte;
030300110420         endif;
030400110420         if %error or RSut = *blanks;
030500110420           clear TIBS34ds;
030600110420           tibs34r ( tibs34ds );
030700110420           in §AzUte;
030800110420           in §DatiUte;
030900110420         endif;
031000110420
031100110420       ENDSR;
031200110420
031300110420       //--------------------------------------------------------------
031400110420       //?Gestione videata D01                                         ?
031500110420       //--------------------------------------------------------------
031600110420       BEGSR  sr_GesD01;
031700110420
031800110420         // -?Inizializzazione videata?
031900110420         if  $InzD01 = *on;
032000110420           exsr  sr_InzD01;
032100110420           $InzD01 = *off;
032200110420         endif;
032300110421
032400110421         // -?SE ricevuti codice cliente & trk?
032500110421         //  ?NON si emette la 1ª videata?
032600110421         if  $Called  and  (W0Cksc > *zero  and  W0Ctrk <> *blank);
032700110421           exsr  sr_CtrD01;
032800110421           if  Not ErrGenerico;
032900110421             $Video  = 'D2';
033000110421             $InzD02 = *on;
033100110421             leavesr;
033200110421           else;
033300110421             clear W0Cksc;
033400110421             clear W0Ctrk;
033500110421           endif;
033600110421         endif;
033700110420
033800110420         // -?Emissione Testata & Piede?
033900110420         write  TB86T01;
034000110420         write  TB86P01;
034100110420
034200110420         // -?Emissione videata?
034300110420         exfmt  TB86D01;
034400110420
034500110420         reset  ErrMessage;
034600110420         reset  ErrGenerico;
034700110420         clear  V1Dmsg;
034800110420
034900110420         SELECT;
035000110420
035100110420           // -?F3=Fine?
035200110420           WHEN  dsp_aid = c_F03;
035300110420             $Fine = *on;
035400110420
035500110420           // -?Invio?
035600110420           OTHER;
035700110420             exsr sr_CtrD01;
035800110420             if  ErrGenerico = *on;
035900110420               leavesr;
036000110420             endif;
036100110420             $Video    = 'D2';
036200110420             $InzD02   = *on;
036300110420
036400110420         ENDSL;
036500110420
036600110420       ENDSR;
036700110420
036800110420       //--------------------------------------------------------------
036900110420       //?Inizializzazione videata D01                                 ?
037000110420       //--------------------------------------------------------------
037100110420       BEGSR  sr_InzD01;
037200110420
037300110420         clear  TB86D01;
037400110421
037500110421         // -?Se è stato richiamato con un codice cliente & un trk,?
037600110421         //  ?si impostano tali codici a video?
037700110421         if  $Called;
037800110421           if  W0Cksc > *zero;
037900110421             V1Cksc = W0Cksc;
038000110421           endif;
038100110421           if  W0Ctrk <> *blank;
038200110421             V1Ctrk = W0Ctrk;
038300110421           endif;
038400110421         else;
038500110421           V1Cksc = '?';
038600110421         endif;
038700110420
038800110420       ENDSR;
038900110420
039000110420       //--------------------------------------------------------------
039100110420       //?Controllo dati immessi nella videata D01                     ?
039200110420       //--------------------------------------------------------------
039300110420       BEGSR  sr_CtrD01;
039400110420
039500110420         %subst(IndDspF : 28) = *off;
039600110420
039700110421         clear  V1Dksc;
039800110420         SELECT;
039900110420
040000110420           // -?Ricerca codice cliente?
040100110420           WHEN  %scan('?' : V1Cksc) > *zeros;
040200110420             reset TIBS02ds;
040300110420             //T02mod = 'R';    ?(già così)?
040400110420             //T02cod = c_Tab;  ?(già così)?
040500110420             T02sif = KNSIF;
040600110420             TNTBE_RicercaControllo ( kpjba : tibs02ds );
040700110420             if  T02err = *blanks;
040800110420               V1Cksc = %subst(T02ke1 : 1 : %len(V1Cksc));
040900110421               V1Ctrk = %subst(T02ke2 : 1 : %len(V1Ctrk));
041000110420               ErrGenerico = *on;
041100110420               leavesr;
041200110420             else;
041300110420               ErrMessage  = *on;
041400110420               ErrGenerico = *on;
041500110420               PosCurKSC   = *on;
041600110420               V1Dmsg = T02msg;
041700110420               leavesr;
041800110420             endif;
041900110420
042000110420           // -?Controllo immissione codice cliente?
042100110420           WHEN  V1Cksc = *blank  or  V1Cksc  = *zero;
042200110420             ErrMessage  = *on;
042300110420             ErrGenerico = *on;
042400110420             PosCurKSC   = *on;
042500110420             V1Dmsg = $Msg(01);
042600110420             leavesr;
042700110420
042800110420           // -?Controllo immissione solo caratteri numerici?
042900110421           WHEN  %check(c_Digits : V1Cksc) > *zero;
043000110420             ErrMessage  = *on;
043100110420             ErrGenerico = *on;
043200110420             PosCurKSC   = *on;
043300110420             V1Dmsg = $Msg(02);
043400110420             leavesr;
043500110420
043600110420         ENDSL;
043700110420
043800110420         // -?Controllo/decodifica cliente?
043900110420         clear  TIBS69ds;
044000110420         I69kcc = DUTkci;
044100110420         I69kac = %int( V1Cksc );
044200110420         I69sif = knsif;
044300110420         tibs69r(tibs69ds : ds_cnaco : ds_cnind : ds_cnclp : ds_fncls);
044400110420         if  O69err = *on;
044500110420           V1Dmsg = $Msg(03);
044600110420           ErrMessage  = *on;
044700110420           ErrGenerico = *on;
044800110420           PosCurKSC   = *on;
044900110420           leavesr;
045000110420         endif;
045100110420         V1Dksc = %subst(ACOrag : 1 : %len(V1Dksc));
045200110420
045300110420         // -?NON ammessi nuovi record per cliente annullato?
045400110420         reset  TNTBE000;
045500110420         k_TBEcod = c_Tab;
045600110420         k_TBEke1 = V1Cksc;
045700110421         k_TBEke2 = V1Ctrk;
045800110420         clear  k_TBElin;
045900110420         clear  k_TBEsif;
046000110421         setll  %kds(k05tntbe01)  TNTBE000;
046100110420         $Immissione = (NOT %equal(TNTBE01L));
046200110420         reset  $Ripristino;
046300110420         if  NOT  %equal(TNTBE01L)  and  ACOflg <> *blanks;
046400110420           V1Dmsg = $Msg(04);
046500110420           ErrMessage  = *on;
046600110420           ErrGenerico = *on;
046700110420           PosCurKSC   = *on;
046800110420           leavesr;
046900110420         endif;
047000110421
047100110421         // -?Controllo immissione trk?
047200110421         if  V1Ctrk = *blank;
047300110421           ErrMessage  = *on;
047400110421           ErrGenerico = *on;
047500110421           PosCurTRK   = *on;
047600110421           V1Dmsg = $Msg(05);
047700110421           leavesr;
047800110421         endif;
047900110420
048000110420       ENDSR;
048100110420
048200110420       //--------------------------------------------------------------
048300110420       //?Gestione videata D02                                         ?
048400110420       //--------------------------------------------------------------
048500110420       BEGSR  sr_GesD02;
048600110420
048700110420         // -?Inizializzazione videata?
048800110420         if  $InzD02 = *on;
048900110420           exsr sr_InzD02;
049000110420           $InzD02 = *off;
049100110420         endif;
049200110420
049300110420         // -?Emissione Testata e Piede con tasti funzionali abilitati?
049400110420         if  errGenerico = *off;
049500110420           write  TB86T01;
049600110420           write  TB86D01;
049700110420           write  Protect;
049800110420           write  TB86P02;
049900110420         endif;
050000110420
050100110420         // -?Emissione videata?
050200110420         exfmt  TB86D02;
050300110420         reset  ErrMessage;
050400110420         reset  ErrGenerico;
050500110420         clear  V1Dmsg;
050600110420         reset  $Annullamento;
050700110420
050800110420         SELECT;
050900110420
051000110420           // -?F3=Fine?
051100110420           WHEN  dsp_aid = c_F03;
051200110420             unlock TNTBE01L;
051300110420             $Fine = *on;
051400110420
051500110420           // -?F12=Ritorno?
051600110420           WHEN  dsp_aid = c_F12;
051700110420             unlock TNTBE01L;
051800110420             reset  $Video;
051900110420             clear  V1Topz;
052000110421             if  $Called;
052100110421               $Fine = *on;
052200110421             endif;
052300110420
052400110420           // -?Invio;?
052500110420           //  ?F5=Ripristino; F6=Conferma; F16=Annullamento;?
052600110420           //  ?F10=Copia?
052700110420           OTHER;
052800110420             exsr  sr_CtrD02;
052900110420             if  ErrGenerico;
053000110420               leavesr;
053100110420             endif;
053200110420             Select;
053300110420               // -?F5=Ripristino; F6=Conferma; F16=Annullamento?
053400110420               When  dsp_aid = c_F05  or
053500110420                     dsp_aid = c_F06  or
053600110420                     dsp_aid = c_F16;
053700110420                 $Annullamento = (dsp_aid = c_F16);
053800110421                 $VideoPrec = $Video;
053900110421                 $Video = 'W1';
054000110420                 reset  $InzW01;
054100110420               // -?F10=Copia?
054200110420               When  dsp_aid = c_F10;
054300110420                 unlock TNTBE01L;
054400110420                 $Video  = 'D3';
054500110420                 reset  $InzD03;
054600110420             EndSl;
054700110420
054800110420         ENDSL;
054900110420
055000110420       ENDSR;
055100110420
055200110420       //--------------------------------------------------------------
055300110420       //?Inizializzazione videata D02                                 ?
055400110420       //--------------------------------------------------------------
055500110420       BEGSR  sr_InzD02;
055600110420
055700110420         // -?Spegnimento di tutti gli indicatori?
055800110420         IndDspF = *off;
055900110420
056000110420         // -?Pulizia videata?
056100110420         clear  TB86D02;
056200110420
056300110421         // -?Reperimento dati da caricare a video?
056400110421         chain  %kds(k05tntbe01)  TNTBE000;
056500110421
056600110421         // -?Impostazione tipo manutenzione?
056700110421         $Immissione = (Not %found(TNTBE01L));
056800110421         $Ripristino = (%found(TNTBE01L)  and  TBEatb <> *blank);
056900110421         reset  $Annullamento;
057000110421
057100110421         select;
057200110421           when  $Immissione;
057300110421             V1Topz = 'IMMISSIONE';
057400110421           when  $Ripristino;
057500110421             V1Topz = 'RIPRISTINO';
057600110421           other;
057700110421             V1Topz = ' MODIFICA ';
057800110421         endsl;
057900110421
058000110421         // -?Caricamento dati a video?
058100110421         If  $Immissione;
058200110421           clear  dKB4;
058300110421           V2Crag = ACOrag;
058400110421           select;
058500110421             when  V1Ctrk = '8';
058600110421               V2Ctrd = 'NOTE PARTENZA 1';
058700110421             when  V1Ctrk = '9';
058800110421               V2Ctrd = 'NOTE PARTENZA 2';
058900110421           endsl;
059000110421         Else;
059100110421           dKB4 = TBEuni;
059200110421           V2Crag = §KB4rag;
059300110421           V2Ctrd = §KB4trd;
059400110421         EndIf;
059500110420
059600110420         // -?(Ri)Abilitazione tasti funzionali?
059700110421         exsr  sr_AbilFxxD02;
059800110420
059900110420       ENDSR;
060000110420
060100110420       //--------------------------------------------------------------
060200110421       //?Abilitazione tasti funzionali in P02 (per D02).              ?
060300110420       //--------------------------------------------------------------
060400110421       BEGSR  sr_AbilFxxD02;
060500110421
060600110421         // ->?F3 = Fine?
060700110421         F3Attivo = (NOT $Called);
060800110420
060900110421         // -?F5=Ripristino?
061000110420         F5Attivo  = (NOT $Immissione  and  $Ripristino);
061100110420
061200110421         // -?F6=Conferma?
061300110420         F6Attivo  = (NOT F5Attivo);
061400110420
061500110421         // -?F10=Copia?
061600110420         F10Attivo = (NOT $Immissione  and  NOT $Ripristino);
061700110420
061800110421         // -?F16=Annullamento?
061900110420         F16Attivo = (NOT $Immissione  and  NOT $Ripristino);
062000110420
062100110420       ENDSR;
062200110421
062300110421       //--------------------------------------------------------------
062400110421       //?Controllo dati immessi nella videata D02                     ?
062500110421       //--------------------------------------------------------------
062600110421       BEGSR  sr_CtrD02;
062700110421
062800110421         %subst(IndDspF : 50) = *off;
062900110421
063000110421         // -?AUTOMATISMI - SE NON annullamento:?
063100110421         If  dsp_aid <> c_F16;
063200110421
063300110421           // -?Ragione sociale mancante: proposta quella anagrafica?
063400110421           if  V2Crag  = *blank;
063500110421             V2Crag = ACOrag;
063600110421           endif;
063700110421
063800110421           // -?Descriz. trk estens. bolla mancante: proposto default?
063900110421           if  V2Ctrd  = *blank;
064000110421             select;
064100110421               when  V1Ctrk = '8';
064200110421                 V2Ctrd = 'NOTE PARTENZA 1';
064300110421               when  V1Ctrk = '9';
064400110421                 V2Ctrd = 'NOTE PARTENZA 2';
064500110421               other;
064600110421                 ErrGenerico = *on;
064700110421                 ErrMessage  = *on;
064800110421                 PosCurTRD   = *on;
064900110421                 V1Dmsg = $Msg(06);
065000110421                 leavesr;
065100110421             endsl;
065200110421           endif;
065300110421
065400110421         EndIf;
065500110421
065600110421       ENDSR;
065700110420
065800110420       //--------------------------------------------------------------
065900110420       //?Gestione videata D03                                         ?
066000110420       //--------------------------------------------------------------
066100110420       BEGSR  sr_GesD03;
066200110420
066300110421         // -?Inizializzazione videata?
066400110420         if  $InzD03 = *on;
066500110420           exsr sr_InzD03;
066600110420           $InzD03 = *off;
066700110420         endif;
066800110420
066900110421         // -?Emissione Testata e Piede con tasti funzionali abilitati?
067000110420         if  errGenerico = *off;
067100110420           write  TB86T01;
067200110420           write  TB86D01;
067300110420           write  Protect;
067400110420           write  TB86P03;
067500110420         endif;
067600110420
067700110421         // -?Emissione videata?
067800110420         exfmt  TB86D03;
067900110421
068000110420         reset  ErrGenerico;
068100110421         reset  ErrMessage;
068200110420         clear  V1Dmsg;
068300110420
068400110420         SELECT;
068500110420
068600110421           // -?F3=Fine?
068700110420           WHEN  dsp_aid = c_F03;
068800110420             $Fine = *on;
068900110420
069000110421           // -?F12=Ritorno?
069100110420           WHEN  dsp_aid = c_F12;
069200110420             reset  $Video;
069300110420             clear  V1Topz;
069400110421             if  $Called;
069500110421               $Fine = *on;
069600110421             endif;
069700110420
069800110421           // -?Invio; F5=Ripristino; F6=Conferma; F16=Annullamento?
069900110420           OTHER;
070000110420             exsr  sr_CtrD03;
070100110420             if  ErrGenerico = *on;
070200110420               leavesr;
070300110420             endif;
070400110421             // -?F5=Ripristino; F6=Conferma; F16=Annullamento?
070500110420             if  dsp_aid = c_F05  or
070600110420                 dsp_aid = c_F06  or
070700110421                 dsp_aid = c_F16;
070800110421               $VideoPrec = $Video;
070900110420               $Video  = 'W1';
071000110420               reset  $InzW01;
071100110420             endif;
071200110420
071300110420         ENDSL;
071400110420
071500110420       ENDSR;
071600110420
071700110420       //--------------------------------------------------------------
071800110420       //?Inizializzazione videata D03                                 ?
071900110420       //--------------------------------------------------------------
072000110420       BEGSR  sr_InzD03;
072100110420
072200110421         // -?Spegnimento di tutti gli indicatori?
072300110420         IndDspF = *off;
072400110420
072500110421         // -?Pulizia PARZIALE videata?
072600110420         clear  V3Cksc;
072700110420         clear  V3Dksc;
072800110421         clear  V3Ctrk;
072900110420
073000110421         // -?Impostazione tipo manutenzione?
073100110420         V1Topz = '  COPIA   ';
073200110421
073300110421         // -?Disabilitazione ripristino annullati (in fase di copia)?
073400110421         $Ripristino = *off;
073500110420
073600110420       ENDSR;
073700110420
073800110420       //--------------------------------------------------------------
073900110420       //?Controllo dati immessi nella videata D03                     ?
074000110420       //--------------------------------------------------------------
074100110420       BEGSR  sr_CtrD03;
074200110421
074300110421         %subst(IndDspF : 50) = *off;
074400110420
074500110421         // -?Controllo codice cliente & trk?
074600110420         clear  V3Dksc;
074700110420         V1Cksc = %editc( V3Cksc : 'X' );
074800110421         V1Ctrk = V3Ctrk;
074900110420         exsr  sr_CtrD01;
075000110420         V3Dksc = V1Dksc;
075100110421         if  ErrGenerico;
075200110421           leavesr;
075300110421         endif;
075400110421
075500110421         // -?Controllo dati da copiare?
075600110421         exsr  sr_CtrD02;
075700110421         if  ErrGenerico;
075800110421           leavesr;
075900110421         endif;
076000110420
076100110421         // -?Verifica esistenza record in immissione in tab. "KB4"?
076200110420         reset  TNTBE000;
076300110420         k_TBEcod = c_Tab;
076400110421         k_TBEke1 = %editc( V3Cksc : 'X' );
076500110421         k_TBEke2 = V3Ctrk;
076600110420         clear  k_TBElin;
076700110420         clear  k_TBEsif;
076800110421         setll  %kds(k05tntbe01)  TNTBE000;
076900110420         $Immissione = (NOT %equal(TNTBE01L));
077000110420         reset  $Ripristino;
077100110420         if  %equal(TNTBE01L);
077200110420           ErrGenerico = *on;
077300110421           ErrMessage  = *on;
077400110420           PosCurKSC   = *on;
077500110421           V1Dmsg = $Msg(07);
077600110420           leavesr;
077700110420         endif;
077800110420
077900110420       ENDSR;
078000110420
078100110420       //--------------------------------------------------------------
078200110420       //?Gestione trasmissione aggiornamento                          ?
078300110420       //--------------------------------------------------------------
078400110420       BEGSR  sr_GesW01;
078500110420
078600110421         // -?Inizializzazione videata?
078700110420         if $InzW01 = *on;
078800110420           exsr  sr_InzW01;
078900110420           $InzW01 = *off;
079000110420         endif;
079100110420
079200110421         // -?Emissione window?
079300110420         if  TBXftt = 'S';
079400110420           exfmt  TB86W01;
079500110420           ErrGenerico = *off;
079600110421           ErrMessage  = *off;
079700110420           clear  V1Dmsg;
079800110421           Select;
079900110421             // -?F12=Ritorno (gestito solo se emesso W01)?
080000110421             When  dsp_aid = c_F12;
080100110421               $Video = $VideoPrec;
080200110421               leavesr;
080300110421             // -?Invio (gestito solo se emesso W01)?
080400110421             When  dsp_aid <> c_F06;
080500110421               leavesr;
080600110421           EndSl;
080700110420         endif;
080800110420
080900110421         // -?Aggiornamento TNTBE00F?
081000110421         exsr  sr_Upd_TNTBE;
081100110421         select;
081200110421           // ·?Rilevato errore in fase di aggiornamento?
081300110421           when  ErrGenerico;
081400110421             leavesr;
081500110421           // ·?Uscita dal programma SE richiamato?
081600110421           when  $Called;
081700110421             $Fine = *on;
081800110421           // ·?Videata iniziale altrimenti?
081900110421           other;
082000110421             clear  V1Topz;
082100110421             $Video  = 'D1';
082200110421             $InzD01 = *on;
082300110421         endsl;
082400110420
082500110420       ENDSR;
082600110420
082700110420       //--------------------------------------------------------------
082800110420       //?Inizializzazione videata W01                                 ?
082900110420       //--------------------------------------------------------------
083000110420       BEGSR  sr_InzW01;
083100110420
083200110420         clear TB86W01;
083300110420
083400110420         if  $Immissione;
083500110420
083600110421           // -?Se immissione?
083700110420           W1ftt  = TBXftt;
083800110420
083900110420         else;
084000110420
084100110421           // -?Se NON immissione:?
084200110421           //  ?visualizza i dati relativi all'ultima trasmissione?
084300110420           W1ftt  = TBEftt;
084400110420           W1flt  = TBEflt;
084500110420           W1ftr  = TBEftr;
084600110420           if TBEdtr <> *zero;
084700110420             wDate_EUR = %date(TBEdtr : *iso);
084800110420             W1dtr     = %dec(wDate_EUR);
084900110420           endif;
085000110420
085100110420         endif;
085200110420
085300110420       ENDSR;
085400110420
085500110420       //--------------------------------------------------------------
085600110421       //?Aggiornamento record TNTBE00F (tab. "KB4")                   ?
085700110420       //--------------------------------------------------------------
085800110421       BEGSR  sr_Upd_TNTBE;
085900110421
086000110421         // -?Impostazione campi della struttura dati?
086100110421         if  Not  $Annullamento;
086200110421           clear  dKB4;
086300110421           §KB4rag = V2Crag;
086400110421           §KB4trd = V2Ctrd;
086500110421         endif;
086600110420
086700110421         // -?Impostazione campi del record di TNTBE00F?
086800110421         If  $Immissione;
086900110421           clear TNTBE000;
087000110421           TBEcod  = k_TBEcod;
087100110421           TBEke1  = k_TBEke1;
087200110421           TBEke2  = k_TBEke2;
087300110421           TBElin  = k_TBElin;
087400110421           TBEsif  = k_TBEsif;
087500110421         EndIf;
087600110421
087700110421         TBEapl = TBXapl;
087800110421         if  $Annullamento;
087900110421           TBEatb = 'A';
088000110421         else;
088100110421           clear  TBEatb;
088200110421         endif;
088300110421         TBEftt = W1ftt;
088400110421         TBEFlt = W1flt;
088500110421         clear  TBEftr;
088600110421         //clear TBEdtr;
088700110421
088800110421         TBEuni = dKB4;
088900110421
089000110421         // ·?Aggiornamento record?
089100110421         if  $Immissione;
089200110421           //_____________
089300110421           WRITE  TNTBE000;
089400110421           //¯¯¯¯¯¯¯¯¯¯¯¯¯
089500110421         else;
089600110421           //_____________
089700110421           UPDATE TNTBE000;
089800110421           //¯¯¯¯¯¯¯¯¯¯¯¯¯
089900110421         endif;
090000110420
090100110420       ENDSR;
090200110420
090300110420       //--------------------------------------------------------------
090400110420       //?Operazioni finali.                                           ?
090500110420       //--------------------------------------------------------------
090600110420       BEGSR  sr_RoutEnd;
090700110420
090800110421         // -?Chiusura funzioni precedentemente aperte?
090900110420         clear  TIBS69ds;
091000110420         I69tla = 'C';
091100110420         tibs69r (TIBS69ds : ds_CNACO :
091200110420                             ds_CNIND :
091300110420                             ds_CNCLP :
091400110420                             ds_FNCLS);
091500110420
091600110421         // -?Uscita dal *pgm?
091700110420         return;
091800110420
091900110420       ENDSR;
092000110420
092100110420      /end-free
092200110420
092300110420       //--------------------------------------------------------------
092400110420       //?Definizione schiere a tempo di compilazione                  ?
092500110420       //--------------------------------------------------------------
092600110420
092700110421** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
092800110420Immettere il codice cliente                                                     1
092900110420Immettere SOLO caratteri numerici                                               2
093000110420Codice cliente errato                                                           3
093100110420Cliente annullato                                                               4
093200110421Immettere il tipo record                                                        5
093300110421Immettere la descrizione trk estensione bolla mancante                          6
093400110421Cliente/Trk già inseriti                                                        7
