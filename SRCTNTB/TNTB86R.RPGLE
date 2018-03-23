000100110421       //==============================================================
000200110421       //?Gestione tabella "KB4" (Clienti invio BL4 a Sede)            ?
000300110421       //==============================================================
000400110811
000500110811      *?  ATTENZIONE!!  ?
000600110811      *?    Questa tabella è utilizzata anche dal pgm TNTA61R  ?
000700110811      *?    'Interrogazione abilitazioni clienti'              ?
000800110811      *?    In caso di aggiunta/modifica campi alla tabella    ?
000900110811      *?    verificare se sono validi per la visualizzazione   ?
001000110811      *?    da TA61                                            ?
001100110420
001200110420       //--------------------------------------------------------------
001300110420       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
001400110420       //--------------------------------------------------------------
001500110420
001600110420     /*PRM dbgview(*source)
001700110420     /*END
001800110420
001900110420       //--------------------------------------------------------------
002000110420       //?Specifiche di controllo.                                     ?
002100110420       //--------------------------------------------------------------
002200110420
002300110420     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002400110420     h dftactgrp(*no) actgrp('QILE')
002500110420
002600110420       //--------------------------------------------------------------
002700110420       //?Dichiarazione file.                                          ?
002800110420       //--------------------------------------------------------------
002900110420
003000110420       // -?Tabella "KB4"?
003100110420     fTNTBE01L  Uf A e           k disk
003200110420
003300110420       // -?Video Gestione?
003400110420     fTNTB86D   cf   e             workstn
003500110420     f                                     indds(IndDspF)
003600110420     f                                     infds(InfDspF)
003700110420
003800110420       //--------------------------------------------------------------
003900110420       //?Definizione costanti.                                        ?
004000110420       //--------------------------------------------------------------
004100110420
004200110420       // -?Codice tabella in gestione?
004300110420     d c_Tab           c                   const('KB4')
004400110420
004500110420       // -?Costante per controllo "caratteri solo numerici"?
004600110421     d c_Digits        c                   const('0123456789')
004700110420
004800110420       // -?Tasti funzionali a video?
004900110420     d c_F01           c                   const(x'31')
005000110420     d c_F02           c                   const(x'32')
005100110420     d c_F03           c                   const(x'33')
005200110420     d c_F04           c                   const(x'34')
005300110420     d c_F05           c                   const(x'35')
005400110420     d c_F06           c                   const(x'36')
005500110420     d c_F07           c                   const(x'37')
005600110420     d c_F08           c                   const(x'38')
005700110420     d c_F09           c                   const(x'39')
005800110420     d c_F10           c                   const(x'3A')
005900110420     d c_F11           c                   const(x'3B')
006000110420     d c_F12           c                   const(x'3C')
006100110420     d c_F13           c                   const(x'B1')
006200110420     d c_F14           c                   const(x'B2')
006300110420     d c_F15           c                   const(x'B3')
006400110420     d c_F16           c                   const(x'B4')
006500110420     d c_F17           c                   const(x'B5')
006600110420     d c_F18           c                   const(x'B6')
006700110420     d c_F19           c                   const(x'B7')
006800110420     d c_F20           c                   const(x'B8')
006900110420     d c_F21           c                   const(x'B9')
007000110420     d c_F22           c                   const(x'BA')
007100110420     d c_F23           c                   const(x'BB')
007200110420     d c_F24           c                   const(x'BC')
007300110420     d c_Enter         c                   const(x'F1')
007400110420     d c_RollDown      c                   const(x'F4')
007500110420     d c_RollUp        c                   const(x'F5')
007600110420
007700110420       //--------------------------------------------------------------
007800110420       //?Definizione schiere.                                         ?
007900110420       //--------------------------------------------------------------
008000110420
008100110420       // -?Messaggi di errore?
008200110421     d $Msg            s             78    dim( 7)  ctdata  perrcd(01)
008300110420
008400110420       //--------------------------------------------------------------
008500110420       //?Definizione aree dati.                                       ?
008600110420       //--------------------------------------------------------------
008700110420
008800110420       // -?Dati utente?
008900110420     d §AzUte        e ds                  extname(AZUTE00F)
009000110420     d                                     dtaara
009100110420     d §DatiUte      e ds                  extname(dDatiUte)
009200110420     d                                     dtaara
009300110420
009400110420       //--------------------------------------------------------------
009500110420       //?Definizione strutture dati.                                  ?
009600110420       //--------------------------------------------------------------
009700110420
009800110420       // -?Status ds?
009900110420     d Status         sds
010000110420     d  SDSpgm           *proc
010100110420
010200110420       // -?InfDS?
010300110420     d InfDspF         ds
010400110420     d   dsp_aid             369    369a
010500110420
010600110420       // -?Indicatori su DspF?
010700110420     d IndDspF         ds                  inz
010800110420         // -?Abilitazione tasti funzionali?
010900110421     d   F3Attivo                      n   overlay(IndDspF : 03)
011000110420     d   F5Attivo                      n   overlay(IndDspF : 05)
011100110420     d   F6Attivo                      n   overlay(IndDspF : 06)
011200110420     d   F10Attivo                     n   overlay(IndDspF : 10)
011300110420     d   F16Attivo                     n   overlay(IndDspF : 16)
011400110420         // -?Emissione messaggio di errore?
011500110420     d   ErrMessage                    n   overlay(IndDspF : 28)
011600110420         // -?Indicatori per Attibuti di visualizzazione?
011700110420         // -?Posizionamento cursore & Segnalazione errore?
011800110421     d   PosCurKSC                     n   overlay(IndDspF : 51)
011900110421     d   PosCurTRK                     n   overlay(IndDspF : 52)
012000110421     d   PosCurRAG                     n   overlay(IndDspF : 53)
012100110421     d   PosCurTRD                     n   overlay(IndDspF : 54)
012200110420         // -?Riemissione videata?
012300110420     d   ErrGenerico                   n   overlay(IndDspF : 99)
012400110420
012500110420       // -?Parametri ricevuti?
012600110420     d KPJBA         e ds
012700110420
012800110420       // -?Dati record principale della tabella?
012900110420     d TNTBEds       e ds                  inz  extname(TNTBE00F)
013000110420     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
013100110420     d                                          prefix(TBX:3)
013200110421
013300110421       // -?Tabella in manutenzione?
013400110421     d dKB4          e ds                  inz
013500110420
013600110420       //--------------------------------------------------------------
013700110420       //?Definizione variabili globali.                               ?
013800110420       //--------------------------------------------------------------
013900110420
014000110420       // -?Flags booleani?
014100110420     d $Fine           s               n   inz
014200110420     d $InzD01         s               n   inz(*on)
014300110420     d $InzD02         s               n   inz(*on)
014400110420     d $InzD03         s               n   inz(*on)
014500110420     d $InzW01         s               n   inz(*on)
014600110420     d $Annullamento   s               n   inz
014700110420     d $Immissione     s               n   inz
014800110420     d $Ripristino     s               n   inz
014900110421     d $Called         s               n   inz
015000110420
015100110420       // -?Variabili per la gestione del video?
015200110420     d $Video          s              2    inz('D1')
015300110421     d $VideoPrec      s                   like($Video)  inz
015400110421
015500110421       // -?Codice cliente eventualmente ricevuto?
015600110421     d W0Cksc          s                   like(V1Cksc)  inz
015700110421       // -?Trk eventualmente ricevuto?
015800110421     d W0Ctrk          s                   like(V1Ctrk)  inz
015900110420
016000110420       // -?Campi di comodo?
016100110420     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
016200110420
016300110420       //--------------------------------------------------------------
016400110420       //?Definizione prototipi procedure.                             ?
016500110420       //--------------------------------------------------------------
016600110420
016700110420       // -?Reperimento dati utente?
016800110420     d TIBS34ds      e ds                  inz
016900110420      /copy gaitrasrc/srcProtoPR,TIBS34R
017000110420
017100110420       // -?Ricerca/Controllo tabelle (TNTBE)?
017200110420     d TIBS02ds      e ds
017300110420     d   T02mod      e                     inz('R')
017400110420     d   T02cod      e                     inz(c_Tab)
017500110420      /copy gaitrasrc/srcProtoPR,TIBS02R
017600110420
017700110420       // -?Parametri per pgm controllo/decodifica cliente?
017800110420      /copy gaitrasrc/srcProtoPI,TIBS69R
017900110420       // -?Controllo/Decodifica cliente?
018000110420      /copy gaitrasrc/srcProtoPR,TIBS69R
018100110420
018200110420       //--------------------------------------------------------------
018300110420       //?Definizione key-list.                                        ?
018400110420       //--------------------------------------------------------------
018500110420
018600110420       // -?File TNTBE01L?
018700110420     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
018800110420     d                                     prefix(k_)   inz
018900110420
019000110420       //--------------------------------------------------------------
019100110420       //?Riepilogo indicatori utilizzati.                             ?
019200110420       //--------------------------------------------------------------
019300110420       //--------------------------------------------------------------
019400110420
019500110420       //--------------------------------------------------------------
019600110420       //?M A I N - L I N E                                            ?
019700110420       //--------------------------------------------------------------
019800110420
019900110420     c     *Entry        plist
020000110420     c                   parm                    KPJBA
020100110420
020200110420      /free
020300110420
020400110420       // -?Operazioni iniziali?
020500110420       exsr sr_RoutInz;
020600110420
020700110420       // -?Gestione videate?
020800110420       DOW  $Fine = *off;
020900110420
021000110420         select;
021100110420
021200110420           // -?Gestione videata D1?
021300110420           //  ?(richiesta chiave tabella)?
021400110420           when $Video = 'D1';
021500110420             exsr sr_GesD01;
021600110420
021700110420           // -?Gestione videata D2?
021800110420           //  ?(manutenzione dati tabella "KB4")?
021900110420           when $Video = 'D2';
022000110420             exsr sr_GesD02;
022100110420
022200110420           // -?Gestione videata D3?
022300110420           //  ?(manutenzione dati da COPIARE in tab. "KB4")?
022400110420           when $Video = 'D3';
022500110420             exsr sr_GesD03;
022600110420
022700110420           // -?Gestione videata W1?
022800110420           //  ?(richiesta dati per trasmissione record)?
022900110420           when $Video = 'W1';
023000110420             exsr sr_GesW01;
023100110420
023200110420           // -?Gestione videata ????
023300110420           other;
023400110420             $Fine = *on;
023500110420
023600110420         endsl;
023700110420
023800110420       ENDDO;
023900110420
024000110420       // -?Operazioni finali?
024100110420       exsr sr_RoutEnd;
024200110420
024300110420       //--------------------------------------------------------------
024400110420       //?Operazioni iniziali.                                         ?
024500110420       //--------------------------------------------------------------
024600110420       BEGSR  sr_RoutInz;
024700110420
024800110420         *inLR = *on;
024900110420
025000110420         // -?Reperimento dati job?
025100110420         exsr  sr_DatiJob;
025200110420
025300110420         // -?Impostazione nome programma a video?
025400110420         V1Tpgm = SDSpgm;
025500110420
025600110420         // -?Aggancio dati generali della tabella in esame?
025700110420         clear  k_TBEcod;
025800110420         k_TBEke1 = *zero;
025900110420         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
026000110420                = c_Tab;
026100110420         clear  k_TBEke2;
026200110420         clear  k_TBElin;
026300110420         k_TBEsif = KNSIF;
026400110420         chain(n)  %kds(k05tntbe01)  TNTBE000;
026500110420         if  not  %found(TNTBE01L);
026600110420           clear  k_TBEsif;
026700110420           chain(n)  %kds(k05tntbe01)  TNTBE000;
026800110420         endif;
026900110420         if  %found(TNTBE01L);
027000110420           xTNTBEds = TNTBEds;
027100110420         else;
027200110420           clear  xTNTBEds;
027300110420         endif;
027400110420
027500110420         // -?Impostazione iniziale / pulizia dei campi chiave?
027600110420         k_TBEcod = c_Tab;
027700110420         clear  k_TBEke1;
027800110420         clear  k_TBEke2;
027900110420         clear  k_TBElin;
028000110420         clear  k_TBEsif;
028100110421
028200110421         // -?Verifica se *pgm richiamato:?
028300110421         //  ?test delle prime 8 posizioni della KPJBA per sapere se il?
028400110421         //  ?pgm è stato richiamato da un altro pgm o da menù tabelle.?
028500110421         $Called = (%subst( KPJBU : 1 : %len(V1Cksc) + %len(V1Ctrk) )
028600110421                   <> *blank);
028700110421         clear  W0Cksc;
028800110421         clear  W0Ctrk;
028900110421         if  $Called;
029000110421           if  %check( c_Digits : %subst( KPJBU : 1 :
029100110421                                          %len(V1Cksc) ) ) = *zero;
029200110421             W0Cksc = %subst( KPJBU : 1 : %len(V1Cksc) );
029300110421           endif;
029400110421           if  %subst( KPJBU : %len(V1Cksc) + 1 :
029500110421                                            %len(V1Ctrk) )  <> *blank;
029600110421             W0Ctrk = %subst( KPJBU : %len(V1Cksc)+1 : %len(V1Ctrk) );
029700110421           endif;
029800110421         endif;
029900110420
030000110420       ENDSR;
030100110420
030200110420       //--------------------------------------------------------------
030300110420       //?Reperimento Dati del job (Utente/Operativi).                 ?
030400110420       //--------------------------------------------------------------
030500110420       BEGSR  sr_DatiJob;
030600110420
030700110420         in(E) §AzUte;
030800110420         if NOT %error;
030900110420           in(E) §DatiUte;
031000110420         endif;
031100110420         if %error or RSut = *blanks;
031200110420           clear TIBS34ds;
031300110420           tibs34r ( tibs34ds );
031400110420           in §AzUte;
031500110420           in §DatiUte;
031600110420         endif;
031700110420
031800110420       ENDSR;
031900110420
032000110420       //--------------------------------------------------------------
032100110420       //?Gestione videata D01                                         ?
032200110420       //--------------------------------------------------------------
032300110420       BEGSR  sr_GesD01;
032400110420
032500110420         // -?Inizializzazione videata?
032600110420         if  $InzD01 = *on;
032700110420           exsr  sr_InzD01;
032800110420           $InzD01 = *off;
032900110420         endif;
033000110421
033100110421         // -?SE ricevuti codice cliente & trk?
033200110421         //  ?NON si emette la 1ª videata?
033300110421         if  $Called  and  (W0Cksc > *zero  and  W0Ctrk <> *blank);
033400110421           exsr  sr_CtrD01;
033500110421           if  Not ErrGenerico;
033600110421             $Video  = 'D2';
033700110421             $InzD02 = *on;
033800110421             leavesr;
033900110421           else;
034000110421             clear W0Cksc;
034100110421             clear W0Ctrk;
034200110421           endif;
034300110421         endif;
034400110420
034500110420         // -?Emissione Testata & Piede?
034600110420         write  TB86T01;
034700110420         write  TB86P01;
034800110420
034900110420         // -?Emissione videata?
035000110420         exfmt  TB86D01;
035100110420
035200110420         reset  ErrMessage;
035300110420         reset  ErrGenerico;
035400110420         clear  V1Dmsg;
035500110420
035600110420         SELECT;
035700110420
035800110420           // -?F3=Fine?
035900110420           WHEN  dsp_aid = c_F03;
036000110420             $Fine = *on;
036100110420
036200110420           // -?Invio?
036300110420           OTHER;
036400110420             exsr sr_CtrD01;
036500110420             if  ErrGenerico = *on;
036600110420               leavesr;
036700110420             endif;
036800110420             $Video    = 'D2';
036900110420             $InzD02   = *on;
037000110420
037100110420         ENDSL;
037200110420
037300110420       ENDSR;
037400110420
037500110420       //--------------------------------------------------------------
037600110420       //?Inizializzazione videata D01                                 ?
037700110420       //--------------------------------------------------------------
037800110420       BEGSR  sr_InzD01;
037900110420
038000110420         clear  TB86D01;
038100110421
038200110421         // -?Se è stato richiamato con un codice cliente & un trk,?
038300110421         //  ?si impostano tali codici a video?
038400110421         if  $Called;
038500110421           if  W0Cksc > *zero;
038600110421             V1Cksc = W0Cksc;
038700110421           endif;
038800110421           if  W0Ctrk <> *blank;
038900110421             V1Ctrk = W0Ctrk;
039000110421           endif;
039100110421         else;
039200110421           V1Cksc = '?';
039300110421         endif;
039400110420
039500110420       ENDSR;
039600110420
039700110420       //--------------------------------------------------------------
039800110420       //?Controllo dati immessi nella videata D01                     ?
039900110420       //--------------------------------------------------------------
040000110420       BEGSR  sr_CtrD01;
040100110420
040200110420         %subst(IndDspF : 28) = *off;
040300110420
040400110421         clear  V1Dksc;
040500110420         SELECT;
040600110420
040700110420           // -?Ricerca codice cliente?
040800110420           WHEN  %scan('?' : V1Cksc) > *zeros;
040900110420             reset TIBS02ds;
041000110420             //T02mod = 'R';    ?(già così)?
041100110420             //T02cod = c_Tab;  ?(già così)?
041200110420             T02sif = KNSIF;
041300110420             TNTBE_RicercaControllo ( kpjba : tibs02ds );
041400110420             if  T02err = *blanks;
041500110420               V1Cksc = %subst(T02ke1 : 1 : %len(V1Cksc));
041600110421               V1Ctrk = %subst(T02ke2 : 1 : %len(V1Ctrk));
041700110420               ErrGenerico = *on;
041800110420               leavesr;
041900110420             else;
042000110420               ErrMessage  = *on;
042100110420               ErrGenerico = *on;
042200110420               PosCurKSC   = *on;
042300110420               V1Dmsg = T02msg;
042400110420               leavesr;
042500110420             endif;
042600110420
042700110420           // -?Controllo immissione codice cliente?
042800110420           WHEN  V1Cksc = *blank  or  V1Cksc  = *zero;
042900110420             ErrMessage  = *on;
043000110420             ErrGenerico = *on;
043100110420             PosCurKSC   = *on;
043200110420             V1Dmsg = $Msg(01);
043300110420             leavesr;
043400110420
043500110420           // -?Controllo immissione solo caratteri numerici?
043600110421           WHEN  %check(c_Digits : V1Cksc) > *zero;
043700110420             ErrMessage  = *on;
043800110420             ErrGenerico = *on;
043900110420             PosCurKSC   = *on;
044000110420             V1Dmsg = $Msg(02);
044100110420             leavesr;
044200110420
044300110420         ENDSL;
044400110420
044500110420         // -?Controllo/decodifica cliente?
044600110420         clear  TIBS69ds;
044700110420         I69kcc = DUTkci;
044800110420         I69kac = %int( V1Cksc );
044900110420         I69sif = knsif;
045000110420         tibs69r(tibs69ds : ds_cnaco : ds_cnind : ds_cnclp : ds_fncls);
045100110420         if  O69err = *on;
045200110420           V1Dmsg = $Msg(03);
045300110420           ErrMessage  = *on;
045400110420           ErrGenerico = *on;
045500110420           PosCurKSC   = *on;
045600110420           leavesr;
045700110420         endif;
045800110420         V1Dksc = %subst(ACOrag : 1 : %len(V1Dksc));
045900110420
046000110420         // -?NON ammessi nuovi record per cliente annullato?
046100110420         reset  TNTBE000;
046200110420         k_TBEcod = c_Tab;
046300110420         k_TBEke1 = V1Cksc;
046400110421         k_TBEke2 = V1Ctrk;
046500110420         clear  k_TBElin;
046600110420         clear  k_TBEsif;
046700110421         setll  %kds(k05tntbe01)  TNTBE000;
046800110420         $Immissione = (NOT %equal(TNTBE01L));
046900110420         reset  $Ripristino;
047000110420         if  NOT  %equal(TNTBE01L)  and  ACOflg <> *blanks;
047100110420           V1Dmsg = $Msg(04);
047200110420           ErrMessage  = *on;
047300110420           ErrGenerico = *on;
047400110420           PosCurKSC   = *on;
047500110420           leavesr;
047600110420         endif;
047700110421
047800110421         // -?Controllo immissione trk?
047900110421         if  V1Ctrk = *blank;
048000110421           ErrMessage  = *on;
048100110421           ErrGenerico = *on;
048200110421           PosCurTRK   = *on;
048300110421           V1Dmsg = $Msg(05);
048400110421           leavesr;
048500110421         endif;
048600110420
048700110420       ENDSR;
048800110420
048900110420       //--------------------------------------------------------------
049000110420       //?Gestione videata D02                                         ?
049100110420       //--------------------------------------------------------------
049200110420       BEGSR  sr_GesD02;
049300110420
049400110420         // -?Inizializzazione videata?
049500110420         if  $InzD02 = *on;
049600110420           exsr sr_InzD02;
049700110420           $InzD02 = *off;
049800110420         endif;
049900110420
050000110420         // -?Emissione Testata e Piede con tasti funzionali abilitati?
050100110420         if  errGenerico = *off;
050200110420           write  TB86T01;
050300110420           write  TB86D01;
050400110420           write  Protect;
050500110420           write  TB86P02;
050600110420         endif;
050700110420
050800110420         // -?Emissione videata?
050900110420         exfmt  TB86D02;
051000110420         reset  ErrMessage;
051100110420         reset  ErrGenerico;
051200110420         clear  V1Dmsg;
051300110420         reset  $Annullamento;
051400110420
051500110420         SELECT;
051600110420
051700110420           // -?F3=Fine?
051800110420           WHEN  dsp_aid = c_F03;
051900110420             unlock TNTBE01L;
052000110420             $Fine = *on;
052100110420
052200110420           // -?F12=Ritorno?
052300110420           WHEN  dsp_aid = c_F12;
052400110420             unlock TNTBE01L;
052500110420             reset  $Video;
052600110420             clear  V1Topz;
052700110421             if  $Called;
052800110421               $Fine = *on;
052900110421             endif;
053000110420
053100110420           // -?Invio;?
053200110420           //  ?F5=Ripristino; F6=Conferma; F16=Annullamento;?
053300110420           //  ?F10=Copia?
053400110420           OTHER;
053500110420             exsr  sr_CtrD02;
053600110420             if  ErrGenerico;
053700110420               leavesr;
053800110420             endif;
053900110420             Select;
054000110420               // -?F5=Ripristino; F6=Conferma; F16=Annullamento?
054100110420               When  dsp_aid = c_F05  or
054200110420                     dsp_aid = c_F06  or
054300110420                     dsp_aid = c_F16;
054400110420                 $Annullamento = (dsp_aid = c_F16);
054500110421                 $VideoPrec = $Video;
054600110421                 $Video = 'W1';
054700110420                 reset  $InzW01;
054800110420               // -?F10=Copia?
054900110420               When  dsp_aid = c_F10;
055000110420                 unlock TNTBE01L;
055100110420                 $Video  = 'D3';
055200110420                 reset  $InzD03;
055300110420             EndSl;
055400110420
055500110420         ENDSL;
055600110420
055700110420       ENDSR;
055800110420
055900110420       //--------------------------------------------------------------
056000110420       //?Inizializzazione videata D02                                 ?
056100110420       //--------------------------------------------------------------
056200110420       BEGSR  sr_InzD02;
056300110420
056400110420         // -?Spegnimento di tutti gli indicatori?
056500110420         IndDspF = *off;
056600110420
056700110420         // -?Pulizia videata?
056800110420         clear  TB86D02;
056900110420
057000110421         // -?Reperimento dati da caricare a video?
057100110421         chain  %kds(k05tntbe01)  TNTBE000;
057200110421
057300110421         // -?Impostazione tipo manutenzione?
057400110421         $Immissione = (Not %found(TNTBE01L));
057500110421         $Ripristino = (%found(TNTBE01L)  and  TBEatb <> *blank);
057600110421         reset  $Annullamento;
057700110421
057800110421         select;
057900110421           when  $Immissione;
058000110421             V1Topz = 'IMMISSIONE';
058100110421           when  $Ripristino;
058200110421             V1Topz = 'RIPRISTINO';
058300110421           other;
058400110421             V1Topz = ' MODIFICA ';
058500110421         endsl;
058600110421
058700110421         // -?Caricamento dati a video?
058800110421         If  $Immissione;
058900110421           clear  dKB4;
059000110421           V2Crag = ACOrag;
059100110421           select;
059200110421             when  V1Ctrk = '8';
059300110421               V2Ctrd = 'NOTE PARTENZA 1';
059400110421             when  V1Ctrk = '9';
059500110421               V2Ctrd = 'NOTE PARTENZA 2';
059600110421           endsl;
059700110421         Else;
059800110421           dKB4 = TBEuni;
059900110421           V2Crag = §KB4rag;
060000110421           V2Ctrd = §KB4trd;
060100110421         EndIf;
060200110420
060300110420         // -?(Ri)Abilitazione tasti funzionali?
060400110421         exsr  sr_AbilFxxD02;
060500110420
060600110420       ENDSR;
060700110420
060800110420       //--------------------------------------------------------------
060900110421       //?Abilitazione tasti funzionali in P02 (per D02).              ?
061000110420       //--------------------------------------------------------------
061100110421       BEGSR  sr_AbilFxxD02;
061200110421
061300110421         // ->?F3 = Fine?
061400110421         F3Attivo = (NOT $Called);
061500110420
061600110421         // -?F5=Ripristino?
061700110420         F5Attivo  = (NOT $Immissione  and  $Ripristino);
061800110420
061900110421         // -?F6=Conferma?
062000110420         F6Attivo  = (NOT F5Attivo);
062100110420
062200110421         // -?F10=Copia?
062300110420         F10Attivo = (NOT $Immissione  and  NOT $Ripristino);
062400110420
062500110421         // -?F16=Annullamento?
062600110420         F16Attivo = (NOT $Immissione  and  NOT $Ripristino);
062700110420
062800110420       ENDSR;
062900110421
063000110421       //--------------------------------------------------------------
063100110421       //?Controllo dati immessi nella videata D02                     ?
063200110421       //--------------------------------------------------------------
063300110421       BEGSR  sr_CtrD02;
063400110421
063500110421         %subst(IndDspF : 50) = *off;
063600110421
063700110421         // -?AUTOMATISMI - SE NON annullamento:?
063800110421         If  dsp_aid <> c_F16;
063900110421
064000110421           // -?Ragione sociale mancante: proposta quella anagrafica?
064100110421           if  V2Crag  = *blank;
064200110421             V2Crag = ACOrag;
064300110421           endif;
064400110421
064500110421           // -?Descriz. trk estens. bolla mancante: proposto default?
064600110421           if  V2Ctrd  = *blank;
064700110421             select;
064800110421               when  V1Ctrk = '8';
064900110421                 V2Ctrd = 'NOTE PARTENZA 1';
065000110421               when  V1Ctrk = '9';
065100110421                 V2Ctrd = 'NOTE PARTENZA 2';
065200110421               other;
065300110421                 ErrGenerico = *on;
065400110421                 ErrMessage  = *on;
065500110421                 PosCurTRD   = *on;
065600110421                 V1Dmsg = $Msg(06);
065700110421                 leavesr;
065800110421             endsl;
065900110421           endif;
066000110421
066100110421         EndIf;
066200110421
066300110421       ENDSR;
066400110420
066500110420       //--------------------------------------------------------------
066600110420       //?Gestione videata D03                                         ?
066700110420       //--------------------------------------------------------------
066800110420       BEGSR  sr_GesD03;
066900110420
067000110421         // -?Inizializzazione videata?
067100110420         if  $InzD03 = *on;
067200110420           exsr sr_InzD03;
067300110420           $InzD03 = *off;
067400110420         endif;
067500110420
067600110421         // -?Emissione Testata e Piede con tasti funzionali abilitati?
067700110420         if  errGenerico = *off;
067800110420           write  TB86T01;
067900110420           write  TB86D01;
068000110420           write  Protect;
068100110420           write  TB86P03;
068200110420         endif;
068300110420
068400110421         // -?Emissione videata?
068500110420         exfmt  TB86D03;
068600110421
068700110420         reset  ErrGenerico;
068800110421         reset  ErrMessage;
068900110420         clear  V1Dmsg;
069000110420
069100110420         SELECT;
069200110420
069300110421           // -?F3=Fine?
069400110420           WHEN  dsp_aid = c_F03;
069500110420             $Fine = *on;
069600110420
069700110421           // -?F12=Ritorno?
069800110420           WHEN  dsp_aid = c_F12;
069900110420             reset  $Video;
070000110420             clear  V1Topz;
070100110421             if  $Called;
070200110421               $Fine = *on;
070300110421             endif;
070400110420
070500110421           // -?Invio; F5=Ripristino; F6=Conferma; F16=Annullamento?
070600110420           OTHER;
070700110420             exsr  sr_CtrD03;
070800110420             if  ErrGenerico = *on;
070900110420               leavesr;
071000110420             endif;
071100110421             // -?F5=Ripristino; F6=Conferma; F16=Annullamento?
071200110420             if  dsp_aid = c_F05  or
071300110420                 dsp_aid = c_F06  or
071400110421                 dsp_aid = c_F16;
071500110421               $VideoPrec = $Video;
071600110420               $Video  = 'W1';
071700110420               reset  $InzW01;
071800110420             endif;
071900110420
072000110420         ENDSL;
072100110420
072200110420       ENDSR;
072300110420
072400110420       //--------------------------------------------------------------
072500110420       //?Inizializzazione videata D03                                 ?
072600110420       //--------------------------------------------------------------
072700110420       BEGSR  sr_InzD03;
072800110420
072900110421         // -?Spegnimento di tutti gli indicatori?
073000110420         IndDspF = *off;
073100110420
073200110421         // -?Pulizia PARZIALE videata?
073300110420         clear  V3Cksc;
073400110420         clear  V3Dksc;
073500110421         clear  V3Ctrk;
073600110420
073700110421         // -?Impostazione tipo manutenzione?
073800110420         V1Topz = '  COPIA   ';
073900110421
074000110421         // -?Disabilitazione ripristino annullati (in fase di copia)?
074100110421         $Ripristino = *off;
074200110420
074300110420       ENDSR;
074400110420
074500110420       //--------------------------------------------------------------
074600110420       //?Controllo dati immessi nella videata D03                     ?
074700110420       //--------------------------------------------------------------
074800110420       BEGSR  sr_CtrD03;
074900110421
075000110421         %subst(IndDspF : 50) = *off;
075100110420
075200110421         // -?Controllo codice cliente & trk?
075300110420         clear  V3Dksc;
075400110420         V1Cksc = %editc( V3Cksc : 'X' );
075500110421         V1Ctrk = V3Ctrk;
075600110420         exsr  sr_CtrD01;
075700110420         V3Dksc = V1Dksc;
075800110421         if  ErrGenerico;
075900110421           leavesr;
076000110421         endif;
076100110421
076200110421         // -?Controllo dati da copiare?
076300110421         exsr  sr_CtrD02;
076400110421         if  ErrGenerico;
076500110421           leavesr;
076600110421         endif;
076700110420
076800110421         // -?Verifica esistenza record in immissione in tab. "KB4"?
076900110420         reset  TNTBE000;
077000110420         k_TBEcod = c_Tab;
077100110421         k_TBEke1 = %editc( V3Cksc : 'X' );
077200110421         k_TBEke2 = V3Ctrk;
077300110420         clear  k_TBElin;
077400110420         clear  k_TBEsif;
077500110421         setll  %kds(k05tntbe01)  TNTBE000;
077600110420         $Immissione = (NOT %equal(TNTBE01L));
077700110420         reset  $Ripristino;
077800110420         if  %equal(TNTBE01L);
077900110420           ErrGenerico = *on;
078000110421           ErrMessage  = *on;
078100110420           PosCurKSC   = *on;
078200110421           V1Dmsg = $Msg(07);
078300110420           leavesr;
078400110420         endif;
078500110420
078600110420       ENDSR;
078700110420
078800110420       //--------------------------------------------------------------
078900110420       //?Gestione trasmissione aggiornamento                          ?
079000110420       //--------------------------------------------------------------
079100110420       BEGSR  sr_GesW01;
079200110420
079300110421         // -?Inizializzazione videata?
079400110420         if $InzW01 = *on;
079500110420           exsr  sr_InzW01;
079600110420           $InzW01 = *off;
079700110420         endif;
079800110420
079900110421         // -?Emissione window?
080000110420         if  TBXftt = 'S';
080100110420           exfmt  TB86W01;
080200110420           ErrGenerico = *off;
080300110421           ErrMessage  = *off;
080400110420           clear  V1Dmsg;
080500110421           Select;
080600110421             // -?F12=Ritorno (gestito solo se emesso W01)?
080700110421             When  dsp_aid = c_F12;
080800110421               $Video = $VideoPrec;
080900110421               leavesr;
081000110421             // -?Invio (gestito solo se emesso W01)?
081100110421             When  dsp_aid <> c_F06;
081200110421               leavesr;
081300110421           EndSl;
081400110420         endif;
081500110420
081600110421         // -?Aggiornamento TNTBE00F?
081700110421         exsr  sr_Upd_TNTBE;
081800110421         select;
081900110421           // ·?Rilevato errore in fase di aggiornamento?
082000110421           when  ErrGenerico;
082100110421             leavesr;
082200110421           // ·?Uscita dal programma SE richiamato?
082300110421           when  $Called;
082400110421             $Fine = *on;
082500110421           // ·?Videata iniziale altrimenti?
082600110421           other;
082700110421             clear  V1Topz;
082800110421             $Video  = 'D1';
082900110421             $InzD01 = *on;
083000110421         endsl;
083100110420
083200110420       ENDSR;
083300110420
083400110420       //--------------------------------------------------------------
083500110420       //?Inizializzazione videata W01                                 ?
083600110420       //--------------------------------------------------------------
083700110420       BEGSR  sr_InzW01;
083800110420
083900110420         clear TB86W01;
084000110420
084100110420         if  $Immissione;
084200110420
084300110421           // -?Se immissione?
084400110420           W1ftt  = TBXftt;
084500110420
084600110420         else;
084700110420
084800110421           // -?Se NON immissione:?
084900110421           //  ?visualizza i dati relativi all'ultima trasmissione?
085000110420           W1ftt  = TBEftt;
085100110420           W1flt  = TBEflt;
085200110420           W1ftr  = TBEftr;
085300110420           if TBEdtr <> *zero;
085400110420             wDate_EUR = %date(TBEdtr : *iso);
085500110420             W1dtr     = %dec(wDate_EUR);
085600110420           endif;
085700110420
085800110420         endif;
085900110420
086000110420       ENDSR;
086100110420
086200110420       //--------------------------------------------------------------
086300110421       //?Aggiornamento record TNTBE00F (tab. "KB4")                   ?
086400110420       //--------------------------------------------------------------
086500110421       BEGSR  sr_Upd_TNTBE;
086600110421
086700110421         // -?Impostazione campi della struttura dati?
086800110421         if  Not  $Annullamento;
086900110421           clear  dKB4;
087000110421           §KB4rag = V2Crag;
087100110421           §KB4trd = V2Ctrd;
087200110421         endif;
087300110420
087400110421         // -?Impostazione campi del record di TNTBE00F?
087500110421         If  $Immissione;
087600110421           clear TNTBE000;
087700110421           TBEcod  = k_TBEcod;
087800110421           TBEke1  = k_TBEke1;
087900110421           TBEke2  = k_TBEke2;
088000110421           TBElin  = k_TBElin;
088100110421           TBEsif  = k_TBEsif;
088200110421         EndIf;
088300110421
088400110421         TBEapl = TBXapl;
088500110421         if  $Annullamento;
088600110421           TBEatb = 'A';
088700110421         else;
088800110421           clear  TBEatb;
088900110421         endif;
089000110421         TBEftt = W1ftt;
089100110421         TBEFlt = W1flt;
089200110421         clear  TBEftr;
089300110421         //clear TBEdtr;
089400110421
089500110421         TBEuni = dKB4;
089600110421
089700110421         // ·?Aggiornamento record?
089800110421         if  $Immissione;
089900110421           //_____________
090000110421           WRITE  TNTBE000;
090100110421           //¯¯¯¯¯¯¯¯¯¯¯¯¯
090200110421         else;
090300110421           //_____________
090400110421           UPDATE TNTBE000;
090500110421           //¯¯¯¯¯¯¯¯¯¯¯¯¯
090600110421         endif;
090700110420
090800110420       ENDSR;
090900110420
091000110420       //--------------------------------------------------------------
091100110420       //?Operazioni finali.                                           ?
091200110420       //--------------------------------------------------------------
091300110420       BEGSR  sr_RoutEnd;
091400110420
091500110421         // -?Chiusura funzioni precedentemente aperte?
091600110420         clear  TIBS69ds;
091700110420         I69tla = 'C';
091800110420         tibs69r (TIBS69ds : ds_CNACO :
091900110420                             ds_CNIND :
092000110420                             ds_CNCLP :
092100110420                             ds_FNCLS);
092200110420
092300110421         // -?Uscita dal *pgm?
092400110420         return;
092500110420
092600110420       ENDSR;
092700110420
092800110420      /end-free
092900110420
093000110420       //--------------------------------------------------------------
093100110420       //?Definizione schiere a tempo di compilazione                  ?
093200110420       //--------------------------------------------------------------
093300110420
093400110421** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
093500110420Immettere il codice cliente                                                     1
093600110420Immettere SOLO caratteri numerici                                               2
093700110420Codice cliente errato                                                           3
093800110420Cliente annullato                                                               4
093900110421Immettere il tipo record                                                        5
094000110421Immettere la descrizione trk estensione bolla mancante                          6
094100110421Cliente/Trk già inseriti                                                        7
