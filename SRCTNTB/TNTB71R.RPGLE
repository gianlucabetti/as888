000100110421       //==============================================================
000200090220       //?Gestione tabella "PXA" (Testi per cliente nel box            ?
000300090220       //?"Promemoria per Autotrasportaore" della LdV)                 ?
000400110421       //==============================================================
000500110811
000600110811      *?  ATTENZIONE!!  ?
000700110811      *?    Questa tabella è utilizzata anche dal pgm TNTA61R  ?
000800110811      *?    'Interrogazione abilitazioni clienti'              ?
000900110811      *?    In caso di aggiunta/modifica campi alla tabella    ?
001000110811      *?    verificare se sono validi per la visualizzazione   ?
001100110811      *?    da TA61                                            ?
001200110411
001300110411       //--------------------------------------------------------------
001400110411       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
001500110411       //--------------------------------------------------------------
001600110411
001700110411     /*PRM dbgview(*source)
001800110411     /*END
001900110411
002000110411       //--------------------------------------------------------------
002100110411       //?Specifiche di controllo.                                     ?
002200110411       //--------------------------------------------------------------
002300090219
002400090219     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002500090219     h dftactgrp(*no) actgrp('QILE')
002600090219
002700090219       //--------------------------------------------------------------
002800090219       //?Dichiarazione file.                                          ?
002900090219       //--------------------------------------------------------------
003000090219
003100110411       // -?Tabella "PXA"?
003200090219     fTNTBE01L  Uf A e           k disk
003300090219
003400110411       // -?Video Gestione?
003500090220     fTNTB71D   cf   e             workstn
003600090219     f                                     indds(IndDspF)
003700090219     f                                     infds(InfDspF)
003800090219
003900090219       //--------------------------------------------------------------
004000090219       //?Definizione costanti.                                        ?
004100090219       //--------------------------------------------------------------
004200090219
004300110411       // -?Codice tabella in gestione?
004400090219     d c_Tab           c                   const('PXA')
004500090224
004600110411       // -?Numero massimo di righe di testo elaborabili?
004700090224     d c_MaxRec        c                   const(006)
004800090219
004900110411       // -?Costante per controllo "caratteri solo numerici"?
005000110411     d c_Digits        c                   const('0123456789')
005100090219
005200110411       // -?Tasti funzionali a video?
005300090219     d c_F01           c                   const(x'31')
005400090219     d c_F02           c                   const(x'32')
005500090219     d c_F03           c                   const(x'33')
005600110411     d c_F04           c                   const(x'34')
005700090219     d c_F05           c                   const(x'35')
005800090219     d c_F06           c                   const(x'36')
005900090219     d c_F07           c                   const(x'37')
006000090219     d c_F08           c                   const(x'38')
006100090219     d c_F09           c                   const(x'39')
006200090219     d c_F10           c                   const(x'3A')
006300110411     d c_F11           c                   const(x'3B')
006400090219     d c_F12           c                   const(x'3C')
006500090219     d c_F13           c                   const(x'B1')
006600090219     d c_F14           c                   const(x'B2')
006700090219     d c_F15           c                   const(x'B3')
006800090219     d c_F16           c                   const(x'B4')
006900090219     d c_F17           c                   const(x'B5')
007000090219     d c_F18           c                   const(x'B6')
007100090219     d c_F19           c                   const(x'B7')
007200090219     d c_F20           c                   const(x'B8')
007300090219     d c_F21           c                   const(x'B9')
007400090219     d c_F22           c                   const(x'BA')
007500090219     d c_F23           c                   const(x'BB')
007600090219     d c_F24           c                   const(x'BC')
007700090219     d c_Enter         c                   const(x'F1')
007800090219     d c_RollDown      c                   const(x'F4')
007900090219     d c_RollUp        c                   const(x'F5')
008000090219
008100090219       //--------------------------------------------------------------
008200090219       //?Definizione schiere.                                         ?
008300090219       //--------------------------------------------------------------
008400090219
008500110411       // -?Messaggi di errore?
008600110413     d $Msg            s             78    dim( 7)  ctdata  perrcd(01)
008700090219
008800090219       //--------------------------------------------------------------
008900090219       //?Definizione aree dati.                                       ?
009000090219       //--------------------------------------------------------------
009100090219
009200110411       // -?Dati utente?
009300090219     d §AzUte        e ds                  extname(AZUTE00F)
009400090219     d                                     dtaara
009500090219     d §DatiUte      e ds                  extname(dDatiUte)
009600090219     d                                     dtaara
009700090219
009800090219       //--------------------------------------------------------------
009900090219       //?Definizione strutture dati.                                  ?
010000090219       //--------------------------------------------------------------
010100090219
010200110411       // -?Status ds?
010300090219     d Status         sds
010400090219     d  SDSpgm           *proc
010500090219
010600110411       // -?InfDS?
010700090219     d InfDspF         ds
010800090219     d   dsp_aid             369    369a
010900090219
011000110411       // -?Indicatori su DspF?
011100090219     d IndDspF         ds                  inz
011200110411         // -?Abilitazione tasti funzionali?
011300110413     d   F3Attivo                      n   overlay(IndDspF : 03)
011400090219     d   F5Attivo                      n   overlay(IndDspF : 05)
011500090219     d   F6Attivo                      n   overlay(IndDspF : 06)
011600090420     d   F10Attivo                     n   overlay(IndDspF : 10)
011700090219     d   F16Attivo                     n   overlay(IndDspF : 16)
011800110411         // -?Emissione messaggio di errore?
011900090219     d   ErrMessage                    n   overlay(IndDspF : 28)
012000110420         // -?Indicatori per Attibuti di visualizzazione?
012100110420     d   NotDspKSC                     n   overlay(IndDspF : 41)
012200110420     d   NotDspGMA                     n   overlay(IndDspF : 42)
012300110411         // -?Posizionamento cursore & Segnalazione errore?
012400090219     d   PosCurKSC                     n   overlay(IndDspF : 50)
012500110408     d   PosCurGMA                     n   overlay(IndDspF : 51)
012600110414     d   PosCurTXT                     n   overlay(IndDspF : 52)
012700110411         // -?Riemissione videata?
012800090219     d   ErrGenerico                   n   overlay(IndDspF : 99)
012900090219
013000110411       // -?Parametri ricevuti?
013100090219     d KPJBA         e ds
013200111017     d TNTA61ds      e ds
013300090219
013400110411       // -?Dati record principale della tabella?
013500090219     d TNTBEds       e ds                  inz  extname(TNTBE00F)
013600090219     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
013700090220     d                                          prefix(TBX:3)
013800090224
013900110411       // -?Ridefinizione dati a video?
014000110411     d dsH02prg        ds                  inz
014100110411     d   H02prg1                           inz
014200110411     d   H02prg2                           inz
014300110411     d   H02prg3                           inz
014400110411     d   H02prg4                           inz
014500110411     d   H02prg5                           inz
014600110411     d   H02prg6                           inz
014700110411     d   H02prg                1     18s 0 dim(c_MaxRec)  inz
014800110411
014900090224     d dsD02prg        ds                  inz
015000090224     d   D02prg1                           inz
015100090224     d   D02prg2                           inz
015200090224     d   D02prg3                           inz
015300090224     d   D02prg4                           inz
015400090224     d   D02prg5                           inz
015500090224     d   D02prg6                           inz
015600110411     d   D02prg                1      6s 0 dim(c_MaxRec)  inz
015700090224
015800090224     d dsD02txt        ds                  inz
015900090224     d   D02txt1                           inz
016000090224     d   D02txt2                           inz
016100090224     d   D02txt3                           inz
016200090224     d   D02txt4                           inz
016300090224     d   D02txt5                           inz
016400090224     d   D02txt6                           inz
016500090224     d   D02txt                1    456    dim(c_MaxRec)  inz
016600090219
016700090219       //--------------------------------------------------------------
016800090219       //?Definizione variabili globali.                               ?
016900090219       //--------------------------------------------------------------
017000090219
017100110411       // -?Flags booleani?
017200090219     d $Fine           s               n   inz
017300090219     d $InzD01         s               n   inz(*on)
017400090224     d $InzD02         s               n   inz(*on)
017500090420     d $InzD03         s               n   inz(*on)
017600090220     d $InzW01         s               n   inz(*on)
017700090420     d $Annullamento   s               n   inz
017800090220     d $Immissione     s               n   inz
017900110412     d $Ripristino     s               n   inz(*on)
018000110413     d $Called         s               n   inz
018100111017     d $Visualizza     s               n   inz
018200090220
018300110411       // -?Indici di schiera?
018400090220     d xx              s              3  0 inz
018500090219
018600110411       // -?Variabili per la gestione del video?
018700090219     d $Video          s              2    inz('D1')
018800110421     d $VideoPrec      s                   like($Video)  inz
018900110413
019000110413       // -?Codice cliente o particolarità consegna eventualmente ricevuto?
019100110413     d W0Cksc          s                   like(V1Cksc)  inz
019200110413     d W0Cgma          s                   like(V1Cgma)  inz
019300090220
019400110411       // -?Campi di comodo?
019500090220     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
019600090219
019700090219       //--------------------------------------------------------------
019800090219       //?Definizione prototipi procedure.                             ?
019900090219       //--------------------------------------------------------------
020000090219
020100110408       // -?Reperimento dati utente?
020200110408     d TIBS34ds      e ds                  inz
020300110408      /copy gaitrasrc/srcProtoPR,TIBS34R
020400090219
020500110408       // -?Interrogazione particolarità consegne - tab. "7R" (TABEL)?
020600110408     d ds7Pqrs       e ds                  inz
020700110408     d   ds7ric      e                     inz('S')
020800110408     d   ds7ges      e                     inz('V')
020900110408      /copy gaitrasrc/srcProtoPR,TRTB70R
021000110408
021100110408       // -?Ricerca/Controllo tabelle (TNTBE)?
021200110408     d TIBS02ds      e ds
021300110408     d   T02mod      e                     inz('R')
021400110408     d   T02cod      e                     inz(c_Tab)
021500110408      /copy gaitrasrc/srcprotopr,TIBS02R
021600090219
021700110408       // -?Parametri per pgm controllo/decodifica cliente?
021800110408      /copy gaitrasrc/srcProtoPI,TIBS69R
021900110408       // -?Controllo/Decodifica cliente?
022000090219      /copy gaitrasrc/srcprotopr,TIBS69R
022100090219
022200090219       //--------------------------------------------------------------
022300090219       //?Definizione key-list.                                        ?
022400090219       //--------------------------------------------------------------
022500090219
022600110411       // -?File TNTBE01L?
022700090219     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
022800090220     d                                     prefix(k_)   inz
022900090219
023000090219       //--------------------------------------------------------------
023100090219       //?Riepilogo indicatori utilizzati.                             ?
023200090219       //--------------------------------------------------------------
023300090219       //--------------------------------------------------------------
023400090219
023500090219       //--------------------------------------------------------------
023600090219       //?M A I N - L I N E                                            ?
023700090219       //--------------------------------------------------------------
023800090219
023900090219     c     *Entry        plist
024000090219     c                   parm                    KPJBA
024100111017     c                   parm                    TNTA61ds
024200090219
024300090219      /free
024400090219
024500110411       // -?Operazioni iniziali?
024600110411       exsr  sr_RoutInz;
024700090219
024800110411       // -?Gestione videate?
024900090219       DOW  $Fine = *off;
025000110412
025100090219         select;
025200110413
025300110413           // -?Gestione videata D1?
025400110413           //  ?(richiesta codice cliente O particolarità consegna)?
025500110413           when $Video = 'D1';
025600110413             exsr  sr_GesD01;
025700110412
025800110411           // -?Gestione videata D2?
025900110411           //  ?(manutenzione testi da registrare in tab. "PXA")?
026000090224           when $Video = 'D2';
026100110411             exsr  sr_GesD02;
026200110412
026300110411           // -?Gestione videata D3?
026400110411           //  ?(manutenzione testi da COPIARE in tab. "PXA")?
026500090420           when $Video = 'D3';
026600110411             exsr  sr_GesD03;
026700110412
026800110411           // -?Gestione videata W1?
026900110411           //  ?(richiesta dati per trasmissione record)?
027000090220           when $Video = 'W1';
027100110411             exsr  sr_GesW01;
027200110412
027300110411           // -?Gestione videata ????
027400090219           other;
027500090219             $Fine = *on;
027600110412
027700090219         endsl;
027800110412
027900090219       ENDDO;
028000090219
028100110411       // -?Operazioni finali?
028200110411       exsr  sr_RoutEnd;
028300090219
028400090219       //--------------------------------------------------------------
028500090219       //?Operazioni iniziali.                                         ?
028600090219       //--------------------------------------------------------------
028700090219       BEGSR  sr_RoutInz;
028800090219
028900110413         *inLR = *on;
029000090219
029100110411         // -?Reperimento dati job?
029200090219         exsr  sr_DatiJob;
029300090219
029400110411         // -?Impostazione nome programma a video?
029500090220         V1Tpgm = SDSpgm;
029600090219
029700110411         // -?Aggancio dati generali della tabella in esame?
029800090220         clear  k_TBEcod;
029900090220         k_TBEke1 = *zero;
030000090220         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
030100090220                = c_Tab;
030200090220         clear  k_TBEke2;
030300090220         clear  k_TBElin;
030400090220         k_TBEsif = KNSIF;
030500090219         chain(n)  %kds(k05tntbe01)  TNTBE000;
030600090220         if  not  %found(TNTBE01L);
030700090220           clear  k_TBEsif;
030800090220           chain(n)  %kds(k05tntbe01)  TNTBE000;
030900090220         endif;
031000090219         if  %found(TNTBE01L);
031100090219           xTNTBEds = TNTBEds;
031200090219         else;
031300090219           clear  xTNTBEds;
031400090219         endif;
031500090219
031600110411         // -?Impostazione iniziale / pulizia dei campi chiave?
031700090220         k_TBEcod = c_Tab;
031800090220         clear  k_TBEke1;
031900090220         clear  k_TBEke2;
032000090220         clear  k_TBElin;
032100090220         clear  k_TBEsif;
032200110413
032300110413         // -?Verifica se *pgm richiamato:?
032400110413         //  ?test delle prime 7 posizioni della KPJBA per sapere se il?
032500110413         //  ?pgm è stato richiamato da un altro pgm o da menù tabelle.?
032600110413         $Called = (%subst( KPJBU : 1 : %len(V1Cksc) ) <> *blank);
032700110413         clear  W0Cksc;
032800110413         clear  W0Cgma;
032900110413         select;
033000110413           when  NOT $Called;
033100110413           when  %check( c_Digits : %subst( KPJBU : 1 :
033200110413                                            %len(V1Cksc) ) ) = *zero;
033300110413             W0Cksc = %subst( KPJBU : 1 : %len(V1Cksc) );
033400110413           other;
033500110413             W0Cgma = %subst( KPJBU : 1 : %len(V1Cgma) );
033600110413         endsl;
033700111017
033800111017         // -?Verifica se *pgm richiamato da anagrafica clienti?
033900111017         SELECT;
034000111017           WHEN  %parms() > 1 and TA61ksc <= *zeros;
034100111017             TA61err = *on;
034200111017             TA61msg = 'Richiesto il codice cliente';
034300111017             $Fine = *on;
034400111017           WHEN  %parms() > 1;
034500111017             W0Cksc = %editc(TA61ksc:'X');
034600111017             $Called = *on;
034700111017             $Visualizza = *on;
034800111017         ENDSL;
034900090219
035000090219       ENDSR;
035100090219
035200090219       //--------------------------------------------------------------
035300090219       //?Reperimento Dati del job (Utente/Operativi).                 ?
035400090219       //--------------------------------------------------------------
035500090219       BEGSR  sr_DatiJob;
035600090219
035700090219         in(E) §AzUte;
035800090219         if NOT %error;
035900090219           in(E) §DatiUte;
036000090219         endif;
036100110411         if %error or RSut = *blank;
036200090219           clear TIBS34ds;
036300090219           tibs34r ( tibs34ds );
036400090219           in §AzUte;
036500090219           in §DatiUte;
036600090219         endif;
036700090219
036800090219       ENDSR;
036900090219
037000090219       //--------------------------------------------------------------
037100090219       //?Gestione videata D01                                         ?
037200090219       //--------------------------------------------------------------
037300090219       BEGSR  sr_GesD01;
037400090219
037500110411         // -?Inizializzazione videata?
037600090219         if  $InzD01 = *on;
037700090219           exsr  sr_InzD01;
037800090219           $InzD01 = *off;
037900110502         else;
038000110502           NotDspKSC = *off;
038100110502           NotDspGMA = *off;
038200090219         endif;
038300110412
038400110412         // -?SE ricevuto codice cliente NON si emette la prima videata?
038500110413         if  $Called  and  (W0Cksc > *zero  or  W0Cgma <> *blank);
038600110412           exsr  sr_CtrD01;
038700110412           if  Not ErrGenerico;
038800110412             $Video  = 'D2';
038900110412             $InzD02 = *on;
039000111017             IF  $Visualizza;
039100111017               $Immissione = *off;
039200111017               $Ripristino = *off;
039300111017             ENDIF;
039400110412             leavesr;
039500110413           else;
039600110413             clear W0Cksc;
039700110413             clear W0Cgma;
039800110412           endif;
039900110412         endif;
040000090219
040100110412         // -?Emissione Testata & Piede?
040200090220         write  TB71T01;
040300090220         write  TB71P01;
040400110412
040500110412         // -?Emissione videata?
040600090220         exfmt  TB71D01;
040700110411
040800110411         clear  ErrGenerico;
040900110411         clear  ErrMessage;
041000090220         clear  V1Dmsg;
041100090219
041200090219         SELECT;
041300090219
041400110411           // -?F3=Fine?
041500090219           WHEN  dsp_aid = c_F03;
041600090219             $Fine = *on;
041700090219
041800110411           // -?Invio?
041900090219           OTHER;
042000110408             exsr  sr_CtrD01;
042100110411             if  ErrGenerico = *on;
042200110411               leavesr;
042300110411             endif;
042400110411             $Video  = 'D2';
042500110411             $InzD02 = *on;
042600090219
042700090219         ENDSL;
042800090219
042900090219       ENDSR;
043000090219
043100090219       //--------------------------------------------------------------
043200090219       //?Inizializzazione videata D01                                 ?
043300090219       //--------------------------------------------------------------
043400090219       BEGSR  sr_InzD01;
043500090219
043600090220         clear  TB71D01;
043700110502         NotDspKSC = *off;
043800110502         NotDspGMA = *off;
043900110412
044000110412         // -?Se è stato richiamato con un codice particolarità?
044100110412         //  ?consegna o con un codice cliente, si imposta tale?
044200110412         //  ?codice a video?
044300110412         select;
044400110413           when  $Called  and  W0Cgma <> *blank;
044500110413             V1Cgma = W0Cgma;
044600110413           when  $Called  and  W0Cksc > *zero;
044700110413             V1Cksc = W0Cksc;
044800110413           other;
044900110412             V1Cksc = '?';
045000110412         endsl;
045100090219
045200090219       ENDSR;
045300090219
045400090219       //--------------------------------------------------------------
045500090219       //?Controllo dati immessi nella videata D01                     ?
045600090219       //--------------------------------------------------------------
045700090219       BEGSR  sr_CtrD01;
045800090219
045900090219         %subst(IndDspF : 28) = *off;
046000090219
046100110411         clear  V1Dksc;
046200090219         SELECT;
046300090219
046400110411           // -?Ricerca codice cliente?
046500110411           WHEN  %scan('?' : V1Cksc) > *zero;
046600090219             reset TIBS02ds;
046700090219             //T02mod = 'R';     (già così)
046800090219             //T02cod = c_Tab;   (già così)
046900090219             T02sif = KNSIF;
047000090220             TNTBE_RicercaControllo ( kpjba : tibs02ds );
047100110411             if  T02err = *blank;
047200110411               if  %len( %trimr( T02ke1 ) ) > %len( V1Cgma );
047300110411                 V1Cksc = %subst( T02ke1 : 1 : %len( V1Cksc ) );
047400110412                 clear  V1Cgma;
047500110411               else;
047600110411                 clear  V1Cksc;
047700110411                 V1Cgma = %subst( T02ke1 : 1 : %len( V1Cgma ) );
047800110411               endif;
047900090219               ErrGenerico = *on;
048000090219               leavesr;
048100090219             else;
048200090219               ErrGenerico = *on;
048300110411               ErrMessage  = *on;
048400090219               PosCurKSC   = *on;
048500090219               V1Dmsg = T02msg;
048600090219               leavesr;
048700090219             endif;
048800090219
048900110411           // -?Controllo immissione codice cliente o codice?
049000110411           //  ?particolarità consegna?
049100110411           WHEN  (V1Cksc = *blank  or  V1Cksc = *zero)  and
049200110411                 (V1Cgma = *blank  or  V1Cgma = *zero);
049300090219             ErrGenerico = *on;
049400110411             ErrMessage  = *on;
049500090219             PosCurKSC   = *on;
049600090219             V1Dmsg = $Msg(01);
049700090219             leavesr;
049800110411           WHEN  V1Cksc  > *zero  and
049900110411                 V1Cgma <> *blank;
050000110411             ErrGenerico = *on;
050100110411             ErrMessage  = *on;
050200110411             PosCurKSC   = *on;
050300110411             V1Dmsg = $Msg(02);
050400110411             leavesr;
050500090219
050600110411           // -?Controllo immissione solo caratteri numerici?
050700110411           WHEN  V1Cksc <> *blank  and
050800110411                 %check( c_Digits : V1Cksc ) > *zero;
050900090219             ErrGenerico = *on;
051000110411             ErrMessage  = *on;
051100090219             PosCurKSC   = *on;
051200110411             V1Dmsg = $Msg(03);
051300090219             leavesr;
051400090219
051500090219         ENDSL;
051600090219
051700110411         IF  V1Cksc > *zero;
051800110411
051900110411           // -?Controllo/decodifica cliente?
052000110411           clear  TIBS69ds;
052100110411           I69kcc = DUTkci;
052200110411           I69kac = %int( V1Cksc );
052300110411           I69sif = knsif;
052400110411           tibs69r(tibs69ds : ds_cnaco : ds_cnind : ds_cnclp : ds_fncls);
052500110411           if  O69err = *on;
052600110411             V1Dmsg = $Msg(04);
052700110411             ErrGenerico = *on;
052800110411             ErrMessage  = *on;
052900110411             PosCurKSC   = *on;
053000110411             leavesr;
053100110411           endif;
053200110411           V1Dksc = %subst(ACOrag : 1 : %len(V1Dksc));
053300110411
053400110411           // -?NON ammessi nuovi record per cliente annullato?
053500110411           reset  TNTBE000;
053600110412           k_TBEke1 = V1Cksc;
053700110412           k_TBEke2 = '001';
053800110412           setll  %kds(k05tntbe01 : 3)  TNTBE000;
053900110412           reade  %kds(k05tntbe01 : 2)  TNTBE000;
054000110412           $Immissione = (%eof(TNTBE01L));
054100110412           reset  $Ripristino;
054200110412           If  V1Cksc > *zero  and
054300110412               %eof(TNTBE01L)  and  ACOflg <> *blank;
054400110412             V1Dmsg = $Msg(05);
054500110412             ErrGenerico = *on;
054600110412             ErrMessage  = *on;
054700110412             PosCurKSC   = *on;
054800110412             leavesr;
054900110411           EndIf;
055000110411
055100110411         ENDIF;
055200110411
055300110408
055400110408         SELECT;
055500110408
055600110411           // -?Ricerca codice particolarità?
055700110408           WHEN  %scan('?' : V1Cgma) > *zero;
055800110408             clear  V1Cgma;
055900110408             reset  DS7Pqrs;
056000110408             clear  DS7cod;
056100110408             KPJBU = DS7Pqrs;
056200110408             TRTB70R  ( kpjba );
056300110408             DS7Pqrs = KPJBU;
056400110408             V1Cgma  = DS7cod;
056500110408             PosCurGMA   = *on;
056600110408             ErrGenerico = *on;
056700110408             leavesr;
056800110408
056900110411           // -?Controllo codice particolarità?
057000110413           WHEN  V1Cgma <> *blank;
057100110413             if  NOT $Called;
057200110412               clear  DS7Pqrs;
057300110412               DS7ric = 'S';
057400110412               DS7ges = 'C';
057500110412               //DS7ksc = %int( V1Cksc );
057600110412               DS7cod = V1Cgma;
057700110412               KPJBU = DS7Pqrs;
057800110412               TRTB70R  ( kpjba );
057900110412               DS7Pqrs = KPJBU;
058000110412               if  DS7err = *on;
058100110412                 V1Dmsg = DS7msg;
058200110412                 PosCurGMA   = *on;
058300110412                 ErrMessage  = *on;
058400110412                 ErrGenerico = *on;
058500110412                 leavesr;
058600110412               endif;
058700110412             endif;
058800110412             // -?Verifica esistenza records in tab. "PXA"?
058900110412             k_TBEke1 = V1Cgma;
059000110412             k_TBEke2 = '001';
059100110412             setll  %kds(k05tntbe01 : 3)  TNTBE000;
059200110412             reade  %kds(k05tntbe01 : 2)  TNTBE000;
059300110412             $Immissione = (%eof(TNTBE01L));
059400110412             reset  $Ripristino;
059500110408
059600110408         ENDSL;
059700110411
059800110411
059900110411         // -?Come chiave si considera il codice cliente (se inserito)?
060000110411         //  ?il codice particolarità consegne (altrimenti)?
060100110411         if  V1Cksc > *zero;
060200110411           clear  V1Cgma;
060300110411         else;
060400110411           clear  V1Cksc;
060500110411           clear  V1Dksc;
060600110411         endif;
060700090219
060800090219       ENDSR;
060900090219
061000090219       //--------------------------------------------------------------
061100090224       //?Gestione videata D02                                         ?
061200090219       //--------------------------------------------------------------
061300090224       BEGSR  sr_GesD02;
061400090219
061500110411         // -?Inizializzazione videata?
061600090224         if  $InzD02 = *on;
061700110411           exsr  sr_InzD02;
061800090224           $InzD02 = *off;
061900090219         endif;
062000090220
062100110411         // -?Emissione Testata e Piede con tasti funzionali abilitati?
062200090220         if  errGenerico = *off;
062300090220           write  TB71T01;
062400090220           write  TB71D01;
062500090220           write  Protect;
062600090220           write  TB71P02;
062700090220         endif;
062800111017
062900111017         // -?Se richiamato in interrogazione proteggo tutto il video?
063000111017         IF  $Visualizza;
063100111017           write TB71D02;
063200111017           exfmt Protect;
063300111017         ELSE;
063400090219
063500110411         // -?Emissione videata?
063600090224         exfmt  TB71D02;
063700111017         ENDIF;
063800110411         clear  ErrGenerico;
063900110411         clear  ErrMessage;
064000090219         clear  V1Dmsg;
064100090420         reset  $Annullamento;
064200090219
064300090219         SELECT;
064400090219
064500110411           // -?F3=Fine?
064600090219           WHEN  dsp_aid = c_F03;
064700090420             unlock TNTBE01L;
064800090219             $Fine = *on;
064900110411
065000110411           // -?F12=Ritorno?
065100110411           WHEN  dsp_aid = c_F12;
065200110411             unlock TNTBE01L;
065300110411             reset  $Video;
065400110411             clear  V1Topz;
065500110413             if  $Called;
065600110413               $Fine = *on;
065700110413             endif;
065800110414
065900110414           // -?Invio;?
066000110414           //  ?F5=Ripristino; F6=Conferma; F16=Annullamento;?
066100110414           //  ?F10=Copia?
066200110414           OTHER;
066300110414             exsr  sr_CtrD02;
066400110414             if  ErrGenerico;
066500110414               leavesr;
066600110414             endif;
066700110414             Select;
066800110414               // -?F5=Ripristino; F6=Conferma; F16=Annullamento?
066900110414               When  dsp_aid = c_F05  or
067000110414                     dsp_aid = c_F06  or
067100110414                     dsp_aid = c_F16;
067200110414                 $Annullamento = (dsp_aid = c_F16);
067300110421                 $VideoPrec = $Video;
067400110414                 $Video  = 'W1';
067500110414                 reset  $InzW01;
067600110414               // -?F10=Copia?
067700110414               When  dsp_aid = c_F10;
067800110414                 unlock TNTBE01L;
067900110414                 $Video  = 'D3';
068000110414                 reset  $InzD03;
068100110414             EndSl;
068200090219
068300090219         ENDSL;
068400090219
068500090219       ENDSR;
068600090219
068700090219       //--------------------------------------------------------------
068800090224       //?Inizializzazione videata D02                                 ?
068900090219       //--------------------------------------------------------------
069000090224       BEGSR  sr_InzD02;
069100090219
069200110411         // -?Spegnimento di tutti gli indicatori?
069300110411         IndDspF = *off;
069400110420
069500110420         // -?NON visualizzazione del campo chiave NON gestito?
069600110420         NotDspKSC = (V1Cksc = *blank);
069700110420         NotDspGMA = (V1Cgma = *blank);
069800090220
069900110411         // -?Pulizia videata?
070000090224         clear  TB71D02;
070100090219
070200110411         // -?Caricamento testi per il cliente o per la particolarità?
070300110411         //  ?consegne?
070400110411         if  V1Cksc > *zero;
070500110411           k_TBEke1 = V1Cksc;
070600110411         else;
070700110411           k_TBEke1 = V1Cgma;
070800110411         endif;
070900110411         // ·?Testi per LDV?
071000110411         k_TBEke2 = '001';
071100110408         setll  %kds(k05tntbe01 : 3)  TNTBE000;
071200110411         For  xx = 1  To  c_MaxRec;
071300090224           if  Not $Immissione   and   Not %eof(TNTBE01L);
071400090224             reade  %kds(k05tntbe01 : 2)  TNTBE000;
071500090224           endif;
071600110412           //if  $Immissione   OR  (NOT %eof(TNTBE01L)  and
071700110412           //    %trimr(TBEke2) >= '001 '  and  %trimr(TBEke2) <= '999 ');
071800110408             exsr  sr_RieD02;
071900110412           //endif;
072000110411         EndFor;
072100110411         // ·?Testi per PDA?
072200110411         k_TBEke2 = 'PDA';
072300110411         chain  %kds(k05tntbe01 : 3)  TNTBE000;
072400110413         if  %found(TNTBE01L)  and
072500110413             ( (TBEatb  = *blank  and  Not $Ripristino)  or
072600110413               (TBEatb <> *blank  and  $Ripristino) );
072700110411           D02txtPDA = %subst( TBEuni : 1 : %len( D02txtPDA ) );
072800110411         endif;
072900090224
073000110411         // -?Impostazione tipo manutenzione?
073100090224         select;
073200111017           when  $Visualizza;
073300111017             V1Topz = 'VISUALIZZA';
073400110412           when  $Immissione  and  NOT %found(TNTBE01L);
073500090224             V1Topz = 'IMMISSIONE';
073600110412           when  $Ripristino   or
073700110412                 (%found(TNTBE01L)  and  TBEatb <> *blank);
073800090224             V1Topz = 'RIPRISTINO';
073900090224           other;
074000090224             V1Topz = ' MODIFICA ';
074100090224         endsl;
074200090220
074300110411         // -?(Ri)Abilitazione tasti funzionali?
074400110411         exsr  sr_AbilFxxD02;
074500090220
074600090220       ENDSR;
074700090220
074800090220       //--------------------------------------------------------------
074900090224       //?Caricamento singola riga di testo nella videata D02          ?
075000090220       //--------------------------------------------------------------
075100090224       BEGSR  sr_RieD02;
075200110412
075300110411         H02prg(xx) = xx;
075400090224         D02prg(xx) = xx;
075500110412
075600110412         if  $Immissione;
075700110412           $Ripristino = *off;
075800110412           leavesr;
075900110412         endif;
076000110412
076100110412         // -?Progressivo oltre il massimo previsto?
076200110419         if  Not %eof(TNTBE01L)         and
076300110419            (%int( %trimr(TBEke2) ) < 1  or
076400110419             %int( %trimr(TBEke2) ) > c_MaxRec);
076500110412           leavesr;
076600110412         endif;
076700110412
076800110412         // -?Lasciate righe vuote?
076900110419         DoW  Not %eof(TNTBE01L)  and  xx < %int( %trimr(TBEke2) );
077000110412           xx += 1;
077100110412           H02prg(xx) = xx;
077200110412           D02prg(xx) = xx;
077300110412         EndDo;
077400110413
077500110413         // -?Verifica se note TUTTE annullate (ripristinabili)?
077600110419         if  $Ripristino  and  Not %eof(TNTBE01L);
077700110419           $Ripristino = (TBEatb <> *blank);
077800110413         endif;
077900090224
078000110412         // -?Impostazione riga di testo a video?
078100110413         if  Not %eof(TNTBE01L)  and
078200110413             ( (Not $Ripristino  and  TBEatb  = *blank)  or
078300110413               ($Ripristino      and  TBEatb <> *blank) );
078400090224           D02txt(xx) = %subst( TBEuni : 1 : %len( D02txt(xx) ) );
078500090224         endif;
078600090220
078700090220       ENDSR;
078800090219
078900090219       //--------------------------------------------------------------
079000110411       //?Abilitazione tasti funzionali in P02 (per D02).              ?
079100090219       //--------------------------------------------------------------
079200110411       BEGSR  sr_AbilFxxD02;
079300110413
079400110413         // ->?F3 = Fine?
079500110413         F3Attivo = (NOT $Called);
079600090219
079700110411         // -?F5=Ripristino?
079800090220         F5Attivo  = (NOT $Immissione  and  $Ripristino);
079900090220
080000110411         // -?F6=Conferma?
080100111017         F6Attivo  = (NOT F5Attivo and NOT $Visualizza);
080200090420
080300110411         // -?F10=Copia?
080400111017         F10Attivo = (NOT $Immissione  and  NOT $Ripristino and
080500111017                      NOT $Visualizza);
080600090220
080700110411         // -?F16=Annullamento?
080800111017         F16Attivo = ( (NOT $Immissione   and  NOT $Ripristino and
080900111017                        NOT $Visualizza)  OR
081000110412                       (%found(TNTBE01L)  and  TBEatb = *blank) );
081100090219
081200090219       ENDSR;
081300110413
081400110413       //--------------------------------------------------------------
081500110413       //?Controllo dati immessi nella videata D02                     ?
081600110413       //--------------------------------------------------------------
081700110413       BEGSR  sr_CtrD02;
081800110413
081900110420         %subst(IndDspF : 50) = *off;
082000110413
082100110413         // -?Mancano le note e NON premuto F16=Annullamento?
082200110414         if  dsp_aid <> c_F16    and
082300110414             dsD02txt  = *blank  and  D02txtPDA = *blank;
082400110413           ErrGenerico = *on;
082500110413           ErrMessage  = *on;
082600110414           PosCurTXT   = *on;
082700110413           V1Dmsg = $Msg(06);
082800110413           leavesr;
082900110413         endif;
083000110413
083100110413       ENDSR;
083200090420
083300090420       //--------------------------------------------------------------
083400090420       //?Gestione videata D03                                         ?
083500090420       //--------------------------------------------------------------
083600090420       BEGSR  sr_GesD03;
083700090420
083800110411         // -?Inizializzazione videata?
083900090420         if  $InzD03 = *on;
084000110411           exsr  sr_InzD03;
084100090420           $InzD03 = *off;
084200090420         endif;
084300090420
084400110411         // -?Emissione Testata e Piede con tasti funzionali abilitati?
084500090420         if  errGenerico = *off;
084600090420           write  TB71T01;
084700090420           write  TB71D01;
084800090420           write  Protect;
084900090420           write  TB71P03;
085000090420         endif;
085100090420
085200110411         // -?Emissione videata?
085300090420         exfmt  TB71D03;
085400110411         clear  ErrGenerico;
085500110411         clear  ErrMessage;
085600090420         clear  V1Dmsg;
085700090420
085800090420         SELECT;
085900090420
086000110411           // -?F3=Fine?
086100090420           WHEN  dsp_aid = c_F03;
086200090420             $Fine = *on;
086300090420
086400110411           // -?F12=Ritorno?
086500090420           WHEN  dsp_aid = c_F12;
086600110413             unlock TNTBE01L;
086700090420             reset  $Video;
086800090420             clear  V1Topz;
086900110413             if  $Called;
087000110413               $Fine = *on;
087100110413             endif;
087200090420
087300110411           // -?Invio; F5=Ripristino; F6=Conferma; F16=Annullamento?
087400090420           OTHER;
087500090420             exsr  sr_CtrD03;
087600090420             if  ErrGenerico = *on;
087700090420               leavesr;
087800090420             endif;
087900110411             // -?F5=Ripristino; F6=Conferma; F16=Annullamento?
088000090420             if  dsp_aid = c_F05  or
088100090420                 dsp_aid = c_F06  or
088200090420                 dsp_aid = c_F16;
088300110421               $VideoPrec = $Video;
088400090420               $Video  = 'W1';
088500090420               reset  $InzW01;
088600090420             endif;
088700090420
088800090420         ENDSL;
088900090420
089000090420       ENDSR;
089100090420
089200090420       //--------------------------------------------------------------
089300090420       //?Inizializzazione videata D03                                 ?
089400090420       //--------------------------------------------------------------
089500090420       BEGSR  sr_InzD03;
089600090420
089700110411         // -?Spegnimento di tutti gli indicatori?
089800090420         IndDspF = *off;
089900090420
090000110411         // -?Pulizia PARZIALE videata?
090100090420         clear  V3Cksc;
090200090420         clear  V3Dksc;
090300110408         clear  V3Cgma;
090400090420
090500110411         // -?Caricamento testi per il cliente?
090600110411         // ·?Testi per LDV?
090700110411         //k_TBEke2 = '001';
090800110411         //setll  %kds(k05tntbe01 : 3)  TNTBE000;
090900110411         //For  xx = 1  To  c_MaxRec;
091000110411         //  if  Not %eof(TNTBE01L);
091100110411         //    reade  %kds(k05tntbe01 : 2)  TNTBE000;
091200110411         //  endif;
091300110411         //  exsr  sr_RieD03;
091400110411         //EndFor;
091500110411         // ·?Testi per PDA?
091600110411         //k_TBEke2 = 'PDA';
091700110411         //chain  %kds(k05tntbe01 : 3)  TNTBE000;
091800110411         //if  %found(TNTBE01L)  and  TBEatb = *blank;
091900110411         //  D02txtPDA = %subst( TBEuni : 1 : %len( D02txtPDA ) );
092000110411         //endif;
092100090420
092200110411         // -?Impostazione tipo manutenzione?
092300090420         V1Topz = '  COPIA   ';
092400110411
092500110411         // -?Disabilitazione ripristino annullate (in fase di copia)?
092600110411         $Ripristino = *off;
092700090420
092800090420       ENDSR;
092900090420
093000090420       //--------------------------------------------------------------
093100090420       //?Caricamento singola riga di testo nella videata D03          ?
093200090420       //--------------------------------------------------------------
093300110412       //BEGSR  sr_RieD03;
093400110412       //
093500110412       //  H02prg(xx) = xx;
093600110412       //  D02prg(xx) = xx;
093700110412       //
093800110412       //  // -?Progressivo oltre il massimo previsto?
093900110412       //  if  %int( %trimr(TBEke2) ) < 1  or
094000110412       //      %int( %trimr(TBEke2) ) > c_MaxRec;
094100110412       //    leavesr;
094200110412       //  endif;
094300110412       //
094400110412       //  // -?Lasciate righe vuote?
094500110412       //  DoW  xx < %int( %trimr(TBEke2) );
094600110412       //    xx += 1;
094700110412       //    H02prg(xx) = xx;
094800110412       //    D02prg(xx) = xx;
094900110412       //  EndDo;
095000110412       //
095100110412       //  // -?Impostazione riga di testo a video?
095200110412       //  if  Not %eof(TNTBE01L);
095300110412       //    D02txt(xx) = %subst( TBEuni : 1 : %len( D02txt(xx) ) );
095400110412       //  endif;
095500110412       //
095600110412       //ENDSR;
095700090420
095800090420       //--------------------------------------------------------------
095900090420       //?Controllo dati immessi nella videata D03                     ?
096000090420       //--------------------------------------------------------------
096100090420       BEGSR  sr_CtrD03;
096200110420
096300110420         %subst(IndDspF : 50) = *off;
096400090420
096500110411         // -?Controllo codice cliente / codice particolarità?
096600090420         clear  V3Dksc;
096700090420         V1Cksc = %editc( V3Cksc : 'X' );
096800110408         V1Cgma = V3Cgma;
096900090420         exsr  sr_CtrD01;
097000090420         V3Dksc = V1Dksc;
097100110421         if  ErrGenerico = *on;
097200110421           leavesr;
097300110421         endif;
097400090420
097500110411         // -?Verifica esistenza record in tab. "PXA" per il cliente o?
097600110411         //  ?la particolarità consegna indicate a video?
097700090420         reset  TNTBE000;
097800090420         k_TBEcod = c_Tab;
097900110421         if  V1Cksc > *zero;
098000110421           k_TBEke1 = V1Cksc;
098100110411         else;
098200110421           k_TBEke1 = V1Cgma;
098300110411         endif;
098400110408         k_TBEke2 = '001 ';
098500090420         clear  k_TBElin;
098600090420         clear  k_TBEsif;
098700110408         setll  %kds(k05tntbe01 : 3)  TNTBE000;
098800110408         reade  %kds(k05tntbe01 : 2)  TNTBE000;
098900110408         $Immissione = (%eof(TNTBE01L));
099000090420         reset  $Ripristino;
099100110408         if  Not %eof(TNTBE01L);
099200090420           ErrGenerico = *on;
099300110411           ErrMessage  = *on;
099400110421           if  V3Cksc > *zero;
099500110421             PosCurKSC   = *on;
099600110421             V1Dmsg = %trimr( $Msg(07) ) + ' il cliente ' + V1Cksc;
099700110421           else;
099800110421             PosCurGMA   = *on;
099900110421             V1Dmsg = %trimr( $Msg(07) ) + ' la particolarità +
100000110421                                           consegna ' + V1Cgma;
100100110421           endif;
100200090420           leavesr;
100300090420         endif;
100400110414
100500110414         // -?Verifica inserimento corretto di note?
100600110414         exsr  sr_CtrD02;
100700110414         if  ErrGenerico;
100800110414           leavesr;
100900110414         endif;
101000090420
101100090420       ENDSR;
101200090220
101300090220       //--------------------------------------------------------------
101400090220       //?Gestione trasmissione aggiornamento                          ?
101500090220       //--------------------------------------------------------------
101600090220       BEGSR  sr_GesW01;
101700090220
101800110411         // -?Inizializzazione videata?
101900090220         if $InzW01 = *on;
102000090224           exsr  sr_InzW01;
102100090220           $InzW01 = *off;
102200090220         endif;
102300090220
102400110411         // -?Emissione window?
102500110421         IF  TBXftt = 'S';
102600090220           exfmt  TB71W01;
102700110411           clear  ErrGenerico;
102800110411           clear  ErrMessage;
102900090220           clear  V1Dmsg;
103000110421           Select;
103100110421             // -?F12=Ritorno (gestito solo se emesso W01)?
103200110421             When  dsp_aid = c_F12;
103300110421               $Video = $VideoPrec;
103400110421               leavesr;
103500110421             // -?Invio (gestito solo se emesso W01)?
103600110421             When  dsp_aid <> c_F06;
103700110421               leavesr;
103800110421           EndSl;
103900110421         ENDIF;
104000110421
104100110421         // -?Aggiornamento TNTBE00F?
104200110421         exsr  sr_UpdTNTBE;
104300110421         if  NOT  ErrGenerico;
104400110421           exsr  sr_UpdTNTBE_PDA;
104500110421         endif;
104600110421         select;
104700110421           // ·?Rilevato errore in fase di aggiornamento?
104800110421           when  ErrGenerico;
104900110421             leavesr;
105000110421           // ·?Uscita dal programma SE richiamato?
105100110421           when  $Called  and  (W0Cksc > *zero  or  W0Cgma <> *blank);
105200110421             $Fine = *on;
105300110421           // ·?Videata iniziale altrimenti?
105400110421           other;
105500110421             clear  V1Topz;
105600110421             $Video  = 'D1';
105700110421             $InzD01 = *on;
105800110421         endsl;
105900090220
106000090220       ENDSR;
106100090220
106200090220       //--------------------------------------------------------------
106300090220       //?Inizializzazione videata W01                                 ?
106400090220       //--------------------------------------------------------------
106500090220       BEGSR  sr_InzW01;
106600090220
106700090220         clear TB71W01;
106800090220
106900090220         if  $Immissione;
107000090220
107100110411           // -?Se immissione?
107200090220           W1ftt  = TBXftt;
107300090220
107400090220         else;
107500090220
107600110411           // -?Se NON immissione:?
107700110411           //  ?visualizza i dati relativi all'ultima trasmissione?
107800090220           W1ftt  = TBEftt;
107900090220           W1flt  = TBEflt;
108000090220           W1ftr  = TBEftr;
108100090220           if TBEdtr <> *zero;
108200090220             wDate_EUR = %date(TBEdtr : *iso);
108300090220             W1dtr     = %dec(wDate_EUR);
108400090220           endif;
108500090220
108600090220         endif;
108700090220
108800090220       ENDSR;
108900090219
109000090219       //--------------------------------------------------------------
109100110408       //?Aggiornamento records TNTBE00F (tab. PXA) x LdV              ?
109200090219       //--------------------------------------------------------------
109300110411       BEGSR  sr_UpdTNTBE;
109400090219
109500110411         // -?Ciclo di lettura del subfile?
109600090224         FOR  xx = 1  TO  c_MaxRec;
109700090220
109800110411           // -?Aggancio record di TNTBE00F?
109900110411           k_TBEcod = c_Tab;
110000110411           if  V1Cksc > *zero;
110100110411             k_TBEke1 = V1Cksc;
110200110411           else;
110300110411             k_TBEke1 = V1Cgma;
110400110411           endif;
110500110411           k_TBEke2 = %editc(H02prg(xx) : 'X');
110600090220           clear  k_TBElin;
110700090220           clear  k_TBEsif;
110800110411           //k_TBElin = TBXlin;
110900110411           //k_TBEsif = TBXsif;
111000090220
111100110412           chain  %kds(k05tntbe01) TNTBE000;
111200090220
111300110411           // -?Aggiornamento singolo record?
111400110412           Select;
111500090223
111600110411             // -?Skip dei nuovi record vuoti?
111700110412             When  not  %found(TNTBE01L)   and   D02txt(xx) = *blank;
111800090220
111900110411             // -?Cancellazione record svuotato nel sfl?
112000110412             When  %found(TNTBE01L)   and   D02txt(xx) = *blank;
112100110411               ////_______________
112200110411               //DELETE  TNTBE000;
112300110411               ////¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
112400110412               If  TBEatb = *blank;
112500110412                 TBEatb = 'A';
112600110412                 TBEftt = W1ftt;
112700110412                 TBEFlt = W1flt;
112800110412                 clear  TBEftr;
112900110412                 //clear TBEdtr;
113000110412                 //_____________
113100110412                 UPDATE TNTBE000;
113200110412                 //¯¯¯¯¯¯¯¯¯¯¯¯¯
113300110412               EndIf;
113400090220
113500110412             // -?Annullamento o Immissione o Modifica?
113600110412             Other;
113700090220               if  NOT %found(TNTBE01L);
113800090220                 clear TNTBE000;
113900090220                 TBEcod  = k_TBEcod;
114000090220                 TBEke1  = k_TBEke1;
114100090224                 TBEke2  = k_TBEke2;
114200090220                 TBElin  = k_TBElin;
114300090220                 TBEsif  = k_TBEsif;
114400090220               endif;
114500090220               TBEapl = TBXapl;
114600090420               if  $Annullamento;
114700090420                 TBEatb = 'A';
114800090420               else;
114900090420                 clear  TBEatb;
115000090420               endif;
115100090220               TBEftt = W1ftt;
115200090220               TBEFlt = W1flt;
115300090224               clear  TBEftr;
115400090220               //clear TBEdtr;
115500110412               // ·?Aggiornamento record?
115600090224               TBEuni = D02txt(xx);
115700090420               if  %found(TNTBE01L);
115800090420                 //_____________
115900090420                 UPDATE TNTBE000;
116000090420                 //¯¯¯¯¯¯¯¯¯¯¯¯¯
116100090420               else;
116200090420                 //_____________
116300090420                 WRITE  TNTBE000;
116400090420                 //¯¯¯¯¯¯¯¯¯¯¯¯¯
116500090420               endif;
116600090220
116700110412           EndSl;
116800090220
116900090224         ENDFOR;
117000090219
117100090219       ENDSR;
117200110408
117300110408       //--------------------------------------------------------------
117400110408       //?Aggiornamento records TNTBE00F (tab. PXA) x PDA              ?
117500110408       //--------------------------------------------------------------
117600110411       BEGSR  sr_UpdTNTBE_PDA;
117700110408
117800110411         // -?Aggancio record di TNTBE00F?
117900110411         k_TBEcod = c_Tab;
118000110411         if  V1Cksc > *zero;
118100110411           k_TBEke1 = V1Cksc;
118200110411         else;
118300110411           k_TBEke1 = V1Cgma;
118400110411         endif;
118500110411         k_TBEke2 = 'PDA ';
118600110408         clear  k_TBElin;
118700110408         clear  k_TBEsif;
118800110408         //k_TBElin  = TBXlin;
118900110408         //k_TBEsif  = TBXsif;
119000110408
119100110412         chain  %kds(k05tntbe01) TNTBE000;
119200110408
119300110411         // -?Aggiornamento singolo record?
119400110412         Select;
119500110408
119600110411           // -?Skip del nuovo record vuoto?
119700110412           When  not  %found(TNTBE01L)   and   D02txtPDA = *blank;
119800110408
119900110411           // -?Cancellazione record svuotato nel sfl?
120000110412           When  %found(TNTBE01L)   and   D02txtPDA = *blank;
120100110411             ////_______________
120200110411             //DELETE  TNTBE000;
120300110411             ////¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
120400110412             If  TBEatb = *blank;
120500110412               TBEatb = 'A';
120600110412               TBEftt = W1ftt;
120700110412               TBEFlt = W1flt;
120800110412               clear  TBEftr;
120900110412               //clear TBEdtr;
121000110412               //_____________
121100110412               UPDATE TNTBE000;
121200110412               //¯¯¯¯¯¯¯¯¯¯¯¯¯
121300110412             EndIf;
121400110408
121500110412           // -?Annullamento o Immissione o Modifica?
121600110412           Other;
121700110408             if  NOT %found(TNTBE01L);
121800110408               clear TNTBE000;
121900110408               TBEcod  = k_TBEcod;
122000110408               TBEke1  = k_TBEke1;
122100110408               TBEke2  = k_TBEke2;
122200110408               TBElin  = k_TBElin;
122300110408               TBEsif  = k_TBEsif;
122400110408             endif;
122500110408             TBEapl = TBXapl;
122600110408             if  $Annullamento;
122700110408               TBEatb = 'A';
122800110408             else;
122900110408               clear  TBEatb;
123000110408             endif;
123100110408             TBEftt = W1ftt;
123200110408             TBEFlt = W1flt;
123300110408             clear  TBEftr;
123400110408             //clear TBEdtr;
123500110412             // ·?Aggiornamento record?
123600110412             TBEuni = D02txtPDA;
123700110408             if  %found(TNTBE01L);
123800110408               //_____________
123900110408               UPDATE TNTBE000;
124000110408               //¯¯¯¯¯¯¯¯¯¯¯¯¯
124100110408             else;
124200110408               //_____________
124300110408               WRITE  TNTBE000;
124400110408               //¯¯¯¯¯¯¯¯¯¯¯¯¯
124500110408             endif;
124600110408
124700110408         ENDSL;
124800110408
124900110408       ENDSR;
125000110412
125100110412       //--------------------------------------------------------------
125200110412       //?Annullamento records TNTBE00F (tab. PXA) x LdV               ?
125300110412       //--------------------------------------------------------------
125400110412       BEGSR  sr_AnnullTNTBE;
125500110412
125600110412         k_TBEke2 = *loval;
125700110412         setll  %kds(k05tntbe01 : 3)  TNTBE000;
125800110412         reade  %kds(k05tntbe01 : 2)  TNTBE000;
125900110412
126000110412         DoW  Not  %eof(TNTBE01L);
126100110412           TBEatb = 'A';
126200110412           TBEftt = W1ftt;
126300110412           TBEFlt = W1flt;
126400110412           clear  TBEftr;
126500110412           //clear TBEdtr;
126600110412           //_____________
126700110412           UPDATE TNTBE000;
126800110412           //¯¯¯¯¯¯¯¯¯¯¯¯¯
126900110412           reade  %kds(k05tntbe01 : 2)  TNTBE000;
127000110412         EndDo;
127100110412
127200110412       ENDSR;
127300110413
127400110413       //--------------------------------------------------------------
127500110413       //?Ripristino records TNTBE00F (tab. PXA) x LdV                 ?
127600110413       //--------------------------------------------------------------
127700110413       BEGSR  sr_RiprisTNTBE;
127800110413
127900110413         k_TBEke2 = *loval;
128000110413         setll  %kds(k05tntbe01 : 3)  TNTBE000;
128100110413         reade  %kds(k05tntbe01 : 2)  TNTBE000;
128200110413
128300110413         DoW  Not  %eof(TNTBE01L);
128400110413           clear  TBEatb;
128500110413           TBEftt = W1ftt;
128600110413           TBEFlt = W1flt;
128700110413           clear  TBEftr;
128800110413           //clear TBEdtr;
128900110413           //_____________
129000110413           UPDATE TNTBE000;
129100110413           //¯¯¯¯¯¯¯¯¯¯¯¯¯
129200110413           reade  %kds(k05tntbe01 : 2)  TNTBE000;
129300110413         EndDo;
129400110413
129500110413       ENDSR;
129600090219
129700090219       //--------------------------------------------------------------
129800090219       //?Operazioni finali.                                           ?
129900090219       //--------------------------------------------------------------
130000090219       BEGSR  sr_RoutEnd;
130100090219
130200110411         // -?Chiusura funzioni precedentemente aperte?
130300090219         clear  TIBS69ds;
130400090219         I69tla = 'C';
130500090219         tibs69r (TIBS69ds : ds_CNACO :
130600090219                             ds_CNIND :
130700090219                             ds_CNCLP :
130800090219                             ds_FNCLS);
130900090219
131000110412         // -?Uscita dal *pgm?
131100090219         return;
131200090219
131300090219       ENDSR;
131400090219
131500090219      /end-free
131600090219
131700090219       //--------------------------------------------------------------
131800090219       //?Definizione schiere a tempo di compilazione                  ?
131900090219       //--------------------------------------------------------------
132000090219
132100110411** - $Msg:?Messaggi di Errore?-----------------------------------------------*
132200110411Immettere il codice cliente oppure il codice particolarità consegna             1
132300110411Immettere solo il codice cliente o solo il codice particolarità consegna        2
132400110411Immettere SOLO caratteri numerici                                               3
132500110411Codice cliente errato                                                           4
132600110411Cliente annullato                                                               5
132700110413Inserire almeno una nota                                                        6
132800110421Promemoria già inserito per                                                     7
