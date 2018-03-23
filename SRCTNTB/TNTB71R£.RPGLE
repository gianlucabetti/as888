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
013200090219
013300110411       // -?Dati record principale della tabella?
013400090219     d TNTBEds       e ds                  inz  extname(TNTBE00F)
013500090219     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
013600090220     d                                          prefix(TBX:3)
013700090224
013800110411       // -?Ridefinizione dati a video?
013900110411     d dsH02prg        ds                  inz
014000110411     d   H02prg1                           inz
014100110411     d   H02prg2                           inz
014200110411     d   H02prg3                           inz
014300110411     d   H02prg4                           inz
014400110411     d   H02prg5                           inz
014500110411     d   H02prg6                           inz
014600110411     d   H02prg                1     18s 0 dim(c_MaxRec)  inz
014700110411
014800090224     d dsD02prg        ds                  inz
014900090224     d   D02prg1                           inz
015000090224     d   D02prg2                           inz
015100090224     d   D02prg3                           inz
015200090224     d   D02prg4                           inz
015300090224     d   D02prg5                           inz
015400090224     d   D02prg6                           inz
015500110411     d   D02prg                1      6s 0 dim(c_MaxRec)  inz
015600090224
015700090224     d dsD02txt        ds                  inz
015800090224     d   D02txt1                           inz
015900090224     d   D02txt2                           inz
016000090224     d   D02txt3                           inz
016100090224     d   D02txt4                           inz
016200090224     d   D02txt5                           inz
016300090224     d   D02txt6                           inz
016400090224     d   D02txt                1    456    dim(c_MaxRec)  inz
016500090219
016600090219       //--------------------------------------------------------------
016700090219       //?Definizione variabili globali.                               ?
016800090219       //--------------------------------------------------------------
016900090219
017000110411       // -?Flags booleani?
017100090219     d $Fine           s               n   inz
017200090219     d $InzD01         s               n   inz(*on)
017300090224     d $InzD02         s               n   inz(*on)
017400090420     d $InzD03         s               n   inz(*on)
017500090220     d $InzW01         s               n   inz(*on)
017600090420     d $Annullamento   s               n   inz
017700090220     d $Immissione     s               n   inz
017800110412     d $Ripristino     s               n   inz(*on)
017900110413     d $Called         s               n   inz
018000090220
018100110411       // -?Indici di schiera?
018200090220     d xx              s              3  0 inz
018300090219
018400110411       // -?Variabili per la gestione del video?
018500090219     d $Video          s              2    inz('D1')
018600110421     d $VideoPrec      s                   like($Video)  inz
018700110413
018800110413       // -?Codice cliente o particolarità consegna eventualmente ricevuto?
018900110413     d W0Cksc          s                   like(V1Cksc)  inz
019000110413     d W0Cgma          s                   like(V1Cgma)  inz
019100090220
019200110411       // -?Campi di comodo?
019300090220     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
019400090219
019500090219       //--------------------------------------------------------------
019600090219       //?Definizione prototipi procedure.                             ?
019700090219       //--------------------------------------------------------------
019800090219
019900110408       // -?Reperimento dati utente?
020000110408     d TIBS34ds      e ds                  inz
020100110408      /copy gaitrasrc/srcProtoPR,TIBS34R
020200090219
020300110408       // -?Interrogazione particolarità consegne - tab. "7R" (TABEL)?
020400110408     d ds7Pqrs       e ds                  inz
020500110408     d   ds7ric      e                     inz('S')
020600110408     d   ds7ges      e                     inz('V')
020700110408      /copy gaitrasrc/srcProtoPR,TRTB70R
020800110408
020900110408       // -?Ricerca/Controllo tabelle (TNTBE)?
021000110408     d TIBS02ds      e ds
021100110408     d   T02mod      e                     inz('R')
021200110408     d   T02cod      e                     inz(c_Tab)
021300110408      /copy gaitrasrc/srcprotopr,TIBS02R
021400090219
021500110408       // -?Parametri per pgm controllo/decodifica cliente?
021600110408      /copy gaitrasrc/srcProtoPI,TIBS69R
021700110408       // -?Controllo/Decodifica cliente?
021800090219      /copy gaitrasrc/srcprotopr,TIBS69R
021900090219
022000090219       //--------------------------------------------------------------
022100090219       //?Definizione key-list.                                        ?
022200090219       //--------------------------------------------------------------
022300090219
022400110411       // -?File TNTBE01L?
022500090219     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
022600090220     d                                     prefix(k_)   inz
022700090219
022800090219       //--------------------------------------------------------------
022900090219       //?Riepilogo indicatori utilizzati.                             ?
023000090219       //--------------------------------------------------------------
023100090219       //--------------------------------------------------------------
023200090219
023300090219       //--------------------------------------------------------------
023400090219       //?M A I N - L I N E                                            ?
023500090219       //--------------------------------------------------------------
023600090219
023700090219     c     *Entry        plist
023800090219     c                   parm                    KPJBA
023900090219
024000090219      /free
024100090219
024200110411       // -?Operazioni iniziali?
024300110411       exsr  sr_RoutInz;
024400090219
024500110411       // -?Gestione videate?
024600090219       DOW  $Fine = *off;
024700110412
024800090219         select;
024900110413
025000110413           // -?Gestione videata D1?
025100110413           //  ?(richiesta codice cliente O particolarità consegna)?
025200110413           when $Video = 'D1';
025300110413             exsr  sr_GesD01;
025400110412
025500110411           // -?Gestione videata D2?
025600110411           //  ?(manutenzione testi da registrare in tab. "PXA")?
025700090224           when $Video = 'D2';
025800110411             exsr  sr_GesD02;
025900110412
026000110411           // -?Gestione videata D3?
026100110411           //  ?(manutenzione testi da COPIARE in tab. "PXA")?
026200090420           when $Video = 'D3';
026300110411             exsr  sr_GesD03;
026400110412
026500110411           // -?Gestione videata W1?
026600110411           //  ?(richiesta dati per trasmissione record)?
026700090220           when $Video = 'W1';
026800110411             exsr  sr_GesW01;
026900110412
027000110411           // -?Gestione videata ????
027100090219           other;
027200090219             $Fine = *on;
027300110412
027400090219         endsl;
027500110412
027600090219       ENDDO;
027700090219
027800110411       // -?Operazioni finali?
027900110411       exsr  sr_RoutEnd;
028000090219
028100090219       //--------------------------------------------------------------
028200090219       //?Operazioni iniziali.                                         ?
028300090219       //--------------------------------------------------------------
028400090219       BEGSR  sr_RoutInz;
028500090219
028600110413         *inLR = *on;
028700090219
028800110411         // -?Reperimento dati job?
028900090219         exsr  sr_DatiJob;
029000090219
029100110411         // -?Impostazione nome programma a video?
029200090220         V1Tpgm = SDSpgm;
029300090219
029400110411         // -?Aggancio dati generali della tabella in esame?
029500090220         clear  k_TBEcod;
029600090220         k_TBEke1 = *zero;
029700090220         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
029800090220                = c_Tab;
029900090220         clear  k_TBEke2;
030000090220         clear  k_TBElin;
030100090220         k_TBEsif = KNSIF;
030200090219         chain(n)  %kds(k05tntbe01)  TNTBE000;
030300090220         if  not  %found(TNTBE01L);
030400090220           clear  k_TBEsif;
030500090220           chain(n)  %kds(k05tntbe01)  TNTBE000;
030600090220         endif;
030700090219         if  %found(TNTBE01L);
030800090219           xTNTBEds = TNTBEds;
030900090219         else;
031000090219           clear  xTNTBEds;
031100090219         endif;
031200090219
031300110411         // -?Impostazione iniziale / pulizia dei campi chiave?
031400090220         k_TBEcod = c_Tab;
031500090220         clear  k_TBEke1;
031600090220         clear  k_TBEke2;
031700090220         clear  k_TBElin;
031800090220         clear  k_TBEsif;
031900110413
032000110413         // -?Verifica se *pgm richiamato:?
032100110413         //  ?test delle prime 7 posizioni della KPJBA per sapere se il?
032200110413         //  ?pgm è stato richiamato da un altro pgm o da menù tabelle.?
032300110413         $Called = (%subst( KPJBU : 1 : %len(V1Cksc) ) <> *blank);
032400110413         clear  W0Cksc;
032500110413         clear  W0Cgma;
032600110413         select;
032700110413           when  NOT $Called;
032800110413           when  %check( c_Digits : %subst( KPJBU : 1 :
032900110413                                            %len(V1Cksc) ) ) = *zero;
033000110413             W0Cksc = %subst( KPJBU : 1 : %len(V1Cksc) );
033100110413           other;
033200110413             W0Cgma = %subst( KPJBU : 1 : %len(V1Cgma) );
033300110413         endsl;
033400090219
033500090219       ENDSR;
033600090219
033700090219       //--------------------------------------------------------------
033800090219       //?Reperimento Dati del job (Utente/Operativi).                 ?
033900090219       //--------------------------------------------------------------
034000090219       BEGSR  sr_DatiJob;
034100090219
034200090219         in(E) §AzUte;
034300090219         if NOT %error;
034400090219           in(E) §DatiUte;
034500090219         endif;
034600110411         if %error or RSut = *blank;
034700090219           clear TIBS34ds;
034800090219           tibs34r ( tibs34ds );
034900090219           in §AzUte;
035000090219           in §DatiUte;
035100090219         endif;
035200090219
035300090219       ENDSR;
035400090219
035500090219       //--------------------------------------------------------------
035600090219       //?Gestione videata D01                                         ?
035700090219       //--------------------------------------------------------------
035800090219       BEGSR  sr_GesD01;
035900090219
036000110411         // -?Inizializzazione videata?
036100090219         if  $InzD01 = *on;
036200090219           exsr  sr_InzD01;
036300090219           $InzD01 = *off;
036400110502         else;
036500110502           NotDspKSC = *off;
036600110502           NotDspGMA = *off;
036700090219         endif;
036800110412
036900110412         // -?SE ricevuto codice cliente NON si emette la prima videata?
037000110413         if  $Called  and  (W0Cksc > *zero  or  W0Cgma <> *blank);
037100110412           exsr  sr_CtrD01;
037200110412           if  Not ErrGenerico;
037300110412             $Video  = 'D2';
037400110412             $InzD02 = *on;
037500110412             leavesr;
037600110413           else;
037700110413             clear W0Cksc;
037800110413             clear W0Cgma;
037900110412           endif;
038000110412         endif;
038100090219
038200110412         // -?Emissione Testata & Piede?
038300090220         write  TB71T01;
038400090220         write  TB71P01;
038500110412
038600110412         // -?Emissione videata?
038700090220         exfmt  TB71D01;
038800110411
038900110411         clear  ErrGenerico;
039000110411         clear  ErrMessage;
039100090220         clear  V1Dmsg;
039200090219
039300090219         SELECT;
039400090219
039500110411           // -?F3=Fine?
039600090219           WHEN  dsp_aid = c_F03;
039700090219             $Fine = *on;
039800090219
039900110411           // -?Invio?
040000090219           OTHER;
040100110408             exsr  sr_CtrD01;
040200110411             if  ErrGenerico = *on;
040300110411               leavesr;
040400110411             endif;
040500110411             $Video  = 'D2';
040600110411             $InzD02 = *on;
040700090219
040800090219         ENDSL;
040900090219
041000090219       ENDSR;
041100090219
041200090219       //--------------------------------------------------------------
041300090219       //?Inizializzazione videata D01                                 ?
041400090219       //--------------------------------------------------------------
041500090219       BEGSR  sr_InzD01;
041600090219
041700090220         clear  TB71D01;
041800110502         NotDspKSC = *off;
041900110502         NotDspGMA = *off;
042000110412
042100110412         // -?Se è stato richiamato con un codice particolarità?
042200110412         //  ?consegna o con un codice cliente, si imposta tale?
042300110412         //  ?codice a video?
042400110412         select;
042500110413           when  $Called  and  W0Cgma <> *blank;
042600110413             V1Cgma = W0Cgma;
042700110413           when  $Called  and  W0Cksc > *zero;
042800110413             V1Cksc = W0Cksc;
042900110413           other;
043000110412             V1Cksc = '?';
043100110412         endsl;
043200090219
043300090219       ENDSR;
043400090219
043500090219       //--------------------------------------------------------------
043600090219       //?Controllo dati immessi nella videata D01                     ?
043700090219       //--------------------------------------------------------------
043800090219       BEGSR  sr_CtrD01;
043900090219
044000090219         %subst(IndDspF : 28) = *off;
044100090219
044200110411         clear  V1Dksc;
044300090219         SELECT;
044400090219
044500110411           // -?Ricerca codice cliente?
044600110411           WHEN  %scan('?' : V1Cksc) > *zero;
044700090219             reset TIBS02ds;
044800090219             //T02mod = 'R';     (già così)
044900090219             //T02cod = c_Tab;   (già così)
045000090219             T02sif = KNSIF;
045100090220             TNTBE_RicercaControllo ( kpjba : tibs02ds );
045200110411             if  T02err = *blank;
045300110411               if  %len( %trimr( T02ke1 ) ) > %len( V1Cgma );
045400110411                 V1Cksc = %subst( T02ke1 : 1 : %len( V1Cksc ) );
045500110412                 clear  V1Cgma;
045600110411               else;
045700110411                 clear  V1Cksc;
045800110411                 V1Cgma = %subst( T02ke1 : 1 : %len( V1Cgma ) );
045900110411               endif;
046000090219               ErrGenerico = *on;
046100090219               leavesr;
046200090219             else;
046300090219               ErrGenerico = *on;
046400110411               ErrMessage  = *on;
046500090219               PosCurKSC   = *on;
046600090219               V1Dmsg = T02msg;
046700090219               leavesr;
046800090219             endif;
046900090219
047000110411           // -?Controllo immissione codice cliente o codice?
047100110411           //  ?particolarità consegna?
047200110411           WHEN  (V1Cksc = *blank  or  V1Cksc = *zero)  and
047300110411                 (V1Cgma = *blank  or  V1Cgma = *zero);
047400090219             ErrGenerico = *on;
047500110411             ErrMessage  = *on;
047600090219             PosCurKSC   = *on;
047700090219             V1Dmsg = $Msg(01);
047800090219             leavesr;
047900110411           WHEN  V1Cksc  > *zero  and
048000110411                 V1Cgma <> *blank;
048100110411             ErrGenerico = *on;
048200110411             ErrMessage  = *on;
048300110411             PosCurKSC   = *on;
048400110411             V1Dmsg = $Msg(02);
048500110411             leavesr;
048600090219
048700110411           // -?Controllo immissione solo caratteri numerici?
048800110411           WHEN  V1Cksc <> *blank  and
048900110411                 %check( c_Digits : V1Cksc ) > *zero;
049000090219             ErrGenerico = *on;
049100110411             ErrMessage  = *on;
049200090219             PosCurKSC   = *on;
049300110411             V1Dmsg = $Msg(03);
049400090219             leavesr;
049500090219
049600090219         ENDSL;
049700090219
049800110411         IF  V1Cksc > *zero;
049900110411
050000110411           // -?Controllo/decodifica cliente?
050100110411           clear  TIBS69ds;
050200110411           I69kcc = DUTkci;
050300110411           I69kac = %int( V1Cksc );
050400110411           I69sif = knsif;
050500110411           tibs69r(tibs69ds : ds_cnaco : ds_cnind : ds_cnclp : ds_fncls);
050600110411           if  O69err = *on;
050700110411             V1Dmsg = $Msg(04);
050800110411             ErrGenerico = *on;
050900110411             ErrMessage  = *on;
051000110411             PosCurKSC   = *on;
051100110411             leavesr;
051200110411           endif;
051300110411           V1Dksc = %subst(ACOrag : 1 : %len(V1Dksc));
051400110411
051500110411           // -?NON ammessi nuovi record per cliente annullato?
051600110411           reset  TNTBE000;
051700110412           k_TBEke1 = V1Cksc;
051800110412           k_TBEke2 = '001';
051900110412           setll  %kds(k05tntbe01 : 3)  TNTBE000;
052000110412           reade  %kds(k05tntbe01 : 2)  TNTBE000;
052100110412           $Immissione = (%eof(TNTBE01L));
052200110412           reset  $Ripristino;
052300110412           If  V1Cksc > *zero  and
052400110412               %eof(TNTBE01L)  and  ACOflg <> *blank;
052500110412             V1Dmsg = $Msg(05);
052600110412             ErrGenerico = *on;
052700110412             ErrMessage  = *on;
052800110412             PosCurKSC   = *on;
052900110412             leavesr;
053000110411           EndIf;
053100110411
053200110411         ENDIF;
053300110411
053400110408
053500110408         SELECT;
053600110408
053700110411           // -?Ricerca codice particolarità?
053800110408           WHEN  %scan('?' : V1Cgma) > *zero;
053900110408             clear  V1Cgma;
054000110408             reset  DS7Pqrs;
054100110408             clear  DS7cod;
054200110408             KPJBU = DS7Pqrs;
054300110408             TRTB70R  ( kpjba );
054400110408             DS7Pqrs = KPJBU;
054500110408             V1Cgma  = DS7cod;
054600110408             PosCurGMA   = *on;
054700110408             ErrGenerico = *on;
054800110408             leavesr;
054900110408
055000110411           // -?Controllo codice particolarità?
055100110413           WHEN  V1Cgma <> *blank;
055200110413             if  NOT $Called;
055300110412               clear  DS7Pqrs;
055400110412               DS7ric = 'S';
055500110412               DS7ges = 'C';
055600110412               //DS7ksc = %int( V1Cksc );
055700110412               DS7cod = V1Cgma;
055800110412               KPJBU = DS7Pqrs;
055900110412               TRTB70R  ( kpjba );
056000110412               DS7Pqrs = KPJBU;
056100110412               if  DS7err = *on;
056200110412                 V1Dmsg = DS7msg;
056300110412                 PosCurGMA   = *on;
056400110412                 ErrMessage  = *on;
056500110412                 ErrGenerico = *on;
056600110412                 leavesr;
056700110412               endif;
056800110412             endif;
056900110412             // -?Verifica esistenza records in tab. "PXA"?
057000110412             k_TBEke1 = V1Cgma;
057100110412             k_TBEke2 = '001';
057200110412             setll  %kds(k05tntbe01 : 3)  TNTBE000;
057300110412             reade  %kds(k05tntbe01 : 2)  TNTBE000;
057400110412             $Immissione = (%eof(TNTBE01L));
057500110412             reset  $Ripristino;
057600110408
057700110408         ENDSL;
057800110411
057900110411
058000110411         // -?Come chiave si considera il codice cliente (se inserito)?
058100110411         //  ?il codice particolarità consegne (altrimenti)?
058200110411         if  V1Cksc > *zero;
058300110411           clear  V1Cgma;
058400110411         else;
058500110411           clear  V1Cksc;
058600110411           clear  V1Dksc;
058700110411         endif;
058800090219
058900090219       ENDSR;
059000090219
059100090219       //--------------------------------------------------------------
059200090224       //?Gestione videata D02                                         ?
059300090219       //--------------------------------------------------------------
059400090224       BEGSR  sr_GesD02;
059500090219
059600110411         // -?Inizializzazione videata?
059700090224         if  $InzD02 = *on;
059800110411           exsr  sr_InzD02;
059900090224           $InzD02 = *off;
060000090219         endif;
060100090220
060200110411         // -?Emissione Testata e Piede con tasti funzionali abilitati?
060300090220         if  errGenerico = *off;
060400090220           write  TB71T01;
060500090220           write  TB71D01;
060600090220           write  Protect;
060700090220           write  TB71P02;
060800090220         endif;
060900090219
061000110411         // -?Emissione videata?
061100090224         exfmt  TB71D02;
061200110411         clear  ErrGenerico;
061300110411         clear  ErrMessage;
061400090219         clear  V1Dmsg;
061500090420         reset  $Annullamento;
061600090219
061700090219         SELECT;
061800090219
061900110411           // -?F3=Fine?
062000090219           WHEN  dsp_aid = c_F03;
062100090420             unlock TNTBE01L;
062200090219             $Fine = *on;
062300110411
062400110411           // -?F12=Ritorno?
062500110411           WHEN  dsp_aid = c_F12;
062600110411             unlock TNTBE01L;
062700110411             reset  $Video;
062800110411             clear  V1Topz;
062900110413             if  $Called;
063000110413               $Fine = *on;
063100110413             endif;
063200110414
063300110414           // -?Invio;?
063400110414           //  ?F5=Ripristino; F6=Conferma; F16=Annullamento;?
063500110414           //  ?F10=Copia?
063600110414           OTHER;
063700110414             exsr  sr_CtrD02;
063800110414             if  ErrGenerico;
063900110414               leavesr;
064000110414             endif;
064100110414             Select;
064200110414               // -?F5=Ripristino; F6=Conferma; F16=Annullamento?
064300110414               When  dsp_aid = c_F05  or
064400110414                     dsp_aid = c_F06  or
064500110414                     dsp_aid = c_F16;
064600110414                 $Annullamento = (dsp_aid = c_F16);
064700110421                 $VideoPrec = $Video;
064800110414                 $Video  = 'W1';
064900110414                 reset  $InzW01;
065000110414               // -?F10=Copia?
065100110414               When  dsp_aid = c_F10;
065200110414                 unlock TNTBE01L;
065300110414                 $Video  = 'D3';
065400110414                 reset  $InzD03;
065500110414             EndSl;
065600090219
065700090219         ENDSL;
065800090219
065900090219       ENDSR;
066000090219
066100090219       //--------------------------------------------------------------
066200090224       //?Inizializzazione videata D02                                 ?
066300090219       //--------------------------------------------------------------
066400090224       BEGSR  sr_InzD02;
066500090219
066600110411         // -?Spegnimento di tutti gli indicatori?
066700110411         IndDspF = *off;
066800110420
066900110420         // -?NON visualizzazione del campo chiave NON gestito?
067000110420         NotDspKSC = (V1Cksc = *blank);
067100110420         NotDspGMA = (V1Cgma = *blank);
067200090220
067300110411         // -?Pulizia videata?
067400090224         clear  TB71D02;
067500090219
067600110411         // -?Caricamento testi per il cliente o per la particolarità?
067700110411         //  ?consegne?
067800110411         if  V1Cksc > *zero;
067900110411           k_TBEke1 = V1Cksc;
068000110411         else;
068100110411           k_TBEke1 = V1Cgma;
068200110411         endif;
068300110411         // ·?Testi per LDV?
068400110411         k_TBEke2 = '001';
068500110408         setll  %kds(k05tntbe01 : 3)  TNTBE000;
068600110411         For  xx = 1  To  c_MaxRec;
068700090224           if  Not $Immissione   and   Not %eof(TNTBE01L);
068800090224             reade  %kds(k05tntbe01 : 2)  TNTBE000;
068900090224           endif;
069000110412           //if  $Immissione   OR  (NOT %eof(TNTBE01L)  and
069100110412           //    %trimr(TBEke2) >= '001 '  and  %trimr(TBEke2) <= '999 ');
069200110408             exsr  sr_RieD02;
069300110412           //endif;
069400110411         EndFor;
069500110411         // ·?Testi per PDA?
069600110411         k_TBEke2 = 'PDA';
069700110411         chain  %kds(k05tntbe01 : 3)  TNTBE000;
069800110413         if  %found(TNTBE01L)  and
069900110413             ( (TBEatb  = *blank  and  Not $Ripristino)  or
070000110413               (TBEatb <> *blank  and  $Ripristino) );
070100110411           D02txtPDA = %subst( TBEuni : 1 : %len( D02txtPDA ) );
070200110411         endif;
070300090224
070400110411         // -?Impostazione tipo manutenzione?
070500090224         select;
070600110412           when  $Immissione  and  NOT %found(TNTBE01L);
070700090224             V1Topz = 'IMMISSIONE';
070800110412           when  $Ripristino   or
070900110412                 (%found(TNTBE01L)  and  TBEatb <> *blank);
071000090224             V1Topz = 'RIPRISTINO';
071100090224           other;
071200090224             V1Topz = ' MODIFICA ';
071300090224         endsl;
071400090220
071500110411         // -?(Ri)Abilitazione tasti funzionali?
071600110411         exsr  sr_AbilFxxD02;
071700090220
071800090220       ENDSR;
071900090220
072000090220       //--------------------------------------------------------------
072100090224       //?Caricamento singola riga di testo nella videata D02          ?
072200090220       //--------------------------------------------------------------
072300090224       BEGSR  sr_RieD02;
072400110412
072500110411         H02prg(xx) = xx;
072600090224         D02prg(xx) = xx;
072700110412
072800110412         if  $Immissione;
072900110412           $Ripristino = *off;
073000110412           leavesr;
073100110412         endif;
073200110412
073300110412         // -?Progressivo oltre il massimo previsto?
073400110419         if  Not %eof(TNTBE01L)         and
073500110419            (%int( %trimr(TBEke2) ) < 1  or
073600110419             %int( %trimr(TBEke2) ) > c_MaxRec);
073700110412           leavesr;
073800110412         endif;
073900110412
074000110412         // -?Lasciate righe vuote?
074100110419         DoW  Not %eof(TNTBE01L)  and  xx < %int( %trimr(TBEke2) );
074200110412           xx += 1;
074300110412           H02prg(xx) = xx;
074400110412           D02prg(xx) = xx;
074500110412         EndDo;
074600110413
074700110413         // -?Verifica se note TUTTE annullate (ripristinabili)?
074800110419         if  $Ripristino  and  Not %eof(TNTBE01L);
074900110419           $Ripristino = (TBEatb <> *blank);
075000110413         endif;
075100090224
075200110412         // -?Impostazione riga di testo a video?
075300110413         if  Not %eof(TNTBE01L)  and
075400110413             ( (Not $Ripristino  and  TBEatb  = *blank)  or
075500110413               ($Ripristino      and  TBEatb <> *blank) );
075600090224           D02txt(xx) = %subst( TBEuni : 1 : %len( D02txt(xx) ) );
075700090224         endif;
075800090220
075900090220       ENDSR;
076000090219
076100090219       //--------------------------------------------------------------
076200110411       //?Abilitazione tasti funzionali in P02 (per D02).              ?
076300090219       //--------------------------------------------------------------
076400110411       BEGSR  sr_AbilFxxD02;
076500110413
076600110413         // ->?F3 = Fine?
076700110413         F3Attivo = (NOT $Called);
076800090219
076900110411         // -?F5=Ripristino?
077000090220         F5Attivo  = (NOT $Immissione  and  $Ripristino);
077100090220
077200110411         // -?F6=Conferma?
077300090219         F6Attivo  = (NOT F5Attivo);
077400090420
077500110411         // -?F10=Copia?
077600090420         F10Attivo = (NOT $Immissione  and  NOT $Ripristino);
077700090220
077800110411         // -?F16=Annullamento?
077900110412         F16Attivo = ( (NOT $Immissione   and  NOT $Ripristino)  OR
078000110412                       (%found(TNTBE01L)  and  TBEatb = *blank) );
078100090219
078200090219       ENDSR;
078300110413
078400110413       //--------------------------------------------------------------
078500110413       //?Controllo dati immessi nella videata D02                     ?
078600110413       //--------------------------------------------------------------
078700110413       BEGSR  sr_CtrD02;
078800110413
078900110420         %subst(IndDspF : 50) = *off;
079000110413
079100110413         // -?Mancano le note e NON premuto F16=Annullamento?
079200110414         if  dsp_aid <> c_F16    and
079300110414             dsD02txt  = *blank  and  D02txtPDA = *blank;
079400110413           ErrGenerico = *on;
079500110413           ErrMessage  = *on;
079600110414           PosCurTXT   = *on;
079700110413           V1Dmsg = $Msg(06);
079800110413           leavesr;
079900110413         endif;
080000110413
080100110413       ENDSR;
080200090420
080300090420       //--------------------------------------------------------------
080400090420       //?Gestione videata D03                                         ?
080500090420       //--------------------------------------------------------------
080600090420       BEGSR  sr_GesD03;
080700090420
080800110411         // -?Inizializzazione videata?
080900090420         if  $InzD03 = *on;
081000110411           exsr  sr_InzD03;
081100090420           $InzD03 = *off;
081200090420         endif;
081300090420
081400110411         // -?Emissione Testata e Piede con tasti funzionali abilitati?
081500090420         if  errGenerico = *off;
081600090420           write  TB71T01;
081700090420           write  TB71D01;
081800090420           write  Protect;
081900090420           write  TB71P03;
082000090420         endif;
082100090420
082200110411         // -?Emissione videata?
082300090420         exfmt  TB71D03;
082400110411         clear  ErrGenerico;
082500110411         clear  ErrMessage;
082600090420         clear  V1Dmsg;
082700090420
082800090420         SELECT;
082900090420
083000110411           // -?F3=Fine?
083100090420           WHEN  dsp_aid = c_F03;
083200090420             $Fine = *on;
083300090420
083400110411           // -?F12=Ritorno?
083500090420           WHEN  dsp_aid = c_F12;
083600110413             unlock TNTBE01L;
083700090420             reset  $Video;
083800090420             clear  V1Topz;
083900110413             if  $Called;
084000110413               $Fine = *on;
084100110413             endif;
084200090420
084300110411           // -?Invio; F5=Ripristino; F6=Conferma; F16=Annullamento?
084400090420           OTHER;
084500090420             exsr  sr_CtrD03;
084600090420             if  ErrGenerico = *on;
084700090420               leavesr;
084800090420             endif;
084900110411             // -?F5=Ripristino; F6=Conferma; F16=Annullamento?
085000090420             if  dsp_aid = c_F05  or
085100090420                 dsp_aid = c_F06  or
085200090420                 dsp_aid = c_F16;
085300110421               $VideoPrec = $Video;
085400090420               $Video  = 'W1';
085500090420               reset  $InzW01;
085600090420             endif;
085700090420
085800090420         ENDSL;
085900090420
086000090420       ENDSR;
086100090420
086200090420       //--------------------------------------------------------------
086300090420       //?Inizializzazione videata D03                                 ?
086400090420       //--------------------------------------------------------------
086500090420       BEGSR  sr_InzD03;
086600090420
086700110411         // -?Spegnimento di tutti gli indicatori?
086800090420         IndDspF = *off;
086900090420
087000110411         // -?Pulizia PARZIALE videata?
087100090420         clear  V3Cksc;
087200090420         clear  V3Dksc;
087300110408         clear  V3Cgma;
087400090420
087500110411         // -?Caricamento testi per il cliente?
087600110411         // ·?Testi per LDV?
087700110411         //k_TBEke2 = '001';
087800110411         //setll  %kds(k05tntbe01 : 3)  TNTBE000;
087900110411         //For  xx = 1  To  c_MaxRec;
088000110411         //  if  Not %eof(TNTBE01L);
088100110411         //    reade  %kds(k05tntbe01 : 2)  TNTBE000;
088200110411         //  endif;
088300110411         //  exsr  sr_RieD03;
088400110411         //EndFor;
088500110411         // ·?Testi per PDA?
088600110411         //k_TBEke2 = 'PDA';
088700110411         //chain  %kds(k05tntbe01 : 3)  TNTBE000;
088800110411         //if  %found(TNTBE01L)  and  TBEatb = *blank;
088900110411         //  D02txtPDA = %subst( TBEuni : 1 : %len( D02txtPDA ) );
089000110411         //endif;
089100090420
089200110411         // -?Impostazione tipo manutenzione?
089300090420         V1Topz = '  COPIA   ';
089400110411
089500110411         // -?Disabilitazione ripristino annullate (in fase di copia)?
089600110411         $Ripristino = *off;
089700090420
089800090420       ENDSR;
089900090420
090000090420       //--------------------------------------------------------------
090100090420       //?Caricamento singola riga di testo nella videata D03          ?
090200090420       //--------------------------------------------------------------
090300110412       //BEGSR  sr_RieD03;
090400110412       //
090500110412       //  H02prg(xx) = xx;
090600110412       //  D02prg(xx) = xx;
090700110412       //
090800110412       //  // -?Progressivo oltre il massimo previsto?
090900110412       //  if  %int( %trimr(TBEke2) ) < 1  or
091000110412       //      %int( %trimr(TBEke2) ) > c_MaxRec;
091100110412       //    leavesr;
091200110412       //  endif;
091300110412       //
091400110412       //  // -?Lasciate righe vuote?
091500110412       //  DoW  xx < %int( %trimr(TBEke2) );
091600110412       //    xx += 1;
091700110412       //    H02prg(xx) = xx;
091800110412       //    D02prg(xx) = xx;
091900110412       //  EndDo;
092000110412       //
092100110412       //  // -?Impostazione riga di testo a video?
092200110412       //  if  Not %eof(TNTBE01L);
092300110412       //    D02txt(xx) = %subst( TBEuni : 1 : %len( D02txt(xx) ) );
092400110412       //  endif;
092500110412       //
092600110412       //ENDSR;
092700090420
092800090420       //--------------------------------------------------------------
092900090420       //?Controllo dati immessi nella videata D03                     ?
093000090420       //--------------------------------------------------------------
093100090420       BEGSR  sr_CtrD03;
093200110420
093300110420         %subst(IndDspF : 50) = *off;
093400090420
093500110411         // -?Controllo codice cliente / codice particolarità?
093600090420         clear  V3Dksc;
093700090420         V1Cksc = %editc( V3Cksc : 'X' );
093800110408         V1Cgma = V3Cgma;
093900090420         exsr  sr_CtrD01;
094000090420         V3Dksc = V1Dksc;
094100110421         if  ErrGenerico = *on;
094200110421           leavesr;
094300110421         endif;
094400090420
094500110411         // -?Verifica esistenza record in tab. "PXA" per il cliente o?
094600110411         //  ?la particolarità consegna indicate a video?
094700090420         reset  TNTBE000;
094800090420         k_TBEcod = c_Tab;
094900110421         if  V1Cksc > *zero;
095000110421           k_TBEke1 = V1Cksc;
095100110411         else;
095200110421           k_TBEke1 = V1Cgma;
095300110411         endif;
095400110408         k_TBEke2 = '001 ';
095500090420         clear  k_TBElin;
095600090420         clear  k_TBEsif;
095700110408         setll  %kds(k05tntbe01 : 3)  TNTBE000;
095800110408         reade  %kds(k05tntbe01 : 2)  TNTBE000;
095900110408         $Immissione = (%eof(TNTBE01L));
096000090420         reset  $Ripristino;
096100110408         if  Not %eof(TNTBE01L);
096200090420           ErrGenerico = *on;
096300110411           ErrMessage  = *on;
096400110421           if  V3Cksc > *zero;
096500110421             PosCurKSC   = *on;
096600110421             V1Dmsg = %trimr( $Msg(07) ) + ' il cliente ' + V1Cksc;
096700110421           else;
096800110421             PosCurGMA   = *on;
096900110421             V1Dmsg = %trimr( $Msg(07) ) + ' la particolarità +
097000110421                                           consegna ' + V1Cgma;
097100110421           endif;
097200090420           leavesr;
097300090420         endif;
097400110414
097500110414         // -?Verifica inserimento corretto di note?
097600110414         exsr  sr_CtrD02;
097700110414         if  ErrGenerico;
097800110414           leavesr;
097900110414         endif;
098000090420
098100090420       ENDSR;
098200090220
098300090220       //--------------------------------------------------------------
098400090220       //?Gestione trasmissione aggiornamento                          ?
098500090220       //--------------------------------------------------------------
098600090220       BEGSR  sr_GesW01;
098700090220
098800110411         // -?Inizializzazione videata?
098900090220         if $InzW01 = *on;
099000090224           exsr  sr_InzW01;
099100090220           $InzW01 = *off;
099200090220         endif;
099300090220
099400110411         // -?Emissione window?
099500110421         IF  TBXftt = 'S';
099600090220           exfmt  TB71W01;
099700110411           clear  ErrGenerico;
099800110411           clear  ErrMessage;
099900090220           clear  V1Dmsg;
100000110421           Select;
100100110421             // -?F12=Ritorno (gestito solo se emesso W01)?
100200110421             When  dsp_aid = c_F12;
100300110421               $Video = $VideoPrec;
100400110421               leavesr;
100500110421             // -?Invio (gestito solo se emesso W01)?
100600110421             When  dsp_aid <> c_F06;
100700110421               leavesr;
100800110421           EndSl;
100900110421         ENDIF;
101000110421
101100110421         // -?Aggiornamento TNTBE00F?
101200110421         exsr  sr_UpdTNTBE;
101300110421         if  NOT  ErrGenerico;
101400110421           exsr  sr_UpdTNTBE_PDA;
101500110421         endif;
101600110421         select;
101700110421           // ·?Rilevato errore in fase di aggiornamento?
101800110421           when  ErrGenerico;
101900110421             leavesr;
102000110421           // ·?Uscita dal programma SE richiamato?
102100110421           when  $Called  and  (W0Cksc > *zero  or  W0Cgma <> *blank);
102200110421             $Fine = *on;
102300110421           // ·?Videata iniziale altrimenti?
102400110421           other;
102500110421             clear  V1Topz;
102600110421             $Video  = 'D1';
102700110421             $InzD01 = *on;
102800110421         endsl;
102900090220
103000090220       ENDSR;
103100090220
103200090220       //--------------------------------------------------------------
103300090220       //?Inizializzazione videata W01                                 ?
103400090220       //--------------------------------------------------------------
103500090220       BEGSR  sr_InzW01;
103600090220
103700090220         clear TB71W01;
103800090220
103900090220         if  $Immissione;
104000090220
104100110411           // -?Se immissione?
104200090220           W1ftt  = TBXftt;
104300090220
104400090220         else;
104500090220
104600110411           // -?Se NON immissione:?
104700110411           //  ?visualizza i dati relativi all'ultima trasmissione?
104800090220           W1ftt  = TBEftt;
104900090220           W1flt  = TBEflt;
105000090220           W1ftr  = TBEftr;
105100090220           if TBEdtr <> *zero;
105200090220             wDate_EUR = %date(TBEdtr : *iso);
105300090220             W1dtr     = %dec(wDate_EUR);
105400090220           endif;
105500090220
105600090220         endif;
105700090220
105800090220       ENDSR;
105900090219
106000090219       //--------------------------------------------------------------
106100110408       //?Aggiornamento records TNTBE00F (tab. PXA) x LdV              ?
106200090219       //--------------------------------------------------------------
106300110411       BEGSR  sr_UpdTNTBE;
106400090219
106500110411         // -?Ciclo di lettura del subfile?
106600090224         FOR  xx = 1  TO  c_MaxRec;
106700090220
106800110411           // -?Aggancio record di TNTBE00F?
106900110411           k_TBEcod = c_Tab;
107000110411           if  V1Cksc > *zero;
107100110411             k_TBEke1 = V1Cksc;
107200110411           else;
107300110411             k_TBEke1 = V1Cgma;
107400110411           endif;
107500110411           k_TBEke2 = %editc(H02prg(xx) : 'X');
107600090220           clear  k_TBElin;
107700090220           clear  k_TBEsif;
107800110411           //k_TBElin = TBXlin;
107900110411           //k_TBEsif = TBXsif;
108000090220
108100110412           chain  %kds(k05tntbe01) TNTBE000;
108200090220
108300110411           // -?Aggiornamento singolo record?
108400110412           Select;
108500090223
108600110411             // -?Skip dei nuovi record vuoti?
108700110412             When  not  %found(TNTBE01L)   and   D02txt(xx) = *blank;
108800090220
108900110411             // -?Cancellazione record svuotato nel sfl?
109000110412             When  %found(TNTBE01L)   and   D02txt(xx) = *blank;
109100110411               ////_______________
109200110411               //DELETE  TNTBE000;
109300110411               ////¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
109400110412               If  TBEatb = *blank;
109500110412                 TBEatb = 'A';
109600110412                 TBEftt = W1ftt;
109700110412                 TBEFlt = W1flt;
109800110412                 clear  TBEftr;
109900110412                 //clear TBEdtr;
110000110412                 //_____________
110100110412                 UPDATE TNTBE000;
110200110412                 //¯¯¯¯¯¯¯¯¯¯¯¯¯
110300110412               EndIf;
110400090220
110500110412             // -?Annullamento o Immissione o Modifica?
110600110412             Other;
110700090220               if  NOT %found(TNTBE01L);
110800090220                 clear TNTBE000;
110900090220                 TBEcod  = k_TBEcod;
111000090220                 TBEke1  = k_TBEke1;
111100090224                 TBEke2  = k_TBEke2;
111200090220                 TBElin  = k_TBElin;
111300090220                 TBEsif  = k_TBEsif;
111400090220               endif;
111500090220               TBEapl = TBXapl;
111600090420               if  $Annullamento;
111700090420                 TBEatb = 'A';
111800090420               else;
111900090420                 clear  TBEatb;
112000090420               endif;
112100090220               TBEftt = W1ftt;
112200090220               TBEFlt = W1flt;
112300090224               clear  TBEftr;
112400090220               //clear TBEdtr;
112500110412               // ·?Aggiornamento record?
112600090224               TBEuni = D02txt(xx);
112700090420               if  %found(TNTBE01L);
112800090420                 //_____________
112900090420                 UPDATE TNTBE000;
113000090420                 //¯¯¯¯¯¯¯¯¯¯¯¯¯
113100090420               else;
113200090420                 //_____________
113300090420                 WRITE  TNTBE000;
113400090420                 //¯¯¯¯¯¯¯¯¯¯¯¯¯
113500090420               endif;
113600090220
113700110412           EndSl;
113800090220
113900090224         ENDFOR;
114000090219
114100090219       ENDSR;
114200110408
114300110408       //--------------------------------------------------------------
114400110408       //?Aggiornamento records TNTBE00F (tab. PXA) x PDA              ?
114500110408       //--------------------------------------------------------------
114600110411       BEGSR  sr_UpdTNTBE_PDA;
114700110408
114800110411         // -?Aggancio record di TNTBE00F?
114900110411         k_TBEcod = c_Tab;
115000110411         if  V1Cksc > *zero;
115100110411           k_TBEke1 = V1Cksc;
115200110411         else;
115300110411           k_TBEke1 = V1Cgma;
115400110411         endif;
115500110411         k_TBEke2 = 'PDA ';
115600110408         clear  k_TBElin;
115700110408         clear  k_TBEsif;
115800110408         //k_TBElin  = TBXlin;
115900110408         //k_TBEsif  = TBXsif;
116000110408
116100110412         chain  %kds(k05tntbe01) TNTBE000;
116200110408
116300110411         // -?Aggiornamento singolo record?
116400110412         Select;
116500110408
116600110411           // -?Skip del nuovo record vuoto?
116700110412           When  not  %found(TNTBE01L)   and   D02txtPDA = *blank;
116800110408
116900110411           // -?Cancellazione record svuotato nel sfl?
117000110412           When  %found(TNTBE01L)   and   D02txtPDA = *blank;
117100110411             ////_______________
117200110411             //DELETE  TNTBE000;
117300110411             ////¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
117400110412             If  TBEatb = *blank;
117500110412               TBEatb = 'A';
117600110412               TBEftt = W1ftt;
117700110412               TBEFlt = W1flt;
117800110412               clear  TBEftr;
117900110412               //clear TBEdtr;
118000110412               //_____________
118100110412               UPDATE TNTBE000;
118200110412               //¯¯¯¯¯¯¯¯¯¯¯¯¯
118300110412             EndIf;
118400110408
118500110412           // -?Annullamento o Immissione o Modifica?
118600110412           Other;
118700110408             if  NOT %found(TNTBE01L);
118800110408               clear TNTBE000;
118900110408               TBEcod  = k_TBEcod;
119000110408               TBEke1  = k_TBEke1;
119100110408               TBEke2  = k_TBEke2;
119200110408               TBElin  = k_TBElin;
119300110408               TBEsif  = k_TBEsif;
119400110408             endif;
119500110408             TBEapl = TBXapl;
119600110408             if  $Annullamento;
119700110408               TBEatb = 'A';
119800110408             else;
119900110408               clear  TBEatb;
120000110408             endif;
120100110408             TBEftt = W1ftt;
120200110408             TBEFlt = W1flt;
120300110408             clear  TBEftr;
120400110408             //clear TBEdtr;
120500110412             // ·?Aggiornamento record?
120600110412             TBEuni = D02txtPDA;
120700110408             if  %found(TNTBE01L);
120800110408               //_____________
120900110408               UPDATE TNTBE000;
121000110408               //¯¯¯¯¯¯¯¯¯¯¯¯¯
121100110408             else;
121200110408               //_____________
121300110408               WRITE  TNTBE000;
121400110408               //¯¯¯¯¯¯¯¯¯¯¯¯¯
121500110408             endif;
121600110408
121700110408         ENDSL;
121800110408
121900110408       ENDSR;
122000110412
122100110412       //--------------------------------------------------------------
122200110412       //?Annullamento records TNTBE00F (tab. PXA) x LdV               ?
122300110412       //--------------------------------------------------------------
122400110412       BEGSR  sr_AnnullTNTBE;
122500110412
122600110412         k_TBEke2 = *loval;
122700110412         setll  %kds(k05tntbe01 : 3)  TNTBE000;
122800110412         reade  %kds(k05tntbe01 : 2)  TNTBE000;
122900110412
123000110412         DoW  Not  %eof(TNTBE01L);
123100110412           TBEatb = 'A';
123200110412           TBEftt = W1ftt;
123300110412           TBEFlt = W1flt;
123400110412           clear  TBEftr;
123500110412           //clear TBEdtr;
123600110412           //_____________
123700110412           UPDATE TNTBE000;
123800110412           //¯¯¯¯¯¯¯¯¯¯¯¯¯
123900110412           reade  %kds(k05tntbe01 : 2)  TNTBE000;
124000110412         EndDo;
124100110412
124200110412       ENDSR;
124300110413
124400110413       //--------------------------------------------------------------
124500110413       //?Ripristino records TNTBE00F (tab. PXA) x LdV                 ?
124600110413       //--------------------------------------------------------------
124700110413       BEGSR  sr_RiprisTNTBE;
124800110413
124900110413         k_TBEke2 = *loval;
125000110413         setll  %kds(k05tntbe01 : 3)  TNTBE000;
125100110413         reade  %kds(k05tntbe01 : 2)  TNTBE000;
125200110413
125300110413         DoW  Not  %eof(TNTBE01L);
125400110413           clear  TBEatb;
125500110413           TBEftt = W1ftt;
125600110413           TBEFlt = W1flt;
125700110413           clear  TBEftr;
125800110413           //clear TBEdtr;
125900110413           //_____________
126000110413           UPDATE TNTBE000;
126100110413           //¯¯¯¯¯¯¯¯¯¯¯¯¯
126200110413           reade  %kds(k05tntbe01 : 2)  TNTBE000;
126300110413         EndDo;
126400110413
126500110413       ENDSR;
126600090219
126700090219       //--------------------------------------------------------------
126800090219       //?Operazioni finali.                                           ?
126900090219       //--------------------------------------------------------------
127000090219       BEGSR  sr_RoutEnd;
127100090219
127200110411         // -?Chiusura funzioni precedentemente aperte?
127300090219         clear  TIBS69ds;
127400090219         I69tla = 'C';
127500090219         tibs69r (TIBS69ds : ds_CNACO :
127600090219                             ds_CNIND :
127700090219                             ds_CNCLP :
127800090219                             ds_FNCLS);
127900090219
128000110412         // -?Uscita dal *pgm?
128100090219         return;
128200090219
128300090219       ENDSR;
128400090219
128500090219      /end-free
128600090219
128700090219       //--------------------------------------------------------------
128800090219       //?Definizione schiere a tempo di compilazione                  ?
128900090219       //--------------------------------------------------------------
129000090219
129100110411** - $Msg:?Messaggi di Errore?-----------------------------------------------*
129200110411Immettere il codice cliente oppure il codice particolarità consegna             1
129300110411Immettere solo il codice cliente o solo il codice particolarità consegna        2
129400110411Immettere SOLO caratteri numerici                                               3
129500110411Codice cliente errato                                                           4
129600110411Cliente annullato                                                               5
129700110413Inserire almeno una nota                                                        6
129800110421Promemoria già inserito per                                                     7
