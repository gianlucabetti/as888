000100090521      *PARMS dfactgrp(*no) actgrp(*caller) dbgview(*source)
000200090521       //--------------------------------------------------------------
000300100503       //?Gestione tabella "TTA" (Tipo attività marketing)          ?
000400090521       //--------------------------------------------------------------
000500090521
000600090521     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700090521     h dftactgrp(*no) actgrp(*caller)
000800090521
000900090521       //--------------------------------------------------------------
001000090521       //?Dichiarazione file.                                          ?
001100090521       //--------------------------------------------------------------
001200090521
001300090521       // - Tabelle
001400090525     fTABEL00F  if   e           k disk
001500090521     fTNTBE01L  Uf A e           k disk
001600090521
001700090521       // - Video Gestione
001800100504     fTNTB81D   cf   e             workstn
001900090521     f                                     indds(IndDspF)
002000090521     f                                     infds(InfDspF)
002100090521
002200090521       //--------------------------------------------------------------
002300090521       //?Definizione costanti.                                        ?
002400090521       //--------------------------------------------------------------
002500090521
002600090521       // - Codice tabella in gestione
002700100504     d c_Tab           c                   const('TTA')
002800090525
002900090525       // - Codice utente in vecchie tabelle (TABEL00F)
003000090525     d c_Kut           c                   const(1)
003100090521
003200090521       // - Tasti funzionali a video
003300090521     d c_F01           c                   const(x'31')
003400090521     d c_F02           c                   const(x'32')
003500090521     d c_F03           c                   const(x'33')
003600090521     d c_F05           c                   const(x'35')
003700090521     d c_F06           c                   const(x'36')
003800090521     d c_F07           c                   const(x'37')
003900090521     d c_F08           c                   const(x'38')
004000090521     d c_F09           c                   const(x'39')
004100090521     d c_F10           c                   const(x'3A')
004200090521     d c_F12           c                   const(x'3C')
004300090521     d c_F13           c                   const(x'B1')
004400090521     d c_F14           c                   const(x'B2')
004500090521     d c_F15           c                   const(x'B3')
004600090521     d c_F16           c                   const(x'B4')
004700090521     d c_F17           c                   const(x'B5')
004800090521     d c_F18           c                   const(x'B6')
004900090521     d c_F19           c                   const(x'B7')
005000090521     d c_F20           c                   const(x'B8')
005100090521     d c_F21           c                   const(x'B9')
005200090521     d c_F22           c                   const(x'BA')
005300090521     d c_F23           c                   const(x'BB')
005400090521     d c_F24           c                   const(x'BC')
005500090521     d c_Enter         c                   const(x'F1')
005600090521     d c_RollDown      c                   const(x'F4')
005700090521     d c_RollUp        c                   const(x'F5')
005800100127
005900100127     d $_opz           c                   const(' 25G')
006000090521
006100090521       //--------------------------------------------------------------
006200090521       //?Definizione schiere.                                         ?
006300090521       //--------------------------------------------------------------
006400090521
006500090521       // - Messaggi di errore
006600100126     d $Msg            s             78    dim(20)  ctdata  perrcd(01)
006700090521
006800090521       //--------------------------------------------------------------
006900090521       //?Definizione aree dati.                                       ?
007000090521       //--------------------------------------------------------------
007100090521
007200090521       // - Dati utente
007300090521     d §AzUte        e ds                  extname(AZUTE00F)
007400090521     d                                     dtaara
007500090521     d §DatiUte      e ds                  extname(dDatiUte)
007600090521     d                                     dtaara
007700090521
007800090521       //--------------------------------------------------------------
007900090521       //?Definizione strutture dati.                                  ?
008000090521       //--------------------------------------------------------------
008100090521
008200090521       // - Status ds
008300090521     d Status         sds
008400090521     d  SDSpgm           *proc
008500090521
008600090521       // - InfDS
008700090521     d InfDspF         ds
008800090521     d   dsp_aid             369    369a
008900090521
009000090521       // - Indicatori su DspF
009100090521     d IndDspF         ds                  inz
009200090521         // - Abilitazione tasti funzionali
009300090521     d   F5Attivo                      n   overlay(IndDspF : 05)
009400090521     d   F6Attivo                      n   overlay(IndDspF : 06)
009500090521     d   F16Attivo                     n   overlay(IndDspF : 16)
009600090521         // - Emissione messaggio di errore
009700090521     d   ErrMessage                    n   overlay(IndDspF : 28)
009800090521         // - Posizionamento cursore & segnalazione errore
009900100504     d   PosCurTTA                     n   overlay(IndDspF : 50)
010000100504     d   PosCurDES                     n   overlay(IndDspF : 51)
010100100504     d   PosCurTIT                     n   overlay(IndDspF : 52)
010200100504     d   PosCurDESB                    n   overlay(IndDspF : 53)
010300090521         //   - Riemissione videata
010400090521     d   ErrGenerico                   n   overlay(IndDspF : 99)
010500090521
010600090521       // - Parametri ricevuti
010700090521     d KPJBA         e ds
010800090521
010900090521       // - Parametri per Reperimento dati utente
011000090521     d TIBS34ds      e ds
011100090521
011200090521       // - Parametri per Ricerca/controllo tabelle
011300090521     d TIBS02ds      e ds                  inz
011400090521     d   T02mod      e                     inz('R')
011500090521     d   T02cod      e                     inz(c_Tab)
011600090525
011700100504       // - Tab. TTA = Tipo attività marketing
011800100504     d dTTA          e ds                  inz
011900100504
012000100504       // - Parametri per Interrogazione/Selezione tab. "TTA"
012100100504     d TNTB81ds      e ds                  inz
012200090521
012300090521       // - Dati record principale della tabella
012400090521     d TNTBEds       e ds                  inz  extname(TNTBE00F)
012500090521     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
012600090521     d                                          prefix(TBX:3)
012700090521
012800090521       //--------------------------------------------------------------
012900090521       //?Definizione variabili globali.                               ?
013000090521       //--------------------------------------------------------------
013100090521
013200090521       // - Flags booleani
013300090521     d $Fine           s               n   inz
013400090521     d $InzD01         s               n   inz(*on)
013500090521     d $InzD02         s               n   inz(*on)
013600090521     d $InzW01         s               n   inz(*on)
013700090521     d $Annullamento   s               n   inz
013800090521     d $Immissione     s               n   inz
013900090521     d $Ripristino     s               n   inz
014000090521
014100090521       // - Indici di schiera
014200090521     d xx              s              3  0 inz
014300090521
014400090521       // - Variabili per la gestione del video
014500090521     d $Video          s              2    inz('D1')
014600090525
014700090525       // - Flag di errore
014800090525     d $Err            s              1    inz(*off)
014900090521
015000090521       // - Campi di comodo
015100090521     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
015200090521
015300090525       // - Parametri per pgm interrogazione tabelle
015400090525      /copy gaitrasrc/srcProtoPI,X§TABER
015500090521
015600090521       //--------------------------------------------------------------
015700090521       //?Definizione prototipi procedure.                             ?
015800090521       //--------------------------------------------------------------
015900090521
016000090521       // - Reperimento dati utente
016100090521      /copy gaitrasrc/srcProtoPR,TIBS34R
016200090521
016300090525       // - Ricerca vecchie tabelle
016400090525      /copy gaitrasrc/srcProtoPR,X§TABER
016500090525
016600090525       // - Ricerca e controllo nuove tabelle
016700090521      /copy gaitrasrc/srcProtoPR,TIBS02R
016800090521
016900090525       // - Selezione singola causale attività
017000100504     d tntb81r1        pr                  extpgm('TNTB81R1')
017100090525     d   kpjba                             likeds(KPJBA)
017200100504     d   tntb81ds                          likeds(TNTB81ds)
017300090521
017400090521       //--------------------------------------------------------------
017500090521       //?Definizione key-list.                                        ?
017600090521       //--------------------------------------------------------------
017700090521
017800090525       // - File TABEL00F
017900090525     d k03tabel00    e ds                  extname(TABEL00F : *key)
018000090525     d                                     prefix(k_)   inz
018100090525
018200090521       // - File TNTBE01L
018300090521     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
018400090521     d                                     prefix(k_)   inz
018500090521
018600090521       //--------------------------------------------------------------
018700090521       //?Riepilogo indicatori utilizzati.                             ?
018800090521       //--------------------------------------------------------------
018900090521       //--------------------------------------------------------------
019000090521
019100090521       //--------------------------------------------------------------
019200090521       //?M A I N - L I N E                                            ?
019300090521       //--------------------------------------------------------------
019400090521
019500090521     c     *Entry        plist
019600090521     c                   parm                    KPJBA
019700090521
019800090521      /free
019900090521
020000090521       //?Operazioni iniziali?
020100090521       exsr sr_RoutInz;
020200090521
020300090521       //?Gestione video?
020400090521       DOW  $Fine = *off;
020500090521         select;
020600090521           // - Manutenzione singola tabella
020700090521           when $Video = 'D2';
020800090521             exsr sr_GesD02;
020900090521           // - Filtro di lancio
021000090521           when $Video = 'D1';
021100090521             exsr sr_GesD01;
021200100504           // - Dati da copiare in tab. "TTA"
021300090526           //when $Video = 'D4';
021400090526           //  exsr sr_GesD04;
021500090521           // - Richiesta dati per trasmissione record
021600090521           when $Video = 'W1';
021700090521             exsr sr_GesW01;
021800090521           other;
021900090521             $Fine = *on;
022000090521         endsl;
022100090521       ENDDO;
022200090521
022300090521       //?Operazioni finali?
022400090521       exsr sr_RoutEnd;
022500090521
022600090521       //--------------------------------------------------------------
022700090521       //?Operazioni iniziali.                                         ?
022800090521       //--------------------------------------------------------------
022900090521       BEGSR  sr_RoutInz;
023000090521
023100090521         *inLR = *on;
023200090521
023300090521         //?Reperimento dati job?
023400090521         exsr  sr_DatiJob;
023500090521
023600090521         //?Impostazione nome programma a video?
023700090521         V1Tpgm = SDSpgm;
023800090521
023900090521         //?Aggancio dati generali della tabella in esame?
024000090521         clear  k_TBEcod;
024100090521         k_TBEke1 = *zero;
024200090521         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
024300090521                = c_Tab;
024400090521         clear  k_TBEke2;
024500090521         clear  k_TBElin;
024600090521         k_TBEsif = KNSIF;
024700090521         chain(n)  %kds(k05tntbe01)  TNTBE000;
024800090521         if  not  %found(TNTBE01L);
024900090521           clear  k_TBEsif;
025000090521           chain(n)  %kds(k05tntbe01)  TNTBE000;
025100090521         endif;
025200090521         if  %found(TNTBE01L);
025300090521           xTNTBEds = TNTBEds;
025400090521         else;
025500090521           clear  xTNTBEds;
025600090521         endif;
025700090521
025800090521         //?Impostazione iniziale / pulizia dei campi chiave?
025900090521         k_TBEcod = c_Tab;
026000090521         clear  k_TBEke1;
026100090521         clear  k_TBEke2;
026200090521         clear  k_TBElin;
026300090521         clear  k_TBEsif;
026400090521
026500090521         //?1ª inizializzazione della videata D01?
026600090521         exsr  sr_InzD01;
026700100504         V1Ctta  = '?';
026800090521         $InzD01 = *off;
026900090521
027000090521       ENDSR;
027100090521
027200090521       //--------------------------------------------------------------
027300090521       //?Reperimento Dati del job (Utente/Operativi).                 ?
027400090521       //--------------------------------------------------------------
027500090521       BEGSR  sr_DatiJob;
027600090521
027700090521         in(E) §AzUte;
027800090521         if NOT %error;
027900090521           in(E) §DatiUte;
028000090521         endif;
028100090521         if %error or RSut = *blanks;
028200090521           clear TIBS34ds;
028300090521           tibs34r ( tibs34ds );
028400090521           in §AzUte;
028500090521           in §DatiUte;
028600090521         endif;
028700090521
028800090521       ENDSR;
028900090521
029000090521       //--------------------------------------------------------------
029100090521       //?Gestione videata D01                                         ?
029200090521       //--------------------------------------------------------------
029300090521       BEGSR  sr_GesD01;
029400090521
029500090521         //?Inizializzazione videata?
029600090521         if  $InzD01 = *on;
029700090521           exsr  sr_InzD01;
029800090521           $InzD01 = *off;
029900090521         endif;
030000090521
030100090521         //?Emissione videata?
030200100503         write  TB81T01;
030300100503         write  TB81P01;
030400100503         exfmt  TB81D01;
030500090521
030600090521         reset  ErrMessage;
030700090521         reset  ErrGenerico;
030800090521         clear  V1Dmsg;
030900090521
031000090521         SELECT;
031100090521
031200090521           //?F3=Fine?
031300090521           WHEN  dsp_aid = c_F03;
031400090521             $Fine = *on;
031500090521
031600090521           //?Invio?
031700090521           OTHER;
031800090521             exsr sr_CtrD01;
031900090521             if  ErrGenerico = *off;
032000090521               $Video    = 'D2';
032100090521               $InzD02   = *on;
032200090521             endif;
032300090521
032400090521         ENDSL;
032500090521
032600090521       ENDSR;
032700090521
032800090521       //--------------------------------------------------------------
032900090521       //?Inizializzazione videata D01                                 ?
033000090521       //--------------------------------------------------------------
033100090521       BEGSR  sr_InzD01;
033200090521
033300100503         clear  TB81D01;
033400090521
033500090521       ENDSR;
033600090521
033700090521       //--------------------------------------------------------------
033800090521       //?Controllo dati immessi nella videata D01                     ?
033900090521       //--------------------------------------------------------------
034000090521       BEGSR  sr_CtrD01;
034100090521
034200090521         %subst(IndDspF : 28) = *off;
034300090521
034400090521         SELECT;
034500090521
034600100504           //?Ricerca codice Tipo Attività?
034700100504           WHEN  %scan('?' : V1Ctta) > *zeros;
034800100504             clear  TNTB81ds;
034900100504             tntb81r1 ( kpjba : tntb81ds );
035000090521             select;
035100100504               when  oTTAfxx <> *blank;
035200100504               when  oTTAerr = *on;
035300090521                 ErrMessage  = *on;
035400100504                 PosCurTTA   = *on;
035500100504                 V1Dmsg = oTTAmsg;
035600090521               other;
035700100504                 V1Ctta = %subst(oTTAke1 : 1 : %len(V1Ctta));
035800090521             endsl;
035900090521             ErrGenerico = *on;
036000090521             leavesr;
036100090521
036200090521           //?Controllo immissione codice causale?
036300100504           WHEN  V1Ctta = *blank;
036400090521             ErrMessage  = *on;
036500090521             ErrGenerico = *on;
036600100504             PosCurTTA   = *on;
036700090521             V1Dmsg = $Msg(01);
036800090521             leavesr;
036900090521
037000090521         ENDSL;
037100090521
037200090521       ENDSR;
037300090521
037400090521       //--------------------------------------------------------------
037500090521       //?Gestione videata D02                                         ?
037600090521       //--------------------------------------------------------------
037700090521       BEGSR  sr_GesD02;
037800090521
037900090521         //?Inizializzazione videata?
038000090521         if  $InzD02 = *on;
038100090521           exsr sr_InzD02;
038200090521           $InzD02 = *off;
038300090521         endif;
038400090521
038500090521         //?Emissione Testata e Piede con tasti funzionali abilitati?
038600090521         if  errGenerico = *off;
038700100503           write  TB81T01;
038800100503           write  TB81D01;
038900090521           write  Protect;
039000100503           write  TB81P02;
039100090521         endif;
039200090521
039300090521         //?Emissione videata?
039400100503         exfmt  TB81D02;
039500090521
039600090521         reset  ErrMessage;
039700090521         reset  ErrGenerico;
039800090521         clear  V1Dmsg;
039900090521         reset  $Annullamento;
040000090521
040100090521         SELECT;
040200090521
040300090521           //?F3=Fine?
040400090521           WHEN  dsp_aid = c_F03;
040500090521             unlock TNTBE01L;
040600090521             $Fine = *on;
040700090521
040800090521           //?F12=Ritorno?
040900090521           WHEN  dsp_aid = c_F12;
041000090521             unlock TNTBE01L;
041100090521             reset  $Video;
041200090521             clear  V1Topz;
041300090521
041400090521           //?Enter; F5=Ripristino; F6=Conferma; F16=Annullamento?
041500090521           OTHER;
041600090521             $Ripristino   = (dsp_aid = c_F05);
041700090521             $Annullamento = (dsp_aid = c_F16);
041800090521             if  Not $Annullamento;
041900090521               exsr  sr_CtrD02;
042000090526               if  ErrGenerico;
042100090526                 leavesr;
042200090526               endif;
042300090521             endif;
042400090526             select;
042500090526               when  dsp_aid = c_F05  or
042600090526                     dsp_aid = c_F06  or
042700090526                     dsp_aid = c_F16;
042800090526                 $Video  = 'W1';
042900090526                 reset  $InzW01;
043000090526             endsl;
043100090521
043200090521         ENDSL;
043300090521
043400090521       ENDSR;
043500090521
043600090521       //--------------------------------------------------------------
043700090521       //?Inizializzazione videata D02                                 ?
043800090521       //--------------------------------------------------------------
043900090521       BEGSR  sr_InzD02;
044000090521
044100090521         //?Spegnimento di TUTTI gli indicatori?
044200090521         IndDspF     = *off;
044300090521
044400090521         //?Pulizia videata?
044500100503         clear  TB81D02;
044600090521
044700090521         //?Reperimento dati della causale contatto marketing?
044800090521         reset  $Immissione;
044900090521         reset  $Ripristino;
045000100504         k_TBEke1 = V1Ctta;
045100090521         chain  %kds(k05tntbe01)  TNTBE000;
045200090521         select;
045300090521           when  Not %found(TNTBE01L);
045400090521             $Immissione = *on;
045500090521           when  TBEatb <> *blank;
045600090521             $Ripristino = *on;
045700090521         endsl;
045800090521
045900090521         //?Caricamento dati della causale contatto marketing?
046000090521         if  %found(TNTBE01L);
046100090521           exsr  sr_RieD02;
046200090525         endif;
046300090521
046400090521         //?Impostazione tipo manutenzione?
046500090521         select;
046600090521           when  $Immissione;
046700090521             V1Topz = 'IMMISSIONE';
046800090521           when  $Ripristino;
046900090521             V1Topz = 'RIPRISTINO';
047000090521           other;
047100090521             V1Topz = ' MODIFICA ';
047200090521         endsl;
047300090521
047400090521         //?Impostazione indicatori per abilitazione tasti funzionali?
047500090521         exsr  Set_Ind_D02;
047600090521
047700090521       ENDSR;
047800090521
047900090521       //--------------------------------------------------------------
048000090521       //?Caricamento singola riga di testo nella videata D02          ?
048100090521       //--------------------------------------------------------------
048200090521       BEGSR  sr_RieD02;
048300090521
048400100504         dTTA = TBEuni;
048500090521
048600100504         V1Cdes   = §TTAdes;
048700100504         V1Ctit   = §TTAtit;
048800100504         V1Cdesb  = §TTAdesb;
048900100504         V1Cleg   = §TTAleg;
049000090521
049100090521       ENDSR;
049200090521
049300090521       //--------------------------------------------------------------
049400090521       //?Settaggio indicatori nella videata D02 per abilitazione      ?
049500090521       //?  tasti funzionali (sul subfile control)                     ?
049600090521       //--------------------------------------------------------------
049700090521       BEGSR Set_Ind_D02;
049800090521
049900090521         //?F5=Ripristino?
050000090521         F5Attivo  = (NOT $Immissione  and  $Ripristino);
050100090521
050200090521         //?F6=Conferma?
050300090521         F6Attivo  = (NOT F5Attivo);
050400090521
050500090521         //?F16=Annullamento?
050600090521         F16Attivo = (NOT $Immissione  and  NOT $Ripristino);
050700090521
050800090521       ENDSR;
050900090521
051000090521       //--------------------------------------------------------------
051100090521       //?Controllo dati videata D02                                   ?
051200090521       //--------------------------------------------------------------
051300090521       BEGSR  sr_CtrD02;
051400090521
051500090521         %subst(IndDspF : 50) = *off;
051600090521
051700090521         select;
051800090521
051900090521           //?Descrizione obbligatoria?
052000090521           when  V1Cdes = *blank;
052100090521             ErrMessage  = *on;
052200090521             ErrGenerico = *on;
052300090521             PosCurDES   = *on;
052400090521             V1Dmsg = $Msg(02);
052500090521             leavesr;
052600090521
052700090521         endsl;
052800090526
052900090526       ENDSR;
053000090521
053100090521       //--------------------------------------------------------------
053200090521       //?Gestione trasmissione aggiornamento                          ?
053300090521       //--------------------------------------------------------------
053400090521       BEGSR  sr_GesW01;
053500090521
053600090521         // Inizializzazione videata
053700090521         if $InzW01 = *on;
053800090521           exsr  sr_InzW01;
053900090521           $InzW01 = *off;
054000090521         endif;
054100090521
054200090521         // Emissione window
054300090521         if  TBXftt = 'S';
054400100503           exfmt  TB81W01;
054500090521           ErrMessage  = *off;
054600090521           ErrGenerico = *off;
054700090521           clear  V1Dmsg;
054800090526         else;
054900090526           dsp_aid = c_F06;
055000090521         endif;
055100090521
055200090521         select;
055300090521
055400090521           // F12=Ritorno (gestito solo se emesso W01)
055500090521           when  dsp_aid = c_F12;
055600090521             $Video = 'D2';
055700090521
055800090521           // F6=Conferma
055900090521           when  dsp_aid = c_F06;
056000090526             exsr  sr_UpdTNTBE;
056100090521             // Ritorno alla videata iniziale
056200090521             if  NOT  ErrGenerico;
056300090521               clear  V1Topz;
056400090521               $Video  = 'D1';
056500090521               $InzD01 = *on;
056600090521             endif;
056700090521         endsl;
056800090521
056900090521       ENDSR;
057000090521
057100090521       //--------------------------------------------------------------
057200090521       //?Inizializzazione videata W01                                 ?
057300090521       //--------------------------------------------------------------
057400090521       BEGSR  sr_InzW01;
057500090521
057600100503         clear TB81W01;
057700090521
057800090521         if  $Immissione;
057900090521
058000090521           // Se immissione
058100090521           W1ftt  = TBXftt;
058200090521
058300090521         else;
058400090521
058500090521           // Se NON immissione:
058600090521           // visualizza i dati relativi all'ultima trasmissione
058700090521           W1ftt  = TBEftt;
058800090521           W1flt  = TBEflt;
058900090521           W1ftr  = TBEftr;
059000090521           if TBEdtr <> *zero;
059100090521             wDate_EUR = %date(TBEdtr : *iso);
059200090521             W1dtr     = %dec(wDate_EUR);
059300090521           endif;
059400090521
059500090521         endif;
059600090521
059700090521       ENDSR;
059800090521
059900090521       //--------------------------------------------------------------
060000100504       //?Aggiornamento records TNTBE00F (tab. "TTA")                  ?
060100090521       //--------------------------------------------------------------
060200090526       BEGSR  sr_UpdTNTBE;
060300090521
060400090521         //?Aggancio record di TNTBE00F?
060500090521         k_TBEcod  = c_Tab;
060600100504         k_TBEke1  = V1Ctta;
060700090521         clear  k_TBEke2;
060800090521         clear  k_TBElin;
060900090521         clear  k_TBEsif;
061000090521         //k_TBElin  = TBXlin;
061100090521         //k_TBEsif  = TBXsif;
061200090521
061300090521         chain  %kds(K05tntbe01) TNTBE000;
061400090526
061500100504         exsr  sr_RieTTA;
061600100504         TBEuni = dTTA;
061700090521
061800100504         //?Aggiornamento tab. "TTA"?
061900090521         SELECT;
062000090521
062100090521           // -?F5=Ripristino?
062200090521           WHEN  $Ripristino;
062300090521             clear  TBEatb;
062400090521             TBEftt = W1ftt;
062500090521             clear  TBEftr;
062600090521             //_______________
062700090521             UPDATE  TNTBE000;
062800090521             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
062900090521
063000090521           // -?F16=Annullamento?
063100090521           WHEN  $Annullamento;
063200090521             TBEatb = 'A';
063300090521             TBEftt = W1ftt;
063400090521             clear  TBEftr;
063500090521             //_______________
063600090521             UPDATE  TNTBE000;
063700090521             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
063800090521
063900090521           // -?Immissione o Modifica?
064000090521           OTHER;
064100090521
064200090521             if  NOT %found(TNTBE01L);
064300090521               clear TNTBE000;
064400090521               TBEcod  = k_TBEcod;
064500090521               TBEke1  = k_TBEke1;
064600090521               TBEke2  = k_TBEke2;
064700090521               TBElin  = k_TBElin;
064800090521               TBEsif  = k_TBEsif;
064900100504               TBEuni  = dTTA;
065000090521             endif;
065100090521
065200090521             TBEapl = TBXapl;
065300090521             clear  TBEatb;
065400090521             TBEftt = W1ftt;
065500090521             clear  TBEflt;
065600090521             clear  TBEftr;
065700090521             //clear TBEdtr;
065800090521
065900090521             // - Aggiornamento record
066000090521             if  %found(TNTBE01L);
066100090521               //_____________
066200090521               UPDATE TNTBE000;
066300090521               //¯¯¯¯¯¯¯¯¯¯¯¯¯
066400090521             else;
066500090521               //_____________
066600090521               WRITE  TNTBE000;
066700090521               //¯¯¯¯¯¯¯¯¯¯¯¯¯
066800090521             endif;
066900090521
067000090521         ENDSL;
067100090521
067200090521       ENDSR;
067300090521
067400090521       //--------------------------------------------------------------
067500100504       //?Caricamento dati in tab. "TTA"                               ?
067600090521       //--------------------------------------------------------------
067700100504       BEGSR  sr_RieTTA;
067800090521
067900100504         clear  dTTA;
068000090521
068100100504         §TTAdes  = V1Cdes;
068200100504         §TTAtit  = V1Ctit;
068300100504         §TTAdesb = V1Cdesb;
068400100504         §TTAleg  = V1Cleg;
068500090521
068600090521       ENDSR;
068700090521
068800090521       //--------------------------------------------------------------
068900090521       //?Operazioni finali.                                           ?
069000090521       //--------------------------------------------------------------
069100090521       BEGSR  sr_RoutEnd;
069200090521
069300090521         //?Chiusura funzioni precedentemente aperte?
069400090521         clear  TIBS02ds;
069500090521         T02tla = 'C';
069600090521         TNTBE_RicercaControllo ( kpjba : tibs02ds );
069700090521
069800090521         //?Chiusura pgm?
069900090521         return;
070000090521
070100090521       ENDSR;
070200090521
070300090521      /end-free
070400090521
070500090521       //--------------------------------------------------------------
070600090521       //?Definizione schiere a tempo di compilazione                  ?
070700090521       //--------------------------------------------------------------
070800090521
070900090521** > $Msg -------------------------------------------------------------------*
071000100504Immettere il Tipo Attività Marketing                                            1
071100090521Descrizione obbligatoria                                                        2
