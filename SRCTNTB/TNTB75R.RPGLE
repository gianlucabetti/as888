000100090521      *PARMS dfactgrp(*no) actgrp(*caller) dbgview(*source)
000200090521       //--------------------------------------------------------------
000300090521       //?Gestione tabella "CCO" (Causali contatto marketing)          ?
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
001800090521     fTNTB75D   cf   e             workstn
001900090521     f                                     indds(IndDspF)
002000090521     f                                     infds(InfDspF)
002100090521
002200090521       //--------------------------------------------------------------
002300090521       //?Definizione costanti.                                        ?
002400090521       //--------------------------------------------------------------
002500090521
002600090521       // - Codice tabella in gestione
002700090521     d c_Tab           c                   const('CCO')
002800090525
002900090525       // - Codice utente in vecchie tabelle (TABEL00F)
003000090525     d c_Kut           c                   const(1)
003100090525
003200090525       // - Forzatura *blank
003300090525     d c_Blank         c                   const('BL')
003400090525     d c_Blank_Des     c                   const('Nessuno')
003500090525     d c_All           c                   const('99')
003600090525     d c_All_Des       c                   const('Qualsiasi')
003700090521
003800090521       // - Tasti funzionali a video
003900090521     d c_F01           c                   const(x'31')
004000090521     d c_F02           c                   const(x'32')
004100090521     d c_F03           c                   const(x'33')
004200090521     d c_F05           c                   const(x'35')
004300090521     d c_F06           c                   const(x'36')
004400090521     d c_F07           c                   const(x'37')
004500090521     d c_F08           c                   const(x'38')
004600090521     d c_F09           c                   const(x'39')
004700090521     d c_F10           c                   const(x'3A')
004800090521     d c_F12           c                   const(x'3C')
004900090521     d c_F13           c                   const(x'B1')
005000090521     d c_F14           c                   const(x'B2')
005100090521     d c_F15           c                   const(x'B3')
005200090521     d c_F16           c                   const(x'B4')
005300090521     d c_F17           c                   const(x'B5')
005400090521     d c_F18           c                   const(x'B6')
005500090521     d c_F19           c                   const(x'B7')
005600090521     d c_F20           c                   const(x'B8')
005700090521     d c_F21           c                   const(x'B9')
005800090521     d c_F22           c                   const(x'BA')
005900090521     d c_F23           c                   const(x'BB')
006000090521     d c_F24           c                   const(x'BC')
006100090521     d c_Enter         c                   const(x'F1')
006200090521     d c_RollDown      c                   const(x'F4')
006300090521     d c_RollUp        c                   const(x'F5')
006400100127
006500100127     d $_opz           c                   const(' 25G')
006600090521
006700090521       //--------------------------------------------------------------
006800090521       //?Definizione schiere.                                         ?
006900090521       //--------------------------------------------------------------
007000090521
007100090521       // - Messaggi di errore
007200100126     d $Msg            s             78    dim(20)  ctdata  perrcd(01)
007300090521
007400090521       //--------------------------------------------------------------
007500090521       //?Definizione aree dati.                                       ?
007600090521       //--------------------------------------------------------------
007700090521
007800090521       // - Dati utente
007900090521     d §AzUte        e ds                  extname(AZUTE00F)
008000090521     d                                     dtaara
008100090521     d §DatiUte      e ds                  extname(dDatiUte)
008200090521     d                                     dtaara
008300090521
008400090521       //--------------------------------------------------------------
008500090521       //?Definizione strutture dati.                                  ?
008600090521       //--------------------------------------------------------------
008700090521
008800090521       // - Status ds
008900090521     d Status         sds
009000090521     d  SDSpgm           *proc
009100090521
009200090521       // - InfDS
009300090521     d InfDspF         ds
009400090521     d   dsp_aid             369    369a
009500090521
009600090521       // - Indicatori su DspF
009700090521     d IndDspF         ds                  inz
009800090521         // - Abilitazione tasti funzionali
009900090521     d   F5Attivo                      n   overlay(IndDspF : 05)
010000090521     d   F6Attivo                      n   overlay(IndDspF : 06)
010100090521     d   F10Attivo                     n   overlay(IndDspF : 10)
010200090521     d   F16Attivo                     n   overlay(IndDspF : 16)
010300090521         // - Emissione messaggio di errore
010400090521     d   ErrMessage                    n   overlay(IndDspF : 28)
010500090521         // - Posizionamento cursore & segnalazione errore
010600101209     d   PosCurTPV6                    n   overlay(IndDspF : 40)
010700101209     d   PosCurTPV7                    n   overlay(IndDspF : 41)
010800101209     d   PosCurTPV8                    n   overlay(IndDspF : 42)
010900101209     d   PosCurTPV9                    n   overlay(IndDspF : 43)
011000101209     d   PosCurTPV0                    n   overlay(IndDspF : 44)
011100101209     d   PosCurCAE                     n   overlay(IndDspF : 45)
011200110429     d   PosCurCP1                     n   overlay(IndDspF : 46)
011300110429     d   PosCurCP2                     n   overlay(IndDspF : 47)
011400110429     d   PosCurCP3                     n   overlay(IndDspF : 48)
011500110429     d   PosCurCP4                     n   overlay(IndDspF : 49)
011600090521     d   PosCurCAM                     n   overlay(IndDspF : 50)
011700090521     d   PosCurDES                     n   overlay(IndDspF : 51)
011800090521     d   PosCurGIO                     n   overlay(IndDspF : 52)
011900100127     d   PosCurOPZ1                    n   overlay(IndDspF : 53)
012000100127     d   PosCurOPZ2                    n   overlay(IndDspF : 54)
012100100127     d   PosCurOPZ3                    n   overlay(IndDspF : 55)
012200100127     d   PosCurOPZ4                    n   overlay(IndDspF : 56)
012300100127     d   PosCurOPZ5                    n   overlay(IndDspF : 57)
012400100924     d   PosCurUPM                     n   overlay(IndDspF : 61)
012500100504     d   PosCurTPV1                    n   overlay(IndDspF : 62)
012600100504     d   PosCurTPV2                    n   overlay(IndDspF : 63)
012700100504     d   PosCurTPV3                    n   overlay(IndDspF : 64)
012800100504     d   PosCurTPV4                    n   overlay(IndDspF : 65)
012900100504     d   PosCurTPV5                    n   overlay(IndDspF : 66)
013000100126     d   PosCurATT                     n   overlay(IndDspF : 87)
013100090629     d   PosCurTAT                     n   overlay(IndDspF : 88)
013200090521         //   - Riemissione videata
013300090521     d   ErrGenerico                   n   overlay(IndDspF : 99)
013400090521
013500090521       // - Parametri ricevuti
013600090521     d KPJBA         e ds
013700090521
013800090521       // - Parametri per Reperimento dati utente
013900090521     d TIBS34ds      e ds
014000090521
014100090521       // - Parametri per Ricerca/controllo tabelle
014200090521     d TIBS02ds      e ds                  inz
014300090521     d   T02mod      e                     inz('R')
014400090521     d   T02cod      e                     inz(c_Tab)
014500090525
014600090525       // - Tab. CCO = Causali contatto marketing
014700090525     d dCCO          e ds                  inz
014800090525       // - Tab. ECO = Esiti contatto marketing
014900090525     d dECO          e ds                  inz
015000090525
015100090521       // - Parametri per Interrogazione/Selezione tab. "CCO"
015200090521     d TNTB74ds      e ds                  inz
015300090521
015400090521       // - Dati record principale della tabella
015500090521     d TNTBEds       e ds                  inz  extname(TNTBE00F)
015600090521     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
015700090521     d                                          prefix(TBX:3)
015800090521
015900100219       // - Ridefinizione dati a video
016000100219       // - - Tipi Attività abilitati
016100100219     d dsV1CTAA        ds            10    inz
016200100219     d  V1Caa1                             inz
016300100219     d  V1Caa2                             inz
016400100219     d  V1Caa3                             inz
016500100219     d  V1Caa4                             inz
016600100219     d  V1Caa5                             inz
016700100219     d  V1Caa6                             inz
016800100219     d  V1Caa7                             inz
016900100219     d  V1Caa8                             inz
017000100219     d  V1Caa9                             inz
017100100219     d  V1Ca10                             inz
017200100504       // - - Tipi Trattativa
017300101209     d dsV1CTPV        ds            10    inz
017400100504     d  V1Ctpv1                            inz
017500100504     d  V1Ctpv2                            inz
017600100504     d  V1Ctpv3                            inz
017700100504     d  V1Ctpv4                            inz
017800100504     d  V1Ctpv5                            inz
017900101209     d  V1Ctpv6                            inz
018000101209     d  V1Ctpv7                            inz
018100101209     d  V1Ctpv8                            inz
018200101209     d  V1Ctpv9                            inz
018300101209     d  V1Ctpv0                            inz
018400101209     d   $V1Ctpv               1     10    inz  dim(10)
018500100219
018600110429       // - - Categoria Potenziale abilitate
018700110429     d dsV1Ccpo        ds             4    inz
018800110429     d  V1Ccp1                             inz
018900110429     d  V1Ccp2                             inz
019000110429     d  V1Ccp3                             inz
019100110429     d  V1Ccp4                             inz
019200110429     d   $V1Ccpo               1      4    inz  dim(4)
019300090521
019400090521       //--------------------------------------------------------------
019500090521       //?Definizione variabili globali.                               ?
019600090521       //--------------------------------------------------------------
019700090521
019800090521       // - Flags booleani
019900090521     d $Fine           s               n   inz
020000090521     d $InzD01         s               n   inz(*on)
020100090521     d $InzD02         s               n   inz(*on)
020200101209     d $InzD03         s               n   inz(*on)
020300090521     d $InzW01         s               n   inz(*on)
020400090521     d $Annullamento   s               n   inz
020500090521     d $Immissione     s               n   inz
020600090521     d $Ripristino     s               n   inz
020700090521
020800090521       // - Indici di schiera
020900090521     d xx              s              3  0 inz
021000090521
021100090521       // - Variabili per la gestione del video
021200090521     d $Video          s              2    inz('D1')
021300090525
021400090525       // - Flag di errore
021500090525     d $Err            s              1    inz(*off)
021600090521
021700090521       // - Campi di comodo
021800090521     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
021900110429     d savcod          s              1
022000090521
022100090525       // - Parametri per pgm interrogazione tabelle
022200090525      /copy gaitrasrc/srcProtoPI,X§TABER
022300090521
022400090521       //--------------------------------------------------------------
022500090521       //?Definizione prototipi procedure.                             ?
022600090521       //--------------------------------------------------------------
022700090521
022800090521       // - Reperimento dati utente
022900090521      /copy gaitrasrc/srcProtoPR,TIBS34R
023000090521
023100090525       // - Ricerca vecchie tabelle
023200090525      /copy gaitrasrc/srcProtoPR,X§TABER
023300090525
023400090525       // - Ricerca e controllo nuove tabelle
023500090521      /copy gaitrasrc/srcProtoPR,TIBS02R
023600090521
023700090525       // - Selezione singola causale attività
023800090521     d tntb74r         pr                  extpgm('TNTB74R')
023900090525     d   kpjba                             likeds(KPJBA)
024000090521     d   tntb74ds                          likeds(TNTB74ds)
024100090521
024200090521       //--------------------------------------------------------------
024300090521       //?Definizione key-list.                                        ?
024400090521       //--------------------------------------------------------------
024500090521
024600090525       // - File TABEL00F
024700090525     d k03tabel00    e ds                  extname(TABEL00F : *key)
024800090525     d                                     prefix(k_)   inz
024900090525
025000090521       // - File TNTBE01L
025100090521     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
025200090521     d                                     prefix(k_)   inz
025300090521
025400090521       //--------------------------------------------------------------
025500090521       //?Riepilogo indicatori utilizzati.                             ?
025600090521       //--------------------------------------------------------------
025700090521       //--------------------------------------------------------------
025800090521
025900090521       //--------------------------------------------------------------
026000090521       //?M A I N - L I N E                                            ?
026100090521       //--------------------------------------------------------------
026200090521
026300090521     c     *Entry        plist
026400090521     c                   parm                    KPJBA
026500090521
026600090521      /free
026700090521
026800090521       //?Operazioni iniziali?
026900090521       exsr sr_RoutInz;
027000090521
027100090521       //?Gestione video?
027200090521       DOW  $Fine = *off;
027300090521         select;
027400090521           // - Manutenzione singola tabella
027500090521           when $Video = 'D2';
027600090521             exsr sr_GesD02;
027700090526           when $Video = 'D3';
027800090526             exsr sr_GesD03;
027900090521           // - Filtro di lancio
028000090521           when $Video = 'D1';
028100090521             exsr sr_GesD01;
028200090521           // - Richiesta dati per trasmissione record
028300090521           when $Video = 'W1';
028400090521             exsr sr_GesW01;
028500090521           other;
028600090521             $Fine = *on;
028700090521         endsl;
028800090521       ENDDO;
028900090521
029000090521       //?Operazioni finali?
029100090521       exsr sr_RoutEnd;
029200090521
029300090521       //--------------------------------------------------------------
029400090521       //?Operazioni iniziali.                                         ?
029500090521       //--------------------------------------------------------------
029600090521       BEGSR  sr_RoutInz;
029700090521
029800090521         *inLR = *on;
029900090521
030000090521         //?Reperimento dati job?
030100090521         exsr  sr_DatiJob;
030200090521
030300090521         //?Impostazione nome programma a video?
030400090521         V1Tpgm = SDSpgm;
030500090521
030600090521         //?Aggancio dati generali della tabella in esame?
030700090521         clear  k_TBEcod;
030800090521         k_TBEke1 = *zero;
030900090521         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
031000090521                = c_Tab;
031100090521         clear  k_TBEke2;
031200090521         clear  k_TBElin;
031300090521         k_TBEsif = KNSIF;
031400090521         chain(n)  %kds(k05tntbe01)  TNTBE000;
031500090521         if  not  %found(TNTBE01L);
031600090521           clear  k_TBEsif;
031700090521           chain(n)  %kds(k05tntbe01)  TNTBE000;
031800090521         endif;
031900090521         if  %found(TNTBE01L);
032000090521           xTNTBEds = TNTBEds;
032100090521         else;
032200090521           clear  xTNTBEds;
032300090521         endif;
032400090521
032500090521         //?Impostazione iniziale / pulizia dei campi chiave?
032600090521         k_TBEcod = c_Tab;
032700090521         clear  k_TBEke1;
032800090521         clear  k_TBEke2;
032900090521         clear  k_TBElin;
033000090521         clear  k_TBEsif;
033100090521
033200090521         //?1ª inizializzazione della videata D01?
033300090521         exsr  sr_InzD01;
033400090521         V1Ccam  = '?';
033500090521         $InzD01 = *off;
033600090521
033700090521       ENDSR;
033800090521
033900090521       //--------------------------------------------------------------
034000090521       //?Reperimento Dati del job (Utente/Operativi).                 ?
034100090521       //--------------------------------------------------------------
034200090521       BEGSR  sr_DatiJob;
034300090521
034400090521         in(E) §AzUte;
034500090521         if NOT %error;
034600090521           in(E) §DatiUte;
034700090521         endif;
034800090521         if %error or RSut = *blanks;
034900090521           clear TIBS34ds;
035000090521           tibs34r ( tibs34ds );
035100090521           in §AzUte;
035200090521           in §DatiUte;
035300090521         endif;
035400090521
035500090521       ENDSR;
035600090521
035700090521       //--------------------------------------------------------------
035800090521       //?Gestione videata D01                                         ?
035900090521       //--------------------------------------------------------------
036000090521       BEGSR  sr_GesD01;
036100090521
036200090521         //?Inizializzazione videata?
036300090521         if  $InzD01 = *on;
036400090521           exsr  sr_InzD01;
036500090521           $InzD01 = *off;
036600090521         endif;
036700090521
036800090521         //?Emissione videata?
036900090521         write  TB75T01;
037000090521         write  TB75P01;
037100090521         exfmt  TB75D01;
037200090521
037300090521         reset  ErrMessage;
037400090521         reset  ErrGenerico;
037500090521         clear  V1Dmsg;
037600090521
037700090521         SELECT;
037800090521
037900090521           //?F3=Fine?
038000090521           WHEN  dsp_aid = c_F03;
038100090521             $Fine = *on;
038200090521
038300090521           //?Invio?
038400090521           OTHER;
038500090521             exsr sr_CtrD01;
038600090521             if  ErrGenerico = *off;
038700090521               $Video    = 'D2';
038800090521               $InzD02   = *on;
038900090521             endif;
039000090521
039100090521         ENDSL;
039200090521
039300090521       ENDSR;
039400090521
039500090521       //--------------------------------------------------------------
039600090521       //?Inizializzazione videata D01                                 ?
039700090521       //--------------------------------------------------------------
039800090521       BEGSR  sr_InzD01;
039900090521
040000090525         clear  TB75D01;
040100090521
040200090521       ENDSR;
040300090521
040400090521       //--------------------------------------------------------------
040500090521       //?Controllo dati immessi nella videata D01                     ?
040600090521       //--------------------------------------------------------------
040700090521       BEGSR  sr_CtrD01;
040800090521
040900090521         %subst(IndDspF : 28) = *off;
041000090521
041100090521         SELECT;
041200090521
041300090521           //?Ricerca codice Causale Attività?
041400090521           WHEN  %scan('?' : V1Ccam) > *zeros;
041500090521             clear  TNTB74ds;
041600101201             kpjbu = 'TNTB75R' ;
041700090521             tntb74r ( kpjba : tntb74ds );
041800090521             select;
041900090521               when  oCCOfxx <> *blank;
042000090521               when  oCCOerr = *on;
042100090521                 ErrMessage  = *on;
042200090521                 PosCurCAM   = *on;
042300090521                 V1Dmsg = oCCOmsg;
042400090521               other;
042500090521                 V1Ccam = %subst(oCCOke1 : 1 : %len(V1Ccam));
042600090521             endsl;
042700090521             ErrGenerico = *on;
042800090521             leavesr;
042900090521
043000090521           //?Controllo immissione codice causale?
043100090521           WHEN  V1Ccam = *blank;
043200090521             ErrMessage  = *on;
043300090521             ErrGenerico = *on;
043400090521             PosCurCAM   = *on;
043500090521             V1Dmsg = $Msg(01);
043600090521             leavesr;
043700090521
043800090521         ENDSL;
043900090521
044000090521       ENDSR;
044100090521
044200090521       //--------------------------------------------------------------
044300090521       //?Gestione videata D02                                         ?
044400090521       //--------------------------------------------------------------
044500090521       BEGSR  sr_GesD02;
044600090521
044700090521         //?Inizializzazione videata?
044800090521         if  $InzD02 = *on;
044900090521           exsr sr_InzD02;
045000090521           $InzD02 = *off;
045100090521         endif;
045200090521
045300090521         //?Emissione Testata e Piede con tasti funzionali abilitati?
045400090521         if  errGenerico = *off;
045500090521           write  TB75T01;
045600090521           write  TB75D01;
045700090521           write  Protect;
045800090521           write  TB75P02;
045900090521         endif;
046000090521
046100090521         //?Emissione videata?
046200090521         exfmt  TB75D02;
046300090521
046400090521         reset  ErrMessage;
046500090521         reset  ErrGenerico;
046600090521         clear  V1Dmsg;
046700090521         reset  $Annullamento;
046800090521
046900090521         SELECT;
047000090521
047100090521           //?F3=Fine?
047200090521           WHEN  dsp_aid = c_F03;
047300090521             unlock TNTBE01L;
047400090521             $Fine = *on;
047500090521
047600090521           //?F12=Ritorno?
047700090521           WHEN  dsp_aid = c_F12;
047800090521             unlock TNTBE01L;
047900090521             reset  $Video;
048000090521             clear  V1Topz;
048100090526
048200090526           //?RollUp=Videata successiva?
048300090526           WHEN  dsp_aid = c_RollUp;
048400090526             $Video  = 'D3';
048500090521
048600090521
048700090521           //?Enter; F5=Ripristino; F6=Conferma; F16=Annullamento?
048800090521           OTHER;
048900090521             $Ripristino   = (dsp_aid = c_F05);
049000090521             $Annullamento = (dsp_aid = c_F16);
049100090521             if  Not $Annullamento;
049200090521               exsr  sr_CtrD02;
049300090526               if  ErrGenerico;
049400090526                 leavesr;
049500090526               endif;
049600090526               exsr  sr_CtrD03;
049700090526               if  ErrGenerico;
049800090526                 $Video  = 'D3';
049900090526                 leavesr;
050000090526               endif;
050100090521             endif;
050200090526             select;
050300101210               when  dsp_aid = c_Enter  ;
050400090526                 $Video  = 'D3';
050500090526               when  dsp_aid = c_F05  or
050600090526                     dsp_aid = c_F06  or
050700090526                     dsp_aid = c_F16;
050800090526                 $Video  = 'W1';
050900090526                 reset  $InzW01;
051000090526             endsl;
051100090521
051200090521         ENDSL;
051300090521
051400090521       ENDSR;
051500090521
051600090521       //--------------------------------------------------------------
051700090521       //?Inizializzazione videata D02                                 ?
051800090521       //--------------------------------------------------------------
051900090521       BEGSR  sr_InzD02;
052000090521
052100090521         //?Spegnimento di TUTTI gli indicatori?
052200090521         IndDspF     = *off;
052300090521
052400090521         //?Pulizia videata?
052500090521         clear  TB75D02;
052600090526         clear  TB75D03;
052700090521
052800090521         //?Reperimento dati della causale contatto marketing?
052900090521         reset  $Immissione;
053000090521         reset  $Ripristino;
053100090521         k_TBEke1 = V1Ccam;
053200090521         chain  %kds(k05tntbe01)  TNTBE000;
053300090521         select;
053400090521           when  Not %found(TNTBE01L);
053500090521             $Immissione = *on;
053600090521           when  TBEatb <> *blank;
053700090521             $Ripristino = *on;
053800090521         endsl;
053900090521
054000090521         //?Caricamento dati della causale contatto marketing?
054100090521         if  %found(TNTBE01L);
054200090521           exsr  sr_RieD02;
054300090525         endif;
054400090521
054500090521         //?Impostazione tipo manutenzione?
054600090521         select;
054700090521           when  $Immissione;
054800090521             V1Topz = 'IMMISSIONE';
054900090521           when  $Ripristino;
055000090521             V1Topz = 'RIPRISTINO';
055100090521           other;
055200090521             V1Topz = ' MODIFICA ';
055300090521         endsl;
055400090521
055500090521         //?Impostazione indicatori per abilitazione tasti funzionali?
055600090521         exsr  Set_Ind_D02;
055700090521
055800090521       ENDSR;
055900090521
056000090521       //--------------------------------------------------------------
056100090521       //?Caricamento singola riga di testo nella videata D02          ?
056200090521       //--------------------------------------------------------------
056300090521       BEGSR  sr_RieD02;
056400090521
056500090521         dCCO = TBEuni;
056600090521
056700090521         V1Cdes   = §CCOdes;
056800100126         V1Cord   = §CCOord;
056900090521         V1Cgio   = §CCOgio;
057000100126         V1Ctat   = §CCOtat;
057100100126         V1Catt   = §CCOatt;
057200110429         dsV1Ccpo = §CCOcpo;
057300100924         V1Cupm   = §CCOupm;
057400100126         V1Cuti   = §CCOuti;
057500100126         V1Cesi   = §CCOesi;
057600100126         V1Ceaf   = §CCOeaf;
057700100126         V1Copz1  = %subst(§CCOopz:1:1);
057800100126         V1Copz2  = %subst(§CCOopz:2:1);
057900100126         V1Copz3  = %subst(§CCOopz:3:1);
058000100126         V1Copz4  = %subst(§CCOopz:4:1);
058100100126         V1Copz5  = %subst(§CCOopz:5:1);
058200100219         V1Caa1   = %subst(§CCOtaa:1:1);
058300100219         V1Caa2   = %subst(§CCOtaa:2:1);
058400100219         V1Caa3   = %subst(§CCOtaa:3:1);
058500100219         V1Caa4   = %subst(§CCOtaa:4:1);
058600100219         V1Caa5   = %subst(§CCOtaa:5:1);
058700100219         V1Caa6   = %subst(§CCOtaa:6:1);
058800100219         V1Caa7   = %subst(§CCOtaa:7:1);
058900100219         V1Caa8   = %subst(§CCOtaa:8:1);
059000100219         V1Caa9   = %subst(§CCOtaa:9:1);
059100100219         V1Ca10   = %subst(§CCOtaa:10:1);
059200100126         V1Cnot   = §CCOnot;
059300100126         V1Csta   = §CCOsta;
059400100219         V1Coff   = §CCOoff;
059500100305         V1Ccht   = §CCOcht;
059600110429         Savcod   = §CCOcod;
059700100305         V1Cres   = §CCOres;
059800100305         V1Ccom   = §CCOcoi;
059900120622         V1Cdpc   = §CCOdpc;
060000100126
060100101209         // imposto dati video 3
060200100504         dsV1Ctpv = §CCOtpv;
060300100126         V1Capt   = §CCOapt;
060400100126         V1Ctra   = §CCOtra;
060500100126         V1Ceso   = §CCOeso;
060600101209         V1Chot   = §CCOhot;
060700101209         V1Ccae   = §CCOcae;
060800101209         V1Cann   = §CCOann;
060900101209         V1Cblc   = §CCOblc;
061000101214         V1Cccp   = §CCOccp;
061100110211         V1Cstl   = §CCOstl;
061200110211         V1Cpgmblc = §CCOpgmblc;
061300110405         V1Ccns   = §CCOcns;
061400110405         V1Cucns  = §CCOucns;
061500090526
061600090526         // Decodifiche
061700090526         exsr  sr_CtrD02;
061800090526         exsr  sr_CtrD03;
061900090526         clear  ErrMessage;
062000090526         clear  ErrGenerico;
062100090526         clear  V1Dmsg;
062200090521
062300090521       ENDSR;
062400090521
062500090521       //--------------------------------------------------------------
062600090521       //?Settaggio indicatori nella videata D02 per abilitazione      ?
062700090521       //?  tasti funzionali (sul subfile control)                     ?
062800090521       //--------------------------------------------------------------
062900090521       BEGSR Set_Ind_D02;
063000090521
063100090521         //?F5=Ripristino?
063200090521         F5Attivo  = (NOT $Immissione  and  $Ripristino);
063300090521
063400090521         //?F6=Conferma?
063500090521         F6Attivo  = (NOT F5Attivo);
063600090521
063700090521         //?F16=Annullamento?
063800090521         F16Attivo = (NOT $Immissione  and  NOT $Ripristino);
063900090521
064000090521       ENDSR;
064100090521
064200090521       //--------------------------------------------------------------
064300090521       //?Controllo dati videata D02                                   ?
064400090521       //--------------------------------------------------------------
064500090521       BEGSR  sr_CtrD02;
064600090521
064700101209         %subst(IndDspF : 40) = *off;
064800090521
064900090521         select;
065000090521
065100090521           //?Descrizione obbligatoria?
065200090521           when  V1Cdes = *blank;
065300090521             ErrMessage  = *on;
065400090521             ErrGenerico = *on;
065500090521             PosCurDES   = *on;
065600090521             V1Dmsg = $Msg(02);
065700090521             leavesr;
065800090629
065900090521           //?Giorni proposti per prossimo contatto negativi?
066000090521           when  V1Cgio < *zero;
066100090521             ErrMessage  = *on;
066200090521             ErrGenerico = *on;
066300090521             PosCurGIO   = *on;
066400090521             V1Dmsg = $Msg(03);
066500090521             leavesr;
066600090521
066700090521         endsl;
066800090521
066900100504           //?Tipo contatto futuro obbligatorio?
067000090629           If    %scan('?' : V1Ctat) > *zero;
067100090629             errGenerico = *on;
067200090629             PosCurTat   = *on;
067300090629             clear  TIBS02ds;
067400090629             T02mod = 'R';
067500090629             T02cod = 'TTA';
067600090629             T02sif = KNSIF;
067700090629             TNTBE_RicercaControllo  (kpjba : tibs02ds);
067800090629             if  T02err = *blanks;
067900090629               v1ctat = T02ke1;
068000090629               errGenerico = *on;
068100090629               leavesr;
068200090629             else;
068300090629               errMessage  = *on;
068400090629               errGenerico = *on;
068500090629               V1Dmsg = T02msg;
068600090629               leavesr;
068700090629             endif;
068800090629           endif;
068900090629
069000090629         // Controllo tipo contatto
069100090629           If    v1ctat <> *blanks ;
069200090629                 clear  TIBS02ds;
069300090629                 T02mod = 'C';
069400090629                 T02cod = 'TTA';
069500090629                 T02ke1 = v1ctat;
069600090629                 T02sif = KNSIF;
069700090629                 TNTBE_RicercaControllo  (kpjba : tibs02ds);
069800090629                 if  T02err <> *blanks;
069900090629                     errMessage  = *on;
070000090629                     errGenerico = *on;
070100090629                     PosCurTat   = *on;
070200090629                     V1Dmsg = $Msg(10);
070300090629                     leavesr;
070400090629                 endif;
070500090629           else ;
070600100924        // se segno operazione <> da blanks obbligatorio l'inserimento
070700100924                 if v1cupm <> ' ' ;
070800090630                    errMessage  = *on;
070900090630                    errGenerico = *on;
071000090630                    PosCurTat   = *on;
071100090630                    V1Dmsg = $Msg(10);
071200090630                    leavesr;
071300090630                 endif;
071400090629           endif;
071500090525
071600100126
071700100127           //?Tipo attività associata?
071800100126           If    %scan('?' : V1Catt) > *zero;
071900100126             errGenerico = *on;
072000100126             PosCurATT   = *on;
072100100126             clear  TIBS02ds;
072200100126             T02mod = 'R';
072300100126             T02cod = 'TTA';
072400100126             T02sif = KNSIF;
072500100126             TNTBE_RicercaControllo  (kpjba : tibs02ds);
072600100126             if  T02err <> *blanks;
072700100126               errMessage  = *on;
072800100126               errGenerico = *on;
072900100126               V1Dmsg = T02msg;
073000100126               leavesr;
073100100126             endif;
073200100126               v1catt = T02ke1;
073300100126               errGenerico = *on;
073400100126               leavesr;
073500100126           endif;
073600100126
073700100126         // Controllo tipo attività
073800100126           If    v1catt <> *blanks ;
073900100126                 clear  TIBS02ds;
074000100126                 T02mod = 'C';
074100100126                 T02cod = 'TTA';
074200100126                 T02ke1 = v1catt;
074300100126                 T02sif = KNSIF;
074400100126                 TNTBE_RicercaControllo  (kpjba : tibs02ds);
074500100126                 if  T02err <> *blanks;
074600100126                     errMessage  = *on;
074700100126                     errGenerico = *on;
074800100126                     PosCurATT   = *on;
074900100126                     V1Dmsg = $Msg(09);
075000100126                     leavesr;
075100100126                 endif;
075200100126           endif;
075300100126
075400090526
075500100924        //?Segno operazione da fare alla data della prossima attività
075600100924           if    V1Cupm = '=' and V1CGio <> 0 ;
075700090525             errMessage  = *on;
075800090525             errGenerico = *on;
075900100924             PosCurUPM   = *on;
076000100924             V1Dmsg = $Msg(14);
076100090525             leavesr;
076200100126           endif;
076300100127
076400100924           if   (V1Cupm = '+' or V1Cupm = '-') and V1CGio = 0 ;
076500100924             errMessage  = *on;
076600100924             errGenerico = *on;
076700100924             PosCurUPM   = *on;
076800100924             V1Dmsg = $Msg(15);
076900100924             leavesr;
077000100924           endif;
077100100924
077200100924           if    V1Cupm = ' ' and V1CGio <> 0 ;
077300100924             errMessage  = *on;
077400100924             errGenerico = *on;
077500100924             PosCurUPM   = *on;
077600100924             V1Dmsg = $Msg(16);
077700100924             leavesr;
077800100924           endif;
077900100127
078000100127        //?Opzioni inibite?
078100100127           SELECT;
078200100127
078300100127           WHEN  %check($_opz:V1Copz1) > 0;
078400100127             errMessage  = *on;
078500100127             errGenerico = *on;
078600100127             PosCurOPZ1  = *on;
078700100127             V1Dmsg = $Msg(05);
078800100127             leavesr;
078900100127
079000100127           WHEN  %check($_opz:V1Copz2) > 0;
079100100127             errMessage  = *on;
079200100127             errGenerico = *on;
079300100127             PosCurOPZ2  = *on;
079400100127             V1Dmsg = $Msg(05);
079500100127             leavesr;
079600100127
079700100127           WHEN  %check($_opz:V1Copz3) > 0;
079800100127             errMessage  = *on;
079900100127             errGenerico = *on;
080000100127             PosCurOPZ3  = *on;
080100100127             V1Dmsg = $Msg(05);
080200100127             leavesr;
080300100127
080400100127           WHEN  %check($_opz:V1Copz4) > 0;
080500100127             errMessage  = *on;
080600100127             errGenerico = *on;
080700100127             PosCurOPZ4  = *on;
080800100127             V1Dmsg = $Msg(05);
080900100127             leavesr;
081000100127
081100100127           WHEN  %check($_opz:V1Copz5) > 0;
081200100127             errMessage  = *on;
081300100127             errGenerico = *on;
081400100127             PosCurOPZ5  = *on;
081500100127             V1Dmsg = $Msg(05);
081600100127             leavesr;
081700100127           ENDSL;
081800090526
081900090526
082000090526       ENDSR;
082100090526
082200090526       //--------------------------------------------------------------
082300090526       //?Gestione videata D03                                         ?
082400090526       //--------------------------------------------------------------
082500090526       BEGSR  sr_GesD03;
082600090526
082700090526         //?Inizializzazione videata?
082800090526         if  $InzD02 = *on;
082900090526           exsr sr_InzD02;
083000090526           $InzD02 = *off;
083100090526         endif;
083200090526
083300090526         //?Emissione Testata e Piede con tasti funzionali abilitati?
083400090526         if  errGenerico = *off;
083500090526           write  TB75T01;
083600090526           write  TB75D01;
083700090526           write  TB75D02;
083800090526           write  Protect;
083900090526           write  TB75P02;
084000090526         endif;
084100090526
084200090526         //?Emissione videata?
084300090526         exfmt  TB75D03;
084400090526
084500090526         reset  ErrMessage;
084600090526         reset  ErrGenerico;
084700090526         clear  V1Dmsg;
084800090526         reset  $Annullamento;
084900090526
085000090526         SELECT;
085100090526
085200090526           //?F3=Fine?
085300090526           WHEN  dsp_aid = c_F03;
085400090526             unlock TNTBE01L;
085500090526             $Fine = *on;
085600090526
085700090526           //?F12=Ritorno?
085800090526           WHEN  dsp_aid = c_F12;
085900090526             $Video  = 'D2';
086000090526
086100090526           //?RollDown=Videata precedente?
086200090526           WHEN  dsp_aid = c_RollDown;
086300090526             $Video  = 'D2';
086400090526
086500090526
086600090526           //?Enter; F5=Ripristino; F6=Conferma; F16=Annullamento?
086700090526           OTHER;
086800090526             $Ripristino   = (dsp_aid = c_F05);
086900090526             $Annullamento = (dsp_aid = c_F16);
087000090526             if  Not $Annullamento;
087100090526               exsr  sr_CtrD03;
087200090526               if  ErrGenerico;
087300090526                 leavesr;
087400090526               endif;
087500090526               exsr  sr_CtrD02;
087600090526               if  ErrGenerico;
087700090526                 $Video  = 'D2';
087800090526                 leavesr;
087900090526               endif;
088000090526             endif;
088100121018               IF    dsp_aid = c_F05  or
088200101210                     dsp_aid = c_F06  or
088300101210                     dsp_aid = c_F16;
088400090526               $Video  = 'W1';
088500090526               reset  $InzW01;
088600121018             endif;
088700090526
088800090526         ENDSL;
088900090526
089000090526       ENDSR;
089100090526
089200090526       //--------------------------------------------------------------
089300090526       //?Controllo dati videata D03                                   ?
089400090526       //--------------------------------------------------------------
089500090526       BEGSR  sr_CtrD03;
089600090526
089700101209         %subst(IndDspF : 40) = *off;
089800100504
089900100504         // -?Tipi trattativa?
090000100504         If  dsV1Ctpv <> *blank;
090100100504
090200100504           if  V1Ctra <> 'S';
090300100504             errMessage  = *on;
090400100504             errGenerico = *on;
090500100504             %subst(IndDspF : 62 : 5) = *on;
090600100504             V1Dmsg = $Msg(12);
090700100504             leavesr;
090800100504           endif;
090900100504
091000101209           For  xx = 1  To  5;
091100100504             if  $V1Ctpv(xx) = *blank;
091200100504               iter;
091300100504             endif;
091400100504             clear  TIBS02ds;
091500100504             T02mod = 'C';
091600100504             T02cod = 'TTR';
091700100504             T02ke1 = $V1Ctpv(xx);
091800100504             TNTBE_RicercaControllo  (kpjba : tibs02ds);
091900100504             if  T02err <> *blanks;
092000100504               errMessage  = *on;
092100100504               errGenerico = *on;
092200100504               %subst(IndDspF : 61 + xx : 1) = *on;
092300100504               V1Dmsg = $Msg(13);
092400100504               leavesr;
092500100504             endif;
092600100504           EndFor;
092700101209
092800101209           For  xx = 6  To  %elem($V1Ctpv);
092900101209             if  $V1Ctpv(xx) = *blank;
093000101209               iter;
093100101209             endif;
093200101209             clear  TIBS02ds;
093300101209             T02mod = 'C';
093400101209             T02cod = 'TTR';
093500101209             T02ke1 = $V1Ctpv(xx);
093600101209             TNTBE_RicercaControllo  (kpjba : tibs02ds);
093700101209             if  T02err <> *blanks;
093800101209               errMessage  = *on;
093900101209               errGenerico = *on;
094000101210               %subst(IndDspF : 34 + xx : 1) = *on;
094100101209               V1Dmsg = $Msg(13);
094200101209               leavesr;
094300101209             endif;
094400101209           EndFor;
094500100504
094600100504         EndIf;
094700100504
094800110429         // -?Categoria potenziali abilitate
094900110429         If  dsV1Ccpo <> *blank;
095000110429
095100110429           For  xx = 1  To  4;
095200110429             if  $V1Ccpo(xx) = *blank;
095300110429               iter;
095400110429             endif;
095500110429             IF  %scan('?' : $V1Ccpo(xx)) > 0;
095600110429                 ErrGenerico = *on;
095700110429                 %subst(IndDspF : 45 + xx : 1) = *on;
095800110429                 clear TIBS02ds;
095900110429                 T02mod = 'R';
096000110429                 T02cod = 'CPO';
096100110429                 T02sif = KNSIF;
096200110429                 TNTBE_RicercaControllo  (kpjba : tibs02ds);
096300110429                 IF  T02err = *blanks;
096400110429                     $V1Ccpo(xx) = t02ke1;
096500110429                     errGenerico = *on;
096600110429                     leavesr;
096700110429                 else;
096800110429                     errMessage  = *on;
096900110429                     errGenerico = *on;
097000110429                     V1Dmsg = T02msg;
097100110429                     leavesr;
097200110429                 endif;
097300110429             endif;
097400110429             clear  TIBS02ds;
097500110429             T02mod = 'C';
097600110429             T02cod = 'CPO';
097700110429             T02ke1 = $V1Ccpo(xx);
097800110429             TNTBE_RicercaControllo  (kpjba : tibs02ds);
097900110429             if  T02err <> *blanks;
098000110429               errMessage  = *on;
098100110429               errGenerico = *on;
098200110429               %subst(IndDspF : 45 + xx : 1) = *on;
098300110429               V1Dmsg = $Msg(18);
098400110429               leavesr;
098500110429             endif;
098600110429           EndFor;
098700110429
098800110429         EndIf;
098900091113
099000101209         //?Controllo se causale abilitata solo ad un'altra causale Es la 72 si usa solo con la 71
099100101209           If V1CCae <> *blanks  ;
099200101210             clear  TIBS02ds;
099300101209             T02mod = 'C';
099400101209             T02cod = 'CCO';
099500101209             T02ke1 = V1Ccae;
099600101209             TNTBE_RicercaControllo  (kpjba : tibs02ds);
099700101209             if  T02err <> *blanks;
099800101209               errMessage  = *on;
099900101209               errGenerico = *on;
100000101210               PosCurCae = *on;
100100101209               V1Dmsg = $Msg(17);
100200101210               clear v1dcae ;
100300101209               leavesr;
100400101210             else ;
100500101210               v1dcae = t02uni;
100600101209             endif;
100700101210           endif;
100800090526
100900090526       ENDSR;
101000101209
101100090521       //--------------------------------------------------------------
101200090521       //?Gestione trasmissione aggiornamento                          ?
101300090521       //--------------------------------------------------------------
101400090521       BEGSR  sr_GesW01;
101500090521
101600090521         // Inizializzazione videata
101700090521         if $InzW01 = *on;
101800090521           exsr  sr_InzW01;
101900090521           $InzW01 = *off;
102000090521         endif;
102100090521
102200090521         // Emissione window
102300090521         if  TBXftt = 'S';
102400090525           exfmt  TB75W01;
102500090521           ErrMessage  = *off;
102600090521           ErrGenerico = *off;
102700090521           clear  V1Dmsg;
102800090526         else;
102900090526           dsp_aid = c_F06;
103000090521         endif;
103100090521
103200090521         select;
103300090521
103400090521           // F12=Ritorno (gestito solo se emesso W01)
103500090521           when  dsp_aid = c_F12;
103600090521             $Video = 'D2';
103700090521
103800090521           // F6=Conferma
103900090521           when  dsp_aid = c_F06;
104000090526             exsr  sr_UpdTNTBE;
104100090521             // Ritorno alla videata iniziale
104200090521             if  NOT  ErrGenerico;
104300090521               clear  V1Topz;
104400090521               $Video  = 'D1';
104500090521               $InzD01 = *on;
104600090521             endif;
104700090521         endsl;
104800090521
104900090521       ENDSR;
105000090521
105100090521       //--------------------------------------------------------------
105200090521       //?Inizializzazione videata W01                                 ?
105300090521       //--------------------------------------------------------------
105400090521       BEGSR  sr_InzW01;
105500090521
105600090525         clear TB75W01;
105700090521
105800090521         if  $Immissione;
105900090521
106000090521           // Se immissione
106100090521           W1ftt  = TBXftt;
106200090521
106300090521         else;
106400090521
106500090521           // Se NON immissione:
106600090521           // visualizza i dati relativi all'ultima trasmissione
106700090521           W1ftt  = TBEftt;
106800090521           W1flt  = TBEflt;
106900090521           W1ftr  = TBEftr;
107000090521           if TBEdtr <> *zero;
107100090521             wDate_EUR = %date(TBEdtr : *iso);
107200090521             W1dtr     = %dec(wDate_EUR);
107300090521           endif;
107400090521
107500090521         endif;
107600090521
107700090521       ENDSR;
107800090521
107900090521       //--------------------------------------------------------------
108000090521       //?Aggiornamento records TNTBE00F (tab. "CCO")                  ?
108100090521       //--------------------------------------------------------------
108200090526       BEGSR  sr_UpdTNTBE;
108300090521
108400090521         //?Aggancio record di TNTBE00F?
108500090521         k_TBEcod  = c_Tab;
108600090521         k_TBEke1  = V1Ccam;
108700090521         clear  k_TBEke2;
108800090521         clear  k_TBElin;
108900090521         clear  k_TBEsif;
109000090521         //k_TBElin  = TBXlin;
109100090521         //k_TBEsif  = TBXsif;
109200090521
109300090521         chain  %kds(K05tntbe01) TNTBE000;
109400090526
109500090526         exsr  sr_RieCCO;
109600090526         TBEuni = dCCO;
109700090521
109800090521         //?Aggiornamento tab. "CCO"?
109900090521         SELECT;
110000090521
110100090521           // -?F5=Ripristino?
110200090521           WHEN  $Ripristino;
110300090521             clear  TBEatb;
110400090521             TBEftt = W1ftt;
110500090521             clear  TBEftr;
110600090521             //_______________
110700090521             UPDATE  TNTBE000;
110800090521             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
110900090521
111000090521           // -?F16=Annullamento?
111100090521           WHEN  $Annullamento;
111200090521             TBEatb = 'A';
111300090521             TBEftt = W1ftt;
111400090521             clear  TBEftr;
111500090521             //_______________
111600090521             UPDATE  TNTBE000;
111700090521             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
111800090521
111900090521           // -?Immissione o Modifica?
112000090521           OTHER;
112100090521
112200090521             if  NOT %found(TNTBE01L);
112300090521               clear TNTBE000;
112400090521               TBEcod  = k_TBEcod;
112500090521               TBEke1  = k_TBEke1;
112600090521               TBEke2  = k_TBEke2;
112700090521               TBElin  = k_TBElin;
112800090521               TBEsif  = k_TBEsif;
112900091016               TBEuni  = dCCO;
113000090521             endif;
113100090521
113200090521             TBEapl = TBXapl;
113300090521             clear  TBEatb;
113400090521             TBEftt = W1ftt;
113500090521             clear  TBEflt;
113600090521             clear  TBEftr;
113700090521             //clear TBEdtr;
113800090521
113900090521             // - Aggiornamento record
114000090521             if  %found(TNTBE01L);
114100090521               //_____________
114200090521               UPDATE TNTBE000;
114300090521               //¯¯¯¯¯¯¯¯¯¯¯¯¯
114400090521             else;
114500090521               //_____________
114600090521               WRITE  TNTBE000;
114700090521               //¯¯¯¯¯¯¯¯¯¯¯¯¯
114800090521             endif;
114900090521
115000090521         ENDSL;
115100090521
115200090521       ENDSR;
115300090521
115400090521       //--------------------------------------------------------------
115500090521       //?Caricamento dati in tab. "CCO"                               ?
115600090521       //--------------------------------------------------------------
115700090521       BEGSR  sr_RieCCO;
115800090521
115900090521         clear  dCCO;
116000090521
116100090521         §CCOdes = V1Cdes;
116200100126         §CCOord = V1Cord;
116300090521         §CCOgio = V1Cgio;
116400100126         §CCOtat = V1Ctat;
116500100126         §CCOatt = V1Catt;
116600100924         §CCOupm = V1Cupm;
116700100126         §CCOuti = V1Cuti;
116800100126         §CCOesi = V1Cesi;
116900100126         §CCOeaf = V1Ceaf;
117000100126         %subst(§CCOopz:1:1) = V1Copz1;
117100100126         %subst(§CCOopz:2:1) = V1Copz2;
117200100126         %subst(§CCOopz:3:1) = V1Copz3;
117300100126         %subst(§CCOopz:4:1) = V1Copz4;
117400100126         %subst(§CCOopz:5:1) = V1Copz5;
117500100126         §CCOnot = V1Cnot;
117600100126         §CCOsta = V1Csta;
117700091113         §CCOtra = V1Ctra;
117800091113         §CCOapt = V1Capt;
117900100126         §CCOeso = V1Ceso;
118000100219         %subst(§CCOtaa:1:1) = V1Caa1;
118100100219         %subst(§CCOtaa:2:1) = V1Caa2;
118200100219         %subst(§CCOtaa:3:1) = V1Caa3;
118300100219         %subst(§CCOtaa:4:1) = V1Caa4;
118400100219         %subst(§CCOtaa:5:1) = V1Caa5;
118500100219         %subst(§CCOtaa:6:1) = V1Caa6;
118600100219         %subst(§CCOtaa:7:1) = V1Caa7;
118700100219         %subst(§CCOtaa:8:1) = V1Caa8;
118800100219         %subst(§CCOtaa:9:1) = V1Caa9;
118900100219         %subst(§CCOtaa:10:1) = V1Ca10;
119000100219         §CCOoff = V1Coff;
119100100305         §CCOcht = V1Ccht;
119200110429         §CCOcod = Savcod;
119300100305         §CCOres = V1Cres;
119400100305         §CCOcoi = V1Ccom;
119500100504         §CCOtpv = dsV1Ctpv;
119600110429         §CCOcpo = dsV1Ccpo;
119700101210         §CCOhot = V1Chot;
119800101210         §CCOcae = V1Ccae;
119900101210         §CCOann = V1Cann;
120000101210         §CCOblc = V1Cblc;
120100101214         §CCOccp = V1Cccp;
120200110211         §CCOstl = V1Cstl;
120300110211         §CCOpgmblc = V1Cpgmblc;
120400110405         §CCOcns = V1Ccns;
120500110405         §CCOucns = V1Cucns;
120600120622         §CCOdpc = V1Cdpc;
120700090521
120800090521       ENDSR;
120900090521
121000090521       //--------------------------------------------------------------
121100090521       //?Operazioni finali.                                           ?
121200090521       //--------------------------------------------------------------
121300090521       BEGSR  sr_RoutEnd;
121400090521
121500090521         //?Chiusura funzioni precedentemente aperte?
121600090521         clear  TIBS02ds;
121700090521         T02tla = 'C';
121800090521         TNTBE_RicercaControllo ( kpjba : tibs02ds );
121900090521
122000090521         //?Chiusura pgm?
122100090521         return;
122200090521
122300090521       ENDSR;
122400090521
122500090521      /end-free
122600090521
122700090521       //--------------------------------------------------------------
122800090521       //?Definizione schiere a tempo di compilazione                  ?
122900090521       //--------------------------------------------------------------
123000090521
123100090521** > $Msg -------------------------------------------------------------------*
123200091113Immettere la Causale Contatto Marketing                                         1
123300090521Descrizione obbligatoria                                                        2
123400090521Giorni proposti per prossimo contatto NON possono essere negativi               3
123500090525Valore NON previsto.  Unici valori previsti: _=No, L=Sì                         4
123600100127Opzione errata                                                                  5
123700121018...........                                                                     6
123800121018...........                                                                     7
123900121018...........                                                                     8
124000100126Tipo attività associata errata                                                  9
124100090629Tipo contatto futuro errato                                                     10
124200091113Se causale crea Trattativa non può non essere legata ad una Trattativa          11
124300100504Richiesto "Causale legata a Trattativa" = "S" per indicare "Tipi Trattativa"    12
124400100504Tipo trattativa errato                                                          13
124500100924Impostare giorni prossimo contatto a zero se segno =                            14
124600100924Impostare giorni prossimo contatto > di zero se segno + o -                     15
124700100924Se impostati i giorni prossimo contatto obbligatorio il segno + o -             16
124800101209Causale errata                                                                  17
124900110429Categoria Potenziale errata                                                     18
