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
015100090525       // - Tab. 1Q = Stato clienti potenziali
015200090525     d ds1Q          e ds                  inz
015300090521
015400090521       // - Parametri per Interrogazione/Selezione tab. "CCO"
015500090521     d TNTB74ds      e ds                  inz
015600090521
015700090521       // - Dati record principale della tabella
015800090521     d TNTBEds       e ds                  inz  extname(TNTBE00F)
015900090521     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
016000090521     d                                          prefix(TBX:3)
016100090521
016200100219       // - Ridefinizione dati a video
016300100219       // - - Tipi Attività abilitati
016400100219     d dsV1CTAA        ds            10    inz
016500100219     d  V1Caa1                             inz
016600100219     d  V1Caa2                             inz
016700100219     d  V1Caa3                             inz
016800100219     d  V1Caa4                             inz
016900100219     d  V1Caa5                             inz
017000100219     d  V1Caa6                             inz
017100100219     d  V1Caa7                             inz
017200100219     d  V1Caa8                             inz
017300100219     d  V1Caa9                             inz
017400100219     d  V1Ca10                             inz
017500100504       // - - Tipi Trattativa
017600101209     d dsV1CTPV        ds            10    inz
017700100504     d  V1Ctpv1                            inz
017800100504     d  V1Ctpv2                            inz
017900100504     d  V1Ctpv3                            inz
018000100504     d  V1Ctpv4                            inz
018100100504     d  V1Ctpv5                            inz
018200101209     d  V1Ctpv6                            inz
018300101209     d  V1Ctpv7                            inz
018400101209     d  V1Ctpv8                            inz
018500101209     d  V1Ctpv9                            inz
018600101209     d  V1Ctpv0                            inz
018700101209     d   $V1Ctpv               1     10    inz  dim(10)
018800100219
018900110429       // - - Categoria Potenziale abilitate
019000110429     d dsV1Ccpo        ds             4    inz
019100110429     d  V1Ccp1                             inz
019200110429     d  V1Ccp2                             inz
019300110429     d  V1Ccp3                             inz
019400110429     d  V1Ccp4                             inz
019500110429     d   $V1Ccpo               1      4    inz  dim(4)
019600090521
019700090521       //--------------------------------------------------------------
019800090521       //?Definizione variabili globali.                               ?
019900090521       //--------------------------------------------------------------
020000090521
020100090521       // - Flags booleani
020200090521     d $Fine           s               n   inz
020300090521     d $InzD01         s               n   inz(*on)
020400090521     d $InzD02         s               n   inz(*on)
020500101209     d $InzD03         s               n   inz(*on)
020600090521     d $InzW01         s               n   inz(*on)
020700090521     d $Annullamento   s               n   inz
020800090521     d $Immissione     s               n   inz
020900090521     d $Ripristino     s               n   inz
021000090521
021100090521       // - Indici di schiera
021200090521     d xx              s              3  0 inz
021300090521
021400090521       // - Variabili per la gestione del video
021500090521     d $Video          s              2    inz('D1')
021600090525
021700090525       // - Flag di errore
021800090525     d $Err            s              1    inz(*off)
021900090521
022000090521       // - Campi di comodo
022100090521     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
022200110429     d savcod          s              1
022300090521
022400090525       // - Parametri per pgm interrogazione tabelle
022500090525      /copy gaitrasrc/srcProtoPI,X§TABER
022600090521
022700090521       //--------------------------------------------------------------
022800090521       //?Definizione prototipi procedure.                             ?
022900090521       //--------------------------------------------------------------
023000090521
023100090521       // - Reperimento dati utente
023200090521      /copy gaitrasrc/srcProtoPR,TIBS34R
023300090521
023400090525       // - Ricerca vecchie tabelle
023500090525      /copy gaitrasrc/srcProtoPR,X§TABER
023600090525
023700090525       // - Ricerca e controllo nuove tabelle
023800090521      /copy gaitrasrc/srcProtoPR,TIBS02R
023900090521
024000090525       // - Selezione singola causale attività
024100090521     d tntb74r         pr                  extpgm('TNTB74R')
024200090525     d   kpjba                             likeds(KPJBA)
024300090521     d   tntb74ds                          likeds(TNTB74ds)
024400090521
024500090521       //--------------------------------------------------------------
024600090521       //?Definizione key-list.                                        ?
024700090521       //--------------------------------------------------------------
024800090521
024900090525       // - File TABEL00F
025000090525     d k03tabel00    e ds                  extname(TABEL00F : *key)
025100090525     d                                     prefix(k_)   inz
025200090525
025300090521       // - File TNTBE01L
025400090521     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
025500090521     d                                     prefix(k_)   inz
025600090521
025700090521       //--------------------------------------------------------------
025800090521       //?Riepilogo indicatori utilizzati.                             ?
025900090521       //--------------------------------------------------------------
026000090521       //--------------------------------------------------------------
026100090521
026200090521       //--------------------------------------------------------------
026300090521       //?M A I N - L I N E                                            ?
026400090521       //--------------------------------------------------------------
026500090521
026600090521     c     *Entry        plist
026700090521     c                   parm                    KPJBA
026800090521
026900090521      /free
027000090521
027100090521       //?Operazioni iniziali?
027200090521       exsr sr_RoutInz;
027300090521
027400090521       //?Gestione video?
027500090521       DOW  $Fine = *off;
027600090521         select;
027700090521           // - Manutenzione singola tabella
027800090521           when $Video = 'D2';
027900090521             exsr sr_GesD02;
028000090526           when $Video = 'D3';
028100090526             exsr sr_GesD03;
028200090521           // - Filtro di lancio
028300090521           when $Video = 'D1';
028400090521             exsr sr_GesD01;
028500090521           // - Richiesta dati per trasmissione record
028600090521           when $Video = 'W1';
028700090521             exsr sr_GesW01;
028800090521           other;
028900090521             $Fine = *on;
029000090521         endsl;
029100090521       ENDDO;
029200090521
029300090521       //?Operazioni finali?
029400090521       exsr sr_RoutEnd;
029500090521
029600090521       //--------------------------------------------------------------
029700090521       //?Operazioni iniziali.                                         ?
029800090521       //--------------------------------------------------------------
029900090521       BEGSR  sr_RoutInz;
030000090521
030100090521         *inLR = *on;
030200090521
030300090521         //?Reperimento dati job?
030400090521         exsr  sr_DatiJob;
030500090521
030600090521         //?Impostazione nome programma a video?
030700090521         V1Tpgm = SDSpgm;
030800090521
030900090521         //?Aggancio dati generali della tabella in esame?
031000090521         clear  k_TBEcod;
031100090521         k_TBEke1 = *zero;
031200090521         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
031300090521                = c_Tab;
031400090521         clear  k_TBEke2;
031500090521         clear  k_TBElin;
031600090521         k_TBEsif = KNSIF;
031700090521         chain(n)  %kds(k05tntbe01)  TNTBE000;
031800090521         if  not  %found(TNTBE01L);
031900090521           clear  k_TBEsif;
032000090521           chain(n)  %kds(k05tntbe01)  TNTBE000;
032100090521         endif;
032200090521         if  %found(TNTBE01L);
032300090521           xTNTBEds = TNTBEds;
032400090521         else;
032500090521           clear  xTNTBEds;
032600090521         endif;
032700090521
032800090521         //?Impostazione iniziale / pulizia dei campi chiave?
032900090521         k_TBEcod = c_Tab;
033000090521         clear  k_TBEke1;
033100090521         clear  k_TBEke2;
033200090521         clear  k_TBElin;
033300090521         clear  k_TBEsif;
033400090521
033500090521         //?1ª inizializzazione della videata D01?
033600090521         exsr  sr_InzD01;
033700090521         V1Ccam  = '?';
033800090521         $InzD01 = *off;
033900090521
034000090521       ENDSR;
034100090521
034200090521       //--------------------------------------------------------------
034300090521       //?Reperimento Dati del job (Utente/Operativi).                 ?
034400090521       //--------------------------------------------------------------
034500090521       BEGSR  sr_DatiJob;
034600090521
034700090521         in(E) §AzUte;
034800090521         if NOT %error;
034900090521           in(E) §DatiUte;
035000090521         endif;
035100090521         if %error or RSut = *blanks;
035200090521           clear TIBS34ds;
035300090521           tibs34r ( tibs34ds );
035400090521           in §AzUte;
035500090521           in §DatiUte;
035600090521         endif;
035700090521
035800090521       ENDSR;
035900090521
036000090521       //--------------------------------------------------------------
036100090521       //?Gestione videata D01                                         ?
036200090521       //--------------------------------------------------------------
036300090521       BEGSR  sr_GesD01;
036400090521
036500090521         //?Inizializzazione videata?
036600090521         if  $InzD01 = *on;
036700090521           exsr  sr_InzD01;
036800090521           $InzD01 = *off;
036900090521         endif;
037000090521
037100090521         //?Emissione videata?
037200090521         write  TB75T01;
037300090521         write  TB75P01;
037400090521         exfmt  TB75D01;
037500090521
037600090521         reset  ErrMessage;
037700090521         reset  ErrGenerico;
037800090521         clear  V1Dmsg;
037900090521
038000090521         SELECT;
038100090521
038200090521           //?F3=Fine?
038300090521           WHEN  dsp_aid = c_F03;
038400090521             $Fine = *on;
038500090521
038600090521           //?Invio?
038700090521           OTHER;
038800090521             exsr sr_CtrD01;
038900090521             if  ErrGenerico = *off;
039000090521               $Video    = 'D2';
039100090521               $InzD02   = *on;
039200090521             endif;
039300090521
039400090521         ENDSL;
039500090521
039600090521       ENDSR;
039700090521
039800090521       //--------------------------------------------------------------
039900090521       //?Inizializzazione videata D01                                 ?
040000090521       //--------------------------------------------------------------
040100090521       BEGSR  sr_InzD01;
040200090521
040300090525         clear  TB75D01;
040400090521
040500090521       ENDSR;
040600090521
040700090521       //--------------------------------------------------------------
040800090521       //?Controllo dati immessi nella videata D01                     ?
040900090521       //--------------------------------------------------------------
041000090521       BEGSR  sr_CtrD01;
041100090521
041200090521         %subst(IndDspF : 28) = *off;
041300090521
041400090521         SELECT;
041500090521
041600090521           //?Ricerca codice Causale Attività?
041700090521           WHEN  %scan('?' : V1Ccam) > *zeros;
041800090521             clear  TNTB74ds;
041900101201             kpjbu = 'TNTB75R' ;
042000090521             tntb74r ( kpjba : tntb74ds );
042100090521             select;
042200090521               when  oCCOfxx <> *blank;
042300090521               when  oCCOerr = *on;
042400090521                 ErrMessage  = *on;
042500090521                 PosCurCAM   = *on;
042600090521                 V1Dmsg = oCCOmsg;
042700090521               other;
042800090521                 V1Ccam = %subst(oCCOke1 : 1 : %len(V1Ccam));
042900090521             endsl;
043000090521             ErrGenerico = *on;
043100090521             leavesr;
043200090521
043300090521           //?Controllo immissione codice causale?
043400090521           WHEN  V1Ccam = *blank;
043500090521             ErrMessage  = *on;
043600090521             ErrGenerico = *on;
043700090521             PosCurCAM   = *on;
043800090521             V1Dmsg = $Msg(01);
043900090521             leavesr;
044000090521
044100090521         ENDSL;
044200090521
044300090521       ENDSR;
044400090521
044500090521       //--------------------------------------------------------------
044600090521       //?Gestione videata D02                                         ?
044700090521       //--------------------------------------------------------------
044800090521       BEGSR  sr_GesD02;
044900090521
045000090521         //?Inizializzazione videata?
045100090521         if  $InzD02 = *on;
045200090521           exsr sr_InzD02;
045300090521           $InzD02 = *off;
045400090521         endif;
045500090521
045600090521         //?Emissione Testata e Piede con tasti funzionali abilitati?
045700090521         if  errGenerico = *off;
045800090521           write  TB75T01;
045900090521           write  TB75D01;
046000090521           write  Protect;
046100090521           write  TB75P02;
046200090521         endif;
046300090521
046400090521         //?Emissione videata?
046500090521         exfmt  TB75D02;
046600090521
046700090521         reset  ErrMessage;
046800090521         reset  ErrGenerico;
046900090521         clear  V1Dmsg;
047000090521         reset  $Annullamento;
047100090521
047200090521         SELECT;
047300090521
047400090521           //?F3=Fine?
047500090521           WHEN  dsp_aid = c_F03;
047600090521             unlock TNTBE01L;
047700090521             $Fine = *on;
047800090521
047900090521           //?F12=Ritorno?
048000090521           WHEN  dsp_aid = c_F12;
048100090521             unlock TNTBE01L;
048200090521             reset  $Video;
048300090521             clear  V1Topz;
048400090526
048500090526           //?RollUp=Videata successiva?
048600090526           WHEN  dsp_aid = c_RollUp;
048700090526             $Video  = 'D3';
048800090521
048900090521
049000090521           //?Enter; F5=Ripristino; F6=Conferma; F16=Annullamento?
049100090521           OTHER;
049200090521             $Ripristino   = (dsp_aid = c_F05);
049300090521             $Annullamento = (dsp_aid = c_F16);
049400090521             if  Not $Annullamento;
049500090521               exsr  sr_CtrD02;
049600090526               if  ErrGenerico;
049700090526                 leavesr;
049800090526               endif;
049900090526               exsr  sr_CtrD03;
050000090526               if  ErrGenerico;
050100090526                 $Video  = 'D3';
050200090526                 leavesr;
050300090526               endif;
050400090521             endif;
050500090526             select;
050600101210               when  dsp_aid = c_Enter  ;
050700090526                 $Video  = 'D3';
050800090526               when  dsp_aid = c_F05  or
050900090526                     dsp_aid = c_F06  or
051000090526                     dsp_aid = c_F16;
051100090526                 $Video  = 'W1';
051200090526                 reset  $InzW01;
051300090526             endsl;
051400090521
051500090521         ENDSL;
051600090521
051700090521       ENDSR;
051800090521
051900090521       //--------------------------------------------------------------
052000090521       //?Inizializzazione videata D02                                 ?
052100090521       //--------------------------------------------------------------
052200090521       BEGSR  sr_InzD02;
052300090521
052400090521         //?Spegnimento di TUTTI gli indicatori?
052500090521         IndDspF     = *off;
052600090521
052700090521         //?Pulizia videata?
052800090521         clear  TB75D02;
052900090526         clear  TB75D03;
053000090521
053100090521         //?Reperimento dati della causale contatto marketing?
053200090521         reset  $Immissione;
053300090521         reset  $Ripristino;
053400090521         k_TBEke1 = V1Ccam;
053500090521         chain  %kds(k05tntbe01)  TNTBE000;
053600090521         select;
053700090521           when  Not %found(TNTBE01L);
053800090521             $Immissione = *on;
053900090521           when  TBEatb <> *blank;
054000090521             $Ripristino = *on;
054100090521         endsl;
054200090521
054300090521         //?Caricamento dati della causale contatto marketing?
054400090521         if  %found(TNTBE01L);
054500090521           exsr  sr_RieD02;
054600090525         endif;
054700090521
054800090521         //?Impostazione tipo manutenzione?
054900090521         select;
055000090521           when  $Immissione;
055100090521             V1Topz = 'IMMISSIONE';
055200090521           when  $Ripristino;
055300090521             V1Topz = 'RIPRISTINO';
055400090521           other;
055500090521             V1Topz = ' MODIFICA ';
055600090521         endsl;
055700090521
055800090521         //?Impostazione indicatori per abilitazione tasti funzionali?
055900090521         exsr  Set_Ind_D02;
056000090521
056100090521       ENDSR;
056200090521
056300090521       //--------------------------------------------------------------
056400090521       //?Caricamento singola riga di testo nella videata D02          ?
056500090521       //--------------------------------------------------------------
056600090521       BEGSR  sr_RieD02;
056700090521
056800090521         dCCO = TBEuni;
056900090521
057000090521         V1Cdes   = §CCOdes;
057100100126         V1Cord   = §CCOord;
057200090521         V1Cgio   = §CCOgio;
057300100126         V1Ctat   = §CCOtat;
057400100126         V1Catt   = §CCOatt;
057500110429         dsV1Ccpo = §CCOcpo;
057600100924         V1Cupm   = §CCOupm;
057700100126         V1Cuti   = §CCOuti;
057800100126         V1Cesi   = §CCOesi;
057900100126         V1Ceaf   = §CCOeaf;
058000100126         V1Copz1  = %subst(§CCOopz:1:1);
058100100126         V1Copz2  = %subst(§CCOopz:2:1);
058200100126         V1Copz3  = %subst(§CCOopz:3:1);
058300100126         V1Copz4  = %subst(§CCOopz:4:1);
058400100126         V1Copz5  = %subst(§CCOopz:5:1);
058500100219         V1Caa1   = %subst(§CCOtaa:1:1);
058600100219         V1Caa2   = %subst(§CCOtaa:2:1);
058700100219         V1Caa3   = %subst(§CCOtaa:3:1);
058800100219         V1Caa4   = %subst(§CCOtaa:4:1);
058900100219         V1Caa5   = %subst(§CCOtaa:5:1);
059000100219         V1Caa6   = %subst(§CCOtaa:6:1);
059100100219         V1Caa7   = %subst(§CCOtaa:7:1);
059200100219         V1Caa8   = %subst(§CCOtaa:8:1);
059300100219         V1Caa9   = %subst(§CCOtaa:9:1);
059400100219         V1Ca10   = %subst(§CCOtaa:10:1);
059500100126         V1Cnot   = §CCOnot;
059600100126         V1Csta   = §CCOsta;
059700100219         V1Coff   = §CCOoff;
059800100305         V1Ccht   = §CCOcht;
059900110429         Savcod   = §CCOcod;
060000100305         V1Cres   = §CCOres;
060100100305         V1Ccom   = §CCOcoi;
060200120622         V1Cdpc   = §CCOdpc;
060300100126
060400101209         // imposto dati video 3
060500100504         dsV1Ctpv = §CCOtpv;
060600100126         V1Capt   = §CCOapt;
060700100126         V1Ctra   = §CCOtra;
060800100126         V1Ceso   = §CCOeso;
060900101209         V1Chot   = §CCOhot;
061000101209         V1Ccae   = §CCOcae;
061100101209         V1Cann   = §CCOann;
061200101209         V1Cblc   = §CCOblc;
061300101214         V1Cccp   = §CCOccp;
061400110211         V1Cstl   = §CCOstl;
061500110211         V1Cpgmblc = §CCOpgmblc;
061600110405         V1Ccns   = §CCOcns;
061700110405         V1Cucns  = §CCOucns;
061800090526
061900090526         // Decodifiche
062000090526         exsr  sr_CtrD02;
062100090526         exsr  sr_CtrD03;
062200090526         clear  ErrMessage;
062300090526         clear  ErrGenerico;
062400090526         clear  V1Dmsg;
062500090521
062600090521       ENDSR;
062700090521
062800090521       //--------------------------------------------------------------
062900090521       //?Settaggio indicatori nella videata D02 per abilitazione      ?
063000090521       //?  tasti funzionali (sul subfile control)                     ?
063100090521       //--------------------------------------------------------------
063200090521       BEGSR Set_Ind_D02;
063300090521
063400090521         //?F5=Ripristino?
063500090521         F5Attivo  = (NOT $Immissione  and  $Ripristino);
063600090521
063700090521         //?F6=Conferma?
063800090521         F6Attivo  = (NOT F5Attivo);
063900090521
064000090521         //?F16=Annullamento?
064100090521         F16Attivo = (NOT $Immissione  and  NOT $Ripristino);
064200090521
064300090521       ENDSR;
064400090521
064500090521       //--------------------------------------------------------------
064600090521       //?Controllo dati videata D02                                   ?
064700090521       //--------------------------------------------------------------
064800090521       BEGSR  sr_CtrD02;
064900090521
065000101209         %subst(IndDspF : 40) = *off;
065100090521
065200090521         select;
065300090521
065400090521           //?Descrizione obbligatoria?
065500090521           when  V1Cdes = *blank;
065600090521             ErrMessage  = *on;
065700090521             ErrGenerico = *on;
065800090521             PosCurDES   = *on;
065900090521             V1Dmsg = $Msg(02);
066000090521             leavesr;
066100090629
066200090521           //?Giorni proposti per prossimo contatto negativi?
066300090521           when  V1Cgio < *zero;
066400090521             ErrMessage  = *on;
066500090521             ErrGenerico = *on;
066600090521             PosCurGIO   = *on;
066700090521             V1Dmsg = $Msg(03);
066800090521             leavesr;
066900090521
067000090521         endsl;
067100090521
067200100504           //?Tipo contatto futuro obbligatorio?
067300090629           If    %scan('?' : V1Ctat) > *zero;
067400090629             errGenerico = *on;
067500090629             PosCurTat   = *on;
067600090629             clear  TIBS02ds;
067700090629             T02mod = 'R';
067800090629             T02cod = 'TTA';
067900090629             T02sif = KNSIF;
068000090629             TNTBE_RicercaControllo  (kpjba : tibs02ds);
068100090629             if  T02err = *blanks;
068200090629               v1ctat = T02ke1;
068300090629               errGenerico = *on;
068400090629               leavesr;
068500090629             else;
068600090629               errMessage  = *on;
068700090629               errGenerico = *on;
068800090629               V1Dmsg = T02msg;
068900090629               leavesr;
069000090629             endif;
069100090629           endif;
069200090629
069300090629         // Controllo tipo contatto
069400090629           If    v1ctat <> *blanks ;
069500090629                 clear  TIBS02ds;
069600090629                 T02mod = 'C';
069700090629                 T02cod = 'TTA';
069800090629                 T02ke1 = v1ctat;
069900090629                 T02sif = KNSIF;
070000090629                 TNTBE_RicercaControllo  (kpjba : tibs02ds);
070100090629                 if  T02err <> *blanks;
070200090629                     errMessage  = *on;
070300090629                     errGenerico = *on;
070400090629                     PosCurTat   = *on;
070500090629                     V1Dmsg = $Msg(10);
070600090629                     leavesr;
070700090629                 endif;
070800090629           else ;
070900100924        // se segno operazione <> da blanks obbligatorio l'inserimento
071000100924                 if v1cupm <> ' ' ;
071100090630                    errMessage  = *on;
071200090630                    errGenerico = *on;
071300090630                    PosCurTat   = *on;
071400090630                    V1Dmsg = $Msg(10);
071500090630                    leavesr;
071600090630                 endif;
071700090629           endif;
071800090525
071900100126
072000100127           //?Tipo attività associata?
072100100126           If    %scan('?' : V1Catt) > *zero;
072200100126             errGenerico = *on;
072300100126             PosCurATT   = *on;
072400100126             clear  TIBS02ds;
072500100126             T02mod = 'R';
072600100126             T02cod = 'TTA';
072700100126             T02sif = KNSIF;
072800100126             TNTBE_RicercaControllo  (kpjba : tibs02ds);
072900100126             if  T02err <> *blanks;
073000100126               errMessage  = *on;
073100100126               errGenerico = *on;
073200100126               V1Dmsg = T02msg;
073300100126               leavesr;
073400100126             endif;
073500100126               v1catt = T02ke1;
073600100126               errGenerico = *on;
073700100126               leavesr;
073800100126           endif;
073900100126
074000100126         // Controllo tipo attività
074100100126           If    v1catt <> *blanks ;
074200100126                 clear  TIBS02ds;
074300100126                 T02mod = 'C';
074400100126                 T02cod = 'TTA';
074500100126                 T02ke1 = v1catt;
074600100126                 T02sif = KNSIF;
074700100126                 TNTBE_RicercaControllo  (kpjba : tibs02ds);
074800100126                 if  T02err <> *blanks;
074900100126                     errMessage  = *on;
075000100126                     errGenerico = *on;
075100100126                     PosCurATT   = *on;
075200100126                     V1Dmsg = $Msg(09);
075300100126                     leavesr;
075400100126                 endif;
075500100126           endif;
075600100126
075700090526
075800100924        //?Segno operazione da fare alla data della prossima attività
075900100924           if    V1Cupm = '=' and V1CGio <> 0 ;
076000090525             errMessage  = *on;
076100090525             errGenerico = *on;
076200100924             PosCurUPM   = *on;
076300100924             V1Dmsg = $Msg(14);
076400090525             leavesr;
076500100126           endif;
076600100127
076700100924           if   (V1Cupm = '+' or V1Cupm = '-') and V1CGio = 0 ;
076800100924             errMessage  = *on;
076900100924             errGenerico = *on;
077000100924             PosCurUPM   = *on;
077100100924             V1Dmsg = $Msg(15);
077200100924             leavesr;
077300100924           endif;
077400100924
077500100924           if    V1Cupm = ' ' and V1CGio <> 0 ;
077600100924             errMessage  = *on;
077700100924             errGenerico = *on;
077800100924             PosCurUPM   = *on;
077900100924             V1Dmsg = $Msg(16);
078000100924             leavesr;
078100100924           endif;
078200100127
078300100127        //?Opzioni inibite?
078400100127           SELECT;
078500100127
078600100127           WHEN  %check($_opz:V1Copz1) > 0;
078700100127             errMessage  = *on;
078800100127             errGenerico = *on;
078900100127             PosCurOPZ1  = *on;
079000100127             V1Dmsg = $Msg(05);
079100100127             leavesr;
079200100127
079300100127           WHEN  %check($_opz:V1Copz2) > 0;
079400100127             errMessage  = *on;
079500100127             errGenerico = *on;
079600100127             PosCurOPZ2  = *on;
079700100127             V1Dmsg = $Msg(05);
079800100127             leavesr;
079900100127
080000100127           WHEN  %check($_opz:V1Copz3) > 0;
080100100127             errMessage  = *on;
080200100127             errGenerico = *on;
080300100127             PosCurOPZ3  = *on;
080400100127             V1Dmsg = $Msg(05);
080500100127             leavesr;
080600100127
080700100127           WHEN  %check($_opz:V1Copz4) > 0;
080800100127             errMessage  = *on;
080900100127             errGenerico = *on;
081000100127             PosCurOPZ4  = *on;
081100100127             V1Dmsg = $Msg(05);
081200100127             leavesr;
081300100127
081400100127           WHEN  %check($_opz:V1Copz5) > 0;
081500100127             errMessage  = *on;
081600100127             errGenerico = *on;
081700100127             PosCurOPZ5  = *on;
081800100127             V1Dmsg = $Msg(05);
081900100127             leavesr;
082000100127           ENDSL;
082100090526
082200090526
082300090526       ENDSR;
082400090526
082500090526       //--------------------------------------------------------------
082600090526       //?Gestione videata D03                                         ?
082700090526       //--------------------------------------------------------------
082800090526       BEGSR  sr_GesD03;
082900090526
083000090526         //?Inizializzazione videata?
083100090526         if  $InzD02 = *on;
083200090526           exsr sr_InzD02;
083300090526           $InzD02 = *off;
083400090526         endif;
083500090526
083600090526         //?Emissione Testata e Piede con tasti funzionali abilitati?
083700090526         if  errGenerico = *off;
083800090526           write  TB75T01;
083900090526           write  TB75D01;
084000090526           write  TB75D02;
084100090526           write  Protect;
084200090526           write  TB75P02;
084300090526         endif;
084400090526
084500090526         //?Emissione videata?
084600090526         exfmt  TB75D03;
084700090526
084800090526         reset  ErrMessage;
084900090526         reset  ErrGenerico;
085000090526         clear  V1Dmsg;
085100090526         reset  $Annullamento;
085200090526
085300090526         SELECT;
085400090526
085500090526           //?F3=Fine?
085600090526           WHEN  dsp_aid = c_F03;
085700090526             unlock TNTBE01L;
085800090526             $Fine = *on;
085900090526
086000090526           //?F12=Ritorno?
086100090526           WHEN  dsp_aid = c_F12;
086200090526             $Video  = 'D2';
086300090526
086400090526           //?RollDown=Videata precedente?
086500090526           WHEN  dsp_aid = c_RollDown;
086600090526             $Video  = 'D2';
086700090526
086800090526
086900090526           //?Enter; F5=Ripristino; F6=Conferma; F16=Annullamento?
087000090526           OTHER;
087100090526             $Ripristino   = (dsp_aid = c_F05);
087200090526             $Annullamento = (dsp_aid = c_F16);
087300090526             if  Not $Annullamento;
087400090526               exsr  sr_CtrD03;
087500090526               if  ErrGenerico;
087600090526                 leavesr;
087700090526               endif;
087800090526               exsr  sr_CtrD02;
087900090526               if  ErrGenerico;
088000090526                 $Video  = 'D2';
088100090526                 leavesr;
088200090526               endif;
088300090526             endif;
088400121018               IF    dsp_aid = c_F05  or
088500101210                     dsp_aid = c_F06  or
088600101210                     dsp_aid = c_F16;
088700090526               $Video  = 'W1';
088800090526               reset  $InzW01;
088900121018             endif;
089000090526
089100090526         ENDSL;
089200090526
089300090526       ENDSR;
089400090526
089500090526       //--------------------------------------------------------------
089600090526       //?Controllo dati videata D03                                   ?
089700090526       //--------------------------------------------------------------
089800090526       BEGSR  sr_CtrD03;
089900090526
090000101209         %subst(IndDspF : 40) = *off;
090100100504
090200100504         // -?Tipi trattativa?
090300100504         If  dsV1Ctpv <> *blank;
090400100504
090500100504           if  V1Ctra <> 'S';
090600100504             errMessage  = *on;
090700100504             errGenerico = *on;
090800100504             %subst(IndDspF : 62 : 5) = *on;
090900100504             V1Dmsg = $Msg(12);
091000100504             leavesr;
091100100504           endif;
091200100504
091300101209           For  xx = 1  To  5;
091400100504             if  $V1Ctpv(xx) = *blank;
091500100504               iter;
091600100504             endif;
091700100504             clear  TIBS02ds;
091800100504             T02mod = 'C';
091900100504             T02cod = 'TTR';
092000100504             T02ke1 = $V1Ctpv(xx);
092100100504             TNTBE_RicercaControllo  (kpjba : tibs02ds);
092200100504             if  T02err <> *blanks;
092300100504               errMessage  = *on;
092400100504               errGenerico = *on;
092500100504               %subst(IndDspF : 61 + xx : 1) = *on;
092600100504               V1Dmsg = $Msg(13);
092700100504               leavesr;
092800100504             endif;
092900100504           EndFor;
093000101209
093100101209           For  xx = 6  To  %elem($V1Ctpv);
093200101209             if  $V1Ctpv(xx) = *blank;
093300101209               iter;
093400101209             endif;
093500101209             clear  TIBS02ds;
093600101209             T02mod = 'C';
093700101209             T02cod = 'TTR';
093800101209             T02ke1 = $V1Ctpv(xx);
093900101209             TNTBE_RicercaControllo  (kpjba : tibs02ds);
094000101209             if  T02err <> *blanks;
094100101209               errMessage  = *on;
094200101209               errGenerico = *on;
094300101210               %subst(IndDspF : 34 + xx : 1) = *on;
094400101209               V1Dmsg = $Msg(13);
094500101209               leavesr;
094600101209             endif;
094700101209           EndFor;
094800100504
094900100504         EndIf;
095000100504
095100110429         // -?Categoria potenziali abilitate
095200110429         If  dsV1Ccpo <> *blank;
095300110429
095400110429           For  xx = 1  To  4;
095500110429             if  $V1Ccpo(xx) = *blank;
095600110429               iter;
095700110429             endif;
095800110429             IF  %scan('?' : $V1Ccpo(xx)) > 0;
095900110429                 ErrGenerico = *on;
096000110429                 %subst(IndDspF : 45 + xx : 1) = *on;
096100110429                 clear TIBS02ds;
096200110429                 T02mod = 'R';
096300110429                 T02cod = 'CPO';
096400110429                 T02sif = KNSIF;
096500110429                 TNTBE_RicercaControllo  (kpjba : tibs02ds);
096600110429                 IF  T02err = *blanks;
096700110429                     $V1Ccpo(xx) = t02ke1;
096800110429                     errGenerico = *on;
096900110429                     leavesr;
097000110429                 else;
097100110429                     errMessage  = *on;
097200110429                     errGenerico = *on;
097300110429                     V1Dmsg = T02msg;
097400110429                     leavesr;
097500110429                 endif;
097600110429             endif;
097700110429             clear  TIBS02ds;
097800110429             T02mod = 'C';
097900110429             T02cod = 'CPO';
098000110429             T02ke1 = $V1Ccpo(xx);
098100110429             TNTBE_RicercaControllo  (kpjba : tibs02ds);
098200110429             if  T02err <> *blanks;
098300110429               errMessage  = *on;
098400110429               errGenerico = *on;
098500110429               %subst(IndDspF : 45 + xx : 1) = *on;
098600110429               V1Dmsg = $Msg(18);
098700110429               leavesr;
098800110429             endif;
098900110429           EndFor;
099000110429
099100110429         EndIf;
099200091113
099300101209         //?Controllo se causale abilitata solo ad un'altra causale Es la 72 si usa solo con la 71
099400101209           If V1CCae <> *blanks  ;
099500101210             clear  TIBS02ds;
099600101209             T02mod = 'C';
099700101209             T02cod = 'CCO';
099800101209             T02ke1 = V1Ccae;
099900101209             TNTBE_RicercaControllo  (kpjba : tibs02ds);
100000101209             if  T02err <> *blanks;
100100101209               errMessage  = *on;
100200101209               errGenerico = *on;
100300101210               PosCurCae = *on;
100400101209               V1Dmsg = $Msg(17);
100500101210               clear v1dcae ;
100600101209               leavesr;
100700101210             else ;
100800101210               v1dcae = t02uni;
100900101209             endif;
101000101210           endif;
101100090526
101200090526       ENDSR;
101300101209
101400090521       //--------------------------------------------------------------
101500090521       //?Gestione trasmissione aggiornamento                          ?
101600090521       //--------------------------------------------------------------
101700090521       BEGSR  sr_GesW01;
101800090521
101900090521         // Inizializzazione videata
102000090521         if $InzW01 = *on;
102100090521           exsr  sr_InzW01;
102200090521           $InzW01 = *off;
102300090521         endif;
102400090521
102500090521         // Emissione window
102600090521         if  TBXftt = 'S';
102700090525           exfmt  TB75W01;
102800090521           ErrMessage  = *off;
102900090521           ErrGenerico = *off;
103000090521           clear  V1Dmsg;
103100090526         else;
103200090526           dsp_aid = c_F06;
103300090521         endif;
103400090521
103500090521         select;
103600090521
103700090521           // F12=Ritorno (gestito solo se emesso W01)
103800090521           when  dsp_aid = c_F12;
103900090521             $Video = 'D2';
104000090521
104100090521           // F6=Conferma
104200090521           when  dsp_aid = c_F06;
104300090526             exsr  sr_UpdTNTBE;
104400090521             // Ritorno alla videata iniziale
104500090521             if  NOT  ErrGenerico;
104600090521               clear  V1Topz;
104700090521               $Video  = 'D1';
104800090521               $InzD01 = *on;
104900090521             endif;
105000090521         endsl;
105100090521
105200090521       ENDSR;
105300090521
105400090521       //--------------------------------------------------------------
105500090521       //?Inizializzazione videata W01                                 ?
105600090521       //--------------------------------------------------------------
105700090521       BEGSR  sr_InzW01;
105800090521
105900090525         clear TB75W01;
106000090521
106100090521         if  $Immissione;
106200090521
106300090521           // Se immissione
106400090521           W1ftt  = TBXftt;
106500090521
106600090521         else;
106700090521
106800090521           // Se NON immissione:
106900090521           // visualizza i dati relativi all'ultima trasmissione
107000090521           W1ftt  = TBEftt;
107100090521           W1flt  = TBEflt;
107200090521           W1ftr  = TBEftr;
107300090521           if TBEdtr <> *zero;
107400090521             wDate_EUR = %date(TBEdtr : *iso);
107500090521             W1dtr     = %dec(wDate_EUR);
107600090521           endif;
107700090521
107800090521         endif;
107900090521
108000090521       ENDSR;
108100090521
108200090521       //--------------------------------------------------------------
108300090521       //?Aggiornamento records TNTBE00F (tab. "CCO")                  ?
108400090521       //--------------------------------------------------------------
108500090526       BEGSR  sr_UpdTNTBE;
108600090521
108700090521         //?Aggancio record di TNTBE00F?
108800090521         k_TBEcod  = c_Tab;
108900090521         k_TBEke1  = V1Ccam;
109000090521         clear  k_TBEke2;
109100090521         clear  k_TBElin;
109200090521         clear  k_TBEsif;
109300090521         //k_TBElin  = TBXlin;
109400090521         //k_TBEsif  = TBXsif;
109500090521
109600090521         chain  %kds(K05tntbe01) TNTBE000;
109700090526
109800090526         exsr  sr_RieCCO;
109900090526         TBEuni = dCCO;
110000090521
110100090521         //?Aggiornamento tab. "CCO"?
110200090521         SELECT;
110300090521
110400090521           // -?F5=Ripristino?
110500090521           WHEN  $Ripristino;
110600090521             clear  TBEatb;
110700090521             TBEftt = W1ftt;
110800090521             clear  TBEftr;
110900090521             //_______________
111000090521             UPDATE  TNTBE000;
111100090521             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
111200090521
111300090521           // -?F16=Annullamento?
111400090521           WHEN  $Annullamento;
111500090521             TBEatb = 'A';
111600090521             TBEftt = W1ftt;
111700090521             clear  TBEftr;
111800090521             //_______________
111900090521             UPDATE  TNTBE000;
112000090521             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
112100090521
112200090521           // -?Immissione o Modifica?
112300090521           OTHER;
112400090521
112500090521             if  NOT %found(TNTBE01L);
112600090521               clear TNTBE000;
112700090521               TBEcod  = k_TBEcod;
112800090521               TBEke1  = k_TBEke1;
112900090521               TBEke2  = k_TBEke2;
113000090521               TBElin  = k_TBElin;
113100090521               TBEsif  = k_TBEsif;
113200091016               TBEuni  = dCCO;
113300090521             endif;
113400090521
113500090521             TBEapl = TBXapl;
113600090521             clear  TBEatb;
113700090521             TBEftt = W1ftt;
113800090521             clear  TBEflt;
113900090521             clear  TBEftr;
114000090521             //clear TBEdtr;
114100090521
114200090521             // - Aggiornamento record
114300090521             if  %found(TNTBE01L);
114400090521               //_____________
114500090521               UPDATE TNTBE000;
114600090521               //¯¯¯¯¯¯¯¯¯¯¯¯¯
114700090521             else;
114800090521               //_____________
114900090521               WRITE  TNTBE000;
115000090521               //¯¯¯¯¯¯¯¯¯¯¯¯¯
115100090521             endif;
115200090521
115300090521         ENDSL;
115400090521
115500090521       ENDSR;
115600090521
115700090521       //--------------------------------------------------------------
115800090521       //?Caricamento dati in tab. "CCO"                               ?
115900090521       //--------------------------------------------------------------
116000090521       BEGSR  sr_RieCCO;
116100090521
116200090521         clear  dCCO;
116300090521
116400090521         §CCOdes = V1Cdes;
116500100126         §CCOord = V1Cord;
116600090521         §CCOgio = V1Cgio;
116700100126         §CCOtat = V1Ctat;
116800100126         §CCOatt = V1Catt;
116900100924         §CCOupm = V1Cupm;
117000100126         §CCOuti = V1Cuti;
117100100126         §CCOesi = V1Cesi;
117200100126         §CCOeaf = V1Ceaf;
117300100126         %subst(§CCOopz:1:1) = V1Copz1;
117400100126         %subst(§CCOopz:2:1) = V1Copz2;
117500100126         %subst(§CCOopz:3:1) = V1Copz3;
117600100126         %subst(§CCOopz:4:1) = V1Copz4;
117700100126         %subst(§CCOopz:5:1) = V1Copz5;
117800100126         §CCOnot = V1Cnot;
117900100126         §CCOsta = V1Csta;
118000091113         §CCOtra = V1Ctra;
118100091113         §CCOapt = V1Capt;
118200100126         §CCOeso = V1Ceso;
118300100219         %subst(§CCOtaa:1:1) = V1Caa1;
118400100219         %subst(§CCOtaa:2:1) = V1Caa2;
118500100219         %subst(§CCOtaa:3:1) = V1Caa3;
118600100219         %subst(§CCOtaa:4:1) = V1Caa4;
118700100219         %subst(§CCOtaa:5:1) = V1Caa5;
118800100219         %subst(§CCOtaa:6:1) = V1Caa6;
118900100219         %subst(§CCOtaa:7:1) = V1Caa7;
119000100219         %subst(§CCOtaa:8:1) = V1Caa8;
119100100219         %subst(§CCOtaa:9:1) = V1Caa9;
119200100219         %subst(§CCOtaa:10:1) = V1Ca10;
119300100219         §CCOoff = V1Coff;
119400100305         §CCOcht = V1Ccht;
119500110429         §CCOcod = Savcod;
119600100305         §CCOres = V1Cres;
119700100305         §CCOcoi = V1Ccom;
119800100504         §CCOtpv = dsV1Ctpv;
119900110429         §CCOcpo = dsV1Ccpo;
120000101210         §CCOhot = V1Chot;
120100101210         §CCOcae = V1Ccae;
120200101210         §CCOann = V1Cann;
120300101210         §CCOblc = V1Cblc;
120400101214         §CCOccp = V1Cccp;
120500110211         §CCOstl = V1Cstl;
120600110211         §CCOpgmblc = V1Cpgmblc;
120700110405         §CCOcns = V1Ccns;
120800110405         §CCOucns = V1Cucns;
120900120622         §CCOdpc = V1Cdpc;
121000090521
121100090521       ENDSR;
121200090521
121300090521       //--------------------------------------------------------------
121400090521       //?Operazioni finali.                                           ?
121500090521       //--------------------------------------------------------------
121600090521       BEGSR  sr_RoutEnd;
121700090521
121800090521         //?Chiusura funzioni precedentemente aperte?
121900090521         clear  TIBS02ds;
122000090521         T02tla = 'C';
122100090521         TNTBE_RicercaControllo ( kpjba : tibs02ds );
122200090521
122300090521         //?Chiusura pgm?
122400090521         return;
122500090521
122600090521       ENDSR;
122700090521
122800090521      /end-free
122900090521
123000090521       //--------------------------------------------------------------
123100090521       //?Definizione schiere a tempo di compilazione                  ?
123200090521       //--------------------------------------------------------------
123300090521
123400090521** > $Msg -------------------------------------------------------------------*
123500091113Immettere la Causale Contatto Marketing                                         1
123600090521Descrizione obbligatoria                                                        2
123700090521Giorni proposti per prossimo contatto NON possono essere negativi               3
123800090525Valore NON previsto.  Unici valori previsti: _=No, L=Sì                         4
123900100127Opzione errata                                                                  5
124000121018...........                                                                     6
124100121018...........                                                                     7
124200121018...........                                                                     8
124300100126Tipo attività associata errata                                                  9
124400090629Tipo contatto futuro errato                                                     10
124500091113Se causale crea Trattativa non può non essere legata ad una Trattativa          11
124600100504Richiesto "Causale legata a Trattativa" = "S" per indicare "Tipi Trattativa"    12
124700100504Tipo trattativa errato                                                          13
124800100924Impostare giorni prossimo contatto a zero se segno =                            14
124900100924Impostare giorni prossimo contatto > di zero se segno + o -                     15
125000100924Se impostati i giorni prossimo contatto obbligatorio il segno + o -             16
125100101209Causale errata                                                                  17
125200110429Categoria Potenziale errata                                                     18
