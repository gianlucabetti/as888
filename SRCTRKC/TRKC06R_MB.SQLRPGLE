000100150127       //==============================================================
000200150127       //?TRKC06R * Inserimento Cliente in Campagna Commerciale.       ?
000300150127       //==============================================================
000400150127
000500150127       //--------------------------------------------------------------
000600150127       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700150127       //--------------------------------------------------------------
000800150127
000900150127       //--------------------------------------------------------------
001000150127       //?Specifiche di controllo.                                     ?
001100150127       //--------------------------------------------------------------
001200141110
001300150224     h decedit('0,')  datedit(*dmy/)  option(*nodebugio)
001400141110     h dftactgrp(*no) actgrp(*caller)
001500141110
001600150127       //--------------------------------------------------------------
001700150127       //?Dichiarazione file.                                          ?
001800150127       //--------------------------------------------------------------
001900141110
002000150127       // -?Organigramma?
002100141110     fAZORG01L  if   e           k disk
002200150224
002300150224       // -?Anagrafica Commerciali?
002400150224     fAZCMM01L  if   e           k disk
002500141110
002600150127       // -?Anagrafica Campagne Commerciali?
002700141110     fTICMP01L  if   e           k disk
002800141110
002900150127       // -?Clienti in Campagna?
003000150206     fTICMC02L  if A e           k disk
003100141110
003200150127       // -?Info Clienti in Campagna?
003300150127     fTICMI00F  O    e             disk
003400141110
003500150127       // -?Video Gestione?
003600150112     fTRKC06D   cf   e             workstn
003700141110     f                                     indds(IndDspF)
003800141110     f                                     infds(InfDspF)
003900141110
004000150127       //--------------------------------------------------------------
004100150127       //?Definizione costanti.                                        ?
004200150127       //--------------------------------------------------------------
004300150127
004400150127       // -?Fase di Apertura Campagna comm.le per il Cliente?
004500150127     d c_FaseApertura  c                   const(' 10')
004600150127
004700150127       // -?Costante per controllo "caratteri solo numerici"?
004800150127     d c_Digits        c                   const('0123456789')
004900141110
005000150127       // -?Tasti funzionali a video?
005100141110     d c_F01           c                   const(x'31')
005200141110     d c_F02           c                   const(x'32')
005300141110     d c_F03           c                   const(x'33')
005400141110     d c_F04           c                   const(x'34')
005500141110     d c_F05           c                   const(x'35')
005600141110     d c_F06           c                   const(x'36')
005700141110     d c_F07           c                   const(x'37')
005800141110     d c_F08           c                   const(x'38')
005900141110     d c_F09           c                   const(x'39')
006000141110     d c_F10           c                   const(x'3A')
006100141110     d c_F11           c                   const(x'3B')
006200141110     d c_F12           c                   const(x'3C')
006300141110     d c_F13           c                   const(x'B1')
006400141110     d c_F14           c                   const(x'B2')
006500141110     d c_F15           c                   const(x'B3')
006600141110     d c_F16           c                   const(x'B4')
006700141110     d c_F17           c                   const(x'B5')
006800141110     d c_F18           c                   const(x'B6')
006900141110     d c_F19           c                   const(x'B7')
007000141110     d c_F20           c                   const(x'B8')
007100141110     d c_F21           c                   const(x'B9')
007200141110     d c_F22           c                   const(x'BA')
007300141110     d c_F23           c                   const(x'BB')
007400141110     d c_F24           c                   const(x'BC')
007500141110     d c_Enter         c                   const(x'F1')
007600141110     d c_RollDown      c                   const(x'F4')
007700141110     d c_RollUp        c                   const(x'F5')
007800141110
007900150127       //--------------------------------------------------------------
008000150127       //?Definizione schiere.                                         ?
008100150127       //--------------------------------------------------------------
008200141110
008300150127       // -?Messaggi di errore?
008400160202     d sk_Msg          s             78    dim(15)  ctdata  perrcd(1)
008500141110
008600150127       //--------------------------------------------------------------
008700150127       //?Definizione aree dati.                                       ?
008800150127       //--------------------------------------------------------------
008900141110
009000150127       // -?Dati utente?
009100141110     d §AzUte        e ds                  extname(AZUTE00F)
009200141110     d                                     dtaara
009300141110     d §DatiUte      e ds                  extname(dDatiUte)
009400141110     d                                     dtaara
009500141110
009600150127       //--------------------------------------------------------------
009700150127       //?Definizione strutture dati.                                  ?
009800150127       //--------------------------------------------------------------
009900141110
010000150127       // -?Status?
010100150127     d Status         sds
010200141110     d   SDSpgm          *proc
010300141110
010400150127       // -?InfDS?
010500141110     d InfDspF         ds
010600150127     d   dsp_aid             369    369a
010700141110
010800150127       // -?Indicatori su DspF?
010900141110     d IndDspF         ds
011000150127         // -?Abilitazione tasti funzionali?
011100150127     d   F6Attivo                      n   overlay(IndDspF : 06)
011200150127     d   F12Attivo                     n   overlay(IndDspF : 12)
011300150127         // -?Emissione messaggio di errore?
011400150127     d   ErrMessage                    n   overlay(IndDspF : 28)
011500160202         // -?Visualizzazione campi
011600160202     d   VisDecoObj                    n   overlay(IndDspF : 40)
011700150127         // -?Posizionamento cursore & segnalazione errore?
011800150127     d   PosCurKSU                     n   overlay(IndDspF : 50)
011900150127     d   PosCurCLN                     n   overlay(IndDspF : 51)
012000150127     d   PosCurMES                     n   overlay(IndDspF : 52)
012100150206     d   PosCurRICS                    n   overlay(IndDspF : 53)
012200150206     d   PosCurRIC                     n   overlay(IndDspF : 54)
012300150206     d   PosCurSPE                     n   overlay(IndDspF : 55)
012400150206     d   PosCurPKG                     n   overlay(IndDspF : 56)
012500150206     d   PosCurDELS                    n   overlay(IndDspF : 57)
012600150206     d   PosCurDEL                     n   overlay(IndDspF : 58)
012700150206     d   PosCurIST                     n   overlay(IndDspF : 59)
012800150206     d   PosCurPEAS                    n   overlay(IndDspF : 60)
012900150206     d   PosCurPEA                     n   overlay(IndDspF : 61)
013000160202     d   PosCurDECO                    n   overlay(IndDspF : 62)
013100150127         // -?Riemissione videata?
013200150127     d   ErrGenerico                   n   overlay(IndDspF : 99)
013300141110
013400150127       // -?Parametri ricevuti?
013500141110     d KPJBA         e ds
013600150127     d TRKC06ds      e ds
013700160202
013800160202       // -?Ricerca/Controllo tabelle
013900160202     d TIBS02DS      e ds                  inz
014000160202
014100160202       // -?Tabella FCM - Fase Campagna
014200160202     d dFCM          e ds
014300160202
014400160202     d wMmAaaa_ds      ds             6    inz  qualified
014500160202     d   wMm                          2s 0 inz
014600160202     d   wAaaa                        4s 0 inz
014700141110
014800141110      //---------------------------------------------------------------
014900141110      //?Definizione variabili globali.
015000141110      //---------------------------------------------------------------
015100141110
015200150128       // -?Flags booleani?
015300150128     d $Err            s               n   inz
015400150128     d $Fine           s               n   inz
015500150128     d $InzD01         s               n   inz(*on)
015600150128     d $InzD02         s               n   inz(*on)
015700160128     d ForzaCmp        s               n   inz
015800160128     d ForzaPea        s               n   inz
015900141110
016000150128       // -?Indici di schiera?
016100150128     d xx              s              3  0 inz
016200141110
016300150128       // -?Campi associati al video?
016400150128     d $Video          s              2    inz('D1')
016500141110
016600150128       // -?Campi di Comodo?
016700160202     d Data_ISO        s               d   datfmt(*iso)
016800160202     d wAnno           s              4s 0 inz
016900160202     d wAnno1          s              4s 0 inz
017000160202     d wAnno2          s              4s 0 inz
017100150128     d wDate_EUR       s               d   datfmt(*EUR)     inz
017200150128     d wOggi           s              8  0 inz
017300150128     d wTotCli         s              5i 0 inz
017400150128     d wAbi            s                   like(oTAA1cabi)  inz
017500141110
017600150128       //--------------------------------------------------------------
017700150128       //?Definizione prototipi/procedure usate.                       ?
017800150128       //--------------------------------------------------------------
017900160202       // -?Ricerca/Controllo tabelle
018000160202      /copy gaitrasrc/srcprotopr,TIBS02R
018100150128
018200150128       // -?Reperimento dati utente?
018300150128     d TIBS34ds      e ds                  inz
018400150128      /copy gaitrasrc/srcProtoPR,TIBS34R
018500150128
018600150128       // -?Ricerca Unificante Padre?
018700150128     d TIBS10ds      e ds                  inz
018800150128      /copy gaitrasrc/srcprotopr,TIBS10R
018900150128
019000150128       // -?Reperimento dati Anagrafica Clienti?
019100150128      /copy gaitrasrc/srcprotopi,TIBS69R
019200150128      /copy gaitrasrc/srcprotopr,TIBS69R
019300150128
019400150128       // -?Controllo Abilitazioni Utente?
019500150128     d TNTAA1ds      e ds                  inz
019600150128      /copy gaitrasrc/srcprotopr,TNTAA1C
019700141110
019800150128       // -?Ricerca Anagrafica Clienti?
019900150128     d TNTAI1ds      e ds                  inz
020000150128     d tntaI1R         pr                  extpgm('TNTAI1R')
020100150128     d   kpjba                             likeds(kpjba)
020200150128     d   tntaI1ds                          likeds(TNTAI1ds)
020300150128
020400150128       // -?Scrittura Fase per Cliente in Campagna?
020500150128     d TRKC71ds      e ds                  inz
020600150128     d trkc71R         pr                  extpgm('TRKC71R')
020700150128     d   kpjba                             likeds(kpjba)
020800150128     d   trkc71ds                          likeds(TRKC71ds)
020900160202
021000160202       // -?Controllo/Inversione data?
021100160202     d WLBdat          ds                  inz
021200160202     d   G08dat                       8  0 inz
021300160202     d   G08inv                       8  0 inz
021400160202     d   G08err                       1    inz('3')
021500160202     d   G08tgi                       5  0 inz
021600160202      /copy gaitrasrc/srcProtoPR,XSRDA8
021700141110
021800150128       //--------------------------------------------------------------
021900150128       //?Definizione key-list.                                        ?
022000150128       //--------------------------------------------------------------
022100150129
022200150206       // -?File TICMC02L?
022300150206     d keyTICMC02    e ds                  extname(TICMC02L : *key)
022400150129     d                                     prefix(k_)   inz
022500150224
022600150224       // -?File AZCMM01L?
022700150224     d keyAZCMM01    e ds                  extname(AZCMM01L : *key)
022800150224     d                                     prefix(k_)   inz
022900150129
023000150129       // -?File AZORG01L?
023100150129     d keyAZORG01    e ds                  extname(AZORG01L : *key)
023200150129     d                                     prefix(k_)   inz
023300141110
023400150128       //--------------------------------------------------------------
023500150128       //?M A I N - L I N E                                            ?
023600150128       //--------------------------------------------------------------
023700141110
023800141110     c     *Entry        plist
023900150129     c                   parm                    KPJBA
024000150129     c                   parm                    TRKC06ds
024100141110
024200141110      /free
024300141110
024400150128       // -?Operazioni iniziali?
024500150128       exsr  sr_RoutInz;
024600141110
024700150128       // -?Ciclo di gestione del file video?
024800150128       DoW  $Fine = *off;
024900150128
025000150128         select;
025100150128
025200150128           // -?Gestione videata D1 (richiesta cod. Unificante)?
025300150128           when  $Video = 'D1';
025400150128             exsr  sr_GesD01;
025500150128
025600150128           // -?Gestione videata D1 (richiesta dati Unificante)?
025700150128           when  $Video = 'D2';
025800150128             exsr  sr_GesD02;
025900150128
026000150128           // -?? ? ??
026100150128           other;
026200150128             $Fine = *on;
026300150128
026400150128         endsl;
026500150128
026600150128       EndDo;
026700141110
026800150128       // -?Operazioni finali?
026900150128       exsr  sr_RoutEnd;
027000141110
027100150128       //--------------------------------------------------------------
027200150128       //?Operazioni iniziali.                                         ?
027300150128       //--------------------------------------------------------------
027400150128       BEGSR  sr_RoutInz;
027500141110
027600141110         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
027700150128
027800150128         // -?Impostazione chiusura?
027900150128         *inLR = *on;
028000150128
028100150128         // -?Impostazione nome programma a video?
028200150128         T01pgm = SDSpgm;
028300141110
028400150128         // -?Impostazione campi chiave fissi?
028500150128         $Video  = 'D1';
028600150128         $InzD01 = *on;
028700150128
028800150128         // -?Reperimento data odierna?
028900150128         wOggi = %int( %subst( %char( %dec( %timestamp() ) )
029000150128                               : 1 : 8 ) );
029100160202
029200160202         //?Anno in corso
029300160202         Data_ISO = %date(wOggi);
029400160202         wAnno    = %subdt(Data_ISO:*years);
029500160202         //?Anno in corso + 1
029600160202         Data_ISO += %years(1);
029700160202         wAnno1    = %subdt(Data_ISO:*years);
029800160202         //?Anno in corso - 1
029900160202         Data_ISO  = %date(wOggi);
030000160202         Data_ISO -= %years(1);
030100160202         wAnno2    = %subdt(Data_ISO:*years);
030200150128
030300150128         // -?Reperimento dati job?
030400150128         exsr  sr_DatiJob;
030500150128
030600150128         // -?Pulizia parametri di solo Output?
030700150129         clear  oKC06err;
030800150128         clear  oKC06msg;
030900150205
031000150205         // -?Controllo Sistema Informativo?
031100150205         If  DUTlpo <> 'S';
031200150205           $Fine    = *on;
031300150205           oKC06err = *on;
031400150205           oKC06msg = sk_Msg(01);
031500150205           leavesr;
031600150205         EndIf;
031700141111
031800150128         // -?Controllo Abilitazioni Utente?
031900150128         clear  TNTAA1ds;
032000150128         iTAA1cAut = '§utegtc';
032100150128         kpjbu = TNTAA1ds;
032200150128         TNTAA1C ( kpjba );
032300150128         TNTAA1ds = kpjbu;
032400141111
032500150128         If  oTAA1fabi = 'N';
032600150128           $Fine    = *on;
032700150128           oKC06err = *on;
032800150205           oKC06msg = sk_Msg(02);
032900141111           leavesr;
033000150128         EndIf;
033100150129         wAbi = oTAA1cAbi;
033200150128
033300150128         // -?Controllo dei parametri ricevuti?
033400150128         If  iKC06ncm = *zero;
033500150128           $Fine    = *on;
033600150128           oKC06err = *on;
033700150205           oKC06msg = sk_Msg(03);
033800141110           leavesr;
033900150128         EndIf;
034000150128
034100150128         chain  ( iKC06ncm )  TICMP000;
034200150128         If  NOT %found(TICMP01L)  or  CMPdfc < wOggi;
034300150128           $Fine    = *on;
034400150128           oKC06err = *on;
034500150205           oKC06msg = sk_Msg(03);
034600141110           leavesr;
034700150128         EndIf;
034800141110
034900141110       ENDSR;
035000141110
035100141110       //--------------------------------------------------------------
035200150128       //?Reperimento Dati del job (Utente/Operativi).                 ?
035300141110       //--------------------------------------------------------------
035400150128       BEGSR  sr_DatiJob;
035500141110
035600150128         in(e) §AzUte;
035700150128         if NOT %error;
035800150128           in(e) §DatiUte;
035900150128         endif;
036000150128         if %error or RSut = *blank;
036100150128           tibs34r ( tibs34ds );
036200150128           in §AzUte;
036300150128           in §DatiUte;
036400150128         endif;
036500141110
036600141110       ENDSR;
036700141110
036800141110       //--------------------------------------------------------------
036900150128       //?Gestione videata D01.                                        ?
037000141110       //--------------------------------------------------------------
037100150129       BEGSR  sr_GesD01;
037200141110
037300150128         // -?Inizializzazione videata?
037400150128         if  $InzD01 = *on;
037500150128           exsr  sr_InzD01;
037600150128           $InzD01 = *off;
037700150128         endif;
037800141110
037900150128         // -?Emissione Testata & Piede con tasti funzionali abilitati?
038000150112         write  KC06T01;
038100150128         write  KC06P01;
038200141110
038300150128         // -?Emissione videata?
038400150128         exfmt  KC06D01;
038500150128
038600150128         clear  V01msg;
038700150128         reset  ErrMessage;
038800150128         reset  ErrGenerico;
038900141110
039000141110         SELECT;
039100150128
039200150128           // -?F3=Fine?
039300150128           WHEN  dsp_aid = c_F03;
039400150128             $Fine = *on;
039500141110
039600150128           // -?Invio?
039700150128           OTHER;
039800150128             exsr  sr_CtrD01;
039900150128             if  ErrGenerico;
040000141110               leavesr;
040100150128             endif;
040200150128             $Video  = 'D2';
040300150128             $InzD02 = *on;
040400150128
040500150128         ENDSL;
040600150128
040700150128       ENDSR;
040800150128
040900150128       //--------------------------------------------------------------
041000150128       //?Inizializzazione videata D01.                                ?
041100150128       //--------------------------------------------------------------
041200150128       BEGSR  sr_InzD01;
041300150128
041400150128         clear  KC06D01;
041500150128
041600150129         // -?Numero Campagna?
041700150128         V01ncm = iKC06ncm;
041800150128
041900150129         // -?Decodifica Campagna?
042000150128         chain  ( iKC06ncm )  TICMP000;
042100150128         If  %found(TICMP01L);
042200150128           V01des = CMPdes;
042300150128           wDate_EUR = %date( CMPdic : *ISO );
042400150128           V01dic = %dec( wDate_Eur );
042500150128           wDate_EUR = %date( CMPdfc : *ISO );
042600150128           V01dfc = %dec( wDate_Eur );
042700150128           V01tpc = CMPtpc;
042800150128         Else;
042900150128           V01des = *all'? ';
043000150128         EndIf;
043100150129
043200150129         // -?Codice Cliente Unificante?
043300150129         V01ksu = '?';
043400150128
043500150128         // -?(Dis)Abilitazione Tasti Funzionali?
043600150128         F6Attivo  = *off;
043700150128         F12Attivo = *off;
043800150128
043900150128       ENDSR;
044000150128
044100150128       //--------------------------------------------------------------
044200150128       //?Controllo dati in videata D01.                               ?
044300150128       //--------------------------------------------------------------
044400150128       BEGSR  sr_CtrD01;
044500150128
044600150128         // -?Spegnimento degli indicatori di errore?
044700150128         %subst(IndDspF : 50) = *off;
044800150128
044900150128         clear  V01rag;
045000150128
045100150129         // -?Controllo codice Cliente Unificante?
045200150128         Select;
045300150128           // ·?Controllo inserimento?
045400150128           When  V01ksu = *blank  or  V01ksu = *zero;
045500150205             PosCurKSU   = *on;
045600150128             ErrMessage  = *on;
045700150128             ErrGenerico = *on;
045800150205             V01msg = sk_Msg(04);
045900150128             leavesr;
046000150128           // ·?Ricerca?
046100150128           When  %scan( '?' : V01ksu ) > *zero;
046200150128             clear  V01ksu;
046300150205             PosCurKSU   = *on;
046400150129             ErrGenerico = *on;
046500150128             clear  TNTAI1ds;
046600150128             iTAI1ric = 'S';
046700150129             iTAI1abi = oTAA1cAbi;
046800150128             iTAI1aut = '*C';
046900150128             tntaI1r ( kpjba : TNTAI1ds );
047000150128             select;
047100150128               when  oTAI1f03 <> *blank  or  oTAI1f12 <> *blank;
047200150128               when  oTAI1err <> *blank;
047300150128                 ErrMessage  = *on;
047400150129                 V01msg = oTAI1msg;
047500150129               other;
047600150129                 V01ksu = %editc( oTAI1ksc : 'X' );
047700150128             endsl;
047800150129             leavesr;
047900150128           // ·?Controllo numericità?
048000150128           When  %check( c_Digits : V01ksu ) > *zero;
048100150205             PosCurKSU   = *on;
048200150128             ErrMessage  = *on;
048300150128             ErrGenerico = *on;
048400150205             V01msg = sk_Msg(04);
048500150128             leavesr;
048600150129         EndSl;
048700150129
048800150129         // -?Controllo esistenza?
048900150129         clear  TIBS69ds;
049000150129         I69sif = knsif;
049100150129         I69kcc = DUTkci;
049200150129         I69kac = %int( V01ksu );
049300150129         I69kin = I69kac;
049400150129         I69kcp = I69kac;
049500150129         tibs69r( tibs69ds :
049600150129                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
049700150129         if  O69err = *on;
049800150205           PosCurKSU   = *on;
049900150129           ErrMessage  = *on;
050000150205           ErrGenerico = *on;
050100150205           V01msg = sk_Msg(04);
050200150129           leavesr;
050300150129         endif;
050400150129
050500150129         V01rag  = ACOrag;
050600150129
050700150806         // -?Controllo SE cliente già presente in Campagna attiva ?
050800150206         clear  keyTICMC02;
050900150129         k_CMCksu = I69kac;
051000150806         setll %kds(keyTICMC02:1) TICMC02L;
051100150806         reade %kds(keyTICMC02:1) TICMC02L;
051200150806         DOW  not %eof(TICMC02L);
051300150806           chain (CMCncm) TICMP01L;
051400160128           IF  %found(TICMP01L) and CMPdfc > wOggi and not ForzaCmp;
051500150806             PosCurKSU   = *on;
051600150806             ErrMessage  = *on;
051700150806             ErrGenerico = *on;
051800150806             V01msg = %trimR( sk_Msg(04) ) + ': già presente in Campagna '
051900160128                    + %trim( %editc( CMCncm : '2' ) )
052000160128                    + %trim('. Enter per Forzare');
052100160128             ForzaCmp = *on;
052200150806             leavesr;
052300150806           ENDIF;
052400150806           reade %kds(keyTICMC02:1) TICMC02L;
052500150806         ENDDO;
052600150129
052700150129         // -?Controllo SE cliente Unificante?
052800150129         clear  TIBS10ds;
052900150129         D10tle = 'ST';
053000150129         D10paf = 'P';
053100150129         D10cod = I69kac;
053200150129         TIBS10R ( TIBS10ds );
053300150129         // ·?Se NON ritorna errore: vuol dire che è un codice Figlio?
053400150129         if  D10err = *blank;
053500150205           PosCurKSU   = *on;
053600150129           ErrMessage  = *on;
053700150129           ErrGenerico = *on;
053800150205           V01msg = %trim( sk_Msg(04) ) + ': deve essere un codice Padre';
053900150129           leavesr;
054000150129         endif;
054100150128
054200150128       ENDSR;
054300150129
054400150129       //--------------------------------------------------------------
054500150129       //?Gestione videata D02.                                        ?
054600150129       //--------------------------------------------------------------
054700150129       BEGSR  sr_GesD02;
054800150129
054900150129         // -?Inizializzazione videata?
055000150129         if  $InzD02 = *on;
055100150129           exsr  sr_InzD02;
055200150129           $InzD02 = *off;
055300150129         endif;
055400150129
055500150129         if  Not ErrMessage;
055600150129           // -?Emissione Testata & Piede con tasti funzionali abilitati?
055700150129           write  KC06T01;
055800150129           write  KC06P01;
055900150129           // -?Emissione videata precedente (protetta)?
056000150129           write  KC06D01;
056100150129           write  Protect;
056200150129         endif;
056300150129
056400150129         // -?Emissione videata?
056500150129         exfmt  KC06D02;
056600150129
056700150129         clear  V01msg;
056800150129         reset  ErrMessage;
056900150129         reset  ErrGenerico;
057000150129
057100150129         SELECT;
057200150129
057300150129           // -?F3=Fine?
057400150129           WHEN  dsp_aid = c_F03;
057500150129             $Fine = *on;
057600150129
057700150129           // -?F12=Ritorno?
057800150129           WHEN  dsp_aid = c_F12;
057900150129             $Video = 'D1';
058000150129
058100150129           // -?Invio / F6=Conferma?
058200150129           OTHER;
058300150129             exsr  sr_CtrD02;
058400150129             if  ErrGenerico  or  dsp_aid <> c_F06;
058500150129               leavesr;
058600150129             endif;
058700150129             exsr  sr_F06D02;
058800150129             $Video  = 'D1';
058900150129             $InzD01 = *on;
059000150129
059100150129         ENDSL;
059200150129
059300150129       ENDSR;
059400150129
059500150129       //--------------------------------------------------------------
059600150129       //?Inizializzazione videata D02.                                ?
059700150129       //--------------------------------------------------------------
059800150129       BEGSR  sr_InzD02;
059900150129
060000150129         clear  KC06D02;
060100150129
060200150129         // -?Altri dati del Cliente Unificante?
060300150129         V02ind  = INDvia;
060400150129         V02cap  = INDcae;
060500150129         V02loc  = INDcit;
060600150129         V02prv  = INDprv;
060700150129         V02naz  = INDsta;
060800150129         //V02cmm  = CLPage;  ?(serve il comm.le Unificante)?
060900150224         V02clv  = CLPclv;
061000150129
061100150224         // -?Reperisce e Decodifica il Comm.le Unificante?
061200150224         V02cmmD = *all'? ';
061300150224         k_CMMcod = CLPage;
061400150224         chain  %kds( keyAZCMM01 )  AZCMM000;
061500150224
061600150224         IF  %found(AZCMM01L)  and  CMMdtFin >= wOggi;
061700150224
061800150224           k_CMMcod = CMMuni;
061900150224           chain  %kds( keyAZCMM01 )  AZCMM000;
062000150224           If  %found(AZCMM01L)  and  CMMdtFin >= wOggi;
062100150224
062200150224             V02cmm  = CMMuni;
062300150224             V02cmmD = CMMdes;
062400150224             k_ORGfil = CMMuni / 10000;
062500150224             chain  %kds( keyAZORG01 )  AZORG;
062600150224             if  %found(AZORG01L);
062700150224               V02car  = ORGcar;    // -?Area?
062800150224               V02dis  = ORGfl3;    // -?Distretto?
062900150224             endif;
063000150224
063100150224           EndIf;
063200150224
063300150224         ENDIF;
063400150129
063500150224         // -?Segno Obiettivo Iniziale?
063600150129         V02peaS = '+';
063700160202
063800160202         // -?Decorrenza Obiettivo?
063900160202         clear TIBS02DS;
064000160202         clear dFCM;
064100160202         T02mod = 'C';
064200160202         T02cod = 'FCM';
064300160202         T02ke1 = c_FaseApertura;
064400160202         T02sif = KNSIF;
064500160202         TNTBE_RicercaControllo  (kpjba : tibs02ds);
064600160202       //?Dati della fase
064700160202         dFCM = T02uni;
064800160202         IF  §FCMDECO = 'S';
064900160202           VisDecoObj = *on;
065000160202         ELSE;
065100160202           VisDecoObj = *off;
065200160202         ENDIF;
065300150129
065400150129         // -?(Dis)Abilitazione Tasti Funzionali?
065500150129         F6Attivo  = *on;
065600150129         F12Attivo = *on;
065700141110
065800141110       ENDSR;
065900150129
066000150129       //--------------------------------------------------------------
066100150129       //?Controllo dati in videata D02.                               ?
066200150129       //--------------------------------------------------------------
066300150129       BEGSR  sr_CtrD02;
066400150129
066500150129         // -?Spegnimento degli indicatori di errore?
066600150129         %subst(IndDspF : 50) = *off;
066700150205
066800150205         // -?Flaq Nuovo/Acquisito:?
066900150205         if  V02cln <> 'A'  and  V02cln <> 'N';
067000150205           ErrGenerico = *on;
067100150205           ErrMessage  = *on;
067200150205           PosCurCLN   = *on;
067300150205           V01msg = %trim( sk_Msg(05) ) + ' "A"=Acquisito, "N"=Nuovo';
067400150205           leavesr;
067500150205         endif;
067600150129
067700150129         // -?Mese Acquisizione:?
067800150129         select;
067900150129           // -?obbligatorio SE cliente Nuovo?
068000150129           when  V02cln  = 'N'  and  ( V02newMes < 1  or
068100150129                                       V02newMes > 12 );
068200150129             ErrGenerico = *on;
068300150129             ErrMessage  = *on;
068400150129             PosCurMES   = *on;
068500150205             V01msg = sk_Msg(06);
068600150129             leavesr;
068700150129           // -?non richiesto SE cliente Acquisito?
068800150129           when  V02cln  = 'A'  and  V02newMes <> *zero;
068900150129             ErrGenerico = *on;
069000150129             ErrMessage  = *on;
069100150129             PosCurMES   = *on;
069200150205             V01msg = sk_Msg(07);
069300150129             leavesr;
069400150129         endsl;
069500150205
069600150206         // -?Segno Ricavi errato?
069700150206         if  V02ricS <> '+'  and  V02ricS <> '-';
069800150206           ErrGenerico = *on;
069900150206           ErrMessage  = *on;
070000150206           PosCurRICS  = *on;
070100150206           V01msg = %trim( sk_Msg(05) ) + ' "+"=Valore Positivo, +
070200150206                                            "-"=Valore Negativo';
070300150206           leavesr;
070400150206         endif;
070500150206         // -?Valore dei Ricavi obbligatorio?
070600150206         select;
070700150206           when  V02ric = *zero;
070800150206             ErrGenerico = *on;
070900150206             ErrMessage  = *on;
071000150206             PosCurRIC   = *on;
071100150206             V01msg = sk_Msg(08);
071200150206             leavesr;
071300150206           when  V02ric < *zero;
071400150206             V02ric = %abs(V02ric);
071500150206         endsl;
071600150129
071700150205         // -?Num. Spedizioni   (può essere SOLO positivo)?
071800150205         select;
071900150205           when  V02spe < *zero;
072000150205             V02spe  = %abs( V02spe );
072100150205           when  V02spe = *zero;
072200150205             ErrGenerico = *on;
072300150205             ErrMessage  = *on;
072400150205             PosCurSPE   = *on;
072500150205             V01msg = sk_Msg(09);
072600150205             leavesr;
072700150205         endsl;
072800150205
072900150205         // -?Peso Medio        (può essere SOLO positivo)?
073000150205         select;
073100150205           when  V02pkg = *zero;
073200150205             ErrGenerico = *on;
073300150205             ErrMessage  = *on;
073400150205             PosCurPKG   = *on;
073500150205             V01msg = sk_Msg(10);
073600150205             leavesr;
073700150206           when  V02pkg < *zero;
073800150206             V02pkg  = %abs( V02pkg );
073900150205         endsl;
074000150205
074100150206         // -?Segno Delta errato?
074200150206         if  V02delS <> '+'  and  V02delS <> '-';
074300150206           ErrGenerico = *on;
074400150206           ErrMessage  = *on;
074500150206           PosCurDELS  = *on;
074600150206           V01msg = %trim( sk_Msg(05) ) + ' "+"=Valore Positivo, +
074700150206                                            "-"=Valore Negativo';
074800150206           leavesr;
074900150206         endif;
075000150206         // -?Delta obbligatorio?
075100150206         select;
075200150206           when  V02del = *zero;
075300150206             ErrGenerico = *on;
075400150206             ErrMessage  = *on;
075500150206             PosCurDEL   = *on;
075600150206             V01msg = sk_Msg(11);
075700150206             leavesr;
075800150206           when  V02del < *zero;
075900150206             V02del = %abs(V02del);
076000150206         endsl;
076100150129
076200150129         // -?Istat?
076300150129         if  V02ist <> 'S'  and  V02ist <> 'N';
076400150129           ErrGenerico = *on;
076500150129           ErrMessage  = *on;
076600150129           PosCurIST   = *on;
076700150205           V01msg = %trim( sk_Msg(05) ) + ' "S"=Sì, "N"=No';
076800150129           leavesr;
076900150129         endif;
077000150129
077100150206         // -?Segno Obiettivo errato?
077200150206         if  V02peaS <> '+'  and  V02peaS <> '-';
077300150206           ErrGenerico = *on;
077400150206           ErrMessage  = *on;
077500150206           PosCurPEAS  = *on;
077600150206           V01msg = %trim( sk_Msg(05) ) + ' "+"=Valore Positivo, +
077700150206                                            "-"=Valore Negativo';
077800150206           leavesr;
077900150206         endif;
078000150206         // -?Obiettivo obbligatorio?
078100150129         select;
078200160128           when  V02pea = *zero and not ForzaPea;
078300150129             ErrGenerico = *on;
078400150129             ErrMessage  = *on;
078500150129             PosCurPEA   = *on;
078600150205             V01msg = sk_Msg(12);
078700160128             ForzaPea = *on;
078800150129             leavesr;
078900150129           when  V02pea < *zero;
079000150129             V02pea = %abs(V02pea);
079100150129         endsl;
079200160202
079300160202         // -?Decorrenza Obiettivo?
079400160202         IF  VisDecoObj;
079500160202           wMmAaaa_ds = %editc(V02deco:'X');
079600160202           clear WLBdat;
079700160202           IF  wMmAaaa_ds.wMm > *zero;       //?Inserito anno di 4 cifre?
079800160202             G08dat = 01000000 + V02deco;
079900160202           ELSE;                             //?Inserito anno di 2 cifre?
080000160202             G08dat = 00010000 + V02deco;
080100160202           ENDIF;
080200160202           XSRDA8(WLBdat);
080300160202           IF  G08err = *on;
080400160202             V01msg = sk_Msg(13);
080500160202             ErrMessage  = *on;
080600160202             ErrGenerico = *on;
080700160202             PosCurDECO  = *on;
080800160202             leavesr;
080900160202           ENDIF;
081000160202           V02deco = G08dat - 01000000;
081100160202           wMmAaaa_ds = %editc(V02deco:'X');
081200160202         //?Verifico che l'anno sia o anno in corso o anno in corso -1 o
081300160202         //?anno in corso +1
081400160202           IF wMmAaaa_ds.wAaaa <> wAnno and
081500160202              wMmAaaa_ds.wAaaa <> wAnno1 and
081600160202              wMmAaaa_ds.wAaaa <> wAnno2;
081700160202             V01msg = sk_Msg(14);
081800160202             ErrMessage  = *on;
081900160202             ErrGenerico = *on;
082000160202             PosCurDECO  = *on;
082100160202             leavesr;
082200160202           ENDIF;
082300160202         ENDIF;
082400141110
082500141110       ENDSR;
082600141110
082700141110       //--------------------------------------------------------------
082800150129       //?Gestione tasto funzionale F06 da videata D02:                ?
082900150129       //?F6=Conferma                                                  ?
083000141110       //--------------------------------------------------------------
083100150129       BEGSR  sr_F06D02;
083200141110
083300150129         // -?Aggiunta del cliente nel file INFO clienti?
083400150129         clear  TICMI000;
083500150129         CMIncm = V01ncm;
083600141110         CMIksu = I69kac;
083700141110         CMIclv = V02clv;
083800141110         CMIcmm = V02cmm;
083900141110         CMIdcm = V02dis;
084000141110         CMIacm = V02car;
084100150129         CMIfcm = k_ORGfil;
084200150213         CMIpde = V02del;
084300150213         if  V02delS = '-';
084400150213           CMIpde *= -1;
084500150213         endif;
084600141110         CMInsp = V02spe;
084700141110         CMIric = V02ric;
084800141110         CMIpme = V02pkg;
084900141110         CMIist = V02ist;
085000141110         CMIcln = V02cln;
085100150129         CMImes = V02newMes;
085200150129         //______________
085300150129         write  TICMI000;
085400150129         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
085500150129         feod   TICMI00F;
085600141110
085700150129         // -?Aggiunta del cliente nel file Clienti in Campagna?
085800150129         clear  TICMC000;
085900150129         CMCncm = V01ncm;
086000141110         CMCksu = I69kac;
086100150129         CMCpru = KNMUS;
086200150128         CMCdin = wOggi;
086300150129         //______________
086400150129         write  TICMC000;
086500150129         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
086600150206         feod   TICMC02L;
086700141110
086800150129         // -?Scrittura della Fase " 10" (Automatica)?
086900150129         clear  TRKC71ds;
087000150129         iKC71ncm = V01ncm;
087100150129         iKC71ksu = I69kac;
087200150129         iKC71acm = c_FaseApertura;
087300150129         iKC71pea = V02pea;
087400150129         if  V02peaS = '-';
087500150129           iKC71pea *= -1;
087600150129         endif;
087700150129         iKC71aut = 'A';
087800160202         wMmAaaa_ds = %editc(V02deco:'X');
087900160202         iKC71aaCF  = wMmAaaa_ds.wAaaa;
088000160202         iKC71mmCF  = wMmAaaa_ds.wMm;
088100150129
088200150129         TRKC71R ( kpjba : TRKC71ds );
088300141110
088400141110       ENDSR;
088500141110
088600141110       //--------------------------------------------------------------
088700150129       //?Operazioni finali.                                           ?
088800141110       //--------------------------------------------------------------
088900150129       BEGSR  sr_RoutEnd;
089000150129
089100150129         // -?Uscita dal *pgm?
089200141110         return;
089300141110
089400141110       ENDSR;
089500141110
089600141110      /end-free
089700141110
089800141110       //--------------------------------------------------------------
089900141110       //?Schiere a tempo di compilazione.
090000141110       //--------------------------------------------------------------
090100141110
090200150128** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
090300150205Funzione eseguibile solo dalla SEDE                                            1
090400150205Utente non Abilitato alla funzione                                             2
090500150205Numero Campagna errato                                                         3
090600150205Cliente errato                                                                 4
090700150205Valore errato:                                                                 5
090800150205Mese acquisizione errato o mancante                                            6
090900150205Mese acquisizione NON richiesto                                                7
091000150205Immettere il valore dei Ricavi                                                 8
091100150205Immettere il numero delle Spedizioni                                           9
091200150205Immettere il Peso Medio                                                       10
091300150205Immettere il Delta                                                            11
091400160128Inserito Obiettivo a zero. Enter per continuare.                              12
091500160202Decorrenza Obiettivo errata.                                                  13
091600160202Anno decorrenza Obiettivo errato.                                             14
