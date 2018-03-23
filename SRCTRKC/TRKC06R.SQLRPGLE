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
008400150205     d sk_Msg          s             78    dim(12)  ctdata  perrcd(1)
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
011500150127         // -?Posizionamento cursore & segnalazione errore?
011600150127     d   PosCurKSU                     n   overlay(IndDspF : 50)
011700150127     d   PosCurCLN                     n   overlay(IndDspF : 51)
011800150127     d   PosCurMES                     n   overlay(IndDspF : 52)
011900150206     d   PosCurRICS                    n   overlay(IndDspF : 53)
012000150206     d   PosCurRIC                     n   overlay(IndDspF : 54)
012100150206     d   PosCurSPE                     n   overlay(IndDspF : 55)
012200150206     d   PosCurPKG                     n   overlay(IndDspF : 56)
012300150206     d   PosCurDELS                    n   overlay(IndDspF : 57)
012400150206     d   PosCurDEL                     n   overlay(IndDspF : 58)
012500150206     d   PosCurIST                     n   overlay(IndDspF : 59)
012600150206     d   PosCurPEAS                    n   overlay(IndDspF : 60)
012700150206     d   PosCurPEA                     n   overlay(IndDspF : 61)
012800150127         // -?Riemissione videata?
012900150127     d   ErrGenerico                   n   overlay(IndDspF : 99)
013000141110
013100150127       // -?Parametri ricevuti?
013200141110     d KPJBA         e ds
013300150127     d TRKC06ds      e ds
013400141110
013500141110      //---------------------------------------------------------------
013600141110      //?Definizione variabili globali.
013700141110      //---------------------------------------------------------------
013800141110
013900150128       // -?Flags booleani?
014000150128     d $Err            s               n   inz
014100150128     d $Fine           s               n   inz
014200150128     d $InzD01         s               n   inz(*on)
014300150128     d $InzD02         s               n   inz(*on)
014400141110
014500150128       // -?Indici di schiera?
014600150128     d xx              s              3  0 inz
014700141110
014800150128       // -?Campi associati al video?
014900150128     d $Video          s              2    inz('D1')
015000141110
015100150128       // -?Campi di Comodo?
015200150128     d wDate_EUR       s               d   datfmt(*EUR)     inz
015300150128     d wOggi           s              8  0 inz
015400150128     d wTotCli         s              5i 0 inz
015500150128     d wAbi            s                   like(oTAA1cabi)  inz
015600141110
015700150128       //--------------------------------------------------------------
015800150128       //?Definizione prototipi/procedure usate.                       ?
015900150128       //--------------------------------------------------------------
016000150128
016100150128       // -?Reperimento dati utente?
016200150128     d TIBS34ds      e ds                  inz
016300150128      /copy gaitrasrc/srcProtoPR,TIBS34R
016400150128
016500150128       // -?Ricerca Unificante Padre?
016600150128     d TIBS10ds      e ds                  inz
016700150128      /copy gaitrasrc/srcprotopr,TIBS10R
016800150128
016900150128       // -?Reperimento dati Anagrafica Clienti?
017000150128      /copy gaitrasrc/srcprotopi,TIBS69R
017100150128      /copy gaitrasrc/srcprotopr,TIBS69R
017200150128
017300150128       // -?Controllo Abilitazioni Utente?
017400150128     d TNTAA1ds      e ds                  inz
017500150128      /copy gaitrasrc/srcprotopr,TNTAA1C
017600141110
017700150128       // -?Ricerca Anagrafica Clienti?
017800150128     d TNTAI1ds      e ds                  inz
017900150128     d tntaI1R         pr                  extpgm('TNTAI1R')
018000150128     d   kpjba                             likeds(kpjba)
018100150128     d   tntaI1ds                          likeds(TNTAI1ds)
018200150128
018300150128       // -?Scrittura Fase per Cliente in Campagna?
018400150128     d TRKC71ds      e ds                  inz
018500150128     d trkc71R         pr                  extpgm('TRKC71R')
018600150128     d   kpjba                             likeds(kpjba)
018700150128     d   trkc71ds                          likeds(TRKC71ds)
018800141110
018900150128       //--------------------------------------------------------------
019000150128       //?Definizione key-list.                                        ?
019100150128       //--------------------------------------------------------------
019200150129
019300150206       // -?File TICMC02L?
019400150206     d keyTICMC02    e ds                  extname(TICMC02L : *key)
019500150129     d                                     prefix(k_)   inz
019600150224
019700150224       // -?File AZCMM01L?
019800150224     d keyAZCMM01    e ds                  extname(AZCMM01L : *key)
019900150224     d                                     prefix(k_)   inz
020000150129
020100150129       // -?File AZORG01L?
020200150129     d keyAZORG01    e ds                  extname(AZORG01L : *key)
020300150129     d                                     prefix(k_)   inz
020400141110
020500150128       //--------------------------------------------------------------
020600150128       //?M A I N - L I N E                                            ?
020700150128       //--------------------------------------------------------------
020800141110
020900141110     c     *Entry        plist
021000150129     c                   parm                    KPJBA
021100150129     c                   parm                    TRKC06ds
021200141110
021300141110      /free
021400141110
021500150128       // -?Operazioni iniziali?
021600150128       exsr  sr_RoutInz;
021700141110
021800150128       // -?Ciclo di gestione del file video?
021900150128       DoW  $Fine = *off;
022000150128
022100150128         select;
022200150128
022300150128           // -?Gestione videata D1 (richiesta cod. Unificante)?
022400150128           when  $Video = 'D1';
022500150128             exsr  sr_GesD01;
022600150128
022700150128           // -?Gestione videata D1 (richiesta dati Unificante)?
022800150128           when  $Video = 'D2';
022900150128             exsr  sr_GesD02;
023000150128
023100150128           // -?? ? ??
023200150128           other;
023300150128             $Fine = *on;
023400150128
023500150128         endsl;
023600150128
023700150128       EndDo;
023800141110
023900150128       // -?Operazioni finali?
024000150128       exsr  sr_RoutEnd;
024100141110
024200150128       //--------------------------------------------------------------
024300150128       //?Operazioni iniziali.                                         ?
024400150128       //--------------------------------------------------------------
024500150128       BEGSR  sr_RoutInz;
024600141110
024700141110         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
024800150128
024900150128         // -?Impostazione chiusura?
025000150128         *inLR = *on;
025100150128
025200150128         // -?Impostazione nome programma a video?
025300150128         T01pgm = SDSpgm;
025400141110
025500150128         // -?Impostazione campi chiave fissi?
025600150128         $Video  = 'D1';
025700150128         $InzD01 = *on;
025800150128
025900150128         // -?Reperimento data odierna?
026000150128         wOggi = %int( %subst( %char( %dec( %timestamp() ) )
026100150128                               : 1 : 8 ) );
026200150128
026300150128         // -?Reperimento dati job?
026400150128         exsr  sr_DatiJob;
026500150128
026600150128         // -?Pulizia parametri di solo Output?
026700150129         clear  oKC06err;
026800150128         clear  oKC06msg;
026900150205
027000150205         // -?Controllo Sistema Informativo?
027100150205         If  DUTlpo <> 'S';
027200150205           $Fine    = *on;
027300150205           oKC06err = *on;
027400150205           oKC06msg = sk_Msg(01);
027500150205           leavesr;
027600150205         EndIf;
027700141111
027800150128         // -?Controllo Abilitazioni Utente?
027900150128         clear  TNTAA1ds;
028000150128         iTAA1cAut = '§utegtc';
028100150128         kpjbu = TNTAA1ds;
028200150128         TNTAA1C ( kpjba );
028300150128         TNTAA1ds = kpjbu;
028400141111
028500150128         If  oTAA1fabi = 'N';
028600150128           $Fine    = *on;
028700150128           oKC06err = *on;
028800150205           oKC06msg = sk_Msg(02);
028900141111           leavesr;
029000150128         EndIf;
029100150129         wAbi = oTAA1cAbi;
029200150128
029300150128         // -?Controllo dei parametri ricevuti?
029400150128         If  iKC06ncm = *zero;
029500150128           $Fine    = *on;
029600150128           oKC06err = *on;
029700150205           oKC06msg = sk_Msg(03);
029800141110           leavesr;
029900150128         EndIf;
030000150128
030100150128         chain  ( iKC06ncm )  TICMP000;
030200150128         If  NOT %found(TICMP01L)  or  CMPdfc < wOggi;
030300150128           $Fine    = *on;
030400150128           oKC06err = *on;
030500150205           oKC06msg = sk_Msg(03);
030600141110           leavesr;
030700150128         EndIf;
030800141110
030900141110       ENDSR;
031000141110
031100141110       //--------------------------------------------------------------
031200150128       //?Reperimento Dati del job (Utente/Operativi).                 ?
031300141110       //--------------------------------------------------------------
031400150128       BEGSR  sr_DatiJob;
031500141110
031600150128         in(e) §AzUte;
031700150128         if NOT %error;
031800150128           in(e) §DatiUte;
031900150128         endif;
032000150128         if %error or RSut = *blank;
032100150128           tibs34r ( tibs34ds );
032200150128           in §AzUte;
032300150128           in §DatiUte;
032400150128         endif;
032500141110
032600141110       ENDSR;
032700141110
032800141110       //--------------------------------------------------------------
032900150128       //?Gestione videata D01.                                        ?
033000141110       //--------------------------------------------------------------
033100150129       BEGSR  sr_GesD01;
033200141110
033300150128         // -?Inizializzazione videata?
033400150128         if  $InzD01 = *on;
033500150128           exsr  sr_InzD01;
033600150128           $InzD01 = *off;
033700150128         endif;
033800141110
033900150128         // -?Emissione Testata & Piede con tasti funzionali abilitati?
034000150112         write  KC06T01;
034100150128         write  KC06P01;
034200141110
034300150128         // -?Emissione videata?
034400150128         exfmt  KC06D01;
034500150128
034600150128         clear  V01msg;
034700150128         reset  ErrMessage;
034800150128         reset  ErrGenerico;
034900141110
035000141110         SELECT;
035100150128
035200150128           // -?F3=Fine?
035300150128           WHEN  dsp_aid = c_F03;
035400150128             $Fine = *on;
035500141110
035600150128           // -?Invio?
035700150128           OTHER;
035800150128             exsr  sr_CtrD01;
035900150128             if  ErrGenerico;
036000141110               leavesr;
036100150128             endif;
036200150128             $Video  = 'D2';
036300150128             $InzD02 = *on;
036400150128
036500150128         ENDSL;
036600150128
036700150128       ENDSR;
036800150128
036900150128       //--------------------------------------------------------------
037000150128       //?Inizializzazione videata D01.                                ?
037100150128       //--------------------------------------------------------------
037200150128       BEGSR  sr_InzD01;
037300150128
037400150128         clear  KC06D01;
037500150128
037600150129         // -?Numero Campagna?
037700150128         V01ncm = iKC06ncm;
037800150128
037900150129         // -?Decodifica Campagna?
038000150128         chain  ( iKC06ncm )  TICMP000;
038100150128         If  %found(TICMP01L);
038200150128           V01des = CMPdes;
038300150128           wDate_EUR = %date( CMPdic : *ISO );
038400150128           V01dic = %dec( wDate_Eur );
038500150128           wDate_EUR = %date( CMPdfc : *ISO );
038600150128           V01dfc = %dec( wDate_Eur );
038700150128           V01tpc = CMPtpc;
038800150128         Else;
038900150128           V01des = *all'? ';
039000150128         EndIf;
039100150129
039200150129         // -?Codice Cliente Unificante?
039300150129         V01ksu = '?';
039400150128
039500150128         // -?(Dis)Abilitazione Tasti Funzionali?
039600150128         F6Attivo  = *off;
039700150128         F12Attivo = *off;
039800150128
039900150128       ENDSR;
040000150128
040100150128       //--------------------------------------------------------------
040200150128       //?Controllo dati in videata D01.                               ?
040300150128       //--------------------------------------------------------------
040400150128       BEGSR  sr_CtrD01;
040500150128
040600150128         // -?Spegnimento degli indicatori di errore?
040700150128         %subst(IndDspF : 50) = *off;
040800150128
040900150128         clear  V01rag;
041000150128
041100150129         // -?Controllo codice Cliente Unificante?
041200150128         Select;
041300150128           // ·?Controllo inserimento?
041400150128           When  V01ksu = *blank  or  V01ksu = *zero;
041500150205             PosCurKSU   = *on;
041600150128             ErrMessage  = *on;
041700150128             ErrGenerico = *on;
041800150205             V01msg = sk_Msg(04);
041900150128             leavesr;
042000150128           // ·?Ricerca?
042100150128           When  %scan( '?' : V01ksu ) > *zero;
042200150128             clear  V01ksu;
042300150205             PosCurKSU   = *on;
042400150129             ErrGenerico = *on;
042500150128             clear  TNTAI1ds;
042600150128             iTAI1ric = 'S';
042700150129             iTAI1abi = oTAA1cAbi;
042800150128             iTAI1aut = '*C';
042900150128             tntaI1r ( kpjba : TNTAI1ds );
043000150128             select;
043100150128               when  oTAI1f03 <> *blank  or  oTAI1f12 <> *blank;
043200150128               when  oTAI1err <> *blank;
043300150128                 ErrMessage  = *on;
043400150129                 V01msg = oTAI1msg;
043500150129               other;
043600150129                 V01ksu = %editc( oTAI1ksc : 'X' );
043700150128             endsl;
043800150129             leavesr;
043900150128           // ·?Controllo numericità?
044000150128           When  %check( c_Digits : V01ksu ) > *zero;
044100150205             PosCurKSU   = *on;
044200150128             ErrMessage  = *on;
044300150128             ErrGenerico = *on;
044400150205             V01msg = sk_Msg(04);
044500150128             leavesr;
044600150129         EndSl;
044700150129
044800150129         // -?Controllo esistenza?
044900150129         clear  TIBS69ds;
045000150129         I69sif = knsif;
045100150129         I69kcc = DUTkci;
045200150129         I69kac = %int( V01ksu );
045300150129         I69kin = I69kac;
045400150129         I69kcp = I69kac;
045500150129         tibs69r( tibs69ds :
045600150129                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
045700150129         if  O69err = *on;
045800150205           PosCurKSU   = *on;
045900150129           ErrMessage  = *on;
046000150205           ErrGenerico = *on;
046100150205           V01msg = sk_Msg(04);
046200150129           leavesr;
046300150129         endif;
046400150129
046500150129         V01rag  = ACOrag;
046600150129
046700150806         // -?Controllo SE cliente già presente in Campagna attiva ?
046800150206         clear  keyTICMC02;
046900150129         k_CMCksu = I69kac;
047000150806         setll %kds(keyTICMC02:1) TICMC02L;
047100150806         reade %kds(keyTICMC02:1) TICMC02L;
047200150806         DOW  not %eof(TICMC02L);
047300150806           chain (CMCncm) TICMP01L;
047400150806           IF  %found(TICMP01L) and CMPdfc < wOggi;
047500150806             PosCurKSU   = *on;
047600150806             ErrMessage  = *on;
047700150806             ErrGenerico = *on;
047800150806             V01msg = %trimR( sk_Msg(04) ) + ': già presente in Campagna '
047900150806                    + %trim( %editc( CMCncm : '2' ) );
048000150806             leavesr;
048100150806           ENDIF;
048200150806           reade %kds(keyTICMC02:1) TICMC02L;
048300150806         ENDDO;
048400150129
048500150129         // -?Controllo SE cliente Unificante?
048600150129         clear  TIBS10ds;
048700150129         D10tle = 'ST';
048800150129         D10paf = 'P';
048900150129         D10cod = I69kac;
049000150129         TIBS10R ( TIBS10ds );
049100150129         // ·?Se NON ritorna errore: vuol dire che è un codice Figlio?
049200150129         if  D10err = *blank;
049300150205           PosCurKSU   = *on;
049400150129           ErrMessage  = *on;
049500150129           ErrGenerico = *on;
049600150205           V01msg = %trim( sk_Msg(04) ) + ': deve essere un codice Padre';
049700150129           leavesr;
049800150129         endif;
049900150128
050000150128       ENDSR;
050100150129
050200150129       //--------------------------------------------------------------
050300150129       //?Gestione videata D02.                                        ?
050400150129       //--------------------------------------------------------------
050500150129       BEGSR  sr_GesD02;
050600150129
050700150129         // -?Inizializzazione videata?
050800150129         if  $InzD02 = *on;
050900150129           exsr  sr_InzD02;
051000150129           $InzD02 = *off;
051100150129         endif;
051200150129
051300150129         if  Not ErrMessage;
051400150129           // -?Emissione Testata & Piede con tasti funzionali abilitati?
051500150129           write  KC06T01;
051600150129           write  KC06P01;
051700150129           // -?Emissione videata precedente (protetta)?
051800150129           write  KC06D01;
051900150129           write  Protect;
052000150129         endif;
052100150129
052200150129         // -?Emissione videata?
052300150129         exfmt  KC06D02;
052400150129
052500150129         clear  V01msg;
052600150129         reset  ErrMessage;
052700150129         reset  ErrGenerico;
052800150129
052900150129         SELECT;
053000150129
053100150129           // -?F3=Fine?
053200150129           WHEN  dsp_aid = c_F03;
053300150129             $Fine = *on;
053400150129
053500150129           // -?F12=Ritorno?
053600150129           WHEN  dsp_aid = c_F12;
053700150129             $Video = 'D1';
053800150129
053900150129           // -?Invio / F6=Conferma?
054000150129           OTHER;
054100150129             exsr  sr_CtrD02;
054200150129             if  ErrGenerico  or  dsp_aid <> c_F06;
054300150129               leavesr;
054400150129             endif;
054500150129             exsr  sr_F06D02;
054600150129             $Video  = 'D1';
054700150129             $InzD01 = *on;
054800150129
054900150129         ENDSL;
055000150129
055100150129       ENDSR;
055200150129
055300150129       //--------------------------------------------------------------
055400150129       //?Inizializzazione videata D02.                                ?
055500150129       //--------------------------------------------------------------
055600150129       BEGSR  sr_InzD02;
055700150129
055800150129         clear  KC06D02;
055900150129
056000150129         // -?Altri dati del Cliente Unificante?
056100150129         V02ind  = INDvia;
056200150129         V02cap  = INDcae;
056300150129         V02loc  = INDcit;
056400150129         V02prv  = INDprv;
056500150129         V02naz  = INDsta;
056600150129         //V02cmm  = CLPage;  ?(serve il comm.le Unificante)?
056700150224         V02clv  = CLPclv;
056800150129
056900150224         // -?Reperisce e Decodifica il Comm.le Unificante?
057000150224         V02cmmD = *all'? ';
057100150224         k_CMMcod = CLPage;
057200150224         chain  %kds( keyAZCMM01 )  AZCMM000;
057300150224
057400150224         IF  %found(AZCMM01L)  and  CMMdtFin >= wOggi;
057500150224
057600150224           k_CMMcod = CMMuni;
057700150224           chain  %kds( keyAZCMM01 )  AZCMM000;
057800150224           If  %found(AZCMM01L)  and  CMMdtFin >= wOggi;
057900150224
058000150224             V02cmm  = CMMuni;
058100150224             V02cmmD = CMMdes;
058200150224             k_ORGfil = CMMuni / 10000;
058300150224             chain  %kds( keyAZORG01 )  AZORG;
058400150224             if  %found(AZORG01L);
058500150224               V02car  = ORGcar;    // -?Area?
058600150224               V02dis  = ORGfl3;    // -?Distretto?
058700150224             endif;
058800150224
058900150224           EndIf;
059000150224
059100150224         ENDIF;
059200150129
059300150224         // -?Segno Obiettivo Iniziale?
059400150129         V02peaS = '+';
059500150129
059600150129         // -?(Dis)Abilitazione Tasti Funzionali?
059700150129         F6Attivo  = *on;
059800150129         F12Attivo = *on;
059900141110
060000141110       ENDSR;
060100150129
060200150129       //--------------------------------------------------------------
060300150129       //?Controllo dati in videata D02.                               ?
060400150129       //--------------------------------------------------------------
060500150129       BEGSR  sr_CtrD02;
060600150129
060700150129         // -?Spegnimento degli indicatori di errore?
060800150129         %subst(IndDspF : 50) = *off;
060900150205
061000150205         // -?Flaq Nuovo/Acquisito:?
061100150205         if  V02cln <> 'A'  and  V02cln <> 'N';
061200150205           ErrGenerico = *on;
061300150205           ErrMessage  = *on;
061400150205           PosCurCLN   = *on;
061500150205           V01msg = %trim( sk_Msg(05) ) + ' "A"=Acquisito, "N"=Nuovo';
061600150205           leavesr;
061700150205         endif;
061800150129
061900150129         // -?Mese Acquisizione:?
062000150129         select;
062100150129           // -?obbligatorio SE cliente Nuovo?
062200150129           when  V02cln  = 'N'  and  ( V02newMes < 1  or
062300150129                                       V02newMes > 12 );
062400150129             ErrGenerico = *on;
062500150129             ErrMessage  = *on;
062600150129             PosCurMES   = *on;
062700150205             V01msg = sk_Msg(06);
062800150129             leavesr;
062900150129           // -?non richiesto SE cliente Acquisito?
063000150129           when  V02cln  = 'A'  and  V02newMes <> *zero;
063100150129             ErrGenerico = *on;
063200150129             ErrMessage  = *on;
063300150129             PosCurMES   = *on;
063400150205             V01msg = sk_Msg(07);
063500150129             leavesr;
063600150129         endsl;
063700150205
063800150206         // -?Segno Ricavi errato?
063900150206         if  V02ricS <> '+'  and  V02ricS <> '-';
064000150206           ErrGenerico = *on;
064100150206           ErrMessage  = *on;
064200150206           PosCurRICS  = *on;
064300150206           V01msg = %trim( sk_Msg(05) ) + ' "+"=Valore Positivo, +
064400150206                                            "-"=Valore Negativo';
064500150206           leavesr;
064600150206         endif;
064700150206         // -?Valore dei Ricavi obbligatorio?
064800150206         select;
064900150206           when  V02ric = *zero;
065000150206             ErrGenerico = *on;
065100150206             ErrMessage  = *on;
065200150206             PosCurRIC   = *on;
065300150206             V01msg = sk_Msg(08);
065400150206             leavesr;
065500150206           when  V02ric < *zero;
065600150206             V02ric = %abs(V02ric);
065700150206         endsl;
065800150129
065900150205         // -?Num. Spedizioni   (può essere SOLO positivo)?
066000150205         select;
066100150205           when  V02spe < *zero;
066200150205             V02spe  = %abs( V02spe );
066300150205           when  V02spe = *zero;
066400150205             ErrGenerico = *on;
066500150205             ErrMessage  = *on;
066600150205             PosCurSPE   = *on;
066700150205             V01msg = sk_Msg(09);
066800150205             leavesr;
066900150205         endsl;
067000150205
067100150205         // -?Peso Medio        (può essere SOLO positivo)?
067200150205         select;
067300150205           when  V02pkg = *zero;
067400150205             ErrGenerico = *on;
067500150205             ErrMessage  = *on;
067600150205             PosCurPKG   = *on;
067700150205             V01msg = sk_Msg(10);
067800150205             leavesr;
067900150206           when  V02pkg < *zero;
068000150206             V02pkg  = %abs( V02pkg );
068100150205         endsl;
068200150205
068300150206         // -?Segno Delta errato?
068400150206         if  V02delS <> '+'  and  V02delS <> '-';
068500150206           ErrGenerico = *on;
068600150206           ErrMessage  = *on;
068700150206           PosCurDELS  = *on;
068800150206           V01msg = %trim( sk_Msg(05) ) + ' "+"=Valore Positivo, +
068900150206                                            "-"=Valore Negativo';
069000150206           leavesr;
069100150206         endif;
069200150206         // -?Delta obbligatorio?
069300150206         select;
069400150206           when  V02del = *zero;
069500150206             ErrGenerico = *on;
069600150206             ErrMessage  = *on;
069700150206             PosCurDEL   = *on;
069800150206             V01msg = sk_Msg(11);
069900150206             leavesr;
070000150206           when  V02del < *zero;
070100150206             V02del = %abs(V02del);
070200150206         endsl;
070300150129
070400150129         // -?Istat?
070500150129         if  V02ist <> 'S'  and  V02ist <> 'N';
070600150129           ErrGenerico = *on;
070700150129           ErrMessage  = *on;
070800150129           PosCurIST   = *on;
070900150205           V01msg = %trim( sk_Msg(05) ) + ' "S"=Sì, "N"=No';
071000150129           leavesr;
071100150129         endif;
071200150129
071300150206         // -?Segno Obiettivo errato?
071400150206         if  V02peaS <> '+'  and  V02peaS <> '-';
071500150206           ErrGenerico = *on;
071600150206           ErrMessage  = *on;
071700150206           PosCurPEAS  = *on;
071800150206           V01msg = %trim( sk_Msg(05) ) + ' "+"=Valore Positivo, +
071900150206                                            "-"=Valore Negativo';
072000150206           leavesr;
072100150206         endif;
072200150206         // -?Obiettivo obbligatorio?
072300150129         select;
072400150129           when  V02pea = *zero;
072500150129             ErrGenerico = *on;
072600150129             ErrMessage  = *on;
072700150129             PosCurPEA   = *on;
072800150205             V01msg = sk_Msg(12);
072900150129             leavesr;
073000150129           when  V02pea < *zero;
073100150129             V02pea = %abs(V02pea);
073200150129         endsl;
073300141110
073400141110       ENDSR;
073500141110
073600141110       //--------------------------------------------------------------
073700150129       //?Gestione tasto funzionale F06 da videata D02:                ?
073800150129       //?F6=Conferma                                                  ?
073900141110       //--------------------------------------------------------------
074000150129       BEGSR  sr_F06D02;
074100141110
074200150129         // -?Aggiunta del cliente nel file INFO clienti?
074300150129         clear  TICMI000;
074400150129         CMIncm = V01ncm;
074500141110         CMIksu = I69kac;
074600141110         CMIclv = V02clv;
074700141110         CMIcmm = V02cmm;
074800141110         CMIdcm = V02dis;
074900141110         CMIacm = V02car;
075000150129         CMIfcm = k_ORGfil;
075100150213         CMIpde = V02del;
075200150213         if  V02delS = '-';
075300150213           CMIpde *= -1;
075400150213         endif;
075500141110         CMInsp = V02spe;
075600141110         CMIric = V02ric;
075700141110         CMIpme = V02pkg;
075800141110         CMIist = V02ist;
075900141110         CMIcln = V02cln;
076000150129         CMImes = V02newMes;
076100150129         //______________
076200150129         write  TICMI000;
076300150129         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
076400150129         feod   TICMI00F;
076500141110
076600150129         // -?Aggiunta del cliente nel file Clienti in Campagna?
076700150129         clear  TICMC000;
076800150129         CMCncm = V01ncm;
076900141110         CMCksu = I69kac;
077000150129         CMCpru = KNMUS;
077100150128         CMCdin = wOggi;
077200150129         //______________
077300150129         write  TICMC000;
077400150129         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
077500150206         feod   TICMC02L;
077600141110
077700150129         // -?Scrittura della Fase " 10" (Automatica)?
077800150129         clear  TRKC71ds;
077900150129         iKC71ncm = V01ncm;
078000150129         iKC71ksu = I69kac;
078100150129         iKC71acm = c_FaseApertura;
078200150129         iKC71pea = V02pea;
078300150129         if  V02peaS = '-';
078400150129           iKC71pea *= -1;
078500150129         endif;
078600150129         iKC71aut = 'A';
078700150129
078800150129         TRKC71R ( kpjba : TRKC71ds );
078900141110
079000141110       ENDSR;
079100141110
079200141110       //--------------------------------------------------------------
079300150129       //?Operazioni finali.                                           ?
079400141110       //--------------------------------------------------------------
079500150129       BEGSR  sr_RoutEnd;
079600150129
079700150129         // -?Uscita dal *pgm?
079800141110         return;
079900141110
080000141110       ENDSR;
080100141110
080200141110      /end-free
080300141110
080400141110       //--------------------------------------------------------------
080500141110       //?Schiere a tempo di compilazione.
080600141110       //--------------------------------------------------------------
080700141110
080800150128** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
080900150205Funzione eseguibile solo dalla SEDE                                            1
081000150205Utente non Abilitato alla funzione                                             2
081100150205Numero Campagna errato                                                         3
081200150205Cliente errato                                                                 4
081300150205Valore errato:                                                                 5
081400150205Mese acquisizione errato o mancante                                            6
081500150205Mese acquisizione NON richiesto                                                7
081600150205Immettere il valore dei Ricavi                                                 8
081700150205Immettere il numero delle Spedizioni                                           9
081800150205Immettere il Peso Medio                                                       10
081900150205Immettere il Delta                                                            11
082000150205Immettere l'Obiettivo                                                         12
