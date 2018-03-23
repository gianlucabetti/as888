000100150127       //==============================================================
000200150127       //?TRKC06R * Inserimento Cliente in Campagna Commerciale.       ?
000300150127       //==============================================================
000400150127
000500150127       //--------------------------------------------------------------
000600150127       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700150127       //--------------------------------------------------------------
000800150127
000900150127     /*PRM  dbgview(*source)
001000150127     /*END
001100150127
001200150127       //--------------------------------------------------------------
001300150127       //?Specifiche di controllo.                                     ?
001400150127       //--------------------------------------------------------------
001500141110
001600150224     h decedit('0,')  datedit(*dmy/)  option(*nodebugio)
001700141110     h dftactgrp(*no) actgrp(*caller)
001800141110
001900150127       //--------------------------------------------------------------
002000150127       //?Dichiarazione file.                                          ?
002100150127       //--------------------------------------------------------------
002200141110
002300150127       // -?Organigramma?
002400141110     fAZORG01L  if   e           k disk
002500150224
002600150224       // -?Anagrafica Commerciali?
002700150224     fAZCMM01L  if   e           k disk
002800141110
002900150127       // -?Anagrafica Campagne Commerciali?
003000141110     fTICMP01L  if   e           k disk
003100141110
003200150127       // -?Clienti in Campagna?
003300150206     fTICMC02L  if A e           k disk
003400141110
003500150127       // -?Info Clienti in Campagna?
003600150127     fTICMI00F  O    e             disk
003700141110
003800150127       // -?Video Gestione?
003900150112     fTRKC06D   cf   e             workstn
004000141110     f                                     indds(IndDspF)
004100141110     f                                     infds(InfDspF)
004200141110
004300150127       //--------------------------------------------------------------
004400150127       //?Definizione costanti.                                        ?
004500150127       //--------------------------------------------------------------
004600150127
004700150127       // -?Fase di Apertura Campagna comm.le per il Cliente?
004800150127     d c_FaseApertura  c                   const(' 10')
004900150127
005000150127       // -?Costante per controllo "caratteri solo numerici"?
005100150127     d c_Digits        c                   const('0123456789')
005200141110
005300150127       // -?Tasti funzionali a video?
005400141110     d c_F01           c                   const(x'31')
005500141110     d c_F02           c                   const(x'32')
005600141110     d c_F03           c                   const(x'33')
005700141110     d c_F04           c                   const(x'34')
005800141110     d c_F05           c                   const(x'35')
005900141110     d c_F06           c                   const(x'36')
006000141110     d c_F07           c                   const(x'37')
006100141110     d c_F08           c                   const(x'38')
006200141110     d c_F09           c                   const(x'39')
006300141110     d c_F10           c                   const(x'3A')
006400141110     d c_F11           c                   const(x'3B')
006500141110     d c_F12           c                   const(x'3C')
006600141110     d c_F13           c                   const(x'B1')
006700141110     d c_F14           c                   const(x'B2')
006800141110     d c_F15           c                   const(x'B3')
006900141110     d c_F16           c                   const(x'B4')
007000141110     d c_F17           c                   const(x'B5')
007100141110     d c_F18           c                   const(x'B6')
007200141110     d c_F19           c                   const(x'B7')
007300141110     d c_F20           c                   const(x'B8')
007400141110     d c_F21           c                   const(x'B9')
007500141110     d c_F22           c                   const(x'BA')
007600141110     d c_F23           c                   const(x'BB')
007700141110     d c_F24           c                   const(x'BC')
007800141110     d c_Enter         c                   const(x'F1')
007900141110     d c_RollDown      c                   const(x'F4')
008000141110     d c_RollUp        c                   const(x'F5')
008100141110
008200150127       //--------------------------------------------------------------
008300150127       //?Definizione schiere.                                         ?
008400150127       //--------------------------------------------------------------
008500141110
008600150127       // -?Messaggi di errore?
008700150205     d sk_Msg          s             78    dim(12)  ctdata  perrcd(1)
008800141110
008900150127       //--------------------------------------------------------------
009000150127       //?Definizione aree dati.                                       ?
009100150127       //--------------------------------------------------------------
009200141110
009300150127       // -?Dati utente?
009400141110     d §AzUte        e ds                  extname(AZUTE00F)
009500141110     d                                     dtaara
009600141110     d §DatiUte      e ds                  extname(dDatiUte)
009700141110     d                                     dtaara
009800141110
009900150127       //--------------------------------------------------------------
010000150127       //?Definizione strutture dati.                                  ?
010100150127       //--------------------------------------------------------------
010200141110
010300150127       // -?Status?
010400150127     d Status         sds
010500141110     d   SDSpgm          *proc
010600150127     d*//SDSprm          *parms
010700150127     d*//SDSusr              254    263
010800141110
010900150127       // -?InfDS?
011000141110     d InfDspF         ds
011100150127     d   dsp_aid             369    369a
011200150127     d*//sfl_rrn             376    377i 0
011300150127     d*//min_nrr             378    379i 0
011400150127     d*//num_rcds            380    381i 0
011500141110
011600150127       // -?Indicatori su DspF?
011700141110     d IndDspF         ds
011800150127         // -?Abilitazione tasti funzionali?
011900150127     d   F6Attivo                      n   overlay(IndDspF : 06)
012000150127     d   F12Attivo                     n   overlay(IndDspF : 12)
012100150127         // -?Emissione messaggio di errore?
012200150127     d   ErrMessage                    n   overlay(IndDspF : 28)
012300150127         // -?Posizionamento cursore & segnalazione errore?
012400150127     d   PosCurKSU                     n   overlay(IndDspF : 50)
012500150127     d   PosCurCLN                     n   overlay(IndDspF : 51)
012600150127     d   PosCurMES                     n   overlay(IndDspF : 52)
012700150206     d   PosCurRICS                    n   overlay(IndDspF : 53)
012800150206     d   PosCurRIC                     n   overlay(IndDspF : 54)
012900150206     d   PosCurSPE                     n   overlay(IndDspF : 55)
013000150206     d   PosCurPKG                     n   overlay(IndDspF : 56)
013100150206     d   PosCurDELS                    n   overlay(IndDspF : 57)
013200150206     d   PosCurDEL                     n   overlay(IndDspF : 58)
013300150206     d   PosCurIST                     n   overlay(IndDspF : 59)
013400150206     d   PosCurPEAS                    n   overlay(IndDspF : 60)
013500150206     d   PosCurPEA                     n   overlay(IndDspF : 61)
013600150127         // -?Riemissione videata?
013700150127     d   ErrGenerico                   n   overlay(IndDspF : 99)
013800141110
013900150127       // -?Parametri ricevuti?
014000141110     d KPJBA         e ds
014100150127     d TRKC06ds      e ds
014200141110
014300141110      //---------------------------------------------------------------
014400141110      //?Definizione variabili globali.
014500141110      //---------------------------------------------------------------
014600141110
014700150128       // -?Flags booleani?
014800150128     d $Err            s               n   inz
014900150128     d $Fine           s               n   inz
015000150128     d $InzD01         s               n   inz(*on)
015100150128     d $InzD02         s               n   inz(*on)
015200141110
015300150128       // -?Indici di schiera?
015400150128     d xx              s              3  0 inz
015500141110
015600150128       // -?Campi associati al video?
015700150128     d $Video          s              2    inz('D1')
015800141110
015900150128       // -?Campi di Comodo?
016000150128     d wDate_EUR       s               d   datfmt(*EUR)     inz
016100150128     d wOggi           s              8  0 inz
016200150128     d wTotCli         s              5i 0 inz
016300150128     d wAbi            s                   like(oTAA1cabi)  inz
016400141110
016500150128       //--------------------------------------------------------------
016600150128       //?Definizione prototipi/procedure usate.                       ?
016700150128       //--------------------------------------------------------------
016800150128
016900150128       // -?Reperimento dati utente?
017000150128     d TIBS34ds      e ds                  inz
017100150128      /copy gaitrasrc/srcProtoPR,TIBS34R
017200150128
017300150128       // -?Ricerca Unificante Padre?
017400150128     d TIBS10ds      e ds                  inz
017500150128      /copy gaitrasrc/srcprotopr,TIBS10R
017600150128
017700150128       // -?Reperimento dati Anagrafica Clienti?
017800150128      /copy gaitrasrc/srcprotopi,TIBS69R
017900150128      /copy gaitrasrc/srcprotopr,TIBS69R
018000150128
018100150128       // -?Controllo Abilitazioni Utente?
018200150128     d TNTAA1ds      e ds                  inz
018300150128      /copy gaitrasrc/srcprotopr,TNTAA1C
018400141110
018500150128       // -?Ricerca Anagrafica Clienti?
018600150128     d TNTAI1ds      e ds                  inz
018700150128     d tntaI1R         pr                  extpgm('TNTAI1R')
018800150128     d   kpjba                             likeds(kpjba)
018900150128     d   tntaI1ds                          likeds(TNTAI1ds)
019000150128
019100150128       // -?Scrittura Fase per Cliente in Campagna?
019200150128     d TRKC71ds      e ds                  inz
019300150128     d trkc71R         pr                  extpgm('TRKC71R')
019400150128     d   kpjba                             likeds(kpjba)
019500150128     d   trkc71ds                          likeds(TRKC71ds)
019600141110
019700150128       //--------------------------------------------------------------
019800150128       //?Definizione key-list.                                        ?
019900150128       //--------------------------------------------------------------
020000150129
020100150206       // -?File TICMC02L?
020200150206     d keyTICMC02    e ds                  extname(TICMC02L : *key)
020300150129     d                                     prefix(k_)   inz
020400150224
020500150224       // -?File AZCMM01L?
020600150224     d keyAZCMM01    e ds                  extname(AZCMM01L : *key)
020700150224     d                                     prefix(k_)   inz
020800150129
020900150129       // -?File AZORG01L?
021000150129     d keyAZORG01    e ds                  extname(AZORG01L : *key)
021100150129     d                                     prefix(k_)   inz
021200141110
021300150128       //--------------------------------------------------------------
021400150128       //?M A I N - L I N E                                            ?
021500150128       //--------------------------------------------------------------
021600141110
021700141110     c     *Entry        plist
021800150129     c                   parm                    KPJBA
021900150129     c                   parm                    TRKC06ds
022000141110
022100141110      /free
022200141110
022300150128       // -?Operazioni iniziali?
022400150128       exsr  sr_RoutInz;
022500141110
022600150128       // -?Ciclo di gestione del file video?
022700150128       DoW  $Fine = *off;
022800150128
022900150128         select;
023000150128
023100150128           // -?Gestione videata D1 (richiesta cod. Unificante)?
023200150128           when  $Video = 'D1';
023300150128             exsr  sr_GesD01;
023400150128
023500150128           // -?Gestione videata D1 (richiesta dati Unificante)?
023600150128           when  $Video = 'D2';
023700150128             exsr  sr_GesD02;
023800150128
023900150128           // -?? ? ??
024000150128           other;
024100150128             $Fine = *on;
024200150128
024300150128         endsl;
024400150128
024500150128       EndDo;
024600141110
024700150128       // -?Operazioni finali?
024800150128       exsr  sr_RoutEnd;
024900141110
025000150128       //--------------------------------------------------------------
025100150128       //?Operazioni iniziali.                                         ?
025200150128       //--------------------------------------------------------------
025300150128       BEGSR  sr_RoutInz;
025400141110
025500141110         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
025600150128
025700150128         // -?Impostazione chiusura?
025800150128         *inLR = *on;
025900150128
026000150128         // -?Impostazione nome programma a video?
026100150128         T01pgm = SDSpgm;
026200141110
026300150128         // -?Impostazione campi chiave fissi?
026400150128         $Video  = 'D1';
026500150128         $InzD01 = *on;
026600150128
026700150128         // -?Reperimento data odierna?
026800150128         wOggi = %int( %subst( %char( %dec( %timestamp() ) )
026900150128                               : 1 : 8 ) );
027000150128
027100150128         // -?Reperimento dati job?
027200150128         exsr  sr_DatiJob;
027300150128
027400150128         // -?Pulizia parametri di solo Output?
027500150129         clear  oKC06err;
027600150128         clear  oKC06msg;
027700150205
027800150205         // -?Controllo Sistema Informativo?
027900150205         If  DUTlpo <> 'S';
028000150205           $Fine    = *on;
028100150205           oKC06err = *on;
028200150205           oKC06msg = sk_Msg(01);
028300150205           leavesr;
028400150205         EndIf;
028500141111
028600150128         // -?Controllo Abilitazioni Utente?
028700150128         clear  TNTAA1ds;
028800150128         iTAA1cAut = '§utegtc';
028900150128         kpjbu = TNTAA1ds;
029000150128         TNTAA1C ( kpjba );
029100150128         TNTAA1ds = kpjbu;
029200141111
029300150128         If  oTAA1fabi = 'N';
029400150128           $Fine    = *on;
029500150128           oKC06err = *on;
029600150205           oKC06msg = sk_Msg(02);
029700141111           leavesr;
029800150128         EndIf;
029900150129         wAbi = oTAA1cAbi;
030000150128
030100150128         // -?Controllo dei parametri ricevuti?
030200150128         If  iKC06ncm = *zero;
030300150128           $Fine    = *on;
030400150128           oKC06err = *on;
030500150205           oKC06msg = sk_Msg(03);
030600141110           leavesr;
030700150128         EndIf;
030800150128
030900150128         chain  ( iKC06ncm )  TICMP000;
031000150128         If  NOT %found(TICMP01L)  or  CMPdfc < wOggi;
031100150128           $Fine    = *on;
031200150128           oKC06err = *on;
031300150205           oKC06msg = sk_Msg(03);
031400141110           leavesr;
031500150128         EndIf;
031600141110
031700141110       ENDSR;
031800141110
031900141110       //--------------------------------------------------------------
032000150128       //?Reperimento Dati del job (Utente/Operativi).                 ?
032100141110       //--------------------------------------------------------------
032200150128       BEGSR  sr_DatiJob;
032300141110
032400150128         in(e) §AzUte;
032500150128         if NOT %error;
032600150128           in(e) §DatiUte;
032700150128         endif;
032800150128         if %error or RSut = *blank;
032900150128           tibs34r ( tibs34ds );
033000150128           in §AzUte;
033100150128           in §DatiUte;
033200150128         endif;
033300141110
033400141110       ENDSR;
033500141110
033600141110       //--------------------------------------------------------------
033700150128       //?Gestione videata D01.                                        ?
033800141110       //--------------------------------------------------------------
033900150129       BEGSR  sr_GesD01;
034000141110
034100150128         // -?Inizializzazione videata?
034200150128         if  $InzD01 = *on;
034300150128           exsr  sr_InzD01;
034400150128           $InzD01 = *off;
034500150128         endif;
034600141110
034700150128         // -?Emissione Testata & Piede con tasti funzionali abilitati?
034800150112         write  KC06T01;
034900150128         write  KC06P01;
035000141110
035100150128         // -?Emissione videata?
035200150128         exfmt  KC06D01;
035300150128
035400150128         clear  V01msg;
035500150128         reset  ErrMessage;
035600150128         reset  ErrGenerico;
035700141110
035800141110         SELECT;
035900150128
036000150128           // -?F3=Fine?
036100150128           WHEN  dsp_aid = c_F03;
036200150128             $Fine = *on;
036300141110
036400150128           // -?Invio?
036500150128           OTHER;
036600150128             exsr  sr_CtrD01;
036700150128             if  ErrGenerico;
036800141110               leavesr;
036900150128             endif;
037000150128             $Video  = 'D2';
037100150128             $InzD02 = *on;
037200150128
037300150128         ENDSL;
037400150128
037500150128       ENDSR;
037600150128
037700150128       //--------------------------------------------------------------
037800150128       //?Inizializzazione videata D01.                                ?
037900150128       //--------------------------------------------------------------
038000150128       BEGSR  sr_InzD01;
038100150128
038200150128         clear  KC06D01;
038300150128
038400150129         // -?Numero Campagna?
038500150128         V01ncm = iKC06ncm;
038600150128
038700150129         // -?Decodifica Campagna?
038800150128         chain  ( iKC06ncm )  TICMP000;
038900150128         If  %found(TICMP01L);
039000150128           V01des = CMPdes;
039100150128           wDate_EUR = %date( CMPdic : *ISO );
039200150128           V01dic = %dec( wDate_Eur );
039300150128           wDate_EUR = %date( CMPdfc : *ISO );
039400150128           V01dfc = %dec( wDate_Eur );
039500150128           V01tpc = CMPtpc;
039600150128         Else;
039700150128           V01des = *all'? ';
039800150128         EndIf;
039900150129
040000150129         // -?Codice Cliente Unificante?
040100150129         V01ksu = '?';
040200150128
040300150128         // -?(Dis)Abilitazione Tasti Funzionali?
040400150128         F6Attivo  = *off;
040500150128         F12Attivo = *off;
040600150128
040700150128       ENDSR;
040800150128
040900150128       //--------------------------------------------------------------
041000150128       //?Controllo dati in videata D01.                               ?
041100150128       //--------------------------------------------------------------
041200150128       BEGSR  sr_CtrD01;
041300150128
041400150128         // -?Spegnimento degli indicatori di errore?
041500150128         %subst(IndDspF : 50) = *off;
041600150128
041700150128         clear  V01rag;
041800150128
041900150129         // -?Controllo codice Cliente Unificante?
042000150128         Select;
042100150128           // ·?Controllo inserimento?
042200150128           When  V01ksu = *blank  or  V01ksu = *zero;
042300150205             PosCurKSU   = *on;
042400150128             ErrMessage  = *on;
042500150128             ErrGenerico = *on;
042600150205             V01msg = sk_Msg(04);
042700150128             leavesr;
042800150128           // ·?Ricerca?
042900150128           When  %scan( '?' : V01ksu ) > *zero;
043000150128             clear  V01ksu;
043100150205             PosCurKSU   = *on;
043200150129             ErrGenerico = *on;
043300150128             clear  TNTAI1ds;
043400150128             iTAI1ric = 'S';
043500150129             iTAI1abi = oTAA1cAbi;
043600150128             iTAI1aut = '*C';
043700150128             tntaI1r ( kpjba : TNTAI1ds );
043800150128             select;
043900150128               when  oTAI1f03 <> *blank  or  oTAI1f12 <> *blank;
044000150128               when  oTAI1err <> *blank;
044100150128                 ErrMessage  = *on;
044200150129                 V01msg = oTAI1msg;
044300150129               other;
044400150129                 V01ksu = %editc( oTAI1ksc : 'X' );
044500150128             endsl;
044600150129             leavesr;
044700150128           // ·?Controllo numericità?
044800150128           When  %check( c_Digits : V01ksu ) > *zero;
044900150205             PosCurKSU   = *on;
045000150128             ErrMessage  = *on;
045100150128             ErrGenerico = *on;
045200150205             V01msg = sk_Msg(04);
045300150128             leavesr;
045400150129         EndSl;
045500150129
045600150129         // -?Controllo esistenza?
045700150129         clear  TIBS69ds;
045800150129         I69sif = knsif;
045900150129         I69kcc = DUTkci;
046000150129         I69kac = %int( V01ksu );
046100150129         I69kin = I69kac;
046200150129         I69kcp = I69kac;
046300150129         tibs69r( tibs69ds :
046400150129                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
046500150129         if  O69err = *on;
046600150205           PosCurKSU   = *on;
046700150129           ErrMessage  = *on;
046800150205           ErrGenerico = *on;
046900150205           V01msg = sk_Msg(04);
047000150129           leavesr;
047100150129         endif;
047200150129
047300150129         V01rag  = ACOrag;
047400150129
047500150129         // -?Controllo SE cliente già presente in Campagna?
047600150206         clear  keyTICMC02;
047700150129         k_CMCksu = I69kac;
047800150206         //setll  %kds( keyTICMC02 : 1 )  TICMC000;
047900150206         //if  %equal(TICMC02L);
048000150206         chain  %kds( keyTICMC02 : 1 )  TICMC000;
048100150206         if  %found(TICMC02L);
048200150129           PosCurKSU   = *on;
048300150129           ErrMessage  = *on;
048400150129           ErrGenerico = *on;
048500150206           V01msg = %trimR( sk_Msg(04) ) + ': già presente in Campagna '
048600150206                  + %trim( %editc( CMCncm : '2' ) );
048700150129           leavesr;
048800150129         endif;
048900150129
049000150129         // -?Controllo SE cliente Unificante?
049100150129         clear  TIBS10ds;
049200150129         D10tle = 'ST';
049300150129         D10paf = 'P';
049400150129         D10cod = I69kac;
049500150129         TIBS10R ( TIBS10ds );
049600150129         // ·?Se NON ritorna errore: vuol dire che è un codice Figlio?
049700150129         if  D10err = *blank;
049800150205           PosCurKSU   = *on;
049900150129           ErrMessage  = *on;
050000150129           ErrGenerico = *on;
050100150205           V01msg = %trim( sk_Msg(04) ) + ': deve essere un codice Padre';
050200150129           leavesr;
050300150129         endif;
050400150128
050500150128       ENDSR;
050600150129
050700150129       //--------------------------------------------------------------
050800150129       //?Gestione videata D02.                                        ?
050900150129       //--------------------------------------------------------------
051000150129       BEGSR  sr_GesD02;
051100150129
051200150129         // -?Inizializzazione videata?
051300150129         if  $InzD02 = *on;
051400150129           exsr  sr_InzD02;
051500150129           $InzD02 = *off;
051600150129         endif;
051700150129
051800150129         if  Not ErrMessage;
051900150129           // -?Emissione Testata & Piede con tasti funzionali abilitati?
052000150129           write  KC06T01;
052100150129           write  KC06P01;
052200150129           // -?Emissione videata precedente (protetta)?
052300150129           write  KC06D01;
052400150129           write  Protect;
052500150129         endif;
052600150129
052700150129         // -?Emissione videata?
052800150129         exfmt  KC06D02;
052900150129
053000150129         clear  V01msg;
053100150129         reset  ErrMessage;
053200150129         reset  ErrGenerico;
053300150129
053400150129         SELECT;
053500150129
053600150129           // -?F3=Fine?
053700150129           WHEN  dsp_aid = c_F03;
053800150129             $Fine = *on;
053900150129
054000150129           // -?F12=Ritorno?
054100150129           WHEN  dsp_aid = c_F12;
054200150129             $Video = 'D1';
054300150129
054400150129           // -?Invio / F6=Conferma?
054500150129           OTHER;
054600150129             exsr  sr_CtrD02;
054700150129             if  ErrGenerico  or  dsp_aid <> c_F06;
054800150129               leavesr;
054900150129             endif;
055000150129             exsr  sr_F06D02;
055100150129             $Video  = 'D1';
055200150129             $InzD01 = *on;
055300150129
055400150129         ENDSL;
055500150129
055600150129       ENDSR;
055700150129
055800150129       //--------------------------------------------------------------
055900150129       //?Inizializzazione videata D02.                                ?
056000150129       //--------------------------------------------------------------
056100150129       BEGSR  sr_InzD02;
056200150129
056300150129         clear  KC06D02;
056400150129
056500150129         // -?Altri dati del Cliente Unificante?
056600150129         V02ind  = INDvia;
056700150129         V02cap  = INDcae;
056800150129         V02loc  = INDcit;
056900150129         V02prv  = INDprv;
057000150129         V02naz  = INDsta;
057100150129         //V02cmm  = CLPage;  ?(serve il comm.le Unificante)?
057200150224         V02clv  = CLPclv;
057300150129
057400150224         // -?Reperisce e Decodifica il Comm.le Unificante?
057500150224         V02cmmD = *all'? ';
057600150224         k_CMMcod = CLPage;
057700150224         chain  %kds( keyAZCMM01 )  AZCMM000;
057800150224
057900150224         IF  %found(AZCMM01L)  and  CMMdtFin >= wOggi;
058000150224
058100150224           k_CMMcod = CMMuni;
058200150224           chain  %kds( keyAZCMM01 )  AZCMM000;
058300150224           If  %found(AZCMM01L)  and  CMMdtFin >= wOggi;
058400150224
058500150224             V02cmm  = CMMuni;
058600150224             V02cmmD = CMMdes;
058700150224             k_ORGfil = CMMuni / 10000;
058800150224             chain  %kds( keyAZORG01 )  AZORG;
058900150224             if  %found(AZORG01L);
059000150224               V02car  = ORGcar;    // -?Area?
059100150224               V02dis  = ORGfl3;    // -?Distretto?
059200150224             endif;
059300150224
059400150224           EndIf;
059500150224
059600150224         ENDIF;
059700150129
059800150224         // -?Segno Obiettivo Iniziale?
059900150129         V02peaS = '+';
060000150129
060100150129         // -?(Dis)Abilitazione Tasti Funzionali?
060200150129         F6Attivo  = *on;
060300150129         F12Attivo = *on;
060400141110
060500141110       ENDSR;
060600150129
060700150129       //--------------------------------------------------------------
060800150129       //?Controllo dati in videata D02.                               ?
060900150129       //--------------------------------------------------------------
061000150129       BEGSR  sr_CtrD02;
061100150129
061200150129         // -?Spegnimento degli indicatori di errore?
061300150129         %subst(IndDspF : 50) = *off;
061400150205
061500150205         // -?Flaq Nuovo/Acquisito:?
061600150205         if  V02cln <> 'A'  and  V02cln <> 'N';
061700150205           ErrGenerico = *on;
061800150205           ErrMessage  = *on;
061900150205           PosCurCLN   = *on;
062000150205           V01msg = %trim( sk_Msg(05) ) + ' "A"=Acquisito, "N"=Nuovo';
062100150205           leavesr;
062200150205         endif;
062300150129
062400150129         // -?Mese Acquisizione:?
062500150129         select;
062600150129           // -?obbligatorio SE cliente Nuovo?
062700150129           when  V02cln  = 'N'  and  ( V02newMes < 1  or
062800150129                                       V02newMes > 12 );
062900150129             ErrGenerico = *on;
063000150129             ErrMessage  = *on;
063100150129             PosCurMES   = *on;
063200150205             V01msg = sk_Msg(06);
063300150129             leavesr;
063400150129           // -?non richiesto SE cliente Acquisito?
063500150129           when  V02cln  = 'A'  and  V02newMes <> *zero;
063600150129             ErrGenerico = *on;
063700150129             ErrMessage  = *on;
063800150129             PosCurMES   = *on;
063900150205             V01msg = sk_Msg(07);
064000150129             leavesr;
064100150129         endsl;
064200150205
064300150206         // -?Segno Ricavi errato?
064400150206         if  V02ricS <> '+'  and  V02ricS <> '-';
064500150206           ErrGenerico = *on;
064600150206           ErrMessage  = *on;
064700150206           PosCurRICS  = *on;
064800150206           V01msg = %trim( sk_Msg(05) ) + ' "+"=Valore Positivo, +
064900150206                                            "-"=Valore Negativo';
065000150206           leavesr;
065100150206         endif;
065200150206         // -?Valore dei Ricavi obbligatorio?
065300150206         select;
065400150206           when  V02ric = *zero;
065500150206             ErrGenerico = *on;
065600150206             ErrMessage  = *on;
065700150206             PosCurRIC   = *on;
065800150206             V01msg = sk_Msg(08);
065900150206             leavesr;
066000150206           when  V02ric < *zero;
066100150206             V02ric = %abs(V02ric);
066200150206         endsl;
066300150129
066400150205         // -?Num. Spedizioni   (può essere SOLO positivo)?
066500150205         select;
066600150205           when  V02spe < *zero;
066700150205             V02spe  = %abs( V02spe );
066800150205           when  V02spe = *zero;
066900150205             ErrGenerico = *on;
067000150205             ErrMessage  = *on;
067100150205             PosCurSPE   = *on;
067200150205             V01msg = sk_Msg(09);
067300150205             leavesr;
067400150205         endsl;
067500150205
067600150205         // -?Peso Medio        (può essere SOLO positivo)?
067700150205         select;
067800150205           when  V02pkg = *zero;
067900150205             ErrGenerico = *on;
068000150205             ErrMessage  = *on;
068100150205             PosCurPKG   = *on;
068200150205             V01msg = sk_Msg(10);
068300150205             leavesr;
068400150206           when  V02pkg < *zero;
068500150206             V02pkg  = %abs( V02pkg );
068600150205         endsl;
068700150205
068800150206         // -?Segno Delta errato?
068900150206         if  V02delS <> '+'  and  V02delS <> '-';
069000150206           ErrGenerico = *on;
069100150206           ErrMessage  = *on;
069200150206           PosCurDELS  = *on;
069300150206           V01msg = %trim( sk_Msg(05) ) + ' "+"=Valore Positivo, +
069400150206                                            "-"=Valore Negativo';
069500150206           leavesr;
069600150206         endif;
069700150206         // -?Delta obbligatorio?
069800150206         select;
069900150206           when  V02del = *zero;
070000150206             ErrGenerico = *on;
070100150206             ErrMessage  = *on;
070200150206             PosCurDEL   = *on;
070300150206             V01msg = sk_Msg(11);
070400150206             leavesr;
070500150206           when  V02del < *zero;
070600150206             V02del = %abs(V02del);
070700150206         endsl;
070800150129
070900150129         // -?Istat?
071000150129         if  V02ist <> 'S'  and  V02ist <> 'N';
071100150129           ErrGenerico = *on;
071200150129           ErrMessage  = *on;
071300150129           PosCurIST   = *on;
071400150205           V01msg = %trim( sk_Msg(05) ) + ' "S"=Sì, "N"=No';
071500150129           leavesr;
071600150129         endif;
071700150129
071800150206         // -?Segno Obiettivo errato?
071900150206         if  V02peaS <> '+'  and  V02peaS <> '-';
072000150206           ErrGenerico = *on;
072100150206           ErrMessage  = *on;
072200150206           PosCurPEAS  = *on;
072300150206           V01msg = %trim( sk_Msg(05) ) + ' "+"=Valore Positivo, +
072400150206                                            "-"=Valore Negativo';
072500150206           leavesr;
072600150206         endif;
072700150206         // -?Obiettivo obbligatorio?
072800150129         select;
072900150129           when  V02pea = *zero;
073000150129             ErrGenerico = *on;
073100150129             ErrMessage  = *on;
073200150129             PosCurPEA   = *on;
073300150205             V01msg = sk_Msg(12);
073400150129             leavesr;
073500150129           when  V02pea < *zero;
073600150129             V02pea = %abs(V02pea);
073700150129         endsl;
073800141110
073900141110       ENDSR;
074000141110
074100141110       //--------------------------------------------------------------
074200150129       //?Gestione tasto funzionale F06 da videata D02:                ?
074300150129       //?F6=Conferma                                                  ?
074400141110       //--------------------------------------------------------------
074500150129       BEGSR  sr_F06D02;
074600141110
074700150129         // -?Aggiunta del cliente nel file INFO clienti?
074800150129         clear  TICMI000;
074900150129         CMIncm = V01ncm;
075000141110         CMIksu = I69kac;
075100141110         CMIclv = V02clv;
075200141110         CMIcmm = V02cmm;
075300141110         CMIdcm = V02dis;
075400141110         CMIacm = V02car;
075500150129         CMIfcm = k_ORGfil;
075600150213         CMIpde = V02del;
075700150213         if  V02delS = '-';
075800150213           CMIpde *= -1;
075900150213         endif;
076000141110         CMInsp = V02spe;
076100141110         CMIric = V02ric;
076200141110         CMIpme = V02pkg;
076300141110         CMIist = V02ist;
076400141110         CMIcln = V02cln;
076500150129         CMImes = V02newMes;
076600150129         //______________
076700150129         write  TICMI000;
076800150129         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
076900150129         feod   TICMI00F;
077000141110
077100150129         // -?Aggiunta del cliente nel file Clienti in Campagna?
077200150129         clear  TICMC000;
077300150129         CMCncm = V01ncm;
077400141110         CMCksu = I69kac;
077500150129         CMCpru = KNMUS;
077600150128         CMCdin = wOggi;
077700150129         //______________
077800150129         write  TICMC000;
077900150129         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
078000150206         feod   TICMC02L;
078100141110
078200150129         // -?Scrittura della Fase " 10" (Automatica)?
078300150129         clear  TRKC71ds;
078400150129         iKC71ncm = V01ncm;
078500150129         iKC71ksu = I69kac;
078600150129         iKC71acm = c_FaseApertura;
078700150129         iKC71pea = V02pea;
078800150129         if  V02peaS = '-';
078900150129           iKC71pea *= -1;
079000150129         endif;
079100150129         iKC71aut = 'A';
079200150129
079300150129         TRKC71R ( kpjba : TRKC71ds );
079400141110
079500141110       ENDSR;
079600141110
079700141110       //--------------------------------------------------------------
079800150129       //?Operazioni finali.                                           ?
079900141110       //--------------------------------------------------------------
080000150129       BEGSR  sr_RoutEnd;
080100150129
080200150129         // -?Uscita dal *pgm?
080300141110         return;
080400141110
080500141110       ENDSR;
080600141110
080700141110      /end-free
080800141110
080900141110       //--------------------------------------------------------------
081000141110       //?Schiere a tempo di compilazione.
081100141110       //--------------------------------------------------------------
081200141110
081300150128** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
081400150205Funzione eseguibile solo dalla SEDE                                            1
081500150205Utente non Abilitato alla funzione                                             2
081600150205Numero Campagna errato                                                         3
081700150205Cliente errato                                                                 4
081800150205Valore errato:                                                                 5
081900150205Mese acquisizione errato o mancante                                            6
082000150205Mese acquisizione NON richiesto                                                7
082100150205Immettere il valore dei Ricavi                                                 8
082200150205Immettere il numero delle Spedizioni                                           9
082300150205Immettere il Peso Medio                                                       10
082400150205Immettere il Delta                                                            11
082500150205Immettere l'Obiettivo                                                         12
