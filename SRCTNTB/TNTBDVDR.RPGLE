000100180116       //--------------------------------------------------------------
000200180116       //?Gestione tabella "DVD" (Creazione Automatica DVD)      ?
000300180116       //--------------------------------------------------------------
000400180116     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500180116
000600180116       //--------------------------------------------------------------
000700180116       //?ATTENZIONE: Compilare il file video con RSTDSP *YES     ?
000800180116       //--------------------------------------------------------------
000900180116
001000180116       //--------------------------------------------------------------
001100180116       //?Dichiarazione file.                                          ?
001200180116       //--------------------------------------------------------------
001300180116       // - Tabelle
001400180116     fTABEL00F  if   e           k disk
001500180116     fTNTBE01L  uf a e           k disk
001600180116
001700180116       // - Video Gestione
001800180116     fTNTBDVDD  cf   e             workstn
001900180116     f                                     indds(IndDspF)
002000180116     f                                     infds(InfDspF)
002100180116
002200180116       //--------------------------------------------------------------
002300180116       //?Definizione costanti.                                        ?
002400180116       //--------------------------------------------------------------
002500180116       // - Codice tabella in gestione
002600180116     d c_Tab           c                   const('DVD')
002700180116
002800180116       // - Codice utente in vecchie tabelle (TABEL00F)
002900180116     d c_Kut           c                   const(1)
003000180116
003100180116       // - Tasti funzionali a video
003200180116     d c_F01           c                   const(x'31')
003300180116     d c_F02           c                   const(x'32')
003400180116     d c_F03           c                   const(x'33')
003500180116     d c_F05           c                   const(x'35')
003600180116     d c_F06           c                   const(x'36')
003700180116     d c_F07           c                   const(x'37')
003800180116     d c_F08           c                   const(x'38')
003900180116     d c_F09           c                   const(x'39')
004000180116     d c_F10           c                   const(x'3A')
004100180116     d c_F12           c                   const(x'3C')
004200180116     d c_F13           c                   const(x'B1')
004300180116     d c_F14           c                   const(x'B2')
004400180116     d c_F15           c                   const(x'B3')
004500180116     d c_F16           c                   const(x'B4')
004600180116     d c_F17           c                   const(x'B5')
004700180116     d c_F18           c                   const(x'B6')
004800180116     d c_F19           c                   const(x'B7')
004900180116     d c_F20           c                   const(x'B8')
005000180116     d c_F21           c                   const(x'B9')
005100180116     d c_F22           c                   const(x'BA')
005200180116     d c_F23           c                   const(x'BB')
005300180116     d c_F24           c                   const(x'BC')
005400180116     d c_Enter         c                   const(x'F1')
005500180116     d c_RollDown      c                   const(x'F4')
005600180116     d c_RollUp        c                   const(x'F5')
005700180116
005800180116       //--------------------------------------------------------------
005900180116       //?Definizione schiere.                                         ?
006000180116       //--------------------------------------------------------------
006100180116       // - Messaggi di errore
006200180116     d Msg             s             78    dim(20)  ctdata  perrcd(01)
006300180116
006400180116       //--------------------------------------------------------------
006500180116       //?Definizione aree dati.                                       ?
006600180116       //--------------------------------------------------------------
006700180116       // - Dati utente
006800180116     d §AzUte        e ds                  extname(AZUTE00F)
006900180116     d                                     dtaara
007000180116     d §DatiUte      e ds                  extname(dDatiUte)
007100180116     d                                     dtaara
007200180116
007300180116       //--------------------------------------------------------------
007400180116       //?Definizione strutture dati.                                  ?
007500180116       //--------------------------------------------------------------
007600180116       // - Status ds
007700180116     d Status         sds
007800180116     d  SDSpgm           *proc
007900180116
008000180116       // - InfDS
008100180116     d InfDspF         ds
008200180116     d   dsp_aid             369    369a
008300180116
008400180116       // - Indicatori su DspF
008500180116     d IndDspF         ds                  inz
008600180116       // - Abilitazione tasti funzionali
008700180116     d  F5Attivo                       n   overlay(IndDspF : 05)
008800180116     d  F6Attivo                       n   overlay(IndDspF : 06)
008900180116     d  F16Attivo                      n   overlay(IndDspF : 16)
009000180116       // - Emissione messaggio di errore
009100180116     d  ErrMessage                     n   overlay(IndDspF : 28)
009200180116       // - Posizionamento cursore & segnalazione errore
009300180116     d  PosCurDVD                      n   overlay(IndDspF : 50)
009400180116     d  PosCurCOVER                    n   overlay(IndDspF : 51)
009500180116     d  PosCurROOT                     n   overlay(IndDspF : 52)
009600180116       //   - Riemissione videata
009700180116     d  ErrGenerico                    n   overlay(IndDspF : 99)
009800180116
009900180116       // - Parametri ricevuti
010000180116     d KPJBA         e ds
010100180116
010200180116       // - Parametri per Reperimento dati utente
010300180116     d TIBS34DS      e ds
010400180116
011000180116       // - Tab. DVD = Creazione automatica DVD
011100180116     d dDVD          e ds                  inz
011200180116
011300180116       // - Parametri per Interrogazione/Selezione tab. "DVD"
011400180116     d TNTBDVDDS     e ds                  inz
011500180116
011600180116       // - Dati record principale della tabella
011700180116     d TNTBEds       e ds                  inz  extname(TNTBE00F)
011800180116     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
011900180116     d                                          prefix(TBX:3)
012000180116
012100180116       //--------------------------------------------------------------
012200180116       //?Definizione variabili globali.                               ?
012300180116       //--------------------------------------------------------------
012400180116       // - Flags booleani
012500180116     d Fine            s               n   inz
012600180116     d wInzD01         s               n   inz(*on)
012700180116     d wInzD02         s               n   inz(*on)
012800180116     d wInzW01         s               n   inz(*on)
012900180116     d Annullamento    s               n   inz
013000180116     d Immissione      s               n   inz
013100180116     d Ripristino      s               n   inz
013200180116
013300180116       // - Indici di schiera
013400180116     d xx              s              3  0 inz
013500180116
013600180116       // - Variabili per la gestione del video
013700180116     d Video           s              2    inz('D1')
013800180116
013900180116       // - Flag di errore
014000180116     d Err             s              1    inz(*off)
014100180116
014200180116       // - Campi di comodo
014300180116     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
014400180116
014500180116       // - Parametri per pgm interrogazione tabelle
014600180116      /copy gaitrasrc/srcProtoPI,X§TABER
014700180116
014800180116       //--------------------------------------------------------------
014900180116       //?Definizione prototipi procedure.                             ?
015000180116       //--------------------------------------------------------------
015100180116       // - Reperimento dati utente
015200180116      /copy gaitrasrc/srcProtoPR,TIBS34R
015300180116
015400180116       // - Ricerca vecchie tabelle
015500180116      /copy gaitrasrc/srcProtoPR,X§TABER
015600180116
016000180116       // - Selezione tipo assenza
016100180116     d tntbdvdr1       pr                  extpgm('TNTBDVDR1')
016200180116     d  kpjba                              likeds(KPJBA)
016300180116     d  tntbdvdds                          likeds(TNTBDVDDS)
016400180116
016500180116       //--------------------------------------------------------------
016600180116       //?Definizione key-list.                                        ?
016700180116       //--------------------------------------------------------------
016800180116       // - File TABEL00F
016900180116     d k03tabel00    e ds                  extname(TABEL00F : *key)
017000180116     d                                     prefix(k_)   inz
017100180116
017200180116       // - File TNTBE01L
017300180116     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
017400180116     d                                     prefix(k_)   inz
017500180116
017600180116       //--------------------------------------------------------------
017700180116       //?Riepilogo indicatori utilizzati.                             ?
017800180116       //--------------------------------------------------------------
017900180116
018000180116       //--------------------------------------------------------------
018100180116
018200180116       //--------------------------------------------------------------
018300180116       //?M A I N - L I N E                                            ?
018400180116       //--------------------------------------------------------------
018500180116     c     *ENTRY        plist
018600180116     c                   parm                    KPJBA
018700180116
018800180116       // Operazioni iniziali
018900180116       exsr RoutInz;
019000180116
019100180116       // Gestione video?
019200180116       DOW  not Fine;
019300180116         SELECT;
019400180116         // Manutenzione singola tabella
019500180116         WHEN  Video = 'D2';
019600180116           exsr GesD02;
019700180116         // Filtro di lancio
019800180116         WHEN  Video = 'D1';
019900180116           exsr GesD01;
020000180116         // Dati da copiare in tab. "DVD"
020100180116         WHEN  Video = 'W1';
020200180116           exsr GesW01;
020300180116         OTHER;
020400180116           Fine = *on;
020500180116         ENDSL;
020600180116       ENDDO;
020700180116
020800180116       // Operazioni finali?
020900180116       exsr RoutEnd;
021000180116
021100180116       //--------------------------------------------------------------
021200180116       //?Operazioni iniziali.                                         ?
021300180116       //--------------------------------------------------------------
021400180116       BEGSR RoutInz;
021500180116
021600180116         *inLR = *on;
021700180116
021800180116       // Reperimento dati job?
021900180116         exsr DatiJob;
022000180116
022100180116       // Impostazione nome programma a video?
022200180116         V1Tpgm = SDSpgm;
022300180116
022400180116       // Aggancio dati generali della tabella in esame?
022500180116         clear  k_TBEcod;
022600180116         k_TBEke1 = *zero;
022700180116         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
022800180116                = c_Tab;
022900180116         clear  k_TBEke2;
023000180116         clear  k_TBElin;
023100180116         k_TBEsif = KNSIF;
023200180116         chain(n) %kds(k05tntbe01)  TNTBE01L;
023300180116         IF  not %found(TNTBE01L);
023400180116           clear k_TBEsif;
023500180116           chain(n) %kds(k05tntbe01)  TNTBE01L;
023600180116         ENDIF;
023700180116         IF  %found(TNTBE01L);
023800180116           xTNTBEds = TNTBEds;
023900180116         ELSE;
024000180116           clear xTNTBEds;
024100180116         ENDIF;
024200180116
024300180116       // Impostazione iniziale / pulizia dei campi chiave?
024400180116         k_TBEcod = c_Tab;
024500180116         clear k_TBEke1;
024600180116         clear k_TBEke2;
024700180116         clear k_TBElin;
024800180116         clear k_TBEsif;
024900180116
025000180116       // 1ª inizializzazione della videata D01?
025100180116         exsr InzD01;
025200180116         V1Cdvd  = '?';
025300180116         wInzD01 = *off;
025400180116
025500180116       ENDSR;
025600180116
025700180116       //--------------------------------------------------------------
025800180116       //?Reperimento Dati del job (Utente/Operativi).                 ?
025900180116       //--------------------------------------------------------------
026000180116       BEGSR DatiJob;
026100180116
026200180116         in(E) §AzUte;
026300180116         IF  NOT %error;
026400180116           in(E) §DatiUte;
026500180116         ENDIF;
026600180116         IF  %error or RSut = *blanks;
026700180116           clear TIBS34ds;
026800180116           tibs34r ( tibs34ds );
026900180116           in §AzUte;
027000180116           in §DatiUte;
027100180116         ENDIF;
027200180116
027300180116       ENDSR;
027400180116
027500180116       //--------------------------------------------------------------
027600180116       //?Gestione videata D01                                         ?
027700180116       //--------------------------------------------------------------
027800180116       BEGSR GesD01;
027900180116
028000180116       // Inizializzazione videata?
028100180116         IF  wInzD01;
028200180116           exsr InzD01;
028300180116           wInzD01 = *off;
028400180116         ENDIF;
028500180116
028600180116       // Emissione videata?
028700180116         write TBDVDT01;
028800180116         write TBDVDP01;
028900180116         exfmt TBDVDD01;
029000180116
029100180116         reset ErrMessage;
029200180116         reset ErrGenerico;
029300180116         clear V1Dmsg;
029400180116
029500180116         SELECT;
029600180116
029700180116       // F3=Fine?
029800180116         WHEN  dsp_aid = c_F03;
029900180116           Fine = *on;
030000180116
030100180116       // Invio?
030200180116         OTHER;
030300180116           exsr CtrD01;
030400180116           IF  not ErrGenerico;
030500180116             Video   = 'D2';
030600180116             wInzD02 = *on;
030700180116           ENDIF;
030800180116
030900180116         ENDSL;
031000180116
031100180116       ENDSR;
031200180116
031300180116       //--------------------------------------------------------------
031400180116       //?Inizializzazione videata D01                                 ?
031500180116       //--------------------------------------------------------------
031600180116       BEGSR InzD01;
031700180116
031800180116         clear TBDVDD01;
031900180116
032000180116       ENDSR;
032100180116
032200180116       //--------------------------------------------------------------
032300180116       //?Controllo dati immessi nella videata D01                     ?
032400180116       //--------------------------------------------------------------
032500180116       BEGSR CtrD01;
032600180116
032700180116         %subst(IndDspF : 28) = *off;
032800180116
032900180116         SELECT;
033000180116
033100180116       // Ricerca codice Task DVD ?
033200180116         WHEN  %scan('?' : V1Cdvd) > *zeros;
033300180116           clear TNTBDVDds;
033400180116           tntbdvdr1 (kpjba:tntbdvdds);
033500180116           SELECT;
033600180116           WHEN ODVDfxx <> *blank;
033700180116           WHEN ODVDerr = *on;
033800180116             ErrMessage = *on;
033900180116             PosCurDVD  = *on;
034000180116             V1Dmsg = ODVDmsg;
034100180116           OTHER;
034200180116             V1Cdvd = %subst(ODVDke1:1:%len(V1Cdvd));
034300180116           ENDSL;
034400180116           ErrGenerico = *on;
034500180116           leavesr;
034600180116
034700180116       // Controllo immissione codice TASK DVD
034800180116         WHEN  V1Cdvd = *blank;
034900180116           ErrMessage  = *on;
035000180116           ErrGenerico = *on;
035100180116           PosCurDVD   = *on;
035200180116           V1Dmsg = Msg(01);
035300180116           leavesr;
035400180116
035500180116         ENDSL;
035600180116
035700180116       ENDSR;
035800180116
035900180116       //--------------------------------------------------------------
036000180116       //?Gestione videata D02                                         ?
036100180116       //--------------------------------------------------------------
036200180116       BEGSR GesD02;
036300180116
036400180116       // Inizializzazione videata?
036500180116         IF  wInzD02;
036600180116           exsr InzD02;
036700180116           wInzD02 = *off;
036800180116         ENDIF;
036900180116
037000180116       // Emissione Testata e Piede con tasti funzionali abilitati?
037100180116         IF  ErrGenerico = *off;
037200180116           write TBDVDT01;
037300180116           write TBDVDD01;
037400180116           write Protect;
037500180116           write TBDVDP02;
037600180116         ENDIF;
037700180116
037800180116       // Emissione videata?
037900180116         exfmt TBDVDD02;
038000180116
038100180116         reset ErrMessage;
038200180116         reset ErrGenerico;
038300180116         clear V1Dmsg;
038400180116         reset Annullamento;
038500180116
038600180116         SELECT;
038700180116
038800180116       // F3=Fine?
038900180116         WHEN  dsp_aid = c_F03;
039000180116           unlock TNTBE01L;
039100180116           Fine = *on;
039200180116
039300180116       // F12=Ritorno?
039400180116         WHEN  dsp_aid = c_F12;
039500180116           unlock TNTBE01L;
039600180116           reset Video;
039700180116           clear V1Topz;
039800180116
039900180116       // Enter; F5=Ripristino; F6=Conferma; F16=Annullamento?
040000180116         OTHER;
040100180116           Ripristino   = (dsp_aid = c_F05);
040200180116           Annullamento = (dsp_aid = c_F16);
040300180116           IF  not Annullamento;
040400180116             exsr CtrD02;
040500180116             IF  ErrGenerico;
040600180116               leavesr;
040700180116             ENDIF;
040800180116           ENDIF;
040900180116           SELECT;
041000180116           WHEN  dsp_aid = c_F05  or
041100180116                 dsp_aid = c_F06  or
041200180116                 dsp_aid = c_F16;
041300180116             Video = 'W1';
041400180116             reset wInzW01;
041500180116           ENDSL;
041600180116
041700180116         ENDSL;
041800180116
041900180116       ENDSR;
042000180116
042100180116       //--------------------------------------------------------------
042200180116       //?Inizializzazione videata D02                                 ?
042300180116       //--------------------------------------------------------------
042400180116       BEGSR InzD02;
042500180116
042600180116       // Spegnimento di TUTTI gli indicatori?
042700180116         IndDspF = *off;
042800180116
042900180116       // Pulizia videata?
043000180116         clear TBDVDD02;
043100180116
043200180116       // Reperimento dati
043300180116         reset Immissione;
043400180116         reset Ripristino;
043500180116         k_TBEke1 = V1Cdvd;
043600180116         chain  %kds(k05tntbe01)  TNTBE000;
043700180116         SELECT;
043800180116         WHEN  not %found(TNTBE01L);
043900180116           Immissione = *on;
044000180116         WHEN  TBEatb <> *blank;
044100180116           Ripristino = *on;
044200180116         ENDSL;
044300180116
044400180116       // Caricamento dati
044500180116         IF  %found(TNTBE01L);
044600180116           exsr RieD02;
044700180116         ENDIF;
044800180116
044900180116       // Impostazione tipo manutenzione?
045000180116         SELECT;
045100180116         WHEN  Immissione;
045200180116           V1Topz = 'IMMISSIONE';
045300180116         WHEN  Ripristino;
045400180116           V1Topz = 'RIPRISTINO';
045500180116         OTHER;
045600180116           V1Topz = ' MODIFICA ';
045700180116         ENDSL;
045800180116
045900180116       // Impostazione indicatori per abilitazione tasti funzionali?
046000180116         exsr Set_Ind_D02;
046100180116
046200180116       ENDSR;
046300180116
046400180116       //--------------------------------------------------------------
046500180116       //?Caricamento singola riga di testo nella videata D02          ?
046600180116       //--------------------------------------------------------------
046700180116       BEGSR RieD02;
046800180116
046900180116         dDVD = TBEuni;
047000180116
047100180116         V1Ccover = §DVDcover;
047200180116         V1Croot1 = %subst(§DVDroot:1:50);
047300180116         V1Croot2 = %subst(§DVDroot:51:50);
047400180116
047500180116       ENDSR;
047600180116
047700180116       //--------------------------------------------------------------
047800180116       //?Settaggio indicatori nella videata D02 per abilitazione      ?
047900180116       //?  tasti funzionali (sul subfile control)                     ?
048000180116       //--------------------------------------------------------------
048100180116       BEGSR Set_Ind_D02;
048200180116
048300180116       // F5=Ripristino?
048400180116         F5Attivo = (not Immissione and Ripristino);
048500180116
048600180116       // F6=Conferma?
048700180116         F6Attivo = (not F5Attivo);
048800180116
048900180116       // F16=Annullamento?
049000180116         F16Attivo = (not Immissione and not Ripristino);
049100180116
049200180116       ENDSR;
049300180116
049400180116       //--------------------------------------------------------------
049500180116       //?Controllo dati videata D02                                   ?
049600180116       //--------------------------------------------------------------
049700180116       BEGSR  CtrD02;
049800180116
049900180116         %subst(IndDspF : 50) = *off;
050000180116
050100180116         SELECT;
050200180116
050300180116       // Cover
050400180116         WHEN  V1Ccover = *blank;
050500180116           ErrMessage  = *on;
050600180116           ErrGenerico = *on;
050700180116           PosCurCOVER = *on;
050800180116           V1Dmsg = Msg(02);
050900180116           leavesr;
051000180116
051100180116       // Root
051200180116         WHEN  V1Croot1 = *blank;
051300180116           ErrMessage  = *on;
051400180116           ErrGenerico = *on;
051500180116           PosCurROOT = *on;
051600180116           V1Dmsg = Msg(03);
051700180116           leavesr;
051800180116
051900180116         ENDSL;
052000180116
052100180116       ENDSR;
052200180116
052300180116       //--------------------------------------------------------------
052400180116       //?Gestione trasmissione aggiornamento                          ?
052500180116       //--------------------------------------------------------------
052600180116       BEGSR GesW01;
052700180116
052800180116       // Inizializzazione videata
052900180116         IF  wInzW01;
053000180116           exsr InzW01;
053100180116           wInzW01 = *off;
053200180116         ENDIF;
053300180116
053400180116       // Emissione window
053500180116         IF  TBXftt = 'S';
053600180116           exfmt TBDVDW01;
053700180116           ErrMessage  = *off;
053800180116           ErrGenerico = *off;
053900180116           clear V1Dmsg;
054000180116         ELSE;
054100180116           dsp_aid = c_F06;
054200180116         ENDIF;
054300180116
054400180116         SELECT;
054500180116
054600180116       // F12=Ritorno (gestito solo se emesso W01)
054700180116         WHEN  dsp_aid = c_F12;
054800180116           Video = 'D2';
054900180116
055000180116       // F6=Conferma
055100180116         WHEN  dsp_aid = c_F06;
055200180116           exsr UpdTNTBE;
055300180116         // Ritorno alla videata iniziale
055400180116           IF  not ErrGenerico;
055500180116             clear V1Topz;
055600180116             Video = 'D1';
055700180116             wInzD01 = *on;
055800180116           ENDIF;
055900180116         ENDSL;
056000180116
056100180116       ENDSR;
056200180116
056300180116       //--------------------------------------------------------------
056400180116       //?Inizializzazione videata W01                                 ?
056500180116       //--------------------------------------------------------------
056600180116       BEGSR InzW01;
056700180116
056800180116         clear TBDVDW01;
056900180116
057000180116         IF  Immissione;
057100180116
057200180116         // Se immissione
057300180116           W1ftt  = TBXftt;
057400180116
057500180116         ELSE;
057600180116
057700180116         // Se NON immissione:
057800180116         // visualizza i dati relativi all'ultima trasmissione
057900180116           W1ftt  = TBEftt;
058000180116           W1flt  = TBEflt;
058100180116           W1ftr  = TBEftr;
058200180116           IF  TBEdtr <> *zero;
058300180116             wDate_EUR = %date(TBEdtr : *iso);
058400180116             W1dtr     = %dec(wDate_EUR);
058500180116           ENDIF;
058600180116
058700180116         ENDIF;
058800180116
058900180116       ENDSR;
059000180116
059100180116       //--------------------------------------------------------------
059200180116       //?Aggiornamento records TNTBE00F (tab. "DVD")                  ?
059300180116       //--------------------------------------------------------------
059400180116       BEGSR  UpdTNTBE;
059500180116
059600180116       // Aggancio record di TNTBE00F?
059700180116         k_TBEcod = c_Tab;
059800180116         k_TBEke1 = V1Cdvd;
059900180116         clear k_TBEke2;
060000180116         clear k_TBElin;
060100180116         clear k_TBEsif;
060200180116
060300180116         chain  %kds(K05tntbe01) TNTBE01L;
060400180116
060500180116         exsr  RieDVD;
060600180116         TBEuni = dDVD;
060700180116
060800180116       // Aggiornamento tab. "DVD"?
060900180116         SELECT;
061000180116
061100180116       // F5=Ripristino?
061200180116         WHEN  Ripristino;
061300180116           clear TBEatb;
061400180116           TBEftt = W1ftt;
061500180116           clear TBEftr;
061600180116           //_______________
061700180116           UPDATE  TNTBE000;
061800180116           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
061900180116
062000180116       // F16=Annullamento?
062100180116         WHEN  Annullamento;
062200180116           TBEatb = 'A';
062300180116           TBEftt = W1ftt;
062400180116           clear TBEftr;
062500180116           //_______________
062600180116           UPDATE  TNTBE000;
062700180116           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
062800180116
062900180116       // Immissione o Modifica?
063000180116         OTHER;
063100180116           IF  not %found(TNTBE01L);
063200180116             clear TNTBE000;
063300180116             TBEcod = k_TBEcod;
063400180116             TBEke1 = k_TBEke1;
063500180116             TBEke2 = k_TBEke2;
063600180116             TBElin = k_TBElin;
063700180116             TBEsif = k_TBEsif;
063800180116             TBEuni = dDVD;
063900180116           ENDIF;
064000180116
064100180116           TBEapl = TBXapl;
064200180116           clear TBEatb;
064300180116           TBEftt = W1ftt;
064400180116           clear  TBEflt;
064500180116           clear  TBEftr;
064600180116
064700180116         // Aggiornamento record
064800180116           IF  %found(TNTBE01L);
064900180116             //_____________
065000180116             UPDATE TNTBE000;
065100180116             //¯¯¯¯¯¯¯¯¯¯¯¯¯
065200180116           ELSE;
065300180116             //_____________
065400180116             WRITE  TNTBE000;
065500180116             //¯¯¯¯¯¯¯¯¯¯¯¯¯
065600180116           ENDIF;
065700180116
065800180116         ENDSL;
065900180116
066000180116       ENDSR;
066100180116
066200180116       //--------------------------------------------------------------
066300180116       //?Caricamento dati in tab. "DVD"                               ?
066400180116       //--------------------------------------------------------------
066500180116       BEGSR RieDVD;
066600180116
066700180116         clear dDVD;
066800180116
066900180116         §DVDcover  = V1Ccover;
067000180116         %subst(§DVDroot:1:50) = V1Croot1;
067100180116         %subst(§DVDroot:51:50) = V1Croot2;
067200180116
067300180116       ENDSR;
067400180116
067500180116       //--------------------------------------------------------------
067600180116       //?Operazioni finali.                                           ?
067700180116       //--------------------------------------------------------------
067800180116       BEGSR RoutEnd;
067900180116
068500180116       // Chiusura pgm?
068600180116         return;
068700180116
068800180116       ENDSR;
068900180116
069000180116       //--------------------------------------------------------------
069100180116       //?Definizione schiere a tempo di compilazione                  ?
069200180116       //--------------------------------------------------------------
069300180116
069400180116** >  Msg -------------------------------------------------------------------*
069500180116Immettere il Codice Task del DVD                                                1
069600180116Tipo di Cover da utilizzare obbligatoria                                        2
069700180116Immettere il percorso (Root) dei dati                                           3
