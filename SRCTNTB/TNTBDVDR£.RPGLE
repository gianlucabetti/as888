000100160929       //--------------------------------------------------------------
000200161212       //?Gestione tabella "ATA" (Tipo assenza autisti)          ?
000300160929       //--------------------------------------------------------------
000400160929     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500160930
000600160930       //--------------------------------------------------------------
000700160930       //?ATTENZIONE: Compilare il file video con RSTDSP *YES     ?
000800160930       //--------------------------------------------------------------
000900160929
001000160929       //--------------------------------------------------------------
001100160929       //?Dichiarazione file.                                          ?
001200160929       //--------------------------------------------------------------
001300160929       // - Tabelle
001400160929     fTABEL00F  if   e           k disk
001500160929     fTNTBE01L  uf a e           k disk
001600160929
001700160929       // - Video Gestione
001800160929     fTNTBATAD  cf   e             workstn
001900160929     f                                     indds(IndDspF)
002000160929     f                                     infds(InfDspF)
002100160929
002200160929       //--------------------------------------------------------------
002300160929       //?Definizione costanti.                                        ?
002400160929       //--------------------------------------------------------------
002500160929       // - Codice tabella in gestione
002600160929     d c_Tab           c                   const('ATA')
002700160929
002800160929       // - Codice utente in vecchie tabelle (TABEL00F)
002900160929     d c_Kut           c                   const(1)
003000160929
003100160929       // - Tasti funzionali a video
003200160929     d c_F01           c                   const(x'31')
003300160929     d c_F02           c                   const(x'32')
003400160929     d c_F03           c                   const(x'33')
003500160929     d c_F05           c                   const(x'35')
003600160929     d c_F06           c                   const(x'36')
003700160929     d c_F07           c                   const(x'37')
003800160929     d c_F08           c                   const(x'38')
003900160929     d c_F09           c                   const(x'39')
004000160929     d c_F10           c                   const(x'3A')
004100160929     d c_F12           c                   const(x'3C')
004200160929     d c_F13           c                   const(x'B1')
004300160929     d c_F14           c                   const(x'B2')
004400160929     d c_F15           c                   const(x'B3')
004500160929     d c_F16           c                   const(x'B4')
004600160929     d c_F17           c                   const(x'B5')
004700160929     d c_F18           c                   const(x'B6')
004800160929     d c_F19           c                   const(x'B7')
004900160929     d c_F20           c                   const(x'B8')
005000160929     d c_F21           c                   const(x'B9')
005100160929     d c_F22           c                   const(x'BA')
005200160929     d c_F23           c                   const(x'BB')
005300160929     d c_F24           c                   const(x'BC')
005400160929     d c_Enter         c                   const(x'F1')
005500160929     d c_RollDown      c                   const(x'F4')
005600160929     d c_RollUp        c                   const(x'F5')
005700160929
005800160929       //--------------------------------------------------------------
005900160929       //?Definizione schiere.                                         ?
006000160929       //--------------------------------------------------------------
006100160929       // - Messaggi di errore
006200160929     d Msg             s             78    dim(20)  ctdata  perrcd(01)
006300160929
006400160929       //--------------------------------------------------------------
006500160929       //?Definizione aree dati.                                       ?
006600160929       //--------------------------------------------------------------
006700160929       // - Dati utente
006800160929     d §AzUte        e ds                  extname(AZUTE00F)
006900160929     d                                     dtaara
007000160929     d §DatiUte      e ds                  extname(dDatiUte)
007100160929     d                                     dtaara
007200160929
007300160929       //--------------------------------------------------------------
007400160929       //?Definizione strutture dati.                                  ?
007500160929       //--------------------------------------------------------------
007600160929       // - Status ds
007700160929     d Status         sds
007800160929     d  SDSpgm           *proc
007900160929
008000160929       // - InfDS
008100160929     d InfDspF         ds
008200160929     d   dsp_aid             369    369a
008300160929
008400160929       // - Indicatori su DspF
008500160929     d IndDspF         ds                  inz
008600160929       // - Abilitazione tasti funzionali
008700160929     d  F5Attivo                       n   overlay(IndDspF : 05)
008800160929     d  F6Attivo                       n   overlay(IndDspF : 06)
008900160929     d  F16Attivo                      n   overlay(IndDspF : 16)
009000160929       // - Emissione messaggio di errore
009100160929     d  ErrMessage                     n   overlay(IndDspF : 28)
009200160929       // - Posizionamento cursore & segnalazione errore
009300160929     d  PosCurATA                      n   overlay(IndDspF : 50)
009400160929     d  PosCurDES                      n   overlay(IndDspF : 51)
009500160929       //   - Riemissione videata
009600160929     d  ErrGenerico                    n   overlay(IndDspF : 99)
009700160929
009800160929       // - Parametri ricevuti
009900160929     d KPJBA         e ds
010000160929
010100160929       // - Parametri per Reperimento dati utente
010200160929     d TIBS34DS      e ds
010300160929
010400160929       // - Parametri per Ricerca/controllo tabelle
010500160929     d TIBS02DS      e ds                  inz
010600160929     d  T02mod       e                     inz('R')
010700160929     d  T02cod       e                     inz(c_Tab)
010800160929
010900161212       // - Tab. ATA = Tipo assenza autisti
011000160929     d dATA          e ds                  inz
011100160929
011200160929       // - Parametri per Interrogazione/Selezione tab. "ATA"
011300160929     d TNTBATADS     e ds                  inz
011400160929
011500160929       // - Dati record principale della tabella
011600160929     d TNTBEds       e ds                  inz  extname(TNTBE00F)
011700160929     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
011800160929     d                                          prefix(TBX:3)
011900160929
012000160929       //--------------------------------------------------------------
012100160929       //?Definizione variabili globali.                               ?
012200160929       //--------------------------------------------------------------
012300160929       // - Flags booleani
012400160929     d Fine            s               n   inz
012500160929     d wInzD01         s               n   inz(*on)
012600160929     d wInzD02         s               n   inz(*on)
012700160929     d wInzW01         s               n   inz(*on)
012800160929     d Annullamento    s               n   inz
012900160929     d Immissione      s               n   inz
013000160929     d Ripristino      s               n   inz
013100160929
013200160929       // - Indici di schiera
013300160929     d xx              s              3  0 inz
013400160929
013500160929       // - Variabili per la gestione del video
013600160929     d Video           s              2    inz('D1')
013700160929
013800160929       // - Flag di errore
013900160929     d Err             s              1    inz(*off)
014000160929
014100160929       // - Campi di comodo
014200160929     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
014300160929
014400160929       // - Parametri per pgm interrogazione tabelle
014500160929      /copy gaitrasrc/srcProtoPI,X§TABER
014600160929
014700160929       //--------------------------------------------------------------
014800160929       //?Definizione prototipi procedure.                             ?
014900160929       //--------------------------------------------------------------
015000160929       // - Reperimento dati utente
015100160929      /copy gaitrasrc/srcProtoPR,TIBS34R
015200160929
015300160929       // - Ricerca vecchie tabelle
015400160929      /copy gaitrasrc/srcProtoPR,X§TABER
015500160929
015600160929       // - Ricerca e controllo nuove tabelle
015700160929      /copy gaitrasrc/srcProtoPR,TIBS02R
015800160929
015900161212       // - Selezione tipo assenza
016000160929     d tntbatar1       pr                  extpgm('TNTBATAR1')
016100160929     d  kpjba                              likeds(KPJBA)
016200160929     d  tntbatads                          likeds(TNTBATADS)
016300160929
016400160929       //--------------------------------------------------------------
016500160929       //?Definizione key-list.                                        ?
016600160929       //--------------------------------------------------------------
016700160929       // - File TABEL00F
016800160929     d k03tabel00    e ds                  extname(TABEL00F : *key)
016900160929     d                                     prefix(k_)   inz
017000160929
017100160929       // - File TNTBE01L
017200160929     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
017300160929     d                                     prefix(k_)   inz
017400160929
017500160929       //--------------------------------------------------------------
017600160929       //?Riepilogo indicatori utilizzati.                             ?
017700160929       //--------------------------------------------------------------
017800160929
017900160929       //--------------------------------------------------------------
018000160929
018100160929       //--------------------------------------------------------------
018200160929       //?M A I N - L I N E                                            ?
018300160929       //--------------------------------------------------------------
018400160929     c     *ENTRY        plist
018500160929     c                   parm                    KPJBA
018600160929
018700160929      /free
018800160929
018900160929       // Operazioni iniziali
019000160929       exsr RoutInz;
019100160929
019200160929       // Gestione video?
019300160929       DOW  not Fine;
019400160929         SELECT;
019500160929         // Manutenzione singola tabella
019600160929         WHEN  Video = 'D2';
019700160929           exsr GesD02;
019800160929         // Filtro di lancio
019900160929         WHEN  Video = 'D1';
020000160929           exsr GesD01;
020100160929         // Dati da copiare in tab. "ATA"
020200160929         WHEN  Video = 'W1';
020300160929           exsr GesW01;
020400160929         OTHER;
020500160929           Fine = *on;
020600160929         ENDSL;
020700160929       ENDDO;
020800160929
020900160929       // Operazioni finali?
021000160929       exsr RoutEnd;
021100160929
021200160929       //--------------------------------------------------------------
021300160929       //?Operazioni iniziali.                                         ?
021400160929       //--------------------------------------------------------------
021500160929       BEGSR RoutInz;
021600160929
021700160929         *inLR = *on;
021800160929
021900160929       // Reperimento dati job?
022000160929         exsr DatiJob;
022100160929
022200160929       // Impostazione nome programma a video?
022300160929         V1Tpgm = SDSpgm;
022400160929
022500160929       // Aggancio dati generali della tabella in esame?
022600160929         clear  k_TBEcod;
022700160929         k_TBEke1 = *zero;
022800160929         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
022900160929                = c_Tab;
023000160929         clear  k_TBEke2;
023100160929         clear  k_TBElin;
023200160929         k_TBEsif = KNSIF;
023300160929         chain(n) %kds(k05tntbe01)  TNTBE01L;
023400160929         IF  not %found(TNTBE01L);
023500160929           clear k_TBEsif;
023600160929           chain(n) %kds(k05tntbe01)  TNTBE01L;
023700160929         ENDIF;
023800160929         IF  %found(TNTBE01L);
023900160929           xTNTBEds = TNTBEds;
024000160929         ELSE;
024100160929           clear xTNTBEds;
024200160929         ENDIF;
024300160929
024400160929       // Impostazione iniziale / pulizia dei campi chiave?
024500160929         k_TBEcod = c_Tab;
024600160929         clear k_TBEke1;
024700160929         clear k_TBEke2;
024800160929         clear k_TBElin;
024900160929         clear k_TBEsif;
025000160929
025100160929       // 1ª inizializzazione della videata D01?
025200160929         exsr InzD01;
025300160929         V1Cata  = '?';
025400160929         wInzD01 = *off;
025500160929
025600160929       ENDSR;
025700160929
025800160929       //--------------------------------------------------------------
025900160929       //?Reperimento Dati del job (Utente/Operativi).                 ?
026000160929       //--------------------------------------------------------------
026100160929       BEGSR DatiJob;
026200160929
026300160929         in(E) §AzUte;
026400160929         IF  NOT %error;
026500160929           in(E) §DatiUte;
026600160929         ENDIF;
026700160929         IF  %error or RSut = *blanks;
026800160929           clear TIBS34ds;
026900160929           tibs34r ( tibs34ds );
027000160929           in §AzUte;
027100160929           in §DatiUte;
027200160929         ENDIF;
027300160929
027400160929       ENDSR;
027500160929
027600160929       //--------------------------------------------------------------
027700160929       //?Gestione videata D01                                         ?
027800160929       //--------------------------------------------------------------
027900160929       BEGSR GesD01;
028000160929
028100160929       // Inizializzazione videata?
028200160929         IF  wInzD01;
028300160929           exsr InzD01;
028400160929           wInzD01 = *off;
028500160929         ENDIF;
028600160929
028700160929       // Emissione videata?
028800160929         write TBATAT01;
028900160929         write TBATAP01;
029000160929         exfmt TBATAD01;
029100160929
029200160929         reset ErrMessage;
029300160929         reset ErrGenerico;
029400160929         clear V1Dmsg;
029500160929
029600160929         SELECT;
029700160929
029800160929       // F3=Fine?
029900160929         WHEN  dsp_aid = c_F03;
030000160929           Fine = *on;
030100160929
030200160929       // Invio?
030300160929         OTHER;
030400160929           exsr CtrD01;
030500160929           IF  not ErrGenerico;
030600160929             Video   = 'D2';
030700160929             wInzD02 = *on;
030800160929           ENDIF;
030900160929
031000160929         ENDSL;
031100160929
031200160929       ENDSR;
031300160929
031400160929       //--------------------------------------------------------------
031500160929       //?Inizializzazione videata D01                                 ?
031600160929       //--------------------------------------------------------------
031700160929       BEGSR InzD01;
031800160929
031900160929         clear TBATAD01;
032000160929
032100160929       ENDSR;
032200160929
032300160929       //--------------------------------------------------------------
032400160929       //?Controllo dati immessi nella videata D01                     ?
032500160929       //--------------------------------------------------------------
032600160929       BEGSR CtrD01;
032700160929
032800160929         %subst(IndDspF : 28) = *off;
032900160929
033000160929         SELECT;
033100160929
033200161212       // Ricerca codice Tipo Assenza ?
033300160929         WHEN  %scan('?' : V1Cata) > *zeros;
033400160929           clear TNTBATAds;
033500160929           tntbatar1 (kpjba:tntbatads);
033600160929           SELECT;
033700160929           WHEN OATAfxx <> *blank;
033800160929           WHEN OATAerr = *on;
033900160929             ErrMessage = *on;
034000160929             PosCurATA  = *on;
034100160929             V1Dmsg = OATAmsg;
034200160929           OTHER;
034300160929             V1Cata = %subst(OATAke1:1:%len(V1Cata));
034400160929           ENDSL;
034500160929           ErrGenerico = *on;
034600160929           leavesr;
034700160929
034800161212       // Controllo immissione tipo assenza  ?
034900160929         WHEN  V1Cata = *blank;
035000160929           ErrMessage  = *on;
035100160929           ErrGenerico = *on;
035200160929           PosCurATA   = *on;
035300160929           V1Dmsg = Msg(01);
035400160929           leavesr;
035500160929
035600160929         ENDSL;
035700160929
035800160929       ENDSR;
035900160929
036000160929       //--------------------------------------------------------------
036100160929       //?Gestione videata D02                                         ?
036200160929       //--------------------------------------------------------------
036300160929       BEGSR GesD02;
036400160929
036500160929       // Inizializzazione videata?
036600160929         IF  wInzD02;
036700160929           exsr InzD02;
036800160929           wInzD02 = *off;
036900160929         ENDIF;
037000160929
037100160929       // Emissione Testata e Piede con tasti funzionali abilitati?
037200160929         IF  ErrGenerico = *off;
037300160929           write TBATAT01;
037400160929           write TBATAD01;
037500160929           write Protect;
037600160929           write TBATAP02;
037700160929         ENDIF;
037800160929
037900160929       // Emissione videata?
038000160929         exfmt TBATAD02;
038100160929
038200160929         reset ErrMessage;
038300160929         reset ErrGenerico;
038400160929         clear V1Dmsg;
038500160929         reset Annullamento;
038600160929
038700160929         SELECT;
038800160929
038900160929       // F3=Fine?
039000160929         WHEN  dsp_aid = c_F03;
039100160929           unlock TNTBE01L;
039200160929           Fine = *on;
039300160929
039400160929       // F12=Ritorno?
039500160929         WHEN  dsp_aid = c_F12;
039600160929           unlock TNTBE01L;
039700160929           reset Video;
039800160929           clear V1Topz;
039900160929
040000160929       // Enter; F5=Ripristino; F6=Conferma; F16=Annullamento?
040100160929         OTHER;
040200160929           Ripristino   = (dsp_aid = c_F05);
040300160929           Annullamento = (dsp_aid = c_F16);
040400160929           IF  not Annullamento;
040500160929             exsr CtrD02;
040600160929             IF  ErrGenerico;
040700160929               leavesr;
040800160929             ENDIF;
040900160929           ENDIF;
041000160929           SELECT;
041100160929           WHEN  dsp_aid = c_F05  or
041200160929                 dsp_aid = c_F06  or
041300160929                 dsp_aid = c_F16;
041400160929             Video = 'W1';
041500160929             reset wInzW01;
041600160929           ENDSL;
041700160929
041800160929         ENDSL;
041900160929
042000160929       ENDSR;
042100160929
042200160929       //--------------------------------------------------------------
042300160929       //?Inizializzazione videata D02                                 ?
042400160929       //--------------------------------------------------------------
042500160929       BEGSR InzD02;
042600160929
042700160929       // Spegnimento di TUTTI gli indicatori?
042800160929         IndDspF = *off;
042900160929
043000160929       // Pulizia videata?
043100160929         clear TBATAD02;
043200160929
043300161212       // Reperimento dati
043400160929         reset Immissione;
043500160929         reset Ripristino;
043600160929         k_TBEke1 = V1Cata;
043700160929         chain  %kds(k05tntbe01)  TNTBE000;
043800160929         SELECT;
043900160929         WHEN  not %found(TNTBE01L);
044000160929           Immissione = *on;
044100160929         WHEN  TBEatb <> *blank;
044200160929           Ripristino = *on;
044300160929         ENDSL;
044400160929
044500161212       // Caricamento dati
044600160929         IF  %found(TNTBE01L);
044700160929           exsr RieD02;
044800160929         ENDIF;
044900160929
045000160929       // Impostazione tipo manutenzione?
045100160929         SELECT;
045200160929         WHEN  Immissione;
045300160929           V1Topz = 'IMMISSIONE';
045400160929         WHEN  Ripristino;
045500160929           V1Topz = 'RIPRISTINO';
045600160929         OTHER;
045700160929           V1Topz = ' MODIFICA ';
045800160929         ENDSL;
045900160929
046000160929       // Impostazione indicatori per abilitazione tasti funzionali?
046100160929         exsr Set_Ind_D02;
046200160929
046300160929       ENDSR;
046400160929
046500160929       //--------------------------------------------------------------
046600160929       //?Caricamento singola riga di testo nella videata D02          ?
046700160929       //--------------------------------------------------------------
046800160929       BEGSR RieD02;
046900160929
047000160929         dATA = TBEuni;
047100160929
047200160929         V1Cdes  = §ATAdes;
047300160929
047400160929       ENDSR;
047500160929
047600160929       //--------------------------------------------------------------
047700160929       //?Settaggio indicatori nella videata D02 per abilitazione      ?
047800160929       //?  tasti funzionali (sul subfile control)                     ?
047900160929       //--------------------------------------------------------------
048000160929       BEGSR Set_Ind_D02;
048100160929
048200160929       // F5=Ripristino?
048300160929         F5Attivo = (not Immissione and Ripristino);
048400160929
048500160929       // F6=Conferma?
048600160929         F6Attivo = (not F5Attivo);
048700160929
048800160929       // F16=Annullamento?
048900160929         F16Attivo = (not Immissione and not Ripristino);
049000160929
049100160929       ENDSR;
049200160929
049300160929       //--------------------------------------------------------------
049400160929       //?Controllo dati videata D02                                   ?
049500160929       //--------------------------------------------------------------
049600160929       BEGSR  CtrD02;
049700160929
049800160929         %subst(IndDspF : 50) = *off;
049900160929
050000160929         SELECT;
050100160929
050200160929       // Descrizione obbligatoria?
050300160929         WHEN  V1Cdes = *blank;
050400160929           ErrMessage  = *on;
050500160929           ErrGenerico = *on;
050600160929           PosCurDES   = *on;
050700160929           V1Dmsg = Msg(02);
050800160929           leavesr;
050900160929
051000160929         ENDSL;
051100160929
051200160929       ENDSR;
051300160929
051400160929       //--------------------------------------------------------------
051500160929       //?Gestione trasmissione aggiornamento                          ?
051600160929       //--------------------------------------------------------------
051700160929       BEGSR GesW01;
051800160929
051900160929       // Inizializzazione videata
052000160929         IF  wInzW01;
052100160929           exsr InzW01;
052200160929           wInzW01 = *off;
052300160929         ENDIF;
052400160929
052500160929       // Emissione window
052600160929         IF  TBXftt = 'S';
052700160929           exfmt TBATAW01;
052800160929           ErrMessage  = *off;
052900160929           ErrGenerico = *off;
053000160929           clear V1Dmsg;
053100160929         ELSE;
053200160929           dsp_aid = c_F06;
053300160929         ENDIF;
053400160929
053500160929         SELECT;
053600160929
053700160929       // F12=Ritorno (gestito solo se emesso W01)
053800160929         WHEN  dsp_aid = c_F12;
053900160929           Video = 'D2';
054000160929
054100160929       // F6=Conferma
054200160929         WHEN  dsp_aid = c_F06;
054300160929           exsr UpdTNTBE;
054400160929         // Ritorno alla videata iniziale
054500160929           IF  not ErrGenerico;
054600160929             clear V1Topz;
054700160929             Video = 'D1';
054800160929             wInzD01 = *on;
054900160929           ENDIF;
055000160929         ENDSL;
055100160929
055200160929       ENDSR;
055300160929
055400160929       //--------------------------------------------------------------
055500160929       //?Inizializzazione videata W01                                 ?
055600160929       //--------------------------------------------------------------
055700160929       BEGSR InzW01;
055800160929
055900160929         clear TBATAW01;
056000160929
056100160929         IF  Immissione;
056200160929
056300160929         // Se immissione
056400160929           W1ftt  = TBXftt;
056500160929
056600160929         ELSE;
056700160929
056800160929         // Se NON immissione:
056900160929         // visualizza i dati relativi all'ultima trasmissione
057000160929           W1ftt  = TBEftt;
057100160929           W1flt  = TBEflt;
057200160929           W1ftr  = TBEftr;
057300160929           IF  TBEdtr <> *zero;
057400160929             wDate_EUR = %date(TBEdtr : *iso);
057500160929             W1dtr     = %dec(wDate_EUR);
057600160929           ENDIF;
057700160929
057800160929         ENDIF;
057900160929
058000160929       ENDSR;
058100160929
058200160929       //--------------------------------------------------------------
058300160929       //?Aggiornamento records TNTBE00F (tab. "ATA")                  ?
058400160929       //--------------------------------------------------------------
058500160929       BEGSR  UpdTNTBE;
058600160929
058700160929       // Aggancio record di TNTBE00F?
058800160929         k_TBEcod = c_Tab;
058900160929         k_TBEke1 = V1Cata;
059000160929         clear k_TBEke2;
059100160929         clear k_TBElin;
059200160929         clear k_TBEsif;
059300160929
059400160929         chain  %kds(K05tntbe01) TNTBE01L;
059500160929
059600160929         exsr  RieATA;
059700160929         TBEuni = dATA;
059800160929
059900160929       // Aggiornamento tab. "ATA"?
060000160929         SELECT;
060100160929
060200160929       // F5=Ripristino?
060300160929         WHEN  Ripristino;
060400160929           clear TBEatb;
060500160929           TBEftt = W1ftt;
060600160929           clear TBEftr;
060700160929           //_______________
060800160929           UPDATE  TNTBE000;
060900160929           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
061000160929
061100160929       // F16=Annullamento?
061200160929         WHEN  Annullamento;
061300160929           TBEatb = 'A';
061400160929           TBEftt = W1ftt;
061500160929           clear TBEftr;
061600160929           //_______________
061700160929           UPDATE  TNTBE000;
061800160929           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
061900160929
062000160929       // Immissione o Modifica?
062100160929         OTHER;
062200160929           IF  not %found(TNTBE01L);
062300160929             clear TNTBE000;
062400160929             TBEcod = k_TBEcod;
062500160929             TBEke1 = k_TBEke1;
062600160929             TBEke2 = k_TBEke2;
062700160929             TBElin = k_TBElin;
062800160929             TBEsif = k_TBEsif;
062900160929             TBEuni = dATA;
063000160929           ENDIF;
063100160929
063200160929           TBEapl = TBXapl;
063300160929           clear TBEatb;
063400160929           TBEftt = W1ftt;
063500160929           clear  TBEflt;
063600160929           clear  TBEftr;
063700160929
063800160929         // Aggiornamento record
063900160929           IF  %found(TNTBE01L);
064000160929             //_____________
064100160929             UPDATE TNTBE000;
064200160929             //¯¯¯¯¯¯¯¯¯¯¯¯¯
064300160929           ELSE;
064400160929             //_____________
064500160929             WRITE  TNTBE000;
064600160929             //¯¯¯¯¯¯¯¯¯¯¯¯¯
064700160929           ENDIF;
064800160929
064900160929         ENDSL;
065000160929
065100160929       ENDSR;
065200160929
065300160929       //--------------------------------------------------------------
065400160929       //?Caricamento dati in tab. "ATA"                               ?
065500160929       //--------------------------------------------------------------
065600160929       BEGSR RieATA;
065700160929
065800160929         clear dATA;
065900160929
066000160929         §ATAdes  = V1Cdes;
066100160929
066200160929       ENDSR;
066300160929
066400160929       //--------------------------------------------------------------
066500160929       //?Operazioni finali.                                           ?
066600160929       //--------------------------------------------------------------
066700160929       BEGSR RoutEnd;
066800160929
066900160929       // Chiusura funzioni precedentemente aperte?
067000160929         clear TIBS02ds;
067100160929         T02tla = 'C';
067200160929         TNTBE_RicercaControllo (kpjba:tibs02ds);
067300160929
067400160929       // Chiusura pgm?
067500160929         return;
067600160929
067700160929       ENDSR;
067800160929
067900160929      /end-free
068000160929
068100160929       //--------------------------------------------------------------
068200160929       //?Definizione schiere a tempo di compilazione                  ?
068300160929       //--------------------------------------------------------------
068400160929
068500160929** >  Msg -------------------------------------------------------------------*
068600161212Immettere il Tipo Assenza                                                       1
068700160929Descrizione obbligatoria                                                        2
