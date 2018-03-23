000100160929       //--------------------------------------------------------------
000200161212       //?Gestione tabella "ACA" (Causali assenza autisti)          ?
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
001500160929     fTNTBE01L  Uf A e           k disk
001600160929
001700160929       // - Video Gestione
001800160929     fTNTBACAD  cf   e             workstn
001900160929     f                                     indds(IndDspF)
002000160929     f                                     infds(InfDspF)
002100160929
002200160929       //--------------------------------------------------------------
002300160929       //?Definizione costanti.                                        ?
002400160929       //--------------------------------------------------------------
002500160929       // - Codice tabella in gestione
002600160929     d c_Tab           c                   const('ACA')
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
006300170113     d skFLG           s              1    dim(99)
006400160929
006500160929       //--------------------------------------------------------------
006600160929       //?Definizione aree dati.                                       ?
006700160929       //--------------------------------------------------------------
006800160929       // - Dati utente
006900160929     d §AzUte        e ds                  extname(AZUTE00F)
007000160929     d                                     dtaara
007100160929     d §DatiUte      e ds                  extname(dDatiUte)
007200160929     d                                     dtaara
007300160929
007400160929       //--------------------------------------------------------------
007500160929       //?Definizione strutture dati.                                  ?
007600160929       //--------------------------------------------------------------
007700160929       // - Status ds
007800160929     d Status         sds
007900160929     d  SDSpgm           *proc
008000160929
008100160929       // - InfDS
008200160929     d InfDspF         ds
008300160929     d   dsp_aid             369    369a
008400160929
008500160929       // - Indicatori su DspF
008600160929     d IndDspF         ds                  inz
008700160929       // - Abilitazione tasti funzionali
008800160929     d  F5Attivo                       n   overlay(IndDspF : 05)
008900160929     d  F6Attivo                       n   overlay(IndDspF : 06)
009000160929     d  F10Attivo                      n   overlay(IndDspF : 10)
009100160929     d  F16Attivo                      n   overlay(IndDspF : 16)
009200160929       // - Emissione messaggio di errore
009300160929     d  ErrMessage                     n   overlay(IndDspF : 28)
009400160929       // - Posizionamento cursore & segnalazione errore
009500160929     d  PosCurCAU                      n   overlay(IndDspF : 50)
009600160929     d  PosCurDES                      n   overlay(IndDspF : 51)
009700160929     d  PosCurATA                      n   overlay(IndDspF : 52)
009800161012     d  PosCurTPA                      n   overlay(IndDspF : 53)
009900170113     d  PosCurFLG                      n   overlay(IndDspF : 54)
010000160929       //   - Riemissione videata
010100160929     d  ErrGenerico                    n   overlay(IndDspF : 99)
010200160929
010300160929       // - Parametri ricevuti
010400160929     d KPJBA         e ds
010500160929
010600160929       // - Parametri per Reperimento dati utente
010700160929     d TIBS34DS      e ds
010800160929
010900160929       // - Parametri per Ricerca/controllo tabelle
011000160929     d TIBS02ds      e ds                  inz
011100160929     d  T02mod       e                     inz('R')
011200160929     d  T02cod       e                     inz(c_Tab)
011300160929
011400161212       // - Tab. ACA = Causali assenza autisti
011500160929     d dACA          e ds                  inz
011600161212
011700161212       // - Tab. ATA = Tipo assenza autisti
011800161212     d dATA          e ds                  inz
011900160929
012000160929       // - Parametri per Interrogazione/Selezione tab. "ACA"
012100160929     d TNTBACADS     e ds                  inz
012200160929
012300160929       // - Dati record principale della tabella
012400160929     d TNTBEds       e ds                  inz  extname(TNTBE00F)
012500160929     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
012600160929     d                                          prefix(TBX:3)
012700161012
012800161012     d WindDspF        ds                  inz
012900161012     d  WindDspFa              1     49    inz(*zero)
013000161012     d  WindDspFb             50     99    inz(*zero)
013100160929
013200160929       //--------------------------------------------------------------
013300160929       //?Definizione variabili globali.                               ?
013400160929       //--------------------------------------------------------------
013500160929       // - Flags booleani
013600160929     d Fine            s               n   inz
013700160929     d wInzD01         s               n   inz(*on)
013800160929     d wInzD02         s               n   inz(*on)
013900160929     d wInzD03         s               n   inz(*on)
014000160929     d wInzW01         s               n   inz(*on)
014100160929     d Annullamento    s               n   inz
014200160929     d Immissione      s               n   inz
014300160929     d Ripristino      s               n   inz
014400160929
014500160929       // - Indici di schiera
014600160929     d xx              s              3  0 inz
014700160929
014800160929       // - Variabili per la gestione del video
014900160929     d Video           s              2    inz('D1')
015000160929
015100160929       // - Flag di errore
015200160929     d Err             s              1    inz(*off)
015300160929
015400160929       // - Campi di comodo
015500160929     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
015600160929     d savcod          s              1
015700160929
015800160929       // - Parametri per pgm interrogazione tabelle
015900160929      /copy gaitrasrc/srcProtoPI,X§TABER
016000160929
016100160929       //--------------------------------------------------------------
016200160929       //?Definizione prototipi procedure.                             ?
016300160929       //--------------------------------------------------------------
016400160929       // - Reperimento dati utente
016500160929      /copy gaitrasrc/srcProtoPR,TIBS34R
016600160929
016700160929       // - Ricerca vecchie tabelle
016800160929      /copy gaitrasrc/srcProtoPR,X§TABER
016900160929
017000160929       // - Ricerca e controllo nuove tabelle
017100160929      /copy gaitrasrc/srcProtoPR,TIBS02R
017200160929
017300161212       // - Selezione singola causale assenza
017400160930     d tntbacar1       pr                  extpgm('TNTBACAR1')
017500160929     d   kpjba                             likeds(KPJBA)
017600160930     d   tntbacads                         likeds(TNTBACADS)
017700160929
017800160929       //--------------------------------------------------------------
017900160929       //?Definizione key-list.                                        ?
018000160929       //--------------------------------------------------------------
018100160929       // - File TABEL00F
018200160929     d k03tabel00    e ds                  extname(TABEL00F : *key)
018300160929     d                                     prefix(k_)   inz
018400160929
018500160929       // - File TNTBE01L
018600160929     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
018700160929     d                                     prefix(k_)   inz
018800160929
018900160929       //--------------------------------------------------------------
019000160929       //?Riepilogo indicatori utilizzati.                             ?
019100160929       //--------------------------------------------------------------
019200160929
019300160929       //--------------------------------------------------------------
019400160929
019500160929       //--------------------------------------------------------------
019600160929       //?M A I N - L I N E                                            ?
019700160929       //--------------------------------------------------------------
019800160929     c     *ENTRY        plist
019900160929     c                   parm                    KPJBA
020000160929
020100160929      /free
020200160929
020300160929       // Operazioni iniziali?
020400160929       exsr RoutInz;
020500160929
020600160929       // Gestione video?
020700160929       DOW  not Fine;
020800160929         SELECT;
020900160929         // Manutenzione singola tabella
021000160929         WHEN  Video = 'D2';
021100160929           exsr GesD02;
021200160929         // Filtro di lancio
021300160929         WHEN  Video = 'D1';
021400160929           exsr GesD01;
021500160929         // Richiesta dati per trasmissione record
021600160929         WHEN  Video = 'W1';
021700160929           exsr GesW01;
021800160929         OTHER;
021900160929           Fine = *on;
022000160929         ENDSL;
022100160929       ENDDO;
022200160929
022300160929       // Operazioni finali?
022400160929       exsr RoutEnd;
022500160929
022600160929       //--------------------------------------------------------------
022700160929       //?Operazioni iniziali.                                         ?
022800160929       //--------------------------------------------------------------
022900160929       BEGSR RoutInz;
023000160929
023100160929         *inLR = *on;
023200160929
023300160929       // Reperimento dati job?
023400160929         exsr DatiJob;
023500160929
023600160929       // Impostazione nome programma a video?
023700160929         V1Tpgm = SDSpgm;
023800170113
023900170113       // Carico una schiera con i flag già utilizzati per immissione veloce
024000170113         exsr Carica_skFLG;
024100160929
024200160929       // Aggancio dati generali della tabella in esame?
024300160929         clear  k_TBEcod;
024400160929         k_TBEke1 = *zero;
024500160929         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
024600160929                = c_Tab;
024700160929         clear k_TBEke2;
024800160929         clear k_TBElin;
024900160929         k_TBEsif = KNSIF;
025000160929         chain(n) %kds(k05tntbe01) TNTBE01L;
025100160929         IF  not %found(TNTBE01L);
025200160929           clear k_TBEsif;
025300160929           chain(n) %kds(k05tntbe01) TNTBE01L;
025400160929         ENDIF;
025500160929         IF  %found(TNTBE01L);
025600160929           xTNTBEds = TNTBEds;
025700160929         ELSE;
025800160929           clear xTNTBEds;
025900160929         ENDIF;
026000160929
026100160929       // Impostazione iniziale / pulizia dei campi chiave?
026200160929         k_TBEcod = c_Tab;
026300160929         clear k_TBEke1;
026400160929         clear k_TBEke2;
026500160929         clear k_TBElin;
026600160929         clear k_TBEsif;
026700160929
026800160929       // 1ª inizializzazione della videata D01?
026900160929         exsr InzD01;
027000160929         V1Ccau  = '?';
027100160929         wInzD01 = *off;
027200160929
027300160929       ENDSR;
027400160929
027500160929       //--------------------------------------------------------------
027600160929       //?Reperimento Dati del job (Utente/Operativi).                 ?
027700160929       //--------------------------------------------------------------
027800160929       BEGSR DatiJob;
027900160929
028000160929         in(E) §AzUte;
028100160929         IF  not %error;
028200160929           in(E) §DatiUte;
028300160929         ENDIF;
028400160929         IF  %error or RSut = *blanks;
028500160929           clear TIBS34DS;
028600160929           tibs34r ( tibs34ds );
028700160929           in §AzUte;
028800160929           in §DatiUte;
028900160929         ENDIF;
029000160929
029100160929       ENDSR;
029200170113
029300170113       //--------------------------------------------------------------
029400170113       //?Carico sk dei flag già utilizzati.                           ?
029500170113       //--------------------------------------------------------------
029600170113       BEGSR Carica_skFLG;
029700170113
029800170113         clear skFLG;
029900170113         clear xx;
030000170113         k_TBEcod = 'ACA';
030100170113         setll (k_TBEcod) TNTBE01L;
030200170113         reade(n) (k_TBEcod) TNTBE01L;
030300170113         DOW not %eof(TNTBE01L);
030400170113           IF  TBEatb = *blanks;
030500170113             dACA = TBEuni;
030600170113             IF  §ACAflg <> *blanks;
030700170113               xx += 1;
030800170113               skFLG(xx) = §ACAflg;
030900170113             ENDIF;
031000170113           ENDIF;
031100170113           reade(n) (k_TBEcod) TNTBE01L;
031200170113         ENDDO;
031300170113
031400170113       ENDSR;
031500160929
031600160929       //--------------------------------------------------------------
031700160929       //?Gestione videata D01                                         ?
031800160929       //--------------------------------------------------------------
031900160929       BEGSR GesD01;
032000160929
032100160929       // Inizializzazione videata?
032200160929         IF  wInzD01;
032300160929           exsr InzD01;
032400160929           wInzD01 = *off;
032500160929         ENDIF;
032600160929
032700160929       //?Emissione videata?
032800160929         write TBACAT01;
032900160929         write TBACAP01;
033000160929         exfmt TBACAD01;
033100160929
033200160929         reset ErrMessage;
033300160929         reset ErrGenerico;
033400160929         clear V1Dmsg;
033500160929
033600160929         SELECT;
033700160929
033800160929         // F3=Fine?
033900160929         WHEN  dsp_aid = c_F03;
034000160929             Fine = *on;
034100160929
034200160929         // Invio?
034300160929         OTHER;
034400160929           exsr CtrD01;
034500160929           IF  not ErrGenerico;
034600160929             Video   = 'D2';
034700160929             wInzD02 = *on;
034800160929           ENDIF;
034900160929
035000160929         ENDSL;
035100160929
035200160929       ENDSR;
035300160929
035400160929       //--------------------------------------------------------------
035500160929       //?Inizializzazione videata D01                                 ?
035600160929       //--------------------------------------------------------------
035700160929       BEGSR InzD01;
035800160929
035900160929         clear TBACAD01;
036000160929
036100160929       ENDSR;
036200160929
036300160929       //--------------------------------------------------------------
036400160929       //?Controllo dati immessi nella videata D01                     ?
036500160929       //--------------------------------------------------------------
036600160929       BEGSR CtrD01;
036700161012
036800161012         WindDspF = IndDspF;
036900161012         reset WindDspFb;
037000161012         IndDspF  = WindDspF;
037100160929
037200160929         SELECT;
037300160929
037400161212         //?Ricerca codice Causale Assenza?
037500161012         WHEN  %scan('?' : V1Ccau) > *zeros;
037600161012           clear TNTBACADS;
037700161012           kpjbu = 'TNTBACAR' ;
037800161012           tntbacar1 (kpjba:tntbacads);
037900161012           SELECT;
038000161012           WHEN  OACAfxx <> *blank;
038100161012           WHEN  OACAerr = *on;
038200161012             ErrMessage  = *on;
038300161012             PosCurCAU   = *on;
038400161012             V1Dmsg = OACAmsg;
038500161012           OTHER;
038600161012             V1Ccau = %subst(OACAke1 : 1 : %len(V1Ccau));
038700161012           ENDSL;
038800161012           ErrGenerico = *on;
038900161012           leavesr;
039000160929
039100161012       // Controllo immissione codice causale?
039200161012         WHEN  V1Ccau = *blank;
039300161012           ErrMessage  = *on;
039400161012           ErrGenerico = *on;
039500161012           PosCurCAU   = *on;
039600161012           V1Dmsg = Msg(01);
039700161012           leavesr;
039800160929
039900160929         ENDSL;
040000160929
040100160929       ENDSR;
040200160929
040300160929       //--------------------------------------------------------------
040400160929       //?Gestione videata D02                                         ?
040500160929       //--------------------------------------------------------------
040600160929       BEGSR GesD02;
040700160929
040800160929       //?Inizializzazione videata?
040900160929         IF  wInzD02;
041000160929           exsr InzD02;
041100160929           wInzD02 = *off;
041200160929         ENDIF;
041300160929
041400160929       // Emissione Testata e Piede con tasti funzionali abilitati?
041500161012         IF  not ErrGenerico;
041600160929           write TBACAT01;
041700160929           write TBACAD01;
041800160929           write Protect;
041900160929           write TBACAP02;
042000161012         ENDIF;
042100160929
042200160929       // Emissione videata?
042300160929         exfmt TBACAD02;
042400160929
042500160929         reset ErrMessage;
042600160929         reset ErrGenerico;
042700161012         reset Annullamento;
042800161012         clear V2Dmsg;
042900160929
043000160929         SELECT;
043100160929
043200160929       // F3=Fine?
043300160929         WHEN  dsp_aid = c_F03;
043400160929           unlock TNTBE01L;
043500160929           Fine = *on;
043600160929
043700160929       // F12=Ritorno?
043800160929         WHEN  dsp_aid = c_F12;
043900160929           unlock TNTBE01L;
044000160929           reset Video;
044100160929           clear V1Topz;
044200160929
044300160929       // RollUp=Videata successiva?
044400160929         WHEN  dsp_aid = c_RollUp;
044500160929           Video = 'D3';
044600160929
044700160929       // Enter; F5=Ripristino; F6=Conferma; F16=Annullamento?
044800160929         OTHER;
044900160929           Ripristino   = (dsp_aid = c_F05);
045000160929           Annullamento = (dsp_aid = c_F16);
045100160929           IF  not Annullamento;
045200160929             exsr CtrD02;
045300160929             IF  ErrGenerico;
045400160929               leavesr;
045500160929             ENDIF;
045600160929           ENDIF;
045700160929           SELECT;
045800160929           WHEN  dsp_aid = c_F05  or
045900160929                 dsp_aid = c_F06  or
046000160929                 dsp_aid = c_F16;
046100160929             Video = 'W1';
046200160929             reset wInzW01;
046300160929           ENDSL;
046400160929
046500160929         ENDSL;
046600160929
046700160929       ENDSR;
046800160929
046900160929       //--------------------------------------------------------------
047000160929       //?Inizializzazione videata D02                                 ?
047100160929       //--------------------------------------------------------------
047200160929       BEGSR InzD02;
047300160929
047400160929       // Pulizia videata?
047500160929         clear TBACAD02;
047600160929
047700161212       // Reperimento dati della causale assenza autisti  ?
047800160929         reset Immissione;
047900160929         reset Ripristino;
048000160930         k_TBEke1 = V1Ccau;
048100160929         chain %kds(k05tntbe01) TNTBE01L;
048200160929         SELECT;
048300160929         WHEN  not %found(TNTBE01L);
048400160929           Immissione = *on;
048500160929         WHEN  TBEatb <> *blank;
048600160929           Ripristino = *on;
048700160929         ENDSL;
048800160929
048900161212       // Caricamento dati della causale assenza autisti  ?
049000160929         IF  %found(TNTBE01L);
049100160929           exsr RieD02;
049200160929         ENDIF;
049300160929
049400160929       // Impostazione tipo manutenzione?
049500160929         SELECT;
049600160929         WHEN  Immissione;
049700160929           V1Topz = 'IMMISSIONE';
049800160929         WHEN  Ripristino;
049900160929           V1Topz = 'RIPRISTINO';
050000160929         OTHER;
050100160929           V1Topz = ' MODIFICA ';
050200160929         ENDSL;
050300160929
050400160929       // Impostazione indicatori per abilitazione tasti funzionali?
050500160929         exsr Set_Ind_D02;
050600160929
050700160929       ENDSR;
050800160929
050900160929       //--------------------------------------------------------------
051000160929       //?Caricamento singola riga di testo nella videata D02          ?
051100160929       //--------------------------------------------------------------
051200160929       BEGSR RieD02;
051300160929
051400160929         dACA = TBEuni;
051500160929
051600161012         V1Cdes = §ACAdes;
051700161012         V1Cata = §ACAata;
051800161012         V1Cuti = §ACAuti;
051900161012         V1Cann = §ACAann;
052000161012         V1Ctpa = §ACAtpa;
052100170113         V1Cflg = §ACAflg;
052200160929
052300160929         // Decodifiche
052400160929         exsr CtrD02;
052500160929
052600160929       ENDSR;
052700160929
052800160929       //--------------------------------------------------------------
052900160929       //?Settaggio indicatori nella videata D02 per abilitazione      ?
053000160929       //?  tasti funzionali (sul subfile control)                     ?
053100160929       //--------------------------------------------------------------
053200160929       BEGSR Set_Ind_D02;
053300160929
053400160929       // F5=Ripristino?
053500160929         F5Attivo = (not Immissione and Ripristino);
053600160929
053700160929       // F6=Conferma?
053800160929         F6Attivo = (not F5Attivo);
053900160929
054000160929       // F16=Annullamento?
054100160929         F16Attivo = (not Immissione and not Ripristino);
054200160929
054300160929       ENDSR;
054400160929
054500160929       //--------------------------------------------------------------
054600160929       //?Controllo dati videata D02                                   ?
054700160929       //--------------------------------------------------------------
054800160929       BEGSR CtrD02;
054900161012
055000161012       // Spegnimento di TUTTI gli indicatori?
055100161012         WindDspF = IndDspF;
055200161012         reset WindDspFb;
055300161012         IndDspF  = WindDspF;
055400160929
055500160929         SELECT;
055600160929
055700160929       // Descrizione obbligatoria?
055800160929         WHEN  V1Cdes = *blank;
055900160929           ErrMessage  = *on;
056000160929           ErrGenerico = *on;
056100160929           PosCurDES   = *on;
056200161012           V2Dmsg = Msg(02);
056300160929           leavesr;
056400160929         ENDSL;
056500161212
056600161212       // Tipo assenza
056700161212         clear V1data;
056800161212         clear dATA;
056900160929         IF  %scan('?' : V1Cata) > *zero;
057000160929           ErrGenerico = *on;
057100160929           PosCurATA   = *on;
057200160929           clear TIBS02ds;
057300160929           T02mod = 'R';
057400160929           T02cod = 'ATA';
057500160929           T02sif = KNSIF;
057600160929           TNTBE_RicercaControllo  (kpjba : tibs02ds);
057700160929           IF  T02err <> *blanks;
057800160929             ErrMessage  = *on;
057900160929             ErrGenerico = *on;
058000161012             V2Dmsg = T02msg;
058100160929             leavesr;
058200160929           ENDIF;
058300161012           v1cata = T02ke1;
058400161212           dATA = T02uni;
058500161212           v1data = §ATAdes;
058600161012           ErrGenerico = *on;
058700161012           leavesr;
058800161012         ENDIF;
058900160929
059000161212       // Controllo tipo assenza
059100161012         IF  v1cata <> *blanks ;
059200161012           clear  TIBS02ds;
059300161212           clear V1data;
059400161212           clear dATA;
059500161012           T02mod = 'C';
059600161012           T02cod = 'ATA';
059700161012           T02ke1 = v1cata;
059800161012           T02sif = KNSIF;
059900161012           TNTBE_RicercaControllo  (kpjba : tibs02ds);
060000161012           IF  T02err <> *blanks;
060100161012             ErrMessage  = *on;
060200161012             ErrGenerico = *on;
060300161012             PosCurATA   = *on;
060400161012             V2Dmsg = Msg(03);
060500161012             leavesr;
060600161012           ENDIF;
060700161212           dATA = T02uni;
060800161212           v1data = §ATAdes;
060900161012         ENDIF;
061000161012
061100161212       // Se assenza Totale o Parziale è obbligatorio
061200161212         IF  V1Ctpa = *blanks;
061300161012           ErrMessage  = *on;
061400161012           ErrGenerico = *on;
061500161012           PosCurTPA   = *on;
061600161012           V2Dmsg = Msg(04);
061700161012           leavesr;
061800161012         ENDIF;
061900170113
062000170113       // Il flag di immissione veloce non deve essere già stato usato
062100170113         IF  Immissione and
062200170113             V1Cflg <> *blanks and %lookup(V1Cflg:skFLG) > 0;
062300170113           ErrMessage  = *on;
062400170113           ErrGenerico = *on;
062500170113           PosCurFLG   = *on;
062600170113           V2Dmsg = Msg(05);
062700170113           leavesr;
062800170113         ENDIF;
062900170113         IF  not Immissione and V1Cflg <> §ACAflg and
063000170113             %lookup(V1Cflg:skFLG) > 0;
063100170113           ErrMessage  = *on;
063200170113           ErrGenerico = *on;
063300170113           PosCurFLG   = *on;
063400170113           V2Dmsg = Msg(05);
063500170113           leavesr;
063600170113         ENDIF;
063700160929
063800160929       ENDSR;
063900160929
064000160929       //--------------------------------------------------------------
064100160929       //?Gestione trasmissione aggiornamento                          ?
064200160929       //--------------------------------------------------------------
064300160929       BEGSR GesW01;
064400160929
064500160929       // Inizializzazione videata
064600160929         IF  wInzW01;
064700160929           exsr InzW01;
064800160929           wInzW01 = *off;
064900160929         ENDIF;
065000160929
065100160929       // Emissione window
065200160929         IF  TBXftt = 'S';
065300160930           exfmt TBACAW01;
065400160929           ErrMessage  = *off;
065500160929           ErrGenerico = *off;
065600160929           clear V1Dmsg;
065700160929         ELSE;
065800160929           dsp_aid = c_F06;
065900160929         ENDIF;
066000160929
066100160929         SELECT;
066200160929
066300160929       // F12=Ritorno (gestito solo se emesso W01)
066400160929         WHEN  dsp_aid = c_F12;
066500160929           Video = 'D2';
066600160929
066700160929       // F6=Conferma
066800160929         WHEN  dsp_aid = c_F06;
066900160929           exsr  UpdTNTBE;
067000160929         // Ritorno alla videata iniziale
067100160929           IF  not ErrGenerico;
067200160929             clear V1Topz;
067300160929             Video = 'D1';
067400160929             wInzD01 = *on;
067500160929           ENDIF;
067600160929         ENDSL;
067700160929
067800160929       ENDSR;
067900160929
068000160929       //--------------------------------------------------------------
068100160929       //?Inizializzazione videata W01                                 ?
068200160929       //--------------------------------------------------------------
068300160929       BEGSR InzW01;
068400160929
068500160930         clear TBACAW01;
068600160929
068700160929         IF  Immissione;
068800160929
068900160929       // Se immissione
069000160929           W1ftt  = TBXftt;
069100160929
069200160929         ELSE;
069300160929
069400160929       // Se NON immissione:
069500160929       // visualizza i dati relativi all'ultima trasmissione
069600160929           W1ftt = TBEftt;
069700160929           W1flt = TBEflt;
069800160929           W1ftr = TBEftr;
069900160929           IF  TBEdtr <> *zero;
070000160929             wDate_EUR = %date(TBEdtr : *iso);
070100160929             W1dtr     = %dec(wDate_EUR);
070200160929           ENDIF;
070300160929
070400160929         ENDIF;
070500160929
070600160929       ENDSR;
070700160929
070800160929       //--------------------------------------------------------------
070900160929       //?Aggiornamento records TNTBE00F (tab. "ACA")                  ?
071000160929       //--------------------------------------------------------------
071100160929       BEGSR UpdTNTBE;
071200160929
071300160929       // Aggancio record di TNTBE00F?
071400160929         k_TBEcod = c_Tab;
071500160929         k_TBEke1 = V1Ccau;
071600160929         clear k_TBEke2;
071700160929         clear k_TBElin;
071800160929         clear k_TBEsif;
071900160929
072000160929         chain  %kds(K05tntbe01) TNTBE01L;
072100160929
072200160929         exsr RieACA;
072300160929         TBEuni = dACA;
072400160929
072500160929       // Aggiornamento tab. "ACA"?
072600160929         SELECT;
072700160929
072800160929       // F5=Ripristino?
072900160929         WHEN  Ripristino;
073000160929           clear TBEatb;
073100160929           TBEftt = W1ftt;
073200160929           clear TBEftr;
073300160929           //_______________
073400160929           UPDATE  TNTBE000;
073500160929           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
073600160929
073700160929       // F16=Annullamento?
073800160929         WHEN  Annullamento;
073900160929           TBEatb = 'A';
074000160929           TBEftt = W1ftt;
074100160930           clear TBEftr;
074200160929           //_______________
074300160929           UPDATE  TNTBE000;
074400160929           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
074500160929
074600160929       // Immissione o Modifica?
074700160929         OTHER;
074800160929
074900160929           IF  not %found(TNTBE01L);
075000160929             clear TNTBE000;
075100160929             TBEcod = k_TBEcod;
075200160929             TBEke1 = k_TBEke1;
075300160929             TBEke2 = k_TBEke2;
075400160929             TBElin = k_TBElin;
075500160929             TBEsif = k_TBEsif;
075600160929             TBEuni = dACA;
075700160929           ENDIF;
075800160929
075900160929           TBEapl = TBXapl;
076000160929           clear TBEatb;
076100160929           TBEftt = W1ftt;
076200160929           clear TBEflt;
076300160929           clear TBEftr;
076400160929
076500160929         // Aggiornamento record
076600160929           IF  %found(TNTBE01L);
076700160929             //_____________
076800160929             UPDATE TNTBE000;
076900160929             //¯¯¯¯¯¯¯¯¯¯¯¯¯
077000160929           ELSE;
077100160929             //_____________
077200160929             WRITE  TNTBE000;
077300160929             //¯¯¯¯¯¯¯¯¯¯¯¯¯
077400160929           ENDIF;
077500160929
077600160929         ENDSL;
077700170113
077800170113       // Carico una schiera con i flag già utilizzati per immissione veloce
077900170113         exsr Carica_skFLG;
078000160929
078100160929       ENDSR;
078200160929
078300160929       //--------------------------------------------------------------
078400160929       //?Caricamento dati in tab. "ACA"                               ?
078500160929       //--------------------------------------------------------------
078600160929       BEGSR RieACA;
078700160929
078800160929         clear dACA;
078900160929
079000160929         §ACAdes = V1Cdes;
079100160929         §ACAata = V1Cata;
079200160929         §ACAuti = V1Cuti;
079300160929         §ACAann = V1Cann;
079400161012         §ACAtpa = V1Ctpa;
079500170113         §ACAflg = V1Cflg;
079600160929
079700160929       ENDSR;
079800160929
079900160929       //--------------------------------------------------------------
080000160929       //?Operazioni finali.                                           ?
080100160929       //--------------------------------------------------------------
080200160929       BEGSR RoutEnd;
080300160929
080400160929       // Chiusura funzioni precedentemente aperte?
080500160929         clear TIBS02DS;
080600160929         T02tla = 'C';
080700160929         TNTBE_RicercaControllo (kpjba:tibs02ds);
080800160929
080900160929       // Chiusura pgm?
081000160929         return;
081100160929
081200160929       ENDSR;
081300160929
081400160929      /end-free
081500160929
081600160929       //--------------------------------------------------------------
081700160929       //?Definizione schiere a tempo di compilazione                  ?
081800160929       //--------------------------------------------------------------
081900160929
082000160929** >  Msg -------------------------------------------------------------------*
082100161212Immettere la Causale Assenza Autista                                            1
082200160929Descrizione obbligatoria                                                        2
082300161212Tipo assenza errato                                                             3
082400161212Immettere se Assenza Totale o Parziale                                          4
082500170113Flag già utilizzato                                                             5
