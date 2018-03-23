000100160929       //--------------------------------------------------------------
000200170315       //?Gestione tabella "GPD" (Giorni Posticipo Data)          ?
000300160929       //--------------------------------------------------------------
000400160929     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500160929
000600160929       //--------------------------------------------------------------
000700160929       //?Dichiarazione file.                                          ?
000800160929       //--------------------------------------------------------------
000900170315       // - Organigramma
001000170315     fAZORG01L  if   e           k disk
001100170315
001200160929       // - Tabelle
001300160929     fTNTBE01L  Uf A e           k disk
001400160929
001500160929       // - Video Gestione
001600170315     fTNTBGPDD  cf   e             workstn
001700160929     f                                     indds(IndDspF)
001800160929     f                                     infds(InfDspF)
001900160929
002000160929       //--------------------------------------------------------------
002100160929       //?Definizione costanti.                                        ?
002200160929       //--------------------------------------------------------------
002300160929       // - Codice tabella in gestione
002400170315     d c_Tab           c                   const('GPD')
002500160929
002600160929       // - Tasti funzionali a video
002700160929     d c_F01           c                   const(x'31')
002800160929     d c_F02           c                   const(x'32')
002900160929     d c_F03           c                   const(x'33')
003000160929     d c_F05           c                   const(x'35')
003100160929     d c_F06           c                   const(x'36')
003200160929     d c_F07           c                   const(x'37')
003300160929     d c_F08           c                   const(x'38')
003400160929     d c_F09           c                   const(x'39')
003500160929     d c_F10           c                   const(x'3A')
003600160929     d c_F12           c                   const(x'3C')
003700160929     d c_F13           c                   const(x'B1')
003800160929     d c_F14           c                   const(x'B2')
003900160929     d c_F15           c                   const(x'B3')
004000160929     d c_F16           c                   const(x'B4')
004100160929     d c_F17           c                   const(x'B5')
004200160929     d c_F18           c                   const(x'B6')
004300160929     d c_F19           c                   const(x'B7')
004400160929     d c_F20           c                   const(x'B8')
004500160929     d c_F21           c                   const(x'B9')
004600160929     d c_F22           c                   const(x'BA')
004700160929     d c_F23           c                   const(x'BB')
004800160929     d c_F24           c                   const(x'BC')
004900160929     d c_Enter         c                   const(x'F1')
005000160929     d c_RollDown      c                   const(x'F4')
005100160929     d c_RollUp        c                   const(x'F5')
005200160929
005300170315     d Digits          c                   const('0123456789')
005400170315
005500160929       //--------------------------------------------------------------
005600160929       //?Definizione schiere.                                         ?
005700160929       //--------------------------------------------------------------
005800160929       // - Messaggi di errore
005900160929     d Msg             s             78    dim(20)  ctdata  perrcd(01)
006000160929
006100160929       //--------------------------------------------------------------
006200160929       //?Definizione aree dati.                                       ?
006300160929       //--------------------------------------------------------------
006400160929       // - Dati utente
006500160929     d §AzUte        e ds                  extname(AZUTE00F)
006600160929     d                                     dtaara
006700160929     d §DatiUte      e ds                  extname(dDatiUte)
006800160929     d                                     dtaara
006900160929
007000160929       //--------------------------------------------------------------
007100160929       //?Definizione strutture dati.                                  ?
007200160929       //--------------------------------------------------------------
007300160929       // - Status ds
007400160929     d Status         sds
007500160929     d  SDSpgm           *proc
007600160929
007700160929       // - InfDS
007800160929     d InfDspF         ds
007900160929     d   dsp_aid             369    369a
008000160929
008100160929       // - Indicatori su DspF
008200160929     d IndDspF         ds                  inz
008300160929       // - Abilitazione tasti funzionali
008400160929     d  F5Attivo                       n   overlay(IndDspF : 05)
008500160929     d  F6Attivo                       n   overlay(IndDspF : 06)
008600160929     d  F10Attivo                      n   overlay(IndDspF : 10)
008700160929     d  F16Attivo                      n   overlay(IndDspF : 16)
008800160929       // - Emissione messaggio di errore
008900160929     d  ErrMessage                     n   overlay(IndDspF : 28)
009000160929       // - Posizionamento cursore & segnalazione errore
009100170315     d  PosCurFIL                      n   overlay(IndDspF : 50)
009200170315     d  PosCurLUN                      n   overlay(IndDspF : 51)
009300170315     d  PosCurMAR                      n   overlay(IndDspF : 52)
009400170315     d  PosCurMER                      n   overlay(IndDspF : 53)
009500170315     d  PosCurGIO                      n   overlay(IndDspF : 54)
009600170315     d  PosCurVEN                      n   overlay(IndDspF : 55)
009700170315     d  PosCurSAB                      n   overlay(IndDspF : 56)
009800170315     d  PosCurDOM                      n   overlay(IndDspF : 57)
009900160929       //   - Riemissione videata
010000160929     d  ErrGenerico                    n   overlay(IndDspF : 99)
010100160929
010200160929       // - Parametri ricevuti
010300160929     d KPJBA         e ds
010400160929
010500160929       // - Parametri per Reperimento dati utente
010600160929     d TIBS34DS      e ds
010700160929
010800160929       // - Parametri per Ricerca/controllo tabelle
010900160929     d TIBS02ds      e ds                  inz
011000160929     d  T02mod       e                     inz('R')
011100160929     d  T02cod       e                     inz(c_Tab)
011200160929
011300170315       // - Tab. GPD = Giorni Posticipo Data
011400170315     d dGPD          e ds                  inz
011500160929
011600170315       // - Parametri per Interrogazione/Selezione tab. "GPD"
011700170315     d TNTBGPDDS     e ds                  inz
011800160929
011900160929       // - Dati record principale della tabella
012000160929     d TNTBEds       e ds                  inz  extname(TNTBE00F)
012100160929     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
012200160929     d                                          prefix(TBX:3)
012300161012
012400161012     d WindDspF        ds                  inz
012500161012     d  WindDspFa              1     49    inz(*zero)
012600161012     d  WindDspFb             50     99    inz(*zero)
012700160929
012800160929       //--------------------------------------------------------------
012900160929       //?Definizione variabili globali.                               ?
013000160929       //--------------------------------------------------------------
013100160929       // - Flags booleani
013200160929     d Fine            s               n   inz
013300160929     d wInzD01         s               n   inz(*on)
013400160929     d wInzD02         s               n   inz(*on)
013500160929     d wInzD03         s               n   inz(*on)
013600160929     d wInzW01         s               n   inz(*on)
013700160929     d Annullamento    s               n   inz
013800160929     d Immissione      s               n   inz
013900160929     d Ripristino      s               n   inz
014000160929
014100160929       // - Indici di schiera
014200160929     d xx              s              3  0 inz
014300160929
014400160929       // - Variabili per la gestione del video
014500160929     d Video           s              2    inz('D1')
014600160929
014700160929       // - Flag di errore
014800160929     d Err             s              1    inz(*off)
014900160929
015000160929       // - Campi di comodo
015100170315     d wDate_EUR       s               d   datfmt(*eur) inz(*loval)
015200170315     d w0030           s              3s 0 inz
015300160929
015400160929       //--------------------------------------------------------------
015500160929       //?Definizione prototipi procedure.                             ?
015600160929       //--------------------------------------------------------------
015700160929       // - Reperimento dati utente
015800160929      /copy gaitrasrc/srcProtoPR,TIBS34R
015900160929
016000160929       // - Ricerca e controllo nuove tabelle
016100160929      /copy gaitrasrc/srcProtoPR,TIBS02R
016200160929
016300161212       // - Selezione singola causale assenza
016400170315     d tntbGPDr1       pr                  extpgm('TNTBGPDR1')
016500160929     d   kpjba                             likeds(KPJBA)
016600170315     d   tntbGPDds                         likeds(TNTBGPDDS)
016700160929
016800160929       //--------------------------------------------------------------
016900160929       //?Definizione key-list.                                        ?
017000160929       //--------------------------------------------------------------
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
018900160929       // Operazioni iniziali?
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
020100160929         // Richiesta dati per trasmissione record
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
023000160929         clear k_TBEke2;
023100160929         clear k_TBElin;
023200160929         k_TBEsif = KNSIF;
023300160929         chain(n) %kds(k05tntbe01) TNTBE01L;
023400160929         IF  not %found(TNTBE01L);
023500160929           clear k_TBEsif;
023600160929           chain(n) %kds(k05tntbe01) TNTBE01L;
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
025300160929         wInzD01 = *off;
025400160929
025500160929       ENDSR;
025600160929
025700160929       //--------------------------------------------------------------
025800160929       //?Reperimento Dati del job (Utente/Operativi).                 ?
025900160929       //--------------------------------------------------------------
026000160929       BEGSR DatiJob;
026100160929
026200160929         in(E) §AzUte;
026300160929         IF  not %error;
026400160929           in(E) §DatiUte;
026500160929         ENDIF;
026600160929         IF  %error or RSut = *blanks;
026700160929           clear TIBS34DS;
026800160929           tibs34r ( tibs34ds );
026900160929           in §AzUte;
027000160929           in §DatiUte;
027100160929         ENDIF;
027200160929
027300160929       ENDSR;
027400160929
027500160929       //--------------------------------------------------------------
027600160929       //?Gestione videata D01                                         ?
027700160929       //--------------------------------------------------------------
027800160929       BEGSR GesD01;
027900160929
028000160929       // Inizializzazione videata?
028100160929         IF  wInzD01;
028200160929           exsr InzD01;
028300160929           wInzD01 = *off;
028400160929         ENDIF;
028500160929
028600160929       //?Emissione videata?
028700170315         write TBGPDT01;
028800170315         write TBGPDP01;
028900170315         exfmt TBGPDD01;
029000160929
029100160929         reset ErrMessage;
029200160929         reset ErrGenerico;
029300160929         clear V1Dmsg;
029400160929
029500160929         SELECT;
029600160929
029700160929         // F3=Fine?
029800160929         WHEN  dsp_aid = c_F03;
029900160929             Fine = *on;
030000160929
030100160929         // Invio?
030200160929         OTHER;
030300160929           exsr CtrD01;
030400160929           IF  not ErrGenerico;
030500160929             Video   = 'D2';
030600160929             wInzD02 = *on;
030700160929           ENDIF;
030800160929
030900160929         ENDSL;
031000160929
031100160929       ENDSR;
031200160929
031300160929       //--------------------------------------------------------------
031400160929       //?Inizializzazione videata D01                                 ?
031500160929       //--------------------------------------------------------------
031600160929       BEGSR InzD01;
031700160929
031800170316         V1Cfil = '?';
031900170315         clear V1Cdes;
032000160929
032100160929       ENDSR;
032200160929
032300160929       //--------------------------------------------------------------
032400160929       //?Controllo dati immessi nella videata D01                     ?
032500160929       //--------------------------------------------------------------
032600160929       BEGSR CtrD01;
032700161012
032800161012         WindDspF = IndDspF;
032900161012         reset WindDspFb;
033000161012         IndDspF  = WindDspF;
033100170315
033200170315         clear V1Cdes;
033300170315
033400170315         //?Obbligatoria?
033500170315         IF  V1Cfil = *blanks;
033600170315           ErrMessage  = *on;
033700170315           ErrGenerico = *on;
033800170315           PosCurFIL   = *on;
033900170315           V1Dmsg = Msg(01);
034000170315           leavesr;
034100170315         ENDIF;
034200170315
034300170315         //?Ricerca Filiale?
034400170315         IF  %scan('?' : V1Cfil) > *zeros;
034500170315           clear TNTBGPDDS;
034600170315           kpjbu = 'TNTBGPDR' ;
034700170315           tntbgpdr1 (kpjba:tntbgpdds);
034800161012           SELECT;
034900170315           WHEN  OGPDfxx <> *blank;
035000170315           WHEN  OGPDerr = *on;
035100161012             ErrMessage  = *on;
035200170315             PosCurFIL   = *on;
035300170315             V1Dmsg = OGPDmsg;
035400161012           OTHER;
035500170315             V1Cfil = %subst(OGPDke1 : 1 : %len(V1Cfil));
035600161012           ENDSL;
035700161012           ErrGenerico = *on;
035800161012           leavesr;
035900170315         ENDIF;
036000160929
036100170315         //?Se 999 OK?
036200170315         IF  V1Cfil = '999';
036300170315           V1Cdes = 'Tutte le Filiali';
036400170315           leavesr;
036500170315         ENDIF;
036600170315
036700170315         //?Valida?
036800170315         xx = 1;
036900170315         FOR xx by 1 to %len(V1Cfil);
037000170315           IF  %subst(V1Cfil:xx:1) <> *blanks and
037100170315               %check(Digits:%subst(V1Cfil:xx:1)) > *zeros;
037200170315             ErrMessage  = *on;
037300170315             ErrGenerico = *on;
037400170315             V1Dmsg = Msg(01);
037500170315             PosCurFIL = *on;
037600170315             leavesr;
037700170315           ENDIF;
037800170315           IF  %subst(V1Cfil:xx:1) = *blanks;
037900170315             %subst(V1Cfil:xx:1) = '0';
038000170315           ENDIF;
038100170315         ENDFOR;
038200170315         w0030 = %dec(V1Cfil:3:0);
038300170315         chain w0030 AZORG01L;
038400170315         IF  not %found(AZORG01L) or
038500170315             ORGfva <> *blanks or
038600170315            (ORGfag <> 'A' and ORGfag <> 'F');
038700170315           ErrMessage  = *on;
038800170315           ErrGenerico = *on;
038900170315           V1Dmsg = Msg(01);
039000170315           PosCurFIL = *on;
039100170315           leavesr;
039200170315         ENDIF;
039300170315         V1Cdes = ORGdes;
039400160929
039500160929       ENDSR;
039600160929
039700160929       //--------------------------------------------------------------
039800160929       //?Gestione videata D02                                         ?
039900160929       //--------------------------------------------------------------
040000160929       BEGSR GesD02;
040100160929
040200160929       //?Inizializzazione videata?
040300160929         IF  wInzD02;
040400160929           exsr InzD02;
040500160929           wInzD02 = *off;
040600160929         ENDIF;
040700160929
040800160929       // Emissione Testata e Piede con tasti funzionali abilitati?
040900161012         IF  not ErrGenerico;
041000170315           write TBGPDT01;
041100170315           write TBGPDD01;
041200160929           write Protect;
041300170315           write TBGPDP02;
041400161012         ENDIF;
041500160929
041600160929       // Emissione videata?
041700170315         exfmt TBGPDD02;
041800160929
041900160929         reset ErrMessage;
042000160929         reset ErrGenerico;
042100161012         reset Annullamento;
042200161012         clear V2Dmsg;
042300160929
042400160929         SELECT;
042500160929
042600160929       // F3=Fine?
042700160929         WHEN  dsp_aid = c_F03;
042800160929           unlock TNTBE01L;
042900160929           Fine = *on;
043000160929
043100160929       // F12=Ritorno?
043200160929         WHEN  dsp_aid = c_F12;
043300160929           unlock TNTBE01L;
043400160929           reset Video;
043500160929           clear V1Topz;
043600160929
043700160929       // RollUp=Videata successiva?
043800160929         WHEN  dsp_aid = c_RollUp;
043900160929           Video = 'D3';
044000160929
044100160929       // Enter; F5=Ripristino; F6=Conferma; F16=Annullamento?
044200160929         OTHER;
044300160929           Ripristino   = (dsp_aid = c_F05);
044400160929           Annullamento = (dsp_aid = c_F16);
044500160929           IF  not Annullamento;
044600160929             exsr CtrD02;
044700160929             IF  ErrGenerico;
044800160929               leavesr;
044900160929             ENDIF;
045000160929           ENDIF;
045100160929           SELECT;
045200160929           WHEN  dsp_aid = c_F05  or
045300160929                 dsp_aid = c_F06  or
045400160929                 dsp_aid = c_F16;
045500160929             Video = 'W1';
045600160929             reset wInzW01;
045700160929           ENDSL;
045800160929
045900160929         ENDSL;
046000160929
046100160929       ENDSR;
046200160929
046300160929       //--------------------------------------------------------------
046400160929       //?Inizializzazione videata D02                                 ?
046500160929       //--------------------------------------------------------------
046600160929       BEGSR InzD02;
046700160929
046800160929       // Pulizia videata?
046900170315         clear TBGPDD02;
047000160929
047100161212       // Reperimento dati della causale assenza autisti  ?
047200160929         reset Immissione;
047300160929         reset Ripristino;
047400170315         k_TBEke1 = V1Cfil;
047500160929         chain %kds(k05tntbe01) TNTBE01L;
047600160929         SELECT;
047700160929         WHEN  not %found(TNTBE01L);
047800160929           Immissione = *on;
047900160929         WHEN  TBEatb <> *blank;
048000160929           Ripristino = *on;
048100160929         ENDSL;
048200160929
048300161212       // Caricamento dati della causale assenza autisti  ?
048400160929         IF  %found(TNTBE01L);
048500160929           exsr RieD02;
048600160929         ENDIF;
048700160929
048800160929       // Impostazione tipo manutenzione?
048900160929         SELECT;
049000160929         WHEN  Immissione;
049100160929           V1Topz = 'IMMISSIONE';
049200160929         WHEN  Ripristino;
049300160929           V1Topz = 'RIPRISTINO';
049400160929         OTHER;
049500160929           V1Topz = ' MODIFICA ';
049600160929         ENDSL;
049700160929
049800160929       // Impostazione indicatori per abilitazione tasti funzionali?
049900160929         exsr Set_Ind_D02;
050000160929
050100160929       ENDSR;
050200160929
050300160929       //--------------------------------------------------------------
050400160929       //?Caricamento singola riga di testo nella videata D02          ?
050500160929       //--------------------------------------------------------------
050600160929       BEGSR RieD02;
050700160929
050800170315         dGPD = TBEuni;
050900160929
051000170315         V1lun  = §GPDlun;
051100170315         V1mar  = §GPDmar;
051200170315         V1mer  = §GPDmer;
051300170315         V1gio  = §GPDgio;
051400170315         V1ven  = §GPDven;
051500170315         V1sab  = §GPDsab;
051600170315         V1dom  = §GPDdom;
051700170315         V1luna = §GPDluna;
051800170315         V1mara = §GPDmara;
051900170315         V1mera = §GPDmera;
052000170315         V1gioa = §GPDgioa;
052100170315         V1vena = §GPDvena;
052200170315         V1saba = §GPDsaba;
052300170315         V1doma = §GPDdoma;
052400160929
052500170315         // Controlli
052600160929         exsr CtrD02;
052700160929
052800160929       ENDSR;
052900160929
053000160929       //--------------------------------------------------------------
053100160929       //?Settaggio indicatori nella videata D02 per abilitazione      ?
053200160929       //?  tasti funzionali (sul subfile control)                     ?
053300160929       //--------------------------------------------------------------
053400160929       BEGSR Set_Ind_D02;
053500160929
053600160929       // F5=Ripristino?
053700160929         F5Attivo = (not Immissione and Ripristino);
053800160929
053900160929       // F6=Conferma?
054000160929         F6Attivo = (not F5Attivo);
054100160929
054200160929       // F16=Annullamento?
054300160929         F16Attivo = (not Immissione and not Ripristino);
054400160929
054500160929       ENDSR;
054600160929
054700160929       //--------------------------------------------------------------
054800160929       //?Controllo dati videata D02                                   ?
054900160929       //--------------------------------------------------------------
055000160929       BEGSR CtrD02;
055100161012
055200161012       // Spegnimento di TUTTI gli indicatori?
055300161012         WindDspF = IndDspF;
055400161012         reset WindDspFb;
055500161012         IndDspF  = WindDspF;
055600160929
055700170315       // L'anticipo non può essere maggiore del posticipo
055800170315         IF  V1luna > V1lun;
055900160929           ErrMessage  = *on;
056000160929           ErrGenerico = *on;
056100170315           PosCurLUN   = *on;
056200161012           V2Dmsg = Msg(02);
056300160929           leavesr;
056400170315         ENDIF;
056500170315         IF  V1mara > V1mar;
056600170315           ErrMessage  = *on;
056700170315           ErrGenerico = *on;
056800170315           PosCurMAR   = *on;
056900170315           V2Dmsg = Msg(02);
057000170315           leavesr;
057100170315         ENDIF;
057200170315         IF  V1mera > V1mer;
057300170315           ErrMessage  = *on;
057400170315           ErrGenerico = *on;
057500170315           PosCurMER   = *on;
057600170315           V2Dmsg = Msg(02);
057700170315           leavesr;
057800170315         ENDIF;
057900170315         IF  V1gioa > V1gio;
058000170315           ErrMessage  = *on;
058100170315           ErrGenerico = *on;
058200170315           PosCurGIO   = *on;
058300170315           V2Dmsg = Msg(02);
058400170315           leavesr;
058500170315         ENDIF;
058600170315         IF  V1vena > V1ven;
058700170315           ErrMessage  = *on;
058800170315           ErrGenerico = *on;
058900170315           PosCurVEN   = *on;
059000170315           V2Dmsg = Msg(02);
059100170315           leavesr;
059200170315         ENDIF;
059300170315         IF  V1saba > V1sab;
059400170315           ErrMessage  = *on;
059500170315           ErrGenerico = *on;
059600170315           PosCurSAB   = *on;
059700170315           V2Dmsg = Msg(02);
059800170315           leavesr;
059900170315         ENDIF;
060000170315         IF  V1doma > V1dom;
060100170315           ErrMessage  = *on;
060200170315           ErrGenerico = *on;
060300170315           PosCurDOM   = *on;
060400170315           V2Dmsg = Msg(02);
060500170315           leavesr;
060600170315         ENDIF;
060700160929
060800160929       ENDSR;
060900160929
061000160929       //--------------------------------------------------------------
061100160929       //?Gestione trasmissione aggiornamento                          ?
061200160929       //--------------------------------------------------------------
061300160929       BEGSR GesW01;
061400160929
061500160929       // Inizializzazione videata
061600160929         IF  wInzW01;
061700160929           exsr InzW01;
061800160929           wInzW01 = *off;
061900160929         ENDIF;
062000160929
062100160929       // Emissione window
062200160929         IF  TBXftt = 'S';
062300170315           exfmt TBGPDW01;
062400160929           ErrMessage  = *off;
062500160929           ErrGenerico = *off;
062600160929           clear V1Dmsg;
062700160929         ELSE;
062800160929           dsp_aid = c_F06;
062900160929         ENDIF;
063000160929
063100160929         SELECT;
063200160929
063300160929       // F12=Ritorno (gestito solo se emesso W01)
063400160929         WHEN  dsp_aid = c_F12;
063500160929           Video = 'D2';
063600160929
063700160929       // F6=Conferma
063800160929         WHEN  dsp_aid = c_F06;
063900160929           exsr  UpdTNTBE;
064000160929         // Ritorno alla videata iniziale
064100160929           IF  not ErrGenerico;
064200160929             clear V1Topz;
064300160929             Video = 'D1';
064400160929             wInzD01 = *on;
064500160929           ENDIF;
064600160929         ENDSL;
064700160929
064800160929       ENDSR;
064900160929
065000160929       //--------------------------------------------------------------
065100160929       //?Inizializzazione videata W01                                 ?
065200160929       //--------------------------------------------------------------
065300160929       BEGSR InzW01;
065400160929
065500170315         clear TBGPDW01;
065600160929
065700160929         IF  Immissione;
065800160929
065900160929       // Se immissione
066000160929           W1ftt  = TBXftt;
066100160929
066200160929         ELSE;
066300160929
066400160929       // Se NON immissione:
066500160929       // visualizza i dati relativi all'ultima trasmissione
066600160929           W1ftt = TBEftt;
066700160929           W1flt = TBEflt;
066800160929           W1ftr = TBEftr;
066900160929           IF  TBEdtr <> *zero;
067000160929             wDate_EUR = %date(TBEdtr : *iso);
067100160929             W1dtr     = %dec(wDate_EUR);
067200160929           ENDIF;
067300160929
067400160929         ENDIF;
067500160929
067600160929       ENDSR;
067700160929
067800160929       //--------------------------------------------------------------
067900170315       //?Aggiornamento records TNTBE00F (tab. "GPD")                  ?
068000160929       //--------------------------------------------------------------
068100160929       BEGSR UpdTNTBE;
068200160929
068300160929       // Aggancio record di TNTBE00F?
068400160929         k_TBEcod = c_Tab;
068500170315         k_TBEke1 = V1Cfil;
068600160929         clear k_TBEke2;
068700160929         clear k_TBElin;
068800160929         clear k_TBEsif;
068900160929
069000160929         chain  %kds(K05tntbe01) TNTBE01L;
069100160929
069200170315         exsr RieGPD;
069300170315         TBEuni = dGPD;
069400160929
069500170315       // Aggiornamento tab. "GPD"?
069600160929         SELECT;
069700160929
069800160929       // F5=Ripristino?
069900160929         WHEN  Ripristino;
070000160929           clear TBEatb;
070100160929           TBEftt = W1ftt;
070200160929           clear TBEftr;
070300160929           //_______________
070400160929           UPDATE  TNTBE000;
070500160929           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
070600160929
070700160929       // F16=Annullamento?
070800160929         WHEN  Annullamento;
070900160929           TBEatb = 'A';
071000160929           TBEftt = W1ftt;
071100160930           clear TBEftr;
071200160929           //_______________
071300160929           UPDATE  TNTBE000;
071400160929           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
071500160929
071600160929       // Immissione o Modifica?
071700160929         OTHER;
071800160929
071900160929           IF  not %found(TNTBE01L);
072000160929             clear TNTBE000;
072100160929             TBEcod = k_TBEcod;
072200160929             TBEke1 = k_TBEke1;
072300160929             TBEke2 = k_TBEke2;
072400160929             TBElin = k_TBElin;
072500160929             TBEsif = k_TBEsif;
072600170315             TBEuni = dGPD;
072700160929           ENDIF;
072800160929
072900160929           TBEapl = TBXapl;
073000160929           clear TBEatb;
073100160929           TBEftt = W1ftt;
073200160929           clear TBEflt;
073300160929           clear TBEftr;
073400160929
073500160929         // Aggiornamento record
073600160929           IF  %found(TNTBE01L);
073700160929             //_____________
073800160929             UPDATE TNTBE000;
073900160929             //¯¯¯¯¯¯¯¯¯¯¯¯¯
074000160929           ELSE;
074100160929             //_____________
074200160929             WRITE  TNTBE000;
074300160929             //¯¯¯¯¯¯¯¯¯¯¯¯¯
074400160929           ENDIF;
074500160929
074600160929         ENDSL;
074700160929
074800160929       ENDSR;
074900160929
075000160929       //--------------------------------------------------------------
075100170315       //?Caricamento dati in tab. "GPD"                               ?
075200160929       //--------------------------------------------------------------
075300170315       BEGSR RieGPD;
075400160929
075500170315         clear dGPD;
075600160929
075700170315         §GPDlun  = V1lun;
075800170315         §GPDmar  = V1mar;
075900170315         §GPDmer  = V1mer;
076000170315         §GPDgio  = V1gio;
076100170315         §GPDven  = V1ven;
076200170315         §GPDsab  = V1sab;
076300170315         §GPDdom  = V1dom;
076400170315         §GPDluna = V1luna;
076500170315         §GPDmara = V1mara;
076600170315         §GPDmera = V1mera;
076700170315         §GPDgioa = V1gioa;
076800170315         §GPDvena = V1vena;
076900170315         §GPDsaba = V1saba;
077000170315         §GPDdoma = V1doma;
077100160929
077200160929       ENDSR;
077300160929
077400160929       //--------------------------------------------------------------
077500160929       //?Operazioni finali.                                           ?
077600160929       //--------------------------------------------------------------
077700160929       BEGSR RoutEnd;
077800160929
077900160929       // Chiusura funzioni precedentemente aperte?
078000160929         clear TIBS02DS;
078100160929         T02tla = 'C';
078200160929         TNTBE_RicercaControllo (kpjba:tibs02ds);
078300160929
078400160929       // Chiusura pgm?
078500160929         return;
078600160929
078700160929       ENDSR;
078800160929
078900160929      /end-free
079000160929
079100160929       //--------------------------------------------------------------
079200160929       //?Definizione schiere a tempo di compilazione                  ?
079300160929       //--------------------------------------------------------------
079400160929
079500160929** >  Msg -------------------------------------------------------------------*
079600170315Filiale errata                                                                  1
079700170315L'anticipo non può essere maggiore del posticipo                                2
