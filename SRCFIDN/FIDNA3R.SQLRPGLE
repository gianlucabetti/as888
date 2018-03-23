000100150318      //--------------------------------------------------------------
000200150318      //?FIDNA3R - STORICO VARIAZIONI
000300150318      //--------------------------------------------------------------
000400150318
000500150318     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600150318     h dftactgrp(*no) actgrp(*caller)
000700150318
000800150318      //---------------------------------------------------------------
000900150318      //?Dichiarazione file.
001000150318      //---------------------------------------------------------------
001100151204      // - Organigramma
001200151204     fAZORG01L  if   e           k disk
001300150318
001400150318      // - Video
001500150318     fFIDNA3D   cf   e             workstn
001600151203     f                                     sfile(DNA3S02 : S02nrr)
001700151203     f                                     sfile(DNA3S03 : S03nrr)
001800150318     f                                     indds(IndDspF)
001900150318     f                                     infds(InfDspF)
002000150318
002100150318      //---------------------------------------------------------------
002200150318      //?Definizione costanti.
002300150318      //---------------------------------------------------------------
002400150318
002500150318      // - Tasti funzionali a video
002600150318     d c_F01           c                   const(x'31')
002700150318     d c_F02           c                   const(x'32')
002800150318     d c_F03           c                   const(x'33')
002900150318     d c_F04           c                   const(x'34')
003000150318     d c_F05           c                   const(x'35')
003100150318     d c_F06           c                   const(x'36')
003200150318     d c_F07           c                   const(x'37')
003300150318     d c_F08           c                   const(x'38')
003400150318     d c_F09           c                   const(x'39')
003500150318     d c_F10           c                   const(x'3A')
003600150318     d c_F11           c                   const(x'3B')
003700150318     d c_F12           c                   const(x'3C')
003800150318     d c_F13           c                   const(x'B1')
003900150318     d c_F14           c                   const(x'B2')
004000150318     d c_F15           c                   const(x'B3')
004100150318     d c_F16           c                   const(x'B4')
004200150318     d c_F17           c                   const(x'B5')
004300150318     d c_F18           c                   const(x'B6')
004400150318     d c_F19           c                   const(x'B7')
004500150318     d c_F20           c                   const(x'B8')
004600150318     d c_F21           c                   const(x'B9')
004700150318     d c_F22           c                   const(x'BA')
004800150318     d c_F23           c                   const(x'BB')
004900150318     d c_F24           c                   const(x'BC')
005000150318     d c_Enter         c                   const(x'F1')
005100150318     d c_RollDown      c                   const(x'F4')
005200150318     d c_RollUp        c                   const(x'F5')
005300150318
005400150318     d Digits          c                   const('0123456789')
005500150318
005600150318      //---------------------------------------------------------------
005700150318      //?Definizione schiere.
005800150318      //---------------------------------------------------------------
005900151203      // - Messaggi di errore
006000151203     d Msg             s             78    dim(20) ctdata perrcd(1)
006100150318
006200150318      //---------------------------------------------------------------
006300150318      //?Definizione aree dati.
006400150318      //---------------------------------------------------------------
006500150318
006600150318      // - Dati utente
006700150318     d §AzUte        e ds                  extname(AZUTE00F)
006800150318     d                                     dtaara
006900150318     d §DatiUte      e ds                  extname(dDatiUte)
007000150318     d                                     dtaara
007100150318
007200150318      //---------------------------------------------------------------
007300150318      //?Definizione strutture dati.
007400150318      //---------------------------------------------------------------
007500150318
007600150318      // - Status
007700150318     d Psds           sds
007800150318     d   SDSpgm          *proc
007900150318
008000150318      // - InfDS
008100150318     d InfDspF         ds
008200150318     d  dsp_aid              369    369a
008300150318     d  sfl_rrn              376    377i 0
008400150318     d  min_nrr              378    379i 0
008500150318     d  num_rcds             380    381i 0
008600150318
008700150318      // - Indicatori su DspF
008800150318     d IndDspF         ds
008900151204        // - Indicatori di abilitazioni Tasti Funzione
009000151204     d  AbilitaF09                    1n   overlay(IndDspF : 09)
009100150318        // - Indicatori di errore in videata
009200150318     d  ErrMessage                    1n   overlay(IndDspF : 28)
009300150318        // - Indicatori di gestione del subfile
009400151204     d  SflDsp02                      1n   overlay(IndDspF : 30)
009500151204     d  SflDspCtl02                   1n   overlay(IndDspF : 31)
009600150318     d  SflNxtChg                     1n   overlay(IndDspF : 32)
009700151204     d  SflEnd02                      1n   overlay(IndDspF : 33)
009800151204     d  SflDsp03                      1n   overlay(IndDspF : 34)
009900151204     d  SflDspCtl03                   1n   overlay(IndDspF : 35)
010000151204     d  SflEnd03                      1n   overlay(IndDspF : 36)
010100151204        // - Indicatori di visualizzazione campi
010200151204     d  DataOraRi                     1n   overlay(IndDspF : 40)
010300151204     d  DataOraNd                     1n   overlay(IndDspF : 41)
010400151203        // - Indicatori di errore
010500151203     d  PosCurOPZ                     1n   overlay(IndDspF : 50)
010600150318        // - Indicatori di errore generico
010700150318     d  ErrGenerico                   1n   overlay(IndDspF : 99)
010800150318
010900150318     d WindDspF        ds                  inz
011000150318     d  WindDspFa              1     49    inz(*zero)
011100150318     d  WindDspFb             50     99    inz(*zero)
011200150318
011300150318      // - Parametri ricevuti
011400150318     d KPJBA         e ds
011500150318     d FIDNA0DS      e ds
011600150318
011700150318      // - File Storico Variazioni
011800150318     d FITGM00F      e ds                  extname(FITGM00F)
011900150318
012000150318      // - Ricerca/Controllo tabelle
012100150318     d TIBS02DS      e ds                  inz
012200150318
012300150318      // - Reperimento dati utente
012400150318     d TIBS34DS      e ds
012500151204
012600151204      // - Reperimento dati Anagrafica Clienti
012700151204      /copy gaitrasrc/srcprotopi,TIBS69R
012800150318
012900150318      // - Tabella CHR - Causale Chiusura
013000150318     d dCHR          e ds
013100150326
013200150326      // - Tabella PRR - Stato RA
013300150326     d dPRR          e ds
013400150318
013500150318      // - Tabella TMO - Tipo Modifica
013600150318     d dTMO          e ds
013700151218
013800151218      // - Ds Variazioni di tipo 'A' - Richiesta Risposta
013900151218     d dTGMa         e ds
014000150318
014100150318      // - Ds Variazioni di tipo 'C' - Causale Chiusura
014200150318     d dTGMc         e ds
014300150318
014400150318      // - Ds Variazioni di tipo 'K' - Cod. Chiamante
014500150318     d dTGMk         e ds
014600150318
014700150318      // - Ds Variazioni di tipo 'R' - Filiale Responsabile
014800150318     d dTGMr         e ds
014900150326
015000150326      // - Ds Variazioni di tipo 'S' - Stato
015100150326     d dTGMs         e ds
015200150318
015300150318      // - Ds Variazioni di tipo 'U' - Utente/Filiale gestione
015400150318     d dTGMu         e ds
015500151203
015600151203      // - Ds per data variazione
015700151203     d                 ds                  inz
015800151203     d wDataVar                1      8  0 inz
015900151203     d wgg                     1      2  0 inz
016000151203     d wmm                     3      4  0 inz
016100151203     d waa4                    5      8  0 inz
016200151203     d waa2                    6      8  0 inz
016300150318
016400150318      //---------------------------------------------------------------
016500150318      //?Definizione variabili globali.
016600150318      //---------------------------------------------------------------
016700150318
016800150318      // - Flags booleani
016900150318     d EndS02          s               n   inz(*off)
017000150318     d Fine            s               n   inz(*off)
017100151204     d Reparto         s               n   inz(*off)
017200150318     d wInzS02         s               n   inz(*off)
017300150318
017400150318      // - Indici di schiera
017500150318     d xx              s              4s 0 inz
017600150318
017700150318      // - Campi associati al video
017800150318     d Video           s              2a   inz('D2')
017900150318     d S02nrr          s              4s 0 inz
018000151203     d S03nrr          s              4s 0 inz
018100150318
018200150318      // - Campi di comodo data
018300150318     d Data_EUR        s               d   datfmt(*eur)
018400150318     d Data_ISO        s               d   datfmt(*iso)
018500150318
018600150318      // - Campi di comodo
018700150318     d Oggi            s              8s 0 inz
018800151204     d sav_S03nrr      s                   like(S03nrr)
018900151204     d sav_TGMdmo      s                   like(TGMdmo)
019000151204     d sav_TGMomo      s                   like(TGMomo)
019100151204     d sav_TGMutn      s                   like(TGMutn)
019200151204     d sav_VH3dtv      s                   like(VH3dtv)
019300151204     d sav_VS3ora      s                   like(VS3ora)
019400151204     d sav_VS3ute      s                   like(VS3ute)
019500151204     d tmoa            s                   like(VS2tmoa)
019600151204     d tmoc            s                   like(VS2tmoc)
019700151204     d tmok            s                   like(VS2tmok)
019800151204     d tmor            s                   like(VS2tmor)
019900151204     d tmos            s                   like(VS2tmos)
020000151204     d tmou            s                   like(VS2tmou)
020100151204     d wData           s                   like(TGMdmo)
020200151204     d wOra            s                   like(TGMomo)
020300151204     d wUte            s                   like(TGMutn)
020400150318
020500150318      //---------------------------------------------------------------
020600150318      //?Definizione procedure usate.
020700150318      //---------------------------------------------------------------
020800150318
020900150318      //---------------------------------------------------------------
021000150318      //?Definizione Prototipi.
021100150318      //---------------------------------------------------------------
021200150318      /copy gaitrasrc/srcprotopr,TIBS02R
021300150318      /copy gaitrasrc/srcprotopr,TIBS34R
021400151204      /copy gaitrasrc/srcprotopr,TIBS69R
021500150318
021600150318      //---------------------------------------------------------------
021700150318      //?Definizione key-list.
021800150318      //---------------------------------------------------------------
021900150318
022000150318      //---------------------------------------------------------------
022100150318      //?M A I N - L I N E
022200150318      //---------------------------------------------------------------
022300150318
022400150318     c     *Entry        plist
022500150318     c                   parm                    kpjba
022600150318     c                   parm                    FIDNA0DS
022700150318
022800150318      /free
022900150318
023000150318       //?Operazioni iniziali
023100150318       exsr RoutInz;
023200150318
023300150318       //?Gestione video
023400150318       DOW  Fine = *off;
023500150318         SELECT;
023600150318           WHEN  Video = 'S2';
023700150318             exsr GesS02;
023800151203           WHEN  Video = 'S3';
023900151203             exsr GesS03;
024000150318           OTHER;
024100150318             Fine = *on;
024200150318         ENDSL;
024300150318       ENDDO;
024400150318
024500150318       //?Operazioni finali
024600150318       exsr RoutEnd;
024700150318
024800150318       //--------------------------------------------------------------
024900150318       //?Operazioni iniziali.
025000150318       //--------------------------------------------------------------
025100150318       BEGSR RoutInz;
025200150318
025300150318         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
025400150318
025500150318       //?Impostazione campi "fissi"
025600150318         V01pgm = SDSpgm;
025700150318         Video = 'S2';
025800150318         wInzS02 = *on;
025900150318
026000150318       //?Imposto oggi
026100150318         Oggi = %dec(%date());
026200150318
026300150318       //?Reperimento dati job
026400150318         exsr DatiJob;
026500150318
026600150318       ENDSR;
026700150318
026800150318       //--------------------------------------------------------------
026900150318       //?Reperimento Dati del job (Utente/Operativi).
027000150318       //--------------------------------------------------------------
027100150318       BEGSR DatiJob;
027200150318
027300150318         in(E) §AzUte;
027400150318         IF  NOT %error;
027500150318           in(E) §DatiUte;
027600150318         ENDIF;
027700150318         IF  %error or RSut = *blanks;
027800150318           clear TIBS34ds;
027900150318           tibs34r(tibs34ds);
028000150318           in §AzUte;
028100150318           in §DatiUte;
028200150318         ENDIF;
028300150318
028400150318       ENDSR;
028500150318
028600150318       //--------------------------------------------------------------
028700150318       //?Gestione videata S02.
028800150318       //--------------------------------------------------------------
028900150318       BEGSR GesS02;
029000150318
029100150318       //?Inizializzazione videata
029200150318         IF  wInzS02;
029300150318           exsr InzS02;
029400150318           wInzS02 = *off;
029500150318         ENDIF;
029600150318
029700150318       //?Visualizzazione del SFL (se ci sono dati)
029800151204         SflDsp02 = (S02nrr <= *zeros);
029900150318
030000150318       //?Posizionamento cursore
030100150318         V02rcd = V02csr;
030200150318
030300150318       //?Emissione Testata e Piede
030400151204         write DNA3T01;
030500151204         write DNA3P02;
030600150318
030700150318       //?Emissione videata
030800151204         exfmt DNA3C02;
030900150318         reset ErrMessage;
031000150318         reset ErrGenerico;
031100150318         clear V02msg;
031200150318
031300150318         SELECT;
031400151203
031500151203       //?- F09=Analitico
031600151203           WHEN  dsp_aid = c_F09;
031700151203             exsr F09S02;
031800150318
031900150318       //?- F12=Ritorno
032000150318           WHEN  dsp_aid = c_F12;
032100150318             exsr F12S02;
032200150318
032300150318       //?Invio
032400150318           OTHER;
032500151204             IF  V02csr > *zeros;
032600151203               exsr OpzS02;
032700151203               IF  ErrGenerico;
032800151203                 leavesr;
032900151203               ENDIF;
033000151203             ENDIF;
033100150318
033200150318         ENDSL;
033300150318
033400150318       ENDSR;
033500150318
033600150318       //--------------------------------------------------------------
033700150318       //?Inizializzazione videata S02.
033800150318       //--------------------------------------------------------------
033900150318       BEGSR InzS02;
034000151204
034100151204       //?Inizializzo anche il subfile sintetico
034200151204       //?visto che viene caricato assieme al subfile analitico
034300151204         exsr InzS03;
034400150318
034500150318         EndS02 = *off;
034600151204         clear sav_TGMdmo;
034700151204         clear sav_TGMomo;
034800151204         clear sav_TGMutn;
034900150318
035000150318       //?Pulizia subfile
035100150318         exsr PulS02;
035200150318
035300150318       //?Caricamento subfile
035400150318         exsr Ries02;
035500150318
035600151204         SflEnd02 = *on;
035700151204         SflEnd03 = *on;
035800150318
035900150318       //?Imposto il posizionamento al primo rcd
036000150318         IF  S02nrr > 0;
036100150318           V02rcd = 1;
036200150318         ELSE;
036300150318           clear V02rcd;
036400150318         ENDIF;
036500150318
036600150318         V02csr = V02rcd;
036700150318
036800150318       ENDSR;
036900150318
037000150318       //--------------------------------------------------------------
037100150318       //?Pulizia Subfile S02.
037200150318       //--------------------------------------------------------------
037300150318       BEGSR PulS02;
037400150318
037500150318       //?Pulizia subfile
037600151204         SflDsp02 = *on;
037700151204         SflDspCtl02 = *on;
037800150318         write DNA3C02;
037900151204         SflDspCtl02 = *off;
038000151204         SflEnd02 = *off;
038100150318
038200150318         clear V02rcd;
038300150318         clear V02csr;
038400150318         clear S02nrr;
038500150318         clear V02msg;
038600150318
038700150318         ErrMessage  = *off;
038800150318         ErrGenerico = *off;
038900150318
039000150318         WindDspF = IndDspF;
039100150318         reset WindDspFb;
039200150318         IndDspF  = WindDspF;
039300150318
039400150318       ENDSR;
039500150318
039600150318       //--------------------------------------------------------------
039700150318       //?Caricamento Subfile S02.
039800150318       //--------------------------------------------------------------
039900150318       BEGSR RieS02;
040000150318
040100150318         EndS02 = *off;
040200150318
040300150318       //?Dichiarazione cursore
040400150318         exec sql
040500150318         declare TGM cursor for
040600150318         select * from FITGM00F
040700150318         where TGMant = :IA0ant and TGMnut = :IA0nut and
040800150318               TGMpdt = :IA0pdt and TGMpot = :IA0pot
040900151204         order by TGMdmo desc, TGMomo desc, TGMtmo;
041000150318
041100150318         //?Apertura del cursore
041200150318         exec sql
041300150318           open TGM;
041400150318
041500151204         IF  sqlcode < 0;
041600150318           EndS02 = *on;
041700150318           exec sql close TGM;
041800150318           leavesr;
041900150318         ENDIF;
042000150318
042100150318         DOW  not EndS02;
042200150318           exec sql
042300150318             fetch next from TGM into :FITGM00F;
042400151204           IF  sqlcod = 100 or sqlcod < 0;
042500150318             EndS02 = *on;
042600150318             leave;
042700150318           ENDIF;
042800151204
042900151204         //?Carico i dati nel subfile
043000151204         //?a rottura di data/ora
043100151204           IF  TGMdmo <> sav_TGMdmo or TGMomo <> sav_TGMomo or
043200151204               TGMutn <> sav_TGMutn;
043300151204             IF  sav_TGMomo > 0;
043400151204               exsr CarS02;
043500151204               clear tmoa;
043600151204               clear tmoc;
043700151204               clear tmok;
043800151204               clear tmor;
043900151204               clear tmos;
044000151204               clear tmou;
044100151204             ENDIF;
044200151204             sav_TGMdmo = TGMdmo;
044300151204             sav_TGMomo = TGMomo;
044400151204             sav_TGMutn = TGMutn;
044500151204           ENDIF;
044600151204
044700151204         //?Carico i dati nel subfile di dettaglio
044800151204           exsr CarS03;
044900151204
045000151204         //?Imposto le variazioni fatte
045100151204           SELECT;
045200151204         //?Variazione Richiesta Risposta
045300151204           WHEN  TGMtmo = 'A';
045400151204             tmoa = 'SI';
045500151204         //?Variazione causale chiusura
045600151204           WHEN  TGMtmo = 'C';
045700151204             tmoc = 'SI';
045800151204         //?Variazione Codice Chiamante
045900151204           WHEN  TGMtmo = 'K';
046000151204             tmok = 'SI';
046100151204         //?Variazione Filiale Responsabile
046200151204           WHEN  TGMtmo = 'R';
046300151204             tmor = 'SI';
046400151204         //?Variazione Stato
046500151204           WHEN  TGMtmo = 'S';
046600151204             tmos = 'SI';
046700151204         //?Variazione Utente/Filiale Gestione
046800151204           WHEN  TGMtmo = 'U';
046900151204             tmou = 'SI';
047000151204           ENDSL;
047100150318
047200150318         ENDDO;
047300151204
047400151204       //?scrivo l'ultimo rcd
047500151204         IF  sav_TGMomo > 0;
047600151204           exsr CarS02;
047700151204         ENDIF;
047800150318
047900150318         exec sql
048000150318           close TGM;
048100150318
048200150318       ENDSR;
048300150318
048400150318       //--------------------------------------------------------------
048500150318       //?Carico i dati nel Subfile S02.
048600150318       //--------------------------------------------------------------
048700150318       BEGSR CarS02;
048800150318
048900151204         clear DNA3S02;
049000151204
049100151204         VS2tmoa = tmoa;
049200151204         VS2tmoc = tmoc;
049300151204         VS2tmok = tmok;
049400151204         VS2tmor = tmor;
049500151204         VS2tmos = tmos;
049600151204         VS2tmou = tmou;
049700151203
049800151204         VH2dtv = sav_TGMdmo;
049900151204         Data_ISO = %date(sav_TGMdmo:*iso);
050000151203         Data_EUR = Data_ISO;
050100151203         wDataVar = %dec(Data_EUR);
050200151203         VS2data = wgg * 10000;
050300151203         VS2data += wmm * 100;
050400151203         VS2data += waa2;
050500151204         VS2ora = sav_TGMomo;
050600151204         VS2ute = sav_TGMutn;
050700150318
050800150318         S02nrr += 1;
050900150318
051000151204         write DNA3S02;
051100150318
051200150318       ENDSR;
051300151203
051400151203       //--------------------------------------------------------------
051500151204       //?Gestione tasto funzionale F09 da videata S02.
051600151203       //?F09=Analitico
051700151203       //--------------------------------------------------------------
051800151203       BEGSR F09S02;
051900151203
052000151203       //?Richiamo Subfile analitico
052100151203         Video = 'S3';
052200151203
052300151203       ENDSR;
052400150318
052500150318       //--------------------------------------------------------------
052600151204       //?Gestione tasto funzionale F12 da videata S02.
052700150318       //?F12=Ritorno
052800150318       //--------------------------------------------------------------
052900150318       BEGSR F12S02;
053000150318
053100150318       //?Chiusura del programma
053200150318         Fine = *on;
053300150318
053400150318       ENDSR;
053500151203
053600151203       //--------------------------------------------------------------
053700151203       //?Gestione opzioni Subfile S02.
053800151203       //--------------------------------------------------------------
053900151203       BEGSR OpzS02;
054000151203
054100151203         readc DNA3S02;
054200151203
054300151204         DOW  NOT %eof(FIDNA3D);
054400151203
054500151204           WindDspF = IndDspF;
054600151203           reset WindDspFb;
054700151204           IndDspF  = WindDspF;
054800151204
054900151204           SflNxtChg = *off;
055000151204           V02rcd = S02nrr;
055100151203
055200151203           PosCurOpz = *off;
055300151204           clear wData;
055400151204           clear wOra;
055500151204           clear wUte;
055600151203
055700151203           SELECT;
055800151203
055900151203         //?- Nessuna opzione
056000151203           WHEN  VS2opz  = *blank;
056100151203
056200151203         //?- 5 = Visualizza
056300151203           WHEN  VS2opz  = '5';
056400151203             exsr Opzione5;
056500151203
056600151203           OTHER;
056700151203             ErrMessage  = *on;
056800151203             ErrGenerico = *on;
056900151203             PosCurOpz   = *on;
057000151203             V02msg = Msg(01);
057100151203
057200151203           ENDSL;
057300151203
057400151203           //?Aggiornamento sfl
057500151203           SELECT;
057600151203             WHEN  ErrMessage;
057700151203               SflNxtChg = *on;
057800151203               V02csr    = V02rcd;
057900151203             WHEN  ErrGenerico;
058000151203               V02csr = V02rcd;
058100151203               clear VS2opz;
058200151203             OTHER;
058300151203               V02csr = V02rcd;
058400151203               clear VS2opz;
058500151203           ENDSL;
058600151203
058700151203           update DNA3S02;
058800151203
058900151203           IF  ErrMessage or ErrGenerico;
059000151203             leave;
059100151203           ENDIF;
059200151203
059300151203           readc DNA3S02;
059400151203
059500151203         ENDDO;
059600151203
059700151203       ENDSR;
059800151203
059900151203       //--------------------------------------------------------------
060000151203       //?Opzione 5 videata S02.
060100151203       //--------------------------------------------------------------
060200151203       BEGSR Opzione5;
060300151203
060400151204         wData = VH2dtv;
060500151204         wOra  = VS2ora;
060600151204         wUte  = VS2ute;
060700151203         Video = 'S3';
060800151203
060900151203       ENDSR;
061000151203
061100151203       //--------------------------------------------------------------
061200151203       //?Gestione videata S03.
061300151203       //--------------------------------------------------------------
061400151203       BEGSR GesS03;
061500151204
061600151204         AbilitaF09 = *on;
061700151204         sav_s03nrr = S03nrr;
061800151204
061900151204       //?Se richiesta una data/ora specifica leggo il subfile
062000151204       //?per impostare in RI i campi corrispondenti alla data/ora richiesti
062100151204       //?e per posizionare il subfile
062200151204         IF  wData > 0;
062300151204           exsr LeggiS03;
062400151204           AbilitaF09 = *off;
062500151204         ENDIF;
062600151203
062700151203       //?Visualizzazione del SFL (se ci sono dati)
062800151204         SflDsp03 = (S03nrr <= *zeros);
062900151203
063000151203       //?Posizionamento cursore
063100151204         IF  V03csr = 0;
063200151204           V03csr = 1;
063300151204         ENDIF;
063400151203         V03rcd = V03csr;
063500151203
063600151203       //?Emissione Testata e Piede
063700151204         write DNA3T01;
063800151204         write DNA3P03;
063900151203
064000151203       //?Emissione videata
064100151204         exfmt DNA3C03;
064200151203         reset ErrMessage;
064300151203         reset ErrGenerico;
064400151203         clear V03msg;
064500151203
064600151203         SELECT;
064700151203
064800151203       //?- F09=Sintetico
064900151203           WHEN  dsp_aid = c_F09;
065000151203             exsr F09S03;
065100151203
065200151203       //?- F12=Ritorno
065300151203           WHEN  dsp_aid = c_F12;
065400151203             exsr F12S03;
065500151203
065600151203       //?Invio
065700151203           OTHER;
065800151203
065900151203         ENDSL;
066000151203
066100151203       ENDSR;
066200151203
066300151203       //--------------------------------------------------------------
066400151203       //?Inizializzazione videata S03.
066500151203       //--------------------------------------------------------------
066600151203       BEGSR InzS03;
066700151203
066800151204         DataOraRi = *off;
066900151204         clear wData;
067000151204         clear wOra;
067100151204         clear wUte;
067200151204         clear sav_VH3dtv;
067300151204         clear sav_VS3ora;
067400151204         clear sav_VS3ute;
067500151204
067600151204       //?Pulizia subfile
067700151204         exsr PulS03;
067800151203
067900151203       //?Imposto il posizionamento al primo rcd
068000151203         IF  S03nrr > 0;
068100151203           V03rcd = 1;
068200151203         ELSE;
068300151203           clear V03rcd;
068400151203         ENDIF;
068500151203
068600151203         V03csr = V03rcd;
068700151203
068800151203       ENDSR;
068900151203
069000151203       //--------------------------------------------------------------
069100151203       //?Pulizia Subfile S03.
069200151203       //--------------------------------------------------------------
069300151203       BEGSR PulS03;
069400151203
069500151203       //?Pulizia subfile
069600151204         SflDsp03 = *on;
069700151204         SflDspCtl03 = *on;
069800151203         write DNA3C03;
069900151204         SflDspCtl03 = *off;
070000151204         SflEnd03 = *off;
070100151203
070200151203         clear V03rcd;
070300151203         clear V03csr;
070400151203         clear S03nrr;
070500151203         clear V03msg;
070600151203
070700151203         ErrMessage  = *off;
070800151203         ErrGenerico = *off;
070900151203
071000151203         WindDspF = IndDspF;
071100151203         reset WindDspFb;
071200151203         IndDspF  = WindDspF;
071300151203
071400151203       ENDSR;
071500151203
071600151203       //--------------------------------------------------------------
071700151203       //?Carico i dati nel Subfile S03.
071800151203       //--------------------------------------------------------------
071900151203       BEGSR CarS03;
072000151203
072100151204         clear DNA3S03;
072200151203
072300151204         Reparto = *off;
072400151204         DataORaRi = *off;
072500151204         DataORaNd = *off;
072600151203         clear dTGMc;
072700151203         clear dTGMk;
072800151203         clear dTGMr;
072900151203         clear dTGMs;
073000151203         clear dTGMu;
073100151203
073200151203         SELECT;
073300151218       //?Variazione Richiesta Risposta
073400151218         WHEN  TGMtmo = 'A';
073500151218           dTGMa = TGMmod;
073600151218         //?Prima
073700151218           VS3old = 'Attesa Risp. "' +
073800160205           §MODatto + '" Risp. Ricevuta"' +
073900151218           §MODriso + '" Fil/Rep "' + %editc(§MODfilo:'X') +
074000151218           '/' + §MODrepo + '"';
074100151218         //?Dopo
074200151218           VS3new = '             "' +
074300151218           §MODattn + '"               "' +
074400151218           §MODrisn + '"         "' + %editc(§MODfiln:'X') +
074500151218           '/' + §MODrepn + '"';
074600151203       //?Variazione causale chiusura
074700151203         WHEN  TGMtmo = 'C';
074800151203           dTGMc = TGMmod;
074900151203         //?Prima
075000151203           clear TIBS02DS;
075100151203           clear dCHR;
075200151203           T02mod = 'C';
075300151203           T02cod = 'CHR';
075400151203           T02ke1 = §MODcdto;
075500151203           T02sif = KNSIF;
075600151203           TNTBE_RicercaControllo  (kpjba : tibs02ds);
075700151203           dCHR = T02uni;
075800151204           VS3old = 'Causale Chiusura "' +
075900151218           §MODcdto + '" ' + §CHRdesc + '"';
076000151203         //?Dopo
076100151203           clear dCHR;
076200151203           clear T02uni;
076300151203           clear T02err;
076400151203           T02ke1 = §MODcdtn;
076500151203           TNTBE_RicercaControllo  (kpjba : tibs02ds);
076600151203           dCHR = T02uni;
076700151204           VS3new = '                 "' +
076800151218           §MODcdtn + '" ' + §CHRdesc + '"';
076900151203       //?Variazione Codice Chiamante
077000151203         WHEN  TGMtmo = 'K';
077100151203           dTGMk = TGMmod;
077200151203           IF  §MODksco = 0;
077300151203             clear VS3old;
077400151203           ELSE;
077500151204             clear TIBS69DS;
077600151204             clear ACOrag;
077700151204             I69kac = §MODksco;
077800151204             TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
077900151204             VS3old = 'Chiamante "' +
078000151218             %editc(§MODksco:'X') + '" ' + ACOrag + '"';
078100151203           ENDIF;
078200151203           IF  §MODkscn = 0;
078300151203             clear VS3new;
078400151203           ELSE;
078500151204             clear TIBS69DS;
078600151204             clear ACOrag;
078700151204             I69kac = §MODkscn;
078800151204             TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
078900151204             VS3new = '          "' +
079000151218             %editc(§MODkscn:'X') + '" ' + ACOrag + '"';
079100151203           ENDIF;
079200151203       //?Variazione Filiale Responsabile
079300151203         WHEN  TGMtmo = 'R';
079400151203           dTGMr = TGMmod;
079500151204           clear ORGdes;
079600151204           chain §MODporo AZORG01L;
079700151204           VS3old = 'Filiale Responsabile "' +
079800151218           %editc(§MODporo:'X') + '" ' + ORGdes + '"';
079900151204           clear ORGdes;
080000151204           chain §MODporo AZORG01L;
080100151204           VS3new = '                     "' +
080200151218           %editc(§MODporn:'X') + '" ' + ORGdes + '"';
080300151203       //?Variazione Stato
080400151203         WHEN  TGMtmo = 'S';
080500151203           dTGMs = TGMmod;
080600151203         //?Prima
080700151203           clear TIBS02DS;
080800151203           clear dPRR;
080900151203           T02mod = 'C';
081000151203           T02cod = 'PRR';
081100151203           T02ke1 = §MODprto;
081200151203           T02sif = KNSIF;
081300151203           TNTBE_RicercaControllo  (kpjba : tibs02ds);
081400151203           dPRR = T02uni;
081500151204           VS3old = 'Stato "' +
081600160111           §MODprto + '" ' + §PRRdesc;
081700151203         //?Dopo
081800151203           clear dPRR;
081900151203           clear T02uni;
082000151203           clear T02err;
082100151203           T02ke1 = §MODprtn;
082200151203           TNTBE_RicercaControllo  (kpjba : tibs02ds);
082300151203           dPRR = T02uni;
082400151204           VS3new = '      "' +
082500160111           §MODprtn + '" ' + §PRRdesc;
082600151204       //?Variazione Utente/Reparto e Filiale Gestione
082700151203         WHEN  TGMtmo = 'U';
082800151203           dTGMu = TGMmod;
082900151204           IF  %subst(§MODuteo:4) <> *blanks;
083000151204             Reparto = *off;
083100160108             VS3old = 'Filiale Gestione "' +
083200151204             %editc(§MODpogo:'X') + '" Utente  "' +
083300151204             §MODuteo + '"';
083400151204           ELSE;
083500151204             Reparto = *on;
083600160108             VS3old = 'Filiale Gestione "' +
083700151204             %editc(§MODpogo:'X') + '" Reparto "' +
083800151204             §MODuteo + '"';
083900151204           ENDIF;
084000151204           IF  %subst(§MODuten:4) <> *blanks and Reparto;
084100160111             VS3new = '                 "' +
084200151204             %editc(§MODpogn:'X') + '" Utente  "' +
084300151204             §MODuten + '"';
084400151204           ENDIF;
084500151204           IF  %subst(§MODuten:4) <> *blanks and not Reparto;
084600160111             VS3new = '                 "' +
084700151204             %editc(§MODpogn:'X') + '"         "' +
084800151204             §MODuten + '"';
084900151204           ENDIF;
085000151204           IF  %subst(§MODuten:4) = *blanks and not Reparto;
085100160111             VS3new = '                 "' +
085200151204             %editc(§MODpogn:'X') + '" Reparto "' +
085300151204             §MODuten + '"';
085400151204           ENDIF;
085500151204           IF  %subst(§MODuten:4) = *blanks and Reparto;
085600160111             VS3new = '                 "' +
085700151204             %editc(§MODpogn:'X') + '"         "' +
085800151204             §MODuten + '"';
085900151204           ENDIF;
086000151203         ENDSL;
086100151204
086200151204         VH3dtv = TGMdmo;
086300151203         Data_ISO = %date(TGMdmo:*iso);
086400151203         Data_EUR = Data_ISO;
086500151203         wDataVar = %dec(Data_EUR);
086600151203         VS3data = wgg * 10000;
086700151203         VS3data += wmm * 100;
086800151203         VS3data += waa2;
086900151203         VS3ora = TGMomo;
087000151203         VS3ute = TGMutn;
087100151204
087200151204         IF  VH3dtv = sav_VH3dtv and VS3ora = sav_VS3ora and
087300151204             VS3ute = sav_VS3ute;
087400151204           DataOraNd = *on;
087500151204         ENDIF;
087600151204
087700151204         VH3in41 = DataOraNd;
087800151204         sav_VH3dtv = VH3dtv;
087900151204         sav_VS3ora = VS3ora;
088000151204         sav_VS3ute = VS3ute;
088100151203
088200151203         S03nrr += 1;
088300151203
088400151204         write DNA3S03;
088500151203
088600151203       ENDSR;
088700151204
088800151204       //--------------------------------------------------------------
088900151204       //?Leggo Subfile S03 per posizionamento.
089000151204       //--------------------------------------------------------------
089100151204       BEGSR LeggiS03;
089200151204
089300151204         FOR xx = 1 to sav_S03nrr;
089400151204           chain xx DNA3S03;
089500151204           IF  not %found;
089600151204             iter;
089700151204           ENDIF;
089800151204           S03nrr = xx;
089900151204           DataOraNd = VH3in41;
090000151204           IF  wData = VH3dtv and wOra = VS3ora and wUte = VS3ute;
090100151204             DataOraRi = *on;
090200151204             V03csr = xx;
090300151204           ELSE;
090400151204             DataORaRi = *off;
090500151204           ENDIF;
090600151204           update DNA3S03;
090700151204         ENDFOR;
090800151204
090900151204         IF  V03csr = 0;
091000151204           V03csr = 1;
091100151204         ENDIF;
091200151204
091300151204       ENDSR;
091400151203
091500151203       //--------------------------------------------------------------
091600151204       //?Gestione tasto funzionale F09 da videata S03.
091700151203       //?F09=Sintetico
091800151203       //--------------------------------------------------------------
091900151203       BEGSR F09S03;
092000151203
092100151203       //?Richiamo Subfile sintetico
092200151203         Video = 'S2';
092300151203         wInzS02 = *on;
092400151203
092500151203       ENDSR;
092600151203
092700151203       //--------------------------------------------------------------
092800151204       //?Gestione tasto funzionale F12 da videata S03.
092900151203       //?F12=Ritorno
093000151203       //--------------------------------------------------------------
093100151203       BEGSR F12S03;
093200151203
093300151203       //?Richiamo Subfile sintetico
093400151203         Video = 'S2';
093500151203         wInzS02 = *on;
093600151203
093700151203       ENDSR;
093800151203
093900150318       //--------------------------------------------------------------
094000150318       //?Operazioni finali.
094100150318       //--------------------------------------------------------------
094200150318       BEGSR RoutEnd;
094300150318
094400150318         *inLR = *on;
094500150318         return;
094600150318
094700150318       ENDSR;
094800150318
094900150318      /end-free
095000151203
095100151203       //--------------------------------------------------------------
095200151203       //?Schiere a tempo di compilazione.
095300151203       //--------------------------------------------------------------
095400151203
095500151203** -- MSG -------------------------------------------------------------------*
095600151203Opzione non valida                                                             1
