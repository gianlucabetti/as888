000100080206      //--------------------------------------------------------------
000200120127      //?TRMKQ5R - Modifica trattativa chiusa: Tipo - Info - SIC
000300080206      //--------------------------------------------------------------
000400080206
000500091124     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600090807     h dftactgrp(*no) actgrp(*caller)
000700080206
000800080206      //---------------------------------------------------------------
000900080206      //?Dichiarazione file.
001000080206      //---------------------------------------------------------------
001100120130
001200120130       // -?File Tabelle
001300120130     fTABEL00F  if   e           k disk
001400120305     fTNTBE01L  if   e           k disk
001500130918
001600130918       // -?Anagrafica Commerciali
001700130918     fAZCMM01L  if   e           k disk
001800120130
001900120130       // -?File per SIC
002000120130     fTIVIIC0F  o    e             disk
002100110706
002200110706       // -?File trattative
002300120127     fTIVIS05L  uf   e           k disk
002400120130
002500120130       // -?File offerte
002600120306     fTIVOF01L  uf   e           k disk    rename(TIVOF000:TIVOF01)
002700120306     fTIVOF11L  uf   e           k disk
002800150504
002900150504      // -?Campagna
003000150504     fTICMC01L  if   e           k disk
003100150504     fTICMF01L  uf   e           k disk
003200110706
003300110706       // -?File video
003400120127     fTRMKQ5D   cf   e             workstn
003500120127     f                                     sfile(mkq5S02 : S02nrr)
003600080208     f                                     indds(IndDspF)
003700080206     f                                     infds(InfDspF)
003800080206
003900080206      //---------------------------------------------------------------
004000090406      //?Definizione costanti.
004100080206      //---------------------------------------------------------------
004200080207
004300110706       // -?Tasti funzionali a video
004400080207     d c_F01           c                   const(x'31')
004500080207     d c_F02           c                   const(x'32')
004600080207     d c_F03           c                   const(x'33')
004700090406     d c_F04           c                   const(x'34')
004800080207     d c_F05           c                   const(x'35')
004900080207     d c_F06           c                   const(x'36')
005000080207     d c_F07           c                   const(x'37')
005100080207     d c_F08           c                   const(x'38')
005200080207     d c_F09           c                   const(x'39')
005300080207     d c_F10           c                   const(x'3A')
005400090303     d c_F11           c                   const(x'3B')
005500080207     d c_F12           c                   const(x'3C')
005600080207     d c_F13           c                   const(x'B1')
005700080207     d c_F14           c                   const(x'B2')
005800080207     d c_F15           c                   const(x'B3')
005900080207     d c_F16           c                   const(x'B4')
006000080207     d c_F17           c                   const(x'B5')
006100080207     d c_F18           c                   const(x'B6')
006200080207     d c_F19           c                   const(x'B7')
006300080207     d c_F20           c                   const(x'B8')
006400080207     d c_F21           c                   const(x'B9')
006500080207     d c_F22           c                   const(x'BA')
006600080207     d c_F23           c                   const(x'BB')
006700080207     d c_F24           c                   const(x'BC')
006800080207     d c_Enter         c                   const(x'F1')
006900080207     d c_RollDown      c                   const(x'F4')
007000080207     d c_RollUp        c                   const(x'F5')
007100091124
007200110706       // -?Costante per controllo "caratteri solo numerici"?
007300110706     d c_Digits        c                   const('0123456789')
007400080206
007500080206      //---------------------------------------------------------------
007600080206      //?Definizione schiere.
007700080206      //---------------------------------------------------------------
007800120305
007900120305       // -?Sk Tipi Trattativa con INFO
008000120305     d skTTRinfo       s              1    dim(30)
008100080206
008200110706       // -?Messaggi di errore
008300120130     d w_Msg           s             78    dim(10) ctdata perrcd(1)
008400080206
008500080206      //---------------------------------------------------------------
008600080206      //?Definizione aree dati.
008700080206      //---------------------------------------------------------------
008800080206
008900110706       // -?Dati utente
009000080206     d §AzUte        e ds                  extname(AZUTE00F)
009100080206     d                                     dtaara
009200080206     d §DatiUte      e ds                  extname(dDatiUte)
009300080206     d                                     dtaara
009400080206
009500080206      //---------------------------------------------------------------
009600080206      //?Definizione strutture dati.
009700080206      //---------------------------------------------------------------
009800080206
009900110706       // -?Status
010000080206     d Psds           sds
010100080206     d   SDSpgm          *proc
010200080206
010300110706       // -?InfDS
010400080206     d InfDspF         ds
010500080207     d  dsp_aid              369    369a
010600080206
010700110706       // -?Indicatori su DspF
010800080208     d IndDspF         ds
010900120127       // -?Indicatori di errore
011000120127     d  ErrMessage                    1n   overlay(IndDspF : 28)
011100120127       // -?Indicatori di gestione subfile
011200120130     d  SflDsp                        1n   overlay(IndDspF : 30)
011300120130     d  SflDspCtl                     1n   overlay(IndDspF : 31)
011400120127     d  SflNxtChg                     1n   overlay(IndDspF : 32)
011500120127     d  SflEnd                        1n   overlay(IndDspF : 33)
011600120127       // -?Altri Indicatori
011700110706     d  PosCurNrv                     1n   overlay(IndDspF : 50)
011800120130     d  PosCurTpv                     1n   overlay(IndDspF : 51)
011900090807     d  ErrGenerico                   1n   overlay(IndDspF : 99)
012000090407
012100090407     d WindDspF        ds                  inz
012200090407     d   WindDspFa             1     49    inz(*zero)
012300090407     d   WindDspFb            50     99    inz(*zero)
012400080206
012500110706       // -?Paremetri ricevuti
012600080206     d KPJBA         e ds
012700120130
012800120130      // -?Ricerca/Controllo tabelle
012900120130     d TIBS02ds      e ds                  inz
013000080206
013100110706       // -?Reperimento dati utente
013200080206     d TIBS34ds      e ds
013300120130
013400120130       // -?Reperimento dati anagrafici cliente codificato
013500120130     d TIBS69ds      e ds
013600120130     d DS_cnaco      e ds                  inz extname(CNACO00F)
013700120130     d DS_cnind      e ds                  inz extname(CNIND00F)
013800120130     d DS_cnclp      e ds                  inz extname(CNCLP00F)
013900120130     d DS_fncls      e ds                  inz extname(FNCLS00F)
014000110706
014100110706       // -?Info Trattativa
014200110706     d TNTA41DS      e ds                  inz
014300120305
014400120305       // -?Cancella Dati tipo
014500120305     d TNTA47DS      e ds                  inz
014600120130
014700120130      // -?Gestione Note clienti/potenziali
014800120130     d TRMK20ds      e ds                  inz
014900110706
015000110706       // -?File Info Trattative
015100110706     d TIVIIds       e ds                  extname(TIVII00F)
015200120127
015300150504       // -?File Fasi Campagna
015400150504     d TICMFds       e ds                  extname(TICMF00F)
015500120130
015600120130       // -?DS Tabella TR - Tipi Tariffa
015700120130     d dsTR          e ds
015800120130
015900120130       // -?DS Tabella TTR - Tipo trattativa
016000120130     d dTTR          e ds
016100090629
016200080206      //---------------------------------------------------------------
016300080206      //?Definizione variabili globali.
016400080206      //---------------------------------------------------------------
016500080206
016600110706       // -?Flags booleani
016700120305     d wConvalida      s               n   inz(*off)
016800120130     d wEoF            s               n   inz(*off)
016900150504     d wEoF_CMF        s               n   inz(*off)
017000120305     d wFine           s               n   inz(*off)
017100120130     d wInfo           s               n   inz(*off)
017200120130     d wInzD01         s               n   inz(*on)
017300120130     d wInzS02         s               n   inz(*off)
017400120305     d wOfferte        s               n   inz(*off)
017500120306     d wOff999         s               n   inz(*off)
017600120130     d wOkInfo         s               n   inz(*off)
017700080206
017800110706       // -?Campi associati al video
017900120130     d wVideo          s              2    inz('D1')
018000120127     d S02nrr          s              4  0 inz
018100110706
018200110706       // -?Campi di comodo
018300120130     d old_VIStpv      s                   like(VIStpv)
018400110706     d Tiket           s                   like(VICtck)
018500120706     d wOggi           s              8  0 inz
018600120306     d w0030           s              3s 0
018700120130
018800120130       // -?Campi di comodo data
018900120130     d wDataEUR        s               d   datfmt(*eur)
019000120305
019100120305       // -?Indici di schiera
019200120305     d xx              s              3  0 inz
019300090806
019400110706       // -?Stringa SQL da eseguire
019500090806     d wSQL            s           2048    Varying        inz
019600090508
019700090508      //---------------------------------------------------------------
019800090807      //?Definizione procedure esterne.
019900090508      //---------------------------------------------------------------
020000110506
020100110706       // -?Gestione Info Trattativa
020200110706     d TNTA41R         pr                  extpgm('TNTA41R')
020300110706     d  kpjba                              likeds(kpjba)
020400110706     d  tnta41ds                           likeds(tnta41ds)
020500120305
020600120305       // -?Cancella dati tipo
020700120305     d TNTA47R         pr                  extpgm('TNTA47R')
020800120305     d  kpjba                              likeds(kpjba)
020900150504
021000150504       // -?Scrittura note su Campagna
021100150504     d TRKC10DS      e ds                  inz
021200150504     d TRKC10R         pr                  extpgm('TRKC10R')
021300150504     d   kpjba                             likeds(KPJBA)
021400150504     d   trkc10ds                          likeds(TRKC10DS)
021500110706
021600110706      //---------------------------------------------------------------
021700110706      //?Definizione prototipi.
021800110706      //---------------------------------------------------------------
021900120130
022000120130       // -?Ricerc/Controllo tabelle
022100120130      /copy gaitrasrc/srcprotopr,tibs02r
022200110706
022300110706       // -?Reperimento dati utente
022400110706      /copy gaitrasrc/srcprotopr,tibs34r
022500120130
022600120130       // -?Reperimento dati anagrafici cliente codificato
022700120130      /copy gaitrasrc/srcprotopr,tibs69r
022800120130
022900120130       // -?Scrittura note
023000120130      /copy gaitrasrc/srcprotopr,trmk20r
023100090807
023200080206      //---------------------------------------------------------------
023300080206      //?Definizione key-list.
023400080206      //---------------------------------------------------------------
023500090807
023600120130       // -?File TABEL00F
023700120130     d k03tabel      e ds                  extname(TABEL00F:*key)
023800120130     d                                     prefix(k_)
023900080206
024000080206      //---------------------------------------------------------------
024100080206      //?Riepilogo indicatori.
024200080206      //---------------------------------------------------------------
024300090807
024400090807
024500080206      //---------------------------------------------------------------
024600080206
024700080206      //---------------------------------------------------------------
024800080206      //?M A I N - L I N E
024900080206      //---------------------------------------------------------------
025000080206
025100080206     c     *Entry        plist
025200080206     c                   parm                    KPJBA
025300080206
025400080206      /free
025500080206
025600090807       //?Operazioni iniziali
025700080206       exsr RoutInz;
025800080206
025900090807       //?Gestione video
026000120130       DOW wFine = *off;
026100090807
026200080206         select;
026300120130           when wVideo = 'D1';
026400110706             exsr GesD01;
026500120130           when wVideo = 'S2';
026600120130             exsr GesS02;
026700090428           other;
026800120130             wFine = *on;
026900080206         endsl;
027000090807
027100080206       ENDDO;
027200080206
027300090807       //?Operazioni finali
027400080206       exsr RoutEnd;
027500080206
027600080206       //--------------------------------------------------------------
027700080206       //?Operazioni iniziali.
027800080206       //--------------------------------------------------------------
027900080206       BEGSR RoutInz;
028000080206
028100120130         wVideo = 'D1';
028200120130         wInzD01 = *on;
028300120706
028400120706       //?Imposto data elaborazione
028500120706         wOggi = %dec(%date());
028600090807
028700110706       //?Reperimento dati job
028800090807         exsr DatiJob;
028900100209
029000090807         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
029100090806
029200110706       //?Impostazione campi "fissi"
029300110706         V1Tpgm = SDSpgm;
029400120130         k_TBLkut = 1;
029500120305
029600120305       //?Carico Tipi trattative che prevedono INFO
029700120305         clear xx;
029800120305         setll ('TTR') TNTBE01L;
029900120305         reade ('TTR') TNTBE01L;
030000120305
030100120305 2       DOW  not %eof(TNTBE01L);
030200120305 3         IF  TBEatb = *blanks;
030300120305             dTTR = TBEuni;
030400120305             IF  §TTRinfo = 'S';
030500120305               xx += 1;
030600120305               skTTRinfo(xx) = TBEke1;
030700120305             ENDIF;
030800120305 3         ENDIF;
030900120305
031000120305           reade ('TTR') TNTBE01L;
031100120305 2       ENDDO;
031200090714
031300080206       ENDSR;
031400080206
031500080206       //--------------------------------------------------------------
031600080206       //?Reperimento Dati del job (Utente/Operativi).
031700080206       //--------------------------------------------------------------
031800080206       BEGSR DatiJob;
031900080206
032000080206         in(E) §AzUte;
032100110706         IF NOT %error;
032200080206           in(E) §DatiUte;
032300110706         ENDIF;
032400110706         IF %error or RSut = *blanks;
032500080206           clear TIBS34ds;
032600080206           tibs34r(tibs34ds);
032700080206           in §AzUte;
032800080206           in §DatiUte;
032900110706         ENDIF;
033000080206
033100080206       ENDSR;
033200080206
033300090421       //--------------------------------------------------------------
033400110706       //?Gestione videata D01
033500080206       //--------------------------------------------------------------
033600110706       BEGSR GesD01;
033700080207
033800110706       //?Inizializzazione videata
033900120130         IF  wInzD01 = *on;
034000110706            exsr InzD01;
034100120130            wInzD01  = *off;
034200110706         ENDIF;
034300080207
034400110706       //?Emissione Testata
034500120130         write  MKQ5T01;
034600080207
034700110706       //?Emissione videata
034800120130         exfmt  MKQ5D01;
034900110706         reset ErrMessage;
035000110706         reset ErrGenerico;
035100120127         clear V01msg;
035200080207
035300080207         SELECT;
035400110706
035500110706         //?- F03=Fine
035600110706           WHEN  dsp_aid = c_F03;
035700110706             exsr F03D01;
035800080207
035900090807         //?Invio
036000080207           OTHER;
036100110706         //?Eseguo i controlli
036200110706             exsr CtrD01;
036300110706             IF  ErrGenerico;
036400080207               leavesr;
036500110706             ENDIF;
036600120127         //?Se tutto OK vado nella seconda videata
036700120130           wVideo = 'S2';
036800120130           wInzS02 = *on;
036900080207
037000080207         ENDSL;
037100080207
037200080207       ENDSR;
037300080207
037400080207       //--------------------------------------------------------------
037500110706       //?Inizializzazione videata D01.
037600080207       //--------------------------------------------------------------
037700110706       BEGSR InzD01;
037800080207
037900120127         clear V01nrv;
038000090508
038100080207       ENDSR;
038200080207
038300091111       //--------------------------------------------------------------
038400110706       //?Gestione tasto funzionale F03 da videata D01.
038500110706       //?F3=Fine
038600091111       //--------------------------------------------------------------
038700110706       BEGSR F03D01;
038800091111
038900110706       //?Chiusura del programma
039000120130         wFine = *on;
039100091111
039200091111       ENDSR;
039300120130
039400120130       //--------------------------------------------------------------
039500120130       //?Controllo dati videata D01.
039600120130       //--------------------------------------------------------------
039700120130       BEGSR CtrD01;
039800120130
039900120130         WindDspF  = IndDspF;
040000120130         reset WindDspFb;
040100120130         IndDspF   = WindDspF;
040200120130
040300120130       //?Obbligo della trattativa
040400120130         IF  V01nrv = 0;
040500120130           ErrMessage  = *on;
040600120130           ErrGenerico = *on;
040700120130           PosCurNrv   = *on;
040800120130           V01msg      = w_Msg(01);
040900120130           leavesr;
041000120130         ENDIF;
041100120130
041200120130       //?La trattativa deve esistere
041300120130         chain(n) V01nrv TIVIS05L;
041400120130         IF  not %found(TIVIS05L);
041500120130           ErrMessage  = *on;
041600120130           ErrGenerico = *on;
041700120130           PosCurNrv   = *on;
041800120130           V01msg      = w_Msg(02);
041900120130           leavesr;
042000120130         ENDIF;
042100120130
042200120130       //?La trattativa deve essere chiusa
042300120130         IF  VISdch = 0;
042400120130           ErrMessage  = *on;
042500120130           ErrGenerico = *on;
042600120130           PosCurNrv   = *on;
042700120130           V01msg      = w_Msg(03);
042800120130           leavesr;
042900120130         ENDIF;
043000120706
043100120706       //?La trattativa deve avere data chiusura < data del giorno
043200120706         IF  VISdch >= wOggi;
043300120706           ErrMessage  = *on;
043400120706           ErrGenerico = *on;
043500120706           PosCurNrv   = *on;
043600120706           V01msg      = w_Msg(08);
043700120706           leavesr;
043800120706         ENDIF;
043900120130
044000120130       //?Accetto solo Trattative di tipo 'A' o 'I'
044100120130         IF  VIStpv <> 'A' and VIStpv <> 'I';
044200120130           ErrMessage  = *on;
044300120130           ErrGenerico = *on;
044400120130           PosCurNrv   = *on;
044500120130           V01msg      = w_Msg(04);
044600120130           leavesr;
044700120130         ENDIF;
044800120130
044900120130       ENDSR;
045000120130
045100120130       //--------------------------------------------------------------
045200120130       //?Gestione videata S02
045300120130       //--------------------------------------------------------------
045400120130       BEGSR GesS02;
045500120130
045600120130       //?Inizializzazione videata
045700120130         IF  wInzS02 = *on;
045800120130            exsr InzS02;
045900120130            wInzS02  = *off;
046000120130         ENDIF;
046100120130
046200120130       //?Emissione Testata + Piede
046300120130         write  MKQ5T01;
046400120130         write  MKQ5P02;
046500120130
046600120130       //?Emissione subfile
046700120130         exfmt  MKQ5C02;
046800120130         reset ErrMessage;
046900120130         reset ErrGenerico;
047000120130         clear V02msg;
047100120130
047200120130         SELECT;
047300120130
047400120130         //?- F03=Fine
047500120130           WHEN  dsp_aid = c_F03;
047600120130             exsr F03D01;
047700120130
047800120130         //?- F06=Conferma
047900120130           WHEN  dsp_aid = c_F06;
048000120130           //?Eseguo i controlli
048100120130             exsr CtrC02 ;
048200120130             IF  ErrGenerico;
048300120130               leavesr;
048400120130             ENDIF;
048500120130             exsr F06S02;
048600120130
048700120130         //?- F12=Ritorno
048800120130           WHEN  dsp_aid = c_F12;
048900120130             exsr F12S02;
049000120306
049100120306         //?- F15=Info Trattativa
049200120306           WHEN  dsp_aid = c_F15;
049300120306             exsr F15S02;
049400120130
049500120130         //?Invio
049600120130           OTHER;
049700120130         //?Eseguo i controlli
049800120130             exsr CtrC02 ;
049900120130             IF  ErrGenerico;
050000120130               leavesr;
050100120130             ENDIF;
050200120130
050300120130         ENDSL;
050400120130
050500120130       ENDSR;
050600120130
050700120130       //--------------------------------------------------------------
050800120130       //?Inizializzazione videata S02.
050900120130       //--------------------------------------------------------------
051000120130       BEGSR InzS02;
051100120130
051200120130       //?Pulizia subfile
051300120130         exsr PulS02;
051400120130
051500120130       //?Carico dati nel control
051600120130         exsr CarC02;
051700120130
051800120130       //?Caricamento dati
051900120130         exsr CarS02;
052000120130
052100120130       //?Visualizzazione del Subfile
052200120130         SflDsp = (S02nrr <= *zeros);
052300120130
052400120130       //?Attivazione SFLEND
052500120130         SflEnd = *on;
052600120130
052700120130       //?Impaginazione Subfile
052800120130       //?Forzo sempre il primo rcd
052900120130         IF  S02nrr > *zero;
053000120130           C02rcd = 1;
053100120130           C02csr = 1;
053200120130         ELSE;
053300120130           clear C02rcd;
053400120130         ENDIF;
053500120130
053600120130         C02csr = C02rcd;
053700120130
053800120130       ENDSR;
053900120130
054000120130       //--------------------------------------------------------------
054100120130       //?Pulizia Subfile S02.
054200120130       //--------------------------------------------------------------
054300120130       BEGSR PulS02;
054400120130
054500120130       //?Pulizia subfile
054600120130         SflDsp    = *on;
054700120130         SflDspCtl = *on;
054800120130         write  MKQ5C02;
054900120130         SflDspCtl = *off;
055000120130         SflEnd    = *off;
055100120130
055200120130         clear C02rcd;
055300120130         clear C02csr;
055400120130         clear S02nrr;
055500120130         clear V02msg;
055600120130         ErrMessage  = *off;
055700120130         ErrGenerico = *off;
055800120130
055900120130         WindDspF = IndDspF;
056000120130         reset WindDspFb;
056100120130         IndDspF  = WindDspF;
056200120130
056300120130       ENDSR;
056400120130
056500120130       //--------------------------------------------------------------
056600120130       //?Caricamento Control C02.
056700120130       //--------------------------------------------------------------
056800120130       BEGSR CarC02;
056900120130
057000120130         C02nrv = V01nrv;
057100120130         wDataEUR = %date(VISdat:*iso);
057200120130         C02dat   = %dec(wDataEUR);
057300120130         C02esi   = VISesi;
057400120130         wDataEUR = %date(VISdch:*iso);
057500120130         C02dch   = %dec(wDataEUR);
057600120130         C02ksc   = VISksc;
057700120130         C02rag   = VISrag;
057800120130
057900120130       //?Se cliente codificato recupero i dati dall'anagrafico clienti
058000120130         IF  C02ksc > 0;
058100120130           clear TIBS69ds;
058200120130           I69kac = C02ksc;
058300120130           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
058400120130           C02rag = ACOrag;
058500120130         ENDIF;
058600120130
058700120130       //?Tipo trattativa + decodifica
058800120130         C02tpv = VIStpv;
058900120130         clear dTTR;
059000120130         clear TIBS02ds;
059100120130         T02mod = 'C';
059200120130         T02cod = 'TTR';
059300120130         T02ke1 = C02tpv;
059400120130         T02sif = KNSIF;
059500120130         TNTBE_RicercaControllo (kpjba : tibs02ds);
059600120130         IF  T02err = *blanks;
059700120130           dTTR = t02uni ;
059800120130         ENDIF;
059900120130
060000120130         C02dtpv = §TTRdes;
060100120130
060200120130       //?Codice commerciale + decodifica
060300130918         clear  C02cmm;
060400120130         C02cmm = VIScmm;
060500130918         chain  (C02cmm)  AZCMM000;
060600130918         IF  %found(AZCMM01L);
060700130918           C02dcmm = CMMdes;
060800130918         else;
060900130918           C02dcmm = *all'? ';
061000120130         ENDIF;
061100120130
061200120130       //?Mi posiziono sempre sul tipo trattativa
061300120130         PosCurTpv = *on;
061400120130
061500120130       ENDSR;
061600120130
061700120130       //--------------------------------------------------------------
061800120130       //?Caricamento Subfile S02.
061900120130       //--------------------------------------------------------------
062000120130       BEGSR CarS02;
062100120130
062200120130       //?Leggo il file delle offerte
062300120130         wEoF = *off;
062400120305         wOfferte = *off;
062500120305         wConvalida = *off;
062600120130         setll C02nrv TIVOF11L;
062700120306         reade(n) C02nrv TIVOF11L;
062800120130         DOW  not wEoF;
062900120130           IF  %eof(TIVOF11L);
063000120130             wEoF = *on;
063100120130             leave;
063200120130           ENDIF;
063300120306         //?Se offerta annullata la salto
063400120306           IF  VOFeso <> '*';
063500120306           //?Mi salvo che ho trovato offerte
063600120306             IF  S02nrr = 0;
063700120306               wOfferte = *on;
063800120306             ENDIF;
063900120306           //?Carico il subfile
064000120306             exsr RieS02;
064100120306           //?Scrico il subfile
064200120306             S02nrr += 1;
064300120306             write MKQ5S02;
064400120306           ENDIF;
064500120306           reade(n) C02nrv TIVOF11L;
064600120130         ENDDO;
064700120306
064800120306       //?Se tipo offerta "I" faccio un giro con la 999
064900120306         IF  VIStpv = 'I';
065000120306           exsr Cerca_999;
065100120306         ENDIF;
065200120130
065300120130       ENDSR;
065400120130
065500120130       //--------------------------------------------------------------
065600120130       //?Carico i dati nel subfile S02.
065700120130       //--------------------------------------------------------------
065800120130       BEGSR RieS02;
065900120130
066000120130         clear  MKQ5S02;
066100120130
066200120130         S02ctr = VOFctr;
066300120130         S02prg = VOFprg;
066400120130
066500120130       //?Decodifco il tipo tariffa
066600120130         clear dsTR;
066700120130         k_TBLcod = 'TR';
066800120130         k_TBLkey = %subst(%editc(S02ctr:'X'):1:1);
066900120130         chain  %kds(K03tabel) TABEL00F;
067000120130         IF  %found(TABEL00F) and
067100120130              TBLflg = *blanks;
067200120130           dsTR = TBLuni;
067300120130         ENDIF;
067400120130
067500120130         S02dtr = §TRde3;
067600120130
067700120130         S02tpt = VOFtpt;
067800120130
067900120130       //?Esito
068000120130         SELECT;
068100120130         WHEN  VOFeso = 'H';
068200120130           S02eso = 'Congelata  ';
068300120130         WHEN  VOFeso = 'A';
068400120130           S02eso = 'Accettata  ';
068500120130         WHEN  VOFeso = 'C';
068600120130           S02eso = 'Convalidata';
068700120305           wConvalida = *on;
068800120130         OTHER;
068900120130           clear S02eso;
069000120130         ENDSL;
069100120130
069200120130       //?Cliente convalidato
069300120130         S02ksc = VOFksc;
069400120130         IF  S02ksc > 0;
069500120130           clear TIBS69ds;
069600120130           I69kac = S02ksc;
069700120130           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
069800120130           S02rag = ACOrag;
069900120130         ENDIF;
070000120130
070100120130       //?Data reale affido merci
070200120130         IF VOFdra > 0;
070300120130           wDataEUR = %date(VOFdra:*iso);
070400120130           S02dra   = %dec(wDataEUR);
070500120130         ENDIF;
070600120130
070700120130       ENDSR;
070800120130
070900120130       //--------------------------------------------------------------
071000120130       //?Controllo i dati relativi al formato C02
071100120130       //--------------------------------------------------------------
071200120305       BEGSR CtrC02;
071300120130
071400120130         WindDspF = IndDspF;
071500120130         reset WindDspFb;
071600120130         IndDspF  = WindDspF;
071700120130
071800120130       //?Se stesso tipo trattativa non ho variato nulla
071900120130         IF  C02tpv = VIStpv;
072000120130           ErrMessage  = *on;
072100120130           ErrGenerico = *on;
072200120130           PosCurTpv   = *on;
072300120130           V02msg      = w_Msg(05);
072400120130           leavesr;
072500120130         ENDIF;
072600120130
072700120130       //?Controllo il tipo trattativa
072800120130         clear dTTR;
072900120130         clear TIBS02ds;
073000120130         T02mod = 'C';
073100120130         T02cod = 'TTR';
073200120130         T02ke1 = C02tpv;
073300120130         T02sif = KNSIF;
073400120130         TNTBE_RicercaControllo (kpjba : tibs02ds);
073500120130         IF  T02err <> *blanks;
073600120130           ErrMessage  = *on;
073700120130           ErrGenerico = *on;
073800120130           PosCurTpv   = *on;
073900120305           V02msg      = w_Msg(07);
074000120130           leavesr;
074100120130         ENDIF;
074200120130
074300120130         dTTR = t02uni ;
074400120130         C02dtpv = §TTRdes;
074500120305
074600120305       //?Controllo se posso fare il cambio
074700120305         SELECT;
074800120305         //?Da "A" a "I" o da "I" ad "A" sempre possibile
074900120305           WHEN  C02tpv = 'A' or C02tpv = 'I';
075000120306         //?Da "A" a "R" possibile solo se presenti offerte
075100120306           WHEN  VIStpv = 'A' and C02tpv = 'R' and wOfferte;
075200120306         //?Da "I" a "R" possibile solo se presenti offerte
075300120306         //?                          e NON presente offerta 999
075400120306           WHEN  VIStpv = 'I' and C02tpv = 'R' and wOfferte
075500120306                                               and not wOff999;
075600120313         //?Da "A" a "M" possibile solo se NON presenti offerte convalidate
075700120313           WHEN  VIStpv = 'A' and C02tpv = 'M' and not wConvalida;
075800120306         //?Da "I" a "M" non possibile se presente offerta 999
075900120306         //?                         e se NON presenti offerte convalidate
076000120306           WHEN  VIStpv = 'I' and C02tpv = 'M' and not wOff999
076100120306                                               and not wConvalida;
076200120305           OTHER;
076300120305         //?Modifica non possibile
076400120305           ErrMessage  = *on;
076500120305           ErrGenerico = *on;
076600120305           PosCurTpv   = *on;
076700120305           V02msg      = w_Msg(06);
076800120313           IF  C02tpv <> 'M' and C02tpv <> 'R' and
076900120313               C02tpv <> 'A' and C02tpv <> 'I';
077000120313             V02msg = %trim(V02msg);
077100120313             leavesr;
077200120313           ENDIF;
077300120306           IF  wOff999;
077400120306             V02msg = %trim(V02msg) + ' presente CALDO senza offerta';
077500120313             leavesr;
077600120306           ENDIF;
077700120306           IF  wConvalida;
077800120306             V02msg = %trim(V02msg) + ' presenti offerte convalidate';
077900120313             leavesr;
078000120306           ENDIF;
078100120305           IF  not wOfferte;
078200120305             V02msg = %trim(V02msg) + ' NON presenti offerte';
078300120313             leavesr;
078400120305           ENDIF;
078500120305         ENDSL;
078600120130
078700120130       ENDSR ;
078800120127
078900120130       //--------------------------------------------------------------
079000120130       //?Gestione tasto funzionale F12 da videata S02.
079100120130       //?F12=Ritorno
079200120130       //--------------------------------------------------------------
079300120130       BEGSR F12S02;
079400120127
079500120130         wVideo = 'D1';
079600120130         wInzD01 = *on;
079700120130
079800120130       ENDSR;
079900120306
080000120306       //--------------------------------------------------------------
080100120306       //?Gestione tasto funzionale F15 da videata S02.
080200120306       //?F15=Info Trattativa
080300120306       //--------------------------------------------------------------
080400120306       BEGSR F15S02;
080500120306
080600120306         clear TNTA41DS;
080700120306         I41nrv    = VISnrv;
080800120306         I41rag    = VISrag;
080900120306         I41mod    = 'I';
081000120306         I41ifotot = VISinfot;
081100120306         I41tpv    = VIStpv;
081200120306         tnta41r (kpjba:tnta41ds);
081300120306
081400120306       ENDSR;
081500120127
081600110706       //--------------------------------------------------------------
081700120130       //?Gestione tasto funzionale F06 da videata S02.
081800110706       //?F6=Conferma
081900110706       //--------------------------------------------------------------
082000120130       BEGSR F06S02;
082100110706
082200110706       //?Modifico info trattativa
082300120130         exsr Mod_Info;
082400120130
082500120130       //?Modifico trattativa
082600120130         chain C02nrv TIVIS05L;
082700120130         IF  %found(TIVIS05L);
082800120131           old_VIStpv = VIStpv;
082900120305           VISTPV = C02tpv;
083000120130           update TIVIS000;
083100120130         ENDIF;
083200120305
083300120321       //?Cancellazione offerte se congelate o accettate solo se new tipo = 'M'
083400120321         IF  wOfferte and C02tpv = 'M';
083500120305           exsr Mod_Offerte;
083600120305         ENDIF;
083700120306
083800120306       //?Se la trattativa era di tipo "I" cancello eventuale offerta 999
083900120306         IF  old_VIStpv = 'I';
084000120306           exsr Mod_Off999;
084100120306         ENDIF;
084200110706
084300120306       //?Modifico dati SIC
084400120306         exsr Mod_Sic;
084500120130
084600120130       //?Scrivo note come promemoria
084700120130         exsr Wrt_Note;
084800150504
084900150504       //?Modifico Campagna
085000150504         exsr Mod_Campagna;
085100120130
085200120130       //?Alla fine torno alla prima videata
085300120130         exsr F12S02;
085400110706
085500110706       ENDSR;
085600120306
085700120306       //--------------------------------------------------------------
085800120306       //?Cerco se c'è l'offerta 999.
085900120306       //--------------------------------------------------------------
086000120306       BEGSR Cerca_999;
086100120306
086200120306         wOff999 = *off;
086300120306         w0030   = 999;
086400120306         chain(n) (C02nrv:w0030) TIVOF01L;
086500120306         IF  %found(TIVOF01L) and VOFeso <> '*';
086600120306           wOff999 = *on;
086700120306         ENDIF;
086800120306
086900120306       ENDSR;
087000110706
087100110706       //--------------------------------------------------------------
087200110706       //?Modifico Info Trattative.
087300110706       //--------------------------------------------------------------
087400110706       BEGSR Mod_Info;
087500120130
087600120130         wInfo = *off;
087700120130
087800120130       //?Cancello le vecchie info
087900120130         exec sql
088000120130         DELETE from TIVII00F
088100120130         WHERE VIInrv = :VISnrv;
088200120305
088300120305       //?Pulisco il campo di INFO inserite su TIVIS
088400120305         clear VISinfot;
088500120130
088600120305       //?Se la trattativa le prevede richiedo le INFO
088700120305         IF  %lookup(C02tpv:skTTRinfo) > 0;
088800120305           clear TNTA41DS;
088900120305           I41nrv    = VISnrv;
089000120305           I41rag    = VISrag;
089100120305           I41mod    = 'G';
089200120305           I41ifotot = VISinfot;
089300120305           I41tpv    = C02tpv;
089400120305           tnta41r (kpjba:tnta41ds);
089500120306           IF  O41f06 = 'S';
089600120305             wInfo = *on;
089700120305             VISinfot = O41ifotot;
089800120305           ENDIF;
089900120305
090000120305       //?Se ci sono offerte le INFO sono obbligatorie, se non inserite prima
090100120305       //?ciclo a richiamare F15 fino a che non sono state inserite
090200120305           DOW  VISinfot = *blanks and wOfferte;
090300120305             clear TNTA41DS;
090400120305             I41nrv    = VISnrv;
090500120305             I41rag    = VISrag;
090600120305             I41mod    = 'G';
090700120305             I41ifotot = VISinfot;
090800120305             I41tpv    = C02tpv;
090900120305             tnta41r (kpjba:tnta41ds);
091000120306             IF  O41f06 = 'S';
091100120305               wInfo = *on;
091200120305               VISinfot = O41ifotot;
091300120305             ENDIF;
091400120305           ENDDO;
091500120130         ENDIF;
091600120130
091700120130       ENDSR;
091800120305
091900120305       //--------------------------------------------------------------
092000120305       //?Cancello le offerte se congelate o accettate.
092100120305       //--------------------------------------------------------------
092200120305       BEGSR Mod_Offerte;
092300110706
092400120305         wEoF = *off;
092500120305         setll C02nrv TIVOF11L;
092600120305         reade C02nrv TIVOF11L;
092700120305         DOW  not wEoF;
092800120305           IF  %eof(TIVOF11L);
092900120305             wEoF = *on;
093000120305             leave;
093100120305           ENDIF;
093200120305           IF  VOFeso = 'H' or VOFeso = 'A';
093300120305             VOFeso = '*';
093400120306             exsr Del_Offerte;
093500120306             exsr Del_VofSic;
093600120305             update TIVOF000;
093700120305           ENDIF;
093800120305           reade C02nrv TIVOF11L;
093900120305         ENDDO;
094000120305
094100120305       ENDSR;
094200120305
094300120305       //--------------------------------------------------------------
094400120305       //?Deleto le offerte.
094500120305       //--------------------------------------------------------------
094600120305       BEGSR Del_Offerte;
094700120305
094800120305       //?Annullo dati tipo legati all'offerta
094900120305         clear TNTA47DS;
095000120305         D47tup = '2';
095100120305         D47cto = 'O';
095200120305         D47dsf = 'S';
095300120312         D47ksc = VOFnrv;
095400120305         D47ctr = VOFctr;
095500120305         D47prg = VOFprg;
095600120305         kpjbu  = TNTA47DS;
095700120305         tnta47r (kpjba);
095800120305
095900120305       //?Cancello le offerte
096000120312         IF  %subst(knsif : 7 : 1) = 'P';
096100120312           exec sql
096200120312           DELETE from FILTRAGRPF/TIOGC00F
096300120312           WHERE TGCksc = :VOFnrv and TGCctr = :VOFctr and TGCprg = :VOFprg;
096400120312           exec sql
096500120312           DELETE from FILTRAGRPF/TIOPD00F
096600120312           WHERE TPDksc = :VOFnrv and TPDctr = :VOFctr and TPDprg = :VOFprg;
096700120312           exec sql
096800120312           DELETE from FILTRAGRPF/TIOPT00F
096900120312           WHERE TPTksc = :VOFnrv and TPTctr = :VOFctr and TPTprg = :VOFprg;
097000120312           exec sql
097100120312           DELETE from FILTRAGRPF/TIOFD00F
097200120312           WHERE TADksc = :VOFnrv and TADctr = :VOFctr and TADprg = :VOFprg;
097300120312           exec sql
097400120312           DELETE from FILTRAGRPF/TNOFM00F
097500120312           WHERE TAMksc = :VOFnrv and TAMctr = :VOFctr and TAMprg = :VOFprg;
097600120312         ELSE;
097700120312           exec sql
097800120312           DELETE from FILTRAGRU/TIOGC00F
097900120312           WHERE TGCksc = :VOFnrv and TGCctr = :VOFctr and TGCprg = :VOFprg;
098000120312           exec sql
098100120312           DELETE from FILTRAGRU/TIOPD00F
098200120312           WHERE TPDksc = :VOFnrv and TPDctr = :VOFctr and TPDprg = :VOFprg;
098300120312           exec sql
098400120312           DELETE from FILTRAGRU/TIOPT00F
098500120312           WHERE TPTksc = :VOFnrv and TPTctr = :VOFctr and TPTprg = :VOFprg;
098600120312           exec sql
098700120312           DELETE from FILTRAGRU/TIOFD00F
098800120312           WHERE TADksc = :VOFnrv and TADctr = :VOFctr and TADprg = :VOFprg;
098900120312           exec sql
099000120312           DELETE from FILTRAGRU/TNOFM00F
099100120312           WHERE TAMksc = :VOFnrv and TAMctr = :VOFctr and TAMprg = :VOFprg;
099200120312         ENDIF;
099300120305
099400120305       ENDSR;
099500120306
099600120306       //--------------------------------------------------------------
099700120306       //?Cancello l'offerta 999.
099800120306       //--------------------------------------------------------------
099900120306       BEGSR Mod_Off999;
100000120306
100100120306         wEoF = *off;
100200120306         setll C02nrv TIVOF01L;
100300120306         reade C02nrv TIVOF01L;
100400120306         DOW  not wEoF;
100500120306           IF  %eof(TIVOF01L);
100600120306             wEoF = *on;
100700120306             leave;
100800120306           ENDIF;
100900120306           IF  VOFctr = 999;
101000120306             exsr Del_VofSic;
101100120306             delete TIVOF01;
101200120306           ENDIF;
101300120306           reade C02nrv TIVOF01L;
101400120306         ENDDO;
101500120306
101600120306       ENDSR;
101700120306
101800120306       //--------------------------------------------------------------
101900120306       //?Deleto le offerte dal SIC.
102000120306       //--------------------------------------------------------------
102100120306       BEGSR Del_VofSic;
102200120306
102300120306       //?Cancello le offerte presenti sul SIC
102400120306         exec sql
102500120306         DELETE from TIVOFC0F
102600120306         WHERE VOCnrv = :VOFnrv and VOCctr = :VOFctr and VOCprg = :VOFprg;
102700120306
102800120306       ENDSR;
102900120305
103000110706       //--------------------------------------------------------------
103100120306       //?Modifico Sic.
103200110706       //--------------------------------------------------------------
103300120306       BEGSR Mod_Sic;
103400120130
103500120130         clear Tiket;
103600110706
103700120130       //?Recupero il tiket dalle info e cancello i dati del SIC delle info
103800110706         IF  %subst(knsif : 7 : 1) = 'P';
103900110706           exec sql
104000110706           SELECT VICtck into :Tiket
104100110706           FROM GAITRAAZP/TIVIIC0F
104200110706           WHERE VICnrv = :VISnrv;
104300110706
104400110706           exec sql
104500110706           DELETE from GAITRAAZP/TIVIIC0F
104600110706           WHERE VICnrv = :VISnrv;
104700110706         ELSE;
104800110706           exec sql
104900110706           SELECT VICtck into :Tiket
105000110706           FROM GAITRAAZM/TIVIIC0F
105100110706           WHERE VICnrv = :VISnrv;
105200110706
105300110706           exec sql
105400110706           DELETE from GAITRAAZM/TIVIIC0F
105500110706           WHERE VICnrv = :VISnrv;
105600110706         ENDIF;
105700120130
105800120130       //?Se non ho trovato il tiket sulle info (vuol dire che non c'erano info)
105900120130       //?lo recupero dalla trattativa
106000120130         IF  Tiket = *blanks;
106100120130           IF  %subst(knsif : 7 : 1) = 'P';
106200120130             exec sql
106300120130             SELECT VICtck into :Tiket
106400120130             FROM GAITRAAZP/TIVISC0F
106500120130             WHERE VICnrv = :VISnrv;
106600120130           ELSE;
106700120130             exec sql
106800120130             SELECT VICtck into :Tiket
106900120130             FROM GAITRAAZM/TIVISC0F
107000120130             WHERE VICnrv = :VISnrv;
107100120130           ENDIF;
107200120130         ENDIF;
107300110706
107400110706         clear TIVIIC00;
107500120130         wEoF    = *off;
107600120130         wOKinfo = *off;
107700110706
107800110926         wSQL = 'select * from TIVII00F ';
107900110706
108000110706         wSQL += 'where VIInrv = ' + %editc(VISnrv:'X') +
108100110706                   ' order by VIItpf';
108200110706
108300110706         exec sql prepare A1 from :wSQL;
108400110706
108500110706         exec sql declare VII cursor for A1;
108600110706
108700110706         exec sql open VII;
108800110706
108900110706         IF  sqlcode < 0;
109000120130           wEoF = *on;
109100110706         ENDIF;
109200110706
109300120130         DOW  not wEoF;
109400110706           exec sql fetch next from VII into :TIVIIDS;
109500110706           IF  sqlcod = 100 or sqlcod < 0;
109600120130             wEoF = *on;
109700110706             leave;
109800110706           ENDIF;
109900110706
110000110706       //?Imposto i dati per scrivere il record
110100120130           wOKinfo = *on;
110200110706           VICnrv = VIInrv;
110300110706
110400110706           SELECT;
110500110706         //?Fatturato/Spedizioni totale
110600110706             WHEN  VIItpf = 'TRT';
110700110706               VICfatt = VIIpft;
110800110706               VICnspt = VIIsna;
110900110706         //?Fatturato/Spedizioni estero
111000110706             WHEN  VIItpf = 'TRE';
111100110706               VICfate = VIIpft;
111200110706               VICnspe = VIIsna;
111300110706         //?Aumento/Sconto
111400110706             WHEN  VIItpf = 'A/S';
111500110706               VICausc = VIIvald;
111600110706         //?Delta
111700110706             WHEN  VIItpf = 'DEL';
111800110706               VICdel = VIIval;
111900110706         //?Peso
112000110706             WHEN  VIItpf = 'KMS';
112100110706               VICpkg = VIIval;
112200110706         //?Concorrenti
112300110706             WHEN  VIItpf = '10' and VIIspf = 'ARCO';
112400110706               VICarco = 'S';
112500110706             WHEN  VIItpf = '10' and VIIspf = 'ARTO';
112600110706               VICarto = 'S';
112700110706             WHEN  VIItpf = '10' and VIIspf = 'AWS';
112800110706               VICaws  = 'S';
112900110706             WHEN  VIItpf = '10' and VIIspf = 'BARS';
113000110706               VICbars = 'S';
113100110706             WHEN  VIItpf = '10' and VIIspf = 'DHL';
113200110706               VICdhl  = 'S';
113300110706             WHEN  VIItpf = '10' and VIIspf = 'EXEC';
113400110706               VICexec = 'S';
113500110706             WHEN  VIItpf = '10' and VIIspf = 'FERC';
113600110706               VICferc = 'S';
113700110706             WHEN  VIItpf = '10' and VIIspf = 'MAIL';
113800110706               VICmail = 'S';
113900110706             WHEN  VIItpf = '10' and VIIspf = 'MEP';
114000110706               VICmep  = 'S';
114100110706             WHEN  VIItpf = '10' and VIIspf = 'MTN';
114200110706               VICmtn  = 'S';
114300110706             WHEN  VIItpf = '10' and VIIspf = 'ONEE';
114400110706               VIConee = 'S';
114500110706             WHEN  VIItpf = '10' and VIIspf = 'PALL';
114600110706               VICpall = 'S';
114700110706             WHEN  VIItpf = '10' and VIIspf = 'PALW';
114800110706               VICpalw = 'S';
114900110706             WHEN  VIItpf = '10' and VIIspf = 'SDA';
115000110706               VICsda  = 'S';
115100110706             WHEN  VIItpf = '10' and VIIspf = 'SUSA';
115200110706               VICsusa = 'S';
115300110706             WHEN  VIItpf = '10' and VIIspf = 'TNT';
115400110706               VICtnt  = 'S';
115500110706             WHEN  VIItpf = '10' and VIIspf = 'UPS';
115600110706               VICups  = 'S';
115700110706             WHEN  VIItpf = '10' and VIIspf = '9910';
115800110706               VICaltr = 'S';
115900110706             WHEN  VIItpf = '10' and VIIspf = '8810';
116000110706               VICness = 'S';
116100110706           ENDSL;
116200110706         ENDDO;
116300110706
116400110706       //?Scrivo il rcd delle info commerciali
116500120130         IF  wOKinfo;
116600110706           VICtck = Tiket;
116700110706           write  TIVIIC00;
116800111102           feod TIVIIC0F;
116900110706         ENDIF;
117000110706
117100110706         exec sql close VII;
117200120130
117300120130       //?Devo aggiornare anche la trattativa sul SIC
117400120130         IF  %subst(knsif : 7 : 1) = 'P';
117500120130           exec sql
117600120130           UPDATE GAITRAAZP/TIVISC0F
117700120130           SET VICtpv = :VIStpv
117800120130           WHERE VICnrv = :VISnrv;
117900120130         ELSE;
118000120130           exec sql
118100120130           UPDATE GAITRAAZM/TIVISC0F
118200120130           SET VICtpv = :VIStpv
118300120130           WHERE VICnrv = :VISnrv;
118400120130         ENDIF;
118500110706
118600110706       ENDSR;
118700120130
118800120130       //--------------------------------------------------------------
118900120130       //?Scrivo note per promemoria.
119000120130       //--------------------------------------------------------------
119100120130       BEGSR Wrt_Note;
119200120130
119300120130         clear TRMK20ds;
119400120130         IMK20tla = 'L';
119500120130         IMK20flm = 'C';
119600120130         IMK20fno = 'S';
119700120130         IMK20cpo = VIScpo;
119800120130         IF  C02ksc > 0;
119900120130           IMK20ksc = C02ksc;
120000120130         ENDIF;
120100120130         IMK20nrv = VISnrv;
120200120130         EMK20no1 = 'Trattativa variata da ' +
120300120130                     old_VIStpv + ' a ' + VIStpv +
120400120313                    '. Variazione eseguita da ' + knmus;
120500120130         trmk20r ( kpjba : trmk20ds );
120600120130
120700120130       ENDSR;
120800150504
120900150504       //--------------------------------------------------------------
121000150504       //?Modifico Campagna.
121100150504       //--------------------------------------------------------------
121200150504       BEGSR Mod_Campagna;
121300150504
121400150504       //?Se non è una trattativa di Aumento
121500150504       //?e prima lo era
121600150504       //?devo cancellare eventuali fasi TR presenti in campagna
121700150504         IF  C02tpv <> 'A' and old_VIStpv = 'A';
121800150504
121900150504           wEoF_CMF = *off;
122000150504
122100150504           exec sql
122200150504           DECLARE CMF cursor for
122300150504           SELECT * from TICMF00F
122400150504           WHERE CMFnrv = :C02nrv and CMFacm = ' TR';
122500150504
122600150504           exec sql OPEN CMF;
122700150504
122800150504           IF  sqlcode < 0;
122900150504             wEoF_CMF = *on;
123000150504             exec sql CLOSE CMF;
123100150504             leavesr;
123200150504           ENDIF;
123300150504
123400150504           DOW  not wEoF_CMF;
123500150504             exec sql
123600150504             fetch next from CMF into :ticmfDS;
123700150504
123800150504             IF  sqlcod = 100 or sqlcod < 0;
123900150504               wEoF_CMF = *on;
124000150504             leave;
124100150504             ENDIF;
124200150504
124300150504           //?Cancello fase TR
124400150504             chain (CMFncm:CMFksu:CMFksc:CMFcpo:CMFacm:CMFdfa:CMFhfc)
124500150504                   TICMF01L;
124600150504             IF  %found(TICMF01L);
124700150504               delete  TICMF000;
124800150504               chain (CMFncm:CMFksu:CMFksc:CMFcpo) TICMC01L;
124900150504           //?Scrivo nota sulla campagna
125000150504               clear TRKC10DS;
125100150504               IKC10tla = 'L';
125200150504               IKC10flm = 'C';
125300150504               IKC10fno = 'S';
125400150504               IKC10ncm = CMFncm;
125500150504               IKC10ksu = CMFksu;
125600150504               IKC10ksc = CMFksc;
125700150504               IKC10cpo = CMFcpo;
125800150504               IKC10acm = CMCufe;
125900150504               EKC10no1 = 'Cancellata fase TR su Trattativa ' +
126000150504                          %editc(C02nrv:'X') +
126100150504                          '. Variato tipo trattativa';
126200150504               EKC10dim = %dec(%date());
126300150504               EKC10him = %dec(%time());
126400150504               trkc10r (kpjba:TRKC10DS);
126500150504             ENDIF;
126600150504           ENDDO;
126700150504
126800150504           exec sql CLOSE CMF;
126900150504
127000150504         ENDIF;
127100150504
127200150504       ENDSR;
127300090807
127400080206       //--------------------------------------------------------------
127500080206       //?Operazioni finali.
127600080206       //--------------------------------------------------------------
127700080206       BEGSR RoutEnd;
127800090521
127900080206         *inLR = *on;
128000080206         return;
128100080206
128200080206       ENDSR;
128300080206
128400080206      /end-free
128500090807
128600080206       //--------------------------------------------------------------
128700080206       //?Schiere a tempo di compilazione.
128800080206       //--------------------------------------------------------------
128900080206
129000120130** - w_MSG -------------------------------------------------------------------*
129100110706Immettere la trattativa da variare                                             1
129200110706Trattativa inesistente                                                         2
129300110930Trattativa ancora aperta                                                       3
129400120130Trattativa non di tipo "A" o "I"                                               4
129500120130Il tipo trattativa non è stato variato                                         5
129600120305Modifica tipo trattativa non possibile                                         6
129700120305Tipo trattativa errato                                                         7
129800120706La trattativa è stata chiusa oggi, modificarla domani!!!                       8
