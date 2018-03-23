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
001500120130
001600120130       // -?File per SIC
001700120130     fTIVIIC0F  o    e             disk
001800110706
001900110706       // -?File trattative
002000120127     fTIVIS05L  uf   e           k disk
002100120130
002200120130       // -?File offerte
002300120306     fTIVOF01L  uf   e           k disk    rename(TIVOF000:TIVOF01)
002400120306     fTIVOF11L  uf   e           k disk
002500110706
002600110706       // -?File video
002700120127     fTRMKQ5D   cf   e             workstn
002800120127     f                                     sfile(mkq5S02 : S02nrr)
002900080208     f                                     indds(IndDspF)
003000080206     f                                     infds(InfDspF)
003100080206
003200080206      //---------------------------------------------------------------
003300090406      //?Definizione costanti.
003400080206      //---------------------------------------------------------------
003500080207
003600110706       // -?Tasti funzionali a video
003700080207     d c_F01           c                   const(x'31')
003800080207     d c_F02           c                   const(x'32')
003900080207     d c_F03           c                   const(x'33')
004000090406     d c_F04           c                   const(x'34')
004100080207     d c_F05           c                   const(x'35')
004200080207     d c_F06           c                   const(x'36')
004300080207     d c_F07           c                   const(x'37')
004400080207     d c_F08           c                   const(x'38')
004500080207     d c_F09           c                   const(x'39')
004600080207     d c_F10           c                   const(x'3A')
004700090303     d c_F11           c                   const(x'3B')
004800080207     d c_F12           c                   const(x'3C')
004900080207     d c_F13           c                   const(x'B1')
005000080207     d c_F14           c                   const(x'B2')
005100080207     d c_F15           c                   const(x'B3')
005200080207     d c_F16           c                   const(x'B4')
005300080207     d c_F17           c                   const(x'B5')
005400080207     d c_F18           c                   const(x'B6')
005500080207     d c_F19           c                   const(x'B7')
005600080207     d c_F20           c                   const(x'B8')
005700080207     d c_F21           c                   const(x'B9')
005800080207     d c_F22           c                   const(x'BA')
005900080207     d c_F23           c                   const(x'BB')
006000080207     d c_F24           c                   const(x'BC')
006100080207     d c_Enter         c                   const(x'F1')
006200080207     d c_RollDown      c                   const(x'F4')
006300080207     d c_RollUp        c                   const(x'F5')
006400091124
006500110706       // -?Costante per controllo "caratteri solo numerici"?
006600110706     d c_Digits        c                   const('0123456789')
006700080206
006800080206      //---------------------------------------------------------------
006900080206      //?Definizione schiere.
007000080206      //---------------------------------------------------------------
007100120305
007200120305       // -?Sk Tipi Trattativa con INFO
007300120305     d skTTRinfo       s              1    dim(30)
007400080206
007500110706       // -?Messaggi di errore
007600120130     d w_Msg           s             78    dim(10) ctdata perrcd(1)
007700080206
007800080206      //---------------------------------------------------------------
007900080206      //?Definizione aree dati.
008000080206      //---------------------------------------------------------------
008100080206
008200110706       // -?Dati utente
008300080206     d §AzUte        e ds                  extname(AZUTE00F)
008400080206     d                                     dtaara
008500080206     d §DatiUte      e ds                  extname(dDatiUte)
008600080206     d                                     dtaara
008700080206
008800080206      //---------------------------------------------------------------
008900080206      //?Definizione strutture dati.
009000080206      //---------------------------------------------------------------
009100080206
009200110706       // -?Status
009300080206     d Psds           sds
009400080206     d   SDSpgm          *proc
009500080206
009600110706       // -?InfDS
009700080206     d InfDspF         ds
009800080207     d  dsp_aid              369    369a
009900120127     d  sfl_rrn              376    377i 0
010000120127     d  min_nrr              378    379i 0
010100120127     d  num_rcds             380    381i 0
010200080206
010300110706       // -?Indicatori su DspF
010400080208     d IndDspF         ds
010500120127       // -?Indicatori di errore
010600120127     d  ErrMessage                    1n   overlay(IndDspF : 28)
010700120127       // -?Indicatori di gestione subfile
010800120130     d  SflDsp                        1n   overlay(IndDspF : 30)
010900120130     d  SflDspCtl                     1n   overlay(IndDspF : 31)
011000120127     d  SflNxtChg                     1n   overlay(IndDspF : 32)
011100120127     d  SflEnd                        1n   overlay(IndDspF : 33)
011200120127       // -?Altri Indicatori
011300110706     d  PosCurNrv                     1n   overlay(IndDspF : 50)
011400120130     d  PosCurTpv                     1n   overlay(IndDspF : 51)
011500090807     d  ErrGenerico                   1n   overlay(IndDspF : 99)
011600090407
011700090407     d WindDspF        ds                  inz
011800090407     d   WindDspFa             1     49    inz(*zero)
011900090407     d   WindDspFb            50     99    inz(*zero)
012000080206
012100110706       // -?Paremetri ricevuti
012200080206     d KPJBA         e ds
012300120130
012400120130      // -?Ricerca/Controllo tabelle
012500120130     d TIBS02ds      e ds                  inz
012600080206
012700110706       // -?Reperimento dati utente
012800080206     d TIBS34ds      e ds
012900120130
013000120130       // -?Reperimento dati anagrafici cliente codificato
013100120130     d TIBS69ds      e ds
013200120130     d DS_cnaco      e ds                  inz extname(CNACO00F)
013300120130     d DS_cnind      e ds                  inz extname(CNIND00F)
013400120130     d DS_cnclp      e ds                  inz extname(CNCLP00F)
013500120130     d DS_fncls      e ds                  inz extname(FNCLS00F)
013600110706
013700110706       // -?Info Trattativa
013800110706     d TNTA41DS      e ds                  inz
013900120305
014000120305       // -?Cancella Dati tipo
014100120305     d TNTA47DS      e ds                  inz
014200120130
014300120130      // -?Gestione Note clienti/potenziali
014400120130     d TRMK20ds      e ds                  inz
014500110706
014600110706       // -?File Info Trattative
014700110706     d TIVIIds       e ds                  extname(TIVII00F)
014800120127
014900120127       // -?File SIC Trattative chiuse
015000120127     d TIVISCds      e ds                  extname(TIVISC0F)
015100120130
015200120130       // -?DS Tabella TR - Tipi Tariffa
015300120130     d dsTR          e ds
015400120130
015500120130       // -?DS Tabella 01 - Codice Commerciale
015600120130     d ds01          e ds
015700120130
015800120130       // -?DS Tabella TTR - Tipo trattativa
015900120130     d dTTR          e ds
016000090629
016100080206      //---------------------------------------------------------------
016200080206      //?Definizione variabili globali.
016300080206      //---------------------------------------------------------------
016400080206
016500110706       // -?Flags booleani
016600120305     d wConvalida      s               n   inz(*off)
016700120130     d wEoF            s               n   inz(*off)
016800120305     d wFine           s               n   inz(*off)
016900120130     d wInfo           s               n   inz(*off)
017000120130     d wInzD01         s               n   inz(*on)
017100120130     d wInzS02         s               n   inz(*off)
017200120305     d wOfferte        s               n   inz(*off)
017300120306     d wOff999         s               n   inz(*off)
017400120130     d wOkInfo         s               n   inz(*off)
017500080206
017600110706       // -?Campi associati al video
017700120130     d wVideo          s              2    inz('D1')
017800090601     d dsp_mod         s             10I 0
017900120127     d S02nrr          s              4  0 inz
018000110706
018100110706       // -?Campi di comodo
018200120130     d old_VIStpv      s                   like(VIStpv)
018300110706     d Tiket           s                   like(VICtck)
018400120706     d wOggi           s              8  0 inz
018500120306     d w0030           s              3s 0
018600120130
018700120130       // -?Campi di comodo data
018800120130     d wDataEUR        s               d   datfmt(*eur)
018900120130     d wDataISO        s               d   datfmt(*iso)
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
020900110706
021000110706      //---------------------------------------------------------------
021100110706      //?Definizione prototipi.
021200110706      //---------------------------------------------------------------
021300120130
021400120130       // -?Ricerc/Controllo tabelle
021500120130      /copy gaitrasrc/srcprotopr,tibs02r
021600110706
021700110706       // -?Reperimento dati utente
021800110706      /copy gaitrasrc/srcprotopr,tibs34r
021900120130
022000120130       // -?Reperimento dati anagrafici cliente codificato
022100120130      /copy gaitrasrc/srcprotopr,tibs69r
022200120130
022300120130       // -?Scrittura note
022400120130      /copy gaitrasrc/srcprotopr,trmk20r
022500090807
022600080206      //---------------------------------------------------------------
022700080206      //?Definizione key-list.
022800080206      //---------------------------------------------------------------
022900090807
023000120130       // -?File TABEL00F
023100120130     d k03tabel      e ds                  extname(TABEL00F:*key)
023200120130     d                                     prefix(k_)
023300080206
023400080206      //---------------------------------------------------------------
023500080206      //?Riepilogo indicatori.
023600080206      //---------------------------------------------------------------
023700090807
023800090807
023900080206      //---------------------------------------------------------------
024000080206
024100080206      //---------------------------------------------------------------
024200080206      //?M A I N - L I N E
024300080206      //---------------------------------------------------------------
024400080206
024500080206     c     *Entry        plist
024600080206     c                   parm                    KPJBA
024700080206
024800080206      /free
024900080206
025000090807       //?Operazioni iniziali
025100080206       exsr RoutInz;
025200080206
025300090807       //?Gestione video
025400120130       DOW wFine = *off;
025500090807
025600080206         select;
025700120130           when wVideo = 'D1';
025800110706             exsr GesD01;
025900120130           when wVideo = 'S2';
026000120130             exsr GesS02;
026100090428           other;
026200120130             wFine = *on;
026300080206         endsl;
026400090807
026500080206       ENDDO;
026600080206
026700090807       //?Operazioni finali
026800080206       exsr RoutEnd;
026900080206
027000080206       //--------------------------------------------------------------
027100080206       //?Operazioni iniziali.
027200080206       //--------------------------------------------------------------
027300080206       BEGSR RoutInz;
027400080206
027500120130         wVideo = 'D1';
027600120130         wInzD01 = *on;
027700120706
027800120706       //?Imposto data elaborazione
027900120706         wOggi = %dec(%date());
028000090807
028100110706       //?Reperimento dati job
028200090807         exsr DatiJob;
028300100209
028400090807         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
028500090806
028600110706       //?Impostazione campi "fissi"
028700110706         V1Tpgm = SDSpgm;
028800120130         k_TBLkut = 1;
028900120305
029000120305       //?Carico Tipi trattative che prevedono INFO
029100120305         clear xx;
029200120305         setll ('TTR') TNTBE01L;
029300120305         reade ('TTR') TNTBE01L;
029400120305
029500120305 2       DOW  not %eof(TNTBE01L);
029600120305 3         IF  TBEatb = *blanks;
029700120305             dTTR = TBEuni;
029800120305             IF  §TTRinfo = 'S';
029900120305               xx += 1;
030000120305               skTTRinfo(xx) = TBEke1;
030100120305             ENDIF;
030200120305 3         ENDIF;
030300120305
030400120305           reade ('TTR') TNTBE01L;
030500120305 2       ENDDO;
030600090714
030700080206       ENDSR;
030800080206
030900080206       //--------------------------------------------------------------
031000080206       //?Reperimento Dati del job (Utente/Operativi).
031100080206       //--------------------------------------------------------------
031200080206       BEGSR DatiJob;
031300080206
031400080206         in(E) §AzUte;
031500110706         IF NOT %error;
031600080206           in(E) §DatiUte;
031700110706         ENDIF;
031800110706         IF %error or RSut = *blanks;
031900080206           clear TIBS34ds;
032000080206           tibs34r(tibs34ds);
032100080206           in §AzUte;
032200080206           in §DatiUte;
032300110706         ENDIF;
032400080206
032500080206       ENDSR;
032600080206
032700090421       //--------------------------------------------------------------
032800110706       //?Gestione videata D01
032900080206       //--------------------------------------------------------------
033000110706       BEGSR GesD01;
033100080207
033200110706       //?Inizializzazione videata
033300120130         IF  wInzD01 = *on;
033400110706            exsr InzD01;
033500120130            wInzD01  = *off;
033600110706         ENDIF;
033700080207
033800110706       //?Emissione Testata
033900120130         write  MKQ5T01;
034000080207
034100110706       //?Emissione videata
034200120130         exfmt  MKQ5D01;
034300110706         reset ErrMessage;
034400110706         reset ErrGenerico;
034500120127         clear V01msg;
034600080207
034700080207         SELECT;
034800110706
034900110706         //?- F03=Fine
035000110706           WHEN  dsp_aid = c_F03;
035100110706             exsr F03D01;
035200080207
035300090807         //?Invio
035400080207           OTHER;
035500110706         //?Eseguo i controlli
035600110706             exsr CtrD01;
035700110706             IF  ErrGenerico;
035800080207               leavesr;
035900110706             ENDIF;
036000120127         //?Se tutto OK vado nella seconda videata
036100120130           wVideo = 'S2';
036200120130           wInzS02 = *on;
036300080207
036400080207         ENDSL;
036500080207
036600080207       ENDSR;
036700080207
036800080207       //--------------------------------------------------------------
036900110706       //?Inizializzazione videata D01.
037000080207       //--------------------------------------------------------------
037100110706       BEGSR InzD01;
037200080207
037300120127         clear V01nrv;
037400090508
037500080207       ENDSR;
037600080207
037700091111       //--------------------------------------------------------------
037800110706       //?Gestione tasto funzionale F03 da videata D01.
037900110706       //?F3=Fine
038000091111       //--------------------------------------------------------------
038100110706       BEGSR F03D01;
038200091111
038300110706       //?Chiusura del programma
038400120130         wFine = *on;
038500091111
038600091111       ENDSR;
038700120130
038800120130       //--------------------------------------------------------------
038900120130       //?Controllo dati videata D01.
039000120130       //--------------------------------------------------------------
039100120130       BEGSR CtrD01;
039200120130
039300120130         WindDspF  = IndDspF;
039400120130         reset WindDspFb;
039500120130         IndDspF   = WindDspF;
039600120130
039700120130       //?Obbligo della trattativa
039800120130         IF  V01nrv = 0;
039900120130           ErrMessage  = *on;
040000120130           ErrGenerico = *on;
040100120130           PosCurNrv   = *on;
040200120130           V01msg      = w_Msg(01);
040300120130           leavesr;
040400120130         ENDIF;
040500120130
040600120130       //?La trattativa deve esistere
040700120130         chain(n) V01nrv TIVIS05L;
040800120130         IF  not %found(TIVIS05L);
040900120130           ErrMessage  = *on;
041000120130           ErrGenerico = *on;
041100120130           PosCurNrv   = *on;
041200120130           V01msg      = w_Msg(02);
041300120130           leavesr;
041400120130         ENDIF;
041500120130
041600120130       //?La trattativa deve essere chiusa
041700120130         IF  VISdch = 0;
041800120130           ErrMessage  = *on;
041900120130           ErrGenerico = *on;
042000120130           PosCurNrv   = *on;
042100120130           V01msg      = w_Msg(03);
042200120130           leavesr;
042300120130         ENDIF;
042400120706
042500120706       //?La trattativa deve avere data chiusura < data del giorno
042600120706         IF  VISdch >= wOggi;
042700120706           ErrMessage  = *on;
042800120706           ErrGenerico = *on;
042900120706           PosCurNrv   = *on;
043000120706           V01msg      = w_Msg(08);
043100120706           leavesr;
043200120706         ENDIF;
043300120130
043400120130       //?Accetto solo Trattative di tipo 'A' o 'I'
043500120130         IF  VIStpv <> 'A' and VIStpv <> 'I';
043600120130           ErrMessage  = *on;
043700120130           ErrGenerico = *on;
043800120130           PosCurNrv   = *on;
043900120130           V01msg      = w_Msg(04);
044000120130           leavesr;
044100120130         ENDIF;
044200120130
044300120130       ENDSR;
044400120130
044500120130       //--------------------------------------------------------------
044600120130       //?Gestione videata S02
044700120130       //--------------------------------------------------------------
044800120130       BEGSR GesS02;
044900120130
045000120130       //?Inizializzazione videata
045100120130         IF  wInzS02 = *on;
045200120130            exsr InzS02;
045300120130            wInzS02  = *off;
045400120130         ENDIF;
045500120130
045600120130       //?Emissione Testata + Piede
045700120130         write  MKQ5T01;
045800120130         write  MKQ5P02;
045900120130
046000120130       //?Emissione subfile
046100120130         exfmt  MKQ5C02;
046200120130         reset ErrMessage;
046300120130         reset ErrGenerico;
046400120130         clear V02msg;
046500120130
046600120130         SELECT;
046700120130
046800120130         //?- F03=Fine
046900120130           WHEN  dsp_aid = c_F03;
047000120130             exsr F03D01;
047100120130
047200120130         //?- F06=Conferma
047300120130           WHEN  dsp_aid = c_F06;
047400120130           //?Eseguo i controlli
047500120130             exsr CtrC02 ;
047600120130             IF  ErrGenerico;
047700120130               leavesr;
047800120130             ENDIF;
047900120130             exsr F06S02;
048000120130
048100120130         //?- F12=Ritorno
048200120130           WHEN  dsp_aid = c_F12;
048300120130             exsr F12S02;
048400120306
048500120306         //?- F15=Info Trattativa
048600120306           WHEN  dsp_aid = c_F15;
048700120306             exsr F15S02;
048800120130
048900120130         //?Invio
049000120130           OTHER;
049100120130         //?Eseguo i controlli
049200120130             exsr CtrC02 ;
049300120130             IF  ErrGenerico;
049400120130               leavesr;
049500120130             ENDIF;
049600120130
049700120130         ENDSL;
049800120130
049900120130       ENDSR;
050000120130
050100120130       //--------------------------------------------------------------
050200120130       //?Inizializzazione videata S02.
050300120130       //--------------------------------------------------------------
050400120130       BEGSR InzS02;
050500120130
050600120130       //?Pulizia subfile
050700120130         exsr PulS02;
050800120130
050900120130       //?Carico dati nel control
051000120130         exsr CarC02;
051100120130
051200120130       //?Caricamento dati
051300120130         exsr CarS02;
051400120130
051500120130       //?Visualizzazione del Subfile
051600120130         SflDsp = (S02nrr <= *zeros);
051700120130
051800120130       //?Attivazione SFLEND
051900120130         SflEnd = *on;
052000120130
052100120130       //?Impaginazione Subfile
052200120130       //?Forzo sempre il primo rcd
052300120130         IF  S02nrr > *zero;
052400120130           C02rcd = 1;
052500120130           C02csr = 1;
052600120130         ELSE;
052700120130           clear C02rcd;
052800120130         ENDIF;
052900120130
053000120130         C02csr = C02rcd;
053100120130
053200120130       ENDSR;
053300120130
053400120130       //--------------------------------------------------------------
053500120130       //?Pulizia Subfile S02.
053600120130       //--------------------------------------------------------------
053700120130       BEGSR PulS02;
053800120130
053900120130       //?Pulizia subfile
054000120130         SflDsp    = *on;
054100120130         SflDspCtl = *on;
054200120130         write  MKQ5C02;
054300120130         SflDspCtl = *off;
054400120130         SflEnd    = *off;
054500120130
054600120130         clear C02rcd;
054700120130         clear C02csr;
054800120130         clear S02nrr;
054900120130         clear V02msg;
055000120130         ErrMessage  = *off;
055100120130         ErrGenerico = *off;
055200120130
055300120130         WindDspF = IndDspF;
055400120130         reset WindDspFb;
055500120130         IndDspF  = WindDspF;
055600120130
055700120130       ENDSR;
055800120130
055900120130       //--------------------------------------------------------------
056000120130       //?Caricamento Control C02.
056100120130       //--------------------------------------------------------------
056200120130       BEGSR CarC02;
056300120130
056400120130         C02nrv = V01nrv;
056500120130         wDataEUR = %date(VISdat:*iso);
056600120130         C02dat   = %dec(wDataEUR);
056700120130         C02esi   = VISesi;
056800120130         wDataEUR = %date(VISdch:*iso);
056900120130         C02dch   = %dec(wDataEUR);
057000120130         C02ksc   = VISksc;
057100120130         C02rag   = VISrag;
057200120130
057300120130       //?Se cliente codificato recupero i dati dall'anagrafico clienti
057400120130         IF  C02ksc > 0;
057500120130           clear TIBS69ds;
057600120130           I69kac = C02ksc;
057700120130           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
057800120130           C02rag = ACOrag;
057900120130         ENDIF;
058000120130
058100120130       //?Tipo trattativa + decodifica
058200120130         C02tpv = VIStpv;
058300120130         clear dTTR;
058400120130         clear TIBS02ds;
058500120130         T02mod = 'C';
058600120130         T02cod = 'TTR';
058700120130         T02ke1 = C02tpv;
058800120130         T02sif = KNSIF;
058900120130         TNTBE_RicercaControllo (kpjba : tibs02ds);
059000120130         IF  T02err = *blanks;
059100120130           dTTR = t02uni ;
059200120130         ENDIF;
059300120130
059400120130         C02dtpv = §TTRdes;
059500120130
059600120130       //?Codice commerciale + decodifica
059700120130         C02cmm = VIScmm;
059800120130         clear ds01;
059900120130         k_TBLcod = '01';
060000120130         k_TBLkey = %editc(C02cmm:'X');
060100120130         chain  %kds(K03tabel) TABEL00F;
060200120130         IF  %found(TABEL00F) and
060300120130              TBLflg = *blanks;
060400120130           ds01 = TBLuni;
060500120130         ENDIF;
060600120130
060700120130         C02dcmm = §01age;
060800120130
060900120130       //?Mi posiziono sempre sul tipo trattativa
061000120130         PosCurTpv = *on;
061100120130
061200120130       ENDSR;
061300120130
061400120130       //--------------------------------------------------------------
061500120130       //?Caricamento Subfile S02.
061600120130       //--------------------------------------------------------------
061700120130       BEGSR CarS02;
061800120130
061900120130       //?Leggo il file delle offerte
062000120130         wEoF = *off;
062100120305         wOfferte = *off;
062200120305         wConvalida = *off;
062300120130         setll C02nrv TIVOF11L;
062400120306         reade(n) C02nrv TIVOF11L;
062500120130         DOW  not wEoF;
062600120130           IF  %eof(TIVOF11L);
062700120130             wEoF = *on;
062800120130             leave;
062900120130           ENDIF;
063000120306         //?Se offerta annullata la salto
063100120306           IF  VOFeso <> '*';
063200120306           //?Mi salvo che ho trovato offerte
063300120306             IF  S02nrr = 0;
063400120306               wOfferte = *on;
063500120306             ENDIF;
063600120306           //?Carico il subfile
063700120306             exsr RieS02;
063800120306           //?Scrico il subfile
063900120306             S02nrr += 1;
064000120306             write MKQ5S02;
064100120306           ENDIF;
064200120306           reade(n) C02nrv TIVOF11L;
064300120130         ENDDO;
064400120306
064500120306       //?Se tipo offerta "I" faccio un giro con la 999
064600120306         IF  VIStpv = 'I';
064700120306           exsr Cerca_999;
064800120306         ENDIF;
064900120130
065000120130       ENDSR;
065100120130
065200120130       //--------------------------------------------------------------
065300120130       //?Carico i dati nel subfile S02.
065400120130       //--------------------------------------------------------------
065500120130       BEGSR RieS02;
065600120130
065700120130         clear  MKQ5S02;
065800120130
065900120130         S02ctr = VOFctr;
066000120130         S02prg = VOFprg;
066100120130
066200120130       //?Decodifco il tipo tariffa
066300120130         clear dsTR;
066400120130         k_TBLcod = 'TR';
066500120130         k_TBLkey = %subst(%editc(S02ctr:'X'):1:1);
066600120130         chain  %kds(K03tabel) TABEL00F;
066700120130         IF  %found(TABEL00F) and
066800120130              TBLflg = *blanks;
066900120130           dsTR = TBLuni;
067000120130         ENDIF;
067100120130
067200120130         S02dtr = §TRde3;
067300120130
067400120130         S02tpt = VOFtpt;
067500120130
067600120130       //?Esito
067700120130         SELECT;
067800120130         WHEN  VOFeso = 'H';
067900120130           S02eso = 'Congelata  ';
068000120130         WHEN  VOFeso = 'A';
068100120130           S02eso = 'Accettata  ';
068200120130         WHEN  VOFeso = 'C';
068300120130           S02eso = 'Convalidata';
068400120305           wConvalida = *on;
068500120130         OTHER;
068600120130           clear S02eso;
068700120130         ENDSL;
068800120130
068900120130       //?Cliente convalidato
069000120130         S02ksc = VOFksc;
069100120130         IF  S02ksc > 0;
069200120130           clear TIBS69ds;
069300120130           I69kac = S02ksc;
069400120130           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
069500120130           S02rag = ACOrag;
069600120130         ENDIF;
069700120130
069800120130       //?Data reale affido merci
069900120130         IF VOFdra > 0;
070000120130           wDataEUR = %date(VOFdra:*iso);
070100120130           S02dra   = %dec(wDataEUR);
070200120130         ENDIF;
070300120130
070400120130       ENDSR;
070500120130
070600120130       //--------------------------------------------------------------
070700120130       //?Controllo i dati relativi al formato C02
070800120130       //--------------------------------------------------------------
070900120305       BEGSR CtrC02;
071000120130
071100120130         WindDspF = IndDspF;
071200120130         reset WindDspFb;
071300120130         IndDspF  = WindDspF;
071400120130
071500120130       //?Se stesso tipo trattativa non ho variato nulla
071600120130         IF  C02tpv = VIStpv;
071700120130           ErrMessage  = *on;
071800120130           ErrGenerico = *on;
071900120130           PosCurTpv   = *on;
072000120130           V02msg      = w_Msg(05);
072100120130           leavesr;
072200120130         ENDIF;
072300120130
072400120130       //?Controllo il tipo trattativa
072500120130         clear dTTR;
072600120130         clear TIBS02ds;
072700120130         T02mod = 'C';
072800120130         T02cod = 'TTR';
072900120130         T02ke1 = C02tpv;
073000120130         T02sif = KNSIF;
073100120130         TNTBE_RicercaControllo (kpjba : tibs02ds);
073200120130         IF  T02err <> *blanks;
073300120130           ErrMessage  = *on;
073400120130           ErrGenerico = *on;
073500120130           PosCurTpv   = *on;
073600120305           V02msg      = w_Msg(07);
073700120130           leavesr;
073800120130         ENDIF;
073900120130
074000120130         dTTR = t02uni ;
074100120130         C02dtpv = §TTRdes;
074200120305
074300120305       //?Controllo se posso fare il cambio
074400120305         SELECT;
074500120305         //?Da "A" a "I" o da "I" ad "A" sempre possibile
074600120305           WHEN  C02tpv = 'A' or C02tpv = 'I';
074700120306         //?Da "A" a "R" possibile solo se presenti offerte
074800120306           WHEN  VIStpv = 'A' and C02tpv = 'R' and wOfferte;
074900120306         //?Da "I" a "R" possibile solo se presenti offerte
075000120306         //?                          e NON presente offerta 999
075100120306           WHEN  VIStpv = 'I' and C02tpv = 'R' and wOfferte
075200120306                                               and not wOff999;
075300120313         //?Da "A" a "M" possibile solo se NON presenti offerte convalidate
075400120313           WHEN  VIStpv = 'A' and C02tpv = 'M' and not wConvalida;
075500120306         //?Da "I" a "M" non possibile se presente offerta 999
075600120306         //?                         e se NON presenti offerte convalidate
075700120306           WHEN  VIStpv = 'I' and C02tpv = 'M' and not wOff999
075800120306                                               and not wConvalida;
075900120305           OTHER;
076000120305         //?Modifica non possibile
076100120305           ErrMessage  = *on;
076200120305           ErrGenerico = *on;
076300120305           PosCurTpv   = *on;
076400120305           V02msg      = w_Msg(06);
076500120313           IF  C02tpv <> 'M' and C02tpv <> 'R' and
076600120313               C02tpv <> 'A' and C02tpv <> 'I';
076700120313             V02msg = %trim(V02msg);
076800120313             leavesr;
076900120313           ENDIF;
077000120306           IF  wOff999;
077100120306             V02msg = %trim(V02msg) + ' presente CALDO senza offerta';
077200120313             leavesr;
077300120306           ENDIF;
077400120306           IF  wConvalida;
077500120306             V02msg = %trim(V02msg) + ' presenti offerte convalidate';
077600120313             leavesr;
077700120306           ENDIF;
077800120305           IF  not wOfferte;
077900120305             V02msg = %trim(V02msg) + ' NON presenti offerte';
078000120313             leavesr;
078100120305           ENDIF;
078200120305         ENDSL;
078300120130
078400120130       ENDSR ;
078500120127
078600120130       //--------------------------------------------------------------
078700120130       //?Gestione tasto funzionale F12 da videata S02.
078800120130       //?F12=Ritorno
078900120130       //--------------------------------------------------------------
079000120130       BEGSR F12S02;
079100120127
079200120130         wVideo = 'D1';
079300120130         wInzD01 = *on;
079400120130
079500120130       ENDSR;
079600120306
079700120306       //--------------------------------------------------------------
079800120306       //?Gestione tasto funzionale F15 da videata S02.
079900120306       //?F15=Info Trattativa
080000120306       //--------------------------------------------------------------
080100120306       BEGSR F15S02;
080200120306
080300120306         clear TNTA41DS;
080400120306         I41nrv    = VISnrv;
080500120306         I41rag    = VISrag;
080600120306         I41mod    = 'I';
080700120306         I41ifotot = VISinfot;
080800120306         I41tpv    = VIStpv;
080900120306         tnta41r (kpjba:tnta41ds);
081000120306
081100120306       ENDSR;
081200120127
081300110706       //--------------------------------------------------------------
081400120130       //?Gestione tasto funzionale F06 da videata S02.
081500110706       //?F6=Conferma
081600110706       //--------------------------------------------------------------
081700120130       BEGSR F06S02;
081800110706
081900110706       //?Modifico info trattativa
082000120130         exsr Mod_Info;
082100120130
082200120130       //?Modifico trattativa
082300120130         chain C02nrv TIVIS05L;
082400120130         IF  %found(TIVIS05L);
082500120131           old_VIStpv = VIStpv;
082600120305           VISTPV = C02tpv;
082700120130           update TIVIS000;
082800120130         ENDIF;
082900120305
083000120321       //?Cancellazione offerte se congelate o accettate solo se new tipo = 'M'
083100120321         IF  wOfferte and C02tpv = 'M';
083200120305           exsr Mod_Offerte;
083300120305         ENDIF;
083400120306
083500120306       //?Se la trattativa era di tipo "I" cancello eventuale offerta 999
083600120306         IF  old_VIStpv = 'I';
083700120306           exsr Mod_Off999;
083800120306         ENDIF;
083900110706
084000120306       //?Modifico dati SIC
084100120306         exsr Mod_Sic;
084200120130
084300120130       //?Scrivo note come promemoria
084400120130         exsr Wrt_Note;
084500120130
084600120130       //?Alla fine torno alla prima videata
084700120130         exsr F12S02;
084800110706
084900110706       ENDSR;
085000120306
085100120306       //--------------------------------------------------------------
085200120306       //?Cerco se c'è l'offerta 999.
085300120306       //--------------------------------------------------------------
085400120306       BEGSR Cerca_999;
085500120306
085600120306         wOff999 = *off;
085700120306         w0030   = 999;
085800120306         chain(n) (C02nrv:w0030) TIVOF01L;
085900120306         IF  %found(TIVOF01L) and VOFeso <> '*';
086000120306           wOff999 = *on;
086100120306         ENDIF;
086200120306
086300120306       ENDSR;
086400110706
086500110706       //--------------------------------------------------------------
086600110706       //?Modifico Info Trattative.
086700110706       //--------------------------------------------------------------
086800110706       BEGSR Mod_Info;
086900120130
087000120130         wInfo = *off;
087100120130
087200120130       //?Cancello le vecchie info
087300120130         exec sql
087400120130         DELETE from TIVII00F
087500120130         WHERE VIInrv = :VISnrv;
087600120305
087700120305       //?Pulisco il campo di INFO inserite su TIVIS
087800120305         clear VISinfot;
087900120130
088000120305       //?Se la trattativa le prevede richiedo le INFO
088100120305         IF  %lookup(C02tpv:skTTRinfo) > 0;
088200120305           clear TNTA41DS;
088300120305           I41nrv    = VISnrv;
088400120305           I41rag    = VISrag;
088500120305           I41mod    = 'G';
088600120305           I41ifotot = VISinfot;
088700120305           I41tpv    = C02tpv;
088800120305           tnta41r (kpjba:tnta41ds);
088900120306           IF  O41f06 = 'S';
089000120305             wInfo = *on;
089100120305             VISinfot = O41ifotot;
089200120305           ENDIF;
089300120305
089400120305       //?Se ci sono offerte le INFO sono obbligatorie, se non inserite prima
089500120305       //?ciclo a richiamare F15 fino a che non sono state inserite
089600120305           DOW  VISinfot = *blanks and wOfferte;
089700120305             clear TNTA41DS;
089800120305             I41nrv    = VISnrv;
089900120305             I41rag    = VISrag;
090000120305             I41mod    = 'G';
090100120305             I41ifotot = VISinfot;
090200120305             I41tpv    = C02tpv;
090300120305             tnta41r (kpjba:tnta41ds);
090400120306             IF  O41f06 = 'S';
090500120305               wInfo = *on;
090600120305               VISinfot = O41ifotot;
090700120305             ENDIF;
090800120305           ENDDO;
090900120130         ENDIF;
091000120130
091100120130       ENDSR;
091200120305
091300120305       //--------------------------------------------------------------
091400120305       //?Cancello le offerte se congelate o accettate.
091500120305       //--------------------------------------------------------------
091600120305       BEGSR Mod_Offerte;
091700110706
091800120305         wEoF = *off;
091900120305         setll C02nrv TIVOF11L;
092000120305         reade C02nrv TIVOF11L;
092100120305         DOW  not wEoF;
092200120305           IF  %eof(TIVOF11L);
092300120305             wEoF = *on;
092400120305             leave;
092500120305           ENDIF;
092600120305           IF  VOFeso = 'H' or VOFeso = 'A';
092700120305             VOFeso = '*';
092800120306             exsr Del_Offerte;
092900120306             exsr Del_VofSic;
093000120305             update TIVOF000;
093100120305           ENDIF;
093200120305           reade C02nrv TIVOF11L;
093300120305         ENDDO;
093400120305
093500120305       ENDSR;
093600120305
093700120305       //--------------------------------------------------------------
093800120305       //?Deleto le offerte.
093900120305       //--------------------------------------------------------------
094000120305       BEGSR Del_Offerte;
094100120305
094200120305       //?Annullo dati tipo legati all'offerta
094300120305         clear TNTA47DS;
094400120305         D47tup = '2';
094500120305         D47cto = 'O';
094600120305         D47dsf = 'S';
094700120312         D47ksc = VOFnrv;
094800120305         D47ctr = VOFctr;
094900120305         D47prg = VOFprg;
095000120305         kpjbu  = TNTA47DS;
095100120305         tnta47r (kpjba);
095200120305
095300120305       //?Cancello le offerte
095400120312         IF  %subst(knsif : 7 : 1) = 'P';
095500120312           exec sql
095600120312           DELETE from FILTRAGRPF/TIOGC00F
095700120312           WHERE TGCksc = :VOFnrv and TGCctr = :VOFctr and TGCprg = :VOFprg;
095800120312           exec sql
095900120312           DELETE from FILTRAGRPF/TIOPD00F
096000120312           WHERE TPDksc = :VOFnrv and TPDctr = :VOFctr and TPDprg = :VOFprg;
096100120312           exec sql
096200120312           DELETE from FILTRAGRPF/TIOPT00F
096300120312           WHERE TPTksc = :VOFnrv and TPTctr = :VOFctr and TPTprg = :VOFprg;
096400120312           exec sql
096500120312           DELETE from FILTRAGRPF/TIOFD00F
096600120312           WHERE TADksc = :VOFnrv and TADctr = :VOFctr and TADprg = :VOFprg;
096700120312           exec sql
096800120312           DELETE from FILTRAGRPF/TNOFM00F
096900120312           WHERE TAMksc = :VOFnrv and TAMctr = :VOFctr and TAMprg = :VOFprg;
097000120312         ELSE;
097100120312           exec sql
097200120312           DELETE from FILTRAGRU/TIOGC00F
097300120312           WHERE TGCksc = :VOFnrv and TGCctr = :VOFctr and TGCprg = :VOFprg;
097400120312           exec sql
097500120312           DELETE from FILTRAGRU/TIOPD00F
097600120312           WHERE TPDksc = :VOFnrv and TPDctr = :VOFctr and TPDprg = :VOFprg;
097700120312           exec sql
097800120312           DELETE from FILTRAGRU/TIOPT00F
097900120312           WHERE TPTksc = :VOFnrv and TPTctr = :VOFctr and TPTprg = :VOFprg;
098000120312           exec sql
098100120312           DELETE from FILTRAGRU/TIOFD00F
098200120312           WHERE TADksc = :VOFnrv and TADctr = :VOFctr and TADprg = :VOFprg;
098300120312           exec sql
098400120312           DELETE from FILTRAGRU/TNOFM00F
098500120312           WHERE TAMksc = :VOFnrv and TAMctr = :VOFctr and TAMprg = :VOFprg;
098600120312         ENDIF;
098700120305
098800120305       ENDSR;
098900120306
099000120306       //--------------------------------------------------------------
099100120306       //?Cancello l'offerta 999.
099200120306       //--------------------------------------------------------------
099300120306       BEGSR Mod_Off999;
099400120306
099500120306         wEoF = *off;
099600120306         setll C02nrv TIVOF01L;
099700120306         reade C02nrv TIVOF01L;
099800120306         DOW  not wEoF;
099900120306           IF  %eof(TIVOF01L);
100000120306             wEoF = *on;
100100120306             leave;
100200120306           ENDIF;
100300120306           IF  VOFctr = 999;
100400120306             exsr Del_VofSic;
100500120306             delete TIVOF01;
100600120306           ENDIF;
100700120306           reade C02nrv TIVOF01L;
100800120306         ENDDO;
100900120306
101000120306       ENDSR;
101100120306
101200120306       //--------------------------------------------------------------
101300120306       //?Deleto le offerte dal SIC.
101400120306       //--------------------------------------------------------------
101500120306       BEGSR Del_VofSic;
101600120306
101700120306       //?Cancello le offerte presenti sul SIC
101800120306         exec sql
101900120306         DELETE from TIVOFC0F
102000120306         WHERE VOCnrv = :VOFnrv and VOCctr = :VOFctr and VOCprg = :VOFprg;
102100120306
102200120306       ENDSR;
102300120305
102400110706       //--------------------------------------------------------------
102500120306       //?Modifico Sic.
102600110706       //--------------------------------------------------------------
102700120306       BEGSR Mod_Sic;
102800120130
102900120130         clear Tiket;
103000110706
103100120130       //?Recupero il tiket dalle info e cancello i dati del SIC delle info
103200110706         IF  %subst(knsif : 7 : 1) = 'P';
103300110706           exec sql
103400110706           SELECT VICtck into :Tiket
103500110706           FROM GAITRAAZP/TIVIIC0F
103600110706           WHERE VICnrv = :VISnrv;
103700110706
103800110706           exec sql
103900110706           DELETE from GAITRAAZP/TIVIIC0F
104000110706           WHERE VICnrv = :VISnrv;
104100110706         ELSE;
104200110706           exec sql
104300110706           SELECT VICtck into :Tiket
104400110706           FROM GAITRAAZM/TIVIIC0F
104500110706           WHERE VICnrv = :VISnrv;
104600110706
104700110706           exec sql
104800110706           DELETE from GAITRAAZM/TIVIIC0F
104900110706           WHERE VICnrv = :VISnrv;
105000110706         ENDIF;
105100120130
105200120130       //?Se non ho trovato il tiket sulle info (vuol dire che non c'erano info)
105300120130       //?lo recupero dalla trattativa
105400120130         IF  Tiket = *blanks;
105500120130           IF  %subst(knsif : 7 : 1) = 'P';
105600120130             exec sql
105700120130             SELECT VICtck into :Tiket
105800120130             FROM GAITRAAZP/TIVISC0F
105900120130             WHERE VICnrv = :VISnrv;
106000120130           ELSE;
106100120130             exec sql
106200120130             SELECT VICtck into :Tiket
106300120130             FROM GAITRAAZM/TIVISC0F
106400120130             WHERE VICnrv = :VISnrv;
106500120130           ENDIF;
106600120130         ENDIF;
106700110706
106800110706         clear TIVIIC00;
106900120130         wEoF    = *off;
107000120130         wOKinfo = *off;
107100110706
107200110926         wSQL = 'select * from TIVII00F ';
107300110706
107400110706         wSQL += 'where VIInrv = ' + %editc(VISnrv:'X') +
107500110706                   ' order by VIItpf';
107600110706
107700110706         exec sql prepare A1 from :wSQL;
107800110706
107900110706         exec sql declare VII cursor for A1;
108000110706
108100110706         exec sql open VII;
108200110706
108300110706         IF  sqlcode < 0;
108400120130           wEoF = *on;
108500110706         ENDIF;
108600110706
108700120130         DOW  not wEoF;
108800110706           exec sql fetch next from VII into :TIVIIDS;
108900110706           IF  sqlcod = 100 or sqlcod < 0;
109000120130             wEoF = *on;
109100110706             leave;
109200110706           ENDIF;
109300110706
109400110706       //?Imposto i dati per scrivere il record
109500120130           wOKinfo = *on;
109600110706           VICnrv = VIInrv;
109700110706
109800110706           SELECT;
109900110706         //?Fatturato/Spedizioni totale
110000110706             WHEN  VIItpf = 'TRT';
110100110706               VICfatt = VIIpft;
110200110706               VICnspt = VIIsna;
110300110706         //?Fatturato/Spedizioni estero
110400110706             WHEN  VIItpf = 'TRE';
110500110706               VICfate = VIIpft;
110600110706               VICnspe = VIIsna;
110700110706         //?Aumento/Sconto
110800110706             WHEN  VIItpf = 'A/S';
110900110706               VICausc = VIIvald;
111000110706         //?Delta
111100110706             WHEN  VIItpf = 'DEL';
111200110706               VICdel = VIIval;
111300110706         //?Peso
111400110706             WHEN  VIItpf = 'KMS';
111500110706               VICpkg = VIIval;
111600110706         //?Concorrenti
111700110706             WHEN  VIItpf = '10' and VIIspf = 'ARCO';
111800110706               VICarco = 'S';
111900110706             WHEN  VIItpf = '10' and VIIspf = 'ARTO';
112000110706               VICarto = 'S';
112100110706             WHEN  VIItpf = '10' and VIIspf = 'AWS';
112200110706               VICaws  = 'S';
112300110706             WHEN  VIItpf = '10' and VIIspf = 'BARS';
112400110706               VICbars = 'S';
112500110706             WHEN  VIItpf = '10' and VIIspf = 'DHL';
112600110706               VICdhl  = 'S';
112700110706             WHEN  VIItpf = '10' and VIIspf = 'EXEC';
112800110706               VICexec = 'S';
112900110706             WHEN  VIItpf = '10' and VIIspf = 'FERC';
113000110706               VICferc = 'S';
113100110706             WHEN  VIItpf = '10' and VIIspf = 'MAIL';
113200110706               VICmail = 'S';
113300110706             WHEN  VIItpf = '10' and VIIspf = 'MEP';
113400110706               VICmep  = 'S';
113500110706             WHEN  VIItpf = '10' and VIIspf = 'MTN';
113600110706               VICmtn  = 'S';
113700110706             WHEN  VIItpf = '10' and VIIspf = 'ONEE';
113800110706               VIConee = 'S';
113900110706             WHEN  VIItpf = '10' and VIIspf = 'PALL';
114000110706               VICpall = 'S';
114100110706             WHEN  VIItpf = '10' and VIIspf = 'PALW';
114200110706               VICpalw = 'S';
114300110706             WHEN  VIItpf = '10' and VIIspf = 'SDA';
114400110706               VICsda  = 'S';
114500110706             WHEN  VIItpf = '10' and VIIspf = 'SUSA';
114600110706               VICsusa = 'S';
114700110706             WHEN  VIItpf = '10' and VIIspf = 'TNT';
114800110706               VICtnt  = 'S';
114900110706             WHEN  VIItpf = '10' and VIIspf = 'UPS';
115000110706               VICups  = 'S';
115100110706             WHEN  VIItpf = '10' and VIIspf = '9910';
115200110706               VICaltr = 'S';
115300110706             WHEN  VIItpf = '10' and VIIspf = '8810';
115400110706               VICness = 'S';
115500110706           ENDSL;
115600110706         ENDDO;
115700110706
115800110706       //?Scrivo il rcd delle info commerciali
115900120130         IF  wOKinfo;
116000110706           VICtck = Tiket;
116100110706           write  TIVIIC00;
116200111102           feod TIVIIC0F;
116300110706         ENDIF;
116400110706
116500110706         exec sql close VII;
116600120130
116700120130       //?Devo aggiornare anche la trattativa sul SIC
116800120130         IF  %subst(knsif : 7 : 1) = 'P';
116900120130           exec sql
117000120130           UPDATE GAITRAAZP/TIVISC0F
117100120130           SET VICtpv = :VIStpv
117200120130           WHERE VICnrv = :VISnrv;
117300120130         ELSE;
117400120130           exec sql
117500120130           UPDATE GAITRAAZM/TIVISC0F
117600120130           SET VICtpv = :VIStpv
117700120130           WHERE VICnrv = :VISnrv;
117800120130         ENDIF;
117900110706
118000110706       ENDSR;
118100120130
118200120130       //--------------------------------------------------------------
118300120130       //?Scrivo note per promemoria.
118400120130       //--------------------------------------------------------------
118500120130       BEGSR Wrt_Note;
118600120130
118700120130         clear TRMK20ds;
118800120130         IMK20tla = 'L';
118900120130         IMK20flm = 'C';
119000120130         IMK20fno = 'S';
119100120130         IMK20cpo = VIScpo;
119200120130         IF  C02ksc > 0;
119300120130           IMK20ksc = C02ksc;
119400120130         ENDIF;
119500120130         IMK20nrv = VISnrv;
119600120130         EMK20no1 = 'Trattativa variata da ' +
119700120130                     old_VIStpv + ' a ' + VIStpv +
119800120313                    '. Variazione eseguita da ' + knmus;
119900120130         trmk20r ( kpjba : trmk20ds );
120000120130
120100120130       ENDSR;
120200090807
120300080206       //--------------------------------------------------------------
120400080206       //?Operazioni finali.
120500080206       //--------------------------------------------------------------
120600080206       BEGSR RoutEnd;
120700090521
120800080206         *inLR = *on;
120900080206         return;
121000080206
121100080206       ENDSR;
121200080206
121300080206      /end-free
121400090807
121500080206       //--------------------------------------------------------------
121600080206       //?Schiere a tempo di compilazione.
121700080206       //--------------------------------------------------------------
121800080206
121900120130** - w_MSG -------------------------------------------------------------------*
122000110706Immettere la trattativa da variare                                             1
122100110706Trattativa inesistente                                                         2
122200110930Trattativa ancora aperta                                                       3
122300120130Trattativa non di tipo "A" o "I"                                               4
122400120130Il tipo trattativa non è stato variato                                         5
122500120305Modifica tipo trattativa non possibile                                         6
122600120305Tipo trattativa errato                                                         7
122700120706La trattativa è stata chiusa oggi, modificarla domani!!!                       8
