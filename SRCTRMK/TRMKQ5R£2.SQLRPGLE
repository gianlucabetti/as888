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
002800110706
002900110706       // -?File video
003000120127     fTRMKQ5D   cf   e             workstn
003100120127     f                                     sfile(mkq5S02 : S02nrr)
003200080208     f                                     indds(IndDspF)
003300080206     f                                     infds(InfDspF)
003400080206
003500080206      //---------------------------------------------------------------
003600090406      //?Definizione costanti.
003700080206      //---------------------------------------------------------------
003800080207
003900110706       // -?Tasti funzionali a video
004000080207     d c_F01           c                   const(x'31')
004100080207     d c_F02           c                   const(x'32')
004200080207     d c_F03           c                   const(x'33')
004300090406     d c_F04           c                   const(x'34')
004400080207     d c_F05           c                   const(x'35')
004500080207     d c_F06           c                   const(x'36')
004600080207     d c_F07           c                   const(x'37')
004700080207     d c_F08           c                   const(x'38')
004800080207     d c_F09           c                   const(x'39')
004900080207     d c_F10           c                   const(x'3A')
005000090303     d c_F11           c                   const(x'3B')
005100080207     d c_F12           c                   const(x'3C')
005200080207     d c_F13           c                   const(x'B1')
005300080207     d c_F14           c                   const(x'B2')
005400080207     d c_F15           c                   const(x'B3')
005500080207     d c_F16           c                   const(x'B4')
005600080207     d c_F17           c                   const(x'B5')
005700080207     d c_F18           c                   const(x'B6')
005800080207     d c_F19           c                   const(x'B7')
005900080207     d c_F20           c                   const(x'B8')
006000080207     d c_F21           c                   const(x'B9')
006100080207     d c_F22           c                   const(x'BA')
006200080207     d c_F23           c                   const(x'BB')
006300080207     d c_F24           c                   const(x'BC')
006400080207     d c_Enter         c                   const(x'F1')
006500080207     d c_RollDown      c                   const(x'F4')
006600080207     d c_RollUp        c                   const(x'F5')
006700091124
006800110706       // -?Costante per controllo "caratteri solo numerici"?
006900110706     d c_Digits        c                   const('0123456789')
007000080206
007100080206      //---------------------------------------------------------------
007200080206      //?Definizione schiere.
007300080206      //---------------------------------------------------------------
007400120305
007500120305       // -?Sk Tipi Trattativa con INFO
007600120305     d skTTRinfo       s              1    dim(30)
007700080206
007800110706       // -?Messaggi di errore
007900120130     d w_Msg           s             78    dim(10) ctdata perrcd(1)
008000080206
008100080206      //---------------------------------------------------------------
008200080206      //?Definizione aree dati.
008300080206      //---------------------------------------------------------------
008400080206
008500110706       // -?Dati utente
008600080206     d §AzUte        e ds                  extname(AZUTE00F)
008700080206     d                                     dtaara
008800080206     d §DatiUte      e ds                  extname(dDatiUte)
008900080206     d                                     dtaara
009000080206
009100080206      //---------------------------------------------------------------
009200080206      //?Definizione strutture dati.
009300080206      //---------------------------------------------------------------
009400080206
009500110706       // -?Status
009600080206     d Psds           sds
009700080206     d   SDSpgm          *proc
009800080206
009900110706       // -?InfDS
010000080206     d InfDspF         ds
010100080207     d  dsp_aid              369    369a
010200130918     d**sfl_rrn              376    377i 0
010300130918     d**min_nrr              378    379i 0
010400130918     d**num_rcds             380    381i 0
010500080206
010600110706       // -?Indicatori su DspF
010700080208     d IndDspF         ds
010800120127       // -?Indicatori di errore
010900120127     d  ErrMessage                    1n   overlay(IndDspF : 28)
011000120127       // -?Indicatori di gestione subfile
011100120130     d  SflDsp                        1n   overlay(IndDspF : 30)
011200120130     d  SflDspCtl                     1n   overlay(IndDspF : 31)
011300120127     d  SflNxtChg                     1n   overlay(IndDspF : 32)
011400120127     d  SflEnd                        1n   overlay(IndDspF : 33)
011500120127       // -?Altri Indicatori
011600110706     d  PosCurNrv                     1n   overlay(IndDspF : 50)
011700120130     d  PosCurTpv                     1n   overlay(IndDspF : 51)
011800090807     d  ErrGenerico                   1n   overlay(IndDspF : 99)
011900090407
012000090407     d WindDspF        ds                  inz
012100090407     d   WindDspFa             1     49    inz(*zero)
012200090407     d   WindDspFb            50     99    inz(*zero)
012300080206
012400110706       // -?Paremetri ricevuti
012500080206     d KPJBA         e ds
012600120130
012700120130      // -?Ricerca/Controllo tabelle
012800120130     d TIBS02ds      e ds                  inz
012900080206
013000110706       // -?Reperimento dati utente
013100080206     d TIBS34ds      e ds
013200120130
013300120130       // -?Reperimento dati anagrafici cliente codificato
013400120130     d TIBS69ds      e ds
013500120130     d DS_cnaco      e ds                  inz extname(CNACO00F)
013600120130     d DS_cnind      e ds                  inz extname(CNIND00F)
013700120130     d DS_cnclp      e ds                  inz extname(CNCLP00F)
013800120130     d DS_fncls      e ds                  inz extname(FNCLS00F)
013900110706
014000110706       // -?Info Trattativa
014100110706     d TNTA41DS      e ds                  inz
014200120305
014300120305       // -?Cancella Dati tipo
014400120305     d TNTA47DS      e ds                  inz
014500120130
014600120130      // -?Gestione Note clienti/potenziali
014700120130     d TRMK20ds      e ds                  inz
014800110706
014900110706       // -?File Info Trattative
015000110706     d TIVIIds       e ds                  extname(TIVII00F)
015100120127
015200120127       // -?File SIC Trattative chiuse
015300130918     d*// TIVISCds      e ds                  extname(TIVISC0F)
015400120130
015500120130       // -?DS Tabella TR - Tipi Tariffa
015600120130     d dsTR          e ds
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
017800120127     d S02nrr          s              4  0 inz
017900110706
018000110706       // -?Campi di comodo
018100120130     d old_VIStpv      s                   like(VIStpv)
018200110706     d Tiket           s                   like(VICtck)
018300120706     d wOggi           s              8  0 inz
018400120306     d w0030           s              3s 0
018500120130
018600120130       // -?Campi di comodo data
018700120130     d wDataEUR        s               d   datfmt(*eur)
018800120305
018900120305       // -?Indici di schiera
019000120305     d xx              s              3  0 inz
019100090806
019200110706       // -?Stringa SQL da eseguire
019300090806     d wSQL            s           2048    Varying        inz
019400090508
019500090508      //---------------------------------------------------------------
019600090807      //?Definizione procedure esterne.
019700090508      //---------------------------------------------------------------
019800110506
019900110706       // -?Gestione Info Trattativa
020000110706     d TNTA41R         pr                  extpgm('TNTA41R')
020100110706     d  kpjba                              likeds(kpjba)
020200110706     d  tnta41ds                           likeds(tnta41ds)
020300120305
020400120305       // -?Cancella dati tipo
020500120305     d TNTA47R         pr                  extpgm('TNTA47R')
020600120305     d  kpjba                              likeds(kpjba)
020700110706
020800110706      //---------------------------------------------------------------
020900110706      //?Definizione prototipi.
021000110706      //---------------------------------------------------------------
021100120130
021200120130       // -?Ricerc/Controllo tabelle
021300120130      /copy gaitrasrc/srcprotopr,tibs02r
021400110706
021500110706       // -?Reperimento dati utente
021600110706      /copy gaitrasrc/srcprotopr,tibs34r
021700120130
021800120130       // -?Reperimento dati anagrafici cliente codificato
021900120130      /copy gaitrasrc/srcprotopr,tibs69r
022000120130
022100120130       // -?Scrittura note
022200120130      /copy gaitrasrc/srcprotopr,trmk20r
022300090807
022400080206      //---------------------------------------------------------------
022500080206      //?Definizione key-list.
022600080206      //---------------------------------------------------------------
022700090807
022800120130       // -?File TABEL00F
022900120130     d k03tabel      e ds                  extname(TABEL00F:*key)
023000120130     d                                     prefix(k_)
023100080206
023200080206      //---------------------------------------------------------------
023300080206      //?Riepilogo indicatori.
023400080206      //---------------------------------------------------------------
023500090807
023600090807
023700080206      //---------------------------------------------------------------
023800080206
023900080206      //---------------------------------------------------------------
024000080206      //?M A I N - L I N E
024100080206      //---------------------------------------------------------------
024200080206
024300080206     c     *Entry        plist
024400080206     c                   parm                    KPJBA
024500080206
024600080206      /free
024700080206
024800090807       //?Operazioni iniziali
024900080206       exsr RoutInz;
025000080206
025100090807       //?Gestione video
025200120130       DOW wFine = *off;
025300090807
025400080206         select;
025500120130           when wVideo = 'D1';
025600110706             exsr GesD01;
025700120130           when wVideo = 'S2';
025800120130             exsr GesS02;
025900090428           other;
026000120130             wFine = *on;
026100080206         endsl;
026200090807
026300080206       ENDDO;
026400080206
026500090807       //?Operazioni finali
026600080206       exsr RoutEnd;
026700080206
026800080206       //--------------------------------------------------------------
026900080206       //?Operazioni iniziali.
027000080206       //--------------------------------------------------------------
027100080206       BEGSR RoutInz;
027200080206
027300120130         wVideo = 'D1';
027400120130         wInzD01 = *on;
027500120706
027600120706       //?Imposto data elaborazione
027700120706         wOggi = %dec(%date());
027800090807
027900110706       //?Reperimento dati job
028000090807         exsr DatiJob;
028100100209
028200090807         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
028300090806
028400110706       //?Impostazione campi "fissi"
028500110706         V1Tpgm = SDSpgm;
028600120130         k_TBLkut = 1;
028700120305
028800120305       //?Carico Tipi trattative che prevedono INFO
028900120305         clear xx;
029000120305         setll ('TTR') TNTBE01L;
029100120305         reade ('TTR') TNTBE01L;
029200120305
029300120305 2       DOW  not %eof(TNTBE01L);
029400120305 3         IF  TBEatb = *blanks;
029500120305             dTTR = TBEuni;
029600120305             IF  §TTRinfo = 'S';
029700120305               xx += 1;
029800120305               skTTRinfo(xx) = TBEke1;
029900120305             ENDIF;
030000120305 3         ENDIF;
030100120305
030200120305           reade ('TTR') TNTBE01L;
030300120305 2       ENDDO;
030400090714
030500080206       ENDSR;
030600080206
030700080206       //--------------------------------------------------------------
030800080206       //?Reperimento Dati del job (Utente/Operativi).
030900080206       //--------------------------------------------------------------
031000080206       BEGSR DatiJob;
031100080206
031200080206         in(E) §AzUte;
031300110706         IF NOT %error;
031400080206           in(E) §DatiUte;
031500110706         ENDIF;
031600110706         IF %error or RSut = *blanks;
031700080206           clear TIBS34ds;
031800080206           tibs34r(tibs34ds);
031900080206           in §AzUte;
032000080206           in §DatiUte;
032100110706         ENDIF;
032200080206
032300080206       ENDSR;
032400080206
032500090421       //--------------------------------------------------------------
032600110706       //?Gestione videata D01
032700080206       //--------------------------------------------------------------
032800110706       BEGSR GesD01;
032900080207
033000110706       //?Inizializzazione videata
033100120130         IF  wInzD01 = *on;
033200110706            exsr InzD01;
033300120130            wInzD01  = *off;
033400110706         ENDIF;
033500080207
033600110706       //?Emissione Testata
033700120130         write  MKQ5T01;
033800080207
033900110706       //?Emissione videata
034000120130         exfmt  MKQ5D01;
034100110706         reset ErrMessage;
034200110706         reset ErrGenerico;
034300120127         clear V01msg;
034400080207
034500080207         SELECT;
034600110706
034700110706         //?- F03=Fine
034800110706           WHEN  dsp_aid = c_F03;
034900110706             exsr F03D01;
035000080207
035100090807         //?Invio
035200080207           OTHER;
035300110706         //?Eseguo i controlli
035400110706             exsr CtrD01;
035500110706             IF  ErrGenerico;
035600080207               leavesr;
035700110706             ENDIF;
035800120127         //?Se tutto OK vado nella seconda videata
035900120130           wVideo = 'S2';
036000120130           wInzS02 = *on;
036100080207
036200080207         ENDSL;
036300080207
036400080207       ENDSR;
036500080207
036600080207       //--------------------------------------------------------------
036700110706       //?Inizializzazione videata D01.
036800080207       //--------------------------------------------------------------
036900110706       BEGSR InzD01;
037000080207
037100120127         clear V01nrv;
037200090508
037300080207       ENDSR;
037400080207
037500091111       //--------------------------------------------------------------
037600110706       //?Gestione tasto funzionale F03 da videata D01.
037700110706       //?F3=Fine
037800091111       //--------------------------------------------------------------
037900110706       BEGSR F03D01;
038000091111
038100110706       //?Chiusura del programma
038200120130         wFine = *on;
038300091111
038400091111       ENDSR;
038500120130
038600120130       //--------------------------------------------------------------
038700120130       //?Controllo dati videata D01.
038800120130       //--------------------------------------------------------------
038900120130       BEGSR CtrD01;
039000120130
039100120130         WindDspF  = IndDspF;
039200120130         reset WindDspFb;
039300120130         IndDspF   = WindDspF;
039400120130
039500120130       //?Obbligo della trattativa
039600120130         IF  V01nrv = 0;
039700120130           ErrMessage  = *on;
039800120130           ErrGenerico = *on;
039900120130           PosCurNrv   = *on;
040000120130           V01msg      = w_Msg(01);
040100120130           leavesr;
040200120130         ENDIF;
040300120130
040400120130       //?La trattativa deve esistere
040500120130         chain(n) V01nrv TIVIS05L;
040600120130         IF  not %found(TIVIS05L);
040700120130           ErrMessage  = *on;
040800120130           ErrGenerico = *on;
040900120130           PosCurNrv   = *on;
041000120130           V01msg      = w_Msg(02);
041100120130           leavesr;
041200120130         ENDIF;
041300120130
041400120130       //?La trattativa deve essere chiusa
041500120130         IF  VISdch = 0;
041600120130           ErrMessage  = *on;
041700120130           ErrGenerico = *on;
041800120130           PosCurNrv   = *on;
041900120130           V01msg      = w_Msg(03);
042000120130           leavesr;
042100120130         ENDIF;
042200120706
042300120706       //?La trattativa deve avere data chiusura < data del giorno
042400120706         IF  VISdch >= wOggi;
042500120706           ErrMessage  = *on;
042600120706           ErrGenerico = *on;
042700120706           PosCurNrv   = *on;
042800120706           V01msg      = w_Msg(08);
042900120706           leavesr;
043000120706         ENDIF;
043100120130
043200120130       //?Accetto solo Trattative di tipo 'A' o 'I'
043300120130         IF  VIStpv <> 'A' and VIStpv <> 'I';
043400120130           ErrMessage  = *on;
043500120130           ErrGenerico = *on;
043600120130           PosCurNrv   = *on;
043700120130           V01msg      = w_Msg(04);
043800120130           leavesr;
043900120130         ENDIF;
044000120130
044100120130       ENDSR;
044200120130
044300120130       //--------------------------------------------------------------
044400120130       //?Gestione videata S02
044500120130       //--------------------------------------------------------------
044600120130       BEGSR GesS02;
044700120130
044800120130       //?Inizializzazione videata
044900120130         IF  wInzS02 = *on;
045000120130            exsr InzS02;
045100120130            wInzS02  = *off;
045200120130         ENDIF;
045300120130
045400120130       //?Emissione Testata + Piede
045500120130         write  MKQ5T01;
045600120130         write  MKQ5P02;
045700120130
045800120130       //?Emissione subfile
045900120130         exfmt  MKQ5C02;
046000120130         reset ErrMessage;
046100120130         reset ErrGenerico;
046200120130         clear V02msg;
046300120130
046400120130         SELECT;
046500120130
046600120130         //?- F03=Fine
046700120130           WHEN  dsp_aid = c_F03;
046800120130             exsr F03D01;
046900120130
047000120130         //?- F06=Conferma
047100120130           WHEN  dsp_aid = c_F06;
047200120130           //?Eseguo i controlli
047300120130             exsr CtrC02 ;
047400120130             IF  ErrGenerico;
047500120130               leavesr;
047600120130             ENDIF;
047700120130             exsr F06S02;
047800120130
047900120130         //?- F12=Ritorno
048000120130           WHEN  dsp_aid = c_F12;
048100120130             exsr F12S02;
048200120306
048300120306         //?- F15=Info Trattativa
048400120306           WHEN  dsp_aid = c_F15;
048500120306             exsr F15S02;
048600120130
048700120130         //?Invio
048800120130           OTHER;
048900120130         //?Eseguo i controlli
049000120130             exsr CtrC02 ;
049100120130             IF  ErrGenerico;
049200120130               leavesr;
049300120130             ENDIF;
049400120130
049500120130         ENDSL;
049600120130
049700120130       ENDSR;
049800120130
049900120130       //--------------------------------------------------------------
050000120130       //?Inizializzazione videata S02.
050100120130       //--------------------------------------------------------------
050200120130       BEGSR InzS02;
050300120130
050400120130       //?Pulizia subfile
050500120130         exsr PulS02;
050600120130
050700120130       //?Carico dati nel control
050800120130         exsr CarC02;
050900120130
051000120130       //?Caricamento dati
051100120130         exsr CarS02;
051200120130
051300120130       //?Visualizzazione del Subfile
051400120130         SflDsp = (S02nrr <= *zeros);
051500120130
051600120130       //?Attivazione SFLEND
051700120130         SflEnd = *on;
051800120130
051900120130       //?Impaginazione Subfile
052000120130       //?Forzo sempre il primo rcd
052100120130         IF  S02nrr > *zero;
052200120130           C02rcd = 1;
052300120130           C02csr = 1;
052400120130         ELSE;
052500120130           clear C02rcd;
052600120130         ENDIF;
052700120130
052800120130         C02csr = C02rcd;
052900120130
053000120130       ENDSR;
053100120130
053200120130       //--------------------------------------------------------------
053300120130       //?Pulizia Subfile S02.
053400120130       //--------------------------------------------------------------
053500120130       BEGSR PulS02;
053600120130
053700120130       //?Pulizia subfile
053800120130         SflDsp    = *on;
053900120130         SflDspCtl = *on;
054000120130         write  MKQ5C02;
054100120130         SflDspCtl = *off;
054200120130         SflEnd    = *off;
054300120130
054400120130         clear C02rcd;
054500120130         clear C02csr;
054600120130         clear S02nrr;
054700120130         clear V02msg;
054800120130         ErrMessage  = *off;
054900120130         ErrGenerico = *off;
055000120130
055100120130         WindDspF = IndDspF;
055200120130         reset WindDspFb;
055300120130         IndDspF  = WindDspF;
055400120130
055500120130       ENDSR;
055600120130
055700120130       //--------------------------------------------------------------
055800120130       //?Caricamento Control C02.
055900120130       //--------------------------------------------------------------
056000120130       BEGSR CarC02;
056100120130
056200120130         C02nrv = V01nrv;
056300120130         wDataEUR = %date(VISdat:*iso);
056400120130         C02dat   = %dec(wDataEUR);
056500120130         C02esi   = VISesi;
056600120130         wDataEUR = %date(VISdch:*iso);
056700120130         C02dch   = %dec(wDataEUR);
056800120130         C02ksc   = VISksc;
056900120130         C02rag   = VISrag;
057000120130
057100120130       //?Se cliente codificato recupero i dati dall'anagrafico clienti
057200120130         IF  C02ksc > 0;
057300120130           clear TIBS69ds;
057400120130           I69kac = C02ksc;
057500120130           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
057600120130           C02rag = ACOrag;
057700120130         ENDIF;
057800120130
057900120130       //?Tipo trattativa + decodifica
058000120130         C02tpv = VIStpv;
058100120130         clear dTTR;
058200120130         clear TIBS02ds;
058300120130         T02mod = 'C';
058400120130         T02cod = 'TTR';
058500120130         T02ke1 = C02tpv;
058600120130         T02sif = KNSIF;
058700120130         TNTBE_RicercaControllo (kpjba : tibs02ds);
058800120130         IF  T02err = *blanks;
058900120130           dTTR = t02uni ;
059000120130         ENDIF;
059100120130
059200120130         C02dtpv = §TTRdes;
059300120130
059400120130       //?Codice commerciale + decodifica
059500130918         clear  C02cmm;
059600120130         C02cmm = VIScmm;
059700130918         chain  (C02cmm)  AZCMM000;
059800130918         IF  %found(AZCMM01L);
059900130918           C02dcmm = CMMdes;
060000130918         else;
060100130918           C02dcmm = *all'? ';
060200120130         ENDIF;
060300120130
060400120130       //?Mi posiziono sempre sul tipo trattativa
060500120130         PosCurTpv = *on;
060600120130
060700120130       ENDSR;
060800120130
060900120130       //--------------------------------------------------------------
061000120130       //?Caricamento Subfile S02.
061100120130       //--------------------------------------------------------------
061200120130       BEGSR CarS02;
061300120130
061400120130       //?Leggo il file delle offerte
061500120130         wEoF = *off;
061600120305         wOfferte = *off;
061700120305         wConvalida = *off;
061800120130         setll C02nrv TIVOF11L;
061900120306         reade(n) C02nrv TIVOF11L;
062000120130         DOW  not wEoF;
062100120130           IF  %eof(TIVOF11L);
062200120130             wEoF = *on;
062300120130             leave;
062400120130           ENDIF;
062500120306         //?Se offerta annullata la salto
062600120306           IF  VOFeso <> '*';
062700120306           //?Mi salvo che ho trovato offerte
062800120306             IF  S02nrr = 0;
062900120306               wOfferte = *on;
063000120306             ENDIF;
063100120306           //?Carico il subfile
063200120306             exsr RieS02;
063300120306           //?Scrico il subfile
063400120306             S02nrr += 1;
063500120306             write MKQ5S02;
063600120306           ENDIF;
063700120306           reade(n) C02nrv TIVOF11L;
063800120130         ENDDO;
063900120306
064000120306       //?Se tipo offerta "I" faccio un giro con la 999
064100120306         IF  VIStpv = 'I';
064200120306           exsr Cerca_999;
064300120306         ENDIF;
064400120130
064500120130       ENDSR;
064600120130
064700120130       //--------------------------------------------------------------
064800120130       //?Carico i dati nel subfile S02.
064900120130       //--------------------------------------------------------------
065000120130       BEGSR RieS02;
065100120130
065200120130         clear  MKQ5S02;
065300120130
065400120130         S02ctr = VOFctr;
065500120130         S02prg = VOFprg;
065600120130
065700120130       //?Decodifco il tipo tariffa
065800120130         clear dsTR;
065900120130         k_TBLcod = 'TR';
066000120130         k_TBLkey = %subst(%editc(S02ctr:'X'):1:1);
066100120130         chain  %kds(K03tabel) TABEL00F;
066200120130         IF  %found(TABEL00F) and
066300120130              TBLflg = *blanks;
066400120130           dsTR = TBLuni;
066500120130         ENDIF;
066600120130
066700120130         S02dtr = §TRde3;
066800120130
066900120130         S02tpt = VOFtpt;
067000120130
067100120130       //?Esito
067200120130         SELECT;
067300120130         WHEN  VOFeso = 'H';
067400120130           S02eso = 'Congelata  ';
067500120130         WHEN  VOFeso = 'A';
067600120130           S02eso = 'Accettata  ';
067700120130         WHEN  VOFeso = 'C';
067800120130           S02eso = 'Convalidata';
067900120305           wConvalida = *on;
068000120130         OTHER;
068100120130           clear S02eso;
068200120130         ENDSL;
068300120130
068400120130       //?Cliente convalidato
068500120130         S02ksc = VOFksc;
068600120130         IF  S02ksc > 0;
068700120130           clear TIBS69ds;
068800120130           I69kac = S02ksc;
068900120130           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
069000120130           S02rag = ACOrag;
069100120130         ENDIF;
069200120130
069300120130       //?Data reale affido merci
069400120130         IF VOFdra > 0;
069500120130           wDataEUR = %date(VOFdra:*iso);
069600120130           S02dra   = %dec(wDataEUR);
069700120130         ENDIF;
069800120130
069900120130       ENDSR;
070000120130
070100120130       //--------------------------------------------------------------
070200120130       //?Controllo i dati relativi al formato C02
070300120130       //--------------------------------------------------------------
070400120305       BEGSR CtrC02;
070500120130
070600120130         WindDspF = IndDspF;
070700120130         reset WindDspFb;
070800120130         IndDspF  = WindDspF;
070900120130
071000120130       //?Se stesso tipo trattativa non ho variato nulla
071100120130         IF  C02tpv = VIStpv;
071200120130           ErrMessage  = *on;
071300120130           ErrGenerico = *on;
071400120130           PosCurTpv   = *on;
071500120130           V02msg      = w_Msg(05);
071600120130           leavesr;
071700120130         ENDIF;
071800120130
071900120130       //?Controllo il tipo trattativa
072000120130         clear dTTR;
072100120130         clear TIBS02ds;
072200120130         T02mod = 'C';
072300120130         T02cod = 'TTR';
072400120130         T02ke1 = C02tpv;
072500120130         T02sif = KNSIF;
072600120130         TNTBE_RicercaControllo (kpjba : tibs02ds);
072700120130         IF  T02err <> *blanks;
072800120130           ErrMessage  = *on;
072900120130           ErrGenerico = *on;
073000120130           PosCurTpv   = *on;
073100120305           V02msg      = w_Msg(07);
073200120130           leavesr;
073300120130         ENDIF;
073400120130
073500120130         dTTR = t02uni ;
073600120130         C02dtpv = §TTRdes;
073700120305
073800120305       //?Controllo se posso fare il cambio
073900120305         SELECT;
074000120305         //?Da "A" a "I" o da "I" ad "A" sempre possibile
074100120305           WHEN  C02tpv = 'A' or C02tpv = 'I';
074200120306         //?Da "A" a "R" possibile solo se presenti offerte
074300120306           WHEN  VIStpv = 'A' and C02tpv = 'R' and wOfferte;
074400120306         //?Da "I" a "R" possibile solo se presenti offerte
074500120306         //?                          e NON presente offerta 999
074600120306           WHEN  VIStpv = 'I' and C02tpv = 'R' and wOfferte
074700120306                                               and not wOff999;
074800120313         //?Da "A" a "M" possibile solo se NON presenti offerte convalidate
074900120313           WHEN  VIStpv = 'A' and C02tpv = 'M' and not wConvalida;
075000120306         //?Da "I" a "M" non possibile se presente offerta 999
075100120306         //?                         e se NON presenti offerte convalidate
075200120306           WHEN  VIStpv = 'I' and C02tpv = 'M' and not wOff999
075300120306                                               and not wConvalida;
075400120305           OTHER;
075500120305         //?Modifica non possibile
075600120305           ErrMessage  = *on;
075700120305           ErrGenerico = *on;
075800120305           PosCurTpv   = *on;
075900120305           V02msg      = w_Msg(06);
076000120313           IF  C02tpv <> 'M' and C02tpv <> 'R' and
076100120313               C02tpv <> 'A' and C02tpv <> 'I';
076200120313             V02msg = %trim(V02msg);
076300120313             leavesr;
076400120313           ENDIF;
076500120306           IF  wOff999;
076600120306             V02msg = %trim(V02msg) + ' presente CALDO senza offerta';
076700120313             leavesr;
076800120306           ENDIF;
076900120306           IF  wConvalida;
077000120306             V02msg = %trim(V02msg) + ' presenti offerte convalidate';
077100120313             leavesr;
077200120306           ENDIF;
077300120305           IF  not wOfferte;
077400120305             V02msg = %trim(V02msg) + ' NON presenti offerte';
077500120313             leavesr;
077600120305           ENDIF;
077700120305         ENDSL;
077800120130
077900120130       ENDSR ;
078000120127
078100120130       //--------------------------------------------------------------
078200120130       //?Gestione tasto funzionale F12 da videata S02.
078300120130       //?F12=Ritorno
078400120130       //--------------------------------------------------------------
078500120130       BEGSR F12S02;
078600120127
078700120130         wVideo = 'D1';
078800120130         wInzD01 = *on;
078900120130
079000120130       ENDSR;
079100120306
079200120306       //--------------------------------------------------------------
079300120306       //?Gestione tasto funzionale F15 da videata S02.
079400120306       //?F15=Info Trattativa
079500120306       //--------------------------------------------------------------
079600120306       BEGSR F15S02;
079700120306
079800120306         clear TNTA41DS;
079900120306         I41nrv    = VISnrv;
080000120306         I41rag    = VISrag;
080100120306         I41mod    = 'I';
080200120306         I41ifotot = VISinfot;
080300120306         I41tpv    = VIStpv;
080400120306         tnta41r (kpjba:tnta41ds);
080500120306
080600120306       ENDSR;
080700120127
080800110706       //--------------------------------------------------------------
080900120130       //?Gestione tasto funzionale F06 da videata S02.
081000110706       //?F6=Conferma
081100110706       //--------------------------------------------------------------
081200120130       BEGSR F06S02;
081300110706
081400110706       //?Modifico info trattativa
081500120130         exsr Mod_Info;
081600120130
081700120130       //?Modifico trattativa
081800120130         chain C02nrv TIVIS05L;
081900120130         IF  %found(TIVIS05L);
082000120131           old_VIStpv = VIStpv;
082100120305           VISTPV = C02tpv;
082200120130           update TIVIS000;
082300120130         ENDIF;
082400120305
082500120321       //?Cancellazione offerte se congelate o accettate solo se new tipo = 'M'
082600120321         IF  wOfferte and C02tpv = 'M';
082700120305           exsr Mod_Offerte;
082800120305         ENDIF;
082900120306
083000120306       //?Se la trattativa era di tipo "I" cancello eventuale offerta 999
083100120306         IF  old_VIStpv = 'I';
083200120306           exsr Mod_Off999;
083300120306         ENDIF;
083400110706
083500120306       //?Modifico dati SIC
083600120306         exsr Mod_Sic;
083700120130
083800120130       //?Scrivo note come promemoria
083900120130         exsr Wrt_Note;
084000120130
084100120130       //?Alla fine torno alla prima videata
084200120130         exsr F12S02;
084300110706
084400110706       ENDSR;
084500120306
084600120306       //--------------------------------------------------------------
084700120306       //?Cerco se c'è l'offerta 999.
084800120306       //--------------------------------------------------------------
084900120306       BEGSR Cerca_999;
085000120306
085100120306         wOff999 = *off;
085200120306         w0030   = 999;
085300120306         chain(n) (C02nrv:w0030) TIVOF01L;
085400120306         IF  %found(TIVOF01L) and VOFeso <> '*';
085500120306           wOff999 = *on;
085600120306         ENDIF;
085700120306
085800120306       ENDSR;
085900110706
086000110706       //--------------------------------------------------------------
086100110706       //?Modifico Info Trattative.
086200110706       //--------------------------------------------------------------
086300110706       BEGSR Mod_Info;
086400120130
086500120130         wInfo = *off;
086600120130
086700120130       //?Cancello le vecchie info
086800120130         exec sql
086900120130         DELETE from TIVII00F
087000120130         WHERE VIInrv = :VISnrv;
087100120305
087200120305       //?Pulisco il campo di INFO inserite su TIVIS
087300120305         clear VISinfot;
087400120130
087500120305       //?Se la trattativa le prevede richiedo le INFO
087600120305         IF  %lookup(C02tpv:skTTRinfo) > 0;
087700120305           clear TNTA41DS;
087800120305           I41nrv    = VISnrv;
087900120305           I41rag    = VISrag;
088000120305           I41mod    = 'G';
088100120305           I41ifotot = VISinfot;
088200120305           I41tpv    = C02tpv;
088300120305           tnta41r (kpjba:tnta41ds);
088400120306           IF  O41f06 = 'S';
088500120305             wInfo = *on;
088600120305             VISinfot = O41ifotot;
088700120305           ENDIF;
088800120305
088900120305       //?Se ci sono offerte le INFO sono obbligatorie, se non inserite prima
089000120305       //?ciclo a richiamare F15 fino a che non sono state inserite
089100120305           DOW  VISinfot = *blanks and wOfferte;
089200120305             clear TNTA41DS;
089300120305             I41nrv    = VISnrv;
089400120305             I41rag    = VISrag;
089500120305             I41mod    = 'G';
089600120305             I41ifotot = VISinfot;
089700120305             I41tpv    = C02tpv;
089800120305             tnta41r (kpjba:tnta41ds);
089900120306             IF  O41f06 = 'S';
090000120305               wInfo = *on;
090100120305               VISinfot = O41ifotot;
090200120305             ENDIF;
090300120305           ENDDO;
090400120130         ENDIF;
090500120130
090600120130       ENDSR;
090700120305
090800120305       //--------------------------------------------------------------
090900120305       //?Cancello le offerte se congelate o accettate.
091000120305       //--------------------------------------------------------------
091100120305       BEGSR Mod_Offerte;
091200110706
091300120305         wEoF = *off;
091400120305         setll C02nrv TIVOF11L;
091500120305         reade C02nrv TIVOF11L;
091600120305         DOW  not wEoF;
091700120305           IF  %eof(TIVOF11L);
091800120305             wEoF = *on;
091900120305             leave;
092000120305           ENDIF;
092100120305           IF  VOFeso = 'H' or VOFeso = 'A';
092200120305             VOFeso = '*';
092300120306             exsr Del_Offerte;
092400120306             exsr Del_VofSic;
092500120305             update TIVOF000;
092600120305           ENDIF;
092700120305           reade C02nrv TIVOF11L;
092800120305         ENDDO;
092900120305
093000120305       ENDSR;
093100120305
093200120305       //--------------------------------------------------------------
093300120305       //?Deleto le offerte.
093400120305       //--------------------------------------------------------------
093500120305       BEGSR Del_Offerte;
093600120305
093700120305       //?Annullo dati tipo legati all'offerta
093800120305         clear TNTA47DS;
093900120305         D47tup = '2';
094000120305         D47cto = 'O';
094100120305         D47dsf = 'S';
094200120312         D47ksc = VOFnrv;
094300120305         D47ctr = VOFctr;
094400120305         D47prg = VOFprg;
094500120305         kpjbu  = TNTA47DS;
094600120305         tnta47r (kpjba);
094700120305
094800120305       //?Cancello le offerte
094900120312         IF  %subst(knsif : 7 : 1) = 'P';
095000120312           exec sql
095100120312           DELETE from FILTRAGRPF/TIOGC00F
095200120312           WHERE TGCksc = :VOFnrv and TGCctr = :VOFctr and TGCprg = :VOFprg;
095300120312           exec sql
095400120312           DELETE from FILTRAGRPF/TIOPD00F
095500120312           WHERE TPDksc = :VOFnrv and TPDctr = :VOFctr and TPDprg = :VOFprg;
095600120312           exec sql
095700120312           DELETE from FILTRAGRPF/TIOPT00F
095800120312           WHERE TPTksc = :VOFnrv and TPTctr = :VOFctr and TPTprg = :VOFprg;
095900120312           exec sql
096000120312           DELETE from FILTRAGRPF/TIOFD00F
096100120312           WHERE TADksc = :VOFnrv and TADctr = :VOFctr and TADprg = :VOFprg;
096200120312           exec sql
096300120312           DELETE from FILTRAGRPF/TNOFM00F
096400120312           WHERE TAMksc = :VOFnrv and TAMctr = :VOFctr and TAMprg = :VOFprg;
096500120312         ELSE;
096600120312           exec sql
096700120312           DELETE from FILTRAGRU/TIOGC00F
096800120312           WHERE TGCksc = :VOFnrv and TGCctr = :VOFctr and TGCprg = :VOFprg;
096900120312           exec sql
097000120312           DELETE from FILTRAGRU/TIOPD00F
097100120312           WHERE TPDksc = :VOFnrv and TPDctr = :VOFctr and TPDprg = :VOFprg;
097200120312           exec sql
097300120312           DELETE from FILTRAGRU/TIOPT00F
097400120312           WHERE TPTksc = :VOFnrv and TPTctr = :VOFctr and TPTprg = :VOFprg;
097500120312           exec sql
097600120312           DELETE from FILTRAGRU/TIOFD00F
097700120312           WHERE TADksc = :VOFnrv and TADctr = :VOFctr and TADprg = :VOFprg;
097800120312           exec sql
097900120312           DELETE from FILTRAGRU/TNOFM00F
098000120312           WHERE TAMksc = :VOFnrv and TAMctr = :VOFctr and TAMprg = :VOFprg;
098100120312         ENDIF;
098200120305
098300120305       ENDSR;
098400120306
098500120306       //--------------------------------------------------------------
098600120306       //?Cancello l'offerta 999.
098700120306       //--------------------------------------------------------------
098800120306       BEGSR Mod_Off999;
098900120306
099000120306         wEoF = *off;
099100120306         setll C02nrv TIVOF01L;
099200120306         reade C02nrv TIVOF01L;
099300120306         DOW  not wEoF;
099400120306           IF  %eof(TIVOF01L);
099500120306             wEoF = *on;
099600120306             leave;
099700120306           ENDIF;
099800120306           IF  VOFctr = 999;
099900120306             exsr Del_VofSic;
100000120306             delete TIVOF01;
100100120306           ENDIF;
100200120306           reade C02nrv TIVOF01L;
100300120306         ENDDO;
100400120306
100500120306       ENDSR;
100600120306
100700120306       //--------------------------------------------------------------
100800120306       //?Deleto le offerte dal SIC.
100900120306       //--------------------------------------------------------------
101000120306       BEGSR Del_VofSic;
101100120306
101200120306       //?Cancello le offerte presenti sul SIC
101300120306         exec sql
101400120306         DELETE from TIVOFC0F
101500120306         WHERE VOCnrv = :VOFnrv and VOCctr = :VOFctr and VOCprg = :VOFprg;
101600120306
101700120306       ENDSR;
101800120305
101900110706       //--------------------------------------------------------------
102000120306       //?Modifico Sic.
102100110706       //--------------------------------------------------------------
102200120306       BEGSR Mod_Sic;
102300120130
102400120130         clear Tiket;
102500110706
102600120130       //?Recupero il tiket dalle info e cancello i dati del SIC delle info
102700110706         IF  %subst(knsif : 7 : 1) = 'P';
102800110706           exec sql
102900110706           SELECT VICtck into :Tiket
103000110706           FROM GAITRAAZP/TIVIIC0F
103100110706           WHERE VICnrv = :VISnrv;
103200110706
103300110706           exec sql
103400110706           DELETE from GAITRAAZP/TIVIIC0F
103500110706           WHERE VICnrv = :VISnrv;
103600110706         ELSE;
103700110706           exec sql
103800110706           SELECT VICtck into :Tiket
103900110706           FROM GAITRAAZM/TIVIIC0F
104000110706           WHERE VICnrv = :VISnrv;
104100110706
104200110706           exec sql
104300110706           DELETE from GAITRAAZM/TIVIIC0F
104400110706           WHERE VICnrv = :VISnrv;
104500110706         ENDIF;
104600120130
104700120130       //?Se non ho trovato il tiket sulle info (vuol dire che non c'erano info)
104800120130       //?lo recupero dalla trattativa
104900120130         IF  Tiket = *blanks;
105000120130           IF  %subst(knsif : 7 : 1) = 'P';
105100120130             exec sql
105200120130             SELECT VICtck into :Tiket
105300120130             FROM GAITRAAZP/TIVISC0F
105400120130             WHERE VICnrv = :VISnrv;
105500120130           ELSE;
105600120130             exec sql
105700120130             SELECT VICtck into :Tiket
105800120130             FROM GAITRAAZM/TIVISC0F
105900120130             WHERE VICnrv = :VISnrv;
106000120130           ENDIF;
106100120130         ENDIF;
106200110706
106300110706         clear TIVIIC00;
106400120130         wEoF    = *off;
106500120130         wOKinfo = *off;
106600110706
106700110926         wSQL = 'select * from TIVII00F ';
106800110706
106900110706         wSQL += 'where VIInrv = ' + %editc(VISnrv:'X') +
107000110706                   ' order by VIItpf';
107100110706
107200110706         exec sql prepare A1 from :wSQL;
107300110706
107400110706         exec sql declare VII cursor for A1;
107500110706
107600110706         exec sql open VII;
107700110706
107800110706         IF  sqlcode < 0;
107900120130           wEoF = *on;
108000110706         ENDIF;
108100110706
108200120130         DOW  not wEoF;
108300110706           exec sql fetch next from VII into :TIVIIDS;
108400110706           IF  sqlcod = 100 or sqlcod < 0;
108500120130             wEoF = *on;
108600110706             leave;
108700110706           ENDIF;
108800110706
108900110706       //?Imposto i dati per scrivere il record
109000120130           wOKinfo = *on;
109100110706           VICnrv = VIInrv;
109200110706
109300110706           SELECT;
109400110706         //?Fatturato/Spedizioni totale
109500110706             WHEN  VIItpf = 'TRT';
109600110706               VICfatt = VIIpft;
109700110706               VICnspt = VIIsna;
109800110706         //?Fatturato/Spedizioni estero
109900110706             WHEN  VIItpf = 'TRE';
110000110706               VICfate = VIIpft;
110100110706               VICnspe = VIIsna;
110200110706         //?Aumento/Sconto
110300110706             WHEN  VIItpf = 'A/S';
110400110706               VICausc = VIIvald;
110500110706         //?Delta
110600110706             WHEN  VIItpf = 'DEL';
110700110706               VICdel = VIIval;
110800110706         //?Peso
110900110706             WHEN  VIItpf = 'KMS';
111000110706               VICpkg = VIIval;
111100110706         //?Concorrenti
111200110706             WHEN  VIItpf = '10' and VIIspf = 'ARCO';
111300110706               VICarco = 'S';
111400110706             WHEN  VIItpf = '10' and VIIspf = 'ARTO';
111500110706               VICarto = 'S';
111600110706             WHEN  VIItpf = '10' and VIIspf = 'AWS';
111700110706               VICaws  = 'S';
111800110706             WHEN  VIItpf = '10' and VIIspf = 'BARS';
111900110706               VICbars = 'S';
112000110706             WHEN  VIItpf = '10' and VIIspf = 'DHL';
112100110706               VICdhl  = 'S';
112200110706             WHEN  VIItpf = '10' and VIIspf = 'EXEC';
112300110706               VICexec = 'S';
112400110706             WHEN  VIItpf = '10' and VIIspf = 'FERC';
112500110706               VICferc = 'S';
112600110706             WHEN  VIItpf = '10' and VIIspf = 'MAIL';
112700110706               VICmail = 'S';
112800110706             WHEN  VIItpf = '10' and VIIspf = 'MEP';
112900110706               VICmep  = 'S';
113000110706             WHEN  VIItpf = '10' and VIIspf = 'MTN';
113100110706               VICmtn  = 'S';
113200110706             WHEN  VIItpf = '10' and VIIspf = 'ONEE';
113300110706               VIConee = 'S';
113400110706             WHEN  VIItpf = '10' and VIIspf = 'PALL';
113500110706               VICpall = 'S';
113600110706             WHEN  VIItpf = '10' and VIIspf = 'PALW';
113700110706               VICpalw = 'S';
113800110706             WHEN  VIItpf = '10' and VIIspf = 'SDA';
113900110706               VICsda  = 'S';
114000110706             WHEN  VIItpf = '10' and VIIspf = 'SUSA';
114100110706               VICsusa = 'S';
114200110706             WHEN  VIItpf = '10' and VIIspf = 'TNT';
114300110706               VICtnt  = 'S';
114400110706             WHEN  VIItpf = '10' and VIIspf = 'UPS';
114500110706               VICups  = 'S';
114600110706             WHEN  VIItpf = '10' and VIIspf = '9910';
114700110706               VICaltr = 'S';
114800110706             WHEN  VIItpf = '10' and VIIspf = '8810';
114900110706               VICness = 'S';
115000110706           ENDSL;
115100110706         ENDDO;
115200110706
115300110706       //?Scrivo il rcd delle info commerciali
115400120130         IF  wOKinfo;
115500110706           VICtck = Tiket;
115600110706           write  TIVIIC00;
115700111102           feod TIVIIC0F;
115800110706         ENDIF;
115900110706
116000110706         exec sql close VII;
116100120130
116200120130       //?Devo aggiornare anche la trattativa sul SIC
116300120130         IF  %subst(knsif : 7 : 1) = 'P';
116400120130           exec sql
116500120130           UPDATE GAITRAAZP/TIVISC0F
116600120130           SET VICtpv = :VIStpv
116700120130           WHERE VICnrv = :VISnrv;
116800120130         ELSE;
116900120130           exec sql
117000120130           UPDATE GAITRAAZM/TIVISC0F
117100120130           SET VICtpv = :VIStpv
117200120130           WHERE VICnrv = :VISnrv;
117300120130         ENDIF;
117400110706
117500110706       ENDSR;
117600120130
117700120130       //--------------------------------------------------------------
117800120130       //?Scrivo note per promemoria.
117900120130       //--------------------------------------------------------------
118000120130       BEGSR Wrt_Note;
118100120130
118200120130         clear TRMK20ds;
118300120130         IMK20tla = 'L';
118400120130         IMK20flm = 'C';
118500120130         IMK20fno = 'S';
118600120130         IMK20cpo = VIScpo;
118700120130         IF  C02ksc > 0;
118800120130           IMK20ksc = C02ksc;
118900120130         ENDIF;
119000120130         IMK20nrv = VISnrv;
119100120130         EMK20no1 = 'Trattativa variata da ' +
119200120130                     old_VIStpv + ' a ' + VIStpv +
119300120313                    '. Variazione eseguita da ' + knmus;
119400120130         trmk20r ( kpjba : trmk20ds );
119500120130
119600120130       ENDSR;
119700090807
119800080206       //--------------------------------------------------------------
119900080206       //?Operazioni finali.
120000080206       //--------------------------------------------------------------
120100080206       BEGSR RoutEnd;
120200090521
120300080206         *inLR = *on;
120400080206         return;
120500080206
120600080206       ENDSR;
120700080206
120800080206      /end-free
120900090807
121000080206       //--------------------------------------------------------------
121100080206       //?Schiere a tempo di compilazione.
121200080206       //--------------------------------------------------------------
121300080206
121400120130** - w_MSG -------------------------------------------------------------------*
121500110706Immettere la trattativa da variare                                             1
121600110706Trattativa inesistente                                                         2
121700110930Trattativa ancora aperta                                                       3
121800120130Trattativa non di tipo "A" o "I"                                               4
121900120130Il tipo trattativa non è stato variato                                         5
122000120305Modifica tipo trattativa non possibile                                         6
122100120305Tipo trattativa errato                                                         7
122200120706La trattativa è stata chiusa oggi, modificarla domani!!!                       8
