000100130809     /*END
000200090610      //--------------------------------------------------------------
000300091201      //?TRMK22R - AFFIANCAMENTO ATTIVITA' COMMERCIALE
000400090610      //--------------------------------------------------------------
000500090610
000600091203     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000700090610     h dftactgrp(*no) actgrp(*caller)
000800090610
000900090610      //---------------------------------------------------------------
001000090610      //?Dichiarazione file.
001100090610      //---------------------------------------------------------------
001200090610
001300091201      // - Attività commerciale
001400091215     fTIATC01L  uf a e           k disk    commit(IMK22fcmt) usropn
001500091203
001600091203      // - Legame attività commerciale
001700091215     fTIATL01L  uf a e           k disk    commit(IMK22fcmt) usropn
001800110922     fTIATL02L  if   e           k disk    rename(TIATL000:TIATL02)
001900091202
002000130809      // -?Anagrafica Commerciali?
002100130809     fAZCMM01L  if   e           k disk
002200110223
002300110223      // - Anagrafica potenziale
002400110223     fTNCPO01L  if   e           k disk
002500090610
002600091201      // - Video
002700091201     fTRMK22D   cf   e             workstn
002800091201     f                                     sfile(mk22S01 : S01nrr)
002900090610     f                                     indds(IndDspF)
003000090610     f                                     infds(InfDspF)
003100090610     f                                     usropn
003200090610
003300090610      //---------------------------------------------------------------
003400090610      //?Definizione costanti.
003500090610      //---------------------------------------------------------------
003600090610
003700090610      // - Tasti funzionali a video
003800090610     d c_F01           c                   const(x'31')
003900090610     d c_F02           c                   const(x'32')
004000090610     d c_F03           c                   const(x'33')
004100090610     d c_F04           c                   const(x'34')
004200090610     d c_F05           c                   const(x'35')
004300090610     d c_F06           c                   const(x'36')
004400090610     d c_F07           c                   const(x'37')
004500090610     d c_F08           c                   const(x'38')
004600090610     d c_F09           c                   const(x'39')
004700090610     d c_F10           c                   const(x'3A')
004800090610     d c_F11           c                   const(x'3B')
004900090610     d c_F12           c                   const(x'3C')
005000090610     d c_F13           c                   const(x'B1')
005100090610     d c_F14           c                   const(x'B2')
005200090610     d c_F15           c                   const(x'B3')
005300090610     d c_F16           c                   const(x'B4')
005400090610     d c_F17           c                   const(x'B5')
005500090610     d c_F18           c                   const(x'B6')
005600090610     d c_F19           c                   const(x'B7')
005700090610     d c_F20           c                   const(x'B8')
005800090610     d c_F21           c                   const(x'B9')
005900090610     d c_F22           c                   const(x'BA')
006000090610     d c_F23           c                   const(x'BB')
006100090610     d c_F24           c                   const(x'BC')
006200090610     d c_Enter         c                   const(x'F1')
006300091201     d c_RollDown      c                   const(x'F4')
006400091201     d c_RollUp        c                   const(x'F5')
006500091230
006600091230     d Digits          c                   const('0123456789')
006700090610
006800090610      //---------------------------------------------------------------
006900090610      //?Definizione schiere.
007000090610      //---------------------------------------------------------------
007100090610
007200091202      // - Agenti
007300091202     d $Age            s              7  0 dim(05)
007400091202
007500090610      // - Messaggi di errore
007600091203     d $Msg            s             78    dim(20) ctdata perrcd(1)
007700090610
007800090610      //---------------------------------------------------------------
007900090610      //?Definizione aree dati.
008000090610      //---------------------------------------------------------------
008100090610
008200090610      // - Dati utente
008300090610     d §AzUte        e ds                  extname(AZUTE00F)
008400090610     d                                     dtaara
008500090610     d §DatiUte      e ds                  extname(dDatiUte)
008600090610     d                                     dtaara
008700090610
008800090610      //---------------------------------------------------------------
008900090610      //?Definizione strutture dati.
009000090610      //---------------------------------------------------------------
009100090610
009200090610      // - InfDS
009300090610     d InfDspF         ds
009400090610     d  dsp_aid              369    369a                                        AID byte
009500130809     d*//dsp_rig             370    370
009600130809     d*//dsp_col             371    371
009700090610
009800090610      // - Indicatori su DspF
009900090610     d IndDspF         ds
010000091201        // - Indicatori di gestione del subfile
010100091201     d  SflDsp_N                      1n   overlay(IndDspF : 30)
010200091201     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
010300091201     d  SflEnd                        1n   overlay(IndDspF : 32)
010400090929        // - Indicatori per l'utilizzo del file in formato *DS4
010500090929     d  FmtDspF_DS4                    n   overlay(IndDspF : 04)
010600100412        // - Indicatore di modifica
010700100412     d  Modifica                       n   overlay(IndDspF : 02)
010800091204        // - Indicatore di interrogazione
010900091203     d  Interroga                      n   overlay(IndDspF : 05)
011000091204        // - Indicatore di protezione campi
011100091204     d  Proteggi                       n   overlay(IndDspF : 06)
011200100224        // - Indicatore di protezione campi per annullamento
011300100224     d  Annulla                        n   overlay(IndDspF : 07)
011400091201        // - Indicatori di errore
011500091201     d  ErrMessage                    1n   overlay(IndDspF : 28)
011600091202     d  PosCurCMM                     1n   overlay(IndDspF : 50)
011700091202     d  PosCurHDA                     1n   overlay(IndDspF : 51)
011800091202     d  PosCurOII                     1n   overlay(IndDspF : 52)
011900091202     d  PosCurOFI                     1n   overlay(IndDspF : 53)
012000091203     d  PosCurOPZ                     1n   overlay(IndDspF : 54)
012100090610        // - Indicatori di errore generico
012200091201     d  ErrGenerico                   1n   overlay(IndDspF : 99)
012300090610
012400091201      // Campi di comodo per gestione indicatori a video
012500090610     d WindDspF        ds                  inz
012600090610     d   WindDspFa             1     49    inz(*zero)
012700090610     d   WindDspFb            50     99    inz(*zero)
012800090610
012900090610      // - Parametri ricevuti
013000090610     d KPJBA         e ds
013100091201     d TRMK22DS      e ds
013200091203
013300091203      // - Ricerca/Controllo tabelle
013400091203     d TIBS02ds      e ds                  inz
013500091203     d   T02Mod      e                     inz('C')
013600090610
013700090610      // - Reperimento dati utente
013800090610     d TIBS34ds      e ds
013900090610     d dUte01        e ds
014000091203
014100091203      // - Agenda commerciale
014200091203     d TRMK82ds      e ds
014300091217
014400091217      // -  Controllo se commerciale impegnato
014500091217     d TRMK84ds      e ds
014600091201
014700091201      // - Reperimento filiali
014800091201     d TRUL31DS      e ds
014900090610
015000090610      // - Tabella CCO = Causale contatto
015100090610     d dcco          e ds                  inz
015200090610
015300090610      // - Tabella LAT = Autorizzazione
015400090610     d dlat          e ds                  inz
015500091203
015600091203      // - file Attività Commerciali
015700091203     d Tiatcds       e ds                  extname(TIATC00F)
015800091203      // - Ds di comodo per file Attività Commerciali
015900091203     d ds_Tiatc      e ds                  extname(TIATC00F)
016000091203     d                                     prefix(ds_)
016100110223
016200110223      // - Dati di ATCFLO del file TIATC00F
016300110223     d dATC01        e ds
016400090610
016500090610      //---------------------------------------------------------------
016600090610      //?Definizione variabili globali.
016700090610      //---------------------------------------------------------------
016800090610
016900090610      // - Flags booleani
017000091203     d $EoF            s               n   inz(*off)
017100090610     d $Fine           s               n   inz(*off)
017200090611     d $InzW01         s               n   inz(*on)
017300091203     d $NoAge          s               n   inz(*on)
017400090610
017500090610      // - Indici di schiera
017600090610     d xx              s              3  0 inz
017700091203     d yy              s              3  0 inz
017800090610
017900090610      // - Campi associati al video
018000090611     d $Video          s              2    inz('W1')
018100091201     d S01Nrr          s              4  0 inz
018200090610
018300090610     d dsp_mod         s             10I 0
018400090610
018500090610      // - Campi di comodo data
018600090610     d  data_eur       s               d   Datfmt(*eur)
018700090610     d  data_iso       s               d   Datfmt(*iso)
018800090610
018900090610      // - Campi di comodo
019000091204     d sav_atcatnp     s                   like(atcatnp)
019100090610     d wAbi            s                   like(UTEaut)  inz
019200091203     d wColo           s              3s 0
019300111128     d wOggi           s              8s 0
019400091203     d wRiga           s              3s 0
019500090610
019600090610      //---------------------------------------------------------------
019700091203      //?Definizione procedure esterne.
019800090610      //---------------------------------------------------------------
019900091203
020000091203      // - Agenda
020100091203     d Trmk82r         pr                  extpgm('TRMK82R')
020200091203     d  kpjba                              likeds(kpjba)
020300091217
020400091217      // - Controllo agenda
020500091217     d Trmk84r         pr                  extpgm('TRMK84R')
020600091217     d  trmk84ds                           likeds(trmk84ds)
020700090610
020800090610      /copy gaitrasrc/srcprotopr,tibs02r
020900090610      /copy gaitrasrc/srcprotopr,tibs34r
021000130809      /copy gaitrasrc/srcprotoPI,TRMK43R_1
021100130809      /copy gaitrasrc/srcprotoPR,TRMK43R
021200090610      /copy gaitrasrc/srcprotopr,trul31r
021300090611      /copy gaitrasrc/srcprotopr,QsnQryModS
021400090610
021500090610      //---------------------------------------------------------------
021600090610      //?Definizione key-list.
021700090610      //---------------------------------------------------------------
021800090610
021900090610
022000090610      //---------------------------------------------------------------
022100090610      //?Riepilogo indicatori.
022200090610      //---------------------------------------------------------------
022300090610      // 28    : Emissione messaggio di errore a video
022400091201      // 30    : Condiziona SFLDSP
022500091201      // 31    : Condiziona SFLDSPCTL
022600091201      // 32    : Condiziona SFLEND
022700090610      // 99    : Generico di Errore
022800090610      //---------------------------------------------------------------
022900090610
023000090610      //---------------------------------------------------------------
023100090610      //?M A I N - L I N E
023200090610      //---------------------------------------------------------------
023300090610
023400090610     c     *Entry        plist
023500090610     c                   parm                    KPJBA
023600091201     c                   parm                    trmk22ds
023700090610
023800090610      /free
023900090610
024000091201       //?Operazioni iniziali controllo se video accetta 132 caratteri
024100091201       if not %open(trmk22d);
024200090611           dsp_mod = QueryDisplayModeSupport('4':*omit :*omit :*omit);
024300090611           If dsp_mod <> 0 ;
024400091201              open trmk22d;
024500090611           else ;
024600091203              $Fine = *on;
024700091210              ErrGenerico = *on;
024800091210              WC1msg = 'Videata errata';
024900090611           endif;
025000090611       endif;
025100090610
025200091201       //?Operazioni iniziali
025300091201       exsr sr_RoutInz;
025400090611
025500091201       //?Gestione video
025600091203       DOW Not $Fine;
025700090610         select;
025800090611           when $Video = 'W1';
025900090611             exsr GesW01;
026000090610           other;
026100090610             $Fine = *on;
026200090610         endsl;
026300090610       ENDDO;
026400090610
026500091201       //?Operazioni finali
026600091201       exsr sr_RoutEnd;
026700091201
026800091201       //--------------------------------------------------------------
026900091201       //?Operazioni iniziali.
027000091201       //--------------------------------------------------------------
027100091201       BEGSR sr_RoutInz;
027200111128
027300111128         //?Data del giorno
027400111128         wOggi = %dec(%date());
027500091201
027600091201         //?Reperimento dati job
027700091201         exsr DatiJob;
027800091201
027900091201         //?Verifica se visualizzare la finestra a 80 o 132 colonne:
028000091201         //?  solo se ricevuto il carattere "4" nella ds TRMK22 campo
028100091201         //?  del tipo formato
028200091203         FmtDspF_DS4 = (IMK22fmt = '4');
028300100412
028400100412         //?Se richiamato in modifica
028500100412         Modifica = (IMK22fle = '2');
028600091203
028700091203         //?Se richiamato in interrogazione proteggo tutto
028800091203         Interroga = (IMK22fle = '5');
028900091204
029000091204         //?Se richiamato in gestione proteggo tutto tranne la scelta
029100091204         Proteggi = (IMK22fle = 'G');
029200100224
029300100224         //?Se richiamato in annullamento proteggo tutto e imposto la scelta 'A'
029400100224         Annulla = (IMK22fle = 'D');
029500091201
029600091201         //?Verifica errori e autorità profilo
029700091201         clear wAbi;
029800091201         clear dLAT;
029900091201         select;
030000091201         //?- Se ho errori nei dati utente esco dal pgm
030100091201           when  DUTerr = 'E';
030200091201             $Fine = *on;
030300091210             ErrGenerico = *on;
030400091210             WC1msg = $Msg(01);
030500091210             leavesr;
030600091201         //?- Se non c'è l'abilitazione:
030700091201         //?  - se 1° livello, abilitazioni al terminal
030800091201         //?  - se 2° livello, abilitazioni al punto operativo
030900091201         //?  - se sede è impossibile ma se capita mando a fine il pgm
031000091201           when  UTEaut = *blank;
031100091201             select;
031200091201               when  DUTlpo = '1';
031300091201                 wAbi  = 'TP';
031400091201               when  DUTlpo = '2';
031500091201                 wAbi  = 'PO';
031600091201               when  DUTlpo = 'S';
031700091201                 $Fine = *on;
031800091210                 ErrGenerico = *on;
031900091210                 WC1msg = $Msg(01);
032000091210                 leavesr;
032100091201             endsl;
032200091201         //?- Altrimenti si caricano le abilitazioni del profilo
032300091201           other;
032400091201             dUTE01 = UTEfaf;
032500091201             if  §UTEpot <> *blank;
032600091201               wAbi = §UTEpot;
032700091201             else;
032800091201               wAbi = UTEaut;
032900091201             endif;
033000091201         ENDSL;
033100091201
033200091201         //?Controllo se ok l'abilitazione dell'utente
033300091201         reset TIBS02ds;
033400091201         T02sif = knsif;
033500091201         T02cod = 'LAT';
033600091201         T02ke1 = wAbi;
033700091201         TNTBE_RicercaControllo  (kpjba : tibs02ds);
033800091201
033900091201         if  T02err  = *blank;
034000091201           dLAT = T02uni;
034100091201         endif;
034200091201
034300091201         //?Errore o utente non abilitato
034400091201         if  T02err <> *blanks   or
034500091201             §LATabi = 'S';
034600091210           $Fine = *on;
034700091210           ErrGenerico = *on;
034800091201           WC1msg = $Msg(01);
034900091201           leavesr;
035000091201         endif;
035100091201
035200091201         //?Carico le filiali in base alle abilitazioni
035300091201         clear trul31ds;
035400091201         i31abi = wabi;
035500091201         I31cdi = DUTdis;
035600091201         I31car = DUTare;
035700091201         I31cpo = DUTpou;
035800091201         callp TRUL31R (kpjba:trul31ds);
035900091201         if o31pog<=*zeros;
036000091210           $Fine = *on;
036100091210           ErrGenerico = *on;
036200091201           WC1msg = $Msg(01);
036300091201           leavesr;
036400091201         endif;
036500091215
036600091215         //?Flag commit
036700091215         IF IMK22fcmt <> '1';
036800091215           IMK22fcmt = '0';
036900091215           IMK22cmt = 'N';
037000091215         ENDIF;
037100091201
037200091215         //?Apro file
037300091201         open tiatc01l;
037400091203         open tiatl01l;
037500091215
037600091215         //?Controllo congruenza flag commit
037700091215         //?se apertura file sotto commit si deve specificare se è questo pgm
037800091215         //?a dover fare il commit o no
037900091215         IF  IMK22fcmt = '1' and IMK22cmt <> 'N' and IMK22cmt <> 'S';
038000091215           $Fine = *on;
038100091215           ErrGenerico = *on;
038200091215           WC1msg = $Msg(11);
038300091215           leavesr;
038400091215         ENDIF;
038500091203
038600091203         //?Controllo se esiste l'appuntamento passato
038700091203         chain(n) (IMK22tat : IMK22atn : IMK22atnp) TIATC01L;
038800091203         IF  Not %found(TIATC01L);
038900091210           $Fine = *on;
039000091210           ErrGenerico = *on;
039100091203           WC1msg = $Msg(09);
039200091203           leavesr;
039300091203         ENDIF;
039400100310
039500091203         //?Salvo dati appuntamento passato
039600091203         ds_tiatc = tiatcds;
039700100310
039800100310         //?Aggancio tabella CCO per recupero flag di azione eseguibile da comm.Interno
039900100310         //?CHIODO attività 92
040000100310         clear dCCO;
040100100310         reset TIBS02DS;
040200100310         T02sif = knsif;
040300100310         T02cod = 'CCO';
040400100310         T02ke1 = '92';
040500100310         TNTBE_RicercaControllo  (kpjba : tibs02ds);
040600100310         if  T02err  = *blank;
040700100310           dCCO = T02uni;
040800100310         endif;
040900091201
041000091201       ENDSR;
041100091201
041200091201       //--------------------------------------------------------------
041300091201       //?Reperimento Dati del job (Utente/Operativi).
041400091201       //--------------------------------------------------------------
041500091201       BEGSR DatiJob;
041600091201
041700091201         in(E) §AzUte;
041800091201         if NOT %error;
041900091201           in(E) §DatiUte;
042000091201         endif;
042100091201         if %error or RSut = *blanks;
042200091201           clear TIBS34ds;
042300091201           tibs34r(tibs34ds);
042400091201           in §AzUte;
042500091201           in §DatiUte;
042600091201         endif;
042700091201
042800091201       ENDSR;
042900090610
043000090610       //--------------------------------------------------------------
043100090611       //?Gestione videata W01
043200090610       //--------------------------------------------------------------
043300090611       BEGSR GesW01;
043400090610
043500091201       //?Inizializzazione videata
043600091203         if $InzW01;
043700090611           exsr InzW01;
043800090611           $InzW01 = *off;
043900090610         endif;
044000091203
044100091203         $Eof   = *off;
044200090610
044300091201       //?Emissione videata
044400091201         sfldsp_n = *off;
044500091202         WC1rcd = 1;
044600091202         write MK22Z01;
044700091201         exfmt MK22C01;
044800091202         ErrMessage   = *off;
044900091202         ErrGenerico  = *off;
045000091201         clear WC1msg;
045100090610
045200091201         SELECT;
045300091202
045400091201       //?F12 = Ritorno
045500091201           WHEN dsp_aid = c_F12;
045600091201             exsr sr_F12W01;
045700090611             leavesr ;
045800091201
045900091201       //?F11 = Agenda
046000091201           WHEN dsp_aid = c_F11;
046100091201             exsr sr_F11W01;
046200091201
046300091201       //?F06 = Conferma
046400091201           WHEN dsp_aid = c_F06;
046500091203             //?prima faccio i controlli
046600091203             exsr CtrW01;
046700091203             IF  ErrGenerico;
046800091203               leavesr;
046900091203             ENDIF;
047000091203             //?tutto ok confermo i dati
047100091201             exsr sr_F06W01;
047200091203             $Fine  = *on;
047300090611
047400091201       //?Enter
047500091201           OTHER;
047600091201       //?Controllo i dati immessi nel subfile
047700091203            IF  Not Interroga;
047800091203              exsr CtrW01;
047900091203              IF  ErrGenerico;
048000091203                leavesr;
048100091203              ENDIF;
048200091201            ENDIF;
048300091201
048400091201         ENDSL;
048500090610
048600090610       ENDSR;
048700090610
048800090610       //--------------------------------------------------------------
048900090611       //?Caricamento videata telefonata W01
049000090610       //--------------------------------------------------------------
049100090611       BEGSR Inzw01;
049200090610
049300091201         //?Pulizia subfile
049400091201         exsr Sr_PulSfl;
049500091201
049600091201         //?imposto i dati ricevuti
049700091203         //?Commerciale
049800091201         WC1cmm = IMK22cmm;
049900091203         //?decodifico commerciale
050000091201         clear WC1dcmm;
050100130809         chain (WC1cmm) AZCMM000;
050200130809         IF  %found(AZCMM01L);
050300130809           WC1dcmm = CMMdes;
050400091201         ENDIF;
050500091201
050600091203         //?Data attività
050700091211         data_iso = (%date(IMK22dad));
050800091211         data_eur = data_iso;
050900091211         WC1dad = %dec(data_eur);
051000091201
051100091203         //?Ora attività
051200091211         WC1hda = IMK22hda / 100;
051300091201
051400091203         //?Ora inizio/fine
051500091201         WC1oii = IMK22oii;
051600091201         WC1ofi = IMK22ofi;
051700091201
051800091201         //?Carico subfile
051900091202         exsr sr_CarW01;
052000091201
052100090610       ENDSR;
052200091201
052300091201       //--------------------------------------------------------------
052400091201       //?Pulisco subfile.
052500091201       //--------------------------------------------------------------
052600091201       BEGSR sr_PulSfl;
052700091201
052800091201         SflDsp_N    = *on;
052900091201         SflDspCtl_N = *on;
053000091201         write  MK22C01;
053100091201         SflDspCtl_N = *off;
053200091201         SflEnd      = *off;
053300091201
053400091201         clear WC1rcd;
053500091201         clear WC1msg;
053600091202         ErrMessage  = *off;
053700091202         ErrGenerico = *off;
053800091201         clear S01Nrr;
053900091201
054000091201       ENDSR;
054100091201
054200091201       //--------------------------------------------------------------
054300091201       //?Carico il subfile.
054400091201       //--------------------------------------------------------------
054500091202       BEGSR sr_CarW01;
054600091202
054700091202         //?Pulisco sk di comodo commerciali
054800091202         clear $AGE;
054900091201
055000091201         //?Carico subfile in base al tipo elaborazione
055100091201         SELECT;
055200091201
055300091201         //?Vuoto se sono in Immissione Affiancamento
055400091201           WHEN  IMK22fle = 'I';
055500091201
055600091202           xx = 1;
055700091202           FOR xx by 1 to %elem($Age);
055800091201             clear MK22S01;
055900091201             S01Nrr += 1;
056000091201             write MK22S01;
056100091201           ENDFOR;
056200091204
056300091204         //?mi salvo il progressivo
056400091204            sav_atcatnp = IMK22atnp;
056500091201
056600091201         //?Pieno se NON sono in Immissione
056700091201           OTHER;
056800110922
056900110922           //?Leggo il file legami con il n. attività dell'appuntamento
057000110922             setll (IMK22atn:IMK22atnp) TIATL02L;
057100110922             reade (IMK22atn:IMK22atnp) TIATL02L;
057200110922             DOW not %eof(TIATL02L);
057300110922             //?Aggangio l'attività di affiancamento
057400110922               chain(n) ('A':ATLatn:ATLatnp) TIATC01L;
057500110922               IF  %found(TIATC01L);
057600091201
057700091203                //?Dati key non visualizzati
057800091203                WH1tat  = atctat;
057900091203                WH1atn  = atcatn;
058000091203                WH1atnp = atcatnp;
058100091204                //?salvo il progressivo
058200091204                sav_atcatnp = atcatnp;
058300091203                //?Commerciale
058400091202                WS1cmm = %editc(atccmm:'X');
058500100310                WH1savcmm = WS1cmm;
058600091203                //?decodifico commerciale
058700091201                clear WS1dcmm;
058800130809                chain (ATCcmm) AZCMM000;
058900130809                IF  %found(AZCMM01L);
059000130809                  WS1dcmm = CMMdes;
059100091201                ENDIF;
059200091201
059300091203                //?Ora attività
059400091211                WS1hda = atchda / 100;
059500091201
059600091203                //?Ora inizio/fine
059700091201                WS1oii = atcoii;
059800091201                WS1ofi = atcofi;
059900091216
060000091216                //?in visualizzazione faccio vedere se esito attività negativo
060100091217                IF  atccac = 'NO';
060200091216                  WS1ese = 'N';
060300091216                ENDIF;
060400100224
060500100224                //?se annullamento imposto opzione 'A'
060600100224                IF  Annulla;
060700100224                  WS1sce = 'A';
060800100224                ENDIF;
060900091201
061000091201                S01Nrr += 1;
061100091202
061200091203                //?Controllo di non superare il n. max di elementi per il sbfl e per la
061300091203                //?schiera di comodo, non dovrebbe capitare, ma così sono + tranquilla
061400091202                IF  S01Nrr > 5;
061500091202                  leave;
061600091202                ENDIF;
061700110922
061800091201                write MK22S01;
061900091202
062000091202                //?Incremento la schiera di comodo commerciali
062100091202                $AGE(S01Nrr) = %dec(WS1cmm:7:0);
062200110922               ENDIF;
062300110922
062400110922               reade (IMK22atn:IMK22atnp) TIATL02L;
062500110922             ENDDO;
062600091201
062700091201         ENDSL;
062800091203
062900091203         //?Riempo il subfile di righe vuote
063000091203         //?se richiamato in modifica "2"
063100091203         IF  S01Nrr < %elem($Age) and IMK22fle = '2';
063200100224          //?se non ho caricato nessuna riga mi salvo il progressivo
063300100224            IF S01Nrr = 0;
063400100224              sav_atcatnp = atcatnp;
063500100224            ENDIF;
063600091203           yy = %elem($Age) - S01Nrr;
063700091203           xx = 1;
063800091203           FOR xx by 1 to yy;
063900091203             clear MK22S01;
064000091203             S01Nrr += 1;
064100091203             write MK22S01;
064200091203           ENDFOR;
064300091203         ENDIF;
064400091203
064500091203         SflEnd = *on;
064600091201
064700091201       ENDSR;
064800090610
064900090611       //--------------------------------------------------------------
065000090612       //?Controllo videata W01
065100090611       //--------------------------------------------------------------
065200091202       BEGSR CtrW01;
065300090611
065400091202         WindDspF = IndDspF;
065500091202         reset WindDspFb;
065600091202         IndDspF  = WindDspF;
065700091202
065800091202         clear S01Nrr;
065900091203
066000091203         //?Pulisco sk di comodo commerciali
066100091203         clear $AGE;
066200091202
066300091202         DOW  not $EoF;
066400091202           S01Nrr += 1;
066500091202           chain S01Nrr MK22S01;
066600091202           IF  not %found;
066700091202             $EoF = *on;
066800091202             leave;
066900091202           ENDIF;
067000091203
067100091204         //?Opzione 'A' non l'accetto se:
067200091204         //?--> sono in immissione o se non ho il commerciale
067300091204         //?--> non ho il commerciale
067400091204         //?--> non ho il n. di attività (nuovo rcd nel subfile)
067500091204           IF  WS1sce = 'A' and
067600100113              (IMK22fle = 'I' or WH1atn = 0  or
067700100113               WS1cmm = *blanks or WS1cmm = *zeros);
067800091203             ErrMessage  = *on;
067900091203             ErrGenerico = *on;
068000091203             PosCurOPZ   = *on;
068100091203             WC1msg      = $Msg(10);
068200091203             WC1Rcd      = S01Nrr;
068300091203             update MK22S01;
068400091203             $EoF = *on;
068500091203             leave;
068600091203           ENDIF;
068700091202
068800091202         //?Se non c'è il codice commerciale ritorno a leggere il subfile
068900100113           IF  WS1cmm = *blanks;
069000091202             iter;
069100091202           ENDIF;
069200100113         //?Se il codice commerciale è a 0 ripulisco i campi e ritorno a leggere
069300100113         //?il subfile
069400100113           IF  WS1cmm = *zeros;
069500100310             clear WH1fimp;
069600100310             clear WH1fint;
069700100310             clear WH1savcmm;
069800100113             clear WS1cmm;
069900100113             clear WS1dcmm;
070000100113             clear WS1hda;
070100100113             clear WS1oii;
070200100113             clear WS1ofi;
070300100113             WC1Rcd = S01Nrr;
070400100113             update MK22S01;
070500100113             iter;
070600100113           ENDIF;
070700100310
070800100310            IF  WS1cmm <> WH1savcmm;
070900100310              clear WH1fimp;
071000100310              clear WH1fint;
071100100310            ENDIF;
071200090611
071300091202         //?Codice Commerciale
071400091202           // --> Ricerca
071500091202           IF  %scan('?':WS1cmm) > 0;
071600091202             ErrGenerico = *on;
071700091202             PosCurCMM   = *on;
071800130809             clear  TRMK43ds;
071900130809             iMK43fil  = (WC1cmm / 10000);
072000130809             TRMK43R (kpjba : TRMK43ds);
072100130809             if  oMK43err = *off  and  oMK43fxx = *blank;
072200130809               WS1cmm  = %editc( oMK43cmm : 'X' );
072300130809               WS1dcmm = oMK43des;
072400130809             endif;
072500091202             WC1Rcd  = S01Nrr;
072600091202             update MK22S01;
072700091202             $EoF = *on;
072800091202             leave;
072900091202           ENDIF;
073000091202
073100091202           // --> Controllo
073200091202           IF  WS1cmm > *zeros;
073300130809             // solo numeri
073400130809             IF  %check(digits:WS1cmm) > 0;
073500130809               ErrMessage  = *on;
073600130809               ErrGenerico = *on;
073700130809               PosCurCMM   = *on;
073800130809               WC1msg      = $Msg(02);
073900130809               WC1Rcd      = S01Nrr;
074000130809               update MK22S01;
074100130809               $EoF = *on;
074200130809               leave;
074300130809             ENDIF;
074400100310             // non deve essere uguale al commerciale dell'appuntamento principale
074500100310             IF  WS1cmm = %editc(WC1cmm:'X');
074600100202               ErrMessage  = *on;
074700100202               ErrGenerico = *on;
074800100202               PosCurCMM   = *on;
074900100202               WC1msg      = $Msg(13);
075000100202               WC1Rcd      = S01Nrr;
075100100202               update MK22S01;
075200100202               $EoF = *on;
075300100202               leave;
075400100202             ENDIF;
075500100310             // controllo con tabella agenti
075600091202             clear WS1dcmm;
075700130809             CMMcod = %int(WS1cmm);
075800130809             chain (CMMcod) AZCMM000;
075900130809             IF  not %found(AZCMM01L);
076000091202               ErrMessage  = *on;
076100091202               ErrGenerico = *on;
076200091202               PosCurCMM   = *on;
076300091203               WC1msg      = $Msg(02);
076400091203               WC1Rcd      = S01Nrr;
076500091202               update MK22S01;
076600091202               $EoF = *on;
076700091202               leave;
076800091202             ELSE;
076900130809               WS1dcmm = CMMdes;
077000091202             ENDIF;
077100110518             // no commerciali VARI e INATTIVI
077200130809             IF  CMMpar <> *blanks;
077300110518               ErrMessage  = *on;
077400110518               ErrGenerico = *on;
077500110518               PosCurCMM   = *on;
077600110518               WC1msg      = $Msg(02);
077700110518               WC1Rcd      = S01Nrr;
077800110518               update MK22S01;
077900110518               $EoF = *on;
078000110518               leave;
078100110518             ENDIF;
078200111128             // commerciale valido in base alla data inizio e fine attività
078300130809             IF  CMMdtINI > wOggi  or
078400130809                 CMMdtFin < wOggi;
078500111128               ErrMessage  = *on;
078600111128               ErrGenerico = *on;
078700111128               PosCurCMM   = *on;
078800111128               WC1msg      = $Msg(02);
078900111128               WC1Rcd      = S01Nrr;
079000111128               update MK22S01;
079100111128               $EoF = *on;
079200111128               leave;
079300111128             ENDIF;
079400100310             // se commerciale interno (funzione aziendale COMIN) controllo con flag
079500100310             // tabella CCO se si può fare l'attività
079600130809             IF  CMMfun = 'COMIN' or CMMfun = 'ASC' or CMMfun = 'SA';
079700100310               SELECT;
079800100319                 WHEN  WH1fint = *blanks and §CCOcoi = 'F';
079900100310                   ErrMessage  = *on;
080000100310                   ErrGenerico = *on;
080100100310                   PosCurCMM   = *on;
080200100310                   WC1msg      = $Msg(14);
080300100310                   WC1msg      = %trim(WC1msg) + ' Enter per forzare';
080400100310                   WC1Rcd      = S01Nrr;
080500100310                   WH1fint     = '1';
080600100310                   WH1savcmm   = WS1cmm;
080700100310                   update MK22S01;
080800100310                   $EoF = *on;
080900100310                   leave;
081000100319                 WHEN  §CCOcoi = 'N';
081100100310                   ErrMessage  = *on;
081200100310                   ErrGenerico = *on;
081300100310                   PosCurCMM   = *on;
081400100310                   WC1msg      = $Msg(14);
081500100310                   WC1Rcd      = S01Nrr;
081600100310                   update MK22S01;
081700100310                   $EoF = *on;
081800100310                   leave;
081900100310               ENDSL;
082000100310             ENDIF;
082100091202           ENDIF;
082200091203
082300091203         //?Incremento la schiera di comodo commerciali
082400100113           xx = %lookup(%dec(WS1cmm:7:0) : $AGE);
082500100113           IF  xx > 0;
082600100113             ErrMessage  = *on;
082700100113             ErrGenerico = *on;
082800100113             PosCurCMM   = *on;
082900100113             WC1msg      = $Msg(12);
083000100113             WC1Rcd      = S01Nrr;
083100100113             update MK22S01;
083200100113             $EoF = *on;
083300100113             leave;
083400100113           ENDIF;
083500091203           $AGE(S01Nrr) = %dec(WS1cmm:7:0);
083600100310
083700091202
083800091202         //?Ora appuntamento
083900171221           IF  WS1hda <> 0;
084000171221             IF  WS1hda  < *zeros                              or
084100171221                 %dec(%subst(%editc(WS1hda:'X'):1:2):2:0) > 23 or
084200091202                 %dec(%subst(%editc(WS1hda:'X'):3:2):2:0) > 59;
084300091202               ErrMessage  = *on;
084400091202               ErrGenerico = *on;
084500091202               PosCurHDA   = *on;
084600091202               WC1msg      = $Msg(03);
084700171222               if  WS1hda  < *zeros;
084800171222                 WC1msg    = %trimR( WC1msg ) + ' (negativa)';
084900171222               endif;
085000091203               WC1Rcd      = S01Nrr;
085100091202               update MK22S01;
085200091202               $EoF = *on;
085300091202               leave;
085400091202             ENDIF;
085500091202           ELSE ;
085600100304           //?Se non inserita imposto quella dell'appuntamento principale
085700100304             Ws1hda = WC1hda;
085800100304             // ErrMessage  = *on;
085900100304             // ErrGenerico = *on;
086000100304             // PosCurHDA   = *on;
086100100304             // WC1msg      = $Msg(03);
086200100304             // WC1Rcd      = S01Nrr;
086300100304             // update MK22S01;
086400100304             // $EoF = *on;
086500100304             // leave;
086600091202           ENDIF;
086700091202
086800091202         //?Ora inizio impegno
086900171221           IF  WS1oii <> 0;
087000171221             IF  WS1oii < *zeros                               or
087100171221                 %dec(%subst(%editc(WS1oii:'X'):1:2):2:0) > 23 or
087200091202                 %dec(%subst(%editc(WS1oii:'X'):3:2):2:0) > 59;
087300091202               ErrMessage  = *on;
087400091202               ErrGenerico = *on;
087500091202               PosCurOII   = *on;
087600091202               WC1msg      = $Msg(03);
087700171222               if  WS1oii  < *zeros;
087800171222                 WC1msg    = %trimR( WC1msg ) + ' (negativa)';
087900171222               endif;
088000091203               WC1Rcd      = S01Nrr;
088100091202               update MK22S01;
088200091202               $EoF = *on;
088300091202               leave;
088400091202             ENDIF;
088500091203           ELSE;
088600100304           //?Se non inserita imposto quella dell'appuntamento principale
088700100304             WS1oii = WC1oii;
088800100304             // ErrMessage  = *on;
088900100304             // ErrGenerico = *on;
089000100304             // PosCurOII   = *on;
089100100304             // WC1msg      = $Msg(03);
089200100304             // WC1Rcd      = S01Nrr;
089300100304             // update MK22S01;
089400100304             // $EoF = *on;
089500100304             // leave;
089600091202           ENDIF;
089700090612
089800091202         //?Ora fine impegno
089900171221           IF  WS1ofi <> 0;
090000171221             IF  WS1ofi < *zeros                               or
090100171221                 %dec(%subst(%editc(WS1ofi:'X'):1:2):2:0) > 23 or
090200091202                 %dec(%subst(%editc(WS1ofi:'X'):3:2):2:0) > 59;
090300091202               ErrMessage  = *on;
090400091202               ErrGenerico = *on;
090500091202               PosCurOFI   = *on;
090600091202               WC1msg      = $Msg(03);
090700171222               if  WS1ofi  < *zeros;
090800171222                 WC1msg    = %trimR( WC1msg ) + ' (negativa)';
090900171222               endif;
091000091202               WC1Rcd = S01Nrr;
091100091202               update MK22S01;
091200091202               $EoF = *on;
091300091202               leave;
091400091202             ENDIF;
091500091203           ELSE;
091600100304           //?Se non inserita imposto quella dell'appuntamento principale
091700100304             WS1ofi = WC1ofi;
091800100304             // ErrMessage  = *on;
091900100304             // ErrGenerico = *on;
092000100304             // PosCurOFI   = *on;
092100100304             // WC1msg      = $Msg(03);
092200100304             // WC1Rcd      = S01Nrr;
092300100304             // update MK22S01;
092400100304             // $EoF = *on;
092500100304             // leave;
092600091202           ENDIF;
092700090624
092800091202         //?Congruenza tra inizio e fine impegno
092900091203           IF  WS1oii > WS1ofi;
093000091203             ErrMessage  = *on;
093100091203             ErrGenerico = *on;
093200091203             PosCurOFI   = *on;
093300091203             WC1msg      = $Msg(04);
093400091203             WC1Rcd      = S01Nrr;
093500091203             update MK22S01;
093600091203             $EoF = *on;
093700091203             leave;
093800091203           ENDIF;
093900091202
094000091203         //?Verifico che l'impegno sia congruente con ora appuntamento
094100091211           IF  WS1oii > WS1hda or WS1ofi < WS1hda;
094200091202             ErrMessage  = *on;
094300091202             ErrGenerico = *on;
094400091202             PosCurOFI   = *on;
094500091202             WC1msg      = $Msg(05);
094600091203             WC1Rcd      = S01Nrr;
094700091202             update MK22S01;
094800091202             $EoF = *on;
094900091202             leave;
095000091202           ENDIF;
095100091203
095200091203         //?Verifico che l'impegno sia congruente con l'impegno dell'appuntamento principale
095300091203           SELECT;
095400091203             WHEN  WS1ofi < WC1oii;
095500091203               ErrMessage  = *on;
095600091203               ErrGenerico = *on;
095700091203               PosCurOFI   = *on;
095800091203               WC1msg      = $Msg(06);
095900091203               WC1Rcd      = S01Nrr;
096000091203               update MK22S01;
096100091203               $EoF = *on;
096200091203               leave;
096300091203             WHEN  WS1oii > WC1ofi;
096400091203               ErrMessage  = *on;
096500091203               ErrGenerico = *on;
096600091203               PosCurOII   = *on;
096700091203               WC1msg      = $Msg(06);
096800091203               WC1Rcd      = S01Nrr;
096900091203               update MK22S01;
097000091203               $EoF = *on;
097100091203               leave;
097200091203           ENDSL;
097300091217
097400091217         //?Verifico se il commerciale è libero
097500091217           clear trmk84ds;
097600091217           IMK84cmm = %dec(WS1cmm:7:0);
097700091217           IMK84dad = WC1dad;
097800091217           IMK84oii = WS1oii;
097900091217           IMK84ofi = WS1ofi;
098000091217           trmk84r (trmk84ds);
098100091217         //?se torna errore <> "9" vuol dire che il commerciale è impegnato
098200091217           IF  OMK84err <> *blanks and OMK84err <> '9' and
098300100310               WH1fimp = *blanks;
098400091217             ErrMessage    = *on;
098500091217             ErrGenerico   = *on;
098600091217             PosCurCMM     = *on;
098700091217             WC1msg        = OMK84msg;
098800100310             WC1msg        = %trim(WC1msg) + ' Enter per forzare';
098900091217             WC1Rcd        = S01Nrr;
099000100310             WH1fimp       = '1';
099100100310             WH1savcmm     = WS1cmm;
099200091217             update MK22S01;
099300091217             $EoF = *on;
099400091217             leave;
099500091217           ENDIF;
099600091202
099700100310           WH1savcmm = WS1cmm;
099800091202           WC1Rcd = S01Nrr;
099900091202           update MK22S01;
100000091202         ENDDO;
100100100113
100200100113         //?Se errore vado a fine della routine
100300100113         IF  ErrGenerico;
100400100113           leavesr;
100500100113         ENDIF;
100600091203
100700091203         //?Controllo se ho caricato almeno un commerciale
100800091203         xx = 1;
100900091203         $NoAge = *on;
101000091203         FOR xx by 1 to %elem($Age);
101100091203           IF  $age(xx) > 0;
101200091203             $NoAge = *off;
101300091203           ENDIF;
101400091203         ENDFOR;
101500091203
101600091203         //?Se non ho caricato commerciali errore
101700091203         IF  $NoAge;
101800091203           ErrMessage  = *on;
101900091203           ErrGenerico = *on;
102000091203           WC1msg      = $Msg(07);
102100091203         ENDIF;
102200090612
102300090611       ENDSR;
102400091202
102500091202       //--------------------------------------------------------------
102600091202       //?Gestione tasto funzionale F06 da videata W01
102700091202       //?F06=Conferma
102800091202       //--------------------------------------------------------------
102900091202       BEGSR sr_F06W01;
103000091203
103100091203         $Eof = *off;
103200091203         clear S01Nrr;
103300091203
103400091203         //?Leggo subfile
103500091203         DOW  not $EoF;
103600091203           S01Nrr += 1;
103700091203           chain S01Nrr MK22S01;
103800091203           IF  not %found;
103900091203             $EoF = *on;
104000091203             leave;
104100091203           ENDIF;
104200100113           IF  WS1cmm = *blanks or WS1cmm = *zeros;
104300091203             iter;
104400091203           ENDIF;
104500091203
104600091203           SELECT;
104700091203
104800091204           //?Immissione sto creando una nuova attività
104900091204           //?Manutenzione sto scrivendo un nuovo commerciale
105000091204           //?vado sempre in scrittura
105100091204             WHEN  IMK22fle = 'I'  or
105200091204                  (IMK22fle = '2' and WH1atn = 0);
105300091203               //?TIATC
105400091203               clear TIATC000;
105500091211               tiatcds      = ds_tiatc;
105600091204               sav_atcatnp += 1;
105700091211               atcatnp      = sav_atcatnp;
105800091211               atccad       = '92';
105900091211               atchda       = WS1hda * 100;
106000091211               atccmm       = %dec(WS1cmm:7:0);
106100091211               atcoii       = WS1oii;
106200091211               atcofi       = WS1ofi;
106300091223               atcdim       = %dec(%date());
106400091223               atchim       = %dec(%time());
106500091211               atcpri       = knmus;
106600091203               write TIATC000;
106700091203               //?TIATL
106800091203               clear TIATL000;
106900091203               atlatn  = atcatn;
107000091203               atlatnp = atcatnp;
107100091203               atlatl  = ds_atcatn;
107200091203               atlatlp = ds_atcatnp;
107300091203               atlcau  = 'AFF';
107400091203               write TIATL000;
107500091204
107600091204           //?Modifica sto modificando un'attività già immessa
107700100224           //?Oppure annulla
107800091204           //?posso modificare tutto, posso Annullare (delete)
107900100224             WHEN  IMK22fle = '2' or IMK22fle = 'D';
108000091204               //?TIATC
108100091204               chain (WH1tat : WH1atn : WH1atnp) TIATC01L;
108200091204               IF  %found(TIATC01L);
108300091204                 IF  WS1sce <> *blanks;
108400091204                   delete TIATC000;
108500091204                 ELSE;
108600100416                 //?in modifica può essere che è stata variata la data appuntamento
108700100416                   IF  IMK22dad <> ATCdad;
108800100416                     ATCdad = IMK22dad;
108900100416                   ENDIF;
109000091211                   atchda   = WS1hda * 100;
109100091204                   atccmm   = %dec(WS1cmm:7:0);
109200091204                   atcoii   = WS1oii;
109300091204                   atcofi   = WS1ofi;
109400091204                   update TIATC000;
109500091204                 ENDIF;
109600091204               ENDIF;
109700091204               //?TIATL
109800091204               chain (WH1atn : WH1atnp) TIATL01L;
109900110121               IF  %found(TIATL01L);
110000091204                 IF  WS1sce <> *blanks;
110100091204                   delete TIATL000;
110200091204                 ELSE;
110300091204                   update TIATL000;
110400091204                 ENDIF;
110500091204               ENDIF;
110600091203
110700091204           //?Gestione sto esitando l'attività
110800091204           //?posso solo esitare in positivo o negativo "A"
110900091204             WHEN  IMK22fle = 'G';
111000091203               //?TIATC
111100091203               chain (WH1tat : WH1atn : WH1atnp) TIATC01L;
111200091203               IF  %found(TIATC01L);
111300100114                 atcest = IMK22esi;
111400091211                 atcdco = IMK22dad;
111500091211                 atchco = WS1hda * 100;
111600091204                 atccco = %dec(WS1cmm:7:0);
111700091204                 atcpru = knmus;
111800100114                 SELECT;
111900100114                   WHEN WS1sce <> *blanks;
112000100114                     atccac = 'NO';
112100100114                     atcest = 'N';
112200100114                   WHEN IMK22esi = 'N';
112300100114                     atccac = 'NO';
112400100114                   OTHER;
112500100114                     atccac = 'SI';
112600100114                 ENDSL;
112700110223                 dATC01 = ATCflo;
112800110223               //?aggancio il potenziale per recupare la categoria
112900110223                 chain ATCcpo TNCPO01L;
113000110223                 IF  not %found(TNCPO01L);
113100110223                   clear CPOfls;
113200110223                 ENDIF;
113300110223                 §ATCcapo2 = CPOfls;
113400110223                 ATCflo = dATC01;
113500091203                 update TIATC000;
113600091203               ENDIF;
113700091203
113800091203           ENDSL;
113900091203
114000091203         ENDDO;
114100091202
114200091202       ENDSR;
114300091202
114400091202       //--------------------------------------------------------------
114500091202       //?Gestione tasto funzionale F11 da videata W01
114600091202       //?F11=Agenda
114700091202       //--------------------------------------------------------------
114800091202       BEGSR sr_F11W01;
114900091202
115000091203         clear TRMK82ds;
115100100112         IMK82rich = '1';
115200091203         IMK82data = IMK22dad;
115300091230         IMK82cage = %dec(WC1cmm:7:0);
115400100331         //?Aggancio il subfile per recuperare il primo commerciale inserito
115500100331         S01Nrr = 1;
115600100331         chain S01Nrr MK22S01;
115700100331         IF  %found and WS1cmm > *zeros;
115800100331           IMK82cage = %dec(WS1cmm:7:0);
115900100331         ENDIF;
116000100331         kpjbu = TRMK82ds;
116100091203         trmk82r (kpjba);
116200091202
116300091202       ENDSR;
116400090611
116500090611       //--------------------------------------------------------------
116600090612       //?Gestione tasto funzionale F12 da videata W01
116700090611       //?F12=Ritorno
116800090611       //--------------------------------------------------------------
116900091202       BEGSR sr_F12W01;
117000090611
117100090612         // Ritorno al pgm richiamante
117200091201         OMK22f12 = 'S';
117300091203         $Fine    = *on;
117400090611
117500090611       ENDSR;
117600090610
117700090610       //--------------------------------------------------------------
117800090610       //?Operazioni finali.
117900090610       //--------------------------------------------------------------
118000091201       BEGSR sr_RoutEnd;
118100091210
118200091210?        //?Ritorno errore
118300091210         IF  ErrGenerico;
118400091210           OMK22err = 'E';
118500091210           OMK22msg = WC1msg;
118600091210         ENDIF;
118700090610
118800091201?        //?Comitto se richiesto e se non ci sono errori
118900091201         IF  IMK22cmt = 'S' and OMK22err = ' ';
119000091201           commit;
119100091201         ENDIF;
119200091026
119300091201         IF  IMK22cmt = 'S' and OMK22err <> ' ';
119400091203           rolbk;
119500091201         ENDIF;
119600091201
119700090610         *inLR = *on;
119800090610         return;
119900090610
120000090610       ENDSR;
120100090610
120200090610      /end-free
120300091201
120400090610       //--------------------------------------------------------------
120500090610       //?Schiere a tempo di compilazione.
120600090610       //--------------------------------------------------------------
120700090610
120800090610** - $MSG -------------------------------------------------------------------*
120900091201Utente non abilitato alla funzione                                             01
121000091202Codice commerciale errato                                                      02
121100091202Ora errata                                                                     03
121200091202Ora inizio impegno incongruente con ora fine impegno                           04
121300091202Ore inizio e fine impegno incongruenti con ora appuntamento                    05
121400091203Ore impegno affiancamento incongruenti con ore impegno appuntamento principale 06
121500091203Immettere almeno un commerciale                                                07
121600091203Cursore in area protetta dello schermo.                                        08
121700091203Appuntamento non trovato                                                       09
121800091203Opzione non ammessa                                                            10
121900091215Dati commit non specificati                                                    11
122000100113Commerciale già inserito                                                       12
122100100202Commerciale inserito uguale al commerciale principale dell'appuntamento        13
122200100310Attività non possibile per commerciale interno.                                14
