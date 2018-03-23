000100090610      //--------------------------------------------------------------
000200091201      //?TRMK22R - AFFIANCAMENTO ATTIVITA' COMMERCIALE
000300090610      //--------------------------------------------------------------
000400090610
000500091203     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600090610     h dftactgrp(*no) actgrp(*caller)
000700090610
000800090610      //---------------------------------------------------------------
000900090610      //?Dichiarazione file.
001000090610      //---------------------------------------------------------------
001100090610
001200091201      // - Attività commerciale
001300091215     fTIATC01L  uf a e           k disk    commit(IMK22fcmt) usropn
001400091203
001500091203      // - Legame attività commerciale
001600091215     fTIATL01L  uf a e           k disk    commit(IMK22fcmt) usropn
001700110922     fTIATL02L  if   e           k disk    rename(TIATL000:TIATL02)
001800091202
001900091201      // - Tabelle
002000091201     fTabel00f  if   e           k disk
002100110223
002200110223      // - Anagrafica potenziale
002300110223     fTNCPO01L  if   e           k disk
002400090610
002500091201      // - Video
002600091201     fTRMK22D   cf   e             workstn
002700091201     f                                     sfile(mk22S01 : S01nrr)
002800090610     f                                     indds(IndDspF)
002900090610     f                                     infds(InfDspF)
003000090610     f                                     usropn
003100090610
003200090610      //---------------------------------------------------------------
003300090610      //?Definizione costanti.
003400090610      //---------------------------------------------------------------
003500090610
003600090610      // - Tasti funzionali a video
003700090610     d c_F01           c                   const(x'31')
003800090610     d c_F02           c                   const(x'32')
003900090610     d c_F03           c                   const(x'33')
004000090610     d c_F04           c                   const(x'34')
004100090610     d c_F05           c                   const(x'35')
004200090610     d c_F06           c                   const(x'36')
004300090610     d c_F07           c                   const(x'37')
004400090610     d c_F08           c                   const(x'38')
004500090610     d c_F09           c                   const(x'39')
004600090610     d c_F10           c                   const(x'3A')
004700090610     d c_F11           c                   const(x'3B')
004800090610     d c_F12           c                   const(x'3C')
004900090610     d c_F13           c                   const(x'B1')
005000090610     d c_F14           c                   const(x'B2')
005100090610     d c_F15           c                   const(x'B3')
005200090610     d c_F16           c                   const(x'B4')
005300090610     d c_F17           c                   const(x'B5')
005400090610     d c_F18           c                   const(x'B6')
005500090610     d c_F19           c                   const(x'B7')
005600090610     d c_F20           c                   const(x'B8')
005700090610     d c_F21           c                   const(x'B9')
005800090610     d c_F22           c                   const(x'BA')
005900090610     d c_F23           c                   const(x'BB')
006000090610     d c_F24           c                   const(x'BC')
006100090610     d c_Enter         c                   const(x'F1')
006200091201     d c_RollDown      c                   const(x'F4')
006300091201     d c_RollUp        c                   const(x'F5')
006400091230
006500091230     d Digits          c                   const('0123456789')
006600090610
006700090610      //---------------------------------------------------------------
006800090610      //?Definizione schiere.
006900090610      //---------------------------------------------------------------
007000090610
007100091202      // - Agenti
007200091202     d $Age            s              7  0 dim(05)
007300091202
007400090610      // - Messaggi di errore
007500091203     d $Msg            s             78    dim(20) ctdata perrcd(1)
007600090610
007700090610      //---------------------------------------------------------------
007800090610      //?Definizione aree dati.
007900090610      //---------------------------------------------------------------
008000090610
008100090610      // - Dati utente
008200090610     d §AzUte        e ds                  extname(AZUTE00F)
008300090610     d                                     dtaara
008400090610     d §DatiUte      e ds                  extname(dDatiUte)
008500090610     d                                     dtaara
008600090610
008700090610      //---------------------------------------------------------------
008800090610      //?Definizione strutture dati.
008900090610      //---------------------------------------------------------------
009000090610
009100090610      // - Status
009200090610     d Psds           sds
009300090610     d   SDSpgm          *proc
009400090610
009500090610      // - InfDS
009600090610     d InfDspF         ds
009700090610     d  dsp_aid              369    369a                                        AID byte
009800091203     d  dsp_rig              370    370
009900091203     d  dsp_col              371    371
010000090610
010100090610      // - Indicatori su DspF
010200090610     d IndDspF         ds
010300091201        // - Indicatori di gestione del subfile
010400091201     d  SflDsp_N                      1n   overlay(IndDspF : 30)
010500091201     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
010600091201     d  SflEnd                        1n   overlay(IndDspF : 32)
010700090929        // - Indicatori per l'utilizzo del file in formato *DS4
010800090929     d  FmtDspF_DS4                    n   overlay(IndDspF : 04)
010900100412        // - Indicatore di modifica
011000100412     d  Modifica                       n   overlay(IndDspF : 02)
011100091204        // - Indicatore di interrogazione
011200091203     d  Interroga                      n   overlay(IndDspF : 05)
011300091204        // - Indicatore di protezione campi
011400091204     d  Proteggi                       n   overlay(IndDspF : 06)
011500100224        // - Indicatore di protezione campi per annullamento
011600100224     d  Annulla                        n   overlay(IndDspF : 07)
011700091201        // - Indicatori di errore
011800091201     d  ErrMessage                    1n   overlay(IndDspF : 28)
011900091202     d  PosCurCMM                     1n   overlay(IndDspF : 50)
012000091202     d  PosCurHDA                     1n   overlay(IndDspF : 51)
012100091202     d  PosCurOII                     1n   overlay(IndDspF : 52)
012200091202     d  PosCurOFI                     1n   overlay(IndDspF : 53)
012300091203     d  PosCurOPZ                     1n   overlay(IndDspF : 54)
012400090610        // - Indicatori di errore generico
012500091201     d  ErrGenerico                   1n   overlay(IndDspF : 99)
012600091202
012700091202      // Campi di comodo per controllo ore/minuti
012800091202     d Whhmm           ds
012900091202     d  Whh                    1      2  0
013000091202     d  Wmm                    3      4  0
013100090610
013200091201      // Campi di comodo per gestione indicatori a video
013300090610     d WindDspF        ds                  inz
013400090610     d   WindDspFa             1     49    inz(*zero)
013500090610     d   WindDspFb            50     99    inz(*zero)
013600091203
013700091203      // Cursore
013800091203     d ds_cursor       ds
013900091203     d  ds_riga                1      2b 0 inz
014000091203     d  $riga                  2      2
014100091203     d  ds_colo                3      4b 0 inz
014200091203     d  $colo                  4      4
014300090610
014400091201      // - Controllo data
014500090610     d wlbdat          ds                  inz
014600090610     d  g02dat                 1      8  0
014700090610     d  g02inv                 9     16  0
014800090610     d  g02err                17     17
014900090610     d  g02tgi                18     22  0
015000090610
015100090610      // - Parametri ricevuti
015200090610     d KPJBA         e ds
015300091201     d TRMK22DS      e ds
015400091203
015500091203      // - Ricerca/Controllo tabelle
015600091203     d TIBS02ds      e ds                  inz
015700091203     d   T02Mod      e                     inz('C')
015800090610
015900090610      // - Reperimento dati utente
016000090610     d TIBS34ds      e ds
016100090610     d dUte01        e ds
016200091203
016300091203      // - Agenda commerciale
016400091203     d TRMK82ds      e ds
016500091217
016600091217      // -  Controllo se commerciale impegnato
016700091217     d TRMK84ds      e ds
016800091201
016900091201      // - Reperimento filiali
017000091201     d TRUL31DS      e ds
017100091201     d POG                    10    759    DIM(250)
017200090610
017300090610      // - Tabella CCO = Causale contatto
017400090610     d dcco          e ds                  inz
017500090610
017600090610      // - Tabella LAT = Autorizzazione
017700090610     d dlat          e ds                  inz
017800091201
017900090610      // - Tabella 01 = Codici Commerciali
018000090610     d ds01          e ds                  inz
018100091203
018200091203      // - file Attività Commerciali
018300091203     d Tiatcds       e ds                  extname(TIATC00F)
018400091203      // - Ds di comodo per file Attività Commerciali
018500091203     d ds_Tiatc      e ds                  extname(TIATC00F)
018600091203     d                                     prefix(ds_)
018700110223
018800110223      // - Dati di ATCFLO del file TIATC00F
018900110223     d dATC01        e ds
019000090610
019100090610      //---------------------------------------------------------------
019200090610      //?Definizione variabili globali.
019300090610      //---------------------------------------------------------------
019400090610
019500090610      // - Flags booleani
019600091203     d $EoF            s               n   inz(*off)
019700090610     d $Fine           s               n   inz(*off)
019800090611     d $InzW01         s               n   inz(*on)
019900091203     d $NoAge          s               n   inz(*on)
020000090610
020100090610      // - Indici di schiera
020200090610     d xx              s              3  0 inz
020300091203     d yy              s              3  0 inz
020400090610
020500090610      // - Campi associati al video
020600090611     d $Video          s              2    inz('W1')
020700091201     d S01Nrr          s              4  0 inz
020800090610
020900090610     d dsp_mod         s             10I 0
021000090610
021100090610      // - Campi di comodo data
021200090610     d  data_eur       s               d   Datfmt(*eur)
021300090610     d  data_iso       s               d   Datfmt(*iso)
021400090610     d  data_dmy       s               d   Datfmt(*dmy)
021500090610
021600090610      // - Campi di comodo
021700091204     d sav_atcatnp     s                   like(atcatnp)
021800090610     d wAbi            s                   like(UTEaut)  inz
021900091203     d wColo           s              3s 0
022000111128     d wOggi           s              8s 0
022100091203     d wRiga           s              3s 0
022200090610
022300091202     d tb85Kfil        s              3
022400091202     d tb85Key         s              8
022500091202     d tb85Des1        s             25
022600090610
022700090610      //---------------------------------------------------------------
022800091203      //?Definizione procedure esterne.
022900090610      //---------------------------------------------------------------
023000091203
023100091203      // - Agenda
023200091203     d Trmk82r         pr                  extpgm('TRMK82R')
023300091203     d  kpjba                              likeds(kpjba)
023400091217
023500091217      // - Controllo agenda
023600091217     d Trmk84r         pr                  extpgm('TRMK84R')
023700091217     d  trmk84ds                           likeds(trmk84ds)
023800090610
023900090610      /copy gaitrasrc/srcprotopr,tibs02r
024000090610      /copy gaitrasrc/srcprotopr,tibs34r
024100090610      /copy gaitrasrc/srcprotopr,trul31r
024200090610      /copy gaitrasrc/srcprotopr,xsrda8
024300090610      /copy gaitrasrc/srcprotopr,trtb85r
024400090611      /copy gaitrasrc/srcprotopr,QsnQryModS
024500090610
024600090610      //---------------------------------------------------------------
024700090610      //?Definizione key-list.
024800090610      //---------------------------------------------------------------
024900090610
025000090610
025100090610      //---------------------------------------------------------------
025200090610      //?Riepilogo indicatori.
025300090610      //---------------------------------------------------------------
025400090610      // 28    : Emissione messaggio di errore a video
025500091201      // 30    : Condiziona SFLDSP
025600091201      // 31    : Condiziona SFLDSPCTL
025700091201      // 32    : Condiziona SFLEND
025800090610      // 99    : Generico di Errore
025900090610      //---------------------------------------------------------------
026000090610
026100090610      //---------------------------------------------------------------
026200090610      //?M A I N - L I N E
026300090610      //---------------------------------------------------------------
026400090610
026500090610     c     *Entry        plist
026600090610     c                   parm                    KPJBA
026700091201     c                   parm                    trmk22ds
026800090610
026900090610      /free
027000090610
027100091201       //?Operazioni iniziali controllo se video accetta 132 caratteri
027200091201       if not %open(trmk22d);
027300090611           dsp_mod = QueryDisplayModeSupport('4':*omit :*omit :*omit);
027400090611           If dsp_mod <> 0 ;
027500091201              open trmk22d;
027600090611           else ;
027700091203              $Fine = *on;
027800091210              ErrGenerico = *on;
027900091210              WC1msg = 'Videata errata';
028000090611           endif;
028100090611       endif;
028200090610
028300091201       //?Operazioni iniziali
028400091201       exsr sr_RoutInz;
028500090611
028600091201       //?Gestione video
028700091203       DOW Not $Fine;
028800090610         select;
028900090611           when $Video = 'W1';
029000090611             exsr GesW01;
029100090610           other;
029200090610             $Fine = *on;
029300090610         endsl;
029400090610       ENDDO;
029500090610
029600091201       //?Operazioni finali
029700091201       exsr sr_RoutEnd;
029800091201
029900091201       //--------------------------------------------------------------
030000091201       //?Operazioni iniziali.
030100091201       //--------------------------------------------------------------
030200091201       BEGSR sr_RoutInz;
030300091201
030400091201         //?Impostazione campi "fissi"
030500091201         TBLkut = 1;
030600111128
030700111128         //?Data del giorno
030800111128         wOggi = %dec(%date());
030900091201
031000091201         //?Reperimento dati job
031100091201         exsr DatiJob;
031200091201
031300091201         //?Verifica se visualizzare la finestra a 80 o 132 colonne:
031400091201         //?  solo se ricevuto il carattere "4" nella ds TRMK22 campo
031500091201         //?  del tipo formato
031600091203         FmtDspF_DS4 = (IMK22fmt = '4');
031700100412
031800100412         //?Se richiamato in modifica
031900100412         Modifica = (IMK22fle = '2');
032000091203
032100091203         //?Se richiamato in interrogazione proteggo tutto
032200091203         Interroga = (IMK22fle = '5');
032300091204
032400091204         //?Se richiamato in gestione proteggo tutto tranne la scelta
032500091204         Proteggi = (IMK22fle = 'G');
032600100224
032700100224         //?Se richiamato in annullamento proteggo tutto e imposto la scelta 'A'
032800100224         Annulla = (IMK22fle = 'D');
032900091201
033000091201         //?Verifica errori e autorità profilo
033100091201         clear wAbi;
033200091201         clear dLAT;
033300091201         select;
033400091201         //?- Se ho errori nei dati utente esco dal pgm
033500091201           when  DUTerr = 'E';
033600091201             $Fine = *on;
033700091210             ErrGenerico = *on;
033800091210             WC1msg = $Msg(01);
033900091210             leavesr;
034000091201         //?- Se non c'è l'abilitazione:
034100091201         //?  - se 1° livello, abilitazioni al terminal
034200091201         //?  - se 2° livello, abilitazioni al punto operativo
034300091201         //?  - se sede è impossibile ma se capita mando a fine il pgm
034400091201           when  UTEaut = *blank;
034500091201             select;
034600091201               when  DUTlpo = '1';
034700091201                 wAbi  = 'TP';
034800091201               when  DUTlpo = '2';
034900091201                 wAbi  = 'PO';
035000091201               when  DUTlpo = 'S';
035100091201                 $Fine = *on;
035200091210                 ErrGenerico = *on;
035300091210                 WC1msg = $Msg(01);
035400091210                 leavesr;
035500091201             endsl;
035600091201         //?- Altrimenti si caricano le abilitazioni del profilo
035700091201           other;
035800091201             dUTE01 = UTEfaf;
035900091201             if  §UTEpot <> *blank;
036000091201               wAbi = §UTEpot;
036100091201             else;
036200091201               wAbi = UTEaut;
036300091201             endif;
036400091201         ENDSL;
036500091201
036600091201         //?Controllo se ok l'abilitazione dell'utente
036700091201         reset TIBS02ds;
036800091201         T02sif = knsif;
036900091201         T02cod = 'LAT';
037000091201         T02ke1 = wAbi;
037100091201         TNTBE_RicercaControllo  (kpjba : tibs02ds);
037200091201
037300091201         if  T02err  = *blank;
037400091201           dLAT = T02uni;
037500091201         endif;
037600091201
037700091201         //?Errore o utente non abilitato
037800091201         if  T02err <> *blanks   or
037900091201             §LATabi = 'S';
038000091210           $Fine = *on;
038100091210           ErrGenerico = *on;
038200091201           WC1msg = $Msg(01);
038300091201           leavesr;
038400091201         endif;
038500091201
038600091201         //?Carico le filiali in base alle abilitazioni
038700091201         clear trul31ds;
038800091201         i31abi = wabi;
038900091201         I31cdi = DUTdis;
039000091201         I31car = DUTare;
039100091201         I31cpo = DUTpou;
039200091201         callp TRUL31R (kpjba:trul31ds);
039300091201         if o31pog<=*zeros;
039400091210           $Fine = *on;
039500091210           ErrGenerico = *on;
039600091201           WC1msg = $Msg(01);
039700091201           leavesr;
039800091201         endif;
039900091215
040000091215         //?Flag commit
040100091215         IF IMK22fcmt <> '1';
040200091215           IMK22fcmt = '0';
040300091215           IMK22cmt = 'N';
040400091215         ENDIF;
040500091201
040600091215         //?Apro file
040700091201         open tiatc01l;
040800091203         open tiatl01l;
040900091215
041000091215         //?Controllo congruenza flag commit
041100091215         //?se apertura file sotto commit si deve specificare se è questo pgm
041200091215         //?a dover fare il commit o no
041300091215         IF  IMK22fcmt = '1' and IMK22cmt <> 'N' and IMK22cmt <> 'S';
041400091215           $Fine = *on;
041500091215           ErrGenerico = *on;
041600091215           WC1msg = $Msg(11);
041700091215           leavesr;
041800091215         ENDIF;
041900091203
042000091203         //?Controllo se esiste l'appuntamento passato
042100091203         chain(n) (IMK22tat : IMK22atn : IMK22atnp) TIATC01L;
042200091203         IF  Not %found(TIATC01L);
042300091210           $Fine = *on;
042400091210           ErrGenerico = *on;
042500091203           WC1msg = $Msg(09);
042600091203           leavesr;
042700091203         ENDIF;
042800100310
042900091203         //?Salvo dati appuntamento passato
043000091203         ds_tiatc = tiatcds;
043100100310
043200100310         //?Aggancio tabella CCO per recupero flag di azione eseguibile da comm.Interno
043300100310         //?CHIODO attività 92
043400100310         clear dCCO;
043500100310         reset TIBS02DS;
043600100310         T02sif = knsif;
043700100310         T02cod = 'CCO';
043800100310         T02ke1 = '92';
043900100310         TNTBE_RicercaControllo  (kpjba : tibs02ds);
044000100310         if  T02err  = *blank;
044100100310           dCCO = T02uni;
044200100310         endif;
044300091201
044400091201       ENDSR;
044500091201
044600091201       //--------------------------------------------------------------
044700091201       //?Reperimento Dati del job (Utente/Operativi).
044800091201       //--------------------------------------------------------------
044900091201       BEGSR DatiJob;
045000091201
045100091201         in(E) §AzUte;
045200091201         if NOT %error;
045300091201           in(E) §DatiUte;
045400091201         endif;
045500091201         if %error or RSut = *blanks;
045600091201           clear TIBS34ds;
045700091201           tibs34r(tibs34ds);
045800091201           in §AzUte;
045900091201           in §DatiUte;
046000091201         endif;
046100091201
046200091201       ENDSR;
046300090610
046400090610       //--------------------------------------------------------------
046500090611       //?Gestione videata W01
046600090610       //--------------------------------------------------------------
046700090611       BEGSR GesW01;
046800090610
046900091201       //?Inizializzazione videata
047000091203         if $InzW01;
047100090611           exsr InzW01;
047200090611           $InzW01 = *off;
047300090610         endif;
047400091203
047500091203         $Eof   = *off;
047600090610
047700091201       //?Emissione videata
047800091201         sfldsp_n = *off;
047900091202         WC1rcd = 1;
048000091202         write MK22Z01;
048100091201         exfmt MK22C01;
048200091202         ErrMessage   = *off;
048300091202         ErrGenerico  = *off;
048400091201         clear WC1msg;
048500090610
048600091201         SELECT;
048700091202
048800091201       //?F12 = Ritorno
048900091201           WHEN dsp_aid = c_F12;
049000091201             exsr sr_F12W01;
049100090611             leavesr ;
049200091201
049300091201       //?F11 = Agenda
049400091201           WHEN dsp_aid = c_F11;
049500091201             exsr sr_F11W01;
049600091201
049700091201       //?F06 = Conferma
049800091201           WHEN dsp_aid = c_F06;
049900091203             //?prima faccio i controlli
050000091203             exsr CtrW01;
050100091203             IF  ErrGenerico;
050200091203               leavesr;
050300091203             ENDIF;
050400091203             //?tutto ok confermo i dati
050500091201             exsr sr_F06W01;
050600091203             $Fine  = *on;
050700090611
050800091201       //?Enter
050900091201           OTHER;
051000091201       //?Controllo i dati immessi nel subfile
051100091203            IF  Not Interroga;
051200091203              exsr CtrW01;
051300091203              IF  ErrGenerico;
051400091203                leavesr;
051500091203              ENDIF;
051600091201            ENDIF;
051700091201
051800091201         ENDSL;
051900090610
052000090610       ENDSR;
052100090610
052200090610       //--------------------------------------------------------------
052300090611       //?Caricamento videata telefonata W01
052400090610       //--------------------------------------------------------------
052500090611       BEGSR Inzw01;
052600090610
052700091201         //?Pulizia subfile
052800091201         exsr Sr_PulSfl;
052900091201
053000091201         //?imposto i dati ricevuti
053100091203         //?Commerciale
053200091201         WC1cmm = IMK22cmm;
053300091203         //?decodifico commerciale
053400091201         clear ds01;
053500091201         clear WC1dcmm;
053600091201         TBLcod = '01';
053700091202         TBLkey = %editc(WC1cmm:'X');
053800091201         chain (TBLkut : TBLcod : TBLkey) TABEL;
053900091201         IF  %found(TABEL00F);
054000091201           ds01 = TBLuni;
054100091201         ENDIF;
054200091201         WC1dcmm = §01age;
054300091201
054400091203         //?Data attività
054500091211         data_iso = (%date(IMK22dad));
054600091211         data_eur = data_iso;
054700091211         WC1dad = %dec(data_eur);
054800091201
054900091203         //?Ora attività
055000091211         WC1hda = IMK22hda / 100;
055100091201
055200091203         //?Ora inizio/fine
055300091201         WC1oii = IMK22oii;
055400091201         WC1ofi = IMK22ofi;
055500091201
055600091201         //?Carico subfile
055700091202         exsr sr_CarW01;
055800091201
055900090610       ENDSR;
056000091201
056100091201       //--------------------------------------------------------------
056200091201       //?Pulisco subfile.
056300091201       //--------------------------------------------------------------
056400091201       BEGSR sr_PulSfl;
056500091201
056600091201         SflDsp_N    = *on;
056700091201         SflDspCtl_N = *on;
056800091201         write  MK22C01;
056900091201         SflDspCtl_N = *off;
057000091201         SflEnd      = *off;
057100091201
057200091201         clear WC1rcd;
057300091201         clear WC1msg;
057400091202         ErrMessage  = *off;
057500091202         ErrGenerico = *off;
057600091201         clear S01Nrr;
057700091201
057800091201       ENDSR;
057900091201
058000091201       //--------------------------------------------------------------
058100091201       //?Carico il subfile.
058200091201       //--------------------------------------------------------------
058300091202       BEGSR sr_CarW01;
058400091202
058500091202         //?Pulisco sk di comodo commerciali
058600091202         clear $AGE;
058700091201
058800091201         //?Carico subfile in base al tipo elaborazione
058900091201         SELECT;
059000091201
059100091201         //?Vuoto se sono in Immissione Affiancamento
059200091201           WHEN  IMK22fle = 'I';
059300091201
059400091202           xx = 1;
059500091202           FOR xx by 1 to %elem($Age);
059600091201             clear MK22S01;
059700091201             S01Nrr += 1;
059800091201             write MK22S01;
059900091201           ENDFOR;
060000091204
060100091204         //?mi salvo il progressivo
060200091204            sav_atcatnp = IMK22atnp;
060300091201
060400091201         //?Pieno se NON sono in Immissione
060500091201           OTHER;
060600110922
060700110922           //?Leggo il file legami con il n. attività dell'appuntamento
060800110922             setll (IMK22atn:IMK22atnp) TIATL02L;
060900110922             reade (IMK22atn:IMK22atnp) TIATL02L;
061000110922             DOW not %eof(TIATL02L);
061100110922             //?Aggangio l'attività di affiancamento
061200110922               chain(n) ('A':ATLatn:ATLatnp) TIATC01L;
061300110922               IF  %found(TIATC01L);
061400091201
061500091203                //?Dati key non visualizzati
061600091203                WH1tat  = atctat;
061700091203                WH1atn  = atcatn;
061800091203                WH1atnp = atcatnp;
061900091204                //?salvo il progressivo
062000091204                sav_atcatnp = atcatnp;
062100091203                //?Commerciale
062200091202                WS1cmm = %editc(atccmm:'X');
062300100310                WH1savcmm = WS1cmm;
062400091203                //?decodifico commerciale
062500091201                clear ds01;
062600091201                clear WS1dcmm;
062700091201                TBLcod = '01';
062800091201                TBLkey = WS1cmm;
062900091201                chain (TBLkut : TBLcod : TBLkey) TABEL;
063000091201                IF  %found(TABEL00F);
063100091201                  ds01 = TBLuni;
063200091201                ENDIF;
063300091201                WS1dcmm = §01age;
063400091201
063500091203                //?Ora attività
063600091211                WS1hda = atchda / 100;
063700091201
063800091203                //?Ora inizio/fine
063900091201                WS1oii = atcoii;
064000091201                WS1ofi = atcofi;
064100091216
064200091216                //?in visualizzazione faccio vedere se esito attività negativo
064300091217                IF  atccac = 'NO';
064400091216                  WS1ese = 'N';
064500091216                ENDIF;
064600100224
064700100224                //?se annullamento imposto opzione 'A'
064800100224                IF  Annulla;
064900100224                  WS1sce = 'A';
065000100224                ENDIF;
065100091201
065200091201                S01Nrr += 1;
065300091202
065400091203                //?Controllo di non superare il n. max di elementi per il sbfl e per la
065500091203                //?schiera di comodo, non dovrebbe capitare, ma così sono + tranquilla
065600091202                IF  S01Nrr > 5;
065700091202                  leave;
065800091202                ENDIF;
065900110922
066000091201                write MK22S01;
066100091202
066200091202                //?Incremento la schiera di comodo commerciali
066300091202                $AGE(S01Nrr) = %dec(WS1cmm:7:0);
066400110922               ENDIF;
066500110922
066600110922               reade (IMK22atn:IMK22atnp) TIATL02L;
066700110922             ENDDO;
066800091201
066900091201         ENDSL;
067000091203
067100091203         //?Riempo il subfile di righe vuote
067200091203         //?se richiamato in modifica "2"
067300091203         IF  S01Nrr < %elem($Age) and IMK22fle = '2';
067400100224          //?se non ho caricato nessuna riga mi salvo il progressivo
067500100224            IF S01Nrr = 0;
067600100224              sav_atcatnp = atcatnp;
067700100224            ENDIF;
067800091203           yy = %elem($Age) - S01Nrr;
067900091203           xx = 1;
068000091203           FOR xx by 1 to yy;
068100091203             clear MK22S01;
068200091203             S01Nrr += 1;
068300091203             write MK22S01;
068400091203           ENDFOR;
068500091203         ENDIF;
068600091203
068700091203         SflEnd = *on;
068800091201
068900091201       ENDSR;
069000090610
069100090611       //--------------------------------------------------------------
069200090612       //?Controllo videata W01
069300090611       //--------------------------------------------------------------
069400091202       BEGSR CtrW01;
069500090611
069600091202         WindDspF = IndDspF;
069700091202         reset WindDspFb;
069800091202         IndDspF  = WindDspF;
069900091202
070000091202         clear S01Nrr;
070100091203
070200091203         //?Pulisco sk di comodo commerciali
070300091203         clear $AGE;
070400091202
070500091202         DOW  not $EoF;
070600091202           S01Nrr += 1;
070700091202           chain S01Nrr MK22S01;
070800091202           IF  not %found;
070900091202             $EoF = *on;
071000091202             leave;
071100091202           ENDIF;
071200091203
071300091204         //?Opzione 'A' non l'accetto se:
071400091204         //?--> sono in immissione o se non ho il commerciale
071500091204         //?--> non ho il commerciale
071600091204         //?--> non ho il n. di attività (nuovo rcd nel subfile)
071700091204           IF  WS1sce = 'A' and
071800100113              (IMK22fle = 'I' or WH1atn = 0  or
071900100113               WS1cmm = *blanks or WS1cmm = *zeros);
072000091203             ErrMessage  = *on;
072100091203             ErrGenerico = *on;
072200091203             PosCurOPZ   = *on;
072300091203             WC1msg      = $Msg(10);
072400091203             WC1Rcd      = S01Nrr;
072500091203             update MK22S01;
072600091203             $EoF = *on;
072700091203             leave;
072800091203           ENDIF;
072900091202
073000091202         //?Se non c'è il codice commerciale ritorno a leggere il subfile
073100100113           IF  WS1cmm = *blanks;
073200091202             iter;
073300091202           ENDIF;
073400100113         //?Se il codice commerciale è a 0 ripulisco i campi e ritorno a leggere
073500100113         //?il subfile
073600100113           IF  WS1cmm = *zeros;
073700100310             clear WH1fimp;
073800100310             clear WH1fint;
073900100310             clear WH1savcmm;
074000100113             clear WS1cmm;
074100100113             clear WS1dcmm;
074200100113             clear WS1hda;
074300100113             clear WS1oii;
074400100113             clear WS1ofi;
074500100113             WC1Rcd = S01Nrr;
074600100113             update MK22S01;
074700100113             iter;
074800100113           ENDIF;
074900100310
075000100310            IF  WS1cmm <> WH1savcmm;
075100100310              clear WH1fimp;
075200100310              clear WH1fint;
075300100310            ENDIF;
075400090611
075500091202         //?Codice Commerciale
075600091202           // --> Ricerca
075700091202           IF  %scan('?':WS1cmm) > 0;
075800091202             ErrGenerico = *on;
075900091202             PosCurCMM   = *on;
076000100113             tb85Kfil = %subst(%editc(WC1cmm:'X'):1:3);
076100091202             clear tb85Key ;
076200091202             clear tb85Des1;
076300091202             trtb85r (kpjba : tb85kfil : tb85key : tb85des1);
076400091202             WS1cmm  = tb85Key;
076500091202             WS1dcmm = tb85Des1;
076600091202             WC1Rcd  = S01Nrr;
076700091202             update MK22S01;
076800091202             $EoF = *on;
076900091202             leave;
077000091202           ENDIF;
077100091202
077200091202           // --> Controllo
077300091202           IF  WS1cmm > *zeros;
077400100310             // solo numeri
077500091230             IF  %check(digits:WS1cmm) > 0;
077600091230               ErrMessage  = *on;
077700091230               ErrGenerico = *on;
077800091230               PosCurCMM   = *on;
077900091230               WC1msg      = $Msg(02);
078000091230               WC1Rcd      = S01Nrr;
078100091230               update MK22S01;
078200091230               $EoF = *on;
078300091230               leave;
078400091230             ENDIF;
078500100310             // non deve essere uguale al commerciale dell'appuntamento principale
078600100310             IF  WS1cmm = %editc(WC1cmm:'X');
078700100202               ErrMessage  = *on;
078800100202               ErrGenerico = *on;
078900100202               PosCurCMM   = *on;
079000100202               WC1msg      = $Msg(13);
079100100202               WC1Rcd      = S01Nrr;
079200100202               update MK22S01;
079300100202               $EoF = *on;
079400100202               leave;
079500100202             ENDIF;
079600100310             // controllo con tabella agenti
079700091202             clear WS1dcmm;
079800091202             clear ds01;
079900091202             TBLcod = '01';
080000091202             TBLkey = WS1cmm;
080100091202             chain (TBLkut : TBLcod : TBLkey) TABEL;
080200091202             IF  not %found(TABEL00F)
080300091202                 or  TBLflg <> *blanks;
080400091202               ErrMessage  = *on;
080500091202               ErrGenerico = *on;
080600091202               PosCurCMM   = *on;
080700091203               WC1msg      = $Msg(02);
080800091203               WC1Rcd      = S01Nrr;
080900091202               update MK22S01;
081000091202               $EoF = *on;
081100091202               leave;
081200091202             ELSE;
081300091202               ds01    = TBLuni;
081400091202               WS1dcmm = §01age;
081500091202             ENDIF;
081600110518             // no commerciali VARI e INATTIVI
081700110518             IF  §01par <> *blanks;
081800110518               ErrMessage  = *on;
081900110518               ErrGenerico = *on;
082000110518               PosCurCMM   = *on;
082100110518               WC1msg      = $Msg(02);
082200110518               WC1Rcd      = S01Nrr;
082300110518               update MK22S01;
082400110518               $EoF = *on;
082500110518               leave;
082600110518             ENDIF;
082700111128             // commerciale valido in base alla data inizio e fine attività
082800111128             IF  §01dtdec > %editc(wOggi:'X') or
082900111128                 §01dtfin < %editc(wOggi:'X');
083000111128               ErrMessage  = *on;
083100111128               ErrGenerico = *on;
083200111128               PosCurCMM   = *on;
083300111128               WC1msg      = $Msg(02);
083400111128               WC1Rcd      = S01Nrr;
083500111128               update MK22S01;
083600111128               $EoF = *on;
083700111128               leave;
083800111128             ENDIF;
083900100310             // se commerciale interno (funzione aziendale COMIN) controllo con flag
084000100310             // tabella CCO se si può fare l'attività
084100101018             IF  §01fun = 'COMIN' or §01fun = 'ASC' or §01fun = 'SA';
084200100310               SELECT;
084300100319                 WHEN  WH1fint = *blanks and §CCOcoi = 'F';
084400100310                   ErrMessage  = *on;
084500100310                   ErrGenerico = *on;
084600100310                   PosCurCMM   = *on;
084700100310                   WC1msg      = $Msg(14);
084800100310                   WC1msg      = %trim(WC1msg) + ' Enter per forzare';
084900100310                   WC1Rcd      = S01Nrr;
085000100310                   WH1fint     = '1';
085100100310                   WH1savcmm   = WS1cmm;
085200100310                   update MK22S01;
085300100310                   $EoF = *on;
085400100310                   leave;
085500100319                 WHEN  §CCOcoi = 'N';
085600100310                   ErrMessage  = *on;
085700100310                   ErrGenerico = *on;
085800100310                   PosCurCMM   = *on;
085900100310                   WC1msg      = $Msg(14);
086000100310                   WC1Rcd      = S01Nrr;
086100100310                   update MK22S01;
086200100310                   $EoF = *on;
086300100310                   leave;
086400100310               ENDSL;
086500100310             ENDIF;
086600091202           ENDIF;
086700091203
086800091203         //?Incremento la schiera di comodo commerciali
086900100113           xx = %lookup(%dec(WS1cmm:7:0) : $AGE);
087000100113           IF  xx > 0;
087100100113             ErrMessage  = *on;
087200100113             ErrGenerico = *on;
087300100113             PosCurCMM   = *on;
087400100113             WC1msg      = $Msg(12);
087500100113             WC1Rcd      = S01Nrr;
087600100113             update MK22S01;
087700100113             $EoF = *on;
087800100113             leave;
087900100113           ENDIF;
088000091203           $AGE(S01Nrr) = %dec(WS1cmm:7:0);
088100100310
088200091202
088300091202         //?Ora appuntamento
088400091202           IF  WS1hda > 0;
088500120104             IF  %dec(%subst(%editc(WS1hda:'X'):1:2):2:0) > 23 or
088600091202                 %dec(%subst(%editc(WS1hda:'X'):3:2):2:0) > 59;
088700091202               ErrMessage  = *on;
088800091202               ErrGenerico = *on;
088900091202               PosCurHDA   = *on;
089000091202               WC1msg      = $Msg(03);
089100091203               WC1Rcd      = S01Nrr;
089200091202               update MK22S01;
089300091202               $EoF = *on;
089400091202               leave;
089500091202             ENDIF;
089600091202           ELSE ;
089700100304           //?Se non inserita imposto quella dell'appuntamento principale
089800100304             Ws1hda = WC1hda;
089900100304             // ErrMessage  = *on;
090000100304             // ErrGenerico = *on;
090100100304             // PosCurHDA   = *on;
090200100304             // WC1msg      = $Msg(03);
090300100304             // WC1Rcd      = S01Nrr;
090400100304             // update MK22S01;
090500100304             // $EoF = *on;
090600100304             // leave;
090700091202           ENDIF;
090800091202
090900091202         //?Ora inizio impegno
091000091202           IF  WS1oii > 0;
091100120104             IF  %dec(%subst(%editc(WS1oii:'X'):1:2):2:0) > 23 or
091200091202                 %dec(%subst(%editc(WS1oii:'X'):3:2):2:0) > 59;
091300091202               ErrMessage  = *on;
091400091202               ErrGenerico = *on;
091500091202               PosCurOII   = *on;
091600091202               WC1msg      = $Msg(03);
091700091203               WC1Rcd      = S01Nrr;
091800091202               update MK22S01;
091900091202               $EoF = *on;
092000091202               leave;
092100091202             ENDIF;
092200091203           ELSE;
092300100304           //?Se non inserita imposto quella dell'appuntamento principale
092400100304             WS1oii = WC1oii;
092500100304             // ErrMessage  = *on;
092600100304             // ErrGenerico = *on;
092700100304             // PosCurOII   = *on;
092800100304             // WC1msg      = $Msg(03);
092900100304             // WC1Rcd      = S01Nrr;
093000100304             // update MK22S01;
093100100304             // $EoF = *on;
093200100304             // leave;
093300091202           ENDIF;
093400090612
093500091202         //?Ora fine impegno
093600091202           IF  WS1ofi > 0;
093700120104             IF  %dec(%subst(%editc(WS1ofi:'X'):1:2):2:0) > 23 or
093800091202                 %dec(%subst(%editc(WS1ofi:'X'):3:2):2:0) > 59;
093900091202               ErrMessage  = *on;
094000091202               ErrGenerico = *on;
094100091202               PosCurOFI   = *on;
094200091202               WC1msg      = $Msg(03);
094300091202               WC1Rcd = S01Nrr;
094400091202               update MK22S01;
094500091202               $EoF = *on;
094600091202               leave;
094700091202             ENDIF;
094800091203           ELSE;
094900100304           //?Se non inserita imposto quella dell'appuntamento principale
095000100304             WS1ofi = WC1ofi;
095100100304             // ErrMessage  = *on;
095200100304             // ErrGenerico = *on;
095300100304             // PosCurOFI   = *on;
095400100304             // WC1msg      = $Msg(03);
095500100304             // WC1Rcd      = S01Nrr;
095600100304             // update MK22S01;
095700100304             // $EoF = *on;
095800100304             // leave;
095900091202           ENDIF;
096000090624
096100091202         //?Congruenza tra inizio e fine impegno
096200091203           IF  WS1oii > WS1ofi;
096300091203             ErrMessage  = *on;
096400091203             ErrGenerico = *on;
096500091203             PosCurOFI   = *on;
096600091203             WC1msg      = $Msg(04);
096700091203             WC1Rcd      = S01Nrr;
096800091203             update MK22S01;
096900091203             $EoF = *on;
097000091203             leave;
097100091203           ENDIF;
097200091202
097300091203         //?Verifico che l'impegno sia congruente con ora appuntamento
097400091211           IF  WS1oii > WS1hda or WS1ofi < WS1hda;
097500091202             ErrMessage  = *on;
097600091202             ErrGenerico = *on;
097700091202             PosCurOFI   = *on;
097800091202             WC1msg      = $Msg(05);
097900091203             WC1Rcd      = S01Nrr;
098000091202             update MK22S01;
098100091202             $EoF = *on;
098200091202             leave;
098300091202           ENDIF;
098400091203
098500091203         //?Verifico che l'impegno sia congruente con l'impegno dell'appuntamento principale
098600091203           SELECT;
098700091203             WHEN  WS1ofi < WC1oii;
098800091203               ErrMessage  = *on;
098900091203               ErrGenerico = *on;
099000091203               PosCurOFI   = *on;
099100091203               WC1msg      = $Msg(06);
099200091203               WC1Rcd      = S01Nrr;
099300091203               update MK22S01;
099400091203               $EoF = *on;
099500091203               leave;
099600091203             WHEN  WS1oii > WC1ofi;
099700091203               ErrMessage  = *on;
099800091203               ErrGenerico = *on;
099900091203               PosCurOII   = *on;
100000091203               WC1msg      = $Msg(06);
100100091203               WC1Rcd      = S01Nrr;
100200091203               update MK22S01;
100300091203               $EoF = *on;
100400091203               leave;
100500091203           ENDSL;
100600091217
100700091217         //?Verifico se il commerciale è libero
100800091217           clear trmk84ds;
100900091217           IMK84cmm = %dec(WS1cmm:7:0);
101000091217           IMK84dad = WC1dad;
101100091217           IMK84oii = WS1oii;
101200091217           IMK84ofi = WS1ofi;
101300091217           trmk84r (trmk84ds);
101400091217         //?se torna errore <> "9" vuol dire che il commerciale è impegnato
101500091217           IF  OMK84err <> *blanks and OMK84err <> '9' and
101600100310               WH1fimp = *blanks;
101700091217             ErrMessage    = *on;
101800091217             ErrGenerico   = *on;
101900091217             PosCurCMM     = *on;
102000091217             WC1msg        = OMK84msg;
102100100310             WC1msg        = %trim(WC1msg) + ' Enter per forzare';
102200091217             WC1Rcd        = S01Nrr;
102300100310             WH1fimp       = '1';
102400100310             WH1savcmm     = WS1cmm;
102500091217             update MK22S01;
102600091217             $EoF = *on;
102700091217             leave;
102800091217           ENDIF;
102900091202
103000100310           WH1savcmm = WS1cmm;
103100091202           WC1Rcd = S01Nrr;
103200091202           update MK22S01;
103300091202         ENDDO;
103400100113
103500100113         //?Se errore vado a fine della routine
103600100113         IF  ErrGenerico;
103700100113           leavesr;
103800100113         ENDIF;
103900091203
104000091203         //?Controllo se ho caricato almeno un commerciale
104100091203         xx = 1;
104200091203         $NoAge = *on;
104300091203         FOR xx by 1 to %elem($Age);
104400091203           IF  $age(xx) > 0;
104500091203             $NoAge = *off;
104600091203           ENDIF;
104700091203         ENDFOR;
104800091203
104900091203         //?Se non ho caricato commerciali errore
105000091203         IF  $NoAge;
105100091203           ErrMessage  = *on;
105200091203           ErrGenerico = *on;
105300091203           WC1msg      = $Msg(07);
105400091203         ENDIF;
105500090612
105600090611       ENDSR;
105700091202
105800091202       //--------------------------------------------------------------
105900091202       //?Gestione tasto funzionale F06 da videata W01
106000091202       //?F06=Conferma
106100091202       //--------------------------------------------------------------
106200091202       BEGSR sr_F06W01;
106300091203
106400091203         $Eof = *off;
106500091203         clear S01Nrr;
106600091203
106700091203         //?Leggo subfile
106800091203         DOW  not $EoF;
106900091203           S01Nrr += 1;
107000091203           chain S01Nrr MK22S01;
107100091203           IF  not %found;
107200091203             $EoF = *on;
107300091203             leave;
107400091203           ENDIF;
107500100113           IF  WS1cmm = *blanks or WS1cmm = *zeros;
107600091203             iter;
107700091203           ENDIF;
107800091203
107900091203           SELECT;
108000091203
108100091204           //?Immissione sto creando una nuova attività
108200091204           //?Manutenzione sto scrivendo un nuovo commerciale
108300091204           //?vado sempre in scrittura
108400091204             WHEN  IMK22fle = 'I'  or
108500091204                  (IMK22fle = '2' and WH1atn = 0);
108600091203               //?TIATC
108700091203               clear TIATC000;
108800091211               tiatcds      = ds_tiatc;
108900091204               sav_atcatnp += 1;
109000091211               atcatnp      = sav_atcatnp;
109100091211               atccad       = '92';
109200091211               atchda       = WS1hda * 100;
109300091211               atccmm       = %dec(WS1cmm:7:0);
109400091211               atcoii       = WS1oii;
109500091211               atcofi       = WS1ofi;
109600091223               atcdim       = %dec(%date());
109700091223               atchim       = %dec(%time());
109800091211               atcpri       = knmus;
109900091203               write TIATC000;
110000091203               //?TIATL
110100091203               clear TIATL000;
110200091203               atlatn  = atcatn;
110300091203               atlatnp = atcatnp;
110400091203               atlatl  = ds_atcatn;
110500091203               atlatlp = ds_atcatnp;
110600091203               atlcau  = 'AFF';
110700091203               write TIATL000;
110800091204
110900091204           //?Modifica sto modificando un'attività già immessa
111000100224           //?Oppure annulla
111100091204           //?posso modificare tutto, posso Annullare (delete)
111200100224             WHEN  IMK22fle = '2' or IMK22fle = 'D';
111300091204               //?TIATC
111400091204               chain (WH1tat : WH1atn : WH1atnp) TIATC01L;
111500091204               IF  %found(TIATC01L);
111600091204                 IF  WS1sce <> *blanks;
111700091204                   delete TIATC000;
111800091204                 ELSE;
111900100416                 //?in modifica può essere che è stata variata la data appuntamento
112000100416                   IF  IMK22dad <> ATCdad;
112100100416                     ATCdad = IMK22dad;
112200100416                   ENDIF;
112300091211                   atchda   = WS1hda * 100;
112400091204                   atccmm   = %dec(WS1cmm:7:0);
112500091204                   atcoii   = WS1oii;
112600091204                   atcofi   = WS1ofi;
112700091204                   update TIATC000;
112800091204                 ENDIF;
112900091204               ENDIF;
113000091204               //?TIATL
113100091204               chain (WH1atn : WH1atnp) TIATL01L;
113200110121               IF  %found(TIATL01L);
113300091204                 IF  WS1sce <> *blanks;
113400091204                   delete TIATL000;
113500091204                 ELSE;
113600091204                   update TIATL000;
113700091204                 ENDIF;
113800091204               ENDIF;
113900091203
114000091204           //?Gestione sto esitando l'attività
114100091204           //?posso solo esitare in positivo o negativo "A"
114200091204             WHEN  IMK22fle = 'G';
114300091203               //?TIATC
114400091203               chain (WH1tat : WH1atn : WH1atnp) TIATC01L;
114500091203               IF  %found(TIATC01L);
114600100114                 atcest = IMK22esi;
114700091211                 atcdco = IMK22dad;
114800091211                 atchco = WS1hda * 100;
114900091204                 atccco = %dec(WS1cmm:7:0);
115000091204                 atcpru = knmus;
115100100114                 SELECT;
115200100114                   WHEN WS1sce <> *blanks;
115300100114                     atccac = 'NO';
115400100114                     atcest = 'N';
115500100114                   WHEN IMK22esi = 'N';
115600100114                     atccac = 'NO';
115700100114                   OTHER;
115800100114                     atccac = 'SI';
115900100114                 ENDSL;
116000110223                 dATC01 = ATCflo;
116100110223               //?aggancio il potenziale per recupare la categoria
116200110223                 chain ATCcpo TNCPO01L;
116300110223                 IF  not %found(TNCPO01L);
116400110223                   clear CPOfls;
116500110223                 ENDIF;
116600110223                 §ATCcapo2 = CPOfls;
116700110223                 ATCflo = dATC01;
116800091203                 update TIATC000;
116900091203               ENDIF;
117000091203
117100091203           ENDSL;
117200091203
117300091203         ENDDO;
117400091202
117500091202       ENDSR;
117600091202
117700091202       //--------------------------------------------------------------
117800091202       //?Gestione tasto funzionale F11 da videata W01
117900091202       //?F11=Agenda
118000091202       //--------------------------------------------------------------
118100091202       BEGSR sr_F11W01;
118200091202
118300091203         clear TRMK82ds;
118400100112         IMK82rich = '1';
118500091203         IMK82data = IMK22dad;
118600091230         IMK82cage = %dec(WC1cmm:7:0);
118700100331         //?Aggancio il subfile per recuperare il primo commerciale inserito
118800100331         S01Nrr = 1;
118900100331         chain S01Nrr MK22S01;
119000100331         IF  %found and WS1cmm > *zeros;
119100100331           IMK82cage = %dec(WS1cmm:7:0);
119200100331         ENDIF;
119300100331         kpjbu = TRMK82ds;
119400091203         trmk82r (kpjba);
119500091202
119600091202       ENDSR;
119700090611
119800090611       //--------------------------------------------------------------
119900090612       //?Gestione tasto funzionale F12 da videata W01
120000090611       //?F12=Ritorno
120100090611       //--------------------------------------------------------------
120200091202       BEGSR sr_F12W01;
120300090611
120400090612         // Ritorno al pgm richiamante
120500091201         OMK22f12 = 'S';
120600091203         $Fine    = *on;
120700090611
120800090611       ENDSR;
120900090610
121000090610       //--------------------------------------------------------------
121100090610       //?Operazioni finali.
121200090610       //--------------------------------------------------------------
121300091201       BEGSR sr_RoutEnd;
121400091210
121500091210?        //?Ritorno errore
121600091210         IF  ErrGenerico;
121700091210           OMK22err = 'E';
121800091210           OMK22msg = WC1msg;
121900091210         ENDIF;
122000090610
122100091201?        //?Comitto se richiesto e se non ci sono errori
122200091201         IF  IMK22cmt = 'S' and OMK22err = ' ';
122300091201           commit;
122400091201         ENDIF;
122500091026
122600091201         IF  IMK22cmt = 'S' and OMK22err <> ' ';
122700091203           rolbk;
122800091201         ENDIF;
122900091201
123000090610         *inLR = *on;
123100090610         return;
123200090610
123300090610       ENDSR;
123400090610
123500090610      /end-free
123600091201
123700090610       //--------------------------------------------------------------
123800090610       //?Schiere a tempo di compilazione.
123900090610       //--------------------------------------------------------------
124000090610
124100090610** - $MSG -------------------------------------------------------------------*
124200091201Utente non abilitato alla funzione                                             01
124300091202Codice commerciale errato                                                      02
124400091202Ora errata                                                                     03
124500091202Ora inizio impegno incongruente con ora fine impegno                           04
124600091202Ore inizio e fine impegno incongruenti con ora appuntamento                    05
124700091203Ore impegno affiancamento incongruenti con ore impegno appuntamento principale 06
124800091203Immettere almeno un commerciale                                                07
124900091203Cursore in area protetta dello schermo.                                        08
125000091203Appuntamento non trovato                                                       09
125100091203Opzione non ammessa                                                            10
125200091215Dati commit non specificati                                                    11
125300100113Commerciale già inserito                                                       12
125400100202Commerciale inserito uguale al commerciale principale dell'appuntamento        13
125500100310Attività non possibile per commerciale interno.                                14
