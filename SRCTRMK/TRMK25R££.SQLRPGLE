000100090610      //--------------------------------------------------------------
000200110204      //?TRMK25R - CREAZIONE ATTIVITA' BLOCCO CLIENTI
000300090610      //--------------------------------------------------------------
000400130828
000500130828       //--------------------------------------------------------------
000600130828       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700130828       //--------------------------------------------------------------
000800130828
000900130828     /*END
001000130828
001100130828       //--------------------------------------------------------------
001200130828       //?Specifiche di controllo.                                     ?
001300130828       //--------------------------------------------------------------
001400090610
001500091203     h decedit('0,') datedit(*ymd/) option(*nodebugio)
001600090610     h dftactgrp(*no) actgrp(*caller)
001700090610
001800090610      //---------------------------------------------------------------
001900090610      //?Dichiarazione file.
002000090610      //---------------------------------------------------------------
002100090610
002200091201      // - Attività commerciale
002300110204     fTIATC01L  uf a e           k disk    commit(IMK25fcmt) usropn
002400091202
002500110204      // - Anagrafica Clienti
002600110204     fCNACO16L  if   e           k disk
002700130828
002800130828      // -?Anagrafica Commerciali?
002900130828     fAZCMM01L  if   e           k disk
003000110204
003100091201      // - Tabelle
003200091201     fTabel00f  if   e           k disk
003300090610
003400091201      // - Video
003500110204     fTRMK25D   cf   e             workstn
003600110204     f                                     sfile(mk25S01 : S01nrr)
003700090610     f                                     indds(IndDspF)
003800090610     f                                     infds(InfDspF)
003900090610     f                                     usropn
004000090610
004100090610      //---------------------------------------------------------------
004200090610      //?Definizione costanti.
004300090610      //---------------------------------------------------------------
004400090610
004500090610      // - Tasti funzionali a video
004600090610     d c_F01           c                   const(x'31')
004700090610     d c_F02           c                   const(x'32')
004800090610     d c_F03           c                   const(x'33')
004900090610     d c_F04           c                   const(x'34')
005000090610     d c_F05           c                   const(x'35')
005100090610     d c_F06           c                   const(x'36')
005200090610     d c_F07           c                   const(x'37')
005300090610     d c_F08           c                   const(x'38')
005400090610     d c_F09           c                   const(x'39')
005500090610     d c_F10           c                   const(x'3A')
005600090610     d c_F11           c                   const(x'3B')
005700090610     d c_F12           c                   const(x'3C')
005800090610     d c_F13           c                   const(x'B1')
005900090610     d c_F14           c                   const(x'B2')
006000090610     d c_F15           c                   const(x'B3')
006100090610     d c_F16           c                   const(x'B4')
006200090610     d c_F17           c                   const(x'B5')
006300090610     d c_F18           c                   const(x'B6')
006400090610     d c_F19           c                   const(x'B7')
006500090610     d c_F20           c                   const(x'B8')
006600090610     d c_F21           c                   const(x'B9')
006700090610     d c_F22           c                   const(x'BA')
006800090610     d c_F23           c                   const(x'BB')
006900090610     d c_F24           c                   const(x'BC')
007000090610     d c_Enter         c                   const(x'F1')
007100091201     d c_RollDown      c                   const(x'F4')
007200091201     d c_RollUp        c                   const(x'F5')
007300091230
007400091230     d Digits          c                   const('0123456789')
007500090610
007600090610      //---------------------------------------------------------------
007700090610      //?Definizione schiere.
007800090610      //---------------------------------------------------------------
007900090610
008000091202      // - Agenti
008100110208     d $Age            s              7  0 dim(5000)
008200091202
008300090610      // - Messaggi di errore
008400091203     d $Msg            s             78    dim(20) ctdata perrcd(1)
008500090610
008600090610      //---------------------------------------------------------------
008700090610      //?Definizione aree dati.
008800090610      //---------------------------------------------------------------
008900090610
009000090610      // - Dati utente
009100090610     d §AzUte        e ds                  extname(AZUTE00F)
009200090610     d                                     dtaara
009300090610     d §DatiUte      e ds                  extname(dDatiUte)
009400090610     d                                     dtaara
009500090610
009600090610      //---------------------------------------------------------------
009700090610      //?Definizione strutture dati.
009800090610      //---------------------------------------------------------------
009900090610
010000090610      // - InfDS
010100090610     d InfDspF         ds
010200090610     d  dsp_aid              369    369a                                        AID byte
010300090610
010400090610      // - Indicatori su DspF
010500090610     d IndDspF         ds
010600091201        // - Indicatori di gestione del subfile
010700091201     d  SflDsp_N                      1n   overlay(IndDspF : 30)
010800091201     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
010900091201     d  SflEnd                        1n   overlay(IndDspF : 32)
011000090929        // - Indicatori per l'utilizzo del file in formato *DS4
011100090929     d  FmtDspF_DS4                    n   overlay(IndDspF : 04)
011200091204        // - Indicatore di protezione campi
011300110208     d  ProtezCmm                      n   overlay(IndDspF : 19)
011400091201        // - Indicatori di errore
011500091201     d  ErrMessage                    1n   overlay(IndDspF : 28)
011600091202     d  PosCurCMM                     1n   overlay(IndDspF : 50)
011700110411     d  PosCurT54                     1n   overlay(IndDspF : 51)
011800110408     d  PosCurOPZ                     1n   overlay(IndDspF : 54)
011900110408     d  PosCurDSC                     1n   overlay(IndDspF : 60)
012000110408     d  PosCurOSC                     1n   overlay(IndDspF : 61)
012100110408     d  PosCurBLC                     1n   overlay(IndDspF : 62)
012200090610        // - Indicatori di errore generico
012300091201     d  ErrGenerico                   1n   overlay(IndDspF : 99)
012400090610
012500091201      // Campi di comodo per gestione indicatori a video
012600090610     d WindDspF        ds                  inz
012700090610     d   WindDspFa             1     49    inz(*zero)
012800090610     d   WindDspFb            50     99    inz(*zero)
012900090610
013000110411      //  ora esecuzione contatto
013100110411     d SAVora          ds                  inz
013200110411     d   ora                   1      2  0
013300110411     d   minuti                3      4  0
013400110411
013500091201      // - Controllo data
013600090610     d wlbdat          ds                  inz
013700090610     d  g02dat                 1      8  0
013800090610     d  g02inv                 9     16  0
013900090610     d  g02err                17     17
014000090610     d  g02tgi                18     22  0
014100090610
014200090610      // - Parametri ricevuti
014300090610     d KPJBA         e ds
014400110204     d TRMK25DS      e ds
014500091203
014600110208      // -  Gestione Note clienti/potenziali
014700110208     d TRMK20ds      e ds                  inz
014800110208
014900091203      // - Ricerca/Controllo tabelle
015000091203     d TIBS02ds      e ds                  inz
015100091203     d   T02Mod      e                     inz('C')
015200090610
015300090610      // - Reperimento dati utente
015400090610     d TIBS34ds      e ds
015500090610     d dUte01        e ds
015600091201
015700091201      // - Reperimento filiali
015800091201     d TRUL31DS      e ds
015900091201     d POG                    10    759    DIM(250)
016000090610
016100110204       // - Reperimento dati anagrafici
016200110204     d TIBS69ds      e ds
016300110204     d DS_cnaco      e ds                  inz extname(CNACO00F)
016400110204     d DS_cnind      e ds                  inz extname(CNIND00F)
016500110204     d DS_cnclp      e ds                  inz extname(CNCLP00F)
016600110204     d DS_fncls      e ds                  inz extname(FNCLS00F)
016700110315
016800110315      // - Stampa lettara testo 54 - Blocco clienti
016900110315     d TNTA53ds      e ds                  inz
017000110204
017100110208      // - Ricerca ultimo numero attività
017200110208     d TRUL33ds      e ds                  inz
017300110208
017400110204      // - Calcolo date prossima attività
017500110204     d Xgiolavds     e ds                  inz
017600110204
017700110211      // - FLO del file attività
017800110211     d datc01        e ds                  inz
017900110211
018000090610      // - Tabella CCO = Causale contatto
018100090610     d dcco          e ds                  inz
018200090610
018300090610      // - Tabella LAT = Autorizzazione
018400090610     d dlat          e ds                  inz
018500110411
018600110411      // - Tabella BI = Codici Blocco
018700110411     d dsbi          e ds                  inz
018800110208
018900110208      // - Controllo abilitazioni
019000110208     d TNTAA1DS      e ds                  inz
019100090610
019200090610      //---------------------------------------------------------------
019300090610      //?Definizione variabili globali.
019400090610      //---------------------------------------------------------------
019500090610
019600090610      // - Flags booleani
019700091203     d $EoF            s               n   inz(*off)
019800090610     d $Fine           s               n   inz(*off)
019900090611     d $InzW01         s               n   inz(*on)
020000110204     d $Blocco_cli     s               n   inz(*on)
020100090610
020200090610      // - Indici di schiera
020300110725     d xx              s              5  0 inz
020400090610
020500090610      // - Campi associati al video
020600090611     d $Video          s              2    inz('W1')
020700091201     d S01Nrr          s              4  0 inz
020800090610
020900090610     d dsp_mod         s             10I 0
021000090610
021100090610      // - Campi di comodo data
021200090610     d  data_eur       s               d   Datfmt(*eur)
021300090610
021400090610      // - Campi di comodo
021500090610     d wAbi            s                   like(UTEaut)  inz
021600111129     d wOggi           s              8s 0
021700110411     d ww1cdsc         s              8  0
021800110208
021900130828     d*// idTabella       s              2
022000130828     d*// Ordinamento     s              1
022100130828     d*// idElemento      s              8
022200130828     d*// TastoUscita     s              1
022300110208
022400090610
022500090610      //---------------------------------------------------------------
022600091203      //?Definizione procedure esterne.
022700090610      //---------------------------------------------------------------
022800110204
022900110204      // - Aggiunge/Toglie gg/mm dalla data
023000110204     d Xgiolav         pr                  extpgm('XGIOLAV')
023100110204     d  xgiolavds                          likeds(xgiolavds)
023200110315
023300110315      // - Stampa testo 54
023400110315     d Tnta53r         pr                  extpgm('TNTA53R')
023500110315     d  kpjba                              likeds(kpjba)
023600110315     d  tnta53ds                           likeds(tnta53ds)
023700121128
023800121128      // - Interrogazione tabella BI
023900121128     d TRTBBIR         pr                  extpgm('TRTBBIR')
024000121128     d  kpjba                              likeds(kpjba)
024100110204
024200090610      /copy gaitrasrc/srcprotopr,tibs02r
024300090610      /copy gaitrasrc/srcprotopr,tibs34r
024400110204      /copy gaitrasrc/srcprotopr,tibs69r
024500110208      /copy gaitrasrc/srcprotopr,tntaa1c
024600110208      /copy gaitrasrc/srcprotopr,trmk20r
024700130828       // -?Ricerca commerciali?
024800130828      /copy gaitrasrc/srcprotoPI,TRMK43R_1
024900130828      /copy gaitrasrc/srcprotoPR,TRMK43R
025000130828
025100130828      *///copy gaitrasrc/srcprotopr,trul19r
025200090610      /copy gaitrasrc/srcprotopr,trul31r
025300110208      /copy gaitrasrc/srcprotopr,trul33r
025400090610      /copy gaitrasrc/srcprotopr,xsrda8
025500090611      /copy gaitrasrc/srcprotopr,QsnQryModS
025600090610
025700090610      //---------------------------------------------------------------
025800090610      //?Definizione key-list.
025900090610      //---------------------------------------------------------------
026000110208
026100090610
026200090610      //---------------------------------------------------------------
026300090610      //?Riepilogo indicatori.
026400090610      //---------------------------------------------------------------
026500090610      // 28    : Emissione messaggio di errore a video
026600091201      // 30    : Condiziona SFLDSP
026700091201      // 31    : Condiziona SFLDSPCTL
026800091201      // 32    : Condiziona SFLEND
026900090610      // 99    : Generico di Errore
027000090610      //---------------------------------------------------------------
027100090610
027200090610      //---------------------------------------------------------------
027300090610      //?M A I N - L I N E
027400090610      //---------------------------------------------------------------
027500090610
027600090610     c     *Entry        plist
027700090610     c                   parm                    KPJBA
027800110204     c                   parm                    trmk25ds
027900090610
028000090610      /free
028100090610
028200091201       //?Operazioni iniziali controllo se video accetta 132 caratteri
028300110204       if not %open(trmk25d);
028400090611           dsp_mod = QueryDisplayModeSupport('4':*omit :*omit :*omit);
028500090611           If dsp_mod <> 0 ;
028600110204              open trmk25d;
028700090611           else ;
028800091203              $Fine = *on;
028900091210              ErrGenerico = *on;
029000091210              WC1msg = 'Videata errata';
029100090611           endif;
029200090611       endif;
029300110113
029400110113         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
029500090610
029600091201       //?Operazioni iniziali
029700091201       exsr sr_RoutInz;
029800090611
029900091201       //?Gestione video
030000091203       DOW Not $Fine;
030100090610         select;
030200090611           when $Video = 'W1';
030300090611             exsr GesW01;
030400090610           other;
030500090610             $Fine = *on;
030600090610         endsl;
030700090610       ENDDO;
030800090610
030900091201       //?Operazioni finali
031000091201       exsr sr_RoutEnd;
031100091201
031200091201       //--------------------------------------------------------------
031300091201       //?Operazioni iniziali.
031400091201       //--------------------------------------------------------------
031500091201       BEGSR sr_RoutInz;
031600091201
031700091201         //?Impostazione campi "fissi"
031800091201         TBLkut = 1;
031900091201
032000091201         //?Reperimento dati job
032100091201         exsr DatiJob;
032200111129
032300111129         //?Data del giorno
032400111129         wOggi = %dec(%date());
032500091201
032600091201         //?Verifica se visualizzare la finestra a 80 o 132 colonne:
032700110207         //?  solo se ricevuto il carattere "4" nella ds TRMK25 campo
032800091201         //?  del tipo formato
032900110204         FmtDspF_DS4 = (IMK25fmt = '4');
033000100224
033100091201         //?Verifica errori e autorità profilo
033200091201         clear wAbi;
033300091201         clear dLAT;
033400091201         select;
033500091201         //?- Se ho errori nei dati utente esco dal pgm
033600091201           when  DUTerr = 'E';
033700091201             $Fine = *on;
033800091210             ErrGenerico = *on;
033900091210             WC1msg = $Msg(01);
034000091210             leavesr;
034100091201         //?- Se non c'è l'abilitazione:
034200091201         //?  - se 1° livello, abilitazioni al terminal
034300091201         //?  - se 2° livello, abilitazioni al punto operativo
034400091201         //?  - se sede è impossibile ma se capita mando a fine il pgm
034500091201           when  UTEaut = *blank;
034600091201             select;
034700091201               when  DUTlpo = '1';
034800091201                 wAbi  = 'TP';
034900091201               when  DUTlpo = '2';
035000091201                 wAbi  = 'PO';
035100091201               when  DUTlpo = 'S';
035200091201                 $Fine = *on;
035300091210                 ErrGenerico = *on;
035400091210                 WC1msg = $Msg(01);
035500091210                 leavesr;
035600091201             endsl;
035700091201         //?- Altrimenti si caricano le abilitazioni del profilo
035800091201           other;
035900091201             dUTE01 = UTEfaf;
036000091201             if  §UTEpot <> *blank;
036100091201               wAbi = §UTEpot;
036200091201             else;
036300091201               wAbi = UTEaut;
036400091201             endif;
036500091201         ENDSL;
036600091201
036700091201         //?Controllo se ok l'abilitazione dell'utente
036800091201         reset TIBS02ds;
036900091201         T02sif = knsif;
037000091201         T02cod = 'LAT';
037100091201         T02ke1 = wAbi;
037200091201         TNTBE_RicercaControllo  (kpjba : tibs02ds);
037300091201
037400091201         if  T02err  = *blank;
037500091201           dLAT = T02uni;
037600091201         endif;
037700091201
037800091201         //?Errore o utente non abilitato
037900091201         if  T02err <> *blanks   or
038000091201             §LATabi = 'S';
038100091210           $Fine = *on;
038200091210           ErrGenerico = *on;
038300091201           WC1msg = $Msg(01);
038400091201           leavesr;
038500091201         endif;
038600091201
038700091201         //?Carico le filiali in base alle abilitazioni
038800091201         clear trul31ds;
038900091201         i31abi = wabi;
039000091201         I31cdi = DUTdis;
039100091201         I31car = DUTare;
039200091201         I31cpo = DUTpou;
039300091201         callp TRUL31R (kpjba:trul31ds);
039400091201         if o31pog<=*zeros;
039500091210           $Fine = *on;
039600091210           ErrGenerico = *on;
039700091201           WC1msg = $Msg(01);
039800091201           leavesr;
039900091201         endif;
040000091215
040100091215         //?Flag commit
040200110204         IF IMK25fcmt <> '1';
040300110204           IMK25fcmt = '0';
040400110204           IMK25cmt = 'N';
040500091215         ENDIF;
040600091201
040700091215         //?Apro file
040800091201         open tiatc01l;
040900091215
041000091215         //?Controllo congruenza flag commit
041100091215         //?se apertura file sotto commit si deve specificare se è questo pgm
041200091215         //?a dover fare il commit o no
041300110204         IF  IMK25fcmt = '1' and IMK25cmt <> 'N' and IMK25cmt <> 'S';
041400091215           $Fine = *on;
041500091215           ErrGenerico = *on;
041600110209           WC1msg = $Msg(05);
041700091215           leavesr;
041800091215         ENDIF;
041900091203
042000110207         //?Recupero le caratteristiche della causale
042100110207         clear  dCCO;
042200110207         clear  TIBS02ds;
042300110207         T02mod = 'C';
042400110207         T02cod = 'CCO';
042500110207         T02ke1 = imk25cac ;
042600110207         T02sif = KNSIF;
042700110207         TNTBE_RicercaControllo  (kpjba : tibs02ds);
042800110207         if  T02err = *blanks;
042900110207             dcco = t02uni ;
043000110207         endif;
043100110208       //?Carico gli agenti gestibili dal'utente
043200110208         clear $Age;
043300110208         clear xx;
043400110208
043500130828           setll  (*loval)  AZCMM000;
043600130828           read  AZCMM000;
043700130828           DOW  NOT %eof(AZCMM01L);
043800130828             if  %lookup(%subst(%editc(CMMcod:'X'):1:3) : POG) > *zero  or
043900130828                 %lookup(%subst(%editc(CMMuni:'X'):1:3) : POG) > *zero;
044000110208               xx += 1;
044100130828               $Age(xx) = CMMcod;
044200110208             endif;
044300130828             read  AZCMM000;
044400110208           ENDDO;
044500100310
044600091201       ENDSR;
044700091201
044800091201       //--------------------------------------------------------------
044900091201       //?Reperimento Dati del job (Utente/Operativi).
045000091201       //--------------------------------------------------------------
045100091201       BEGSR DatiJob;
045200091201
045300091201         in(E) §AzUte;
045400091201         if NOT %error;
045500091201           in(E) §DatiUte;
045600091201         endif;
045700091201         if %error or RSut = *blanks;
045800091201           clear TIBS34ds;
045900091201           tibs34r(tibs34ds);
046000091201           in §AzUte;
046100091201           in §DatiUte;
046200091201         endif;
046300091201
046400091201       ENDSR;
046500090610
046600090610       //--------------------------------------------------------------
046700090611       //?Gestione videata W01
046800090610       //--------------------------------------------------------------
046900090611       BEGSR GesW01;
047000090610
047100091201       //?Inizializzazione videata
047200091203         if $InzW01;
047300090611           exsr InzW01;
047400090611           $InzW01 = *off;
047500110209           WC1rcd = 1;
047600090610         endif;
047700091203
047800091203         $Eof   = *off;
047900090610
048000091201       //?Emissione videata
048100091201         sfldsp_n = *off;
048200110204         write MK25Z01;
048300110207         exfmt MK25C01;
048400091202         ErrMessage   = *off;
048500091202         ErrGenerico  = *off;
048600091201         clear WC1msg;
048700090610
048800091201         SELECT;
048900091202
049000091201       //?F12 = Ritorno
049100091201           WHEN dsp_aid = c_F12;
049200091201             exsr sr_F12W01;
049300090611             leavesr ;
049400091201
049500091201       //?F06 = Conferma
049600091201           WHEN dsp_aid = c_F06;
049700091203             //?prima faccio i controlli
049800110408             exsr Ctrc01;
049900110408             if errGenerico;
050000110408               leavesr;
050100110408             endif;
050200091203             exsr CtrW01;
050300091203             IF  ErrGenerico;
050400091203               leavesr;
050500091203             ENDIF;
050600091203             //?tutto ok confermo i dati
050700091201             exsr sr_F06W01;
050800091203             $Fine  = *on;
050900090611
051000091201       //?Enter
051100091201           OTHER;
051200091201       //?Controllo i dati immessi nel subfile
051300110408             exsr Ctrc01;
051400110408             if errGenerico;
051500110408               leavesr;
051600110408             endif;
051700091203              exsr CtrW01;
051800091203              IF  ErrGenerico;
051900091203                leavesr;
052000091203              ENDIF;
052100091201
052200091201         ENDSL;
052300090610
052400090610       ENDSR;
052500090610
052600090610       //--------------------------------------------------------------
052700090611       //?Caricamento videata telefonata W01
052800090610       //--------------------------------------------------------------
052900090611       BEGSR Inzw01;
053000090610
053100091201         //?Pulizia subfile
053200091201         exsr Sr_PulSfl;
053300110408
053400110408          // calcola data scadenza attività
053500110408               If §ccoupm = '+' or §ccoupm = '-' ;
053600110408                  clear XGIOLAVDS;
053700110408                  IXGLdata = %dec(%date());
053800110408                  If §ccoupm = '+' ;
053900110408                     IXGLadd  = 'S';
054000110408                  Else ;
054100110408                     IXGLsub  = 'S';
054200110408                  Endif ;
054300110408                  IXGLgg   = §CCOgio;
054400110408                  IXGLpa   = 'P';
054500110408                  IXGLfil = DUTpou;
054600110408                  XGIOLAV (xgiolavds);
054700110408                  If  OXGLerr = *blanks;
054800110408                      data_eur = %date(OXGLdata:*iso);
054900110408                      W1cdsc   = %dec(data_eur);
055000110408                  Endif;
055100110408               Endif ;
055200110408       // calcolo data scadenza se segno causale = ad '='
055300110408            If §ccoupm = '=' ;
055400110408                   W1cdsc   = *date;
055500110408            Endif ;
055600110408       // propongo l'ora del momento
055700110408             w1cosc = %dec(%subst(%EDITC(%dec(%time()):'X'):1:4):4:0);
055800110408
055900110308
056000110308         //?Imposto la causale
056100110308         W1Ccad = IMK25cac;
056200110308         W1Dcad = §CCOdes;
056300110408
056400110408         Poscurblc = *on ;
056500091201
056600091201         //?Carico subfile
056700091202         exsr sr_CarW01;
056800091201
056900090610       ENDSR;
057000091201
057100091201       //--------------------------------------------------------------
057200091201       //?Pulisco subfile.
057300091201       //--------------------------------------------------------------
057400091201       BEGSR sr_PulSfl;
057500091201
057600091201         SflDsp_N    = *on;
057700091201         SflDspCtl_N = *on;
057800110204         write  MK25C01;
057900091201         SflDspCtl_N = *off;
058000091201         SflEnd      = *off;
058100091201
058200091201         clear WC1rcd;
058300091201         clear WC1msg;
058400110411         clear W1cblc;
058500110411         clear W1dblc;
058600110411         clear W1cdsc;
058700110411         clear W1cosc;
058800091202         ErrMessage  = *off;
058900091202         ErrGenerico = *off;
059000091201         clear S01Nrr;
059100091201
059200091201       ENDSR;
059300091201
059400091201       //--------------------------------------------------------------
059500091201       //?Carico il subfile.
059600091201       //--------------------------------------------------------------
059700091202       BEGSR sr_CarW01;
059800091202
059900110204         //?Carico il subfile con i clienti sbloccati legati al potenziale
060000110204         Setll imk25cpo Cnaco16l ;
060100110204         reade imk25cpo Cnaco16l ;
060200110204
060300110204         Dow not %eof(Cnaco16l) ;
060400110207
060500110207           If acoabl = ' ' ;
060600110204             clear MK25S01;
060700110207             w01blc = 'B' ;
060800110518             w01t54 = ' ' ;
060900110204             W01ksc = acoksc ;
061000110204             W01rag = acorag ;
061100110204
061200110204             clear  tibs69ds;
061300110207             I69kcp = w01ksc;
061400110204             TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
061500110207             W01cmm = %editc(clpage:'X') ;
061600110512             // decodifico agente
061700130828             chain  (CLPage)  AZCMM000;
061800130828             IF  not %found(AZCMM01L);
061900110512               WD1Cmm = *all'*';
062000110512             ELSE;
062100130828               WD1Cmm  = CMMdes;
062200110512             ENDIF;
062300110208       //?Abilito la modifica del codice commerciale solo se sono autorizzata al cliente
062400110208             clear TNTAA1DS;
062500110208             ITAA1ksc  = w01ksc;
062600110208             ITAA1caut = '§utecli';
062700110208             kpjbu = tntaa1ds ;
062800110208             tntaa1c (kpjba) ;
062900110208             TNTAA1DS = kpjbu ;
063000110208
063100110208       //?Se cliente non in gestione
063200110208             IF  OTAA1fabi = 'N';
063300110208       //?Verifico se commerciale presente nella schiera dei comm.le gestiti da me
063400110208               IF  %lookup(CLPage : $Age) = *zero;
063500110208                   ProtezCMM = *on ;
063600110208               ENDIF;
063700110208             ENDIF;
063800110208
063900091201             S01Nrr += 1;
064000110204             write MK25S01;
064100110209
064200110209             ProtezCMM = *off ;
064300110207           endif ;
064400110207
064500110204             reade imk25cpo Cnaco16l ;
064600110204
064700110204           ENDDO ;
064800091203
064900091203         SflEnd = *on;
065000091201
065100091201       ENDSR;
065200090610
065300110408       //--------------------------------------------------------------
065400110408       //?Controllo Control Window W01
065500110408       //--------------------------------------------------------------
065600110408       BEGSR CtrC01;
065700110408
065800110408         WindDspF = IndDspF;
065900110408         reset WindDspFb;
066000110408         IndDspF  = WindDspF;
066100110411
066200110411         //?Data scadenza
066300110411         // controllo data scadenza contatto
066400110411           If    w1cdsc <> 0;
066500110411             clear wlbdat;
066600110411             g02dat = w1cdsc;
066700110411             xsrda8(wlbdat);
066800110411             if g02err = '1';
066900110411               errMessage  = *on;
067000110411               errGenerico = *on;
067100110411               PosCurDsc   = *on;
067200110411               Wc1msg = $Msg(07);
067300110411               leavesr;
067400110411             endif;
067500110411
067600110411             w1cdsc = g02dat;
067700110411             WW1cdsc = g02inv;
067800110411           else ;
067900110411             errMessage  = *on;
068000110411             errGenerico = *on;
068100110411             PosCurDsc   = *on;
068200110411             wc1msg = $Msg(07);
068300110411             leavesr;
068400110411           endif;
068500110411
068600110411         // controllo che la data non sia inferiore al 2000 o superiore al 2039
068700110411         IF  ww1cdsc < 20000101 or ww1cdsc > 20391231;
068800110411           errMessage  = *on;
068900110411           errGenerico = *on;
069000110411           PosCurDsc   = *on;
069100110411           wc1msg = $Msg(03);
069200110411           leavesr;
069300110411         ENDIF;
069400110411
069500110411       // controllo che la data non sia inferiore a 3 gg lavorativi da oggi
069600110411         clear XGIOLAVDS;
069700110411         IXGLdata = %dec(%date());
069800110411         IXGLsub  = 'S';
069900110411         IXGLgg   = 3;
070000110411         IXGLpa   = 'P';
070100110411         IXGLlav  = 'S';
070200110411         IXGLfil = dutpou;
070300110411         XGIOLAV (xgiolavds);
070400110411         IF  ww1cdsc < OXGLdata;
070500110411             errMessage  = *on;
070600110411             errGenerico = *on;
070700110411             PosCurDsc   = *on;
070800110411             wc1msg = $Msg(07);
070900110411             wc1msg = %trim(wc1msg) + ' ' +
071000110411                      'Non può essere inferiore a 3 gg.lavorativi';
071100110411             leavesr;
071200110411         ENDIF;
071300110411
071400110411       // controllo che la data non sia maggiore  a 14 mesi da oggi
071500110411         clear XGIOLAVDS;
071600110411         IXGLdata = %dec(%date());
071700110411         IXGLadd  = 'S';
071800110411         IXGLmm   = 14;
071900110411         IXGLpa   = 'P';
072000110411         IXGLfil = Dutpou;
072100110411         XGIOLAV (xgiolavds);
072200110411         IF  ww1cdsc > OXGLdata;
072300110411             errMessage  = *on;
072400110411             errGenerico = *on;
072500110411             PosCurDsc   = *on;
072600110411             wc1msg = $Msg(07);
072700110411             wc1msg = %trim(wc1msg) + ' ' +
072800110411                    'Non può essere maggiore di 14 mesi';
072900110411             leavesr;
073000110411         ENDIF;
073100110411
073200110411
073300110411         // controllo ora  esecuzione contatto
073400110411           If W1cosc <> 0 ;
073500110411              savora = %editc(W1cosc:'X');
073600120104              If  ora > 23 or minuti > 59;
073700110411                   errMessage  = *on;
073800110411                   errGenerico = *on;
073900110411                   PosCurOsc   = *on;
074000110411                   wc1msg = $Msg(08);
074100110411                   leavesr;
074200110411              endif;
074300110411           else ;
074400110411             errMessage  = *on;
074500110411             errGenerico = *on;
074600110411             PosCurOsc   = *on;
074700110411             wc1msg = $Msg(08);
074800110411             leavesr;
074900110411           endif;
075000110408
075100110408         //?Codice BLOCCO
075200110408           // --> Ricerca
075300110408           IF  %scan('?':W1cblc) > 0;
075400110408             ErrGenerico = *on;
075500110408             PosCurBLC   = *on;
075600121128             //idTabella = 'BI';
075700121128             //clear Ordinamento;
075800121128             //clear idElemento;
075900121128             //clear TastoUscita;
076000121128             //Tabel_Ricerca (idTabella:Ordinamento:idElemento:TastoUscita);
076100121128             //W1cblc = idElemento;
076200121128             clear kpjbu;
076300121128             TRTBBIR (kpjba);
076400121128             W1cblc = %subst(kpjbu:1:3);
076500110408           ENDIF;
076600110408
076700110408           // --> Controllo
076800110408           IF  W1cblc <> *blanks and W1cblc <> *zeros ;
076900110408             // solo numeri
077000110408             IF  %check(digits:W1cblc) > 0;
077100110408               ErrMessage  = *on;
077200110408               ErrGenerico = *on;
077300110408               PosCurBLC   = *on;
077400110408               WC1msg      = $Msg(03);
077500110408               leavesr;
077600110408             ENDIF;
077700110408             // controllo con tabella blocco clienti
077800110408             TBLcod = 'BI';
077900110408             TBLkey = W1cblc;
078000110408             chain (TBLkut : TBLcod : TBLkey) TABEL;
078100110408             IF  not %found(TABEL00F)
078200110408                 or  TBLflg <> *blanks;
078300110408               ErrMessage  = *on;
078400110408               ErrGenerico = *on;
078500110408               PosCurBLC   = *on;
078600110408               WC1msg      = $Msg(03);
078700110408               leavesr ;
078800110408             ENDIF;
078900110411             dsbi = tbluni ;
079000110411             w1dblc = §BIdes ;
079100110408           ELSE ;
079200110408             ErrMessage  = *on;
079300110408             ErrGenerico = *on;
079400110408             PosCurBLC   = *on;
079500110408             WC1msg      = $Msg(03);
079600110408             leavesr ;
079700110408           ENDIF;
079800110408
079900121128       //?Controllo se causale ancora utilizzabile
080000121128           IF  W1cblc <> *blanks and W1cblc <> *zeros ;
080100121128             IF  §BIuso = 'N';
080200121128               ErrMessage  = *on;
080300121128               ErrGenerico = *on;
080400121128               PosCurBlc   = *on;
080500121128               WC1msg      = 'Causale non più utilizzabile';
080600121128               leavesr;
080700121128             ENDIF;
080800121128           ENDIF;
080900110408
081000110408
081100110408
081200110408       ENDSR;
081300110408
081400090611       //--------------------------------------------------------------
081500090612       //?Controllo videata W01
081600090611       //--------------------------------------------------------------
081700091202       BEGSR CtrW01;
081800090611
081900091202         WindDspF = IndDspF;
082000091202         reset WindDspFb;
082100091202         IndDspF  = WindDspF;
082200091202
082300091202         clear S01Nrr;
082400110204         $Blocco_cli = *off ;
082500091203
082600091202
082700091202         DOW  not $EoF;
082800091202           S01Nrr += 1;
082900110204           chain S01Nrr MK25S01;
083000091202           IF  not %found;
083100091202             $EoF = *on;
083200091202             leave;
083300091202           ENDIF;
083400091202
083500110411         //?Se c'è l'opzione di stampa testo e non il blocco do errore
083600110411           IF  W01t54 = 'S' and W01blc = ' ' ;
083700110411               ErrMessage  = *on;
083800110411               ErrGenerico = *on;
083900110411               PosCurT54   = *on;
084000110411               WC1msg      = $Msg(09);
084100110411               WC1Rcd      = S01Nrr;
084200110411               update MK25S01;
084300110411               leavesr ;
084400110411             ENDIF;
084500110411         //?Se non c'è l'opzione di blocco torno a leggere
084600110207           IF  W01blc = *blanks;
084700110209             PosCurCMM = *off ;
084800110209             update MK25S01;
084900091202             iter;
085000091202           ENDIF;
085100110204
085200110204         //?Se invece si vuole creare il blocco ai clienti collegati memorizzo che c'è
085300110204         //?almeno un cliente bloccato
085400110204           $blocco_cli = *on ;
085500090611
085600091202         //?Codice Commerciale
085700091202           // --> Ricerca
085800110204           IF  %scan('?':W01cmm) > 0;
085900091202             ErrGenerico = *on;
086000091202             PosCurCMM   = *on;
086100130828             iMK43fil = W01ksc / 10000;
086200130828             TRMK43R (kpjba : TRMK43ds);
086300130828             if  oMK43err = *off  and  oMK43fxx = *blank;
086400130828               W01cmm  = %editc( oMK43cmm : 'X' );
086500130828               WD1cmm  = oMK43des;
086600130828             endif;
086700091202             WC1Rcd  = S01Nrr;
086800110204             update MK25S01;
086900091202             $EoF = *on;
087000091202             leave;
087100091202           ENDIF;
087200091202
087300091202           // --> Controllo
087400110204           IF  W01cmm > *zeros;
087500100310             // solo numeri
087600110204             IF  %check(digits:W01cmm) > 0;
087700091230               ErrMessage  = *on;
087800091230               ErrGenerico = *on;
087900091230               PosCurCMM   = *on;
088000091230               WC1msg      = $Msg(02);
088100110512               WD1Cmm      = *blanks ;
088200091230               WC1Rcd      = S01Nrr;
088300110204               update MK25S01;
088400091230               $EoF = *on;
088500091230               leave;
088600091230             ENDIF;
088700100310             // controllo con tabella agenti
088800130828             chain  (%int(W01cmm))  AZCMM000;
088900130828             IF  not %found(AZCMM01L);
089000091202               ErrMessage  = *on;
089100091202               ErrGenerico = *on;
089200091202               PosCurCMM   = *on;
089300091203               WC1msg      = $Msg(02);
089400091203               WC1Rcd      = S01Nrr;
089500110512               WD1Cmm      = *blanks ;
089600110207               update MK25S01;
089700091202               $EoF = *on;
089800091202               leave;
089900091202             ELSE;
090000130828               WD1cmm  = CMMdes;
090100091202             ENDIF;
090200110209             // no commerciali CHE APPARTENGONO A FILIALI diverse da quella del cliente
090300110209             IF  %subst(%editc(W01ksc:'X'):1:3) <>
090400110209               %subst(W01cmm:1:3);
090500110209               ErrMessage  = *on;
090600110209               ErrGenerico = *on;
090700110209               PosCurCMM   = *on;
090800110209               WC1msg      = $Msg(06);
090900110209               WC1Rcd      = S01Nrr;
091000110209               update MK25S01;
091100110209               $EoF = *on;
091200110209               leave;
091300110209             ENDIF;
091400110204             // no commerciali VARI e INATTIVI
091500130828             IF  CMMpar <> ' ' ;
091600110204               ErrMessage  = *on;
091700110204               ErrGenerico = *on;
091800110204               PosCurCMM   = *on;
091900110204               WC1msg      = $Msg(02);
092000110204               WC1Rcd      = S01Nrr;
092100110204               update MK25S01;
092200110204               $EoF = *on;
092300110204               leave;
092400110204             ENDIF;
092500111129             // Il commerciale deve essere ancora attivo
092600130828             IF  CMMdtIni > wOggi  or
092700130828                 CMMdtFin < wOggi;
092800111129               ErrMessage  = *on;
092900111129               ErrGenerico = *on;
093000111129               PosCurCMM   = *on;
093100111129               WC1msg      = $Msg(02);
093200111129               WC1Rcd      = S01Nrr;
093300111129               update MK25S01;
093400111129               $EoF = *on;
093500111129               leave;
093600111129             ENDIF;
093700110208           ELSE ;
093800110208             ErrMessage  = *on;
093900110208             ErrGenerico = *on;
094000110208             PosCurCMM   = *on;
094100110208             WC1msg      = $Msg(02);
094200110208             WC1Rcd      = S01Nrr;
094300110208             update MK25S01;
094400110208             $EoF = *on;
094500110208             leave;
094600110207           ENDIF;
094700110208
094800110411           WC1Rcd = S01Nrr;
094900110411           update MK25S01;
095000110411         ENDDO;
095100091203
095200110204         //?Se non ho bloccato nessun cliente errore
095300110204         IF  $Blocco_cli = *off ;
095400091203           ErrMessage  = *on;
095500091203           ErrGenerico = *on;
095600110208           PosCurOpz   = *on;
095700110208           WC1Rcd      = 1     ;
095800110209           WC1msg      = $Msg(04);
095900091203         ENDIF;
096000090612
096100090611       ENDSR;
096200091202
096300091202       //--------------------------------------------------------------
096400091202       //?Gestione tasto funzionale F06 da videata W01
096500091202       //?F06=Conferma
096600091202       //--------------------------------------------------------------
096700091202       BEGSR sr_F06W01;
096800091203
096900091203         $Eof = *off;
097000091203         clear S01Nrr;
097100110208
097200091203
097300091203         //?Leggo subfile
097400091203         DOW  not $EoF;
097500091203           S01Nrr += 1;
097600110204           chain S01Nrr MK25S01;
097700091203           IF  not %found;
097800091203             $EoF = *on;
097900091203             leave;
098000091203           ENDIF;
098100110207           IF  W01blc = *blanks ;
098200091203             iter;
098300091203           ENDIF;
098400091203
098500091204           //?Immissione sto creando una nuova attività
098600110208           //?Stacco nuovo numero attività
098700110208               exsr NewAttivita;
098800110207
098900091203               //?TIATC
099000091203               clear TIATC000;
099100110208               atctat  = §ccotat ;
099200110208               atcatn  = O33Nrf;
099300110208               atcatnp = 1;
099400110208               atccpo  = imk25cpo ;
099500110208               atcksc  = W01KSC ;
099600110411               atcprg  = W1cblc ;
099700110208               atccad  = imk25cac ;
099800110408               atchda  = w1cosc * 100 ;
099900110411               atcdad  = ww1cdsc  ;
100000110208               atccmm  = %dec(W01cmm:7:0);
100100110208               atcco3  = imk25cmm ;
100200110208               atccnw  = 'C' ;
100300110208               atcdim  = %dec(%date());
100400110208               atchim  = %dec(%time());
100500110208               atcpri  = knmus;
100600110211
100700110211               // imposto la categoria del potenziale fissa in quanto se sto creando delle
100800110211               // attività per bloccare i clienti il potenziale è sicuramente un CODIFICATO
100900110211                  datc01 = atcflo ;
101000110211                  §atccapo1 = 'C' ;
101100110211                  atcflo = datc01 ;
101200110411
101300091203               write TIATC000;
101400091203
101500110208               //?TICPN note con la causale del blocco
101600110208               clear TRMK20ds;
101700110208               Imk20tla = 'L';
101800110208               IMK20flm  = 'C';
101900110208               Imk20fno = 'S';
102000110208               IMK20cmt = '1';
102100110208               IMK20cpo  = atccpo;
102200110208               IMK20ksc  = atcksc;
102300110208               IMK20tat  = atctat;
102400110208               IMK20atn  = atcatn;
102500110208               IMK20atnp = atcatnp - 1;
102600110208               emk20no1 = 'Bloccare il cliente con la causale: ' +
102700110411                        w1cblc + ' - ' + W1dblc ;
102800110208               trmk20r (kpjba : TRMK20ds);
102900110315
103000110315         //?Se attività che lo richiede stampo lettera
103100110411           IF  W01t54  = 'S';
103200110315             clear TNTA53ds;
103300110315             DTA53ksc = ATCksc;
103400110411             DTA53fil = Dutpou;
103500110315             tnta53r (kpjba:tnta53ds);
103600110315           ENDIF;
103700110315
103800091203         ENDDO;
103900091202
104000091202       ENDSR;
104100091202
104200110208       //--------------------------------------------------------------
104300110208       //?Stacco nuovo numero attività.
104400110208       //--------------------------------------------------------------
104500110208       BEGSR NewAttivita;
104600110208
104700110208         clear TRUL33ds;
104800110208         I33tla = 'L';
104900110208         I33ope = 0;
105000110208         I33cnu = 070;
105100110208         I33num = 1;
105200110208         I33aaa = *YEAR;
105300110208         kpjbu  = TRUL33ds;
105400110208         trul33r (kpjba);
105500110208         TRUL33ds = kpjbu;
105600110208
105700110208       ENDSR;
105800110208
105900090611       //--------------------------------------------------------------
106000090612       //?Gestione tasto funzionale F12 da videata W01
106100090611       //?F12=Ritorno
106200090611       //--------------------------------------------------------------
106300091202       BEGSR sr_F12W01;
106400090611
106500090612         // Ritorno al pgm richiamante
106600110207         OMK25f12 = 'S';
106700091203         $Fine    = *on;
106800090611
106900090611       ENDSR;
107000090610
107100090610       //--------------------------------------------------------------
107200090610       //?Operazioni finali.
107300090610       //--------------------------------------------------------------
107400091201       BEGSR sr_RoutEnd;
107500091210
107600091210?        //?Ritorno errore
107700091210         IF  ErrGenerico;
107800110207           OMK25err = 'E';
107900110207           OMK25msg = WC1msg;
108000091210         ENDIF;
108100090610
108200091201?        //?Comitto se richiesto e se non ci sono errori
108300110207         IF  IMK25cmt = 'S' and OMK25err = ' ';
108400091201           commit;
108500091201         ENDIF;
108600091026
108700110207         IF  IMK25cmt = 'S' and OMK25err <> ' ';
108800091203           rolbk;
108900091201         ENDIF;
109000091201
109100090610         *inLR = *on;
109200090610         return;
109300090610
109400090610       ENDSR;
109500090610
109600090610      /end-free
109700091201
109800090610       //--------------------------------------------------------------
109900090610       //?Schiere a tempo di compilazione.
110000090610       //--------------------------------------------------------------
110100090610
110200090610** - $MSG -------------------------------------------------------------------*
110300091201Utente non abilitato alla funzione                                             01
110400091202Codice commerciale errato                                                      02
110500110208Codice Blocco errato                                                           03
110600110209Bloccare almeno un cliente                                                     04
110700110209Dati commit non specificati                                                    05
110800110209Fil. del codice commerciale incongruente con la fil. del cliente               06
110900110411Data Errata                                                                    07
111000110411Ora errata                                                                     08
111100110411Non scegliere stampa della lettera se il cliente non viene bloccato            09
