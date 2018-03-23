000100090601      *PARMS DFTACTGRP(*NO) ACTGRP(*caller) OPTION(*NOXREF)
000200090320      *PARMS CLOSQLCSR(*ENDMOD)
000300090320      *PARMS DYNUSRPRF(*OWNER)
000400100623     /*PRM closqlcsr(*endmod) dynusrprf(*owner)
000500100623     /*END
000600080206      //--------------------------------------------------------------
000700090716      //?TNTA87R - GESTIONE TRATTATIVE COMMERCIALI
000800080206      //--------------------------------------------------------------
000900080206
001000090407     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001100090601     h dftactgrp(*no) actgrp(*caller)
001200080206
001300080206      //---------------------------------------------------------------
001400080206      //?Dichiarazione file.
001500080206      //---------------------------------------------------------------
001600080206
001700090717      // - Archivio Trattative commerciale
001800090717     fTIVIS05L  if   e           k disk
001900090720      // - Archivio Offerte    commerciale
002000101222     fTIVOF11L  if   e           k disk
002100130806      // - Anagrafica Commerciali
002200130806     fAZCMM01L  if   e           k disk
002300090720
002400090716      // - Video Gestione trattative commerciali
002500090716     fTNTA87D   cf   e             workstn
002600090716     f                                     sfile(TA87S01 : S01nrr)
002700080208     f                                     indds(IndDspF)
002800080206     f                                     infds(InfDspF)
002900080206
003000080206      //---------------------------------------------------------------
003100090406      //?Definizione costanti.
003200080206      //---------------------------------------------------------------
003300050519
003400080207      // - Numero di record in ciascuna videata di subfile
003500090716     d c_SflPag        c                   const(19)
003600080207
003700080207      // - Tasti funzionali a video
003800080207     d c_F01           c                   const(x'31')
003900080207     d c_F02           c                   const(x'32')
004000080207     d c_F03           c                   const(x'33')
004100090406     d c_F04           c                   const(x'34')
004200080207     d c_F05           c                   const(x'35')
004300080207     d c_F06           c                   const(x'36')
004400080207     d c_F07           c                   const(x'37')
004500080207     d c_F08           c                   const(x'38')
004600080207     d c_F09           c                   const(x'39')
004700080207     d c_F10           c                   const(x'3A')
004800090303     d c_F11           c                   const(x'3B')
004900080207     d c_F12           c                   const(x'3C')
005000080207     d c_F13           c                   const(x'B1')
005100080207     d c_F14           c                   const(x'B2')
005200080207     d c_F15           c                   const(x'B3')
005300080207     d c_F16           c                   const(x'B4')
005400080207     d c_F17           c                   const(x'B5')
005500080207     d c_F18           c                   const(x'B6')
005600080207     d c_F19           c                   const(x'B7')
005700080207     d c_F20           c                   const(x'B8')
005800080207     d c_F21           c                   const(x'B9')
005900080207     d c_F22           c                   const(x'BA')
006000080207     d c_F23           c                   const(x'BB')
006100080207     d c_F24           c                   const(x'BC')
006200080207     d c_Enter         c                   const(x'F1')
006300080207     d c_RollDown      c                   const(x'F4')
006400080207     d c_RollUp        c                   const(x'F5')
006500080214
006600080214      // - Attributi di visualizzazione
006700080215      //  -  DspAtr() - Normale
006800080214     d C_dspatr_Norm   c                   const(x'20')
006900080215      //  -  DspAtr(RI)
007000080214     d C_dspatr_RI     c                   const(x'21')
007100081021     d C_dspatr_HI     c                   const(x'22')
007200080215      //  -  DspAtr(ND)
007300080214     d C_dspatr_ND     c                   const(x'27')
007400080215      //  -  DspAtr(BL) / Color(Red)
007500080214     d C_dspatr_BL     c                   const(x'28')
007600100211
007700100211     d Digits          c                   const('0123456789')
007800080206
007900080206      //---------------------------------------------------------------
008000080206      //?Definizione schiere.
008100080206      //---------------------------------------------------------------
008200080206
008300080206      // - Messaggi di errore
008400100408     d $Msg            s             78    dim(20) ctdata perrcd(1)
008500100723
008600100723      // - Commerciali della stessa famiglia del
008700100723      //   "Commerciale gestione trattativa"
008800100723     d wCmm_ds         ds                  inz
008900160614     d   $Cmm                         9a   inz  dim(999)
009000080206
009100080206      //---------------------------------------------------------------
009200080206      //?Definizione aree dati.
009300080206      //---------------------------------------------------------------
009400080206
009500080206      // - Dati utente
009600080206     d §AzUte        e ds                  extname(AZUTE00F)
009700080206     d                                     dtaara
009800080206     d §DatiUte      e ds                  extname(dDatiUte)
009900080206     d                                     dtaara
010000090720     d tivisDS       e ds                  ExtName(tivis00f)
010100080206
010200080206      //---------------------------------------------------------------
010300080206      //?Definizione strutture dati.
010400080206      //---------------------------------------------------------------
010500080206
010600080206      // - Status
010700080206     d Psds           sds
010800080206     d   SDSpgm          *proc
010900080206
011000080206      // - InfDS
011100080206     d InfDspF         ds
011200080207     d  dsp_aid              369    369a
011300130806     d**sfl_rrn              376    377i 0
011400130806     d**min_nrr              378    379i 0
011500130806     d**num_rcds             380    381i 0
011600080206
011700080206      // - Indicatori su DspF
011800080208     d IndDspF         ds
011900110915     d  Sede                          1n   overlay(IndDspF : 20)
012000080206        // - Indicatori di gestione del subfile
012100080206     d  SflDsp_N                      1n   overlay(IndDspF : 30)
012200080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
012300080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
012400080206     d  SflEnd                        1n   overlay(IndDspF : 33)
012500090422        // - Indicatori di errore 1° videata
012600080206     d  errMessage                    1n   overlay(IndDspF : 28)
012700090716     d  PosCurNrv                     1n   overlay(IndDspF : 50)
012800090716     d  PosCurRag                     1n   overlay(IndDspF : 51)
012900090720     d  PosCurCom                     1n   overlay(IndDspF : 52)
013000090716     d  PosCurApd                     1n   overlay(IndDspF : 53)
013100090716     d  PosCurApa                     1n   overlay(IndDspF : 54)
013200090716     d  PosCurChd                     1n   overlay(IndDspF : 55)
013300090716     d  PosCurCha                     1n   overlay(IndDspF : 56)
013400090716     d  PosCurAce                     1n   overlay(IndDspF : 57)
013500090716     d  PosCurOff                     1n   overlay(IndDspF : 58)
013600100302     d  PosCurChi                     1n   overlay(IndDspF : 59)
013700090529        // - Indicatori di errore subfile
013800090529     d  PosCurOpz                     1n   overlay(IndDspF : 50)
013900090422        // - Indicatori di errore generico
014000080206     d  errGenerico                   1n   overlay(IndDspF : 99)
014100100719        // - Indicatori di VISUALIZZAZIONE
014200100719     d  Pres_offerte                  1n   overlay(IndDspF : 40)
014300080206
014400090407
014500090407     d WindDspF        ds                  inz
014600090407     d   WindDspFa             1     49    inz(*zero)
014700090407     d   WindDspFb            50     99    inz(*zero)
014800090407
014900090319
015000090319     d wlbdat          ds                  inz
015100090319     d  g02dat                 1      8  0
015200090319     d  g02inv                 9     16  0
015300090319     d  g02err                17     17
015400090319     d  g02tgi                18     22  0
015500080206
015600080206      // - Parametri ricevuti
015700080206     d KPJBA         e ds
015800080206
015900080206      // - Reperimento dati utente
016000080206     d TIBS34ds      e ds
016100100312     d TRUL31DS      e ds
016200090504
016300090717      // - Interrogazione anagrafica clienti potenziali
016400091104     d TRMK01ds      e ds                  inz
016500100809
016600100809      // - Gestione contatti
016700100809     d TRMK19ds      e ds                  inz
016800100809
016900150127      *//// - Interrogazione anagrafica clienti
017000150127     d*// TNTAI1DS      e ds                  inz
017100090717
017200100719      // - Chiusura trattativa
017300100719     d TNTA16ds      e ds                  inz
017400100719     d  esi_ta16c      s              1
017500100719
017600090717      // - Gestione trattative commerciali
017700090717     d TNTA88ds      e ds                  inz
017800090422
017900100211      // - controllo abilitazioni
018000100211     d TNTAA1DS      e ds                  inz
018100101015
018200101015      // - FLO del file TRATTATIVA
018300101015     d dVis01        e ds                  inz  qualified
018400100428
018500080206      //---------------------------------------------------------------
018600080206      //?Definizione variabili globali.
018700080206      //---------------------------------------------------------------
018800080206
018900080206      // - Flags booleani
019000080208     d $Fine           s               n   inz(*off)
019100100719     d $Fine2          s               n   inz(*off)
019200080208     d $InzD01         s               n   inz(*on)
019300080208     d $InzS01         s               n   inz(*on)
019400080206     d $ErrGrave       s               n   inz(*off)
019500080207     d $EoF            s               n   inz(*off)
019600080208     d $RecOK          s               n   inz(*off)
019700090717     d $where          s               n   inz(*off)
019800080206
019900080206      // - Indici di schiera
020000100723     d xx              s              3  0 inz
020100080206
020200080207      // - Campi associati al video
020300080207     d $Video          s              2    inz('D1')
020400080208     d S01nrr          s              4  0 inz
020500080207     d W_SflPag        s              3  0 inz
020600130806     d*// wPag            s              4  0 inz
020700130806     d*// wRig            s              3  0 inz
020800080206
020900090401      // - Campi di comodo data
021000090401     d  data_eur       s               d   Datfmt(*eur)
021100090401
021200080206      // - Campi di comodo
021300090406     d wAbi            s                   like(UTEaut)  inz
021400100916     d WWWcom          s                   like(V01com)  inz
021500090720     d  w01Apd         s              8  0 inz
021600090720     d  w01Apa         s              8  0 inz
021700090720     d  w01Chd         s              8  0 inz
021800090720     d  w01Cha         s              8  0 inz
021900111227     d wOggi           s              8s 0
022000090717
022100160614     d StringaSql      s          10000    Varying       inz
022200080208
022300090508      //---------------------------------------------------------------
022400090508      //?Definizione procedure usate.
022500090508      //---------------------------------------------------------------
022600100809
022700100809       // - Interrogazione anagrafica clienti
022800150127     d*// TNTAI1R         pr                  extpgm('TNTAI1R')
022900150127     d*//  kpjba                              likeds(kpjba)
023000150127     d*//  tntai1ds                           likeds(tntai1ds)
023100150127     d TRKC20ds      e ds                  inz
023200150127     d TRKC20R         pr                  extpgm('TRKC20R')
023300150127     d  kpjba                              likeds(kpjba)
023400150127     d  TRKC20ds                           likeds(TRKC20ds)
023500100809
023600100809       // - Interrogazione Potenziali
023700100809     d TRMK01R         pr                  extpgm('TRMK01R')
023800100809     d  kpjba                              likeds(kpjba)
023900100809     d  trmk19ds                           likeds(trmk19ds) options(*nopass)
024000080206
024100080626      //---------------------------------------------------------------
024200080626      //?prototipi
024300080626      //---------------------------------------------------------------
024400080626      /copy gaitrasrc/srcprotopr,tibs34r
024500130806      /copy gaitrasrc/srcprotoPI,TRMK43R_1
024600130806      /copy gaitrasrc/srcprotoPR,TRMK43R
024700100312      /copy gaitrasrc/srcprotopr,trul31r
024800090717      /copy gaitrasrc/srcprotopr,tnta88r
024900100211      /copy gaitrasrc/srcprotopr,tntaa1c
025000090406      /copy gaitrasrc/srcprotopr,xsrda8
025100141111
025200141111       // -?Interrogazione clienti in Campagna?
025300150112     d TRKC21ds      e ds                  inz
025400150112     d wSaveNCM        s                   inz  like(oKC21ncm)
025500150112      /copy gaitrasrc/srcProtoPR,TRKC21R
025600100719
025700100719     d TNTA16C         pr                  extpgm('TNTA16C')
025800100719     d  kpjba                              likeds(kpjba)
025900100719     d  tnta16ds                           likeds(tnta16ds)
026000100719     d  esi_ta16c                     1A
026100080206      //---------------------------------------------------------------
026200080206      //?Definizione key-list.
026300080206      //---------------------------------------------------------------
026400080206
026500130806       // -?File AZCMM01L?
026600130806     d k_azcmm01     e ds                  extname(AZCMM01L : *key)
026700130806     d                                     prefix(k_)
026800080206
026900080206      //---------------------------------------------------------------
027000080206      //?Riepilogo indicatori.
027100080206      //---------------------------------------------------------------
027200090518      // - Comuni
027300080207      // 28    : Emissione messaggio di errore a video
027400080207      // - C01/S01
027500080207      // 30    : Condiziona SFLDSP    (*not)
027600080207      // 31    : Condiziona SFLDSPCTL (*not)
027700080207      // 30+31 : Condiziona SFLCLR
027800080207      // 32    : Condiziona SFLNXTCHG in S01
027900080207      // 33    : Condiziona SFLEND
028000090518      // 50    : Errore: Posizione cursore
028100080207      // - D01
028200090716      // 50    : Errore: Numero trattativa
028300090716      // 51    : Errore: Ragione sociale
028400090716      // 52    : Errore: Commerciale
028500100623      // 53    : Errore: Data Avvio    trattativa dal
028600100623      // 54    : Errore: Data Avvio    trattativa al
028700090716      // 55    : Errore: Data Chiusura trattativa dal
028800090716      // 56    : Errore: Data Chiusura trattativa al
028900100623      // 57    : Errore: Selezione delle trattative Avviate/chiuse/entrambe
029000090716      // 58    : Errore: Selezione delle trattative con solo o senza Offerta
029100080207      // - Comuni
029200080207      // 99    : Generico di Errore
029300080206      //---------------------------------------------------------------
029400080206
029500080206      //---------------------------------------------------------------
029600080206      //?M A I N - L I N E
029700080206      //---------------------------------------------------------------
029800080206
029900080206     c     *Entry        plist
030000080206     c                   parm                    KPJBA
030100080206
030200080206      /free
030300080206
030400080206       // Operazioni iniziali
030500080206       exsr RoutInz;
030600090601
030700080206
030800080206       // Gestione video
030900080206       DOW $Fine = *off;
031000080206         select;
031100080206           when $Video = 'D1';
031200080206             exsr GesD01;
031300080206           when $Video = 'S1';
031400080206             exsr GesS01;
031500090428           other;
031600080206             $Fine = *on;
031700080206         endsl;
031800080206       ENDDO;
031900080206
032000080206       // Operazioni finali
032100080206       exsr RoutEnd;
032200080206
032300080206       //--------------------------------------------------------------
032400080206       //?Operazioni iniziali.
032500080206       //--------------------------------------------------------------
032600080206       BEGSR RoutInz;
032700100427
032800100427         exec sql  set option  dynusrprf = *owner,
032900100427                               closqlcsr = *endmod;
033000080206
033100080206         // Impostazione campi "fissi"
033200080208         T01pgm = SDSpgm;
033300080206
033400080206         // Reperimento dati job
033500080206         exsr DatiJob;
033600100211
033700100211         // Controllo se utente autorizzato alla funzione
033800100211         reset TNTAA1DS;
033900100211         ITAA1caut = '§utegtc';
034000100211         kpjbu     = TNTAA1DS;
034100100211         tntaa1c (kpjba);
034200100211         TNTAA1DS = kpjbu;
034300100211
034400100211         IF  OTAA1fabi = 'N';
034500080206           $ErrGrave   = *on;
034600080206           errMessage  = *on;
034700080206           errGenerico = *on;
034800080206           V1Dmsg = $Msg(01);
034900080206           leavesr;
035000080206         endif;
035100100809
035200100809         wabi = OTAA1cabi;
035300100312
035400100312         clear TRUL31DS;
035500100312         I31abi = OTAA1cabi;
035600100312         I31cdi = DUTdis;
035700100312         I31car = DUTare;
035800100312         I31cpo = DUTpou;
035900100312         callp trul31r (kpjba : trul31ds);
036000100312         IF  O31pog <= *zeros;
036100100312           $ErrGrave   = *on;
036200100312           errMessage  = *on;
036300100312           errGenerico = *on;
036400100312           V1Dmsg = $Msg(01);
036500100312           leavesr;
036600100312         ENDIF;
036700110915
036800110915          // Verifico se sono utente di sede
036900110915         Sede = (Dutpou = 046);
037000111227
037100111227         //?Data del giorno
037200111227         wOggi = %dec(%date());
037300090714
037400080206       ENDSR;
037500080206
037600080206       //--------------------------------------------------------------
037700080206       //?Reperimento Dati del job (Utente/Operativi).
037800080206       //--------------------------------------------------------------
037900080206       BEGSR DatiJob;
038000080206
038100080206         in(E) §AzUte;
038200080206         if NOT %error;
038300080206           in(E) §DatiUte;
038400080206         endif;
038500080206         if %error or RSut = *blanks;
038600080206           clear TIBS34ds;
038700080206           tibs34r(tibs34ds);
038800080206           in §AzUte;
038900080206           in §DatiUte;
039000080206         endif;
039100080206
039200080206       ENDSR;
039300080206
039400080206       //--------------------------------------------------------------
039500080206       //?Gestione videata D01
039600080206       //--------------------------------------------------------------
039700080206       BEGSR GesD01;
039800080206
039900080206         // Inizializzazione videata
040000080206         if $InzD01 = *on;
040100080206           exsr InzD01;
040200080206           $InzD01 = *off;
040300080214         else;
040400080225           if  errGenerico = *off;
040500090318           // per adesso non prevedo nulla
040600080225           endif;
040700080206         endif;
040800080206
040900080206         // Emissione testata
041000090813         // if  errGenerico = *off;
041100090716           write TA87T01;
041200090813         // endif;
041300080206
041400080206         // Emissione videata
041500090716         exfmt TA87D01;
041600080206         errMessage   = *off;
041700080206         errGenerico  = *off;
041800080206         clear V1Dmsg;
041900080207
042000080206         select;
042100080206         // Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
042200080206           when  $ErrGrave = *on;
042300080206             $Fine  = *on;
042400080206         // F3=Fine
042500080208           when dsp_aid = c_F03;
042600080206             exsr F03D01;
042700090716         // F04=Prima telefonata Cliente
042800090716           when dsp_aid = c_F04;
042900101111             exsr CtrD01;
043000101111             if errGenerico;
043100101111               leavesr;
043200101111             endif;
043300090716             exsr F04D01;
043400090317         // F07=Prima telefonata Potenziale
043500090317           when dsp_aid = c_F07;
043600101111             exsr CtrD01;
043700101111             if errGenerico;
043800101111               leavesr;
043900101111             endif;
044000090317             exsr F07D01;
044100150127           //// -?F8=Nuova da Campagna?
044200150127           ////  ?(integrato nell'F4)?
044300150127           //when  dsp_aid = c_F08;
044400150127           //  exsr  CtrD01;
044500150127           //  if  errGenerico;
044600150127           //    leavesr;
044700150127           //  endif;
044800150127           //  exsr  F08D01;
044900081021
045000080206         // Enter
045100080206           other;
045200080206             exsr CtrD01;
045300080206             if errGenerico;
045400080206               leavesr;
045500080206             endif;
045600090428             $Video = 'S1';
045700080206             $InzS01 = *on;
045800080206         endsl;
045900080206
046000080206       ENDSR;
046100080206
046200080206       //--------------------------------------------------------------
046300080206       //?Inizializzazione videata D01
046400080206       //--------------------------------------------------------------
046500080206       BEGSR InzD01;
046600080206
046700090716           v01ace = 'A'   ;
046800100920           v01ffz = 'N'   ;
046900080206
047000080206       ENDSR;
047100080206
047200080206       //--------------------------------------------------------------
047300080206       //?Gestione tasto funzionale F3 da videata D01
047400080206       //?F3=Fine
047500080206       //--------------------------------------------------------------
047600080206       BEGSR F03D01;
047700080206
047800080207         // Chiusura del programma
047900080206         $Fine = *on;
048000080206
048100080206
048200080206       ENDSR;
048300080206
048400080206       //--------------------------------------------------------------
048500090609       //?Gestione tasto funzionale F07 da videata D01  richiamo il pgm
048600090717       //?di interrogazione potenziale
048700090717       //?F07=Prima trattativa potenziale
048800080206       //--------------------------------------------------------------
048900090317       BEGSR F07D01;
049000090717
049100100809         // Richiamo il pgm interrogazione potenziali
049200090717
049300100809             clear trmk19ds;
049400100809             imk19cmt = 'S';
049500100809             imk19fle = 'P' ;
049600100809             imk19tco = 'O' ;
049700100809                     if v01chi <> *blanks ;
049800100809                        imk19com = %dec(v01chi :7:0);
049900100809                        imk19comd = v01chid ;
050000100809                     endif ;
050100100809
050200100809             clear trmk01ds;
050300100809             mk01k11='1'   ;
050400100809             mk01imm='S'   ;
050500100809             mk01K10='B'   ;
050600100809             kpjbu=trmk01ds;
050700120201
050800120201             IF Sede;
050900120201               clear kpjbu;
051000120201               Trmk01r ( kpjba );
051100120201             ELSE;
051200100809             Trmk01r ( kpjba : trmk19ds );
051300120201             ENDIF;
051400120201
051500100809             trmk01ds=kpjbu  ;
051600090717
051700090717         // Se restituito errore => lo si visualizza
051800091104         if  mk01err = 'E';
051900090717           errMessage  = *on;
052000090717           errGenerico = *on;
052100091104           v1dmsg = mk01m10;
052200090717           leavesr;
052300090717         endif;
052400100302
052500100302
052600100302       ENDSR;
052700100302
052800100302
052900100302       //--------------------------------------------------------------
053000100302       //?Gestione tasto funzionale F04 da videata D01  richiamo il pgm
053100100302       //?di interrogazione clienti
053200090717       //?F04=Prima trattativa cliente
053300090717       //--------------------------------------------------------------
053400090717       BEGSR F04D01;
053500090717
053600100809       // Richiamo il pgm interrogazione clienti
053700150127         //clear tntai1ds;
053800150127         //ITAI1ric = 'T';
053900150127         //ITAI1abi = wabi;
054000150127         //ITAI1aut = '*C';
054100150127         //IF  v01chi <> *blanks ;
054200150127         //  ITAI1com  = %dec(v01chi :7:0);
054300150127         //  ITAI1comd = v01chid;
054400150127         //ENDIF;
054500150127         //
054600150127         //clear kpjbu;
054700150127         //tntai1r( kpjba : TNTAI1DS );
054800090717
054900150127         clear  TRKC20ds;
055000150127         iKC20ric = 'T';
055100150127         iKC20abi = wABI;
055200150127
055300150127         clear  kpjbu;
055400150127
055500150127         trkc20r ( kpjba : TRKC20ds );
055600090717
055700090717       ENDSR;
055800141111
055900141111       //--------------------------------------------------------------
056000141111       //?Gestione tasto funzionale F08 da videata D01: richiamo il pgm?
056100141111       //?di interrogazione clienti in Campagna                        ?
056200141111       //?F8=Nuova da Campagna                                         ?
056300141111       //--------------------------------------------------------------
056400141111       BEGSR  F08D01;
056500141111
056600141111         // -?Richiamo il *pgm interrogazione clienti in Campagna?
056700150112         clear  TRKC21ds;
056800150112         iKC21abi = wABI;
056900150112         iKC21ric = 'T';       // -?Trattative?
057000150112         iKC21ncm = wSaveNCM;
057100141111
057200150112         trkc21r ( KPJBA : TRKC21ds );
057300141111
057400150112         if  oKC21err = *on;
057500141111           errGenerico = *on;
057600141111           errMessage  = *on;
057700150112           V1Dmsg = oKC21msg;
057800141111         endif;
057900141112
058000150112         wSaveNCM = oKC21ncm;
058100141111
058200141111       ENDSR;
058300090717
058400080206       //--------------------------------------------------------------
058500080206       //?Controllo videata D01
058600080206       //--------------------------------------------------------------
058700080206       BEGSR CtrD01;
058800081017
058900090427
059000090427            WindDspF = IndDspF;
059100090427            reset WindDspFb;
059200090427            IndDspF  = WindDspF;
059300100302
059400100430            clear w01apd ;
059500100430            clear w01apa ;
059600100430            clear w01chd ;
059700100430            clear w01cha ;
059800100430
059900100302         // Verifico se inserito commerciale chi sono
060000100728           clear  v01chid;
060100100302           If    %scan('?' : v01chi) > 0  ;
060200100302             errGenerico = *on;
060300100302             PosCurChi   = *on;
060400130806             clear  TRMK43ds;
060500130806             iMK43fil = DUTpou;
060600130806             TRMK43R (kpjba : TRMK43ds);
060700130806             if  oMK43err = *off  and  oMK43fxx = *blank;
060800130806               v01chi  = %editc( oMK43cmm : 'X' );
060900130806               v01chid = oMK43des;
061000130806             endif;
061100130806             leavesr;
061200130806           EndIf;
061300100302
061400100302           IF  V01chi <> *blanks and V01chi <> *zeros;
061500100302
061600100302             IF  %check(digits:V01chi) > 0;
061700100302               ErrMessage  = *on;
061800100302               ErrGenerico = *on;
061900100302               PosCurChi   = *on;
062000100302               V1Dmsg      = $Msg(04);
062100100302               leavesr;
062200100302             ENDIF;
062300100302
062400100302           //?utente abilitato al commerciale
062500100302             clear TNTAA1DS;
062600100302             ITAA1caut = '§utegtc';
062700100302             ITAA1cmm  = %dec(V01chi:7:0);
062800100302             kpjbu     = tntaa1ds;
062900100302             tntaa1c (kpjba);
063000100302             TNTAA1DS = kpjbu;
063100100302             IF  OTAA1fabi = 'N';
063200100302               ErrMessage  = *on;
063300100302               ErrGenerico = *on;
063400100302               PosCurChi   = *on;
063500100302               V1Dmsg      = $Msg(04);
063600100302               leavesr;
063700100302             ENDIF;
063800100302
063900100302         // Controllo codice commerciale (chi sono )
064000130806             k_CMMcod = %int(V01chi);
064100130806             chain  %kds(k_azcmm01)  AZCMM000;
064200130806             if  NOT  %found(AZCMM01L);
064300100302               errMessage  = *on;
064400100302               errGenerico = *on;
064500100302               PosCurChi   = *on;
064600100302               V1Dmsg = $Msg(04);
064700100302               leavesr;
064800100302             else;
064900130806               V01chid = CMMdes;
065000100302             endif;
065100110614
065200110614           //?Commerciale no Vari o Inattivi
065300130806             IF  CMMpar <> *blanks;
065400110614               ErrMessage  = *on;
065500110614               ErrGenerico = *on;
065600110614               PosCurChi   = *on;
065700110614               V1Dmsg      = $Msg(04);
065800130806               leavesr;
065900110614             ENDIF;
066000111227
066100111227           //?Commerciale valido in base alla data inizio e fine attività
066200130806           IF  CMMdtIni > wOggi  or
066300130806               CMMdtFin < wOggi;
066400111227             ErrMessage  = *on;
066500111227             ErrGenerico = *on;
066600111227             PosCurChi   = *on;
066700111227             V1Dmsg = $Msg(04);
066800111227             leavesr;
066900111227           ENDIF;
067000100302
067100100302           endif;
067200090427
067300090717         // Controllo numero trattativa
067400090717           If   v01nrv > 0  ;
067500090717              chain v01nrv tivis05l  ;
067600090717              If   not %found(tivis05l);
067700090717                   errMessage  = *on;
067800090717                   errGenerico = *on;
067900090720                   PosCurNrv   = *on;
068000090717                   V1Dmsg      = $Msg(03);
068100090717                   leavesr;
068200100211              endif;
068300100211       // verifico se utente abilitato alla trattativa
068400100211           clear tntaa1ds;
068500100211           itaa1nrv  = V01nrv;
068600100211           itaa1caut = '§UTEGTC';
068700100211           kpjbu = tntaa1ds ;
068800100211           Tntaa1c (kpjba) ;
068900100211           tntaa1ds = kpjbu ;
069000100211        // Non abilitato
069100100211           IF  OTAA1fabi='N' ;
069200100211             errMessage  = *on;
069300100211             errGenerico = *on;
069400100211             PosCurNrv   = *on;
069500100211             V1Dmsg      = $Msg(10);
069600100211             leavesr;
069700100211           ENDIF;
069800100211
069900100211              clear tnta88ds ;
070000110926              If dutpou = 046 ;
070100110926              ita88fle = '5' ;
070200110926              else ;
070300100211              ita88cmt = 'S';
070400100211              ita88fle = 'G' ;
070500110926              endif ;
070600100211              ita88nrv = v01nrv ;
070700100302              If v01chi <> *blanks ;
070800100302                 ita88com = %dec(v01chi:7:0) ;
070900100302                 ita88comd = v01chid ;
071000100302              Endif ;
071100100211              Tnta88r ( kpjba : tnta88ds );
071200100211              errgenerico = *on ;
071300100211              leavesr ;
071400090717           endif;
071500090717
071600100302         // Verifico se inserita il commerciale che gestisce la trattativa
071700100728           clear  v01comd;
071800090717           If    %scan('?' : v01com) > 0  ;
071900081017             errGenerico = *on;
072000090720             PosCurCom   = *on;
072100130806             clear  TRMK43ds;
072200130806             iMK43fil = DUTpou;
072300130806             TRMK43R (kpjba : TRMK43ds);
072400130806             if  oMK43err = *off  and  oMK43fxx = *blank;
072500130806               v01com  = %editc( oMK43cmm : 'X' );
072600130806               v01comd = oMK43des;
072700130806             endif;
072800081017             leavesr;
072900090319           endif;
073000100211
073100100211           IF  V01com <> *blanks and V01com <> *zeros;
073200100211
073300100211             IF  %check(digits:V01com) > 0;
073400100211               ErrMessage  = *on;
073500100211               ErrGenerico = *on;
073600100211               PosCurCom   = *on;
073700100211               V1Dmsg      = $Msg(04);
073800100211               leavesr;
073900100211             ENDIF;
074000100211
074100100211             IF  %subst(V01com:4:4) = '0000' or
074200100211                 %subst(V01com:4:4) = '0999';
074300100211               ErrMessage  = *on;
074400100211               ErrGenerico = *on;
074500100211               PosCurCom   = *on;
074600100211               V1Dmsg      = $Msg(04);
074700100211              leavesr;
074800100211             ENDIF;
074900100211
075000100211           //?utente abilitato al commerciale
075100100211             clear TNTAA1DS;
075200100211             ITAA1caut = '§utegtc';
075300100211             ITAA1cmm  = %dec(V01com:7:0);
075400100211             kpjbu     = tntaa1ds;
075500100211             tntaa1c (kpjba);
075600100211             TNTAA1DS = kpjbu;
075700100211             IF  OTAA1fabi = 'N';
075800100211               ErrMessage  = *on;
075900100211               ErrGenerico = *on;
076000100211               PosCurCom   = *on;
076100100211               V1Dmsg      = $Msg(04);
076200100211               leavesr;
076300100211             ENDIF;
076400080215
076500100723         // Controllo codice commerciale gestione trattativa
076600130806             k_CMMcod = %int(V01com);
076700130806             chain  %kds(k_azcmm01)  AZCMM000;
076800130806             if  NOT  %found(AZCMM01L);
076900090318               errMessage  = *on;
077000090318               errGenerico = *on;
077100090720               PosCurCom   = *on;
077200090717               V1Dmsg = $Msg(04);
077300090318               leavesr;
077400090318             else;
077500130806               V01comd = CMMdes;
077600090318             endif;
077700100916
077800100916           //?utente abilitato all'unificante?
077900130806             if  %int(V01com) = CMMuni;
078000100916               WWWcom = V01com;
078100100916             else;
078200100916               clear TNTAA1DS;
078300100916               ITAA1caut = '§utegtc';
078400130806               ITAA1cmm  = CMMuni;
078500100916               kpjbu     = tntaa1ds;
078600100916               tntaa1c (kpjba);
078700100916               TNTAA1DS = kpjbu;
078800100916               if  OTAA1fabi = 'N';
078900100916                 WWWcom = V01com;                  // -?No?
079000100916               else;
079100130806                 WWWcom = %editc( CMMuni : 'X' );  // -?Sì?
079200100916               endif;
079300100916             endif;
079400100211
079500090319           endif;
079600090318
079700100623         // controllo data limite dal selezione trattative avviate dal al
079800090717           If    V01apd <> 0;
079900090319             clear wlbdat;
080000090717             g02dat = v01apd;
080100090319             xsrda8(wlbdat);
080200090319             if g02err = '1';
080300090319               errMessage  = *on;
080400090319               errGenerico = *on;
080500090717               PosCurApd   = *on;
080600090717               V1Dmsg = $Msg(05);
080700090319               leavesr;
080800090319             endif;
080900090319
081000090717             v01apd = g02dat;
081100090717             w01apd = g02inv;
081200090406         endif;
081300090319
081400100623         // controllo data limite al selezione trattative avviate dal al
081500090717           If    V01apa <> 0;
081600090319             clear wlbdat;
081700090717             g02dat = v01apa;
081800090319             xsrda8(wlbdat);
081900090319             if g02err = '1';
082000090319               errMessage  = *on;
082100090319               errGenerico = *on;
082200090717               PosCurApa   = *on;
082300090717               V1Dmsg = $Msg(05);
082400090319               leavesr;
082500090319             endif;
082600090319
082700090717             v01apa = g02dat;
082800090717             w01apa = g02inv;
082900090319           endif;
083000090319
083100090319         // Controllo se data congruenti
083200090717         if     w01apd > w01apa;
083300090319               errMessage  = *on;
083400090319               errGenerico = *on;
083500090717               PosCurApa   = *on;
083600090717               V1Dmsg = $Msg(06);
083700090319               leavesr;
083800090319         endif;
083900090319
084000090717         // controllo data limite dal selezione trattative chiuse  dal al
084100090717           If    V01chd <> 0;
084200090717             clear wlbdat;
084300090717             g02dat = v01chd;
084400090717             xsrda8(wlbdat);
084500090717             if g02err = '1';
084600090717               errMessage  = *on;
084700090717               errGenerico = *on;
084800090717               PosCurChd   = *on;
084900090717               V1Dmsg = $Msg(05);
085000090717               leavesr;
085100090717             endif;
085200090717
085300090717             v01chd = g02dat;
085400090717             w01chd = g02inv;
085500090717         endif;
085600090717
085700090717         // controllo data limite al selezione trattative chiuse dal al
085800090717           If    V01cha <> 0;
085900090717             clear wlbdat;
086000090717             g02dat = v01cha;
086100090717             xsrda8(wlbdat);
086200090717             if g02err = '1';
086300090717               errMessage  = *on;
086400090717               errGenerico = *on;
086500090717               PosCurCha   = *on;
086600090717               V1Dmsg = $Msg(05);
086700090717               leavesr;
086800090717             endif;
086900090717
087000090717             v01cha = g02dat;
087100090717             w01cha = g02inv;
087200090717           endif;
087300100408
087400100408       //?Se impostata data chiusura "dal" e non "al"
087500100408       //?imposto "al" = data del giorno
087600100408       IF  V01chd > 0 and V01cha = 0;
087700100408         w01cha   = %dec(%date());
087800100408         data_eur = %date(w01cha:*iso);
087900100408         v01cha   = %dec(data_eur);
088000100408       ENDIF;
088100100430       IF  V01chd = 0 and V01cha > 0 and v01ace = 'C' ;
088200100430         ErrMessage  = *on;
088300100430         ErrGenerico = *on;
088400100430         PosCurChd   = *on;
088500100430         V1Dmsg      = $Msg(15);
088600100430         leavesr;
088700100430       ENDIF;
088800090717
088900090717         // Controllo se data congruenti
089000090717         if     w01chd > w01cha;
089100090717               errMessage  = *on;
089200090717               errGenerico = *on;
089300090717               PosCurCha   = *on;
089400090717               V1Dmsg = $Msg(06);
089500090717               leavesr;
089600090717         endif;
089700100408
089800100623       //?Se data chiusura "dal" impostata e anche data avvio "dal"
089900100623       //?la data chiusura non può essere inferiore alla data avvio
090000100408       IF  v01apd > 0 and v01chd > 0 and
090100100408           w01chd < w01apd;
090200100408         ErrMessage  = *on;
090300100408         ErrGenerico = *on;
090400100408         PosCurCha   = *on;
090500100408         V1Dmsg      = $Msg(11);
090600100408         leavesr;
090700100408       ENDIF;
090800100408
090900100408       //?Se non impostata nessuna data imposto io in automatico
091000100623       //?la data avvio "al" = alla data del giorno
091100100408       //?in questo modo posso fare una selezione di trattative
091200100408       //?anche senza impostare niente nella videata
091300100408       IF  V01apa = 0 and v01cha = 0;
091400100504        // w01apa   = %dec(%date());
091500100504         w01apa   = 20391231;
091600100408       ENDIF;
091700100408
091800100623       //?Stato trattative Avviate, Chiuse, Entrambe
091900100408       //?Se richieste date chiusura posso solo richiedere le trattative chiuse
092000100430       IF  V01ace <> 'C' and (V01chd > 0 or V01cha >0);
092100100408         ErrMessage  = *on;
092200100408         ErrGenerico = *on;
092300100408         PosCurAce   = *on;
092400100408         V1Dmsg      = $Msg(12);
092500100408         leavesr;
092600100408       ENDIF;
092700100430
092800100430       //?Se richieste trattative chiuse impostare le date chiusura
092900100430       IF  V01ace =  'C' and V01chd = 0 and V01cha = 0;
093000100430         ErrMessage  = *on;
093100100430         ErrGenerico = *on;
093200100430         PosCurchd   = *on;
093300100430         V1Dmsg      = $Msg(15);
093400100430         leavesr;
093500100430       ENDIF;
093600110915
093700110915       // verifico: se sono utente di SEDE devo parzializzare per almeno
093800110915       // ragione sociale , codice commerciale o numero trattativa
093900141111       //?solo de dato enter, se F4 o F7 o F8 vado avanti senza controlli
094000141111       IF  dsp_aid <> c_F04 and dsp_aid <> c_F07 and  dsp_aid <> c_F08;
094100110915       If sede and v01nrv = 0 and v01rag = *blanks and (v01com = *blanks
094200110915                   or v01com = *zeros) ;
094300110915         ErrMessage  = *on;
094400110915         ErrGenerico = *on;
094500110915         PosCurNrv   = *on;
094600110915         V1Dmsg      = $Msg(20);
094700110915         leavesr;
094800110915       ENDIF;
094900120201       ENDIF;
095000090717
095100080206       ENDSR;
095200080206
095300090421       //--------------------------------------------------------------
095400080206       //?Gestione videata S01
095500080206       //--------------------------------------------------------------
095600080206       BEGSR GesS01;
095700080207
095800080207         // Inizializzazione videata
095900080207         if  $InzS01 = *on;
096000080207            exsr InzS01;
096100080207            $InzS01  = *off;
096200080207         endif;
096300080207
096400080207         // Posizionamento cursore
096500080207         if  C01csr  > *zero;
096600080207           C01rcd = C01csr;
096700080207         else;
096800080207         // Se non è stato caricato nulla si riemette la videata D01
096900080207           C01rcd = 1;
097000080207           $Video = 'D1';
097100080207           errMessage  = *on;
097200080207           errGenerico = *on;
097300090717           PosCurNrv   = *on;
097400090319           V1Dmsg = $Msg(07);
097500080207           leavesr;
097600080207         endif;
097700080207
097800080207         // Emissione Testata e Piede con tasti funzionali abilitati
097900090717           write  TA87T01;
098000090717           write  TA87P01;
098100090429
098200080207
098300080207         // Emissione videata
098400090717         exfmt  TA87C01;
098500080207         reset errMessage;
098600080207         reset errGenerico;
098700080207         clear V1Dmsg;
098800080207
098900080207         SELECT;
099000080207
099100080207         // - F3=Fine
099200080207           WHEN  dsp_aid = c_F03;
099300080207             exsr F03D01;
099400080207
099500080207         // - F12=Ritorno
099600080207           WHEN  dsp_aid = c_F12;
099700080207             exsr F12S01;
099800090319
099900080207         // - Roll-Up
100000080207           WHEN  dsp_aid = c_RollUp;
100100080207
100200080207         // Invio
100300080207           OTHER;
100400080207             exsr OpzS01;
100500080207             if  errGenerico = *on;
100600080207               leavesr;
100700080207             endif;
100800080207
100900080207         ENDSL;
101000080207
101100080207       ENDSR;
101200080207
101300080207       //--------------------------------------------------------------
101400080207       //?Inizializzazione videata S01
101500080207       //--------------------------------------------------------------
101600080207       BEGSR InzS01;
101700100723
101800100723       // - Memorizzazione codici agenti elaborabili
101900100726       //   (agenti che hanno come unificante quello inserito)
102000100723       clear  wCmm_ds;
102100100723       if  V01com > *zero;
102200100723         exsr  sr_InschTab01;
102300100723       endif;
102400080207
102500090508       // Pulizia subfile
102600090508         exsr pulsfl1;
102700090508
102800090717         $where = *off;
102900081017
103000080207       // Caricamento dei dati da presentare nel sfl
103100080207         $EoF = *off;
103200090720         //  Preparo la stringa sql
103300090717           StringaSql = 'select * from tivis05l ' ;
103400090717
103500090717
103600090717         // - se è stato selezionato il commerciale
103700090720           If   V01com > *zeros   ;
103800090717                exsr SR_where     ;
103900090717                 StringaSql = %trim(Stringasql) +
104000100726                 ' VIScmm in (' + %trimr(wCmm_ds) + ')';
104100100723               //' digits(viscmm)  = ' + v01com ;
104200090717           endif ;
104300090717
104400100408         //?Trattative chiuse
104500100408           If    v01ace = 'C';
104600090717                 exsr SR_where     ;
104700090717                 StringaSql = %trim(Stringasql) +
104800100408                   ' visdch between ' + %editc(w01chd:'X') +
104900100408                   ' and ' + %editc(w01cha:'X');
105000100623           //?con anche range di date di avvio trattativa
105100100408                 IF  w01apd > 0;
105200100408                   stringasql +=
105300100408                   ' and visdat between ' + %editc(w01apd:'X') +
105400100408                   ' and ' + %editc(w01apa:'X');
105500100408                 ENDIF;
105600090717           endif ;
105700090720
105800100623         //?Trattative avviate o tutte
105900100408           If    v01ace <> 'C' ;
106000090720                 exsr SR_where     ;
106100090720                 StringaSql = %trim(Stringasql) +
106200100408                   ' visdat between ' + %editc(w01apd:'X') +
106300100408                   ' and ' + %editc(w01apa:'X');
106400100623           //?se richieste solo le avviate
106500100408                   IF  v01ace = 'A';
106600100408                     stringasql += ' and visdch = 0 ';
106700100408                   ENDIF;
106800090720           endif ;
106900100312
107000090717
107100090720        // imposto l'ordinamento per ragione sociale
107200090720
107300090720          stringasql = %trim(stringasql) + ' order by visrag +
107400090720          for fetch only';
107500090323
107600090323
107700090323         exec sql prepare S1 from :StringaSql;
107800090323
107900090323         exec sql declare C1 cursor for S1;
108000090323
108100090424         exec sql open C1 ;
108200090424
108300080213         exsr sr_ReadRec;
108400080207
108500100705         dow  $EoF   = *off and
108600100705              S01nrr < *hival;
108700080207           exsr RollUpS01;
108800080207         enddo;
108900080207
109000090424         exec sql close C1 ;
109100090625
109200080207       // Impaginazione subfile
109300080207       // -> forza l'impaginazione sul 1° rec. del subfile
109400080207         if S01nrr > *zero;
109500080207           C01rcd  = 1;
109600080207           C01csr  = 1;
109700080207         else;
109800080207           clear C01rcd;
109900080207         endif;
110000080207
110100090508
110200080207       ENDSR;
110300090717       //--------------------------------------------------------------
110400090717       //?Routine che imposta WHERE oppure AND nella stringa sql
110500090717       //--------------------------------------------------------------
110600090717       BEGSR SR_where ;
110700090717
110800090717       // Pulizia subfile
110900080207
111000090717       If $where = *off ;
111100090717            StringaSql = %trim(Stringasql) + ' where ' ;
111200090717            $where = *on ;
111300090717       else ;
111400090717            StringaSql = %trim(Stringasql) + ' and  ' ;
111500090717       endif ;
111600090717
111700090717       ENDSR;
111800100723
111900100723       //--------------------------------------------------------------
112000100723       //?Intabellamento commerciali gestione trattativa elaborabili
112100100723       //--------------------------------------------------------------
112200100723       BEGSR  sr_InschTab01;
112300100723
112400100723         clear  $Cmm;
112500100723         clear  xx;
112600100723
112700130806         StringaSql = 'select CMMcod from AZCMM00F +
112800130806                        where substr(digits(CMMcod), 4, 4) not in +
112900100726                              (''0000'', ''0999'') +
113000130806                          and ( CMMcod = ' + WWWcom + ' or +
113100130806                                CMMuni = ' + WWWcom + ' ) +
113200100723                          for fetch only';
113300100723
113400100723         exec sql   prepare S2   from :StringaSql;
113500100723         exec sql   declare C2   cursor for S2;
113600100723         exec sql   open C2 ;
113700100723
113800130806         exec sql   fetch C2     into :CMMcod;
113900100723
114000100723         DoW  SqlCode = *zero;
114100100723
114200100723           if  xx > *zero;
114300100723             $Cmm(xx) = %trim( $Cmm(xx) ) + ',';
114400100723           endif;
114500100723
114600100723           xx += 1;
114700130806           $Cmm(xx) = %editc( CMMcod : 'X' );
114800100723
114900130806           exec sql   fetch C2   into :CMMcod;
115000100723
115100100723         EndDo;
115200100723
115300100723         exec sql   close C2;
115400100723
115500100723       ENDSR;
115600090717
115700090508       //--------------------------------------------------------------
115800090508       //?Pulizia SFL01
115900090508       //--------------------------------------------------------------
116000090508       BEGSR Pulsfl1;
116100090508
116200090508       // Pulizia subfile
116300090508         SflDsp_N    = *on;
116400090508         SflDspCtl_N = *on;
116500090717         write  TA87C01;
116600090508         SflDspCtl_N = *off;
116700090508         SflEnd      = *off;
116800090508
116900090508
117000090508         clear W_SflPag;
117100090508         clear C01rcd;
117200090508         clear C01csr;
117300090508         clear S01nrr;
117400090508         clear V1Dmsg;
117500090508         errMessage  = *off;
117600090508         errGenerico = *off;
117700090508         WindDspF    = IndDspF;
117800090508         reset WindDspFb;
117900090508         IndDspF     = WindDspF;
118000090508
118100090508       ENDSR ;
118200090508
118300080207       //--------------------------------------------------------------
118400090507       //?Caricamento di tutto il sfl
118500080207       //--------------------------------------------------------------
118600080207       BEGSR RollUpS01;
118700080207
118800090507         clear  s01nrr;
118900080207         SflNxtChg = *off;
119000080207
119100080207         // Ciclo di caricamento dati nel sfl / lettura rec. successivo
119200090507         DOW  $EoF   = *off  and
119300080208              S01nrr < *hival;
119400080207
119500080207         // - Caricamento dati nel record del subfile
119600080213           exsr  RieS01;
119700090601           If $recok = *on ;
119800090601              S01nrr += 1;
119900090720              write TA87S01;
120000100705              If s01nrr = *hival ;
120100100705               errMessage  = *on;
120200100705               errGenerico = *on;
120300100705               V1Dmsg = $Msg(16);
120400100705               leave;
120500100705              endif ;
120600090601           endif ;
120700080207
120800080207         // - Lettura record successivo nell'archivio
120900080213           exsr sr_ReadRec;
121000080207
121100080207         ENDDO;
121200080207
121300080207         // Visualizzazione del SFL (se ci sono dati)
121400080207         SflDsp_N = (S01nrr <= *zeros);
121500080207
121600080207         // Attivazione (eventuale) del SFLEND
121700090507         SflEnd  =  *on;
121800080207
121900080207         // Posizionamento cursore al 1° rcd della pagina
122000080207         if  S01nrr > *zero;
122100090507         //  wPag   = S01nrr / C_SflPag;
122200090507         //  wRig   = S01nrr - (C_SflPag * wPag);
122300090507         //  C01rcd = wPag * C_SflPag;
122400090507         //  if  wRig > *zeros;
122500090507         //    C01rcd = C01rcd + 1;
122600090507         //  else;
122700090507         //    C01rcd = C01rcd - C_SflPag + 1;
122800090507         //  endif;
122900090507             c01rcd = 1;
123000080207         else;
123100080207           clear  C01rcd;
123200080207         endif;
123300080207
123400080207         C01csr = C01rcd;
123500080207
123600080207       ENDSR;
123700080213
123800080213       //--------------------------------------------------------------
123900090323       //?Lettura record successivo
124000080213       //--------------------------------------------------------------
124100080213       BEGSR sr_ReadRec;
124200090323
124300090323         $RecOK  = *off;
124400090323
124500090720         exec sql Fetch C1 into :tivisds;
124600090323
124700090323         if  Sqlcod = 100 ;
124800090323              $Eof  = *on;
124900090323         endif;
125000090323         If  Sqlcod = 0 ;
125100090323             $RecOK  = *on;
125200090323         endif;
125300090407         if  Sqlcod  < 0 ;
125400090407              $Eof  = *on;
125500090407         endif;
125600080213       ENDSR;
125700080207
125800080207
125900080207       //--------------------------------------------------------------
126000080207       //?Caricamento singolo record nel SubFile S01
126100080207       //--------------------------------------------------------------
126200080208       BEGSR RieS01;
126300080207
126400090720         clear  TA87S01;
126500100318
126600100728       // -?controllo se utente abilitato alla trattativa
126700100318         clear tntaa1ds;
126800100318         itaa1nrv  = visnrv;
126900100318         itaa1caut = '§UTEGTC';
127000100318         kpjbu = tntaa1ds ;
127100100318         Tntaa1c (kpjba) ;
127200100318         tntaa1ds = kpjbu ;
127300100318         IF  OTAA1fabi='N' ;
127400100318           eval $RecOK = *off ;
127500100318           leavesr;
127600100318         ENDIF;
127700090720
127800100728         // - verifico se ha delle offerte
127900101222         setll  visnrv  tivof11l ;
128000101222         reade  visnrv  tivof11l ;
128100101222         DOW not %eof(tivof11l) ;
128200100723           If vofeso <> '*' ;
128300100723              s01off = '  OFF' ;
128400100723              if  VOFeso = 'H'   and   VOFdch > *zero;
128500100723                %subst(S01off:1:2) = 'h ';
128600100726                leave ;
128700100723              endif;
128800100723           EndIf ;
128900101222           reade  visnrv  tivof11l ;
129000100723         ENDDO ;
129100090720
129200100504
129300100728         // - Selezioni
129400100728
129500100504         select;
129600100723
129700100728           // - se richieste le sole trattative con le offerte
129800100723           when  V01off = 'S' and S01off = *blank ;
129900100723             eval $RecOK = *off ;
130000100723             leavesr ;
130100100728           // - se richieste le sole trattative senza  offerte
130200100723           when  V01off = 'N' and  S01off <> *blank ;
130300100723             eval $RecOK = *off ;
130400100723             leavesr ;
130500100723
130600100728           // - se richieste le sole trattative fittizie
130700100504           when  V01ffz = 'S' and VISffz = *blank;
130800100504             eval $RecOK = *off ;
130900100504             leavesr ;
131000100728           // - se richieste le sole trattative NON fittizie
131100100504           when  V01ffz = 'N' and VISffz <> *blank;
131200100504             eval $RecOK = *off ;
131300100504             leavesr ;
131400100723
131500100728           // - verifico se è stata richiesta parzializzazione per
131600100723           //   ragione sociale
131700100723           when  V01rag <> *blank  and
131800100723                 %scan( %trimr(V01rag) : VISrag ) = *zero;
131900100723             $RecOK  = *off;
132000100723             leavesr ;
132100100723
132200100504         endsl ;
132300080211
132400100723         // Imposto gli altri campi del subfile
132500090720
132600101015         dVis01 = VISflo;
132700101015
132800100723         H01ffz = VISffz;         // - hidden
132900100728         S01rag = VISrag;
133000100728         if  VISksc > *zero;
133100100728           S01ksc = %editc( VISksc : 'Z' );
133200100728         else;
133300100802           evalr S01ksc = %subst( %editc( VIScmm : 'Z' ) : 1 : 3 );
133400100728         endif;
133500100728         data_eur = %date(VISdat:*iso);
133600100915         //S01dta = %dec(data_eur);
133700100915         S01dta = ( ( %subdt(Data_Eur : *days)  * 10000 ) +
133800100915                    ( %subdt(Data_Eur : *months) * 100 ) +
133900100915              ( %int( %subst( %editc(VISdat : 'X') : 3 : 2 ) ) ) );
134000100728         if  VISffz <> 'S';
134100100802           S01ftv  = VIStpv + '  ' + VISffz;
134200100728         else;
134300100802           S01ftv = '   F';
134400100728         endif;
134500100728         S01esi = VISesi;
134600101015         S01esi01 = dVIS01.§VISesito;
134700100728
134800100728         S01nrv = VISnrv;
134900100728         S01cmm = VIScmm;
135000100723
135100100728         // -?decodifica commerciale
135200130806         chain  (VIScmm)  AZCMM000;
135300130806         if  %found(AZCMM01L);
135400130806           S01cmmD = CMMdes;
135500100723         endif;
135600090720
135700080207       ENDSR;
135800080207
135900080207       //--------------------------------------------------------------
136000080207       //?Gestione tasto funzionale F12 da videata S01
136100080207       //?F12=Ritorno
136200080207       //--------------------------------------------------------------
136300080207       BEGSR F12S01;
136400080207
136500080207         // Ritorno alla videata D01
136600080207         $Video = 'D1';
136700080207
136800080207       ENDSR;
136900080207
137000080207       //--------------------------------------------------------------
137100080207       //?Gestione opzioni subfile
137200080207       //--------------------------------------------------------------
137300080207       BEGSR OpzS01;
137400080207
137500090720         readc TA87S01;
137600080207
137700090720         DOW  NOT  %eof(TNTA87D);
137800080207
137900080207           SflNxtChg = *off;
138000080207           WindDspF  = IndDspF;
138100080207           reset WindDspFb;
138200080207           IndDspF   = WindDspF;
138300080207           C01rcd    = S01nrr;
138400080207
138500080207           SELECT;
138600080207
138700080207             // - Nessuna opzione
138800080208             WHEN  S01opz  = *blank;
138900080208
139000110915             // - SOLO opzione 5 IN CASO di utenete di sede
139100110915             WHEN  S01opz <> '5' and Sede ;
139200110915                   errMessage  = *on;
139300110915                   errGenerico = *on;
139400110915                   PosCurOpz   = *on;
139500110915                   S01opz = 'E' ;
139600110915                   V1Dmsg = $Msg(19);
139700110915
139800080208             // - Precedente segnalazione di errore
139900080208             WHEN  S01opz  = 'E';
140000080208               clear S01opz;
140100080207
140200090514             // - G = Gestione
140300090514             WHEN  S01opz  = 'G';
140400100312             //?controllo se utente abilitato alla trattativa
140500100312               clear TNTAA1DS;
140600100312               ITAA1caut = '§utegtc';
140700100318               ITAA1nrv  = S01nrv;
140800100312               kpjbu     = TNTAA1DS;
140900100312               tntaa1c (kpjba);
141000100312               TNTAA1DS = kpjbu;
141100100312               IF  OTAA1fabi = 'N';
141200100312                 ErrMessage  = *on;
141300100312                 ErrGenerico = *on;
141400100312                 PosCurOpz   = *on;
141500100719                 S01opz = 'E' ;
141600100312                 V1Dmsg      = $Msg(10);
141700100312               ELSE;
141800090514
141900090720                     exsr Calltnta88  ;
142000090623                     $inzs01 = *on ;
142100100312
142200100312               ENDIF;
142300090514
142400090514             // - 5 = Visualizzazione
142500090514             WHEN  S01opz  = '5';
142600090720                exsr Calltnta88  ;
142700090514
142800100719             // - 4 = Annulla
142900100719             WHEN  S01opz  = '4';
143000100719             // verifico se visita fittizia altrimenti errore
143100100722               IF  H01ffz <> 'S'  ;
143200100719                 ErrMessage  = *on;
143300100719                 ErrGenerico = *on;
143400100719                 PosCurOpz   = *on;
143500100719                 V1Dmsg      = $Msg(17);
143600100719               ELSE;
143700100719                 exsr Sr_AnnTratt;
143800100719               ENDIF;
143900100719
144000080207             // - ? = Opzione NON valida
144100080207             OTHER;
144200080207               errMessage  = *on;
144300080207               errGenerico = *on;
144400080207               PosCurOpz   = *on;
144500090720               V1Dmsg = $Msg(09);
144600080207
144700080207           ENDSL;
144800080207
144900080207           // Aggiornamento sfl
145000080207           select;
145100080207             when  errMessage  = *on;
145200080207               SflNxtChg = *on;
145300080207               C01csr    = C01rcd;
145400080207             when  errGenerico = *on;
145500080208               C01csr    = C01rcd;
145600080208               clear  S01opz;
145700080207             other;
145800080207               C01csr    = C01rcd;
145900080208               clear  S01opz;
146000080207           endsl;
146100080207
146200090720           update TA87S01;
146300080207
146400080208           if  errMessage = *on  or  errGenerico = *on;
146500080207             leave;
146600080207           endif;
146700080207
146800090720           readc TA87S01;
146900080207
147000080207         ENDDO;
147100100719
147200100720           if  errMessage = *off and errGenerico = *off;
147300100720               $inzs01 = *on ;
147400100720           endif;
147500080207
147600080207       ENDSR;
147700080207
147800090611       //--------------------------------------------------------------
147900090720       //?Richiamo tnta88r Gestione trattative commerciali
148000090611       //--------------------------------------------------------------
148100090720       BEGSR Calltnta88 ;
148200090611
148300090723         clear tnta88ds ;
148400090723       // Se si tratta di manutenzione attivo il commit altrimenti no
148500090723         if s01opz = 'G' ;
148600090723            ita88cmt = 'S';
148700090723         endif ;
148800090723         ita88fle = s01opz ;
148900090723         ita88nrv = s01nrv ;
149000100302         If v01chi <> *blanks ;
149100100302            ita88com = %dec(v01chi:7:0) ;
149200100302            ita88comd = v01chid ;
149300100302         Endif ;
149400090723         Tnta88r ( kpjba : tnta88ds );
149500090723
149600090611       endsr;
149700100719
149800100719       //--------------------------------------------------------------
149900100719       //?Richiamo tnta16r Annullamento Trattativa
150000100719       //--------------------------------------------------------------
150100100719       BEGSR Sr_AnnTratt;
150200100719
150300100719       // emetto la videata di richiesta conferma di annullamento
150400100719
150500100719         clear w02conf ;
150600100719         W02RAG = S01RAG ;
150700100722         IF  S01OFF <> *blank;
150800100719             Pres_offerte = *on ;
150900100719         else ;
151000100719             Pres_offerte = *off ;
151100100719         endif ;
151200100719
151300100719         $fine2 = *off ;
151400100719
151500100719         Dow $fine2 = *off ;
151600100719         // Emissione Finestra
151700100719             exfmt  TA87W02;
151800100719
151900100719         SELECT;
152000100719
152100100719         // - F06=Conferma
152200100719           WHEN  dsp_aid = c_F06;
152300100719             If W02conf = 'S' ;
152400100719                clear tnta16ds ;
152500100719
152600100719                ita16fcmt = '1' ;
152700100719                ita16cmt = 'S' ;
152800100719                ita16nrv = s01nrv ;
152900100722                ita16ffz = H01ffz ;
153000100719                tnta16c (Kpjba:tnta16ds:esi_ta16c) ;
153100100719
153200100719                If ota16err = 'E' or esi_ta16c = 'E';
153300100719                   ErrMessage  = *on;
153400100719                   ErrGenerico = *on;
153500100719                   W02msg      = $Msg(18);
153600100719                else ;
153700100719                   $fine2 = *on ;
153800100719                endif ;
153900100719
154000100719             endif ;
154100100719
154200100719         // - F12=Ritorno
154300100719           WHEN  dsp_aid = c_F12;
154400100719                $fine2 = *on ;
154500100719
154600100719         ENDSL;
154700100719       ENDDO;
154800100719
154900100719
155000100719       endsr;
155100080206       //--------------------------------------------------------------
155200080206       //?Operazioni finali.
155300080206       //--------------------------------------------------------------
155400080206       BEGSR RoutEnd;
155500080206
155600080206         *inLR = *on;
155700080206         return;
155800080206
155900080206       ENDSR;
156000080206
156100080206      /end-free
156200080206       //--------------------------------------------------------------
156300080206       //?Schiere a tempo di compilazione.
156400080206       //--------------------------------------------------------------
156500080206
156600080206** - $MSG -------------------------------------------------------------------*
156700100211Utente non abilitato alla funzione                                             1
156800090717Non è stato selezionato nessun cliente                                         2
156900090717Numero trattativa insistente                                                   3
157000100211Codice commerciale errato o non in gestione all'utente                         4
157100090717Data errata                                                                    5
157200090717Data al deve essere maggiore o uguale a data dal                               6
157300090717Non è stata trovato nessuna Trattativa                                         7
157400100623Se richieste le sole trattative avviate non parzializzare per data chiusura    8
157500090720Opzione errata                                                                 9
157600100211Utente non abilitato alla trattativa                                          10
157700100623Incongruenza tra le date di AVVIO a quelle di CHIUSURA trattativa             11
157800100408Se impostato range di date CHIUSURA richiedere solo le trattative CHIUSE      12
157900110118.....Libero.....                                                              13
158000100428Manca Potenziale sul cliente impossibile aprire trattativa                    14
158100100430Se richieste trattative CHIUSE impostare range di date CHIUSURA               15
158200100705Raggiunta l'ampiezza massima del subfile                                      16
158300100719Non si può annullare una trattativa se non è fittizia                         17
158400100719Errore in fase di annullamento                                                18
158500110915Opzione non valida. E' possibile solo la VISUALIZZAZIONE                      19
158600110915Parzializzare o per Ragione Sociale o numero trattativa o commerciale         20
