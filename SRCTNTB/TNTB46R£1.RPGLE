000100071207      *PARMS OPTION(*NOXREF) DFTACTGRP(*NO) ACTGRP(QILE)
000200101103     /*PRM dftactgrp(*no)
000300101103     /*END
000400090306
000500090306       //--------------------------------------------------------------
000600090306       //?GESTIONE TABELLA "LAC" - Laguna: clienti per ritorno immagini?
000700121213       //?                       + Tabella LA2 a richiesta           ?
000800090306       //--------------------------------------------------------------
000900110804
001000110804      *?  ATTENZIONE!!  ?
001100110804      *?    Questa tabella è utilizzata anche dal pgm TNTA61R  ?
001200110804      *?    'Interrogazione abilitazioni clienti'              ?
001300110804      *?    In caso di aggiunta/modifica campi alla tabella    ?
001400110804      *?    verificare anche relativo pgm di interrogazione    ?
001500110804      *?    per le abilitazioni clienti --> TNTB462R.          ?
001600090306
001700071206     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001800071206
001900090306       //--------------------------------------------------------------
002000090306       //?Dichiarazione file.                                          ?
002100090306       //--------------------------------------------------------------
002200090306
002300090311     fAZORG01L  if   e           k disk
002400090311
002500071206     fTNTBE01L  Uf A e           k disk
002600090311
002700071206     fTNTB46D   cf   e             workstn
002800071207     f                                     indds(IndDspF)
002900090306     f                                     infds(InfDspF)
003000071207
003100090306       //--------------------------------------------------------------
003200090306       //?Definizione costanti.                                        ?
003300090306       //--------------------------------------------------------------
003400090306
003500090306       // - Tabella in gestione
003600090310     d c_Tab           c                   const('LAC')
003700090306
003800090306       // - Costante per controllo "caratteri solo numerici"
003900090310     d c_Digits        c                   const('0123456789')
004000090306
004100090306       // - Tasti funzionali a video
004200090306     d c_F01           c                   const(x'31')
004300090306     d c_F02           c                   const(x'32')
004400090306     d c_F03           c                   const(x'33')
004500090319     d c_F04           c                   const(x'34')
004600090306     d c_F05           c                   const(x'35')
004700090306     d c_F06           c                   const(x'36')
004800090306     d c_F07           c                   const(x'37')
004900090306     d c_F08           c                   const(x'38')
005000090306     d c_F09           c                   const(x'39')
005100090306     d c_F10           c                   const(x'3A')
005200110713     d c_F11           c                   const(x'3B')
005300090306     d c_F12           c                   const(x'3C')
005400090306     d c_F13           c                   const(x'B1')
005500090306     d c_F14           c                   const(x'B2')
005600090306     d c_F15           c                   const(x'B3')
005700090306     d c_F16           c                   const(x'B4')
005800090306     d c_F17           c                   const(x'B5')
005900090306     d c_F18           c                   const(x'B6')
006000090306     d c_F19           c                   const(x'B7')
006100090306     d c_F20           c                   const(x'B8')
006200090306     d c_F21           c                   const(x'B9')
006300090306     d c_F22           c                   const(x'BA')
006400090306     d c_F23           c                   const(x'BB')
006500090306     d c_F24           c                   const(x'BC')
006600090306     d c_Enter         c                   const(x'F1')
006700090306     d c_RollDown      c                   const(x'F4')
006800090306     d c_RollUp        c                   const(x'F5')
006900071207
007000090306       //--------------------------------------------------------------
007100090306       //?Definizione schiere.                                         ?
007200090306       //--------------------------------------------------------------
007300090306
007400090306       // - Messaggi a video
007500120112     d $Msg            s             78    dim(14)  ctdata  perrcd(1)
007600071207
007700090306       //--------------------------------------------------------------
007800090306       //?Definizione aree dati.                                       ?
007900090306       //--------------------------------------------------------------
008000090306
008100090306       // - Dati utente
008200071207     d §AzUte        e ds                  extname(AZUTE00F)
008300071207     d                                     dtaara
008400071207     d §DatiUte      e ds                  extname(dDatiUte)
008500071207     d                                     dtaara
008600071207
008700090306       //--------------------------------------------------------------
008800090306       //?Definizione strutture dati.                                  ?
008900090306       //--------------------------------------------------------------
009000090306
009100090306       // - Status
009200071207     d Psds           sds
009300071207     d   SDSpgm          *proc
009400071207
009500090306       // - InfDS
009600071207     d InfDspF         ds
009700071207     d  dsp_aid              369    369a                                        AID byte
009800071207
009900090306       // - Indicatori su DspF
010000090311     d IndDspF         ds
010100090306         // - Abilitazione tasti funzionali
010200090323     d  F4Attivo                       n   overlay(IndDspF:04)
010300090311     d  F5Attivo                       n   overlay(IndDspF:05)
010400090311     d  F6Attivo                       n   overlay(IndDspF:06)
010500090311     d  F16Attivo                      n   overlay(IndDspF:16)
010600090306         // - Emissione messaggio di errore
010700090311     d  ErrMessage                     n   overlay(IndDspF:28)
010800090306         // - Protezione campi
010900090311     d  ProtDATAI                      n   overlay(IndDspF:40)
011000120112     d  InserCCM2                      n   overlay(IndDspF:41)
011100090306         // - Posizionamento cursore & segnalazione errore
011200090311     d  PosCurCcm                      n   overlay(IndDspF:50)
011300090311     d  PosCurAut                      n   overlay(IndDspF:51)
011400090311     d  PosCurTpi                      n   overlay(IndDspF:52)
011500090311     d  PosCurDir                      n   overlay(IndDspF:53)
011600090311     d  PosCurSchGg                    n   overlay(IndDspF:54)
011700090311     d  PosCurSch01                    n   overlay(IndDspF:55)
011800090311     d  PosCurSch02                    n   overlay(IndDspF:56)
011900090311     d  PosCurSch04                    n   overlay(IndDspF:57)
012000090311     d  PosCurSch06                    n   overlay(IndDspF:58)
012100090311     d  PosCurSch09                    n   overlay(IndDspF:59)
012200090311     d  PosCurSch11                    n   overlay(IndDspF:60)
012300090311     d  PosCurKsu                      n   overlay(IndDspF:61)
012400090311     d  PosCurMesi                     n   overlay(IndDspF:62)
012500090311     d  PosCurTdta                     n   overlay(IndDspF:63)
012600090311     d  PosCurTad                      n   overlay(IndDspF:64)
012700090311     d  PosCurTadu                     n   overlay(IndDspF:65)
012800090311     d  PosCurKsc                      n   overlay(IndDspF:66)
012900090311     d  PosCurCtr                      n   overlay(IndDspF:67)
013000090311     d  PosCurImp                      n   overlay(IndDspF:68)
013100090311     d  PosCurFimp                     n   overlay(IndDspF:69)
013200090311     d  PosCurFmi                      n   overlay(IndDspF:70)
013300090311     d  PosCurDatai                    n   overlay(IndDspF:71)
013400090311     d  PosCurLnp                      n   overlay(IndDspF:72)
013500090327     d  PosCurTpt                      n   overlay(IndDspF:73)
013600110713     d  PosCurPgm                      n   overlay(IndDspF:74)
013700110713     d  PosCurEle                      n   overlay(IndDspF:75)
013800120112     d  PosCurCCM2                     n   overlay(IndDspF:76)
013900090306         //   - Riemissione videata
014000090311     d  ErrGenerico                    n   overlay(IndDspF:99)
014100071207
014200090306       // - Parametri ricevuti
014300071206     d KPJBA         e ds
014400071217     d TNTB46ds      e ds                  inz
014500121212     d TNTB464ds     e ds                  inz
014600071207
014700090306       // - Reperimento dati utente
014800071207     d TIBS34ds      e ds
014900071207
015000090306       // - Controllo/Decodifica cliente
015100090304     d TIBS69ds      e ds                  qualified  inz
015200090304     d ds_CNACO      e ds                  extname(CNACO00F)
015300090304     d                                     qualified  inz
015400090304     d ds_CNIND      e ds                  extname(CNIND00F)
015500090304     d                                     qualified  inz
015600090304     d ds_CNCLP      e ds                  extname(CNCLP00F)
015700090304     d                                     qualified  inz
015800090304     d ds_FNCLS      e ds                  extname(FNCLS00F)
015900090304     d                                     qualified  inz
016000090306
016100090306       // - Dati record principale della tabella
016200090306     d TNTBEds       e ds                  inz  extname(TNTBE00F)
016300090306     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
016400090306     d                                          prefix(TBX:3)
016500090312
016600090312       // - Parametri per Ricerca/controllo tabelle
016700090312     d TIBS02ds      e ds                  inz
016800090312     d  T02mod       e                     inz('C')
016900090312     d  T02cod       e                     inz(c_Tab)
017000071207
017100090306       // - Tabella "LAC" = Clienti per ritorno immagini
017200071206     d dLAC          e ds                  inz
017300090306       // - Tabella "LAC" del padre per controllo legami
017400090303     d dLACp         e ds                  extname(dLAC) inz
017500121016     d                                     prefix(p_)
017600090303     d  p_SchMm               71     82
017700121016
017800121016       // - Tabella "JDC"
017900121016     d dJDC          e ds                  extname(dJDC) inz
018000121217       // - Tabella "LA2" = Estensione tab.LAC
018100121217     d dLA2          e ds                  inz
018200121016
018300090312       // - Tabella "GED" = Divise area gestionale
018400090312     d dGED          e ds                  inz
018500090312       // - Tabella "GEI" = Importi vari area gestionale
018600090312     d dGEI          e ds                  inz
018700090303
018800090306       // - Mesi in cui è schedulata la mail di promemoria creazione CD
018900090303     d ds_SchMm        ds            12    inz
019000090303     d  V2Csch01                           inz
019100090303     d  V2Csch02                           inz
019200090303     d  V2Csch03                           inz
019300090303     d  V2Csch04                           inz
019400090303     d  V2Csch05                           inz
019500090303     d  V2Csch06                           inz
019600090303     d  V2Csch07                           inz
019700090303     d  V2Csch08                           inz
019800090303     d  V2Csch09                           inz
019900090303     d  V2Csch10                           inz
020000090303     d  V2Csch11                           inz
020100090303     d  V2Csch12                           inz
020200090311
020300090311       // - Struttura di comodo x ctrl mesi di schedulazione
020400090311     d wSchMm_ds       ds            12    inz
020500090311     d  $SchMm                 1     12    dim(12)
020600090319       // - Struttura per passaggio dati ad interrogazione tabella
020700090327     d Param01         ds                  inz
020800090327     d  P01cod                        7  0 inz
020900090327     d  P01ord                        1    inz
021000090327     d  P01ksu                        7  0 inz
021100090327     d  P01ke1                        7    inz
021200090327     d  P01ke2                       15    inz
021300090327     d  P01rit                        1    inz
021400071207
021500090306       //--------------------------------------------------------------
021600090306       //?Definizione procedure usate.                                 ?
021700090306       //--------------------------------------------------------------
021800090306
021900090306       // - Reperimento dati utente
022000090306      /copy gaitrasrc/srcProtoPR,TIBS34R
022100071207
022200090304       // - Ricerca/Controllo tabelle
022300090306      /copy gaitrasrc/srcProtoPR,TIBS02R
022400071207
022500090304       // - Controllo/Decodifica cliente
022600090306      /copy gaitrasrc/srcProtoPR,TIBS69R
022700090319
022800090319       // - Interrogazione tabella LAC
022900090319     d tntb461r        pr                  extpgm('TNTB461R')
023000090319     d  kpjba                              likeds(KPJBA)
023100120112     d  p_SNopz3                      1a   const  options(*nopass)
023200121212
023300121212       // - Gestione tabella LA2
023400121212     d tntb464r        pr                  extpgm('TNTB464R')
023500121213     d  kpjba                              likeds(KPJBA)
023600121212     d  tntb464ds                          likeds(TNTB464DS)
023700071207
023800090306       //--------------------------------------------------------------
023900090306       //?Definizione variabili globali.                               ?
024000090306       //--------------------------------------------------------------
024100090306
024200090306       // - Flags booleani
024300090306     d $InzD01         s               n   inz(*on)
024400090306     d $InzD02         s               n   inz(*on)
024500090306     d $Fine           s               n   inz
024600090923     d $forza          s               n   inz
024700090306     d $CtrMm          s               n   inz
024800090313     d $ByPass         s               n   inz
024900120112     d $Copia          s               n   inz
025000121016     d wForza          s               n   inz
025100121030     d wForzaTPI       s               n   inz
025200121030     d wForzaFMI       s               n   inz
025300121030     d wForzaDIR       s               n   inz
025400071207
025500090306       // - Gestione video
025600071206     d $Video          s              2    inz('D1')
025700090313
025800090313       // - Indici di schiera
025900090313     d xx              s              3  0 inz
026000071207
026100090313       // - Campi di comodo
026200090306     d wDataI          s              6
026300090306     d wDatEla         s              6
026400090306     d wPos1           s              2  0 inz
026500090306     d wPos2           s              2  0 inz
026600090313     d wCount          s              2  0 inz
026700090313     d wSavSch         s                   like(ds_SchMm)  inz
026800090316     d SAVke1          s                   like(TBEke1)    inz
026900090923     d savtad          s                   like(§lactad)
027000090923     d savtadu         s                   like(§lactadu)
027100090923     d savksc          s                   like(§lacksc)
027200090923     d savctr          s                   like(§lacctr)
027300090923     d savimp          s                   like(§lacimp)
027400090923     d savfimp         s                   like(§lacfimp)
027500121220     d woggi           s              8  0 inz
027600071207
027700090306       //--------------------------------------------------------------
027800090306       //?Definizione key-list.                                        ?
027900090306       //--------------------------------------------------------------
028000090306
028100090306       // - File TNTBE01L
028200090306     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
028300090306     d                                     prefix(k_)  inz
028400071206
028500090306       //--------------------------------------------------------------
028600090306       //?Riepilogo indicatori.                                        ?
028700090306       //--------------------------------------------------------------
028800090306       // 01    - RECORD annullato
028900090306       // 02    - acceso IMMISSIONE - spento MANUTENZIONE
029000090306       // 25    - Comodo
029100090306       // 28    - Emissione del messaggio di errore a video
029200090306       // 40    - Protezione campi in prima estrazione
029300090306       // x D01:
029400090306       // 50    - Codice cliente errato o mancante
029500090306       // x D02:
029600120112       // 41    - Copia in D02: V2CCCM non protetto
029700090306       // 53    - Directory per immagini       errata
029800090306       // 54    - Giorno di schedulazione CD   errato
029900090306       // 55    - Selezione mesi obbligatoria
030000090306       // 56    - Selezione del mese di febbraio
030100090306       // 57    - Selezione del mese di aprile
030200090306       // 58    - Selezione del mese di giugno
030300090306       // 59    - Selezione del mese di settembre
030400090306       // 60    - Selezione del mese di novembre
030500090306       // 61    - Codice Padre                 errato
030600090306       // 62    - N. mesi da estrarre          errato
030700090306       // 63    - Tipo data di elaborazione    errato
030800090306       // 64    - Tipo addebito                errato
030900090306       // 65    - Creazione addebito           errato
031000090306       // 66    - Cliente tassazione           errato
031100090306       // 67    - Codice tariffa               errato
031200090306       // 68    - Importo forzato              errato
031300090306       // 69    - Tipo importo forzato         errato
031400090306       // 70    - Flag nome immagine           errato
031500090306       // 71    - Prima elaborazione           errata
031600090306       // 99    - Generico di errore
031700090306       //--------------------------------------------------------------
031800071206
031900071206     c     *Entry        plist
032000071206     c                   parm                    KPJBA
032100071207
032200071207      /free
032300071207
032400071207       // Operazioni iniziali
032500090306       exsr  sr_RoutInz;
032600071207
032700071207       // Gestione video
032800090306       DOW  $Fine = *off;
032900071207         select;
033000090306           when  $Video = 'D1';
033100090306             exsr  sr_GesD01;
033200090306           when  $Video = 'D2';
033300090306             exsr  sr_GesD02;
033400101103           when  $Video = 'W1';
033500101103             exsr  sr_GesW01;
033600071207           other;
033700071207             leave;
033800071207         endsl;
033900071207       ENDDO;
034000071207
034100071207       // Operazioni finali
034200090306       exsr  sr_RoutEnd;
034300071206
034400071212       //--------------------------------------------------------------
034500090306       //?Operazioni iniziali.                                         ?
034600071212       //--------------------------------------------------------------
034700090306       BEGSR  sr_RoutInz;
034800090306
034900090306         *inLR = *on;
035000071207
035100090306         if  kpjbu <> *blank;
035200071217           TNTB46ds = kpjbu;
035300071207         else;
035400090306           clear  TNTB46ds;
035500071207         endif;
035600071217         B46fxx = *blank;
035700071217         B46err = *off;
035800071217         B46msg = *blank;
035900071207
036000090306         // Reperimento dati job
036100090306         exsr  DatiJob;
036200071207
036300090306         // Impostazione nome programma a video
036400071207         V1Tpgm = SDSpgm;
036500090306
036600090306         // Aggancio dati generali della tabella in esame
036700090306         clear  k_TBEcod;
036800090306         k_TBEke1 = *zero;
036900090306         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
037000090306                = c_Tab;
037100090306         clear  k_TBEke2;
037200090306         clear  k_TBElin;
037300090306         k_TBEsif = KNSIF;
037400090306         chain(n)  %kds(k05tntbe01)  TNTBE000;
037500090306         if  not  %found(TNTBE01L);
037600090306           clear  k_TBEsif;
037700090306           chain(n)  %kds(k05tntbe01)  TNTBE000;
037800090306         endif;
037900090306         if  %found(TNTBE01L);
038000090306           xTNTBEds = TNTBEds;
038100090306         else;
038200090306           clear  xTNTBEds;
038300090306         endif;
038400071207
038500090306         // Verifica parametri ricecvuti
038600090306         if  B46ke1  <> *blank;
038700090306           exsr  sr_InzD01;
038800090306           exsr  sr_CtrD01;
038900090306           if  errMessage = *on;
039000071217             B46err = *on;
039100071217             B46msg = V1Dmsg;
039200071217             $Fine  = *on;
039300071217             leavesr;
039400071217           endif;
039500071207           $Video  = 'D2';
039600071207           $InzD02 = *on;
039700071207         endif;
039800121220
039900121220         // Imposto la data di oggi
040000121220         woggi = %dec(%date());
040100071207
040200071207       ENDSR;
040300071206
040400071212       //--------------------------------------------------------------
040500090306       //?Reperimento Dati del job (Utente/Operativi).                 ?
040600071212       //--------------------------------------------------------------
040700090306       BEGSR  DatiJob;
040800071207
040900071207         in(E) §AzUte;
041000071207         if NOT %error;
041100071207           in(E) §DatiUte;
041200071207         endif;
041300090306         if %error or RSut = *blank;
041400071207           clear TIBS34ds;
041500071207           tibs34r(tibs34ds);
041600071207           in §AzUte;
041700071207           in §DatiUte;
041800071207         endif;
041900071207
042000071207       ENDSR;
042100071206
042200071212       //--------------------------------------------------------------
042300090306       //?Gestione videata D01                                         ?
042400071212       //--------------------------------------------------------------
042500090306       BEGSR  sr_GesD01;
042600071207
042700090306         // Inizializzazione videata
042800090306         if  $InzD01 = *on;
042900090306           exsr  sr_InzD01;
043000071207           $InzD01 = *off;
043100071207         endif;
043200071207
043300090306         // Emissione testata
043400090306         write  TN46T01;
043500071207
043600090306         // Emissione videata
043700090306         exfmt  TN46D01;
043800071207         errMessage  = *off;
043900071207         errGenerico = *off;
044000090306         clear  V1Dmsg;
044100071207
044200071207         select;
044300090306           // F3=Fine
044400090306           when  dsp_aid = c_F03;
044500090306             exsr  sr_F03D01;
044600090306           // Enter
044700071207           other;
044800090306             exsr  sr_CtrD01;
044900090306             if  errGenerico;
045000071207               leavesr;
045100071207             endif;
045200071207             $Video  = 'D2';
045300071207             $InzD02 = *on;
045400071207         endsl;
045500071207
045600071207       ENDSR;
045700071206
045800071212       //--------------------------------------------------------------
045900090306       //?Inizializzazione videata D01                                 ?
046000071212       //--------------------------------------------------------------
046100090306       BEGSR  sr_InzD01;
046200071207
046300090306         clear  TN46D01;
046400071207
046500090306         if  B46ke1 = *blank;
046600090306           %subst(V1Cccm : %len(V1Cccm) : 1) = '?';
046700071207         else;
046800090306           V1Cccm = %trimr(B46ke1);
046900071207         endif;
047000071207
047100071207       ENDSR;
047200071206
047300071212       //--------------------------------------------------------------
047400090306       //?Controllo videata D01                                        ?
047500071212       //--------------------------------------------------------------
047600090306       BEGSR  sr_CtrD01;
047700071207
047800090306         IndDspF = *zero;
047900120112         reset  $Copia;
048000071207
048100071207         SELECT;
048200071207
048300120112           // - Ricerca codice cliente
048400090306           WHEN  %scan('?' : V1Cccm) > *zero;
048500120112             clear  Param01;
048600090327             P01ord = '0';
048700090327             KPJBU  = Param01;
048800120112             tntb461r (KPJBA : '3');
048900090327             Param01 = KPJBU;
049000090319
049100090327             if  P01ke1 <> *zero;
049200090327               V1Cccm = P01ke1;
049300090319             endif;
049400120112             if  P01rit <> '3';
049500120112               errGenerico = *on;
049600120112               PosCurCcm   = *on;
049700120112               leavesr;
049800120112             else;
049900120112               $Copia = *on;
050000120112             endif;
050100071207
050200120112           // - Controllo immissione codice cliente
050300090306           WHEN  V1Cccm = *blank  or  V1Cccm  = *zero;
050400071207             errMessage  = *on;
050500071207             errGenerico = *on;
050600071207             PosCurCcm   = *on;
050700071207             V1Dmsg = $Msg(01);
050800071207             leavesr;
050900071207
051000120112           // - Controllo immissione solo caratteri numerici
051100090310           WHEN  %check(c_Digits : V1Cccm) > *zero;
051200071207             errMessage  = *on;
051300071207             errGenerico = *on;
051400071207             PosCurCcm   = *on;
051500071207             V1Dmsg = $Msg(02);
051600071207             leavesr;
051700071207
051800071207         ENDSL;
051900071207
052000090306         // - Controllo esistenza codice cliente in anagrafica clienti
052100120112         //   (SE non richiesta copia: il vero codice cliente verrà
052200120112         //    inserito in D02)
052300120112         If  Not  $Copia;
052400120112           clear  TIBS69ds;
052500120112           tibs69ds.I69kcc = DUTkci;
052600120112           tibs69ds.I69kac = %int(V1Cccm);
052700120112           tibs69ds.I69sif = knsif;
052800120112           tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
052900120112           if  tibs69ds.O69err = *on;
053000120112             errMessage  = *on;
053100120112             errGenerico = *on;
053200120112             PosCurCcm   = *on;
053300120112             V1Dmsg = $Msg(03);
053400120112             leavesr;
053500120112           endif;
053600120112         EndIf;
053700071207
053800090306         // - Verifica esistenza codice cliente in tabella
053900090306         reset  TNTBE000;
054000090310         k_TBEcod = c_Tab;
054100090306         k_TBEke1 = V1Cccm;
054200090306         clear  k_TBEke2;
054300090306         k_TBElin = TBXlin;
054400090306         k_TBEsif = TBXsif;
054500120112
054600120112         If  Not  $Copia;
054700120112
054800120112           chain  %kds(K05tntbe01) TNTBE000;
054900120112           // - - NON ammesso nuovo record per cliente annullato
055000120112           if  NOT  %found(TNTBE01L)  and  ds_CNACO.ACOflg <> *blank;
055100120112             errMessage  = *on;
055200120112             errGenerico = *on;
055300120112             PosCurCcm   = *on;
055400120112             V1Dmsg = $Msg(04);
055500120112             leavesr;
055600120112           endif;
055700120112
055800120112         Else;
055900120112
056000120112           chain(N)  %kds(K05tntbe01) TNTBE000;
056100120112           // - - NON ammessa copia di record inesistente
056200120112           if  NOT  %found(TNTBE01L);
056300120112             errMessage  = *on;
056400120112             errGenerico = *on;
056500120112             PosCurCcm   = *on;
056600120112             V1Dmsg = $Msg(13);
056700120112             leavesr;
056800120112           endif;
056900120112
057000120112         EndIf;
057100071207
057200120112         // - Decodifica cliente
057300090306         if  %found(TNTBE01L);
057400071207           dLAC = TBEuni;
057500130115          // if  Not  $Copia;
057600130115          //   V1Dccm = §LACrag;
057700130115          // else;
057800120112             V1Dccm = %subst(ds_CNACO.ACOrag : 1 : %len(V1Dccm));
057900130115          // endif;
058000071207         else;
058100090306           clear  dLAC;
058200090304           V1Dccm = %subst(ds_CNACO.ACOrag : 1 : %len(V1Dccm));
058300071207         endif;
058400071207
058500071207       ENDSR;
058600071206
058700071212       //--------------------------------------------------------------
058800090306       //?Gestione tasto funzionale F3 da videata D01                  ?
058900090306       //?F3=Fine                                                      ?
059000071212       //--------------------------------------------------------------
059100090306       BEGSR  sr_F03D01;
059200071207
059300090306         // Chiusura del programma
059400071207         $Fine = *on;
059500071217
059600090306         // Restituzione indicazione del tasto funzionale premuto
059700071217         B46fxx = '1';
059800071207
059900071207       ENDSR;
060000071206
060100071212       //--------------------------------------------------------------
060200090306       //?Gestione videata D02                                         ?
060300071212       //--------------------------------------------------------------
060400090306       BEGSR  sr_GesD02;
060500071207
060600090306         // Inizializzazione videata
060700090306         if   $InzD02 = *on;
060800090306           exsr  sr_InzD02;
060900090306           $InzD02 = *off;
061000071207         endif;
061100071207
061200090306         // Emissione testata
061300090306         write  TN46T01;
061400071207
061500090306         // Emissione videata
061600090306         if  B46opz = *blank;
061700090306           exfmt  TN46D02;
061800071217         else;
061900090306           write  TN46D02;
062000090306           exfmt  PROTECT;
062100071217         endif;
062200071207         errMessage  = *off;
062300071207         errGenerico = *off;
062400090306         clear  V1Dmsg;
062500071207
062600071207         SELECT;
062700090306           // F3=Fine
062800090306           WHEN  dsp_aid = c_F03;
062900090306             exsr  sr_F03D01;
063000090319           // F04=Interrogazione Padre
063100090319           WHEN  dsp_aid = c_F04;
063200090319             exsr  sr_F04D02;
063300090306           // F12=Ritorno
063400090306           WHEN  dsp_aid = c_F12;
063500090306             exsr  sr_F12D02;
063600090306           // Enter o F5 o F6 o F16
063700071207           OTHER;
063800090306             // - Controllo dati
063900090306             if  dsp_aid = c_F16;
064000090306               exsr  sr_CtrAnnull;
064100090306             else;
064200090306               exsr  sr_CtrD02;
064300090306             endif;
064400090306             if  errGenerico;
064500071207               leavesr;
064600071207             endif;
064700121212
064800090306             // - Conferma dati:
064900090306             //   F5=Ripristino; F6=Conferma; F16=Annullamento
065000090306             if  dsp_aid = c_F05  or
065100090306                 dsp_aid = c_F06  or
065200090306                 dsp_aid = c_F16;
065300121212
065400090306               exsr  Upd_TNTBE;
065500121213
065600121217           //?Se F6 e immagine da duplicare su altra direcotru devo andare su tabella LA2
065700121217               IF  dsp_aid = c_F06 and V2Cla2 = 'S';
065800121217                 exsr GESLA2;
065900121213                 $Video = 'D2';
066000121213                 $InzD02 = *on;
066100121213                 chain  %kds(K05tntbe01) TNTBE000;
066200121213                 IF  B464err <> '0' and B464err <> *blanks;
066300121213                   ErrMessage  = *on;
066400121213                   ErrGenerico = *on;
066500121213                   V1Dmsg = B464msg;
066600121213                   leavesr;
066700121213                 ENDIF;
066800121213                 unlock TNTBE01L;
066900121213               ENDIF;
067000121212
067100071207               if  NOT  errGenerico;
067200071207                 clear V1Tlav;
067300071207                 $Video  = 'D1';
067400090306                 //$InzD01 = *on;
067500071207               endif;
067600071207             EndIf;
067700071207
067800071207         ENDSL;
067900071207
068000071207       ENDSR;
068100071206
068200071212       //--------------------------------------------------------------
068300090306       //?Inizializzazione videata D01                                 ?
068400071212       //--------------------------------------------------------------
068500090306       BEGSR  sr_InzD02;
068600071207
068700090306         reset  $CtrMm;
068800090313         reset  $ByPass;
068900121016         reset  wForza;
069000121030         reset  wForzaTPI;
069100121030         reset  wForzaFMI;
069200121030         reset  wForzaDIR;
069300071207
069400101103         clear  TB46W01;
069500101103
069600090306         clear  TN46D02;
069700071210
069800090306         IndDspF = *zero;
069900090309
070000090309         // Impostazione indicatori per abilitazione tasti funzionali
070100090309         exsr  Set_Ind_D02;
070200090309
070300120112         // - Codice estrazione (chiave)
070400120112         if  Not  $Copia;
070500120112           V2Cccm = %int(V1Cccm);
070600120112           V2Dccm = V1Dccm;
070700120112         endif;
070800071207
070900090306         IF  %found(TNTBE01L);
071000071207
071100090306           // Impostazione dati di record trovato
071200071217           select;
071300090306             when  B46opz = '5';
071400071217               V1Tlav = 'VISUALIZZA';
071500120112             when  $Copia;
071600120112               V1Tlav = '  COPIA   ';
071700090306             when  TBEatb = *blank;
071800071217               V1Tlav = 'VARIAZIONE';
071900071217             other;
072000071217               V1Tlav = 'ANNULLATO ';
072100071217           endsl;
072200090225
072300090306           // Codice padre
072400090306           V2Cksu = §LACksu;
072500090309           clear  TIBS69ds;
072600090309           clear  ds_CNACO;
072700090309           clear  ds_CNIND;
072800090309           clear  ds_CNCLP;
072900090309           clear  ds_FNCLS;
073000090310           select;
073100090310             when  §LACksu =  V2Cccm;
073200090310               V2Dksu = V2Dccm;
073300090310             when  §LACksu <> *zero;
073400090310               tibs69ds.I69kcc = DUTkci;
073500090310               tibs69ds.I69kac = %int(V2Cksu);
073600090310               tibs69ds.I69sif = knsif;
073700090310               tibs69r(TIBS69ds:ds_CNACO:ds_CNIND:ds_CNCLP:ds_FNCLS);
073800090310               if tibs69ds.O69err = *on;
073900090310                 V1Dmsg = $Msg(03);
074000090310                 errMessage  = *on;
074100090310                 errGenerico = *on;
074200090310                 PosCurKsu   = *on;
074300090310                 leavesr;
074400090310               endif;
074500090310               V2Dksu = ds_CNACO.ACOrag;
074600090310           endsl;
074700090225
074800090225           V2Caut   = §LACaut;
074900090309           V2Ctpi   = §LACtpi;
075000090225
075100090306           // Flag nome immagine
075200090306           V2Cfmi = §LACfmi;
075300090310           reset  TIBS02ds;
075400090303           T02cod = 'NIM';
075500090303           T02ke1 = §LACfmi;
075600090304           TNTBE_RicercaControllo(kpjba : tibs02ds);
075700090306           if  T02err = *blank;
075800090303             V2Dfmi = T02uni;
075900090303           endif;
076000121016
076100121016           V2merge = §LACmerge;
076200121217           V2Cla2  = §LACla2;
076300090225
076400090309           V2Cdir   = §LACdir;
076500090309           V2Ctpt   = §LACtpt;
076600090309           V2Cssr   = §LACssr;
076700090309           V2Clnp   = §LAClnp;
076800090309           V2Cres   = §LACres;
076900090309           V2Crec   = §LACrec;
077000090309           V2Ccsr   = §LACcsr;
077100090309
077200090309           // Prima estrazione
077300090309           clear  wDataI;
077400090309           if  §LACdatai > *zero;
077500090309             %subst(wDataI:1:2) = %subst(%editc(§LACdatai:'X'):5:2);
077600090309             %subst(wDataI:3:4) = %subst(%editc(§LACdatai:'X'):1:4);
077700090310             V2datai = %int(wDataI);
077800090309           endif;
077900090309
078000090309           // Ultima estrazione
078100090309           clear  wDatEla;
078200090309           if  §LACdatela > *zero;
078300090309             %subst(wDatEla:1:2) = %subst(%editc(§LACdatela:'X'):5:2);
078400090309             %subst(wDatEla:3:4) = %subst(%editc(§LACdatela:'X'):1:4);
078500090310             V2datela = %int(wDatEla);
078600090309           endif;
078700090309
078800090309           V2tdta   = §LACtdta;
078900090309           V2mesi   = §LACmesi;
079000071207           V2Cschgg = §LACschgg;
079100071207           V2Csch01 = §LACsch01;
079200071207           V2Csch02 = §LACsch02;
079300071207           V2Csch03 = §LACsch03;
079400071207           V2Csch04 = §LACsch04;
079500071207           V2Csch05 = §LACsch05;
079600071207           V2Csch06 = §LACsch06;
079700071207           V2Csch07 = §LACsch07;
079800071207           V2Csch08 = §LACsch08;
079900071207           V2Csch09 = §LACsch09;
080000071207           V2Csch10 = §LACsch10;
080100071207           V2Csch11 = §LACsch11;
080200071207           V2Csch12 = §LACsch12;
080300090225
080400090306           V2tadu   = §LACtadu;
080500090309           V2Ctad   = §LACtad;
080600090306           V2cksc   = §LACksc;
080700090306           V2cctr   = §LACctr;
080800090306           V2imp    = §LACimp;
080900090306           V2fimp   = §LACfimp;
081000110713           V2pgm    = §LACpgm;
081100110713           V2ele    = §LACele;
081200090306           V2note   = §LACnote;
081300090923
081400090923           *in02 = *off;
081500121217
081600121217           // Cerco i dati della tabella LA2
081700121217           clear dLA2;
081800121217           reset  TIBS02ds;
081900121217           IF  V2Cla2 = 'S';
082000121217             T02cod = 'LA2';
082100121217             T02ke1 = B464ke1;
082200121217             TNTBE_RicercaControllo(kpjba : tibs02ds);
082300121217             IF  T02err = *blank;
082400121217               dLA2 = T02uni;
082500121217             ENDIF;
082600121217           ENDIF;
082700071207
082800071207         ELSE;
082900071207
083000090306           // Impostazione dati di record nuovo
083100071207           V1Tlav = 'IMMISSIONE';
083200090226           V2Cdir = %editc(V2Cccm:'X');
083300090923
083400090923           *in02 = *on;
083500121217           clear dLA2;
083600071207
083700071207         ENDIF;
083800090923
083900090923         // Salvo i dati sensibili per la creazione addebito
084000090923         savtad = v2ctad;
084100090923         savtadu = v2tadu;
084200090923         savksc = v2cksc;
084300090923         savctr = v2cctr;
084400090923         savimp = v2imp;
084500090923         savfimp = v2fimp;
084600090923         $forza = *off;
084700130201
084800130201         // Imposto (se c'è) data ultima variazione
084900130201         clear V2datauv;
085000130201         IF  TBEdtr > 0;
085100130201           V2datauv = %subst(%editc(TBEdtr:'X'):7:2) + '/' +
085200130201                      %subst(%editc(TBEdtr:'X'):5:2) + '/' +
085300130201                      %subst(%editc(TBEdtr:'X'):1:4);
085400130201         ENDIF;
085500071207
085600071207       ENDSR;
085700071217
085800071217       //--------------------------------------------------------------
085900090306       //?Settaggio indicatori nella videata D02 per abilitazione      ?
086000090306       //?  tasti funzionali                                           ?
086100071217       //--------------------------------------------------------------
086200090306       BEGSR  Set_Ind_D02;
086300090319
086400090323         F4Attivo  = *on;
086500090306         F5Attivo  = ((%found(TNTBE01L) and TBEatb <> *blank)
086600071217                                        and B46opz <> '5');
086700071217         F6Attivo  = (NOT F5Attivo and B46opz <> '5');
086800090306         F16Attivo = ((%found(TNTBE01L) and TBEatb = *blank)
086900071217                                        and B46opz <> '5');
087000120112
087100120112         // - Protezione campo chiave SE NON copia
087200120112         InserCCM2 = $Copia;
087300090306
087400120112         // - Se ultima estrazione impostata proteggo la prima estrazione
087500090309         ProtDATAI = (§LACdatela <> *zero);
087600071217
087700071217       ENDSR;
087800090319
087900090319       //--------------------------------------------------------------
088000090319       //?Gestione tasto funzionale F04 da videata D02                 ?
088100090319       //?F04=Interrogazione Padre                                     ?
088200090319       //--------------------------------------------------------------
088300090319       BEGSR  sr_F04D02;
088400090319
088500090319         // Richiamo pgm interrogazione
088600090327         clear  Param01;
088700090327         P01ord = '1';
088800090327         P01ksu = V2Cksu;
088900090327         KPJBU  = Param01;
089000090327         tntb461r (KPJBA);
089100090319
089200090319       ENDSR;
089300090319
089400071217
089500071217       //--------------------------------------------------------------
089600090306       //?Gestione tasto funzionale F12 da videata D02                 ?
089700090306       //?F12=Ritorno                                                  ?
089800071217       //--------------------------------------------------------------
089900090306       BEGSR  sr_F12D02;
090000071217
090100090306         // Ritorno alla videata precedente se NON richiamato
090200090306         if  B46opz = *blank  and
090300090306             B46sif = *blank  and  B46lin = *blank  and
090400090306             B46ke1 = *blank  and  B46ke2 = *blank;
090500071217           clear V1Tlav;
090600071217           $Video = 'D1';
090700090306         // Ritorno al pgm chiamante se richiamato
090800071217         else;
090900071217           B46fxx = '2';
091000071217           $Fine = *on;
091100071217         endif;
091200071217
091300071217       ENDSR;
091400090306
091500090306       //--------------------------------------------------------------
091600090306       //?Controllo "annullabilità" del record (padre senza figli)     ?
091700090306       //--------------------------------------------------------------
091800090306       BEGSR  sr_CtrAnnull;
091900090306
092000090306         IndDspF = *zero;
092100090306         // REimpostazione indicatori per abilitazione tasti funzionali
092200090306         exsr  Set_Ind_D02;
092300090306
092400090306         if  V2Cksu <= *zero;
092500090306           V2Cksu = §LACksu;
092600090306         endif;
092700090306
092800090306         // Se figlio (non padre): fine controlli
092900090306         if  V2Cccm <> V2Cksu;
093000090306           leavesr;
093100090306         endif;
093200090306
093300090306         //?Verifica se padre con figli?
093400090327         clear  dLACp;
093500090316         SAVke1 = TBEke1;
093600090306         setll     %kds(k05tntbe01 : 1)  TNTBE000;
093700090306         reade(n)  %kds(k05tntbe01 : 1)  TNTBE000;
093800090306
093900090306         dow  not  %eof(TNTBE01L);
094000090316           if  TBEatb = *blank   and
094100090316               %editc(V2Cccm : 'X') <> %trimr(TBEke1);
094200090306             dLACp = TBEuni;
094300090316             if  p_§LACksu = V2Cccm;
094400090306               errMessage  = *on;
094500090306               errGenerico = *on;
094600090306               PosCurKsu   = *on;
094700090306               V1Dmsg = 'Record NON cancellabile: trattasi di PADRE +
094800090306                         con figli (vedi ' + %trimr(TBEke1) + ')';
094900090306               leavesr;
095000090306             endif;
095100090306           endif;
095200090306         reade(n)  %kds(k05tntbe01 : 1)  TNTBE000;
095300090306         enddo;
095400090316
095500120112         // - Riaggancio del rec. in manutenzione/copia
095600090316         TBEke1 = SAVke1;
095700090316         reset  TNTBE000;
095800090316         k_TBEcod = c_Tab;
095900120112         k_TBEke1 = %editc( V2Cccm : 'X' );
096000090316         clear  k_TBEke2;
096100090316         k_TBElin = TBXlin;
096200090316         k_TBEsif = TBXsif;
096300090316         chain  %kds(K05tntbe01) TNTBE000;
096400090306
096500090306       ENDSR;
096600071206
096700071212       //--------------------------------------------------------------
096800090306       //?Controllo videata D02                                        ?
096900071212       //--------------------------------------------------------------
097000090306       BEGSR  sr_CtrD02;
097100071207
097200090309         %subst(IndDspF : 50) = *off;
097300120112
097400120112         IF  $Copia;
097500120112
097600120112           V1Cccm = %editc( V2Cccm : 'X' );
097700120112
097800120112           Select;
097900120112
098000120112             // -?Codice estrazione (SE copia)?
098100120112             When  V2Cccm = *zero;
098200120112               errMessage  = *on;
098300120112               errGenerico = *on;
098400120112               PosCurCcm2  = *on;
098500120112               V1Dmsg = $Msg(01);
098600120112               leavesr;
098700120112
098800120112             // -?Controllo / Decodifica?
098900120112             When  tibs69ds.I69kac <> V2Cccm  or
099000120112                   k_TBEke1 <> %editc( V2Cccm : 'X' );
099100120112               clear  TIBS69ds;
099200120112               tibs69ds.I69kcc = DUTkci;
099300120112               tibs69ds.I69kac = V2Cccm;
099400120112               tibs69ds.I69sif = knsif;
099500120112               tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
099600120112               if  tibs69ds.O69err = *on;
099700120112                 errMessage  = *on;
099800120112                 errGenerico = *on;
099900120112                 PosCurCcm2  = *on;
100000120112                 V1Dmsg = $Msg(03);
100100120112                 leavesr;
100200120112               endif;
100300120112               V2Dccm = ds_CNACO.ACOrag;
100400120112               // -?Verifica esistenza in tabella?
100500120112               k_TBEke1 = %editc( V2Cccm : 'X' );
100600120112               chain  %kds(K05tntbe01) TNTBE000;
100700120112               select;
100800120112                 // - -?NON ammesso nuovo record per cliente annullato?
100900120112                 when  NOT  %found(TNTBE01L)  and  ds_CNACO.ACOflg <> *blank;
101000120112                   errMessage  = *on;
101100120112                   errGenerico = *on;
101200120112                   PosCurCcm2  = *on;
101300120112                   V1Dmsg = $Msg(04);
101400120112                   leavesr;
101500120112                 // - -?Impossibile inserimento di record già esistente?
101600120112                 when  %found(TNTBE01L);
101700120112                   errMessage  = *on;
101800120112                   errGenerico = *on;
101900120112                   PosCurCcm2  = *on;
102000120112                   V1Dmsg = $Msg(14);
102100120112                   leavesr;
102200120112               endsl;
102300120112
102400120112           EndSl;
102500120112
102600120112         ENDIF;
102700090313
102800090313         // Verifica se variata la schedulazione dell'estrazione
102900090313         if  $ByPass;
103000090313           $ByPass = (ds_SchMm = wSavSch);
103100090313         endif;
103200090313         wSavSch = ds_SchMm;
103300090225
103400090306         //?Codice padre?
103500090225         select;
103600090306           when  V2Cksu  = *zero;
103700090306             errMessage  = *on;
103800090225             errGenerico = *on;
103900090303             PosCurKsu   = *on;
104000090225             V1Dmsg = $Msg(11);
104100090225             leavesr;
104200090306           when  V2Cksu = V2Cccm;
104300090303             V2Dksu = V2Dccm;
104400090225           other;
104500090327             // - Controllo esistenza codice padre come cliente
104600090309             //   in tabella LAC
104700090303             exsr  sr_Padre;
104800090303             if  errGenerico = *on;
104900090226               leavesr;
105000090225             endif;
105100090303             V2Dksu = p_§LACrag;
105200090226         endsl;
105300101103         //?SE codice padre <> da codice cliente?
105400101103         // ?& sono in immissione?
105500101103         IF  V2Cccm <> V2Cksu   and   *in02;
105600101103           // ?=> chiedo se si vogliono riportare i dati del padre?
105700101103           If  W1Ccpy = *blank;
105800101103             $Video  = 'W1';
105900101103             leavesr;
106000101103           EndIf;
106100101103           // ?& il padre ha già fatto estrazioni?
106200101103           If  p_§LACdatela <> *zero;
106300101103             // ?=> riporto le date del padre, proteggo il campo e non controllo?
106400101103             clear wDataI;
106500101103             %subst(wDataI:1:2) = %subst(%editc(p_§LACdatai:'X'):5:2);
106600101103             %subst(wDataI:3:4) = %subst(%editc(p_§LACdatai:'X'):1:4);
106700101103             V2datai = %int(wDataI);
106800101103             clear wDatEla;
106900101103             %subst(wDatEla:1:2) = %subst(%editc(p_§LACdatela:'X'):5:2);
107000101103             %subst(wDatEla:3:4) = %subst(%editc(p_§LACdatela:'X'):1:4);
107100101103             V2datela = %int(wDatEla);
107200101103             ProtDATAI = *on;
107300101103           EndIf;
107400100521         ENDIF;
107500090311
107600090311         //?Tipo elaborazione?
107700090327         select;
107800090327           // - Obbligatorio
107900090327           when  V2Caut <> 'A'  and  V2Caut <> 'S'  and  V2Caut <> 'N';
108000090327             errMessage  = *on;
108100090327             errGenerico = *on;
108200090327             PosCurAut   = *on;
108300090327             V1Dmsg = 'Tipo elaborazione errato o mancante';
108400090327             leavesr;
108500090327           // - Se codice padre <> da codice cliente
108600090327           //   il tipo elaborazione deve essere uguale
108700090327           when  V2Cccm <> V2Cksu   and   p_§LACaut <> V2Caut;
108800120424           //  errMessage  = *on;
108900120424           //  errGenerico = *on;
109000120424           //  PosCurAut   = *on;
109100120424           //  V1Dmsg = 'Tipo elaborazione +
109200120424           //            <> quello del cod. padre ('
109300120424           //         +  p_§LACaut + ')';
109400120424           //  leavesr;
109500090327         endsl;
109600090327
109700090327         //?Tipo immagine?
109800120918         if  V2Ctpi <> 'TIF'   and   V2Ctpi <> 'JPG' and
109900120918             V2Ctpi <> 'PDF';
110000090327           errMessage  = *on;
110100090327           errGenerico = *on;
110200090327           PosCurTpi   = *on;
110300090327           V1Dmsg = 'Tipo immagine errato o mancante';
110400090327           leavesr;
110500090327         endif;
110600090226
110700090306         //?Flag nome immagine?
110800090310         clear  V2Dfmi;
110900090310         select;
111000090310           // - Obbligatorietà
111100090310           when  V2Cfmi = *blank;
111200090310             errMessage  = *on;
111300090310             errGenerico = *on;
111400090310             PosCurFmi   = *on;
111500090310             V1Dmsg = 'Flag nome immagine obbligatorio';
111600090310             leavesr;
111700090310           // - Ricerca
111800090310           when  %scan('?' : V2Cfmi) > *zero;
111900090310             clear  TIBS02ds;
112000090310             T02mod = 'R';
112100090310             T02cod = 'NIM';
112200090310             T02sif = KNSIF;
112300090310             TNTBE_RicercaControllo(kpjba : tibs02ds);
112400090310             if  T02err = *blank;
112500090310               V2Cfmi = %subst(T02ke1 : 1 : %len(V2Cfmi));
112600090310               V2Dfmi = T02uni;
112700090310               errGenerico = *on;
112800090310               leavesr;
112900090310             else;
113000090310               errMessage  = *on;
113100090310               errGenerico = *on;
113200090310               PosCurFmi   = *on;
113300090310               V1Dmsg = T02msg;
113400090310               leavesr;
113500090310             endif;
113600090310           // - Controllo
113700090310           other;
113800090310             reset  TIBS02ds;
113900090310             T02cod = 'NIM';
114000090310             T02ke1 = V2Cfmi;
114100090310             TNTBE_RicercaControllo(kpjba : tibs02ds);
114200090310             if  T02err <> *blank;
114300090310               errMessage  = *on;
114400090310               errGenerico = *on;
114500090310               PosCurFmi   = *on;
114600090310               V1Dmsg = 'Flag nome immagine errato';
114700090310               leavesr;
114800090310             endif;
114900090310             V2Dfmi = T02uni;
115000090310         ENDSL;
115100090226
115200090306         //?Directory per immagini?
115300090226         select;
115400090306           when  V2Cdir  = *blank;
115500090226             errMessage  = *on;
115600090226             errGenerico = *on;
115700090226             PosCurDir   = *on;
115800090226             V1Dmsg = $Msg(05);
115900090226             leavesr;
116000090316           when  %subst(V2Cdir : 1 : 1) = *blank;
116100090316             errMessage  = *on;
116200090316             errGenerico = *on;
116300090316             PosCurDir   = *on;
116400090316             V1Dmsg = 'Non ammessi spazi vuoti all''inizio del nome +
116500090316                       della directory';
116600090316             leavesr;
116700121217           WHEN  V2Cdir = §LA2dir and V2Cla2 = 'S';
116800121217             errMessage  = *on;
116900121217             errGenerico = *on;
117000121217             PosCurDir   = *on;
117100121217             V1Dmsg = 'La directory deve essere diversa da tab. LA2';
117200121217             leavesr;
117300090306           when  %scan('\':V2Cdir) > *zero;
117400090226             errMessage  = *on;
117500090226             errGenerico = *on;
117600090226             PosCurDir   = *on;
117700090226             V1Dmsg = $Msg(06);
117800090226             leavesr;
117900090306           when  %scan('%':V2Cdir) > *zero   and
118000090306                           V2Caut  = 'A';
118100090226             errMessage  = *on;
118200090226             errGenerico = *on;
118300090226             PosCurDir   = *on;
118400090226             V1Dmsg = $Msg(07);
118500090226             leavesr;
118600090309           when  %scan('&':V2Cdir) > *zero  and
118700090309                           V2Caut  = 'A';
118800090309             errMessage  = *on;
118900090309             errGenerico = *on;
119000090309             PosCurDir   = *on;
119100090309             V1Dmsg = $Msg(07);
119200090309             leavesr;
119300090226         endsl;
119400090309
119500090316         wPos1 = %scan('/' : V2Cdir);
119600090316         DoW  wPos1 > *zero;
119700090316           if  %subst( V2Cdir : wPos1 : 2 ) = '//';
119800090316             errMessage  = *on;
119900090316             errGenerico = *on;
120000090316             PosCurDir   = *on;
120100090316             V1Dmsg = 'Nome directory NON valido: non ammesso il +
120200090316                       doppio "/"';
120300090316             leavesr;
120400090316           else;
120500090316             wPos1 = %scan('/' : V2Cdir : wPos1 + 1 );
120600090316           endif;
120700090316         EndDo;
120800090316
120900090309         wPos1 = %scan('&' : V2Cdir);
121000090309         IF  wPos1 > *zero;
121100090309           if  %subst( V2Cdir : wPos1 : 4 ) <> '&AAA'   and
121200090309               %subst( V2Cdir : wPos1 : 2 ) <> '&M';
121300090309             errMessage  = *on;
121400090309             errGenerico = *on;
121500090309             PosCurDir   = *on;
121600090309             V1Dmsg = 'Ammessi solo "&AAA" e "&M" (come caratteri +
121700090309                       sostituibili)';
121800090309             leavesr;
121900090309           endif;
122000090309           wPos2 = %scan( '&' : V2Cdir : wPos1 + 1 );
122100090309           If  wPos2 > *zero;
122200090309             if  %subst( V2Cdir : wPos2 : 4 ) <> '&AAA'   and
122300090309                 %subst( V2Cdir : wPos2 : 2 ) <> '&M';
122400090309               errMessage  = *on;
122500090309               errGenerico = *on;
122600090309               PosCurDir   = *on;
122700090309               V1Dmsg = 'Ammessi solo "&AAA" e "&M" (come caratteri +
122800090309                         sostituibili)';
122900090309               leavesr;
123000090309             endif;
123100090309           EndIf;
123200090309         ENDIF;
123300090309
123400090309         wPos1 = %scan('&M' : V2Cdir);
123500090309         IF  wPos1 > *zero;
123600090309           wPos2 = %scan( '&M' : V2Cdir : wPos1 + 1 );
123700090309           If  wPos2 > *zero   and
123800090309               %scan( '&M' : V2Cdir : wPos2 + 1 ) > *zero;
123900090309             errMessage  = *on;
124000090309             errGenerico = *on;
124100090309             PosCurDir   = *on;
124200090309             V1Dmsg = 'Ammessi max 2 "&M"';
124300090309             leavesr;
124400090309           EndIf;
124500090309         ENDIF;
124600090226
124700090327         // - Se codice padre <> da codice cliente
124800090306         //   la directory deve coincidere almeno fino alla 2ª subdir
124900090306         IF  V2Cccm <> V2Cksu   and   p_§LACdir <> V2Cdir;
125000090311
125100090311           // - Controllo 1ª directory
125200090311           wPos1 = %scan( '/' : %trim( V2Cdir ) );
125300090311           wPos2 = %scan( '/' : %trim( p_§LACdir ) );
125400090313           If  wPos1 <> wPos2                               OR
125500090313             ( wPos1  > *zero   and   wPos2 > *zero   and
125600090311               %subst( %trim( V2Cdir    ) : 1 : wPos1 ) <>
125700090313               %subst( %trim( p_§LACdir ) : 1 : wPos2 ) )   OR
125800090313             ( wPos1  = *zero   and   wPos2  =  *zero
125900090313                                and   V2Cdir <> p_§LACdir );
126000090311             errMessage  = *on;
126100090311             errGenerico = *on;
126200090311             PosCurDir   = *on;
126300090311             select;
126400090311               when  wPos1 =  *zero   and   wPos2 <> *zero;
126500090316                 V1Dmsg = 'Il padre ha altri rami directory (';
126600090311               when  wPos1 <> *zero   and   wPos2 =  *zero;
126700090311                 V1Dmsg = 'Il padre ha una directory unica (';
126800090311               other;
126900090311                 V1Dmsg = 'Primo ramo directory +
127000090311                           <> da quello del cod. padre (';
127100090311             EndSl;
127200090313             if  %len( %trim( p_§LACdir ) ) <=
127300090311                 %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) ) - 1;
127400090311               V1Dmsg = %trimr( V1Dmsg )
127500090311                      + %trim( p_§LACdir ) + ')';
127600090311             else;
127700090313               wPos1 = %len( V1Dmsg );
127800090313               wPos2 = %len( %trimr( V1Dmsg ) );
127900090311               V1Dmsg = %trimr( V1Dmsg )
128000090311                      + %trim( %subst( p_§LACdir : 1 :
128100090311                          %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) )
128200090311                          - 4 ) )
128300090311                      + '...)';
128400090311             endif;
128500090311             leavesr;
128600090311           EndIf;
128700090311
128800090311           // - Controllo 2ª directory
128900090311           wPos1 = %scan( '/' : %trim( V2Cdir ) : wPos1 + 1 );
129000090311           wPos2 = %scan( '/' : %trim( p_§LACdir ) : wPos2 + 1 );
129100090316           //If  wPos1 <> wPos2                               OR
129200090316           //  ( wPos1  > *zero   and   wPos2 > *zero   and
129300090316           //    %subst( %trim( V2Cdir    ) : 1 : wPos1 ) <>
129400090316           //    %subst( %trim( p_§LACdir ) : 1 : wPos2 ) )   OR
129500090316           //  ( wPos1  = *zero   and   wPos2  =  *zero
129600090316           //                     and   V2Cdir <> p_§LACdir );
129700090316           if  wPos1 = *zero;
129800090316             wPos1 = %len( %trimr( V2Cdir ) );
129900090316           else;
130000090316             wPos1 -= 1;
130100090316           endif;
130200090316           if  wPos2 = *zero;
130300090316             wPos2 = %len( %trimr( p_§LACdir ) );
130400090316           else;
130500090316             wPos2 -= 1;
130600090316           endif;
130700090316           If  wPos1 <> wPos2   OR
130800090316               %subst(V2Cdir : 1 : wPos1) <>
130900090316                 %subst(p_§LACdir : 1 : wPos2);
131000090311             errMessage  = *on;
131100090311             errGenerico = *on;
131200090311             PosCurDir   = *on;
131300090316             //select;
131400090316             //  when  wPos1 =  *zero   and   wPos2 <> *zero;
131500090316             //    V1Dmsg = 'Il padre ha altri rami directory (';
131600090316             //  when  wPos1 <> *zero   and   wPos2 =  *zero;
131700090316             //    V1Dmsg = 'Il padre ha meno rami directory (';
131800090316             //  other;
131900090313                 V1Dmsg = 'Secondo ramo directory +
132000090311                           <> da quello del cod. padre (';
132100090316             //EndSl;
132200090313             if  %len( %trim( p_§LACdir ) ) <=
132300090311                 %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) ) - 1;
132400090311               V1Dmsg = %trimr( V1Dmsg )
132500090311                      + %trim( p_§LACdir ) + ')';
132600090311             else;
132700090311               V1Dmsg = %trimr( V1Dmsg )
132800090311                      + %trim( %subst( p_§LACdir : 1 :
132900090311                          %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) )
133000090311                          - 4 ) )
133100090311                      + '...)';
133200090311             endif;
133300090311             leavesr;
133400090311           EndIf;
133500090311
133600090306         ENDIF;
133700121016
133800121016         //?Immagine con Merge (LDV + DOC)
133900121016         IF  V2merge = 'S';
134000121016           exsr CTR_merge;
134100121016         ENDIF;
134200121016         IF  ErrGenerico;
134300121016           leavesr;
134400121016         ENDIF;
134500090327
134600090327         //?Tipo porto?
134700090327         if  V2Ctpt <> 'F'   and   V2Ctpt <> 'A'   and   V2Ctpt <> 'E';
134800090327           errMessage  = *on;
134900090327           errGenerico = *on;
135000090327           PosCurTpt   = *on;
135100090327           V1Dmsg = 'Tipo porto errato o mancante';
135200090327           leavesr;
135300090327         endif;
135400090311
135500090311         //?LNP per selezione?
135600090311         if  V2Clnp <> *zero;
135700090311           setll  (V2Clnp)  AZORG;
135800090311           if  NOT  %equal(AZORG01L);
135900090311             errMessage  = *on;
136000090311             errGenerico = *on;
136100090311             PosCurLnp   = *on;
136200090311             V1Dmsg = 'Linea di partenza errata';
136300090311             leavesr;
136400090311           endif;
136500090311         endif;
136600090226
136700090309         //?Prima estrazione dal?
136800090310         select;
136900090310           // - Obbligatoria se tipo elaborazione CD
137000090310           when  V2Caut = 'S'   and   V2datai = *zero;
137100090310             errMessage  = *on;
137200090310             errGenerico = *on;
137300090310             PosCurDatai = *on;
137400090310             V1Dmsg = 'Periodo iniziale della prima estrazione +
137500090310                       obbligatorio';
137600090310             leavesr;
137700090310           // - Verifica correttezza
137800090310           when  V2datai <> *zero                                  AND
137900090310              (%subst( %editc( V2datai : 'X' ) : 1 : 2 ) < '01'   or
138000090310               %subst( %editc( V2datai : 'X' ) : 1 : 2 ) > '12'   or
138100090310               %subst( %editc( V2datai : 'X' ) : 3 : 4 ) < '2000' or
138200090310               %subst( %editc( V2datai : 'X' ) : 3 : 4 ) > %char(*year));
138300090310             errMessage  = *on;
138400090310             errGenerico = *on;
138500090310             PosCurDatai = *on;
138600090310             V1Dmsg = 'Periodo iniziale della prima estrazione +
138700090310                       errato';
138800090310             leavesr;
138900090310         endsl;
139000090306         // - Se codice padre <> da codice cliente
139100090309         //   il periodo deve essere uguale
139200100521         if  V2Cccm <> V2Cksu;
139300090310           clear  wDataI;
139400090316           %subst( wDataI : 1 : 2 ) =
139500090316                   %subst( %editc( p_§LACdatai : 'X' ) : 5 : 2 );
139600090316           %subst( wDataI : 3 : 4 ) =
139700090316                   %subst( %editc( p_§LACdatai : 'X' ) : 1 : 4 );
139800090310           if  V2datai <> %int(wDataI);
139900090310             errMessage  = *on;
140000090310             errGenerico = *on;
140100090310             PosCurDatai = *on;
140200090310             V1Dmsg = 'Periodo iniziale della prima estrazione +
140300090310                       <> da quello del cod. padre ('
140400090310                    + %subst(%editc(p_§LACdatai : 'X') : 5 : 2)
140500090310                    + '/'
140600090310                    + %subst(%editc(p_§LACdatai : 'X') : 1 : 4)
140700090310                    + ')';
140800090310             leavesr;
140900090310           endif;
141000090226         endif;
141100090306
141200090309         //?Tipo data da elaborare?
141300090310         select;
141400090327           when  V2tdta <> ' '  and  V2tdta <> 'C'  and  V2tdta <> 'S';
141500090327             errMessage  = *on;
141600090327             errGenerico = *on;
141700090327             PosCurTdta  = *on;
141800090327             V1Dmsg = '"Flag "Data da elaborare" errato';
141900090327             leavesr;
142000090310           // - Tipo data da elaborare obbligatorio
142100090310           when  V2Caut = 'S'   and   V2tdta = *blank;
142200090310             errMessage  = *on;
142300090310             errGenerico = *on;
142400090310             PosCurTdta  = *on;
142500090310             V1Dmsg = '"Data da elaborare" obbligatoria per +
142600090310                       "Tipo Elaborazione" = "S" (schedulata)';
142700090310             leavesr;
142800090310           // - Se codice padre <> da codice cliente
142900090310           //   il tipo data da elaborare deve essere uguale
143000090310           when  V2Cccm <> V2Cksu   and   p_§LACtdta <> V2tdta;
143100090310             errMessage  = *on;
143200090310             errGenerico = *on;
143300090310             PosCurTdta  = *on;
143400090310             V1Dmsg = 'Tipo data da elaborare +
143500090310                       <> da quello del cod. padre ('
143600090310                    + p_§LACtdta + ')';
143700090310             leavesr;
143800090310         endsl;
143900090309
144000090309         //?Periodo da estrarre in mesi?
144100090310         select;
144200090310           // - Periodo da estrarre in mesi obbligatorio
144300090310           when  V2Caut = 'S'   and   V2mesi = *zero;
144400090310             errMessage  = *on;
144500090310             errGenerico = *on;
144600090310             PosCurMesi  = *on;
144700090310             V1Dmsg = '"Periodo da estrarre" obbligatorio per +
144800090310                       "Tipo Elaborazione" = "S" (schedulata)';
144900090310             leavesr;
145000090310           // - Se codice padre <> da codice cliente
145100090310           //   il periodo da estrarre deve essere uguale
145200090310           when  V2Cccm <> V2Cksu   and   p_§LACmesi <> V2mesi;
145300090310             errMessage  = *on;
145400090310             errGenerico = *on;
145500090310             PosCurMesi  = *on;
145600090310             V1Dmsg = 'Periodo da estrarre +
145700090310                       <> da quello del cod. padre ('
145800090310                    + %editc(p_§LACmesi:'X') + ')';
145900090310             leavesr;
146000090310         endsl;
146100090226
146200090309         //?Schedulazione giorno e mesi x preparazione CD?
146300090226         select;
146400090318           // when (V2Caut  =  'A'    or  V2Caut  =  'N')    AND
146500090318           when  V2Caut  =  'A'    AND
146600090311                (V2datai <> *zero  or  V2tdta   <> *blank   or
146700090311                 V2mesi  <> *zero  or  V2Cschgg <> *zero    or
146800090311                 ds_SchMm <> *blank);
146900090311             errMessage  = *on;
147000090311             errGenerico = *on;
147100090311             PosCurAut   = *on;
147200090311             V1Dmsg = 'Schedulazione incompatibile con il +
147300090311                       "Tipo Elaborazione" = ' + V2Caut;
147400090311             leavesr;
147500090318           when  V2Caut = 'S'  and  (V2Cschgg <= *zero);
147600090306             errMessage  = *on;
147700090306             errGenerico = *on;
147800090306             PosCurSchGg = *on;
147900090306             V1Dmsg = 'Schedulazione obbligatoria per +
148000090306                       "Tipo Elaborazione" = "S" (schedulata)';
148100090306             leavesr;
148200090306           when  V2Caut = 'S'  and  (ds_SchMm = *blank);
148300090306             errMessage  = *on;
148400090306             errGenerico = *on;
148500090306             PosCurSch01 = *on;
148600090306             V1Dmsg = 'Indicare almeno un mese per +
148700090306                       "Tipo Elaborazione" = "S" (schedulata)';
148800090306             leavesr;
148900090311         endsl;
149000090226
149100090327         // - Se codice padre <> da codice cliente
149200090306         //   i 2 codici devono avere gli stessi dati di estrazione
149300090306         //   gg e mese e n. mesi da elaborare
149400090309         if  V2Cccm <> V2Cksu;
149500090309           select;
149600090309             when  p_§LACschgg <> V2Cschgg;
149700090226               errMessage  = *on;
149800090226               errGenerico = *on;
149900090303               PosCurSchgg = *on;
150000090309               V1Dmsg = 'Giorno creazione CD +
150100090309                         <> da quello del cod. padre ('
150200090309                      + %editc(p_§LACschgg:'X') + ')';
150300090226               leavesr;
150400090303             when  p_SchMm <> ds_SchMm;
150500090226               errMessage  = *on;
150600090226               errGenerico = *on;
150700090226               PosCursch01 = *on;
150800090309               V1Dmsg = 'Mesi di elaborazione +
150900090309                         <> da quelli del cod. padre ('
151000090309                      + p_SchMm + ')';
151100090226               leavesr;
151200090226           endsl;
151300090226         endif;
151400090309
151500090327         // - Verifica congruenza tra mese di schedulazione e
151600090309         //   mesi di estrazione
151700090309         select;
151800090311           // - Nessun dato inserito
151900090310           when  V2mesi   = *zero    and   V2Cschgg = *zero    and
152000090311                 ds_SchMm = *blank;
152100090311           // - Periodo da estrarre in mesi richiesto
152200090310           when  V2mesi = *zero;
152300090310             errMessage  = *on;
152400090310             errGenerico = *on;
152500090310             PosCurMesi  = *on;
152600090310             V1Dmsg = 'Periodo da estrarre obbligatorio SE inserita +
152700090310                       una schedulazione';
152800090310             leavesr;
152900090311           // - Giorno di schedulazione richiesto
153000090310           when  V2Cschgg = *zero;
153100090310             errMessage  = *on;
153200090310             errGenerico = *on;
153300090310             PosCurSchgg = *on;
153400090311             V1Dmsg = $Msg(08);
153500090310             leavesr;
153600090311           // - Selezione mesi richiesta
153700090311           when  ds_SchMm = *blank;
153800090310             errMessage  = *on;
153900090310             errGenerico = *on;
154000090310             PosCurSch01 = *on;
154100090311             V1Dmsg = $Msg(09);
154200090310             leavesr;
154300090311         endsl;
154400090313
154500090313         // - Conteggio/Controllo mesi schedulati
154600090313         clear  wCount;
154700090311         wSchMm_ds = %triml(ds_SchMm);
154800090313         For  xx = 1  To  %elem($SchMm);
154900090313           if  $SchMm(xx) <> *blank;
155000090313             wCount += 1;
155100090313           endif;
155200090313         EndFor;
155300090313
155400090311         select;
155500090313           // - Errori bloccanti:
155600090313           when   wCount <> *zero   and
155700090313                 (V2mesi <> (12 / wCount));
155800090313             errMessage  = *on;
155900090313             errGenerico = *on;
156000090313             PosCurSch01 = *on;
156100090313             V1Dmsg = 'Incongruenza tra il "periodo da estrarre" +
156200090313                       ed i mesi schedulati';
156300090313           // - Errori bypassabili:
156400090313           when ( (V2mesi =  2   and  ($SchMm(03) = *blank     or
156500090311                                       $SchMm(05) = *blank     or
156600090311                                       $SchMm(07) = *blank     or
156700090311                                       $SchMm(09) = *blank     or
156800090311                                       $SchMm(11) = *blank))   or
156900090313                  (V2mesi =  3   and  ($SchMm(04) = *blank     or
157000090311                                       $SchMm(07) = *blank     or
157100090311                                       $SchMm(10) = *blank))   or
157200090313                  (V2mesi =  4   and  ($SchMm(05) = *blank     or
157300090311                                       $SchMm(09) = *blank))   or
157400090313                  (V2mesi =  6   and   $SchMm(07) = *blank) )  AND
157500090313                 $ByPass =  *off;
157600090313             $ByPass = *on;
157700090309             errMessage  = *on;
157800090309             errGenerico = *on;
157900090310             PosCurSch01 = *on;
158000090313             V1Dmsg = 'Mesi schedulati in maniera irregolare... +
158100090313                       Premere Enter per forzare.';
158200090309             leavesr;
158300090313           when ( (V2mesi =  2  and  ($SchMm(02) <> *blank     or
158400090311                                      $SchMm(04) <> *blank     or
158500090311                                      $SchMm(06) <> *blank     or
158600090311                                      $SchMm(08) <> *blank     or
158700090311                                      $SchMm(10) <> *blank     or
158800090311                                      $SchMm(12) <> *blank))   OR
158900090313                  (V2mesi =  3  and  ($SchMm(02) <> *blank     or
159000090311                                      $SchMm(03) <> *blank     or
159100090311                                      $SchMm(05) <> *blank     or
159200090311                                      $SchMm(06) <> *blank     or
159300090311                                      $SchMm(08) <> *blank     or
159400090311                                      $SchMm(09) <> *blank     or
159500090311                                      $SchMm(11) <> *blank     or
159600090311                                      $SchMm(12) <> *blank))   OR
159700090313                  (V2mesi =  4  and  ($SchMm(02) <> *blank     or
159800090311                                      $SchMm(03) <> *blank     or
159900090311                                      $SchMm(04) <> *blank     or
160000090311                                      $SchMm(06) <> *blank     or
160100090311                                      $SchMm(07) <> *blank     or
160200090311                                      $SchMm(08) <> *blank     or
160300090311                                      $SchMm(10) <> *blank     or
160400090311                                      $SchMm(11) <> *blank     or
160500090311                                      $SchMm(12) <> *blank))   OR
160600090313                  (V2mesi =  6  and  ($SchMm(02) <> *blank     or
160700090311                                      $SchMm(03) <> *blank     or
160800090311                                      $SchMm(04) <> *blank     or
160900090311                                      $SchMm(05) <> *blank     or
161000090311                                      $SchMm(06) <> *blank     or
161100090311                                      $SchMm(08) <> *blank     or
161200090311                                      $SchMm(09) <> *blank     or
161300090311                                      $SchMm(10) <> *blank     or
161400090311                                      $SchMm(11) <> *blank     or
161500090311                                      $SchMm(12) <> *blank))   OR
161600090313                  (V2mesi = 12  and  ($SchMm(02) <> *blank     or
161700090311                                      $SchMm(03) <> *blank     or
161800090311                                      $SchMm(04) <> *blank     or
161900090311                                      $SchMm(05) <> *blank     or
162000090311                                      $SchMm(06) <> *blank     or
162100090311                                      $SchMm(07) <> *blank     or
162200090311                                      $SchMm(08) <> *blank     or
162300090311                                      $SchMm(09) <> *blank     or
162400090311                                      $SchMm(10) <> *blank     or
162500090311                                      $SchMm(11) <> *blank     or
162600090313                                      $SchMm(12) <> *blank)) ) AND
162700090313                 $ByPass =  *off;
162800090313             $ByPass = *on;
162900090310             errMessage  = *on;
163000090310             errGenerico = *on;
163100090311             PosCurSch01 = *on;
163200090313             V1Dmsg = 'Mesi schedulati in maniera irregolare... +
163300090313                       Premere Enter per forzare.';
163400090310             leavesr;
163500090313           // - Controllo giorno del mese (bypassabile)
163600090313           when ( (V2Cschgg > 28  and  V2Csch02 <> *blank)  or
163700090313                  (V2Cschgg > 30  and (V2Csch04 <> *blank
163800090313                                   or  V2Csch06 <> *blank
163900090313                                   or  V2Csch09 <> *blank
164000090313                                   or  V2Csch11 <> *blank)) )
164100090313                and  $CtrMm = *off;
164200090313             $CtrMm = *on;
164300090313             errMessage  = *on;
164400090313             errGenerico = *on;
164500090313             PosCurSch01 = (V2Csch01 <> *blank);
164600090313             PosCurSch02 = (V2Csch02 <> *blank);
164700090313             PosCurSch04 = (V2Csch04 <> *blank);
164800090313             PosCurSch06 = (V2Csch06 <> *blank);
164900090313             PosCurSch09 = (V2Csch09 <> *blank);
165000090313             PosCurSch11 = (V2Csch11 <> *blank);
165100090313             V1Dmsg = %replace( %char(V2Cschgg) : $Msg(10) :
165200090313                                %scan('&g':$Msg(10)) );
165300090313             leavesr;
165400090313         endsl;
165500090226
165600090309         //?Frequenza addebito?
165700090310         select;
165800090327           // - Obbligatoria
165900090327           when  V2tadu <> 'I'   and   V2tadu <> 'J'   and
166000090327                 V2tadu <> 'S'   and   V2tadu <> 'M';
166100090327             errMessage  = *on;
166200090327             errGenerico = *on;
166300090327             PosCurTadu  = *on;
166400090327             V1Dmsg = 'Frequenza addebito errata o mancante';
166500090327             leavesr;
166600090327           // - Se tipo elaborazione CD la creazione addebito non può
166700090313           //   essere mensile o settimanale
166800090310           when  V2Caut = 'S'  and  (V2tadu = 'S'  or  V2tadu = 'M');
166900090310             errMessage  = *on;
167000090310             errGenerico = *on;
167100090310             PosCurTadu  = *on;
167200090310             V1Dmsg = 'Per creazione CD non è possibile addebito +
167300090310                       mensile o settimanale';
167400090310             leavesr;
167500090327           // - Se codice padre <> da codice cliente
167600090327           //   devono avere la stessa frequenza addebito
167700090310           when  V2Cccm <> V2Cksu   and   p_§LACtadu <> V2tadu;
167800090310             errMessage  = *on;
167900090310             errGenerico = *on;
168000090310             PosCurTadu  = *on;
168100090310             V1Dmsg = 'Frequenza addebito +
168200090310                        <> da quella del cod. padre ('
168300090310                    + p_§LACtadu + ')';
168400090310             leavesr;
168500090310         endsl;
168600090309
168700090309         //?Tipo addebito (Creazione TITAS)?
168800090310         select;
168900090327           // - Obbligatorio
169000090327           when  V2Ctad <> 'N'  and  V2Ctad <> 'S'  and  V2Ctad <> 'V';
169100090327             errMessage  = *on;
169200090327             errGenerico = *on;
169300090327             PosCurTad   = *on;
169400090327             V1Dmsg = 'Flag "Creazione TITAS" errato o mancante';
169500090327             leavesr;
169600090327           // - Se "N" non impostare i dati dell'addebito forzato
169700090310           when  V2Ctad = 'N'     and
169800090310                (V2Cksc <> *zero  or  V2Cctr <> *blank
169900090310                                  or  V2imp <> *zero);
170000090310             errMessage  = *on;
170100090310             errGenerico = *on;
170200090310             PosCurTad   = *on;
170300090310             V1Dmsg = 'Se NO addebito non impostare le forzature';
170400090310             leavesr;
170500090327           // - Se codice padre <> da codice cliente
170600090310           //   devono avere lo stesso tipo addebito
170700090310           when  V2Cccm <> V2Cksu   and   p_§LACtad <> V2Ctad;
170800090310             errMessage  = *on;
170900090310             errGenerico = *on;
171000090310             PosCurTad   = *on;
171100090310             V1Dmsg = 'Tipo addebito +
171200090310                       <> da quello del cod. padre ('
171300090310                        + p_§LACtad + ')';
171400090310             leavesr;
171500090310         endsl;
171600090226
171700090311         //?Dati da forzare in tassazione:?
171800090312         //?- Codice cliente / Tariffa?
171900090312         clear  TIBS69ds;
172000090312         if  V2Cksc <> *zero;
172100090312           tibs69ds.I69kcc = DUTkci;
172200090312           tibs69ds.I69kac = %int(V2Cksc);
172300090312           tibs69ds.I69sif = knsif;
172400090312           tibs69r(TIBS69ds : ds_CNACO :
172500090312                              ds_CNIND :
172600090312                              ds_CNCLP :
172700090312                              ds_FNCLS);
172800090312         endif;
172900090312         select;
173000090312           when  tibs69ds.O69err = *on;
173100090312             errMessage  = *on;
173200090312             errGenerico = *on;
173300090312             PosCurKsc   = *on;
173400090312             V1Dmsg = 'Cliente tassazione errato';
173500090312             leavesr;
173600090312           when  V2Cksc <> *zero   and   V2Cctr = *blank;
173700090312             errMessage  = *on;
173800090312             errGenerico = *on;
173900090312             PosCurCtr   = *on;
174000090312             V1Dmsg = 'Codice tariffa obbligatorio SE +
174100090312                       inserito il cliente tassazione';
174200090312             leavesr;
174300090311           // - Non ammessa tariffa senza cliente
174400090311           when  V2Cksc =  *zero   and   V2Cctr <> *blank;
174500090311             errMessage  = *on;
174600090311             errGenerico = *on;
174700090311             PosCurKsc   = *on;
174800090311             V1Dmsg = 'Cliente tassazione obbligatorio SE +
174900090311                       inserita la relativa tariffa';
175000090311             leavesr;
175100090311           // - Ammesso solo un codice tariffa numerico
175200090311           when  V2Cctr <> *blank   and
175300090311                 %check(c_Digits : V2Cctr) > *zero;
175400090311             errMessage  = *on;
175500090311             errGenerico = *on;
175600090312             PosCurCtr   = *on;
175700090312             V1Dmsg = 'Ammessi SOLO caratteri numerici';
175800090311             leavesr;
175900090310         endsl;
176000090226
176100090327         // - Se codice padre <> da codice cliente
176200090306         //   stesso ksc/ctr/importo
176300090303         if  V2Cccm <> V2Cksu;
176400090226           select;
176500090303             when  p_§LACksc <> V2Cksc;
176600090226               errMessage  = *on;
176700090226               errGenerico = *on;
176800090303               PosCurKsc   = *on;
176900090309               V1Dmsg = 'Cliente tassazione +
177000090309                         <> da quello del cod. padre ('
177100090309                      + %editc(p_§LACksc : 'X') + ')';
177200090226               leavesr;
177300090303             when  p_§LACctr <> V2Cctr;
177400090226               errMessage  = *on;
177500090226               errGenerico = *on;
177600090303               PosCurCtr   = *on;
177700090309               V1Dmsg = 'Codice tariffa +
177800090309                         <> quello del cod. padre ('
177900090309                      + p_§LACctr + ')';
178000090226               leavesr;
178100090226           endsl;
178200090226         endif;
178300090226
178400090309         //?Importo & Tipo importo forzato?
178500090310         select;
178600090310           // - Non ammesso importo per Varia Negata
178700090310           when  V2Ctad = 'V'   and   V2imp <> *zero;
178800090310             errMessage  = *on;
178900090310             errGenerico = *on;
179000090310             PosCurImp   = *on;
179100090310             V1Dmsg = 'NON ammesso l''importo forzato per +
179200090310                       Varia Negata';
179300090310             leavesr;
179400090310           // - Non ammesso tipo importo senza importo
179500090310           when  V2imp =  *zero   and   V2fimp <> *blank;
179600090310             errMessage  = *on;
179700090310             errGenerico = *on;
179800090310             PosCurImp   = *on;
179900090310             V1Dmsg = 'Importo forzato obbligatorio SE +
180000090310                       inserito il tipo di importo';
180100090310             leavesr;
180200090312           // - Se inserito importo devono essere inseriti:
180300090312           //   · il codice cliente forzato (ksc),
180400090312           //   · la tariffa forzata (ctr) ed
180500090312           //   · il tipo importo (fimp)
180600090310           when  V2imp <> *zero;
180700090310             select;
180800090310               when  V2fimp  = *blank;
180900090310                 errMessage  = *on;
181000090310                 errGenerico = *on;
181100090310                 PosCurFimp  = *on;
181200090310                 V1Dmsg = 'Immettere il tipo importo';
181300090310                 leavesr;
181400090310               when  V2Cksc  = *zero;
181500090310                 errMessage  = *on;
181600090310                 errGenerico = *on;
181700090310                 PosCurKsc   = *on;
181800090310                 V1Dmsg = 'Immettere il cliente forzato';
181900090310                 leavesr;
182000090312               when  V2Cctr  = *blank;
182100090310                 errMessage  = *on;
182200090310                 errGenerico = *on;
182300090310                 PosCurCtr   = *on;
182400090310                 V1Dmsg = 'Immettere la tariffa forzata';
182500090310                 leavesr;
182600090310             endsl;
182700090310         endsl;
182800090312
182900090327         // Verifica compatibilità dell'importo forzato con il
183000090312         //   massimo imponibile IVA in tab. "GEI"/"LAC".
183100090312         if  V2imp <> *zero   and   V2fimp = 'T';
183200090312           // - Reperimento divisa moneta di conto gestionale
183300090312           clear  dGED;
183400090312           reset  TIBS02ds;
183500090312           T02cod = 'GED';
183600090312           T02ke1 = '1';
183700090312           TNTBE_RicercaControllo(kpjba : tibs02ds);
183800090312           if  T02err = *blank;
183900090312             dGED = T02uni;
184000090312           endif;
184100090312           // - Reperimento importi da tab. "GEI"
184200090312           //   (interessa il massimo imponibile IVA per spedizione)
184300090312           clear  dGEI;
184400090312           reset  TIBS02ds;
184500090312           T02cod = 'GEI';
184600090312           T02ke1 = §GEDcn;
184700090312           TNTBE_RicercaControllo(kpjba : tibs02ds);
184800090312           if  T02err = *blank;
184900090312             dGEI = T02uni;
185000090312           endif;
185100090312           if  V2imp > §GEImf;
185200090312             errMessage  = *on;
185300090312             errGenerico = *on;
185400090312             PosCurImp   = *on;
185500090312             V1Dmsg = 'Importo superiore al MASSIMO IMPONIBILE IVA +
185600090312                       PER SPEDIZIONE ('
185700090312                      + %trim( %editc( §GEImf : 'J' ) ) + ')';
185800090312             leavesr;
185900090312           endif;
186000090312         endif;
186100090226
186200090312         // Se codice padre <> da codice cliente
186300090306         //   stesso importo e tipo importo
186400090303         if  V2Cccm <> V2Cksu;
186500090226           select;
186600090303             when  p_§LACimp <> V2imp;
186700090226               errMessage  = *on;
186800090226               errGenerico = *on;
186900090312               PosCurImp   = *on;
187000090309               V1Dmsg = 'Importo forzato +
187100090309                         <> da quello del cod. padre ('
187200090309                      + %editc(p_§LACimp : 'J') + ')';
187300090226               leavesr;
187400090303             when  p_§LACfimp <> V2fimp;
187500090226               errMessage  = *on;
187600090226               errGenerico = *on;
187700090312               PosCurFimp  = *on;
187800090309               V1Dmsg = 'Tipo importo forzato +
187900090309                         <> da quello del cod. padre ('
188000090309                      + p_§LACfimp + ')';
188100090226               leavesr;
188200090226           endsl;
188300090225         endif;
188400090923
188500090923         //?Modifica dati sensibili per l'addebito
188600090923         if (v2tadu <> savtadu or v2ctad <> savtad or
188700090923             v2cksc <> savksc or v2cctr <> savctr or
188800090923             v2imp <> savimp or v2fimp <> savfimp) and not *in02
188900090923             and not $forza;
189000090923               errMessage  = *on;
189100090923               errGenerico = *on;
189200090923               PosCurTadu  = *on;
189300090923               V1Dmsg = 'ATTENZIONE!! Possibili problemi ' +
189400090923                        'in fase di addebito. Avvisare Monica/Nicola';
189500090923               $forza = *on;
189600090923               leavesr;
189700090923         endif;
189800110713
189900110713         //?Creazione file elenco per il cliente
190000110713         //?possibile solo se NON è elaborazione automatica (FTP)
190100110713         IF  V2pgm <> *blanks and V2caut = 'A';
190200110713           ErrMessage  = *on;
190300110713           ErrGenerico = *on;
190400110713           PosCurPgm   = *on;
190500110713           V1Dmsg = 'NON possibile creazione elenco se elaborazione '+
190600110713                    'automatica';
190700110713           leavesr;
190800110713         ENDIF;
190900110713         //?se inserito il nome del programma deve esserci anche il tipo elenco
191000110713         IF  V2pgm <> *blanks and V2ele = *blanks;
191100110713           ErrMessage  = *on;
191200110713           ErrGenerico = *on;
191300110713           PosCurEle   = *on;
191400110713           V1Dmsg = 'Immettere anche il tipo elenco da creare';
191500110713           leavesr;
191600110713         ENDIF;
191700110713         //?e viceversa se c'è il tipo elenco deve esserci anche il nome del pgm
191800110713         IF  V2pgm = *blanks and V2ele <> *blanks;
191900110713           ErrMessage  = *on;
192000110713           ErrGenerico = *on;
192100110713           PosCurPgm   = *on;
192200110713           V1Dmsg = 'Immettere anche il nome del programma x elenco da creare';
192300110713           leavesr;
192400110713         ENDIF;
192500110713         //?Se codice padre <> da codice cliente
192600110713         //?  devono avere gli stessi dati
192700110713         IF  V2Cccm <> V2Cksu;
192800110713           SELECT;
192900110713             WHEN  p_§LACpgm <> V2pgm;
193000110713               ErrMessage  = *on;
193100110713               ErrGenerico = *on;
193200110713               PosCurPgm   = *on;
193300110713               V1Dmsg = 'Nome programma +
193400110713                         <> da quello del cod. padre ('
193500110713                      + p_§LACpgm + ')';
193600110713               leavesr;
193700110713             WHEN  p_§LACele <> V2ele;
193800110713               ErrMessage  = *on;
193900110713               ErrGenerico = *on;
194000110713               PosCurEle   = *on;
194100110713               V1Dmsg = 'Tipo elenco +
194200110713                         <> da quello del cod. padre ('
194300110713                      + p_§LACele + ')';
194400110713               leavesr;
194500110713           ENDSL;
194600110713         ENDIF;
194700071207
194800071207       ENDSR;
194900090225
195000090225       //--------------------------------------------------------------
195100090306       //?Controllo esistenza del codice padre in tabella LAC          ?
195200090225       //--------------------------------------------------------------
195300090303       BEGSR  sr_Padre;
195400090302
195500090327         // Controlla esistenza in tabella "LAC"
195600090310         clear  dLACp;
195700090310         reset  TIBS02ds;
195800090310         T02sif = KNSIF;
195900090310         %subst(T02ke1 : 1 : 7) = %editc(V2Cksu : 'X');
196000090310         TNTBE_RicercaControllo(kpjba : tibs02ds);
196100090310         if  T02err = *blank;
196200090310           dLACp  = T02uni;
196300090310         else;
196400090310           errMessage  = *on;
196500090310           errGenerico = *on;
196600090310           PosCurksu   = *on;
196700090310           V1Dmsg = $Msg(12);
196800090310           leavesr;
196900090310         endif;
197000090327
197100090327         // Controlla se già inserito come figlio di altro padre
197200090327         if  p_§LACksu <> V2Cksu;
197300090327           errMessage  = *on;
197400090327           errGenerico = *on;
197500090327           PosCurKsu   = *on;
197600090327           V1Dmsg = 'Non ammesso cod. PADRE già FIGLIO di altro +
197700090327                     cliente (' + %editc(p_§LACksu : 'X') + ')';
197800090327           leavesr;
197900090327         endif;
198000090225
198100090225       ENDSR;
198200121016
198300121016       //--------------------------------------------------------------
198400121016       //?Controllo Merge.
198500121016       //--------------------------------------------------------------
198600121016       BEGSR  CTR_merge;
198700121016
198800121016       // -?L'immagine se con Merge deve essere di tipo PDF
198900121016         IF  V2Ctpi <> 'PDF';
199000121016           errMessage  = *on;
199100121016           errGenerico = *on;
199200121016           V1Dmsg = 'Immagine con Merge, deve essere di tipo PDF';
199300121016           PosCurTpi = *on;
199400121016           leavesr;
199500121016         ENDIF;
199600121016
199700121016       // -?La directory deve finire con /PF
199800121016         wPos1 = %scan('  ':V2Cdir);
199900121016         wPos1 -= 3;
200000121016         IF  %subst(V2Cdir:wPos1:3) <> '/PF';
200100121016           ErrMessage  = *on;
200200121016           ErrGenerico = *on;
200300121016           PosCurDir   = *on;
200400121016           V1Dmsg = 'Immagine con Merge, l''ultimo ramo della +
200500121016                     DIR deve essere "PF"';
200600121016           leavesr;
200700121016         ENDIF;
200800121016
200900121016       // -?Cerco il corrispondente in JDC
201000121016         clear  TIBS02DS;
201100121016         T02mod = 'C';
201200121016         T02cod = 'JDC';
201300121016         T02ke1 = %editc(V2Cccm:'X');
201400121016         TNTBE_RicercaControllo(kpjba : tibs02ds);
201500121016         // -?Non trovo la tabella
201600121016         IF  T02err <> *blanks;
201700121016           // -?Immissione forzo
201800121016           IF  *in02 and not wForza;
201900121016             wForza = *on;
202000121016             ErrMessage  = *on;
202100121016             ErrGenerico = *on;
202200121016             V1Dmsg = 'Ricordarsi di attivare anche la corrispondente +
202300121016                       tabella JDC. Enter forza.';
202400121016             leavesr;
202500121016           ENDIF;
202600121016         // -?Manutenzione blocco
202700121016           IF  not *in02;
202800121016             ErrMessage  = *on;
202900121016             ErrGenerico = *on;
203000121016             V1Dmsg = 'Manca la corrispondente tabella JDC';
203100121016             leavesr;
203200121016           ENDIF;
203300121016         // -?Trovo la tabella
203400121016         ELSE;
203500121016           dJDC = T02uni;
203600121016         // -?Controllo il tipo immagine
203700121030           IF  V2Ctpi <> §JDCtpi and not wForzaTPI;
203800121030             wForzaTPI = *on;
203900121016             ErrMessage  = *on;
204000121016             ErrGenerico = *on;
204100121016             PosCurTpi   = *on;
204200121016             V1Dmsg = 'Tipo immagine diverso dalla corrispondente +
204300121016                       tabella JDC --> ' + §JDCtpi;
204400121016             leavesr;
204500121016           ENDIF;
204600121016         // -?Controllo il nome immagine
204700121030           IF  V2Cfmi <> §JDCfmi and not wForzaFMI;
204800121030             wForzaFMI = *on;
204900121016             ErrMessage  = *on;
205000121016             ErrGenerico = *on;
205100121016             PosCurFmi   = *on;
205200121016             V1Dmsg = 'Nome immagine diverso dalla corrispondente +
205300121016                       tabella JDC --> ' + §JDCfmi;
205400121016             leavesr;
205500121016           ENDIF;
205600121016         // -?Controllo la directory
205700121016           wPos2 = %scan('  ':§JDCdir);
205800121016           wPos2 -= 3;
205900121016           IF  %subst(V2Cdir:1:wPos1) <>
206000121030               %subst(§JDCdir:1:wPos2) and not wForzaDIR;
206100121030             wForzaDIR = *on;
206200121016             ErrMessage  = *on;
206300121016             ErrGenerico = *on;
206400121016             PosCurDir   = *on;
206500121016             V1Dmsg = 'Directory diversa dalla corrispondente +
206600121016                       tabella JDC --> ' + %subst(§JDCdir:1:wPos2);
206700121016             leavesr;
206800121016           ENDIF;
206900121016         ENDIF;
207000121016
207100121016       ENDSR;
207200101103
207300101103       //--------------------------------------------------------------
207400101103       //?Gestione window W01                                          ?
207500101103       //--------------------------------------------------------------
207600101103       BEGSR  sr_GesW01;
207700101103
207800101103         // -?Emissione window?
207900101103         exfmt  TB46W01;
208000101103
208100101103         errMessage  = *off;
208200101103         errGenerico = *off;
208300101103         clear  V1Dmsg;
208400101103
208500101103         $Video = 'D2';
208600101103
208700101103         select;
208800101103           // -?F12=Ritorno?
208900101103           when  dsp_aid = c_F12;
209000101103             leavesr;
209100101103           // -?Invio con "S"?
209200101103           when  W1Ccpy = 'S';
209300101103             if  V2Caut = *blank;
209400101103               V2Caut   = p_§LACaut;
209500101103             endif;
209600101103             if  V2Ctpi = *blank;
209700101103               V2Ctpi   = p_§LACtpi;
209800101103             endif;
209900101103             if  V2Cfmi = *blank;
210000101103               V2Cfmi   = p_§LACfmi;
210100101103               clear  V2Dfmi;
210200101103             endif;
210300121016             if  V2merge = *blank;
210400121016               V2merge  = p_§LACmerge;
210500121016             endif;
210600121217             if  V2Cla2 = *blank;
210700121217               V2Cla2 = p_§LACla2;
210800121217             endif;
210900101103             //if  V2Cdir = *blank;      //? Directory ?
211000101103               V2Cdir   = p_§LACdir;     //? COMUNQUE  ?
211100101103             //endif;                    //? del padre ?
211200101103             if  V2Ctpt = *blank;
211300101103               V2Ctpt   = p_§LACtpt;
211400101103             endif;
211500101103             if  V2Cssr = *blank;
211600101103               V2Cssr   = p_§LACssr;
211700101103             endif;
211800101103             if  V2Clnp = *zero;
211900101103               V2Clnp   = p_§LAClnp;
212000101103             endif;
212100101103             if  V2Cres = *blank;
212200101103               V2Cres   = p_§LACres;
212300101103             endif;
212400101103             if  V2Crec = *blank;
212500101103               V2Crec   = p_§LACrec;
212600101103             endif;
212700101103             if  V2Ccsr = *blank;
212800101103               V2Ccsr   = p_§LACcsr;
212900101103             endif;
213000101103             if  V2tdta = *blank;
213100101103               V2tdta   = p_§LACtdta;
213200101103             endif;
213300101103             if  V2mesi = *zero;
213400101103               V2mesi   = p_§LACmesi;
213500101103             endif;
213600101103             if  V2Cschgg = *zero;
213700101103               V2Cschgg = p_§LACschgg;
213800101103             endif;
213900101103             if  ds_SchMm = *blank;
214000101103               ds_SchMm = p_SchMm;
214100101103             endif;
214200101103             if  V2tadu = *blank;
214300101103               V2tadu   = p_§LACtadu;
214400101103             endif;
214500101103             if  V2Ctad = *blank;
214600101103               V2Ctad   = p_§LACtad;
214700101103             endif;
214800101103             if  V2Cksc = *zero;
214900101103               V2Cksc   = p_§LACksc;
215000101103             endif;
215100101103             if  V2Cctr = *blank;
215200101103               V2Cctr   = p_§LACctr;
215300101103             endif;
215400101103             if  V2imp  = *zero;
215500101103               V2imp    = p_§LACimp;
215600101103             endif;
215700101103             if  V2fimp = *blank;
215800101103               V2fimp   = p_§LACfimp;
215900101103             endif;
216000110713             if  V2pgm  = *blank;
216100110713               V2pgm    = p_§LACpgm;
216200110713             endif;
216300110713             if  V2ele  = *blank;
216400110713               V2ele    = p_§LACele;
216500110713             endif;
216600101103             if  V2note = *blank;
216700101103               V2note   = p_§LACnote;
216800101103             endif;
216900101103         endsl;
217000101103
217100101103       ENDSR;
217200121212
217300121212       //--------------------------------------------------------------
217400121217       //?Gestione estensione tabella --> LA2.                       ?
217500121212       //--------------------------------------------------------------
217600121217       BEGSR  GesLA2;
217700121212
217800121217         TNTB464ds = TNTB46ds;
217900121217         B464ke1 = %editc(V2Cccm:'X');
218000121217         clear B464fxx;
218100121217         clear B464err;
218200121217         clear B46msg;
218300121217         tntb464r (kpjba:TNTB464DS);
218400121212
218500121212       ENDSR;
218600071206
218700071212       //--------------------------------------------------------------
218800090306       //?Aggiornamento record TNTBE00F (tab. LAC)                     ?
218900071212       //--------------------------------------------------------------
219000090306       BEGSR  Upd_TNTBE;
219100071207
219200090306         clear  dLAC;
219300071207         §LACrag   = V2Dccm;
219400071207         §LACtpt   = V2Ctpt;
219500071207         §LACres   = V2Cres;
219600071207         §LACrec   = V2Crec;
219700071207         §LACcsr   = V2Ccsr;
219800071207         §LACssr   = V2Cssr;
219900071207         §LACdir   = V2Cdir;
220000071207         §LACtpi   = V2Ctpi;
220100071207         §LACfmi   = V2Cfmi;
220200121016         §LACmerge = V2merge;
220300121217         §LACla2   = V2Cla2;
220400071207         §LACaut   = V2Caut;
220500090309         §LACtdta  = V2tdta;
220600071207         §LACtad   = V2Ctad;
220700071207         §LACschgg = V2Cschgg;
220800071207         §LACsch01 = V2Csch01;
220900071207         §LACsch02 = V2Csch02;
221000071207         §LACsch03 = V2Csch03;
221100071207         §LACsch04 = V2Csch04;
221200071207         §LACsch05 = V2Csch05;
221300071207         §LACsch06 = V2Csch06;
221400071207         §LACsch07 = V2Csch07;
221500071207         §LACsch08 = V2Csch08;
221600071207         §LACsch09 = V2Csch09;
221700071207         §LACsch10 = V2Csch10;
221800071207         §LACsch11 = V2Csch11;
221900071207         §LACsch12 = V2Csch12;
222000090226         §LACnote  = V2note;
222100090309         §LAClnp   = V2Clnp;
222200090309         §LACksu   = V2Cksu;
222300090309         §LACmesi  = V2mesi;
222400090313         §LACdatai = (%int( %subst( %editc( V2datai : 'X') : 3 : 4 ) )
222500090313                      * 100)
222600090313                   + %int( %subst( %editc( V2datai : 'X') : 1 : 2 ) );
222700090310         §LACdatela...
222800090313                   = (%int( %subst( %editc( V2datela : 'X') : 3 : 4 ) )
222900090319                      * 100)
223000090313                    + %int( %subst( %editc( V2datela : 'X') : 1 : 2 ) );
223100090309         §LACtadu  = V2tadu;
223200090309         §LACksc   = V2Cksc;
223300090309         §LACctr   = V2Cctr;
223400090309         §LACfimp  = V2fimp;
223500090309         §LACimp   = V2imp;
223600110713         §LACpgm   = V2pgm;
223700110713         §LACele   = V2ele;
223800071207
223900090309         TBEuni    = dLAC;
224000071207
224100090306         clear  TBEftt;
224200090306         clear  TBEftr;
224300071207
224400071207         select;
224500090306           // F5=Ripristino
224600090306           when  dsp_aid = c_F05;
224700090306             clear  TBEatb;
224800090306           // F16=Annullamento
224900090306           when  dsp_aid = c_F16;
225000071207             TBEatb = 'A';
225100071207         endsl;
225200121220
225300121220         TBEdtr = woggi;
225400071207
225500071207         IF  NOT  %found(TNTBE01L);
225600090306           clear  TBEatb;
225700090306           clear  TBEflt;
225800090306           clear  TBEftr;
225900090306           TBEsif = TBXsif;
226000090306           TBElin = TBXlin;
226100090306           TBEapl = TBXapl;
226200090306           TBEcod = k_TBEcod;
226300090306           TBEke1 = k_TBEke1;
226400090306           TBEke2 = k_TBEke2;
226500090306           //_____________
226600071207           WRITE TNTBE000;
226700090306           //¯¯¯¯¯¯¯¯¯¯¯¯¯
226800071207         ELSE;
226900090306           //______________
227000071207           UPDATE TNTBE000;
227100090306           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
227200071207         ENDIF;
227300121213
227400121213
227500121213         // -?Porto a pari anche la tabella LA2
227600121213         SELECT;
227700121213           //?F5=Ripristino tabella
227800121213           WHEN  dsp_aid = c_F05;
227900121213             //?Ripristino anche la LA2 se esiste
228000121213             k_TBEcod = 'LA2';
228100121213             k_TBEke1 = %editc(V2Cccm:'X');
228200121213             clear k_TBEke2;
228300121213             k_TBElin = TBXlin;
228400121213             k_TBEsif = TBXsif;
228500121213             chain  %kds(K05tntbe01) TNTBE01L;
228600121213             IF  %found(TNTBE01L);
228700121213               clear TBEatb;
228800121213               UPDATE  TNTBE000;
228900121213             ENDIF;
229000121213           WHEN  dsp_aid = c_F16;
229100121213             //?Annullo anche la LA2 se esiste
229200121213             k_TBEcod = 'LA2';
229300121213             k_TBEke1 = %editc(V2Cccm:'X');
229400121213             clear k_TBEke2;
229500121213             k_TBElin = TBXlin;
229600121213             k_TBEsif = TBXsif;
229700121213             chain  %kds(K05tntbe01) TNTBE01L;
229800121213             IF  %found(TNTBE01L);
229900121213               TBEatb = 'A';
230000121213               UPDATE  TNTBE000;
230100121213             ENDIF;
230200121213           OTHER;
230300121213           //?Modifica
230400121217           //?se viene tolto il flag si altra direcotry controllo se esiste rcd LA2
230500121213           //?se esiste cancello
230600121217             IF  §LACla2 = *blanks;
230700121213               k_TBEcod = 'LA2';
230800121213               k_TBEke1 = %editc(V2Cccm:'X');
230900121213               clear k_TBEke2;
231000121213               k_TBElin = TBXlin;
231100121213               k_TBEsif = TBXsif;
231200121213               chain  %kds(K05tntbe01) TNTBE01L;
231300121213               IF  %found(TNTBE01L);
231400121213                DELETE  TNTBE000;
231500121213               ENDIF;
231600121213             ENDIF;
231700121213         ENDSL;
231800071207
231900071207       ENDSR;
232000071207
232100071212       //--------------------------------------------------------------
232200090306       //?Operazioni finali.                                           ?
232300071212       //--------------------------------------------------------------
232400090306       BEGSR  sr_RoutEnd;
232500071207
232600090306         clear  TIBS69ds;
232700090304         tibs69ds.I69tla = 'C';
232800090304         tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
232900071207
233000071217         kpjbu = TNTB46ds;
233100071207         return;
233200071207
233300071207       ENDSR;
233400090306
233500071207      /end-free
233600071207
233700071212       //--------------------------------------------------------------
233800090306       //?Schiere a tempo di compilazione.                             ?
233900071212       //--------------------------------------------------------------
234000071207
234100071207** - $MSG -------------------------------------------------------------------*
234200071207Immettere il codice cliente                                                     1
234300071207Immettere SOLO caratteri numerici                                               2
234400071207Codice errato                                                                   3
234500071207Cliente annullato                                                               4
234600071207Immettere la directory                                                          5
234700071207Carattere "\" non valido; per indicare una sottocartella utilizzare "/"         6
234800071210Caratteri & e %  non ammessi per la directory di clienti con invio automatico   7
234900071207Indicare il giorno del mese se specificato almeno un mese                       8
235000071207Specificare almeno un mese se indicato il giorno del mese                       9
235100071207Per i mesi con n° di giorni < &g verrà assunto il loro ultimo giorno -> Enter  10
235200090225Inserire il codice padre                                                       11
235300090225Il codice padre deve essere = al codice cliente o = a cliente già in tabella   12
235400120112Cliente da copiare NON più reperibile in tab. "LAC"                            13
235500120112Cliente già inserito in tab. "LAC"                                             14
