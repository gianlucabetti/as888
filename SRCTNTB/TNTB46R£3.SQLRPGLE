000100090306       //--------------------------------------------------------------
000200090306       //?GESTIONE TABELLA "LAC" - Laguna: clienti per ritorno immagini?
000300121213       //?                       + Tabella LA2 a richiesta           ?
000400090306       //--------------------------------------------------------------
000500110804
000600110804      *?  ATTENZIONE!!  ?
000700110804      *?    Questa tabella è utilizzata anche dal pgm TNTA61R  ?
000800110804      *?    'Interrogazione abilitazioni clienti'              ?
000900110804      *?    In caso di aggiunta/modifica campi alla tabella    ?
001000110804      *?    verificare anche relativo pgm di interrogazione    ?
001100110804      *?    per le abilitazioni clienti --> TNTB462R.          ?
001200090306
001300071206     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001400071206
001500090306       //--------------------------------------------------------------
001600090306       //?Dichiarazione file.                                          ?
001700090306       //--------------------------------------------------------------
001800090306
001900090311     fAZORG01L  if   e           k disk
002000090311
002100071206     fTNTBE01L  Uf A e           k disk
002200090311
002300071206     fTNTB46D   cf   e             workstn
002400071207     f                                     indds(IndDspF)
002500090306     f                                     infds(InfDspF)
002600071207
002700090306       //--------------------------------------------------------------
002800090306       //?Definizione costanti.                                        ?
002900090306       //--------------------------------------------------------------
003000090306
003100090306       // - Tabella in gestione
003200090310     d c_Tab           c                   const('LAC')
003300090306
003400090306       // - Costante per controllo "caratteri solo numerici"
003500090310     d c_Digits        c                   const('0123456789')
003600090306
003700090306       // - Tasti funzionali a video
003800090306     d c_F01           c                   const(x'31')
003900090306     d c_F02           c                   const(x'32')
004000090306     d c_F03           c                   const(x'33')
004100090319     d c_F04           c                   const(x'34')
004200090306     d c_F05           c                   const(x'35')
004300090306     d c_F06           c                   const(x'36')
004400090306     d c_F07           c                   const(x'37')
004500090306     d c_F08           c                   const(x'38')
004600090306     d c_F09           c                   const(x'39')
004700090306     d c_F10           c                   const(x'3A')
004800110713     d c_F11           c                   const(x'3B')
004900090306     d c_F12           c                   const(x'3C')
005000090306     d c_F13           c                   const(x'B1')
005100090306     d c_F14           c                   const(x'B2')
005200090306     d c_F15           c                   const(x'B3')
005300090306     d c_F16           c                   const(x'B4')
005400090306     d c_F17           c                   const(x'B5')
005500090306     d c_F18           c                   const(x'B6')
005600090306     d c_F19           c                   const(x'B7')
005700090306     d c_F20           c                   const(x'B8')
005800090306     d c_F21           c                   const(x'B9')
005900090306     d c_F22           c                   const(x'BA')
006000090306     d c_F23           c                   const(x'BB')
006100090306     d c_F24           c                   const(x'BC')
006200090306     d c_Enter         c                   const(x'F1')
006300090306     d c_RollDown      c                   const(x'F4')
006400090306     d c_RollUp        c                   const(x'F5')
006500071207
006600090306       //--------------------------------------------------------------
006700090306       //?Definizione schiere.                                         ?
006800090306       //--------------------------------------------------------------
006900090306
007000090306       // - Messaggi a video
007100141203     d $Msg            s             78    dim(20)  ctdata  perrcd(1)
007200071207
007300090306       //--------------------------------------------------------------
007400090306       //?Definizione aree dati.                                       ?
007500090306       //--------------------------------------------------------------
007600090306
007700090306       // - Dati utente
007800071207     d §AzUte        e ds                  extname(AZUTE00F)
007900071207     d                                     dtaara
008000071207     d §DatiUte      e ds                  extname(dDatiUte)
008100071207     d                                     dtaara
008200071207
008300090306       //--------------------------------------------------------------
008400090306       //?Definizione strutture dati.                                  ?
008500090306       //--------------------------------------------------------------
008600090306
008700090306       // - Status
008800071207     d Psds           sds
008900071207     d   SDSpgm          *proc
009000071207
009100090306       // - InfDS
009200071207     d InfDspF         ds
009300071207     d  dsp_aid              369    369a                                        AID byte
009400071207
009500090306       // - Indicatori su DspF
009600090311     d IndDspF         ds
009700090306         // - Abilitazione tasti funzionali
009800090323     d  F4Attivo                       n   overlay(IndDspF:04)
009900090311     d  F5Attivo                       n   overlay(IndDspF:05)
010000090311     d  F6Attivo                       n   overlay(IndDspF:06)
010100141205     d  F8Attivo                       n   overlay(IndDspF:08)
010200090311     d  F16Attivo                      n   overlay(IndDspF:16)
010300090306         // - Emissione messaggio di errore
010400090311     d  ErrMessage                     n   overlay(IndDspF:28)
010500090306         // - Protezione campi
010600090311     d  ProtDATAI                      n   overlay(IndDspF:40)
010700120112     d  InserCCM2                      n   overlay(IndDspF:41)
010800141106         // - Visualizza campi
010900141128     d  VisCEksu                       n   overlay(IndDspF:46)
011000090306         // - Posizionamento cursore & segnalazione errore
011100090311     d  PosCurCcm                      n   overlay(IndDspF:50)
011200090311     d  PosCurAut                      n   overlay(IndDspF:51)
011300090311     d  PosCurTpi                      n   overlay(IndDspF:52)
011400090311     d  PosCurDir                      n   overlay(IndDspF:53)
011500090311     d  PosCurSchGg                    n   overlay(IndDspF:54)
011600090311     d  PosCurSch01                    n   overlay(IndDspF:55)
011700090311     d  PosCurSch02                    n   overlay(IndDspF:56)
011800090311     d  PosCurSch04                    n   overlay(IndDspF:57)
011900090311     d  PosCurSch06                    n   overlay(IndDspF:58)
012000090311     d  PosCurSch09                    n   overlay(IndDspF:59)
012100090311     d  PosCurSch11                    n   overlay(IndDspF:60)
012200090311     d  PosCurKsu                      n   overlay(IndDspF:61)
012300090311     d  PosCurMesi                     n   overlay(IndDspF:62)
012400090311     d  PosCurTdta                     n   overlay(IndDspF:63)
012500090311     d  PosCurTad                      n   overlay(IndDspF:64)
012600090311     d  PosCurTadu                     n   overlay(IndDspF:65)
012700090311     d  PosCurKsc                      n   overlay(IndDspF:66)
012800090311     d  PosCurCtr                      n   overlay(IndDspF:67)
012900090311     d  PosCurImp                      n   overlay(IndDspF:68)
013000090311     d  PosCurFimp                     n   overlay(IndDspF:69)
013100090311     d  PosCurFmi                      n   overlay(IndDspF:70)
013200090311     d  PosCurDatai                    n   overlay(IndDspF:71)
013300090311     d  PosCurLnp                      n   overlay(IndDspF:72)
013400090327     d  PosCurTpt                      n   overlay(IndDspF:73)
013500110713     d  PosCurPgm                      n   overlay(IndDspF:74)
013600110713     d  PosCurEle                      n   overlay(IndDspF:75)
013700120112     d  PosCurCCM2                     n   overlay(IndDspF:76)
013800140813     d  PosCurTpAdd                    n   overlay(IndDspF:77)
013900140813     d  PosCurCompCE                   n   overlay(IndDspF:78)
014000141203     d  PosCurAutDG                    n   overlay(IndDspF:79)
014100090306         //   - Riemissione videata
014200090311     d  ErrGenerico                    n   overlay(IndDspF:99)
014300071207
014400090306       // - Parametri ricevuti
014500071206     d KPJBA         e ds
014600071217     d TNTB46ds      e ds                  inz
014700121212     d TNTB464ds     e ds                  inz
014800071207
014900090306       // - Reperimento dati utente
015000071207     d TIBS34ds      e ds
015100071207
015200090306       // - Controllo/Decodifica cliente
015300090304     d TIBS69ds      e ds                  qualified  inz
015400090304     d ds_CNACO      e ds                  extname(CNACO00F)
015500090304     d                                     qualified  inz
015600090304     d ds_CNIND      e ds                  extname(CNIND00F)
015700090304     d                                     qualified  inz
015800090304     d ds_CNCLP      e ds                  extname(CNCLP00F)
015900090304     d                                     qualified  inz
016000090304     d ds_FNCLS      e ds                  extname(FNCLS00F)
016100090304     d                                     qualified  inz
016200141203
016300090306       // - Dati record principale della tabella
016400090306     d TNTBEds       e ds                  inz  extname(TNTBE00F)
016500090306     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
016600090306     d                                          prefix(TBX:3)
016700090312
016800090312       // - Parametri per Ricerca/controllo tabelle
016900090312     d TIBS02ds      e ds                  inz
017000090312     d  T02mod       e                     inz('C')
017100090312     d  T02cod       e                     inz(c_Tab)
017200071207
017300090306       // - Tabella "LAC" = Clienti per ritorno immagini
017400071206     d dLAC          e ds                  inz
017500090306       // - Tabella "LAC" del padre per controllo legami
017600090303     d dLACp         e ds                  extname(dLAC) inz
017700121016     d                                     prefix(p_)
017800090303     d  p_SchMm               71     82
017900121016
018000121016       // - Tabella "JDC"
018100121016     d dJDC          e ds                  extname(dJDC) inz
018200121217       // - Tabella "LA2" = Estensione tab.LAC
018300121217     d dLA2          e ds                  inz
018400121016
018500090312       // - Tabella "GED" = Divise area gestionale
018600090312     d dGED          e ds                  inz
018700090312       // - Tabella "GEI" = Importi vari area gestionale
018800090312     d dGEI          e ds                  inz
018900141128
019000141128       // - Tabella "TAL" = Tipo addebito dichiarato
019100141128     d dTAL          e ds                  inz
019200141106
019300141106       // - File Conto Economico
019400141106     d ECEVD00F      e ds                  extname(ECEVD00F)
019500141203
019600141203      // - File Date
019700141203     d DATECNV0F     e ds                  extname(DATECNV0F)
019800090303
019900090306       // - Mesi in cui è schedulata la mail di promemoria creazione CD
020000090303     d ds_SchMm        ds            12    inz
020100090303     d  V2Csch01                           inz
020200090303     d  V2Csch02                           inz
020300090303     d  V2Csch03                           inz
020400090303     d  V2Csch04                           inz
020500090303     d  V2Csch05                           inz
020600090303     d  V2Csch06                           inz
020700090303     d  V2Csch07                           inz
020800090303     d  V2Csch08                           inz
020900090303     d  V2Csch09                           inz
021000090303     d  V2Csch10                           inz
021100090303     d  V2Csch11                           inz
021200090303     d  V2Csch12                           inz
021300090311
021400090311       // - Struttura di comodo x ctrl mesi di schedulazione
021500090311     d wSchMm_ds       ds            12    inz
021600090311     d  $SchMm                 1     12    dim(12)
021700090319       // - Struttura per passaggio dati ad interrogazione tabella
021800090327     d Param01         ds                  inz
021900090327     d  P01cod                        7  0 inz
022000090327     d  P01ord                        1    inz
022100090327     d  P01ksu                        7  0 inz
022200090327     d  P01ke1                        7    inz
022300090327     d  P01ke2                       15    inz
022400090327     d  P01rit                        1    inz
022500071207
022600090306       //--------------------------------------------------------------
022700090306       //?Definizione procedure usate.                                 ?
022800090306       //--------------------------------------------------------------
022900090306
023000090306       // - Reperimento dati utente
023100090306      /copy gaitrasrc/srcProtoPR,TIBS34R
023200071207
023300090304       // - Ricerca/Controllo tabelle
023400090306      /copy gaitrasrc/srcProtoPR,TIBS02R
023500071207
023600090304       // - Controllo/Decodifica cliente
023700090306      /copy gaitrasrc/srcProtoPR,TIBS69R
023800090319
023900090319       // - Interrogazione tabella LAC
024000090319     d tntb461r        pr                  extpgm('TNTB461R')
024100090319     d  kpjba                              likeds(KPJBA)
024200141204     d  P01opz3                       1a   const
024300121212
024400121212       // - Gestione tabella LA2
024500121212     d tntb464r        pr                  extpgm('TNTB464R')
024600121213     d  kpjba                              likeds(KPJBA)
024700121212     d  tntb464ds                          likeds(TNTB464DS)
024800071207
024900090306       //--------------------------------------------------------------
025000090306       //?Definizione variabili globali.                               ?
025100090306       //--------------------------------------------------------------
025200090306
025300090306       // - Flags booleani
025400090306     d $InzD02         s               n   inz(*on)
025500090306     d $Fine           s               n   inz
025600090923     d $forza          s               n   inz
025700090306     d $CtrMm          s               n   inz
025800090313     d $ByPass         s               n   inz
025900120112     d $Copia          s               n   inz
026000121016     d wForza          s               n   inz
026100121030     d wForzaTPI       s               n   inz
026200121030     d wForzaFMI       s               n   inz
026300121030     d wForzaDIR       s               n   inz
026400071207
026500090306       // - Gestione video
026600141117     d $Video          s              2    inz('D2')
026700090313
026800090313       // - Indici di schiera
026900090313     d xx              s              3  0 inz
027000071207
027100090313       // - Campi di comodo
027200090306     d wDataI          s              6
027300090306     d wDatEla         s              6
027400090306     d wPos1           s              2  0 inz
027500090306     d wPos2           s              2  0 inz
027600090313     d wCount          s              2  0 inz
027700090313     d wSavSch         s                   like(ds_SchMm)  inz
027800090316     d SAVke1          s                   like(TBEke1)    inz
027900090923     d savtad          s                   like(§lactad)
028000090923     d savtadu         s                   like(§lactadu)
028100090923     d savksc          s                   like(§lacksc)
028200090923     d savctr          s                   like(§lacctr)
028300090923     d savimp          s                   like(§lacimp)
028400090923     d savfimp         s                   like(§lacfimp)
028500121220     d woggi           s              8  0 inz
028600141201     d wceksc          s              5  3 inz
028700141201     d wceksu          s              5  3 inz
028800141203     d conta           s              5i 0 inz
028900141204     d P01opz3         s              1a   inz('U')
029000071207
029100090306       //--------------------------------------------------------------
029200090306       //?Definizione key-list.                                        ?
029300090306       //--------------------------------------------------------------
029400090306
029500090306       // - File TNTBE01L
029600090306     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
029700090306     d                                     prefix(k_)  inz
029800071206
029900090306       //--------------------------------------------------------------
030000090306       //?Riepilogo indicatori.                                        ?
030100090306       //--------------------------------------------------------------
030200090306       // 01    - RECORD annullato
030300090306       // 02    - acceso IMMISSIONE - spento MANUTENZIONE
030400090306       // 25    - Comodo
030500090306       // 28    - Emissione del messaggio di errore a video
030600090306       // 40    - Protezione campi in prima estrazione
030700090306       // x D01:
030800090306       // 50    - Codice cliente errato o mancante
030900090306       // x D02:
031000120112       // 41    - Copia in D02: V2CCCM non protetto
031100090306       // 53    - Directory per immagini       errata
031200090306       // 54    - Giorno di schedulazione CD   errato
031300090306       // 55    - Selezione mesi obbligatoria
031400090306       // 56    - Selezione del mese di febbraio
031500090306       // 57    - Selezione del mese di aprile
031600090306       // 58    - Selezione del mese di giugno
031700090306       // 59    - Selezione del mese di settembre
031800090306       // 60    - Selezione del mese di novembre
031900090306       // 61    - Codice Padre                 errato
032000090306       // 62    - N. mesi da estrarre          errato
032100090306       // 63    - Tipo data di elaborazione    errato
032200090306       // 64    - Tipo addebito                errato
032300090306       // 65    - Creazione addebito           errato
032400090306       // 66    - Cliente tassazione           errato
032500090306       // 67    - Codice tariffa               errato
032600090306       // 68    - Importo forzato              errato
032700090306       // 69    - Tipo importo forzato         errato
032800090306       // 70    - Flag nome immagine           errato
032900090306       // 71    - Prima elaborazione           errata
033000090306       // 99    - Generico di errore
033100090306       //--------------------------------------------------------------
033200071206
033300071206     c     *Entry        plist
033400071206     c                   parm                    KPJBA
033500071207
033600071207      /free
033700071207
033800071207       // Operazioni iniziali
033900090306       exsr  sr_RoutInz;
034000071207
034100071207       // Gestione video
034200090306       DOW  $Fine = *off;
034300071207         select;
034400090306           when  $Video = 'D2';
034500090306             exsr  sr_GesD02;
034600101103           when  $Video = 'W1';
034700101103             exsr  sr_GesW01;
034800071207           other;
034900071207             leave;
035000071207         endsl;
035100071207       ENDDO;
035200071207
035300071207       // Operazioni finali
035400090306       exsr  sr_RoutEnd;
035500071206
035600071212       //--------------------------------------------------------------
035700090306       //?Operazioni iniziali.                                         ?
035800071212       //--------------------------------------------------------------
035900090306       BEGSR  sr_RoutInz;
036000141106
036100141106         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
036200090306
036300090306         *inLR = *on;
036400071207
036500090306         if  kpjbu <> *blank;
036600071217           TNTB46ds = kpjbu;
036700071207         else;
036800090306           clear  TNTB46ds;
036900071207         endif;
037000071217         B46fxx = *blank;
037100071217         B46err = *off;
037200071217         B46msg = *blank;
037300071207
037400090306         // Reperimento dati job
037500090306         exsr  DatiJob;
037600071207
037700090306         // Impostazione nome programma a video
037800071207         V1Tpgm = SDSpgm;
037900090306
038000090306         // Aggancio dati generali della tabella in esame
038100090306         clear  k_TBEcod;
038200090306         k_TBEke1 = *zero;
038300090306         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
038400090306                = c_Tab;
038500090306         clear  k_TBEke2;
038600090306         clear  k_TBElin;
038700090306         k_TBEsif = KNSIF;
038800090306         chain(n)  %kds(k05tntbe01)  TNTBE000;
038900090306         if  not  %found(TNTBE01L);
039000090306           clear  k_TBEsif;
039100090306           chain(n)  %kds(k05tntbe01)  TNTBE000;
039200090306         endif;
039300090306         if  %found(TNTBE01L);
039400090306           xTNTBEds = TNTBEds;
039500090306         else;
039600090306           clear  xTNTBEds;
039700090306         endif;
039800071207
039900090306         // Verifica parametri ricecvuti
040000090306         if  B46ke1  <> *blank;
040100141117           $Video  = 'D2';
040200141117           $InzD02 = *on;
040300071207         endif;
040400121220
040500121220         // Imposto la data di oggi
040600121220         woggi = %dec(%date());
040700071207
040800071207       ENDSR;
040900071206
041000071212       //--------------------------------------------------------------
041100090306       //?Reperimento Dati del job (Utente/Operativi).                 ?
041200071212       //--------------------------------------------------------------
041300090306       BEGSR  DatiJob;
041400071207
041500071207         in(E) §AzUte;
041600071207         if NOT %error;
041700071207           in(E) §DatiUte;
041800071207         endif;
041900090306         if %error or RSut = *blank;
042000071207           clear TIBS34ds;
042100071207           tibs34r(tibs34ds);
042200071207           in §AzUte;
042300071207           in §DatiUte;
042400071207         endif;
042500071207
042600071207       ENDSR;
042700141117
042800141117       //--------------------------------------------------------------
042900141117       //?Decodifica cliente passato.                                  ?
043000141117       //--------------------------------------------------------------
043100141117       BEGSR  Decodifica;
043200141117
043300141117         $Copia = (B46opz = '3');
043400141117
043500141117         IF  B46ke1 = *blanks;
043600141117           leavesr;
043700141117         ENDIF;
043800141117
043900141117         If  Not  $Copia;
044000141117           clear  TIBS69ds;
044100141117           tibs69ds.I69kcc = DUTkci;
044200141117           tibs69ds.I69kac = %int(B46ke1);
044300141117           tibs69ds.I69sif = knsif;
044400141117           tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
044500141117           if  tibs69ds.O69err = *on;
044600141117             errMessage  = *on;
044700141117             V1Dmsg = $Msg(03);
044800141117             leavesr;
044900141117           endif;
045000141117         EndIf;
045100141117
045200141117         // - Verifica esistenza codice cliente in tabella
045300141117         reset  TNTBE000;
045400141117         k_TBEke1 = B46ke1;
045500141117         clear  k_TBEke2;
045600141117         k_TBElin = TBXlin;
045700141117         k_TBEsif = TBXsif;
045800141117         chain  %kds(K05tntbe01) TNTBE000;
045900141117         if  NOT  %found(TNTBE01L);
046000141117           errMessage  = *on;
046100141117           V1Dmsg = 'Cliente non presente in tabella';
046200141117           leavesr;
046300141117         endif;
046400141117
046500141117         dLAC = TBEuni;
046600141117
046700141117       ENDSR;
046800071206
046900071212       //--------------------------------------------------------------
047000090306       //?Gestione videata D02                                         ?
047100071212       //--------------------------------------------------------------
047200090306       BEGSR  sr_GesD02;
047300071207
047400090306         // Inizializzazione videata
047500090306         if   $InzD02 = *on;
047600090306           exsr  sr_InzD02;
047700090306           $InzD02 = *off;
047800071207         endif;
047900071207
048000090306         // Emissione testata
048100090306         write  TN46T01;
048200071207
048300090306         // Emissione videata
048400141117         if  B46opz = '5';
048500141117           write  TN46D02;
048600141117           exfmt  PROTECT;
048700141117         else;
048800090306           exfmt  TN46D02;
048900071217         endif;
049000071207         errMessage  = *off;
049100071207         errGenerico = *off;
049200090306         clear  V1Dmsg;
049300071207
049400071207         SELECT;
049500090306           // F3=Fine
049600090306           WHEN  dsp_aid = c_F03;
049700141117             exsr  sr_F03D02;
049800090319           // F04=Interrogazione Padre
049900090319           WHEN  dsp_aid = c_F04;
050000090319             exsr  sr_F04D02;
050100141205           // F08=Vis. tabella LA2
050200141205           WHEN  dsp_aid = c_F08;
050300141205             exsr  sr_F08D02;
050400090306           // F12=Ritorno
050500090306           WHEN  dsp_aid = c_F12;
050600090306             exsr  sr_F12D02;
050700090306           // Enter o F5 o F6 o F16
050800071207           OTHER;
050900090306             // - Controllo dati
051000090306             if  dsp_aid = c_F16;
051100090306               exsr  sr_CtrAnnull;
051200090306             else;
051300090306               exsr  sr_CtrD02;
051400090306             endif;
051500090306             if  errGenerico;
051600071207               leavesr;
051700071207             endif;
051800121212
051900090306             // - Conferma dati:
052000090306             //   F5=Ripristino; F6=Conferma; F16=Annullamento
052100090306             if  dsp_aid = c_F05  or
052200090306                 dsp_aid = c_F06  or
052300090306                 dsp_aid = c_F16;
052400121212
052500090306               exsr  Upd_TNTBE;
052600121213
052700121217           //?Se F6 e immagine da duplicare su altra direcotru devo andare su tabella LA2
052800121217               IF  dsp_aid = c_F06 and V2Cla2 = 'S';
052900121217                 exsr GESLA2;
053000121213                 $Video = 'D2';
053100121213                 $InzD02 = *on;
053200121213                 chain  %kds(K05tntbe01) TNTBE000;
053300121213                 IF  B464err <> '0' and B464err <> *blanks;
053400121213                   ErrMessage  = *on;
053500121213                   ErrGenerico = *on;
053600121213                   V1Dmsg = B464msg;
053700121213                   leavesr;
053800121213                 ENDIF;
053900121213                 unlock TNTBE01L;
054000121213               ENDIF;
054100121212
054200071207               if  NOT  errGenerico;
054300141117                 $Fine  = *on;
054400071207               endif;
054500071207             EndIf;
054600071207
054700071207         ENDSL;
054800071207
054900071207       ENDSR;
055000071206
055100071212       //--------------------------------------------------------------
055200090306       //?Inizializzazione videata D01                                 ?
055300071212       //--------------------------------------------------------------
055400090306       BEGSR  sr_InzD02;
055500071207
055600090306         reset  $CtrMm;
055700090313         reset  $ByPass;
055800121016         reset  wForza;
055900121030         reset  wForzaTPI;
056000121030         reset  wForzaFMI;
056100121030         reset  wForzaDIR;
056200071207
056300101103         clear  TB46W01;
056400101103
056500090306         clear  TN46D02;
056600071210
056700090306         IndDspF = *zero;
056800141201         k_TBEcod = c_Tab;
056900141117
057000141117         exsr Decodifica;
057100141117         if  errMessage = *on;
057200141117           B46err = *on;
057300141117           B46msg = V1Dmsg;
057400141117           $Fine  = *on;
057500141117           leavesr;
057600141117         endif;
057700090309
057800090309         // Impostazione indicatori per abilitazione tasti funzionali
057900090309         exsr  Set_Ind_D02;
058000090309
058100120112         // - Codice estrazione (chiave)
058200141117         if  Not  $Copia and B46ke1 <> *blanks;
058300141117           V2Cccm = %int(B46ke1);
058400141117           V2Dccm = ds_CNACO.ACOrag;
058500141117           InserCCM2 = *off;
058600120112         endif;
058700141117
058800141117         IF  $Copia;
058900141117           V2Cksu = %int(B46ke1);
059000141117           V2Dksu = ds_CNACO.ACOrag;
059100141117           InserCCM2 = *on;
059200141117         ENDIF;
059300141117
059400141117         // Impostazione dati di record nuovo
059500141117         IF  B46ke1 = *blanks;
059600141117           InserCCM2 = *on;
059700141117           V1Tlav = 'IMMISSIONE';
059800141117           *in02 = *on;
059900141117           clear dLA2;
060000141117           leavesr;
060100141117         ENDIF;
060200071207
060300090306         IF  %found(TNTBE01L);
060400071207
060500090306           // Impostazione dati di record trovato
060600071217           select;
060700141117             when  B46opz = '2';
060800141117               V1Tlav = 'VARIAZIONE';
060900090306             when  B46opz = '5';
061000071217               V1Tlav = 'VISUALIZZA';
061100120112             when  $Copia;
061200120112               V1Tlav = '  COPIA   ';
061300071217             other;
061400071217               V1Tlav = 'ANNULLATO ';
061500071217           endsl;
061600090225
061700090306           // Codice padre
061800090306           V2Cksu = §LACksu;
061900090309           clear  TIBS69ds;
062000090309           clear  ds_CNACO;
062100090309           clear  ds_CNIND;
062200090309           clear  ds_CNCLP;
062300090309           clear  ds_FNCLS;
062400090310           select;
062500090310             when  §LACksu =  V2Cccm;
062600090310               V2Dksu = V2Dccm;
062700090310             when  §LACksu <> *zero;
062800090310               tibs69ds.I69kcc = DUTkci;
062900090310               tibs69ds.I69kac = %int(V2Cksu);
063000090310               tibs69ds.I69sif = knsif;
063100090310               tibs69r(TIBS69ds:ds_CNACO:ds_CNIND:ds_CNCLP:ds_FNCLS);
063200090310               if tibs69ds.O69err = *on;
063300090310                 V1Dmsg = $Msg(03);
063400090310                 errMessage  = *on;
063500090310                 errGenerico = *on;
063600090310                 PosCurKsu   = *on;
063700090310                 leavesr;
063800090310               endif;
063900090310               V2Dksu = ds_CNACO.ACOrag;
064000090310           endsl;
064100090225
064200090225           V2Caut   = §LACaut;
064300090309           V2Ctpi   = §LACtpi;
064400090225
064500090306           // Flag nome immagine
064600090306           V2Cfmi = §LACfmi;
064700090310           reset  TIBS02ds;
064800090303           T02cod = 'NIM';
064900090303           T02ke1 = §LACfmi;
065000090304           TNTBE_RicercaControllo(kpjba : tibs02ds);
065100090306           if  T02err = *blank;
065200090303             V2Dfmi = T02uni;
065300090303           endif;
065400121016
065500121016           V2merge = §LACmerge;
065600121217           V2Cla2  = §LACla2;
065700090225
065800090309           V2Cdir   = §LACdir;
065900090309           V2Ctpt   = §LACtpt;
066000090309           V2Cssr   = §LACssr;
066100090309           V2Clnp   = §LAClnp;
066200090309           V2Cres   = §LACres;
066300090309           V2Crec   = §LACrec;
066400090309           V2Ccsr   = §LACcsr;
066500090309
066600090309           // Prima estrazione
066700090309           clear  wDataI;
066800090309           if  §LACdatai > *zero;
066900090309             %subst(wDataI:1:2) = %subst(%editc(§LACdatai:'X'):5:2);
067000090309             %subst(wDataI:3:4) = %subst(%editc(§LACdatai:'X'):1:4);
067100090310             V2datai = %int(wDataI);
067200090309           endif;
067300090309
067400090309           // Ultima estrazione
067500090309           clear  wDatEla;
067600090309           if  §LACdatela > *zero;
067700090309             %subst(wDatEla:1:2) = %subst(%editc(§LACdatela:'X'):5:2);
067800090309             %subst(wDatEla:3:4) = %subst(%editc(§LACdatela:'X'):1:4);
067900090310             V2datela = %int(wDatEla);
068000090309           endif;
068100090309
068200090309           V2tdta   = §LACtdta;
068300090309           V2mesi   = §LACmesi;
068400071207           V2Cschgg = §LACschgg;
068500071207           V2Csch01 = §LACsch01;
068600071207           V2Csch02 = §LACsch02;
068700071207           V2Csch03 = §LACsch03;
068800071207           V2Csch04 = §LACsch04;
068900071207           V2Csch05 = §LACsch05;
069000071207           V2Csch06 = §LACsch06;
069100071207           V2Csch07 = §LACsch07;
069200071207           V2Csch08 = §LACsch08;
069300071207           V2Csch09 = §LACsch09;
069400071207           V2Csch10 = §LACsch10;
069500071207           V2Csch11 = §LACsch11;
069600071207           V2Csch12 = §LACsch12;
069700090225
069800090306           V2tadu   = §LACtadu;
069900090309           V2Ctad   = §LACtad;
070000090306           V2cksc   = §LACksc;
070100090306           V2cctr   = §LACctr;
070200090306           V2imp    = §LACimp;
070300090306           V2fimp   = §LACfimp;
070400110713           V2pgm    = §LACpgm;
070500110713           V2ele    = §LACele;
070600090306           V2note   = §LACnote;
070700140813
070800140813           IF  §LACdtaimm <> *blanks;
070900140813             V2dtaimm = %subst(§LACdtaimm:7:2) + '/' +
071000140813                        %subst(§LACdtaimm:5:2) + '/' +
071100140813                        %subst(§LACdtaimm:1:4);
071200140813           ENDIF;
071300141128
071400140813           V2utente = §LACutente;
071500141128
071600140813           V2compce = §LACcompce;
071700141128           reset  TIBS02ds;
071800141128           T02cod = 'CED';
071900141128           T02ke1 = V2compce;
072000141128           T02sif = KNSIF;
072100141128           TNTBE_RicercaControllo(kpjba : tibs02ds);
072200141128           IF  T02err = *blank;
072300141128             V2dcompce = T02uni;
072400141128           ENDIF;
072500141202           IF  V2compce = *blanks;
072600141202             V2dcompce = 'NON DICHIARATO';
072700141202           ENDIF;
072800141202
072900141128           IF V2Cccm <> V2Cksu;
073000141128             clear dLACp;
073100141128             reset  TIBS02ds;
073200141128             T02sif = KNSIF;
073300141128             T02cod = 'LAC';
073400141128             T02ke1 = %editc(V2Cksu:'X');
073500141128             TNTBE_RicercaControllo(kpjba : tibs02ds);
073600141128             IF  T02err = *blank;
073700141128               dLACp = T02uni;
073800141128             ENDIF;
073900141128             reset  TIBS02ds;
074000141128             T02cod = 'CED';
074100141128             T02ke1 = p_§LACcompce;
074200141128             T02sif = KNSIF;
074300141128             TNTBE_RicercaControllo(kpjba : tibs02ds);
074400141128             IF  T02err = *blank;
074500141128               V2dceksu = T02uni;
074600141128             ENDIF;
074700141128             VisCEksu = *on;
074800141128           ELSE;
074900141128             VisCEksu = *off;
075000141128           ENDIF;
075100141128
075200140813           V2tpadd  = §LACtpadd;
075300140813           reset  TIBS02ds;
075400141128           clear dTAL;
075500140813           T02cod = 'TAL';
075600140813           T02ke1 = V2tpadd;
075700140813           T02sif = KNSIF;
075800140813           TNTBE_RicercaControllo(kpjba : tibs02ds);
075900140813           IF  T02err = *blank;
076000141128             dTAL = T02uni;
076100141128           ENDIF;
076200141128           V2dtpadd = §TALdes;
076300141202           IF  V2tpadd = *blanks;
076400141202             V2dtpadd = 'NON DICHIARATO';
076500141202           ENDIF;
076600090923
076700090923           *in02 = *off;
076800121217
076900121217           // Cerco i dati della tabella LA2
077000121217           clear dLA2;
077100121217           reset  TIBS02ds;
077200121217           IF  V2Cla2 = 'S';
077300121217             T02cod = 'LA2';
077400121217             T02ke1 = B464ke1;
077500121217             TNTBE_RicercaControllo(kpjba : tibs02ds);
077600121217             IF  T02err = *blank;
077700121217               dLA2 = T02uni;
077800121217             ENDIF;
077900121217           ENDIF;
078000071207
078100071207         ENDIF;
078200090923
078300090923         // Salvo i dati sensibili per la creazione addebito
078400090923         savtad = v2ctad;
078500090923         savtadu = v2tadu;
078600090923         savksc = v2cksc;
078700090923         savctr = v2cctr;
078800090923         savimp = v2imp;
078900090923         savfimp = v2fimp;
079000090923         $forza = *off;
079100130201
079200130201         // Imposto (se c'è) data ultima variazione
079300130201         clear V2datauv;
079400130201         IF  TBEdtr > 0;
079500130201           V2datauv = %subst(%editc(TBEdtr:'X'):7:2) + '/' +
079600130201                      %subst(%editc(TBEdtr:'X'):5:2) + '/' +
079700130201                      %subst(%editc(TBEdtr:'X'):1:4);
079800130201         ENDIF;
079900141128
080000141128         // Cerco la personalizzazione C/E del KSC
080100141203         exec sql
080200141203         select EVDitr into :wceksc
080300141203         FROM ecevd00f
080400141203         WHERE EVDcli = :V2cccm and
080500141203               EVDvoc = 199 and
080600141203               EVDttv = 'K' and
080700141203               EVDddt <= :woggi and
080800141203               EVDdst >= :woggi;
080900141203         IF  sqlcod = 100 or sqlcod < 0;
081000141203           V2ceksc = 'STD';
081100141203         ELSE;
081200141203           V2ceksc = %editw(wceksc:'0 ,   ');
081300141203         ENDIF;
081400141128
081500141128         // Cerco la personalizzazione C/E del KSU
081600141203         IF  VisCeKsu;
081700141203           exec sql
081800141203           select EVDitr into :wceksu
081900141203           FROM ecevd00f
082000141203           WHERE EVDcli = :V2Cksu and
082100141203                 EVDvoc = 199 and
082200141203                 EVDttv = 'K' and
082300141203                 EVDddt <= :woggi and
082400141203                 EVDdst >= :woggi;
082500141203           IF  sqlcod = 100 or sqlcod < 0;
082600141203             V2ceksu = 'STD';
082700141203           ELSE;
082800141203             V2ceksu = %editw(wceksu:'0 ,   ');
082900141203           ENDIF;
083000141203         ENDIF;
083100141203
083200141203         // Data AUT DG per c/e  a 0 o NO ddt
083300141203         IF  §LACdtaudg <> *blanks;
083400141203           exec sql
083500141203           set :V2dtautdg =
083600141203               (select DC_DMYY_Z from DATECNV0F
083700141203                where DC_YYMD_C = :§LACdtaudg);
083800141203         ENDIF;
083900071207
084000071207       ENDSR;
084100071217
084200071217       //--------------------------------------------------------------
084300090306       //?Settaggio indicatori nella videata D02 per abilitazione      ?
084400090306       //?  tasti funzionali                                           ?
084500071217       //--------------------------------------------------------------
084600090306       BEGSR  Set_Ind_D02;
084700090319
084800141204         F4Attivo  = B46opz <> '5';
084900090306         F5Attivo  = ((%found(TNTBE01L) and TBEatb <> *blank)
085000071217                                        and B46opz <> '5');
085100071217         F6Attivo  = (NOT F5Attivo and B46opz <> '5');
085200141205         F8Attivo  = B46opz = '5' and §LACla2 = 'S';
085300090306         F16Attivo = ((%found(TNTBE01L) and TBEatb = *blank)
085400071217                                        and B46opz <> '5');
085500090306
085600120112         // - Se ultima estrazione impostata proteggo la prima estrazione
085700090309         ProtDATAI = (§LACdatela <> *zero);
085800071217
085900071217       ENDSR;
086000141117
086100141117       //--------------------------------------------------------------
086200141117       //?Gestione tasto funzionale F3 da videata D02                  ?
086300141117       //?F3=Fine                                                      ?
086400141117       //--------------------------------------------------------------
086500141117       BEGSR  sr_F03D02;
086600141117
086700141117         // Chiusura del programma
086800141117         $Fine = *on;
086900141117
087000141117         // Restituzione indicazione del tasto funzionale premuto
087100141117         B46fxx = '1';
087200141117
087300141117       ENDSR;
087400090319
087500090319       //--------------------------------------------------------------
087600090319       //?Gestione tasto funzionale F04 da videata D02                 ?
087700090319       //?F04=Interrogazione Padre                                     ?
087800090319       //--------------------------------------------------------------
087900090319       BEGSR  sr_F04D02;
088000090319
088100090319         // Richiamo pgm interrogazione
088200090327         clear  Param01;
088300090327         P01ord = '1';
088400090327         P01ksu = V2Cksu;
088500090327         KPJBU  = Param01;
088600141204         tntb461r (KPJBA : P01opz3);
088700141204         Param01 = kpjbu;
088800141204         IF  V2Cksu = *zeros and P01ke1 <> *zeros and
088900141204             P01ke1 <> *blanks;
089000141204           V2Cksu = %dec(P01ke1:7:0);
089100141204         ENDIF;
089200090319
089300090319       ENDSR;
089400090319
089500141205       //--------------------------------------------------------------
089600141205       //?Gestione tasto funzionale F08 da videata D02                 ?
089700141205       //?F08=Vis. Tabella LA2
089800141205       //--------------------------------------------------------------
089900141205       BEGSR  sr_F08D02;
090000141205
090100141205         // Richiamo pgm tab. LA2
090200141205         exsr GESLA2;
090300141205
090400141205         // Devo reimpostare i dati per visualizzare nuovamente la tab.LAC
090500141205         $Video = 'D2';
090600141205         $InzD02 = *on;
090700141205         chain  %kds(K05tntbe01) TNTBE000;
090800141205
090900141205         // Se torna errore da tabella LA2 lo emetto
091000141205         IF  B464err <> '0' and B464err <> *blanks;
091100141205           ErrMessage  = *on;
091200141205           ErrGenerico = *on;
091300141205           V1Dmsg = B464msg;
091400141205           leavesr;
091500141205         ENDIF;
091600141205
091700141205       ENDSR;
091800141205
091900071217       //--------------------------------------------------------------
092000090306       //?Gestione tasto funzionale F12 da videata D02                 ?
092100090306       //?F12=Ritorno                                                  ?
092200071217       //--------------------------------------------------------------
092300090306       BEGSR  sr_F12D02;
092400071217
092500071217           B46fxx = '2';
092600071217           $Fine = *on;
092700071217
092800071217       ENDSR;
092900090306
093000090306       //--------------------------------------------------------------
093100090306       //?Controllo "annullabilità" del record (padre senza figli)     ?
093200090306       //--------------------------------------------------------------
093300090306       BEGSR  sr_CtrAnnull;
093400090306
093500090306         IndDspF = *zero;
093600090306         // REimpostazione indicatori per abilitazione tasti funzionali
093700090306         exsr  Set_Ind_D02;
093800090306
093900090306         if  V2Cksu <= *zero;
094000090306           V2Cksu = §LACksu;
094100090306         endif;
094200090306
094300090306         // Se figlio (non padre): fine controlli
094400090306         if  V2Cccm <> V2Cksu;
094500090306           leavesr;
094600090306         endif;
094700090306
094800090306         //?Verifica se padre con figli?
094900090327         clear  dLACp;
095000090316         SAVke1 = TBEke1;
095100090306         setll     %kds(k05tntbe01 : 1)  TNTBE000;
095200090306         reade(n)  %kds(k05tntbe01 : 1)  TNTBE000;
095300090306
095400090306         dow  not  %eof(TNTBE01L);
095500090316           if  TBEatb = *blank   and
095600090316               %editc(V2Cccm : 'X') <> %trimr(TBEke1);
095700090306             dLACp = TBEuni;
095800090316             if  p_§LACksu = V2Cccm;
095900090306               errMessage  = *on;
096000090306               errGenerico = *on;
096100090306               PosCurKsu   = *on;
096200090306               V1Dmsg = 'Record NON cancellabile: trattasi di PADRE +
096300090306                         con figli (vedi ' + %trimr(TBEke1) + ')';
096400090306               leavesr;
096500090306             endif;
096600090306           endif;
096700090306         reade(n)  %kds(k05tntbe01 : 1)  TNTBE000;
096800090306         enddo;
096900090316
097000120112         // - Riaggancio del rec. in manutenzione/copia
097100090316         TBEke1 = SAVke1;
097200090316         reset  TNTBE000;
097300090316         k_TBEcod = c_Tab;
097400120112         k_TBEke1 = %editc( V2Cccm : 'X' );
097500090316         clear  k_TBEke2;
097600090316         k_TBElin = TBXlin;
097700090316         k_TBEsif = TBXsif;
097800090316         chain  %kds(K05tntbe01) TNTBE000;
097900090306
098000090306       ENDSR;
098100071206
098200071212       //--------------------------------------------------------------
098300090306       //?Controllo videata D02                                        ?
098400071212       //--------------------------------------------------------------
098500090306       BEGSR  sr_CtrD02;
098600071207
098700090309         %subst(IndDspF : 50) = *off;
098800120112
098900141117         IF  $Copia or *in02;
099000120112
099100120112           Select;
099200120112
099300120112             // -?Codice estrazione (SE copia)?
099400120112             When  V2Cccm = *zero;
099500120112               errMessage  = *on;
099600120112               errGenerico = *on;
099700120112               PosCurCcm2  = *on;
099800120112               V1Dmsg = $Msg(01);
099900120112               leavesr;
100000120112
100100120112             // -?Controllo / Decodifica?
100200120112             When  tibs69ds.I69kac <> V2Cccm  or
100300120112                   k_TBEke1 <> %editc( V2Cccm : 'X' );
100400120112               clear  TIBS69ds;
100500120112               tibs69ds.I69kcc = DUTkci;
100600120112               tibs69ds.I69kac = V2Cccm;
100700120112               tibs69ds.I69sif = knsif;
100800120112               tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
100900120112               if  tibs69ds.O69err = *on;
101000120112                 errMessage  = *on;
101100120112                 errGenerico = *on;
101200120112                 PosCurCcm2  = *on;
101300120112                 V1Dmsg = $Msg(03);
101400120112                 leavesr;
101500120112               endif;
101600120112               V2Dccm = ds_CNACO.ACOrag;
101700120112               // -?Verifica esistenza in tabella?
101800120112               k_TBEke1 = %editc( V2Cccm : 'X' );
101900120112               chain  %kds(K05tntbe01) TNTBE000;
102000120112               select;
102100120112                 // - -?NON ammesso nuovo record per cliente annullato?
102200120112                 when  NOT  %found(TNTBE01L)  and  ds_CNACO.ACOflg <> *blank;
102300120112                   errMessage  = *on;
102400120112                   errGenerico = *on;
102500120112                   PosCurCcm2  = *on;
102600120112                   V1Dmsg = $Msg(04);
102700120112                   leavesr;
102800120112                 // - -?Impossibile inserimento di record già esistente?
102900120112                 when  %found(TNTBE01L);
103000120112                   errMessage  = *on;
103100120112                   errGenerico = *on;
103200120112                   PosCurCcm2  = *on;
103300120112                   V1Dmsg = $Msg(14);
103400120112                   leavesr;
103500120112               endsl;
103600120112
103700120112           EndSl;
103800141202           //?imposto già la directory
103900141202           IF  V2Cdir = *blanks;
104000141202             V2Cdir = %editc(V2Cccm:'X') + '/PI';
104100141202           ENDIF;
104200120112
104300120112         ENDIF;
104400090313
104500090313         // Verifica se variata la schedulazione dell'estrazione
104600090313         if  $ByPass;
104700090313           $ByPass = (ds_SchMm = wSavSch);
104800090313         endif;
104900090313         wSavSch = ds_SchMm;
105000090225
105100090306         //?Codice padre?
105200090225         select;
105300090306           when  V2Cksu  = *zero;
105400090306             errMessage  = *on;
105500090225             errGenerico = *on;
105600090303             PosCurKsu   = *on;
105700090225             V1Dmsg = $Msg(11);
105800090225             leavesr;
105900090306           when  V2Cksu = V2Cccm;
106000090303             V2Dksu = V2Dccm;
106100090225           other;
106200090327             // - Controllo esistenza codice padre come cliente
106300090309             //   in tabella LAC
106400090303             exsr  sr_Padre;
106500090303             if  errGenerico = *on;
106600090226               leavesr;
106700090225             endif;
106800090303             V2Dksu = p_§LACrag;
106900090226         endsl;
107000101103         //?SE codice padre <> da codice cliente?
107100101103         // ?& sono in immissione?
107200141128         IF  V2Cccm <> V2Cksu   and   (*in02 or $copia);
107300101103           // ?=> chiedo se si vogliono riportare i dati del padre?
107400101103           If  W1Ccpy = *blank;
107500101103             $Video  = 'W1';
107600101103             leavesr;
107700101103           EndIf;
107800101103           // ?& il padre ha già fatto estrazioni?
107900101103           If  p_§LACdatela <> *zero;
108000101103             // ?=> riporto le date del padre, proteggo il campo e non controllo?
108100101103             clear wDataI;
108200101103             %subst(wDataI:1:2) = %subst(%editc(p_§LACdatai:'X'):5:2);
108300101103             %subst(wDataI:3:4) = %subst(%editc(p_§LACdatai:'X'):1:4);
108400101103             V2datai = %int(wDataI);
108500101103             clear wDatEla;
108600101103             %subst(wDatEla:1:2) = %subst(%editc(p_§LACdatela:'X'):5:2);
108700101103             %subst(wDatEla:3:4) = %subst(%editc(p_§LACdatela:'X'):1:4);
108800101103             V2datela = %int(wDatEla);
108900101103             ProtDATAI = *on;
109000101103           EndIf;
109100100521         ENDIF;
109200090311
109300090311         //?Tipo elaborazione?
109400090327         select;
109500090327           // - Obbligatorio
109600090327           when  V2Caut <> 'A'  and  V2Caut <> 'S'  and  V2Caut <> 'N';
109700090327             errMessage  = *on;
109800090327             errGenerico = *on;
109900090327             PosCurAut   = *on;
110000090327             V1Dmsg = 'Tipo elaborazione errato o mancante';
110100090327             leavesr;
110200090327         endsl;
110300090226
110400090306         //?Flag nome immagine?
110500090310         clear  V2Dfmi;
110600090310         select;
110700090310           // - Obbligatorietà
110800090310           when  V2Cfmi = *blank;
110900090310             errMessage  = *on;
111000090310             errGenerico = *on;
111100090310             PosCurFmi   = *on;
111200090310             V1Dmsg = 'Flag nome immagine obbligatorio';
111300090310             leavesr;
111400090310           // - Ricerca
111500090310           when  %scan('?' : V2Cfmi) > *zero;
111600090310             clear  TIBS02ds;
111700090310             T02mod = 'R';
111800090310             T02cod = 'NIM';
111900090310             T02sif = KNSIF;
112000090310             TNTBE_RicercaControllo(kpjba : tibs02ds);
112100090310             if  T02err = *blank;
112200090310               V2Cfmi = %subst(T02ke1 : 1 : %len(V2Cfmi));
112300090310               V2Dfmi = T02uni;
112400090310               errGenerico = *on;
112500090310               leavesr;
112600090310             else;
112700090310               errMessage  = *on;
112800090310               errGenerico = *on;
112900090310               PosCurFmi   = *on;
113000090310               V1Dmsg = T02msg;
113100090310               leavesr;
113200090310             endif;
113300090310           // - Controllo
113400090310           other;
113500090310             reset  TIBS02ds;
113600090310             T02cod = 'NIM';
113700090310             T02ke1 = V2Cfmi;
113800090310             TNTBE_RicercaControllo(kpjba : tibs02ds);
113900090310             if  T02err <> *blank;
114000090310               errMessage  = *on;
114100090310               errGenerico = *on;
114200090310               PosCurFmi   = *on;
114300090310               V1Dmsg = 'Flag nome immagine errato';
114400090310               leavesr;
114500090310             endif;
114600090310             V2Dfmi = T02uni;
114700090310         ENDSL;
114800140813
114900140813         //?Tipo immagine?
115000140813         if  V2Ctpi <> 'TIF'   and   V2Ctpi <> 'JPG' and
115100140813             V2Ctpi <> 'PDF';
115200140813           errMessage  = *on;
115300140813           errGenerico = *on;
115400140813           PosCurTpi   = *on;
115500140813           V1Dmsg = 'Tipo immagine errato o mancante';
115600140813           leavesr;
115700140813         endif;
115800090226
115900090306         //?Directory per immagini?
116000090226         select;
116100090306           when  V2Cdir  = *blank;
116200090226             errMessage  = *on;
116300090226             errGenerico = *on;
116400090226             PosCurDir   = *on;
116500090226             V1Dmsg = $Msg(05);
116600090226             leavesr;
116700090316           when  %subst(V2Cdir : 1 : 1) = *blank;
116800090316             errMessage  = *on;
116900090316             errGenerico = *on;
117000090316             PosCurDir   = *on;
117100090316             V1Dmsg = 'Non ammessi spazi vuoti all''inizio del nome +
117200090316                       della directory';
117300090316             leavesr;
117400121217           WHEN  V2Cdir = §LA2dir and V2Cla2 = 'S';
117500121217             errMessage  = *on;
117600121217             errGenerico = *on;
117700121217             PosCurDir   = *on;
117800121217             V1Dmsg = 'La directory deve essere diversa da tab. LA2';
117900121217             leavesr;
118000090306           when  %scan('\':V2Cdir) > *zero;
118100090226             errMessage  = *on;
118200090226             errGenerico = *on;
118300090226             PosCurDir   = *on;
118400090226             V1Dmsg = $Msg(06);
118500090226             leavesr;
118600090306           when  %scan('%':V2Cdir) > *zero   and
118700090306                           V2Caut  = 'A';
118800090226             errMessage  = *on;
118900090226             errGenerico = *on;
119000090226             PosCurDir   = *on;
119100090226             V1Dmsg = $Msg(07);
119200090226             leavesr;
119300090309           when  %scan('&':V2Cdir) > *zero  and
119400090309                           V2Caut  = 'A';
119500090309             errMessage  = *on;
119600090309             errGenerico = *on;
119700090309             PosCurDir   = *on;
119800090309             V1Dmsg = $Msg(07);
119900090309             leavesr;
120000090226         endsl;
120100090309
120200090316         wPos1 = %scan('/' : V2Cdir);
120300090316         DoW  wPos1 > *zero;
120400090316           if  %subst( V2Cdir : wPos1 : 2 ) = '//';
120500090316             errMessage  = *on;
120600090316             errGenerico = *on;
120700090316             PosCurDir   = *on;
120800090316             V1Dmsg = 'Nome directory NON valido: non ammesso il +
120900090316                       doppio "/"';
121000090316             leavesr;
121100090316           else;
121200090316             wPos1 = %scan('/' : V2Cdir : wPos1 + 1 );
121300090316           endif;
121400090316         EndDo;
121500090316
121600090309         wPos1 = %scan('&' : V2Cdir);
121700090309         IF  wPos1 > *zero;
121800090309           if  %subst( V2Cdir : wPos1 : 4 ) <> '&AAA'   and
121900090309               %subst( V2Cdir : wPos1 : 2 ) <> '&M';
122000090309             errMessage  = *on;
122100090309             errGenerico = *on;
122200090309             PosCurDir   = *on;
122300090309             V1Dmsg = 'Ammessi solo "&AAA" e "&M" (come caratteri +
122400090309                       sostituibili)';
122500090309             leavesr;
122600090309           endif;
122700090309           wPos2 = %scan( '&' : V2Cdir : wPos1 + 1 );
122800090309           If  wPos2 > *zero;
122900090309             if  %subst( V2Cdir : wPos2 : 4 ) <> '&AAA'   and
123000090309                 %subst( V2Cdir : wPos2 : 2 ) <> '&M';
123100090309               errMessage  = *on;
123200090309               errGenerico = *on;
123300090309               PosCurDir   = *on;
123400090309               V1Dmsg = 'Ammessi solo "&AAA" e "&M" (come caratteri +
123500090309                         sostituibili)';
123600090309               leavesr;
123700090309             endif;
123800090309           EndIf;
123900090309         ENDIF;
124000090309
124100090309         wPos1 = %scan('&M' : V2Cdir);
124200090309         IF  wPos1 > *zero;
124300090309           wPos2 = %scan( '&M' : V2Cdir : wPos1 + 1 );
124400090309           If  wPos2 > *zero   and
124500090309               %scan( '&M' : V2Cdir : wPos2 + 1 ) > *zero;
124600090309             errMessage  = *on;
124700090309             errGenerico = *on;
124800090309             PosCurDir   = *on;
124900090309             V1Dmsg = 'Ammessi max 2 "&M"';
125000090309             leavesr;
125100090309           EndIf;
125200090309         ENDIF;
125300090226
125400090327         // - Se codice padre <> da codice cliente
125500090306         //   la directory deve coincidere almeno fino alla 2ª subdir
125600090306         IF  V2Cccm <> V2Cksu   and   p_§LACdir <> V2Cdir;
125700090311
125800090311           // - Controllo 1ª directory
125900090311           wPos1 = %scan( '/' : %trim( V2Cdir ) );
126000090311           wPos2 = %scan( '/' : %trim( p_§LACdir ) );
126100090313           If  wPos1 <> wPos2                               OR
126200090313             ( wPos1  > *zero   and   wPos2 > *zero   and
126300090311               %subst( %trim( V2Cdir    ) : 1 : wPos1 ) <>
126400090313               %subst( %trim( p_§LACdir ) : 1 : wPos2 ) )   OR
126500090313             ( wPos1  = *zero   and   wPos2  =  *zero
126600090313                                and   V2Cdir <> p_§LACdir );
126700090311             errMessage  = *on;
126800090311             errGenerico = *on;
126900090311             PosCurDir   = *on;
127000090311             select;
127100090311               when  wPos1 =  *zero   and   wPos2 <> *zero;
127200090316                 V1Dmsg = 'Il padre ha altri rami directory (';
127300090311               when  wPos1 <> *zero   and   wPos2 =  *zero;
127400090311                 V1Dmsg = 'Il padre ha una directory unica (';
127500090311               other;
127600090311                 V1Dmsg = 'Primo ramo directory +
127700090311                           <> da quello del cod. padre (';
127800090311             EndSl;
127900090313             if  %len( %trim( p_§LACdir ) ) <=
128000090311                 %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) ) - 1;
128100090311               V1Dmsg = %trimr( V1Dmsg )
128200090311                      + %trim( p_§LACdir ) + ')';
128300090311             else;
128400090313               wPos1 = %len( V1Dmsg );
128500090313               wPos2 = %len( %trimr( V1Dmsg ) );
128600090311               V1Dmsg = %trimr( V1Dmsg )
128700090311                      + %trim( %subst( p_§LACdir : 1 :
128800090311                          %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) )
128900090311                          - 4 ) )
129000090311                      + '...)';
129100090311             endif;
129200090311             leavesr;
129300090311           EndIf;
129400090311
129500090311           // - Controllo 2ª directory
129600141201           wPos1 = %scan( '/' : %trim( V2Cdir ) : wPos1 + 1 );
129700141201           wPos2 = %scan( '/' : %trim( p_§LACdir ) : wPos2 + 1 );
129800141201           if  wPos1 = *zero;
129900141201             wPos1 = %len( %trimr( V2Cdir ) );
130000090316           else;
130100141201             wPos1 -= 1;
130200090316           endif;
130300141201           if  wPos2 = *zero;
130400141201             wPos2 = %len( %trimr( p_§LACdir ) );
130500090316           else;
130600090316             wPos2 -= 1;
130700090316           endif;
130800090316           If  wPos1 <> wPos2   OR
130900090316               %subst(V2Cdir : 1 : wPos1) <>
131000090316                 %subst(p_§LACdir : 1 : wPos2);
131100090311             errMessage  = *on;
131200090311             errGenerico = *on;
131300090311             PosCurDir   = *on;
131400090313                 V1Dmsg = 'Secondo ramo directory +
131500090311                           <> da quello del cod. padre (';
131600090313             if  %len( %trim( p_§LACdir ) ) <=
131700090311                 %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) ) - 1;
131800090311               V1Dmsg = %trimr( V1Dmsg )
131900090311                      + %trim( p_§LACdir ) + ')';
132000090311             else;
132100090311               V1Dmsg = %trimr( V1Dmsg )
132200090311                      + %trim( %subst( p_§LACdir : 1 :
132300090311                          %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) )
132400090311                          - 4 ) )
132500090311                      + '...)';
132600090311             endif;
132700090311             leavesr;
132800090311           EndIf;
132900090311
133000090306         ENDIF;
133100121016
133200121016         //?Immagine con Merge (LDV + DOC)
133300121016         IF  V2merge = 'S';
133400121016           exsr CTR_merge;
133500121016         ENDIF;
133600121016         IF  ErrGenerico;
133700121016           leavesr;
133800121016         ENDIF;
133900090327
134000090327         //?Tipo porto?
134100090327         if  V2Ctpt <> 'F'   and   V2Ctpt <> 'A'   and   V2Ctpt <> 'E';
134200090327           errMessage  = *on;
134300090327           errGenerico = *on;
134400090327           PosCurTpt   = *on;
134500090327           V1Dmsg = 'Tipo porto errato o mancante';
134600090327           leavesr;
134700090327         endif;
134800090311
134900090311         //?LNP per selezione?
135000090311         if  V2Clnp <> *zero;
135100090311           setll  (V2Clnp)  AZORG;
135200090311           if  NOT  %equal(AZORG01L);
135300090311             errMessage  = *on;
135400090311             errGenerico = *on;
135500090311             PosCurLnp   = *on;
135600090311             V1Dmsg = 'Linea di partenza errata';
135700090311             leavesr;
135800090311           endif;
135900090311         endif;
136000140813
136100140813         //?Tipo data da elaborare?
136200140813         select;
136300140813           when  V2tdta <> ' '  and  V2tdta <> 'C'  and  V2tdta <> 'S';
136400140813             errMessage  = *on;
136500140813             errGenerico = *on;
136600140813             PosCurTdta  = *on;
136700140813             V1Dmsg = '"Flag "Data da elaborare" errato';
136800140813             leavesr;
136900140813           // - Tipo data da elaborare obbligatorio
137000140813           when  V2Caut = 'S'   and   V2tdta = *blank;
137100140813             errMessage  = *on;
137200140813             errGenerico = *on;
137300140813             PosCurTdta  = *on;
137400140813             V1Dmsg = '"Data da elaborare" obbligatoria per +
137500140813                       "Tipo Elaborazione" = "S" (schedulata)';
137600140813             leavesr;
137700140813           // - Se codice padre <> da codice cliente
137800140813           //   il tipo data da elaborare deve essere uguale
137900140813           when  V2Cccm <> V2Cksu   and   p_§LACtdta <> V2tdta;
138000140813             errMessage  = *on;
138100140813             errGenerico = *on;
138200140813             PosCurTdta  = *on;
138300140813             V1Dmsg = 'Tipo data da elaborare +
138400140813                       <> da quello del cod. padre ('
138500140813                    + p_§LACtdta + ')';
138600140813             leavesr;
138700140813         endsl;
138800090309
138900090309         //?Periodo da estrarre in mesi?
139000090310         select;
139100090310           // - Periodo da estrarre in mesi obbligatorio
139200090310           when  V2Caut = 'S'   and   V2mesi = *zero;
139300090310             errMessage  = *on;
139400090310             errGenerico = *on;
139500090310             PosCurMesi  = *on;
139600090310             V1Dmsg = '"Periodo da estrarre" obbligatorio per +
139700090310                       "Tipo Elaborazione" = "S" (schedulata)';
139800090310             leavesr;
139900090310           // - Se codice padre <> da codice cliente
140000090310           //   il periodo da estrarre deve essere uguale
140100090310           when  V2Cccm <> V2Cksu   and   p_§LACmesi <> V2mesi;
140200090310             errMessage  = *on;
140300090310             errGenerico = *on;
140400090310             PosCurMesi  = *on;
140500090310             V1Dmsg = 'Periodo da estrarre +
140600090310                       <> da quello del cod. padre ('
140700090310                    + %editc(p_§LACmesi:'X') + ')';
140800090310             leavesr;
140900090310         endsl;
141000141201
141100141201         //?Prima estrazione dal?
141200141201         select;
141300141201           // - Obbligatoria se tipo elaborazione CD
141400141201           when  V2Caut = 'S'   and   V2datai = *zero;
141500141201             errMessage  = *on;
141600141201             errGenerico = *on;
141700141201             PosCurDatai = *on;
141800141201             V1Dmsg = 'Periodo iniziale della prima estrazione +
141900141201                       obbligatorio';
142000141201             leavesr;
142100141201           // - Verifica correttezza
142200141201           when  V2datai <> *zero                                  AND
142300141201              (%subst( %editc( V2datai : 'X' ) : 1 : 2 ) < '01'   or
142400141201               %subst( %editc( V2datai : 'X' ) : 1 : 2 ) > '12'   or
142500141201               %subst( %editc( V2datai : 'X' ) : 3 : 4 ) < '2000' or
142600141201               %subst( %editc( V2datai : 'X' ) : 3 : 4 ) > %char(*year));
142700141201             errMessage  = *on;
142800141201             errGenerico = *on;
142900141201             PosCurDatai = *on;
143000141201             V1Dmsg = 'Periodo iniziale della prima estrazione +
143100141201                       errato';
143200141201             leavesr;
143300141201         endsl;
143400141201         // - Se codice padre <> da codice cliente
143500141201         //   il periodo deve essere uguale
143600141201         if  V2Cccm <> V2Cksu;
143700141201           clear  wDataI;
143800141201           %subst( wDataI : 1 : 2 ) =
143900141201                   %subst( %editc( p_§LACdatai : 'X' ) : 5 : 2 );
144000141201           %subst( wDataI : 3 : 4 ) =
144100141201                   %subst( %editc( p_§LACdatai : 'X' ) : 1 : 4 );
144200141201           if  V2datai <> %int(wDataI);
144300141201             errMessage  = *on;
144400141201             errGenerico = *on;
144500141201             PosCurDatai = *on;
144600141201             V1Dmsg = 'Periodo iniziale della prima estrazione +
144700141201                       <> da quello del cod. padre ('
144800141201                    + %subst(%editc(p_§LACdatai : 'X') : 5 : 2)
144900141201                    + '/'
145000141201                    + %subst(%editc(p_§LACdatai : 'X') : 1 : 4)
145100141201                    + ')';
145200141201             leavesr;
145300141201           endif;
145400141201         endif;
145500090226
145600090309         //?Schedulazione giorno e mesi x preparazione CD?
145700090226         select;
145800090318           // when (V2Caut  =  'A'    or  V2Caut  =  'N')    AND
145900090318           when  V2Caut  =  'A'    AND
146000130923               // (V2datai <> *zero  or  V2tdta   <> *blank   or
146100130923                (V2mesi  <> *zero  or  V2Cschgg <> *zero    or
146200090311                 ds_SchMm <> *blank);
146300090311             errMessage  = *on;
146400090311             errGenerico = *on;
146500090311             PosCurAut   = *on;
146600090311             V1Dmsg = 'Schedulazione incompatibile con il +
146700090311                       "Tipo Elaborazione" = ' + V2Caut;
146800090311             leavesr;
146900090318           when  V2Caut = 'S'  and  (V2Cschgg <= *zero);
147000090306             errMessage  = *on;
147100090306             errGenerico = *on;
147200090306             PosCurSchGg = *on;
147300090306             V1Dmsg = 'Schedulazione obbligatoria per +
147400090306                       "Tipo Elaborazione" = "S" (schedulata)';
147500090306             leavesr;
147600090306           when  V2Caut = 'S'  and  (ds_SchMm = *blank);
147700090306             errMessage  = *on;
147800090306             errGenerico = *on;
147900090306             PosCurSch01 = *on;
148000090306             V1Dmsg = 'Indicare almeno un mese per +
148100090306                       "Tipo Elaborazione" = "S" (schedulata)';
148200090306             leavesr;
148300090311         endsl;
148400090226
148500090327         // - Se codice padre <> da codice cliente
148600090306         //   i 2 codici devono avere gli stessi dati di estrazione
148700090306         //   gg e mese e n. mesi da elaborare
148800090309         if  V2Cccm <> V2Cksu;
148900090309           select;
149000090309             when  p_§LACschgg <> V2Cschgg;
149100090226               errMessage  = *on;
149200090226               errGenerico = *on;
149300090303               PosCurSchgg = *on;
149400090309               V1Dmsg = 'Giorno creazione CD +
149500090309                         <> da quello del cod. padre ('
149600090309                      + %editc(p_§LACschgg:'X') + ')';
149700090226               leavesr;
149800090303             when  p_SchMm <> ds_SchMm;
149900090226               errMessage  = *on;
150000090226               errGenerico = *on;
150100090226               PosCursch01 = *on;
150200090309               V1Dmsg = 'Mesi di elaborazione +
150300090309                         <> da quelli del cod. padre ('
150400090309                      + p_SchMm + ')';
150500090226               leavesr;
150600090226           endsl;
150700090226         endif;
150800090309
150900090327         // - Verifica congruenza tra mese di schedulazione e
151000090309         //   mesi di estrazione
151100090309         select;
151200090311           // - Nessun dato inserito
151300090310           when  V2mesi   = *zero    and   V2Cschgg = *zero    and
151400090311                 ds_SchMm = *blank;
151500090311           // - Periodo da estrarre in mesi richiesto
151600090310           when  V2mesi = *zero;
151700090310             errMessage  = *on;
151800090310             errGenerico = *on;
151900090310             PosCurMesi  = *on;
152000090310             V1Dmsg = 'Periodo da estrarre obbligatorio SE inserita +
152100090310                       una schedulazione';
152200090310             leavesr;
152300090311           // - Giorno di schedulazione richiesto
152400090310           when  V2Cschgg = *zero;
152500090310             errMessage  = *on;
152600090310             errGenerico = *on;
152700090310             PosCurSchgg = *on;
152800090311             V1Dmsg = $Msg(08);
152900090310             leavesr;
153000090311           // - Selezione mesi richiesta
153100090311           when  ds_SchMm = *blank;
153200090310             errMessage  = *on;
153300090310             errGenerico = *on;
153400090310             PosCurSch01 = *on;
153500090311             V1Dmsg = $Msg(09);
153600090310             leavesr;
153700090311         endsl;
153800090313
153900090313         // - Conteggio/Controllo mesi schedulati
154000090313         clear  wCount;
154100090311         wSchMm_ds = %triml(ds_SchMm);
154200090313         For  xx = 1  To  %elem($SchMm);
154300090313           if  $SchMm(xx) <> *blank;
154400090313             wCount += 1;
154500090313           endif;
154600090313         EndFor;
154700090313
154800090311         select;
154900090313           // - Errori bloccanti:
155000090313           when   wCount <> *zero   and
155100090313                 (V2mesi <> (12 / wCount));
155200090313             errMessage  = *on;
155300090313             errGenerico = *on;
155400090313             PosCurSch01 = *on;
155500090313             V1Dmsg = 'Incongruenza tra il "periodo da estrarre" +
155600090313                       ed i mesi schedulati';
155700090313           // - Errori bypassabili:
155800090313           when ( (V2mesi =  2   and  ($SchMm(03) = *blank     or
155900090311                                       $SchMm(05) = *blank     or
156000090311                                       $SchMm(07) = *blank     or
156100090311                                       $SchMm(09) = *blank     or
156200090311                                       $SchMm(11) = *blank))   or
156300090313                  (V2mesi =  3   and  ($SchMm(04) = *blank     or
156400090311                                       $SchMm(07) = *blank     or
156500090311                                       $SchMm(10) = *blank))   or
156600090313                  (V2mesi =  4   and  ($SchMm(05) = *blank     or
156700090311                                       $SchMm(09) = *blank))   or
156800090313                  (V2mesi =  6   and   $SchMm(07) = *blank) )  AND
156900090313                 $ByPass =  *off;
157000090313             $ByPass = *on;
157100090309             errMessage  = *on;
157200090309             errGenerico = *on;
157300090310             PosCurSch01 = *on;
157400090313             V1Dmsg = 'Mesi schedulati in maniera irregolare... +
157500090313                       Premere Enter per forzare.';
157600090309             leavesr;
157700090313           when ( (V2mesi =  2  and  ($SchMm(02) <> *blank     or
157800090311                                      $SchMm(04) <> *blank     or
157900090311                                      $SchMm(06) <> *blank     or
158000090311                                      $SchMm(08) <> *blank     or
158100090311                                      $SchMm(10) <> *blank     or
158200090311                                      $SchMm(12) <> *blank))   OR
158300090313                  (V2mesi =  3  and  ($SchMm(02) <> *blank     or
158400090311                                      $SchMm(03) <> *blank     or
158500090311                                      $SchMm(05) <> *blank     or
158600090311                                      $SchMm(06) <> *blank     or
158700090311                                      $SchMm(08) <> *blank     or
158800090311                                      $SchMm(09) <> *blank     or
158900090311                                      $SchMm(11) <> *blank     or
159000090311                                      $SchMm(12) <> *blank))   OR
159100090313                  (V2mesi =  4  and  ($SchMm(02) <> *blank     or
159200090311                                      $SchMm(03) <> *blank     or
159300090311                                      $SchMm(04) <> *blank     or
159400090311                                      $SchMm(06) <> *blank     or
159500090311                                      $SchMm(07) <> *blank     or
159600090311                                      $SchMm(08) <> *blank     or
159700090311                                      $SchMm(10) <> *blank     or
159800090311                                      $SchMm(11) <> *blank     or
159900090311                                      $SchMm(12) <> *blank))   OR
160000090313                  (V2mesi =  6  and  ($SchMm(02) <> *blank     or
160100090311                                      $SchMm(03) <> *blank     or
160200090311                                      $SchMm(04) <> *blank     or
160300090311                                      $SchMm(05) <> *blank     or
160400090311                                      $SchMm(06) <> *blank     or
160500090311                                      $SchMm(08) <> *blank     or
160600090311                                      $SchMm(09) <> *blank     or
160700090311                                      $SchMm(10) <> *blank     or
160800090311                                      $SchMm(11) <> *blank     or
160900090311                                      $SchMm(12) <> *blank))   OR
161000090313                  (V2mesi = 12  and  ($SchMm(02) <> *blank     or
161100090311                                      $SchMm(03) <> *blank     or
161200090311                                      $SchMm(04) <> *blank     or
161300090311                                      $SchMm(05) <> *blank     or
161400090311                                      $SchMm(06) <> *blank     or
161500090311                                      $SchMm(07) <> *blank     or
161600090311                                      $SchMm(08) <> *blank     or
161700090311                                      $SchMm(09) <> *blank     or
161800090311                                      $SchMm(10) <> *blank     or
161900090311                                      $SchMm(11) <> *blank     or
162000090313                                      $SchMm(12) <> *blank)) ) AND
162100090313                 $ByPass =  *off;
162200090313             $ByPass = *on;
162300090310             errMessage  = *on;
162400090310             errGenerico = *on;
162500090311             PosCurSch01 = *on;
162600090313             V1Dmsg = 'Mesi schedulati in maniera irregolare... +
162700090313                       Premere Enter per forzare.';
162800090310             leavesr;
162900090313           // - Controllo giorno del mese (bypassabile)
163000090313           when ( (V2Cschgg > 28  and  V2Csch02 <> *blank)  or
163100090313                  (V2Cschgg > 30  and (V2Csch04 <> *blank
163200090313                                   or  V2Csch06 <> *blank
163300090313                                   or  V2Csch09 <> *blank
163400090313                                   or  V2Csch11 <> *blank)) )
163500090313                and  $CtrMm = *off;
163600090313             $CtrMm = *on;
163700090313             errMessage  = *on;
163800090313             errGenerico = *on;
163900090313             PosCurSch01 = (V2Csch01 <> *blank);
164000090313             PosCurSch02 = (V2Csch02 <> *blank);
164100090313             PosCurSch04 = (V2Csch04 <> *blank);
164200090313             PosCurSch06 = (V2Csch06 <> *blank);
164300090313             PosCurSch09 = (V2Csch09 <> *blank);
164400090313             PosCurSch11 = (V2Csch11 <> *blank);
164500090313             V1Dmsg = %replace( %char(V2Cschgg) : $Msg(10) :
164600090313                                %scan('&g':$Msg(10)) );
164700090313             leavesr;
164800090313         endsl;
164900090226
165000090309         //?Frequenza addebito?
165100090310         select;
165200090327           // - Obbligatoria
165300090327           when  V2tadu <> 'I'   and   V2tadu <> 'J'   and
165400090327                 V2tadu <> 'S'   and   V2tadu <> 'M';
165500090327             errMessage  = *on;
165600090327             errGenerico = *on;
165700090327             PosCurTadu  = *on;
165800090327             V1Dmsg = 'Frequenza addebito errata o mancante';
165900090327             leavesr;
166000090327           // - Se tipo elaborazione CD la creazione addebito non può
166100090313           //   essere mensile o settimanale
166200090310           when  V2Caut = 'S'  and  (V2tadu = 'S'  or  V2tadu = 'M');
166300090310             errMessage  = *on;
166400090310             errGenerico = *on;
166500090310             PosCurTadu  = *on;
166600090310             V1Dmsg = 'Per creazione CD non è possibile addebito +
166700090310                       mensile o settimanale';
166800090310             leavesr;
166900090327           // - Se codice padre <> da codice cliente
167000090327           //   devono avere la stessa frequenza addebito
167100090310           when  V2Cccm <> V2Cksu   and   p_§LACtadu <> V2tadu;
167200090310             errMessage  = *on;
167300090310             errGenerico = *on;
167400090310             PosCurTadu  = *on;
167500090310             V1Dmsg = 'Frequenza addebito +
167600090310                        <> da quella del cod. padre ('
167700090310                    + p_§LACtadu + ')';
167800090310             leavesr;
167900090310         endsl;
168000090309
168100090309         //?Tipo addebito (Creazione TITAS)?
168200090310         select;
168300090327           // - Obbligatorio
168400090327           when  V2Ctad <> 'N'  and  V2Ctad <> 'S'  and  V2Ctad <> 'V';
168500090327             errMessage  = *on;
168600090327             errGenerico = *on;
168700090327             PosCurTad   = *on;
168800090327             V1Dmsg = 'Flag "Creazione TITAS" errato o mancante';
168900090327             leavesr;
169000090327           // - Se "N" non impostare i dati dell'addebito forzato
169100090310           when  V2Ctad = 'N'     and
169200090310                (V2Cksc <> *zero  or  V2Cctr <> *blank
169300090310                                  or  V2imp <> *zero);
169400090310             errMessage  = *on;
169500090310             errGenerico = *on;
169600090310             PosCurTad   = *on;
169700090310             V1Dmsg = 'Se NO addebito non impostare le forzature';
169800090310             leavesr;
169900090327           // - Se codice padre <> da codice cliente
170000090310           //   devono avere lo stesso tipo addebito
170100090310           when  V2Cccm <> V2Cksu   and   p_§LACtad <> V2Ctad;
170200090310             errMessage  = *on;
170300090310             errGenerico = *on;
170400090310             PosCurTad   = *on;
170500090310             V1Dmsg = 'Tipo addebito +
170600090310                       <> da quello del cod. padre ('
170700090310                        + p_§LACtad + ')';
170800090310             leavesr;
170900090310         endsl;
171000090226
171100090311         //?Dati da forzare in tassazione:?
171200090312         //?- Codice cliente / Tariffa?
171300090312         clear  TIBS69ds;
171400090312         if  V2Cksc <> *zero;
171500090312           tibs69ds.I69kcc = DUTkci;
171600090312           tibs69ds.I69kac = %int(V2Cksc);
171700090312           tibs69ds.I69sif = knsif;
171800090312           tibs69r(TIBS69ds : ds_CNACO :
171900090312                              ds_CNIND :
172000090312                              ds_CNCLP :
172100090312                              ds_FNCLS);
172200090312         endif;
172300090312         select;
172400090312           when  tibs69ds.O69err = *on;
172500090312             errMessage  = *on;
172600090312             errGenerico = *on;
172700090312             PosCurKsc   = *on;
172800090312             V1Dmsg = 'Cliente tassazione errato';
172900090312             leavesr;
173000090312           when  V2Cksc <> *zero   and   V2Cctr = *blank;
173100090312             errMessage  = *on;
173200090312             errGenerico = *on;
173300090312             PosCurCtr   = *on;
173400090312             V1Dmsg = 'Codice tariffa obbligatorio SE +
173500090312                       inserito il cliente tassazione';
173600090312             leavesr;
173700090311           // - Non ammessa tariffa senza cliente
173800090311           when  V2Cksc =  *zero   and   V2Cctr <> *blank;
173900090311             errMessage  = *on;
174000090311             errGenerico = *on;
174100090311             PosCurKsc   = *on;
174200090311             V1Dmsg = 'Cliente tassazione obbligatorio SE +
174300090311                       inserita la relativa tariffa';
174400090311             leavesr;
174500090311           // - Ammesso solo un codice tariffa numerico
174600090311           when  V2Cctr <> *blank   and
174700090311                 %check(c_Digits : V2Cctr) > *zero;
174800090311             errMessage  = *on;
174900090311             errGenerico = *on;
175000090312             PosCurCtr   = *on;
175100090312             V1Dmsg = 'Ammessi SOLO caratteri numerici';
175200090311             leavesr;
175300090310         endsl;
175400090226
175500090327         // - Se codice padre <> da codice cliente
175600090306         //   stesso ksc/ctr/importo
175700090303         if  V2Cccm <> V2Cksu;
175800090226           select;
175900090303             when  p_§LACksc <> V2Cksc;
176000090226               errMessage  = *on;
176100090226               errGenerico = *on;
176200090303               PosCurKsc   = *on;
176300090309               V1Dmsg = 'Cliente tassazione +
176400090309                         <> da quello del cod. padre ('
176500090309                      + %editc(p_§LACksc : 'X') + ')';
176600090226               leavesr;
176700090303             when  p_§LACctr <> V2Cctr;
176800090226               errMessage  = *on;
176900090226               errGenerico = *on;
177000090303               PosCurCtr   = *on;
177100090309               V1Dmsg = 'Codice tariffa +
177200090309                         <> quello del cod. padre ('
177300090309                      + p_§LACctr + ')';
177400090226               leavesr;
177500090226           endsl;
177600090226         endif;
177700090226
177800090309         //?Importo & Tipo importo forzato?
177900090310         select;
178000090310           // - Non ammesso importo per Varia Negata
178100090310           when  V2Ctad = 'V'   and   V2imp <> *zero;
178200090310             errMessage  = *on;
178300090310             errGenerico = *on;
178400090310             PosCurImp   = *on;
178500090310             V1Dmsg = 'NON ammesso l''importo forzato per +
178600090310                       Varia Negata';
178700090310             leavesr;
178800090310           // - Non ammesso tipo importo senza importo
178900090310           when  V2imp =  *zero   and   V2fimp <> *blank;
179000090310             errMessage  = *on;
179100090310             errGenerico = *on;
179200090310             PosCurImp   = *on;
179300090310             V1Dmsg = 'Importo forzato obbligatorio SE +
179400090310                       inserito il tipo di importo';
179500090310             leavesr;
179600090312           // - Se inserito importo devono essere inseriti:
179700090312           //   · il codice cliente forzato (ksc),
179800090312           //   · la tariffa forzata (ctr) ed
179900090312           //   · il tipo importo (fimp)
180000090310           when  V2imp <> *zero;
180100090310             select;
180200090310               when  V2fimp  = *blank;
180300090310                 errMessage  = *on;
180400090310                 errGenerico = *on;
180500090310                 PosCurFimp  = *on;
180600090310                 V1Dmsg = 'Immettere il tipo importo';
180700090310                 leavesr;
180800090310               when  V2Cksc  = *zero;
180900090310                 errMessage  = *on;
181000090310                 errGenerico = *on;
181100090310                 PosCurKsc   = *on;
181200090310                 V1Dmsg = 'Immettere il cliente forzato';
181300090310                 leavesr;
181400090312               when  V2Cctr  = *blank;
181500090310                 errMessage  = *on;
181600090310                 errGenerico = *on;
181700090310                 PosCurCtr   = *on;
181800090310                 V1Dmsg = 'Immettere la tariffa forzata';
181900090310                 leavesr;
182000090310             endsl;
182100090310         endsl;
182200090312
182300090327         // Verifica compatibilità dell'importo forzato con il
182400090312         //   massimo imponibile IVA in tab. "GEI"/"LAC".
182500090312         if  V2imp <> *zero   and   V2fimp = 'T';
182600090312           // - Reperimento divisa moneta di conto gestionale
182700090312           clear  dGED;
182800090312           reset  TIBS02ds;
182900090312           T02cod = 'GED';
183000090312           T02ke1 = '1';
183100090312           TNTBE_RicercaControllo(kpjba : tibs02ds);
183200090312           if  T02err = *blank;
183300090312             dGED = T02uni;
183400090312           endif;
183500090312           // - Reperimento importi da tab. "GEI"
183600090312           //   (interessa il massimo imponibile IVA per spedizione)
183700090312           clear  dGEI;
183800090312           reset  TIBS02ds;
183900090312           T02cod = 'GEI';
184000090312           T02ke1 = §GEDcn;
184100090312           TNTBE_RicercaControllo(kpjba : tibs02ds);
184200090312           if  T02err = *blank;
184300090312             dGEI = T02uni;
184400090312           endif;
184500090312           if  V2imp > §GEImf;
184600090312             errMessage  = *on;
184700090312             errGenerico = *on;
184800090312             PosCurImp   = *on;
184900090312             V1Dmsg = 'Importo superiore al MASSIMO IMPONIBILE IVA +
185000090312                       PER SPEDIZIONE ('
185100090312                      + %trim( %editc( §GEImf : 'J' ) ) + ')';
185200090312             leavesr;
185300090312           endif;
185400090312         endif;
185500090226
185600090312         // Se codice padre <> da codice cliente
185700090306         //   stesso importo e tipo importo
185800090303         if  V2Cccm <> V2Cksu;
185900090226           select;
186000090303             when  p_§LACimp <> V2imp;
186100090226               errMessage  = *on;
186200090226               errGenerico = *on;
186300090312               PosCurImp   = *on;
186400090309               V1Dmsg = 'Importo forzato +
186500090309                         <> da quello del cod. padre ('
186600090309                      + %editc(p_§LACimp : 'J') + ')';
186700090226               leavesr;
186800090303             when  p_§LACfimp <> V2fimp;
186900090226               errMessage  = *on;
187000090226               errGenerico = *on;
187100090312               PosCurFimp  = *on;
187200090309               V1Dmsg = 'Tipo importo forzato +
187300090309                         <> da quello del cod. padre ('
187400090309                      + p_§LACfimp + ')';
187500090226               leavesr;
187600090226           endsl;
187700090225         endif;
187800090923
187900090923         //?Modifica dati sensibili per l'addebito
188000090923         if (v2tadu <> savtadu or v2ctad <> savtad or
188100090923             v2cksc <> savksc or v2cctr <> savctr or
188200141128             v2imp <> savimp or v2fimp <> savfimp) and
188300141128             (not *in02 and not $Copia)
188400090923             and not $forza;
188500090923               errMessage  = *on;
188600090923               errGenerico = *on;
188700090923               PosCurTadu  = *on;
188800090923               V1Dmsg = 'ATTENZIONE!! Possibili problemi ' +
188900090923                        'in fase di addebito. Avvisare Monica/Nicola';
189000090923               $forza = *on;
189100090923               leavesr;
189200090923         endif;
189300110713
189400110713         //?Creazione file elenco per il cliente
189500110713         //?possibile solo se NON è elaborazione automatica (FTP)
189600110713         IF  V2pgm <> *blanks and V2caut = 'A';
189700110713           ErrMessage  = *on;
189800110713           ErrGenerico = *on;
189900110713           PosCurPgm   = *on;
190000110713           V1Dmsg = 'NON possibile creazione elenco se elaborazione '+
190100110713                    'automatica';
190200110713           leavesr;
190300110713         ENDIF;
190400110713         //?se inserito il nome del programma deve esserci anche il tipo elenco
190500110713         IF  V2pgm <> *blanks and V2ele = *blanks;
190600110713           ErrMessage  = *on;
190700110713           ErrGenerico = *on;
190800110713           PosCurEle   = *on;
190900110713           V1Dmsg = 'Immettere anche il tipo elenco da creare';
191000110713           leavesr;
191100110713         ENDIF;
191200110713         //?e viceversa se c'è il tipo elenco deve esserci anche il nome del pgm
191300110713         IF  V2pgm = *blanks and V2ele <> *blanks;
191400110713           ErrMessage  = *on;
191500110713           ErrGenerico = *on;
191600110713           PosCurPgm   = *on;
191700110713           V1Dmsg = 'Immettere anche il nome del programma x elenco da creare';
191800110713           leavesr;
191900110713         ENDIF;
192000110713         //?Se codice padre <> da codice cliente
192100110713         //?  devono avere gli stessi dati
192200110713         IF  V2Cccm <> V2Cksu;
192300110713           SELECT;
192400110713             WHEN  p_§LACpgm <> V2pgm;
192500110713               ErrMessage  = *on;
192600110713               ErrGenerico = *on;
192700110713               PosCurPgm   = *on;
192800110713               V1Dmsg = 'Nome programma +
192900110713                         <> da quello del cod. padre ('
193000110713                      + p_§LACpgm + ')';
193100110713               leavesr;
193200110713             WHEN  p_§LACele <> V2ele;
193300110713               ErrMessage  = *on;
193400110713               ErrGenerico = *on;
193500110713               PosCurEle   = *on;
193600110713               V1Dmsg = 'Tipo elenco +
193700110713                         <> da quello del cod. padre ('
193800110713                      + p_§LACele + ')';
193900110713               leavesr;
194000110713           ENDSL;
194100110713         ENDIF;
194200141128
194300141128         //?Competenze C/E dichiarate
194400141128         IF  (*in02 or $copia) and V2compce = *blanks;
194500141128           ErrGenerico = *on;
194600141128           ErrMessage  = *on;
194700141128           PosCurCompCE = *on;
194800141128           V1Dmsg = 'Competenze C/E obbligatorie';
194900141128           leavesr;
195000141128         ENDIF;
195100141201         IF  V2compce <> *blanks;
195200141128         clear  V2dcompce;
195300141128         IF  %scan('?' : V2compce) > *zero;
195400141128           clear  TIBS02ds;
195500141128           T02mod = 'R';
195600141128           T02cod = 'CED';
195700141128           T02sif = KNSIF;
195800141128           TNTBE_RicercaControllo(kpjba : tibs02ds);
195900141128           IF  T02err = *blank;
196000141128             V2compce  = T02ke1;
196100141128             V2dcompce = T02uni;
196200141128             errGenerico = *on;
196300141128             leavesr;
196400141128           ELSE;
196500141128             errMessage  = *on;
196600141128             errGenerico = *on;
196700141128             PosCurCompCE = *on;
196800141128             V1Dmsg = T02msg;
196900141128             leavesr;
197000141128           ENDIF;
197100141128         ELSE;
197200141128           reset  TIBS02ds;
197300141128           T02cod = 'CED';
197400141128           T02ke1 = V2compce;
197500141128           T02sif = KNSIF;
197600141128           TNTBE_RicercaControllo(kpjba : tibs02ds);
197700141128           IF  T02err = *blank;
197800141128             V2dcompce = T02uni;
197900141128           ELSE;
198000141128             ErrMessage  = *on;
198100141128             ErrGenerico = *on;
198200141128             PosCurCompCE = *on;
198300141128             V1Dmsg = 'Competenze C/E errate';
198400141128             leavesr;
198500141128           ENDIF;
198600141128         ENDIF;
198700141202         ELSE;
198800141202           V2dcompce = 'NON DICHIARATO';
198900141201         ENDIF;
199000141128         //?Se codice padre <> da codice cliente
199100141128         //?  devono avere gli stessi dati
199200141128         //?  ma solo se sono in immissione/copia
199300141201         //?  e se il padre ha la personalizzazione impostata
199400141128         IF  V2Cccm <> V2Cksu and
199500141201             p_§LACcompce <> V2compce and (*in02 or $copia)
199600141201             and p_§LACcompce <> *blanks;
199700141128           ErrMessage   = *on;
199800141128           ErrGenerico  = *on;
199900141128           PosCurCompCE = *on;
200000141128           V1Dmsg = 'Competenze C/E +
200100141128                     <> da quelle del cod. padre ('
200200141128                   + p_§LACcompce + ')';
200300141128           leavesr;
200400141128         ENDIF;
200500141128
200600141128         //?Tipo Addebito dichiarato
200700141128         clear  V2dtpadd;
200800141128         IF  %scan('?' : V2tpadd) > *zero;
200900141128           clear  TIBS02ds;
201000141128           clear dTAL;
201100141128           T02mod = 'R';
201200141128           T02cod = 'TAL';
201300141128           T02sif = KNSIF;
201400141128           TNTBE_RicercaControllo(kpjba : tibs02ds);
201500141128           IF  T02err = *blank;
201600141128             V2tpadd  = T02ke1;
201700141128             dTAL = T02uni;
201800141128             V2dtpadd = §TALdes;
201900141128             errGenerico = *on;
202000141128             leavesr;
202100141128           ELSE;
202200141128             errMessage  = *on;
202300141128             errGenerico = *on;
202400141128             PosCurTpAdd = *on;
202500141128             V1Dmsg = T02msg;
202600141128             leavesr;
202700141128           ENDIF;
202800141128         ELSE;
202900141202         IF  V2tpadd = *blanks;
203000141202           V2dtpadd = 'NON DICHIARATO';
203100141202         ELSE;
203200141128           reset  TIBS02ds;
203300141128           clear dTAL;
203400141128           T02cod = 'TAL';
203500141128           T02ke1 = V2tpadd;
203600141128           T02sif = KNSIF;
203700141128           TNTBE_RicercaControllo(kpjba : tibs02ds);
203800141128           IF  T02err = *blank;
203900141128             dTAL = T02uni;
204000141128             V2dtpadd = §TALdes;
204100141128           ELSE;
204200141128             ErrMessage  = *on;
204300141128             ErrGenerico = *on;
204400141128             PosCurTpAdd = *on;
204500141128             V1Dmsg = 'Tipo Addebito errato';
204600141128             leavesr;
204700141128           ENDIF;
204800141202         ENDIF;
204900141128         ENDIF;
205000141128         //?Se codice padre <> da codice cliente
205100141128         //?  devono avere gli stessi dati
205200141128         //?  ma solo se sono in immissione/copia
205300141201         //?  e se il padre ha il campo impostato
205400141128         IF  V2Cccm <> V2Cksu and
205500141201             p_§LACtpadd <> V2tpadd and (*in02 or $copia)
205600141201             and p_§LACtpadd <> *blanks;
205700141128           ErrMessage  = *on;
205800141128           ErrGenerico = *on;
205900141128           PosCurTpAdd = *on;
206000141128           V1Dmsg = 'Tipo addebito +
206100141128                     <> da quello del cod. padre ('
206200141128                   + p_§LACtpadd + ')';
206300141128           leavesr;
206400141128         ENDIF;
206500141203
206600141203         //?Data Aut. DG
206700141203         IF  V2dtautdg <> *zeros;
206800141203           exec sql
206900141203           select count(*) into :conta
207000141203           from DATECNV0F
207100141203           where DC_DMYY_Z = :V2dtautdg or DC_DMY_Z = :V2dtautdg;
207200141203           IF  conta = 0;
207300141203             ErrMessage  = *on;
207400141203             ErrGenerico = *on;
207500141203             PosCurAutDg = *on;
207600141203             V1Dmsg = $Msg(15);
207700141203             leavesr;
207800141203           ENDIF;
207900141203           IF  V2dtautdg < 1000000;
208000141203             exec sql
208100141203             set :V2dtautdg = (select DC_DMYY_Z from DATECNV0F
208200141203                               where DC_DMY_Z = :V2dtautdg);
208300141203           ENDIF;
208400141203         ENDIF;
208500071207
208600071207       ENDSR;
208700090225
208800090225       //--------------------------------------------------------------
208900090306       //?Controllo esistenza del codice padre in tabella LAC          ?
209000090225       //--------------------------------------------------------------
209100090303       BEGSR  sr_Padre;
209200090302
209300090327         // Controlla esistenza in tabella "LAC"
209400090310         clear  dLACp;
209500090310         reset  TIBS02ds;
209600090310         T02sif = KNSIF;
209700090310         %subst(T02ke1 : 1 : 7) = %editc(V2Cksu : 'X');
209800090310         TNTBE_RicercaControllo(kpjba : tibs02ds);
209900090310         if  T02err = *blank;
210000090310           dLACp  = T02uni;
210100090310         else;
210200090310           errMessage  = *on;
210300090310           errGenerico = *on;
210400090310           PosCurksu   = *on;
210500090310           V1Dmsg = $Msg(12);
210600090310           leavesr;
210700090310         endif;
210800090327
210900090327         // Controlla se già inserito come figlio di altro padre
211000090327         if  p_§LACksu <> V2Cksu;
211100090327           errMessage  = *on;
211200090327           errGenerico = *on;
211300090327           PosCurKsu   = *on;
211400090327           V1Dmsg = 'Non ammesso cod. PADRE già FIGLIO di altro +
211500090327                     cliente (' + %editc(p_§LACksu : 'X') + ')';
211600090327           leavesr;
211700090327         endif;
211800090225
211900090225       ENDSR;
212000121016
212100121016       //--------------------------------------------------------------
212200121016       //?Controllo Merge.
212300121016       //--------------------------------------------------------------
212400121016       BEGSR  CTR_merge;
212500121016
212600121016       // -?L'immagine se con Merge deve essere di tipo PDF
212700121016         IF  V2Ctpi <> 'PDF';
212800121016           errMessage  = *on;
212900121016           errGenerico = *on;
213000121016           V1Dmsg = 'Immagine con Merge, deve essere di tipo PDF';
213100121016           PosCurTpi = *on;
213200121016           leavesr;
213300121016         ENDIF;
213400121016
213500121016       // -?La directory deve finire con /PF
213600121016         wPos1 = %scan('  ':V2Cdir);
213700121016         wPos1 -= 3;
213800121016         IF  %subst(V2Cdir:wPos1:3) <> '/PF';
213900121016           ErrMessage  = *on;
214000121016           ErrGenerico = *on;
214100121016           PosCurDir   = *on;
214200121016           V1Dmsg = 'Immagine con Merge, l''ultimo ramo della +
214300121016                     DIR deve essere "PF"';
214400121016           leavesr;
214500121016         ENDIF;
214600121016
214700121016       // -?Cerco il corrispondente in JDC
214800121016         clear  TIBS02DS;
214900121016         T02mod = 'C';
215000121016         T02cod = 'JDC';
215100121016         T02ke1 = %editc(V2Cccm:'X');
215200121016         TNTBE_RicercaControllo(kpjba : tibs02ds);
215300121016         // -?Non trovo la tabella
215400121016         IF  T02err <> *blanks;
215500121016           // -?Immissione forzo
215600121016           IF  *in02 and not wForza;
215700121016             wForza = *on;
215800121016             ErrMessage  = *on;
215900121016             ErrGenerico = *on;
216000121016             V1Dmsg = 'Ricordarsi di attivare anche la corrispondente +
216100121016                       tabella JDC. Enter forza.';
216200121016             leavesr;
216300121016           ENDIF;
216400121016         // -?Manutenzione blocco
216500121016           IF  not *in02;
216600121016             ErrMessage  = *on;
216700121016             ErrGenerico = *on;
216800121016             V1Dmsg = 'Manca la corrispondente tabella JDC';
216900121016             leavesr;
217000121016           ENDIF;
217100121016         // -?Trovo la tabella
217200121016         ELSE;
217300121016           dJDC = T02uni;
217400121016         // -?Controllo il tipo immagine
217500121030           IF  V2Ctpi <> §JDCtpi and not wForzaTPI;
217600121030             wForzaTPI = *on;
217700121016             ErrMessage  = *on;
217800121016             ErrGenerico = *on;
217900121016             PosCurTpi   = *on;
218000121016             V1Dmsg = 'Tipo immagine diverso dalla corrispondente +
218100121016                       tabella JDC --> ' + §JDCtpi;
218200121016             leavesr;
218300121016           ENDIF;
218400121016         // -?Controllo il nome immagine
218500121030           IF  V2Cfmi <> §JDCfmi and not wForzaFMI;
218600121030             wForzaFMI = *on;
218700121016             ErrMessage  = *on;
218800121016             ErrGenerico = *on;
218900121016             PosCurFmi   = *on;
219000121016             V1Dmsg = 'Nome immagine diverso dalla corrispondente +
219100121016                       tabella JDC --> ' + §JDCfmi;
219200121016             leavesr;
219300121016           ENDIF;
219400121016         // -?Controllo la directory
219500121016           wPos2 = %scan('  ':§JDCdir);
219600121016           wPos2 -= 3;
219700121016           IF  %subst(V2Cdir:1:wPos1) <>
219800121030               %subst(§JDCdir:1:wPos2) and not wForzaDIR;
219900121030             wForzaDIR = *on;
220000121016             ErrMessage  = *on;
220100121016             ErrGenerico = *on;
220200121016             PosCurDir   = *on;
220300121016             V1Dmsg = 'Directory diversa dalla corrispondente +
220400121016                       tabella JDC --> ' + %subst(§JDCdir:1:wPos2);
220500121016             leavesr;
220600121016           ENDIF;
220700121016         ENDIF;
220800121016
220900121016       ENDSR;
221000101103
221100101103       //--------------------------------------------------------------
221200101103       //?Gestione window W01                                          ?
221300101103       //--------------------------------------------------------------
221400101103       BEGSR  sr_GesW01;
221500101103
221600101103         // -?Emissione window?
221700101103         exfmt  TB46W01;
221800101103
221900101103         errMessage  = *off;
222000101103         errGenerico = *off;
222100101103         clear  V1Dmsg;
222200101103
222300101103         $Video = 'D2';
222400101103
222500101103         select;
222600101103           // -?F12=Ritorno?
222700101103           when  dsp_aid = c_F12;
222800101103             leavesr;
222900101103           // -?Invio con "S"?
223000101103           when  W1Ccpy = 'S';
223100101103             if  V2Caut = *blank;
223200101103               V2Caut   = p_§LACaut;
223300101103             endif;
223400101103             if  V2Ctpi = *blank;
223500101103               V2Ctpi   = p_§LACtpi;
223600101103             endif;
223700101103             if  V2Cfmi = *blank;
223800101103               V2Cfmi   = p_§LACfmi;
223900101103               clear  V2Dfmi;
224000101103             endif;
224100121016             if  V2merge = *blank;
224200121016               V2merge  = p_§LACmerge;
224300121016             endif;
224400121217             if  V2Cla2 = *blank;
224500121217               V2Cla2 = p_§LACla2;
224600121217             endif;
224700101103             //if  V2Cdir = *blank;      //? Directory ?
224800101103               V2Cdir   = p_§LACdir;     //? COMUNQUE  ?
224900101103             //endif;                    //? del padre ?
225000101103             if  V2Ctpt = *blank;
225100101103               V2Ctpt   = p_§LACtpt;
225200101103             endif;
225300101103             if  V2Cssr = *blank;
225400101103               V2Cssr   = p_§LACssr;
225500101103             endif;
225600101103             if  V2Clnp = *zero;
225700101103               V2Clnp   = p_§LAClnp;
225800101103             endif;
225900101103             if  V2Cres = *blank;
226000101103               V2Cres   = p_§LACres;
226100101103             endif;
226200101103             if  V2Crec = *blank;
226300101103               V2Crec   = p_§LACrec;
226400101103             endif;
226500101103             if  V2Ccsr = *blank;
226600101103               V2Ccsr   = p_§LACcsr;
226700101103             endif;
226800101103             if  V2tdta = *blank;
226900101103               V2tdta   = p_§LACtdta;
227000101103             endif;
227100101103             if  V2mesi = *zero;
227200101103               V2mesi   = p_§LACmesi;
227300101103             endif;
227400101103             if  V2Cschgg = *zero;
227500101103               V2Cschgg = p_§LACschgg;
227600101103             endif;
227700101103             if  ds_SchMm = *blank;
227800101103               ds_SchMm = p_SchMm;
227900101103             endif;
228000101103             if  V2tadu = *blank;
228100101103               V2tadu   = p_§LACtadu;
228200101103             endif;
228300101103             if  V2Ctad = *blank;
228400101103               V2Ctad   = p_§LACtad;
228500101103             endif;
228600101103             if  V2Cksc = *zero;
228700101103               V2Cksc   = p_§LACksc;
228800101103             endif;
228900101103             if  V2Cctr = *blank;
229000101103               V2Cctr   = p_§LACctr;
229100101103             endif;
229200101103             if  V2imp  = *zero;
229300101103               V2imp    = p_§LACimp;
229400101103             endif;
229500101103             if  V2fimp = *blank;
229600101103               V2fimp   = p_§LACfimp;
229700101103             endif;
229800110713             if  V2pgm  = *blank;
229900110713               V2pgm    = p_§LACpgm;
230000110713             endif;
230100110713             if  V2ele  = *blank;
230200110713               V2ele    = p_§LACele;
230300110713             endif;
230400101103             if  V2note = *blank;
230500101103               V2note   = p_§LACnote;
230600101103             endif;
230700140813             if  V2tpadd = *blank;
230800140813               V2tpadd  = p_§LACtpadd;
230900140813             endif;
231000140813             if  V2compce = *blank;
231100140813               V2compce = p_§LACcompce;
231200140813             endif;
231300101103         endsl;
231400101103
231500101103       ENDSR;
231600121212
231700121212       //--------------------------------------------------------------
231800121217       //?Gestione estensione tabella --> LA2.                       ?
231900121212       //--------------------------------------------------------------
232000121217       BEGSR  GesLA2;
232100121212
232200121217         TNTB464ds = TNTB46ds;
232300121217         B464ke1 = %editc(V2Cccm:'X');
232400121217         clear B464fxx;
232500121217         clear B464err;
232600121217         clear B46msg;
232700121217         tntb464r (kpjba:TNTB464DS);
232800121212
232900121212       ENDSR;
233000071206
233100071212       //--------------------------------------------------------------
233200090306       //?Aggiornamento record TNTBE00F (tab. LAC)                     ?
233300071212       //--------------------------------------------------------------
233400090306       BEGSR  Upd_TNTBE;
233500071207
233600090306         clear  dLAC;
233700071207         §LACrag   = V2Dccm;
233800071207         §LACtpt   = V2Ctpt;
233900071207         §LACres   = V2Cres;
234000071207         §LACrec   = V2Crec;
234100071207         §LACcsr   = V2Ccsr;
234200071207         §LACssr   = V2Cssr;
234300071207         §LACdir   = V2Cdir;
234400071207         §LACtpi   = V2Ctpi;
234500071207         §LACfmi   = V2Cfmi;
234600121016         §LACmerge = V2merge;
234700121217         §LACla2   = V2Cla2;
234800071207         §LACaut   = V2Caut;
234900090309         §LACtdta  = V2tdta;
235000071207         §LACtad   = V2Ctad;
235100071207         §LACschgg = V2Cschgg;
235200071207         §LACsch01 = V2Csch01;
235300071207         §LACsch02 = V2Csch02;
235400071207         §LACsch03 = V2Csch03;
235500071207         §LACsch04 = V2Csch04;
235600071207         §LACsch05 = V2Csch05;
235700071207         §LACsch06 = V2Csch06;
235800071207         §LACsch07 = V2Csch07;
235900071207         §LACsch08 = V2Csch08;
236000071207         §LACsch09 = V2Csch09;
236100071207         §LACsch10 = V2Csch10;
236200071207         §LACsch11 = V2Csch11;
236300071207         §LACsch12 = V2Csch12;
236400090226         §LACnote  = V2note;
236500090309         §LAClnp   = V2Clnp;
236600090309         §LACksu   = V2Cksu;
236700090309         §LACmesi  = V2mesi;
236800130923         IF  V2caut = 'A';
236900130923           clear §LACdatai;
237000130923           clear §LACdatela;
237100130923         ELSE;
237200090313         §LACdatai = (%int( %subst( %editc( V2datai : 'X') : 3 : 4 ) )
237300090313                      * 100)
237400090313                   + %int( %subst( %editc( V2datai : 'X') : 1 : 2 ) );
237500090310         §LACdatela...
237600090313                   = (%int( %subst( %editc( V2datela : 'X') : 3 : 4 ) )
237700090319                      * 100)
237800090313                    + %int( %subst( %editc( V2datela : 'X') : 1 : 2 ) );
237900130923         ENDIF;
238000090309         §LACtadu  = V2tadu;
238100090309         §LACksc   = V2Cksc;
238200090309         §LACctr   = V2Cctr;
238300090309         §LACfimp  = V2fimp;
238400090309         §LACimp   = V2imp;
238500110713         §LACpgm   = V2pgm;
238600110713         §LACele   = V2ele;
238700140813         §LACtpadd = V2tpadd;
238800140813         §LACcompce= V2compce;
238900140813         §LACutente= knmus;
239000140813         IF  *in02;
239100140813           §LACdtaimm= %editc(woggi:'X');
239200140813         ENDIF;
239300141203         IF V2dtautdg <> *zeros;
239400141203           exec sql
239500141203           set :§LACdtaudg = (select DC_YYMD_C from DATECNV0F
239600141203                              where DC_DMYY_Z = :V2dtautdg);
239700141203         ENDIF;
239800071207
239900090309         TBEuni    = dLAC;
240000071207
240100090306         clear  TBEftt;
240200090306         clear  TBEftr;
240300071207
240400071207         select;
240500090306           // F5=Ripristino
240600090306           when  dsp_aid = c_F05;
240700090306             clear  TBEatb;
240800090306           // F16=Annullamento
240900090306           when  dsp_aid = c_F16;
241000071207             TBEatb = 'A';
241100071207         endsl;
241200121220
241300121220         TBEdtr = woggi;
241400071207
241500071207         IF  NOT  %found(TNTBE01L);
241600090306           clear  TBEatb;
241700090306           clear  TBEflt;
241800090306           clear  TBEftr;
241900090306           TBEsif = TBXsif;
242000090306           TBElin = TBXlin;
242100090306           TBEapl = TBXapl;
242200090306           TBEcod = k_TBEcod;
242300090306           TBEke1 = k_TBEke1;
242400090306           TBEke2 = k_TBEke2;
242500090306           //_____________
242600071207           WRITE TNTBE000;
242700090306           //¯¯¯¯¯¯¯¯¯¯¯¯¯
242800071207         ELSE;
242900090306           //______________
243000071207           UPDATE TNTBE000;
243100090306           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
243200071207         ENDIF;
243300121213
243400121213
243500121213         // -?Porto a pari anche la tabella LA2
243600121213         SELECT;
243700121213           //?F5=Ripristino tabella
243800121213           WHEN  dsp_aid = c_F05;
243900121213             //?Ripristino anche la LA2 se esiste
244000121213             k_TBEcod = 'LA2';
244100121213             k_TBEke1 = %editc(V2Cccm:'X');
244200121213             clear k_TBEke2;
244300121213             k_TBElin = TBXlin;
244400121213             k_TBEsif = TBXsif;
244500121213             chain  %kds(K05tntbe01) TNTBE01L;
244600121213             IF  %found(TNTBE01L);
244700121213               clear TBEatb;
244800121213               UPDATE  TNTBE000;
244900121213             ENDIF;
245000121213           WHEN  dsp_aid = c_F16;
245100121213             //?Annullo anche la LA2 se esiste
245200121213             k_TBEcod = 'LA2';
245300121213             k_TBEke1 = %editc(V2Cccm:'X');
245400121213             clear k_TBEke2;
245500121213             k_TBElin = TBXlin;
245600121213             k_TBEsif = TBXsif;
245700121213             chain  %kds(K05tntbe01) TNTBE01L;
245800121213             IF  %found(TNTBE01L);
245900121213               TBEatb = 'A';
246000121213               UPDATE  TNTBE000;
246100121213             ENDIF;
246200121213           OTHER;
246300121213           //?Modifica
246400121217           //?se viene tolto il flag si altra direcotry controllo se esiste rcd LA2
246500121213           //?se esiste cancello
246600121217             IF  §LACla2 = *blanks;
246700121213               k_TBEcod = 'LA2';
246800121213               k_TBEke1 = %editc(V2Cccm:'X');
246900121213               clear k_TBEke2;
247000121213               k_TBElin = TBXlin;
247100121213               k_TBEsif = TBXsif;
247200121213               chain  %kds(K05tntbe01) TNTBE01L;
247300121213               IF  %found(TNTBE01L);
247400121213                DELETE  TNTBE000;
247500121213               ENDIF;
247600121213             ENDIF;
247700121213         ENDSL;
247800071207
247900071207       ENDSR;
248000071207
248100071212       //--------------------------------------------------------------
248200090306       //?Operazioni finali.                                           ?
248300071212       //--------------------------------------------------------------
248400090306       BEGSR  sr_RoutEnd;
248500071207
248600090306         clear  TIBS69ds;
248700090304         tibs69ds.I69tla = 'C';
248800090304         tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
248900071207
249000071217         kpjbu = TNTB46ds;
249100071207         return;
249200071207
249300071207       ENDSR;
249400090306
249500071207      /end-free
249600071207
249700071212       //--------------------------------------------------------------
249800090306       //?Schiere a tempo di compilazione.                             ?
249900071212       //--------------------------------------------------------------
250000071207
250100071207** - $MSG -------------------------------------------------------------------*
250200071207Immettere il codice cliente                                                     1
250300071207Immettere SOLO caratteri numerici                                               2
250400071207Codice errato                                                                   3
250500071207Cliente annullato                                                               4
250600071207Immettere la directory                                                          5
250700071207Carattere "\" non valido; per indicare una sottocartella utilizzare "/"         6
250800071210Caratteri & e %  non ammessi per la directory di clienti con invio automatico   7
250900071207Indicare il giorno del mese se specificato almeno un mese                       8
251000071207Specificare almeno un mese se indicato il giorno del mese                       9
251100071207Per i mesi con n° di giorni < &g verrà assunto il loro ultimo giorno -> Enter  10
251200090225Inserire il codice padre                                                       11
251300090225Il codice padre deve essere = al codice cliente o = a cliente già in tabella   12
251400120112Cliente da copiare NON più reperibile in tab. "LAC"                            13
251500120112Cliente già inserito in tab. "LAC"                                             14
251600141203Data errata                                                                    15
