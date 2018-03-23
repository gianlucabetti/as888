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
060000150122           exec sql
060100150122           set :V2dtaimm = (select DC_DMYY_A1 from DATECNV0F
060200150122                            where DC_YYMD_Z = :woggi);
060300141117           leavesr;
060400141117         ENDIF;
060500071207
060600090306         IF  %found(TNTBE01L);
060700071207
060800090306           // Impostazione dati di record trovato
060900071217           select;
061000141117             when  B46opz = '2';
061100141117               V1Tlav = 'VARIAZIONE';
061200090306             when  B46opz = '5';
061300071217               V1Tlav = 'VISUALIZZA';
061400120112             when  $Copia;
061500120112               V1Tlav = '  COPIA   ';
061600071217             other;
061700071217               V1Tlav = 'ANNULLATO ';
061800071217           endsl;
061900090225
062000090306           // Codice padre
062100090306           V2Cksu = §LACksu;
062200090309           clear  TIBS69ds;
062300090309           clear  ds_CNACO;
062400090309           clear  ds_CNIND;
062500090309           clear  ds_CNCLP;
062600090309           clear  ds_FNCLS;
062700090310           select;
062800090310             when  §LACksu =  V2Cccm;
062900090310               V2Dksu = V2Dccm;
063000090310             when  §LACksu <> *zero;
063100090310               tibs69ds.I69kcc = DUTkci;
063200090310               tibs69ds.I69kac = %int(V2Cksu);
063300090310               tibs69ds.I69sif = knsif;
063400090310               tibs69r(TIBS69ds:ds_CNACO:ds_CNIND:ds_CNCLP:ds_FNCLS);
063500090310               if tibs69ds.O69err = *on;
063600090310                 V1Dmsg = $Msg(03);
063700090310                 errMessage  = *on;
063800090310                 errGenerico = *on;
063900090310                 PosCurKsu   = *on;
064000090310                 leavesr;
064100090310               endif;
064200090310               V2Dksu = ds_CNACO.ACOrag;
064300090310           endsl;
064400090225
064500090225           V2Caut   = §LACaut;
064600090309           V2Ctpi   = §LACtpi;
064700090225
064800090306           // Flag nome immagine
064900090306           V2Cfmi = §LACfmi;
065000090310           reset  TIBS02ds;
065100090303           T02cod = 'NIM';
065200090303           T02ke1 = §LACfmi;
065300090304           TNTBE_RicercaControllo(kpjba : tibs02ds);
065400090306           if  T02err = *blank;
065500090303             V2Dfmi = T02uni;
065600090303           endif;
065700121016
065800121016           V2merge = §LACmerge;
065900121217           V2Cla2  = §LACla2;
066000090225
066100090309           V2Cdir   = §LACdir;
066200090309           V2Ctpt   = §LACtpt;
066300090309           V2Cssr   = §LACssr;
066400090309           V2Clnp   = §LAClnp;
066500090309           V2Cres   = §LACres;
066600090309           V2Crec   = §LACrec;
066700090309           V2Ccsr   = §LACcsr;
066800090309
066900090309           // Prima estrazione
067000090309           clear  wDataI;
067100090309           if  §LACdatai > *zero;
067200090309             %subst(wDataI:1:2) = %subst(%editc(§LACdatai:'X'):5:2);
067300090309             %subst(wDataI:3:4) = %subst(%editc(§LACdatai:'X'):1:4);
067400090310             V2datai = %int(wDataI);
067500090309           endif;
067600090309
067700090309           // Ultima estrazione
067800090309           clear  wDatEla;
067900090309           if  §LACdatela > *zero;
068000090309             %subst(wDatEla:1:2) = %subst(%editc(§LACdatela:'X'):5:2);
068100090309             %subst(wDatEla:3:4) = %subst(%editc(§LACdatela:'X'):1:4);
068200090310             V2datela = %int(wDatEla);
068300090309           endif;
068400090309
068500090309           V2tdta   = §LACtdta;
068600090309           V2mesi   = §LACmesi;
068700071207           V2Cschgg = §LACschgg;
068800071207           V2Csch01 = §LACsch01;
068900071207           V2Csch02 = §LACsch02;
069000071207           V2Csch03 = §LACsch03;
069100071207           V2Csch04 = §LACsch04;
069200071207           V2Csch05 = §LACsch05;
069300071207           V2Csch06 = §LACsch06;
069400071207           V2Csch07 = §LACsch07;
069500071207           V2Csch08 = §LACsch08;
069600071207           V2Csch09 = §LACsch09;
069700071207           V2Csch10 = §LACsch10;
069800071207           V2Csch11 = §LACsch11;
069900071207           V2Csch12 = §LACsch12;
070000090225
070100090306           V2tadu   = §LACtadu;
070200090309           V2Ctad   = §LACtad;
070300090306           V2cksc   = §LACksc;
070400090306           V2cctr   = §LACctr;
070500090306           V2imp    = §LACimp;
070600090306           V2fimp   = §LACfimp;
070700110713           V2pgm    = §LACpgm;
070800110713           V2ele    = §LACele;
070900090306           V2note   = §LACnote;
071000140813
071100140813           IF  §LACdtaimm <> *blanks;
071200140813             V2dtaimm = %subst(§LACdtaimm:7:2) + '/' +
071300140813                        %subst(§LACdtaimm:5:2) + '/' +
071400140813                        %subst(§LACdtaimm:1:4);
071500140813           ENDIF;
071600150122
071700150122           IF  $Copia and V2dtaimm = *blanks;
071800150122             exec sql
071900150122             set :V2dtaimm = (select DC_DMYY_A1 from DATECNV0F
072000150122                              where DC_YYMD_Z = :woggi);
072100150122           ENDIF;
072200141128
072300140813           V2utente = §LACutente;
072400141128
072500140813           V2compce = §LACcompce;
072600141128           reset  TIBS02ds;
072700141128           T02cod = 'CED';
072800141128           T02ke1 = V2compce;
072900141128           T02sif = KNSIF;
073000141128           TNTBE_RicercaControllo(kpjba : tibs02ds);
073100141128           IF  T02err = *blank;
073200141128             V2dcompce = T02uni;
073300141128           ENDIF;
073400141202           IF  V2compce = *blanks;
073500141202             V2dcompce = 'NON DICHIARATO';
073600141202           ENDIF;
073700141202
073800141128           IF V2Cccm <> V2Cksu;
073900141128             clear dLACp;
074000141128             reset  TIBS02ds;
074100141128             T02sif = KNSIF;
074200141128             T02cod = 'LAC';
074300141128             T02ke1 = %editc(V2Cksu:'X');
074400141128             TNTBE_RicercaControllo(kpjba : tibs02ds);
074500141128             IF  T02err = *blank;
074600141128               dLACp = T02uni;
074700141128             ENDIF;
074800141128             reset  TIBS02ds;
074900141128             T02cod = 'CED';
075000141128             T02ke1 = p_§LACcompce;
075100141128             T02sif = KNSIF;
075200141128             TNTBE_RicercaControllo(kpjba : tibs02ds);
075300141128             IF  T02err = *blank;
075400141128               V2dceksu = T02uni;
075500141128             ENDIF;
075600141128             VisCEksu = *on;
075700141128           ELSE;
075800141128             VisCEksu = *off;
075900141128           ENDIF;
076000141128
076100140813           V2tpadd  = §LACtpadd;
076200140813           reset  TIBS02ds;
076300141128           clear dTAL;
076400140813           T02cod = 'TAL';
076500140813           T02ke1 = V2tpadd;
076600140813           T02sif = KNSIF;
076700140813           TNTBE_RicercaControllo(kpjba : tibs02ds);
076800140813           IF  T02err = *blank;
076900141128             dTAL = T02uni;
077000141128           ENDIF;
077100141128           V2dtpadd = §TALdes;
077200141202           IF  V2tpadd = *blanks;
077300141202             V2dtpadd = 'NON DICHIARATO';
077400141202           ENDIF;
077500090923
077600090923           *in02 = *off;
077700121217
077800121217           // Cerco i dati della tabella LA2
077900121217           clear dLA2;
078000121217           reset  TIBS02ds;
078100121217           IF  V2Cla2 = 'S';
078200121217             T02cod = 'LA2';
078300121217             T02ke1 = B464ke1;
078400121217             TNTBE_RicercaControllo(kpjba : tibs02ds);
078500121217             IF  T02err = *blank;
078600121217               dLA2 = T02uni;
078700121217             ENDIF;
078800121217           ENDIF;
078900071207
079000071207         ENDIF;
079100090923
079200090923         // Salvo i dati sensibili per la creazione addebito
079300090923         savtad = v2ctad;
079400090923         savtadu = v2tadu;
079500090923         savksc = v2cksc;
079600090923         savctr = v2cctr;
079700090923         savimp = v2imp;
079800090923         savfimp = v2fimp;
079900090923         $forza = *off;
080000130201
080100130201         // Imposto (se c'è) data ultima variazione
080200130201         clear V2datauv;
080300130201         IF  TBEdtr > 0;
080400130201           V2datauv = %subst(%editc(TBEdtr:'X'):7:2) + '/' +
080500130201                      %subst(%editc(TBEdtr:'X'):5:2) + '/' +
080600130201                      %subst(%editc(TBEdtr:'X'):1:4);
080700130201         ENDIF;
080800141128
080900141128         // Cerco la personalizzazione C/E del KSC
081000141203         exec sql
081100141203         select EVDitr into :wceksc
081200141203         FROM ecevd00f
081300141203         WHERE EVDcli = :V2cccm and
081400141203               EVDvoc = 199 and
081500141203               EVDttv = 'K' and
081600141203               EVDddt <= :woggi and
081700141203               EVDdst >= :woggi;
081800141203         IF  sqlcod = 100 or sqlcod < 0;
081900141203           V2ceksc = 'STD';
082000141203         ELSE;
082100141203           V2ceksc = %editw(wceksc:'0 ,   ');
082200141203         ENDIF;
082300141128
082400141128         // Cerco la personalizzazione C/E del KSU
082500141203         IF  VisCeKsu;
082600141203           exec sql
082700141203           select EVDitr into :wceksu
082800141203           FROM ecevd00f
082900141203           WHERE EVDcli = :V2Cksu and
083000141203                 EVDvoc = 199 and
083100141203                 EVDttv = 'K' and
083200141203                 EVDddt <= :woggi and
083300141203                 EVDdst >= :woggi;
083400141203           IF  sqlcod = 100 or sqlcod < 0;
083500141203             V2ceksu = 'STD';
083600141203           ELSE;
083700141203             V2ceksu = %editw(wceksu:'0 ,   ');
083800141203           ENDIF;
083900141203         ENDIF;
084000141203
084100141203         // Data AUT DG per c/e  a 0 o NO ddt
084200141203         IF  §LACdtaudg <> *blanks;
084300141203           exec sql
084400141203           set :V2dtautdg =
084500141203               (select DC_DMYY_Z from DATECNV0F
084600141203                where DC_YYMD_C = :§LACdtaudg);
084700141203         ENDIF;
084800071207
084900071207       ENDSR;
085000071217
085100071217       //--------------------------------------------------------------
085200090306       //?Settaggio indicatori nella videata D02 per abilitazione      ?
085300090306       //?  tasti funzionali                                           ?
085400071217       //--------------------------------------------------------------
085500090306       BEGSR  Set_Ind_D02;
085600090319
085700141204         F4Attivo  = B46opz <> '5';
085800090306         F5Attivo  = ((%found(TNTBE01L) and TBEatb <> *blank)
085900071217                                        and B46opz <> '5');
086000071217         F6Attivo  = (NOT F5Attivo and B46opz <> '5');
086100141205         F8Attivo  = B46opz = '5' and §LACla2 = 'S';
086200090306         F16Attivo = ((%found(TNTBE01L) and TBEatb = *blank)
086300071217                                        and B46opz <> '5');
086400090306
086500120112         // - Se ultima estrazione impostata proteggo la prima estrazione
086600090309         ProtDATAI = (§LACdatela <> *zero);
086700071217
086800071217       ENDSR;
086900141117
087000141117       //--------------------------------------------------------------
087100141117       //?Gestione tasto funzionale F3 da videata D02                  ?
087200141117       //?F3=Fine                                                      ?
087300141117       //--------------------------------------------------------------
087400141117       BEGSR  sr_F03D02;
087500141117
087600141117         // Chiusura del programma
087700141117         $Fine = *on;
087800141117
087900141117         // Restituzione indicazione del tasto funzionale premuto
088000141117         B46fxx = '1';
088100141117
088200141117       ENDSR;
088300090319
088400090319       //--------------------------------------------------------------
088500090319       //?Gestione tasto funzionale F04 da videata D02                 ?
088600090319       //?F04=Interrogazione Padre                                     ?
088700090319       //--------------------------------------------------------------
088800090319       BEGSR  sr_F04D02;
088900090319
089000090319         // Richiamo pgm interrogazione
089100090327         clear  Param01;
089200090327         P01ord = '1';
089300090327         P01ksu = V2Cksu;
089400090327         KPJBU  = Param01;
089500141204         tntb461r (KPJBA : P01opz3);
089600141204         Param01 = kpjbu;
089700141204         IF  V2Cksu = *zeros and P01ke1 <> *zeros and
089800141204             P01ke1 <> *blanks;
089900141204           V2Cksu = %dec(P01ke1:7:0);
090000141204         ENDIF;
090100090319
090200090319       ENDSR;
090300090319
090400141205       //--------------------------------------------------------------
090500141205       //?Gestione tasto funzionale F08 da videata D02                 ?
090600141205       //?F08=Vis. Tabella LA2
090700141205       //--------------------------------------------------------------
090800141205       BEGSR  sr_F08D02;
090900141205
091000141205         // Richiamo pgm tab. LA2
091100141205         exsr GESLA2;
091200141205
091300141205         // Devo reimpostare i dati per visualizzare nuovamente la tab.LAC
091400141205         $Video = 'D2';
091500141205         $InzD02 = *on;
091600141205         chain  %kds(K05tntbe01) TNTBE000;
091700141205
091800141205         // Se torna errore da tabella LA2 lo emetto
091900141205         IF  B464err <> '0' and B464err <> *blanks;
092000141205           ErrMessage  = *on;
092100141205           ErrGenerico = *on;
092200141205           V1Dmsg = B464msg;
092300141205           leavesr;
092400141205         ENDIF;
092500141205
092600141205       ENDSR;
092700141205
092800071217       //--------------------------------------------------------------
092900090306       //?Gestione tasto funzionale F12 da videata D02                 ?
093000090306       //?F12=Ritorno                                                  ?
093100071217       //--------------------------------------------------------------
093200090306       BEGSR  sr_F12D02;
093300071217
093400071217           B46fxx = '2';
093500071217           $Fine = *on;
093600071217
093700071217       ENDSR;
093800090306
093900090306       //--------------------------------------------------------------
094000090306       //?Controllo "annullabilità" del record (padre senza figli)     ?
094100090306       //--------------------------------------------------------------
094200090306       BEGSR  sr_CtrAnnull;
094300090306
094400090306         IndDspF = *zero;
094500090306         // REimpostazione indicatori per abilitazione tasti funzionali
094600090306         exsr  Set_Ind_D02;
094700090306
094800090306         if  V2Cksu <= *zero;
094900090306           V2Cksu = §LACksu;
095000090306         endif;
095100090306
095200090306         // Se figlio (non padre): fine controlli
095300090306         if  V2Cccm <> V2Cksu;
095400090306           leavesr;
095500090306         endif;
095600090306
095700090306         //?Verifica se padre con figli?
095800090327         clear  dLACp;
095900090316         SAVke1 = TBEke1;
096000090306         setll     %kds(k05tntbe01 : 1)  TNTBE000;
096100090306         reade(n)  %kds(k05tntbe01 : 1)  TNTBE000;
096200090306
096300090306         dow  not  %eof(TNTBE01L);
096400090316           if  TBEatb = *blank   and
096500090316               %editc(V2Cccm : 'X') <> %trimr(TBEke1);
096600090306             dLACp = TBEuni;
096700090316             if  p_§LACksu = V2Cccm;
096800090306               errMessage  = *on;
096900090306               errGenerico = *on;
097000090306               PosCurKsu   = *on;
097100090306               V1Dmsg = 'Record NON cancellabile: trattasi di PADRE +
097200090306                         con figli (vedi ' + %trimr(TBEke1) + ')';
097300090306               leavesr;
097400090306             endif;
097500090306           endif;
097600090306         reade(n)  %kds(k05tntbe01 : 1)  TNTBE000;
097700090306         enddo;
097800090316
097900120112         // - Riaggancio del rec. in manutenzione/copia
098000090316         TBEke1 = SAVke1;
098100090316         reset  TNTBE000;
098200090316         k_TBEcod = c_Tab;
098300120112         k_TBEke1 = %editc( V2Cccm : 'X' );
098400090316         clear  k_TBEke2;
098500090316         k_TBElin = TBXlin;
098600090316         k_TBEsif = TBXsif;
098700090316         chain  %kds(K05tntbe01) TNTBE000;
098800090306
098900090306       ENDSR;
099000071206
099100071212       //--------------------------------------------------------------
099200090306       //?Controllo videata D02                                        ?
099300071212       //--------------------------------------------------------------
099400090306       BEGSR  sr_CtrD02;
099500071207
099600090309         %subst(IndDspF : 50) = *off;
099700120112
099800141117         IF  $Copia or *in02;
099900120112
100000120112           Select;
100100120112
100200120112             // -?Codice estrazione (SE copia)?
100300120112             When  V2Cccm = *zero;
100400120112               errMessage  = *on;
100500120112               errGenerico = *on;
100600120112               PosCurCcm2  = *on;
100700120112               V1Dmsg = $Msg(01);
100800120112               leavesr;
100900120112
101000120112             // -?Controllo / Decodifica?
101100120112             When  tibs69ds.I69kac <> V2Cccm  or
101200120112                   k_TBEke1 <> %editc( V2Cccm : 'X' );
101300120112               clear  TIBS69ds;
101400120112               tibs69ds.I69kcc = DUTkci;
101500120112               tibs69ds.I69kac = V2Cccm;
101600120112               tibs69ds.I69sif = knsif;
101700120112               tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
101800120112               if  tibs69ds.O69err = *on;
101900120112                 errMessage  = *on;
102000120112                 errGenerico = *on;
102100120112                 PosCurCcm2  = *on;
102200120112                 V1Dmsg = $Msg(03);
102300120112                 leavesr;
102400120112               endif;
102500120112               V2Dccm = ds_CNACO.ACOrag;
102600120112               // -?Verifica esistenza in tabella?
102700120112               k_TBEke1 = %editc( V2Cccm : 'X' );
102800120112               chain  %kds(K05tntbe01) TNTBE000;
102900120112               select;
103000120112                 // - -?NON ammesso nuovo record per cliente annullato?
103100120112                 when  NOT  %found(TNTBE01L)  and  ds_CNACO.ACOflg <> *blank;
103200120112                   errMessage  = *on;
103300120112                   errGenerico = *on;
103400120112                   PosCurCcm2  = *on;
103500120112                   V1Dmsg = $Msg(04);
103600120112                   leavesr;
103700120112                 // - -?Impossibile inserimento di record già esistente?
103800120112                 when  %found(TNTBE01L);
103900120112                   errMessage  = *on;
104000120112                   errGenerico = *on;
104100120112                   PosCurCcm2  = *on;
104200120112                   V1Dmsg = $Msg(14);
104300120112                   leavesr;
104400120112               endsl;
104500120112
104600120112           EndSl;
104700141202           //?imposto già la directory
104800141202           IF  V2Cdir = *blanks;
104900141202             V2Cdir = %editc(V2Cccm:'X') + '/PI';
105000141202           ENDIF;
105100120112
105200120112         ENDIF;
105300090313
105400090313         // Verifica se variata la schedulazione dell'estrazione
105500090313         if  $ByPass;
105600090313           $ByPass = (ds_SchMm = wSavSch);
105700090313         endif;
105800090313         wSavSch = ds_SchMm;
105900090225
106000090306         //?Codice padre?
106100090225         select;
106200090306           when  V2Cksu  = *zero;
106300090306             errMessage  = *on;
106400090225             errGenerico = *on;
106500090303             PosCurKsu   = *on;
106600090225             V1Dmsg = $Msg(11);
106700090225             leavesr;
106800090306           when  V2Cksu = V2Cccm;
106900090303             V2Dksu = V2Dccm;
107000090225           other;
107100090327             // - Controllo esistenza codice padre come cliente
107200090309             //   in tabella LAC
107300090303             exsr  sr_Padre;
107400090303             if  errGenerico = *on;
107500090226               leavesr;
107600090225             endif;
107700090303             V2Dksu = p_§LACrag;
107800090226         endsl;
107900101103         //?SE codice padre <> da codice cliente?
108000101103         // ?& sono in immissione?
108100141128         IF  V2Cccm <> V2Cksu   and   (*in02 or $copia);
108200101103           // ?=> chiedo se si vogliono riportare i dati del padre?
108300101103           If  W1Ccpy = *blank;
108400101103             $Video  = 'W1';
108500101103             leavesr;
108600101103           EndIf;
108700101103           // ?& il padre ha già fatto estrazioni?
108800101103           If  p_§LACdatela <> *zero;
108900101103             // ?=> riporto le date del padre, proteggo il campo e non controllo?
109000101103             clear wDataI;
109100101103             %subst(wDataI:1:2) = %subst(%editc(p_§LACdatai:'X'):5:2);
109200101103             %subst(wDataI:3:4) = %subst(%editc(p_§LACdatai:'X'):1:4);
109300101103             V2datai = %int(wDataI);
109400101103             clear wDatEla;
109500101103             %subst(wDatEla:1:2) = %subst(%editc(p_§LACdatela:'X'):5:2);
109600101103             %subst(wDatEla:3:4) = %subst(%editc(p_§LACdatela:'X'):1:4);
109700101103             V2datela = %int(wDatEla);
109800101103             ProtDATAI = *on;
109900101103           EndIf;
110000100521         ENDIF;
110100090311
110200090311         //?Tipo elaborazione?
110300090327         select;
110400090327           // - Obbligatorio
110500090327           when  V2Caut <> 'A'  and  V2Caut <> 'S'  and  V2Caut <> 'N';
110600090327             errMessage  = *on;
110700090327             errGenerico = *on;
110800090327             PosCurAut   = *on;
110900090327             V1Dmsg = 'Tipo elaborazione errato o mancante';
111000090327             leavesr;
111100090327         endsl;
111200090226
111300090306         //?Flag nome immagine?
111400090310         clear  V2Dfmi;
111500090310         select;
111600090310           // - Obbligatorietà
111700090310           when  V2Cfmi = *blank;
111800090310             errMessage  = *on;
111900090310             errGenerico = *on;
112000090310             PosCurFmi   = *on;
112100090310             V1Dmsg = 'Flag nome immagine obbligatorio';
112200090310             leavesr;
112300090310           // - Ricerca
112400090310           when  %scan('?' : V2Cfmi) > *zero;
112500090310             clear  TIBS02ds;
112600090310             T02mod = 'R';
112700090310             T02cod = 'NIM';
112800090310             T02sif = KNSIF;
112900090310             TNTBE_RicercaControllo(kpjba : tibs02ds);
113000090310             if  T02err = *blank;
113100090310               V2Cfmi = %subst(T02ke1 : 1 : %len(V2Cfmi));
113200090310               V2Dfmi = T02uni;
113300090310               errGenerico = *on;
113400090310               leavesr;
113500090310             else;
113600090310               errMessage  = *on;
113700090310               errGenerico = *on;
113800090310               PosCurFmi   = *on;
113900090310               V1Dmsg = T02msg;
114000090310               leavesr;
114100090310             endif;
114200090310           // - Controllo
114300090310           other;
114400090310             reset  TIBS02ds;
114500090310             T02cod = 'NIM';
114600090310             T02ke1 = V2Cfmi;
114700090310             TNTBE_RicercaControllo(kpjba : tibs02ds);
114800090310             if  T02err <> *blank;
114900090310               errMessage  = *on;
115000090310               errGenerico = *on;
115100090310               PosCurFmi   = *on;
115200090310               V1Dmsg = 'Flag nome immagine errato';
115300090310               leavesr;
115400090310             endif;
115500090310             V2Dfmi = T02uni;
115600090310         ENDSL;
115700140813
115800140813         //?Tipo immagine?
115900140813         if  V2Ctpi <> 'TIF'   and   V2Ctpi <> 'JPG' and
116000140813             V2Ctpi <> 'PDF';
116100140813           errMessage  = *on;
116200140813           errGenerico = *on;
116300140813           PosCurTpi   = *on;
116400140813           V1Dmsg = 'Tipo immagine errato o mancante';
116500140813           leavesr;
116600140813         endif;
116700090226
116800090306         //?Directory per immagini?
116900090226         select;
117000090306           when  V2Cdir  = *blank;
117100090226             errMessage  = *on;
117200090226             errGenerico = *on;
117300090226             PosCurDir   = *on;
117400090226             V1Dmsg = $Msg(05);
117500090226             leavesr;
117600090316           when  %subst(V2Cdir : 1 : 1) = *blank;
117700090316             errMessage  = *on;
117800090316             errGenerico = *on;
117900090316             PosCurDir   = *on;
118000090316             V1Dmsg = 'Non ammessi spazi vuoti all''inizio del nome +
118100090316                       della directory';
118200090316             leavesr;
118300121217           WHEN  V2Cdir = §LA2dir and V2Cla2 = 'S';
118400121217             errMessage  = *on;
118500121217             errGenerico = *on;
118600121217             PosCurDir   = *on;
118700121217             V1Dmsg = 'La directory deve essere diversa da tab. LA2';
118800121217             leavesr;
118900090306           when  %scan('\':V2Cdir) > *zero;
119000090226             errMessage  = *on;
119100090226             errGenerico = *on;
119200090226             PosCurDir   = *on;
119300090226             V1Dmsg = $Msg(06);
119400090226             leavesr;
119500090306           when  %scan('%':V2Cdir) > *zero   and
119600090306                           V2Caut  = 'A';
119700090226             errMessage  = *on;
119800090226             errGenerico = *on;
119900090226             PosCurDir   = *on;
120000090226             V1Dmsg = $Msg(07);
120100090226             leavesr;
120200090309           when  %scan('&':V2Cdir) > *zero  and
120300090309                           V2Caut  = 'A';
120400090309             errMessage  = *on;
120500090309             errGenerico = *on;
120600090309             PosCurDir   = *on;
120700090309             V1Dmsg = $Msg(07);
120800090309             leavesr;
120900090226         endsl;
121000090309
121100090316         wPos1 = %scan('/' : V2Cdir);
121200090316         DoW  wPos1 > *zero;
121300090316           if  %subst( V2Cdir : wPos1 : 2 ) = '//';
121400090316             errMessage  = *on;
121500090316             errGenerico = *on;
121600090316             PosCurDir   = *on;
121700090316             V1Dmsg = 'Nome directory NON valido: non ammesso il +
121800090316                       doppio "/"';
121900090316             leavesr;
122000090316           else;
122100090316             wPos1 = %scan('/' : V2Cdir : wPos1 + 1 );
122200090316           endif;
122300090316         EndDo;
122400090316
122500090309         wPos1 = %scan('&' : V2Cdir);
122600090309         IF  wPos1 > *zero;
122700090309           if  %subst( V2Cdir : wPos1 : 4 ) <> '&AAA'   and
122800090309               %subst( V2Cdir : wPos1 : 2 ) <> '&M';
122900090309             errMessage  = *on;
123000090309             errGenerico = *on;
123100090309             PosCurDir   = *on;
123200090309             V1Dmsg = 'Ammessi solo "&AAA" e "&M" (come caratteri +
123300090309                       sostituibili)';
123400090309             leavesr;
123500090309           endif;
123600090309           wPos2 = %scan( '&' : V2Cdir : wPos1 + 1 );
123700090309           If  wPos2 > *zero;
123800090309             if  %subst( V2Cdir : wPos2 : 4 ) <> '&AAA'   and
123900090309                 %subst( V2Cdir : wPos2 : 2 ) <> '&M';
124000090309               errMessage  = *on;
124100090309               errGenerico = *on;
124200090309               PosCurDir   = *on;
124300090309               V1Dmsg = 'Ammessi solo "&AAA" e "&M" (come caratteri +
124400090309                         sostituibili)';
124500090309               leavesr;
124600090309             endif;
124700090309           EndIf;
124800090309         ENDIF;
124900090309
125000090309         wPos1 = %scan('&M' : V2Cdir);
125100090309         IF  wPos1 > *zero;
125200090309           wPos2 = %scan( '&M' : V2Cdir : wPos1 + 1 );
125300090309           If  wPos2 > *zero   and
125400090309               %scan( '&M' : V2Cdir : wPos2 + 1 ) > *zero;
125500090309             errMessage  = *on;
125600090309             errGenerico = *on;
125700090309             PosCurDir   = *on;
125800090309             V1Dmsg = 'Ammessi max 2 "&M"';
125900090309             leavesr;
126000090309           EndIf;
126100090309         ENDIF;
126200090226
126300090327         // - Se codice padre <> da codice cliente
126400090306         //   la directory deve coincidere almeno fino alla 2ª subdir
126500090306         IF  V2Cccm <> V2Cksu   and   p_§LACdir <> V2Cdir;
126600090311
126700090311           // - Controllo 1ª directory
126800090311           wPos1 = %scan( '/' : %trim( V2Cdir ) );
126900090311           wPos2 = %scan( '/' : %trim( p_§LACdir ) );
127000090313           If  wPos1 <> wPos2                               OR
127100090313             ( wPos1  > *zero   and   wPos2 > *zero   and
127200090311               %subst( %trim( V2Cdir    ) : 1 : wPos1 ) <>
127300090313               %subst( %trim( p_§LACdir ) : 1 : wPos2 ) )   OR
127400090313             ( wPos1  = *zero   and   wPos2  =  *zero
127500090313                                and   V2Cdir <> p_§LACdir );
127600090311             errMessage  = *on;
127700090311             errGenerico = *on;
127800090311             PosCurDir   = *on;
127900090311             select;
128000090311               when  wPos1 =  *zero   and   wPos2 <> *zero;
128100090316                 V1Dmsg = 'Il padre ha altri rami directory (';
128200090311               when  wPos1 <> *zero   and   wPos2 =  *zero;
128300090311                 V1Dmsg = 'Il padre ha una directory unica (';
128400090311               other;
128500090311                 V1Dmsg = 'Primo ramo directory +
128600090311                           <> da quello del cod. padre (';
128700090311             EndSl;
128800090313             if  %len( %trim( p_§LACdir ) ) <=
128900090311                 %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) ) - 1;
129000090311               V1Dmsg = %trimr( V1Dmsg )
129100090311                      + %trim( p_§LACdir ) + ')';
129200090311             else;
129300090313               wPos1 = %len( V1Dmsg );
129400090313               wPos2 = %len( %trimr( V1Dmsg ) );
129500090311               V1Dmsg = %trimr( V1Dmsg )
129600090311                      + %trim( %subst( p_§LACdir : 1 :
129700090311                          %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) )
129800090311                          - 4 ) )
129900090311                      + '...)';
130000090311             endif;
130100090311             leavesr;
130200090311           EndIf;
130300090311
130400090311           // - Controllo 2ª directory
130500141201           wPos1 = %scan( '/' : %trim( V2Cdir ) : wPos1 + 1 );
130600141201           wPos2 = %scan( '/' : %trim( p_§LACdir ) : wPos2 + 1 );
130700141201           if  wPos1 = *zero;
130800141201             wPos1 = %len( %trimr( V2Cdir ) );
130900090316           else;
131000141201             wPos1 -= 1;
131100090316           endif;
131200141201           if  wPos2 = *zero;
131300141201             wPos2 = %len( %trimr( p_§LACdir ) );
131400090316           else;
131500090316             wPos2 -= 1;
131600090316           endif;
131700090316           If  wPos1 <> wPos2   OR
131800090316               %subst(V2Cdir : 1 : wPos1) <>
131900090316                 %subst(p_§LACdir : 1 : wPos2);
132000090311             errMessage  = *on;
132100090311             errGenerico = *on;
132200090311             PosCurDir   = *on;
132300090313                 V1Dmsg = 'Secondo ramo directory +
132400090311                           <> da quello del cod. padre (';
132500090313             if  %len( %trim( p_§LACdir ) ) <=
132600090311                 %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) ) - 1;
132700090311               V1Dmsg = %trimr( V1Dmsg )
132800090311                      + %trim( p_§LACdir ) + ')';
132900090311             else;
133000090311               V1Dmsg = %trimr( V1Dmsg )
133100090311                      + %trim( %subst( p_§LACdir : 1 :
133200090311                          %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) )
133300090311                          - 4 ) )
133400090311                      + '...)';
133500090311             endif;
133600090311             leavesr;
133700090311           EndIf;
133800090311
133900090306         ENDIF;
134000121016
134100121016         //?Immagine con Merge (LDV + DOC)
134200121016         IF  V2merge = 'S';
134300121016           exsr CTR_merge;
134400121016         ENDIF;
134500121016         IF  ErrGenerico;
134600121016           leavesr;
134700121016         ENDIF;
134800090327
134900090327         //?Tipo porto?
135000090327         if  V2Ctpt <> 'F'   and   V2Ctpt <> 'A'   and   V2Ctpt <> 'E';
135100090327           errMessage  = *on;
135200090327           errGenerico = *on;
135300090327           PosCurTpt   = *on;
135400090327           V1Dmsg = 'Tipo porto errato o mancante';
135500090327           leavesr;
135600090327         endif;
135700090311
135800090311         //?LNP per selezione?
135900090311         if  V2Clnp <> *zero;
136000090311           setll  (V2Clnp)  AZORG;
136100090311           if  NOT  %equal(AZORG01L);
136200090311             errMessage  = *on;
136300090311             errGenerico = *on;
136400090311             PosCurLnp   = *on;
136500090311             V1Dmsg = 'Linea di partenza errata';
136600090311             leavesr;
136700090311           endif;
136800090311         endif;
136900140813
137000140813         //?Tipo data da elaborare?
137100140813         select;
137200140813           when  V2tdta <> ' '  and  V2tdta <> 'C'  and  V2tdta <> 'S';
137300140813             errMessage  = *on;
137400140813             errGenerico = *on;
137500140813             PosCurTdta  = *on;
137600140813             V1Dmsg = '"Flag "Data da elaborare" errato';
137700140813             leavesr;
137800140813           // - Tipo data da elaborare obbligatorio
137900140813           when  V2Caut = 'S'   and   V2tdta = *blank;
138000140813             errMessage  = *on;
138100140813             errGenerico = *on;
138200140813             PosCurTdta  = *on;
138300140813             V1Dmsg = '"Data da elaborare" obbligatoria per +
138400140813                       "Tipo Elaborazione" = "S" (schedulata)';
138500140813             leavesr;
138600140813           // - Se codice padre <> da codice cliente
138700140813           //   il tipo data da elaborare deve essere uguale
138800140813           when  V2Cccm <> V2Cksu   and   p_§LACtdta <> V2tdta;
138900140813             errMessage  = *on;
139000140813             errGenerico = *on;
139100140813             PosCurTdta  = *on;
139200140813             V1Dmsg = 'Tipo data da elaborare +
139300140813                       <> da quello del cod. padre ('
139400140813                    + p_§LACtdta + ')';
139500140813             leavesr;
139600140813         endsl;
139700090309
139800090309         //?Periodo da estrarre in mesi?
139900090310         select;
140000090310           // - Periodo da estrarre in mesi obbligatorio
140100090310           when  V2Caut = 'S'   and   V2mesi = *zero;
140200090310             errMessage  = *on;
140300090310             errGenerico = *on;
140400090310             PosCurMesi  = *on;
140500090310             V1Dmsg = '"Periodo da estrarre" obbligatorio per +
140600090310                       "Tipo Elaborazione" = "S" (schedulata)';
140700090310             leavesr;
140800090310           // - Se codice padre <> da codice cliente
140900090310           //   il periodo da estrarre deve essere uguale
141000090310           when  V2Cccm <> V2Cksu   and   p_§LACmesi <> V2mesi;
141100090310             errMessage  = *on;
141200090310             errGenerico = *on;
141300090310             PosCurMesi  = *on;
141400090310             V1Dmsg = 'Periodo da estrarre +
141500090310                       <> da quello del cod. padre ('
141600090310                    + %editc(p_§LACmesi:'X') + ')';
141700090310             leavesr;
141800090310         endsl;
141900141201
142000141201         //?Prima estrazione dal?
142100141201         select;
142200141201           // - Obbligatoria se tipo elaborazione CD
142300141201           when  V2Caut = 'S'   and   V2datai = *zero;
142400141201             errMessage  = *on;
142500141201             errGenerico = *on;
142600141201             PosCurDatai = *on;
142700141201             V1Dmsg = 'Periodo iniziale della prima estrazione +
142800141201                       obbligatorio';
142900141201             leavesr;
143000141201           // - Verifica correttezza
143100141201           when  V2datai <> *zero                                  AND
143200141201              (%subst( %editc( V2datai : 'X' ) : 1 : 2 ) < '01'   or
143300141201               %subst( %editc( V2datai : 'X' ) : 1 : 2 ) > '12'   or
143400141201               %subst( %editc( V2datai : 'X' ) : 3 : 4 ) < '2000' or
143500141201               %subst( %editc( V2datai : 'X' ) : 3 : 4 ) > %char(*year));
143600141201             errMessage  = *on;
143700141201             errGenerico = *on;
143800141201             PosCurDatai = *on;
143900141201             V1Dmsg = 'Periodo iniziale della prima estrazione +
144000141201                       errato';
144100141201             leavesr;
144200141201         endsl;
144300141201         // - Se codice padre <> da codice cliente
144400141201         //   il periodo deve essere uguale
144500141201         if  V2Cccm <> V2Cksu;
144600141201           clear  wDataI;
144700141201           %subst( wDataI : 1 : 2 ) =
144800141201                   %subst( %editc( p_§LACdatai : 'X' ) : 5 : 2 );
144900141201           %subst( wDataI : 3 : 4 ) =
145000141201                   %subst( %editc( p_§LACdatai : 'X' ) : 1 : 4 );
145100141201           if  V2datai <> %int(wDataI);
145200141201             errMessage  = *on;
145300141201             errGenerico = *on;
145400141201             PosCurDatai = *on;
145500141201             V1Dmsg = 'Periodo iniziale della prima estrazione +
145600141201                       <> da quello del cod. padre ('
145700141201                    + %subst(%editc(p_§LACdatai : 'X') : 5 : 2)
145800141201                    + '/'
145900141201                    + %subst(%editc(p_§LACdatai : 'X') : 1 : 4)
146000141201                    + ')';
146100141201             leavesr;
146200141201           endif;
146300141201         endif;
146400090226
146500090309         //?Schedulazione giorno e mesi x preparazione CD?
146600090226         select;
146700090318           // when (V2Caut  =  'A'    or  V2Caut  =  'N')    AND
146800090318           when  V2Caut  =  'A'    AND
146900130923               // (V2datai <> *zero  or  V2tdta   <> *blank   or
147000130923                (V2mesi  <> *zero  or  V2Cschgg <> *zero    or
147100090311                 ds_SchMm <> *blank);
147200090311             errMessage  = *on;
147300090311             errGenerico = *on;
147400090311             PosCurAut   = *on;
147500090311             V1Dmsg = 'Schedulazione incompatibile con il +
147600090311                       "Tipo Elaborazione" = ' + V2Caut;
147700090311             leavesr;
147800090318           when  V2Caut = 'S'  and  (V2Cschgg <= *zero);
147900090306             errMessage  = *on;
148000090306             errGenerico = *on;
148100090306             PosCurSchGg = *on;
148200090306             V1Dmsg = 'Schedulazione obbligatoria per +
148300090306                       "Tipo Elaborazione" = "S" (schedulata)';
148400090306             leavesr;
148500090306           when  V2Caut = 'S'  and  (ds_SchMm = *blank);
148600090306             errMessage  = *on;
148700090306             errGenerico = *on;
148800090306             PosCurSch01 = *on;
148900090306             V1Dmsg = 'Indicare almeno un mese per +
149000090306                       "Tipo Elaborazione" = "S" (schedulata)';
149100090306             leavesr;
149200090311         endsl;
149300090226
149400090327         // - Se codice padre <> da codice cliente
149500090306         //   i 2 codici devono avere gli stessi dati di estrazione
149600090306         //   gg e mese e n. mesi da elaborare
149700090309         if  V2Cccm <> V2Cksu;
149800090309           select;
149900090309             when  p_§LACschgg <> V2Cschgg;
150000090226               errMessage  = *on;
150100090226               errGenerico = *on;
150200090303               PosCurSchgg = *on;
150300090309               V1Dmsg = 'Giorno creazione CD +
150400090309                         <> da quello del cod. padre ('
150500090309                      + %editc(p_§LACschgg:'X') + ')';
150600090226               leavesr;
150700090303             when  p_SchMm <> ds_SchMm;
150800090226               errMessage  = *on;
150900090226               errGenerico = *on;
151000090226               PosCursch01 = *on;
151100090309               V1Dmsg = 'Mesi di elaborazione +
151200090309                         <> da quelli del cod. padre ('
151300090309                      + p_SchMm + ')';
151400090226               leavesr;
151500090226           endsl;
151600090226         endif;
151700090309
151800090327         // - Verifica congruenza tra mese di schedulazione e
151900090309         //   mesi di estrazione
152000090309         select;
152100090311           // - Nessun dato inserito
152200090310           when  V2mesi   = *zero    and   V2Cschgg = *zero    and
152300090311                 ds_SchMm = *blank;
152400090311           // - Periodo da estrarre in mesi richiesto
152500090310           when  V2mesi = *zero;
152600090310             errMessage  = *on;
152700090310             errGenerico = *on;
152800090310             PosCurMesi  = *on;
152900090310             V1Dmsg = 'Periodo da estrarre obbligatorio SE inserita +
153000090310                       una schedulazione';
153100090310             leavesr;
153200090311           // - Giorno di schedulazione richiesto
153300090310           when  V2Cschgg = *zero;
153400090310             errMessage  = *on;
153500090310             errGenerico = *on;
153600090310             PosCurSchgg = *on;
153700090311             V1Dmsg = $Msg(08);
153800090310             leavesr;
153900090311           // - Selezione mesi richiesta
154000090311           when  ds_SchMm = *blank;
154100090310             errMessage  = *on;
154200090310             errGenerico = *on;
154300090310             PosCurSch01 = *on;
154400090311             V1Dmsg = $Msg(09);
154500090310             leavesr;
154600090311         endsl;
154700090313
154800090313         // - Conteggio/Controllo mesi schedulati
154900090313         clear  wCount;
155000090311         wSchMm_ds = %triml(ds_SchMm);
155100090313         For  xx = 1  To  %elem($SchMm);
155200090313           if  $SchMm(xx) <> *blank;
155300090313             wCount += 1;
155400090313           endif;
155500090313         EndFor;
155600090313
155700090311         select;
155800090313           // - Errori bloccanti:
155900090313           when   wCount <> *zero   and
156000090313                 (V2mesi <> (12 / wCount));
156100090313             errMessage  = *on;
156200090313             errGenerico = *on;
156300090313             PosCurSch01 = *on;
156400090313             V1Dmsg = 'Incongruenza tra il "periodo da estrarre" +
156500090313                       ed i mesi schedulati';
156600090313           // - Errori bypassabili:
156700090313           when ( (V2mesi =  2   and  ($SchMm(03) = *blank     or
156800090311                                       $SchMm(05) = *blank     or
156900090311                                       $SchMm(07) = *blank     or
157000090311                                       $SchMm(09) = *blank     or
157100090311                                       $SchMm(11) = *blank))   or
157200090313                  (V2mesi =  3   and  ($SchMm(04) = *blank     or
157300090311                                       $SchMm(07) = *blank     or
157400090311                                       $SchMm(10) = *blank))   or
157500090313                  (V2mesi =  4   and  ($SchMm(05) = *blank     or
157600090311                                       $SchMm(09) = *blank))   or
157700090313                  (V2mesi =  6   and   $SchMm(07) = *blank) )  AND
157800090313                 $ByPass =  *off;
157900090313             $ByPass = *on;
158000090309             errMessage  = *on;
158100090309             errGenerico = *on;
158200090310             PosCurSch01 = *on;
158300090313             V1Dmsg = 'Mesi schedulati in maniera irregolare... +
158400090313                       Premere Enter per forzare.';
158500090309             leavesr;
158600090313           when ( (V2mesi =  2  and  ($SchMm(02) <> *blank     or
158700090311                                      $SchMm(04) <> *blank     or
158800090311                                      $SchMm(06) <> *blank     or
158900090311                                      $SchMm(08) <> *blank     or
159000090311                                      $SchMm(10) <> *blank     or
159100090311                                      $SchMm(12) <> *blank))   OR
159200090313                  (V2mesi =  3  and  ($SchMm(02) <> *blank     or
159300090311                                      $SchMm(03) <> *blank     or
159400090311                                      $SchMm(05) <> *blank     or
159500090311                                      $SchMm(06) <> *blank     or
159600090311                                      $SchMm(08) <> *blank     or
159700090311                                      $SchMm(09) <> *blank     or
159800090311                                      $SchMm(11) <> *blank     or
159900090311                                      $SchMm(12) <> *blank))   OR
160000090313                  (V2mesi =  4  and  ($SchMm(02) <> *blank     or
160100090311                                      $SchMm(03) <> *blank     or
160200090311                                      $SchMm(04) <> *blank     or
160300090311                                      $SchMm(06) <> *blank     or
160400090311                                      $SchMm(07) <> *blank     or
160500090311                                      $SchMm(08) <> *blank     or
160600090311                                      $SchMm(10) <> *blank     or
160700090311                                      $SchMm(11) <> *blank     or
160800090311                                      $SchMm(12) <> *blank))   OR
160900090313                  (V2mesi =  6  and  ($SchMm(02) <> *blank     or
161000090311                                      $SchMm(03) <> *blank     or
161100090311                                      $SchMm(04) <> *blank     or
161200090311                                      $SchMm(05) <> *blank     or
161300090311                                      $SchMm(06) <> *blank     or
161400090311                                      $SchMm(08) <> *blank     or
161500090311                                      $SchMm(09) <> *blank     or
161600090311                                      $SchMm(10) <> *blank     or
161700090311                                      $SchMm(11) <> *blank     or
161800090311                                      $SchMm(12) <> *blank))   OR
161900090313                  (V2mesi = 12  and  ($SchMm(02) <> *blank     or
162000090311                                      $SchMm(03) <> *blank     or
162100090311                                      $SchMm(04) <> *blank     or
162200090311                                      $SchMm(05) <> *blank     or
162300090311                                      $SchMm(06) <> *blank     or
162400090311                                      $SchMm(07) <> *blank     or
162500090311                                      $SchMm(08) <> *blank     or
162600090311                                      $SchMm(09) <> *blank     or
162700090311                                      $SchMm(10) <> *blank     or
162800090311                                      $SchMm(11) <> *blank     or
162900090313                                      $SchMm(12) <> *blank)) ) AND
163000090313                 $ByPass =  *off;
163100090313             $ByPass = *on;
163200090310             errMessage  = *on;
163300090310             errGenerico = *on;
163400090311             PosCurSch01 = *on;
163500090313             V1Dmsg = 'Mesi schedulati in maniera irregolare... +
163600090313                       Premere Enter per forzare.';
163700090310             leavesr;
163800090313           // - Controllo giorno del mese (bypassabile)
163900090313           when ( (V2Cschgg > 28  and  V2Csch02 <> *blank)  or
164000090313                  (V2Cschgg > 30  and (V2Csch04 <> *blank
164100090313                                   or  V2Csch06 <> *blank
164200090313                                   or  V2Csch09 <> *blank
164300090313                                   or  V2Csch11 <> *blank)) )
164400090313                and  $CtrMm = *off;
164500090313             $CtrMm = *on;
164600090313             errMessage  = *on;
164700090313             errGenerico = *on;
164800090313             PosCurSch01 = (V2Csch01 <> *blank);
164900090313             PosCurSch02 = (V2Csch02 <> *blank);
165000090313             PosCurSch04 = (V2Csch04 <> *blank);
165100090313             PosCurSch06 = (V2Csch06 <> *blank);
165200090313             PosCurSch09 = (V2Csch09 <> *blank);
165300090313             PosCurSch11 = (V2Csch11 <> *blank);
165400090313             V1Dmsg = %replace( %char(V2Cschgg) : $Msg(10) :
165500090313                                %scan('&g':$Msg(10)) );
165600090313             leavesr;
165700090313         endsl;
165800090226
165900090309         //?Frequenza addebito?
166000090310         select;
166100090327           // - Obbligatoria
166200090327           when  V2tadu <> 'I'   and   V2tadu <> 'J'   and
166300090327                 V2tadu <> 'S'   and   V2tadu <> 'M';
166400090327             errMessage  = *on;
166500090327             errGenerico = *on;
166600090327             PosCurTadu  = *on;
166700090327             V1Dmsg = 'Frequenza addebito errata o mancante';
166800090327             leavesr;
166900090327           // - Se tipo elaborazione CD la creazione addebito non può
167000090313           //   essere mensile o settimanale
167100090310           when  V2Caut = 'S'  and  (V2tadu = 'S'  or  V2tadu = 'M');
167200090310             errMessage  = *on;
167300090310             errGenerico = *on;
167400090310             PosCurTadu  = *on;
167500090310             V1Dmsg = 'Per creazione CD non è possibile addebito +
167600090310                       mensile o settimanale';
167700090310             leavesr;
167800090327           // - Se codice padre <> da codice cliente
167900090327           //   devono avere la stessa frequenza addebito
168000090310           when  V2Cccm <> V2Cksu   and   p_§LACtadu <> V2tadu;
168100090310             errMessage  = *on;
168200090310             errGenerico = *on;
168300090310             PosCurTadu  = *on;
168400090310             V1Dmsg = 'Frequenza addebito +
168500090310                        <> da quella del cod. padre ('
168600090310                    + p_§LACtadu + ')';
168700090310             leavesr;
168800090310         endsl;
168900090309
169000090309         //?Tipo addebito (Creazione TITAS)?
169100090310         select;
169200090327           // - Obbligatorio
169300090327           when  V2Ctad <> 'N'  and  V2Ctad <> 'S'  and  V2Ctad <> 'V';
169400090327             errMessage  = *on;
169500090327             errGenerico = *on;
169600090327             PosCurTad   = *on;
169700090327             V1Dmsg = 'Flag "Creazione TITAS" errato o mancante';
169800090327             leavesr;
169900090327           // - Se "N" non impostare i dati dell'addebito forzato
170000090310           when  V2Ctad = 'N'     and
170100090310                (V2Cksc <> *zero  or  V2Cctr <> *blank
170200090310                                  or  V2imp <> *zero);
170300090310             errMessage  = *on;
170400090310             errGenerico = *on;
170500090310             PosCurTad   = *on;
170600090310             V1Dmsg = 'Se NO addebito non impostare le forzature';
170700090310             leavesr;
170800090327           // - Se codice padre <> da codice cliente
170900090310           //   devono avere lo stesso tipo addebito
171000090310           when  V2Cccm <> V2Cksu   and   p_§LACtad <> V2Ctad;
171100090310             errMessage  = *on;
171200090310             errGenerico = *on;
171300090310             PosCurTad   = *on;
171400090310             V1Dmsg = 'Tipo addebito +
171500090310                       <> da quello del cod. padre ('
171600090310                        + p_§LACtad + ')';
171700090310             leavesr;
171800090310         endsl;
171900090226
172000090311         //?Dati da forzare in tassazione:?
172100090312         //?- Codice cliente / Tariffa?
172200090312         clear  TIBS69ds;
172300090312         if  V2Cksc <> *zero;
172400090312           tibs69ds.I69kcc = DUTkci;
172500090312           tibs69ds.I69kac = %int(V2Cksc);
172600090312           tibs69ds.I69sif = knsif;
172700090312           tibs69r(TIBS69ds : ds_CNACO :
172800090312                              ds_CNIND :
172900090312                              ds_CNCLP :
173000090312                              ds_FNCLS);
173100090312         endif;
173200090312         select;
173300090312           when  tibs69ds.O69err = *on;
173400090312             errMessage  = *on;
173500090312             errGenerico = *on;
173600090312             PosCurKsc   = *on;
173700090312             V1Dmsg = 'Cliente tassazione errato';
173800090312             leavesr;
173900090312           when  V2Cksc <> *zero   and   V2Cctr = *blank;
174000090312             errMessage  = *on;
174100090312             errGenerico = *on;
174200090312             PosCurCtr   = *on;
174300090312             V1Dmsg = 'Codice tariffa obbligatorio SE +
174400090312                       inserito il cliente tassazione';
174500090312             leavesr;
174600090311           // - Non ammessa tariffa senza cliente
174700090311           when  V2Cksc =  *zero   and   V2Cctr <> *blank;
174800090311             errMessage  = *on;
174900090311             errGenerico = *on;
175000090311             PosCurKsc   = *on;
175100090311             V1Dmsg = 'Cliente tassazione obbligatorio SE +
175200090311                       inserita la relativa tariffa';
175300090311             leavesr;
175400090311           // - Ammesso solo un codice tariffa numerico
175500090311           when  V2Cctr <> *blank   and
175600090311                 %check(c_Digits : V2Cctr) > *zero;
175700090311             errMessage  = *on;
175800090311             errGenerico = *on;
175900090312             PosCurCtr   = *on;
176000090312             V1Dmsg = 'Ammessi SOLO caratteri numerici';
176100090311             leavesr;
176200090310         endsl;
176300090226
176400090327         // - Se codice padre <> da codice cliente
176500090306         //   stesso ksc/ctr/importo
176600090303         if  V2Cccm <> V2Cksu;
176700090226           select;
176800090303             when  p_§LACksc <> V2Cksc;
176900090226               errMessage  = *on;
177000090226               errGenerico = *on;
177100090303               PosCurKsc   = *on;
177200090309               V1Dmsg = 'Cliente tassazione +
177300090309                         <> da quello del cod. padre ('
177400090309                      + %editc(p_§LACksc : 'X') + ')';
177500090226               leavesr;
177600090303             when  p_§LACctr <> V2Cctr;
177700090226               errMessage  = *on;
177800090226               errGenerico = *on;
177900090303               PosCurCtr   = *on;
178000090309               V1Dmsg = 'Codice tariffa +
178100090309                         <> quello del cod. padre ('
178200090309                      + p_§LACctr + ')';
178300090226               leavesr;
178400090226           endsl;
178500090226         endif;
178600090226
178700090309         //?Importo & Tipo importo forzato?
178800090310         select;
178900090310           // - Non ammesso importo per Varia Negata
179000090310           when  V2Ctad = 'V'   and   V2imp <> *zero;
179100090310             errMessage  = *on;
179200090310             errGenerico = *on;
179300090310             PosCurImp   = *on;
179400090310             V1Dmsg = 'NON ammesso l''importo forzato per +
179500090310                       Varia Negata';
179600090310             leavesr;
179700090310           // - Non ammesso tipo importo senza importo
179800090310           when  V2imp =  *zero   and   V2fimp <> *blank;
179900090310             errMessage  = *on;
180000090310             errGenerico = *on;
180100090310             PosCurImp   = *on;
180200090310             V1Dmsg = 'Importo forzato obbligatorio SE +
180300090310                       inserito il tipo di importo';
180400090310             leavesr;
180500090312           // - Se inserito importo devono essere inseriti:
180600090312           //   · il codice cliente forzato (ksc),
180700090312           //   · la tariffa forzata (ctr) ed
180800090312           //   · il tipo importo (fimp)
180900090310           when  V2imp <> *zero;
181000090310             select;
181100090310               when  V2fimp  = *blank;
181200090310                 errMessage  = *on;
181300090310                 errGenerico = *on;
181400090310                 PosCurFimp  = *on;
181500090310                 V1Dmsg = 'Immettere il tipo importo';
181600090310                 leavesr;
181700090310               when  V2Cksc  = *zero;
181800090310                 errMessage  = *on;
181900090310                 errGenerico = *on;
182000090310                 PosCurKsc   = *on;
182100090310                 V1Dmsg = 'Immettere il cliente forzato';
182200090310                 leavesr;
182300090312               when  V2Cctr  = *blank;
182400090310                 errMessage  = *on;
182500090310                 errGenerico = *on;
182600090310                 PosCurCtr   = *on;
182700090310                 V1Dmsg = 'Immettere la tariffa forzata';
182800090310                 leavesr;
182900090310             endsl;
183000090310         endsl;
183100090312
183200090327         // Verifica compatibilità dell'importo forzato con il
183300090312         //   massimo imponibile IVA in tab. "GEI"/"LAC".
183400090312         if  V2imp <> *zero   and   V2fimp = 'T';
183500090312           // - Reperimento divisa moneta di conto gestionale
183600090312           clear  dGED;
183700090312           reset  TIBS02ds;
183800090312           T02cod = 'GED';
183900090312           T02ke1 = '1';
184000090312           TNTBE_RicercaControllo(kpjba : tibs02ds);
184100090312           if  T02err = *blank;
184200090312             dGED = T02uni;
184300090312           endif;
184400090312           // - Reperimento importi da tab. "GEI"
184500090312           //   (interessa il massimo imponibile IVA per spedizione)
184600090312           clear  dGEI;
184700090312           reset  TIBS02ds;
184800090312           T02cod = 'GEI';
184900090312           T02ke1 = §GEDcn;
185000090312           TNTBE_RicercaControllo(kpjba : tibs02ds);
185100090312           if  T02err = *blank;
185200090312             dGEI = T02uni;
185300090312           endif;
185400090312           if  V2imp > §GEImf;
185500090312             errMessage  = *on;
185600090312             errGenerico = *on;
185700090312             PosCurImp   = *on;
185800090312             V1Dmsg = 'Importo superiore al MASSIMO IMPONIBILE IVA +
185900090312                       PER SPEDIZIONE ('
186000090312                      + %trim( %editc( §GEImf : 'J' ) ) + ')';
186100090312             leavesr;
186200090312           endif;
186300090312         endif;
186400090226
186500090312         // Se codice padre <> da codice cliente
186600090306         //   stesso importo e tipo importo
186700090303         if  V2Cccm <> V2Cksu;
186800090226           select;
186900090303             when  p_§LACimp <> V2imp;
187000090226               errMessage  = *on;
187100090226               errGenerico = *on;
187200090312               PosCurImp   = *on;
187300090309               V1Dmsg = 'Importo forzato +
187400090309                         <> da quello del cod. padre ('
187500090309                      + %editc(p_§LACimp : 'J') + ')';
187600090226               leavesr;
187700090303             when  p_§LACfimp <> V2fimp;
187800090226               errMessage  = *on;
187900090226               errGenerico = *on;
188000090312               PosCurFimp  = *on;
188100090309               V1Dmsg = 'Tipo importo forzato +
188200090309                         <> da quello del cod. padre ('
188300090309                      + p_§LACfimp + ')';
188400090226               leavesr;
188500090226           endsl;
188600090225         endif;
188700090923
188800090923         //?Modifica dati sensibili per l'addebito
188900090923         if (v2tadu <> savtadu or v2ctad <> savtad or
189000090923             v2cksc <> savksc or v2cctr <> savctr or
189100141128             v2imp <> savimp or v2fimp <> savfimp) and
189200141128             (not *in02 and not $Copia)
189300090923             and not $forza;
189400090923               errMessage  = *on;
189500090923               errGenerico = *on;
189600090923               PosCurTadu  = *on;
189700090923               V1Dmsg = 'ATTENZIONE!! Possibili problemi ' +
189800090923                        'in fase di addebito. Avvisare Monica/Nicola';
189900090923               $forza = *on;
190000090923               leavesr;
190100090923         endif;
190200110713
190300110713         //?Creazione file elenco per il cliente
190400110713         //?possibile solo se NON è elaborazione automatica (FTP)
190500110713         IF  V2pgm <> *blanks and V2caut = 'A';
190600110713           ErrMessage  = *on;
190700110713           ErrGenerico = *on;
190800110713           PosCurPgm   = *on;
190900110713           V1Dmsg = 'NON possibile creazione elenco se elaborazione '+
191000110713                    'automatica';
191100110713           leavesr;
191200110713         ENDIF;
191300110713         //?se inserito il nome del programma deve esserci anche il tipo elenco
191400110713         IF  V2pgm <> *blanks and V2ele = *blanks;
191500110713           ErrMessage  = *on;
191600110713           ErrGenerico = *on;
191700110713           PosCurEle   = *on;
191800110713           V1Dmsg = 'Immettere anche il tipo elenco da creare';
191900110713           leavesr;
192000110713         ENDIF;
192100110713         //?e viceversa se c'è il tipo elenco deve esserci anche il nome del pgm
192200110713         IF  V2pgm = *blanks and V2ele <> *blanks;
192300110713           ErrMessage  = *on;
192400110713           ErrGenerico = *on;
192500110713           PosCurPgm   = *on;
192600110713           V1Dmsg = 'Immettere anche il nome del programma x elenco da creare';
192700110713           leavesr;
192800110713         ENDIF;
192900110713         //?Se codice padre <> da codice cliente
193000110713         //?  devono avere gli stessi dati
193100110713         IF  V2Cccm <> V2Cksu;
193200110713           SELECT;
193300110713             WHEN  p_§LACpgm <> V2pgm;
193400110713               ErrMessage  = *on;
193500110713               ErrGenerico = *on;
193600110713               PosCurPgm   = *on;
193700110713               V1Dmsg = 'Nome programma +
193800110713                         <> da quello del cod. padre ('
193900110713                      + p_§LACpgm + ')';
194000110713               leavesr;
194100110713             WHEN  p_§LACele <> V2ele;
194200110713               ErrMessage  = *on;
194300110713               ErrGenerico = *on;
194400110713               PosCurEle   = *on;
194500110713               V1Dmsg = 'Tipo elenco +
194600110713                         <> da quello del cod. padre ('
194700110713                      + p_§LACele + ')';
194800110713               leavesr;
194900110713           ENDSL;
195000110713         ENDIF;
195100141128
195200141128         //?Competenze C/E dichiarate
195300141128         IF  (*in02 or $copia) and V2compce = *blanks;
195400141128           ErrGenerico = *on;
195500141128           ErrMessage  = *on;
195600141128           PosCurCompCE = *on;
195700141128           V1Dmsg = 'Competenze C/E obbligatorie';
195800141128           leavesr;
195900141128         ENDIF;
196000141201         IF  V2compce <> *blanks;
196100141128         clear  V2dcompce;
196200141128         IF  %scan('?' : V2compce) > *zero;
196300141128           clear  TIBS02ds;
196400141128           T02mod = 'R';
196500141128           T02cod = 'CED';
196600141128           T02sif = KNSIF;
196700141128           TNTBE_RicercaControllo(kpjba : tibs02ds);
196800141128           IF  T02err = *blank;
196900141128             V2compce  = T02ke1;
197000141128             V2dcompce = T02uni;
197100141128             errGenerico = *on;
197200141128             leavesr;
197300141128           ELSE;
197400141128             errMessage  = *on;
197500141128             errGenerico = *on;
197600141128             PosCurCompCE = *on;
197700141128             V1Dmsg = T02msg;
197800141128             leavesr;
197900141128           ENDIF;
198000141128         ELSE;
198100141128           reset  TIBS02ds;
198200141128           T02cod = 'CED';
198300141128           T02ke1 = V2compce;
198400141128           T02sif = KNSIF;
198500141128           TNTBE_RicercaControllo(kpjba : tibs02ds);
198600141128           IF  T02err = *blank;
198700141128             V2dcompce = T02uni;
198800141128           ELSE;
198900141128             ErrMessage  = *on;
199000141128             ErrGenerico = *on;
199100141128             PosCurCompCE = *on;
199200141128             V1Dmsg = 'Competenze C/E errate';
199300141128             leavesr;
199400141128           ENDIF;
199500141128         ENDIF;
199600141202         ELSE;
199700141202           V2dcompce = 'NON DICHIARATO';
199800141201         ENDIF;
199900141128         //?Se codice padre <> da codice cliente
200000141128         //?  devono avere gli stessi dati
200100141128         //?  ma solo se sono in immissione/copia
200200141201         //?  e se il padre ha la personalizzazione impostata
200300141128         IF  V2Cccm <> V2Cksu and
200400141201             p_§LACcompce <> V2compce and (*in02 or $copia)
200500141201             and p_§LACcompce <> *blanks;
200600141128           ErrMessage   = *on;
200700141128           ErrGenerico  = *on;
200800141128           PosCurCompCE = *on;
200900141128           V1Dmsg = 'Competenze C/E +
201000141128                     <> da quelle del cod. padre ('
201100141128                   + p_§LACcompce + ')';
201200141128           leavesr;
201300141128         ENDIF;
201400141128
201500141128         //?Tipo Addebito dichiarato
201600141128         clear  V2dtpadd;
201700141128         IF  %scan('?' : V2tpadd) > *zero;
201800141128           clear  TIBS02ds;
201900141128           clear dTAL;
202000141128           T02mod = 'R';
202100141128           T02cod = 'TAL';
202200141128           T02sif = KNSIF;
202300141128           TNTBE_RicercaControllo(kpjba : tibs02ds);
202400141128           IF  T02err = *blank;
202500141128             V2tpadd  = T02ke1;
202600141128             dTAL = T02uni;
202700141128             V2dtpadd = §TALdes;
202800141128             errGenerico = *on;
202900141128             leavesr;
203000141128           ELSE;
203100141128             errMessage  = *on;
203200141128             errGenerico = *on;
203300141128             PosCurTpAdd = *on;
203400141128             V1Dmsg = T02msg;
203500141128             leavesr;
203600141128           ENDIF;
203700141128         ELSE;
203800141202         IF  V2tpadd = *blanks;
203900141202           V2dtpadd = 'NON DICHIARATO';
204000141202         ELSE;
204100141128           reset  TIBS02ds;
204200141128           clear dTAL;
204300141128           T02cod = 'TAL';
204400141128           T02ke1 = V2tpadd;
204500141128           T02sif = KNSIF;
204600141128           TNTBE_RicercaControllo(kpjba : tibs02ds);
204700141128           IF  T02err = *blank;
204800141128             dTAL = T02uni;
204900141128             V2dtpadd = §TALdes;
205000141128           ELSE;
205100141128             ErrMessage  = *on;
205200141128             ErrGenerico = *on;
205300141128             PosCurTpAdd = *on;
205400141128             V1Dmsg = 'Tipo Addebito errato';
205500141128             leavesr;
205600141128           ENDIF;
205700141202         ENDIF;
205800141128         ENDIF;
205900141128         //?Se codice padre <> da codice cliente
206000141128         //?  devono avere gli stessi dati
206100141128         //?  ma solo se sono in immissione/copia
206200141201         //?  e se il padre ha il campo impostato
206300141128         IF  V2Cccm <> V2Cksu and
206400141201             p_§LACtpadd <> V2tpadd and (*in02 or $copia)
206500141201             and p_§LACtpadd <> *blanks;
206600141128           ErrMessage  = *on;
206700141128           ErrGenerico = *on;
206800141128           PosCurTpAdd = *on;
206900141128           V1Dmsg = 'Tipo addebito +
207000141128                     <> da quello del cod. padre ('
207100141128                   + p_§LACtpadd + ')';
207200141128           leavesr;
207300141128         ENDIF;
207400141203
207500141203         //?Data Aut. DG
207600141203         IF  V2dtautdg <> *zeros;
207700141203           exec sql
207800141203           select count(*) into :conta
207900141203           from DATECNV0F
208000141203           where DC_DMYY_Z = :V2dtautdg or DC_DMY_Z = :V2dtautdg;
208100141203           IF  conta = 0;
208200141203             ErrMessage  = *on;
208300141203             ErrGenerico = *on;
208400141203             PosCurAutDg = *on;
208500141203             V1Dmsg = $Msg(15);
208600141203             leavesr;
208700141203           ENDIF;
208800141203           IF  V2dtautdg < 1000000;
208900141203             exec sql
209000141203             set :V2dtautdg = (select DC_DMYY_Z from DATECNV0F
209100141203                               where DC_DMY_Z = :V2dtautdg);
209200141203           ENDIF;
209300141203         ENDIF;
209400071207
209500071207       ENDSR;
209600090225
209700090225       //--------------------------------------------------------------
209800090306       //?Controllo esistenza del codice padre in tabella LAC          ?
209900090225       //--------------------------------------------------------------
210000090303       BEGSR  sr_Padre;
210100090302
210200090327         // Controlla esistenza in tabella "LAC"
210300090310         clear  dLACp;
210400090310         reset  TIBS02ds;
210500090310         T02sif = KNSIF;
210600090310         %subst(T02ke1 : 1 : 7) = %editc(V2Cksu : 'X');
210700090310         TNTBE_RicercaControllo(kpjba : tibs02ds);
210800090310         if  T02err = *blank;
210900090310           dLACp  = T02uni;
211000090310         else;
211100090310           errMessage  = *on;
211200090310           errGenerico = *on;
211300090310           PosCurksu   = *on;
211400090310           V1Dmsg = $Msg(12);
211500090310           leavesr;
211600090310         endif;
211700090327
211800090327         // Controlla se già inserito come figlio di altro padre
211900090327         if  p_§LACksu <> V2Cksu;
212000090327           errMessage  = *on;
212100090327           errGenerico = *on;
212200090327           PosCurKsu   = *on;
212300090327           V1Dmsg = 'Non ammesso cod. PADRE già FIGLIO di altro +
212400090327                     cliente (' + %editc(p_§LACksu : 'X') + ')';
212500090327           leavesr;
212600090327         endif;
212700090225
212800090225       ENDSR;
212900121016
213000121016       //--------------------------------------------------------------
213100121016       //?Controllo Merge.
213200121016       //--------------------------------------------------------------
213300121016       BEGSR  CTR_merge;
213400121016
213500121016       // -?L'immagine se con Merge deve essere di tipo PDF
213600121016         IF  V2Ctpi <> 'PDF';
213700121016           errMessage  = *on;
213800121016           errGenerico = *on;
213900121016           V1Dmsg = 'Immagine con Merge, deve essere di tipo PDF';
214000121016           PosCurTpi = *on;
214100121016           leavesr;
214200121016         ENDIF;
214300121016
214400121016       // -?La directory deve finire con /PF
214500121016         wPos1 = %scan('  ':V2Cdir);
214600121016         wPos1 -= 3;
214700121016         IF  %subst(V2Cdir:wPos1:3) <> '/PF';
214800121016           ErrMessage  = *on;
214900121016           ErrGenerico = *on;
215000121016           PosCurDir   = *on;
215100121016           V1Dmsg = 'Immagine con Merge, l''ultimo ramo della +
215200121016                     DIR deve essere "PF"';
215300121016           leavesr;
215400121016         ENDIF;
215500121016
215600121016       // -?Cerco il corrispondente in JDC
215700121016         clear  TIBS02DS;
215800121016         T02mod = 'C';
215900121016         T02cod = 'JDC';
216000121016         T02ke1 = %editc(V2Cccm:'X');
216100121016         TNTBE_RicercaControllo(kpjba : tibs02ds);
216200121016         // -?Non trovo la tabella
216300121016         IF  T02err <> *blanks;
216400121016           // -?Immissione forzo
216500121016           IF  *in02 and not wForza;
216600121016             wForza = *on;
216700121016             ErrMessage  = *on;
216800121016             ErrGenerico = *on;
216900121016             V1Dmsg = 'Ricordarsi di attivare anche la corrispondente +
217000121016                       tabella JDC. Enter forza.';
217100121016             leavesr;
217200121016           ENDIF;
217300121016         // -?Manutenzione blocco
217400121016           IF  not *in02;
217500121016             ErrMessage  = *on;
217600121016             ErrGenerico = *on;
217700121016             V1Dmsg = 'Manca la corrispondente tabella JDC';
217800121016             leavesr;
217900121016           ENDIF;
218000121016         // -?Trovo la tabella
218100121016         ELSE;
218200121016           dJDC = T02uni;
218300121016         // -?Controllo il tipo immagine
218400121030           IF  V2Ctpi <> §JDCtpi and not wForzaTPI;
218500121030             wForzaTPI = *on;
218600121016             ErrMessage  = *on;
218700121016             ErrGenerico = *on;
218800121016             PosCurTpi   = *on;
218900121016             V1Dmsg = 'Tipo immagine diverso dalla corrispondente +
219000121016                       tabella JDC --> ' + §JDCtpi;
219100121016             leavesr;
219200121016           ENDIF;
219300121016         // -?Controllo il nome immagine
219400121030           IF  V2Cfmi <> §JDCfmi and not wForzaFMI;
219500121030             wForzaFMI = *on;
219600121016             ErrMessage  = *on;
219700121016             ErrGenerico = *on;
219800121016             PosCurFmi   = *on;
219900121016             V1Dmsg = 'Nome immagine diverso dalla corrispondente +
220000121016                       tabella JDC --> ' + §JDCfmi;
220100121016             leavesr;
220200121016           ENDIF;
220300121016         // -?Controllo la directory
220400121016           wPos2 = %scan('  ':§JDCdir);
220500121016           wPos2 -= 3;
220600121016           IF  %subst(V2Cdir:1:wPos1) <>
220700121030               %subst(§JDCdir:1:wPos2) and not wForzaDIR;
220800121030             wForzaDIR = *on;
220900121016             ErrMessage  = *on;
221000121016             ErrGenerico = *on;
221100121016             PosCurDir   = *on;
221200121016             V1Dmsg = 'Directory diversa dalla corrispondente +
221300121016                       tabella JDC --> ' + %subst(§JDCdir:1:wPos2);
221400121016             leavesr;
221500121016           ENDIF;
221600121016         ENDIF;
221700121016
221800121016       ENDSR;
221900101103
222000101103       //--------------------------------------------------------------
222100101103       //?Gestione window W01                                          ?
222200101103       //--------------------------------------------------------------
222300101103       BEGSR  sr_GesW01;
222400101103
222500101103         // -?Emissione window?
222600101103         exfmt  TB46W01;
222700101103
222800101103         errMessage  = *off;
222900101103         errGenerico = *off;
223000101103         clear  V1Dmsg;
223100101103
223200101103         $Video = 'D2';
223300101103
223400101103         select;
223500101103           // -?F12=Ritorno?
223600101103           when  dsp_aid = c_F12;
223700101103             leavesr;
223800101103           // -?Invio con "S"?
223900101103           when  W1Ccpy = 'S';
224000101103             if  V2Caut = *blank;
224100101103               V2Caut   = p_§LACaut;
224200101103             endif;
224300101103             if  V2Ctpi = *blank;
224400101103               V2Ctpi   = p_§LACtpi;
224500101103             endif;
224600101103             if  V2Cfmi = *blank;
224700101103               V2Cfmi   = p_§LACfmi;
224800101103               clear  V2Dfmi;
224900101103             endif;
225000121016             if  V2merge = *blank;
225100121016               V2merge  = p_§LACmerge;
225200121016             endif;
225300121217             if  V2Cla2 = *blank;
225400121217               V2Cla2 = p_§LACla2;
225500121217             endif;
225600101103             //if  V2Cdir = *blank;      //? Directory ?
225700101103               V2Cdir   = p_§LACdir;     //? COMUNQUE  ?
225800101103             //endif;                    //? del padre ?
225900101103             if  V2Ctpt = *blank;
226000101103               V2Ctpt   = p_§LACtpt;
226100101103             endif;
226200101103             if  V2Cssr = *blank;
226300101103               V2Cssr   = p_§LACssr;
226400101103             endif;
226500101103             if  V2Clnp = *zero;
226600101103               V2Clnp   = p_§LAClnp;
226700101103             endif;
226800101103             if  V2Cres = *blank;
226900101103               V2Cres   = p_§LACres;
227000101103             endif;
227100101103             if  V2Crec = *blank;
227200101103               V2Crec   = p_§LACrec;
227300101103             endif;
227400101103             if  V2Ccsr = *blank;
227500101103               V2Ccsr   = p_§LACcsr;
227600101103             endif;
227700101103             if  V2tdta = *blank;
227800101103               V2tdta   = p_§LACtdta;
227900101103             endif;
228000101103             if  V2mesi = *zero;
228100101103               V2mesi   = p_§LACmesi;
228200101103             endif;
228300101103             if  V2Cschgg = *zero;
228400101103               V2Cschgg = p_§LACschgg;
228500101103             endif;
228600101103             if  ds_SchMm = *blank;
228700101103               ds_SchMm = p_SchMm;
228800101103             endif;
228900101103             if  V2tadu = *blank;
229000101103               V2tadu   = p_§LACtadu;
229100101103             endif;
229200101103             if  V2Ctad = *blank;
229300101103               V2Ctad   = p_§LACtad;
229400101103             endif;
229500101103             if  V2Cksc = *zero;
229600101103               V2Cksc   = p_§LACksc;
229700101103             endif;
229800101103             if  V2Cctr = *blank;
229900101103               V2Cctr   = p_§LACctr;
230000101103             endif;
230100101103             if  V2imp  = *zero;
230200101103               V2imp    = p_§LACimp;
230300101103             endif;
230400101103             if  V2fimp = *blank;
230500101103               V2fimp   = p_§LACfimp;
230600101103             endif;
230700110713             if  V2pgm  = *blank;
230800110713               V2pgm    = p_§LACpgm;
230900110713             endif;
231000110713             if  V2ele  = *blank;
231100110713               V2ele    = p_§LACele;
231200110713             endif;
231300101103             if  V2note = *blank;
231400101103               V2note   = p_§LACnote;
231500101103             endif;
231600140813             if  V2tpadd = *blank;
231700140813               V2tpadd  = p_§LACtpadd;
231800140813             endif;
231900140813             if  V2compce = *blank;
232000140813               V2compce = p_§LACcompce;
232100140813             endif;
232200101103         endsl;
232300101103
232400101103       ENDSR;
232500121212
232600121212       //--------------------------------------------------------------
232700121217       //?Gestione estensione tabella --> LA2.                       ?
232800121212       //--------------------------------------------------------------
232900121217       BEGSR  GesLA2;
233000121212
233100121217         TNTB464ds = TNTB46ds;
233200121217         B464ke1 = %editc(V2Cccm:'X');
233300121217         clear B464fxx;
233400121217         clear B464err;
233500121217         clear B46msg;
233600121217         tntb464r (kpjba:TNTB464DS);
233700121212
233800121212       ENDSR;
233900071206
234000071212       //--------------------------------------------------------------
234100090306       //?Aggiornamento record TNTBE00F (tab. LAC)                     ?
234200071212       //--------------------------------------------------------------
234300090306       BEGSR  Upd_TNTBE;
234400071207
234500090306         clear  dLAC;
234600071207         §LACrag   = V2Dccm;
234700071207         §LACtpt   = V2Ctpt;
234800071207         §LACres   = V2Cres;
234900071207         §LACrec   = V2Crec;
235000071207         §LACcsr   = V2Ccsr;
235100071207         §LACssr   = V2Cssr;
235200071207         §LACdir   = V2Cdir;
235300071207         §LACtpi   = V2Ctpi;
235400071207         §LACfmi   = V2Cfmi;
235500121016         §LACmerge = V2merge;
235600121217         §LACla2   = V2Cla2;
235700071207         §LACaut   = V2Caut;
235800090309         §LACtdta  = V2tdta;
235900071207         §LACtad   = V2Ctad;
236000071207         §LACschgg = V2Cschgg;
236100071207         §LACsch01 = V2Csch01;
236200071207         §LACsch02 = V2Csch02;
236300071207         §LACsch03 = V2Csch03;
236400071207         §LACsch04 = V2Csch04;
236500071207         §LACsch05 = V2Csch05;
236600071207         §LACsch06 = V2Csch06;
236700071207         §LACsch07 = V2Csch07;
236800071207         §LACsch08 = V2Csch08;
236900071207         §LACsch09 = V2Csch09;
237000071207         §LACsch10 = V2Csch10;
237100071207         §LACsch11 = V2Csch11;
237200071207         §LACsch12 = V2Csch12;
237300090226         §LACnote  = V2note;
237400090309         §LAClnp   = V2Clnp;
237500090309         §LACksu   = V2Cksu;
237600090309         §LACmesi  = V2mesi;
237700130923         IF  V2caut = 'A';
237800130923           clear §LACdatai;
237900130923           clear §LACdatela;
238000130923         ELSE;
238100090313         §LACdatai = (%int( %subst( %editc( V2datai : 'X') : 3 : 4 ) )
238200090313                      * 100)
238300090313                   + %int( %subst( %editc( V2datai : 'X') : 1 : 2 ) );
238400090310         §LACdatela...
238500090313                   = (%int( %subst( %editc( V2datela : 'X') : 3 : 4 ) )
238600090319                      * 100)
238700090313                    + %int( %subst( %editc( V2datela : 'X') : 1 : 2 ) );
238800130923         ENDIF;
238900090309         §LACtadu  = V2tadu;
239000090309         §LACksc   = V2Cksc;
239100090309         §LACctr   = V2Cctr;
239200090309         §LACfimp  = V2fimp;
239300090309         §LACimp   = V2imp;
239400110713         §LACpgm   = V2pgm;
239500110713         §LACele   = V2ele;
239600140813         §LACtpadd = V2tpadd;
239700140813         §LACcompce= V2compce;
239800140813         §LACutente= knmus;
239900150121         IF  *in02 or $copia;
240000140813           §LACdtaimm= %editc(woggi:'X');
240100141210         ELSE;
240200141210           exec sql
240300141210           set :§LACdtaimm = (select DC_YYMD_C from DATECNV0F
240400141210                              where DC_DMYY_A1 = :V2dtaimm);
240500140813         ENDIF;
240600141203         IF V2dtautdg <> *zeros;
240700141203           exec sql
240800141203           set :§LACdtaudg = (select DC_YYMD_C from DATECNV0F
240900141203                              where DC_DMYY_Z = :V2dtautdg);
241000141203         ENDIF;
241100071207
241200090309         TBEuni    = dLAC;
241300071207
241400090306         clear  TBEftt;
241500090306         clear  TBEftr;
241600071207
241700071207         select;
241800090306           // F5=Ripristino
241900090306           when  dsp_aid = c_F05;
242000090306             clear  TBEatb;
242100090306           // F16=Annullamento
242200090306           when  dsp_aid = c_F16;
242300071207             TBEatb = 'A';
242400071207         endsl;
242500121220
242600121220         TBEdtr = woggi;
242700071207
242800071207         IF  NOT  %found(TNTBE01L);
242900090306           clear  TBEatb;
243000090306           clear  TBEflt;
243100090306           clear  TBEftr;
243200090306           TBEsif = TBXsif;
243300090306           TBElin = TBXlin;
243400090306           TBEapl = TBXapl;
243500090306           TBEcod = k_TBEcod;
243600090306           TBEke1 = k_TBEke1;
243700090306           TBEke2 = k_TBEke2;
243800090306           //_____________
243900071207           WRITE TNTBE000;
244000090306           //¯¯¯¯¯¯¯¯¯¯¯¯¯
244100071207         ELSE;
244200090306           //______________
244300071207           UPDATE TNTBE000;
244400090306           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
244500071207         ENDIF;
244600121213
244700121213
244800121213         // -?Porto a pari anche la tabella LA2
244900121213         SELECT;
245000121213           //?F5=Ripristino tabella
245100121213           WHEN  dsp_aid = c_F05;
245200121213             //?Ripristino anche la LA2 se esiste
245300121213             k_TBEcod = 'LA2';
245400121213             k_TBEke1 = %editc(V2Cccm:'X');
245500121213             clear k_TBEke2;
245600121213             k_TBElin = TBXlin;
245700121213             k_TBEsif = TBXsif;
245800121213             chain  %kds(K05tntbe01) TNTBE01L;
245900121213             IF  %found(TNTBE01L);
246000121213               clear TBEatb;
246100121213               UPDATE  TNTBE000;
246200121213             ENDIF;
246300121213           WHEN  dsp_aid = c_F16;
246400121213             //?Annullo anche la LA2 se esiste
246500121213             k_TBEcod = 'LA2';
246600121213             k_TBEke1 = %editc(V2Cccm:'X');
246700121213             clear k_TBEke2;
246800121213             k_TBElin = TBXlin;
246900121213             k_TBEsif = TBXsif;
247000121213             chain  %kds(K05tntbe01) TNTBE01L;
247100121213             IF  %found(TNTBE01L);
247200121213               TBEatb = 'A';
247300121213               UPDATE  TNTBE000;
247400121213             ENDIF;
247500121213           OTHER;
247600121213           //?Modifica
247700121217           //?se viene tolto il flag si altra direcotry controllo se esiste rcd LA2
247800121213           //?se esiste cancello
247900121217             IF  §LACla2 = *blanks;
248000121213               k_TBEcod = 'LA2';
248100121213               k_TBEke1 = %editc(V2Cccm:'X');
248200121213               clear k_TBEke2;
248300121213               k_TBElin = TBXlin;
248400121213               k_TBEsif = TBXsif;
248500121213               chain  %kds(K05tntbe01) TNTBE01L;
248600121213               IF  %found(TNTBE01L);
248700121213                DELETE  TNTBE000;
248800121213               ENDIF;
248900121213             ENDIF;
249000121213         ENDSL;
249100071207
249200071207       ENDSR;
249300071207
249400071212       //--------------------------------------------------------------
249500090306       //?Operazioni finali.                                           ?
249600071212       //--------------------------------------------------------------
249700090306       BEGSR  sr_RoutEnd;
249800071207
249900090306         clear  TIBS69ds;
250000090304         tibs69ds.I69tla = 'C';
250100090304         tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
250200071207
250300071217         kpjbu = TNTB46ds;
250400071207         return;
250500071207
250600071207       ENDSR;
250700090306
250800071207      /end-free
250900071207
251000071212       //--------------------------------------------------------------
251100090306       //?Schiere a tempo di compilazione.                             ?
251200071212       //--------------------------------------------------------------
251300071207
251400071207** - $MSG -------------------------------------------------------------------*
251500071207Immettere il codice cliente                                                     1
251600071207Immettere SOLO caratteri numerici                                               2
251700071207Codice errato                                                                   3
251800071207Cliente annullato                                                               4
251900071207Immettere la directory                                                          5
252000071207Carattere "\" non valido; per indicare una sottocartella utilizzare "/"         6
252100071210Caratteri & e %  non ammessi per la directory di clienti con invio automatico   7
252200071207Indicare il giorno del mese se specificato almeno un mese                       8
252300071207Specificare almeno un mese se indicato il giorno del mese                       9
252400071207Per i mesi con n° di giorni < &g verrà assunto il loro ultimo giorno -> Enter  10
252500090225Inserire il codice padre                                                       11
252600090225Il codice padre deve essere = al codice cliente o = a cliente già in tabella   12
252700120112Cliente da copiare NON più reperibile in tab. "LAC"                            13
252800120112Cliente già inserito in tab. "LAC"                                             14
252900141203Data errata                                                                    15
