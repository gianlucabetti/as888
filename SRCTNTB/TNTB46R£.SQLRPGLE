000100090306       //--------------------------------------------------------------
000200090306       //?GESTIONE TABELLA "LAC" - Laguna: clienti per ritorno immagini?
000300121213       //?                       + Tabella LA2 a richiesta           ?
000400151217       //?                       + Tabella JDC a richiesta           ?
000500090306       //--------------------------------------------------------------
000600110804
000700110804      *?  ATTENZIONE!!  ?
000800110804      *?    Questa tabella è utilizzata anche dal pgm TNTA61R  ?
000900110804      *?    'Interrogazione abilitazioni clienti'              ?
001000110804      *?    In caso di aggiunta/modifica campi alla tabella    ?
001100110804      *?    verificare anche relativo pgm di interrogazione    ?
001200110804      *?    per le abilitazioni clienti --> TNTB462R.          ?
001300090306
001400071206     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001500071206
001600090306       //--------------------------------------------------------------
001700090306       //?Dichiarazione file.                                          ?
001800090306       //--------------------------------------------------------------
001900090306
002000090311     fAZORG01L  if   e           k disk
002100090311
002200071206     fTNTBE01L  Uf A e           k disk
002300090311
002400071206     fTNTB46D   cf   e             workstn
002500071207     f                                     indds(IndDspF)
002600090306     f                                     infds(InfDspF)
002700071207
002800090306       //--------------------------------------------------------------
002900090306       //?Definizione costanti.                                        ?
003000090306       //--------------------------------------------------------------
003100090306
003200090306       // - Tabella in gestione
003300090310     d c_Tab           c                   const('LAC')
003400090306
003500090306       // - Costante per controllo "caratteri solo numerici"
003600090310     d c_Digits        c                   const('0123456789')
003700090306
003800090306       // - Tasti funzionali a video
003900090306     d c_F01           c                   const(x'31')
004000090306     d c_F02           c                   const(x'32')
004100090306     d c_F03           c                   const(x'33')
004200090319     d c_F04           c                   const(x'34')
004300090306     d c_F05           c                   const(x'35')
004400090306     d c_F06           c                   const(x'36')
004500090306     d c_F07           c                   const(x'37')
004600090306     d c_F08           c                   const(x'38')
004700090306     d c_F09           c                   const(x'39')
004800090306     d c_F10           c                   const(x'3A')
004900110713     d c_F11           c                   const(x'3B')
005000090306     d c_F12           c                   const(x'3C')
005100090306     d c_F13           c                   const(x'B1')
005200090306     d c_F14           c                   const(x'B2')
005300090306     d c_F15           c                   const(x'B3')
005400090306     d c_F16           c                   const(x'B4')
005500090306     d c_F17           c                   const(x'B5')
005600090306     d c_F18           c                   const(x'B6')
005700090306     d c_F19           c                   const(x'B7')
005800090306     d c_F20           c                   const(x'B8')
005900090306     d c_F21           c                   const(x'B9')
006000090306     d c_F22           c                   const(x'BA')
006100090306     d c_F23           c                   const(x'BB')
006200090306     d c_F24           c                   const(x'BC')
006300090306     d c_Enter         c                   const(x'F1')
006400090306     d c_RollDown      c                   const(x'F4')
006500090306     d c_RollUp        c                   const(x'F5')
006600071207
006700090306       //--------------------------------------------------------------
006800090306       //?Definizione schiere.                                         ?
006900090306       //--------------------------------------------------------------
007000090306
007100090306       // - Messaggi a video
007200141203     d $Msg            s             78    dim(20)  ctdata  perrcd(1)
007300071207
007400090306       //--------------------------------------------------------------
007500090306       //?Definizione aree dati.                                       ?
007600090306       //--------------------------------------------------------------
007700090306
007800090306       // - Dati utente
007900071207     d §AzUte        e ds                  extname(AZUTE00F)
008000071207     d                                     dtaara
008100071207     d §DatiUte      e ds                  extname(dDatiUte)
008200071207     d                                     dtaara
008300071207
008400090306       //--------------------------------------------------------------
008500090306       //?Definizione strutture dati.                                  ?
008600090306       //--------------------------------------------------------------
008700090306
008800090306       // - Status
008900071207     d Psds           sds
009000071207     d   SDSpgm          *proc
009100071207
009200090306       // - InfDS
009300071207     d InfDspF         ds
009400071207     d  dsp_aid              369    369a                                        AID byte
009500071207
009600090306       // - Indicatori su DspF
009700090311     d IndDspF         ds
009800090306         // - Abilitazione tasti funzionali
009900090323     d  F4Attivo                       n   overlay(IndDspF:04)
010000090311     d  F5Attivo                       n   overlay(IndDspF:05)
010100090311     d  F6Attivo                       n   overlay(IndDspF:06)
010200141205     d  F8Attivo                       n   overlay(IndDspF:08)
010300151217     d  F9Attivo                       n   overlay(IndDspF:09)
010400090311     d  F16Attivo                      n   overlay(IndDspF:16)
010500090306         // - Emissione messaggio di errore
010600090311     d  ErrMessage                     n   overlay(IndDspF:28)
010700090306         // - Protezione campi
010800090311     d  ProtDATAI                      n   overlay(IndDspF:40)
010900120112     d  InserCCM2                      n   overlay(IndDspF:41)
011000141106         // - Visualizza campi
011100141128     d  VisCEksu                       n   overlay(IndDspF:46)
011200161025     d  VisDVD                         n   overlay(IndDspF:47)
011300090306         // - Posizionamento cursore & segnalazione errore
011400090311     d  PosCurCcm                      n   overlay(IndDspF:50)
011500090311     d  PosCurAut                      n   overlay(IndDspF:51)
011600090311     d  PosCurTpi                      n   overlay(IndDspF:52)
011700090311     d  PosCurDir                      n   overlay(IndDspF:53)
011800090311     d  PosCurSchGg                    n   overlay(IndDspF:54)
011900090311     d  PosCurSch01                    n   overlay(IndDspF:55)
012000090311     d  PosCurSch02                    n   overlay(IndDspF:56)
012100090311     d  PosCurSch04                    n   overlay(IndDspF:57)
012200090311     d  PosCurSch06                    n   overlay(IndDspF:58)
012300090311     d  PosCurSch09                    n   overlay(IndDspF:59)
012400090311     d  PosCurSch11                    n   overlay(IndDspF:60)
012500090311     d  PosCurKsu                      n   overlay(IndDspF:61)
012600090311     d  PosCurMesi                     n   overlay(IndDspF:62)
012700090311     d  PosCurTdta                     n   overlay(IndDspF:63)
012800090311     d  PosCurTad                      n   overlay(IndDspF:64)
012900090311     d  PosCurTadu                     n   overlay(IndDspF:65)
013000090311     d  PosCurKsc                      n   overlay(IndDspF:66)
013100090311     d  PosCurCtr                      n   overlay(IndDspF:67)
013200090311     d  PosCurImp                      n   overlay(IndDspF:68)
013300090311     d  PosCurFimp                     n   overlay(IndDspF:69)
013400090311     d  PosCurFmi                      n   overlay(IndDspF:70)
013500090311     d  PosCurDatai                    n   overlay(IndDspF:71)
013600090311     d  PosCurLnp                      n   overlay(IndDspF:72)
013700090327     d  PosCurTpt                      n   overlay(IndDspF:73)
013800110713     d  PosCurPgm                      n   overlay(IndDspF:74)
013900110713     d  PosCurEle                      n   overlay(IndDspF:75)
014000120112     d  PosCurCCM2                     n   overlay(IndDspF:76)
014100140813     d  PosCurTpAdd                    n   overlay(IndDspF:77)
014200140813     d  PosCurCompCE                   n   overlay(IndDspF:78)
014300141203     d  PosCurAutDG                    n   overlay(IndDspF:79)
014400090306         //   - Riemissione videata
014500090311     d  ErrGenerico                    n   overlay(IndDspF:99)
014600071207
014700090306       // - Parametri ricevuti
014800071206     d KPJBA         e ds
014900071217     d TNTB46ds      e ds                  inz
015000121212     d TNTB464ds     e ds                  inz
015100151217     d TNTBJDCds     e ds                  inz
015200071207
015300090306       // - Reperimento dati utente
015400071207     d TIBS34ds      e ds
015500071207
015600090306       // - Controllo/Decodifica cliente
015700090304     d TIBS69ds      e ds                  qualified  inz
015800090304     d ds_CNACO      e ds                  extname(CNACO00F)
015900090304     d                                     qualified  inz
016000090304     d ds_CNIND      e ds                  extname(CNIND00F)
016100090304     d                                     qualified  inz
016200090304     d ds_CNCLP      e ds                  extname(CNCLP00F)
016300090304     d                                     qualified  inz
016400090304     d ds_FNCLS      e ds                  extname(FNCLS00F)
016500090304     d                                     qualified  inz
016600141203
016700090306       // - Dati record principale della tabella
016800090306     d TNTBEds       e ds                  inz  extname(TNTBE00F)
016900090306     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
017000090306     d                                          prefix(TBX:3)
017100090312
017200090312       // - Parametri per Ricerca/controllo tabelle
017300090312     d TIBS02ds      e ds                  inz
017400090312     d  T02mod       e                     inz('C')
017500090312     d  T02cod       e                     inz(c_Tab)
017600071207
017700090306       // - Tabella "LAC" = Clienti per ritorno immagini
017800071206     d dLAC          e ds                  inz
017900090306       // - Tabella "LAC" del padre per controllo legami
018000090303     d dLACp         e ds                  extname(dLAC) inz
018100121016     d                                     prefix(p_)
018200090303     d  p_SchMm               71     82
018300121016
018400121016       // - Tabella "JDC"
018500121016     d dJDC          e ds                  extname(dJDC) inz
018600121217       // - Tabella "LA2" = Estensione tab.LAC
018700121217     d dLA2          e ds                  inz
018800121016
018900090312       // - Tabella "GED" = Divise area gestionale
019000090312     d dGED          e ds                  inz
019100090312       // - Tabella "GEI" = Importi vari area gestionale
019200090312     d dGEI          e ds                  inz
019300141128
019400141128       // - Tabella "TAL" = Tipo addebito dichiarato
019500141128     d dTAL          e ds                  inz
019600141106
019700141106       // - File Conto Economico
019800141106     d ECEVD00F      e ds                  extname(ECEVD00F)
019900141203
020000141203      // - File Date
020100141203     d DATECNV0F     e ds                  extname(DATECNV0F)
020200090303
020300090306       // - Mesi in cui è schedulata la mail di promemoria creazione CD
020400090303     d ds_SchMm        ds            12    inz
020500090303     d  V2Csch01                           inz
020600090303     d  V2Csch02                           inz
020700090303     d  V2Csch03                           inz
020800090303     d  V2Csch04                           inz
020900090303     d  V2Csch05                           inz
021000090303     d  V2Csch06                           inz
021100090303     d  V2Csch07                           inz
021200090303     d  V2Csch08                           inz
021300090303     d  V2Csch09                           inz
021400090303     d  V2Csch10                           inz
021500090303     d  V2Csch11                           inz
021600090303     d  V2Csch12                           inz
021700090311
021800090311       // - Struttura di comodo x ctrl mesi di schedulazione
021900090311     d wSchMm_ds       ds            12    inz
022000090311     d  $SchMm                 1     12    dim(12)
022100090319       // - Struttura per passaggio dati ad interrogazione tabella
022200090327     d Param01         ds                  inz
022300090327     d  P01cod                        7  0 inz
022400090327     d  P01ord                        1    inz
022500090327     d  P01ksu                        7  0 inz
022600090327     d  P01ke1                        7    inz
022700090327     d  P01ke2                       15    inz
022800090327     d  P01rit                        1    inz
022900071207
023000090306       //--------------------------------------------------------------
023100090306       //?Definizione procedure usate.                                 ?
023200090306       //--------------------------------------------------------------
023300090306
023400090306       // - Reperimento dati utente
023500090306      /copy gaitrasrc/srcProtoPR,TIBS34R
023600071207
023700090304       // - Ricerca/Controllo tabelle
023800090306      /copy gaitrasrc/srcProtoPR,TIBS02R
023900071207
024000090304       // - Controllo/Decodifica cliente
024100090306      /copy gaitrasrc/srcProtoPR,TIBS69R
024200090319
024300090319       // - Interrogazione tabella LAC
024400090319     d tntb461r        pr                  extpgm('TNTB461R')
024500090319     d  kpjba                              likeds(KPJBA)
024600141204     d  P01opz3                       1a   const
024700121212
024800121212       // - Gestione tabella LA2
024900121212     d tntb464r        pr                  extpgm('TNTB464R')
025000121213     d  kpjba                              likeds(KPJBA)
025100121212     d  tntb464ds                          likeds(TNTB464DS)
025200151217
025300151217       // - Gestione tabella JDC
025400151217     d tntbJDCr        pr                  extpgm('TNTBJDCR')
025500151217     d  kpjba                              likeds(KPJBA)
025600071207
025700090306       //--------------------------------------------------------------
025800090306       //?Definizione variabili globali.                               ?
025900090306       //--------------------------------------------------------------
026000090306
026100090306       // - Flags booleani
026200090306     d $InzD02         s               n   inz(*on)
026300090306     d $Fine           s               n   inz
026400090923     d $forza          s               n   inz
026500090306     d $CtrMm          s               n   inz
026600090313     d $ByPass         s               n   inz
026700120112     d $Copia          s               n   inz
026800121016     d wForza          s               n   inz
026900121030     d wForzaTPI       s               n   inz
027000121030     d wForzaFMI       s               n   inz
027100121030     d wForzaDIR       s               n   inz
027200071207
027300090306       // - Gestione video
027400141117     d $Video          s              2    inz('D2')
027500090313
027600090313       // - Indici di schiera
027700090313     d xx              s              3  0 inz
027800071207
027900090313       // - Campi di comodo
028000090306     d wDataI          s              6
028100090306     d wDatEla         s              6
028200090306     d wPos1           s              2  0 inz
028300090306     d wPos2           s              2  0 inz
028400090313     d wCount          s              2  0 inz
028500090313     d wSavSch         s                   like(ds_SchMm)  inz
028600090316     d SAVke1          s                   like(TBEke1)    inz
028700090923     d savtad          s                   like(§lactad)
028800090923     d savtadu         s                   like(§lactadu)
028900090923     d savksc          s                   like(§lacksc)
029000090923     d savctr          s                   like(§lacctr)
029100090923     d savimp          s                   like(§lacimp)
029200090923     d savfimp         s                   like(§lacfimp)
029300121220     d woggi           s              8  0 inz
029400141201     d wceksc          s              5  3 inz
029500141201     d wceksu          s              5  3 inz
029600141203     d conta           s              5i 0 inz
029700141204     d P01opz3         s              1a   inz('U')
029800071207
029900090306       //--------------------------------------------------------------
030000090306       //?Definizione key-list.                                        ?
030100090306       //--------------------------------------------------------------
030200090306
030300090306       // - File TNTBE01L
030400090306     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
030500090306     d                                     prefix(k_)  inz
030600071206
030700090306       //--------------------------------------------------------------
030800090306       //?Riepilogo indicatori.                                        ?
030900090306       //--------------------------------------------------------------
031000090306       // 01    - RECORD annullato
031100090306       // 02    - acceso IMMISSIONE - spento MANUTENZIONE
031200090306       // 25    - Comodo
031300090306       // 28    - Emissione del messaggio di errore a video
031400090306       // 40    - Protezione campi in prima estrazione
031500090306       // x D01:
031600090306       // 50    - Codice cliente errato o mancante
031700090306       // x D02:
031800120112       // 41    - Copia in D02: V2CCCM non protetto
031900090306       // 53    - Directory per immagini       errata
032000090306       // 54    - Giorno di schedulazione CD   errato
032100090306       // 55    - Selezione mesi obbligatoria
032200090306       // 56    - Selezione del mese di febbraio
032300090306       // 57    - Selezione del mese di aprile
032400090306       // 58    - Selezione del mese di giugno
032500090306       // 59    - Selezione del mese di settembre
032600090306       // 60    - Selezione del mese di novembre
032700090306       // 61    - Codice Padre                 errato
032800090306       // 62    - N. mesi da estrarre          errato
032900090306       // 63    - Tipo data di elaborazione    errato
033000090306       // 64    - Tipo addebito                errato
033100090306       // 65    - Creazione addebito           errato
033200090306       // 66    - Cliente tassazione           errato
033300090306       // 67    - Codice tariffa               errato
033400090306       // 68    - Importo forzato              errato
033500090306       // 69    - Tipo importo forzato         errato
033600090306       // 70    - Flag nome immagine           errato
033700090306       // 71    - Prima elaborazione           errata
033800090306       // 99    - Generico di errore
033900090306       //--------------------------------------------------------------
034000071206
034100071206     c     *Entry        plist
034200071206     c                   parm                    KPJBA
034300071207
034400071207      /free
034500071207
034600071207       // Operazioni iniziali
034700090306       exsr  sr_RoutInz;
034800071207
034900071207       // Gestione video
035000090306       DOW  $Fine = *off;
035100071207         select;
035200090306           when  $Video = 'D2';
035300090306             exsr  sr_GesD02;
035400101103           when  $Video = 'W1';
035500101103             exsr  sr_GesW01;
035600071207           other;
035700071207             leave;
035800071207         endsl;
035900071207       ENDDO;
036000071207
036100071207       // Operazioni finali
036200090306       exsr  sr_RoutEnd;
036300071206
036400071212       //--------------------------------------------------------------
036500090306       //?Operazioni iniziali.                                         ?
036600071212       //--------------------------------------------------------------
036700090306       BEGSR  sr_RoutInz;
036800141106
036900141106         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
037000090306
037100090306         *inLR = *on;
037200071207
037300090306         if  kpjbu <> *blank;
037400071217           TNTB46ds = kpjbu;
037500071207         else;
037600090306           clear  TNTB46ds;
037700071207         endif;
037800071217         B46fxx = *blank;
037900071217         B46err = *off;
038000071217         B46msg = *blank;
038100071207
038200090306         // Reperimento dati job
038300090306         exsr  DatiJob;
038400071207
038500090306         // Impostazione nome programma a video
038600071207         V1Tpgm = SDSpgm;
038700090306
038800090306         // Aggancio dati generali della tabella in esame
038900090306         clear  k_TBEcod;
039000090306         k_TBEke1 = *zero;
039100090306         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
039200090306                = c_Tab;
039300090306         clear  k_TBEke2;
039400090306         clear  k_TBElin;
039500090306         k_TBEsif = KNSIF;
039600090306         chain(n)  %kds(k05tntbe01)  TNTBE000;
039700090306         if  not  %found(TNTBE01L);
039800090306           clear  k_TBEsif;
039900090306           chain(n)  %kds(k05tntbe01)  TNTBE000;
040000090306         endif;
040100090306         if  %found(TNTBE01L);
040200090306           xTNTBEds = TNTBEds;
040300090306         else;
040400090306           clear  xTNTBEds;
040500090306         endif;
040600071207
040700090306         // Verifica parametri ricecvuti
040800090306         if  B46ke1  <> *blank;
040900141117           $Video  = 'D2';
041000141117           $InzD02 = *on;
041100071207         endif;
041200121220
041300121220         // Imposto la data di oggi
041400121220         woggi = %dec(%date());
041500071207
041600071207       ENDSR;
041700071206
041800071212       //--------------------------------------------------------------
041900090306       //?Reperimento Dati del job (Utente/Operativi).                 ?
042000071212       //--------------------------------------------------------------
042100090306       BEGSR  DatiJob;
042200071207
042300071207         in(E) §AzUte;
042400071207         if NOT %error;
042500071207           in(E) §DatiUte;
042600071207         endif;
042700090306         if %error or RSut = *blank;
042800071207           clear TIBS34ds;
042900071207           tibs34r(tibs34ds);
043000071207           in §AzUte;
043100071207           in §DatiUte;
043200071207         endif;
043300071207
043400071207       ENDSR;
043500141117
043600141117       //--------------------------------------------------------------
043700141117       //?Decodifica cliente passato.                                  ?
043800141117       //--------------------------------------------------------------
043900141117       BEGSR  Decodifica;
044000141117
044100141117         $Copia = (B46opz = '3');
044200141117
044300141117         IF  B46ke1 = *blanks;
044400141117           leavesr;
044500141117         ENDIF;
044600141117
044700141117         If  Not  $Copia;
044800141117           clear  TIBS69ds;
044900141117           tibs69ds.I69kcc = DUTkci;
045000141117           tibs69ds.I69kac = %int(B46ke1);
045100141117           tibs69ds.I69sif = knsif;
045200141117           tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
045300141117           if  tibs69ds.O69err = *on;
045400141117             errMessage  = *on;
045500141117             V1Dmsg = $Msg(03);
045600141117             leavesr;
045700141117           endif;
045800141117         EndIf;
045900141117
046000141117         // - Verifica esistenza codice cliente in tabella
046100141117         reset  TNTBE000;
046200141117         k_TBEke1 = B46ke1;
046300141117         clear  k_TBEke2;
046400141117         k_TBElin = TBXlin;
046500141117         k_TBEsif = TBXsif;
046600141117         chain  %kds(K05tntbe01) TNTBE000;
046700141117         if  NOT  %found(TNTBE01L);
046800141117           errMessage  = *on;
046900141117           V1Dmsg = 'Cliente non presente in tabella';
047000141117           leavesr;
047100141117         endif;
047200141117
047300141117         dLAC = TBEuni;
047400141117
047500141117       ENDSR;
047600071206
047700071212       //--------------------------------------------------------------
047800090306       //?Gestione videata D02                                         ?
047900071212       //--------------------------------------------------------------
048000090306       BEGSR  sr_GesD02;
048100071207
048200090306         // Inizializzazione videata
048300090306         if   $InzD02 = *on;
048400090306           exsr  sr_InzD02;
048500090306           $InzD02 = *off;
048600071207         endif;
048700071207
048800090306         // Emissione testata
048900090306         write  TN46T01;
049000071207
049100090306         // Emissione videata
049200151130         if  B46opz = '5';
049300141117           write  TN46D02;
049400141117           exfmt  PROTECT;
049500141117         else;
049600090306           exfmt  TN46D02;
049700071217         endif;
049800071207         errMessage  = *off;
049900071207         errGenerico = *off;
050000090306         clear  V1Dmsg;
050100071207
050200071207         SELECT;
050300090306           // F3=Fine
050400090306           WHEN  dsp_aid = c_F03;
050500141117             exsr  sr_F03D02;
050600090319           // F04=Interrogazione Padre
050700090319           WHEN  dsp_aid = c_F04;
050800090319             exsr  sr_F04D02;
050900141205           // F08=Vis. tabella LA2
051000141205           WHEN  dsp_aid = c_F08;
051100141205             exsr  sr_F08D02;
051200151217           // F09=Vis. tabella JDC
051300151217           WHEN  dsp_aid = c_F09;
051400151217             exsr  sr_F09D02;
051500090306           // F12=Ritorno
051600090306           WHEN  dsp_aid = c_F12;
051700090306             exsr  sr_F12D02;
051800090306           // Enter o F5 o F6 o F16
051900071207           OTHER;
052000090306             // - Controllo dati
052100090306             if  dsp_aid = c_F16;
052200090306               exsr  sr_CtrAnnull;
052300090306             else;
052400090306               exsr  sr_CtrD02;
052500090306             endif;
052600090306             if  errGenerico;
052700071207               leavesr;
052800071207             endif;
052900121212
053000090306             // - Conferma dati:
053100090306             //   F5=Ripristino; F6=Conferma; F16=Annullamento
053200090306             if  dsp_aid = c_F05  or
053300090306                 dsp_aid = c_F06  or
053400090306                 dsp_aid = c_F16;
053500121212
053600090306               exsr  Upd_TNTBE;
053700121213
053800121217           //?Se F6 e immagine da duplicare su altra direcotru devo andare su tabella LA2
053900121217               IF  dsp_aid = c_F06 and V2Cla2 = 'S';
054000121217                 exsr GESLA2;
054100121213                 $Video = 'D2';
054200121213                 $InzD02 = *on;
054300121213                 chain  %kds(K05tntbe01) TNTBE000;
054400121213                 IF  B464err <> '0' and B464err <> *blanks;
054500121213                   ErrMessage  = *on;
054600121213                   ErrGenerico = *on;
054700121213                   V1Dmsg = B464msg;
054800121213                   leavesr;
054900121213                 ENDIF;
055000121213                 unlock TNTBE01L;
055100121213               ENDIF;
055200151217
055300151217           //?Se F6 e immagine da duplicare su altra direcotru devo andare su tabella JDC
055400151217               IF  dsp_aid = c_F06 and V2Merge = 'S';
055500151217                 exsr GESJDC;
055600151217                 $Video = 'D2';
055700151217                 $InzD02 = *on;
055800151217                 chain  %kds(K05tntbe01) TNTBE000;
055900151217                 IF  BJDCerr <> '0' and BJDCerr <> *blanks;
056000151217                   ErrMessage  = *on;
056100151217                   ErrGenerico = *on;
056200151217                   V1Dmsg = BJDCmsg;
056300151217                   leavesr;
056400151217                 ENDIF;
056500151217                 unlock TNTBE01L;
056600151217               ENDIF;
056700121212
056800071207               if  NOT  errGenerico;
056900141117                 $Fine  = *on;
057000071207               endif;
057100071207             EndIf;
057200071207
057300071207         ENDSL;
057400071207
057500071207       ENDSR;
057600071206
057700071212       //--------------------------------------------------------------
057800090306       //?Inizializzazione videata D01                                 ?
057900071212       //--------------------------------------------------------------
058000090306       BEGSR  sr_InzD02;
058100071207
058200090306         reset  $CtrMm;
058300090313         reset  $ByPass;
058400121016         reset  wForza;
058500121030         reset  wForzaTPI;
058600121030         reset  wForzaFMI;
058700121030         reset  wForzaDIR;
058800071207
058900101103         clear  TB46W01;
059000101103
059100090306         clear  TN46D02;
059200071210
059300090306         IndDspF = *zero;
059400141201         k_TBEcod = c_Tab;
059500141117
059600141117         exsr Decodifica;
059700141117         if  errMessage = *on;
059800141117           B46err = *on;
059900141117           B46msg = V1Dmsg;
060000141117           $Fine  = *on;
060100141117           leavesr;
060200141117         endif;
060300090309
060400090309         // Impostazione indicatori per abilitazione tasti funzionali
060500090309         exsr  Set_Ind_D02;
060600090309
060700120112         // - Codice estrazione (chiave)
060800141117         if  Not  $Copia and B46ke1 <> *blanks;
060900141117           V2Cccm = %int(B46ke1);
061000141117           V2Dccm = ds_CNACO.ACOrag;
061100141117           InserCCM2 = *off;
061200120112         endif;
061300141117
061400141117         IF  $Copia;
061500141117           V2Cksu = %int(B46ke1);
061600141117           V2Dksu = ds_CNACO.ACOrag;
061700141117           InserCCM2 = *on;
061800141117         ENDIF;
061900141117
062000141117         // Impostazione dati di record nuovo
062100141117         IF  B46ke1 = *blanks;
062200141117           InserCCM2 = *on;
062300141117           V1Tlav = 'IMMISSIONE';
062400141117           *in02 = *on;
062500141117           clear dLA2;
062600151217           clear dJDC;
062700150122           exec sql
062800150122           set :V2dtaimm = (select DC_DMYY_A1 from DATECNV0F
062900150122                            where DC_YYMD_Z = :woggi);
063000141117           leavesr;
063100141117         ENDIF;
063200071207
063300090306         IF  %found(TNTBE01L);
063400071207
063500090306           // Impostazione dati di record trovato
063600071217           select;
063700141117             when  B46opz = '2';
063800141117               V1Tlav = 'VARIAZIONE';
063900090306             when  B46opz = '5';
064000071217               V1Tlav = 'VISUALIZZA';
064100151130             when  B46opz = '7';
064200151130               V1Tlav = 'RIPRISTINO';
064300120112             when  $Copia;
064400120112               V1Tlav = '  COPIA   ';
064500071217             other;
064600071217               V1Tlav = 'ANNULLATO ';
064700071217           endsl;
064800090225
064900090306           // Codice padre
065000090306           V2Cksu = §LACksu;
065100090309           clear  TIBS69ds;
065200090309           clear  ds_CNACO;
065300090309           clear  ds_CNIND;
065400090309           clear  ds_CNCLP;
065500090309           clear  ds_FNCLS;
065600090310           select;
065700090310             when  §LACksu =  V2Cccm;
065800090310               V2Dksu = V2Dccm;
065900090310             when  §LACksu <> *zero;
066000090310               tibs69ds.I69kcc = DUTkci;
066100090310               tibs69ds.I69kac = %int(V2Cksu);
066200090310               tibs69ds.I69sif = knsif;
066300090310               tibs69r(TIBS69ds:ds_CNACO:ds_CNIND:ds_CNCLP:ds_FNCLS);
066400090310               if tibs69ds.O69err = *on;
066500090310                 V1Dmsg = $Msg(03);
066600090310                 errMessage  = *on;
066700090310                 errGenerico = *on;
066800090310                 PosCurKsu   = *on;
066900090310                 leavesr;
067000090310               endif;
067100090310               V2Dksu = ds_CNACO.ACOrag;
067200090310           endsl;
067300090225
067400090225           V2Caut   = §LACaut;
067500161025           if §LACAut = 'S';
067600161025             VisDVD = *on;
067700161025           else;
067800161025             VisDVD = *off;
067900161025           endif;
068000161025           V2CFiD   = §LACFiD;
068100161025           V2CCpy   = §LACCpy;
068200161025           V2CDst   = §LACDst;
068300090309           V2Ctpi   = §LACtpi;
068400090225
068500090306           // Flag nome immagine
068600090306           V2Cfmi = §LACfmi;
068700090310           reset  TIBS02ds;
068800090303           T02cod = 'NIM';
068900090303           T02ke1 = §LACfmi;
069000090304           TNTBE_RicercaControllo(kpjba : tibs02ds);
069100090306           if  T02err = *blank;
069200090303             V2Dfmi = T02uni;
069300090303           endif;
069400121016
069500121016           V2merge = §LACmerge;
069600121217           V2Cla2  = §LACla2;
069700090225
069800090309           V2Cdir   = §LACdir;
069900090309           V2Ctpt   = §LACtpt;
070000090309           V2Cssr   = §LACssr;
070100090309           V2Clnp   = §LAClnp;
070200090309           V2Cres   = §LACres;
070300090309           V2Crec   = §LACrec;
070400090309           V2Ccsr   = §LACcsr;
070500090309
070600090309           // Prima estrazione
070700090309           clear  wDataI;
070800090309           if  §LACdatai > *zero;
070900090309             %subst(wDataI:1:2) = %subst(%editc(§LACdatai:'X'):5:2);
071000090309             %subst(wDataI:3:4) = %subst(%editc(§LACdatai:'X'):1:4);
071100090310             V2datai = %int(wDataI);
071200090309           endif;
071300090309
071400090309           // Ultima estrazione
071500090309           clear  wDatEla;
071600090309           if  §LACdatela > *zero;
071700090309             %subst(wDatEla:1:2) = %subst(%editc(§LACdatela:'X'):5:2);
071800090309             %subst(wDatEla:3:4) = %subst(%editc(§LACdatela:'X'):1:4);
071900090310             V2datela = %int(wDatEla);
072000090309           endif;
072100090309
072200090309           V2tdta   = §LACtdta;
072300090309           V2mesi   = §LACmesi;
072400071207           V2Cschgg = §LACschgg;
072500071207           V2Csch01 = §LACsch01;
072600071207           V2Csch02 = §LACsch02;
072700071207           V2Csch03 = §LACsch03;
072800071207           V2Csch04 = §LACsch04;
072900071207           V2Csch05 = §LACsch05;
073000071207           V2Csch06 = §LACsch06;
073100071207           V2Csch07 = §LACsch07;
073200071207           V2Csch08 = §LACsch08;
073300071207           V2Csch09 = §LACsch09;
073400071207           V2Csch10 = §LACsch10;
073500071207           V2Csch11 = §LACsch11;
073600071207           V2Csch12 = §LACsch12;
073700090225
073800090306           V2tadu   = §LACtadu;
073900090309           V2Ctad   = §LACtad;
074000090306           V2cksc   = §LACksc;
074100090306           V2cctr   = §LACctr;
074200090306           V2imp    = §LACimp;
074300090306           V2fimp   = §LACfimp;
074400110713           V2pgm    = §LACpgm;
074500110713           V2ele    = §LACele;
074600090306           V2note   = §LACnote;
074700140813
074800140813           IF  §LACdtaimm <> *blanks;
074900140813             V2dtaimm = %subst(§LACdtaimm:7:2) + '/' +
075000140813                        %subst(§LACdtaimm:5:2) + '/' +
075100140813                        %subst(§LACdtaimm:1:4);
075200140813           ENDIF;
075300150122
075400150122           IF  $Copia and V2dtaimm = *blanks;
075500150122             exec sql
075600150122             set :V2dtaimm = (select DC_DMYY_A1 from DATECNV0F
075700150122                              where DC_YYMD_Z = :woggi);
075800150122           ENDIF;
075900141128
076000140813           V2utente = §LACutente;
076100141128
076200140813           V2compce = §LACcompce;
076300141128           reset  TIBS02ds;
076400141128           T02cod = 'CED';
076500141128           T02ke1 = V2compce;
076600141128           T02sif = KNSIF;
076700141128           TNTBE_RicercaControllo(kpjba : tibs02ds);
076800141128           IF  T02err = *blank;
076900141128             V2dcompce = T02uni;
077000141128           ENDIF;
077100141202           IF  V2compce = *blanks;
077200141202             V2dcompce = 'NON DICHIARATO';
077300141202           ENDIF;
077400141202
077500141128           IF V2Cccm <> V2Cksu;
077600141128             clear dLACp;
077700141128             reset  TIBS02ds;
077800141128             T02sif = KNSIF;
077900141128             T02cod = 'LAC';
078000141128             T02ke1 = %editc(V2Cksu:'X');
078100141128             TNTBE_RicercaControllo(kpjba : tibs02ds);
078200141128             IF  T02err = *blank;
078300141128               dLACp = T02uni;
078400141128             ENDIF;
078500141128             reset  TIBS02ds;
078600141128             T02cod = 'CED';
078700141128             T02ke1 = p_§LACcompce;
078800141128             T02sif = KNSIF;
078900141128             TNTBE_RicercaControllo(kpjba : tibs02ds);
079000141128             IF  T02err = *blank;
079100141128               V2dceksu = T02uni;
079200141128             ENDIF;
079300141128             VisCEksu = *on;
079400141128           ELSE;
079500141128             VisCEksu = *off;
079600141128           ENDIF;
079700141128
079800140813           V2tpadd  = §LACtpadd;
079900140813           reset  TIBS02ds;
080000141128           clear dTAL;
080100140813           T02cod = 'TAL';
080200140813           T02ke1 = V2tpadd;
080300140813           T02sif = KNSIF;
080400140813           TNTBE_RicercaControllo(kpjba : tibs02ds);
080500140813           IF  T02err = *blank;
080600141128             dTAL = T02uni;
080700141128           ENDIF;
080800141128           V2dtpadd = §TALdes;
080900141202           IF  V2tpadd = *blanks;
081000141202             V2dtpadd = 'NON DICHIARATO';
081100141202           ENDIF;
081200090923
081300090923           *in02 = *off;
081400121217
081500121217           // Cerco i dati della tabella LA2
081600121217           clear dLA2;
081700121217           reset  TIBS02ds;
081800121217           IF  V2Cla2 = 'S';
081900121217             T02cod = 'LA2';
082000121217             T02ke1 = B464ke1;
082100121217             TNTBE_RicercaControllo(kpjba : tibs02ds);
082200121217             IF  T02err = *blank;
082300121217               dLA2 = T02uni;
082400121217             ENDIF;
082500121217           ENDIF;
082600151217
082700151217           // Cerco i dati della tabella JDC
082800151217           clear dJDC;
082900151217           reset  TIBS02ds;
083000151217           IF  V2Merge = 'S';
083100151217             T02cod = 'JDC';
083200151217             T02ke1 = BJDCke1;
083300151217             TNTBE_RicercaControllo(kpjba : tibs02ds);
083400151217             IF  T02err = *blank;
083500151217               dJDC = T02uni;
083600151217             ENDIF;
083700151217           ENDIF;
083800071207
083900071207         ENDIF;
084000090923
084100090923         // Salvo i dati sensibili per la creazione addebito
084200090923         savtad = v2ctad;
084300090923         savtadu = v2tadu;
084400090923         savksc = v2cksc;
084500090923         savctr = v2cctr;
084600090923         savimp = v2imp;
084700090923         savfimp = v2fimp;
084800090923         $forza = *off;
084900130201
085000130201         // Imposto (se c'è) data ultima variazione
085100130201         clear V2datauv;
085200130201         IF  TBEdtr > 0;
085300130201           V2datauv = %subst(%editc(TBEdtr:'X'):7:2) + '/' +
085400130201                      %subst(%editc(TBEdtr:'X'):5:2) + '/' +
085500130201                      %subst(%editc(TBEdtr:'X'):1:4);
085600130201         ENDIF;
085700141128
085800141128         // Cerco la personalizzazione C/E del KSC
085900141203         exec sql
086000141203         select EVDitr into :wceksc
086100141203         FROM ecevd00f
086200141203         WHERE EVDcli = :V2cccm and
086300141203               EVDvoc = 199 and
086400141203               EVDttv = 'K' and
086500141203               EVDddt <= :woggi and
086600141203               EVDdst >= :woggi;
086700141203         IF  sqlcod = 100 or sqlcod < 0;
086800141203           V2ceksc = 'STD';
086900141203         ELSE;
087000141203           V2ceksc = %editw(wceksc:'0 ,   ');
087100141203         ENDIF;
087200141128
087300141128         // Cerco la personalizzazione C/E del KSU
087400141203         IF  VisCeKsu;
087500141203           exec sql
087600141203           select EVDitr into :wceksu
087700141203           FROM ecevd00f
087800141203           WHERE EVDcli = :V2Cksu and
087900141203                 EVDvoc = 199 and
088000141203                 EVDttv = 'K' and
088100141203                 EVDddt <= :woggi and
088200141203                 EVDdst >= :woggi;
088300141203           IF  sqlcod = 100 or sqlcod < 0;
088400141203             V2ceksu = 'STD';
088500141203           ELSE;
088600141203             V2ceksu = %editw(wceksu:'0 ,   ');
088700141203           ENDIF;
088800141203         ENDIF;
088900141203
089000141203         // Data AUT DG per c/e  a 0 o NO ddt
089100141203         IF  §LACdtaudg <> *blanks;
089200141203           exec sql
089300141203           set :V2dtautdg =
089400141203               (select DC_DMYY_Z from DATECNV0F
089500141203                where DC_YYMD_C = :§LACdtaudg);
089600141203         ENDIF;
089700071207
089800071207       ENDSR;
089900071217
090000071217       //--------------------------------------------------------------
090100090306       //?Settaggio indicatori nella videata D02 per abilitazione      ?
090200090306       //?  tasti funzionali                                           ?
090300071217       //--------------------------------------------------------------
090400090306       BEGSR  Set_Ind_D02;
090500090319
090600141204         F4Attivo  = B46opz <> '5';
090700090306         F5Attivo  = ((%found(TNTBE01L) and TBEatb <> *blank)
090800071217                                        and B46opz <> '5');
090900071217         F6Attivo  = (NOT F5Attivo and B46opz <> '5');
091000141205         F8Attivo  = B46opz = '5' and §LACla2 = 'S';
091100151217         F9Attivo  = B46opz = '5' and §LACmerge = 'S';
091200090306         F16Attivo = ((%found(TNTBE01L) and TBEatb = *blank)
091300151130                                        and B46opz <> '5');
091400090306
091500120112         // - Se ultima estrazione impostata proteggo la prima estrazione
091600090309         ProtDATAI = (§LACdatela <> *zero);
091700071217
091800071217       ENDSR;
091900141117
092000141117       //--------------------------------------------------------------
092100141117       //?Gestione tasto funzionale F3 da videata D02                  ?
092200141117       //?F3=Fine                                                      ?
092300141117       //--------------------------------------------------------------
092400141117       BEGSR  sr_F03D02;
092500141117
092600141117         // Chiusura del programma
092700141117         $Fine = *on;
092800141117
092900141117         // Restituzione indicazione del tasto funzionale premuto
093000141117         B46fxx = '1';
093100141117
093200141117       ENDSR;
093300090319
093400090319       //--------------------------------------------------------------
093500090319       //?Gestione tasto funzionale F04 da videata D02                 ?
093600090319       //?F04=Interrogazione Padre                                     ?
093700090319       //--------------------------------------------------------------
093800090319       BEGSR  sr_F04D02;
093900090319
094000090319         // Richiamo pgm interrogazione
094100090327         clear  Param01;
094200090327         P01ord = '1';
094300090327         P01ksu = V2Cksu;
094400090327         KPJBU  = Param01;
094500141204         tntb461r (KPJBA : P01opz3);
094600141204         Param01 = kpjbu;
094700141204         IF  V2Cksu = *zeros and P01ke1 <> *zeros and
094800141204             P01ke1 <> *blanks;
094900141204           V2Cksu = %dec(P01ke1:7:0);
095000141204         ENDIF;
095100090319
095200090319       ENDSR;
095300090319
095400141205       //--------------------------------------------------------------
095500141205       //?Gestione tasto funzionale F08 da videata D02                 ?
095600141205       //?F08=Vis. Tabella LA2
095700141205       //--------------------------------------------------------------
095800141205       BEGSR  sr_F08D02;
095900141205
096000141205         // Richiamo pgm tab. LA2
096100141205         exsr GESLA2;
096200141205
096300141205         // Devo reimpostare i dati per visualizzare nuovamente la tab.LAC
096400141205         $Video = 'D2';
096500141205         $InzD02 = *on;
096600141205         chain  %kds(K05tntbe01) TNTBE000;
096700141205
096800141205         // Se torna errore da tabella LA2 lo emetto
096900141205         IF  B464err <> '0' and B464err <> *blanks;
097000141205           ErrMessage  = *on;
097100141205           ErrGenerico = *on;
097200141205           V1Dmsg = B464msg;
097300141205           leavesr;
097400141205         ENDIF;
097500141205
097600141205       ENDSR;
097700151217
097800151217       //--------------------------------------------------------------
097900151217       //?Gestione tasto funzionale F09 da videata D02                 ?
098000151217       //?F09=Vis. Tabella JDC
098100151217       //--------------------------------------------------------------
098200151217       BEGSR  sr_F09D02;
098300151217
098400151217         // Richiamo pgm tab. LA2
098500151217         exsr GESJDC;
098600151217
098700151217         // Devo reimpostare i dati per visualizzare nuovamente la tab.LAC
098800151217         $Video = 'D2';
098900151217         $InzD02 = *on;
099000151217         chain  %kds(K05tntbe01) TNTBE000;
099100151217
099200151217         // Se torna errore da tabella JDC lo emetto
099300151217         IF  BJDCerr <> '0' and BJDCerr <> *blanks;
099400151217           ErrMessage  = *on;
099500151217           ErrGenerico = *on;
099600151217           V1Dmsg = BJDCmsg;
099700151217           leavesr;
099800151217         ENDIF;
099900151217
100000151217       ENDSR;
100100141205
100200071217       //--------------------------------------------------------------
100300090306       //?Gestione tasto funzionale F12 da videata D02                 ?
100400090306       //?F12=Ritorno                                                  ?
100500071217       //--------------------------------------------------------------
100600090306       BEGSR  sr_F12D02;
100700071217
100800071217           B46fxx = '2';
100900071217           $Fine = *on;
101000071217
101100071217       ENDSR;
101200090306
101300090306       //--------------------------------------------------------------
101400090306       //?Controllo "annullabilità" del record (padre senza figli)     ?
101500090306       //--------------------------------------------------------------
101600090306       BEGSR  sr_CtrAnnull;
101700090306
101800090306         IndDspF = *zero;
101900090306         // REimpostazione indicatori per abilitazione tasti funzionali
102000090306         exsr  Set_Ind_D02;
102100090306
102200090306         if  V2Cksu <= *zero;
102300090306           V2Cksu = §LACksu;
102400090306         endif;
102500090306
102600090306         // Se figlio (non padre): fine controlli
102700090306         if  V2Cccm <> V2Cksu;
102800090306           leavesr;
102900090306         endif;
103000090306
103100090306         //?Verifica se padre con figli?
103200090327         clear  dLACp;
103300090316         SAVke1 = TBEke1;
103400090306         setll     %kds(k05tntbe01 : 1)  TNTBE000;
103500090306         reade(n)  %kds(k05tntbe01 : 1)  TNTBE000;
103600090306
103700090306         dow  not  %eof(TNTBE01L);
103800090316           if  TBEatb = *blank   and
103900090316               %editc(V2Cccm : 'X') <> %trimr(TBEke1);
104000090306             dLACp = TBEuni;
104100090316             if  p_§LACksu = V2Cccm;
104200090306               errMessage  = *on;
104300090306               errGenerico = *on;
104400090306               PosCurKsu   = *on;
104500090306               V1Dmsg = 'Record NON cancellabile: trattasi di PADRE +
104600090306                         con figli (vedi ' + %trimr(TBEke1) + ')';
104700090306               leavesr;
104800090306             endif;
104900090306           endif;
105000090306         reade(n)  %kds(k05tntbe01 : 1)  TNTBE000;
105100090306         enddo;
105200090316
105300120112         // - Riaggancio del rec. in manutenzione/copia
105400090316         TBEke1 = SAVke1;
105500090316         reset  TNTBE000;
105600090316         k_TBEcod = c_Tab;
105700120112         k_TBEke1 = %editc( V2Cccm : 'X' );
105800090316         clear  k_TBEke2;
105900090316         k_TBElin = TBXlin;
106000090316         k_TBEsif = TBXsif;
106100090316         chain  %kds(K05tntbe01) TNTBE000;
106200090306
106300090306       ENDSR;
106400071206
106500071212       //--------------------------------------------------------------
106600090306       //?Controllo videata D02                                        ?
106700071212       //--------------------------------------------------------------
106800090306       BEGSR  sr_CtrD02;
106900071207
107000090309         %subst(IndDspF : 50) = *off;
107100120112
107200141117         IF  $Copia or *in02;
107300120112
107400120112           Select;
107500120112
107600120112             // -?Codice estrazione (SE copia)?
107700120112             When  V2Cccm = *zero;
107800120112               errMessage  = *on;
107900120112               errGenerico = *on;
108000120112               PosCurCcm2  = *on;
108100120112               V1Dmsg = $Msg(01);
108200120112               leavesr;
108300120112
108400120112             // -?Controllo / Decodifica?
108500151130             Other;
108600151130             //il test seguente rimosso immagino fosse fatto per velocizzare ma nel secondo giro
108700151130             //impediva i test
108800151130             //When  tibs69ds.I69kac <> V2Cccm  or
108900151130             //      k_TBEke1 <> %editc( V2Cccm : 'X' );
109000120112               clear  TIBS69ds;
109100120112               tibs69ds.I69kcc = DUTkci;
109200120112               tibs69ds.I69kac = V2Cccm;
109300120112               tibs69ds.I69sif = knsif;
109400120112               tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
109500120112               if  tibs69ds.O69err = *on;
109600120112                 errMessage  = *on;
109700120112                 errGenerico = *on;
109800120112                 PosCurCcm2  = *on;
109900120112                 V1Dmsg = $Msg(03);
110000120112                 leavesr;
110100120112               endif;
110200120112               V2Dccm = ds_CNACO.ACOrag;
110300120112               // -?Verifica esistenza in tabella?
110400120112               k_TBEke1 = %editc( V2Cccm : 'X' );
110500120112               chain  %kds(K05tntbe01) TNTBE000;
110600120112               select;
110700120112                 // - -?NON ammesso nuovo record per cliente annullato?
110800120112                 when  NOT  %found(TNTBE01L)  and  ds_CNACO.ACOflg <> *blank;
110900120112                   errMessage  = *on;
111000120112                   errGenerico = *on;
111100120112                   PosCurCcm2  = *on;
111200120112                   V1Dmsg = $Msg(04);
111300120112                   leavesr;
111400120112                 // - -?Impossibile inserimento di record già esistente?
111500120112                 when  %found(TNTBE01L);
111600120112                   errMessage  = *on;
111700120112                   errGenerico = *on;
111800120112                   PosCurCcm2  = *on;
111900120112                   V1Dmsg = $Msg(14);
112000120112                   leavesr;
112100120112               endsl;
112200120112
112300120112           EndSl;
112400141202           //?imposto già la directory
112500141202           IF  V2Cdir = *blanks;
112600141202             V2Cdir = %editc(V2Cccm:'X') + '/PI';
112700141202           ENDIF;
112800120112
112900120112         ENDIF;
113000090313
113100090313         // Verifica se variata la schedulazione dell'estrazione
113200090313         if  $ByPass;
113300090313           $ByPass = (ds_SchMm = wSavSch);
113400090313         endif;
113500090313         wSavSch = ds_SchMm;
113600090225
113700090306         //?Codice padre?
113800090225         select;
113900090306           when  V2Cksu  = *zero;
114000090306             errMessage  = *on;
114100090225             errGenerico = *on;
114200090303             PosCurKsu   = *on;
114300090225             V1Dmsg = $Msg(11);
114400090225             leavesr;
114500090306           when  V2Cksu = V2Cccm;
114600090303             V2Dksu = V2Dccm;
114700090225           other;
114800090327             // - Controllo esistenza codice padre come cliente
114900090309             //   in tabella LAC
115000090303             exsr  sr_Padre;
115100090303             if  errGenerico = *on;
115200090226               leavesr;
115300090225             endif;
115400090303             V2Dksu = p_§LACrag;
115500090226         endsl;
115600101103         //?SE codice padre <> da codice cliente?
115700101103         // ?& sono in immissione?
115800141128         IF  V2Cccm <> V2Cksu   and   (*in02 or $copia);
115900101103           // ?=> chiedo se si vogliono riportare i dati del padre?
116000101103           If  W1Ccpy = *blank;
116100101103             $Video  = 'W1';
116200101103             leavesr;
116300101103           EndIf;
116400101103           // ?& il padre ha già fatto estrazioni?
116500101103           If  p_§LACdatela <> *zero;
116600101103             // ?=> riporto le date del padre, proteggo il campo e non controllo?
116700101103             clear wDataI;
116800101103             %subst(wDataI:1:2) = %subst(%editc(p_§LACdatai:'X'):5:2);
116900101103             %subst(wDataI:3:4) = %subst(%editc(p_§LACdatai:'X'):1:4);
117000101103             V2datai = %int(wDataI);
117100101103             clear wDatEla;
117200101103             %subst(wDatEla:1:2) = %subst(%editc(p_§LACdatela:'X'):5:2);
117300101103             %subst(wDatEla:3:4) = %subst(%editc(p_§LACdatela:'X'):1:4);
117400101103             V2datela = %int(wDatEla);
117500101103             ProtDATAI = *on;
117600101103           EndIf;
117700100521         ENDIF;
117800090311
117900090311         //?Tipo elaborazione?
118000090327         select;
118100090327           // - Obbligatorio
118200090327           when  V2Caut <> 'A'  and  V2Caut <> 'S'  and  V2Caut <> 'N';
118300090327             errMessage  = *on;
118400090327             errGenerico = *on;
118500090327             PosCurAut   = *on;
118600090327             V1Dmsg = 'Tipo elaborazione errato o mancante';
118700090327             leavesr;
118800090327         endsl;
118900161025
119000161025         //?Dati invio DVD
119100161025         if V2CAut = 'S';
119200161025             VisDVD = *on;
119300161025           // imposto la filiale invio DVD con la filiale del cliente di raggruppamento
119400161025           if  V2CFiD = 0;
119500161025             V2CFiD = %dec(
119600161025                     %subst(%editc(V2CKsU:'X') : 1 : 3)
119700161025                     : 3 : 0);
119800161025           endif;
119900161025           // imposto il nr.copie a 1
120000161025           if  V2CCpy = 0;
120100161025             V2CCpy = 1;
120200161025           endif;
120300161025         else;
120400161025           VisDVD = *off;
120500161025           V2CFiD = 0;
120600161025           V2CCpy = 0;
120700161025           V2CDst = *blank;
120800161025         endif;
120900161025
121000090306         //?Flag nome immagine?
121100090310         clear  V2Dfmi;
121200090310         select;
121300090310           // - Obbligatorietà
121400090310           when  V2Cfmi = *blank;
121500090310             errMessage  = *on;
121600090310             errGenerico = *on;
121700090310             PosCurFmi   = *on;
121800090310             V1Dmsg = 'Flag nome immagine obbligatorio';
121900090310             leavesr;
122000090310           // - Ricerca
122100090310           when  %scan('?' : V2Cfmi) > *zero;
122200090310             clear  TIBS02ds;
122300090310             T02mod = 'R';
122400090310             T02cod = 'NIM';
122500090310             T02sif = KNSIF;
122600090310             TNTBE_RicercaControllo(kpjba : tibs02ds);
122700090310             if  T02err = *blank;
122800090310               V2Cfmi = %subst(T02ke1 : 1 : %len(V2Cfmi));
122900090310               V2Dfmi = T02uni;
123000090310               errGenerico = *on;
123100090310               leavesr;
123200090310             else;
123300090310               errMessage  = *on;
123400090310               errGenerico = *on;
123500090310               PosCurFmi   = *on;
123600090310               V1Dmsg = T02msg;
123700090310               leavesr;
123800090310             endif;
123900090310           // - Controllo
124000090310           other;
124100090310             reset  TIBS02ds;
124200090310             T02cod = 'NIM';
124300090310             T02ke1 = V2Cfmi;
124400090310             TNTBE_RicercaControllo(kpjba : tibs02ds);
124500090310             if  T02err <> *blank;
124600090310               errMessage  = *on;
124700090310               errGenerico = *on;
124800090310               PosCurFmi   = *on;
124900090310               V1Dmsg = 'Flag nome immagine errato';
125000090310               leavesr;
125100090310             endif;
125200090310             V2Dfmi = T02uni;
125300090310         ENDSL;
125400140813
125500140813         //?Tipo immagine?
125600140813         if  V2Ctpi <> 'TIF'   and   V2Ctpi <> 'JPG' and
125700140813             V2Ctpi <> 'PDF';
125800140813           errMessage  = *on;
125900140813           errGenerico = *on;
126000140813           PosCurTpi   = *on;
126100140813           V1Dmsg = 'Tipo immagine errato o mancante';
126200140813           leavesr;
126300140813         endif;
126400090226
126500090306         //?Directory per immagini?
126600090226         select;
126700090306           when  V2Cdir  = *blank;
126800090226             errMessage  = *on;
126900090226             errGenerico = *on;
127000090226             PosCurDir   = *on;
127100090226             V1Dmsg = $Msg(05);
127200090226             leavesr;
127300090316           when  %subst(V2Cdir : 1 : 1) = *blank;
127400090316             errMessage  = *on;
127500090316             errGenerico = *on;
127600090316             PosCurDir   = *on;
127700090316             V1Dmsg = 'Non ammessi spazi vuoti all''inizio del nome +
127800090316                       della directory';
127900090316             leavesr;
128000121217           WHEN  V2Cdir = §LA2dir and V2Cla2 = 'S';
128100121217             errMessage  = *on;
128200121217             errGenerico = *on;
128300121217             PosCurDir   = *on;
128400121217             V1Dmsg = 'La directory deve essere diversa da tab. LA2';
128500121217             leavesr;
128600090306           when  %scan('\':V2Cdir) > *zero;
128700090226             errMessage  = *on;
128800090226             errGenerico = *on;
128900090226             PosCurDir   = *on;
129000090226             V1Dmsg = $Msg(06);
129100090226             leavesr;
129200090306           when  %scan('%':V2Cdir) > *zero   and
129300090306                           V2Caut  = 'A';
129400090226             errMessage  = *on;
129500090226             errGenerico = *on;
129600090226             PosCurDir   = *on;
129700090226             V1Dmsg = $Msg(07);
129800090226             leavesr;
129900090309           when  %scan('&':V2Cdir) > *zero  and
130000090309                           V2Caut  = 'A';
130100090309             errMessage  = *on;
130200090309             errGenerico = *on;
130300090309             PosCurDir   = *on;
130400090309             V1Dmsg = $Msg(07);
130500090309             leavesr;
130600090226         endsl;
130700090309
130800090316         wPos1 = %scan('/' : V2Cdir);
130900090316         DoW  wPos1 > *zero;
131000090316           if  %subst( V2Cdir : wPos1 : 2 ) = '//';
131100090316             errMessage  = *on;
131200090316             errGenerico = *on;
131300090316             PosCurDir   = *on;
131400090316             V1Dmsg = 'Nome directory NON valido: non ammesso il +
131500090316                       doppio "/"';
131600090316             leavesr;
131700090316           else;
131800090316             wPos1 = %scan('/' : V2Cdir : wPos1 + 1 );
131900090316           endif;
132000090316         EndDo;
132100090316
132200090309         wPos1 = %scan('&' : V2Cdir);
132300090309         IF  wPos1 > *zero;
132400090309           if  %subst( V2Cdir : wPos1 : 4 ) <> '&AAA'   and
132500090309               %subst( V2Cdir : wPos1 : 2 ) <> '&M';
132600090309             errMessage  = *on;
132700090309             errGenerico = *on;
132800090309             PosCurDir   = *on;
132900090309             V1Dmsg = 'Ammessi solo "&AAA" e "&M" (come caratteri +
133000090309                       sostituibili)';
133100090309             leavesr;
133200090309           endif;
133300090309           wPos2 = %scan( '&' : V2Cdir : wPos1 + 1 );
133400090309           If  wPos2 > *zero;
133500090309             if  %subst( V2Cdir : wPos2 : 4 ) <> '&AAA'   and
133600090309                 %subst( V2Cdir : wPos2 : 2 ) <> '&M';
133700090309               errMessage  = *on;
133800090309               errGenerico = *on;
133900090309               PosCurDir   = *on;
134000090309               V1Dmsg = 'Ammessi solo "&AAA" e "&M" (come caratteri +
134100090309                         sostituibili)';
134200090309               leavesr;
134300090309             endif;
134400090309           EndIf;
134500090309         ENDIF;
134600090309
134700090309         wPos1 = %scan('&M' : V2Cdir);
134800090309         IF  wPos1 > *zero;
134900090309           wPos2 = %scan( '&M' : V2Cdir : wPos1 + 1 );
135000090309           If  wPos2 > *zero   and
135100090309               %scan( '&M' : V2Cdir : wPos2 + 1 ) > *zero;
135200090309             errMessage  = *on;
135300090309             errGenerico = *on;
135400090309             PosCurDir   = *on;
135500090309             V1Dmsg = 'Ammessi max 2 "&M"';
135600090309             leavesr;
135700090309           EndIf;
135800090309         ENDIF;
135900090226
136000090327         // - Se codice padre <> da codice cliente
136100090306         //   la directory deve coincidere almeno fino alla 2ª subdir
136200090306         IF  V2Cccm <> V2Cksu   and   p_§LACdir <> V2Cdir;
136300090311
136400090311           // - Controllo 1ª directory
136500090311           wPos1 = %scan( '/' : %trim( V2Cdir ) );
136600090311           wPos2 = %scan( '/' : %trim( p_§LACdir ) );
136700090313           If  wPos1 <> wPos2                               OR
136800090313             ( wPos1  > *zero   and   wPos2 > *zero   and
136900090311               %subst( %trim( V2Cdir    ) : 1 : wPos1 ) <>
137000090313               %subst( %trim( p_§LACdir ) : 1 : wPos2 ) )   OR
137100090313             ( wPos1  = *zero   and   wPos2  =  *zero
137200090313                                and   V2Cdir <> p_§LACdir );
137300090311             errMessage  = *on;
137400090311             errGenerico = *on;
137500090311             PosCurDir   = *on;
137600090311             select;
137700090311               when  wPos1 =  *zero   and   wPos2 <> *zero;
137800090316                 V1Dmsg = 'Il padre ha altri rami directory (';
137900090311               when  wPos1 <> *zero   and   wPos2 =  *zero;
138000090311                 V1Dmsg = 'Il padre ha una directory unica (';
138100090311               other;
138200090311                 V1Dmsg = 'Primo ramo directory +
138300090311                           <> da quello del cod. padre (';
138400090311             EndSl;
138500090313             if  %len( %trim( p_§LACdir ) ) <=
138600090311                 %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) ) - 1;
138700090311               V1Dmsg = %trimr( V1Dmsg )
138800090311                      + %trim( p_§LACdir ) + ')';
138900090311             else;
139000090313               wPos1 = %len( V1Dmsg );
139100090313               wPos2 = %len( %trimr( V1Dmsg ) );
139200090311               V1Dmsg = %trimr( V1Dmsg )
139300090311                      + %trim( %subst( p_§LACdir : 1 :
139400090311                          %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) )
139500090311                          - 4 ) )
139600090311                      + '...)';
139700090311             endif;
139800090311             leavesr;
139900090311           EndIf;
140000090311
140100090311           // - Controllo 2ª directory
140200141201           wPos1 = %scan( '/' : %trim( V2Cdir ) : wPos1 + 1 );
140300141201           wPos2 = %scan( '/' : %trim( p_§LACdir ) : wPos2 + 1 );
140400141201           if  wPos1 = *zero;
140500141201             wPos1 = %len( %trimr( V2Cdir ) );
140600090316           else;
140700141201             wPos1 -= 1;
140800090316           endif;
140900141201           if  wPos2 = *zero;
141000141201             wPos2 = %len( %trimr( p_§LACdir ) );
141100090316           else;
141200090316             wPos2 -= 1;
141300090316           endif;
141400090316           If  wPos1 <> wPos2   OR
141500090316               %subst(V2Cdir : 1 : wPos1) <>
141600090316                 %subst(p_§LACdir : 1 : wPos2);
141700090311             errMessage  = *on;
141800090311             errGenerico = *on;
141900090311             PosCurDir   = *on;
142000090313                 V1Dmsg = 'Secondo ramo directory +
142100090311                           <> da quello del cod. padre (';
142200090313             if  %len( %trim( p_§LACdir ) ) <=
142300090311                 %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) ) - 1;
142400090311               V1Dmsg = %trimr( V1Dmsg )
142500090311                      + %trim( p_§LACdir ) + ')';
142600090311             else;
142700090311               V1Dmsg = %trimr( V1Dmsg )
142800090311                      + %trim( %subst( p_§LACdir : 1 :
142900090311                          %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) )
143000090311                          - 4 ) )
143100090311                      + '...)';
143200090311             endif;
143300090311             leavesr;
143400090311           EndIf;
143500090311
143600090306         ENDIF;
143700121016
143800121016         //?Immagine con Merge (LDV + DOC)
143900151217         //?e non sono in ripristino (devo prima ripristinare anche la JDC)
144000151217         IF  V2merge = 'S'
144100151217          and dsp_aid <> c_F05;
144200151217           exsr CTR_merge;
144300121016         ENDIF;
144400121016         IF  ErrGenerico;
144500121016           leavesr;
144600121016         ENDIF;
144700090327
144800090327         //?Tipo porto?
144900090327         if  V2Ctpt <> 'F'   and   V2Ctpt <> 'A'   and   V2Ctpt <> 'E';
145000090327           errMessage  = *on;
145100090327           errGenerico = *on;
145200090327           PosCurTpt   = *on;
145300090327           V1Dmsg = 'Tipo porto errato o mancante';
145400090327           leavesr;
145500090327         endif;
145600090311
145700090311         //?LNP per selezione?
145800090311         if  V2Clnp <> *zero;
145900090311           setll  (V2Clnp)  AZORG;
146000090311           if  NOT  %equal(AZORG01L);
146100090311             errMessage  = *on;
146200090311             errGenerico = *on;
146300090311             PosCurLnp   = *on;
146400090311             V1Dmsg = 'Linea di partenza errata';
146500090311             leavesr;
146600090311           endif;
146700090311         endif;
146800140813
146900140813         //?Tipo data da elaborare?
147000140813         select;
147100140813           when  V2tdta <> ' '  and  V2tdta <> 'C'  and  V2tdta <> 'S';
147200140813             errMessage  = *on;
147300140813             errGenerico = *on;
147400140813             PosCurTdta  = *on;
147500140813             V1Dmsg = '"Flag "Data da elaborare" errato';
147600140813             leavesr;
147700140813           // - Tipo data da elaborare obbligatorio
147800140813           when  V2Caut = 'S'   and   V2tdta = *blank;
147900140813             errMessage  = *on;
148000140813             errGenerico = *on;
148100140813             PosCurTdta  = *on;
148200140813             V1Dmsg = '"Data da elaborare" obbligatoria per +
148300140813                       "Tipo Elaborazione" = "S" (schedulata)';
148400140813             leavesr;
148500140813           // - Se codice padre <> da codice cliente
148600140813           //   il tipo data da elaborare deve essere uguale
148700140813           when  V2Cccm <> V2Cksu   and   p_§LACtdta <> V2tdta;
148800140813             errMessage  = *on;
148900140813             errGenerico = *on;
149000140813             PosCurTdta  = *on;
149100140813             V1Dmsg = 'Tipo data da elaborare +
149200140813                       <> da quello del cod. padre ('
149300140813                    + p_§LACtdta + ')';
149400140813             leavesr;
149500140813         endsl;
149600090309
149700090309         //?Periodo da estrarre in mesi?
149800090310         select;
149900090310           // - Periodo da estrarre in mesi obbligatorio
150000090310           when  V2Caut = 'S'   and   V2mesi = *zero;
150100090310             errMessage  = *on;
150200090310             errGenerico = *on;
150300090310             PosCurMesi  = *on;
150400090310             V1Dmsg = '"Periodo da estrarre" obbligatorio per +
150500090310                       "Tipo Elaborazione" = "S" (schedulata)';
150600090310             leavesr;
150700090310           // - Se codice padre <> da codice cliente
150800090310           //   il periodo da estrarre deve essere uguale
150900090310           when  V2Cccm <> V2Cksu   and   p_§LACmesi <> V2mesi;
151000090310             errMessage  = *on;
151100090310             errGenerico = *on;
151200090310             PosCurMesi  = *on;
151300090310             V1Dmsg = 'Periodo da estrarre +
151400090310                       <> da quello del cod. padre ('
151500090310                    + %editc(p_§LACmesi:'X') + ')';
151600090310             leavesr;
151700090310         endsl;
151800141201
151900141201         //?Prima estrazione dal?
152000141201         select;
152100141201           // - Obbligatoria se tipo elaborazione CD
152200141201           when  V2Caut = 'S'   and   V2datai = *zero;
152300141201             errMessage  = *on;
152400141201             errGenerico = *on;
152500141201             PosCurDatai = *on;
152600141201             V1Dmsg = 'Periodo iniziale della prima estrazione +
152700141201                       obbligatorio';
152800141201             leavesr;
152900141201           // - Verifica correttezza
153000141201           when  V2datai <> *zero                                  AND
153100141201              (%subst( %editc( V2datai : 'X' ) : 1 : 2 ) < '01'   or
153200141201               %subst( %editc( V2datai : 'X' ) : 1 : 2 ) > '12'   or
153300141201               %subst( %editc( V2datai : 'X' ) : 3 : 4 ) < '2000' or
153400141201               %subst( %editc( V2datai : 'X' ) : 3 : 4 ) > %char(*year));
153500141201             errMessage  = *on;
153600141201             errGenerico = *on;
153700141201             PosCurDatai = *on;
153800141201             V1Dmsg = 'Periodo iniziale della prima estrazione +
153900141201                       errato';
154000141201             leavesr;
154100141201         endsl;
154200141201         // - Se codice padre <> da codice cliente
154300141201         //   il periodo deve essere uguale
154400141201         if  V2Cccm <> V2Cksu;
154500141201           clear  wDataI;
154600141201           %subst( wDataI : 1 : 2 ) =
154700141201                   %subst( %editc( p_§LACdatai : 'X' ) : 5 : 2 );
154800141201           %subst( wDataI : 3 : 4 ) =
154900141201                   %subst( %editc( p_§LACdatai : 'X' ) : 1 : 4 );
155000141201           if  V2datai <> %int(wDataI);
155100141201             errMessage  = *on;
155200141201             errGenerico = *on;
155300141201             PosCurDatai = *on;
155400141201             V1Dmsg = 'Periodo iniziale della prima estrazione +
155500141201                       <> da quello del cod. padre ('
155600141201                    + %subst(%editc(p_§LACdatai : 'X') : 5 : 2)
155700141201                    + '/'
155800141201                    + %subst(%editc(p_§LACdatai : 'X') : 1 : 4)
155900141201                    + ')';
156000141201             leavesr;
156100141201           endif;
156200141201         endif;
156300090226
156400090309         //?Schedulazione giorno e mesi x preparazione CD?
156500090226         select;
156600090318           // when (V2Caut  =  'A'    or  V2Caut  =  'N')    AND
156700090318           when  V2Caut  =  'A'    AND
156800130923               // (V2datai <> *zero  or  V2tdta   <> *blank   or
156900130923                (V2mesi  <> *zero  or  V2Cschgg <> *zero    or
157000090311                 ds_SchMm <> *blank);
157100090311             errMessage  = *on;
157200090311             errGenerico = *on;
157300090311             PosCurAut   = *on;
157400090311             V1Dmsg = 'Schedulazione incompatibile con il +
157500090311                       "Tipo Elaborazione" = ' + V2Caut;
157600090311             leavesr;
157700090318           when  V2Caut = 'S'  and  (V2Cschgg <= *zero);
157800090306             errMessage  = *on;
157900090306             errGenerico = *on;
158000090306             PosCurSchGg = *on;
158100090306             V1Dmsg = 'Schedulazione obbligatoria per +
158200090306                       "Tipo Elaborazione" = "S" (schedulata)';
158300090306             leavesr;
158400090306           when  V2Caut = 'S'  and  (ds_SchMm = *blank);
158500090306             errMessage  = *on;
158600090306             errGenerico = *on;
158700090306             PosCurSch01 = *on;
158800090306             V1Dmsg = 'Indicare almeno un mese per +
158900090306                       "Tipo Elaborazione" = "S" (schedulata)';
159000090306             leavesr;
159100090311         endsl;
159200090226
159300090327         // - Se codice padre <> da codice cliente
159400090306         //   i 2 codici devono avere gli stessi dati di estrazione
159500090306         //   gg e mese e n. mesi da elaborare
159600090309         if  V2Cccm <> V2Cksu;
159700090309           select;
159800090309             when  p_§LACschgg <> V2Cschgg;
159900090226               errMessage  = *on;
160000090226               errGenerico = *on;
160100090303               PosCurSchgg = *on;
160200090309               V1Dmsg = 'Giorno creazione CD +
160300090309                         <> da quello del cod. padre ('
160400090309                      + %editc(p_§LACschgg:'X') + ')';
160500090226               leavesr;
160600090303             when  p_SchMm <> ds_SchMm;
160700090226               errMessage  = *on;
160800090226               errGenerico = *on;
160900090226               PosCursch01 = *on;
161000090309               V1Dmsg = 'Mesi di elaborazione +
161100090309                         <> da quelli del cod. padre ('
161200090309                      + p_SchMm + ')';
161300090226               leavesr;
161400090226           endsl;
161500090226         endif;
161600090309
161700090327         // - Verifica congruenza tra mese di schedulazione e
161800090309         //   mesi di estrazione
161900090309         select;
162000090311           // - Nessun dato inserito
162100090310           when  V2mesi   = *zero    and   V2Cschgg = *zero    and
162200090311                 ds_SchMm = *blank;
162300090311           // - Periodo da estrarre in mesi richiesto
162400090310           when  V2mesi = *zero;
162500090310             errMessage  = *on;
162600090310             errGenerico = *on;
162700090310             PosCurMesi  = *on;
162800090310             V1Dmsg = 'Periodo da estrarre obbligatorio SE inserita +
162900090310                       una schedulazione';
163000090310             leavesr;
163100090311           // - Giorno di schedulazione richiesto
163200090310           when  V2Cschgg = *zero;
163300090310             errMessage  = *on;
163400090310             errGenerico = *on;
163500090310             PosCurSchgg = *on;
163600090311             V1Dmsg = $Msg(08);
163700090310             leavesr;
163800090311           // - Selezione mesi richiesta
163900090311           when  ds_SchMm = *blank;
164000090310             errMessage  = *on;
164100090310             errGenerico = *on;
164200090310             PosCurSch01 = *on;
164300090311             V1Dmsg = $Msg(09);
164400090310             leavesr;
164500090311         endsl;
164600090313
164700090313         // - Conteggio/Controllo mesi schedulati
164800090313         clear  wCount;
164900090311         wSchMm_ds = %triml(ds_SchMm);
165000090313         For  xx = 1  To  %elem($SchMm);
165100090313           if  $SchMm(xx) <> *blank;
165200090313             wCount += 1;
165300090313           endif;
165400090313         EndFor;
165500090313
165600090311         select;
165700090313           // - Errori bloccanti:
165800090313           when   wCount <> *zero   and
165900090313                 (V2mesi <> (12 / wCount));
166000090313             errMessage  = *on;
166100090313             errGenerico = *on;
166200090313             PosCurSch01 = *on;
166300090313             V1Dmsg = 'Incongruenza tra il "periodo da estrarre" +
166400090313                       ed i mesi schedulati';
166500090313           // - Errori bypassabili:
166600090313           when ( (V2mesi =  2   and  ($SchMm(03) = *blank     or
166700090311                                       $SchMm(05) = *blank     or
166800090311                                       $SchMm(07) = *blank     or
166900090311                                       $SchMm(09) = *blank     or
167000090311                                       $SchMm(11) = *blank))   or
167100090313                  (V2mesi =  3   and  ($SchMm(04) = *blank     or
167200090311                                       $SchMm(07) = *blank     or
167300090311                                       $SchMm(10) = *blank))   or
167400090313                  (V2mesi =  4   and  ($SchMm(05) = *blank     or
167500090311                                       $SchMm(09) = *blank))   or
167600090313                  (V2mesi =  6   and   $SchMm(07) = *blank) )  AND
167700090313                 $ByPass =  *off;
167800090313             $ByPass = *on;
167900090309             errMessage  = *on;
168000090309             errGenerico = *on;
168100090310             PosCurSch01 = *on;
168200090313             V1Dmsg = 'Mesi schedulati in maniera irregolare... +
168300090313                       Premere Enter per forzare.';
168400090309             leavesr;
168500090313           when ( (V2mesi =  2  and  ($SchMm(02) <> *blank     or
168600090311                                      $SchMm(04) <> *blank     or
168700090311                                      $SchMm(06) <> *blank     or
168800090311                                      $SchMm(08) <> *blank     or
168900090311                                      $SchMm(10) <> *blank     or
169000090311                                      $SchMm(12) <> *blank))   OR
169100090313                  (V2mesi =  3  and  ($SchMm(02) <> *blank     or
169200090311                                      $SchMm(03) <> *blank     or
169300090311                                      $SchMm(05) <> *blank     or
169400090311                                      $SchMm(06) <> *blank     or
169500090311                                      $SchMm(08) <> *blank     or
169600090311                                      $SchMm(09) <> *blank     or
169700090311                                      $SchMm(11) <> *blank     or
169800090311                                      $SchMm(12) <> *blank))   OR
169900090313                  (V2mesi =  4  and  ($SchMm(02) <> *blank     or
170000090311                                      $SchMm(03) <> *blank     or
170100090311                                      $SchMm(04) <> *blank     or
170200090311                                      $SchMm(06) <> *blank     or
170300090311                                      $SchMm(07) <> *blank     or
170400090311                                      $SchMm(08) <> *blank     or
170500090311                                      $SchMm(10) <> *blank     or
170600090311                                      $SchMm(11) <> *blank     or
170700090311                                      $SchMm(12) <> *blank))   OR
170800090313                  (V2mesi =  6  and  ($SchMm(02) <> *blank     or
170900090311                                      $SchMm(03) <> *blank     or
171000090311                                      $SchMm(04) <> *blank     or
171100090311                                      $SchMm(05) <> *blank     or
171200090311                                      $SchMm(06) <> *blank     or
171300090311                                      $SchMm(08) <> *blank     or
171400090311                                      $SchMm(09) <> *blank     or
171500090311                                      $SchMm(10) <> *blank     or
171600090311                                      $SchMm(11) <> *blank     or
171700090311                                      $SchMm(12) <> *blank))   OR
171800090313                  (V2mesi = 12  and  ($SchMm(02) <> *blank     or
171900090311                                      $SchMm(03) <> *blank     or
172000090311                                      $SchMm(04) <> *blank     or
172100090311                                      $SchMm(05) <> *blank     or
172200090311                                      $SchMm(06) <> *blank     or
172300090311                                      $SchMm(07) <> *blank     or
172400090311                                      $SchMm(08) <> *blank     or
172500090311                                      $SchMm(09) <> *blank     or
172600090311                                      $SchMm(10) <> *blank     or
172700090311                                      $SchMm(11) <> *blank     or
172800090313                                      $SchMm(12) <> *blank)) ) AND
172900090313                 $ByPass =  *off;
173000090313             $ByPass = *on;
173100090310             errMessage  = *on;
173200090310             errGenerico = *on;
173300090311             PosCurSch01 = *on;
173400090313             V1Dmsg = 'Mesi schedulati in maniera irregolare... +
173500090313                       Premere Enter per forzare.';
173600090310             leavesr;
173700090313           // - Controllo giorno del mese (bypassabile)
173800090313           when ( (V2Cschgg > 28  and  V2Csch02 <> *blank)  or
173900090313                  (V2Cschgg > 30  and (V2Csch04 <> *blank
174000090313                                   or  V2Csch06 <> *blank
174100090313                                   or  V2Csch09 <> *blank
174200090313                                   or  V2Csch11 <> *blank)) )
174300090313                and  $CtrMm = *off;
174400090313             $CtrMm = *on;
174500090313             errMessage  = *on;
174600090313             errGenerico = *on;
174700090313             PosCurSch01 = (V2Csch01 <> *blank);
174800090313             PosCurSch02 = (V2Csch02 <> *blank);
174900090313             PosCurSch04 = (V2Csch04 <> *blank);
175000090313             PosCurSch06 = (V2Csch06 <> *blank);
175100090313             PosCurSch09 = (V2Csch09 <> *blank);
175200090313             PosCurSch11 = (V2Csch11 <> *blank);
175300090313             V1Dmsg = %replace( %char(V2Cschgg) : $Msg(10) :
175400090313                                %scan('&g':$Msg(10)) );
175500090313             leavesr;
175600090313         endsl;
175700090226
175800090309         //?Frequenza addebito?
175900090310         select;
176000090327           // - Obbligatoria
176100090327           when  V2tadu <> 'I'   and   V2tadu <> 'J'   and
176200090327                 V2tadu <> 'S'   and   V2tadu <> 'M';
176300090327             errMessage  = *on;
176400090327             errGenerico = *on;
176500090327             PosCurTadu  = *on;
176600090327             V1Dmsg = 'Frequenza addebito errata o mancante';
176700090327             leavesr;
176800090327           // - Se tipo elaborazione CD la creazione addebito non può
176900090313           //   essere mensile o settimanale
177000090310           when  V2Caut = 'S'  and  (V2tadu = 'S'  or  V2tadu = 'M');
177100090310             errMessage  = *on;
177200090310             errGenerico = *on;
177300090310             PosCurTadu  = *on;
177400090310             V1Dmsg = 'Per creazione CD non è possibile addebito +
177500090310                       mensile o settimanale';
177600090310             leavesr;
177700090327           // - Se codice padre <> da codice cliente
177800090327           //   devono avere la stessa frequenza addebito
177900090310           when  V2Cccm <> V2Cksu   and   p_§LACtadu <> V2tadu;
178000090310             errMessage  = *on;
178100090310             errGenerico = *on;
178200090310             PosCurTadu  = *on;
178300090310             V1Dmsg = 'Frequenza addebito +
178400090310                        <> da quella del cod. padre ('
178500090310                    + p_§LACtadu + ')';
178600090310             leavesr;
178700090310         endsl;
178800090309
178900090309         //?Tipo addebito (Creazione TITAS)?
179000090310         select;
179100090327           // - Obbligatorio
179200090327           when  V2Ctad <> 'N'  and  V2Ctad <> 'S'  and  V2Ctad <> 'V';
179300090327             errMessage  = *on;
179400090327             errGenerico = *on;
179500090327             PosCurTad   = *on;
179600090327             V1Dmsg = 'Flag "Creazione TITAS" errato o mancante';
179700090327             leavesr;
179800090327           // - Se "N" non impostare i dati dell'addebito forzato
179900090310           when  V2Ctad = 'N'     and
180000090310                (V2Cksc <> *zero  or  V2Cctr <> *blank
180100090310                                  or  V2imp <> *zero);
180200090310             errMessage  = *on;
180300090310             errGenerico = *on;
180400090310             PosCurTad   = *on;
180500090310             V1Dmsg = 'Se NO addebito non impostare le forzature';
180600090310             leavesr;
180700090327           // - Se codice padre <> da codice cliente
180800090310           //   devono avere lo stesso tipo addebito
180900090310           when  V2Cccm <> V2Cksu   and   p_§LACtad <> V2Ctad;
181000090310             errMessage  = *on;
181100090310             errGenerico = *on;
181200090310             PosCurTad   = *on;
181300090310             V1Dmsg = 'Tipo addebito +
181400090310                       <> da quello del cod. padre ('
181500090310                        + p_§LACtad + ')';
181600090310             leavesr;
181700090310         endsl;
181800090226
181900090311         //?Dati da forzare in tassazione:?
182000090312         //?- Codice cliente / Tariffa?
182100090312         clear  TIBS69ds;
182200090312         if  V2Cksc <> *zero;
182300090312           tibs69ds.I69kcc = DUTkci;
182400090312           tibs69ds.I69kac = %int(V2Cksc);
182500090312           tibs69ds.I69sif = knsif;
182600090312           tibs69r(TIBS69ds : ds_CNACO :
182700090312                              ds_CNIND :
182800090312                              ds_CNCLP :
182900090312                              ds_FNCLS);
183000090312         endif;
183100090312         select;
183200090312           when  tibs69ds.O69err = *on;
183300090312             errMessage  = *on;
183400090312             errGenerico = *on;
183500090312             PosCurKsc   = *on;
183600090312             V1Dmsg = 'Cliente tassazione errato';
183700090312             leavesr;
183800090312           when  V2Cksc <> *zero   and   V2Cctr = *blank;
183900090312             errMessage  = *on;
184000090312             errGenerico = *on;
184100090312             PosCurCtr   = *on;
184200090312             V1Dmsg = 'Codice tariffa obbligatorio SE +
184300090312                       inserito il cliente tassazione';
184400090312             leavesr;
184500090311           // - Non ammessa tariffa senza cliente
184600090311           when  V2Cksc =  *zero   and   V2Cctr <> *blank;
184700090311             errMessage  = *on;
184800090311             errGenerico = *on;
184900090311             PosCurKsc   = *on;
185000090311             V1Dmsg = 'Cliente tassazione obbligatorio SE +
185100090311                       inserita la relativa tariffa';
185200090311             leavesr;
185300090311           // - Ammesso solo un codice tariffa numerico
185400090311           when  V2Cctr <> *blank   and
185500090311                 %check(c_Digits : V2Cctr) > *zero;
185600090311             errMessage  = *on;
185700090311             errGenerico = *on;
185800090312             PosCurCtr   = *on;
185900090312             V1Dmsg = 'Ammessi SOLO caratteri numerici';
186000090311             leavesr;
186100090310         endsl;
186200090226
186300090327         // - Se codice padre <> da codice cliente
186400090306         //   stesso ksc/ctr/importo
186500090303         if  V2Cccm <> V2Cksu;
186600090226           select;
186700090303             when  p_§LACksc <> V2Cksc;
186800090226               errMessage  = *on;
186900090226               errGenerico = *on;
187000090303               PosCurKsc   = *on;
187100090309               V1Dmsg = 'Cliente tassazione +
187200090309                         <> da quello del cod. padre ('
187300090309                      + %editc(p_§LACksc : 'X') + ')';
187400090226               leavesr;
187500090303             when  p_§LACctr <> V2Cctr;
187600090226               errMessage  = *on;
187700090226               errGenerico = *on;
187800090303               PosCurCtr   = *on;
187900090309               V1Dmsg = 'Codice tariffa +
188000090309                         <> quello del cod. padre ('
188100090309                      + p_§LACctr + ')';
188200090226               leavesr;
188300090226           endsl;
188400090226         endif;
188500090226
188600090309         //?Importo & Tipo importo forzato?
188700090310         select;
188800090310           // - Non ammesso importo per Varia Negata
188900090310           when  V2Ctad = 'V'   and   V2imp <> *zero;
189000090310             errMessage  = *on;
189100090310             errGenerico = *on;
189200090310             PosCurImp   = *on;
189300090310             V1Dmsg = 'NON ammesso l''importo forzato per +
189400090310                       Varia Negata';
189500090310             leavesr;
189600090310           // - Non ammesso tipo importo senza importo
189700090310           when  V2imp =  *zero   and   V2fimp <> *blank;
189800090310             errMessage  = *on;
189900090310             errGenerico = *on;
190000090310             PosCurImp   = *on;
190100090310             V1Dmsg = 'Importo forzato obbligatorio SE +
190200090310                       inserito il tipo di importo';
190300090310             leavesr;
190400090312           // - Se inserito importo devono essere inseriti:
190500090312           //   · il codice cliente forzato (ksc),
190600090312           //   · la tariffa forzata (ctr) ed
190700090312           //   · il tipo importo (fimp)
190800090310           when  V2imp <> *zero;
190900090310             select;
191000090310               when  V2fimp  = *blank;
191100090310                 errMessage  = *on;
191200090310                 errGenerico = *on;
191300090310                 PosCurFimp  = *on;
191400090310                 V1Dmsg = 'Immettere il tipo importo';
191500090310                 leavesr;
191600090310               when  V2Cksc  = *zero;
191700090310                 errMessage  = *on;
191800090310                 errGenerico = *on;
191900090310                 PosCurKsc   = *on;
192000090310                 V1Dmsg = 'Immettere il cliente forzato';
192100090310                 leavesr;
192200090312               when  V2Cctr  = *blank;
192300090310                 errMessage  = *on;
192400090310                 errGenerico = *on;
192500090310                 PosCurCtr   = *on;
192600090310                 V1Dmsg = 'Immettere la tariffa forzata';
192700090310                 leavesr;
192800090310             endsl;
192900090310         endsl;
193000090312
193100090327         // Verifica compatibilità dell'importo forzato con il
193200090312         //   massimo imponibile IVA in tab. "GEI"/"LAC".
193300090312         if  V2imp <> *zero   and   V2fimp = 'T';
193400090312           // - Reperimento divisa moneta di conto gestionale
193500090312           clear  dGED;
193600090312           reset  TIBS02ds;
193700090312           T02cod = 'GED';
193800090312           T02ke1 = '1';
193900090312           TNTBE_RicercaControllo(kpjba : tibs02ds);
194000090312           if  T02err = *blank;
194100090312             dGED = T02uni;
194200090312           endif;
194300090312           // - Reperimento importi da tab. "GEI"
194400090312           //   (interessa il massimo imponibile IVA per spedizione)
194500090312           clear  dGEI;
194600090312           reset  TIBS02ds;
194700090312           T02cod = 'GEI';
194800090312           T02ke1 = §GEDcn;
194900090312           TNTBE_RicercaControllo(kpjba : tibs02ds);
195000090312           if  T02err = *blank;
195100090312             dGEI = T02uni;
195200090312           endif;
195300090312           if  V2imp > §GEImf;
195400090312             errMessage  = *on;
195500090312             errGenerico = *on;
195600090312             PosCurImp   = *on;
195700090312             V1Dmsg = 'Importo superiore al MASSIMO IMPONIBILE IVA +
195800090312                       PER SPEDIZIONE ('
195900090312                      + %trim( %editc( §GEImf : 'J' ) ) + ')';
196000090312             leavesr;
196100090312           endif;
196200090312         endif;
196300090226
196400090312         // Se codice padre <> da codice cliente
196500090306         //   stesso importo e tipo importo
196600090303         if  V2Cccm <> V2Cksu;
196700090226           select;
196800090303             when  p_§LACimp <> V2imp;
196900090226               errMessage  = *on;
197000090226               errGenerico = *on;
197100090312               PosCurImp   = *on;
197200090309               V1Dmsg = 'Importo forzato +
197300090309                         <> da quello del cod. padre ('
197400090309                      + %editc(p_§LACimp : 'J') + ')';
197500090226               leavesr;
197600090303             when  p_§LACfimp <> V2fimp;
197700090226               errMessage  = *on;
197800090226               errGenerico = *on;
197900090312               PosCurFimp  = *on;
198000090309               V1Dmsg = 'Tipo importo forzato +
198100090309                         <> da quello del cod. padre ('
198200090309                      + p_§LACfimp + ')';
198300090226               leavesr;
198400090226           endsl;
198500090225         endif;
198600090923
198700090923         //?Modifica dati sensibili per l'addebito
198800090923         if (v2tadu <> savtadu or v2ctad <> savtad or
198900090923             v2cksc <> savksc or v2cctr <> savctr or
199000141128             v2imp <> savimp or v2fimp <> savfimp) and
199100141128             (not *in02 and not $Copia)
199200090923             and not $forza;
199300090923               errMessage  = *on;
199400090923               errGenerico = *on;
199500090923               PosCurTadu  = *on;
199600090923               V1Dmsg = 'ATTENZIONE!! Possibili problemi ' +
199700090923                        'in fase di addebito. Avvisare Monica/Nicola';
199800090923               $forza = *on;
199900090923               leavesr;
200000090923         endif;
200100110713
200200110713         //?Creazione file elenco per il cliente
200300110713         //?possibile solo se NON è elaborazione automatica (FTP)
200400110713         IF  V2pgm <> *blanks and V2caut = 'A';
200500110713           ErrMessage  = *on;
200600110713           ErrGenerico = *on;
200700110713           PosCurPgm   = *on;
200800110713           V1Dmsg = 'NON possibile creazione elenco se elaborazione '+
200900110713                    'automatica';
201000110713           leavesr;
201100110713         ENDIF;
201200110713         //?se inserito il nome del programma deve esserci anche il tipo elenco
201300110713         IF  V2pgm <> *blanks and V2ele = *blanks;
201400110713           ErrMessage  = *on;
201500110713           ErrGenerico = *on;
201600110713           PosCurEle   = *on;
201700110713           V1Dmsg = 'Immettere anche il tipo elenco da creare';
201800110713           leavesr;
201900110713         ENDIF;
202000110713         //?e viceversa se c'è il tipo elenco deve esserci anche il nome del pgm
202100110713         IF  V2pgm = *blanks and V2ele <> *blanks;
202200110713           ErrMessage  = *on;
202300110713           ErrGenerico = *on;
202400110713           PosCurPgm   = *on;
202500110713           V1Dmsg = 'Immettere anche il nome del programma x elenco da creare';
202600110713           leavesr;
202700110713         ENDIF;
202800110713         //?Se codice padre <> da codice cliente
202900110713         //?  devono avere gli stessi dati
203000110713         IF  V2Cccm <> V2Cksu;
203100110713           SELECT;
203200110713             WHEN  p_§LACpgm <> V2pgm;
203300110713               ErrMessage  = *on;
203400110713               ErrGenerico = *on;
203500110713               PosCurPgm   = *on;
203600110713               V1Dmsg = 'Nome programma +
203700110713                         <> da quello del cod. padre ('
203800110713                      + p_§LACpgm + ')';
203900110713               leavesr;
204000110713             WHEN  p_§LACele <> V2ele;
204100110713               ErrMessage  = *on;
204200110713               ErrGenerico = *on;
204300110713               PosCurEle   = *on;
204400110713               V1Dmsg = 'Tipo elenco +
204500110713                         <> da quello del cod. padre ('
204600110713                      + p_§LACele + ')';
204700110713               leavesr;
204800110713           ENDSL;
204900110713         ENDIF;
205000141128
205100141128         //?Competenze C/E dichiarate
205200141128         IF  (*in02 or $copia) and V2compce = *blanks;
205300141128           ErrGenerico = *on;
205400141128           ErrMessage  = *on;
205500141128           PosCurCompCE = *on;
205600141128           V1Dmsg = 'Competenze C/E obbligatorie';
205700141128           leavesr;
205800141128         ENDIF;
205900141201         IF  V2compce <> *blanks;
206000141128         clear  V2dcompce;
206100141128         IF  %scan('?' : V2compce) > *zero;
206200141128           clear  TIBS02ds;
206300141128           T02mod = 'R';
206400141128           T02cod = 'CED';
206500141128           T02sif = KNSIF;
206600141128           TNTBE_RicercaControllo(kpjba : tibs02ds);
206700141128           IF  T02err = *blank;
206800141128             V2compce  = T02ke1;
206900141128             V2dcompce = T02uni;
207000141128             errGenerico = *on;
207100141128             leavesr;
207200141128           ELSE;
207300141128             errMessage  = *on;
207400141128             errGenerico = *on;
207500141128             PosCurCompCE = *on;
207600141128             V1Dmsg = T02msg;
207700141128             leavesr;
207800141128           ENDIF;
207900141128         ELSE;
208000141128           reset  TIBS02ds;
208100141128           T02cod = 'CED';
208200141128           T02ke1 = V2compce;
208300141128           T02sif = KNSIF;
208400141128           TNTBE_RicercaControllo(kpjba : tibs02ds);
208500141128           IF  T02err = *blank;
208600141128             V2dcompce = T02uni;
208700141128           ELSE;
208800141128             ErrMessage  = *on;
208900141128             ErrGenerico = *on;
209000141128             PosCurCompCE = *on;
209100141128             V1Dmsg = 'Competenze C/E errate';
209200141128             leavesr;
209300141128           ENDIF;
209400141128         ENDIF;
209500141202         ELSE;
209600141202           V2dcompce = 'NON DICHIARATO';
209700141201         ENDIF;
209800141128         //?Se codice padre <> da codice cliente
209900141128         //?  devono avere gli stessi dati
210000141128         //?  ma solo se sono in immissione/copia
210100141201         //?  e se il padre ha la personalizzazione impostata
210200141128         IF  V2Cccm <> V2Cksu and
210300141201             p_§LACcompce <> V2compce and (*in02 or $copia)
210400141201             and p_§LACcompce <> *blanks;
210500141128           ErrMessage   = *on;
210600141128           ErrGenerico  = *on;
210700141128           PosCurCompCE = *on;
210800141128           V1Dmsg = 'Competenze C/E +
210900141128                     <> da quelle del cod. padre ('
211000141128                   + p_§LACcompce + ')';
211100141128           leavesr;
211200141128         ENDIF;
211300141128
211400141128         //?Tipo Addebito dichiarato
211500141128         clear  V2dtpadd;
211600141128         IF  %scan('?' : V2tpadd) > *zero;
211700141128           clear  TIBS02ds;
211800141128           clear dTAL;
211900141128           T02mod = 'R';
212000141128           T02cod = 'TAL';
212100141128           T02sif = KNSIF;
212200141128           TNTBE_RicercaControllo(kpjba : tibs02ds);
212300141128           IF  T02err = *blank;
212400141128             V2tpadd  = T02ke1;
212500141128             dTAL = T02uni;
212600141128             V2dtpadd = §TALdes;
212700141128             errGenerico = *on;
212800141128             leavesr;
212900141128           ELSE;
213000141128             errMessage  = *on;
213100141128             errGenerico = *on;
213200141128             PosCurTpAdd = *on;
213300141128             V1Dmsg = T02msg;
213400141128             leavesr;
213500141128           ENDIF;
213600141128         ELSE;
213700141202         IF  V2tpadd = *blanks;
213800141202           V2dtpadd = 'NON DICHIARATO';
213900141202         ELSE;
214000141128           reset  TIBS02ds;
214100141128           clear dTAL;
214200141128           T02cod = 'TAL';
214300141128           T02ke1 = V2tpadd;
214400141128           T02sif = KNSIF;
214500141128           TNTBE_RicercaControllo(kpjba : tibs02ds);
214600141128           IF  T02err = *blank;
214700141128             dTAL = T02uni;
214800141128             V2dtpadd = §TALdes;
214900141128           ELSE;
215000141128             ErrMessage  = *on;
215100141128             ErrGenerico = *on;
215200141128             PosCurTpAdd = *on;
215300141128             V1Dmsg = 'Tipo Addebito errato';
215400141128             leavesr;
215500141128           ENDIF;
215600141202         ENDIF;
215700141128         ENDIF;
215800141128         //?Se codice padre <> da codice cliente
215900141128         //?  devono avere gli stessi dati
216000141128         //?  ma solo se sono in immissione/copia
216100141201         //?  e se il padre ha il campo impostato
216200141128         IF  V2Cccm <> V2Cksu and
216300141201             p_§LACtpadd <> V2tpadd and (*in02 or $copia)
216400141201             and p_§LACtpadd <> *blanks;
216500141128           ErrMessage  = *on;
216600141128           ErrGenerico = *on;
216700141128           PosCurTpAdd = *on;
216800141128           V1Dmsg = 'Tipo addebito +
216900141128                     <> da quello del cod. padre ('
217000141128                   + p_§LACtpadd + ')';
217100141128           leavesr;
217200141128         ENDIF;
217300141203
217400141203         //?Data Aut. DG
217500141203         IF  V2dtautdg <> *zeros;
217600141203           exec sql
217700141203           select count(*) into :conta
217800141203           from DATECNV0F
217900141203           where DC_DMYY_Z = :V2dtautdg or DC_DMY_Z = :V2dtautdg;
218000141203           IF  conta = 0;
218100141203             ErrMessage  = *on;
218200141203             ErrGenerico = *on;
218300141203             PosCurAutDg = *on;
218400141203             V1Dmsg = $Msg(15);
218500141203             leavesr;
218600141203           ENDIF;
218700141203           IF  V2dtautdg < 1000000;
218800141203             exec sql
218900141203             set :V2dtautdg = (select DC_DMYY_Z from DATECNV0F
219000141203                               where DC_DMY_Z = :V2dtautdg);
219100141203           ENDIF;
219200141203         ENDIF;
219300071207
219400071207       ENDSR;
219500090225
219600090225       //--------------------------------------------------------------
219700090306       //?Controllo esistenza del codice padre in tabella LAC          ?
219800090225       //--------------------------------------------------------------
219900090303       BEGSR  sr_Padre;
220000090302
220100090327         // Controlla esistenza in tabella "LAC"
220200090310         clear  dLACp;
220300090310         reset  TIBS02ds;
220400090310         T02sif = KNSIF;
220500090310         %subst(T02ke1 : 1 : 7) = %editc(V2Cksu : 'X');
220600090310         TNTBE_RicercaControllo(kpjba : tibs02ds);
220700090310         if  T02err = *blank;
220800090310           dLACp  = T02uni;
220900090310         else;
221000090310           errMessage  = *on;
221100090310           errGenerico = *on;
221200090310           PosCurksu   = *on;
221300090310           V1Dmsg = $Msg(12);
221400090310           leavesr;
221500090310         endif;
221600090327
221700090327         // Controlla se già inserito come figlio di altro padre
221800090327         if  p_§LACksu <> V2Cksu;
221900090327           errMessage  = *on;
222000090327           errGenerico = *on;
222100090327           PosCurKsu   = *on;
222200090327           V1Dmsg = 'Non ammesso cod. PADRE già FIGLIO di altro +
222300090327                     cliente (' + %editc(p_§LACksu : 'X') + ')';
222400090327           leavesr;
222500090327         endif;
222600090225
222700090225       ENDSR;
222800121016
222900121016       //--------------------------------------------------------------
223000121016       //?Controllo Merge.
223100121016       //--------------------------------------------------------------
223200121016       BEGSR  CTR_merge;
223300121016
223400121016       // -?L'immagine se con Merge deve essere di tipo PDF
223500121016         IF  V2Ctpi <> 'PDF';
223600121016           errMessage  = *on;
223700121016           errGenerico = *on;
223800121016           V1Dmsg = 'Immagine con Merge, deve essere di tipo PDF';
223900121016           PosCurTpi = *on;
224000121016           leavesr;
224100121016         ENDIF;
224200121016
224300121016       // -?La directory deve finire con /PF
224400121016         wPos1 = %scan('  ':V2Cdir);
224500121016         wPos1 -= 3;
224600121016         IF  %subst(V2Cdir:wPos1:3) <> '/PF';
224700121016           ErrMessage  = *on;
224800121016           ErrGenerico = *on;
224900121016           PosCurDir   = *on;
225000121016           V1Dmsg = 'Immagine con Merge, l''ultimo ramo della +
225100121016                     DIR deve essere "PF"';
225200121016           leavesr;
225300121016         ENDIF;
225400121016
225500121016       // -?Cerco il corrispondente in JDC
225600121016         clear  TIBS02DS;
225700121016         T02mod = 'C';
225800121016         T02cod = 'JDC';
225900121016         T02ke1 = %editc(V2Cccm:'X');
226000121016         TNTBE_RicercaControllo(kpjba : tibs02ds);
226100121016         // -?Non trovo la tabella
226200121016         IF  T02err <> *blanks;
226300170307           // -?Immissione o copia forzo
226400170307           IF  (*in02 or $copia) and not wForza;
226500121016             wForza = *on;
226600121016             ErrMessage  = *on;
226700121016             ErrGenerico = *on;
226800151130             V1Dmsg = 'Attivare anche la corrispondente +
226900151130                       tabella JDC. Enter per forzare';
227000121016             leavesr;
227100121016           ENDIF;
227200121016         // -?Manutenzione blocco
227300170307           IF  not *in02 and not $copia;
227400121016             ErrMessage  = *on;
227500121016             ErrGenerico = *on;
227600121016             V1Dmsg = 'Manca la corrispondente tabella JDC';
227700121016             leavesr;
227800121016           ENDIF;
227900121016         // -?Trovo la tabella
228000121016         ELSE;
228100121016           dJDC = T02uni;
228200121016         // -?Controllo il tipo immagine
228300121030           IF  V2Ctpi <> §JDCtpi and not wForzaTPI;
228400121030             wForzaTPI = *on;
228500121016             ErrMessage  = *on;
228600121016             ErrGenerico = *on;
228700121016             PosCurTpi   = *on;
228800151130             V1Dmsg = 'Tipo immagine diverso da corrispondente +
228900151130                       tab.JDC --> ' + §JDCtpi + ' Enter per forzare.';
229000121016             leavesr;
229100121016           ENDIF;
229200121016         // -?Controllo il nome immagine
229300121030           IF  V2Cfmi <> §JDCfmi and not wForzaFMI;
229400121030             wForzaFMI = *on;
229500121016             ErrMessage  = *on;
229600121016             ErrGenerico = *on;
229700121016             PosCurFmi   = *on;
229800151130             V1Dmsg = 'Nome immagine diverso da corrispondente +
229900151130                       tab.JDC --> ' + §JDCfmi + ' Enter per forzare.';
230000121016             leavesr;
230100121016           ENDIF;
230200121016         // -?Controllo la directory
230300121016           wPos2 = %scan('  ':§JDCdir);
230400121016           wPos2 -= 3;
230500121016           IF  %subst(V2Cdir:1:wPos1) <>
230600121030               %subst(§JDCdir:1:wPos2) and not wForzaDIR;
230700121030             wForzaDIR = *on;
230800121016             ErrMessage  = *on;
230900121016             ErrGenerico = *on;
231000121016             PosCurDir   = *on;
231100151130             V1Dmsg = 'Directory diversa da corrispondente +
231200151130                       tab.JDC --> ' + %subst(§JDCdir:1:wPos2) +
231300151201                       ' Enter per forzare.';
231400121016             leavesr;
231500121016           ENDIF;
231600121016         ENDIF;
231700121016
231800121016       ENDSR;
231900101103
232000101103       //--------------------------------------------------------------
232100101103       //?Gestione window W01                                          ?
232200101103       //--------------------------------------------------------------
232300101103       BEGSR  sr_GesW01;
232400101103
232500101103         // -?Emissione window?
232600101103         exfmt  TB46W01;
232700101103
232800101103         errMessage  = *off;
232900101103         errGenerico = *off;
233000101103         clear  V1Dmsg;
233100101103
233200101103         $Video = 'D2';
233300101103
233400101103         select;
233500101103           // -?F12=Ritorno?
233600101103           when  dsp_aid = c_F12;
233700101103             leavesr;
233800101103           // -?Invio con "S"?
233900101103           when  W1Ccpy = 'S';
234000101103             if  V2Caut = *blank;
234100101103               V2Caut   = p_§LACaut;
234200101103             endif;
234300101103             if  V2Ctpi = *blank;
234400101103               V2Ctpi   = p_§LACtpi;
234500101103             endif;
234600101103             if  V2Cfmi = *blank;
234700101103               V2Cfmi   = p_§LACfmi;
234800101103               clear  V2Dfmi;
234900101103             endif;
235000121016             if  V2merge = *blank;
235100121016               V2merge  = p_§LACmerge;
235200121016             endif;
235300121217             if  V2Cla2 = *blank;
235400121217               V2Cla2 = p_§LACla2;
235500121217             endif;
235600101103             //if  V2Cdir = *blank;      //? Directory ?
235700101103               V2Cdir   = p_§LACdir;     //? COMUNQUE  ?
235800101103             //endif;                    //? del padre ?
235900101103             if  V2Ctpt = *blank;
236000101103               V2Ctpt   = p_§LACtpt;
236100101103             endif;
236200101103             if  V2Cssr = *blank;
236300101103               V2Cssr   = p_§LACssr;
236400101103             endif;
236500101103             if  V2Clnp = *zero;
236600101103               V2Clnp   = p_§LAClnp;
236700101103             endif;
236800101103             if  V2Cres = *blank;
236900101103               V2Cres   = p_§LACres;
237000101103             endif;
237100101103             if  V2Crec = *blank;
237200101103               V2Crec   = p_§LACrec;
237300101103             endif;
237400101103             if  V2Ccsr = *blank;
237500101103               V2Ccsr   = p_§LACcsr;
237600101103             endif;
237700101103             if  V2tdta = *blank;
237800101103               V2tdta   = p_§LACtdta;
237900101103             endif;
238000101103             if  V2mesi = *zero;
238100101103               V2mesi   = p_§LACmesi;
238200101103             endif;
238300101103             if  V2Cschgg = *zero;
238400101103               V2Cschgg = p_§LACschgg;
238500101103             endif;
238600101103             if  ds_SchMm = *blank;
238700101103               ds_SchMm = p_SchMm;
238800101103             endif;
238900101103             if  V2tadu = *blank;
239000101103               V2tadu   = p_§LACtadu;
239100101103             endif;
239200101103             if  V2Ctad = *blank;
239300101103               V2Ctad   = p_§LACtad;
239400101103             endif;
239500101103             if  V2Cksc = *zero;
239600101103               V2Cksc   = p_§LACksc;
239700101103             endif;
239800101103             if  V2Cctr = *blank;
239900101103               V2Cctr   = p_§LACctr;
240000101103             endif;
240100101103             if  V2imp  = *zero;
240200101103               V2imp    = p_§LACimp;
240300101103             endif;
240400101103             if  V2fimp = *blank;
240500101103               V2fimp   = p_§LACfimp;
240600101103             endif;
240700110713             if  V2pgm  = *blank;
240800110713               V2pgm    = p_§LACpgm;
240900110713             endif;
241000110713             if  V2ele  = *blank;
241100110713               V2ele    = p_§LACele;
241200110713             endif;
241300101103             if  V2note = *blank;
241400101103               V2note   = p_§LACnote;
241500101103             endif;
241600140813             if  V2tpadd = *blank;
241700140813               V2tpadd  = p_§LACtpadd;
241800140813             endif;
241900140813             if  V2compce = *blank;
242000140813               V2compce = p_§LACcompce;
242100140813             endif;
242200101103         endsl;
242300101103
242400101103       ENDSR;
242500121212
242600121212       //--------------------------------------------------------------
242700121217       //?Gestione estensione tabella --> LA2.                       ?
242800121212       //--------------------------------------------------------------
242900121217       BEGSR  GesLA2;
243000121212
243100121217         TNTB464ds = TNTB46ds;
243200121217         B464ke1 = %editc(V2Cccm:'X');
243300121217         clear B464fxx;
243400121217         clear B464err;
243500121217         clear B46msg;
243600121217         tntb464r (kpjba:TNTB464DS);
243700121212
243800121212       ENDSR;
243900151217
244000151217       //--------------------------------------------------------------
244100151217       //?Gestione estensione tabella --> JDC.                       ?
244200151217       //--------------------------------------------------------------
244300151217       BEGSR  GesJDC;
244400151217
244500151217         TNTBJDCds = TNTB46ds;
244600151217         kpjbu = TNTBJDCds;
244700151217         BJDCke1 = %editc(V2Cccm:'X');
244800151217         clear BJDCfxx;
244900151217         clear BJDCerr;
245000151217         clear B46msg;
245100151217         tntbJDCr (kpjba);
245200151217
245300151217       ENDSR;
245400071206
245500071212       //--------------------------------------------------------------
245600090306       //?Aggiornamento record TNTBE00F (tab. LAC)                     ?
245700071212       //--------------------------------------------------------------
245800090306       BEGSR  Upd_TNTBE;
245900071207
246000090306         clear  dLAC;
246100071207         §LACrag   = V2Dccm;
246200071207         §LACtpt   = V2Ctpt;
246300071207         §LACres   = V2Cres;
246400071207         §LACrec   = V2Crec;
246500071207         §LACcsr   = V2Ccsr;
246600071207         §LACssr   = V2Cssr;
246700071207         §LACdir   = V2Cdir;
246800071207         §LACtpi   = V2Ctpi;
246900071207         §LACfmi   = V2Cfmi;
247000121016         §LACmerge = V2merge;
247100121217         §LACla2   = V2Cla2;
247200071207         §LACaut   = V2Caut;
247300161025         §LACFiD   = V2CFid;
247400161025         §LACCpy   = V2CCpy;
247500161025         §LACDst   = V2CDst;
247600090309         §LACtdta  = V2tdta;
247700071207         §LACtad   = V2Ctad;
247800071207         §LACschgg = V2Cschgg;
247900071207         §LACsch01 = V2Csch01;
248000071207         §LACsch02 = V2Csch02;
248100071207         §LACsch03 = V2Csch03;
248200071207         §LACsch04 = V2Csch04;
248300071207         §LACsch05 = V2Csch05;
248400071207         §LACsch06 = V2Csch06;
248500071207         §LACsch07 = V2Csch07;
248600071207         §LACsch08 = V2Csch08;
248700071207         §LACsch09 = V2Csch09;
248800071207         §LACsch10 = V2Csch10;
248900071207         §LACsch11 = V2Csch11;
249000071207         §LACsch12 = V2Csch12;
249100090226         §LACnote  = V2note;
249200090309         §LAClnp   = V2Clnp;
249300090309         §LACksu   = V2Cksu;
249400090309         §LACmesi  = V2mesi;
249500130923         IF  V2caut = 'A';
249600130923           clear §LACdatai;
249700130923           clear §LACdatela;
249800130923         ELSE;
249900090313         §LACdatai = (%int( %subst( %editc( V2datai : 'X') : 3 : 4 ) )
250000090313                      * 100)
250100090313                   + %int( %subst( %editc( V2datai : 'X') : 1 : 2 ) );
250200090310         §LACdatela...
250300090313                   = (%int( %subst( %editc( V2datela : 'X') : 3 : 4 ) )
250400090319                      * 100)
250500090313                    + %int( %subst( %editc( V2datela : 'X') : 1 : 2 ) );
250600130923         ENDIF;
250700090309         §LACtadu  = V2tadu;
250800090309         §LACksc   = V2Cksc;
250900090309         §LACctr   = V2Cctr;
251000090309         §LACfimp  = V2fimp;
251100090309         §LACimp   = V2imp;
251200110713         §LACpgm   = V2pgm;
251300110713         §LACele   = V2ele;
251400140813         §LACtpadd = V2tpadd;
251500140813         §LACcompce= V2compce;
251600140813         §LACutente= knmus;
251700150121         IF  *in02 or $copia;
251800140813           §LACdtaimm= %editc(woggi:'X');
251900141210         ELSE;
252000141210           exec sql
252100141210           set :§LACdtaimm = (select DC_YYMD_C from DATECNV0F
252200141210                              where DC_DMYY_A1 = :V2dtaimm);
252300140813         ENDIF;
252400141203         IF V2dtautdg <> *zeros;
252500141203           exec sql
252600141203           set :§LACdtaudg = (select DC_YYMD_C from DATECNV0F
252700141203                              where DC_DMYY_Z = :V2dtautdg);
252800141203         ENDIF;
252900071207
253000090309         TBEuni    = dLAC;
253100071207
253200090306         clear  TBEftt;
253300090306         clear  TBEftr;
253400071207
253500071207         select;
253600090306           // F5=Ripristino
253700090306           when  dsp_aid = c_F05;
253800090306             clear  TBEatb;
253900090306           // F16=Annullamento
254000090306           when  dsp_aid = c_F16;
254100071207             TBEatb = 'A';
254200071207         endsl;
254300121220
254400121220         TBEdtr = woggi;
254500071207
254600071207         IF  NOT  %found(TNTBE01L);
254700090306           clear  TBEatb;
254800090306           clear  TBEflt;
254900090306           clear  TBEftr;
255000090306           TBEsif = TBXsif;
255100090306           TBElin = TBXlin;
255200090306           TBEapl = TBXapl;
255300090306           TBEcod = k_TBEcod;
255400090306           TBEke1 = k_TBEke1;
255500090306           TBEke2 = k_TBEke2;
255600090306           //_____________
255700071207           WRITE TNTBE000;
255800090306           //¯¯¯¯¯¯¯¯¯¯¯¯¯
255900071207         ELSE;
256000090306           //______________
256100071207           UPDATE TNTBE000;
256200090306           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
256300071207         ENDIF;
256400121213
256500121213
256600121213         // -?Porto a pari anche la tabella LA2
256700121213         SELECT;
256800121213           //?F5=Ripristino tabella
256900121213           WHEN  dsp_aid = c_F05;
257000121213             //?Ripristino anche la LA2 se esiste
257100121213             k_TBEcod = 'LA2';
257200121213             k_TBEke1 = %editc(V2Cccm:'X');
257300121213             clear k_TBEke2;
257400121213             k_TBElin = TBXlin;
257500121213             k_TBEsif = TBXsif;
257600121213             chain  %kds(K05tntbe01) TNTBE01L;
257700121213             IF  %found(TNTBE01L);
257800121213               clear TBEatb;
257900121213               UPDATE  TNTBE000;
258000121213             ENDIF;
258100151217             //?Ripristino anche la JDC se esiste
258200151217             k_TBEcod = 'JDC';
258300151217             k_TBEke1 = %editc(V2Cccm:'X');
258400151217             clear k_TBEke2;
258500151217             k_TBElin = TBXlin;
258600151217             k_TBEsif = TBXsif;
258700151217             chain  %kds(K05tntbe01) TNTBE01L;
258800151217             IF  %found(TNTBE01L);
258900151217               clear TBEatb;
259000151217               UPDATE  TNTBE000;
259100151217             ENDIF;
259200121213           WHEN  dsp_aid = c_F16;
259300121213             //?Annullo anche la LA2 se esiste
259400121213             k_TBEcod = 'LA2';
259500121213             k_TBEke1 = %editc(V2Cccm:'X');
259600121213             clear k_TBEke2;
259700121213             k_TBElin = TBXlin;
259800121213             k_TBEsif = TBXsif;
259900121213             chain  %kds(K05tntbe01) TNTBE01L;
260000121213             IF  %found(TNTBE01L);
260100121213               TBEatb = 'A';
260200121213               UPDATE  TNTBE000;
260300121213             ENDIF;
260400151217             //?Annullo anche la JDC se esiste
260500151217             k_TBEcod = 'JDC';
260600151217             k_TBEke1 = %editc(V2Cccm:'X');
260700151217             clear k_TBEke2;
260800151217             k_TBElin = TBXlin;
260900151217             k_TBEsif = TBXsif;
261000151217             chain  %kds(K05tntbe01) TNTBE01L;
261100151217             IF  %found(TNTBE01L);
261200151217               TBEatb = 'A';
261300151217               UPDATE  TNTBE000;
261400151217             ENDIF;
261500121213           OTHER;
261600121213           //?Modifica
261700121217           //?se viene tolto il flag si altra direcotry controllo se esiste rcd LA2
261800121213           //?se esiste cancello
261900121217             IF  §LACla2 = *blanks;
262000121213               k_TBEcod = 'LA2';
262100121213               k_TBEke1 = %editc(V2Cccm:'X');
262200121213               clear k_TBEke2;
262300121213               k_TBElin = TBXlin;
262400121213               k_TBEsif = TBXsif;
262500121213               chain  %kds(K05tntbe01) TNTBE01L;
262600121213               IF  %found(TNTBE01L);
262700121213                DELETE  TNTBE000;
262800121213               ENDIF;
262900121213             ENDIF;
263000151217           //?se viene tolto il flag si merge controllo se esiste rcd JDC
263100151217           //?se esiste cancello
263200151217             IF  §LACmerge = *blanks;
263300151217               k_TBEcod = 'JDC';
263400151217               k_TBEke1 = %editc(V2Cccm:'X');
263500151217               clear k_TBEke2;
263600151217               k_TBElin = TBXlin;
263700151217               k_TBEsif = TBXsif;
263800151217               chain  %kds(K05tntbe01) TNTBE01L;
263900151217               IF  %found(TNTBE01L);
264000151217                DELETE  TNTBE000;
264100151217               ENDIF;
264200151217             ENDIF;
264300121213         ENDSL;
264400071207
264500071207       ENDSR;
264600071207
264700071212       //--------------------------------------------------------------
264800090306       //?Operazioni finali.                                           ?
264900071212       //--------------------------------------------------------------
265000090306       BEGSR  sr_RoutEnd;
265100071207
265200090306         clear  TIBS69ds;
265300090304         tibs69ds.I69tla = 'C';
265400090304         tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
265500071207
265600071217         kpjbu = TNTB46ds;
265700071207         return;
265800071207
265900071207       ENDSR;
266000090306
266100071207      /end-free
266200071207
266300071212       //--------------------------------------------------------------
266400090306       //?Schiere a tempo di compilazione.                             ?
266500071212       //--------------------------------------------------------------
266600071207
266700071207** - $MSG -------------------------------------------------------------------*
266800071207Immettere il codice cliente                                                     1
266900071207Immettere SOLO caratteri numerici                                               2
267000071207Codice errato                                                                   3
267100071207Cliente annullato                                                               4
267200071207Immettere la directory                                                          5
267300071207Carattere "\" non valido; per indicare una sottocartella utilizzare "/"         6
267400071210Caratteri & e %  non ammessi per la directory di clienti con invio automatico   7
267500071207Indicare il giorno del mese se specificato almeno un mese                       8
267600071207Specificare almeno un mese se indicato il giorno del mese                       9
267700071207Per i mesi con n° di giorni < &g verrà assunto il loro ultimo giorno -> Enter  10
267800090225Inserire il codice padre                                                       11
267900090225Il codice padre deve essere = al codice cliente o = a cliente già in tabella   12
268000120112Cliente da copiare NON più reperibile in tab. "LAC"                            13
268100120112Cliente già inserito in tab. "LAC"                                             14
268200141203Data errata                                                                    15
