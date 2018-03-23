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
011200090306         // - Posizionamento cursore & segnalazione errore
011300090311     d  PosCurCcm                      n   overlay(IndDspF:50)
011400090311     d  PosCurAut                      n   overlay(IndDspF:51)
011500090311     d  PosCurTpi                      n   overlay(IndDspF:52)
011600090311     d  PosCurDir                      n   overlay(IndDspF:53)
011700090311     d  PosCurSchGg                    n   overlay(IndDspF:54)
011800090311     d  PosCurSch01                    n   overlay(IndDspF:55)
011900090311     d  PosCurSch02                    n   overlay(IndDspF:56)
012000090311     d  PosCurSch04                    n   overlay(IndDspF:57)
012100090311     d  PosCurSch06                    n   overlay(IndDspF:58)
012200090311     d  PosCurSch09                    n   overlay(IndDspF:59)
012300090311     d  PosCurSch11                    n   overlay(IndDspF:60)
012400090311     d  PosCurKsu                      n   overlay(IndDspF:61)
012500090311     d  PosCurMesi                     n   overlay(IndDspF:62)
012600090311     d  PosCurTdta                     n   overlay(IndDspF:63)
012700090311     d  PosCurTad                      n   overlay(IndDspF:64)
012800090311     d  PosCurTadu                     n   overlay(IndDspF:65)
012900090311     d  PosCurKsc                      n   overlay(IndDspF:66)
013000090311     d  PosCurCtr                      n   overlay(IndDspF:67)
013100090311     d  PosCurImp                      n   overlay(IndDspF:68)
013200090311     d  PosCurFimp                     n   overlay(IndDspF:69)
013300090311     d  PosCurFmi                      n   overlay(IndDspF:70)
013400090311     d  PosCurDatai                    n   overlay(IndDspF:71)
013500090311     d  PosCurLnp                      n   overlay(IndDspF:72)
013600090327     d  PosCurTpt                      n   overlay(IndDspF:73)
013700110713     d  PosCurPgm                      n   overlay(IndDspF:74)
013800110713     d  PosCurEle                      n   overlay(IndDspF:75)
013900120112     d  PosCurCCM2                     n   overlay(IndDspF:76)
014000140813     d  PosCurTpAdd                    n   overlay(IndDspF:77)
014100140813     d  PosCurCompCE                   n   overlay(IndDspF:78)
014200141203     d  PosCurAutDG                    n   overlay(IndDspF:79)
014300090306         //   - Riemissione videata
014400090311     d  ErrGenerico                    n   overlay(IndDspF:99)
014500071207
014600090306       // - Parametri ricevuti
014700071206     d KPJBA         e ds
014800071217     d TNTB46ds      e ds                  inz
014900121212     d TNTB464ds     e ds                  inz
015000151217     d TNTBJDCds     e ds                  inz
015100071207
015200090306       // - Reperimento dati utente
015300071207     d TIBS34ds      e ds
015400071207
015500090306       // - Controllo/Decodifica cliente
015600090304     d TIBS69ds      e ds                  qualified  inz
015700090304     d ds_CNACO      e ds                  extname(CNACO00F)
015800090304     d                                     qualified  inz
015900090304     d ds_CNIND      e ds                  extname(CNIND00F)
016000090304     d                                     qualified  inz
016100090304     d ds_CNCLP      e ds                  extname(CNCLP00F)
016200090304     d                                     qualified  inz
016300090304     d ds_FNCLS      e ds                  extname(FNCLS00F)
016400090304     d                                     qualified  inz
016500141203
016600090306       // - Dati record principale della tabella
016700090306     d TNTBEds       e ds                  inz  extname(TNTBE00F)
016800090306     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
016900090306     d                                          prefix(TBX:3)
017000090312
017100090312       // - Parametri per Ricerca/controllo tabelle
017200090312     d TIBS02ds      e ds                  inz
017300090312     d  T02mod       e                     inz('C')
017400090312     d  T02cod       e                     inz(c_Tab)
017500071207
017600090306       // - Tabella "LAC" = Clienti per ritorno immagini
017700071206     d dLAC          e ds                  inz
017800090306       // - Tabella "LAC" del padre per controllo legami
017900090303     d dLACp         e ds                  extname(dLAC) inz
018000121016     d                                     prefix(p_)
018100090303     d  p_SchMm               71     82
018200121016
018300121016       // - Tabella "JDC"
018400121016     d dJDC          e ds                  extname(dJDC) inz
018500121217       // - Tabella "LA2" = Estensione tab.LAC
018600121217     d dLA2          e ds                  inz
018700121016
018800090312       // - Tabella "GED" = Divise area gestionale
018900090312     d dGED          e ds                  inz
019000090312       // - Tabella "GEI" = Importi vari area gestionale
019100090312     d dGEI          e ds                  inz
019200141128
019300141128       // - Tabella "TAL" = Tipo addebito dichiarato
019400141128     d dTAL          e ds                  inz
019500141106
019600141106       // - File Conto Economico
019700141106     d ECEVD00F      e ds                  extname(ECEVD00F)
019800141203
019900141203      // - File Date
020000141203     d DATECNV0F     e ds                  extname(DATECNV0F)
020100090303
020200090306       // - Mesi in cui è schedulata la mail di promemoria creazione CD
020300090303     d ds_SchMm        ds            12    inz
020400090303     d  V2Csch01                           inz
020500090303     d  V2Csch02                           inz
020600090303     d  V2Csch03                           inz
020700090303     d  V2Csch04                           inz
020800090303     d  V2Csch05                           inz
020900090303     d  V2Csch06                           inz
021000090303     d  V2Csch07                           inz
021100090303     d  V2Csch08                           inz
021200090303     d  V2Csch09                           inz
021300090303     d  V2Csch10                           inz
021400090303     d  V2Csch11                           inz
021500090303     d  V2Csch12                           inz
021600090311
021700090311       // - Struttura di comodo x ctrl mesi di schedulazione
021800090311     d wSchMm_ds       ds            12    inz
021900090311     d  $SchMm                 1     12    dim(12)
022000090319       // - Struttura per passaggio dati ad interrogazione tabella
022100090327     d Param01         ds                  inz
022200090327     d  P01cod                        7  0 inz
022300090327     d  P01ord                        1    inz
022400090327     d  P01ksu                        7  0 inz
022500090327     d  P01ke1                        7    inz
022600090327     d  P01ke2                       15    inz
022700090327     d  P01rit                        1    inz
022800071207
022900090306       //--------------------------------------------------------------
023000090306       //?Definizione procedure usate.                                 ?
023100090306       //--------------------------------------------------------------
023200090306
023300090306       // - Reperimento dati utente
023400090306      /copy gaitrasrc/srcProtoPR,TIBS34R
023500071207
023600090304       // - Ricerca/Controllo tabelle
023700090306      /copy gaitrasrc/srcProtoPR,TIBS02R
023800071207
023900090304       // - Controllo/Decodifica cliente
024000090306      /copy gaitrasrc/srcProtoPR,TIBS69R
024100090319
024200090319       // - Interrogazione tabella LAC
024300090319     d tntb461r        pr                  extpgm('TNTB461R')
024400090319     d  kpjba                              likeds(KPJBA)
024500141204     d  P01opz3                       1a   const
024600121212
024700121212       // - Gestione tabella LA2
024800121212     d tntb464r        pr                  extpgm('TNTB464R')
024900121213     d  kpjba                              likeds(KPJBA)
025000121212     d  tntb464ds                          likeds(TNTB464DS)
025100151217
025200151217       // - Gestione tabella JDC
025300151217     d tntbJDCr        pr                  extpgm('TNTBJDCR')
025400151217     d  kpjba                              likeds(KPJBA)
025500071207
025600090306       //--------------------------------------------------------------
025700090306       //?Definizione variabili globali.                               ?
025800090306       //--------------------------------------------------------------
025900090306
026000090306       // - Flags booleani
026100090306     d $InzD02         s               n   inz(*on)
026200090306     d $Fine           s               n   inz
026300090923     d $forza          s               n   inz
026400090306     d $CtrMm          s               n   inz
026500090313     d $ByPass         s               n   inz
026600120112     d $Copia          s               n   inz
026700121016     d wForza          s               n   inz
026800121030     d wForzaTPI       s               n   inz
026900121030     d wForzaFMI       s               n   inz
027000121030     d wForzaDIR       s               n   inz
027100071207
027200090306       // - Gestione video
027300141117     d $Video          s              2    inz('D2')
027400090313
027500090313       // - Indici di schiera
027600090313     d xx              s              3  0 inz
027700071207
027800090313       // - Campi di comodo
027900090306     d wDataI          s              6
028000090306     d wDatEla         s              6
028100090306     d wPos1           s              2  0 inz
028200090306     d wPos2           s              2  0 inz
028300090313     d wCount          s              2  0 inz
028400090313     d wSavSch         s                   like(ds_SchMm)  inz
028500090316     d SAVke1          s                   like(TBEke1)    inz
028600090923     d savtad          s                   like(§lactad)
028700090923     d savtadu         s                   like(§lactadu)
028800090923     d savksc          s                   like(§lacksc)
028900090923     d savctr          s                   like(§lacctr)
029000090923     d savimp          s                   like(§lacimp)
029100090923     d savfimp         s                   like(§lacfimp)
029200121220     d woggi           s              8  0 inz
029300141201     d wceksc          s              5  3 inz
029400141201     d wceksu          s              5  3 inz
029500141203     d conta           s              5i 0 inz
029600141204     d P01opz3         s              1a   inz('U')
029700071207
029800090306       //--------------------------------------------------------------
029900090306       //?Definizione key-list.                                        ?
030000090306       //--------------------------------------------------------------
030100090306
030200090306       // - File TNTBE01L
030300090306     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
030400090306     d                                     prefix(k_)  inz
030500071206
030600090306       //--------------------------------------------------------------
030700090306       //?Riepilogo indicatori.                                        ?
030800090306       //--------------------------------------------------------------
030900090306       // 01    - RECORD annullato
031000090306       // 02    - acceso IMMISSIONE - spento MANUTENZIONE
031100090306       // 25    - Comodo
031200090306       // 28    - Emissione del messaggio di errore a video
031300090306       // 40    - Protezione campi in prima estrazione
031400090306       // x D01:
031500090306       // 50    - Codice cliente errato o mancante
031600090306       // x D02:
031700120112       // 41    - Copia in D02: V2CCCM non protetto
031800090306       // 53    - Directory per immagini       errata
031900090306       // 54    - Giorno di schedulazione CD   errato
032000090306       // 55    - Selezione mesi obbligatoria
032100090306       // 56    - Selezione del mese di febbraio
032200090306       // 57    - Selezione del mese di aprile
032300090306       // 58    - Selezione del mese di giugno
032400090306       // 59    - Selezione del mese di settembre
032500090306       // 60    - Selezione del mese di novembre
032600090306       // 61    - Codice Padre                 errato
032700090306       // 62    - N. mesi da estrarre          errato
032800090306       // 63    - Tipo data di elaborazione    errato
032900090306       // 64    - Tipo addebito                errato
033000090306       // 65    - Creazione addebito           errato
033100090306       // 66    - Cliente tassazione           errato
033200090306       // 67    - Codice tariffa               errato
033300090306       // 68    - Importo forzato              errato
033400090306       // 69    - Tipo importo forzato         errato
033500090306       // 70    - Flag nome immagine           errato
033600090306       // 71    - Prima elaborazione           errata
033700090306       // 99    - Generico di errore
033800090306       //--------------------------------------------------------------
033900071206
034000071206     c     *Entry        plist
034100071206     c                   parm                    KPJBA
034200071207
034300071207      /free
034400071207
034500071207       // Operazioni iniziali
034600090306       exsr  sr_RoutInz;
034700071207
034800071207       // Gestione video
034900090306       DOW  $Fine = *off;
035000071207         select;
035100090306           when  $Video = 'D2';
035200090306             exsr  sr_GesD02;
035300101103           when  $Video = 'W1';
035400101103             exsr  sr_GesW01;
035500071207           other;
035600071207             leave;
035700071207         endsl;
035800071207       ENDDO;
035900071207
036000071207       // Operazioni finali
036100090306       exsr  sr_RoutEnd;
036200071206
036300071212       //--------------------------------------------------------------
036400090306       //?Operazioni iniziali.                                         ?
036500071212       //--------------------------------------------------------------
036600090306       BEGSR  sr_RoutInz;
036700141106
036800141106         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
036900090306
037000090306         *inLR = *on;
037100071207
037200090306         if  kpjbu <> *blank;
037300071217           TNTB46ds = kpjbu;
037400071207         else;
037500090306           clear  TNTB46ds;
037600071207         endif;
037700071217         B46fxx = *blank;
037800071217         B46err = *off;
037900071217         B46msg = *blank;
038000071207
038100090306         // Reperimento dati job
038200090306         exsr  DatiJob;
038300071207
038400090306         // Impostazione nome programma a video
038500071207         V1Tpgm = SDSpgm;
038600090306
038700090306         // Aggancio dati generali della tabella in esame
038800090306         clear  k_TBEcod;
038900090306         k_TBEke1 = *zero;
039000090306         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
039100090306                = c_Tab;
039200090306         clear  k_TBEke2;
039300090306         clear  k_TBElin;
039400090306         k_TBEsif = KNSIF;
039500090306         chain(n)  %kds(k05tntbe01)  TNTBE000;
039600090306         if  not  %found(TNTBE01L);
039700090306           clear  k_TBEsif;
039800090306           chain(n)  %kds(k05tntbe01)  TNTBE000;
039900090306         endif;
040000090306         if  %found(TNTBE01L);
040100090306           xTNTBEds = TNTBEds;
040200090306         else;
040300090306           clear  xTNTBEds;
040400090306         endif;
040500071207
040600090306         // Verifica parametri ricecvuti
040700090306         if  B46ke1  <> *blank;
040800141117           $Video  = 'D2';
040900141117           $InzD02 = *on;
041000071207         endif;
041100121220
041200121220         // Imposto la data di oggi
041300121220         woggi = %dec(%date());
041400071207
041500071207       ENDSR;
041600071206
041700071212       //--------------------------------------------------------------
041800090306       //?Reperimento Dati del job (Utente/Operativi).                 ?
041900071212       //--------------------------------------------------------------
042000090306       BEGSR  DatiJob;
042100071207
042200071207         in(E) §AzUte;
042300071207         if NOT %error;
042400071207           in(E) §DatiUte;
042500071207         endif;
042600090306         if %error or RSut = *blank;
042700071207           clear TIBS34ds;
042800071207           tibs34r(tibs34ds);
042900071207           in §AzUte;
043000071207           in §DatiUte;
043100071207         endif;
043200071207
043300071207       ENDSR;
043400141117
043500141117       //--------------------------------------------------------------
043600141117       //?Decodifica cliente passato.                                  ?
043700141117       //--------------------------------------------------------------
043800141117       BEGSR  Decodifica;
043900141117
044000141117         $Copia = (B46opz = '3');
044100141117
044200141117         IF  B46ke1 = *blanks;
044300141117           leavesr;
044400141117         ENDIF;
044500141117
044600141117         If  Not  $Copia;
044700141117           clear  TIBS69ds;
044800141117           tibs69ds.I69kcc = DUTkci;
044900141117           tibs69ds.I69kac = %int(B46ke1);
045000141117           tibs69ds.I69sif = knsif;
045100141117           tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
045200141117           if  tibs69ds.O69err = *on;
045300141117             errMessage  = *on;
045400141117             V1Dmsg = $Msg(03);
045500141117             leavesr;
045600141117           endif;
045700141117         EndIf;
045800141117
045900141117         // - Verifica esistenza codice cliente in tabella
046000141117         reset  TNTBE000;
046100141117         k_TBEke1 = B46ke1;
046200141117         clear  k_TBEke2;
046300141117         k_TBElin = TBXlin;
046400141117         k_TBEsif = TBXsif;
046500141117         chain  %kds(K05tntbe01) TNTBE000;
046600141117         if  NOT  %found(TNTBE01L);
046700141117           errMessage  = *on;
046800141117           V1Dmsg = 'Cliente non presente in tabella';
046900141117           leavesr;
047000141117         endif;
047100141117
047200141117         dLAC = TBEuni;
047300141117
047400141117       ENDSR;
047500071206
047600071212       //--------------------------------------------------------------
047700090306       //?Gestione videata D02                                         ?
047800071212       //--------------------------------------------------------------
047900090306       BEGSR  sr_GesD02;
048000071207
048100090306         // Inizializzazione videata
048200090306         if   $InzD02 = *on;
048300090306           exsr  sr_InzD02;
048400090306           $InzD02 = *off;
048500071207         endif;
048600071207
048700090306         // Emissione testata
048800090306         write  TN46T01;
048900071207
049000090306         // Emissione videata
049100151130         if  B46opz = '5';
049200141117           write  TN46D02;
049300141117           exfmt  PROTECT;
049400141117         else;
049500090306           exfmt  TN46D02;
049600071217         endif;
049700071207         errMessage  = *off;
049800071207         errGenerico = *off;
049900090306         clear  V1Dmsg;
050000071207
050100071207         SELECT;
050200090306           // F3=Fine
050300090306           WHEN  dsp_aid = c_F03;
050400141117             exsr  sr_F03D02;
050500090319           // F04=Interrogazione Padre
050600090319           WHEN  dsp_aid = c_F04;
050700090319             exsr  sr_F04D02;
050800141205           // F08=Vis. tabella LA2
050900141205           WHEN  dsp_aid = c_F08;
051000141205             exsr  sr_F08D02;
051100151217           // F09=Vis. tabella JDC
051200151217           WHEN  dsp_aid = c_F09;
051300151217             exsr  sr_F09D02;
051400090306           // F12=Ritorno
051500090306           WHEN  dsp_aid = c_F12;
051600090306             exsr  sr_F12D02;
051700090306           // Enter o F5 o F6 o F16
051800071207           OTHER;
051900090306             // - Controllo dati
052000090306             if  dsp_aid = c_F16;
052100090306               exsr  sr_CtrAnnull;
052200090306             else;
052300090306               exsr  sr_CtrD02;
052400090306             endif;
052500090306             if  errGenerico;
052600071207               leavesr;
052700071207             endif;
052800121212
052900090306             // - Conferma dati:
053000090306             //   F5=Ripristino; F6=Conferma; F16=Annullamento
053100090306             if  dsp_aid = c_F05  or
053200090306                 dsp_aid = c_F06  or
053300090306                 dsp_aid = c_F16;
053400121212
053500090306               exsr  Upd_TNTBE;
053600121213
053700121217           //?Se F6 e immagine da duplicare su altra direcotru devo andare su tabella LA2
053800121217               IF  dsp_aid = c_F06 and V2Cla2 = 'S';
053900121217                 exsr GESLA2;
054000121213                 $Video = 'D2';
054100121213                 $InzD02 = *on;
054200121213                 chain  %kds(K05tntbe01) TNTBE000;
054300121213                 IF  B464err <> '0' and B464err <> *blanks;
054400121213                   ErrMessage  = *on;
054500121213                   ErrGenerico = *on;
054600121213                   V1Dmsg = B464msg;
054700121213                   leavesr;
054800121213                 ENDIF;
054900121213                 unlock TNTBE01L;
055000121213               ENDIF;
055100151217
055200151217           //?Se F6 e immagine da duplicare su altra direcotru devo andare su tabella JDC
055300151217               IF  dsp_aid = c_F06 and V2Merge = 'S';
055400151217                 exsr GESJDC;
055500151217                 $Video = 'D2';
055600151217                 $InzD02 = *on;
055700151217                 chain  %kds(K05tntbe01) TNTBE000;
055800151217                 IF  BJDCerr <> '0' and BJDCerr <> *blanks;
055900151217                   ErrMessage  = *on;
056000151217                   ErrGenerico = *on;
056100151217                   V1Dmsg = BJDCmsg;
056200151217                   leavesr;
056300151217                 ENDIF;
056400151217                 unlock TNTBE01L;
056500151217               ENDIF;
056600121212
056700071207               if  NOT  errGenerico;
056800141117                 $Fine  = *on;
056900071207               endif;
057000071207             EndIf;
057100071207
057200071207         ENDSL;
057300071207
057400071207       ENDSR;
057500071206
057600071212       //--------------------------------------------------------------
057700090306       //?Inizializzazione videata D01                                 ?
057800071212       //--------------------------------------------------------------
057900090306       BEGSR  sr_InzD02;
058000071207
058100090306         reset  $CtrMm;
058200090313         reset  $ByPass;
058300121016         reset  wForza;
058400121030         reset  wForzaTPI;
058500121030         reset  wForzaFMI;
058600121030         reset  wForzaDIR;
058700071207
058800101103         clear  TB46W01;
058900101103
059000090306         clear  TN46D02;
059100071210
059200090306         IndDspF = *zero;
059300141201         k_TBEcod = c_Tab;
059400141117
059500141117         exsr Decodifica;
059600141117         if  errMessage = *on;
059700141117           B46err = *on;
059800141117           B46msg = V1Dmsg;
059900141117           $Fine  = *on;
060000141117           leavesr;
060100141117         endif;
060200090309
060300090309         // Impostazione indicatori per abilitazione tasti funzionali
060400090309         exsr  Set_Ind_D02;
060500090309
060600120112         // - Codice estrazione (chiave)
060700141117         if  Not  $Copia and B46ke1 <> *blanks;
060800141117           V2Cccm = %int(B46ke1);
060900141117           V2Dccm = ds_CNACO.ACOrag;
061000141117           InserCCM2 = *off;
061100120112         endif;
061200141117
061300141117         IF  $Copia;
061400141117           V2Cksu = %int(B46ke1);
061500141117           V2Dksu = ds_CNACO.ACOrag;
061600141117           InserCCM2 = *on;
061700141117         ENDIF;
061800141117
061900141117         // Impostazione dati di record nuovo
062000141117         IF  B46ke1 = *blanks;
062100141117           InserCCM2 = *on;
062200141117           V1Tlav = 'IMMISSIONE';
062300141117           *in02 = *on;
062400141117           clear dLA2;
062500151217           clear dJDC;
062600150122           exec sql
062700150122           set :V2dtaimm = (select DC_DMYY_A1 from DATECNV0F
062800150122                            where DC_YYMD_Z = :woggi);
062900141117           leavesr;
063000141117         ENDIF;
063100071207
063200090306         IF  %found(TNTBE01L);
063300071207
063400090306           // Impostazione dati di record trovato
063500071217           select;
063600141117             when  B46opz = '2';
063700141117               V1Tlav = 'VARIAZIONE';
063800090306             when  B46opz = '5';
063900071217               V1Tlav = 'VISUALIZZA';
064000151130             when  B46opz = '7';
064100151130               V1Tlav = 'RIPRISTINO';
064200120112             when  $Copia;
064300120112               V1Tlav = '  COPIA   ';
064400071217             other;
064500071217               V1Tlav = 'ANNULLATO ';
064600071217           endsl;
064700090225
064800090306           // Codice padre
064900090306           V2Cksu = §LACksu;
065000090309           clear  TIBS69ds;
065100090309           clear  ds_CNACO;
065200090309           clear  ds_CNIND;
065300090309           clear  ds_CNCLP;
065400090309           clear  ds_FNCLS;
065500090310           select;
065600090310             when  §LACksu =  V2Cccm;
065700090310               V2Dksu = V2Dccm;
065800090310             when  §LACksu <> *zero;
065900090310               tibs69ds.I69kcc = DUTkci;
066000090310               tibs69ds.I69kac = %int(V2Cksu);
066100090310               tibs69ds.I69sif = knsif;
066200090310               tibs69r(TIBS69ds:ds_CNACO:ds_CNIND:ds_CNCLP:ds_FNCLS);
066300090310               if tibs69ds.O69err = *on;
066400090310                 V1Dmsg = $Msg(03);
066500090310                 errMessage  = *on;
066600090310                 errGenerico = *on;
066700090310                 PosCurKsu   = *on;
066800090310                 leavesr;
066900090310               endif;
067000090310               V2Dksu = ds_CNACO.ACOrag;
067100090310           endsl;
067200090225
067300090225           V2Caut   = §LACaut;
067400090309           V2Ctpi   = §LACtpi;
067500090225
067600090306           // Flag nome immagine
067700090306           V2Cfmi = §LACfmi;
067800090310           reset  TIBS02ds;
067900090303           T02cod = 'NIM';
068000090303           T02ke1 = §LACfmi;
068100090304           TNTBE_RicercaControllo(kpjba : tibs02ds);
068200090306           if  T02err = *blank;
068300090303             V2Dfmi = T02uni;
068400090303           endif;
068500121016
068600121016           V2merge = §LACmerge;
068700121217           V2Cla2  = §LACla2;
068800090225
068900090309           V2Cdir   = §LACdir;
069000090309           V2Ctpt   = §LACtpt;
069100090309           V2Cssr   = §LACssr;
069200090309           V2Clnp   = §LAClnp;
069300090309           V2Cres   = §LACres;
069400090309           V2Crec   = §LACrec;
069500090309           V2Ccsr   = §LACcsr;
069600090309
069700090309           // Prima estrazione
069800090309           clear  wDataI;
069900090309           if  §LACdatai > *zero;
070000090309             %subst(wDataI:1:2) = %subst(%editc(§LACdatai:'X'):5:2);
070100090309             %subst(wDataI:3:4) = %subst(%editc(§LACdatai:'X'):1:4);
070200090310             V2datai = %int(wDataI);
070300090309           endif;
070400090309
070500090309           // Ultima estrazione
070600090309           clear  wDatEla;
070700090309           if  §LACdatela > *zero;
070800090309             %subst(wDatEla:1:2) = %subst(%editc(§LACdatela:'X'):5:2);
070900090309             %subst(wDatEla:3:4) = %subst(%editc(§LACdatela:'X'):1:4);
071000090310             V2datela = %int(wDatEla);
071100090309           endif;
071200090309
071300090309           V2tdta   = §LACtdta;
071400090309           V2mesi   = §LACmesi;
071500071207           V2Cschgg = §LACschgg;
071600071207           V2Csch01 = §LACsch01;
071700071207           V2Csch02 = §LACsch02;
071800071207           V2Csch03 = §LACsch03;
071900071207           V2Csch04 = §LACsch04;
072000071207           V2Csch05 = §LACsch05;
072100071207           V2Csch06 = §LACsch06;
072200071207           V2Csch07 = §LACsch07;
072300071207           V2Csch08 = §LACsch08;
072400071207           V2Csch09 = §LACsch09;
072500071207           V2Csch10 = §LACsch10;
072600071207           V2Csch11 = §LACsch11;
072700071207           V2Csch12 = §LACsch12;
072800090225
072900090306           V2tadu   = §LACtadu;
073000090309           V2Ctad   = §LACtad;
073100090306           V2cksc   = §LACksc;
073200090306           V2cctr   = §LACctr;
073300090306           V2imp    = §LACimp;
073400090306           V2fimp   = §LACfimp;
073500110713           V2pgm    = §LACpgm;
073600110713           V2ele    = §LACele;
073700090306           V2note   = §LACnote;
073800140813
073900140813           IF  §LACdtaimm <> *blanks;
074000140813             V2dtaimm = %subst(§LACdtaimm:7:2) + '/' +
074100140813                        %subst(§LACdtaimm:5:2) + '/' +
074200140813                        %subst(§LACdtaimm:1:4);
074300140813           ENDIF;
074400150122
074500150122           IF  $Copia and V2dtaimm = *blanks;
074600150122             exec sql
074700150122             set :V2dtaimm = (select DC_DMYY_A1 from DATECNV0F
074800150122                              where DC_YYMD_Z = :woggi);
074900150122           ENDIF;
075000141128
075100140813           V2utente = §LACutente;
075200141128
075300140813           V2compce = §LACcompce;
075400141128           reset  TIBS02ds;
075500141128           T02cod = 'CED';
075600141128           T02ke1 = V2compce;
075700141128           T02sif = KNSIF;
075800141128           TNTBE_RicercaControllo(kpjba : tibs02ds);
075900141128           IF  T02err = *blank;
076000141128             V2dcompce = T02uni;
076100141128           ENDIF;
076200141202           IF  V2compce = *blanks;
076300141202             V2dcompce = 'NON DICHIARATO';
076400141202           ENDIF;
076500141202
076600141128           IF V2Cccm <> V2Cksu;
076700141128             clear dLACp;
076800141128             reset  TIBS02ds;
076900141128             T02sif = KNSIF;
077000141128             T02cod = 'LAC';
077100141128             T02ke1 = %editc(V2Cksu:'X');
077200141128             TNTBE_RicercaControllo(kpjba : tibs02ds);
077300141128             IF  T02err = *blank;
077400141128               dLACp = T02uni;
077500141128             ENDIF;
077600141128             reset  TIBS02ds;
077700141128             T02cod = 'CED';
077800141128             T02ke1 = p_§LACcompce;
077900141128             T02sif = KNSIF;
078000141128             TNTBE_RicercaControllo(kpjba : tibs02ds);
078100141128             IF  T02err = *blank;
078200141128               V2dceksu = T02uni;
078300141128             ENDIF;
078400141128             VisCEksu = *on;
078500141128           ELSE;
078600141128             VisCEksu = *off;
078700141128           ENDIF;
078800141128
078900140813           V2tpadd  = §LACtpadd;
079000140813           reset  TIBS02ds;
079100141128           clear dTAL;
079200140813           T02cod = 'TAL';
079300140813           T02ke1 = V2tpadd;
079400140813           T02sif = KNSIF;
079500140813           TNTBE_RicercaControllo(kpjba : tibs02ds);
079600140813           IF  T02err = *blank;
079700141128             dTAL = T02uni;
079800141128           ENDIF;
079900141128           V2dtpadd = §TALdes;
080000141202           IF  V2tpadd = *blanks;
080100141202             V2dtpadd = 'NON DICHIARATO';
080200141202           ENDIF;
080300090923
080400090923           *in02 = *off;
080500121217
080600121217           // Cerco i dati della tabella LA2
080700121217           clear dLA2;
080800121217           reset  TIBS02ds;
080900121217           IF  V2Cla2 = 'S';
081000121217             T02cod = 'LA2';
081100121217             T02ke1 = B464ke1;
081200121217             TNTBE_RicercaControllo(kpjba : tibs02ds);
081300121217             IF  T02err = *blank;
081400121217               dLA2 = T02uni;
081500121217             ENDIF;
081600121217           ENDIF;
081700151217
081800151217           // Cerco i dati della tabella JDC
081900151217           clear dJDC;
082000151217           reset  TIBS02ds;
082100151217           IF  V2Merge = 'S';
082200151217             T02cod = 'JDC';
082300151217             T02ke1 = BJDCke1;
082400151217             TNTBE_RicercaControllo(kpjba : tibs02ds);
082500151217             IF  T02err = *blank;
082600151217               dJDC = T02uni;
082700151217             ENDIF;
082800151217           ENDIF;
082900071207
083000071207         ENDIF;
083100090923
083200090923         // Salvo i dati sensibili per la creazione addebito
083300090923         savtad = v2ctad;
083400090923         savtadu = v2tadu;
083500090923         savksc = v2cksc;
083600090923         savctr = v2cctr;
083700090923         savimp = v2imp;
083800090923         savfimp = v2fimp;
083900090923         $forza = *off;
084000130201
084100130201         // Imposto (se c'è) data ultima variazione
084200130201         clear V2datauv;
084300130201         IF  TBEdtr > 0;
084400130201           V2datauv = %subst(%editc(TBEdtr:'X'):7:2) + '/' +
084500130201                      %subst(%editc(TBEdtr:'X'):5:2) + '/' +
084600130201                      %subst(%editc(TBEdtr:'X'):1:4);
084700130201         ENDIF;
084800141128
084900141128         // Cerco la personalizzazione C/E del KSC
085000141203         exec sql
085100141203         select EVDitr into :wceksc
085200141203         FROM ecevd00f
085300141203         WHERE EVDcli = :V2cccm and
085400141203               EVDvoc = 199 and
085500141203               EVDttv = 'K' and
085600141203               EVDddt <= :woggi and
085700141203               EVDdst >= :woggi;
085800141203         IF  sqlcod = 100 or sqlcod < 0;
085900141203           V2ceksc = 'STD';
086000141203         ELSE;
086100141203           V2ceksc = %editw(wceksc:'0 ,   ');
086200141203         ENDIF;
086300141128
086400141128         // Cerco la personalizzazione C/E del KSU
086500141203         IF  VisCeKsu;
086600141203           exec sql
086700141203           select EVDitr into :wceksu
086800141203           FROM ecevd00f
086900141203           WHERE EVDcli = :V2Cksu and
087000141203                 EVDvoc = 199 and
087100141203                 EVDttv = 'K' and
087200141203                 EVDddt <= :woggi and
087300141203                 EVDdst >= :woggi;
087400141203           IF  sqlcod = 100 or sqlcod < 0;
087500141203             V2ceksu = 'STD';
087600141203           ELSE;
087700141203             V2ceksu = %editw(wceksu:'0 ,   ');
087800141203           ENDIF;
087900141203         ENDIF;
088000141203
088100141203         // Data AUT DG per c/e  a 0 o NO ddt
088200141203         IF  §LACdtaudg <> *blanks;
088300141203           exec sql
088400141203           set :V2dtautdg =
088500141203               (select DC_DMYY_Z from DATECNV0F
088600141203                where DC_YYMD_C = :§LACdtaudg);
088700141203         ENDIF;
088800071207
088900071207       ENDSR;
089000071217
089100071217       //--------------------------------------------------------------
089200090306       //?Settaggio indicatori nella videata D02 per abilitazione      ?
089300090306       //?  tasti funzionali                                           ?
089400071217       //--------------------------------------------------------------
089500090306       BEGSR  Set_Ind_D02;
089600090319
089700141204         F4Attivo  = B46opz <> '5';
089800090306         F5Attivo  = ((%found(TNTBE01L) and TBEatb <> *blank)
089900071217                                        and B46opz <> '5');
090000071217         F6Attivo  = (NOT F5Attivo and B46opz <> '5');
090100141205         F8Attivo  = B46opz = '5' and §LACla2 = 'S';
090200151217         F9Attivo  = B46opz = '5' and §LACmerge = 'S';
090300090306         F16Attivo = ((%found(TNTBE01L) and TBEatb = *blank)
090400151130                                        and B46opz <> '5');
090500090306
090600120112         // - Se ultima estrazione impostata proteggo la prima estrazione
090700090309         ProtDATAI = (§LACdatela <> *zero);
090800071217
090900071217       ENDSR;
091000141117
091100141117       //--------------------------------------------------------------
091200141117       //?Gestione tasto funzionale F3 da videata D02                  ?
091300141117       //?F3=Fine                                                      ?
091400141117       //--------------------------------------------------------------
091500141117       BEGSR  sr_F03D02;
091600141117
091700141117         // Chiusura del programma
091800141117         $Fine = *on;
091900141117
092000141117         // Restituzione indicazione del tasto funzionale premuto
092100141117         B46fxx = '1';
092200141117
092300141117       ENDSR;
092400090319
092500090319       //--------------------------------------------------------------
092600090319       //?Gestione tasto funzionale F04 da videata D02                 ?
092700090319       //?F04=Interrogazione Padre                                     ?
092800090319       //--------------------------------------------------------------
092900090319       BEGSR  sr_F04D02;
093000090319
093100090319         // Richiamo pgm interrogazione
093200090327         clear  Param01;
093300090327         P01ord = '1';
093400090327         P01ksu = V2Cksu;
093500090327         KPJBU  = Param01;
093600141204         tntb461r (KPJBA : P01opz3);
093700141204         Param01 = kpjbu;
093800141204         IF  V2Cksu = *zeros and P01ke1 <> *zeros and
093900141204             P01ke1 <> *blanks;
094000141204           V2Cksu = %dec(P01ke1:7:0);
094100141204         ENDIF;
094200090319
094300090319       ENDSR;
094400090319
094500141205       //--------------------------------------------------------------
094600141205       //?Gestione tasto funzionale F08 da videata D02                 ?
094700141205       //?F08=Vis. Tabella LA2
094800141205       //--------------------------------------------------------------
094900141205       BEGSR  sr_F08D02;
095000141205
095100141205         // Richiamo pgm tab. LA2
095200141205         exsr GESLA2;
095300141205
095400141205         // Devo reimpostare i dati per visualizzare nuovamente la tab.LAC
095500141205         $Video = 'D2';
095600141205         $InzD02 = *on;
095700141205         chain  %kds(K05tntbe01) TNTBE000;
095800141205
095900141205         // Se torna errore da tabella LA2 lo emetto
096000141205         IF  B464err <> '0' and B464err <> *blanks;
096100141205           ErrMessage  = *on;
096200141205           ErrGenerico = *on;
096300141205           V1Dmsg = B464msg;
096400141205           leavesr;
096500141205         ENDIF;
096600141205
096700141205       ENDSR;
096800151217
096900151217       //--------------------------------------------------------------
097000151217       //?Gestione tasto funzionale F09 da videata D02                 ?
097100151217       //?F09=Vis. Tabella JDC
097200151217       //--------------------------------------------------------------
097300151217       BEGSR  sr_F09D02;
097400151217
097500151217         // Richiamo pgm tab. LA2
097600151217         exsr GESJDC;
097700151217
097800151217         // Devo reimpostare i dati per visualizzare nuovamente la tab.LAC
097900151217         $Video = 'D2';
098000151217         $InzD02 = *on;
098100151217         chain  %kds(K05tntbe01) TNTBE000;
098200151217
098300151217         // Se torna errore da tabella JDC lo emetto
098400151217         IF  BJDCerr <> '0' and BJDCerr <> *blanks;
098500151217           ErrMessage  = *on;
098600151217           ErrGenerico = *on;
098700151217           V1Dmsg = BJDCmsg;
098800151217           leavesr;
098900151217         ENDIF;
099000151217
099100151217       ENDSR;
099200141205
099300071217       //--------------------------------------------------------------
099400090306       //?Gestione tasto funzionale F12 da videata D02                 ?
099500090306       //?F12=Ritorno                                                  ?
099600071217       //--------------------------------------------------------------
099700090306       BEGSR  sr_F12D02;
099800071217
099900071217           B46fxx = '2';
100000071217           $Fine = *on;
100100071217
100200071217       ENDSR;
100300090306
100400090306       //--------------------------------------------------------------
100500090306       //?Controllo "annullabilità" del record (padre senza figli)     ?
100600090306       //--------------------------------------------------------------
100700090306       BEGSR  sr_CtrAnnull;
100800090306
100900090306         IndDspF = *zero;
101000090306         // REimpostazione indicatori per abilitazione tasti funzionali
101100090306         exsr  Set_Ind_D02;
101200090306
101300090306         if  V2Cksu <= *zero;
101400090306           V2Cksu = §LACksu;
101500090306         endif;
101600090306
101700090306         // Se figlio (non padre): fine controlli
101800090306         if  V2Cccm <> V2Cksu;
101900090306           leavesr;
102000090306         endif;
102100090306
102200090306         //?Verifica se padre con figli?
102300090327         clear  dLACp;
102400090316         SAVke1 = TBEke1;
102500090306         setll     %kds(k05tntbe01 : 1)  TNTBE000;
102600090306         reade(n)  %kds(k05tntbe01 : 1)  TNTBE000;
102700090306
102800090306         dow  not  %eof(TNTBE01L);
102900090316           if  TBEatb = *blank   and
103000090316               %editc(V2Cccm : 'X') <> %trimr(TBEke1);
103100090306             dLACp = TBEuni;
103200090316             if  p_§LACksu = V2Cccm;
103300090306               errMessage  = *on;
103400090306               errGenerico = *on;
103500090306               PosCurKsu   = *on;
103600090306               V1Dmsg = 'Record NON cancellabile: trattasi di PADRE +
103700090306                         con figli (vedi ' + %trimr(TBEke1) + ')';
103800090306               leavesr;
103900090306             endif;
104000090306           endif;
104100090306         reade(n)  %kds(k05tntbe01 : 1)  TNTBE000;
104200090306         enddo;
104300090316
104400120112         // - Riaggancio del rec. in manutenzione/copia
104500090316         TBEke1 = SAVke1;
104600090316         reset  TNTBE000;
104700090316         k_TBEcod = c_Tab;
104800120112         k_TBEke1 = %editc( V2Cccm : 'X' );
104900090316         clear  k_TBEke2;
105000090316         k_TBElin = TBXlin;
105100090316         k_TBEsif = TBXsif;
105200090316         chain  %kds(K05tntbe01) TNTBE000;
105300090306
105400090306       ENDSR;
105500071206
105600071212       //--------------------------------------------------------------
105700090306       //?Controllo videata D02                                        ?
105800071212       //--------------------------------------------------------------
105900090306       BEGSR  sr_CtrD02;
106000071207
106100090309         %subst(IndDspF : 50) = *off;
106200120112
106300141117         IF  $Copia or *in02;
106400120112
106500120112           Select;
106600120112
106700120112             // -?Codice estrazione (SE copia)?
106800120112             When  V2Cccm = *zero;
106900120112               errMessage  = *on;
107000120112               errGenerico = *on;
107100120112               PosCurCcm2  = *on;
107200120112               V1Dmsg = $Msg(01);
107300120112               leavesr;
107400120112
107500120112             // -?Controllo / Decodifica?
107600151130             Other;
107700151130             //il test seguente rimosso immagino fosse fatto per velocizzare ma nel secondo giro
107800151130             //impediva i test
107900151130             //When  tibs69ds.I69kac <> V2Cccm  or
108000151130             //      k_TBEke1 <> %editc( V2Cccm : 'X' );
108100120112               clear  TIBS69ds;
108200120112               tibs69ds.I69kcc = DUTkci;
108300120112               tibs69ds.I69kac = V2Cccm;
108400120112               tibs69ds.I69sif = knsif;
108500120112               tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
108600120112               if  tibs69ds.O69err = *on;
108700120112                 errMessage  = *on;
108800120112                 errGenerico = *on;
108900120112                 PosCurCcm2  = *on;
109000120112                 V1Dmsg = $Msg(03);
109100120112                 leavesr;
109200120112               endif;
109300120112               V2Dccm = ds_CNACO.ACOrag;
109400120112               // -?Verifica esistenza in tabella?
109500120112               k_TBEke1 = %editc( V2Cccm : 'X' );
109600120112               chain  %kds(K05tntbe01) TNTBE000;
109700120112               select;
109800120112                 // - -?NON ammesso nuovo record per cliente annullato?
109900120112                 when  NOT  %found(TNTBE01L)  and  ds_CNACO.ACOflg <> *blank;
110000120112                   errMessage  = *on;
110100120112                   errGenerico = *on;
110200120112                   PosCurCcm2  = *on;
110300120112                   V1Dmsg = $Msg(04);
110400120112                   leavesr;
110500120112                 // - -?Impossibile inserimento di record già esistente?
110600120112                 when  %found(TNTBE01L);
110700120112                   errMessage  = *on;
110800120112                   errGenerico = *on;
110900120112                   PosCurCcm2  = *on;
111000120112                   V1Dmsg = $Msg(14);
111100120112                   leavesr;
111200120112               endsl;
111300120112
111400120112           EndSl;
111500141202           //?imposto già la directory
111600141202           IF  V2Cdir = *blanks;
111700141202             V2Cdir = %editc(V2Cccm:'X') + '/PI';
111800141202           ENDIF;
111900120112
112000120112         ENDIF;
112100090313
112200090313         // Verifica se variata la schedulazione dell'estrazione
112300090313         if  $ByPass;
112400090313           $ByPass = (ds_SchMm = wSavSch);
112500090313         endif;
112600090313         wSavSch = ds_SchMm;
112700090225
112800090306         //?Codice padre?
112900090225         select;
113000090306           when  V2Cksu  = *zero;
113100090306             errMessage  = *on;
113200090225             errGenerico = *on;
113300090303             PosCurKsu   = *on;
113400090225             V1Dmsg = $Msg(11);
113500090225             leavesr;
113600090306           when  V2Cksu = V2Cccm;
113700090303             V2Dksu = V2Dccm;
113800090225           other;
113900090327             // - Controllo esistenza codice padre come cliente
114000090309             //   in tabella LAC
114100090303             exsr  sr_Padre;
114200090303             if  errGenerico = *on;
114300090226               leavesr;
114400090225             endif;
114500090303             V2Dksu = p_§LACrag;
114600090226         endsl;
114700101103         //?SE codice padre <> da codice cliente?
114800101103         // ?& sono in immissione?
114900141128         IF  V2Cccm <> V2Cksu   and   (*in02 or $copia);
115000101103           // ?=> chiedo se si vogliono riportare i dati del padre?
115100101103           If  W1Ccpy = *blank;
115200101103             $Video  = 'W1';
115300101103             leavesr;
115400101103           EndIf;
115500101103           // ?& il padre ha già fatto estrazioni?
115600101103           If  p_§LACdatela <> *zero;
115700101103             // ?=> riporto le date del padre, proteggo il campo e non controllo?
115800101103             clear wDataI;
115900101103             %subst(wDataI:1:2) = %subst(%editc(p_§LACdatai:'X'):5:2);
116000101103             %subst(wDataI:3:4) = %subst(%editc(p_§LACdatai:'X'):1:4);
116100101103             V2datai = %int(wDataI);
116200101103             clear wDatEla;
116300101103             %subst(wDatEla:1:2) = %subst(%editc(p_§LACdatela:'X'):5:2);
116400101103             %subst(wDatEla:3:4) = %subst(%editc(p_§LACdatela:'X'):1:4);
116500101103             V2datela = %int(wDatEla);
116600101103             ProtDATAI = *on;
116700101103           EndIf;
116800100521         ENDIF;
116900090311
117000090311         //?Tipo elaborazione?
117100090327         select;
117200090327           // - Obbligatorio
117300090327           when  V2Caut <> 'A'  and  V2Caut <> 'S'  and  V2Caut <> 'N';
117400090327             errMessage  = *on;
117500090327             errGenerico = *on;
117600090327             PosCurAut   = *on;
117700090327             V1Dmsg = 'Tipo elaborazione errato o mancante';
117800090327             leavesr;
117900090327         endsl;
118000090226
118100090306         //?Flag nome immagine?
118200090310         clear  V2Dfmi;
118300090310         select;
118400090310           // - Obbligatorietà
118500090310           when  V2Cfmi = *blank;
118600090310             errMessage  = *on;
118700090310             errGenerico = *on;
118800090310             PosCurFmi   = *on;
118900090310             V1Dmsg = 'Flag nome immagine obbligatorio';
119000090310             leavesr;
119100090310           // - Ricerca
119200090310           when  %scan('?' : V2Cfmi) > *zero;
119300090310             clear  TIBS02ds;
119400090310             T02mod = 'R';
119500090310             T02cod = 'NIM';
119600090310             T02sif = KNSIF;
119700090310             TNTBE_RicercaControllo(kpjba : tibs02ds);
119800090310             if  T02err = *blank;
119900090310               V2Cfmi = %subst(T02ke1 : 1 : %len(V2Cfmi));
120000090310               V2Dfmi = T02uni;
120100090310               errGenerico = *on;
120200090310               leavesr;
120300090310             else;
120400090310               errMessage  = *on;
120500090310               errGenerico = *on;
120600090310               PosCurFmi   = *on;
120700090310               V1Dmsg = T02msg;
120800090310               leavesr;
120900090310             endif;
121000090310           // - Controllo
121100090310           other;
121200090310             reset  TIBS02ds;
121300090310             T02cod = 'NIM';
121400090310             T02ke1 = V2Cfmi;
121500090310             TNTBE_RicercaControllo(kpjba : tibs02ds);
121600090310             if  T02err <> *blank;
121700090310               errMessage  = *on;
121800090310               errGenerico = *on;
121900090310               PosCurFmi   = *on;
122000090310               V1Dmsg = 'Flag nome immagine errato';
122100090310               leavesr;
122200090310             endif;
122300090310             V2Dfmi = T02uni;
122400090310         ENDSL;
122500140813
122600140813         //?Tipo immagine?
122700140813         if  V2Ctpi <> 'TIF'   and   V2Ctpi <> 'JPG' and
122800140813             V2Ctpi <> 'PDF';
122900140813           errMessage  = *on;
123000140813           errGenerico = *on;
123100140813           PosCurTpi   = *on;
123200140813           V1Dmsg = 'Tipo immagine errato o mancante';
123300140813           leavesr;
123400140813         endif;
123500090226
123600090306         //?Directory per immagini?
123700090226         select;
123800090306           when  V2Cdir  = *blank;
123900090226             errMessage  = *on;
124000090226             errGenerico = *on;
124100090226             PosCurDir   = *on;
124200090226             V1Dmsg = $Msg(05);
124300090226             leavesr;
124400090316           when  %subst(V2Cdir : 1 : 1) = *blank;
124500090316             errMessage  = *on;
124600090316             errGenerico = *on;
124700090316             PosCurDir   = *on;
124800090316             V1Dmsg = 'Non ammessi spazi vuoti all''inizio del nome +
124900090316                       della directory';
125000090316             leavesr;
125100121217           WHEN  V2Cdir = §LA2dir and V2Cla2 = 'S';
125200121217             errMessage  = *on;
125300121217             errGenerico = *on;
125400121217             PosCurDir   = *on;
125500121217             V1Dmsg = 'La directory deve essere diversa da tab. LA2';
125600121217             leavesr;
125700090306           when  %scan('\':V2Cdir) > *zero;
125800090226             errMessage  = *on;
125900090226             errGenerico = *on;
126000090226             PosCurDir   = *on;
126100090226             V1Dmsg = $Msg(06);
126200090226             leavesr;
126300090306           when  %scan('%':V2Cdir) > *zero   and
126400090306                           V2Caut  = 'A';
126500090226             errMessage  = *on;
126600090226             errGenerico = *on;
126700090226             PosCurDir   = *on;
126800090226             V1Dmsg = $Msg(07);
126900090226             leavesr;
127000090309           when  %scan('&':V2Cdir) > *zero  and
127100090309                           V2Caut  = 'A';
127200090309             errMessage  = *on;
127300090309             errGenerico = *on;
127400090309             PosCurDir   = *on;
127500090309             V1Dmsg = $Msg(07);
127600090309             leavesr;
127700090226         endsl;
127800090309
127900090316         wPos1 = %scan('/' : V2Cdir);
128000090316         DoW  wPos1 > *zero;
128100090316           if  %subst( V2Cdir : wPos1 : 2 ) = '//';
128200090316             errMessage  = *on;
128300090316             errGenerico = *on;
128400090316             PosCurDir   = *on;
128500090316             V1Dmsg = 'Nome directory NON valido: non ammesso il +
128600090316                       doppio "/"';
128700090316             leavesr;
128800090316           else;
128900090316             wPos1 = %scan('/' : V2Cdir : wPos1 + 1 );
129000090316           endif;
129100090316         EndDo;
129200090316
129300090309         wPos1 = %scan('&' : V2Cdir);
129400090309         IF  wPos1 > *zero;
129500090309           if  %subst( V2Cdir : wPos1 : 4 ) <> '&AAA'   and
129600090309               %subst( V2Cdir : wPos1 : 2 ) <> '&M';
129700090309             errMessage  = *on;
129800090309             errGenerico = *on;
129900090309             PosCurDir   = *on;
130000090309             V1Dmsg = 'Ammessi solo "&AAA" e "&M" (come caratteri +
130100090309                       sostituibili)';
130200090309             leavesr;
130300090309           endif;
130400090309           wPos2 = %scan( '&' : V2Cdir : wPos1 + 1 );
130500090309           If  wPos2 > *zero;
130600090309             if  %subst( V2Cdir : wPos2 : 4 ) <> '&AAA'   and
130700090309                 %subst( V2Cdir : wPos2 : 2 ) <> '&M';
130800090309               errMessage  = *on;
130900090309               errGenerico = *on;
131000090309               PosCurDir   = *on;
131100090309               V1Dmsg = 'Ammessi solo "&AAA" e "&M" (come caratteri +
131200090309                         sostituibili)';
131300090309               leavesr;
131400090309             endif;
131500090309           EndIf;
131600090309         ENDIF;
131700090309
131800090309         wPos1 = %scan('&M' : V2Cdir);
131900090309         IF  wPos1 > *zero;
132000090309           wPos2 = %scan( '&M' : V2Cdir : wPos1 + 1 );
132100090309           If  wPos2 > *zero   and
132200090309               %scan( '&M' : V2Cdir : wPos2 + 1 ) > *zero;
132300090309             errMessage  = *on;
132400090309             errGenerico = *on;
132500090309             PosCurDir   = *on;
132600090309             V1Dmsg = 'Ammessi max 2 "&M"';
132700090309             leavesr;
132800090309           EndIf;
132900090309         ENDIF;
133000090226
133100090327         // - Se codice padre <> da codice cliente
133200090306         //   la directory deve coincidere almeno fino alla 2ª subdir
133300090306         IF  V2Cccm <> V2Cksu   and   p_§LACdir <> V2Cdir;
133400090311
133500090311           // - Controllo 1ª directory
133600090311           wPos1 = %scan( '/' : %trim( V2Cdir ) );
133700090311           wPos2 = %scan( '/' : %trim( p_§LACdir ) );
133800090313           If  wPos1 <> wPos2                               OR
133900090313             ( wPos1  > *zero   and   wPos2 > *zero   and
134000090311               %subst( %trim( V2Cdir    ) : 1 : wPos1 ) <>
134100090313               %subst( %trim( p_§LACdir ) : 1 : wPos2 ) )   OR
134200090313             ( wPos1  = *zero   and   wPos2  =  *zero
134300090313                                and   V2Cdir <> p_§LACdir );
134400090311             errMessage  = *on;
134500090311             errGenerico = *on;
134600090311             PosCurDir   = *on;
134700090311             select;
134800090311               when  wPos1 =  *zero   and   wPos2 <> *zero;
134900090316                 V1Dmsg = 'Il padre ha altri rami directory (';
135000090311               when  wPos1 <> *zero   and   wPos2 =  *zero;
135100090311                 V1Dmsg = 'Il padre ha una directory unica (';
135200090311               other;
135300090311                 V1Dmsg = 'Primo ramo directory +
135400090311                           <> da quello del cod. padre (';
135500090311             EndSl;
135600090313             if  %len( %trim( p_§LACdir ) ) <=
135700090311                 %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) ) - 1;
135800090311               V1Dmsg = %trimr( V1Dmsg )
135900090311                      + %trim( p_§LACdir ) + ')';
136000090311             else;
136100090313               wPos1 = %len( V1Dmsg );
136200090313               wPos2 = %len( %trimr( V1Dmsg ) );
136300090311               V1Dmsg = %trimr( V1Dmsg )
136400090311                      + %trim( %subst( p_§LACdir : 1 :
136500090311                          %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) )
136600090311                          - 4 ) )
136700090311                      + '...)';
136800090311             endif;
136900090311             leavesr;
137000090311           EndIf;
137100090311
137200090311           // - Controllo 2ª directory
137300141201           wPos1 = %scan( '/' : %trim( V2Cdir ) : wPos1 + 1 );
137400141201           wPos2 = %scan( '/' : %trim( p_§LACdir ) : wPos2 + 1 );
137500141201           if  wPos1 = *zero;
137600141201             wPos1 = %len( %trimr( V2Cdir ) );
137700090316           else;
137800141201             wPos1 -= 1;
137900090316           endif;
138000141201           if  wPos2 = *zero;
138100141201             wPos2 = %len( %trimr( p_§LACdir ) );
138200090316           else;
138300090316             wPos2 -= 1;
138400090316           endif;
138500090316           If  wPos1 <> wPos2   OR
138600090316               %subst(V2Cdir : 1 : wPos1) <>
138700090316                 %subst(p_§LACdir : 1 : wPos2);
138800090311             errMessage  = *on;
138900090311             errGenerico = *on;
139000090311             PosCurDir   = *on;
139100090313                 V1Dmsg = 'Secondo ramo directory +
139200090311                           <> da quello del cod. padre (';
139300090313             if  %len( %trim( p_§LACdir ) ) <=
139400090311                 %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) ) - 1;
139500090311               V1Dmsg = %trimr( V1Dmsg )
139600090311                      + %trim( p_§LACdir ) + ')';
139700090311             else;
139800090311               V1Dmsg = %trimr( V1Dmsg )
139900090311                      + %trim( %subst( p_§LACdir : 1 :
140000090311                          %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) )
140100090311                          - 4 ) )
140200090311                      + '...)';
140300090311             endif;
140400090311             leavesr;
140500090311           EndIf;
140600090311
140700090306         ENDIF;
140800121016
140900121016         //?Immagine con Merge (LDV + DOC)
141000151217         //?e non sono in ripristino (devo prima ripristinare anche la JDC)
141100151217         IF  V2merge = 'S'
141200151217          and dsp_aid <> c_F05;
141300151217           exsr CTR_merge;
141400121016         ENDIF;
141500121016         IF  ErrGenerico;
141600121016           leavesr;
141700121016         ENDIF;
141800090327
141900090327         //?Tipo porto?
142000090327         if  V2Ctpt <> 'F'   and   V2Ctpt <> 'A'   and   V2Ctpt <> 'E';
142100090327           errMessage  = *on;
142200090327           errGenerico = *on;
142300090327           PosCurTpt   = *on;
142400090327           V1Dmsg = 'Tipo porto errato o mancante';
142500090327           leavesr;
142600090327         endif;
142700090311
142800090311         //?LNP per selezione?
142900090311         if  V2Clnp <> *zero;
143000090311           setll  (V2Clnp)  AZORG;
143100090311           if  NOT  %equal(AZORG01L);
143200090311             errMessage  = *on;
143300090311             errGenerico = *on;
143400090311             PosCurLnp   = *on;
143500090311             V1Dmsg = 'Linea di partenza errata';
143600090311             leavesr;
143700090311           endif;
143800090311         endif;
143900140813
144000140813         //?Tipo data da elaborare?
144100140813         select;
144200140813           when  V2tdta <> ' '  and  V2tdta <> 'C'  and  V2tdta <> 'S';
144300140813             errMessage  = *on;
144400140813             errGenerico = *on;
144500140813             PosCurTdta  = *on;
144600140813             V1Dmsg = '"Flag "Data da elaborare" errato';
144700140813             leavesr;
144800140813           // - Tipo data da elaborare obbligatorio
144900140813           when  V2Caut = 'S'   and   V2tdta = *blank;
145000140813             errMessage  = *on;
145100140813             errGenerico = *on;
145200140813             PosCurTdta  = *on;
145300140813             V1Dmsg = '"Data da elaborare" obbligatoria per +
145400140813                       "Tipo Elaborazione" = "S" (schedulata)';
145500140813             leavesr;
145600140813           // - Se codice padre <> da codice cliente
145700140813           //   il tipo data da elaborare deve essere uguale
145800140813           when  V2Cccm <> V2Cksu   and   p_§LACtdta <> V2tdta;
145900140813             errMessage  = *on;
146000140813             errGenerico = *on;
146100140813             PosCurTdta  = *on;
146200140813             V1Dmsg = 'Tipo data da elaborare +
146300140813                       <> da quello del cod. padre ('
146400140813                    + p_§LACtdta + ')';
146500140813             leavesr;
146600140813         endsl;
146700090309
146800090309         //?Periodo da estrarre in mesi?
146900090310         select;
147000090310           // - Periodo da estrarre in mesi obbligatorio
147100090310           when  V2Caut = 'S'   and   V2mesi = *zero;
147200090310             errMessage  = *on;
147300090310             errGenerico = *on;
147400090310             PosCurMesi  = *on;
147500090310             V1Dmsg = '"Periodo da estrarre" obbligatorio per +
147600090310                       "Tipo Elaborazione" = "S" (schedulata)';
147700090310             leavesr;
147800090310           // - Se codice padre <> da codice cliente
147900090310           //   il periodo da estrarre deve essere uguale
148000090310           when  V2Cccm <> V2Cksu   and   p_§LACmesi <> V2mesi;
148100090310             errMessage  = *on;
148200090310             errGenerico = *on;
148300090310             PosCurMesi  = *on;
148400090310             V1Dmsg = 'Periodo da estrarre +
148500090310                       <> da quello del cod. padre ('
148600090310                    + %editc(p_§LACmesi:'X') + ')';
148700090310             leavesr;
148800090310         endsl;
148900141201
149000141201         //?Prima estrazione dal?
149100141201         select;
149200141201           // - Obbligatoria se tipo elaborazione CD
149300141201           when  V2Caut = 'S'   and   V2datai = *zero;
149400141201             errMessage  = *on;
149500141201             errGenerico = *on;
149600141201             PosCurDatai = *on;
149700141201             V1Dmsg = 'Periodo iniziale della prima estrazione +
149800141201                       obbligatorio';
149900141201             leavesr;
150000141201           // - Verifica correttezza
150100141201           when  V2datai <> *zero                                  AND
150200141201              (%subst( %editc( V2datai : 'X' ) : 1 : 2 ) < '01'   or
150300141201               %subst( %editc( V2datai : 'X' ) : 1 : 2 ) > '12'   or
150400141201               %subst( %editc( V2datai : 'X' ) : 3 : 4 ) < '2000' or
150500141201               %subst( %editc( V2datai : 'X' ) : 3 : 4 ) > %char(*year));
150600141201             errMessage  = *on;
150700141201             errGenerico = *on;
150800141201             PosCurDatai = *on;
150900141201             V1Dmsg = 'Periodo iniziale della prima estrazione +
151000141201                       errato';
151100141201             leavesr;
151200141201         endsl;
151300141201         // - Se codice padre <> da codice cliente
151400141201         //   il periodo deve essere uguale
151500141201         if  V2Cccm <> V2Cksu;
151600141201           clear  wDataI;
151700141201           %subst( wDataI : 1 : 2 ) =
151800141201                   %subst( %editc( p_§LACdatai : 'X' ) : 5 : 2 );
151900141201           %subst( wDataI : 3 : 4 ) =
152000141201                   %subst( %editc( p_§LACdatai : 'X' ) : 1 : 4 );
152100141201           if  V2datai <> %int(wDataI);
152200141201             errMessage  = *on;
152300141201             errGenerico = *on;
152400141201             PosCurDatai = *on;
152500141201             V1Dmsg = 'Periodo iniziale della prima estrazione +
152600141201                       <> da quello del cod. padre ('
152700141201                    + %subst(%editc(p_§LACdatai : 'X') : 5 : 2)
152800141201                    + '/'
152900141201                    + %subst(%editc(p_§LACdatai : 'X') : 1 : 4)
153000141201                    + ')';
153100141201             leavesr;
153200141201           endif;
153300141201         endif;
153400090226
153500090309         //?Schedulazione giorno e mesi x preparazione CD?
153600090226         select;
153700090318           // when (V2Caut  =  'A'    or  V2Caut  =  'N')    AND
153800090318           when  V2Caut  =  'A'    AND
153900130923               // (V2datai <> *zero  or  V2tdta   <> *blank   or
154000130923                (V2mesi  <> *zero  or  V2Cschgg <> *zero    or
154100090311                 ds_SchMm <> *blank);
154200090311             errMessage  = *on;
154300090311             errGenerico = *on;
154400090311             PosCurAut   = *on;
154500090311             V1Dmsg = 'Schedulazione incompatibile con il +
154600090311                       "Tipo Elaborazione" = ' + V2Caut;
154700090311             leavesr;
154800090318           when  V2Caut = 'S'  and  (V2Cschgg <= *zero);
154900090306             errMessage  = *on;
155000090306             errGenerico = *on;
155100090306             PosCurSchGg = *on;
155200090306             V1Dmsg = 'Schedulazione obbligatoria per +
155300090306                       "Tipo Elaborazione" = "S" (schedulata)';
155400090306             leavesr;
155500090306           when  V2Caut = 'S'  and  (ds_SchMm = *blank);
155600090306             errMessage  = *on;
155700090306             errGenerico = *on;
155800090306             PosCurSch01 = *on;
155900090306             V1Dmsg = 'Indicare almeno un mese per +
156000090306                       "Tipo Elaborazione" = "S" (schedulata)';
156100090306             leavesr;
156200090311         endsl;
156300090226
156400090327         // - Se codice padre <> da codice cliente
156500090306         //   i 2 codici devono avere gli stessi dati di estrazione
156600090306         //   gg e mese e n. mesi da elaborare
156700090309         if  V2Cccm <> V2Cksu;
156800090309           select;
156900090309             when  p_§LACschgg <> V2Cschgg;
157000090226               errMessage  = *on;
157100090226               errGenerico = *on;
157200090303               PosCurSchgg = *on;
157300090309               V1Dmsg = 'Giorno creazione CD +
157400090309                         <> da quello del cod. padre ('
157500090309                      + %editc(p_§LACschgg:'X') + ')';
157600090226               leavesr;
157700090303             when  p_SchMm <> ds_SchMm;
157800090226               errMessage  = *on;
157900090226               errGenerico = *on;
158000090226               PosCursch01 = *on;
158100090309               V1Dmsg = 'Mesi di elaborazione +
158200090309                         <> da quelli del cod. padre ('
158300090309                      + p_SchMm + ')';
158400090226               leavesr;
158500090226           endsl;
158600090226         endif;
158700090309
158800090327         // - Verifica congruenza tra mese di schedulazione e
158900090309         //   mesi di estrazione
159000090309         select;
159100090311           // - Nessun dato inserito
159200090310           when  V2mesi   = *zero    and   V2Cschgg = *zero    and
159300090311                 ds_SchMm = *blank;
159400090311           // - Periodo da estrarre in mesi richiesto
159500090310           when  V2mesi = *zero;
159600090310             errMessage  = *on;
159700090310             errGenerico = *on;
159800090310             PosCurMesi  = *on;
159900090310             V1Dmsg = 'Periodo da estrarre obbligatorio SE inserita +
160000090310                       una schedulazione';
160100090310             leavesr;
160200090311           // - Giorno di schedulazione richiesto
160300090310           when  V2Cschgg = *zero;
160400090310             errMessage  = *on;
160500090310             errGenerico = *on;
160600090310             PosCurSchgg = *on;
160700090311             V1Dmsg = $Msg(08);
160800090310             leavesr;
160900090311           // - Selezione mesi richiesta
161000090311           when  ds_SchMm = *blank;
161100090310             errMessage  = *on;
161200090310             errGenerico = *on;
161300090310             PosCurSch01 = *on;
161400090311             V1Dmsg = $Msg(09);
161500090310             leavesr;
161600090311         endsl;
161700090313
161800090313         // - Conteggio/Controllo mesi schedulati
161900090313         clear  wCount;
162000090311         wSchMm_ds = %triml(ds_SchMm);
162100090313         For  xx = 1  To  %elem($SchMm);
162200090313           if  $SchMm(xx) <> *blank;
162300090313             wCount += 1;
162400090313           endif;
162500090313         EndFor;
162600090313
162700090311         select;
162800090313           // - Errori bloccanti:
162900090313           when   wCount <> *zero   and
163000090313                 (V2mesi <> (12 / wCount));
163100090313             errMessage  = *on;
163200090313             errGenerico = *on;
163300090313             PosCurSch01 = *on;
163400090313             V1Dmsg = 'Incongruenza tra il "periodo da estrarre" +
163500090313                       ed i mesi schedulati';
163600090313           // - Errori bypassabili:
163700090313           when ( (V2mesi =  2   and  ($SchMm(03) = *blank     or
163800090311                                       $SchMm(05) = *blank     or
163900090311                                       $SchMm(07) = *blank     or
164000090311                                       $SchMm(09) = *blank     or
164100090311                                       $SchMm(11) = *blank))   or
164200090313                  (V2mesi =  3   and  ($SchMm(04) = *blank     or
164300090311                                       $SchMm(07) = *blank     or
164400090311                                       $SchMm(10) = *blank))   or
164500090313                  (V2mesi =  4   and  ($SchMm(05) = *blank     or
164600090311                                       $SchMm(09) = *blank))   or
164700090313                  (V2mesi =  6   and   $SchMm(07) = *blank) )  AND
164800090313                 $ByPass =  *off;
164900090313             $ByPass = *on;
165000090309             errMessage  = *on;
165100090309             errGenerico = *on;
165200090310             PosCurSch01 = *on;
165300090313             V1Dmsg = 'Mesi schedulati in maniera irregolare... +
165400090313                       Premere Enter per forzare.';
165500090309             leavesr;
165600090313           when ( (V2mesi =  2  and  ($SchMm(02) <> *blank     or
165700090311                                      $SchMm(04) <> *blank     or
165800090311                                      $SchMm(06) <> *blank     or
165900090311                                      $SchMm(08) <> *blank     or
166000090311                                      $SchMm(10) <> *blank     or
166100090311                                      $SchMm(12) <> *blank))   OR
166200090313                  (V2mesi =  3  and  ($SchMm(02) <> *blank     or
166300090311                                      $SchMm(03) <> *blank     or
166400090311                                      $SchMm(05) <> *blank     or
166500090311                                      $SchMm(06) <> *blank     or
166600090311                                      $SchMm(08) <> *blank     or
166700090311                                      $SchMm(09) <> *blank     or
166800090311                                      $SchMm(11) <> *blank     or
166900090311                                      $SchMm(12) <> *blank))   OR
167000090313                  (V2mesi =  4  and  ($SchMm(02) <> *blank     or
167100090311                                      $SchMm(03) <> *blank     or
167200090311                                      $SchMm(04) <> *blank     or
167300090311                                      $SchMm(06) <> *blank     or
167400090311                                      $SchMm(07) <> *blank     or
167500090311                                      $SchMm(08) <> *blank     or
167600090311                                      $SchMm(10) <> *blank     or
167700090311                                      $SchMm(11) <> *blank     or
167800090311                                      $SchMm(12) <> *blank))   OR
167900090313                  (V2mesi =  6  and  ($SchMm(02) <> *blank     or
168000090311                                      $SchMm(03) <> *blank     or
168100090311                                      $SchMm(04) <> *blank     or
168200090311                                      $SchMm(05) <> *blank     or
168300090311                                      $SchMm(06) <> *blank     or
168400090311                                      $SchMm(08) <> *blank     or
168500090311                                      $SchMm(09) <> *blank     or
168600090311                                      $SchMm(10) <> *blank     or
168700090311                                      $SchMm(11) <> *blank     or
168800090311                                      $SchMm(12) <> *blank))   OR
168900090313                  (V2mesi = 12  and  ($SchMm(02) <> *blank     or
169000090311                                      $SchMm(03) <> *blank     or
169100090311                                      $SchMm(04) <> *blank     or
169200090311                                      $SchMm(05) <> *blank     or
169300090311                                      $SchMm(06) <> *blank     or
169400090311                                      $SchMm(07) <> *blank     or
169500090311                                      $SchMm(08) <> *blank     or
169600090311                                      $SchMm(09) <> *blank     or
169700090311                                      $SchMm(10) <> *blank     or
169800090311                                      $SchMm(11) <> *blank     or
169900090313                                      $SchMm(12) <> *blank)) ) AND
170000090313                 $ByPass =  *off;
170100090313             $ByPass = *on;
170200090310             errMessage  = *on;
170300090310             errGenerico = *on;
170400090311             PosCurSch01 = *on;
170500090313             V1Dmsg = 'Mesi schedulati in maniera irregolare... +
170600090313                       Premere Enter per forzare.';
170700090310             leavesr;
170800090313           // - Controllo giorno del mese (bypassabile)
170900090313           when ( (V2Cschgg > 28  and  V2Csch02 <> *blank)  or
171000090313                  (V2Cschgg > 30  and (V2Csch04 <> *blank
171100090313                                   or  V2Csch06 <> *blank
171200090313                                   or  V2Csch09 <> *blank
171300090313                                   or  V2Csch11 <> *blank)) )
171400090313                and  $CtrMm = *off;
171500090313             $CtrMm = *on;
171600090313             errMessage  = *on;
171700090313             errGenerico = *on;
171800090313             PosCurSch01 = (V2Csch01 <> *blank);
171900090313             PosCurSch02 = (V2Csch02 <> *blank);
172000090313             PosCurSch04 = (V2Csch04 <> *blank);
172100090313             PosCurSch06 = (V2Csch06 <> *blank);
172200090313             PosCurSch09 = (V2Csch09 <> *blank);
172300090313             PosCurSch11 = (V2Csch11 <> *blank);
172400090313             V1Dmsg = %replace( %char(V2Cschgg) : $Msg(10) :
172500090313                                %scan('&g':$Msg(10)) );
172600090313             leavesr;
172700090313         endsl;
172800090226
172900090309         //?Frequenza addebito?
173000090310         select;
173100090327           // - Obbligatoria
173200090327           when  V2tadu <> 'I'   and   V2tadu <> 'J'   and
173300090327                 V2tadu <> 'S'   and   V2tadu <> 'M';
173400090327             errMessage  = *on;
173500090327             errGenerico = *on;
173600090327             PosCurTadu  = *on;
173700090327             V1Dmsg = 'Frequenza addebito errata o mancante';
173800090327             leavesr;
173900090327           // - Se tipo elaborazione CD la creazione addebito non può
174000090313           //   essere mensile o settimanale
174100090310           when  V2Caut = 'S'  and  (V2tadu = 'S'  or  V2tadu = 'M');
174200090310             errMessage  = *on;
174300090310             errGenerico = *on;
174400090310             PosCurTadu  = *on;
174500090310             V1Dmsg = 'Per creazione CD non è possibile addebito +
174600090310                       mensile o settimanale';
174700090310             leavesr;
174800090327           // - Se codice padre <> da codice cliente
174900090327           //   devono avere la stessa frequenza addebito
175000090310           when  V2Cccm <> V2Cksu   and   p_§LACtadu <> V2tadu;
175100090310             errMessage  = *on;
175200090310             errGenerico = *on;
175300090310             PosCurTadu  = *on;
175400090310             V1Dmsg = 'Frequenza addebito +
175500090310                        <> da quella del cod. padre ('
175600090310                    + p_§LACtadu + ')';
175700090310             leavesr;
175800090310         endsl;
175900090309
176000090309         //?Tipo addebito (Creazione TITAS)?
176100090310         select;
176200090327           // - Obbligatorio
176300090327           when  V2Ctad <> 'N'  and  V2Ctad <> 'S'  and  V2Ctad <> 'V';
176400090327             errMessage  = *on;
176500090327             errGenerico = *on;
176600090327             PosCurTad   = *on;
176700090327             V1Dmsg = 'Flag "Creazione TITAS" errato o mancante';
176800090327             leavesr;
176900090327           // - Se "N" non impostare i dati dell'addebito forzato
177000090310           when  V2Ctad = 'N'     and
177100090310                (V2Cksc <> *zero  or  V2Cctr <> *blank
177200090310                                  or  V2imp <> *zero);
177300090310             errMessage  = *on;
177400090310             errGenerico = *on;
177500090310             PosCurTad   = *on;
177600090310             V1Dmsg = 'Se NO addebito non impostare le forzature';
177700090310             leavesr;
177800090327           // - Se codice padre <> da codice cliente
177900090310           //   devono avere lo stesso tipo addebito
178000090310           when  V2Cccm <> V2Cksu   and   p_§LACtad <> V2Ctad;
178100090310             errMessage  = *on;
178200090310             errGenerico = *on;
178300090310             PosCurTad   = *on;
178400090310             V1Dmsg = 'Tipo addebito +
178500090310                       <> da quello del cod. padre ('
178600090310                        + p_§LACtad + ')';
178700090310             leavesr;
178800090310         endsl;
178900090226
179000090311         //?Dati da forzare in tassazione:?
179100090312         //?- Codice cliente / Tariffa?
179200090312         clear  TIBS69ds;
179300090312         if  V2Cksc <> *zero;
179400090312           tibs69ds.I69kcc = DUTkci;
179500090312           tibs69ds.I69kac = %int(V2Cksc);
179600090312           tibs69ds.I69sif = knsif;
179700090312           tibs69r(TIBS69ds : ds_CNACO :
179800090312                              ds_CNIND :
179900090312                              ds_CNCLP :
180000090312                              ds_FNCLS);
180100090312         endif;
180200090312         select;
180300090312           when  tibs69ds.O69err = *on;
180400090312             errMessage  = *on;
180500090312             errGenerico = *on;
180600090312             PosCurKsc   = *on;
180700090312             V1Dmsg = 'Cliente tassazione errato';
180800090312             leavesr;
180900090312           when  V2Cksc <> *zero   and   V2Cctr = *blank;
181000090312             errMessage  = *on;
181100090312             errGenerico = *on;
181200090312             PosCurCtr   = *on;
181300090312             V1Dmsg = 'Codice tariffa obbligatorio SE +
181400090312                       inserito il cliente tassazione';
181500090312             leavesr;
181600090311           // - Non ammessa tariffa senza cliente
181700090311           when  V2Cksc =  *zero   and   V2Cctr <> *blank;
181800090311             errMessage  = *on;
181900090311             errGenerico = *on;
182000090311             PosCurKsc   = *on;
182100090311             V1Dmsg = 'Cliente tassazione obbligatorio SE +
182200090311                       inserita la relativa tariffa';
182300090311             leavesr;
182400090311           // - Ammesso solo un codice tariffa numerico
182500090311           when  V2Cctr <> *blank   and
182600090311                 %check(c_Digits : V2Cctr) > *zero;
182700090311             errMessage  = *on;
182800090311             errGenerico = *on;
182900090312             PosCurCtr   = *on;
183000090312             V1Dmsg = 'Ammessi SOLO caratteri numerici';
183100090311             leavesr;
183200090310         endsl;
183300090226
183400090327         // - Se codice padre <> da codice cliente
183500090306         //   stesso ksc/ctr/importo
183600090303         if  V2Cccm <> V2Cksu;
183700090226           select;
183800090303             when  p_§LACksc <> V2Cksc;
183900090226               errMessage  = *on;
184000090226               errGenerico = *on;
184100090303               PosCurKsc   = *on;
184200090309               V1Dmsg = 'Cliente tassazione +
184300090309                         <> da quello del cod. padre ('
184400090309                      + %editc(p_§LACksc : 'X') + ')';
184500090226               leavesr;
184600090303             when  p_§LACctr <> V2Cctr;
184700090226               errMessage  = *on;
184800090226               errGenerico = *on;
184900090303               PosCurCtr   = *on;
185000090309               V1Dmsg = 'Codice tariffa +
185100090309                         <> quello del cod. padre ('
185200090309                      + p_§LACctr + ')';
185300090226               leavesr;
185400090226           endsl;
185500090226         endif;
185600090226
185700090309         //?Importo & Tipo importo forzato?
185800090310         select;
185900090310           // - Non ammesso importo per Varia Negata
186000090310           when  V2Ctad = 'V'   and   V2imp <> *zero;
186100090310             errMessage  = *on;
186200090310             errGenerico = *on;
186300090310             PosCurImp   = *on;
186400090310             V1Dmsg = 'NON ammesso l''importo forzato per +
186500090310                       Varia Negata';
186600090310             leavesr;
186700090310           // - Non ammesso tipo importo senza importo
186800090310           when  V2imp =  *zero   and   V2fimp <> *blank;
186900090310             errMessage  = *on;
187000090310             errGenerico = *on;
187100090310             PosCurImp   = *on;
187200090310             V1Dmsg = 'Importo forzato obbligatorio SE +
187300090310                       inserito il tipo di importo';
187400090310             leavesr;
187500090312           // - Se inserito importo devono essere inseriti:
187600090312           //   · il codice cliente forzato (ksc),
187700090312           //   · la tariffa forzata (ctr) ed
187800090312           //   · il tipo importo (fimp)
187900090310           when  V2imp <> *zero;
188000090310             select;
188100090310               when  V2fimp  = *blank;
188200090310                 errMessage  = *on;
188300090310                 errGenerico = *on;
188400090310                 PosCurFimp  = *on;
188500090310                 V1Dmsg = 'Immettere il tipo importo';
188600090310                 leavesr;
188700090310               when  V2Cksc  = *zero;
188800090310                 errMessage  = *on;
188900090310                 errGenerico = *on;
189000090310                 PosCurKsc   = *on;
189100090310                 V1Dmsg = 'Immettere il cliente forzato';
189200090310                 leavesr;
189300090312               when  V2Cctr  = *blank;
189400090310                 errMessage  = *on;
189500090310                 errGenerico = *on;
189600090310                 PosCurCtr   = *on;
189700090310                 V1Dmsg = 'Immettere la tariffa forzata';
189800090310                 leavesr;
189900090310             endsl;
190000090310         endsl;
190100090312
190200090327         // Verifica compatibilità dell'importo forzato con il
190300090312         //   massimo imponibile IVA in tab. "GEI"/"LAC".
190400090312         if  V2imp <> *zero   and   V2fimp = 'T';
190500090312           // - Reperimento divisa moneta di conto gestionale
190600090312           clear  dGED;
190700090312           reset  TIBS02ds;
190800090312           T02cod = 'GED';
190900090312           T02ke1 = '1';
191000090312           TNTBE_RicercaControllo(kpjba : tibs02ds);
191100090312           if  T02err = *blank;
191200090312             dGED = T02uni;
191300090312           endif;
191400090312           // - Reperimento importi da tab. "GEI"
191500090312           //   (interessa il massimo imponibile IVA per spedizione)
191600090312           clear  dGEI;
191700090312           reset  TIBS02ds;
191800090312           T02cod = 'GEI';
191900090312           T02ke1 = §GEDcn;
192000090312           TNTBE_RicercaControllo(kpjba : tibs02ds);
192100090312           if  T02err = *blank;
192200090312             dGEI = T02uni;
192300090312           endif;
192400090312           if  V2imp > §GEImf;
192500090312             errMessage  = *on;
192600090312             errGenerico = *on;
192700090312             PosCurImp   = *on;
192800090312             V1Dmsg = 'Importo superiore al MASSIMO IMPONIBILE IVA +
192900090312                       PER SPEDIZIONE ('
193000090312                      + %trim( %editc( §GEImf : 'J' ) ) + ')';
193100090312             leavesr;
193200090312           endif;
193300090312         endif;
193400090226
193500090312         // Se codice padre <> da codice cliente
193600090306         //   stesso importo e tipo importo
193700090303         if  V2Cccm <> V2Cksu;
193800090226           select;
193900090303             when  p_§LACimp <> V2imp;
194000090226               errMessage  = *on;
194100090226               errGenerico = *on;
194200090312               PosCurImp   = *on;
194300090309               V1Dmsg = 'Importo forzato +
194400090309                         <> da quello del cod. padre ('
194500090309                      + %editc(p_§LACimp : 'J') + ')';
194600090226               leavesr;
194700090303             when  p_§LACfimp <> V2fimp;
194800090226               errMessage  = *on;
194900090226               errGenerico = *on;
195000090312               PosCurFimp  = *on;
195100090309               V1Dmsg = 'Tipo importo forzato +
195200090309                         <> da quello del cod. padre ('
195300090309                      + p_§LACfimp + ')';
195400090226               leavesr;
195500090226           endsl;
195600090225         endif;
195700090923
195800090923         //?Modifica dati sensibili per l'addebito
195900090923         if (v2tadu <> savtadu or v2ctad <> savtad or
196000090923             v2cksc <> savksc or v2cctr <> savctr or
196100141128             v2imp <> savimp or v2fimp <> savfimp) and
196200141128             (not *in02 and not $Copia)
196300090923             and not $forza;
196400090923               errMessage  = *on;
196500090923               errGenerico = *on;
196600090923               PosCurTadu  = *on;
196700090923               V1Dmsg = 'ATTENZIONE!! Possibili problemi ' +
196800090923                        'in fase di addebito. Avvisare Monica/Nicola';
196900090923               $forza = *on;
197000090923               leavesr;
197100090923         endif;
197200110713
197300110713         //?Creazione file elenco per il cliente
197400110713         //?possibile solo se NON è elaborazione automatica (FTP)
197500110713         IF  V2pgm <> *blanks and V2caut = 'A';
197600110713           ErrMessage  = *on;
197700110713           ErrGenerico = *on;
197800110713           PosCurPgm   = *on;
197900110713           V1Dmsg = 'NON possibile creazione elenco se elaborazione '+
198000110713                    'automatica';
198100110713           leavesr;
198200110713         ENDIF;
198300110713         //?se inserito il nome del programma deve esserci anche il tipo elenco
198400110713         IF  V2pgm <> *blanks and V2ele = *blanks;
198500110713           ErrMessage  = *on;
198600110713           ErrGenerico = *on;
198700110713           PosCurEle   = *on;
198800110713           V1Dmsg = 'Immettere anche il tipo elenco da creare';
198900110713           leavesr;
199000110713         ENDIF;
199100110713         //?e viceversa se c'è il tipo elenco deve esserci anche il nome del pgm
199200110713         IF  V2pgm = *blanks and V2ele <> *blanks;
199300110713           ErrMessage  = *on;
199400110713           ErrGenerico = *on;
199500110713           PosCurPgm   = *on;
199600110713           V1Dmsg = 'Immettere anche il nome del programma x elenco da creare';
199700110713           leavesr;
199800110713         ENDIF;
199900110713         //?Se codice padre <> da codice cliente
200000110713         //?  devono avere gli stessi dati
200100110713         IF  V2Cccm <> V2Cksu;
200200110713           SELECT;
200300110713             WHEN  p_§LACpgm <> V2pgm;
200400110713               ErrMessage  = *on;
200500110713               ErrGenerico = *on;
200600110713               PosCurPgm   = *on;
200700110713               V1Dmsg = 'Nome programma +
200800110713                         <> da quello del cod. padre ('
200900110713                      + p_§LACpgm + ')';
201000110713               leavesr;
201100110713             WHEN  p_§LACele <> V2ele;
201200110713               ErrMessage  = *on;
201300110713               ErrGenerico = *on;
201400110713               PosCurEle   = *on;
201500110713               V1Dmsg = 'Tipo elenco +
201600110713                         <> da quello del cod. padre ('
201700110713                      + p_§LACele + ')';
201800110713               leavesr;
201900110713           ENDSL;
202000110713         ENDIF;
202100141128
202200141128         //?Competenze C/E dichiarate
202300141128         IF  (*in02 or $copia) and V2compce = *blanks;
202400141128           ErrGenerico = *on;
202500141128           ErrMessage  = *on;
202600141128           PosCurCompCE = *on;
202700141128           V1Dmsg = 'Competenze C/E obbligatorie';
202800141128           leavesr;
202900141128         ENDIF;
203000141201         IF  V2compce <> *blanks;
203100141128         clear  V2dcompce;
203200141128         IF  %scan('?' : V2compce) > *zero;
203300141128           clear  TIBS02ds;
203400141128           T02mod = 'R';
203500141128           T02cod = 'CED';
203600141128           T02sif = KNSIF;
203700141128           TNTBE_RicercaControllo(kpjba : tibs02ds);
203800141128           IF  T02err = *blank;
203900141128             V2compce  = T02ke1;
204000141128             V2dcompce = T02uni;
204100141128             errGenerico = *on;
204200141128             leavesr;
204300141128           ELSE;
204400141128             errMessage  = *on;
204500141128             errGenerico = *on;
204600141128             PosCurCompCE = *on;
204700141128             V1Dmsg = T02msg;
204800141128             leavesr;
204900141128           ENDIF;
205000141128         ELSE;
205100141128           reset  TIBS02ds;
205200141128           T02cod = 'CED';
205300141128           T02ke1 = V2compce;
205400141128           T02sif = KNSIF;
205500141128           TNTBE_RicercaControllo(kpjba : tibs02ds);
205600141128           IF  T02err = *blank;
205700141128             V2dcompce = T02uni;
205800141128           ELSE;
205900141128             ErrMessage  = *on;
206000141128             ErrGenerico = *on;
206100141128             PosCurCompCE = *on;
206200141128             V1Dmsg = 'Competenze C/E errate';
206300141128             leavesr;
206400141128           ENDIF;
206500141128         ENDIF;
206600141202         ELSE;
206700141202           V2dcompce = 'NON DICHIARATO';
206800141201         ENDIF;
206900141128         //?Se codice padre <> da codice cliente
207000141128         //?  devono avere gli stessi dati
207100141128         //?  ma solo se sono in immissione/copia
207200141201         //?  e se il padre ha la personalizzazione impostata
207300141128         IF  V2Cccm <> V2Cksu and
207400141201             p_§LACcompce <> V2compce and (*in02 or $copia)
207500141201             and p_§LACcompce <> *blanks;
207600141128           ErrMessage   = *on;
207700141128           ErrGenerico  = *on;
207800141128           PosCurCompCE = *on;
207900141128           V1Dmsg = 'Competenze C/E +
208000141128                     <> da quelle del cod. padre ('
208100141128                   + p_§LACcompce + ')';
208200141128           leavesr;
208300141128         ENDIF;
208400141128
208500141128         //?Tipo Addebito dichiarato
208600141128         clear  V2dtpadd;
208700141128         IF  %scan('?' : V2tpadd) > *zero;
208800141128           clear  TIBS02ds;
208900141128           clear dTAL;
209000141128           T02mod = 'R';
209100141128           T02cod = 'TAL';
209200141128           T02sif = KNSIF;
209300141128           TNTBE_RicercaControllo(kpjba : tibs02ds);
209400141128           IF  T02err = *blank;
209500141128             V2tpadd  = T02ke1;
209600141128             dTAL = T02uni;
209700141128             V2dtpadd = §TALdes;
209800141128             errGenerico = *on;
209900141128             leavesr;
210000141128           ELSE;
210100141128             errMessage  = *on;
210200141128             errGenerico = *on;
210300141128             PosCurTpAdd = *on;
210400141128             V1Dmsg = T02msg;
210500141128             leavesr;
210600141128           ENDIF;
210700141128         ELSE;
210800141202         IF  V2tpadd = *blanks;
210900141202           V2dtpadd = 'NON DICHIARATO';
211000141202         ELSE;
211100141128           reset  TIBS02ds;
211200141128           clear dTAL;
211300141128           T02cod = 'TAL';
211400141128           T02ke1 = V2tpadd;
211500141128           T02sif = KNSIF;
211600141128           TNTBE_RicercaControllo(kpjba : tibs02ds);
211700141128           IF  T02err = *blank;
211800141128             dTAL = T02uni;
211900141128             V2dtpadd = §TALdes;
212000141128           ELSE;
212100141128             ErrMessage  = *on;
212200141128             ErrGenerico = *on;
212300141128             PosCurTpAdd = *on;
212400141128             V1Dmsg = 'Tipo Addebito errato';
212500141128             leavesr;
212600141128           ENDIF;
212700141202         ENDIF;
212800141128         ENDIF;
212900141128         //?Se codice padre <> da codice cliente
213000141128         //?  devono avere gli stessi dati
213100141128         //?  ma solo se sono in immissione/copia
213200141201         //?  e se il padre ha il campo impostato
213300141128         IF  V2Cccm <> V2Cksu and
213400141201             p_§LACtpadd <> V2tpadd and (*in02 or $copia)
213500141201             and p_§LACtpadd <> *blanks;
213600141128           ErrMessage  = *on;
213700141128           ErrGenerico = *on;
213800141128           PosCurTpAdd = *on;
213900141128           V1Dmsg = 'Tipo addebito +
214000141128                     <> da quello del cod. padre ('
214100141128                   + p_§LACtpadd + ')';
214200141128           leavesr;
214300141128         ENDIF;
214400141203
214500141203         //?Data Aut. DG
214600141203         IF  V2dtautdg <> *zeros;
214700141203           exec sql
214800141203           select count(*) into :conta
214900141203           from DATECNV0F
215000141203           where DC_DMYY_Z = :V2dtautdg or DC_DMY_Z = :V2dtautdg;
215100141203           IF  conta = 0;
215200141203             ErrMessage  = *on;
215300141203             ErrGenerico = *on;
215400141203             PosCurAutDg = *on;
215500141203             V1Dmsg = $Msg(15);
215600141203             leavesr;
215700141203           ENDIF;
215800141203           IF  V2dtautdg < 1000000;
215900141203             exec sql
216000141203             set :V2dtautdg = (select DC_DMYY_Z from DATECNV0F
216100141203                               where DC_DMY_Z = :V2dtautdg);
216200141203           ENDIF;
216300141203         ENDIF;
216400071207
216500071207       ENDSR;
216600090225
216700090225       //--------------------------------------------------------------
216800090306       //?Controllo esistenza del codice padre in tabella LAC          ?
216900090225       //--------------------------------------------------------------
217000090303       BEGSR  sr_Padre;
217100090302
217200090327         // Controlla esistenza in tabella "LAC"
217300090310         clear  dLACp;
217400090310         reset  TIBS02ds;
217500090310         T02sif = KNSIF;
217600090310         %subst(T02ke1 : 1 : 7) = %editc(V2Cksu : 'X');
217700090310         TNTBE_RicercaControllo(kpjba : tibs02ds);
217800090310         if  T02err = *blank;
217900090310           dLACp  = T02uni;
218000090310         else;
218100090310           errMessage  = *on;
218200090310           errGenerico = *on;
218300090310           PosCurksu   = *on;
218400090310           V1Dmsg = $Msg(12);
218500090310           leavesr;
218600090310         endif;
218700090327
218800090327         // Controlla se già inserito come figlio di altro padre
218900090327         if  p_§LACksu <> V2Cksu;
219000090327           errMessage  = *on;
219100090327           errGenerico = *on;
219200090327           PosCurKsu   = *on;
219300090327           V1Dmsg = 'Non ammesso cod. PADRE già FIGLIO di altro +
219400090327                     cliente (' + %editc(p_§LACksu : 'X') + ')';
219500090327           leavesr;
219600090327         endif;
219700090225
219800090225       ENDSR;
219900121016
220000121016       //--------------------------------------------------------------
220100121016       //?Controllo Merge.
220200121016       //--------------------------------------------------------------
220300121016       BEGSR  CTR_merge;
220400121016
220500121016       // -?L'immagine se con Merge deve essere di tipo PDF
220600121016         IF  V2Ctpi <> 'PDF';
220700121016           errMessage  = *on;
220800121016           errGenerico = *on;
220900121016           V1Dmsg = 'Immagine con Merge, deve essere di tipo PDF';
221000121016           PosCurTpi = *on;
221100121016           leavesr;
221200121016         ENDIF;
221300121016
221400121016       // -?La directory deve finire con /PF
221500121016         wPos1 = %scan('  ':V2Cdir);
221600121016         wPos1 -= 3;
221700121016         IF  %subst(V2Cdir:wPos1:3) <> '/PF';
221800121016           ErrMessage  = *on;
221900121016           ErrGenerico = *on;
222000121016           PosCurDir   = *on;
222100121016           V1Dmsg = 'Immagine con Merge, l''ultimo ramo della +
222200121016                     DIR deve essere "PF"';
222300121016           leavesr;
222400121016         ENDIF;
222500121016
222600121016       // -?Cerco il corrispondente in JDC
222700121016         clear  TIBS02DS;
222800121016         T02mod = 'C';
222900121016         T02cod = 'JDC';
223000121016         T02ke1 = %editc(V2Cccm:'X');
223100121016         TNTBE_RicercaControllo(kpjba : tibs02ds);
223200121016         // -?Non trovo la tabella
223300121016         IF  T02err <> *blanks;
223400121016           // -?Immissione forzo
223500121016           IF  *in02 and not wForza;
223600121016             wForza = *on;
223700121016             ErrMessage  = *on;
223800121016             ErrGenerico = *on;
223900151130             V1Dmsg = 'Attivare anche la corrispondente +
224000151130                       tabella JDC. Enter per forzare';
224100121016             leavesr;
224200121016           ENDIF;
224300121016         // -?Manutenzione blocco
224400121016           IF  not *in02;
224500121016             ErrMessage  = *on;
224600121016             ErrGenerico = *on;
224700121016             V1Dmsg = 'Manca la corrispondente tabella JDC';
224800121016             leavesr;
224900121016           ENDIF;
225000121016         // -?Trovo la tabella
225100121016         ELSE;
225200121016           dJDC = T02uni;
225300121016         // -?Controllo il tipo immagine
225400121030           IF  V2Ctpi <> §JDCtpi and not wForzaTPI;
225500121030             wForzaTPI = *on;
225600121016             ErrMessage  = *on;
225700121016             ErrGenerico = *on;
225800121016             PosCurTpi   = *on;
225900151130             V1Dmsg = 'Tipo immagine diverso da corrispondente +
226000151130                       tab.JDC --> ' + §JDCtpi + ' Enter per forzare.';
226100121016             leavesr;
226200121016           ENDIF;
226300121016         // -?Controllo il nome immagine
226400121030           IF  V2Cfmi <> §JDCfmi and not wForzaFMI;
226500121030             wForzaFMI = *on;
226600121016             ErrMessage  = *on;
226700121016             ErrGenerico = *on;
226800121016             PosCurFmi   = *on;
226900151130             V1Dmsg = 'Nome immagine diverso da corrispondente +
227000151130                       tab.JDC --> ' + §JDCfmi + ' Enter per forzare.';
227100121016             leavesr;
227200121016           ENDIF;
227300121016         // -?Controllo la directory
227400121016           wPos2 = %scan('  ':§JDCdir);
227500121016           wPos2 -= 3;
227600121016           IF  %subst(V2Cdir:1:wPos1) <>
227700121030               %subst(§JDCdir:1:wPos2) and not wForzaDIR;
227800121030             wForzaDIR = *on;
227900121016             ErrMessage  = *on;
228000121016             ErrGenerico = *on;
228100121016             PosCurDir   = *on;
228200151130             V1Dmsg = 'Directory diversa da corrispondente +
228300151130                       tab.JDC --> ' + %subst(§JDCdir:1:wPos2) +
228400151201                       ' Enter per forzare.';
228500121016             leavesr;
228600121016           ENDIF;
228700121016         ENDIF;
228800121016
228900121016       ENDSR;
229000101103
229100101103       //--------------------------------------------------------------
229200101103       //?Gestione window W01                                          ?
229300101103       //--------------------------------------------------------------
229400101103       BEGSR  sr_GesW01;
229500101103
229600101103         // -?Emissione window?
229700101103         exfmt  TB46W01;
229800101103
229900101103         errMessage  = *off;
230000101103         errGenerico = *off;
230100101103         clear  V1Dmsg;
230200101103
230300101103         $Video = 'D2';
230400101103
230500101103         select;
230600101103           // -?F12=Ritorno?
230700101103           when  dsp_aid = c_F12;
230800101103             leavesr;
230900101103           // -?Invio con "S"?
231000101103           when  W1Ccpy = 'S';
231100101103             if  V2Caut = *blank;
231200101103               V2Caut   = p_§LACaut;
231300101103             endif;
231400101103             if  V2Ctpi = *blank;
231500101103               V2Ctpi   = p_§LACtpi;
231600101103             endif;
231700101103             if  V2Cfmi = *blank;
231800101103               V2Cfmi   = p_§LACfmi;
231900101103               clear  V2Dfmi;
232000101103             endif;
232100121016             if  V2merge = *blank;
232200121016               V2merge  = p_§LACmerge;
232300121016             endif;
232400121217             if  V2Cla2 = *blank;
232500121217               V2Cla2 = p_§LACla2;
232600121217             endif;
232700101103             //if  V2Cdir = *blank;      //? Directory ?
232800101103               V2Cdir   = p_§LACdir;     //? COMUNQUE  ?
232900101103             //endif;                    //? del padre ?
233000101103             if  V2Ctpt = *blank;
233100101103               V2Ctpt   = p_§LACtpt;
233200101103             endif;
233300101103             if  V2Cssr = *blank;
233400101103               V2Cssr   = p_§LACssr;
233500101103             endif;
233600101103             if  V2Clnp = *zero;
233700101103               V2Clnp   = p_§LAClnp;
233800101103             endif;
233900101103             if  V2Cres = *blank;
234000101103               V2Cres   = p_§LACres;
234100101103             endif;
234200101103             if  V2Crec = *blank;
234300101103               V2Crec   = p_§LACrec;
234400101103             endif;
234500101103             if  V2Ccsr = *blank;
234600101103               V2Ccsr   = p_§LACcsr;
234700101103             endif;
234800101103             if  V2tdta = *blank;
234900101103               V2tdta   = p_§LACtdta;
235000101103             endif;
235100101103             if  V2mesi = *zero;
235200101103               V2mesi   = p_§LACmesi;
235300101103             endif;
235400101103             if  V2Cschgg = *zero;
235500101103               V2Cschgg = p_§LACschgg;
235600101103             endif;
235700101103             if  ds_SchMm = *blank;
235800101103               ds_SchMm = p_SchMm;
235900101103             endif;
236000101103             if  V2tadu = *blank;
236100101103               V2tadu   = p_§LACtadu;
236200101103             endif;
236300101103             if  V2Ctad = *blank;
236400101103               V2Ctad   = p_§LACtad;
236500101103             endif;
236600101103             if  V2Cksc = *zero;
236700101103               V2Cksc   = p_§LACksc;
236800101103             endif;
236900101103             if  V2Cctr = *blank;
237000101103               V2Cctr   = p_§LACctr;
237100101103             endif;
237200101103             if  V2imp  = *zero;
237300101103               V2imp    = p_§LACimp;
237400101103             endif;
237500101103             if  V2fimp = *blank;
237600101103               V2fimp   = p_§LACfimp;
237700101103             endif;
237800110713             if  V2pgm  = *blank;
237900110713               V2pgm    = p_§LACpgm;
238000110713             endif;
238100110713             if  V2ele  = *blank;
238200110713               V2ele    = p_§LACele;
238300110713             endif;
238400101103             if  V2note = *blank;
238500101103               V2note   = p_§LACnote;
238600101103             endif;
238700140813             if  V2tpadd = *blank;
238800140813               V2tpadd  = p_§LACtpadd;
238900140813             endif;
239000140813             if  V2compce = *blank;
239100140813               V2compce = p_§LACcompce;
239200140813             endif;
239300101103         endsl;
239400101103
239500101103       ENDSR;
239600121212
239700121212       //--------------------------------------------------------------
239800121217       //?Gestione estensione tabella --> LA2.                       ?
239900121212       //--------------------------------------------------------------
240000121217       BEGSR  GesLA2;
240100121212
240200121217         TNTB464ds = TNTB46ds;
240300121217         B464ke1 = %editc(V2Cccm:'X');
240400121217         clear B464fxx;
240500121217         clear B464err;
240600121217         clear B46msg;
240700121217         tntb464r (kpjba:TNTB464DS);
240800121212
240900121212       ENDSR;
241000151217
241100151217       //--------------------------------------------------------------
241200151217       //?Gestione estensione tabella --> JDC.                       ?
241300151217       //--------------------------------------------------------------
241400151217       BEGSR  GesJDC;
241500151217
241600151217         TNTBJDCds = TNTB46ds;
241700151217         kpjbu = TNTBJDCds;
241800151217         BJDCke1 = %editc(V2Cccm:'X');
241900151217         clear BJDCfxx;
242000151217         clear BJDCerr;
242100151217         clear B46msg;
242200151217         tntbJDCr (kpjba);
242300151217
242400151217       ENDSR;
242500071206
242600071212       //--------------------------------------------------------------
242700090306       //?Aggiornamento record TNTBE00F (tab. LAC)                     ?
242800071212       //--------------------------------------------------------------
242900090306       BEGSR  Upd_TNTBE;
243000071207
243100090306         clear  dLAC;
243200071207         §LACrag   = V2Dccm;
243300071207         §LACtpt   = V2Ctpt;
243400071207         §LACres   = V2Cres;
243500071207         §LACrec   = V2Crec;
243600071207         §LACcsr   = V2Ccsr;
243700071207         §LACssr   = V2Cssr;
243800071207         §LACdir   = V2Cdir;
243900071207         §LACtpi   = V2Ctpi;
244000071207         §LACfmi   = V2Cfmi;
244100121016         §LACmerge = V2merge;
244200121217         §LACla2   = V2Cla2;
244300071207         §LACaut   = V2Caut;
244400090309         §LACtdta  = V2tdta;
244500071207         §LACtad   = V2Ctad;
244600071207         §LACschgg = V2Cschgg;
244700071207         §LACsch01 = V2Csch01;
244800071207         §LACsch02 = V2Csch02;
244900071207         §LACsch03 = V2Csch03;
245000071207         §LACsch04 = V2Csch04;
245100071207         §LACsch05 = V2Csch05;
245200071207         §LACsch06 = V2Csch06;
245300071207         §LACsch07 = V2Csch07;
245400071207         §LACsch08 = V2Csch08;
245500071207         §LACsch09 = V2Csch09;
245600071207         §LACsch10 = V2Csch10;
245700071207         §LACsch11 = V2Csch11;
245800071207         §LACsch12 = V2Csch12;
245900090226         §LACnote  = V2note;
246000090309         §LAClnp   = V2Clnp;
246100090309         §LACksu   = V2Cksu;
246200090309         §LACmesi  = V2mesi;
246300130923         IF  V2caut = 'A';
246400130923           clear §LACdatai;
246500130923           clear §LACdatela;
246600130923         ELSE;
246700090313         §LACdatai = (%int( %subst( %editc( V2datai : 'X') : 3 : 4 ) )
246800090313                      * 100)
246900090313                   + %int( %subst( %editc( V2datai : 'X') : 1 : 2 ) );
247000090310         §LACdatela...
247100090313                   = (%int( %subst( %editc( V2datela : 'X') : 3 : 4 ) )
247200090319                      * 100)
247300090313                    + %int( %subst( %editc( V2datela : 'X') : 1 : 2 ) );
247400130923         ENDIF;
247500090309         §LACtadu  = V2tadu;
247600090309         §LACksc   = V2Cksc;
247700090309         §LACctr   = V2Cctr;
247800090309         §LACfimp  = V2fimp;
247900090309         §LACimp   = V2imp;
248000110713         §LACpgm   = V2pgm;
248100110713         §LACele   = V2ele;
248200140813         §LACtpadd = V2tpadd;
248300140813         §LACcompce= V2compce;
248400140813         §LACutente= knmus;
248500150121         IF  *in02 or $copia;
248600140813           §LACdtaimm= %editc(woggi:'X');
248700141210         ELSE;
248800141210           exec sql
248900141210           set :§LACdtaimm = (select DC_YYMD_C from DATECNV0F
249000141210                              where DC_DMYY_A1 = :V2dtaimm);
249100140813         ENDIF;
249200141203         IF V2dtautdg <> *zeros;
249300141203           exec sql
249400141203           set :§LACdtaudg = (select DC_YYMD_C from DATECNV0F
249500141203                              where DC_DMYY_Z = :V2dtautdg);
249600141203         ENDIF;
249700071207
249800090309         TBEuni    = dLAC;
249900071207
250000090306         clear  TBEftt;
250100090306         clear  TBEftr;
250200071207
250300071207         select;
250400090306           // F5=Ripristino
250500090306           when  dsp_aid = c_F05;
250600090306             clear  TBEatb;
250700090306           // F16=Annullamento
250800090306           when  dsp_aid = c_F16;
250900071207             TBEatb = 'A';
251000071207         endsl;
251100121220
251200121220         TBEdtr = woggi;
251300071207
251400071207         IF  NOT  %found(TNTBE01L);
251500090306           clear  TBEatb;
251600090306           clear  TBEflt;
251700090306           clear  TBEftr;
251800090306           TBEsif = TBXsif;
251900090306           TBElin = TBXlin;
252000090306           TBEapl = TBXapl;
252100090306           TBEcod = k_TBEcod;
252200090306           TBEke1 = k_TBEke1;
252300090306           TBEke2 = k_TBEke2;
252400090306           //_____________
252500071207           WRITE TNTBE000;
252600090306           //¯¯¯¯¯¯¯¯¯¯¯¯¯
252700071207         ELSE;
252800090306           //______________
252900071207           UPDATE TNTBE000;
253000090306           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
253100071207         ENDIF;
253200121213
253300121213
253400121213         // -?Porto a pari anche la tabella LA2
253500121213         SELECT;
253600121213           //?F5=Ripristino tabella
253700121213           WHEN  dsp_aid = c_F05;
253800121213             //?Ripristino anche la LA2 se esiste
253900121213             k_TBEcod = 'LA2';
254000121213             k_TBEke1 = %editc(V2Cccm:'X');
254100121213             clear k_TBEke2;
254200121213             k_TBElin = TBXlin;
254300121213             k_TBEsif = TBXsif;
254400121213             chain  %kds(K05tntbe01) TNTBE01L;
254500121213             IF  %found(TNTBE01L);
254600121213               clear TBEatb;
254700121213               UPDATE  TNTBE000;
254800121213             ENDIF;
254900151217             //?Ripristino anche la JDC se esiste
255000151217             k_TBEcod = 'JDC';
255100151217             k_TBEke1 = %editc(V2Cccm:'X');
255200151217             clear k_TBEke2;
255300151217             k_TBElin = TBXlin;
255400151217             k_TBEsif = TBXsif;
255500151217             chain  %kds(K05tntbe01) TNTBE01L;
255600151217             IF  %found(TNTBE01L);
255700151217               clear TBEatb;
255800151217               UPDATE  TNTBE000;
255900151217             ENDIF;
256000121213           WHEN  dsp_aid = c_F16;
256100121213             //?Annullo anche la LA2 se esiste
256200121213             k_TBEcod = 'LA2';
256300121213             k_TBEke1 = %editc(V2Cccm:'X');
256400121213             clear k_TBEke2;
256500121213             k_TBElin = TBXlin;
256600121213             k_TBEsif = TBXsif;
256700121213             chain  %kds(K05tntbe01) TNTBE01L;
256800121213             IF  %found(TNTBE01L);
256900121213               TBEatb = 'A';
257000121213               UPDATE  TNTBE000;
257100121213             ENDIF;
257200151217             //?Annullo anche la JDC se esiste
257300151217             k_TBEcod = 'JDC';
257400151217             k_TBEke1 = %editc(V2Cccm:'X');
257500151217             clear k_TBEke2;
257600151217             k_TBElin = TBXlin;
257700151217             k_TBEsif = TBXsif;
257800151217             chain  %kds(K05tntbe01) TNTBE01L;
257900151217             IF  %found(TNTBE01L);
258000151217               TBEatb = 'A';
258100151217               UPDATE  TNTBE000;
258200151217             ENDIF;
258300121213           OTHER;
258400121213           //?Modifica
258500121217           //?se viene tolto il flag si altra direcotry controllo se esiste rcd LA2
258600121213           //?se esiste cancello
258700121217             IF  §LACla2 = *blanks;
258800121213               k_TBEcod = 'LA2';
258900121213               k_TBEke1 = %editc(V2Cccm:'X');
259000121213               clear k_TBEke2;
259100121213               k_TBElin = TBXlin;
259200121213               k_TBEsif = TBXsif;
259300121213               chain  %kds(K05tntbe01) TNTBE01L;
259400121213               IF  %found(TNTBE01L);
259500121213                DELETE  TNTBE000;
259600121213               ENDIF;
259700121213             ENDIF;
259800151217           //?se viene tolto il flag si merge controllo se esiste rcd JDC
259900151217           //?se esiste cancello
260000151217             IF  §LACmerge = *blanks;
260100151217               k_TBEcod = 'JDC';
260200151217               k_TBEke1 = %editc(V2Cccm:'X');
260300151217               clear k_TBEke2;
260400151217               k_TBElin = TBXlin;
260500151217               k_TBEsif = TBXsif;
260600151217               chain  %kds(K05tntbe01) TNTBE01L;
260700151217               IF  %found(TNTBE01L);
260800151217                DELETE  TNTBE000;
260900151217               ENDIF;
261000151217             ENDIF;
261100121213         ENDSL;
261200071207
261300071207       ENDSR;
261400071207
261500071212       //--------------------------------------------------------------
261600090306       //?Operazioni finali.                                           ?
261700071212       //--------------------------------------------------------------
261800090306       BEGSR  sr_RoutEnd;
261900071207
262000090306         clear  TIBS69ds;
262100090304         tibs69ds.I69tla = 'C';
262200090304         tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
262300071207
262400071217         kpjbu = TNTB46ds;
262500071207         return;
262600071207
262700071207       ENDSR;
262800090306
262900071207      /end-free
263000071207
263100071212       //--------------------------------------------------------------
263200090306       //?Schiere a tempo di compilazione.                             ?
263300071212       //--------------------------------------------------------------
263400071207
263500071207** - $MSG -------------------------------------------------------------------*
263600071207Immettere il codice cliente                                                     1
263700071207Immettere SOLO caratteri numerici                                               2
263800071207Codice errato                                                                   3
263900071207Cliente annullato                                                               4
264000071207Immettere la directory                                                          5
264100071207Carattere "\" non valido; per indicare una sottocartella utilizzare "/"         6
264200071210Caratteri & e %  non ammessi per la directory di clienti con invio automatico   7
264300071207Indicare il giorno del mese se specificato almeno un mese                       8
264400071207Specificare almeno un mese se indicato il giorno del mese                       9
264500071207Per i mesi con n° di giorni < &g verrà assunto il loro ultimo giorno -> Enter  10
264600090225Inserire il codice padre                                                       11
264700090225Il codice padre deve essere = al codice cliente o = a cliente già in tabella   12
264800120112Cliente da copiare NON più reperibile in tab. "LAC"                            13
264900120112Cliente già inserito in tab. "LAC"                                             14
265000141203Data errata                                                                    15
