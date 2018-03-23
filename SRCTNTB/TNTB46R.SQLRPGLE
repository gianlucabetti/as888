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
029800180301     d sav_LACmerge    s                   like(§LACmerge)
029900180301     d sav_JDCmerge    s                   like(§JDCmerge)
030000071207
030100090306       //--------------------------------------------------------------
030200090306       //?Definizione key-list.                                        ?
030300090306       //--------------------------------------------------------------
030400090306
030500090306       // - File TNTBE01L
030600090306     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
030700090306     d                                     prefix(k_)  inz
030800071206
030900090306       //--------------------------------------------------------------
031000090306       //?Riepilogo indicatori.                                        ?
031100090306       //--------------------------------------------------------------
031200090306       // 01    - RECORD annullato
031300090306       // 02    - acceso IMMISSIONE - spento MANUTENZIONE
031400090306       // 25    - Comodo
031500090306       // 28    - Emissione del messaggio di errore a video
031600090306       // 40    - Protezione campi in prima estrazione
031700090306       // x D01:
031800090306       // 50    - Codice cliente errato o mancante
031900090306       // x D02:
032000120112       // 41    - Copia in D02: V2CCCM non protetto
032100090306       // 53    - Directory per immagini       errata
032200090306       // 54    - Giorno di schedulazione CD   errato
032300090306       // 55    - Selezione mesi obbligatoria
032400090306       // 56    - Selezione del mese di febbraio
032500090306       // 57    - Selezione del mese di aprile
032600090306       // 58    - Selezione del mese di giugno
032700090306       // 59    - Selezione del mese di settembre
032800090306       // 60    - Selezione del mese di novembre
032900090306       // 61    - Codice Padre                 errato
033000090306       // 62    - N. mesi da estrarre          errato
033100090306       // 63    - Tipo data di elaborazione    errato
033200090306       // 64    - Tipo addebito                errato
033300090306       // 65    - Creazione addebito           errato
033400090306       // 66    - Cliente tassazione           errato
033500090306       // 67    - Codice tariffa               errato
033600090306       // 68    - Importo forzato              errato
033700090306       // 69    - Tipo importo forzato         errato
033800090306       // 70    - Flag nome immagine           errato
033900090306       // 71    - Prima elaborazione           errata
034000090306       // 99    - Generico di errore
034100090306       //--------------------------------------------------------------
034200071206
034300071206     c     *Entry        plist
034400071206     c                   parm                    KPJBA
034500071207
034600071207      /free
034700071207
034800071207       // Operazioni iniziali
034900090306       exsr  sr_RoutInz;
035000071207
035100071207       // Gestione video
035200090306       DOW  $Fine = *off;
035300071207         select;
035400090306           when  $Video = 'D2';
035500090306             exsr  sr_GesD02;
035600101103           when  $Video = 'W1';
035700101103             exsr  sr_GesW01;
035800071207           other;
035900071207             leave;
036000071207         endsl;
036100071207       ENDDO;
036200071207
036300071207       // Operazioni finali
036400090306       exsr  sr_RoutEnd;
036500071206
036600071212       //--------------------------------------------------------------
036700090306       //?Operazioni iniziali.                                         ?
036800071212       //--------------------------------------------------------------
036900090306       BEGSR  sr_RoutInz;
037000141106
037100141106         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
037200090306
037300090306         *inLR = *on;
037400071207
037500090306         if  kpjbu <> *blank;
037600071217           TNTB46ds = kpjbu;
037700071207         else;
037800090306           clear  TNTB46ds;
037900071207         endif;
038000071217         B46fxx = *blank;
038100071217         B46err = *off;
038200071217         B46msg = *blank;
038300071207
038400090306         // Reperimento dati job
038500090306         exsr  DatiJob;
038600071207
038700090306         // Impostazione nome programma a video
038800071207         V1Tpgm = SDSpgm;
038900090306
039000090306         // Aggancio dati generali della tabella in esame
039100090306         clear  k_TBEcod;
039200090306         k_TBEke1 = *zero;
039300090306         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
039400090306                = c_Tab;
039500090306         clear  k_TBEke2;
039600090306         clear  k_TBElin;
039700090306         k_TBEsif = KNSIF;
039800090306         chain(n)  %kds(k05tntbe01)  TNTBE000;
039900090306         if  not  %found(TNTBE01L);
040000090306           clear  k_TBEsif;
040100090306           chain(n)  %kds(k05tntbe01)  TNTBE000;
040200090306         endif;
040300090306         if  %found(TNTBE01L);
040400090306           xTNTBEds = TNTBEds;
040500090306         else;
040600090306           clear  xTNTBEds;
040700090306         endif;
040800071207
040900090306         // Verifica parametri ricecvuti
041000090306         if  B46ke1  <> *blank;
041100141117           $Video  = 'D2';
041200141117           $InzD02 = *on;
041300071207         endif;
041400121220
041500121220         // Imposto la data di oggi
041600121220         woggi = %dec(%date());
041700071207
041800071207       ENDSR;
041900071206
042000071212       //--------------------------------------------------------------
042100090306       //?Reperimento Dati del job (Utente/Operativi).                 ?
042200071212       //--------------------------------------------------------------
042300090306       BEGSR  DatiJob;
042400071207
042500071207         in(E) §AzUte;
042600071207         if NOT %error;
042700071207           in(E) §DatiUte;
042800071207         endif;
042900090306         if %error or RSut = *blank;
043000071207           clear TIBS34ds;
043100071207           tibs34r(tibs34ds);
043200071207           in §AzUte;
043300071207           in §DatiUte;
043400071207         endif;
043500071207
043600071207       ENDSR;
043700141117
043800141117       //--------------------------------------------------------------
043900141117       //?Decodifica cliente passato.                                  ?
044000141117       //--------------------------------------------------------------
044100141117       BEGSR  Decodifica;
044200141117
044300141117         $Copia = (B46opz = '3');
044400141117
044500141117         IF  B46ke1 = *blanks;
044600141117           leavesr;
044700141117         ENDIF;
044800141117
044900141117         If  Not  $Copia;
045000141117           clear  TIBS69ds;
045100141117           tibs69ds.I69kcc = DUTkci;
045200141117           tibs69ds.I69kac = %int(B46ke1);
045300141117           tibs69ds.I69sif = knsif;
045400141117           tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
045500141117           if  tibs69ds.O69err = *on;
045600141117             errMessage  = *on;
045700141117             V1Dmsg = $Msg(03);
045800141117             leavesr;
045900141117           endif;
046000141117         EndIf;
046100141117
046200141117         // - Verifica esistenza codice cliente in tabella
046300141117         reset  TNTBE000;
046400141117         k_TBEke1 = B46ke1;
046500141117         clear  k_TBEke2;
046600141117         k_TBElin = TBXlin;
046700141117         k_TBEsif = TBXsif;
046800141117         chain  %kds(K05tntbe01) TNTBE000;
046900141117         if  NOT  %found(TNTBE01L);
047000141117           errMessage  = *on;
047100141117           V1Dmsg = 'Cliente non presente in tabella';
047200141117           leavesr;
047300141117         endif;
047400141117
047500141117         dLAC = TBEuni;
047600141117
047700141117       ENDSR;
047800071206
047900071212       //--------------------------------------------------------------
048000090306       //?Gestione videata D02                                         ?
048100071212       //--------------------------------------------------------------
048200090306       BEGSR  sr_GesD02;
048300071207
048400090306         // Inizializzazione videata
048500090306         if   $InzD02 = *on;
048600090306           exsr  sr_InzD02;
048700090306           $InzD02 = *off;
048800071207         endif;
048900071207
049000090306         // Emissione testata
049100090306         write  TN46T01;
049200071207
049300090306         // Emissione videata
049400151130         if  B46opz = '5';
049500141117           write  TN46D02;
049600141117           exfmt  PROTECT;
049700141117         else;
049800090306           exfmt  TN46D02;
049900071217         endif;
050000071207         errMessage  = *off;
050100071207         errGenerico = *off;
050200090306         clear  V1Dmsg;
050300071207
050400071207         SELECT;
050500090306           // F3=Fine
050600090306           WHEN  dsp_aid = c_F03;
050700141117             exsr  sr_F03D02;
050800090319           // F04=Interrogazione Padre
050900090319           WHEN  dsp_aid = c_F04;
051000090319             exsr  sr_F04D02;
051100141205           // F08=Vis. tabella LA2
051200141205           WHEN  dsp_aid = c_F08;
051300141205             exsr  sr_F08D02;
051400151217           // F09=Vis. tabella JDC
051500151217           WHEN  dsp_aid = c_F09;
051600151217             exsr  sr_F09D02;
051700090306           // F12=Ritorno
051800090306           WHEN  dsp_aid = c_F12;
051900090306             exsr  sr_F12D02;
052000090306           // Enter o F5 o F6 o F16
052100071207           OTHER;
052200090306             // - Controllo dati
052300090306             if  dsp_aid = c_F16;
052400090306               exsr  sr_CtrAnnull;
052500090306             else;
052600090306               exsr  sr_CtrD02;
052700090306             endif;
052800090306             if  errGenerico;
052900071207               leavesr;
053000071207             endif;
053100121212
053200090306             // - Conferma dati:
053300090306             //   F5=Ripristino; F6=Conferma; F16=Annullamento
053400090306             if  dsp_aid = c_F05  or
053500090306                 dsp_aid = c_F06  or
053600090306                 dsp_aid = c_F16;
053700121212
053800090306               exsr  Upd_TNTBE;
053900121213
054000121217           //?Se F6 e immagine da duplicare su altra direcotru devo andare su tabella LA2
054100121217               IF  dsp_aid = c_F06 and V2Cla2 = 'S';
054200121217                 exsr GESLA2;
054300121213                 $Video = 'D2';
054400121213                 $InzD02 = *on;
054500121213                 chain  %kds(K05tntbe01) TNTBE000;
054600121213                 IF  B464err <> '0' and B464err <> *blanks;
054700121213                   ErrMessage  = *on;
054800121213                   ErrGenerico = *on;
054900121213                   V1Dmsg = B464msg;
055000121213                   leavesr;
055100121213                 ENDIF;
055200121213                 unlock TNTBE01L;
055300121213               ENDIF;
055400151217
055500151217           //?Se F6 e immagine da duplicare su altra direcotru devo andare su tabella JDC
055600151217               IF  dsp_aid = c_F06 and V2Merge = 'S';
055700151217                 exsr GESJDC;
055800151217                 $Video = 'D2';
055900151217                 $InzD02 = *on;
056000151217                 chain  %kds(K05tntbe01) TNTBE000;
056100151217                 IF  BJDCerr <> '0' and BJDCerr <> *blanks;
056200151217                   ErrMessage  = *on;
056300151217                   ErrGenerico = *on;
056400151217                   V1Dmsg = BJDCmsg;
056500151217                   leavesr;
056600151217                 ENDIF;
056700151217                 unlock TNTBE01L;
056800151217               ENDIF;
056900121212
057000071207               if  NOT  errGenerico;
057100141117                 $Fine  = *on;
057200071207               endif;
057300071207             EndIf;
057400071207
057500071207         ENDSL;
057600071207
057700071207       ENDSR;
057800071206
057900071212       //--------------------------------------------------------------
058000090306       //?Inizializzazione videata D01                                 ?
058100071212       //--------------------------------------------------------------
058200090306       BEGSR  sr_InzD02;
058300071207
058400090306         reset  $CtrMm;
058500090313         reset  $ByPass;
058600121016         reset  wForza;
058700121030         reset  wForzaTPI;
058800121030         reset  wForzaFMI;
058900121030         reset  wForzaDIR;
059000180301         clear  sav_LACmerge;
059100180301         clear  sav_JDCmerge;
059200071207
059300101103         clear  TB46W01;
059400101103
059500090306         clear  TN46D02;
059600071210
059700090306         IndDspF = *zero;
059800141201         k_TBEcod = c_Tab;
059900141117
060000141117         exsr Decodifica;
060100141117         if  errMessage = *on;
060200141117           B46err = *on;
060300141117           B46msg = V1Dmsg;
060400141117           $Fine  = *on;
060500141117           leavesr;
060600141117         endif;
060700090309
060800090309         // Impostazione indicatori per abilitazione tasti funzionali
060900090309         exsr  Set_Ind_D02;
061000090309
061100120112         // - Codice estrazione (chiave)
061200141117         if  Not  $Copia and B46ke1 <> *blanks;
061300141117           V2Cccm = %int(B46ke1);
061400141117           V2Dccm = ds_CNACO.ACOrag;
061500141117           InserCCM2 = *off;
061600120112         endif;
061700141117
061800141117         IF  $Copia;
061900141117           V2Cksu = %int(B46ke1);
062000141117           V2Dksu = ds_CNACO.ACOrag;
062100141117           InserCCM2 = *on;
062200141117         ENDIF;
062300141117
062400141117         // Impostazione dati di record nuovo
062500141117         IF  B46ke1 = *blanks;
062600141117           InserCCM2 = *on;
062700141117           V1Tlav = 'IMMISSIONE';
062800141117           *in02 = *on;
062900141117           clear dLA2;
063000151217           clear dJDC;
063100150122           exec sql
063200150122           set :V2dtaimm = (select DC_DMYY_A1 from DATECNV0F
063300150122                            where DC_YYMD_Z = :woggi);
063400141117           leavesr;
063500141117         ENDIF;
063600071207
063700090306         IF  %found(TNTBE01L);
063800071207
063900090306           // Impostazione dati di record trovato
064000071217           select;
064100141117             when  B46opz = '2';
064200141117               V1Tlav = 'VARIAZIONE';
064300090306             when  B46opz = '5';
064400071217               V1Tlav = 'VISUALIZZA';
064500151130             when  B46opz = '7';
064600151130               V1Tlav = 'RIPRISTINO';
064700120112             when  $Copia;
064800120112               V1Tlav = '  COPIA   ';
064900071217             other;
065000071217               V1Tlav = 'ANNULLATO ';
065100071217           endsl;
065200090225
065300090306           // Codice padre
065400090306           V2Cksu = §LACksu;
065500090309           clear  TIBS69ds;
065600090309           clear  ds_CNACO;
065700090309           clear  ds_CNIND;
065800090309           clear  ds_CNCLP;
065900090309           clear  ds_FNCLS;
066000090310           select;
066100090310             when  §LACksu =  V2Cccm;
066200090310               V2Dksu = V2Dccm;
066300090310             when  §LACksu <> *zero;
066400090310               tibs69ds.I69kcc = DUTkci;
066500090310               tibs69ds.I69kac = %int(V2Cksu);
066600090310               tibs69ds.I69sif = knsif;
066700090310               tibs69r(TIBS69ds:ds_CNACO:ds_CNIND:ds_CNCLP:ds_FNCLS);
066800090310               if tibs69ds.O69err = *on;
066900090310                 V1Dmsg = $Msg(03);
067000090310                 errMessage  = *on;
067100090310                 errGenerico = *on;
067200090310                 PosCurKsu   = *on;
067300090310                 leavesr;
067400090310               endif;
067500090310               V2Dksu = ds_CNACO.ACOrag;
067600090310           endsl;
067700090225
067800090225           V2Caut   = §LACaut;
067900161025           if §LACAut = 'S';
068000161025             VisDVD = *on;
068100161025           else;
068200161025             VisDVD = *off;
068300161025           endif;
068400161025           V2CFiD   = §LACFiD;
068500161025           V2CCpy   = §LACCpy;
068600161025           V2CDst   = §LACDst;
068700090309           V2Ctpi   = §LACtpi;
068800090225
068900090306           // Flag nome immagine
069000090306           V2Cfmi = §LACfmi;
069100090310           reset  TIBS02ds;
069200090303           T02cod = 'NIM';
069300090303           T02ke1 = §LACfmi;
069400090304           TNTBE_RicercaControllo(kpjba : tibs02ds);
069500090306           if  T02err = *blank;
069600090303             V2Dfmi = T02uni;
069700090303           endif;
069800121016
069900180301           sav_LACmerge = §LACmerge;
070000121016           V2merge = §LACmerge;
070100121217           V2Cla2  = §LACla2;
070200090225
070300090309           V2Cdir   = §LACdir;
070400090309           V2Ctpt   = §LACtpt;
070500090309           V2Cssr   = §LACssr;
070600090309           V2Clnp   = §LAClnp;
070700090309           V2Cres   = §LACres;
070800090309           V2Crec   = §LACrec;
070900090309           V2Ccsr   = §LACcsr;
071000090309
071100090309           // Prima estrazione
071200090309           clear  wDataI;
071300090309           if  §LACdatai > *zero;
071400090309             %subst(wDataI:1:2) = %subst(%editc(§LACdatai:'X'):5:2);
071500090309             %subst(wDataI:3:4) = %subst(%editc(§LACdatai:'X'):1:4);
071600090310             V2datai = %int(wDataI);
071700090309           endif;
071800090309
071900090309           // Ultima estrazione
072000090309           clear  wDatEla;
072100090309           if  §LACdatela > *zero;
072200090309             %subst(wDatEla:1:2) = %subst(%editc(§LACdatela:'X'):5:2);
072300090309             %subst(wDatEla:3:4) = %subst(%editc(§LACdatela:'X'):1:4);
072400090310             V2datela = %int(wDatEla);
072500090309           endif;
072600090309
072700090309           V2tdta   = §LACtdta;
072800090309           V2mesi   = §LACmesi;
072900071207           V2Cschgg = §LACschgg;
073000071207           V2Csch01 = §LACsch01;
073100071207           V2Csch02 = §LACsch02;
073200071207           V2Csch03 = §LACsch03;
073300071207           V2Csch04 = §LACsch04;
073400071207           V2Csch05 = §LACsch05;
073500071207           V2Csch06 = §LACsch06;
073600071207           V2Csch07 = §LACsch07;
073700071207           V2Csch08 = §LACsch08;
073800071207           V2Csch09 = §LACsch09;
073900071207           V2Csch10 = §LACsch10;
074000071207           V2Csch11 = §LACsch11;
074100071207           V2Csch12 = §LACsch12;
074200090225
074300090306           V2tadu   = §LACtadu;
074400090309           V2Ctad   = §LACtad;
074500090306           V2cksc   = §LACksc;
074600090306           V2cctr   = §LACctr;
074700090306           V2imp    = §LACimp;
074800090306           V2fimp   = §LACfimp;
074900110713           V2pgm    = §LACpgm;
075000110713           V2ele    = §LACele;
075100090306           V2note   = §LACnote;
075200140813
075300140813           IF  §LACdtaimm <> *blanks;
075400140813             V2dtaimm = %subst(§LACdtaimm:7:2) + '/' +
075500140813                        %subst(§LACdtaimm:5:2) + '/' +
075600140813                        %subst(§LACdtaimm:1:4);
075700140813           ENDIF;
075800150122
075900150122           IF  $Copia and V2dtaimm = *blanks;
076000150122             exec sql
076100150122             set :V2dtaimm = (select DC_DMYY_A1 from DATECNV0F
076200150122                              where DC_YYMD_Z = :woggi);
076300150122           ENDIF;
076400141128
076500140813           V2utente = §LACutente;
076600141128
076700140813           V2compce = §LACcompce;
076800141128           reset  TIBS02ds;
076900141128           T02cod = 'CED';
077000141128           T02ke1 = V2compce;
077100141128           T02sif = KNSIF;
077200141128           TNTBE_RicercaControllo(kpjba : tibs02ds);
077300141128           IF  T02err = *blank;
077400141128             V2dcompce = T02uni;
077500141128           ENDIF;
077600141202           IF  V2compce = *blanks;
077700141202             V2dcompce = 'NON DICHIARATO';
077800141202           ENDIF;
077900141202
078000141128           IF V2Cccm <> V2Cksu;
078100141128             clear dLACp;
078200141128             reset  TIBS02ds;
078300141128             T02sif = KNSIF;
078400141128             T02cod = 'LAC';
078500141128             T02ke1 = %editc(V2Cksu:'X');
078600141128             TNTBE_RicercaControllo(kpjba : tibs02ds);
078700141128             IF  T02err = *blank;
078800141128               dLACp = T02uni;
078900141128             ENDIF;
079000141128             reset  TIBS02ds;
079100141128             T02cod = 'CED';
079200141128             T02ke1 = p_§LACcompce;
079300141128             T02sif = KNSIF;
079400141128             TNTBE_RicercaControllo(kpjba : tibs02ds);
079500141128             IF  T02err = *blank;
079600141128               V2dceksu = T02uni;
079700141128             ENDIF;
079800141128             VisCEksu = *on;
079900141128           ELSE;
080000141128             VisCEksu = *off;
080100141128           ENDIF;
080200141128
080300140813           V2tpadd  = §LACtpadd;
080400140813           reset  TIBS02ds;
080500141128           clear dTAL;
080600140813           T02cod = 'TAL';
080700140813           T02ke1 = V2tpadd;
080800140813           T02sif = KNSIF;
080900140813           TNTBE_RicercaControllo(kpjba : tibs02ds);
081000140813           IF  T02err = *blank;
081100141128             dTAL = T02uni;
081200141128           ENDIF;
081300141128           V2dtpadd = §TALdes;
081400141202           IF  V2tpadd = *blanks;
081500141202             V2dtpadd = 'NON DICHIARATO';
081600141202           ENDIF;
081700090923
081800090923           *in02 = *off;
081900121217
082000121217           // Cerco i dati della tabella LA2
082100121217           clear dLA2;
082200121217           reset  TIBS02ds;
082300121217           IF  V2Cla2 = 'S';
082400121217             T02cod = 'LA2';
082500121217             T02ke1 = B464ke1;
082600121217             TNTBE_RicercaControllo(kpjba : tibs02ds);
082700121217             IF  T02err = *blank;
082800121217               dLA2 = T02uni;
082900121217             ENDIF;
083000121217           ENDIF;
083100151217
083200151217           // Cerco i dati della tabella JDC
083300151217           clear dJDC;
083400151217           reset  TIBS02ds;
083500151217           IF  V2Merge = 'S';
083600151217             T02cod = 'JDC';
083700151217             T02ke1 = BJDCke1;
083800151217             TNTBE_RicercaControllo(kpjba : tibs02ds);
083900151217             IF  T02err = *blank;
084000151217               dJDC = T02uni;
084100151217             ENDIF;
084200180301             sav_JDCmerge = §JDCmerge;
084300151217           ENDIF;
084400071207
084500071207         ENDIF;
084600090923
084700090923         // Salvo i dati sensibili per la creazione addebito
084800090923         savtad = v2ctad;
084900090923         savtadu = v2tadu;
085000090923         savksc = v2cksc;
085100090923         savctr = v2cctr;
085200090923         savimp = v2imp;
085300090923         savfimp = v2fimp;
085400090923         $forza = *off;
085500130201
085600130201         // Imposto (se c'è) data ultima variazione
085700130201         clear V2datauv;
085800130201         IF  TBEdtr > 0;
085900130201           V2datauv = %subst(%editc(TBEdtr:'X'):7:2) + '/' +
086000130201                      %subst(%editc(TBEdtr:'X'):5:2) + '/' +
086100130201                      %subst(%editc(TBEdtr:'X'):1:4);
086200130201         ENDIF;
086300141128
086400141128         // Cerco la personalizzazione C/E del KSC
086500141203         exec sql
086600141203         select EVDitr into :wceksc
086700141203         FROM ecevd00f
086800141203         WHERE EVDcli = :V2cccm and
086900141203               EVDvoc = 199 and
087000141203               EVDttv = 'K' and
087100141203               EVDddt <= :woggi and
087200141203               EVDdst >= :woggi;
087300141203         IF  sqlcod = 100 or sqlcod < 0;
087400141203           V2ceksc = 'STD';
087500141203         ELSE;
087600141203           V2ceksc = %editw(wceksc:'0 ,   ');
087700141203         ENDIF;
087800141128
087900141128         // Cerco la personalizzazione C/E del KSU
088000141203         IF  VisCeKsu;
088100141203           exec sql
088200141203           select EVDitr into :wceksu
088300141203           FROM ecevd00f
088400141203           WHERE EVDcli = :V2Cksu and
088500141203                 EVDvoc = 199 and
088600141203                 EVDttv = 'K' and
088700141203                 EVDddt <= :woggi and
088800141203                 EVDdst >= :woggi;
088900141203           IF  sqlcod = 100 or sqlcod < 0;
089000141203             V2ceksu = 'STD';
089100141203           ELSE;
089200141203             V2ceksu = %editw(wceksu:'0 ,   ');
089300141203           ENDIF;
089400141203         ENDIF;
089500141203
089600141203         // Data AUT DG per c/e  a 0 o NO ddt
089700141203         IF  §LACdtaudg <> *blanks;
089800141203           exec sql
089900141203           set :V2dtautdg =
090000141203               (select DC_DMYY_Z from DATECNV0F
090100141203                where DC_YYMD_C = :§LACdtaudg);
090200141203         ENDIF;
090300071207
090400071207       ENDSR;
090500071217
090600071217       //--------------------------------------------------------------
090700090306       //?Settaggio indicatori nella videata D02 per abilitazione      ?
090800090306       //?  tasti funzionali                                           ?
090900071217       //--------------------------------------------------------------
091000090306       BEGSR  Set_Ind_D02;
091100090319
091200141204         F4Attivo  = B46opz <> '5';
091300090306         F5Attivo  = ((%found(TNTBE01L) and TBEatb <> *blank)
091400071217                                        and B46opz <> '5');
091500071217         F6Attivo  = (NOT F5Attivo and B46opz <> '5');
091600141205         F8Attivo  = B46opz = '5' and §LACla2 = 'S';
091700151217         F9Attivo  = B46opz = '5' and §LACmerge = 'S';
091800090306         F16Attivo = ((%found(TNTBE01L) and TBEatb = *blank)
091900151130                                        and B46opz <> '5');
092000090306
092100120112         // - Se ultima estrazione impostata proteggo la prima estrazione
092200090309         ProtDATAI = (§LACdatela <> *zero);
092300071217
092400071217       ENDSR;
092500141117
092600141117       //--------------------------------------------------------------
092700141117       //?Gestione tasto funzionale F3 da videata D02                  ?
092800141117       //?F3=Fine                                                      ?
092900141117       //--------------------------------------------------------------
093000141117       BEGSR  sr_F03D02;
093100141117
093200141117         // Chiusura del programma
093300141117         $Fine = *on;
093400141117
093500141117         // Restituzione indicazione del tasto funzionale premuto
093600141117         B46fxx = '1';
093700141117
093800141117       ENDSR;
093900090319
094000090319       //--------------------------------------------------------------
094100090319       //?Gestione tasto funzionale F04 da videata D02                 ?
094200090319       //?F04=Interrogazione Padre                                     ?
094300090319       //--------------------------------------------------------------
094400090319       BEGSR  sr_F04D02;
094500090319
094600090319         // Richiamo pgm interrogazione
094700090327         clear  Param01;
094800090327         P01ord = '1';
094900090327         P01ksu = V2Cksu;
095000090327         KPJBU  = Param01;
095100141204         tntb461r (KPJBA : P01opz3);
095200141204         Param01 = kpjbu;
095300141204         IF  V2Cksu = *zeros and P01ke1 <> *zeros and
095400141204             P01ke1 <> *blanks;
095500141204           V2Cksu = %dec(P01ke1:7:0);
095600141204         ENDIF;
095700090319
095800090319       ENDSR;
095900090319
096000141205       //--------------------------------------------------------------
096100141205       //?Gestione tasto funzionale F08 da videata D02                 ?
096200141205       //?F08=Vis. Tabella LA2
096300141205       //--------------------------------------------------------------
096400141205       BEGSR  sr_F08D02;
096500141205
096600141205         // Richiamo pgm tab. LA2
096700141205         exsr GESLA2;
096800141205
096900141205         // Devo reimpostare i dati per visualizzare nuovamente la tab.LAC
097000141205         $Video = 'D2';
097100141205         $InzD02 = *on;
097200141205         chain  %kds(K05tntbe01) TNTBE000;
097300141205
097400141205         // Se torna errore da tabella LA2 lo emetto
097500141205         IF  B464err <> '0' and B464err <> *blanks;
097600141205           ErrMessage  = *on;
097700141205           ErrGenerico = *on;
097800141205           V1Dmsg = B464msg;
097900141205           leavesr;
098000141205         ENDIF;
098100141205
098200141205       ENDSR;
098300151217
098400151217       //--------------------------------------------------------------
098500151217       //?Gestione tasto funzionale F09 da videata D02                 ?
098600151217       //?F09=Vis. Tabella JDC
098700151217       //--------------------------------------------------------------
098800151217       BEGSR  sr_F09D02;
098900151217
099000151217         // Richiamo pgm tab. LA2
099100151217         exsr GESJDC;
099200151217
099300151217         // Devo reimpostare i dati per visualizzare nuovamente la tab.LAC
099400151217         $Video = 'D2';
099500151217         $InzD02 = *on;
099600151217         chain  %kds(K05tntbe01) TNTBE000;
099700151217
099800151217         // Se torna errore da tabella JDC lo emetto
099900151217         IF  BJDCerr <> '0' and BJDCerr <> *blanks;
100000151217           ErrMessage  = *on;
100100151217           ErrGenerico = *on;
100200151217           V1Dmsg = BJDCmsg;
100300151217           leavesr;
100400151217         ENDIF;
100500151217
100600151217       ENDSR;
100700141205
100800071217       //--------------------------------------------------------------
100900090306       //?Gestione tasto funzionale F12 da videata D02                 ?
101000090306       //?F12=Ritorno                                                  ?
101100071217       //--------------------------------------------------------------
101200090306       BEGSR  sr_F12D02;
101300071217
101400071217           B46fxx = '2';
101500071217           $Fine = *on;
101600071217
101700071217       ENDSR;
101800090306
101900090306       //--------------------------------------------------------------
102000090306       //?Controllo "annullabilità" del record (padre senza figli)     ?
102100090306       //--------------------------------------------------------------
102200090306       BEGSR  sr_CtrAnnull;
102300090306
102400090306         IndDspF = *zero;
102500090306         // REimpostazione indicatori per abilitazione tasti funzionali
102600090306         exsr  Set_Ind_D02;
102700090306
102800090306         if  V2Cksu <= *zero;
102900090306           V2Cksu = §LACksu;
103000090306         endif;
103100090306
103200090306         // Se figlio (non padre): fine controlli
103300090306         if  V2Cccm <> V2Cksu;
103400090306           leavesr;
103500090306         endif;
103600090306
103700090306         //?Verifica se padre con figli?
103800090327         clear  dLACp;
103900090316         SAVke1 = TBEke1;
104000090306         setll     %kds(k05tntbe01 : 1)  TNTBE000;
104100090306         reade(n)  %kds(k05tntbe01 : 1)  TNTBE000;
104200090306
104300090306         dow  not  %eof(TNTBE01L);
104400090316           if  TBEatb = *blank   and
104500090316               %editc(V2Cccm : 'X') <> %trimr(TBEke1);
104600090306             dLACp = TBEuni;
104700090316             if  p_§LACksu = V2Cccm;
104800090306               errMessage  = *on;
104900090306               errGenerico = *on;
105000090306               PosCurKsu   = *on;
105100090306               V1Dmsg = 'Record NON cancellabile: trattasi di PADRE +
105200090306                         con figli (vedi ' + %trimr(TBEke1) + ')';
105300090306               leavesr;
105400090306             endif;
105500090306           endif;
105600090306         reade(n)  %kds(k05tntbe01 : 1)  TNTBE000;
105700090306         enddo;
105800090316
105900120112         // - Riaggancio del rec. in manutenzione/copia
106000090316         TBEke1 = SAVke1;
106100090316         reset  TNTBE000;
106200090316         k_TBEcod = c_Tab;
106300120112         k_TBEke1 = %editc( V2Cccm : 'X' );
106400090316         clear  k_TBEke2;
106500090316         k_TBElin = TBXlin;
106600090316         k_TBEsif = TBXsif;
106700090316         chain  %kds(K05tntbe01) TNTBE000;
106800090306
106900090306       ENDSR;
107000071206
107100071212       //--------------------------------------------------------------
107200090306       //?Controllo videata D02                                        ?
107300071212       //--------------------------------------------------------------
107400090306       BEGSR  sr_CtrD02;
107500071207
107600090309         %subst(IndDspF : 50) = *off;
107700120112
107800141117         IF  $Copia or *in02;
107900120112
108000120112           Select;
108100120112
108200120112             // -?Codice estrazione (SE copia)?
108300120112             When  V2Cccm = *zero;
108400120112               errMessage  = *on;
108500120112               errGenerico = *on;
108600120112               PosCurCcm2  = *on;
108700120112               V1Dmsg = $Msg(01);
108800120112               leavesr;
108900120112
109000120112             // -?Controllo / Decodifica?
109100151130             Other;
109200151130             //il test seguente rimosso immagino fosse fatto per velocizzare ma nel secondo giro
109300151130             //impediva i test
109400151130             //When  tibs69ds.I69kac <> V2Cccm  or
109500151130             //      k_TBEke1 <> %editc( V2Cccm : 'X' );
109600120112               clear  TIBS69ds;
109700120112               tibs69ds.I69kcc = DUTkci;
109800120112               tibs69ds.I69kac = V2Cccm;
109900120112               tibs69ds.I69sif = knsif;
110000120112               tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
110100120112               if  tibs69ds.O69err = *on;
110200120112                 errMessage  = *on;
110300120112                 errGenerico = *on;
110400120112                 PosCurCcm2  = *on;
110500120112                 V1Dmsg = $Msg(03);
110600120112                 leavesr;
110700120112               endif;
110800120112               V2Dccm = ds_CNACO.ACOrag;
110900120112               // -?Verifica esistenza in tabella?
111000120112               k_TBEke1 = %editc( V2Cccm : 'X' );
111100120112               chain  %kds(K05tntbe01) TNTBE000;
111200120112               select;
111300120112                 // - -?NON ammesso nuovo record per cliente annullato?
111400120112                 when  NOT  %found(TNTBE01L)  and  ds_CNACO.ACOflg <> *blank;
111500120112                   errMessage  = *on;
111600120112                   errGenerico = *on;
111700120112                   PosCurCcm2  = *on;
111800120112                   V1Dmsg = $Msg(04);
111900120112                   leavesr;
112000120112                 // - -?Impossibile inserimento di record già esistente?
112100120112                 when  %found(TNTBE01L);
112200120112                   errMessage  = *on;
112300120112                   errGenerico = *on;
112400120112                   PosCurCcm2  = *on;
112500120112                   V1Dmsg = $Msg(14);
112600120112                   leavesr;
112700120112               endsl;
112800120112
112900120112           EndSl;
113000141202           //?imposto già la directory
113100141202           IF  V2Cdir = *blanks;
113200141202             V2Cdir = %editc(V2Cccm:'X') + '/PI';
113300141202           ENDIF;
113400120112
113500120112         ENDIF;
113600090313
113700090313         // Verifica se variata la schedulazione dell'estrazione
113800090313         if  $ByPass;
113900090313           $ByPass = (ds_SchMm = wSavSch);
114000090313         endif;
114100090313         wSavSch = ds_SchMm;
114200090225
114300090306         //?Codice padre?
114400090225         select;
114500090306           when  V2Cksu  = *zero;
114600090306             errMessage  = *on;
114700090225             errGenerico = *on;
114800090303             PosCurKsu   = *on;
114900090225             V1Dmsg = $Msg(11);
115000090225             leavesr;
115100090306           when  V2Cksu = V2Cccm;
115200090303             V2Dksu = V2Dccm;
115300090225           other;
115400090327             // - Controllo esistenza codice padre come cliente
115500090309             //   in tabella LAC
115600090303             exsr  sr_Padre;
115700090303             if  errGenerico = *on;
115800090226               leavesr;
115900090225             endif;
116000090303             V2Dksu = p_§LACrag;
116100090226         endsl;
116200101103         //?SE codice padre <> da codice cliente?
116300101103         // ?& sono in immissione?
116400141128         IF  V2Cccm <> V2Cksu   and   (*in02 or $copia);
116500101103           // ?=> chiedo se si vogliono riportare i dati del padre?
116600101103           If  W1Ccpy = *blank;
116700101103             $Video  = 'W1';
116800101103             leavesr;
116900101103           EndIf;
117000101103           // ?& il padre ha già fatto estrazioni?
117100101103           If  p_§LACdatela <> *zero;
117200101103             // ?=> riporto le date del padre, proteggo il campo e non controllo?
117300101103             clear wDataI;
117400101103             %subst(wDataI:1:2) = %subst(%editc(p_§LACdatai:'X'):5:2);
117500101103             %subst(wDataI:3:4) = %subst(%editc(p_§LACdatai:'X'):1:4);
117600101103             V2datai = %int(wDataI);
117700101103             clear wDatEla;
117800101103             %subst(wDatEla:1:2) = %subst(%editc(p_§LACdatela:'X'):5:2);
117900101103             %subst(wDatEla:3:4) = %subst(%editc(p_§LACdatela:'X'):1:4);
118000101103             V2datela = %int(wDatEla);
118100101103             ProtDATAI = *on;
118200101103           EndIf;
118300100521         ENDIF;
118400090311
118500090311         //?Tipo elaborazione?
118600090327         select;
118700090327           // - Obbligatorio
118800090327           when  V2Caut <> 'A'  and  V2Caut <> 'S'  and  V2Caut <> 'N';
118900090327             errMessage  = *on;
119000090327             errGenerico = *on;
119100090327             PosCurAut   = *on;
119200090327             V1Dmsg = 'Tipo elaborazione errato o mancante';
119300090327             leavesr;
119400090327         endsl;
119500161025
119600161025         //?Dati invio DVD
119700161025         if V2CAut = 'S';
119800161025             VisDVD = *on;
119900161025           // imposto la filiale invio DVD con la filiale del cliente di raggruppamento
120000161025           if  V2CFiD = 0;
120100161025             V2CFiD = %dec(
120200161025                     %subst(%editc(V2CKsU:'X') : 1 : 3)
120300161025                     : 3 : 0);
120400161025           endif;
120500161025           // imposto il nr.copie a 1
120600161025           if  V2CCpy = 0;
120700161025             V2CCpy = 1;
120800161025           endif;
120900161025         else;
121000161025           VisDVD = *off;
121100161025           V2CFiD = 0;
121200161025           V2CCpy = 0;
121300161025           V2CDst = *blank;
121400161025         endif;
121500161025
121600090306         //?Flag nome immagine?
121700090310         clear  V2Dfmi;
121800090310         select;
121900090310           // - Obbligatorietà
122000090310           when  V2Cfmi = *blank;
122100090310             errMessage  = *on;
122200090310             errGenerico = *on;
122300090310             PosCurFmi   = *on;
122400090310             V1Dmsg = 'Flag nome immagine obbligatorio';
122500090310             leavesr;
122600090310           // - Ricerca
122700090310           when  %scan('?' : V2Cfmi) > *zero;
122800090310             clear  TIBS02ds;
122900090310             T02mod = 'R';
123000090310             T02cod = 'NIM';
123100090310             T02sif = KNSIF;
123200090310             TNTBE_RicercaControllo(kpjba : tibs02ds);
123300090310             if  T02err = *blank;
123400090310               V2Cfmi = %subst(T02ke1 : 1 : %len(V2Cfmi));
123500090310               V2Dfmi = T02uni;
123600090310               errGenerico = *on;
123700090310               leavesr;
123800090310             else;
123900090310               errMessage  = *on;
124000090310               errGenerico = *on;
124100090310               PosCurFmi   = *on;
124200090310               V1Dmsg = T02msg;
124300090310               leavesr;
124400090310             endif;
124500090310           // - Controllo
124600090310           other;
124700090310             reset  TIBS02ds;
124800090310             T02cod = 'NIM';
124900090310             T02ke1 = V2Cfmi;
125000090310             TNTBE_RicercaControllo(kpjba : tibs02ds);
125100090310             if  T02err <> *blank;
125200090310               errMessage  = *on;
125300090310               errGenerico = *on;
125400090310               PosCurFmi   = *on;
125500090310               V1Dmsg = 'Flag nome immagine errato';
125600090310               leavesr;
125700090310             endif;
125800090310             V2Dfmi = T02uni;
125900090310         ENDSL;
126000140813
126100140813         //?Tipo immagine?
126200140813         if  V2Ctpi <> 'TIF'   and   V2Ctpi <> 'JPG' and
126300140813             V2Ctpi <> 'PDF';
126400140813           errMessage  = *on;
126500140813           errGenerico = *on;
126600140813           PosCurTpi   = *on;
126700140813           V1Dmsg = 'Tipo immagine errato o mancante';
126800140813           leavesr;
126900140813         endif;
127000090226
127100090306         //?Directory per immagini?
127200090226         select;
127300090306           when  V2Cdir  = *blank;
127400090226             errMessage  = *on;
127500090226             errGenerico = *on;
127600090226             PosCurDir   = *on;
127700090226             V1Dmsg = $Msg(05);
127800090226             leavesr;
127900090316           when  %subst(V2Cdir : 1 : 1) = *blank;
128000090316             errMessage  = *on;
128100090316             errGenerico = *on;
128200090316             PosCurDir   = *on;
128300090316             V1Dmsg = 'Non ammessi spazi vuoti all''inizio del nome +
128400090316                       della directory';
128500090316             leavesr;
128600121217           WHEN  V2Cdir = §LA2dir and V2Cla2 = 'S';
128700121217             errMessage  = *on;
128800121217             errGenerico = *on;
128900121217             PosCurDir   = *on;
129000121217             V1Dmsg = 'La directory deve essere diversa da tab. LA2';
129100121217             leavesr;
129200090306           when  %scan('\':V2Cdir) > *zero;
129300090226             errMessage  = *on;
129400090226             errGenerico = *on;
129500090226             PosCurDir   = *on;
129600090226             V1Dmsg = $Msg(06);
129700090226             leavesr;
129800090306           when  %scan('%':V2Cdir) > *zero   and
129900090306                           V2Caut  = 'A';
130000090226             errMessage  = *on;
130100090226             errGenerico = *on;
130200090226             PosCurDir   = *on;
130300090226             V1Dmsg = $Msg(07);
130400090226             leavesr;
130500090309           when  %scan('&':V2Cdir) > *zero  and
130600090309                           V2Caut  = 'A';
130700090309             errMessage  = *on;
130800090309             errGenerico = *on;
130900090309             PosCurDir   = *on;
131000090309             V1Dmsg = $Msg(07);
131100090309             leavesr;
131200090226         endsl;
131300090309
131400090316         wPos1 = %scan('/' : V2Cdir);
131500090316         DoW  wPos1 > *zero;
131600090316           if  %subst( V2Cdir : wPos1 : 2 ) = '//';
131700090316             errMessage  = *on;
131800090316             errGenerico = *on;
131900090316             PosCurDir   = *on;
132000090316             V1Dmsg = 'Nome directory NON valido: non ammesso il +
132100090316                       doppio "/"';
132200090316             leavesr;
132300090316           else;
132400090316             wPos1 = %scan('/' : V2Cdir : wPos1 + 1 );
132500090316           endif;
132600090316         EndDo;
132700090316
132800090309         wPos1 = %scan('&' : V2Cdir);
132900090309         IF  wPos1 > *zero;
133000090309           if  %subst( V2Cdir : wPos1 : 4 ) <> '&AAA'   and
133100090309               %subst( V2Cdir : wPos1 : 2 ) <> '&M';
133200090309             errMessage  = *on;
133300090309             errGenerico = *on;
133400090309             PosCurDir   = *on;
133500090309             V1Dmsg = 'Ammessi solo "&AAA" e "&M" (come caratteri +
133600090309                       sostituibili)';
133700090309             leavesr;
133800090309           endif;
133900090309           wPos2 = %scan( '&' : V2Cdir : wPos1 + 1 );
134000090309           If  wPos2 > *zero;
134100090309             if  %subst( V2Cdir : wPos2 : 4 ) <> '&AAA'   and
134200090309                 %subst( V2Cdir : wPos2 : 2 ) <> '&M';
134300090309               errMessage  = *on;
134400090309               errGenerico = *on;
134500090309               PosCurDir   = *on;
134600090309               V1Dmsg = 'Ammessi solo "&AAA" e "&M" (come caratteri +
134700090309                         sostituibili)';
134800090309               leavesr;
134900090309             endif;
135000090309           EndIf;
135100090309         ENDIF;
135200090309
135300090309         wPos1 = %scan('&M' : V2Cdir);
135400090309         IF  wPos1 > *zero;
135500090309           wPos2 = %scan( '&M' : V2Cdir : wPos1 + 1 );
135600090309           If  wPos2 > *zero   and
135700090309               %scan( '&M' : V2Cdir : wPos2 + 1 ) > *zero;
135800090309             errMessage  = *on;
135900090309             errGenerico = *on;
136000090309             PosCurDir   = *on;
136100090309             V1Dmsg = 'Ammessi max 2 "&M"';
136200090309             leavesr;
136300090309           EndIf;
136400090309         ENDIF;
136500090226
136600090327         // - Se codice padre <> da codice cliente
136700090306         //   la directory deve coincidere almeno fino alla 2ª subdir
136800090306         IF  V2Cccm <> V2Cksu   and   p_§LACdir <> V2Cdir;
136900090311
137000090311           // - Controllo 1ª directory
137100090311           wPos1 = %scan( '/' : %trim( V2Cdir ) );
137200090311           wPos2 = %scan( '/' : %trim( p_§LACdir ) );
137300090313           If  wPos1 <> wPos2                               OR
137400090313             ( wPos1  > *zero   and   wPos2 > *zero   and
137500090311               %subst( %trim( V2Cdir    ) : 1 : wPos1 ) <>
137600090313               %subst( %trim( p_§LACdir ) : 1 : wPos2 ) )   OR
137700090313             ( wPos1  = *zero   and   wPos2  =  *zero
137800090313                                and   V2Cdir <> p_§LACdir );
137900090311             errMessage  = *on;
138000090311             errGenerico = *on;
138100090311             PosCurDir   = *on;
138200090311             select;
138300090311               when  wPos1 =  *zero   and   wPos2 <> *zero;
138400090316                 V1Dmsg = 'Il padre ha altri rami directory (';
138500090311               when  wPos1 <> *zero   and   wPos2 =  *zero;
138600090311                 V1Dmsg = 'Il padre ha una directory unica (';
138700090311               other;
138800090311                 V1Dmsg = 'Primo ramo directory +
138900090311                           <> da quello del cod. padre (';
139000090311             EndSl;
139100090313             if  %len( %trim( p_§LACdir ) ) <=
139200090311                 %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) ) - 1;
139300090311               V1Dmsg = %trimr( V1Dmsg )
139400090311                      + %trim( p_§LACdir ) + ')';
139500090311             else;
139600090313               wPos1 = %len( V1Dmsg );
139700090313               wPos2 = %len( %trimr( V1Dmsg ) );
139800090311               V1Dmsg = %trimr( V1Dmsg )
139900090311                      + %trim( %subst( p_§LACdir : 1 :
140000090311                          %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) )
140100090311                          - 4 ) )
140200090311                      + '...)';
140300090311             endif;
140400090311             leavesr;
140500090311           EndIf;
140600090311
140700090311           // - Controllo 2ª directory
140800141201           wPos1 = %scan( '/' : %trim( V2Cdir ) : wPos1 + 1 );
140900141201           wPos2 = %scan( '/' : %trim( p_§LACdir ) : wPos2 + 1 );
141000141201           if  wPos1 = *zero;
141100141201             wPos1 = %len( %trimr( V2Cdir ) );
141200090316           else;
141300141201             wPos1 -= 1;
141400090316           endif;
141500141201           if  wPos2 = *zero;
141600141201             wPos2 = %len( %trimr( p_§LACdir ) );
141700090316           else;
141800090316             wPos2 -= 1;
141900090316           endif;
142000090316           If  wPos1 <> wPos2   OR
142100090316               %subst(V2Cdir : 1 : wPos1) <>
142200090316                 %subst(p_§LACdir : 1 : wPos2);
142300090311             errMessage  = *on;
142400090311             errGenerico = *on;
142500090311             PosCurDir   = *on;
142600090313                 V1Dmsg = 'Secondo ramo directory +
142700090311                           <> da quello del cod. padre (';
142800090313             if  %len( %trim( p_§LACdir ) ) <=
142900090311                 %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) ) - 1;
143000090311               V1Dmsg = %trimr( V1Dmsg )
143100090311                      + %trim( p_§LACdir ) + ')';
143200090311             else;
143300090311               V1Dmsg = %trimr( V1Dmsg )
143400090311                      + %trim( %subst( p_§LACdir : 1 :
143500090311                          %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) )
143600090311                          - 4 ) )
143700090311                      + '...)';
143800090311             endif;
143900090311             leavesr;
144000090311           EndIf;
144100090311
144200090306         ENDIF;
144300121016
144400121016         //?Immagine con Merge (LDV + DOC)
144500151217         //?e non sono in ripristino (devo prima ripristinare anche la JDC)
144600151217         IF  V2merge = 'S'
144700151217          and dsp_aid <> c_F05;
144800151217           exsr CTR_merge;
144900121016         ENDIF;
145000121016         IF  ErrGenerico;
145100121016           leavesr;
145200121016         ENDIF;
145300090327
145400090327         //?Tipo porto?
145500090327         if  V2Ctpt <> 'F'   and   V2Ctpt <> 'A'   and   V2Ctpt <> 'E';
145600090327           errMessage  = *on;
145700090327           errGenerico = *on;
145800090327           PosCurTpt   = *on;
145900090327           V1Dmsg = 'Tipo porto errato o mancante';
146000090327           leavesr;
146100090327         endif;
146200090311
146300090311         //?LNP per selezione?
146400090311         if  V2Clnp <> *zero;
146500090311           setll  (V2Clnp)  AZORG;
146600090311           if  NOT  %equal(AZORG01L);
146700090311             errMessage  = *on;
146800090311             errGenerico = *on;
146900090311             PosCurLnp   = *on;
147000090311             V1Dmsg = 'Linea di partenza errata';
147100090311             leavesr;
147200090311           endif;
147300090311         endif;
147400140813
147500140813         //?Tipo data da elaborare?
147600140813         select;
147700140813           when  V2tdta <> ' '  and  V2tdta <> 'C'  and  V2tdta <> 'S';
147800140813             errMessage  = *on;
147900140813             errGenerico = *on;
148000140813             PosCurTdta  = *on;
148100140813             V1Dmsg = '"Flag "Data da elaborare" errato';
148200140813             leavesr;
148300140813           // - Tipo data da elaborare obbligatorio
148400140813           when  V2Caut = 'S'   and   V2tdta = *blank;
148500140813             errMessage  = *on;
148600140813             errGenerico = *on;
148700140813             PosCurTdta  = *on;
148800140813             V1Dmsg = '"Data da elaborare" obbligatoria per +
148900140813                       "Tipo Elaborazione" = "S" (schedulata)';
149000140813             leavesr;
149100140813           // - Se codice padre <> da codice cliente
149200140813           //   il tipo data da elaborare deve essere uguale
149300140813           when  V2Cccm <> V2Cksu   and   p_§LACtdta <> V2tdta;
149400140813             errMessage  = *on;
149500140813             errGenerico = *on;
149600140813             PosCurTdta  = *on;
149700140813             V1Dmsg = 'Tipo data da elaborare +
149800140813                       <> da quello del cod. padre ('
149900140813                    + p_§LACtdta + ')';
150000140813             leavesr;
150100140813         endsl;
150200090309
150300090309         //?Periodo da estrarre in mesi?
150400090310         select;
150500090310           // - Periodo da estrarre in mesi obbligatorio
150600090310           when  V2Caut = 'S'   and   V2mesi = *zero;
150700090310             errMessage  = *on;
150800090310             errGenerico = *on;
150900090310             PosCurMesi  = *on;
151000090310             V1Dmsg = '"Periodo da estrarre" obbligatorio per +
151100090310                       "Tipo Elaborazione" = "S" (schedulata)';
151200090310             leavesr;
151300090310           // - Se codice padre <> da codice cliente
151400090310           //   il periodo da estrarre deve essere uguale
151500090310           when  V2Cccm <> V2Cksu   and   p_§LACmesi <> V2mesi;
151600090310             errMessage  = *on;
151700090310             errGenerico = *on;
151800090310             PosCurMesi  = *on;
151900090310             V1Dmsg = 'Periodo da estrarre +
152000090310                       <> da quello del cod. padre ('
152100090310                    + %editc(p_§LACmesi:'X') + ')';
152200090310             leavesr;
152300090310         endsl;
152400141201
152500141201         //?Prima estrazione dal?
152600141201         select;
152700141201           // - Obbligatoria se tipo elaborazione CD
152800141201           when  V2Caut = 'S'   and   V2datai = *zero;
152900141201             errMessage  = *on;
153000141201             errGenerico = *on;
153100141201             PosCurDatai = *on;
153200141201             V1Dmsg = 'Periodo iniziale della prima estrazione +
153300141201                       obbligatorio';
153400141201             leavesr;
153500141201           // - Verifica correttezza
153600141201           when  V2datai <> *zero                                  AND
153700141201              (%subst( %editc( V2datai : 'X' ) : 1 : 2 ) < '01'   or
153800141201               %subst( %editc( V2datai : 'X' ) : 1 : 2 ) > '12'   or
153900141201               %subst( %editc( V2datai : 'X' ) : 3 : 4 ) < '2000' or
154000141201               %subst( %editc( V2datai : 'X' ) : 3 : 4 ) > %char(*year));
154100141201             errMessage  = *on;
154200141201             errGenerico = *on;
154300141201             PosCurDatai = *on;
154400141201             V1Dmsg = 'Periodo iniziale della prima estrazione +
154500141201                       errato';
154600141201             leavesr;
154700141201         endsl;
154800141201         // - Se codice padre <> da codice cliente
154900141201         //   il periodo deve essere uguale
155000141201         if  V2Cccm <> V2Cksu;
155100141201           clear  wDataI;
155200141201           %subst( wDataI : 1 : 2 ) =
155300141201                   %subst( %editc( p_§LACdatai : 'X' ) : 5 : 2 );
155400141201           %subst( wDataI : 3 : 4 ) =
155500141201                   %subst( %editc( p_§LACdatai : 'X' ) : 1 : 4 );
155600141201           if  V2datai <> %int(wDataI);
155700141201             errMessage  = *on;
155800141201             errGenerico = *on;
155900141201             PosCurDatai = *on;
156000141201             V1Dmsg = 'Periodo iniziale della prima estrazione +
156100141201                       <> da quello del cod. padre ('
156200141201                    + %subst(%editc(p_§LACdatai : 'X') : 5 : 2)
156300141201                    + '/'
156400141201                    + %subst(%editc(p_§LACdatai : 'X') : 1 : 4)
156500141201                    + ')';
156600141201             leavesr;
156700141201           endif;
156800141201         endif;
156900090226
157000090309         //?Schedulazione giorno e mesi x preparazione CD?
157100090226         select;
157200090318           // when (V2Caut  =  'A'    or  V2Caut  =  'N')    AND
157300090318           when  V2Caut  =  'A'    AND
157400130923               // (V2datai <> *zero  or  V2tdta   <> *blank   or
157500130923                (V2mesi  <> *zero  or  V2Cschgg <> *zero    or
157600090311                 ds_SchMm <> *blank);
157700090311             errMessage  = *on;
157800090311             errGenerico = *on;
157900090311             PosCurAut   = *on;
158000090311             V1Dmsg = 'Schedulazione incompatibile con il +
158100090311                       "Tipo Elaborazione" = ' + V2Caut;
158200090311             leavesr;
158300090318           when  V2Caut = 'S'  and  (V2Cschgg <= *zero);
158400090306             errMessage  = *on;
158500090306             errGenerico = *on;
158600090306             PosCurSchGg = *on;
158700090306             V1Dmsg = 'Schedulazione obbligatoria per +
158800090306                       "Tipo Elaborazione" = "S" (schedulata)';
158900090306             leavesr;
159000090306           when  V2Caut = 'S'  and  (ds_SchMm = *blank);
159100090306             errMessage  = *on;
159200090306             errGenerico = *on;
159300090306             PosCurSch01 = *on;
159400090306             V1Dmsg = 'Indicare almeno un mese per +
159500090306                       "Tipo Elaborazione" = "S" (schedulata)';
159600090306             leavesr;
159700090311         endsl;
159800090226
159900090327         // - Se codice padre <> da codice cliente
160000090306         //   i 2 codici devono avere gli stessi dati di estrazione
160100090306         //   gg e mese e n. mesi da elaborare
160200090309         if  V2Cccm <> V2Cksu;
160300090309           select;
160400090309             when  p_§LACschgg <> V2Cschgg;
160500090226               errMessage  = *on;
160600090226               errGenerico = *on;
160700090303               PosCurSchgg = *on;
160800090309               V1Dmsg = 'Giorno creazione CD +
160900090309                         <> da quello del cod. padre ('
161000090309                      + %editc(p_§LACschgg:'X') + ')';
161100090226               leavesr;
161200090303             when  p_SchMm <> ds_SchMm;
161300090226               errMessage  = *on;
161400090226               errGenerico = *on;
161500090226               PosCursch01 = *on;
161600090309               V1Dmsg = 'Mesi di elaborazione +
161700090309                         <> da quelli del cod. padre ('
161800090309                      + p_SchMm + ')';
161900090226               leavesr;
162000090226           endsl;
162100090226         endif;
162200090309
162300090327         // - Verifica congruenza tra mese di schedulazione e
162400090309         //   mesi di estrazione
162500090309         select;
162600090311           // - Nessun dato inserito
162700090310           when  V2mesi   = *zero    and   V2Cschgg = *zero    and
162800090311                 ds_SchMm = *blank;
162900090311           // - Periodo da estrarre in mesi richiesto
163000090310           when  V2mesi = *zero;
163100090310             errMessage  = *on;
163200090310             errGenerico = *on;
163300090310             PosCurMesi  = *on;
163400090310             V1Dmsg = 'Periodo da estrarre obbligatorio SE inserita +
163500090310                       una schedulazione';
163600090310             leavesr;
163700090311           // - Giorno di schedulazione richiesto
163800090310           when  V2Cschgg = *zero;
163900090310             errMessage  = *on;
164000090310             errGenerico = *on;
164100090310             PosCurSchgg = *on;
164200090311             V1Dmsg = $Msg(08);
164300090310             leavesr;
164400090311           // - Selezione mesi richiesta
164500090311           when  ds_SchMm = *blank;
164600090310             errMessage  = *on;
164700090310             errGenerico = *on;
164800090310             PosCurSch01 = *on;
164900090311             V1Dmsg = $Msg(09);
165000090310             leavesr;
165100090311         endsl;
165200090313
165300090313         // - Conteggio/Controllo mesi schedulati
165400090313         clear  wCount;
165500090311         wSchMm_ds = %triml(ds_SchMm);
165600090313         For  xx = 1  To  %elem($SchMm);
165700090313           if  $SchMm(xx) <> *blank;
165800090313             wCount += 1;
165900090313           endif;
166000090313         EndFor;
166100090313
166200090311         select;
166300090313           // - Errori bloccanti:
166400090313           when   wCount <> *zero   and
166500090313                 (V2mesi <> (12 / wCount));
166600090313             errMessage  = *on;
166700090313             errGenerico = *on;
166800090313             PosCurSch01 = *on;
166900090313             V1Dmsg = 'Incongruenza tra il "periodo da estrarre" +
167000090313                       ed i mesi schedulati';
167100090313           // - Errori bypassabili:
167200090313           when ( (V2mesi =  2   and  ($SchMm(03) = *blank     or
167300090311                                       $SchMm(05) = *blank     or
167400090311                                       $SchMm(07) = *blank     or
167500090311                                       $SchMm(09) = *blank     or
167600090311                                       $SchMm(11) = *blank))   or
167700090313                  (V2mesi =  3   and  ($SchMm(04) = *blank     or
167800090311                                       $SchMm(07) = *blank     or
167900090311                                       $SchMm(10) = *blank))   or
168000090313                  (V2mesi =  4   and  ($SchMm(05) = *blank     or
168100090311                                       $SchMm(09) = *blank))   or
168200090313                  (V2mesi =  6   and   $SchMm(07) = *blank) )  AND
168300090313                 $ByPass =  *off;
168400090313             $ByPass = *on;
168500090309             errMessage  = *on;
168600090309             errGenerico = *on;
168700090310             PosCurSch01 = *on;
168800090313             V1Dmsg = 'Mesi schedulati in maniera irregolare... +
168900090313                       Premere Enter per forzare.';
169000090309             leavesr;
169100090313           when ( (V2mesi =  2  and  ($SchMm(02) <> *blank     or
169200090311                                      $SchMm(04) <> *blank     or
169300090311                                      $SchMm(06) <> *blank     or
169400090311                                      $SchMm(08) <> *blank     or
169500090311                                      $SchMm(10) <> *blank     or
169600090311                                      $SchMm(12) <> *blank))   OR
169700090313                  (V2mesi =  3  and  ($SchMm(02) <> *blank     or
169800090311                                      $SchMm(03) <> *blank     or
169900090311                                      $SchMm(05) <> *blank     or
170000090311                                      $SchMm(06) <> *blank     or
170100090311                                      $SchMm(08) <> *blank     or
170200090311                                      $SchMm(09) <> *blank     or
170300090311                                      $SchMm(11) <> *blank     or
170400090311                                      $SchMm(12) <> *blank))   OR
170500090313                  (V2mesi =  4  and  ($SchMm(02) <> *blank     or
170600090311                                      $SchMm(03) <> *blank     or
170700090311                                      $SchMm(04) <> *blank     or
170800090311                                      $SchMm(06) <> *blank     or
170900090311                                      $SchMm(07) <> *blank     or
171000090311                                      $SchMm(08) <> *blank     or
171100090311                                      $SchMm(10) <> *blank     or
171200090311                                      $SchMm(11) <> *blank     or
171300090311                                      $SchMm(12) <> *blank))   OR
171400090313                  (V2mesi =  6  and  ($SchMm(02) <> *blank     or
171500090311                                      $SchMm(03) <> *blank     or
171600090311                                      $SchMm(04) <> *blank     or
171700090311                                      $SchMm(05) <> *blank     or
171800090311                                      $SchMm(06) <> *blank     or
171900090311                                      $SchMm(08) <> *blank     or
172000090311                                      $SchMm(09) <> *blank     or
172100090311                                      $SchMm(10) <> *blank     or
172200090311                                      $SchMm(11) <> *blank     or
172300090311                                      $SchMm(12) <> *blank))   OR
172400090313                  (V2mesi = 12  and  ($SchMm(02) <> *blank     or
172500090311                                      $SchMm(03) <> *blank     or
172600090311                                      $SchMm(04) <> *blank     or
172700090311                                      $SchMm(05) <> *blank     or
172800090311                                      $SchMm(06) <> *blank     or
172900090311                                      $SchMm(07) <> *blank     or
173000090311                                      $SchMm(08) <> *blank     or
173100090311                                      $SchMm(09) <> *blank     or
173200090311                                      $SchMm(10) <> *blank     or
173300090311                                      $SchMm(11) <> *blank     or
173400090313                                      $SchMm(12) <> *blank)) ) AND
173500090313                 $ByPass =  *off;
173600090313             $ByPass = *on;
173700090310             errMessage  = *on;
173800090310             errGenerico = *on;
173900090311             PosCurSch01 = *on;
174000090313             V1Dmsg = 'Mesi schedulati in maniera irregolare... +
174100090313                       Premere Enter per forzare.';
174200090310             leavesr;
174300090313           // - Controllo giorno del mese (bypassabile)
174400090313           when ( (V2Cschgg > 28  and  V2Csch02 <> *blank)  or
174500090313                  (V2Cschgg > 30  and (V2Csch04 <> *blank
174600090313                                   or  V2Csch06 <> *blank
174700090313                                   or  V2Csch09 <> *blank
174800090313                                   or  V2Csch11 <> *blank)) )
174900090313                and  $CtrMm = *off;
175000090313             $CtrMm = *on;
175100090313             errMessage  = *on;
175200090313             errGenerico = *on;
175300090313             PosCurSch01 = (V2Csch01 <> *blank);
175400090313             PosCurSch02 = (V2Csch02 <> *blank);
175500090313             PosCurSch04 = (V2Csch04 <> *blank);
175600090313             PosCurSch06 = (V2Csch06 <> *blank);
175700090313             PosCurSch09 = (V2Csch09 <> *blank);
175800090313             PosCurSch11 = (V2Csch11 <> *blank);
175900090313             V1Dmsg = %replace( %char(V2Cschgg) : $Msg(10) :
176000090313                                %scan('&g':$Msg(10)) );
176100090313             leavesr;
176200090313         endsl;
176300090226
176400090309         //?Frequenza addebito?
176500090310         select;
176600090327           // - Obbligatoria
176700090327           when  V2tadu <> 'I'   and   V2tadu <> 'J'   and
176800090327                 V2tadu <> 'S'   and   V2tadu <> 'M';
176900090327             errMessage  = *on;
177000090327             errGenerico = *on;
177100090327             PosCurTadu  = *on;
177200090327             V1Dmsg = 'Frequenza addebito errata o mancante';
177300090327             leavesr;
177400090327           // - Se tipo elaborazione CD la creazione addebito non può
177500090313           //   essere mensile o settimanale
177600090310           when  V2Caut = 'S'  and  (V2tadu = 'S'  or  V2tadu = 'M');
177700090310             errMessage  = *on;
177800090310             errGenerico = *on;
177900090310             PosCurTadu  = *on;
178000090310             V1Dmsg = 'Per creazione CD non è possibile addebito +
178100090310                       mensile o settimanale';
178200090310             leavesr;
178300090327           // - Se codice padre <> da codice cliente
178400090327           //   devono avere la stessa frequenza addebito
178500090310           when  V2Cccm <> V2Cksu   and   p_§LACtadu <> V2tadu;
178600090310             errMessage  = *on;
178700090310             errGenerico = *on;
178800090310             PosCurTadu  = *on;
178900090310             V1Dmsg = 'Frequenza addebito +
179000090310                        <> da quella del cod. padre ('
179100090310                    + p_§LACtadu + ')';
179200090310             leavesr;
179300090310         endsl;
179400090309
179500090309         //?Tipo addebito (Creazione TITAS)?
179600090310         select;
179700090327           // - Obbligatorio
179800090327           when  V2Ctad <> 'N'  and  V2Ctad <> 'S'  and  V2Ctad <> 'V';
179900090327             errMessage  = *on;
180000090327             errGenerico = *on;
180100090327             PosCurTad   = *on;
180200090327             V1Dmsg = 'Flag "Creazione TITAS" errato o mancante';
180300090327             leavesr;
180400090327           // - Se "N" non impostare i dati dell'addebito forzato
180500090310           when  V2Ctad = 'N'     and
180600090310                (V2Cksc <> *zero  or  V2Cctr <> *blank
180700090310                                  or  V2imp <> *zero);
180800090310             errMessage  = *on;
180900090310             errGenerico = *on;
181000090310             PosCurTad   = *on;
181100090310             V1Dmsg = 'Se NO addebito non impostare le forzature';
181200090310             leavesr;
181300090327           // - Se codice padre <> da codice cliente
181400090310           //   devono avere lo stesso tipo addebito
181500090310           when  V2Cccm <> V2Cksu   and   p_§LACtad <> V2Ctad;
181600090310             errMessage  = *on;
181700090310             errGenerico = *on;
181800090310             PosCurTad   = *on;
181900090310             V1Dmsg = 'Tipo addebito +
182000090310                       <> da quello del cod. padre ('
182100090310                        + p_§LACtad + ')';
182200090310             leavesr;
182300090310         endsl;
182400090226
182500090311         //?Dati da forzare in tassazione:?
182600090312         //?- Codice cliente / Tariffa?
182700090312         clear  TIBS69ds;
182800090312         if  V2Cksc <> *zero;
182900090312           tibs69ds.I69kcc = DUTkci;
183000090312           tibs69ds.I69kac = %int(V2Cksc);
183100090312           tibs69ds.I69sif = knsif;
183200090312           tibs69r(TIBS69ds : ds_CNACO :
183300090312                              ds_CNIND :
183400090312                              ds_CNCLP :
183500090312                              ds_FNCLS);
183600090312         endif;
183700090312         select;
183800090312           when  tibs69ds.O69err = *on;
183900090312             errMessage  = *on;
184000090312             errGenerico = *on;
184100090312             PosCurKsc   = *on;
184200090312             V1Dmsg = 'Cliente tassazione errato';
184300090312             leavesr;
184400090312           when  V2Cksc <> *zero   and   V2Cctr = *blank;
184500090312             errMessage  = *on;
184600090312             errGenerico = *on;
184700090312             PosCurCtr   = *on;
184800090312             V1Dmsg = 'Codice tariffa obbligatorio SE +
184900090312                       inserito il cliente tassazione';
185000090312             leavesr;
185100090311           // - Non ammessa tariffa senza cliente
185200090311           when  V2Cksc =  *zero   and   V2Cctr <> *blank;
185300090311             errMessage  = *on;
185400090311             errGenerico = *on;
185500090311             PosCurKsc   = *on;
185600090311             V1Dmsg = 'Cliente tassazione obbligatorio SE +
185700090311                       inserita la relativa tariffa';
185800090311             leavesr;
185900090311           // - Ammesso solo un codice tariffa numerico
186000090311           when  V2Cctr <> *blank   and
186100090311                 %check(c_Digits : V2Cctr) > *zero;
186200090311             errMessage  = *on;
186300090311             errGenerico = *on;
186400090312             PosCurCtr   = *on;
186500090312             V1Dmsg = 'Ammessi SOLO caratteri numerici';
186600090311             leavesr;
186700090310         endsl;
186800090226
186900090327         // - Se codice padre <> da codice cliente
187000090306         //   stesso ksc/ctr/importo
187100090303         if  V2Cccm <> V2Cksu;
187200090226           select;
187300090303             when  p_§LACksc <> V2Cksc;
187400090226               errMessage  = *on;
187500090226               errGenerico = *on;
187600090303               PosCurKsc   = *on;
187700090309               V1Dmsg = 'Cliente tassazione +
187800090309                         <> da quello del cod. padre ('
187900090309                      + %editc(p_§LACksc : 'X') + ')';
188000090226               leavesr;
188100090303             when  p_§LACctr <> V2Cctr;
188200090226               errMessage  = *on;
188300090226               errGenerico = *on;
188400090303               PosCurCtr   = *on;
188500090309               V1Dmsg = 'Codice tariffa +
188600090309                         <> quello del cod. padre ('
188700090309                      + p_§LACctr + ')';
188800090226               leavesr;
188900090226           endsl;
189000090226         endif;
189100090226
189200090309         //?Importo & Tipo importo forzato?
189300090310         select;
189400090310           // - Non ammesso importo per Varia Negata
189500090310           when  V2Ctad = 'V'   and   V2imp <> *zero;
189600090310             errMessage  = *on;
189700090310             errGenerico = *on;
189800090310             PosCurImp   = *on;
189900090310             V1Dmsg = 'NON ammesso l''importo forzato per +
190000090310                       Varia Negata';
190100090310             leavesr;
190200090310           // - Non ammesso tipo importo senza importo
190300090310           when  V2imp =  *zero   and   V2fimp <> *blank;
190400090310             errMessage  = *on;
190500090310             errGenerico = *on;
190600090310             PosCurImp   = *on;
190700090310             V1Dmsg = 'Importo forzato obbligatorio SE +
190800090310                       inserito il tipo di importo';
190900090310             leavesr;
191000090312           // - Se inserito importo devono essere inseriti:
191100090312           //   · il codice cliente forzato (ksc),
191200090312           //   · la tariffa forzata (ctr) ed
191300090312           //   · il tipo importo (fimp)
191400090310           when  V2imp <> *zero;
191500090310             select;
191600090310               when  V2fimp  = *blank;
191700090310                 errMessage  = *on;
191800090310                 errGenerico = *on;
191900090310                 PosCurFimp  = *on;
192000090310                 V1Dmsg = 'Immettere il tipo importo';
192100090310                 leavesr;
192200090310               when  V2Cksc  = *zero;
192300090310                 errMessage  = *on;
192400090310                 errGenerico = *on;
192500090310                 PosCurKsc   = *on;
192600090310                 V1Dmsg = 'Immettere il cliente forzato';
192700090310                 leavesr;
192800090312               when  V2Cctr  = *blank;
192900090310                 errMessage  = *on;
193000090310                 errGenerico = *on;
193100090310                 PosCurCtr   = *on;
193200090310                 V1Dmsg = 'Immettere la tariffa forzata';
193300090310                 leavesr;
193400090310             endsl;
193500090310         endsl;
193600090312
193700090327         // Verifica compatibilità dell'importo forzato con il
193800090312         //   massimo imponibile IVA in tab. "GEI"/"LAC".
193900090312         if  V2imp <> *zero   and   V2fimp = 'T';
194000090312           // - Reperimento divisa moneta di conto gestionale
194100090312           clear  dGED;
194200090312           reset  TIBS02ds;
194300090312           T02cod = 'GED';
194400090312           T02ke1 = '1';
194500090312           TNTBE_RicercaControllo(kpjba : tibs02ds);
194600090312           if  T02err = *blank;
194700090312             dGED = T02uni;
194800090312           endif;
194900090312           // - Reperimento importi da tab. "GEI"
195000090312           //   (interessa il massimo imponibile IVA per spedizione)
195100090312           clear  dGEI;
195200090312           reset  TIBS02ds;
195300090312           T02cod = 'GEI';
195400090312           T02ke1 = §GEDcn;
195500090312           TNTBE_RicercaControllo(kpjba : tibs02ds);
195600090312           if  T02err = *blank;
195700090312             dGEI = T02uni;
195800090312           endif;
195900090312           if  V2imp > §GEImf;
196000090312             errMessage  = *on;
196100090312             errGenerico = *on;
196200090312             PosCurImp   = *on;
196300090312             V1Dmsg = 'Importo superiore al MASSIMO IMPONIBILE IVA +
196400090312                       PER SPEDIZIONE ('
196500090312                      + %trim( %editc( §GEImf : 'J' ) ) + ')';
196600090312             leavesr;
196700090312           endif;
196800090312         endif;
196900090226
197000090312         // Se codice padre <> da codice cliente
197100090306         //   stesso importo e tipo importo
197200090303         if  V2Cccm <> V2Cksu;
197300090226           select;
197400090303             when  p_§LACimp <> V2imp;
197500090226               errMessage  = *on;
197600090226               errGenerico = *on;
197700090312               PosCurImp   = *on;
197800090309               V1Dmsg = 'Importo forzato +
197900090309                         <> da quello del cod. padre ('
198000090309                      + %editc(p_§LACimp : 'J') + ')';
198100090226               leavesr;
198200090303             when  p_§LACfimp <> V2fimp;
198300090226               errMessage  = *on;
198400090226               errGenerico = *on;
198500090312               PosCurFimp  = *on;
198600090309               V1Dmsg = 'Tipo importo forzato +
198700090309                         <> da quello del cod. padre ('
198800090309                      + p_§LACfimp + ')';
198900090226               leavesr;
199000090226           endsl;
199100090225         endif;
199200090923
199300090923         //?Modifica dati sensibili per l'addebito
199400090923         if (v2tadu <> savtadu or v2ctad <> savtad or
199500090923             v2cksc <> savksc or v2cctr <> savctr or
199600141128             v2imp <> savimp or v2fimp <> savfimp) and
199700141128             (not *in02 and not $Copia)
199800090923             and not $forza;
199900090923               errMessage  = *on;
200000090923               errGenerico = *on;
200100090923               PosCurTadu  = *on;
200200090923               V1Dmsg = 'ATTENZIONE!! Possibili problemi ' +
200300090923                        'in fase di addebito. Avvisare Monica/Nicola';
200400090923               $forza = *on;
200500090923               leavesr;
200600090923         endif;
200700110713
200800110713         //?Creazione file elenco per il cliente
200900110713         //?possibile solo se NON è elaborazione automatica (FTP)
201000110713         IF  V2pgm <> *blanks and V2caut = 'A';
201100110713           ErrMessage  = *on;
201200110713           ErrGenerico = *on;
201300110713           PosCurPgm   = *on;
201400110713           V1Dmsg = 'NON possibile creazione elenco se elaborazione '+
201500110713                    'automatica';
201600110713           leavesr;
201700110713         ENDIF;
201800110713         //?se inserito il nome del programma deve esserci anche il tipo elenco
201900110713         IF  V2pgm <> *blanks and V2ele = *blanks;
202000110713           ErrMessage  = *on;
202100110713           ErrGenerico = *on;
202200110713           PosCurEle   = *on;
202300110713           V1Dmsg = 'Immettere anche il tipo elenco da creare';
202400110713           leavesr;
202500110713         ENDIF;
202600110713         //?e viceversa se c'è il tipo elenco deve esserci anche il nome del pgm
202700110713         IF  V2pgm = *blanks and V2ele <> *blanks;
202800110713           ErrMessage  = *on;
202900110713           ErrGenerico = *on;
203000110713           PosCurPgm   = *on;
203100110713           V1Dmsg = 'Immettere anche il nome del programma x elenco da creare';
203200110713           leavesr;
203300110713         ENDIF;
203400110713         //?Se codice padre <> da codice cliente
203500110713         //?  devono avere gli stessi dati
203600110713         IF  V2Cccm <> V2Cksu;
203700110713           SELECT;
203800110713             WHEN  p_§LACpgm <> V2pgm;
203900110713               ErrMessage  = *on;
204000110713               ErrGenerico = *on;
204100110713               PosCurPgm   = *on;
204200110713               V1Dmsg = 'Nome programma +
204300110713                         <> da quello del cod. padre ('
204400110713                      + p_§LACpgm + ')';
204500110713               leavesr;
204600110713             WHEN  p_§LACele <> V2ele;
204700110713               ErrMessage  = *on;
204800110713               ErrGenerico = *on;
204900110713               PosCurEle   = *on;
205000110713               V1Dmsg = 'Tipo elenco +
205100110713                         <> da quello del cod. padre ('
205200110713                      + p_§LACele + ')';
205300110713               leavesr;
205400110713           ENDSL;
205500110713         ENDIF;
205600141128
205700141128         //?Competenze C/E dichiarate
205800141128         IF  (*in02 or $copia) and V2compce = *blanks;
205900141128           ErrGenerico = *on;
206000141128           ErrMessage  = *on;
206100141128           PosCurCompCE = *on;
206200141128           V1Dmsg = 'Competenze C/E obbligatorie';
206300141128           leavesr;
206400141128         ENDIF;
206500141201         IF  V2compce <> *blanks;
206600141128         clear  V2dcompce;
206700141128         IF  %scan('?' : V2compce) > *zero;
206800141128           clear  TIBS02ds;
206900141128           T02mod = 'R';
207000141128           T02cod = 'CED';
207100141128           T02sif = KNSIF;
207200141128           TNTBE_RicercaControllo(kpjba : tibs02ds);
207300141128           IF  T02err = *blank;
207400141128             V2compce  = T02ke1;
207500141128             V2dcompce = T02uni;
207600141128             errGenerico = *on;
207700141128             leavesr;
207800141128           ELSE;
207900141128             errMessage  = *on;
208000141128             errGenerico = *on;
208100141128             PosCurCompCE = *on;
208200141128             V1Dmsg = T02msg;
208300141128             leavesr;
208400141128           ENDIF;
208500141128         ELSE;
208600141128           reset  TIBS02ds;
208700141128           T02cod = 'CED';
208800141128           T02ke1 = V2compce;
208900141128           T02sif = KNSIF;
209000141128           TNTBE_RicercaControllo(kpjba : tibs02ds);
209100141128           IF  T02err = *blank;
209200141128             V2dcompce = T02uni;
209300141128           ELSE;
209400141128             ErrMessage  = *on;
209500141128             ErrGenerico = *on;
209600141128             PosCurCompCE = *on;
209700141128             V1Dmsg = 'Competenze C/E errate';
209800141128             leavesr;
209900141128           ENDIF;
210000141128         ENDIF;
210100141202         ELSE;
210200141202           V2dcompce = 'NON DICHIARATO';
210300141201         ENDIF;
210400141128         //?Se codice padre <> da codice cliente
210500141128         //?  devono avere gli stessi dati
210600141128         //?  ma solo se sono in immissione/copia
210700141201         //?  e se il padre ha la personalizzazione impostata
210800141128         IF  V2Cccm <> V2Cksu and
210900141201             p_§LACcompce <> V2compce and (*in02 or $copia)
211000141201             and p_§LACcompce <> *blanks;
211100141128           ErrMessage   = *on;
211200141128           ErrGenerico  = *on;
211300141128           PosCurCompCE = *on;
211400141128           V1Dmsg = 'Competenze C/E +
211500141128                     <> da quelle del cod. padre ('
211600141128                   + p_§LACcompce + ')';
211700141128           leavesr;
211800141128         ENDIF;
211900141128
212000141128         //?Tipo Addebito dichiarato
212100141128         clear  V2dtpadd;
212200141128         IF  %scan('?' : V2tpadd) > *zero;
212300141128           clear  TIBS02ds;
212400141128           clear dTAL;
212500141128           T02mod = 'R';
212600141128           T02cod = 'TAL';
212700141128           T02sif = KNSIF;
212800141128           TNTBE_RicercaControllo(kpjba : tibs02ds);
212900141128           IF  T02err = *blank;
213000141128             V2tpadd  = T02ke1;
213100141128             dTAL = T02uni;
213200141128             V2dtpadd = §TALdes;
213300141128             errGenerico = *on;
213400141128             leavesr;
213500141128           ELSE;
213600141128             errMessage  = *on;
213700141128             errGenerico = *on;
213800141128             PosCurTpAdd = *on;
213900141128             V1Dmsg = T02msg;
214000141128             leavesr;
214100141128           ENDIF;
214200141128         ELSE;
214300141202         IF  V2tpadd = *blanks;
214400141202           V2dtpadd = 'NON DICHIARATO';
214500141202         ELSE;
214600141128           reset  TIBS02ds;
214700141128           clear dTAL;
214800141128           T02cod = 'TAL';
214900141128           T02ke1 = V2tpadd;
215000141128           T02sif = KNSIF;
215100141128           TNTBE_RicercaControllo(kpjba : tibs02ds);
215200141128           IF  T02err = *blank;
215300141128             dTAL = T02uni;
215400141128             V2dtpadd = §TALdes;
215500141128           ELSE;
215600141128             ErrMessage  = *on;
215700141128             ErrGenerico = *on;
215800141128             PosCurTpAdd = *on;
215900141128             V1Dmsg = 'Tipo Addebito errato';
216000141128             leavesr;
216100141128           ENDIF;
216200141202         ENDIF;
216300141128         ENDIF;
216400141128         //?Se codice padre <> da codice cliente
216500141128         //?  devono avere gli stessi dati
216600141128         //?  ma solo se sono in immissione/copia
216700141201         //?  e se il padre ha il campo impostato
216800141128         IF  V2Cccm <> V2Cksu and
216900141201             p_§LACtpadd <> V2tpadd and (*in02 or $copia)
217000141201             and p_§LACtpadd <> *blanks;
217100141128           ErrMessage  = *on;
217200141128           ErrGenerico = *on;
217300141128           PosCurTpAdd = *on;
217400141128           V1Dmsg = 'Tipo addebito +
217500141128                     <> da quello del cod. padre ('
217600141128                   + p_§LACtpadd + ')';
217700141128           leavesr;
217800141128         ENDIF;
217900141203
218000141203         //?Data Aut. DG
218100141203         IF  V2dtautdg <> *zeros;
218200141203           exec sql
218300141203           select count(*) into :conta
218400141203           from DATECNV0F
218500141203           where DC_DMYY_Z = :V2dtautdg or DC_DMY_Z = :V2dtautdg;
218600141203           IF  conta = 0;
218700141203             ErrMessage  = *on;
218800141203             ErrGenerico = *on;
218900141203             PosCurAutDg = *on;
219000141203             V1Dmsg = $Msg(15);
219100141203             leavesr;
219200141203           ENDIF;
219300141203           IF  V2dtautdg < 1000000;
219400141203             exec sql
219500141203             set :V2dtautdg = (select DC_DMYY_Z from DATECNV0F
219600141203                               where DC_DMY_Z = :V2dtautdg);
219700141203           ENDIF;
219800141203         ENDIF;
219900071207
220000071207       ENDSR;
220100090225
220200090225       //--------------------------------------------------------------
220300090306       //?Controllo esistenza del codice padre in tabella LAC          ?
220400090225       //--------------------------------------------------------------
220500090303       BEGSR  sr_Padre;
220600090302
220700090327         // Controlla esistenza in tabella "LAC"
220800090310         clear  dLACp;
220900090310         reset  TIBS02ds;
221000090310         T02sif = KNSIF;
221100090310         %subst(T02ke1 : 1 : 7) = %editc(V2Cksu : 'X');
221200090310         TNTBE_RicercaControllo(kpjba : tibs02ds);
221300090310         if  T02err = *blank;
221400090310           dLACp  = T02uni;
221500090310         else;
221600090310           errMessage  = *on;
221700090310           errGenerico = *on;
221800090310           PosCurksu   = *on;
221900090310           V1Dmsg = $Msg(12);
222000090310           leavesr;
222100090310         endif;
222200090327
222300090327         // Controlla se già inserito come figlio di altro padre
222400090327         if  p_§LACksu <> V2Cksu;
222500090327           errMessage  = *on;
222600090327           errGenerico = *on;
222700090327           PosCurKsu   = *on;
222800090327           V1Dmsg = 'Non ammesso cod. PADRE già FIGLIO di altro +
222900090327                     cliente (' + %editc(p_§LACksu : 'X') + ')';
223000090327           leavesr;
223100090327         endif;
223200090225
223300090225       ENDSR;
223400121016
223500121016       //--------------------------------------------------------------
223600121016       //?Controllo Merge.
223700121016       //--------------------------------------------------------------
223800121016       BEGSR  CTR_merge;
223900121016
224000121016       // -?L'immagine se con Merge deve essere di tipo PDF
224100121016         IF  V2Ctpi <> 'PDF';
224200121016           errMessage  = *on;
224300121016           errGenerico = *on;
224400121016           V1Dmsg = 'Immagine con Merge, deve essere di tipo PDF';
224500121016           PosCurTpi = *on;
224600121016           leavesr;
224700121016         ENDIF;
224800121016
224900121016       // -?La directory deve finire con /PF
225000121016         wPos1 = %scan('  ':V2Cdir);
225100121016         wPos1 -= 3;
225200121016         IF  %subst(V2Cdir:wPos1:3) <> '/PF';
225300121016           ErrMessage  = *on;
225400121016           ErrGenerico = *on;
225500121016           PosCurDir   = *on;
225600121016           V1Dmsg = 'Immagine con Merge, l''ultimo ramo della +
225700121016                     DIR deve essere "PF"';
225800121016           leavesr;
225900121016         ENDIF;
226000121016
226100121016       // -?Cerco il corrispondente in JDC
226200121016         clear  TIBS02DS;
226300121016         T02mod = 'C';
226400121016         T02cod = 'JDC';
226500121016         T02ke1 = %editc(V2Cccm:'X');
226600121016         TNTBE_RicercaControllo(kpjba : tibs02ds);
226700121016         // -?Non trovo la tabella
226800121016         IF  T02err <> *blanks;
226900170307           // -?Immissione o copia forzo
227000170307           IF  (*in02 or $copia) and not wForza;
227100121016             wForza = *on;
227200121016             ErrMessage  = *on;
227300121016             ErrGenerico = *on;
227400151130             V1Dmsg = 'Attivare anche la corrispondente +
227500151130                       tabella JDC. Enter per forzare';
227600121016             leavesr;
227700121016           ENDIF;
227800121016         // -?Manutenzione blocco
227900170307           IF  not *in02 and not $copia;
228000121016             ErrMessage  = *on;
228100121016             ErrGenerico = *on;
228200121016             V1Dmsg = 'Manca la corrispondente tabella JDC';
228300121016             leavesr;
228400121016           ENDIF;
228500121016         // -?Trovo la tabella
228600121016         ELSE;
228700121016           dJDC = T02uni;
228800121016         // -?Controllo il tipo immagine
228900121030           IF  V2Ctpi <> §JDCtpi and not wForzaTPI;
229000121030             wForzaTPI = *on;
229100121016             ErrMessage  = *on;
229200121016             ErrGenerico = *on;
229300121016             PosCurTpi   = *on;
229400151130             V1Dmsg = 'Tipo immagine diverso da corrispondente +
229500151130                       tab.JDC --> ' + §JDCtpi + ' Enter per forzare.';
229600121016             leavesr;
229700121016           ENDIF;
229800121016         // -?Controllo il nome immagine
229900121030           IF  V2Cfmi <> §JDCfmi and not wForzaFMI;
230000121030             wForzaFMI = *on;
230100121016             ErrMessage  = *on;
230200121016             ErrGenerico = *on;
230300121016             PosCurFmi   = *on;
230400151130             V1Dmsg = 'Nome immagine diverso da corrispondente +
230500151130                       tab.JDC --> ' + §JDCfmi + ' Enter per forzare.';
230600121016             leavesr;
230700121016           ENDIF;
230800121016         // -?Controllo la directory
230900121016           wPos2 = %scan('  ':§JDCdir);
231000121016           wPos2 -= 3;
231100121016           IF  %subst(V2Cdir:1:wPos1) <>
231200121030               %subst(§JDCdir:1:wPos2) and not wForzaDIR;
231300121030             wForzaDIR = *on;
231400121016             ErrMessage  = *on;
231500121016             ErrGenerico = *on;
231600121016             PosCurDir   = *on;
231700151130             V1Dmsg = 'Directory diversa da corrispondente +
231800151130                       tab.JDC --> ' + %subst(§JDCdir:1:wPos2) +
231900151201                       ' Enter per forzare.';
232000121016             leavesr;
232100121016           ENDIF;
232200121016         ENDIF;
232300121016
232400121016       ENDSR;
232500101103
232600101103       //--------------------------------------------------------------
232700101103       //?Gestione window W01                                          ?
232800101103       //--------------------------------------------------------------
232900101103       BEGSR  sr_GesW01;
233000101103
233100101103         // -?Emissione window?
233200101103         exfmt  TB46W01;
233300101103
233400101103         errMessage  = *off;
233500101103         errGenerico = *off;
233600101103         clear  V1Dmsg;
233700101103
233800101103         $Video = 'D2';
233900101103
234000101103         select;
234100101103           // -?F12=Ritorno?
234200101103           when  dsp_aid = c_F12;
234300101103             leavesr;
234400101103           // -?Invio con "S"?
234500101103           when  W1Ccpy = 'S';
234600101103             if  V2Caut = *blank;
234700101103               V2Caut   = p_§LACaut;
234800101103             endif;
234900101103             if  V2Ctpi = *blank;
235000101103               V2Ctpi   = p_§LACtpi;
235100101103             endif;
235200101103             if  V2Cfmi = *blank;
235300101103               V2Cfmi   = p_§LACfmi;
235400101103               clear  V2Dfmi;
235500101103             endif;
235600121016             if  V2merge = *blank;
235700121016               V2merge  = p_§LACmerge;
235800121016             endif;
235900121217             if  V2Cla2 = *blank;
236000121217               V2Cla2 = p_§LACla2;
236100121217             endif;
236200101103             //if  V2Cdir = *blank;      //? Directory ?
236300101103               V2Cdir   = p_§LACdir;     //? COMUNQUE  ?
236400101103             //endif;                    //? del padre ?
236500101103             if  V2Ctpt = *blank;
236600101103               V2Ctpt   = p_§LACtpt;
236700101103             endif;
236800101103             if  V2Cssr = *blank;
236900101103               V2Cssr   = p_§LACssr;
237000101103             endif;
237100101103             if  V2Clnp = *zero;
237200101103               V2Clnp   = p_§LAClnp;
237300101103             endif;
237400101103             if  V2Cres = *blank;
237500101103               V2Cres   = p_§LACres;
237600101103             endif;
237700101103             if  V2Crec = *blank;
237800101103               V2Crec   = p_§LACrec;
237900101103             endif;
238000101103             if  V2Ccsr = *blank;
238100101103               V2Ccsr   = p_§LACcsr;
238200101103             endif;
238300101103             if  V2tdta = *blank;
238400101103               V2tdta   = p_§LACtdta;
238500101103             endif;
238600101103             if  V2mesi = *zero;
238700101103               V2mesi   = p_§LACmesi;
238800101103             endif;
238900101103             if  V2Cschgg = *zero;
239000101103               V2Cschgg = p_§LACschgg;
239100101103             endif;
239200101103             if  ds_SchMm = *blank;
239300101103               ds_SchMm = p_SchMm;
239400101103             endif;
239500101103             if  V2tadu = *blank;
239600101103               V2tadu   = p_§LACtadu;
239700101103             endif;
239800101103             if  V2Ctad = *blank;
239900101103               V2Ctad   = p_§LACtad;
240000101103             endif;
240100101103             if  V2Cksc = *zero;
240200101103               V2Cksc   = p_§LACksc;
240300101103             endif;
240400101103             if  V2Cctr = *blank;
240500101103               V2Cctr   = p_§LACctr;
240600101103             endif;
240700101103             if  V2imp  = *zero;
240800101103               V2imp    = p_§LACimp;
240900101103             endif;
241000101103             if  V2fimp = *blank;
241100101103               V2fimp   = p_§LACfimp;
241200101103             endif;
241300110713             if  V2pgm  = *blank;
241400110713               V2pgm    = p_§LACpgm;
241500110713             endif;
241600110713             if  V2ele  = *blank;
241700110713               V2ele    = p_§LACele;
241800110713             endif;
241900101103             if  V2note = *blank;
242000101103               V2note   = p_§LACnote;
242100101103             endif;
242200140813             if  V2tpadd = *blank;
242300140813               V2tpadd  = p_§LACtpadd;
242400140813             endif;
242500140813             if  V2compce = *blank;
242600140813               V2compce = p_§LACcompce;
242700140813             endif;
242800101103         endsl;
242900101103
243000101103       ENDSR;
243100121212
243200121212       //--------------------------------------------------------------
243300121217       //?Gestione estensione tabella --> LA2.                       ?
243400121212       //--------------------------------------------------------------
243500121217       BEGSR  GesLA2;
243600121212
243700121217         TNTB464ds = TNTB46ds;
243800121217         B464ke1 = %editc(V2Cccm:'X');
243900121217         clear B464fxx;
244000121217         clear B464err;
244100121217         clear B46msg;
244200121217         tntb464r (kpjba:TNTB464DS);
244300121212
244400121212       ENDSR;
244500151217
244600151217       //--------------------------------------------------------------
244700151217       //?Gestione estensione tabella --> JDC.                       ?
244800151217       //--------------------------------------------------------------
244900151217       BEGSR  GesJDC;
245000151217
245100151217         TNTBJDCds = TNTB46ds;
245200151217         kpjbu = TNTBJDCds;
245300151217         BJDCke1 = %editc(V2Cccm:'X');
245400151217         clear BJDCfxx;
245500151217         clear BJDCerr;
245600151217         clear B46msg;
245700151217         tntbJDCr (kpjba);
245800151217
245900151217       ENDSR;
246000071206
246100071212       //--------------------------------------------------------------
246200090306       //?Aggiornamento record TNTBE00F (tab. LAC)                     ?
246300071212       //--------------------------------------------------------------
246400090306       BEGSR  Upd_TNTBE;
246500071207
246600090306         clear  dLAC;
246700071207         §LACrag   = V2Dccm;
246800071207         §LACtpt   = V2Ctpt;
246900071207         §LACres   = V2Cres;
247000071207         §LACrec   = V2Crec;
247100071207         §LACcsr   = V2Ccsr;
247200071207         §LACssr   = V2Cssr;
247300071207         §LACdir   = V2Cdir;
247400071207         §LACtpi   = V2Ctpi;
247500071207         §LACfmi   = V2Cfmi;
247600121016         §LACmerge = V2merge;
247700121217         §LACla2   = V2Cla2;
247800071207         §LACaut   = V2Caut;
247900161025         §LACFiD   = V2CFid;
248000161025         §LACCpy   = V2CCpy;
248100161025         §LACDst   = V2CDst;
248200090309         §LACtdta  = V2tdta;
248300071207         §LACtad   = V2Ctad;
248400071207         §LACschgg = V2Cschgg;
248500071207         §LACsch01 = V2Csch01;
248600071207         §LACsch02 = V2Csch02;
248700071207         §LACsch03 = V2Csch03;
248800071207         §LACsch04 = V2Csch04;
248900071207         §LACsch05 = V2Csch05;
249000071207         §LACsch06 = V2Csch06;
249100071207         §LACsch07 = V2Csch07;
249200071207         §LACsch08 = V2Csch08;
249300071207         §LACsch09 = V2Csch09;
249400071207         §LACsch10 = V2Csch10;
249500071207         §LACsch11 = V2Csch11;
249600071207         §LACsch12 = V2Csch12;
249700090226         §LACnote  = V2note;
249800090309         §LAClnp   = V2Clnp;
249900090309         §LACksu   = V2Cksu;
250000090309         §LACmesi  = V2mesi;
250100130923         IF  V2caut = 'A';
250200130923           clear §LACdatai;
250300130923           clear §LACdatela;
250400130923         ELSE;
250500090313         §LACdatai = (%int( %subst( %editc( V2datai : 'X') : 3 : 4 ) )
250600090313                      * 100)
250700090313                   + %int( %subst( %editc( V2datai : 'X') : 1 : 2 ) );
250800090310         §LACdatela...
250900090313                   = (%int( %subst( %editc( V2datela : 'X') : 3 : 4 ) )
251000090319                      * 100)
251100090313                    + %int( %subst( %editc( V2datela : 'X') : 1 : 2 ) );
251200130923         ENDIF;
251300090309         §LACtadu  = V2tadu;
251400090309         §LACksc   = V2Cksc;
251500090309         §LACctr   = V2Cctr;
251600090309         §LACfimp  = V2fimp;
251700090309         §LACimp   = V2imp;
251800110713         §LACpgm   = V2pgm;
251900110713         §LACele   = V2ele;
252000140813         §LACtpadd = V2tpadd;
252100140813         §LACcompce= V2compce;
252200140813         §LACutente= knmus;
252300150121         IF  *in02 or $copia;
252400140813           §LACdtaimm= %editc(woggi:'X');
252500141210         ELSE;
252600141210           exec sql
252700141210           set :§LACdtaimm = (select DC_YYMD_C from DATECNV0F
252800141210                              where DC_DMYY_A1 = :V2dtaimm);
252900140813         ENDIF;
253000141203         IF V2dtautdg <> *zeros;
253100141203           exec sql
253200141203           set :§LACdtaudg = (select DC_YYMD_C from DATECNV0F
253300141203                              where DC_DMYY_Z = :V2dtautdg);
253400141203         ENDIF;
253500071207
253600090309         TBEuni    = dLAC;
253700071207
253800090306         clear  TBEftt;
253900090306         clear  TBEftr;
254000071207
254100071207         select;
254200090306           // F5=Ripristino
254300090306           when  dsp_aid = c_F05;
254400090306             clear  TBEatb;
254500090306           // F16=Annullamento
254600090306           when  dsp_aid = c_F16;
254700071207             TBEatb = 'A';
254800071207         endsl;
254900121220
255000121220         TBEdtr = woggi;
255100071207
255200071207         IF  NOT  %found(TNTBE01L);
255300090306           clear  TBEatb;
255400090306           clear  TBEflt;
255500090306           clear  TBEftr;
255600090306           TBEsif = TBXsif;
255700090306           TBElin = TBXlin;
255800090306           TBEapl = TBXapl;
255900090306           TBEcod = k_TBEcod;
256000090306           TBEke1 = k_TBEke1;
256100090306           TBEke2 = k_TBEke2;
256200090306           //_____________
256300071207           WRITE TNTBE000;
256400090306           //¯¯¯¯¯¯¯¯¯¯¯¯¯
256500071207         ELSE;
256600090306           //______________
256700071207           UPDATE TNTBE000;
256800090306           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
256900071207         ENDIF;
257000121213
257100121213
257200121213         // -?Porto a pari anche la tabella LA2
257300121213         SELECT;
257400121213           //?F5=Ripristino tabella
257500121213           WHEN  dsp_aid = c_F05;
257600121213             //?Ripristino anche la LA2 se esiste
257700121213             k_TBEcod = 'LA2';
257800121213             k_TBEke1 = %editc(V2Cccm:'X');
257900121213             clear k_TBEke2;
258000121213             k_TBElin = TBXlin;
258100121213             k_TBEsif = TBXsif;
258200121213             chain  %kds(K05tntbe01) TNTBE01L;
258300121213             IF  %found(TNTBE01L);
258400121213               clear TBEatb;
258500121213               UPDATE  TNTBE000;
258600121213             ENDIF;
258700151217             //?Ripristino anche la JDC se esiste
258800151217             k_TBEcod = 'JDC';
258900151217             k_TBEke1 = %editc(V2Cccm:'X');
259000151217             clear k_TBEke2;
259100151217             k_TBElin = TBXlin;
259200151217             k_TBEsif = TBXsif;
259300151217             chain  %kds(K05tntbe01) TNTBE01L;
259400151217             IF  %found(TNTBE01L);
259500151217               clear TBEatb;
259600151217               UPDATE  TNTBE000;
259700151217             ENDIF;
259800121213           WHEN  dsp_aid = c_F16;
259900121213             //?Annullo anche la LA2 se esiste
260000121213             k_TBEcod = 'LA2';
260100121213             k_TBEke1 = %editc(V2Cccm:'X');
260200121213             clear k_TBEke2;
260300121213             k_TBElin = TBXlin;
260400121213             k_TBEsif = TBXsif;
260500121213             chain  %kds(K05tntbe01) TNTBE01L;
260600121213             IF  %found(TNTBE01L);
260700121213               TBEatb = 'A';
260800121213               UPDATE  TNTBE000;
260900121213             ENDIF;
261000151217             //?Annullo anche la JDC se esiste
261100151217             k_TBEcod = 'JDC';
261200151217             k_TBEke1 = %editc(V2Cccm:'X');
261300151217             clear k_TBEke2;
261400151217             k_TBElin = TBXlin;
261500151217             k_TBEsif = TBXsif;
261600151217             chain  %kds(K05tntbe01) TNTBE01L;
261700151217             IF  %found(TNTBE01L);
261800151217               TBEatb = 'A';
261900151217               UPDATE  TNTBE000;
262000151217             ENDIF;
262100121213           OTHER;
262200121213           //?Modifica
262300121217           //?se viene tolto il flag si altra direcotry controllo se esiste rcd LA2
262400121213           //?se esiste cancello
262500180301           //preferisco annullare e non deletare
262600121217             IF  §LACla2 = *blanks;
262700121213               k_TBEcod = 'LA2';
262800121213               k_TBEke1 = %editc(V2Cccm:'X');
262900121213               clear k_TBEke2;
263000121213               k_TBElin = TBXlin;
263100121213               k_TBEsif = TBXsif;
263200121213               chain  %kds(K05tntbe01) TNTBE01L;
263300121213               IF  %found(TNTBE01L);
263400180301                 TBEatb = 'A';
263500180301                 UPDATE TNTBE000;
263600180301                //DELETE  TNTBE000;
263700121213               ENDIF;
263800121213             ENDIF;
263900151217           //?se viene tolto il flag si merge controllo se esiste rcd JDC
264000151217           //?se esiste cancello
264100180301           // cancello solo se prima della variazione la tabella LAC aveva il flag del merge
264200180301           // e anche la tabella JDC deve avere il flag del merge
264300180301           // inoltre preferisco annullare e non deletare
264400151217             IF  §LACmerge = *blanks;
264500180301               IF  sav_LACmerge <> *blanks and sav_JDCmerge <> *blanks;
264600151217               k_TBEcod = 'JDC';
264700151217               k_TBEke1 = %editc(V2Cccm:'X');
264800151217               clear k_TBEke2;
264900151217               k_TBElin = TBXlin;
265000151217               k_TBEsif = TBXsif;
265100151217               chain  %kds(K05tntbe01) TNTBE01L;
265200151217               IF  %found(TNTBE01L);
265300180301                 TBEatb = 'A';
265400180301                 UPDATE TNTBE000;
265500180301                //DELETE  TNTBE000;
265600151217               ENDIF;
265700180301               ENDIF;
265800151217             ENDIF;
265900121213         ENDSL;
266000071207
266100071207       ENDSR;
266200071207
266300071212       //--------------------------------------------------------------
266400090306       //?Operazioni finali.                                           ?
266500071212       //--------------------------------------------------------------
266600090306       BEGSR  sr_RoutEnd;
266700071207
266800090306         clear  TIBS69ds;
266900090304         tibs69ds.I69tla = 'C';
267000090304         tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
267100071207
267200071217         kpjbu = TNTB46ds;
267300071207         return;
267400071207
267500071207       ENDSR;
267600090306
267700071207      /end-free
267800071207
267900071212       //--------------------------------------------------------------
268000090306       //?Schiere a tempo di compilazione.                             ?
268100071212       //--------------------------------------------------------------
268200071207
268300071207** - $MSG -------------------------------------------------------------------*
268400071207Immettere il codice cliente                                                     1
268500071207Immettere SOLO caratteri numerici                                               2
268600071207Codice errato                                                                   3
268700071207Cliente annullato                                                               4
268800071207Immettere la directory                                                          5
268900071207Carattere "\" non valido; per indicare una sottocartella utilizzare "/"         6
269000071210Caratteri & e %  non ammessi per la directory di clienti con invio automatico   7
269100071207Indicare il giorno del mese se specificato almeno un mese                       8
269200071207Specificare almeno un mese se indicato il giorno del mese                       9
269300071207Per i mesi con n° di giorni < &g verrà assunto il loro ultimo giorno -> Enter  10
269400090225Inserire il codice padre                                                       11
269500090225Il codice padre deve essere = al codice cliente o = a cliente già in tabella   12
269600120112Cliente da copiare NON più reperibile in tab. "LAC"                            13
269700120112Cliente già inserito in tab. "LAC"                                             14
269800141203Data errata                                                                    15
