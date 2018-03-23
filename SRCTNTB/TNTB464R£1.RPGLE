000100090306       //--------------------------------------------------------------
000200121213       //?GESTIONE TABELLA "LA2" - Estensione tabella LAC
000300090306       //--------------------------------------------------------------
000400090306
000500071206     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600071206
000700090306       //--------------------------------------------------------------
000800090306       //?Dichiarazione file.                                          ?
000900090306       //--------------------------------------------------------------
001000090306
001100090311     fAZORG01L  if   e           k disk
001200090311
001300071206     fTNTBE01L  Uf A e           k disk
001400090311
001500121213     fTNTB464D  cf   e             workstn
001600071207     f                                     indds(IndDspF)
001700090306     f                                     infds(InfDspF)
001800071207
001900090306       //--------------------------------------------------------------
002000090306       //?Definizione costanti.                                        ?
002100090306       //--------------------------------------------------------------
002200090306
002300090306       // - Tabella in gestione
002400121213     d c_Tab           c                   const('LA2')
002500090306
002600090306       // - Costante per controllo "caratteri solo numerici"
002700090310     d c_Digits        c                   const('0123456789')
002800090306
002900090306       // - Tasti funzionali a video
003000090306     d c_F01           c                   const(x'31')
003100090306     d c_F02           c                   const(x'32')
003200090306     d c_F03           c                   const(x'33')
003300090319     d c_F04           c                   const(x'34')
003400090306     d c_F05           c                   const(x'35')
003500090306     d c_F06           c                   const(x'36')
003600090306     d c_F07           c                   const(x'37')
003700090306     d c_F08           c                   const(x'38')
003800090306     d c_F09           c                   const(x'39')
003900090306     d c_F10           c                   const(x'3A')
004000110713     d c_F11           c                   const(x'3B')
004100090306     d c_F12           c                   const(x'3C')
004200090306     d c_F13           c                   const(x'B1')
004300090306     d c_F14           c                   const(x'B2')
004400090306     d c_F15           c                   const(x'B3')
004500090306     d c_F16           c                   const(x'B4')
004600090306     d c_F17           c                   const(x'B5')
004700090306     d c_F18           c                   const(x'B6')
004800090306     d c_F19           c                   const(x'B7')
004900090306     d c_F20           c                   const(x'B8')
005000090306     d c_F21           c                   const(x'B9')
005100090306     d c_F22           c                   const(x'BA')
005200090306     d c_F23           c                   const(x'BB')
005300090306     d c_F24           c                   const(x'BC')
005400090306     d c_Enter         c                   const(x'F1')
005500090306     d c_RollDown      c                   const(x'F4')
005600090306     d c_RollUp        c                   const(x'F5')
005700071207
005800090306       //--------------------------------------------------------------
005900090306       //?Definizione schiere.                                         ?
006000090306       //--------------------------------------------------------------
006100090306
006200090306       // - Messaggi a video
006300120112     d $Msg            s             78    dim(14)  ctdata  perrcd(1)
006400071207
006500090306       //--------------------------------------------------------------
006600090306       //?Definizione aree dati.                                       ?
006700090306       //--------------------------------------------------------------
006800090306
006900090306       // - Dati utente
007000071207     d §AzUte        e ds                  extname(AZUTE00F)
007100071207     d                                     dtaara
007200071207     d §DatiUte      e ds                  extname(dDatiUte)
007300071207     d                                     dtaara
007400071207
007500090306       //--------------------------------------------------------------
007600090306       //?Definizione strutture dati.                                  ?
007700090306       //--------------------------------------------------------------
007800090306
007900090306       // - Status
008000071207     d Psds           sds
008100071207     d   SDSpgm          *proc
008200071207
008300090306       // - InfDS
008400071207     d InfDspF         ds
008500071207     d  dsp_aid              369    369a                                        AID byte
008600071207
008700090306       // - Indicatori su DspF
008800090311     d IndDspF         ds
008900090306         // - Abilitazione tasti funzionali
009000090311     d  F5Attivo                       n   overlay(IndDspF:05)
009100090311     d  F6Attivo                       n   overlay(IndDspF:06)
009200090311     d  F16Attivo                      n   overlay(IndDspF:16)
009300090306         // - Emissione messaggio di errore
009400090311     d  ErrMessage                     n   overlay(IndDspF:28)
009500090306         // - Protezione campi
009600090311     d  ProtDATAI                      n   overlay(IndDspF:40)
009700120112     d  InserCCM2                      n   overlay(IndDspF:41)
009800090306         // - Posizionamento cursore & segnalazione errore
009900090311     d  PosCurCcm                      n   overlay(IndDspF:50)
010000090311     d  PosCurAut                      n   overlay(IndDspF:51)
010100090311     d  PosCurTpi                      n   overlay(IndDspF:52)
010200090311     d  PosCurDir                      n   overlay(IndDspF:53)
010300090311     d  PosCurSchGg                    n   overlay(IndDspF:54)
010400090311     d  PosCurSch01                    n   overlay(IndDspF:55)
010500090311     d  PosCurSch02                    n   overlay(IndDspF:56)
010600090311     d  PosCurSch04                    n   overlay(IndDspF:57)
010700090311     d  PosCurSch06                    n   overlay(IndDspF:58)
010800090311     d  PosCurSch09                    n   overlay(IndDspF:59)
010900090311     d  PosCurSch11                    n   overlay(IndDspF:60)
011000090311     d  PosCurKsu                      n   overlay(IndDspF:61)
011100090311     d  PosCurMesi                     n   overlay(IndDspF:62)
011200090311     d  PosCurTdta                     n   overlay(IndDspF:63)
011300090311     d  PosCurTad                      n   overlay(IndDspF:64)
011400090311     d  PosCurTadu                     n   overlay(IndDspF:65)
011500090311     d  PosCurKsc                      n   overlay(IndDspF:66)
011600090311     d  PosCurCtr                      n   overlay(IndDspF:67)
011700090311     d  PosCurImp                      n   overlay(IndDspF:68)
011800090311     d  PosCurFimp                     n   overlay(IndDspF:69)
011900090311     d  PosCurFmi                      n   overlay(IndDspF:70)
012000090311     d  PosCurDatai                    n   overlay(IndDspF:71)
012100090311     d  PosCurLnp                      n   overlay(IndDspF:72)
012200090327     d  PosCurTpt                      n   overlay(IndDspF:73)
012300110713     d  PosCurPgm                      n   overlay(IndDspF:74)
012400110713     d  PosCurEle                      n   overlay(IndDspF:75)
012500120112     d  PosCurCCM2                     n   overlay(IndDspF:76)
012600090306         //   - Riemissione videata
012700090311     d  ErrGenerico                    n   overlay(IndDspF:99)
012800071207
012900090306       // - Parametri ricevuti
013000071206     d KPJBA         e ds
013100121213     d TNTB464ds     e ds
013200071207
013300090306       // - Reperimento dati utente
013400071207     d TIBS34ds      e ds
013500071207
013600090306       // - Controllo/Decodifica cliente
013700090304     d TIBS69ds      e ds                  qualified  inz
013800090304     d ds_CNACO      e ds                  extname(CNACO00F)
013900090304     d                                     qualified  inz
014000090304     d ds_CNIND      e ds                  extname(CNIND00F)
014100090304     d                                     qualified  inz
014200090304     d ds_CNCLP      e ds                  extname(CNCLP00F)
014300090304     d                                     qualified  inz
014400090304     d ds_FNCLS      e ds                  extname(FNCLS00F)
014500090304     d                                     qualified  inz
014600090306
014700090306       // - Dati record principale della tabella
014800090306     d TNTBEds       e ds                  inz  extname(TNTBE00F)
014900090306     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
015000090306     d                                          prefix(TBX:3)
015100090312
015200090312       // - Parametri per Ricerca/controllo tabelle
015300090312     d TIBS02ds      e ds                  inz
015400090312     d  T02mod       e                     inz('C')
015500090312     d  T02cod       e                     inz(c_Tab)
015600071207
015700121213       // - Tabella "LA2"
015800121213     d dLA2          e ds                  inz
015900121213       // - Tabella "LA2" - padre
016000121213     d dLA2p         e ds                  extname(dLA2) inz
016100121213     d                                     prefix(p_)
016200121213     d  p_SchMm               71     82
016300121213       // - Tabella "LAC"
016400121213     d dLAC          e ds                  inz
016500121016
016600090312       // - Tabella "GED" = Divise area gestionale
016700090312     d dGED          e ds                  inz
016800090312       // - Tabella "GEI" = Importi vari area gestionale
016900090312     d dGEI          e ds                  inz
017000090303
017100090306       // - Mesi in cui è schedulata la mail di promemoria creazione CD
017200090303     d ds_SchMm        ds            12    inz
017300090303     d  V2Csch01                           inz
017400090303     d  V2Csch02                           inz
017500090303     d  V2Csch03                           inz
017600090303     d  V2Csch04                           inz
017700090303     d  V2Csch05                           inz
017800090303     d  V2Csch06                           inz
017900090303     d  V2Csch07                           inz
018000090303     d  V2Csch08                           inz
018100090303     d  V2Csch09                           inz
018200090303     d  V2Csch10                           inz
018300090303     d  V2Csch11                           inz
018400090303     d  V2Csch12                           inz
018500090311
018600090311       // - Struttura di comodo x ctrl mesi di schedulazione
018700090311     d wSchMm_ds       ds            12    inz
018800090311     d  $SchMm                 1     12    dim(12)
018900071207
019000090306       //--------------------------------------------------------------
019100090306       //?Definizione procedure usate.                                 ?
019200090306       //--------------------------------------------------------------
019300090306
019400090306       // - Reperimento dati utente
019500090306      /copy gaitrasrc/srcProtoPR,TIBS34R
019600071207
019700090304       // - Ricerca/Controllo tabelle
019800090306      /copy gaitrasrc/srcProtoPR,TIBS02R
019900071207
020000090304       // - Controllo/Decodifica cliente
020100090306      /copy gaitrasrc/srcProtoPR,TIBS69R
020200071207
020300090306       //--------------------------------------------------------------
020400090306       //?Definizione variabili globali.                               ?
020500090306       //--------------------------------------------------------------
020600090306
020700090306       // - Flags booleani
020800090306     d $InzD01         s               n   inz(*on)
020900090306     d $InzD02         s               n   inz(*on)
021000090306     d $Fine           s               n   inz
021100090923     d $forza          s               n   inz
021200090306     d $CtrMm          s               n   inz
021300090313     d $ByPass         s               n   inz
021400120112     d $Copia          s               n   inz
021500121016     d wForza          s               n   inz
021600121030     d wForzaTPI       s               n   inz
021700121030     d wForzaFMI       s               n   inz
021800121030     d wForzaDIR       s               n   inz
021900071207
022000090306       // - Gestione video
022100071206     d $Video          s              2    inz('D1')
022200090313
022300090313       // - Indici di schiera
022400090313     d xx              s              3  0 inz
022500071207
022600090313       // - Campi di comodo
022700090306     d wDataI          s              6
022800090306     d wDatEla         s              6
022900090306     d wPos1           s              2  0 inz
023000090306     d wPos2           s              2  0 inz
023100090313     d wCount          s              2  0 inz
023200090313     d wSavSch         s                   like(ds_SchMm)  inz
023300090316     d SAVke1          s                   like(TBEke1)    inz
023400121213     d savtad          s                   like(§la2tad)
023500121213     d savtadu         s                   like(§la2tadu)
023600121213     d savksc          s                   like(§la2ksc)
023700121213     d savctr          s                   like(§la2ctr)
023800121213     d savimp          s                   like(§la2imp)
023900121213     d savfimp         s                   like(§la2fimp)
024000071207
024100090306       //--------------------------------------------------------------
024200090306       //?Definizione key-list.                                        ?
024300090306       //--------------------------------------------------------------
024400090306
024500090306       // - File TNTBE01L
024600090306     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
024700090306     d                                     prefix(k_)  inz
024800071206
024900090306       //--------------------------------------------------------------
025000090306       //?Riepilogo indicatori.                                        ?
025100090306       //--------------------------------------------------------------
025200090306       // 01    - RECORD annullato
025300090306       // 02    - acceso IMMISSIONE - spento MANUTENZIONE
025400090306       // 25    - Comodo
025500090306       // 28    - Emissione del messaggio di errore a video
025600090306       // 40    - Protezione campi in prima estrazione
025700090306       // x D01:
025800090306       // 50    - Codice cliente errato o mancante
025900090306       // x D02:
026000120112       // 41    - Copia in D02: V2CCCM non protetto
026100090306       // 53    - Directory per immagini       errata
026200090306       // 54    - Giorno di schedulazione CD   errato
026300090306       // 55    - Selezione mesi obbligatoria
026400090306       // 56    - Selezione del mese di febbraio
026500090306       // 57    - Selezione del mese di aprile
026600090306       // 58    - Selezione del mese di giugno
026700090306       // 59    - Selezione del mese di settembre
026800090306       // 60    - Selezione del mese di novembre
026900090306       // 61    - Codice Padre                 errato
027000090306       // 62    - N. mesi da estrarre          errato
027100090306       // 63    - Tipo data di elaborazione    errato
027200090306       // 64    - Tipo addebito                errato
027300090306       // 65    - Creazione addebito           errato
027400090306       // 66    - Cliente tassazione           errato
027500090306       // 67    - Codice tariffa               errato
027600090306       // 68    - Importo forzato              errato
027700090306       // 69    - Tipo importo forzato         errato
027800090306       // 70    - Flag nome immagine           errato
027900090306       // 71    - Prima elaborazione           errata
028000090306       // 99    - Generico di errore
028100090306       //--------------------------------------------------------------
028200071206
028300071206     c     *Entry        plist
028400071206     c                   parm                    KPJBA
028500121213     c                   parm                    tntb464ds
028600121213
028700071207      /free
028800071207
028900071207       // Operazioni iniziali
029000090306       exsr  sr_RoutInz;
029100071207
029200071207       // Gestione video
029300090306       DOW  $Fine = *off;
029400071207         select;
029500090306           when  $Video = 'D2';
029600090306             exsr  sr_GesD02;
029700071207           other;
029800071207             leave;
029900071207         endsl;
030000071207       ENDDO;
030100071207
030200071207       // Operazioni finali
030300090306       exsr  sr_RoutEnd;
030400071206
030500071212       //--------------------------------------------------------------
030600090306       //?Operazioni iniziali.                                         ?
030700071212       //--------------------------------------------------------------
030800090306       BEGSR  sr_RoutInz;
030900090306
031000090306         *inLR = *on;
031100121213         B464fxx = *blank;
031200121213         B464err = *off;
031300121213         B464msg = *blank;
031400071207
031500090306         // Reperimento dati job
031600090306         exsr  DatiJob;
031700071207
031800090306         // Impostazione nome programma a video
031900071207         V1Tpgm = SDSpgm;
032000090306
032100090306         // Aggancio dati generali della tabella in esame
032200090306         clear  k_TBEcod;
032300090306         k_TBEke1 = *zero;
032400090306         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
032500090306                = c_Tab;
032600090306         clear  k_TBEke2;
032700090306         clear  k_TBElin;
032800090306         k_TBEsif = KNSIF;
032900090306         chain(n)  %kds(k05tntbe01)  TNTBE000;
033000090306         if  not  %found(TNTBE01L);
033100090306           clear  k_TBEsif;
033200090306           chain(n)  %kds(k05tntbe01)  TNTBE000;
033300090306         endif;
033400090306         if  %found(TNTBE01L);
033500090306           xTNTBEds = TNTBEds;
033600090306         else;
033700090306           clear  xTNTBEds;
033800090306         endif;
033900071207
034000090306         // Verifica parametri ricecvuti
034100121213         if  B464ke1  <> *blank;
034200121213           exsr  CtrDati;
034300121213           if  ErrMessage;
034400121213             B464err = *on;
034500121213             B464msg = V1Dmsg;
034600071217             $Fine  = *on;
034700071217             leavesr;
034800071217           endif;
034900071207           $Video  = 'D2';
035000071207           $InzD02 = *on;
035100071207         endif;
035200071207
035300071207       ENDSR;
035400071206
035500071212       //--------------------------------------------------------------
035600090306       //?Reperimento Dati del job (Utente/Operativi).                 ?
035700071212       //--------------------------------------------------------------
035800090306       BEGSR  DatiJob;
035900071207
036000071207         in(E) §AzUte;
036100071207         if NOT %error;
036200071207           in(E) §DatiUte;
036300071207         endif;
036400090306         if %error or RSut = *blank;
036500071207           clear TIBS34ds;
036600071207           tibs34r(tibs34ds);
036700071207           in §AzUte;
036800071207           in §DatiUte;
036900071207         endif;
037000071207
037100071207       ENDSR;
037200071206
037300071212       //--------------------------------------------------------------
037400121213       //?Controllo dati passati.
037500071212       //--------------------------------------------------------------
037600121213       BEGSR  CtrDati;
037700071207
037800121213         IF  B464ke1 = *blank  or  B464ke1 = *zero;
037900121213           ErrMessage  = *on;
038000121213           ErrGenerico = *on;
038100121213           B464msg = $Msg(01);
038200121213           leavesr;
038300121213         ENDIF;
038400121213
038500121213         clear  TIBS69ds;
038600121213         tibs69ds.I69kcc = DUTkci;
038700121213         tibs69ds.I69kac = %int(%subst(B464ke1:1:7));
038800121213         tibs69ds.I69sif = knsif;
038900121213         tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
039000071207
039100090306         // - Verifica esistenza codice cliente in tabella
039200090306         reset  TNTBE000;
039300090310         k_TBEcod = c_Tab;
039400121213         k_TBEke1 = B464ke1;
039500090306         clear  k_TBEke2;
039600090306         k_TBElin = TBXlin;
039700090306         k_TBEsif = TBXsif;
039800120112
039900121213         chain  %kds(K05tntbe01) TNTBE000;
040000121213         // - - NON ammesso nuovo record per cliente annullato
040100121213         if  NOT  %found(TNTBE01L)  and  ds_CNACO.ACOflg <> *blank;
040200121213           ErrMessage  = *on;
040300121213           ErrGenerico = *on;
040400121213           B464msg = $Msg(04);
040500121213           leavesr;
040600121213         ENDIF;
040700071207
040800120112         // - Decodifica cliente
040900121213         IF  %found(TNTBE01L);
041000121213           dLA2 = TBEuni;
041100121213         ELSE;
041200121213           §LA2rag = %subst(ds_CNACO.ACOrag : 1 : %len(V2Dccm));
041300121213         ENDIF;
041400071207
041500071207       ENDSR;
041600071206
041700071212       //--------------------------------------------------------------
041800090306       //?Gestione videata D02                                         ?
041900071212       //--------------------------------------------------------------
042000090306       BEGSR  sr_GesD02;
042100071207
042200090306         // Inizializzazione videata
042300090306         if   $InzD02 = *on;
042400090306           exsr  sr_InzD02;
042500090306           $InzD02 = *off;
042600071207         endif;
042700071207
042800090306         // Emissione testata
042900121213         write  TN464T01;
043000071207
043100090306         // Emissione videata
043200121213         if  B464opz = *blank;
043300121213           exfmt  TN464D02;
043400071217         else;
043500121213           write  TN464D02;
043600090306           exfmt  PROTECT;
043700071217         endif;
043800071207         errMessage  = *off;
043900071207         errGenerico = *off;
044000090306         clear  V1Dmsg;
044100071207
044200071207         SELECT;
044300090306           // F12=Ritorno
044400090306           WHEN  dsp_aid = c_F12;
044500090306             exsr  sr_F12D02;
044600090306           // Enter o F5 o F6 o F16
044700071207           OTHER;
044800090306             // - Controllo dati
044900090306             if  dsp_aid = c_F16;
045000090306               exsr  sr_CtrAnnull;
045100090306             else;
045200090306               exsr  sr_CtrD02;
045300090306             endif;
045400090306             if  errGenerico;
045500071207               leavesr;
045600071207             endif;
045700121212
045800090306             // - Conferma dati:
045900090306             //   F5=Ripristino; F6=Conferma; F16=Annullamento
046000090306             if  dsp_aid = c_F05  or
046100090306                 dsp_aid = c_F06  or
046200090306                 dsp_aid = c_F16;
046300090306               exsr  Upd_TNTBE;
046400121212
046500071207               if  NOT  errGenerico;
046600121213                 $Fine  = *on;
046700071207               endif;
046800071207             EndIf;
046900071207
047000071207         ENDSL;
047100071207
047200071207       ENDSR;
047300071206
047400071212       //--------------------------------------------------------------
047500090306       //?Inizializzazione videata D01                                 ?
047600071212       //--------------------------------------------------------------
047700090306       BEGSR  sr_InzD02;
047800071207
047900090306         reset  $CtrMm;
048000090313         reset  $ByPass;
048100121016         reset  wForza;
048200121030         reset  wForzaTPI;
048300121030         reset  wForzaFMI;
048400121030         reset  wForzaDIR;
048500101103
048600121213         clear  TN464D02;
048700071210
048800090306         IndDspF = *zero;
048900090309
049000090309         // Impostazione indicatori per abilitazione tasti funzionali
049100090309         exsr  Set_Ind_D02;
049200090309
049300120112         // - Codice estrazione (chiave)
049400121213         V2Cccm = %int(%subst(B464ke1:1:7));
049500121213         V2Dccm = §LA2rag;
049600121213
049700121213         // Immissione nuovo rcd recupero dati da tabella LAC
049800121213         IF  not %found(TNTBE01L);
049900121213           V1Tlav = 'IMMISSIONE';
050000121213           reset  TIBS02ds;
050100121213           T02cod = 'LAC';
050200121213           T02ke1 = B464ke1;
050300121213           TNTBE_RicercaControllo(kpjba : tibs02ds);
050400121213           if  T02err = *blank;
050500121213             dLAC = T02uni;
050600121213             dLA2 = T02uni;
050700121213           endif;
050800121213           clear §LA2dir;
050900121213           *in02 = *on;
051000121213         ENDIF;
051100071207
051200121213         // Rcd già presente
051300121213         IF  %found(TNTBE01L);
051400090306           // Impostazione dati di record trovato
051500071217           select;
051600121213             when  B464opz = '5';
051700071217               V1Tlav = 'VISUALIZZA';
051800090306             when  TBEatb = *blank;
051900071217               V1Tlav = 'VARIAZIONE';
052000071217             other;
052100071217               V1Tlav = 'ANNULLATO ';
052200071217           endsl;
052300121213           *in02 = *off;
052400121213         ENDIF;
052500090225
052600090306           // Codice padre
052700121213           V2Cksu = §LA2ksu;
052800090309           clear  TIBS69ds;
052900090309           clear  ds_CNACO;
053000090309           clear  ds_CNIND;
053100090309           clear  ds_CNCLP;
053200090309           clear  ds_FNCLS;
053300090310           select;
053400121213             when  §LA2ksu =  V2Cccm;
053500090310               V2Dksu = V2Dccm;
053600121213             when  §LA2ksu <> *zero;
053700090310               tibs69ds.I69kcc = DUTkci;
053800090310               tibs69ds.I69kac = %int(V2Cksu);
053900090310               tibs69ds.I69sif = knsif;
054000090310               tibs69r(TIBS69ds:ds_CNACO:ds_CNIND:ds_CNCLP:ds_FNCLS);
054100090310               if tibs69ds.O69err = *on;
054200090310                 V1Dmsg = $Msg(03);
054300090310                 errMessage  = *on;
054400090310                 errGenerico = *on;
054500090310                 PosCurKsu   = *on;
054600090310                 leavesr;
054700090310               endif;
054800090310               V2Dksu = ds_CNACO.ACOrag;
054900090310           endsl;
055000090225
055100121213           V2Caut   = §LA2aut;
055200121213           V2Ctpi   = §LA2tpi;
055300090225
055400090306           // Flag nome immagine
055500121213           V2Cfmi = §LA2fmi;
055600090310           reset  TIBS02ds;
055700090303           T02cod = 'NIM';
055800121213           T02ke1 = §LA2fmi;
055900090304           TNTBE_RicercaControllo(kpjba : tibs02ds);
056000090306           if  T02err = *blank;
056100090303             V2Dfmi = T02uni;
056200090303           endif;
056300090225
056400121213           V2Cdir   = §LA2dir;
056500121213           V2Ctpt   = §LA2tpt;
056600121213           V2Cssr   = §LA2ssr;
056700121213           V2Clnp   = §LA2lnp;
056800121213           V2Cres   = §LA2res;
056900121213           V2Crec   = §LA2rec;
057000121213           V2Ccsr   = §LA2csr;
057100090309
057200090309           // Prima estrazione
057300090309           clear  wDataI;
057400121213           if  §LA2datai > *zero;
057500121213             %subst(wDataI:1:2) = %subst(%editc(§LA2datai:'X'):5:2);
057600121213             %subst(wDataI:3:4) = %subst(%editc(§LA2datai:'X'):1:4);
057700090310             V2datai = %int(wDataI);
057800090309           endif;
057900090309
058000090309           // Ultima estrazione
058100090309           clear  wDatEla;
058200121213           if  §LA2datela > *zero;
058300121213             %subst(wDatEla:1:2) = %subst(%editc(§LA2datela:'X'):5:2);
058400121213             %subst(wDatEla:3:4) = %subst(%editc(§LA2datela:'X'):1:4);
058500090310             V2datela = %int(wDatEla);
058600090309           endif;
058700090309
058800121213           V2tdta   = §LA2tdta;
058900121213           V2mesi   = §LA2mesi;
059000121213           V2Cschgg = §LA2schgg;
059100121213           V2Csch01 = §LA2sch01;
059200121213           V2Csch02 = §LA2sch02;
059300121213           V2Csch03 = §LA2sch03;
059400121213           V2Csch04 = §LA2sch04;
059500121213           V2Csch05 = §LA2sch05;
059600121213           V2Csch06 = §LA2sch06;
059700121213           V2Csch07 = §LA2sch07;
059800121213           V2Csch08 = §LA2sch08;
059900121213           V2Csch09 = §LA2sch09;
060000121213           V2Csch10 = §LA2sch10;
060100121213           V2Csch11 = §LA2sch11;
060200121213           V2Csch12 = §LA2sch12;
060300090225
060400121213           V2tadu   = §LA2tadu;
060500121213           V2Ctad   = §LA2tad;
060600121213           V2cksc   = §LA2ksc;
060700121213           V2cctr   = §LA2ctr;
060800121213           V2imp    = §LA2imp;
060900121213           V2fimp   = §LA2fimp;
061000121213           V2pgm    = §LA2pgm;
061100121213           V2ele    = §LA2ele;
061200121213           V2note   = §LA2note;
061300090923
061400090923         // Salvo i dati sensibili per la creazione addebito
061500090923         savtad = v2ctad;
061600090923         savtadu = v2tadu;
061700090923         savksc = v2cksc;
061800090923         savctr = v2cctr;
061900090923         savimp = v2imp;
062000090923         savfimp = v2fimp;
062100090923         $forza = *off;
062200071207
062300071207       ENDSR;
062400071217
062500071217       //--------------------------------------------------------------
062600090306       //?Settaggio indicatori nella videata D02 per abilitazione      ?
062700090306       //?  tasti funzionali                                           ?
062800071217       //--------------------------------------------------------------
062900090306       BEGSR  Set_Ind_D02;
063000090319
063100090306         F5Attivo  = ((%found(TNTBE01L) and TBEatb <> *blank)
063200121213                                        and B464opz <> '5');
063300121213         F6Attivo  = (NOT F5Attivo and B464opz <> '5');
063400090306         F16Attivo = ((%found(TNTBE01L) and TBEatb = *blank)
063500121213                                        and B464opz <> '5');
063600090306
063700120112         // - Se ultima estrazione impostata proteggo la prima estrazione
063800121213         ProtDATAI = (§LA2datela <> *zero);
063900071217
064000071217       ENDSR;
064100090319
064200071217
064300071217       //--------------------------------------------------------------
064400090306       //?Gestione tasto funzionale F12 da videata D02                 ?
064500090306       //?F12=Ritorno                                                  ?
064600071217       //--------------------------------------------------------------
064700090306       BEGSR  sr_F12D02;
064800071217
064900121213           B464fxx = 'F12';
065000071217           $Fine = *on;
065100071217
065200071217       ENDSR;
065300090306
065400090306       //--------------------------------------------------------------
065500090306       //?Controllo "annullabilità" del record (padre senza figli)     ?
065600090306       //--------------------------------------------------------------
065700090306       BEGSR  sr_CtrAnnull;
065800090306
065900090306         IndDspF = *zero;
066000090306         // REimpostazione indicatori per abilitazione tasti funzionali
066100090306         exsr  Set_Ind_D02;
066200090306
066300090306         if  V2Cksu <= *zero;
066400121213           V2Cksu = §LA2ksu;
066500090306         endif;
066600090306
066700090306         // Se figlio (non padre): fine controlli
066800090306         if  V2Cccm <> V2Cksu;
066900090306           leavesr;
067000090306         endif;
067100090306
067200090306         //?Verifica se padre con figli?
067300121213         clear  dLA2p;
067400090316         SAVke1 = TBEke1;
067500090306         setll     %kds(k05tntbe01 : 1)  TNTBE000;
067600090306         reade(n)  %kds(k05tntbe01 : 1)  TNTBE000;
067700090306
067800090306         dow  not  %eof(TNTBE01L);
067900090316           if  TBEatb = *blank   and
068000090316               %editc(V2Cccm : 'X') <> %trimr(TBEke1);
068100121213             dLA2p = TBEuni;
068200121213             if  p_§LA2ksu = V2Cccm;
068300090306               errMessage  = *on;
068400090306               errGenerico = *on;
068500090306               PosCurKsu   = *on;
068600090306               V1Dmsg = 'Record NON cancellabile: trattasi di PADRE +
068700090306                         con figli (vedi ' + %trimr(TBEke1) + ')';
068800090306               leavesr;
068900090306             endif;
069000090306           endif;
069100090306         reade(n)  %kds(k05tntbe01 : 1)  TNTBE000;
069200090306         enddo;
069300090316
069400120112         // - Riaggancio del rec. in manutenzione/copia
069500090316         TBEke1 = SAVke1;
069600090316         reset  TNTBE000;
069700090316         k_TBEcod = c_Tab;
069800120112         k_TBEke1 = %editc( V2Cccm : 'X' );
069900090316         clear  k_TBEke2;
070000090316         k_TBElin = TBXlin;
070100090316         k_TBEsif = TBXsif;
070200090316         chain  %kds(K05tntbe01) TNTBE000;
070300090306
070400090306       ENDSR;
070500071206
070600071212       //--------------------------------------------------------------
070700090306       //?Controllo videata D02                                        ?
070800071212       //--------------------------------------------------------------
070900090306       BEGSR  sr_CtrD02;
071000071207
071100090309         %subst(IndDspF : 50) = *off;
071200090313
071300090313         // Verifica se variata la schedulazione dell'estrazione
071400090313         if  $ByPass;
071500090313           $ByPass = (ds_SchMm = wSavSch);
071600090313         endif;
071700090313         wSavSch = ds_SchMm;
071800090225
071900090306         //?Codice padre?
072000090225         select;
072100090306           when  V2Cksu  = *zero;
072200090306             errMessage  = *on;
072300090225             errGenerico = *on;
072400090303             PosCurKsu   = *on;
072500090225             V1Dmsg = $Msg(11);
072600090225             leavesr;
072700090306           when  V2Cksu = V2Cccm;
072800090303             V2Dksu = V2Dccm;
072900090225           other;
073000090327             // - Controllo esistenza codice padre come cliente
073100121213             //   in tabella LA2
073200090303             exsr  sr_Padre;
073300090303             if  errGenerico = *on;
073400090226               leavesr;
073500090225             endif;
073600121213             V2Dksu = p_§LA2rag;
073700090226         endsl;
073800101103         //?SE codice padre <> da codice cliente?
073900101103         // ?& sono in immissione?
074000101103         IF  V2Cccm <> V2Cksu   and   *in02;
074100101103           // ?& il padre ha già fatto estrazioni?
074200121213           If  p_§LA2datela <> *zero;
074300101103             // ?=> riporto le date del padre, proteggo il campo e non controllo?
074400101103             clear wDataI;
074500121213             %subst(wDataI:1:2) = %subst(%editc(p_§LA2datai:'X'):5:2);
074600121213             %subst(wDataI:3:4) = %subst(%editc(p_§LA2datai:'X'):1:4);
074700101103             V2datai = %int(wDataI);
074800101103             clear wDatEla;
074900121213             %subst(wDatEla:1:2) = %subst(%editc(p_§LA2datela:'X'):5:2);
075000121213             %subst(wDatEla:3:4) = %subst(%editc(p_§LA2datela:'X'):1:4);
075100101103             V2datela = %int(wDatEla);
075200101103             ProtDATAI = *on;
075300101103           EndIf;
075400100521         ENDIF;
075500090311
075600090311         //?Tipo elaborazione?
075700090327           // - Obbligatorio
075800121213           IF    V2Caut <> 'A'  and  V2Caut <> 'S'  and  V2Caut <> 'N';
075900090327             errMessage  = *on;
076000090327             errGenerico = *on;
076100090327             PosCurAut   = *on;
076200090327             V1Dmsg = 'Tipo elaborazione errato o mancante';
076300090327             leavesr;
076400121213         endif;
076500090327
076600090327         //?Tipo immagine?
076700120918         if  V2Ctpi <> 'TIF'   and   V2Ctpi <> 'JPG' and
076800120918             V2Ctpi <> 'PDF';
076900090327           errMessage  = *on;
077000090327           errGenerico = *on;
077100090327           PosCurTpi   = *on;
077200090327           V1Dmsg = 'Tipo immagine errato o mancante';
077300090327           leavesr;
077400090327         endif;
077500090226
077600090306         //?Flag nome immagine?
077700090310         clear  V2Dfmi;
077800090310         select;
077900090310           // - Obbligatorietà
078000090310           when  V2Cfmi = *blank;
078100090310             errMessage  = *on;
078200090310             errGenerico = *on;
078300090310             PosCurFmi   = *on;
078400090310             V1Dmsg = 'Flag nome immagine obbligatorio';
078500090310             leavesr;
078600090310           // - Ricerca
078700090310           when  %scan('?' : V2Cfmi) > *zero;
078800090310             clear  TIBS02ds;
078900090310             T02mod = 'R';
079000090310             T02cod = 'NIM';
079100090310             T02sif = KNSIF;
079200090310             TNTBE_RicercaControllo(kpjba : tibs02ds);
079300090310             if  T02err = *blank;
079400090310               V2Cfmi = %subst(T02ke1 : 1 : %len(V2Cfmi));
079500090310               V2Dfmi = T02uni;
079600090310               errGenerico = *on;
079700090310               leavesr;
079800090310             else;
079900090310               errMessage  = *on;
080000090310               errGenerico = *on;
080100090310               PosCurFmi   = *on;
080200090310               V1Dmsg = T02msg;
080300090310               leavesr;
080400090310             endif;
080500090310           // - Controllo
080600090310           other;
080700090310             reset  TIBS02ds;
080800090310             T02cod = 'NIM';
080900090310             T02ke1 = V2Cfmi;
081000090310             TNTBE_RicercaControllo(kpjba : tibs02ds);
081100090310             if  T02err <> *blank;
081200090310               errMessage  = *on;
081300090310               errGenerico = *on;
081400090310               PosCurFmi   = *on;
081500090310               V1Dmsg = 'Flag nome immagine errato';
081600090310               leavesr;
081700090310             endif;
081800090310             V2Dfmi = T02uni;
081900090310         ENDSL;
082000090226
082100090306         //?Directory per immagini?
082200090226         select;
082300090306           when  V2Cdir  = *blank;
082400090226             errMessage  = *on;
082500090226             errGenerico = *on;
082600090226             PosCurDir   = *on;
082700090226             V1Dmsg = $Msg(05);
082800090226             leavesr;
082900121213           when  V2Cdir  = §LACdir;
083000121213             errMessage  = *on;
083100121213             errGenerico = *on;
083200121213             PosCurDir   = *on;
083300121213             V1Dmsg = 'La directory deve essere diversa da tab. LAC';
083400121213             leavesr;
083500090316           when  %subst(V2Cdir : 1 : 1) = *blank;
083600090316             errMessage  = *on;
083700090316             errGenerico = *on;
083800090316             PosCurDir   = *on;
083900090316             V1Dmsg = 'Non ammessi spazi vuoti all''inizio del nome +
084000090316                       della directory';
084100090316             leavesr;
084200090306           when  %scan('\':V2Cdir) > *zero;
084300090226             errMessage  = *on;
084400090226             errGenerico = *on;
084500090226             PosCurDir   = *on;
084600090226             V1Dmsg = $Msg(06);
084700090226             leavesr;
084800090306           when  %scan('%':V2Cdir) > *zero   and
084900090306                           V2Caut  = 'A';
085000090226             errMessage  = *on;
085100090226             errGenerico = *on;
085200090226             PosCurDir   = *on;
085300090226             V1Dmsg = $Msg(07);
085400090226             leavesr;
085500090309           when  %scan('&':V2Cdir) > *zero  and
085600090309                           V2Caut  = 'A';
085700090309             errMessage  = *on;
085800090309             errGenerico = *on;
085900090309             PosCurDir   = *on;
086000090309             V1Dmsg = $Msg(07);
086100090309             leavesr;
086200090226         endsl;
086300090309
086400090316         wPos1 = %scan('/' : V2Cdir);
086500090316         DoW  wPos1 > *zero;
086600090316           if  %subst( V2Cdir : wPos1 : 2 ) = '//';
086700090316             errMessage  = *on;
086800090316             errGenerico = *on;
086900090316             PosCurDir   = *on;
087000090316             V1Dmsg = 'Nome directory NON valido: non ammesso il +
087100090316                       doppio "/"';
087200090316             leavesr;
087300090316           else;
087400090316             wPos1 = %scan('/' : V2Cdir : wPos1 + 1 );
087500090316           endif;
087600090316         EndDo;
087700090316
087800090309         wPos1 = %scan('&' : V2Cdir);
087900090309         IF  wPos1 > *zero;
088000090309           if  %subst( V2Cdir : wPos1 : 4 ) <> '&AAA'   and
088100090309               %subst( V2Cdir : wPos1 : 2 ) <> '&M';
088200090309             errMessage  = *on;
088300090309             errGenerico = *on;
088400090309             PosCurDir   = *on;
088500090309             V1Dmsg = 'Ammessi solo "&AAA" e "&M" (come caratteri +
088600090309                       sostituibili)';
088700090309             leavesr;
088800090309           endif;
088900090309           wPos2 = %scan( '&' : V2Cdir : wPos1 + 1 );
089000090309           If  wPos2 > *zero;
089100090309             if  %subst( V2Cdir : wPos2 : 4 ) <> '&AAA'   and
089200090309                 %subst( V2Cdir : wPos2 : 2 ) <> '&M';
089300090309               errMessage  = *on;
089400090309               errGenerico = *on;
089500090309               PosCurDir   = *on;
089600090309               V1Dmsg = 'Ammessi solo "&AAA" e "&M" (come caratteri +
089700090309                         sostituibili)';
089800090309               leavesr;
089900090309             endif;
090000090309           EndIf;
090100090309         ENDIF;
090200090309
090300090309         wPos1 = %scan('&M' : V2Cdir);
090400090309         IF  wPos1 > *zero;
090500090309           wPos2 = %scan( '&M' : V2Cdir : wPos1 + 1 );
090600090309           If  wPos2 > *zero   and
090700090309               %scan( '&M' : V2Cdir : wPos2 + 1 ) > *zero;
090800090309             errMessage  = *on;
090900090309             errGenerico = *on;
091000090309             PosCurDir   = *on;
091100090309             V1Dmsg = 'Ammessi max 2 "&M"';
091200090309             leavesr;
091300090309           EndIf;
091400090309         ENDIF;
091500090226
091600090327         // - Se codice padre <> da codice cliente
091700090306         //   la directory deve coincidere almeno fino alla 2ª subdir
091800121213         IF  V2Cccm <> V2Cksu   and   p_§LA2dir <> V2Cdir;
091900090311
092000090311           // - Controllo 1ª directory
092100090311           wPos1 = %scan( '/' : %trim( V2Cdir ) );
092200121213           wPos2 = %scan( '/' : %trim( p_§LA2dir ) );
092300090313           If  wPos1 <> wPos2                               OR
092400090313             ( wPos1  > *zero   and   wPos2 > *zero   and
092500090311               %subst( %trim( V2Cdir    ) : 1 : wPos1 ) <>
092600121213               %subst( %trim( p_§LA2dir ) : 1 : wPos2 ) )   OR
092700090313             ( wPos1  = *zero   and   wPos2  =  *zero
092800121213                                and   V2Cdir <> p_§LA2dir );
092900090311             errMessage  = *on;
093000090311             errGenerico = *on;
093100090311             PosCurDir   = *on;
093200090311             select;
093300090311               when  wPos1 =  *zero   and   wPos2 <> *zero;
093400090316                 V1Dmsg = 'Il padre ha altri rami directory (';
093500090311               when  wPos1 <> *zero   and   wPos2 =  *zero;
093600090311                 V1Dmsg = 'Il padre ha una directory unica (';
093700090311               other;
093800090311                 V1Dmsg = 'Primo ramo directory +
093900090311                           <> da quello del cod. padre (';
094000090311             EndSl;
094100121213             if  %len( %trim( p_§LA2dir ) ) <=
094200090311                 %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) ) - 1;
094300090311               V1Dmsg = %trimr( V1Dmsg )
094400121213                      + %trim( p_§LA2dir ) + ')';
094500090311             else;
094600090313               wPos1 = %len( V1Dmsg );
094700090313               wPos2 = %len( %trimr( V1Dmsg ) );
094800090311               V1Dmsg = %trimr( V1Dmsg )
094900121213                      + %trim( %subst( p_§LA2dir : 1 :
095000090311                          %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) )
095100090311                          - 4 ) )
095200090311                      + '...)';
095300090311             endif;
095400090311             leavesr;
095500090311           EndIf;
095600090311
095700090311           // - Controllo 2ª directory
095800090311           wPos1 = %scan( '/' : %trim( V2Cdir ) : wPos1 + 1 );
095900121213           wPos2 = %scan( '/' : %trim( p_§LA2dir ) : wPos2 + 1 );
096000090316           //If  wPos1 <> wPos2                               OR
096100090316           //  ( wPos1  > *zero   and   wPos2 > *zero   and
096200090316           //    %subst( %trim( V2Cdir    ) : 1 : wPos1 ) <>
096300121213           //    %subst( %trim( p_§LA2dir ) : 1 : wPos2 ) )   OR
096400090316           //  ( wPos1  = *zero   and   wPos2  =  *zero
096500121213           //                     and   V2Cdir <> p_§LA2dir );
096600090316           if  wPos1 = *zero;
096700090316             wPos1 = %len( %trimr( V2Cdir ) );
096800090316           else;
096900090316             wPos1 -= 1;
097000090316           endif;
097100090316           if  wPos2 = *zero;
097200121213             wPos2 = %len( %trimr( p_§LA2dir ) );
097300090316           else;
097400090316             wPos2 -= 1;
097500090316           endif;
097600090316           If  wPos1 <> wPos2   OR
097700090316               %subst(V2Cdir : 1 : wPos1) <>
097800121213                 %subst(p_§LA2dir : 1 : wPos2);
097900090311             errMessage  = *on;
098000090311             errGenerico = *on;
098100090311             PosCurDir   = *on;
098200090316             //select;
098300090316             //  when  wPos1 =  *zero   and   wPos2 <> *zero;
098400090316             //    V1Dmsg = 'Il padre ha altri rami directory (';
098500090316             //  when  wPos1 <> *zero   and   wPos2 =  *zero;
098600090316             //    V1Dmsg = 'Il padre ha meno rami directory (';
098700090316             //  other;
098800090313                 V1Dmsg = 'Secondo ramo directory +
098900090311                           <> da quello del cod. padre (';
099000090316             //EndSl;
099100121213             if  %len( %trim( p_§LA2dir ) ) <=
099200090311                 %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) ) - 1;
099300090311               V1Dmsg = %trimr( V1Dmsg )
099400121213                      + %trim( p_§LA2dir ) + ')';
099500090311             else;
099600090311               V1Dmsg = %trimr( V1Dmsg )
099700121213                      + %trim( %subst( p_§LA2dir : 1 :
099800090311                          %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) )
099900090311                          - 4 ) )
100000090311                      + '...)';
100100090311             endif;
100200090311             leavesr;
100300090311           EndIf;
100400090311
100500090306         ENDIF;
100600090327
100700090327         //?Tipo porto?
100800090327         if  V2Ctpt <> 'F'   and   V2Ctpt <> 'A'   and   V2Ctpt <> 'E';
100900090327           errMessage  = *on;
101000090327           errGenerico = *on;
101100090327           PosCurTpt   = *on;
101200090327           V1Dmsg = 'Tipo porto errato o mancante';
101300090327           leavesr;
101400090327         endif;
101500090311
101600090311         //?LNP per selezione?
101700090311         if  V2Clnp <> *zero;
101800090311           setll  (V2Clnp)  AZORG;
101900090311           if  NOT  %equal(AZORG01L);
102000090311             errMessage  = *on;
102100090311             errGenerico = *on;
102200090311             PosCurLnp   = *on;
102300090311             V1Dmsg = 'Linea di partenza errata';
102400090311             leavesr;
102500090311           endif;
102600090311         endif;
102700090226
102800090309         //?Prima estrazione dal?
102900090310         select;
103000090310           // - Obbligatoria se tipo elaborazione CD
103100090310           when  V2Caut = 'S'   and   V2datai = *zero;
103200090310             errMessage  = *on;
103300090310             errGenerico = *on;
103400090310             PosCurDatai = *on;
103500090310             V1Dmsg = 'Periodo iniziale della prima estrazione +
103600090310                       obbligatorio';
103700090310             leavesr;
103800090310           // - Verifica correttezza
103900090310           when  V2datai <> *zero                                  AND
104000090310              (%subst( %editc( V2datai : 'X' ) : 1 : 2 ) < '01'   or
104100090310               %subst( %editc( V2datai : 'X' ) : 1 : 2 ) > '12'   or
104200090310               %subst( %editc( V2datai : 'X' ) : 3 : 4 ) < '2000' or
104300090310               %subst( %editc( V2datai : 'X' ) : 3 : 4 ) > %char(*year));
104400090310             errMessage  = *on;
104500090310             errGenerico = *on;
104600090310             PosCurDatai = *on;
104700090310             V1Dmsg = 'Periodo iniziale della prima estrazione +
104800090310                       errato';
104900090310             leavesr;
105000090310         endsl;
105100090306         // - Se codice padre <> da codice cliente
105200090309         //   il periodo deve essere uguale
105300100521         if  V2Cccm <> V2Cksu;
105400090310           clear  wDataI;
105500090316           %subst( wDataI : 1 : 2 ) =
105600121213                   %subst( %editc( p_§LA2datai : 'X' ) : 5 : 2 );
105700090316           %subst( wDataI : 3 : 4 ) =
105800121213                   %subst( %editc( p_§LA2datai : 'X' ) : 1 : 4 );
105900090310           if  V2datai <> %int(wDataI);
106000090310             errMessage  = *on;
106100090310             errGenerico = *on;
106200090310             PosCurDatai = *on;
106300090310             V1Dmsg = 'Periodo iniziale della prima estrazione +
106400090310                       <> da quello del cod. padre ('
106500121213                    + %subst(%editc(p_§LA2datai : 'X') : 5 : 2)
106600090310                    + '/'
106700121213                    + %subst(%editc(p_§LA2datai : 'X') : 1 : 4)
106800090310                    + ')';
106900090310             leavesr;
107000090310           endif;
107100090226         endif;
107200090306
107300090309         //?Tipo data da elaborare?
107400090310         select;
107500090327           when  V2tdta <> ' '  and  V2tdta <> 'C'  and  V2tdta <> 'S';
107600090327             errMessage  = *on;
107700090327             errGenerico = *on;
107800090327             PosCurTdta  = *on;
107900090327             V1Dmsg = '"Flag "Data da elaborare" errato';
108000090327             leavesr;
108100090310           // - Tipo data da elaborare obbligatorio
108200090310           when  V2Caut = 'S'   and   V2tdta = *blank;
108300090310             errMessage  = *on;
108400090310             errGenerico = *on;
108500090310             PosCurTdta  = *on;
108600090310             V1Dmsg = '"Data da elaborare" obbligatoria per +
108700090310                       "Tipo Elaborazione" = "S" (schedulata)';
108800090310             leavesr;
108900090310           // - Se codice padre <> da codice cliente
109000090310           //   il tipo data da elaborare deve essere uguale
109100121213           when  V2Cccm <> V2Cksu   and   p_§LA2tdta <> V2tdta;
109200090310             errMessage  = *on;
109300090310             errGenerico = *on;
109400090310             PosCurTdta  = *on;
109500090310             V1Dmsg = 'Tipo data da elaborare +
109600090310                       <> da quello del cod. padre ('
109700121213                    + p_§LA2tdta + ')';
109800090310             leavesr;
109900090310         endsl;
110000090309
110100090309         //?Periodo da estrarre in mesi?
110200090310         select;
110300090310           // - Periodo da estrarre in mesi obbligatorio
110400090310           when  V2Caut = 'S'   and   V2mesi = *zero;
110500090310             errMessage  = *on;
110600090310             errGenerico = *on;
110700090310             PosCurMesi  = *on;
110800090310             V1Dmsg = '"Periodo da estrarre" obbligatorio per +
110900090310                       "Tipo Elaborazione" = "S" (schedulata)';
111000090310             leavesr;
111100090310           // - Se codice padre <> da codice cliente
111200090310           //   il periodo da estrarre deve essere uguale
111300121213           when  V2Cccm <> V2Cksu   and   p_§LA2mesi <> V2mesi;
111400090310             errMessage  = *on;
111500090310             errGenerico = *on;
111600090310             PosCurMesi  = *on;
111700090310             V1Dmsg = 'Periodo da estrarre +
111800090310                       <> da quello del cod. padre ('
111900121213                    + %editc(p_§LA2mesi:'X') + ')';
112000090310             leavesr;
112100090310         endsl;
112200090226
112300090309         //?Schedulazione giorno e mesi x preparazione CD?
112400090226         select;
112500090318           // when (V2Caut  =  'A'    or  V2Caut  =  'N')    AND
112600090318           when  V2Caut  =  'A'    AND
112700090311                (V2datai <> *zero  or  V2tdta   <> *blank   or
112800090311                 V2mesi  <> *zero  or  V2Cschgg <> *zero    or
112900090311                 ds_SchMm <> *blank);
113000090311             errMessage  = *on;
113100090311             errGenerico = *on;
113200090311             PosCurAut   = *on;
113300090311             V1Dmsg = 'Schedulazione incompatibile con il +
113400090311                       "Tipo Elaborazione" = ' + V2Caut;
113500090311             leavesr;
113600090318           when  V2Caut = 'S'  and  (V2Cschgg <= *zero);
113700090306             errMessage  = *on;
113800090306             errGenerico = *on;
113900090306             PosCurSchGg = *on;
114000090306             V1Dmsg = 'Schedulazione obbligatoria per +
114100090306                       "Tipo Elaborazione" = "S" (schedulata)';
114200090306             leavesr;
114300090306           when  V2Caut = 'S'  and  (ds_SchMm = *blank);
114400090306             errMessage  = *on;
114500090306             errGenerico = *on;
114600090306             PosCurSch01 = *on;
114700090306             V1Dmsg = 'Indicare almeno un mese per +
114800090306                       "Tipo Elaborazione" = "S" (schedulata)';
114900090306             leavesr;
115000090311         endsl;
115100090226
115200090327         // - Se codice padre <> da codice cliente
115300090306         //   i 2 codici devono avere gli stessi dati di estrazione
115400090306         //   gg e mese e n. mesi da elaborare
115500090309         if  V2Cccm <> V2Cksu;
115600090309           select;
115700121213             when  p_§LA2schgg <> V2Cschgg;
115800090226               errMessage  = *on;
115900090226               errGenerico = *on;
116000090303               PosCurSchgg = *on;
116100090309               V1Dmsg = 'Giorno creazione CD +
116200090309                         <> da quello del cod. padre ('
116300121213                      + %editc(p_§LA2schgg:'X') + ')';
116400090226               leavesr;
116500090303             when  p_SchMm <> ds_SchMm;
116600090226               errMessage  = *on;
116700090226               errGenerico = *on;
116800090226               PosCursch01 = *on;
116900090309               V1Dmsg = 'Mesi di elaborazione +
117000090309                         <> da quelli del cod. padre ('
117100090309                      + p_SchMm + ')';
117200090226               leavesr;
117300090226           endsl;
117400090226         endif;
117500090309
117600090327         // - Verifica congruenza tra mese di schedulazione e
117700090309         //   mesi di estrazione
117800090309         select;
117900090311           // - Nessun dato inserito
118000090310           when  V2mesi   = *zero    and   V2Cschgg = *zero    and
118100090311                 ds_SchMm = *blank;
118200090311           // - Periodo da estrarre in mesi richiesto
118300090310           when  V2mesi = *zero;
118400090310             errMessage  = *on;
118500090310             errGenerico = *on;
118600090310             PosCurMesi  = *on;
118700090310             V1Dmsg = 'Periodo da estrarre obbligatorio SE inserita +
118800090310                       una schedulazione';
118900090310             leavesr;
119000090311           // - Giorno di schedulazione richiesto
119100090310           when  V2Cschgg = *zero;
119200090310             errMessage  = *on;
119300090310             errGenerico = *on;
119400090310             PosCurSchgg = *on;
119500090311             V1Dmsg = $Msg(08);
119600090310             leavesr;
119700090311           // - Selezione mesi richiesta
119800090311           when  ds_SchMm = *blank;
119900090310             errMessage  = *on;
120000090310             errGenerico = *on;
120100090310             PosCurSch01 = *on;
120200090311             V1Dmsg = $Msg(09);
120300090310             leavesr;
120400090311         endsl;
120500090313
120600090313         // - Conteggio/Controllo mesi schedulati
120700090313         clear  wCount;
120800090311         wSchMm_ds = %triml(ds_SchMm);
120900090313         For  xx = 1  To  %elem($SchMm);
121000090313           if  $SchMm(xx) <> *blank;
121100090313             wCount += 1;
121200090313           endif;
121300090313         EndFor;
121400090313
121500090311         select;
121600090313           // - Errori bloccanti:
121700090313           when   wCount <> *zero   and
121800090313                 (V2mesi <> (12 / wCount));
121900090313             errMessage  = *on;
122000090313             errGenerico = *on;
122100090313             PosCurSch01 = *on;
122200090313             V1Dmsg = 'Incongruenza tra il "periodo da estrarre" +
122300090313                       ed i mesi schedulati';
122400090313           // - Errori bypassabili:
122500090313           when ( (V2mesi =  2   and  ($SchMm(03) = *blank     or
122600090311                                       $SchMm(05) = *blank     or
122700090311                                       $SchMm(07) = *blank     or
122800090311                                       $SchMm(09) = *blank     or
122900090311                                       $SchMm(11) = *blank))   or
123000090313                  (V2mesi =  3   and  ($SchMm(04) = *blank     or
123100090311                                       $SchMm(07) = *blank     or
123200090311                                       $SchMm(10) = *blank))   or
123300090313                  (V2mesi =  4   and  ($SchMm(05) = *blank     or
123400090311                                       $SchMm(09) = *blank))   or
123500090313                  (V2mesi =  6   and   $SchMm(07) = *blank) )  AND
123600090313                 $ByPass =  *off;
123700090313             $ByPass = *on;
123800090309             errMessage  = *on;
123900090309             errGenerico = *on;
124000090310             PosCurSch01 = *on;
124100090313             V1Dmsg = 'Mesi schedulati in maniera irregolare... +
124200090313                       Premere Enter per forzare.';
124300090309             leavesr;
124400090313           when ( (V2mesi =  2  and  ($SchMm(02) <> *blank     or
124500090311                                      $SchMm(04) <> *blank     or
124600090311                                      $SchMm(06) <> *blank     or
124700090311                                      $SchMm(08) <> *blank     or
124800090311                                      $SchMm(10) <> *blank     or
124900090311                                      $SchMm(12) <> *blank))   OR
125000090313                  (V2mesi =  3  and  ($SchMm(02) <> *blank     or
125100090311                                      $SchMm(03) <> *blank     or
125200090311                                      $SchMm(05) <> *blank     or
125300090311                                      $SchMm(06) <> *blank     or
125400090311                                      $SchMm(08) <> *blank     or
125500090311                                      $SchMm(09) <> *blank     or
125600090311                                      $SchMm(11) <> *blank     or
125700090311                                      $SchMm(12) <> *blank))   OR
125800090313                  (V2mesi =  4  and  ($SchMm(02) <> *blank     or
125900090311                                      $SchMm(03) <> *blank     or
126000090311                                      $SchMm(04) <> *blank     or
126100090311                                      $SchMm(06) <> *blank     or
126200090311                                      $SchMm(07) <> *blank     or
126300090311                                      $SchMm(08) <> *blank     or
126400090311                                      $SchMm(10) <> *blank     or
126500090311                                      $SchMm(11) <> *blank     or
126600090311                                      $SchMm(12) <> *blank))   OR
126700090313                  (V2mesi =  6  and  ($SchMm(02) <> *blank     or
126800090311                                      $SchMm(03) <> *blank     or
126900090311                                      $SchMm(04) <> *blank     or
127000090311                                      $SchMm(05) <> *blank     or
127100090311                                      $SchMm(06) <> *blank     or
127200090311                                      $SchMm(08) <> *blank     or
127300090311                                      $SchMm(09) <> *blank     or
127400090311                                      $SchMm(10) <> *blank     or
127500090311                                      $SchMm(11) <> *blank     or
127600090311                                      $SchMm(12) <> *blank))   OR
127700090313                  (V2mesi = 12  and  ($SchMm(02) <> *blank     or
127800090311                                      $SchMm(03) <> *blank     or
127900090311                                      $SchMm(04) <> *blank     or
128000090311                                      $SchMm(05) <> *blank     or
128100090311                                      $SchMm(06) <> *blank     or
128200090311                                      $SchMm(07) <> *blank     or
128300090311                                      $SchMm(08) <> *blank     or
128400090311                                      $SchMm(09) <> *blank     or
128500090311                                      $SchMm(10) <> *blank     or
128600090311                                      $SchMm(11) <> *blank     or
128700090313                                      $SchMm(12) <> *blank)) ) AND
128800090313                 $ByPass =  *off;
128900090313             $ByPass = *on;
129000090310             errMessage  = *on;
129100090310             errGenerico = *on;
129200090311             PosCurSch01 = *on;
129300090313             V1Dmsg = 'Mesi schedulati in maniera irregolare... +
129400090313                       Premere Enter per forzare.';
129500090310             leavesr;
129600090313           // - Controllo giorno del mese (bypassabile)
129700090313           when ( (V2Cschgg > 28  and  V2Csch02 <> *blank)  or
129800090313                  (V2Cschgg > 30  and (V2Csch04 <> *blank
129900090313                                   or  V2Csch06 <> *blank
130000090313                                   or  V2Csch09 <> *blank
130100090313                                   or  V2Csch11 <> *blank)) )
130200090313                and  $CtrMm = *off;
130300090313             $CtrMm = *on;
130400090313             errMessage  = *on;
130500090313             errGenerico = *on;
130600090313             PosCurSch01 = (V2Csch01 <> *blank);
130700090313             PosCurSch02 = (V2Csch02 <> *blank);
130800090313             PosCurSch04 = (V2Csch04 <> *blank);
130900090313             PosCurSch06 = (V2Csch06 <> *blank);
131000090313             PosCurSch09 = (V2Csch09 <> *blank);
131100090313             PosCurSch11 = (V2Csch11 <> *blank);
131200090313             V1Dmsg = %replace( %char(V2Cschgg) : $Msg(10) :
131300090313                                %scan('&g':$Msg(10)) );
131400090313             leavesr;
131500090313         endsl;
131600090226
131700090309         //?Frequenza addebito?
131800090310         select;
131900090327           // - Obbligatoria
132000090327           when  V2tadu <> 'I'   and   V2tadu <> 'J'   and
132100090327                 V2tadu <> 'S'   and   V2tadu <> 'M';
132200090327             errMessage  = *on;
132300090327             errGenerico = *on;
132400090327             PosCurTadu  = *on;
132500090327             V1Dmsg = 'Frequenza addebito errata o mancante';
132600090327             leavesr;
132700090327           // - Se tipo elaborazione CD la creazione addebito non può
132800090313           //   essere mensile o settimanale
132900090310           when  V2Caut = 'S'  and  (V2tadu = 'S'  or  V2tadu = 'M');
133000090310             errMessage  = *on;
133100090310             errGenerico = *on;
133200090310             PosCurTadu  = *on;
133300090310             V1Dmsg = 'Per creazione CD non è possibile addebito +
133400090310                       mensile o settimanale';
133500090310             leavesr;
133600090327           // - Se codice padre <> da codice cliente
133700090327           //   devono avere la stessa frequenza addebito
133800121213           when  V2Cccm <> V2Cksu   and   p_§LA2tadu <> V2tadu;
133900090310             errMessage  = *on;
134000090310             errGenerico = *on;
134100090310             PosCurTadu  = *on;
134200090310             V1Dmsg = 'Frequenza addebito +
134300090310                        <> da quella del cod. padre ('
134400121213                    + p_§LA2tadu + ')';
134500090310             leavesr;
134600090310         endsl;
134700090309
134800090309         //?Tipo addebito (Creazione TITAS)?
134900090310         select;
135000090327           // - Obbligatorio
135100090327           when  V2Ctad <> 'N'  and  V2Ctad <> 'S'  and  V2Ctad <> 'V';
135200090327             errMessage  = *on;
135300090327             errGenerico = *on;
135400090327             PosCurTad   = *on;
135500090327             V1Dmsg = 'Flag "Creazione TITAS" errato o mancante';
135600090327             leavesr;
135700090327           // - Se "N" non impostare i dati dell'addebito forzato
135800090310           when  V2Ctad = 'N'     and
135900090310                (V2Cksc <> *zero  or  V2Cctr <> *blank
136000090310                                  or  V2imp <> *zero);
136100090310             errMessage  = *on;
136200090310             errGenerico = *on;
136300090310             PosCurTad   = *on;
136400090310             V1Dmsg = 'Se NO addebito non impostare le forzature';
136500090310             leavesr;
136600090327           // - Se codice padre <> da codice cliente
136700090310           //   devono avere lo stesso tipo addebito
136800121213           when  V2Cccm <> V2Cksu   and   p_§LA2tad <> V2Ctad;
136900090310             errMessage  = *on;
137000090310             errGenerico = *on;
137100090310             PosCurTad   = *on;
137200090310             V1Dmsg = 'Tipo addebito +
137300090310                       <> da quello del cod. padre ('
137400121213                        + p_§LA2tad + ')';
137500090310             leavesr;
137600090310         endsl;
137700090226
137800090311         //?Dati da forzare in tassazione:?
137900090312         //?- Codice cliente / Tariffa?
138000090312         clear  TIBS69ds;
138100090312         if  V2Cksc <> *zero;
138200090312           tibs69ds.I69kcc = DUTkci;
138300090312           tibs69ds.I69kac = %int(V2Cksc);
138400090312           tibs69ds.I69sif = knsif;
138500090312           tibs69r(TIBS69ds : ds_CNACO :
138600090312                              ds_CNIND :
138700090312                              ds_CNCLP :
138800090312                              ds_FNCLS);
138900090312         endif;
139000090312         select;
139100090312           when  tibs69ds.O69err = *on;
139200090312             errMessage  = *on;
139300090312             errGenerico = *on;
139400090312             PosCurKsc   = *on;
139500090312             V1Dmsg = 'Cliente tassazione errato';
139600090312             leavesr;
139700090312           when  V2Cksc <> *zero   and   V2Cctr = *blank;
139800090312             errMessage  = *on;
139900090312             errGenerico = *on;
140000090312             PosCurCtr   = *on;
140100090312             V1Dmsg = 'Codice tariffa obbligatorio SE +
140200090312                       inserito il cliente tassazione';
140300090312             leavesr;
140400090311           // - Non ammessa tariffa senza cliente
140500090311           when  V2Cksc =  *zero   and   V2Cctr <> *blank;
140600090311             errMessage  = *on;
140700090311             errGenerico = *on;
140800090311             PosCurKsc   = *on;
140900090311             V1Dmsg = 'Cliente tassazione obbligatorio SE +
141000090311                       inserita la relativa tariffa';
141100090311             leavesr;
141200090311           // - Ammesso solo un codice tariffa numerico
141300090311           when  V2Cctr <> *blank   and
141400090311                 %check(c_Digits : V2Cctr) > *zero;
141500090311             errMessage  = *on;
141600090311             errGenerico = *on;
141700090312             PosCurCtr   = *on;
141800090312             V1Dmsg = 'Ammessi SOLO caratteri numerici';
141900090311             leavesr;
142000090310         endsl;
142100090226
142200090327         // - Se codice padre <> da codice cliente
142300090306         //   stesso ksc/ctr/importo
142400090303         if  V2Cccm <> V2Cksu;
142500090226           select;
142600121213             when  p_§LA2ksc <> V2Cksc;
142700090226               errMessage  = *on;
142800090226               errGenerico = *on;
142900090303               PosCurKsc   = *on;
143000090309               V1Dmsg = 'Cliente tassazione +
143100090309                         <> da quello del cod. padre ('
143200121213                      + %editc(p_§LA2ksc : 'X') + ')';
143300090226               leavesr;
143400121213             when  p_§LA2ctr <> V2Cctr;
143500090226               errMessage  = *on;
143600090226               errGenerico = *on;
143700090303               PosCurCtr   = *on;
143800090309               V1Dmsg = 'Codice tariffa +
143900090309                         <> quello del cod. padre ('
144000121213                      + p_§LA2ctr + ')';
144100090226               leavesr;
144200090226           endsl;
144300090226         endif;
144400090226
144500090309         //?Importo & Tipo importo forzato?
144600090310         select;
144700090310           // - Non ammesso importo per Varia Negata
144800090310           when  V2Ctad = 'V'   and   V2imp <> *zero;
144900090310             errMessage  = *on;
145000090310             errGenerico = *on;
145100090310             PosCurImp   = *on;
145200090310             V1Dmsg = 'NON ammesso l''importo forzato per +
145300090310                       Varia Negata';
145400090310             leavesr;
145500090310           // - Non ammesso tipo importo senza importo
145600090310           when  V2imp =  *zero   and   V2fimp <> *blank;
145700090310             errMessage  = *on;
145800090310             errGenerico = *on;
145900090310             PosCurImp   = *on;
146000090310             V1Dmsg = 'Importo forzato obbligatorio SE +
146100090310                       inserito il tipo di importo';
146200090310             leavesr;
146300090312           // - Se inserito importo devono essere inseriti:
146400090312           //   · il codice cliente forzato (ksc),
146500090312           //   · la tariffa forzata (ctr) ed
146600090312           //   · il tipo importo (fimp)
146700090310           when  V2imp <> *zero;
146800090310             select;
146900090310               when  V2fimp  = *blank;
147000090310                 errMessage  = *on;
147100090310                 errGenerico = *on;
147200090310                 PosCurFimp  = *on;
147300090310                 V1Dmsg = 'Immettere il tipo importo';
147400090310                 leavesr;
147500090310               when  V2Cksc  = *zero;
147600090310                 errMessage  = *on;
147700090310                 errGenerico = *on;
147800090310                 PosCurKsc   = *on;
147900090310                 V1Dmsg = 'Immettere il cliente forzato';
148000090310                 leavesr;
148100090312               when  V2Cctr  = *blank;
148200090310                 errMessage  = *on;
148300090310                 errGenerico = *on;
148400090310                 PosCurCtr   = *on;
148500090310                 V1Dmsg = 'Immettere la tariffa forzata';
148600090310                 leavesr;
148700090310             endsl;
148800090310         endsl;
148900090312
149000090327         // Verifica compatibilità dell'importo forzato con il
149100090312         //   massimo imponibile IVA in tab. "GEI"/"LAC".
149200090312         if  V2imp <> *zero   and   V2fimp = 'T';
149300090312           // - Reperimento divisa moneta di conto gestionale
149400090312           clear  dGED;
149500090312           reset  TIBS02ds;
149600090312           T02cod = 'GED';
149700090312           T02ke1 = '1';
149800090312           TNTBE_RicercaControllo(kpjba : tibs02ds);
149900090312           if  T02err = *blank;
150000090312             dGED = T02uni;
150100090312           endif;
150200090312           // - Reperimento importi da tab. "GEI"
150300090312           //   (interessa il massimo imponibile IVA per spedizione)
150400090312           clear  dGEI;
150500090312           reset  TIBS02ds;
150600090312           T02cod = 'GEI';
150700090312           T02ke1 = §GEDcn;
150800090312           TNTBE_RicercaControllo(kpjba : tibs02ds);
150900090312           if  T02err = *blank;
151000090312             dGEI = T02uni;
151100090312           endif;
151200090312           if  V2imp > §GEImf;
151300090312             errMessage  = *on;
151400090312             errGenerico = *on;
151500090312             PosCurImp   = *on;
151600090312             V1Dmsg = 'Importo superiore al MASSIMO IMPONIBILE IVA +
151700090312                       PER SPEDIZIONE ('
151800090312                      + %trim( %editc( §GEImf : 'J' ) ) + ')';
151900090312             leavesr;
152000090312           endif;
152100090312         endif;
152200090226
152300090312         // Se codice padre <> da codice cliente
152400090306         //   stesso importo e tipo importo
152500090303         if  V2Cccm <> V2Cksu;
152600090226           select;
152700121213             when  p_§LA2imp <> V2imp;
152800090226               errMessage  = *on;
152900090226               errGenerico = *on;
153000090312               PosCurImp   = *on;
153100090309               V1Dmsg = 'Importo forzato +
153200090309                         <> da quello del cod. padre ('
153300121213                      + %editc(p_§LA2imp : 'J') + ')';
153400090226               leavesr;
153500121213             when  p_§LA2fimp <> V2fimp;
153600090226               errMessage  = *on;
153700090226               errGenerico = *on;
153800090312               PosCurFimp  = *on;
153900090309               V1Dmsg = 'Tipo importo forzato +
154000090309                         <> da quello del cod. padre ('
154100121213                      + p_§LA2fimp + ')';
154200090226               leavesr;
154300090226           endsl;
154400090225         endif;
154500090923
154600090923         //?Modifica dati sensibili per l'addebito
154700090923         if (v2tadu <> savtadu or v2ctad <> savtad or
154800090923             v2cksc <> savksc or v2cctr <> savctr or
154900090923             v2imp <> savimp or v2fimp <> savfimp) and not *in02
155000090923             and not $forza;
155100090923               errMessage  = *on;
155200090923               errGenerico = *on;
155300090923               PosCurTadu  = *on;
155400090923               V1Dmsg = 'ATTENZIONE!! Possibili problemi ' +
155500090923                        'in fase di addebito. Avvisare Monica/Nicola';
155600090923               $forza = *on;
155700090923               leavesr;
155800090923         endif;
155900110713
156000110713         //?Creazione file elenco per il cliente
156100110713         //?possibile solo se NON è elaborazione automatica (FTP)
156200110713         IF  V2pgm <> *blanks and V2caut = 'A';
156300110713           ErrMessage  = *on;
156400110713           ErrGenerico = *on;
156500110713           PosCurPgm   = *on;
156600110713           V1Dmsg = 'NON possibile creazione elenco se elaborazione '+
156700110713                    'automatica';
156800110713           leavesr;
156900110713         ENDIF;
157000110713         //?se inserito il nome del programma deve esserci anche il tipo elenco
157100110713         IF  V2pgm <> *blanks and V2ele = *blanks;
157200110713           ErrMessage  = *on;
157300110713           ErrGenerico = *on;
157400110713           PosCurEle   = *on;
157500110713           V1Dmsg = 'Immettere anche il tipo elenco da creare';
157600110713           leavesr;
157700110713         ENDIF;
157800110713         //?e viceversa se c'è il tipo elenco deve esserci anche il nome del pgm
157900110713         IF  V2pgm = *blanks and V2ele <> *blanks;
158000110713           ErrMessage  = *on;
158100110713           ErrGenerico = *on;
158200110713           PosCurPgm   = *on;
158300110713           V1Dmsg = 'Immettere anche il nome del programma x elenco da creare';
158400110713           leavesr;
158500110713         ENDIF;
158600110713         //?Se codice padre <> da codice cliente
158700110713         //?  devono avere gli stessi dati
158800110713         IF  V2Cccm <> V2Cksu;
158900110713           SELECT;
159000121213             WHEN  p_§LA2pgm <> V2pgm;
159100110713               ErrMessage  = *on;
159200110713               ErrGenerico = *on;
159300110713               PosCurPgm   = *on;
159400110713               V1Dmsg = 'Nome programma +
159500110713                         <> da quello del cod. padre ('
159600121213                      + p_§LA2pgm + ')';
159700110713               leavesr;
159800121213             WHEN  p_§LA2ele <> V2ele;
159900110713               ErrMessage  = *on;
160000110713               ErrGenerico = *on;
160100110713               PosCurEle   = *on;
160200110713               V1Dmsg = 'Tipo elenco +
160300110713                         <> da quello del cod. padre ('
160400121213                      + p_§LA2ele + ')';
160500110713               leavesr;
160600110713           ENDSL;
160700110713         ENDIF;
160800071207
160900071207       ENDSR;
161000090225
161100090225       //--------------------------------------------------------------
161200121213       //?Controllo esistenza del codice padre in tabella LA2          ?
161300090225       //--------------------------------------------------------------
161400090303       BEGSR  sr_Padre;
161500090302
161600121213         // Controlla esistenza in tabella "LA2"
161700121213         clear  dLA2p;
161800090310         reset  TIBS02ds;
161900090310         T02sif = KNSIF;
162000090310         %subst(T02ke1 : 1 : 7) = %editc(V2Cksu : 'X');
162100090310         TNTBE_RicercaControllo(kpjba : tibs02ds);
162200090310         if  T02err = *blank;
162300121213           dLA2p  = T02uni;
162400090310         else;
162500090310           errMessage  = *on;
162600090310           errGenerico = *on;
162700090310           PosCurksu   = *on;
162800090310           V1Dmsg = $Msg(12);
162900090310           leavesr;
163000090310         endif;
163100090327
163200090327         // Controlla se già inserito come figlio di altro padre
163300121213         if  p_§LA2ksu <> V2Cksu;
163400090327           errMessage  = *on;
163500090327           errGenerico = *on;
163600090327           PosCurKsu   = *on;
163700090327           V1Dmsg = 'Non ammesso cod. PADRE già FIGLIO di altro +
163800121213                     cliente (' + %editc(p_§LA2ksu : 'X') + ')';
163900090327           leavesr;
164000090327         endif;
164100090225
164200090225       ENDSR;
164300071206
164400071212       //--------------------------------------------------------------
164500121213       //?Aggiornamento record TNTBE00F (tab. LA2)                     ?
164600071212       //--------------------------------------------------------------
164700090306       BEGSR  Upd_TNTBE;
164800071207
164900121213         clear  dLA2;
165000121213         §LA2rag   = V2Dccm;
165100121213         §LA2tpt   = V2Ctpt;
165200121213         §LA2res   = V2Cres;
165300121213         §LA2rec   = V2Crec;
165400121213         §LA2csr   = V2Ccsr;
165500121213         §LA2ssr   = V2Cssr;
165600121213         §LA2dir   = V2Cdir;
165700121213         §LA2tpi   = V2Ctpi;
165800121213         §LA2fmi   = V2Cfmi;
165900121213         §LA2aut   = V2Caut;
166000121213         §LA2tdta  = V2tdta;
166100121213         §LA2tad   = V2Ctad;
166200121213         §LA2schgg = V2Cschgg;
166300121213         §LA2sch01 = V2Csch01;
166400121213         §LA2sch02 = V2Csch02;
166500121213         §LA2sch03 = V2Csch03;
166600121213         §LA2sch04 = V2Csch04;
166700121213         §LA2sch05 = V2Csch05;
166800121213         §LA2sch06 = V2Csch06;
166900121213         §LA2sch07 = V2Csch07;
167000121213         §LA2sch08 = V2Csch08;
167100121213         §LA2sch09 = V2Csch09;
167200121213         §LA2sch10 = V2Csch10;
167300121213         §LA2sch11 = V2Csch11;
167400121213         §LA2sch12 = V2Csch12;
167500121213         §LA2note  = V2note;
167600121213         §LA2lnp   = V2Clnp;
167700121213         §LA2ksu   = V2Cksu;
167800121213         §LA2mesi  = V2mesi;
167900121213         §LA2datai = (%int( %subst( %editc( V2datai : 'X') : 3 : 4 ) )
168000090313                      * 100)
168100090313                   + %int( %subst( %editc( V2datai : 'X') : 1 : 2 ) );
168200121213         §LA2datela...
168300090313                   = (%int( %subst( %editc( V2datela : 'X') : 3 : 4 ) )
168400090319                      * 100)
168500090313                    + %int( %subst( %editc( V2datela : 'X') : 1 : 2 ) );
168600121213         §LA2tadu  = V2tadu;
168700121213         §LA2ksc   = V2Cksc;
168800121213         §LA2ctr   = V2Cctr;
168900121213         §LA2fimp  = V2fimp;
169000121213         §LA2imp   = V2imp;
169100121213         §LA2pgm   = V2pgm;
169200121213         §LA2ele   = V2ele;
169300071207
169400121213         TBEuni    = dLA2;
169500071207
169600090306         clear  TBEftt;
169700090306         clear  TBEftr;
169800071207
169900071207         select;
170000090306           // F5=Ripristino
170100090306           when  dsp_aid = c_F05;
170200090306             clear  TBEatb;
170300090306           // F16=Annullamento
170400090306           when  dsp_aid = c_F16;
170500071207             TBEatb = 'A';
170600071207         endsl;
170700071207
170800071207         IF  NOT  %found(TNTBE01L);
170900090306           clear  TBEatb;
171000090306           clear  TBEflt;
171100090306           clear  TBEftr;
171200090306           //clear  TBEdtr;
171300090306           TBEsif = TBXsif;
171400090306           TBElin = TBXlin;
171500090306           TBEapl = TBXapl;
171600090306           TBEcod = k_TBEcod;
171700090306           TBEke1 = k_TBEke1;
171800090306           TBEke2 = k_TBEke2;
171900090306           //_____________
172000071207           WRITE TNTBE000;
172100090306           //¯¯¯¯¯¯¯¯¯¯¯¯¯
172200071207         ELSE;
172300090306           //______________
172400071207           UPDATE TNTBE000;
172500090306           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
172600071207         ENDIF;
172700071207
172800071207       ENDSR;
172900071207
173000071212       //--------------------------------------------------------------
173100090306       //?Operazioni finali.                                           ?
173200071212       //--------------------------------------------------------------
173300090306       BEGSR  sr_RoutEnd;
173400071207
173500090306         clear  TIBS69ds;
173600090304         tibs69ds.I69tla = 'C';
173700090304         tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
173800071207
173900121213         kpjbu = TNTB464ds;
174000071207         return;
174100071207
174200071207       ENDSR;
174300090306
174400071207      /end-free
174500071207
174600071212       //--------------------------------------------------------------
174700090306       //?Schiere a tempo di compilazione.                             ?
174800071212       //--------------------------------------------------------------
174900071207
175000071207** - $MSG -------------------------------------------------------------------*
175100071207Immettere il codice cliente                                                     1
175200071207Immettere SOLO caratteri numerici                                               2
175300071207Codice errato                                                                   3
175400071207Cliente annullato                                                               4
175500071207Immettere la directory                                                          5
175600071207Carattere "\" non valido; per indicare una sottocartella utilizzare "/"         6
175700071210Caratteri & e %  non ammessi per la directory di clienti con invio automatico   7
175800071207Indicare il giorno del mese se specificato almeno un mese                       8
175900071207Specificare almeno un mese se indicato il giorno del mese                       9
176000071207Per i mesi con n° di giorni < &g verrà assunto il loro ultimo giorno -> Enter  10
176100090225Inserire il codice padre                                                       11
176200090225Il codice padre deve essere = al codice cliente o = a cliente già in tabella   12
176300121213Cliente da copiare NON più reperibile in tab. "LA2"                            13
176400121213Cliente già inserito in tab. "LA2"                                             14
