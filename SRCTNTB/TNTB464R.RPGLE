000100090306       //--------------------------------------------------------------
000200121213       //?GESTIONE TABELLA "LA2" - Estensione tabella LAC
000300170404       //--------------------------------------------------------------
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
009200151201     d  F7Attivo                       n   overlay(IndDspF:07)
009300090311     d  F16Attivo                      n   overlay(IndDspF:16)
009400090306         // - Emissione messaggio di errore
009500090311     d  ErrMessage                     n   overlay(IndDspF:28)
009600090306         // - Protezione campi
009700090311     d  ProtDATAI                      n   overlay(IndDspF:40)
009800120112     d  InserCCM2                      n   overlay(IndDspF:41)
009900090306         // - Posizionamento cursore & segnalazione errore
010000090311     d  PosCurCcm                      n   overlay(IndDspF:50)
010100090311     d  PosCurAut                      n   overlay(IndDspF:51)
010200090311     d  PosCurTpi                      n   overlay(IndDspF:52)
010300090311     d  PosCurDir                      n   overlay(IndDspF:53)
010400090311     d  PosCurSchGg                    n   overlay(IndDspF:54)
010500090311     d  PosCurSch01                    n   overlay(IndDspF:55)
010600090311     d  PosCurSch02                    n   overlay(IndDspF:56)
010700090311     d  PosCurSch04                    n   overlay(IndDspF:57)
010800090311     d  PosCurSch06                    n   overlay(IndDspF:58)
010900090311     d  PosCurSch09                    n   overlay(IndDspF:59)
011000090311     d  PosCurSch11                    n   overlay(IndDspF:60)
011100090311     d  PosCurKsu                      n   overlay(IndDspF:61)
011200090311     d  PosCurMesi                     n   overlay(IndDspF:62)
011300090311     d  PosCurTdta                     n   overlay(IndDspF:63)
011400090311     d  PosCurTad                      n   overlay(IndDspF:64)
011500090311     d  PosCurTadu                     n   overlay(IndDspF:65)
011600090311     d  PosCurKsc                      n   overlay(IndDspF:66)
011700090311     d  PosCurCtr                      n   overlay(IndDspF:67)
011800090311     d  PosCurImp                      n   overlay(IndDspF:68)
011900090311     d  PosCurFimp                     n   overlay(IndDspF:69)
012000090311     d  PosCurFmi                      n   overlay(IndDspF:70)
012100090311     d  PosCurDatai                    n   overlay(IndDspF:71)
012200090311     d  PosCurLnp                      n   overlay(IndDspF:72)
012300090327     d  PosCurTpt                      n   overlay(IndDspF:73)
012400110713     d  PosCurPgm                      n   overlay(IndDspF:74)
012500110713     d  PosCurEle                      n   overlay(IndDspF:75)
012600120112     d  PosCurCCM2                     n   overlay(IndDspF:76)
012700090306         //   - Riemissione videata
012800090311     d  ErrGenerico                    n   overlay(IndDspF:99)
012900071207
013000090306       // - Parametri ricevuti
013100071206     d KPJBA         e ds
013200121213     d TNTB464ds     e ds
013300071207
013400090306       // - Reperimento dati utente
013500071207     d TIBS34ds      e ds
013600071207
013700090306       // - Controllo/Decodifica cliente
013800090304     d TIBS69ds      e ds                  qualified  inz
013900090304     d ds_CNACO      e ds                  extname(CNACO00F)
014000090304     d                                     qualified  inz
014100090304     d ds_CNIND      e ds                  extname(CNIND00F)
014200090304     d                                     qualified  inz
014300090304     d ds_CNCLP      e ds                  extname(CNCLP00F)
014400090304     d                                     qualified  inz
014500090304     d ds_FNCLS      e ds                  extname(FNCLS00F)
014600090304     d                                     qualified  inz
014700090306
014800090306       // - Dati record principale della tabella
014900090306     d TNTBEds       e ds                  inz  extname(TNTBE00F)
015000090306     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
015100090306     d                                          prefix(TBX:3)
015200090312
015300090312       // - Parametri per Ricerca/controllo tabelle
015400090312     d TIBS02ds      e ds                  inz
015500090312     d  T02mod       e                     inz('C')
015600090312     d  T02cod       e                     inz(c_Tab)
015700071207
015800121213       // - Tabella "LA2"
015900121213     d dLA2          e ds                  inz
016000121213       // - Tabella "LA2" - padre
016100121213     d dLA2p         e ds                  extname(dLA2) inz
016200121213     d                                     prefix(p_)
016300121213     d  p_SchMm               71     82
016400121213       // - Tabella "LAC"
016500121213     d dLAC          e ds                  inz
016600121016
016700090312       // - Tabella "GED" = Divise area gestionale
016800090312     d dGED          e ds                  inz
016900090312       // - Tabella "GEI" = Importi vari area gestionale
017000090312     d dGEI          e ds                  inz
017100090303
017200090306       // - Mesi in cui è schedulata la mail di promemoria creazione CD
017300090303     d ds_SchMm        ds            12    inz
017400090303     d  V2Csch01                           inz
017500090303     d  V2Csch02                           inz
017600090303     d  V2Csch03                           inz
017700090303     d  V2Csch04                           inz
017800090303     d  V2Csch05                           inz
017900090303     d  V2Csch06                           inz
018000090303     d  V2Csch07                           inz
018100090303     d  V2Csch08                           inz
018200090303     d  V2Csch09                           inz
018300090303     d  V2Csch10                           inz
018400090303     d  V2Csch11                           inz
018500090303     d  V2Csch12                           inz
018600090311
018700090311       // - Struttura di comodo x ctrl mesi di schedulazione
018800090311     d wSchMm_ds       ds            12    inz
018900090311     d  $SchMm                 1     12    dim(12)
019000071207
019100090306       //--------------------------------------------------------------
019200090306       //?Definizione procedure usate.                                 ?
019300090306       //--------------------------------------------------------------
019400090306
019500090306       // - Reperimento dati utente
019600090306      /copy gaitrasrc/srcProtoPR,TIBS34R
019700071207
019800090304       // - Ricerca/Controllo tabelle
019900090306      /copy gaitrasrc/srcProtoPR,TIBS02R
020000071207
020100090304       // - Controllo/Decodifica cliente
020200090306      /copy gaitrasrc/srcProtoPR,TIBS69R
020300071207
020400090306       //--------------------------------------------------------------
020500090306       //?Definizione variabili globali.                               ?
020600090306       //--------------------------------------------------------------
020700090306
020800090306       // - Flags booleani
020900090306     d $InzD01         s               n   inz(*on)
021000090306     d $InzD02         s               n   inz(*on)
021100090306     d $Fine           s               n   inz
021200090923     d $forza          s               n   inz
021300090306     d $CtrMm          s               n   inz
021400090313     d $ByPass         s               n   inz
021500120112     d $Copia          s               n   inz
021600151201     d wForzaKsu       s               n   inz
021700071207
021800090306       // - Gestione video
021900071206     d $Video          s              2    inz('D1')
022000090313
022100090313       // - Indici di schiera
022200090313     d xx              s              3  0 inz
022300071207
022400090313       // - Campi di comodo
022500090306     d wDataI          s              6
022600090306     d wDatEla         s              6
022700090306     d wPos1           s              2  0 inz
022800090306     d wPos2           s              2  0 inz
022900090313     d wCount          s              2  0 inz
023000090313     d wSavSch         s                   like(ds_SchMm)  inz
023100090316     d SAVke1          s                   like(TBEke1)    inz
023200121213     d savtad          s                   like(§la2tad)
023300121213     d savtadu         s                   like(§la2tadu)
023400121213     d savksc          s                   like(§la2ksc)
023500121213     d savctr          s                   like(§la2ctr)
023600121213     d savimp          s                   like(§la2imp)
023700121213     d savfimp         s                   like(§la2fimp)
023800121220     d woggi           s              8  0 inz
023900071207
024000090306       //--------------------------------------------------------------
024100090306       //?Definizione key-list.                                        ?
024200090306       //--------------------------------------------------------------
024300090306
024400090306       // - File TNTBE01L
024500090306     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
024600090306     d                                     prefix(k_)  inz
024700071206
024800090306       //--------------------------------------------------------------
024900090306       //?Riepilogo indicatori.                                        ?
025000090306       //--------------------------------------------------------------
025100090306       // 01    - RECORD annullato
025200090306       // 02    - acceso IMMISSIONE - spento MANUTENZIONE
025300090306       // 25    - Comodo
025400090306       // 28    - Emissione del messaggio di errore a video
025500090306       // 40    - Protezione campi in prima estrazione
025600090306       // x D01:
025700090306       // 50    - Codice cliente errato o mancante
025800090306       // x D02:
025900120112       // 41    - Copia in D02: V2CCCM non protetto
026000090306       // 53    - Directory per immagini       errata
026100090306       // 54    - Giorno di schedulazione CD   errato
026200090306       // 55    - Selezione mesi obbligatoria
026300090306       // 56    - Selezione del mese di febbraio
026400090306       // 57    - Selezione del mese di aprile
026500090306       // 58    - Selezione del mese di giugno
026600090306       // 59    - Selezione del mese di settembre
026700090306       // 60    - Selezione del mese di novembre
026800090306       // 61    - Codice Padre                 errato
026900090306       // 62    - N. mesi da estrarre          errato
027000090306       // 63    - Tipo data di elaborazione    errato
027100090306       // 64    - Tipo addebito                errato
027200090306       // 65    - Creazione addebito           errato
027300090306       // 66    - Cliente tassazione           errato
027400090306       // 67    - Codice tariffa               errato
027500090306       // 68    - Importo forzato              errato
027600090306       // 69    - Tipo importo forzato         errato
027700090306       // 70    - Flag nome immagine           errato
027800090306       // 71    - Prima elaborazione           errata
027900090306       // 99    - Generico di errore
028000090306       //--------------------------------------------------------------
028100071206
028200071206     c     *Entry        plist
028300071206     c                   parm                    KPJBA
028400121213     c                   parm                    tntb464ds
028500121213
028600071207      /free
028700071207
028800071207       // Operazioni iniziali
028900090306       exsr  sr_RoutInz;
029000071207
029100071207       // Gestione video
029200090306       DOW  $Fine = *off;
029300071207         select;
029400090306           when  $Video = 'D2';
029500090306             exsr  sr_GesD02;
029600071207           other;
029700071207             leave;
029800071207         endsl;
029900071207       ENDDO;
030000071207
030100071207       // Operazioni finali
030200090306       exsr  sr_RoutEnd;
030300071206
030400071212       //--------------------------------------------------------------
030500090306       //?Operazioni iniziali.                                         ?
030600071212       //--------------------------------------------------------------
030700090306       BEGSR  sr_RoutInz;
030800090306
030900090306         *inLR = *on;
031000121213         B464fxx = *blank;
031100121213         B464err = *off;
031200121213         B464msg = *blank;
031300071207
031400090306         // Reperimento dati job
031500090306         exsr  DatiJob;
031600071207
031700090306         // Impostazione nome programma a video
031800071207         V1Tpgm = SDSpgm;
031900090306
032000090306         // Aggancio dati generali della tabella in esame
032100090306         clear  k_TBEcod;
032200090306         k_TBEke1 = *zero;
032300090306         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
032400090306                = c_Tab;
032500090306         clear  k_TBEke2;
032600090306         clear  k_TBElin;
032700090306         k_TBEsif = KNSIF;
032800090306         chain(n)  %kds(k05tntbe01)  TNTBE000;
032900090306         if  not  %found(TNTBE01L);
033000090306           clear  k_TBEsif;
033100090306           chain(n)  %kds(k05tntbe01)  TNTBE000;
033200090306         endif;
033300090306         if  %found(TNTBE01L);
033400090306           xTNTBEds = TNTBEds;
033500090306         else;
033600090306           clear  xTNTBEds;
033700090306         endif;
033800071207
033900090306         // Verifica parametri ricecvuti
034000121213         if  B464ke1  <> *blank;
034100121213           exsr  CtrDati;
034200121213           if  ErrMessage;
034300121213             B464err = *on;
034400121213             B464msg = V1Dmsg;
034500071217             $Fine  = *on;
034600071217             leavesr;
034700071217           endif;
034800071207           $Video  = 'D2';
034900071207           $InzD02 = *on;
035000071207         endif;
035100121220
035200121220         // Imposto la data di oggi
035300121220         woggi = %dec(%date());
035400071207
035500071207       ENDSR;
035600071206
035700071212       //--------------------------------------------------------------
035800090306       //?Reperimento Dati del job (Utente/Operativi).                 ?
035900071212       //--------------------------------------------------------------
036000090306       BEGSR  DatiJob;
036100071207
036200071207         in(E) §AzUte;
036300071207         if NOT %error;
036400071207           in(E) §DatiUte;
036500071207         endif;
036600090306         if %error or RSut = *blank;
036700071207           clear TIBS34ds;
036800071207           tibs34r(tibs34ds);
036900071207           in §AzUte;
037000071207           in §DatiUte;
037100071207         endif;
037200071207
037300071207       ENDSR;
037400071206
037500071212       //--------------------------------------------------------------
037600121213       //?Controllo dati passati.
037700071212       //--------------------------------------------------------------
037800121213       BEGSR  CtrDati;
037900071207
038000121213         IF  B464ke1 = *blank  or  B464ke1 = *zero;
038100121213           ErrMessage  = *on;
038200121213           ErrGenerico = *on;
038300121213           B464msg = $Msg(01);
038400121213           leavesr;
038500121213         ENDIF;
038600121213
038700121213         clear  TIBS69ds;
038800121213         tibs69ds.I69kcc = DUTkci;
038900121213         tibs69ds.I69kac = %int(%subst(B464ke1:1:7));
039000121213         tibs69ds.I69sif = knsif;
039100121213         tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
039200071207
039300090306         // - Verifica esistenza codice cliente in tabella
039400090306         reset  TNTBE000;
039500090310         k_TBEcod = c_Tab;
039600121213         k_TBEke1 = B464ke1;
039700090306         clear  k_TBEke2;
039800090306         k_TBElin = TBXlin;
039900090306         k_TBEsif = TBXsif;
040000120112
040100121213         chain  %kds(K05tntbe01) TNTBE000;
040200121213         // - - NON ammesso nuovo record per cliente annullato
040300121213         if  NOT  %found(TNTBE01L)  and  ds_CNACO.ACOflg <> *blank;
040400121213           ErrMessage  = *on;
040500121213           ErrGenerico = *on;
040600121213           B464msg = $Msg(04);
040700121213           leavesr;
040800121213         ENDIF;
040900071207
041000120112         // - Decodifica cliente
041100121213         IF  %found(TNTBE01L);
041200121213           dLA2 = TBEuni;
041300130115         //ELSE;
041400130115         //  §LA2rag = %subst(ds_CNACO.ACOrag : 1 : %len(V2Dccm));
041500121213         ENDIF;
041600130115
041700130115         §LA2rag = %subst(ds_CNACO.ACOrag : 1 : %len(V2Dccm));
041800071207
041900071207       ENDSR;
042000071206
042100071212       //--------------------------------------------------------------
042200090306       //?Gestione videata D02                                         ?
042300071212       //--------------------------------------------------------------
042400090306       BEGSR  sr_GesD02;
042500071207
042600090306         // Inizializzazione videata
042700090306         if   $InzD02 = *on;
042800090306           exsr  sr_InzD02;
042900090306           $InzD02 = *off;
043000071207         endif;
043100071207
043200090306         // Emissione testata
043300121213         write  TN464T01;
043400071207
043500090306         // Emissione videata
043600141205         if  B464opz <> '5';
043700121213           exfmt  TN464D02;
043800071217         else;
043900121213           write  TN464D02;
044000090306           exfmt  PROTECT;
044100071217         endif;
044200071207         errMessage  = *off;
044300071207         errGenerico = *off;
044400090306         clear  V1Dmsg;
044500071207
044600071207         SELECT;
044700090306           // F12=Ritorno
044800090306           WHEN  dsp_aid = c_F12;
044900090306             exsr  sr_F12D02;
045000090306           // Enter o F5 o F6 o F16
045100071207           OTHER;
045200090306             // - Controllo dati
045300090306             if  dsp_aid = c_F16;
045400090306               exsr  sr_CtrAnnull;
045500090306             else;
045600090306               exsr  sr_CtrD02;
045700090306             endif;
045800090306             if  errGenerico;
045900071207               leavesr;
046000071207             endif;
046100121212
046200090306             // - Conferma dati:
046300151201             //   F7=Ripristino; F6=Conferma; F16=Annullamento
046400151201             if  dsp_aid = c_F06  or
046500151201                 dsp_aid = c_F07  or
046600090306                 dsp_aid = c_F16;
046700090306               exsr  Upd_TNTBE;
046800121212
046900071207               if  NOT  errGenerico;
047000121213                 $Fine  = *on;
047100071207               endif;
047200071207             EndIf;
047300071207
047400071207         ENDSL;
047500071207
047600071207       ENDSR;
047700071206
047800071212       //--------------------------------------------------------------
047900090306       //?Inizializzazione videata D01                                 ?
048000071212       //--------------------------------------------------------------
048100090306       BEGSR  sr_InzD02;
048200071207
048300090306         reset  $CtrMm;
048400090313         reset  $ByPass;
048500151201         reset  wForzaKsu;
048600101103
048700121213         clear  TN464D02;
048800071210
048900090306         IndDspF = *zero;
049000090309
049100090309         // Impostazione indicatori per abilitazione tasti funzionali
049200090309         exsr  Set_Ind_D02;
049300090309
049400120112         // - Codice estrazione (chiave)
049500121213         V2Cccm = %int(%subst(B464ke1:1:7));
049600121213         V2Dccm = §LA2rag;
049700121217
049800121217         // Recupero sempre i dati della tabella LAC
049900121217           reset  TIBS02ds;
050000121217           T02cod = 'LAC';
050100121217           T02ke1 = B464ke1;
050200121217           TNTBE_RicercaControllo(kpjba : tibs02ds);
050300121217           IF  T02err = *blank;
050400121217             dLAC = T02uni;
050500121217           ENDIF;
050600121213
050700121213         // Immissione nuovo rcd recupero dati da tabella LAC
050800170404         IF  not %found(TNTBE01L)
050900170404         // se però non sono entrato in visualizzazione
051000170404          and B464opz <> '5';
051100121213           V1Tlav = 'IMMISSIONE';
051200121217           dLA2 = T02uni;
051300121213           clear §LA2dir;
051400121213           *in02 = *on;
051500121213         ENDIF;
051600071207
051700121213         // Rcd già presente
051800121213         IF  %found(TNTBE01L);
051900090306           // Impostazione dati di record trovato
052000071217           select;
052100121213             when  B464opz = '5';
052200071217               V1Tlav = 'VISUALIZZA';
052300090306             when  TBEatb = *blank;
052400071217               V1Tlav = 'VARIAZIONE';
052500071217             other;
052600071217               V1Tlav = 'ANNULLATO ';
052700071217           endsl;
052800121213           *in02 = *off;
052900121213         ENDIF;
053000090225
053100090306           // Codice padre
053200121213           V2Cksu = §LA2ksu;
053300090309           clear  TIBS69ds;
053400090309           clear  ds_CNACO;
053500090309           clear  ds_CNIND;
053600090309           clear  ds_CNCLP;
053700090309           clear  ds_FNCLS;
053800090310           select;
053900121213             when  §LA2ksu =  V2Cccm;
054000090310               V2Dksu = V2Dccm;
054100121213             when  §LA2ksu <> *zero;
054200090310               tibs69ds.I69kcc = DUTkci;
054300090310               tibs69ds.I69kac = %int(V2Cksu);
054400090310               tibs69ds.I69sif = knsif;
054500090310               tibs69r(TIBS69ds:ds_CNACO:ds_CNIND:ds_CNCLP:ds_FNCLS);
054600090310               if tibs69ds.O69err = *on;
054700090310                 V1Dmsg = $Msg(03);
054800090310                 errMessage  = *on;
054900090310                 errGenerico = *on;
055000090310                 PosCurKsu   = *on;
055100090310                 leavesr;
055200090310               endif;
055300090310               V2Dksu = ds_CNACO.ACOrag;
055400090310           endsl;
055500090225
055600170404         //if B464opz <> '5';
055700121213           V2Caut   = §LA2aut;
055800121213           V2Ctpi   = §LA2tpi;
055900090225
056000090306           // Flag nome immagine
056100121213           V2Cfmi = §LA2fmi;
056200090310           reset  TIBS02ds;
056300090303           T02cod = 'NIM';
056400121213           T02ke1 = §LA2fmi;
056500090304           TNTBE_RicercaControllo(kpjba : tibs02ds);
056600090306           if  T02err = *blank;
056700090303             V2Dfmi = T02uni;
056800090303           endif;
056900090225
057000121213           V2Cdir   = §LA2dir;
057100121213           V2Ctpt   = §LA2tpt;
057200121213           V2Cssr   = §LA2ssr;
057300121213           V2Clnp   = §LA2lnp;
057400121213           V2Cres   = §LA2res;
057500121213           V2Crec   = §LA2rec;
057600121213           V2Ccsr   = §LA2csr;
057700090309
057800090309           // Prima estrazione
057900090309           clear  wDataI;
058000121213           if  §LA2datai > *zero;
058100121213             %subst(wDataI:1:2) = %subst(%editc(§LA2datai:'X'):5:2);
058200121213             %subst(wDataI:3:4) = %subst(%editc(§LA2datai:'X'):1:4);
058300090310             V2datai = %int(wDataI);
058400090309           endif;
058500090309
058600090309           // Ultima estrazione
058700090309           clear  wDatEla;
058800121213           if  §LA2datela > *zero;
058900121213             %subst(wDatEla:1:2) = %subst(%editc(§LA2datela:'X'):5:2);
059000121213             %subst(wDatEla:3:4) = %subst(%editc(§LA2datela:'X'):1:4);
059100090310             V2datela = %int(wDatEla);
059200090309           endif;
059300090309
059400121213           V2tdta   = §LA2tdta;
059500121213           V2mesi   = §LA2mesi;
059600121213           V2Cschgg = §LA2schgg;
059700121213           V2Csch01 = §LA2sch01;
059800121213           V2Csch02 = §LA2sch02;
059900121213           V2Csch03 = §LA2sch03;
060000121213           V2Csch04 = §LA2sch04;
060100121213           V2Csch05 = §LA2sch05;
060200121213           V2Csch06 = §LA2sch06;
060300121213           V2Csch07 = §LA2sch07;
060400121213           V2Csch08 = §LA2sch08;
060500121213           V2Csch09 = §LA2sch09;
060600121213           V2Csch10 = §LA2sch10;
060700121213           V2Csch11 = §LA2sch11;
060800121213           V2Csch12 = §LA2sch12;
060900090225
061000121213           V2tadu   = §LA2tadu;
061100121213           V2Ctad   = §LA2tad;
061200121213           V2cksc   = §LA2ksc;
061300121213           V2cctr   = §LA2ctr;
061400121213           V2imp    = §LA2imp;
061500121213           V2fimp   = §LA2fimp;
061600121213           V2pgm    = §LA2pgm;
061700121213           V2ele    = §LA2ele;
061800121213           V2note   = §LA2note;
061900170404         //endif;
062000090923
062100090923         // Salvo i dati sensibili per la creazione addebito
062200090923         savtad = v2ctad;
062300090923         savtadu = v2tadu;
062400090923         savksc = v2cksc;
062500090923         savctr = v2cctr;
062600090923         savimp = v2imp;
062700090923         savfimp = v2fimp;
062800090923         $forza = *off;
062900071207
063000071207       ENDSR;
063100071217
063200071217       //--------------------------------------------------------------
063300090306       //?Settaggio indicatori nella videata D02 per abilitazione      ?
063400090306       //?  tasti funzionali                                           ?
063500071217       //--------------------------------------------------------------
063600090306       BEGSR  Set_Ind_D02;
063700090319
063800151201         F7Attivo  = ((%found(TNTBE01L) and TBEatb <> *blank)
063900121213                                        and B464opz <> '5');
064000151201         F6Attivo  = (NOT F7Attivo and B464opz <> '5');
064100090306         F16Attivo = ((%found(TNTBE01L) and TBEatb = *blank)
064200121213                                        and B464opz <> '5');
064300090306
064400120112         // - Se ultima estrazione impostata proteggo la prima estrazione
064500121213         ProtDATAI = (§LA2datela <> *zero);
064600071217
064700071217       ENDSR;
064800090319
064900071217
065000071217       //--------------------------------------------------------------
065100090306       //?Gestione tasto funzionale F12 da videata D02                 ?
065200090306       //?F12=Ritorno                                                  ?
065300071217       //--------------------------------------------------------------
065400090306       BEGSR  sr_F12D02;
065500071217
065600121213           B464fxx = 'F12';
065700071217           $Fine = *on;
065800071217
065900071217       ENDSR;
066000090306
066100090306       //--------------------------------------------------------------
066200090306       //?Controllo "annullabilità" del record (padre senza figli)     ?
066300090306       //--------------------------------------------------------------
066400090306       BEGSR  sr_CtrAnnull;
066500090306
066600090306         IndDspF = *zero;
066700090306         // REimpostazione indicatori per abilitazione tasti funzionali
066800090306         exsr  Set_Ind_D02;
066900090306
067000090306         if  V2Cksu <= *zero;
067100121213           V2Cksu = §LA2ksu;
067200090306         endif;
067300090306
067400090306         // Se figlio (non padre): fine controlli
067500090306         if  V2Cccm <> V2Cksu;
067600090306           leavesr;
067700090306         endif;
067800090306
067900090306         //?Verifica se padre con figli?
068000121213         clear  dLA2p;
068100090316         SAVke1 = TBEke1;
068200090306         setll     %kds(k05tntbe01 : 1)  TNTBE000;
068300090306         reade(n)  %kds(k05tntbe01 : 1)  TNTBE000;
068400090306
068500090306         dow  not  %eof(TNTBE01L);
068600090316           if  TBEatb = *blank   and
068700090316               %editc(V2Cccm : 'X') <> %trimr(TBEke1);
068800121213             dLA2p = TBEuni;
068900121213             if  p_§LA2ksu = V2Cccm;
069000090306               errMessage  = *on;
069100090306               errGenerico = *on;
069200090306               PosCurKsu   = *on;
069300090306               V1Dmsg = 'Record NON cancellabile: trattasi di PADRE +
069400090306                         con figli (vedi ' + %trimr(TBEke1) + ')';
069500090306               leavesr;
069600090306             endif;
069700090306           endif;
069800090306         reade(n)  %kds(k05tntbe01 : 1)  TNTBE000;
069900090306         enddo;
070000090316
070100120112         // - Riaggancio del rec. in manutenzione/copia
070200090316         TBEke1 = SAVke1;
070300090316         reset  TNTBE000;
070400090316         k_TBEcod = c_Tab;
070500120112         k_TBEke1 = %editc( V2Cccm : 'X' );
070600090316         clear  k_TBEke2;
070700090316         k_TBElin = TBXlin;
070800090316         k_TBEsif = TBXsif;
070900090316         chain  %kds(K05tntbe01) TNTBE000;
071000090306
071100090306       ENDSR;
071200071206
071300071212       //--------------------------------------------------------------
071400090306       //?Controllo videata D02                                        ?
071500071212       //--------------------------------------------------------------
071600090306       BEGSR  sr_CtrD02;
071700071207
071800090309         %subst(IndDspF : 50) = *off;
071900090313
072000090313         // Verifica se variata la schedulazione dell'estrazione
072100090313         if  $ByPass;
072200090313           $ByPass = (ds_SchMm = wSavSch);
072300090313         endif;
072400090313         wSavSch = ds_SchMm;
072500090225
072600090306         //?Codice padre?
072700090225         select;
072800090306           when  V2Cksu  = *zero;
072900090306             errMessage  = *on;
073000090225             errGenerico = *on;
073100090303             PosCurKsu   = *on;
073200090225             V1Dmsg = $Msg(11);
073300090225             leavesr;
073400090306           when  V2Cksu = V2Cccm;
073500090303             V2Dksu = V2Dccm;
073600090225           other;
073700090327             // - Controllo esistenza codice padre come cliente
073800121213             //   in tabella LA2
073900090303             exsr  sr_Padre;
074000090303             if  errGenerico = *on;
074100090226               leavesr;
074200090225             endif;
074300151201             // se il rcd di tabella è stato trovato
074400151201             If wForzaKsu <> *on;
074500151201               V2Dksu = p_§LA2rag;
074600151201             endif;
074700090226         endsl;
074800101103         //?SE codice padre <> da codice cliente?
074900101103         // ?& sono in immissione?
075000151201         IF  V2Cccm <> V2Cksu   and   *in02
075100151201           // e se il rcd di tabella è stato trovato
075200151201             and wForzaKsu <> *on;
075300101103           // ?& il padre ha già fatto estrazioni?
075400151201           If  p_§LA2datela <> *zero;
075500101103             // ?=> riporto le date del padre, proteggo il campo e non controllo?
075600101103             clear wDataI;
075700121213             %subst(wDataI:1:2) = %subst(%editc(p_§LA2datai:'X'):5:2);
075800121213             %subst(wDataI:3:4) = %subst(%editc(p_§LA2datai:'X'):1:4);
075900101103             V2datai = %int(wDataI);
076000101103             clear wDatEla;
076100121213             %subst(wDatEla:1:2) = %subst(%editc(p_§LA2datela:'X'):5:2);
076200121213             %subst(wDatEla:3:4) = %subst(%editc(p_§LA2datela:'X'):1:4);
076300101103             V2datela = %int(wDatEla);
076400101103             ProtDATAI = *on;
076500101103           EndIf;
076600100521         ENDIF;
076700090311
076800090311         //?Tipo elaborazione?
076900090327           // - Obbligatorio
077000121213           IF    V2Caut <> 'A'  and  V2Caut <> 'S'  and  V2Caut <> 'N';
077100090327             errMessage  = *on;
077200090327             errGenerico = *on;
077300090327             PosCurAut   = *on;
077400090327             V1Dmsg = 'Tipo elaborazione errato o mancante';
077500090327             leavesr;
077600121213         endif;
077700090327
077800090327         //?Tipo immagine?
077900120918         if  V2Ctpi <> 'TIF'   and   V2Ctpi <> 'JPG' and
078000120918             V2Ctpi <> 'PDF';
078100090327           errMessage  = *on;
078200090327           errGenerico = *on;
078300090327           PosCurTpi   = *on;
078400090327           V1Dmsg = 'Tipo immagine errato o mancante';
078500090327           leavesr;
078600090327         endif;
078700090226
078800090306         //?Flag nome immagine?
078900090310         clear  V2Dfmi;
079000090310         select;
079100090310           // - Obbligatorietà
079200090310           when  V2Cfmi = *blank;
079300090310             errMessage  = *on;
079400090310             errGenerico = *on;
079500090310             PosCurFmi   = *on;
079600090310             V1Dmsg = 'Flag nome immagine obbligatorio';
079700090310             leavesr;
079800090310           // - Ricerca
079900090310           when  %scan('?' : V2Cfmi) > *zero;
080000090310             clear  TIBS02ds;
080100090310             T02mod = 'R';
080200090310             T02cod = 'NIM';
080300090310             T02sif = KNSIF;
080400090310             TNTBE_RicercaControllo(kpjba : tibs02ds);
080500090310             if  T02err = *blank;
080600090310               V2Cfmi = %subst(T02ke1 : 1 : %len(V2Cfmi));
080700090310               V2Dfmi = T02uni;
080800090310               errGenerico = *on;
080900090310               leavesr;
081000090310             else;
081100090310               errMessage  = *on;
081200090310               errGenerico = *on;
081300090310               PosCurFmi   = *on;
081400090310               V1Dmsg = T02msg;
081500090310               leavesr;
081600090310             endif;
081700090310           // - Controllo
081800090310           other;
081900090310             reset  TIBS02ds;
082000090310             T02cod = 'NIM';
082100090310             T02ke1 = V2Cfmi;
082200090310             TNTBE_RicercaControllo(kpjba : tibs02ds);
082300090310             if  T02err <> *blank;
082400090310               errMessage  = *on;
082500090310               errGenerico = *on;
082600090310               PosCurFmi   = *on;
082700090310               V1Dmsg = 'Flag nome immagine errato';
082800090310               leavesr;
082900090310             endif;
083000090310             V2Dfmi = T02uni;
083100090310         ENDSL;
083200090226
083300090306         //?Directory per immagini?
083400090226         select;
083500090306           when  V2Cdir  = *blank;
083600090226             errMessage  = *on;
083700090226             errGenerico = *on;
083800090226             PosCurDir   = *on;
083900090226             V1Dmsg = $Msg(05);
084000090226             leavesr;
084100121213           when  V2Cdir  = §LACdir;
084200121213             errMessage  = *on;
084300121213             errGenerico = *on;
084400121213             PosCurDir   = *on;
084500121213             V1Dmsg = 'La directory deve essere diversa da tab. LAC';
084600121213             leavesr;
084700090316           when  %subst(V2Cdir : 1 : 1) = *blank;
084800090316             errMessage  = *on;
084900090316             errGenerico = *on;
085000090316             PosCurDir   = *on;
085100090316             V1Dmsg = 'Non ammessi spazi vuoti all''inizio del nome +
085200090316                       della directory';
085300090316             leavesr;
085400090306           when  %scan('\':V2Cdir) > *zero;
085500090226             errMessage  = *on;
085600090226             errGenerico = *on;
085700090226             PosCurDir   = *on;
085800090226             V1Dmsg = $Msg(06);
085900090226             leavesr;
086000090306           when  %scan('%':V2Cdir) > *zero   and
086100090306                           V2Caut  = 'A';
086200090226             errMessage  = *on;
086300090226             errGenerico = *on;
086400090226             PosCurDir   = *on;
086500090226             V1Dmsg = $Msg(07);
086600090226             leavesr;
086700090309           when  %scan('&':V2Cdir) > *zero  and
086800090309                           V2Caut  = 'A';
086900090309             errMessage  = *on;
087000090309             errGenerico = *on;
087100090309             PosCurDir   = *on;
087200090309             V1Dmsg = $Msg(07);
087300090309             leavesr;
087400090226         endsl;
087500090309
087600090316         wPos1 = %scan('/' : V2Cdir);
087700090316         DoW  wPos1 > *zero;
087800090316           if  %subst( V2Cdir : wPos1 : 2 ) = '//';
087900090316             errMessage  = *on;
088000090316             errGenerico = *on;
088100090316             PosCurDir   = *on;
088200090316             V1Dmsg = 'Nome directory NON valido: non ammesso il +
088300090316                       doppio "/"';
088400090316             leavesr;
088500090316           else;
088600090316             wPos1 = %scan('/' : V2Cdir : wPos1 + 1 );
088700090316           endif;
088800090316         EndDo;
088900090316
089000090309         wPos1 = %scan('&' : V2Cdir);
089100090309         IF  wPos1 > *zero;
089200090309           if  %subst( V2Cdir : wPos1 : 4 ) <> '&AAA'   and
089300090309               %subst( V2Cdir : wPos1 : 2 ) <> '&M';
089400090309             errMessage  = *on;
089500090309             errGenerico = *on;
089600090309             PosCurDir   = *on;
089700090309             V1Dmsg = 'Ammessi solo "&AAA" e "&M" (come caratteri +
089800090309                       sostituibili)';
089900090309             leavesr;
090000090309           endif;
090100090309           wPos2 = %scan( '&' : V2Cdir : wPos1 + 1 );
090200090309           If  wPos2 > *zero;
090300090309             if  %subst( V2Cdir : wPos2 : 4 ) <> '&AAA'   and
090400090309                 %subst( V2Cdir : wPos2 : 2 ) <> '&M';
090500090309               errMessage  = *on;
090600090309               errGenerico = *on;
090700090309               PosCurDir   = *on;
090800090309               V1Dmsg = 'Ammessi solo "&AAA" e "&M" (come caratteri +
090900090309                         sostituibili)';
091000090309               leavesr;
091100090309             endif;
091200090309           EndIf;
091300090309         ENDIF;
091400090309
091500090309         wPos1 = %scan('&M' : V2Cdir);
091600090309         IF  wPos1 > *zero;
091700090309           wPos2 = %scan( '&M' : V2Cdir : wPos1 + 1 );
091800090309           If  wPos2 > *zero   and
091900090309               %scan( '&M' : V2Cdir : wPos2 + 1 ) > *zero;
092000090309             errMessage  = *on;
092100090309             errGenerico = *on;
092200090309             PosCurDir   = *on;
092300090309             V1Dmsg = 'Ammessi max 2 "&M"';
092400090309             leavesr;
092500090309           EndIf;
092600090309         ENDIF;
092700090226
092800090327         // - Se codice padre <> da codice cliente
092900090306         //   la directory deve coincidere almeno fino alla 2ª subdir
093000151201         IF  V2Cccm <> V2Cksu   and   p_§LA2dir <> V2Cdir
093100151201           // e se il rcd di tabella è stato trovato
093200151201             and wForzaKsu <> *on;
093300090311
093400090311           // - Controllo 1ª directory
093500090311           wPos1 = %scan( '/' : %trim( V2Cdir ) );
093600121213           wPos2 = %scan( '/' : %trim( p_§LA2dir ) );
093700090313           If  wPos1 <> wPos2                               OR
093800090313             ( wPos1  > *zero   and   wPos2 > *zero   and
093900090311               %subst( %trim( V2Cdir    ) : 1 : wPos1 ) <>
094000121213               %subst( %trim( p_§LA2dir ) : 1 : wPos2 ) )   OR
094100090313             ( wPos1  = *zero   and   wPos2  =  *zero
094200121213                                and   V2Cdir <> p_§LA2dir );
094300090311             errMessage  = *on;
094400090311             errGenerico = *on;
094500090311             PosCurDir   = *on;
094600090311             select;
094700090311               when  wPos1 =  *zero   and   wPos2 <> *zero;
094800090316                 V1Dmsg = 'Il padre ha altri rami directory (';
094900090311               when  wPos1 <> *zero   and   wPos2 =  *zero;
095000090311                 V1Dmsg = 'Il padre ha una directory unica (';
095100090311               other;
095200090311                 V1Dmsg = 'Primo ramo directory +
095300090311                           <> da quello del cod. padre (';
095400090311             EndSl;
095500121213             if  %len( %trim( p_§LA2dir ) ) <=
095600090311                 %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) ) - 1;
095700090311               V1Dmsg = %trimr( V1Dmsg )
095800121213                      + %trim( p_§LA2dir ) + ')';
095900090311             else;
096000090313               wPos1 = %len( V1Dmsg );
096100090313               wPos2 = %len( %trimr( V1Dmsg ) );
096200090311               V1Dmsg = %trimr( V1Dmsg )
096300121213                      + %trim( %subst( p_§LA2dir : 1 :
096400090311                          %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) )
096500090311                          - 4 ) )
096600090311                      + '...)';
096700090311             endif;
096800090311             leavesr;
096900090311           EndIf;
097000090311
097100090311           // - Controllo 2ª directory
097200090311           wPos1 = %scan( '/' : %trim( V2Cdir ) : wPos1 + 1 );
097300121213           wPos2 = %scan( '/' : %trim( p_§LA2dir ) : wPos2 + 1 );
097400090316           //If  wPos1 <> wPos2                               OR
097500090316           //  ( wPos1  > *zero   and   wPos2 > *zero   and
097600090316           //    %subst( %trim( V2Cdir    ) : 1 : wPos1 ) <>
097700121213           //    %subst( %trim( p_§LA2dir ) : 1 : wPos2 ) )   OR
097800090316           //  ( wPos1  = *zero   and   wPos2  =  *zero
097900121213           //                     and   V2Cdir <> p_§LA2dir );
098000090316           if  wPos1 = *zero;
098100090316             wPos1 = %len( %trimr( V2Cdir ) );
098200090316           else;
098300090316             wPos1 -= 1;
098400090316           endif;
098500090316           if  wPos2 = *zero;
098600121213             wPos2 = %len( %trimr( p_§LA2dir ) );
098700090316           else;
098800090316             wPos2 -= 1;
098900090316           endif;
099000090316           If  wPos1 <> wPos2   OR
099100090316               %subst(V2Cdir : 1 : wPos1) <>
099200121213                 %subst(p_§LA2dir : 1 : wPos2);
099300090311             errMessage  = *on;
099400090311             errGenerico = *on;
099500090311             PosCurDir   = *on;
099600090316             //select;
099700090316             //  when  wPos1 =  *zero   and   wPos2 <> *zero;
099800090316             //    V1Dmsg = 'Il padre ha altri rami directory (';
099900090316             //  when  wPos1 <> *zero   and   wPos2 =  *zero;
100000090316             //    V1Dmsg = 'Il padre ha meno rami directory (';
100100090316             //  other;
100200090313                 V1Dmsg = 'Secondo ramo directory +
100300090311                           <> da quello del cod. padre (';
100400090316             //EndSl;
100500121213             if  %len( %trim( p_§LA2dir ) ) <=
100600090311                 %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) ) - 1;
100700090311               V1Dmsg = %trimr( V1Dmsg )
100800121213                      + %trim( p_§LA2dir ) + ')';
100900090311             else;
101000090311               V1Dmsg = %trimr( V1Dmsg )
101100121213                      + %trim( %subst( p_§LA2dir : 1 :
101200090311                          %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) )
101300090311                          - 4 ) )
101400090311                      + '...)';
101500090311             endif;
101600090311             leavesr;
101700090311           EndIf;
101800090311
101900090306         ENDIF;
102000090327
102100090327         //?Tipo porto?
102200090327         if  V2Ctpt <> 'F'   and   V2Ctpt <> 'A'   and   V2Ctpt <> 'E';
102300090327           errMessage  = *on;
102400090327           errGenerico = *on;
102500090327           PosCurTpt   = *on;
102600090327           V1Dmsg = 'Tipo porto errato o mancante';
102700090327           leavesr;
102800090327         endif;
102900090311
103000090311         //?LNP per selezione?
103100090311         if  V2Clnp <> *zero;
103200090311           setll  (V2Clnp)  AZORG;
103300090311           if  NOT  %equal(AZORG01L);
103400090311             errMessage  = *on;
103500090311             errGenerico = *on;
103600090311             PosCurLnp   = *on;
103700090311             V1Dmsg = 'Linea di partenza errata';
103800090311             leavesr;
103900090311           endif;
104000090311         endif;
104100090226
104200090309         //?Prima estrazione dal?
104300090310         select;
104400090310           // - Obbligatoria se tipo elaborazione CD
104500090310           when  V2Caut = 'S'   and   V2datai = *zero;
104600090310             errMessage  = *on;
104700090310             errGenerico = *on;
104800090310             PosCurDatai = *on;
104900090310             V1Dmsg = 'Periodo iniziale della prima estrazione +
105000090310                       obbligatorio';
105100090310             leavesr;
105200090310           // - Verifica correttezza
105300090310           when  V2datai <> *zero                                  AND
105400090310              (%subst( %editc( V2datai : 'X' ) : 1 : 2 ) < '01'   or
105500090310               %subst( %editc( V2datai : 'X' ) : 1 : 2 ) > '12'   or
105600090310               %subst( %editc( V2datai : 'X' ) : 3 : 4 ) < '2000' or
105700090310               %subst( %editc( V2datai : 'X' ) : 3 : 4 ) > %char(*year));
105800090310             errMessage  = *on;
105900090310             errGenerico = *on;
106000090310             PosCurDatai = *on;
106100090310             V1Dmsg = 'Periodo iniziale della prima estrazione +
106200090310                       errato';
106300090310             leavesr;
106400090310         endsl;
106500090306         // - Se codice padre <> da codice cliente
106600090309         //   il periodo deve essere uguale
106700151201         if  V2Cccm <> V2Cksu
106800151201           // e se il rcd di tabella è stato trovato
106900151201             and wForzaKsu <> *on;
107000090310           clear  wDataI;
107100090316           %subst( wDataI : 1 : 2 ) =
107200121213                   %subst( %editc( p_§LA2datai : 'X' ) : 5 : 2 );
107300090316           %subst( wDataI : 3 : 4 ) =
107400121213                   %subst( %editc( p_§LA2datai : 'X' ) : 1 : 4 );
107500090310           if  V2datai <> %int(wDataI);
107600090310             errMessage  = *on;
107700090310             errGenerico = *on;
107800090310             PosCurDatai = *on;
107900090310             V1Dmsg = 'Periodo iniziale della prima estrazione +
108000090310                       <> da quello del cod. padre ('
108100121213                    + %subst(%editc(p_§LA2datai : 'X') : 5 : 2)
108200090310                    + '/'
108300121213                    + %subst(%editc(p_§LA2datai : 'X') : 1 : 4)
108400090310                    + ')';
108500090310             leavesr;
108600090310           endif;
108700090226         endif;
108800090306
108900090309         //?Tipo data da elaborare?
109000090310         select;
109100090327           when  V2tdta <> ' '  and  V2tdta <> 'C'  and  V2tdta <> 'S';
109200090327             errMessage  = *on;
109300090327             errGenerico = *on;
109400090327             PosCurTdta  = *on;
109500090327             V1Dmsg = '"Flag "Data da elaborare" errato';
109600090327             leavesr;
109700090310           // - Tipo data da elaborare obbligatorio
109800090310           when  V2Caut = 'S'   and   V2tdta = *blank;
109900090310             errMessage  = *on;
110000090310             errGenerico = *on;
110100090310             PosCurTdta  = *on;
110200090310             V1Dmsg = '"Data da elaborare" obbligatoria per +
110300090310                       "Tipo Elaborazione" = "S" (schedulata)';
110400090310             leavesr;
110500090310           // - Se codice padre <> da codice cliente
110600090310           //   il tipo data da elaborare deve essere uguale
110700151201           when  V2Cccm <> V2Cksu   and   p_§LA2tdta <> V2tdta
110800151201           // e se il rcd di tabella è stato trovato
110900151201             and wForzaKsu <> *on;
111000090310             errMessage  = *on;
111100090310             errGenerico = *on;
111200090310             PosCurTdta  = *on;
111300090310             V1Dmsg = 'Tipo data da elaborare +
111400090310                       <> da quello del cod. padre ('
111500121213                    + p_§LA2tdta + ')';
111600090310             leavesr;
111700090310         endsl;
111800090309
111900090309         //?Periodo da estrarre in mesi?
112000090310         select;
112100090310           // - Periodo da estrarre in mesi obbligatorio
112200090310           when  V2Caut = 'S'   and   V2mesi = *zero;
112300090310             errMessage  = *on;
112400090310             errGenerico = *on;
112500090310             PosCurMesi  = *on;
112600090310             V1Dmsg = '"Periodo da estrarre" obbligatorio per +
112700090310                       "Tipo Elaborazione" = "S" (schedulata)';
112800090310             leavesr;
112900090310           // - Se codice padre <> da codice cliente
113000090310           //   il periodo da estrarre deve essere uguale
113100151201           when  V2Cccm <> V2Cksu   and   p_§LA2mesi <> V2mesi
113200151201           // e se il rcd di tabella è stato trovato
113300151201             and wForzaKsu <> *on;
113400090310             errMessage  = *on;
113500090310             errGenerico = *on;
113600090310             PosCurMesi  = *on;
113700090310             V1Dmsg = 'Periodo da estrarre +
113800090310                       <> da quello del cod. padre ('
113900121213                    + %editc(p_§LA2mesi:'X') + ')';
114000090310             leavesr;
114100090310         endsl;
114200090226
114300090309         //?Schedulazione giorno e mesi x preparazione CD?
114400090226         select;
114500090318           // when (V2Caut  =  'A'    or  V2Caut  =  'N')    AND
114600090318           when  V2Caut  =  'A'    AND
114700090311                (V2datai <> *zero  or  V2tdta   <> *blank   or
114800090311                 V2mesi  <> *zero  or  V2Cschgg <> *zero    or
114900090311                 ds_SchMm <> *blank);
115000090311             errMessage  = *on;
115100090311             errGenerico = *on;
115200090311             PosCurAut   = *on;
115300090311             V1Dmsg = 'Schedulazione incompatibile con il +
115400090311                       "Tipo Elaborazione" = ' + V2Caut;
115500090311             leavesr;
115600090318           when  V2Caut = 'S'  and  (V2Cschgg <= *zero);
115700090306             errMessage  = *on;
115800090306             errGenerico = *on;
115900090306             PosCurSchGg = *on;
116000090306             V1Dmsg = 'Schedulazione obbligatoria per +
116100090306                       "Tipo Elaborazione" = "S" (schedulata)';
116200090306             leavesr;
116300090306           when  V2Caut = 'S'  and  (ds_SchMm = *blank);
116400090306             errMessage  = *on;
116500090306             errGenerico = *on;
116600090306             PosCurSch01 = *on;
116700090306             V1Dmsg = 'Indicare almeno un mese per +
116800090306                       "Tipo Elaborazione" = "S" (schedulata)';
116900090306             leavesr;
117000090311         endsl;
117100090226
117200090327         // - Se codice padre <> da codice cliente
117300090306         //   i 2 codici devono avere gli stessi dati di estrazione
117400090306         //   gg e mese e n. mesi da elaborare
117500151201         if  V2Cccm <> V2Cksu
117600151201           // e se il rcd di tabella è stato trovato
117700151201             and wForzaKsu <> *on;
117800090309           select;
117900121213             when  p_§LA2schgg <> V2Cschgg;
118000090226               errMessage  = *on;
118100090226               errGenerico = *on;
118200090303               PosCurSchgg = *on;
118300090309               V1Dmsg = 'Giorno creazione CD +
118400090309                         <> da quello del cod. padre ('
118500121213                      + %editc(p_§LA2schgg:'X') + ')';
118600090226               leavesr;
118700090303             when  p_SchMm <> ds_SchMm;
118800090226               errMessage  = *on;
118900090226               errGenerico = *on;
119000090226               PosCursch01 = *on;
119100090309               V1Dmsg = 'Mesi di elaborazione +
119200090309                         <> da quelli del cod. padre ('
119300090309                      + p_SchMm + ')';
119400090226               leavesr;
119500090226           endsl;
119600090226         endif;
119700090309
119800090327         // - Verifica congruenza tra mese di schedulazione e
119900090309         //   mesi di estrazione
120000090309         select;
120100090311           // - Nessun dato inserito
120200090310           when  V2mesi   = *zero    and   V2Cschgg = *zero    and
120300090311                 ds_SchMm = *blank;
120400090311           // - Periodo da estrarre in mesi richiesto
120500090310           when  V2mesi = *zero;
120600090310             errMessage  = *on;
120700090310             errGenerico = *on;
120800090310             PosCurMesi  = *on;
120900090310             V1Dmsg = 'Periodo da estrarre obbligatorio SE inserita +
121000090310                       una schedulazione';
121100090310             leavesr;
121200090311           // - Giorno di schedulazione richiesto
121300090310           when  V2Cschgg = *zero;
121400090310             errMessage  = *on;
121500090310             errGenerico = *on;
121600090310             PosCurSchgg = *on;
121700090311             V1Dmsg = $Msg(08);
121800090310             leavesr;
121900090311           // - Selezione mesi richiesta
122000090311           when  ds_SchMm = *blank;
122100090310             errMessage  = *on;
122200090310             errGenerico = *on;
122300090310             PosCurSch01 = *on;
122400090311             V1Dmsg = $Msg(09);
122500090310             leavesr;
122600090311         endsl;
122700090313
122800090313         // - Conteggio/Controllo mesi schedulati
122900090313         clear  wCount;
123000090311         wSchMm_ds = %triml(ds_SchMm);
123100090313         For  xx = 1  To  %elem($SchMm);
123200090313           if  $SchMm(xx) <> *blank;
123300090313             wCount += 1;
123400090313           endif;
123500090313         EndFor;
123600090313
123700090311         select;
123800090313           // - Errori bloccanti:
123900090313           when   wCount <> *zero   and
124000090313                 (V2mesi <> (12 / wCount));
124100090313             errMessage  = *on;
124200090313             errGenerico = *on;
124300090313             PosCurSch01 = *on;
124400090313             V1Dmsg = 'Incongruenza tra il "periodo da estrarre" +
124500090313                       ed i mesi schedulati';
124600090313           // - Errori bypassabili:
124700090313           when ( (V2mesi =  2   and  ($SchMm(03) = *blank     or
124800090311                                       $SchMm(05) = *blank     or
124900090311                                       $SchMm(07) = *blank     or
125000090311                                       $SchMm(09) = *blank     or
125100090311                                       $SchMm(11) = *blank))   or
125200090313                  (V2mesi =  3   and  ($SchMm(04) = *blank     or
125300090311                                       $SchMm(07) = *blank     or
125400090311                                       $SchMm(10) = *blank))   or
125500090313                  (V2mesi =  4   and  ($SchMm(05) = *blank     or
125600090311                                       $SchMm(09) = *blank))   or
125700090313                  (V2mesi =  6   and   $SchMm(07) = *blank) )  AND
125800090313                 $ByPass =  *off;
125900090313             $ByPass = *on;
126000090309             errMessage  = *on;
126100090309             errGenerico = *on;
126200090310             PosCurSch01 = *on;
126300090313             V1Dmsg = 'Mesi schedulati in maniera irregolare... +
126400090313                       Premere Enter per forzare.';
126500090309             leavesr;
126600090313           when ( (V2mesi =  2  and  ($SchMm(02) <> *blank     or
126700090311                                      $SchMm(04) <> *blank     or
126800090311                                      $SchMm(06) <> *blank     or
126900090311                                      $SchMm(08) <> *blank     or
127000090311                                      $SchMm(10) <> *blank     or
127100090311                                      $SchMm(12) <> *blank))   OR
127200090313                  (V2mesi =  3  and  ($SchMm(02) <> *blank     or
127300090311                                      $SchMm(03) <> *blank     or
127400090311                                      $SchMm(05) <> *blank     or
127500090311                                      $SchMm(06) <> *blank     or
127600090311                                      $SchMm(08) <> *blank     or
127700090311                                      $SchMm(09) <> *blank     or
127800090311                                      $SchMm(11) <> *blank     or
127900090311                                      $SchMm(12) <> *blank))   OR
128000090313                  (V2mesi =  4  and  ($SchMm(02) <> *blank     or
128100090311                                      $SchMm(03) <> *blank     or
128200090311                                      $SchMm(04) <> *blank     or
128300090311                                      $SchMm(06) <> *blank     or
128400090311                                      $SchMm(07) <> *blank     or
128500090311                                      $SchMm(08) <> *blank     or
128600090311                                      $SchMm(10) <> *blank     or
128700090311                                      $SchMm(11) <> *blank     or
128800090311                                      $SchMm(12) <> *blank))   OR
128900090313                  (V2mesi =  6  and  ($SchMm(02) <> *blank     or
129000090311                                      $SchMm(03) <> *blank     or
129100090311                                      $SchMm(04) <> *blank     or
129200090311                                      $SchMm(05) <> *blank     or
129300090311                                      $SchMm(06) <> *blank     or
129400090311                                      $SchMm(08) <> *blank     or
129500090311                                      $SchMm(09) <> *blank     or
129600090311                                      $SchMm(10) <> *blank     or
129700090311                                      $SchMm(11) <> *blank     or
129800090311                                      $SchMm(12) <> *blank))   OR
129900090313                  (V2mesi = 12  and  ($SchMm(02) <> *blank     or
130000090311                                      $SchMm(03) <> *blank     or
130100090311                                      $SchMm(04) <> *blank     or
130200090311                                      $SchMm(05) <> *blank     or
130300090311                                      $SchMm(06) <> *blank     or
130400090311                                      $SchMm(07) <> *blank     or
130500090311                                      $SchMm(08) <> *blank     or
130600090311                                      $SchMm(09) <> *blank     or
130700090311                                      $SchMm(10) <> *blank     or
130800090311                                      $SchMm(11) <> *blank     or
130900090313                                      $SchMm(12) <> *blank)) ) AND
131000090313                 $ByPass =  *off;
131100090313             $ByPass = *on;
131200090310             errMessage  = *on;
131300090310             errGenerico = *on;
131400090311             PosCurSch01 = *on;
131500090313             V1Dmsg = 'Mesi schedulati in maniera irregolare... +
131600090313                       Premere Enter per forzare.';
131700090310             leavesr;
131800090313           // - Controllo giorno del mese (bypassabile)
131900090313           when ( (V2Cschgg > 28  and  V2Csch02 <> *blank)  or
132000090313                  (V2Cschgg > 30  and (V2Csch04 <> *blank
132100090313                                   or  V2Csch06 <> *blank
132200090313                                   or  V2Csch09 <> *blank
132300090313                                   or  V2Csch11 <> *blank)) )
132400090313                and  $CtrMm = *off;
132500090313             $CtrMm = *on;
132600090313             errMessage  = *on;
132700090313             errGenerico = *on;
132800090313             PosCurSch01 = (V2Csch01 <> *blank);
132900090313             PosCurSch02 = (V2Csch02 <> *blank);
133000090313             PosCurSch04 = (V2Csch04 <> *blank);
133100090313             PosCurSch06 = (V2Csch06 <> *blank);
133200090313             PosCurSch09 = (V2Csch09 <> *blank);
133300090313             PosCurSch11 = (V2Csch11 <> *blank);
133400090313             V1Dmsg = %replace( %char(V2Cschgg) : $Msg(10) :
133500090313                                %scan('&g':$Msg(10)) );
133600090313             leavesr;
133700090313         endsl;
133800090226
133900090309         //?Frequenza addebito?
134000090310         select;
134100090327           // - Obbligatoria
134200090327           when  V2tadu <> 'I'   and   V2tadu <> 'J'   and
134300090327                 V2tadu <> 'S'   and   V2tadu <> 'M';
134400090327             errMessage  = *on;
134500090327             errGenerico = *on;
134600090327             PosCurTadu  = *on;
134700090327             V1Dmsg = 'Frequenza addebito errata o mancante';
134800090327             leavesr;
134900090327           // - Se tipo elaborazione CD la creazione addebito non può
135000090313           //   essere mensile o settimanale
135100090310           when  V2Caut = 'S'  and  (V2tadu = 'S'  or  V2tadu = 'M');
135200090310             errMessage  = *on;
135300090310             errGenerico = *on;
135400090310             PosCurTadu  = *on;
135500090310             V1Dmsg = 'Per creazione CD non è possibile addebito +
135600090310                       mensile o settimanale';
135700090310             leavesr;
135800090327           // - Se codice padre <> da codice cliente
135900090327           //   devono avere la stessa frequenza addebito
136000151201           when  V2Cccm <> V2Cksu   and   p_§LA2tadu <> V2tadu
136100151201           // e se il rcd di tabella è stato trovato
136200151201             and wForzaKsu <> *on;
136300090310             errMessage  = *on;
136400090310             errGenerico = *on;
136500090310             PosCurTadu  = *on;
136600090310             V1Dmsg = 'Frequenza addebito +
136700090310                        <> da quella del cod. padre ('
136800121213                    + p_§LA2tadu + ')';
136900090310             leavesr;
137000090310         endsl;
137100090309
137200090309         //?Tipo addebito (Creazione TITAS)?
137300090310         select;
137400090327           // - Obbligatorio
137500090327           when  V2Ctad <> 'N'  and  V2Ctad <> 'S'  and  V2Ctad <> 'V';
137600090327             errMessage  = *on;
137700090327             errGenerico = *on;
137800090327             PosCurTad   = *on;
137900090327             V1Dmsg = 'Flag "Creazione TITAS" errato o mancante';
138000090327             leavesr;
138100090327           // - Se "N" non impostare i dati dell'addebito forzato
138200090310           when  V2Ctad = 'N'     and
138300090310                (V2Cksc <> *zero  or  V2Cctr <> *blank
138400090310                                  or  V2imp <> *zero);
138500090310             errMessage  = *on;
138600090310             errGenerico = *on;
138700090310             PosCurTad   = *on;
138800090310             V1Dmsg = 'Se NO addebito non impostare le forzature';
138900090310             leavesr;
139000090327           // - Se codice padre <> da codice cliente
139100090310           //   devono avere lo stesso tipo addebito
139200151201           when  V2Cccm <> V2Cksu   and   p_§LA2tad <> V2Ctad
139300151201           // e se il rcd di tabella è stato trovato
139400151201             and wForzaKsu <> *on;
139500090310             errMessage  = *on;
139600090310             errGenerico = *on;
139700090310             PosCurTad   = *on;
139800090310             V1Dmsg = 'Tipo addebito +
139900090310                       <> da quello del cod. padre ('
140000121213                        + p_§LA2tad + ')';
140100090310             leavesr;
140200090310         endsl;
140300090226
140400090311         //?Dati da forzare in tassazione:?
140500090312         //?- Codice cliente / Tariffa?
140600090312         clear  TIBS69ds;
140700090312         if  V2Cksc <> *zero;
140800090312           tibs69ds.I69kcc = DUTkci;
140900090312           tibs69ds.I69kac = %int(V2Cksc);
141000090312           tibs69ds.I69sif = knsif;
141100090312           tibs69r(TIBS69ds : ds_CNACO :
141200090312                              ds_CNIND :
141300090312                              ds_CNCLP :
141400090312                              ds_FNCLS);
141500090312         endif;
141600090312         select;
141700090312           when  tibs69ds.O69err = *on;
141800090312             errMessage  = *on;
141900090312             errGenerico = *on;
142000090312             PosCurKsc   = *on;
142100090312             V1Dmsg = 'Cliente tassazione errato';
142200090312             leavesr;
142300090312           when  V2Cksc <> *zero   and   V2Cctr = *blank;
142400090312             errMessage  = *on;
142500090312             errGenerico = *on;
142600090312             PosCurCtr   = *on;
142700090312             V1Dmsg = 'Codice tariffa obbligatorio SE +
142800090312                       inserito il cliente tassazione';
142900090312             leavesr;
143000090311           // - Non ammessa tariffa senza cliente
143100090311           when  V2Cksc =  *zero   and   V2Cctr <> *blank;
143200090311             errMessage  = *on;
143300090311             errGenerico = *on;
143400090311             PosCurKsc   = *on;
143500090311             V1Dmsg = 'Cliente tassazione obbligatorio SE +
143600090311                       inserita la relativa tariffa';
143700090311             leavesr;
143800090311           // - Ammesso solo un codice tariffa numerico
143900090311           when  V2Cctr <> *blank   and
144000090311                 %check(c_Digits : V2Cctr) > *zero;
144100090311             errMessage  = *on;
144200090311             errGenerico = *on;
144300090312             PosCurCtr   = *on;
144400090312             V1Dmsg = 'Ammessi SOLO caratteri numerici';
144500090311             leavesr;
144600090310         endsl;
144700090226
144800090327         // - Se codice padre <> da codice cliente
144900090306         //   stesso ksc/ctr/importo
145000151201         if  V2Cccm <> V2Cksu
145100151201           // e se il rcd di tabella è stato trovato
145200151201             and wForzaKsu <> *on;
145300090226           select;
145400151201             when  p_§LA2ksc <> V2Cksc;
145500090226               errMessage  = *on;
145600090226               errGenerico = *on;
145700090303               PosCurKsc   = *on;
145800090309               V1Dmsg = 'Cliente tassazione +
145900090309                         <> da quello del cod. padre ('
146000121213                      + %editc(p_§LA2ksc : 'X') + ')';
146100090226               leavesr;
146200151201             when  p_§LA2ctr <> V2Cctr
146300151201           // e se il rcd di tabella è stato trovato
146400151201             and wForzaKsu <> *on;
146500090226               errMessage  = *on;
146600090226               errGenerico = *on;
146700090303               PosCurCtr   = *on;
146800090309               V1Dmsg = 'Codice tariffa +
146900090309                         <> quello del cod. padre ('
147000121213                      + p_§LA2ctr + ')';
147100090226               leavesr;
147200090226           endsl;
147300090226         endif;
147400090226
147500090309         //?Importo & Tipo importo forzato?
147600090310         select;
147700090310           // - Non ammesso importo per Varia Negata
147800090310           when  V2Ctad = 'V'   and   V2imp <> *zero;
147900090310             errMessage  = *on;
148000090310             errGenerico = *on;
148100090310             PosCurImp   = *on;
148200090310             V1Dmsg = 'NON ammesso l''importo forzato per +
148300090310                       Varia Negata';
148400090310             leavesr;
148500090310           // - Non ammesso tipo importo senza importo
148600090310           when  V2imp =  *zero   and   V2fimp <> *blank;
148700090310             errMessage  = *on;
148800090310             errGenerico = *on;
148900090310             PosCurImp   = *on;
149000090310             V1Dmsg = 'Importo forzato obbligatorio SE +
149100090310                       inserito il tipo di importo';
149200090310             leavesr;
149300090312           // - Se inserito importo devono essere inseriti:
149400090312           //   · il codice cliente forzato (ksc),
149500090312           //   · la tariffa forzata (ctr) ed
149600090312           //   · il tipo importo (fimp)
149700090310           when  V2imp <> *zero;
149800090310             select;
149900090310               when  V2fimp  = *blank;
150000090310                 errMessage  = *on;
150100090310                 errGenerico = *on;
150200090310                 PosCurFimp  = *on;
150300090310                 V1Dmsg = 'Immettere il tipo importo';
150400090310                 leavesr;
150500090310               when  V2Cksc  = *zero;
150600090310                 errMessage  = *on;
150700090310                 errGenerico = *on;
150800090310                 PosCurKsc   = *on;
150900090310                 V1Dmsg = 'Immettere il cliente forzato';
151000090310                 leavesr;
151100090312               when  V2Cctr  = *blank;
151200090310                 errMessage  = *on;
151300090310                 errGenerico = *on;
151400090310                 PosCurCtr   = *on;
151500090310                 V1Dmsg = 'Immettere la tariffa forzata';
151600090310                 leavesr;
151700090310             endsl;
151800090310         endsl;
151900090312
152000090327         // Verifica compatibilità dell'importo forzato con il
152100090312         //   massimo imponibile IVA in tab. "GEI"/"LAC".
152200090312         if  V2imp <> *zero   and   V2fimp = 'T';
152300090312           // - Reperimento divisa moneta di conto gestionale
152400090312           clear  dGED;
152500090312           reset  TIBS02ds;
152600090312           T02cod = 'GED';
152700090312           T02ke1 = '1';
152800090312           TNTBE_RicercaControllo(kpjba : tibs02ds);
152900090312           if  T02err = *blank;
153000090312             dGED = T02uni;
153100090312           endif;
153200090312           // - Reperimento importi da tab. "GEI"
153300090312           //   (interessa il massimo imponibile IVA per spedizione)
153400090312           clear  dGEI;
153500090312           reset  TIBS02ds;
153600090312           T02cod = 'GEI';
153700090312           T02ke1 = §GEDcn;
153800090312           TNTBE_RicercaControllo(kpjba : tibs02ds);
153900090312           if  T02err = *blank;
154000090312             dGEI = T02uni;
154100090312           endif;
154200090312           if  V2imp > §GEImf;
154300090312             errMessage  = *on;
154400090312             errGenerico = *on;
154500090312             PosCurImp   = *on;
154600090312             V1Dmsg = 'Importo superiore al MASSIMO IMPONIBILE IVA +
154700090312                       PER SPEDIZIONE ('
154800090312                      + %trim( %editc( §GEImf : 'J' ) ) + ')';
154900090312             leavesr;
155000090312           endif;
155100090312         endif;
155200090226
155300090312         // Se codice padre <> da codice cliente
155400090306         //   stesso importo e tipo importo
155500151201         if  V2Cccm <> V2Cksu
155600151201           // e se il rcd di tabella è stato trovato
155700151201             and wForzaKsu <> *on;
155800090226           select;
155900151201             when  p_§LA2imp <> V2imp;
156000090226               errMessage  = *on;
156100090226               errGenerico = *on;
156200090312               PosCurImp   = *on;
156300090309               V1Dmsg = 'Importo forzato +
156400090309                         <> da quello del cod. padre ('
156500121213                      + %editc(p_§LA2imp : 'J') + ')';
156600090226               leavesr;
156700121213             when  p_§LA2fimp <> V2fimp;
156800090226               errMessage  = *on;
156900090226               errGenerico = *on;
157000090312               PosCurFimp  = *on;
157100090309               V1Dmsg = 'Tipo importo forzato +
157200090309                         <> da quello del cod. padre ('
157300121213                      + p_§LA2fimp + ')';
157400090226               leavesr;
157500090226           endsl;
157600090225         endif;
157700090923
157800090923         //?Modifica dati sensibili per l'addebito
157900090923         if (v2tadu <> savtadu or v2ctad <> savtad or
158000090923             v2cksc <> savksc or v2cctr <> savctr or
158100090923             v2imp <> savimp or v2fimp <> savfimp) and not *in02
158200090923             and not $forza;
158300090923               errMessage  = *on;
158400090923               errGenerico = *on;
158500090923               PosCurTadu  = *on;
158600090923               V1Dmsg = 'ATTENZIONE!! Possibili problemi ' +
158700090923                        'in fase di addebito. Avvisare Monica/Nicola';
158800090923               $forza = *on;
158900090923               leavesr;
159000090923         endif;
159100110713
159200110713         //?Creazione file elenco per il cliente
159300110713         //?possibile solo se NON è elaborazione automatica (FTP)
159400110713         IF  V2pgm <> *blanks and V2caut = 'A';
159500110713           ErrMessage  = *on;
159600110713           ErrGenerico = *on;
159700110713           PosCurPgm   = *on;
159800110713           V1Dmsg = 'NON possibile creazione elenco se elaborazione '+
159900110713                    'automatica';
160000110713           leavesr;
160100110713         ENDIF;
160200110713         //?se inserito il nome del programma deve esserci anche il tipo elenco
160300110713         IF  V2pgm <> *blanks and V2ele = *blanks;
160400110713           ErrMessage  = *on;
160500110713           ErrGenerico = *on;
160600110713           PosCurEle   = *on;
160700110713           V1Dmsg = 'Immettere anche il tipo elenco da creare';
160800110713           leavesr;
160900110713         ENDIF;
161000110713         //?e viceversa se c'è il tipo elenco deve esserci anche il nome del pgm
161100110713         IF  V2pgm = *blanks and V2ele <> *blanks;
161200110713           ErrMessage  = *on;
161300110713           ErrGenerico = *on;
161400110713           PosCurPgm   = *on;
161500110713           V1Dmsg = 'Immettere anche il nome del programma x elenco da creare';
161600110713           leavesr;
161700110713         ENDIF;
161800110713         //?Se codice padre <> da codice cliente
161900110713         //?  devono avere gli stessi dati
162000151201         IF  V2Cccm <> V2Cksu
162100151201           // e se il rcd di tabella è stato trovato
162200151201             and wForzaKsu <> *on;
162300110713           SELECT;
162400121213             WHEN  p_§LA2pgm <> V2pgm;
162500110713               ErrMessage  = *on;
162600110713               ErrGenerico = *on;
162700110713               PosCurPgm   = *on;
162800110713               V1Dmsg = 'Nome programma +
162900110713                         <> da quello del cod. padre ('
163000121213                      + p_§LA2pgm + ')';
163100110713               leavesr;
163200121213             WHEN  p_§LA2ele <> V2ele;
163300110713               ErrMessage  = *on;
163400110713               ErrGenerico = *on;
163500110713               PosCurEle   = *on;
163600110713               V1Dmsg = 'Tipo elenco +
163700110713                         <> da quello del cod. padre ('
163800121213                      + p_§LA2ele + ')';
163900110713               leavesr;
164000110713           ENDSL;
164100110713         ENDIF;
164200071207
164300071207       ENDSR;
164400090225
164500090225       //--------------------------------------------------------------
164600121213       //?Controllo esistenza del codice padre in tabella LA2          ?
164700090225       //--------------------------------------------------------------
164800090303       BEGSR  sr_Padre;
164900090302
165000121213         // Controlla esistenza in tabella "LA2"
165100121213         clear  dLA2p;
165200090310         reset  TIBS02ds;
165300090310         T02sif = KNSIF;
165400090310         %subst(T02ke1 : 1 : 7) = %editc(V2Cksu : 'X');
165500090310         TNTBE_RicercaControllo(kpjba : tibs02ds);
165600090310         if  T02err = *blank;
165700121213           dLA2p  = T02uni;
165800090310         else;
165900151201           if  not wForzaKsu;
166000151201             wForzaKsu = *on;
166100151201             errMessage  = *on;
166200151201             errGenerico = *on;
166300151201             PosCurksu   = *on;
166400151201             V1Dmsg = $Msg(12);
166500151201             leavesr;
166600151201           endif;
166700151201         endif;
166800090327
166900090327         // Controlla se già inserito come figlio di altro padre
167000151201         if  p_§LA2ksu <> V2Cksu
167100151201         // se il rcd di tabella è stato trovato
167200151201             and wForzaKsu <> *on;
167300090327           errMessage  = *on;
167400090327           errGenerico = *on;
167500090327           PosCurKsu   = *on;
167600090327           V1Dmsg = 'Non ammesso cod. PADRE già FIGLIO di altro +
167700121213                     cliente (' + %editc(p_§LA2ksu : 'X') + ')';
167800090327           leavesr;
167900090327         endif;
168000090225
168100090225       ENDSR;
168200071206
168300071212       //--------------------------------------------------------------
168400121213       //?Aggiornamento record TNTBE00F (tab. LA2)                     ?
168500071212       //--------------------------------------------------------------
168600090306       BEGSR  Upd_TNTBE;
168700071207
168800121213         clear  dLA2;
168900121213         §LA2rag   = V2Dccm;
169000121213         §LA2tpt   = V2Ctpt;
169100121213         §LA2res   = V2Cres;
169200121213         §LA2rec   = V2Crec;
169300121213         §LA2csr   = V2Ccsr;
169400121213         §LA2ssr   = V2Cssr;
169500121213         §LA2dir   = V2Cdir;
169600121213         §LA2tpi   = V2Ctpi;
169700121213         §LA2fmi   = V2Cfmi;
169800121213         §LA2aut   = V2Caut;
169900121213         §LA2tdta  = V2tdta;
170000121213         §LA2tad   = V2Ctad;
170100121213         §LA2schgg = V2Cschgg;
170200121213         §LA2sch01 = V2Csch01;
170300121213         §LA2sch02 = V2Csch02;
170400121213         §LA2sch03 = V2Csch03;
170500121213         §LA2sch04 = V2Csch04;
170600121213         §LA2sch05 = V2Csch05;
170700121213         §LA2sch06 = V2Csch06;
170800121213         §LA2sch07 = V2Csch07;
170900121213         §LA2sch08 = V2Csch08;
171000121213         §LA2sch09 = V2Csch09;
171100121213         §LA2sch10 = V2Csch10;
171200121213         §LA2sch11 = V2Csch11;
171300121213         §LA2sch12 = V2Csch12;
171400121213         §LA2note  = V2note;
171500121213         §LA2lnp   = V2Clnp;
171600121213         §LA2ksu   = V2Cksu;
171700121213         §LA2mesi  = V2mesi;
171800121213         §LA2datai = (%int( %subst( %editc( V2datai : 'X') : 3 : 4 ) )
171900090313                      * 100)
172000090313                   + %int( %subst( %editc( V2datai : 'X') : 1 : 2 ) );
172100121213         §LA2datela...
172200090313                   = (%int( %subst( %editc( V2datela : 'X') : 3 : 4 ) )
172300090319                      * 100)
172400090313                    + %int( %subst( %editc( V2datela : 'X') : 1 : 2 ) );
172500121213         §LA2tadu  = V2tadu;
172600121213         §LA2ksc   = V2Cksc;
172700121213         §LA2ctr   = V2Cctr;
172800121213         §LA2fimp  = V2fimp;
172900121213         §LA2imp   = V2imp;
173000121213         §LA2pgm   = V2pgm;
173100121213         §LA2ele   = V2ele;
173200071207
173300121213         TBEuni    = dLA2;
173400071207
173500090306         clear  TBEftt;
173600090306         clear  TBEftr;
173700071207
173800071207         select;
173900151201           // F7=Ripristino
174000151201           when  dsp_aid = c_F07;
174100090306             clear  TBEatb;
174200090306           // F16=Annullamento
174300090306           when  dsp_aid = c_F16;
174400071207             TBEatb = 'A';
174500071207         endsl;
174600121220
174700121220         TBEdtr = woggi;
174800071207
174900071207         IF  NOT  %found(TNTBE01L);
175000090306           clear  TBEatb;
175100090306           clear  TBEflt;
175200090306           clear  TBEftr;
175300090306           //clear  TBEdtr;
175400090306           TBEsif = TBXsif;
175500090306           TBElin = TBXlin;
175600090306           TBEapl = TBXapl;
175700090306           TBEcod = k_TBEcod;
175800090306           TBEke1 = k_TBEke1;
175900090306           TBEke2 = k_TBEke2;
176000090306           //_____________
176100071207           WRITE TNTBE000;
176200090306           //¯¯¯¯¯¯¯¯¯¯¯¯¯
176300071207         ELSE;
176400090306           //______________
176500071207           UPDATE TNTBE000;
176600090306           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
176700071207         ENDIF;
176800071207
176900071207       ENDSR;
177000071207
177100071212       //--------------------------------------------------------------
177200090306       //?Operazioni finali.                                           ?
177300071212       //--------------------------------------------------------------
177400090306       BEGSR  sr_RoutEnd;
177500071207
177600090306         clear  TIBS69ds;
177700090304         tibs69ds.I69tla = 'C';
177800090304         tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
177900071207
178000121213         kpjbu = TNTB464ds;
178100071207         return;
178200071207
178300071207       ENDSR;
178400090306
178500071207      /end-free
178600071207
178700071212       //--------------------------------------------------------------
178800090306       //?Schiere a tempo di compilazione.                             ?
178900071212       //--------------------------------------------------------------
179000071207
179100071207** - $MSG -------------------------------------------------------------------*
179200071207Immettere il codice cliente                                                     1
179300071207Immettere SOLO caratteri numerici                                               2
179400071207Codice errato                                                                   3
179500071207Cliente annullato                                                               4
179600071207Immettere la directory                                                          5
179700071207Carattere "\" non valido; per indicare una sottocartella utilizzare "/"         6
179800071210Caratteri & e %  non ammessi per la directory di clienti con invio automatico   7
179900071207Indicare il giorno del mese se specificato almeno un mese                       8
180000071207Specificare almeno un mese se indicato il giorno del mese                       9
180100071207Per i mesi con n° di giorni < &g verrà assunto il loro ultimo giorno -> Enter  10
180200090225Inserire il codice padre                                                       11
180300151201Cod.Padre <> Cod.Cliente e Cod.Padre inesistente in tab.LA2 - Enter per forzare12
180400121213Cliente da copiare NON più reperibile in tab. "LA2"                            13
180500121213Cliente già inserito in tab. "LA2"                                             14
