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
024000121220     d woggi           s              8  0 inz
024100071207
024200090306       //--------------------------------------------------------------
024300090306       //?Definizione key-list.                                        ?
024400090306       //--------------------------------------------------------------
024500090306
024600090306       // - File TNTBE01L
024700090306     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
024800090306     d                                     prefix(k_)  inz
024900071206
025000090306       //--------------------------------------------------------------
025100090306       //?Riepilogo indicatori.                                        ?
025200090306       //--------------------------------------------------------------
025300090306       // 01    - RECORD annullato
025400090306       // 02    - acceso IMMISSIONE - spento MANUTENZIONE
025500090306       // 25    - Comodo
025600090306       // 28    - Emissione del messaggio di errore a video
025700090306       // 40    - Protezione campi in prima estrazione
025800090306       // x D01:
025900090306       // 50    - Codice cliente errato o mancante
026000090306       // x D02:
026100120112       // 41    - Copia in D02: V2CCCM non protetto
026200090306       // 53    - Directory per immagini       errata
026300090306       // 54    - Giorno di schedulazione CD   errato
026400090306       // 55    - Selezione mesi obbligatoria
026500090306       // 56    - Selezione del mese di febbraio
026600090306       // 57    - Selezione del mese di aprile
026700090306       // 58    - Selezione del mese di giugno
026800090306       // 59    - Selezione del mese di settembre
026900090306       // 60    - Selezione del mese di novembre
027000090306       // 61    - Codice Padre                 errato
027100090306       // 62    - N. mesi da estrarre          errato
027200090306       // 63    - Tipo data di elaborazione    errato
027300090306       // 64    - Tipo addebito                errato
027400090306       // 65    - Creazione addebito           errato
027500090306       // 66    - Cliente tassazione           errato
027600090306       // 67    - Codice tariffa               errato
027700090306       // 68    - Importo forzato              errato
027800090306       // 69    - Tipo importo forzato         errato
027900090306       // 70    - Flag nome immagine           errato
028000090306       // 71    - Prima elaborazione           errata
028100090306       // 99    - Generico di errore
028200090306       //--------------------------------------------------------------
028300071206
028400071206     c     *Entry        plist
028500071206     c                   parm                    KPJBA
028600121213     c                   parm                    tntb464ds
028700121213
028800071207      /free
028900071207
029000071207       // Operazioni iniziali
029100090306       exsr  sr_RoutInz;
029200071207
029300071207       // Gestione video
029400090306       DOW  $Fine = *off;
029500071207         select;
029600090306           when  $Video = 'D2';
029700090306             exsr  sr_GesD02;
029800071207           other;
029900071207             leave;
030000071207         endsl;
030100071207       ENDDO;
030200071207
030300071207       // Operazioni finali
030400090306       exsr  sr_RoutEnd;
030500071206
030600071212       //--------------------------------------------------------------
030700090306       //?Operazioni iniziali.                                         ?
030800071212       //--------------------------------------------------------------
030900090306       BEGSR  sr_RoutInz;
031000090306
031100090306         *inLR = *on;
031200121213         B464fxx = *blank;
031300121213         B464err = *off;
031400121213         B464msg = *blank;
031500071207
031600090306         // Reperimento dati job
031700090306         exsr  DatiJob;
031800071207
031900090306         // Impostazione nome programma a video
032000071207         V1Tpgm = SDSpgm;
032100090306
032200090306         // Aggancio dati generali della tabella in esame
032300090306         clear  k_TBEcod;
032400090306         k_TBEke1 = *zero;
032500090306         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
032600090306                = c_Tab;
032700090306         clear  k_TBEke2;
032800090306         clear  k_TBElin;
032900090306         k_TBEsif = KNSIF;
033000090306         chain(n)  %kds(k05tntbe01)  TNTBE000;
033100090306         if  not  %found(TNTBE01L);
033200090306           clear  k_TBEsif;
033300090306           chain(n)  %kds(k05tntbe01)  TNTBE000;
033400090306         endif;
033500090306         if  %found(TNTBE01L);
033600090306           xTNTBEds = TNTBEds;
033700090306         else;
033800090306           clear  xTNTBEds;
033900090306         endif;
034000071207
034100090306         // Verifica parametri ricecvuti
034200121213         if  B464ke1  <> *blank;
034300121213           exsr  CtrDati;
034400121213           if  ErrMessage;
034500121213             B464err = *on;
034600121213             B464msg = V1Dmsg;
034700071217             $Fine  = *on;
034800071217             leavesr;
034900071217           endif;
035000071207           $Video  = 'D2';
035100071207           $InzD02 = *on;
035200071207         endif;
035300121220
035400121220         // Imposto la data di oggi
035500121220         woggi = %dec(%date());
035600071207
035700071207       ENDSR;
035800071206
035900071212       //--------------------------------------------------------------
036000090306       //?Reperimento Dati del job (Utente/Operativi).                 ?
036100071212       //--------------------------------------------------------------
036200090306       BEGSR  DatiJob;
036300071207
036400071207         in(E) §AzUte;
036500071207         if NOT %error;
036600071207           in(E) §DatiUte;
036700071207         endif;
036800090306         if %error or RSut = *blank;
036900071207           clear TIBS34ds;
037000071207           tibs34r(tibs34ds);
037100071207           in §AzUte;
037200071207           in §DatiUte;
037300071207         endif;
037400071207
037500071207       ENDSR;
037600071206
037700071212       //--------------------------------------------------------------
037800121213       //?Controllo dati passati.
037900071212       //--------------------------------------------------------------
038000121213       BEGSR  CtrDati;
038100071207
038200121213         IF  B464ke1 = *blank  or  B464ke1 = *zero;
038300121213           ErrMessage  = *on;
038400121213           ErrGenerico = *on;
038500121213           B464msg = $Msg(01);
038600121213           leavesr;
038700121213         ENDIF;
038800121213
038900121213         clear  TIBS69ds;
039000121213         tibs69ds.I69kcc = DUTkci;
039100121213         tibs69ds.I69kac = %int(%subst(B464ke1:1:7));
039200121213         tibs69ds.I69sif = knsif;
039300121213         tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
039400071207
039500090306         // - Verifica esistenza codice cliente in tabella
039600090306         reset  TNTBE000;
039700090310         k_TBEcod = c_Tab;
039800121213         k_TBEke1 = B464ke1;
039900090306         clear  k_TBEke2;
040000090306         k_TBElin = TBXlin;
040100090306         k_TBEsif = TBXsif;
040200120112
040300121213         chain  %kds(K05tntbe01) TNTBE000;
040400121213         // - - NON ammesso nuovo record per cliente annullato
040500121213         if  NOT  %found(TNTBE01L)  and  ds_CNACO.ACOflg <> *blank;
040600121213           ErrMessage  = *on;
040700121213           ErrGenerico = *on;
040800121213           B464msg = $Msg(04);
040900121213           leavesr;
041000121213         ENDIF;
041100071207
041200120112         // - Decodifica cliente
041300121213         IF  %found(TNTBE01L);
041400121213           dLA2 = TBEuni;
041500130115         //ELSE;
041600130115         //  §LA2rag = %subst(ds_CNACO.ACOrag : 1 : %len(V2Dccm));
041700121213         ENDIF;
041800130115
041900130115         §LA2rag = %subst(ds_CNACO.ACOrag : 1 : %len(V2Dccm));
042000071207
042100071207       ENDSR;
042200071206
042300071212       //--------------------------------------------------------------
042400090306       //?Gestione videata D02                                         ?
042500071212       //--------------------------------------------------------------
042600090306       BEGSR  sr_GesD02;
042700071207
042800090306         // Inizializzazione videata
042900090306         if   $InzD02 = *on;
043000090306           exsr  sr_InzD02;
043100090306           $InzD02 = *off;
043200071207         endif;
043300071207
043400090306         // Emissione testata
043500121213         write  TN464T01;
043600071207
043700090306         // Emissione videata
043800141205         if  B464opz <> '5';
043900121213           exfmt  TN464D02;
044000071217         else;
044100121213           write  TN464D02;
044200090306           exfmt  PROTECT;
044300071217         endif;
044400071207         errMessage  = *off;
044500071207         errGenerico = *off;
044600090306         clear  V1Dmsg;
044700071207
044800071207         SELECT;
044900090306           // F12=Ritorno
045000090306           WHEN  dsp_aid = c_F12;
045100090306             exsr  sr_F12D02;
045200090306           // Enter o F5 o F6 o F16
045300071207           OTHER;
045400090306             // - Controllo dati
045500090306             if  dsp_aid = c_F16;
045600090306               exsr  sr_CtrAnnull;
045700090306             else;
045800090306               exsr  sr_CtrD02;
045900090306             endif;
046000090306             if  errGenerico;
046100071207               leavesr;
046200071207             endif;
046300121212
046400090306             // - Conferma dati:
046500090306             //   F5=Ripristino; F6=Conferma; F16=Annullamento
046600090306             if  dsp_aid = c_F05  or
046700090306                 dsp_aid = c_F06  or
046800090306                 dsp_aid = c_F16;
046900090306               exsr  Upd_TNTBE;
047000121212
047100071207               if  NOT  errGenerico;
047200121213                 $Fine  = *on;
047300071207               endif;
047400071207             EndIf;
047500071207
047600071207         ENDSL;
047700071207
047800071207       ENDSR;
047900071206
048000071212       //--------------------------------------------------------------
048100090306       //?Inizializzazione videata D01                                 ?
048200071212       //--------------------------------------------------------------
048300090306       BEGSR  sr_InzD02;
048400071207
048500090306         reset  $CtrMm;
048600090313         reset  $ByPass;
048700121016         reset  wForza;
048800121030         reset  wForzaTPI;
048900121030         reset  wForzaFMI;
049000121030         reset  wForzaDIR;
049100101103
049200121213         clear  TN464D02;
049300071210
049400090306         IndDspF = *zero;
049500090309
049600090309         // Impostazione indicatori per abilitazione tasti funzionali
049700090309         exsr  Set_Ind_D02;
049800090309
049900120112         // - Codice estrazione (chiave)
050000121213         V2Cccm = %int(%subst(B464ke1:1:7));
050100121213         V2Dccm = §LA2rag;
050200121217
050300121217         // Recupero sempre i dati della tabella LAC
050400121217           reset  TIBS02ds;
050500121217           T02cod = 'LAC';
050600121217           T02ke1 = B464ke1;
050700121217           TNTBE_RicercaControllo(kpjba : tibs02ds);
050800121217           IF  T02err = *blank;
050900121217             dLAC = T02uni;
051000121217           ENDIF;
051100121213
051200121213         // Immissione nuovo rcd recupero dati da tabella LAC
051300121213         IF  not %found(TNTBE01L);
051400121213           V1Tlav = 'IMMISSIONE';
051500121217           dLA2 = T02uni;
051600121213           clear §LA2dir;
051700121213           *in02 = *on;
051800121213         ENDIF;
051900071207
052000121213         // Rcd già presente
052100121213         IF  %found(TNTBE01L);
052200090306           // Impostazione dati di record trovato
052300071217           select;
052400121213             when  B464opz = '5';
052500071217               V1Tlav = 'VISUALIZZA';
052600090306             when  TBEatb = *blank;
052700071217               V1Tlav = 'VARIAZIONE';
052800071217             other;
052900071217               V1Tlav = 'ANNULLATO ';
053000071217           endsl;
053100121213           *in02 = *off;
053200121213         ENDIF;
053300090225
053400090306           // Codice padre
053500121213           V2Cksu = §LA2ksu;
053600090309           clear  TIBS69ds;
053700090309           clear  ds_CNACO;
053800090309           clear  ds_CNIND;
053900090309           clear  ds_CNCLP;
054000090309           clear  ds_FNCLS;
054100090310           select;
054200121213             when  §LA2ksu =  V2Cccm;
054300090310               V2Dksu = V2Dccm;
054400121213             when  §LA2ksu <> *zero;
054500090310               tibs69ds.I69kcc = DUTkci;
054600090310               tibs69ds.I69kac = %int(V2Cksu);
054700090310               tibs69ds.I69sif = knsif;
054800090310               tibs69r(TIBS69ds:ds_CNACO:ds_CNIND:ds_CNCLP:ds_FNCLS);
054900090310               if tibs69ds.O69err = *on;
055000090310                 V1Dmsg = $Msg(03);
055100090310                 errMessage  = *on;
055200090310                 errGenerico = *on;
055300090310                 PosCurKsu   = *on;
055400090310                 leavesr;
055500090310               endif;
055600090310               V2Dksu = ds_CNACO.ACOrag;
055700090310           endsl;
055800090225
055900121213           V2Caut   = §LA2aut;
056000121213           V2Ctpi   = §LA2tpi;
056100090225
056200090306           // Flag nome immagine
056300121213           V2Cfmi = §LA2fmi;
056400090310           reset  TIBS02ds;
056500090303           T02cod = 'NIM';
056600121213           T02ke1 = §LA2fmi;
056700090304           TNTBE_RicercaControllo(kpjba : tibs02ds);
056800090306           if  T02err = *blank;
056900090303             V2Dfmi = T02uni;
057000090303           endif;
057100090225
057200121213           V2Cdir   = §LA2dir;
057300121213           V2Ctpt   = §LA2tpt;
057400121213           V2Cssr   = §LA2ssr;
057500121213           V2Clnp   = §LA2lnp;
057600121213           V2Cres   = §LA2res;
057700121213           V2Crec   = §LA2rec;
057800121213           V2Ccsr   = §LA2csr;
057900090309
058000090309           // Prima estrazione
058100090309           clear  wDataI;
058200121213           if  §LA2datai > *zero;
058300121213             %subst(wDataI:1:2) = %subst(%editc(§LA2datai:'X'):5:2);
058400121213             %subst(wDataI:3:4) = %subst(%editc(§LA2datai:'X'):1:4);
058500090310             V2datai = %int(wDataI);
058600090309           endif;
058700090309
058800090309           // Ultima estrazione
058900090309           clear  wDatEla;
059000121213           if  §LA2datela > *zero;
059100121213             %subst(wDatEla:1:2) = %subst(%editc(§LA2datela:'X'):5:2);
059200121213             %subst(wDatEla:3:4) = %subst(%editc(§LA2datela:'X'):1:4);
059300090310             V2datela = %int(wDatEla);
059400090309           endif;
059500090309
059600121213           V2tdta   = §LA2tdta;
059700121213           V2mesi   = §LA2mesi;
059800121213           V2Cschgg = §LA2schgg;
059900121213           V2Csch01 = §LA2sch01;
060000121213           V2Csch02 = §LA2sch02;
060100121213           V2Csch03 = §LA2sch03;
060200121213           V2Csch04 = §LA2sch04;
060300121213           V2Csch05 = §LA2sch05;
060400121213           V2Csch06 = §LA2sch06;
060500121213           V2Csch07 = §LA2sch07;
060600121213           V2Csch08 = §LA2sch08;
060700121213           V2Csch09 = §LA2sch09;
060800121213           V2Csch10 = §LA2sch10;
060900121213           V2Csch11 = §LA2sch11;
061000121213           V2Csch12 = §LA2sch12;
061100090225
061200121213           V2tadu   = §LA2tadu;
061300121213           V2Ctad   = §LA2tad;
061400121213           V2cksc   = §LA2ksc;
061500121213           V2cctr   = §LA2ctr;
061600121213           V2imp    = §LA2imp;
061700121213           V2fimp   = §LA2fimp;
061800121213           V2pgm    = §LA2pgm;
061900121213           V2ele    = §LA2ele;
062000121213           V2note   = §LA2note;
062100090923
062200090923         // Salvo i dati sensibili per la creazione addebito
062300090923         savtad = v2ctad;
062400090923         savtadu = v2tadu;
062500090923         savksc = v2cksc;
062600090923         savctr = v2cctr;
062700090923         savimp = v2imp;
062800090923         savfimp = v2fimp;
062900090923         $forza = *off;
063000071207
063100071207       ENDSR;
063200071217
063300071217       //--------------------------------------------------------------
063400090306       //?Settaggio indicatori nella videata D02 per abilitazione      ?
063500090306       //?  tasti funzionali                                           ?
063600071217       //--------------------------------------------------------------
063700090306       BEGSR  Set_Ind_D02;
063800090319
063900090306         F5Attivo  = ((%found(TNTBE01L) and TBEatb <> *blank)
064000121213                                        and B464opz <> '5');
064100121213         F6Attivo  = (NOT F5Attivo and B464opz <> '5');
064200090306         F16Attivo = ((%found(TNTBE01L) and TBEatb = *blank)
064300121213                                        and B464opz <> '5');
064400090306
064500120112         // - Se ultima estrazione impostata proteggo la prima estrazione
064600121213         ProtDATAI = (§LA2datela <> *zero);
064700071217
064800071217       ENDSR;
064900090319
065000071217
065100071217       //--------------------------------------------------------------
065200090306       //?Gestione tasto funzionale F12 da videata D02                 ?
065300090306       //?F12=Ritorno                                                  ?
065400071217       //--------------------------------------------------------------
065500090306       BEGSR  sr_F12D02;
065600071217
065700121213           B464fxx = 'F12';
065800071217           $Fine = *on;
065900071217
066000071217       ENDSR;
066100090306
066200090306       //--------------------------------------------------------------
066300090306       //?Controllo "annullabilità" del record (padre senza figli)     ?
066400090306       //--------------------------------------------------------------
066500090306       BEGSR  sr_CtrAnnull;
066600090306
066700090306         IndDspF = *zero;
066800090306         // REimpostazione indicatori per abilitazione tasti funzionali
066900090306         exsr  Set_Ind_D02;
067000090306
067100090306         if  V2Cksu <= *zero;
067200121213           V2Cksu = §LA2ksu;
067300090306         endif;
067400090306
067500090306         // Se figlio (non padre): fine controlli
067600090306         if  V2Cccm <> V2Cksu;
067700090306           leavesr;
067800090306         endif;
067900090306
068000090306         //?Verifica se padre con figli?
068100121213         clear  dLA2p;
068200090316         SAVke1 = TBEke1;
068300090306         setll     %kds(k05tntbe01 : 1)  TNTBE000;
068400090306         reade(n)  %kds(k05tntbe01 : 1)  TNTBE000;
068500090306
068600090306         dow  not  %eof(TNTBE01L);
068700090316           if  TBEatb = *blank   and
068800090316               %editc(V2Cccm : 'X') <> %trimr(TBEke1);
068900121213             dLA2p = TBEuni;
069000121213             if  p_§LA2ksu = V2Cccm;
069100090306               errMessage  = *on;
069200090306               errGenerico = *on;
069300090306               PosCurKsu   = *on;
069400090306               V1Dmsg = 'Record NON cancellabile: trattasi di PADRE +
069500090306                         con figli (vedi ' + %trimr(TBEke1) + ')';
069600090306               leavesr;
069700090306             endif;
069800090306           endif;
069900090306         reade(n)  %kds(k05tntbe01 : 1)  TNTBE000;
070000090306         enddo;
070100090316
070200120112         // - Riaggancio del rec. in manutenzione/copia
070300090316         TBEke1 = SAVke1;
070400090316         reset  TNTBE000;
070500090316         k_TBEcod = c_Tab;
070600120112         k_TBEke1 = %editc( V2Cccm : 'X' );
070700090316         clear  k_TBEke2;
070800090316         k_TBElin = TBXlin;
070900090316         k_TBEsif = TBXsif;
071000090316         chain  %kds(K05tntbe01) TNTBE000;
071100090306
071200090306       ENDSR;
071300071206
071400071212       //--------------------------------------------------------------
071500090306       //?Controllo videata D02                                        ?
071600071212       //--------------------------------------------------------------
071700090306       BEGSR  sr_CtrD02;
071800071207
071900090309         %subst(IndDspF : 50) = *off;
072000090313
072100090313         // Verifica se variata la schedulazione dell'estrazione
072200090313         if  $ByPass;
072300090313           $ByPass = (ds_SchMm = wSavSch);
072400090313         endif;
072500090313         wSavSch = ds_SchMm;
072600090225
072700090306         //?Codice padre?
072800090225         select;
072900090306           when  V2Cksu  = *zero;
073000090306             errMessage  = *on;
073100090225             errGenerico = *on;
073200090303             PosCurKsu   = *on;
073300090225             V1Dmsg = $Msg(11);
073400090225             leavesr;
073500090306           when  V2Cksu = V2Cccm;
073600090303             V2Dksu = V2Dccm;
073700090225           other;
073800090327             // - Controllo esistenza codice padre come cliente
073900121213             //   in tabella LA2
074000090303             exsr  sr_Padre;
074100090303             if  errGenerico = *on;
074200090226               leavesr;
074300090225             endif;
074400121213             V2Dksu = p_§LA2rag;
074500090226         endsl;
074600101103         //?SE codice padre <> da codice cliente?
074700101103         // ?& sono in immissione?
074800101103         IF  V2Cccm <> V2Cksu   and   *in02;
074900101103           // ?& il padre ha già fatto estrazioni?
075000121213           If  p_§LA2datela <> *zero;
075100101103             // ?=> riporto le date del padre, proteggo il campo e non controllo?
075200101103             clear wDataI;
075300121213             %subst(wDataI:1:2) = %subst(%editc(p_§LA2datai:'X'):5:2);
075400121213             %subst(wDataI:3:4) = %subst(%editc(p_§LA2datai:'X'):1:4);
075500101103             V2datai = %int(wDataI);
075600101103             clear wDatEla;
075700121213             %subst(wDatEla:1:2) = %subst(%editc(p_§LA2datela:'X'):5:2);
075800121213             %subst(wDatEla:3:4) = %subst(%editc(p_§LA2datela:'X'):1:4);
075900101103             V2datela = %int(wDatEla);
076000101103             ProtDATAI = *on;
076100101103           EndIf;
076200100521         ENDIF;
076300090311
076400090311         //?Tipo elaborazione?
076500090327           // - Obbligatorio
076600121213           IF    V2Caut <> 'A'  and  V2Caut <> 'S'  and  V2Caut <> 'N';
076700090327             errMessage  = *on;
076800090327             errGenerico = *on;
076900090327             PosCurAut   = *on;
077000090327             V1Dmsg = 'Tipo elaborazione errato o mancante';
077100090327             leavesr;
077200121213         endif;
077300090327
077400090327         //?Tipo immagine?
077500120918         if  V2Ctpi <> 'TIF'   and   V2Ctpi <> 'JPG' and
077600120918             V2Ctpi <> 'PDF';
077700090327           errMessage  = *on;
077800090327           errGenerico = *on;
077900090327           PosCurTpi   = *on;
078000090327           V1Dmsg = 'Tipo immagine errato o mancante';
078100090327           leavesr;
078200090327         endif;
078300090226
078400090306         //?Flag nome immagine?
078500090310         clear  V2Dfmi;
078600090310         select;
078700090310           // - Obbligatorietà
078800090310           when  V2Cfmi = *blank;
078900090310             errMessage  = *on;
079000090310             errGenerico = *on;
079100090310             PosCurFmi   = *on;
079200090310             V1Dmsg = 'Flag nome immagine obbligatorio';
079300090310             leavesr;
079400090310           // - Ricerca
079500090310           when  %scan('?' : V2Cfmi) > *zero;
079600090310             clear  TIBS02ds;
079700090310             T02mod = 'R';
079800090310             T02cod = 'NIM';
079900090310             T02sif = KNSIF;
080000090310             TNTBE_RicercaControllo(kpjba : tibs02ds);
080100090310             if  T02err = *blank;
080200090310               V2Cfmi = %subst(T02ke1 : 1 : %len(V2Cfmi));
080300090310               V2Dfmi = T02uni;
080400090310               errGenerico = *on;
080500090310               leavesr;
080600090310             else;
080700090310               errMessage  = *on;
080800090310               errGenerico = *on;
080900090310               PosCurFmi   = *on;
081000090310               V1Dmsg = T02msg;
081100090310               leavesr;
081200090310             endif;
081300090310           // - Controllo
081400090310           other;
081500090310             reset  TIBS02ds;
081600090310             T02cod = 'NIM';
081700090310             T02ke1 = V2Cfmi;
081800090310             TNTBE_RicercaControllo(kpjba : tibs02ds);
081900090310             if  T02err <> *blank;
082000090310               errMessage  = *on;
082100090310               errGenerico = *on;
082200090310               PosCurFmi   = *on;
082300090310               V1Dmsg = 'Flag nome immagine errato';
082400090310               leavesr;
082500090310             endif;
082600090310             V2Dfmi = T02uni;
082700090310         ENDSL;
082800090226
082900090306         //?Directory per immagini?
083000090226         select;
083100090306           when  V2Cdir  = *blank;
083200090226             errMessage  = *on;
083300090226             errGenerico = *on;
083400090226             PosCurDir   = *on;
083500090226             V1Dmsg = $Msg(05);
083600090226             leavesr;
083700121213           when  V2Cdir  = §LACdir;
083800121213             errMessage  = *on;
083900121213             errGenerico = *on;
084000121213             PosCurDir   = *on;
084100121213             V1Dmsg = 'La directory deve essere diversa da tab. LAC';
084200121213             leavesr;
084300090316           when  %subst(V2Cdir : 1 : 1) = *blank;
084400090316             errMessage  = *on;
084500090316             errGenerico = *on;
084600090316             PosCurDir   = *on;
084700090316             V1Dmsg = 'Non ammessi spazi vuoti all''inizio del nome +
084800090316                       della directory';
084900090316             leavesr;
085000090306           when  %scan('\':V2Cdir) > *zero;
085100090226             errMessage  = *on;
085200090226             errGenerico = *on;
085300090226             PosCurDir   = *on;
085400090226             V1Dmsg = $Msg(06);
085500090226             leavesr;
085600090306           when  %scan('%':V2Cdir) > *zero   and
085700090306                           V2Caut  = 'A';
085800090226             errMessage  = *on;
085900090226             errGenerico = *on;
086000090226             PosCurDir   = *on;
086100090226             V1Dmsg = $Msg(07);
086200090226             leavesr;
086300090309           when  %scan('&':V2Cdir) > *zero  and
086400090309                           V2Caut  = 'A';
086500090309             errMessage  = *on;
086600090309             errGenerico = *on;
086700090309             PosCurDir   = *on;
086800090309             V1Dmsg = $Msg(07);
086900090309             leavesr;
087000090226         endsl;
087100090309
087200090316         wPos1 = %scan('/' : V2Cdir);
087300090316         DoW  wPos1 > *zero;
087400090316           if  %subst( V2Cdir : wPos1 : 2 ) = '//';
087500090316             errMessage  = *on;
087600090316             errGenerico = *on;
087700090316             PosCurDir   = *on;
087800090316             V1Dmsg = 'Nome directory NON valido: non ammesso il +
087900090316                       doppio "/"';
088000090316             leavesr;
088100090316           else;
088200090316             wPos1 = %scan('/' : V2Cdir : wPos1 + 1 );
088300090316           endif;
088400090316         EndDo;
088500090316
088600090309         wPos1 = %scan('&' : V2Cdir);
088700090309         IF  wPos1 > *zero;
088800090309           if  %subst( V2Cdir : wPos1 : 4 ) <> '&AAA'   and
088900090309               %subst( V2Cdir : wPos1 : 2 ) <> '&M';
089000090309             errMessage  = *on;
089100090309             errGenerico = *on;
089200090309             PosCurDir   = *on;
089300090309             V1Dmsg = 'Ammessi solo "&AAA" e "&M" (come caratteri +
089400090309                       sostituibili)';
089500090309             leavesr;
089600090309           endif;
089700090309           wPos2 = %scan( '&' : V2Cdir : wPos1 + 1 );
089800090309           If  wPos2 > *zero;
089900090309             if  %subst( V2Cdir : wPos2 : 4 ) <> '&AAA'   and
090000090309                 %subst( V2Cdir : wPos2 : 2 ) <> '&M';
090100090309               errMessage  = *on;
090200090309               errGenerico = *on;
090300090309               PosCurDir   = *on;
090400090309               V1Dmsg = 'Ammessi solo "&AAA" e "&M" (come caratteri +
090500090309                         sostituibili)';
090600090309               leavesr;
090700090309             endif;
090800090309           EndIf;
090900090309         ENDIF;
091000090309
091100090309         wPos1 = %scan('&M' : V2Cdir);
091200090309         IF  wPos1 > *zero;
091300090309           wPos2 = %scan( '&M' : V2Cdir : wPos1 + 1 );
091400090309           If  wPos2 > *zero   and
091500090309               %scan( '&M' : V2Cdir : wPos2 + 1 ) > *zero;
091600090309             errMessage  = *on;
091700090309             errGenerico = *on;
091800090309             PosCurDir   = *on;
091900090309             V1Dmsg = 'Ammessi max 2 "&M"';
092000090309             leavesr;
092100090309           EndIf;
092200090309         ENDIF;
092300090226
092400090327         // - Se codice padre <> da codice cliente
092500090306         //   la directory deve coincidere almeno fino alla 2ª subdir
092600121213         IF  V2Cccm <> V2Cksu   and   p_§LA2dir <> V2Cdir;
092700090311
092800090311           // - Controllo 1ª directory
092900090311           wPos1 = %scan( '/' : %trim( V2Cdir ) );
093000121213           wPos2 = %scan( '/' : %trim( p_§LA2dir ) );
093100090313           If  wPos1 <> wPos2                               OR
093200090313             ( wPos1  > *zero   and   wPos2 > *zero   and
093300090311               %subst( %trim( V2Cdir    ) : 1 : wPos1 ) <>
093400121213               %subst( %trim( p_§LA2dir ) : 1 : wPos2 ) )   OR
093500090313             ( wPos1  = *zero   and   wPos2  =  *zero
093600121213                                and   V2Cdir <> p_§LA2dir );
093700090311             errMessage  = *on;
093800090311             errGenerico = *on;
093900090311             PosCurDir   = *on;
094000090311             select;
094100090311               when  wPos1 =  *zero   and   wPos2 <> *zero;
094200090316                 V1Dmsg = 'Il padre ha altri rami directory (';
094300090311               when  wPos1 <> *zero   and   wPos2 =  *zero;
094400090311                 V1Dmsg = 'Il padre ha una directory unica (';
094500090311               other;
094600090311                 V1Dmsg = 'Primo ramo directory +
094700090311                           <> da quello del cod. padre (';
094800090311             EndSl;
094900121213             if  %len( %trim( p_§LA2dir ) ) <=
095000090311                 %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) ) - 1;
095100090311               V1Dmsg = %trimr( V1Dmsg )
095200121213                      + %trim( p_§LA2dir ) + ')';
095300090311             else;
095400090313               wPos1 = %len( V1Dmsg );
095500090313               wPos2 = %len( %trimr( V1Dmsg ) );
095600090311               V1Dmsg = %trimr( V1Dmsg )
095700121213                      + %trim( %subst( p_§LA2dir : 1 :
095800090311                          %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) )
095900090311                          - 4 ) )
096000090311                      + '...)';
096100090311             endif;
096200090311             leavesr;
096300090311           EndIf;
096400090311
096500090311           // - Controllo 2ª directory
096600090311           wPos1 = %scan( '/' : %trim( V2Cdir ) : wPos1 + 1 );
096700121213           wPos2 = %scan( '/' : %trim( p_§LA2dir ) : wPos2 + 1 );
096800090316           //If  wPos1 <> wPos2                               OR
096900090316           //  ( wPos1  > *zero   and   wPos2 > *zero   and
097000090316           //    %subst( %trim( V2Cdir    ) : 1 : wPos1 ) <>
097100121213           //    %subst( %trim( p_§LA2dir ) : 1 : wPos2 ) )   OR
097200090316           //  ( wPos1  = *zero   and   wPos2  =  *zero
097300121213           //                     and   V2Cdir <> p_§LA2dir );
097400090316           if  wPos1 = *zero;
097500090316             wPos1 = %len( %trimr( V2Cdir ) );
097600090316           else;
097700090316             wPos1 -= 1;
097800090316           endif;
097900090316           if  wPos2 = *zero;
098000121213             wPos2 = %len( %trimr( p_§LA2dir ) );
098100090316           else;
098200090316             wPos2 -= 1;
098300090316           endif;
098400090316           If  wPos1 <> wPos2   OR
098500090316               %subst(V2Cdir : 1 : wPos1) <>
098600121213                 %subst(p_§LA2dir : 1 : wPos2);
098700090311             errMessage  = *on;
098800090311             errGenerico = *on;
098900090311             PosCurDir   = *on;
099000090316             //select;
099100090316             //  when  wPos1 =  *zero   and   wPos2 <> *zero;
099200090316             //    V1Dmsg = 'Il padre ha altri rami directory (';
099300090316             //  when  wPos1 <> *zero   and   wPos2 =  *zero;
099400090316             //    V1Dmsg = 'Il padre ha meno rami directory (';
099500090316             //  other;
099600090313                 V1Dmsg = 'Secondo ramo directory +
099700090311                           <> da quello del cod. padre (';
099800090316             //EndSl;
099900121213             if  %len( %trim( p_§LA2dir ) ) <=
100000090311                 %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) ) - 1;
100100090311               V1Dmsg = %trimr( V1Dmsg )
100200121213                      + %trim( p_§LA2dir ) + ')';
100300090311             else;
100400090311               V1Dmsg = %trimr( V1Dmsg )
100500121213                      + %trim( %subst( p_§LA2dir : 1 :
100600090311                          %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) )
100700090311                          - 4 ) )
100800090311                      + '...)';
100900090311             endif;
101000090311             leavesr;
101100090311           EndIf;
101200090311
101300090306         ENDIF;
101400090327
101500090327         //?Tipo porto?
101600090327         if  V2Ctpt <> 'F'   and   V2Ctpt <> 'A'   and   V2Ctpt <> 'E';
101700090327           errMessage  = *on;
101800090327           errGenerico = *on;
101900090327           PosCurTpt   = *on;
102000090327           V1Dmsg = 'Tipo porto errato o mancante';
102100090327           leavesr;
102200090327         endif;
102300090311
102400090311         //?LNP per selezione?
102500090311         if  V2Clnp <> *zero;
102600090311           setll  (V2Clnp)  AZORG;
102700090311           if  NOT  %equal(AZORG01L);
102800090311             errMessage  = *on;
102900090311             errGenerico = *on;
103000090311             PosCurLnp   = *on;
103100090311             V1Dmsg = 'Linea di partenza errata';
103200090311             leavesr;
103300090311           endif;
103400090311         endif;
103500090226
103600090309         //?Prima estrazione dal?
103700090310         select;
103800090310           // - Obbligatoria se tipo elaborazione CD
103900090310           when  V2Caut = 'S'   and   V2datai = *zero;
104000090310             errMessage  = *on;
104100090310             errGenerico = *on;
104200090310             PosCurDatai = *on;
104300090310             V1Dmsg = 'Periodo iniziale della prima estrazione +
104400090310                       obbligatorio';
104500090310             leavesr;
104600090310           // - Verifica correttezza
104700090310           when  V2datai <> *zero                                  AND
104800090310              (%subst( %editc( V2datai : 'X' ) : 1 : 2 ) < '01'   or
104900090310               %subst( %editc( V2datai : 'X' ) : 1 : 2 ) > '12'   or
105000090310               %subst( %editc( V2datai : 'X' ) : 3 : 4 ) < '2000' or
105100090310               %subst( %editc( V2datai : 'X' ) : 3 : 4 ) > %char(*year));
105200090310             errMessage  = *on;
105300090310             errGenerico = *on;
105400090310             PosCurDatai = *on;
105500090310             V1Dmsg = 'Periodo iniziale della prima estrazione +
105600090310                       errato';
105700090310             leavesr;
105800090310         endsl;
105900090306         // - Se codice padre <> da codice cliente
106000090309         //   il periodo deve essere uguale
106100100521         if  V2Cccm <> V2Cksu;
106200090310           clear  wDataI;
106300090316           %subst( wDataI : 1 : 2 ) =
106400121213                   %subst( %editc( p_§LA2datai : 'X' ) : 5 : 2 );
106500090316           %subst( wDataI : 3 : 4 ) =
106600121213                   %subst( %editc( p_§LA2datai : 'X' ) : 1 : 4 );
106700090310           if  V2datai <> %int(wDataI);
106800090310             errMessage  = *on;
106900090310             errGenerico = *on;
107000090310             PosCurDatai = *on;
107100090310             V1Dmsg = 'Periodo iniziale della prima estrazione +
107200090310                       <> da quello del cod. padre ('
107300121213                    + %subst(%editc(p_§LA2datai : 'X') : 5 : 2)
107400090310                    + '/'
107500121213                    + %subst(%editc(p_§LA2datai : 'X') : 1 : 4)
107600090310                    + ')';
107700090310             leavesr;
107800090310           endif;
107900090226         endif;
108000090306
108100090309         //?Tipo data da elaborare?
108200090310         select;
108300090327           when  V2tdta <> ' '  and  V2tdta <> 'C'  and  V2tdta <> 'S';
108400090327             errMessage  = *on;
108500090327             errGenerico = *on;
108600090327             PosCurTdta  = *on;
108700090327             V1Dmsg = '"Flag "Data da elaborare" errato';
108800090327             leavesr;
108900090310           // - Tipo data da elaborare obbligatorio
109000090310           when  V2Caut = 'S'   and   V2tdta = *blank;
109100090310             errMessage  = *on;
109200090310             errGenerico = *on;
109300090310             PosCurTdta  = *on;
109400090310             V1Dmsg = '"Data da elaborare" obbligatoria per +
109500090310                       "Tipo Elaborazione" = "S" (schedulata)';
109600090310             leavesr;
109700090310           // - Se codice padre <> da codice cliente
109800090310           //   il tipo data da elaborare deve essere uguale
109900121213           when  V2Cccm <> V2Cksu   and   p_§LA2tdta <> V2tdta;
110000090310             errMessage  = *on;
110100090310             errGenerico = *on;
110200090310             PosCurTdta  = *on;
110300090310             V1Dmsg = 'Tipo data da elaborare +
110400090310                       <> da quello del cod. padre ('
110500121213                    + p_§LA2tdta + ')';
110600090310             leavesr;
110700090310         endsl;
110800090309
110900090309         //?Periodo da estrarre in mesi?
111000090310         select;
111100090310           // - Periodo da estrarre in mesi obbligatorio
111200090310           when  V2Caut = 'S'   and   V2mesi = *zero;
111300090310             errMessage  = *on;
111400090310             errGenerico = *on;
111500090310             PosCurMesi  = *on;
111600090310             V1Dmsg = '"Periodo da estrarre" obbligatorio per +
111700090310                       "Tipo Elaborazione" = "S" (schedulata)';
111800090310             leavesr;
111900090310           // - Se codice padre <> da codice cliente
112000090310           //   il periodo da estrarre deve essere uguale
112100121213           when  V2Cccm <> V2Cksu   and   p_§LA2mesi <> V2mesi;
112200090310             errMessage  = *on;
112300090310             errGenerico = *on;
112400090310             PosCurMesi  = *on;
112500090310             V1Dmsg = 'Periodo da estrarre +
112600090310                       <> da quello del cod. padre ('
112700121213                    + %editc(p_§LA2mesi:'X') + ')';
112800090310             leavesr;
112900090310         endsl;
113000090226
113100090309         //?Schedulazione giorno e mesi x preparazione CD?
113200090226         select;
113300090318           // when (V2Caut  =  'A'    or  V2Caut  =  'N')    AND
113400090318           when  V2Caut  =  'A'    AND
113500090311                (V2datai <> *zero  or  V2tdta   <> *blank   or
113600090311                 V2mesi  <> *zero  or  V2Cschgg <> *zero    or
113700090311                 ds_SchMm <> *blank);
113800090311             errMessage  = *on;
113900090311             errGenerico = *on;
114000090311             PosCurAut   = *on;
114100090311             V1Dmsg = 'Schedulazione incompatibile con il +
114200090311                       "Tipo Elaborazione" = ' + V2Caut;
114300090311             leavesr;
114400090318           when  V2Caut = 'S'  and  (V2Cschgg <= *zero);
114500090306             errMessage  = *on;
114600090306             errGenerico = *on;
114700090306             PosCurSchGg = *on;
114800090306             V1Dmsg = 'Schedulazione obbligatoria per +
114900090306                       "Tipo Elaborazione" = "S" (schedulata)';
115000090306             leavesr;
115100090306           when  V2Caut = 'S'  and  (ds_SchMm = *blank);
115200090306             errMessage  = *on;
115300090306             errGenerico = *on;
115400090306             PosCurSch01 = *on;
115500090306             V1Dmsg = 'Indicare almeno un mese per +
115600090306                       "Tipo Elaborazione" = "S" (schedulata)';
115700090306             leavesr;
115800090311         endsl;
115900090226
116000090327         // - Se codice padre <> da codice cliente
116100090306         //   i 2 codici devono avere gli stessi dati di estrazione
116200090306         //   gg e mese e n. mesi da elaborare
116300090309         if  V2Cccm <> V2Cksu;
116400090309           select;
116500121213             when  p_§LA2schgg <> V2Cschgg;
116600090226               errMessage  = *on;
116700090226               errGenerico = *on;
116800090303               PosCurSchgg = *on;
116900090309               V1Dmsg = 'Giorno creazione CD +
117000090309                         <> da quello del cod. padre ('
117100121213                      + %editc(p_§LA2schgg:'X') + ')';
117200090226               leavesr;
117300090303             when  p_SchMm <> ds_SchMm;
117400090226               errMessage  = *on;
117500090226               errGenerico = *on;
117600090226               PosCursch01 = *on;
117700090309               V1Dmsg = 'Mesi di elaborazione +
117800090309                         <> da quelli del cod. padre ('
117900090309                      + p_SchMm + ')';
118000090226               leavesr;
118100090226           endsl;
118200090226         endif;
118300090309
118400090327         // - Verifica congruenza tra mese di schedulazione e
118500090309         //   mesi di estrazione
118600090309         select;
118700090311           // - Nessun dato inserito
118800090310           when  V2mesi   = *zero    and   V2Cschgg = *zero    and
118900090311                 ds_SchMm = *blank;
119000090311           // - Periodo da estrarre in mesi richiesto
119100090310           when  V2mesi = *zero;
119200090310             errMessage  = *on;
119300090310             errGenerico = *on;
119400090310             PosCurMesi  = *on;
119500090310             V1Dmsg = 'Periodo da estrarre obbligatorio SE inserita +
119600090310                       una schedulazione';
119700090310             leavesr;
119800090311           // - Giorno di schedulazione richiesto
119900090310           when  V2Cschgg = *zero;
120000090310             errMessage  = *on;
120100090310             errGenerico = *on;
120200090310             PosCurSchgg = *on;
120300090311             V1Dmsg = $Msg(08);
120400090310             leavesr;
120500090311           // - Selezione mesi richiesta
120600090311           when  ds_SchMm = *blank;
120700090310             errMessage  = *on;
120800090310             errGenerico = *on;
120900090310             PosCurSch01 = *on;
121000090311             V1Dmsg = $Msg(09);
121100090310             leavesr;
121200090311         endsl;
121300090313
121400090313         // - Conteggio/Controllo mesi schedulati
121500090313         clear  wCount;
121600090311         wSchMm_ds = %triml(ds_SchMm);
121700090313         For  xx = 1  To  %elem($SchMm);
121800090313           if  $SchMm(xx) <> *blank;
121900090313             wCount += 1;
122000090313           endif;
122100090313         EndFor;
122200090313
122300090311         select;
122400090313           // - Errori bloccanti:
122500090313           when   wCount <> *zero   and
122600090313                 (V2mesi <> (12 / wCount));
122700090313             errMessage  = *on;
122800090313             errGenerico = *on;
122900090313             PosCurSch01 = *on;
123000090313             V1Dmsg = 'Incongruenza tra il "periodo da estrarre" +
123100090313                       ed i mesi schedulati';
123200090313           // - Errori bypassabili:
123300090313           when ( (V2mesi =  2   and  ($SchMm(03) = *blank     or
123400090311                                       $SchMm(05) = *blank     or
123500090311                                       $SchMm(07) = *blank     or
123600090311                                       $SchMm(09) = *blank     or
123700090311                                       $SchMm(11) = *blank))   or
123800090313                  (V2mesi =  3   and  ($SchMm(04) = *blank     or
123900090311                                       $SchMm(07) = *blank     or
124000090311                                       $SchMm(10) = *blank))   or
124100090313                  (V2mesi =  4   and  ($SchMm(05) = *blank     or
124200090311                                       $SchMm(09) = *blank))   or
124300090313                  (V2mesi =  6   and   $SchMm(07) = *blank) )  AND
124400090313                 $ByPass =  *off;
124500090313             $ByPass = *on;
124600090309             errMessage  = *on;
124700090309             errGenerico = *on;
124800090310             PosCurSch01 = *on;
124900090313             V1Dmsg = 'Mesi schedulati in maniera irregolare... +
125000090313                       Premere Enter per forzare.';
125100090309             leavesr;
125200090313           when ( (V2mesi =  2  and  ($SchMm(02) <> *blank     or
125300090311                                      $SchMm(04) <> *blank     or
125400090311                                      $SchMm(06) <> *blank     or
125500090311                                      $SchMm(08) <> *blank     or
125600090311                                      $SchMm(10) <> *blank     or
125700090311                                      $SchMm(12) <> *blank))   OR
125800090313                  (V2mesi =  3  and  ($SchMm(02) <> *blank     or
125900090311                                      $SchMm(03) <> *blank     or
126000090311                                      $SchMm(05) <> *blank     or
126100090311                                      $SchMm(06) <> *blank     or
126200090311                                      $SchMm(08) <> *blank     or
126300090311                                      $SchMm(09) <> *blank     or
126400090311                                      $SchMm(11) <> *blank     or
126500090311                                      $SchMm(12) <> *blank))   OR
126600090313                  (V2mesi =  4  and  ($SchMm(02) <> *blank     or
126700090311                                      $SchMm(03) <> *blank     or
126800090311                                      $SchMm(04) <> *blank     or
126900090311                                      $SchMm(06) <> *blank     or
127000090311                                      $SchMm(07) <> *blank     or
127100090311                                      $SchMm(08) <> *blank     or
127200090311                                      $SchMm(10) <> *blank     or
127300090311                                      $SchMm(11) <> *blank     or
127400090311                                      $SchMm(12) <> *blank))   OR
127500090313                  (V2mesi =  6  and  ($SchMm(02) <> *blank     or
127600090311                                      $SchMm(03) <> *blank     or
127700090311                                      $SchMm(04) <> *blank     or
127800090311                                      $SchMm(05) <> *blank     or
127900090311                                      $SchMm(06) <> *blank     or
128000090311                                      $SchMm(08) <> *blank     or
128100090311                                      $SchMm(09) <> *blank     or
128200090311                                      $SchMm(10) <> *blank     or
128300090311                                      $SchMm(11) <> *blank     or
128400090311                                      $SchMm(12) <> *blank))   OR
128500090313                  (V2mesi = 12  and  ($SchMm(02) <> *blank     or
128600090311                                      $SchMm(03) <> *blank     or
128700090311                                      $SchMm(04) <> *blank     or
128800090311                                      $SchMm(05) <> *blank     or
128900090311                                      $SchMm(06) <> *blank     or
129000090311                                      $SchMm(07) <> *blank     or
129100090311                                      $SchMm(08) <> *blank     or
129200090311                                      $SchMm(09) <> *blank     or
129300090311                                      $SchMm(10) <> *blank     or
129400090311                                      $SchMm(11) <> *blank     or
129500090313                                      $SchMm(12) <> *blank)) ) AND
129600090313                 $ByPass =  *off;
129700090313             $ByPass = *on;
129800090310             errMessage  = *on;
129900090310             errGenerico = *on;
130000090311             PosCurSch01 = *on;
130100090313             V1Dmsg = 'Mesi schedulati in maniera irregolare... +
130200090313                       Premere Enter per forzare.';
130300090310             leavesr;
130400090313           // - Controllo giorno del mese (bypassabile)
130500090313           when ( (V2Cschgg > 28  and  V2Csch02 <> *blank)  or
130600090313                  (V2Cschgg > 30  and (V2Csch04 <> *blank
130700090313                                   or  V2Csch06 <> *blank
130800090313                                   or  V2Csch09 <> *blank
130900090313                                   or  V2Csch11 <> *blank)) )
131000090313                and  $CtrMm = *off;
131100090313             $CtrMm = *on;
131200090313             errMessage  = *on;
131300090313             errGenerico = *on;
131400090313             PosCurSch01 = (V2Csch01 <> *blank);
131500090313             PosCurSch02 = (V2Csch02 <> *blank);
131600090313             PosCurSch04 = (V2Csch04 <> *blank);
131700090313             PosCurSch06 = (V2Csch06 <> *blank);
131800090313             PosCurSch09 = (V2Csch09 <> *blank);
131900090313             PosCurSch11 = (V2Csch11 <> *blank);
132000090313             V1Dmsg = %replace( %char(V2Cschgg) : $Msg(10) :
132100090313                                %scan('&g':$Msg(10)) );
132200090313             leavesr;
132300090313         endsl;
132400090226
132500090309         //?Frequenza addebito?
132600090310         select;
132700090327           // - Obbligatoria
132800090327           when  V2tadu <> 'I'   and   V2tadu <> 'J'   and
132900090327                 V2tadu <> 'S'   and   V2tadu <> 'M';
133000090327             errMessage  = *on;
133100090327             errGenerico = *on;
133200090327             PosCurTadu  = *on;
133300090327             V1Dmsg = 'Frequenza addebito errata o mancante';
133400090327             leavesr;
133500090327           // - Se tipo elaborazione CD la creazione addebito non può
133600090313           //   essere mensile o settimanale
133700090310           when  V2Caut = 'S'  and  (V2tadu = 'S'  or  V2tadu = 'M');
133800090310             errMessage  = *on;
133900090310             errGenerico = *on;
134000090310             PosCurTadu  = *on;
134100090310             V1Dmsg = 'Per creazione CD non è possibile addebito +
134200090310                       mensile o settimanale';
134300090310             leavesr;
134400090327           // - Se codice padre <> da codice cliente
134500090327           //   devono avere la stessa frequenza addebito
134600121213           when  V2Cccm <> V2Cksu   and   p_§LA2tadu <> V2tadu;
134700090310             errMessage  = *on;
134800090310             errGenerico = *on;
134900090310             PosCurTadu  = *on;
135000090310             V1Dmsg = 'Frequenza addebito +
135100090310                        <> da quella del cod. padre ('
135200121213                    + p_§LA2tadu + ')';
135300090310             leavesr;
135400090310         endsl;
135500090309
135600090309         //?Tipo addebito (Creazione TITAS)?
135700090310         select;
135800090327           // - Obbligatorio
135900090327           when  V2Ctad <> 'N'  and  V2Ctad <> 'S'  and  V2Ctad <> 'V';
136000090327             errMessage  = *on;
136100090327             errGenerico = *on;
136200090327             PosCurTad   = *on;
136300090327             V1Dmsg = 'Flag "Creazione TITAS" errato o mancante';
136400090327             leavesr;
136500090327           // - Se "N" non impostare i dati dell'addebito forzato
136600090310           when  V2Ctad = 'N'     and
136700090310                (V2Cksc <> *zero  or  V2Cctr <> *blank
136800090310                                  or  V2imp <> *zero);
136900090310             errMessage  = *on;
137000090310             errGenerico = *on;
137100090310             PosCurTad   = *on;
137200090310             V1Dmsg = 'Se NO addebito non impostare le forzature';
137300090310             leavesr;
137400090327           // - Se codice padre <> da codice cliente
137500090310           //   devono avere lo stesso tipo addebito
137600121213           when  V2Cccm <> V2Cksu   and   p_§LA2tad <> V2Ctad;
137700090310             errMessage  = *on;
137800090310             errGenerico = *on;
137900090310             PosCurTad   = *on;
138000090310             V1Dmsg = 'Tipo addebito +
138100090310                       <> da quello del cod. padre ('
138200121213                        + p_§LA2tad + ')';
138300090310             leavesr;
138400090310         endsl;
138500090226
138600090311         //?Dati da forzare in tassazione:?
138700090312         //?- Codice cliente / Tariffa?
138800090312         clear  TIBS69ds;
138900090312         if  V2Cksc <> *zero;
139000090312           tibs69ds.I69kcc = DUTkci;
139100090312           tibs69ds.I69kac = %int(V2Cksc);
139200090312           tibs69ds.I69sif = knsif;
139300090312           tibs69r(TIBS69ds : ds_CNACO :
139400090312                              ds_CNIND :
139500090312                              ds_CNCLP :
139600090312                              ds_FNCLS);
139700090312         endif;
139800090312         select;
139900090312           when  tibs69ds.O69err = *on;
140000090312             errMessage  = *on;
140100090312             errGenerico = *on;
140200090312             PosCurKsc   = *on;
140300090312             V1Dmsg = 'Cliente tassazione errato';
140400090312             leavesr;
140500090312           when  V2Cksc <> *zero   and   V2Cctr = *blank;
140600090312             errMessage  = *on;
140700090312             errGenerico = *on;
140800090312             PosCurCtr   = *on;
140900090312             V1Dmsg = 'Codice tariffa obbligatorio SE +
141000090312                       inserito il cliente tassazione';
141100090312             leavesr;
141200090311           // - Non ammessa tariffa senza cliente
141300090311           when  V2Cksc =  *zero   and   V2Cctr <> *blank;
141400090311             errMessage  = *on;
141500090311             errGenerico = *on;
141600090311             PosCurKsc   = *on;
141700090311             V1Dmsg = 'Cliente tassazione obbligatorio SE +
141800090311                       inserita la relativa tariffa';
141900090311             leavesr;
142000090311           // - Ammesso solo un codice tariffa numerico
142100090311           when  V2Cctr <> *blank   and
142200090311                 %check(c_Digits : V2Cctr) > *zero;
142300090311             errMessage  = *on;
142400090311             errGenerico = *on;
142500090312             PosCurCtr   = *on;
142600090312             V1Dmsg = 'Ammessi SOLO caratteri numerici';
142700090311             leavesr;
142800090310         endsl;
142900090226
143000090327         // - Se codice padre <> da codice cliente
143100090306         //   stesso ksc/ctr/importo
143200090303         if  V2Cccm <> V2Cksu;
143300090226           select;
143400121213             when  p_§LA2ksc <> V2Cksc;
143500090226               errMessage  = *on;
143600090226               errGenerico = *on;
143700090303               PosCurKsc   = *on;
143800090309               V1Dmsg = 'Cliente tassazione +
143900090309                         <> da quello del cod. padre ('
144000121213                      + %editc(p_§LA2ksc : 'X') + ')';
144100090226               leavesr;
144200121213             when  p_§LA2ctr <> V2Cctr;
144300090226               errMessage  = *on;
144400090226               errGenerico = *on;
144500090303               PosCurCtr   = *on;
144600090309               V1Dmsg = 'Codice tariffa +
144700090309                         <> quello del cod. padre ('
144800121213                      + p_§LA2ctr + ')';
144900090226               leavesr;
145000090226           endsl;
145100090226         endif;
145200090226
145300090309         //?Importo & Tipo importo forzato?
145400090310         select;
145500090310           // - Non ammesso importo per Varia Negata
145600090310           when  V2Ctad = 'V'   and   V2imp <> *zero;
145700090310             errMessage  = *on;
145800090310             errGenerico = *on;
145900090310             PosCurImp   = *on;
146000090310             V1Dmsg = 'NON ammesso l''importo forzato per +
146100090310                       Varia Negata';
146200090310             leavesr;
146300090310           // - Non ammesso tipo importo senza importo
146400090310           when  V2imp =  *zero   and   V2fimp <> *blank;
146500090310             errMessage  = *on;
146600090310             errGenerico = *on;
146700090310             PosCurImp   = *on;
146800090310             V1Dmsg = 'Importo forzato obbligatorio SE +
146900090310                       inserito il tipo di importo';
147000090310             leavesr;
147100090312           // - Se inserito importo devono essere inseriti:
147200090312           //   · il codice cliente forzato (ksc),
147300090312           //   · la tariffa forzata (ctr) ed
147400090312           //   · il tipo importo (fimp)
147500090310           when  V2imp <> *zero;
147600090310             select;
147700090310               when  V2fimp  = *blank;
147800090310                 errMessage  = *on;
147900090310                 errGenerico = *on;
148000090310                 PosCurFimp  = *on;
148100090310                 V1Dmsg = 'Immettere il tipo importo';
148200090310                 leavesr;
148300090310               when  V2Cksc  = *zero;
148400090310                 errMessage  = *on;
148500090310                 errGenerico = *on;
148600090310                 PosCurKsc   = *on;
148700090310                 V1Dmsg = 'Immettere il cliente forzato';
148800090310                 leavesr;
148900090312               when  V2Cctr  = *blank;
149000090310                 errMessage  = *on;
149100090310                 errGenerico = *on;
149200090310                 PosCurCtr   = *on;
149300090310                 V1Dmsg = 'Immettere la tariffa forzata';
149400090310                 leavesr;
149500090310             endsl;
149600090310         endsl;
149700090312
149800090327         // Verifica compatibilità dell'importo forzato con il
149900090312         //   massimo imponibile IVA in tab. "GEI"/"LAC".
150000090312         if  V2imp <> *zero   and   V2fimp = 'T';
150100090312           // - Reperimento divisa moneta di conto gestionale
150200090312           clear  dGED;
150300090312           reset  TIBS02ds;
150400090312           T02cod = 'GED';
150500090312           T02ke1 = '1';
150600090312           TNTBE_RicercaControllo(kpjba : tibs02ds);
150700090312           if  T02err = *blank;
150800090312             dGED = T02uni;
150900090312           endif;
151000090312           // - Reperimento importi da tab. "GEI"
151100090312           //   (interessa il massimo imponibile IVA per spedizione)
151200090312           clear  dGEI;
151300090312           reset  TIBS02ds;
151400090312           T02cod = 'GEI';
151500090312           T02ke1 = §GEDcn;
151600090312           TNTBE_RicercaControllo(kpjba : tibs02ds);
151700090312           if  T02err = *blank;
151800090312             dGEI = T02uni;
151900090312           endif;
152000090312           if  V2imp > §GEImf;
152100090312             errMessage  = *on;
152200090312             errGenerico = *on;
152300090312             PosCurImp   = *on;
152400090312             V1Dmsg = 'Importo superiore al MASSIMO IMPONIBILE IVA +
152500090312                       PER SPEDIZIONE ('
152600090312                      + %trim( %editc( §GEImf : 'J' ) ) + ')';
152700090312             leavesr;
152800090312           endif;
152900090312         endif;
153000090226
153100090312         // Se codice padre <> da codice cliente
153200090306         //   stesso importo e tipo importo
153300090303         if  V2Cccm <> V2Cksu;
153400090226           select;
153500121213             when  p_§LA2imp <> V2imp;
153600090226               errMessage  = *on;
153700090226               errGenerico = *on;
153800090312               PosCurImp   = *on;
153900090309               V1Dmsg = 'Importo forzato +
154000090309                         <> da quello del cod. padre ('
154100121213                      + %editc(p_§LA2imp : 'J') + ')';
154200090226               leavesr;
154300121213             when  p_§LA2fimp <> V2fimp;
154400090226               errMessage  = *on;
154500090226               errGenerico = *on;
154600090312               PosCurFimp  = *on;
154700090309               V1Dmsg = 'Tipo importo forzato +
154800090309                         <> da quello del cod. padre ('
154900121213                      + p_§LA2fimp + ')';
155000090226               leavesr;
155100090226           endsl;
155200090225         endif;
155300090923
155400090923         //?Modifica dati sensibili per l'addebito
155500090923         if (v2tadu <> savtadu or v2ctad <> savtad or
155600090923             v2cksc <> savksc or v2cctr <> savctr or
155700090923             v2imp <> savimp or v2fimp <> savfimp) and not *in02
155800090923             and not $forza;
155900090923               errMessage  = *on;
156000090923               errGenerico = *on;
156100090923               PosCurTadu  = *on;
156200090923               V1Dmsg = 'ATTENZIONE!! Possibili problemi ' +
156300090923                        'in fase di addebito. Avvisare Monica/Nicola';
156400090923               $forza = *on;
156500090923               leavesr;
156600090923         endif;
156700110713
156800110713         //?Creazione file elenco per il cliente
156900110713         //?possibile solo se NON è elaborazione automatica (FTP)
157000110713         IF  V2pgm <> *blanks and V2caut = 'A';
157100110713           ErrMessage  = *on;
157200110713           ErrGenerico = *on;
157300110713           PosCurPgm   = *on;
157400110713           V1Dmsg = 'NON possibile creazione elenco se elaborazione '+
157500110713                    'automatica';
157600110713           leavesr;
157700110713         ENDIF;
157800110713         //?se inserito il nome del programma deve esserci anche il tipo elenco
157900110713         IF  V2pgm <> *blanks and V2ele = *blanks;
158000110713           ErrMessage  = *on;
158100110713           ErrGenerico = *on;
158200110713           PosCurEle   = *on;
158300110713           V1Dmsg = 'Immettere anche il tipo elenco da creare';
158400110713           leavesr;
158500110713         ENDIF;
158600110713         //?e viceversa se c'è il tipo elenco deve esserci anche il nome del pgm
158700110713         IF  V2pgm = *blanks and V2ele <> *blanks;
158800110713           ErrMessage  = *on;
158900110713           ErrGenerico = *on;
159000110713           PosCurPgm   = *on;
159100110713           V1Dmsg = 'Immettere anche il nome del programma x elenco da creare';
159200110713           leavesr;
159300110713         ENDIF;
159400110713         //?Se codice padre <> da codice cliente
159500110713         //?  devono avere gli stessi dati
159600110713         IF  V2Cccm <> V2Cksu;
159700110713           SELECT;
159800121213             WHEN  p_§LA2pgm <> V2pgm;
159900110713               ErrMessage  = *on;
160000110713               ErrGenerico = *on;
160100110713               PosCurPgm   = *on;
160200110713               V1Dmsg = 'Nome programma +
160300110713                         <> da quello del cod. padre ('
160400121213                      + p_§LA2pgm + ')';
160500110713               leavesr;
160600121213             WHEN  p_§LA2ele <> V2ele;
160700110713               ErrMessage  = *on;
160800110713               ErrGenerico = *on;
160900110713               PosCurEle   = *on;
161000110713               V1Dmsg = 'Tipo elenco +
161100110713                         <> da quello del cod. padre ('
161200121213                      + p_§LA2ele + ')';
161300110713               leavesr;
161400110713           ENDSL;
161500110713         ENDIF;
161600071207
161700071207       ENDSR;
161800090225
161900090225       //--------------------------------------------------------------
162000121213       //?Controllo esistenza del codice padre in tabella LA2          ?
162100090225       //--------------------------------------------------------------
162200090303       BEGSR  sr_Padre;
162300090302
162400121213         // Controlla esistenza in tabella "LA2"
162500121213         clear  dLA2p;
162600090310         reset  TIBS02ds;
162700090310         T02sif = KNSIF;
162800090310         %subst(T02ke1 : 1 : 7) = %editc(V2Cksu : 'X');
162900090310         TNTBE_RicercaControllo(kpjba : tibs02ds);
163000090310         if  T02err = *blank;
163100121213           dLA2p  = T02uni;
163200090310         else;
163300090310           errMessage  = *on;
163400090310           errGenerico = *on;
163500090310           PosCurksu   = *on;
163600090310           V1Dmsg = $Msg(12);
163700090310           leavesr;
163800090310         endif;
163900090327
164000090327         // Controlla se già inserito come figlio di altro padre
164100121213         if  p_§LA2ksu <> V2Cksu;
164200090327           errMessage  = *on;
164300090327           errGenerico = *on;
164400090327           PosCurKsu   = *on;
164500090327           V1Dmsg = 'Non ammesso cod. PADRE già FIGLIO di altro +
164600121213                     cliente (' + %editc(p_§LA2ksu : 'X') + ')';
164700090327           leavesr;
164800090327         endif;
164900090225
165000090225       ENDSR;
165100071206
165200071212       //--------------------------------------------------------------
165300121213       //?Aggiornamento record TNTBE00F (tab. LA2)                     ?
165400071212       //--------------------------------------------------------------
165500090306       BEGSR  Upd_TNTBE;
165600071207
165700121213         clear  dLA2;
165800121213         §LA2rag   = V2Dccm;
165900121213         §LA2tpt   = V2Ctpt;
166000121213         §LA2res   = V2Cres;
166100121213         §LA2rec   = V2Crec;
166200121213         §LA2csr   = V2Ccsr;
166300121213         §LA2ssr   = V2Cssr;
166400121213         §LA2dir   = V2Cdir;
166500121213         §LA2tpi   = V2Ctpi;
166600121213         §LA2fmi   = V2Cfmi;
166700121213         §LA2aut   = V2Caut;
166800121213         §LA2tdta  = V2tdta;
166900121213         §LA2tad   = V2Ctad;
167000121213         §LA2schgg = V2Cschgg;
167100121213         §LA2sch01 = V2Csch01;
167200121213         §LA2sch02 = V2Csch02;
167300121213         §LA2sch03 = V2Csch03;
167400121213         §LA2sch04 = V2Csch04;
167500121213         §LA2sch05 = V2Csch05;
167600121213         §LA2sch06 = V2Csch06;
167700121213         §LA2sch07 = V2Csch07;
167800121213         §LA2sch08 = V2Csch08;
167900121213         §LA2sch09 = V2Csch09;
168000121213         §LA2sch10 = V2Csch10;
168100121213         §LA2sch11 = V2Csch11;
168200121213         §LA2sch12 = V2Csch12;
168300121213         §LA2note  = V2note;
168400121213         §LA2lnp   = V2Clnp;
168500121213         §LA2ksu   = V2Cksu;
168600121213         §LA2mesi  = V2mesi;
168700121213         §LA2datai = (%int( %subst( %editc( V2datai : 'X') : 3 : 4 ) )
168800090313                      * 100)
168900090313                   + %int( %subst( %editc( V2datai : 'X') : 1 : 2 ) );
169000121213         §LA2datela...
169100090313                   = (%int( %subst( %editc( V2datela : 'X') : 3 : 4 ) )
169200090319                      * 100)
169300090313                    + %int( %subst( %editc( V2datela : 'X') : 1 : 2 ) );
169400121213         §LA2tadu  = V2tadu;
169500121213         §LA2ksc   = V2Cksc;
169600121213         §LA2ctr   = V2Cctr;
169700121213         §LA2fimp  = V2fimp;
169800121213         §LA2imp   = V2imp;
169900121213         §LA2pgm   = V2pgm;
170000121213         §LA2ele   = V2ele;
170100071207
170200121213         TBEuni    = dLA2;
170300071207
170400090306         clear  TBEftt;
170500090306         clear  TBEftr;
170600071207
170700071207         select;
170800090306           // F5=Ripristino
170900090306           when  dsp_aid = c_F05;
171000090306             clear  TBEatb;
171100090306           // F16=Annullamento
171200090306           when  dsp_aid = c_F16;
171300071207             TBEatb = 'A';
171400071207         endsl;
171500121220
171600121220         TBEdtr = woggi;
171700071207
171800071207         IF  NOT  %found(TNTBE01L);
171900090306           clear  TBEatb;
172000090306           clear  TBEflt;
172100090306           clear  TBEftr;
172200090306           //clear  TBEdtr;
172300090306           TBEsif = TBXsif;
172400090306           TBElin = TBXlin;
172500090306           TBEapl = TBXapl;
172600090306           TBEcod = k_TBEcod;
172700090306           TBEke1 = k_TBEke1;
172800090306           TBEke2 = k_TBEke2;
172900090306           //_____________
173000071207           WRITE TNTBE000;
173100090306           //¯¯¯¯¯¯¯¯¯¯¯¯¯
173200071207         ELSE;
173300090306           //______________
173400071207           UPDATE TNTBE000;
173500090306           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
173600071207         ENDIF;
173700071207
173800071207       ENDSR;
173900071207
174000071212       //--------------------------------------------------------------
174100090306       //?Operazioni finali.                                           ?
174200071212       //--------------------------------------------------------------
174300090306       BEGSR  sr_RoutEnd;
174400071207
174500090306         clear  TIBS69ds;
174600090304         tibs69ds.I69tla = 'C';
174700090304         tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
174800071207
174900121213         kpjbu = TNTB464ds;
175000071207         return;
175100071207
175200071207       ENDSR;
175300090306
175400071207      /end-free
175500071207
175600071212       //--------------------------------------------------------------
175700090306       //?Schiere a tempo di compilazione.                             ?
175800071212       //--------------------------------------------------------------
175900071207
176000071207** - $MSG -------------------------------------------------------------------*
176100071207Immettere il codice cliente                                                     1
176200071207Immettere SOLO caratteri numerici                                               2
176300071207Codice errato                                                                   3
176400071207Cliente annullato                                                               4
176500071207Immettere la directory                                                          5
176600071207Carattere "\" non valido; per indicare una sottocartella utilizzare "/"         6
176700071210Caratteri & e %  non ammessi per la directory di clienti con invio automatico   7
176800071207Indicare il giorno del mese se specificato almeno un mese                       8
176900071207Specificare almeno un mese se indicato il giorno del mese                       9
177000071207Per i mesi con n° di giorni < &g verrà assunto il loro ultimo giorno -> Enter  10
177100090225Inserire il codice padre                                                       11
177200090225Il codice padre deve essere = al codice cliente o = a cliente già in tabella   12
177300121213Cliente da copiare NON più reperibile in tab. "LA2"                            13
177400121213Cliente già inserito in tab. "LA2"                                             14
