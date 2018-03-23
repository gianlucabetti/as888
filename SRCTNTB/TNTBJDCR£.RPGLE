000100120605       //---------------------------------------------------------------
000200120605       //?GESTIONE TABELLA "JDC" - Jdoc: clienti per ritorno documenti
000300120605       //---------------------------------------------------------------
000400110804
000500110804      *?  ATTENZIONE!!  ?
000600110804      *?    Questa tabella è utilizzata anche dal pgm TNTA61R  ?
000700110804      *?    'Interrogazione abilitazioni clienti'              ?
000800110804      *?    In caso di aggiunta/modifica campi alla tabella    ?
000900110804      *?    verificare anche relativo pgm di interrogazione    ?
001000120604      *?    per le abilitazioni clienti --> TNTBJDC2R.         ?
001100090306
001200071206     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001300071206
001400090306       //--------------------------------------------------------------
001500090306       //?Dichiarazione file.                                          ?
001600090306       //--------------------------------------------------------------
001700090306
001800090311     fAZORG01L  if   e           k disk
001900090311
002000071206     fTNTBE01L  Uf A e           k disk
002100090311
002200120604     fTNTBJDCD  cf   e             workstn
002300071207     f                                     indds(IndDspF)
002400090306     f                                     infds(InfDspF)
002500071207
002600090306       //--------------------------------------------------------------
002700090306       //?Definizione costanti.                                        ?
002800090306       //--------------------------------------------------------------
002900090306
003000090306       // - Tabella in gestione
003100120604     d c_Tab           c                   const('JDC')
003200090306
003300090306       // - Costante per controllo "caratteri solo numerici"
003400090310     d c_Digits        c                   const('0123456789')
003500090306
003600090306       // - Tasti funzionali a video
003700090306     d c_F01           c                   const(x'31')
003800090306     d c_F02           c                   const(x'32')
003900090306     d c_F03           c                   const(x'33')
004000090319     d c_F04           c                   const(x'34')
004100090306     d c_F05           c                   const(x'35')
004200090306     d c_F06           c                   const(x'36')
004300090306     d c_F07           c                   const(x'37')
004400090306     d c_F08           c                   const(x'38')
004500090306     d c_F09           c                   const(x'39')
004600090306     d c_F10           c                   const(x'3A')
004700110713     d c_F11           c                   const(x'3B')
004800090306     d c_F12           c                   const(x'3C')
004900090306     d c_F13           c                   const(x'B1')
005000090306     d c_F14           c                   const(x'B2')
005100090306     d c_F15           c                   const(x'B3')
005200090306     d c_F16           c                   const(x'B4')
005300090306     d c_F17           c                   const(x'B5')
005400090306     d c_F18           c                   const(x'B6')
005500090306     d c_F19           c                   const(x'B7')
005600090306     d c_F20           c                   const(x'B8')
005700090306     d c_F21           c                   const(x'B9')
005800090306     d c_F22           c                   const(x'BA')
005900090306     d c_F23           c                   const(x'BB')
006000090306     d c_F24           c                   const(x'BC')
006100090306     d c_Enter         c                   const(x'F1')
006200090306     d c_RollDown      c                   const(x'F4')
006300090306     d c_RollUp        c                   const(x'F5')
006400071207
006500090306       //--------------------------------------------------------------
006600090306       //?Definizione schiere.                                         ?
006700090306       //--------------------------------------------------------------
006800090306
006900090306       // - Messaggi a video
007000120608     d $Msg            s             78    dim(30)  ctdata  perrcd(1)
007100071207
007200090306       //--------------------------------------------------------------
007300090306       //?Definizione aree dati.                                       ?
007400090306       //--------------------------------------------------------------
007500090306
007600090306       // - Dati utente
007700071207     d §AzUte        e ds                  extname(AZUTE00F)
007800071207     d                                     dtaara
007900071207     d §DatiUte      e ds                  extname(dDatiUte)
008000071207     d                                     dtaara
008100071207
008200090306       //--------------------------------------------------------------
008300090306       //?Definizione strutture dati.                                  ?
008400090306       //--------------------------------------------------------------
008500090306
008600090306       // - Status
008700071207     d Psds           sds
008800071207     d   SDSpgm          *proc
008900071207
009000090306       // - InfDS
009100071207     d InfDspF         ds
009200071207     d  dsp_aid              369    369a                                        AID byte
009300071207
009400090306       // - Indicatori su DspF
009500090311     d IndDspF         ds
009600090306         // - Abilitazione tasti funzionali
009700090323     d  F4Attivo                       n   overlay(IndDspF:04)
009800090311     d  F5Attivo                       n   overlay(IndDspF:05)
009900090311     d  F6Attivo                       n   overlay(IndDspF:06)
010000090311     d  F16Attivo                      n   overlay(IndDspF:16)
010100090306         // - Emissione messaggio di errore
010200090311     d  ErrMessage                     n   overlay(IndDspF:28)
010300090306         // - Protezione campi
010400120112     d  InserCCM2                      n   overlay(IndDspF:41)
010500090306         // - Posizionamento cursore & segnalazione errore
010600090311     d  PosCurCcm                      n   overlay(IndDspF:50)
010700090311     d  PosCurAut                      n   overlay(IndDspF:51)
010800090311     d  PosCurTpi                      n   overlay(IndDspF:52)
010900090311     d  PosCurDir                      n   overlay(IndDspF:53)
011000120606     d  PosCurPag                      n   overlay(IndDspF:54)
011100120608     d  PosCurDti                      n   overlay(IndDspF:55)
011200120608     d  PosCurDtf                      n   overlay(IndDspF:56)
011300090311     d  PosCurKsu                      n   overlay(IndDspF:61)
011400120604     d  PosCurFmi                      n   overlay(IndDspF:70)
011500120112     d  PosCurCCM2                     n   overlay(IndDspF:76)
011600090306         //   - Riemissione videata
011700090311     d  ErrGenerico                    n   overlay(IndDspF:99)
011800071207
011900090306       // - Parametri ricevuti
012000071206     d KPJBA         e ds
012100120604     d TNTBJDCds     e ds                  inz
012200071207
012300090306       // - Reperimento dati utente
012400071207     d TIBS34ds      e ds
012500071207
012600090306       // - Controllo/Decodifica cliente
012700090304     d TIBS69ds      e ds                  qualified  inz
012800090304     d ds_CNACO      e ds                  extname(CNACO00F)
012900090304     d                                     qualified  inz
013000090304     d ds_CNIND      e ds                  extname(CNIND00F)
013100090304     d                                     qualified  inz
013200090304     d ds_CNCLP      e ds                  extname(CNCLP00F)
013300090304     d                                     qualified  inz
013400090304     d ds_FNCLS      e ds                  extname(FNCLS00F)
013500090304     d                                     qualified  inz
013600090306
013700090306       // - Dati record principale della tabella
013800090306     d TNTBEds       e ds                  inz  extname(TNTBE00F)
013900090306     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
014000090306     d                                          prefix(TBX:3)
014100090312
014200090312       // - Parametri per Ricerca/controllo tabelle
014300090312     d TIBS02ds      e ds                  inz
014400090312     d  T02mod       e                     inz('C')
014500090312     d  T02cod       e                     inz(c_Tab)
014600071207
014700120605       // - Tabella "JDC" = Clienti per ritorno documenti
014800120604     d dJDC          e ds                  inz
014900120604       // - Tabella "JDC" del padre per controllo legami
015000120604     d dJDCp         e ds                  extname(dJDC) inz
015100090225     d                                     prefix(p_)
015200121016
015300121016       // - Tabella "LAC" = Clienti per ritorno LDV
015400121016     d dLAC          e ds                  inz
015500120604
015600090319       // - Struttura per passaggio dati ad interrogazione tabella
015700090327     d Param01         ds                  inz
015800090327     d  P01cod                        7  0 inz
015900090327     d  P01ord                        1    inz
016000090327     d  P01ksu                        7  0 inz
016100090327     d  P01ke1                        7    inz
016200090327     d  P01ke2                       15    inz
016300090327     d  P01rit                        1    inz
016400120608
016500120608     d wlbdat          ds                  inz
016600120608     d  g02dat                 1      8  0
016700120608     d  g02inv                 9     16  0
016800120608     d  g02err                17     17
016900120608     d  g02tgi                18     22  0
017000071207
017100090306       //--------------------------------------------------------------
017200090306       //?Definizione procedure usate.                                 ?
017300090306       //--------------------------------------------------------------
017400090306
017500090306       // - Reperimento dati utente
017600090306      /copy gaitrasrc/srcProtoPR,TIBS34R
017700071207
017800090304       // - Ricerca/Controllo tabelle
017900090306      /copy gaitrasrc/srcProtoPR,TIBS02R
018000071207
018100090304       // - Controllo/Decodifica cliente
018200090306      /copy gaitrasrc/srcProtoPR,TIBS69R
018300090319
018400120604       // - Interrogazione tabella JDC
018500120604     d tntbJDC1r       pr                  extpgm('TNTBJDC1R')
018600090319     d  kpjba                              likeds(KPJBA)
018700120112     d  p_SNopz3                      1a   const  options(*nopass)
018800120608
018900120608       // - Controllo data
019000120608      /copy gaitrasrc/srcprotopr,xsrda8
019100071207
019200090306       //--------------------------------------------------------------
019300090306       //?Definizione variabili globali.                               ?
019400090306       //--------------------------------------------------------------
019500090306
019600090306       // - Flags booleani
019700090306     d $InzD01         s               n   inz(*on)
019800090306     d $InzD02         s               n   inz(*on)
019900090306     d $Fine           s               n   inz
020000090923     d $forza          s               n   inz
020100090313     d $ByPass         s               n   inz
020200120112     d $Copia          s               n   inz
020300121016     d wForza          s               n   inz
020400121030     d wForzaTPI       s               n   inz
020500121030     d wForzaFMI       s               n   inz
020600121030     d wForzaDIR       s               n   inz
020700071207
020800090306       // - Gestione video
020900071206     d $Video          s              2    inz('D1')
021000090313
021100090313       // - Indici di schiera
021200090313     d xx              s              3  0 inz
021300071207
021400090313       // - Campi di comodo
021500090306     d wPos1           s              2  0 inz
021600090306     d wPos2           s              2  0 inz
021700090316     d SAVke1          s                   like(TBEke1)    inz
021800120608     d wdatai          s              8  0 inz
021900120608     d wdataf          s              8  0 inz
022000120608     d data_eur        s               d   Datfmt(*eur)
022100120608     d data_iso        s               d   Datfmt(*iso)
022200071207
022300090306       //--------------------------------------------------------------
022400090306       //?Definizione key-list.                                        ?
022500090306       //--------------------------------------------------------------
022600090306
022700090306       // - File TNTBE01L
022800090306     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
022900090306     d                                     prefix(k_)  inz
023000071206
023100090306       //--------------------------------------------------------------
023200090306       //?Riepilogo indicatori.                                        ?
023300090306       //--------------------------------------------------------------
023400090306       // 01    - RECORD annullato
023500090306       // 02    - acceso IMMISSIONE - spento MANUTENZIONE
023600090306       // 25    - Comodo
023700090306       // 28    - Emissione del messaggio di errore a video
023800090306       // 40    - Protezione campi in prima estrazione
023900090306       // x D01:
024000090306       // 50    - Codice cliente errato o mancante
024100090306       // x D02:
024200120112       // 41    - Copia in D02: V2CCCM non protetto
024300120605       // 53    - Directory per documenti      errata
024400090306       // 61    - Codice Padre                 errato
024500120604       // 63    - Tipo data di elaborazione    errato
024600120605       // 70    - Flag nome documento          errato
024700090306       // 99    - Generico di errore
024800090306       //--------------------------------------------------------------
024900071206
025000071206     c     *Entry        plist
025100071206     c                   parm                    KPJBA
025200071207
025300071207      /free
025400071207
025500071207       // Operazioni iniziali
025600090306       exsr  sr_RoutInz;
025700071207
025800071207       // Gestione video
025900090306       DOW  $Fine = *off;
026000071207         select;
026100090306           when  $Video = 'D1';
026200090306             exsr  sr_GesD01;
026300090306           when  $Video = 'D2';
026400090306             exsr  sr_GesD02;
026500101103           when  $Video = 'W1';
026600101103             exsr  sr_GesW01;
026700071207           other;
026800071207             leave;
026900071207         endsl;
027000071207       ENDDO;
027100071207
027200071207       // Operazioni finali
027300090306       exsr  sr_RoutEnd;
027400071206
027500071212       //--------------------------------------------------------------
027600090306       //?Operazioni iniziali.                                         ?
027700071212       //--------------------------------------------------------------
027800090306       BEGSR  sr_RoutInz;
027900090306
028000090306         *inLR = *on;
028100071207
028200090306         if  kpjbu <> *blank;
028300120604           TNTBJDCds = kpjbu;
028400071207         else;
028500120604           clear  TNTBJDCds;
028600071207         endif;
028700120604         BJDCfxx = *blank;
028800120604         BJDCerr = *off;
028900120604         BJDCmsg = *blank;
029000071207
029100090306         // Reperimento dati job
029200090306         exsr  DatiJob;
029300071207
029400090306         // Impostazione nome programma a video
029500071207         V1Tpgm = SDSpgm;
029600090306
029700090306         // Aggancio dati generali della tabella in esame
029800090306         clear  k_TBEcod;
029900090306         k_TBEke1 = *zero;
030000090306         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
030100090306                = c_Tab;
030200090306         clear  k_TBEke2;
030300090306         clear  k_TBElin;
030400090306         k_TBEsif = KNSIF;
030500090306         chain(n)  %kds(k05tntbe01)  TNTBE000;
030600090306         if  not  %found(TNTBE01L);
030700090306           clear  k_TBEsif;
030800090306           chain(n)  %kds(k05tntbe01)  TNTBE000;
030900090306         endif;
031000090306         if  %found(TNTBE01L);
031100090306           xTNTBEds = TNTBEds;
031200090306         else;
031300090306           clear  xTNTBEds;
031400090306         endif;
031500071207
031600090306         // Verifica parametri ricecvuti
031700120604         if  BJDCke1  <> *blank;
031800090306           exsr  sr_InzD01;
031900090306           exsr  sr_CtrD01;
032000090306           if  errMessage = *on;
032100120604             BJDCerr = *on;
032200120604             BJDCmsg = V1Dmsg;
032300071217             $Fine  = *on;
032400071217             leavesr;
032500071217           endif;
032600071207           $Video  = 'D2';
032700071207           $InzD02 = *on;
032800071207         endif;
032900071207
033000071207       ENDSR;
033100071206
033200071212       //--------------------------------------------------------------
033300090306       //?Reperimento Dati del job (Utente/Operativi).                 ?
033400071212       //--------------------------------------------------------------
033500090306       BEGSR  DatiJob;
033600071207
033700071207         in(E) §AzUte;
033800071207         if NOT %error;
033900071207           in(E) §DatiUte;
034000071207         endif;
034100090306         if %error or RSut = *blank;
034200071207           clear TIBS34ds;
034300071207           tibs34r(tibs34ds);
034400071207           in §AzUte;
034500071207           in §DatiUte;
034600071207         endif;
034700071207
034800071207       ENDSR;
034900071206
035000071212       //--------------------------------------------------------------
035100090306       //?Gestione videata D01                                         ?
035200071212       //--------------------------------------------------------------
035300090306       BEGSR  sr_GesD01;
035400071207
035500090306         // Inizializzazione videata
035600090306         if  $InzD01 = *on;
035700090306           exsr  sr_InzD01;
035800071207           $InzD01 = *off;
035900071207         endif;
036000071207
036100090306         // Emissione testata
036200120604         write  TBJDCT01;
036300071207
036400090306         // Emissione videata
036500120604         exfmt  TBJDCD01;
036600071207         errMessage  = *off;
036700071207         errGenerico = *off;
036800090306         clear  V1Dmsg;
036900071207
037000071207         select;
037100090306           // F3=Fine
037200090306           when  dsp_aid = c_F03;
037300090306             exsr  sr_F03D01;
037400090306           // Enter
037500071207           other;
037600090306             exsr  sr_CtrD01;
037700090306             if  errGenerico;
037800071207               leavesr;
037900071207             endif;
038000071207             $Video  = 'D2';
038100071207             $InzD02 = *on;
038200071207         endsl;
038300071207
038400071207       ENDSR;
038500071206
038600071212       //--------------------------------------------------------------
038700090306       //?Inizializzazione videata D01                                 ?
038800071212       //--------------------------------------------------------------
038900090306       BEGSR  sr_InzD01;
039000071207
039100120604         clear  TBJDCD01;
039200071207
039300120604         if  BJDCke1 = *blank;
039400090306           %subst(V1Cccm : %len(V1Cccm) : 1) = '?';
039500071207         else;
039600120604           V1Cccm = %trimr(BJDCke1);
039700071207         endif;
039800071207
039900071207       ENDSR;
040000071206
040100071212       //--------------------------------------------------------------
040200090306       //?Controllo videata D01                                        ?
040300071212       //--------------------------------------------------------------
040400090306       BEGSR  sr_CtrD01;
040500071207
040600090306         IndDspF = *zero;
040700120112         reset  $Copia;
040800071207
040900071207         SELECT;
041000071207
041100120112           // - Ricerca codice cliente
041200090306           WHEN  %scan('?' : V1Cccm) > *zero;
041300120112             clear  Param01;
041400090327             P01ord = '0';
041500090327             KPJBU  = Param01;
041600120604             tntbjdc1r (KPJBA : '3');
041700090327             Param01 = KPJBU;
041800090319
041900090327             if  P01ke1 <> *zero;
042000090327               V1Cccm = P01ke1;
042100090319             endif;
042200120112             if  P01rit <> '3';
042300120112               errGenerico = *on;
042400120112               PosCurCcm   = *on;
042500120112               leavesr;
042600120112             else;
042700120112               $Copia = *on;
042800120112             endif;
042900071207
043000120112           // - Controllo immissione codice cliente
043100090306           WHEN  V1Cccm = *blank  or  V1Cccm  = *zero;
043200071207             errMessage  = *on;
043300071207             errGenerico = *on;
043400071207             PosCurCcm   = *on;
043500071207             V1Dmsg = $Msg(01);
043600071207             leavesr;
043700071207
043800120112           // - Controllo immissione solo caratteri numerici
043900090310           WHEN  %check(c_Digits : V1Cccm) > *zero;
044000071207             errMessage  = *on;
044100071207             errGenerico = *on;
044200071207             PosCurCcm   = *on;
044300071207             V1Dmsg = $Msg(02);
044400071207             leavesr;
044500071207
044600071207         ENDSL;
044700071207
044800090306         // - Controllo esistenza codice cliente in anagrafica clienti
044900120112         //   (SE non richiesta copia: il vero codice cliente verrà
045000120112         //    inserito in D02)
045100120112         If  Not  $Copia;
045200120112           clear  TIBS69ds;
045300120112           tibs69ds.I69kcc = DUTkci;
045400120112           tibs69ds.I69kac = %int(V1Cccm);
045500120112           tibs69ds.I69sif = knsif;
045600120112           tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
045700120112           if  tibs69ds.O69err = *on;
045800120112             errMessage  = *on;
045900120112             errGenerico = *on;
046000120112             PosCurCcm   = *on;
046100120112             V1Dmsg = $Msg(03);
046200120112             leavesr;
046300120112           endif;
046400120112         EndIf;
046500071207
046600090306         // - Verifica esistenza codice cliente in tabella
046700090306         reset  TNTBE000;
046800090310         k_TBEcod = c_Tab;
046900090306         k_TBEke1 = V1Cccm;
047000090306         clear  k_TBEke2;
047100090306         k_TBElin = TBXlin;
047200090306         k_TBEsif = TBXsif;
047300120112
047400120112         If  Not  $Copia;
047500120112
047600120112           chain  %kds(K05tntbe01) TNTBE000;
047700120112           // - - NON ammesso nuovo record per cliente annullato
047800120112           if  NOT  %found(TNTBE01L)  and  ds_CNACO.ACOflg <> *blank;
047900120112             errMessage  = *on;
048000120112             errGenerico = *on;
048100120112             PosCurCcm   = *on;
048200120112             V1Dmsg = $Msg(04);
048300120112             leavesr;
048400120112           endif;
048500120112
048600120112         Else;
048700120112
048800120112           chain(N)  %kds(K05tntbe01) TNTBE000;
048900120112           // - - NON ammessa copia di record inesistente
049000120112           if  NOT  %found(TNTBE01L);
049100120112             errMessage  = *on;
049200120112             errGenerico = *on;
049300120112             PosCurCcm   = *on;
049400120604             V1Dmsg = $Msg(10);
049500120112             leavesr;
049600120112           endif;
049700120112
049800120112         EndIf;
049900071207
050000120112         // - Decodifica cliente
050100090306         if  %found(TNTBE01L);
050200120604           dJDC = TBEuni;
050300120112           if  Not  $Copia;
050400120604             V1Dccm = §JDCrag;
050500120112           else;
050600120112             V1Dccm = %subst(ds_CNACO.ACOrag : 1 : %len(V1Dccm));
050700120112           endif;
050800071207         else;
050900120604           clear  dJDC;
051000090304           V1Dccm = %subst(ds_CNACO.ACOrag : 1 : %len(V1Dccm));
051100071207         endif;
051200071207
051300071207       ENDSR;
051400071206
051500071212       //--------------------------------------------------------------
051600090306       //?Gestione tasto funzionale F3 da videata D01                  ?
051700090306       //?F3=Fine                                                      ?
051800071212       //--------------------------------------------------------------
051900090306       BEGSR  sr_F03D01;
052000071207
052100090306         // Chiusura del programma
052200071207         $Fine = *on;
052300071217
052400090306         // Restituzione indicazione del tasto funzionale premuto
052500120604         BJDCfxx = '1';
052600071207
052700071207       ENDSR;
052800071206
052900071212       //--------------------------------------------------------------
053000090306       //?Gestione videata D02                                         ?
053100071212       //--------------------------------------------------------------
053200090306       BEGSR  sr_GesD02;
053300071207
053400090306         // Inizializzazione videata
053500090306         if   $InzD02 = *on;
053600090306           exsr  sr_InzD02;
053700090306           $InzD02 = *off;
053800071207         endif;
053900071207
054000090306         // Emissione testata
054100120604         write  TBJDCT01;
054200071207
054300090306         // Emissione videata
054400151217         if  BJDCopz <> '5';
054500120604           exfmt  TBJDCD02;
054600071217         else;
054700120604           write  TBJDCD02;
054800090306           exfmt  PROTECT;
054900071217         endif;
055000071207         errMessage  = *off;
055100071207         errGenerico = *off;
055200090306         clear  V1Dmsg;
055300071207
055400071207         SELECT;
055500090306           // F3=Fine
055600090306           WHEN  dsp_aid = c_F03;
055700090306             exsr  sr_F03D01;
055800090319           // F04=Interrogazione Padre
055900090319           WHEN  dsp_aid = c_F04;
056000090319             exsr  sr_F04D02;
056100090306           // F12=Ritorno
056200090306           WHEN  dsp_aid = c_F12;
056300090306             exsr  sr_F12D02;
056400090306           // Enter o F5 o F6 o F16
056500071207           OTHER;
056600090306             // - Controllo dati
056700090306             if  dsp_aid = c_F16;
056800090306               exsr  sr_CtrAnnull;
056900090306             else;
057000090306               exsr  sr_CtrD02;
057100090306             endif;
057200090306             if  errGenerico;
057300071207               leavesr;
057400071207             endif;
057500090306             // - Conferma dati:
057600090306             //   F5=Ripristino; F6=Conferma; F16=Annullamento
057700090306             if  dsp_aid = c_F05  or
057800090306                 dsp_aid = c_F06  or
057900090306                 dsp_aid = c_F16;
058000090306               exsr  Upd_TNTBE;
058100071207               if  NOT  errGenerico;
058200071207                 clear V1Tlav;
058300071207                 $Video  = 'D1';
058400090306                 //$InzD01 = *on;
058500071207               endif;
058600071207             EndIf;
058700071207
058800071207         ENDSL;
058900071207
059000071207       ENDSR;
059100071206
059200071212       //--------------------------------------------------------------
059300090306       //?Inizializzazione videata D01                                 ?
059400071212       //--------------------------------------------------------------
059500090306       BEGSR  sr_InzD02;
059600071207
059700090313         reset  $ByPass;
059800121016         reset  wForza;
059900121030         reset  wForzaTPI;
060000121030         reset  wForzaFMI;
060100121030         reset  wForzaDIR;
060200071207
060300120604         clear  TBJDCW01;
060400101103
060500120604         clear  TBJDCD02;
060600071210
060700090306         IndDspF = *zero;
060800090309
060900090309         // Impostazione indicatori per abilitazione tasti funzionali
061000090309         exsr  Set_Ind_D02;
061100090309
061200120112         // - Codice estrazione (chiave)
061300120112         if  Not  $Copia;
061400120112           V2Cccm = %int(V1Cccm);
061500120112           V2Dccm = V1Dccm;
061600120112         endif;
061700071207
061800090306         IF  %found(TNTBE01L);
061900071207
062000090306           // Impostazione dati di record trovato
062100071217           select;
062200120604             when  BJDCopz = '5';
062300071217               V1Tlav = 'VISUALIZZA';
062400120112             when  $Copia;
062500120112               V1Tlav = '  COPIA   ';
062600090306             when  TBEatb = *blank;
062700071217               V1Tlav = 'VARIAZIONE';
062800071217             other;
062900071217               V1Tlav = 'ANNULLATO ';
063000071217           endsl;
063100090225
063200090306           // Codice padre
063300120604           V2Cksu = §JDCksu;
063400090309           clear  TIBS69ds;
063500090309           clear  ds_CNACO;
063600090309           clear  ds_CNIND;
063700090309           clear  ds_CNCLP;
063800090309           clear  ds_FNCLS;
063900090310           select;
064000120604             when  §JDCksu =  V2Cccm;
064100120606               V2Dksu = V2Dccm;
064200120604             when  §JDCksu <> *zero;
064300090310               tibs69ds.I69kcc = DUTkci;
064400090310               tibs69ds.I69kac = %int(V2Cksu);
064500090310               tibs69ds.I69sif = knsif;
064600090310               tibs69r(TIBS69ds:ds_CNACO:ds_CNIND:ds_CNCLP:ds_FNCLS);
064700090310               if tibs69ds.O69err = *on;
064800090310                 V1Dmsg = $Msg(03);
064900090310                 errMessage  = *on;
065000090310                 errGenerico = *on;
065100090310                 PosCurKsu   = *on;
065200090310                 leavesr;
065300090310               endif;
065400120606               V2Dksu = ds_CNACO.ACOrag;
065500090310           endsl;
065600090225
065700120604           V2Caut   = §JDCaut;
065800120604           V2Ctpi   = §JDCtpi;
065900090225
066000120605           // Flag nome documento
066100120604           V2Cfmi = §JDCfmi;
066200090310           reset  TIBS02ds;
066300090303           T02cod = 'NIM';
066400120604           T02ke1 = §JDCfmi;
066500090304           TNTBE_RicercaControllo(kpjba : tibs02ds);
066600090306           if  T02err = *blank;
066700090303             V2Dfmi = T02uni;
066800090303           endif;
066900121016
067000121016           V2merge  = §JDCmerge;
067100090225
067200120607           V2Cpag   = §JDCpag;
067300120604           V2Cdir   = §JDCdir;
067400120604           V2note   = §JDCnote;
067500120605           v2tadu   = §JDCtadu;
067600120608           data_eur = %date(§JDCdti:*iso);
067700120608           V2Cdti   = %dec(data_eur);
067800120608           data_eur = %date(§JDCdtf:*iso);
067900120608           V2Cdtf   = %dec(data_eur);
068000090923
068100090923           *in02 = *off;
068200071207
068300071207         ELSE;
068400071207
068500090306           // Impostazione dati di record nuovo
068600071207           V1Tlav = 'IMMISSIONE';
068700090226           V2Cdir = %editc(V2Cccm:'X');
068800120605
068900120605         //?FORZO frequenza addebito MENSILE (in questo modo avrà id job mensile)
069000120605           v2tadu   = 'M';
069100090923
069200090923           *in02 = *on;
069300071207
069400071207         ENDIF;
069500071207
069600071207       ENDSR;
069700071217
069800071217       //--------------------------------------------------------------
069900090306       //?Settaggio indicatori nella videata D02 per abilitazione      ?
070000090306       //?  tasti funzionali                                           ?
070100071217       //--------------------------------------------------------------
070200090306       BEGSR  Set_Ind_D02;
070300090319
070400120606         F4Attivo  = *off;
070500090306         F5Attivo  = ((%found(TNTBE01L) and TBEatb <> *blank)
070600120604                                        and BJDCopz <> '5');
070700120604         F6Attivo  = (NOT F5Attivo and BJDCopz <> '5');
070800090306         F16Attivo = ((%found(TNTBE01L) and TBEatb = *blank)
070900120604                                        and BJDCopz <> '5');
071000120112
071100120112         // - Protezione campo chiave SE NON copia
071200120112         InserCCM2 = $Copia;
071300071217
071400071217       ENDSR;
071500090319
071600090319       //--------------------------------------------------------------
071700090319       //?Gestione tasto funzionale F04 da videata D02                 ?
071800090319       //?F04=Interrogazione Padre                                     ?
071900090319       //--------------------------------------------------------------
072000090319       BEGSR  sr_F04D02;
072100090319
072200090319         // Richiamo pgm interrogazione
072300090327         clear  Param01;
072400090327         P01ord = '1';
072500090327         P01ksu = V2Cksu;
072600090327         KPJBU  = Param01;
072700120604         tntbjdc1r (KPJBA);
072800120607         Param01 = KPJBU;
072900120607         IF  P01ke1 <> *zero;
073000120607           V2Cksu = P01ksu;
073100120607         ENDIF;
073200090319
073300090319       ENDSR;
073400090319
073500071217
073600071217       //--------------------------------------------------------------
073700090306       //?Gestione tasto funzionale F12 da videata D02                 ?
073800090306       //?F12=Ritorno                                                  ?
073900071217       //--------------------------------------------------------------
074000090306       BEGSR  sr_F12D02;
074100071217
074200090306         // Ritorno alla videata precedente se NON richiamato
074300120604         if  BJDCopz = *blank  and
074400120604             BJDCsif = *blank  and  BJDClin = *blank  and
074500120604             BJDCke1 = *blank  and  BJDCke2 = *blank;
074600071217           clear V1Tlav;
074700071217           $Video = 'D1';
074800090306         // Ritorno al pgm chiamante se richiamato
074900071217         else;
075000120604           BJDCfxx = '2';
075100071217           $Fine = *on;
075200071217         endif;
075300071217
075400071217       ENDSR;
075500090306
075600090306       //--------------------------------------------------------------
075700090306       //?Controllo "annullabilità" del record (padre senza figli)     ?
075800090306       //--------------------------------------------------------------
075900090306       BEGSR  sr_CtrAnnull;
076000090306
076100090306         IndDspF = *zero;
076200090306         // REimpostazione indicatori per abilitazione tasti funzionali
076300090306         exsr  Set_Ind_D02;
076400090306
076500090306         if  V2Cksu <= *zero;
076600120604           V2Cksu = §JDCksu;
076700090306         endif;
076800090306
076900090306         // Se figlio (non padre): fine controlli
077000090306         if  V2Cccm <> V2Cksu;
077100090306           leavesr;
077200090306         endif;
077300090306
077400090306         //?Verifica se padre con figli?
077500120604         clear  dJDCp;
077600090316         SAVke1 = TBEke1;
077700090306         setll     %kds(k05tntbe01 : 1)  TNTBE000;
077800090306         reade(n)  %kds(k05tntbe01 : 1)  TNTBE000;
077900090306
078000090306         dow  not  %eof(TNTBE01L);
078100090316           if  TBEatb = *blank   and
078200090316               %editc(V2Cccm : 'X') <> %trimr(TBEke1);
078300120604             dJDCp = TBEuni;
078400120604             if  p_§JDCksu = V2Cccm;
078500090306               errMessage  = *on;
078600090306               errGenerico = *on;
078700090306               PosCurKsu   = *on;
078800120608               V1Dmsg = $Msg(12);
078900120608               %subst(V1Dmsg:60:7) = %subst(TBEke1:1:7);
079000090306               leavesr;
079100090306             endif;
079200090306           endif;
079300090306         reade(n)  %kds(k05tntbe01 : 1)  TNTBE000;
079400090306         enddo;
079500090316
079600120112         // - Riaggancio del rec. in manutenzione/copia
079700090316         TBEke1 = SAVke1;
079800090316         reset  TNTBE000;
079900090316         k_TBEcod = c_Tab;
080000120112         k_TBEke1 = %editc( V2Cccm : 'X' );
080100090316         clear  k_TBEke2;
080200090316         k_TBElin = TBXlin;
080300090316         k_TBEsif = TBXsif;
080400090316         chain  %kds(K05tntbe01) TNTBE000;
080500090306
080600090306       ENDSR;
080700071206
080800071212       //--------------------------------------------------------------
080900090306       //?Controllo videata D02                                        ?
081000071212       //--------------------------------------------------------------
081100090306       BEGSR  sr_CtrD02;
081200071207
081300090309         %subst(IndDspF : 50) = *off;
081400120112
081500120112         IF  $Copia;
081600120112
081700120112           V1Cccm = %editc( V2Cccm : 'X' );
081800120112
081900120112           Select;
082000120112
082100120112             // -?Codice estrazione (SE copia)?
082200120112             When  V2Cccm = *zero;
082300120112               errMessage  = *on;
082400120112               errGenerico = *on;
082500120112               PosCurCcm2  = *on;
082600120112               V1Dmsg = $Msg(01);
082700120112               leavesr;
082800120112
082900120112             // -?Controllo / Decodifica?
083000120112             When  tibs69ds.I69kac <> V2Cccm  or
083100120112                   k_TBEke1 <> %editc( V2Cccm : 'X' );
083200120112               clear  TIBS69ds;
083300120112               tibs69ds.I69kcc = DUTkci;
083400120112               tibs69ds.I69kac = V2Cccm;
083500120112               tibs69ds.I69sif = knsif;
083600120112               tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
083700120112               if  tibs69ds.O69err = *on;
083800120112                 errMessage  = *on;
083900120112                 errGenerico = *on;
084000120112                 PosCurCcm2  = *on;
084100120112                 V1Dmsg = $Msg(03);
084200120112                 leavesr;
084300120112               endif;
084400120112               V2Dccm = ds_CNACO.ACOrag;
084500120112               // -?Verifica esistenza in tabella?
084600120112               k_TBEke1 = %editc( V2Cccm : 'X' );
084700120112               chain  %kds(K05tntbe01) TNTBE000;
084800120112               select;
084900120112                 // - -?NON ammesso nuovo record per cliente annullato?
085000120112                 when  NOT  %found(TNTBE01L)  and  ds_CNACO.ACOflg <> *blank;
085100120112                   errMessage  = *on;
085200120112                   errGenerico = *on;
085300120112                   PosCurCcm2  = *on;
085400120112                   V1Dmsg = $Msg(04);
085500120112                   leavesr;
085600120112                 // - -?Impossibile inserimento di record già esistente?
085700120112                 when  %found(TNTBE01L);
085800120112                   errMessage  = *on;
085900120112                   errGenerico = *on;
086000120112                   PosCurCcm2  = *on;
086100120604                   V1Dmsg = $Msg(11);
086200120112                   leavesr;
086300120112               endsl;
086400120112
086500120112           EndSl;
086600120112
086700120112         ENDIF;
086800090225
086900090306         //?Codice padre?
087000120606         V2Cksu = V2Cccm;
087100090225         select;
087200090306           when  V2Cksu  = *zero;
087300090306             errMessage  = *on;
087400090225             errGenerico = *on;
087500090303             PosCurKsu   = *on;
087600120604             V1Dmsg = $Msg(08);
087700090225             leavesr;
087800090306           when  V2Cksu = V2Cccm;
087900120606             V2Dksu = V2Dccm;
088000090225           other;
088100090327             // - Controllo esistenza codice padre come cliente
088200120604             //   in tabella JDC
088300090303             exsr  sr_Padre;
088400090303             if  errGenerico = *on;
088500090226               leavesr;
088600090225             endif;
088700120606             V2Dksu = p_§JDCrag;
088800090226         endsl;
088900101103         //?SE codice padre <> da codice cliente?
089000101103         // ?& sono in immissione?
089100101103         IF  V2Cccm <> V2Cksu   and   *in02;
089200101103           // ?=> chiedo se si vogliono riportare i dati del padre?
089300101103           If  W1Ccpy = *blank;
089400101103             $Video  = 'W1';
089500101103             leavesr;
089600101103           EndIf;
089700100521         ENDIF;
089800090311
089900090311         //?Tipo elaborazione?
090000090327           // - Obbligatorio
090100120605           IF  V2Caut <> 'A'  and  V2Caut <> 'N';
090200090327             errMessage  = *on;
090300090327             errGenerico = *on;
090400090327             PosCurAut   = *on;
090500120608             V1Dmsg = $Msg(13);
090600090327             leavesr;
090700120604         endIF;
090800090327
090900120605         //?Tipo documento
091000120917         if  V2Ctpi <> 'TIF' and V2Ctpi <> 'PDF';
091100090327           errMessage  = *on;
091200090327           errGenerico = *on;
091300090327           PosCurTpi   = *on;
091400120608           V1Dmsg = $Msg(14);
091500090327           leavesr;
091600090327         endif;
091700090226
091800120605         //?Flag nome documento
091900090310         clear  V2Dfmi;
092000090310         select;
092100090310           // - Obbligatorietà
092200090310           when  V2Cfmi = *blank;
092300090310             errMessage  = *on;
092400090310             errGenerico = *on;
092500090310             PosCurFmi   = *on;
092600120608             V1Dmsg = $Msg(15);
092700090310             leavesr;
092800090310           // - Ricerca
092900090310           when  %scan('?' : V2Cfmi) > *zero;
093000090310             clear  TIBS02ds;
093100090310             T02mod = 'R';
093200090310             T02cod = 'NIM';
093300090310             T02sif = KNSIF;
093400090310             TNTBE_RicercaControllo(kpjba : tibs02ds);
093500090310             if  T02err = *blank;
093600090310               V2Cfmi = %subst(T02ke1 : 1 : %len(V2Cfmi));
093700090310               V2Dfmi = T02uni;
093800090310               errGenerico = *on;
093900090310               leavesr;
094000090310             else;
094100090310               errMessage  = *on;
094200090310               errGenerico = *on;
094300090310               PosCurFmi   = *on;
094400090310               V1Dmsg = T02msg;
094500090310               leavesr;
094600090310             endif;
094700090310           // - Controllo
094800090310           other;
094900090310             reset  TIBS02ds;
095000090310             T02cod = 'NIM';
095100090310             T02ke1 = V2Cfmi;
095200090310             TNTBE_RicercaControllo(kpjba : tibs02ds);
095300090310             if  T02err <> *blank;
095400090310               errMessage  = *on;
095500090310               errGenerico = *on;
095600090310               PosCurFmi   = *on;
095700120608               V1Dmsg = $Msg(15);
095800090310               leavesr;
095900090310             endif;
096000090310             V2Dfmi = T02uni;
096100090310         ENDSL;
096200120606
096300120606         //?Tipo invio
096400120607         SELECT;
096500120607           WHEN  V2Cpag  = *blanks;
096600120607             ErrMessage  = *on;
096700120607             ErrGenerico = *on;
096800120606             PosCurPag   = *on;
096900120608             V1Dmsg = $Msg(16);
097000120607             leavesr;
097100120607           WHEN  V2Cpag  <> 'CLI' and V2Cpag <> 'CL1';
097200120607             ErrMessage  = *on;
097300120607             ErrGenerico = *on;
097400120607             PosCurPag   = *on;
097500120608             V1Dmsg = $Msg(16);
097600120607             leavesr;
097700120607         ENDSL;
097800090226
097900120605         //?Directory per documenti
098000090226         select;
098100090306           when  V2Cdir  = *blank;
098200090226             errMessage  = *on;
098300090226             errGenerico = *on;
098400090226             PosCurDir   = *on;
098500090226             V1Dmsg = $Msg(05);
098600090226             leavesr;
098700090316           when  %subst(V2Cdir : 1 : 1) = *blank;
098800090316             errMessage  = *on;
098900090316             errGenerico = *on;
099000090316             PosCurDir   = *on;
099100120608             V1Dmsg = $Msg(17);
099200090316             leavesr;
099300090306           when  %scan('\':V2Cdir) > *zero;
099400090226             errMessage  = *on;
099500090226             errGenerico = *on;
099600090226             PosCurDir   = *on;
099700090226             V1Dmsg = $Msg(06);
099800090226             leavesr;
099900090306           when  %scan('%':V2Cdir) > *zero   and
100000090306                           V2Caut  = 'A';
100100090226             errMessage  = *on;
100200090226             errGenerico = *on;
100300090226             PosCurDir   = *on;
100400090226             V1Dmsg = $Msg(07);
100500090226             leavesr;
100600090309           when  %scan('&':V2Cdir) > *zero  and
100700090309                           V2Caut  = 'A';
100800090309             errMessage  = *on;
100900090309             errGenerico = *on;
101000090309             PosCurDir   = *on;
101100090309             V1Dmsg = $Msg(07);
101200090309             leavesr;
101300090226         endsl;
101400090309
101500090316         wPos1 = %scan('/' : V2Cdir);
101600090316         DoW  wPos1 > *zero;
101700090316           if  %subst( V2Cdir : wPos1 : 2 ) = '//';
101800090316             errMessage  = *on;
101900090316             errGenerico = *on;
102000090316             PosCurDir   = *on;
102100120608             V1Dmsg = $Msg(18);
102200090316             leavesr;
102300090316           else;
102400090316             wPos1 = %scan('/' : V2Cdir : wPos1 + 1 );
102500090316           endif;
102600090316         EndDo;
102700090316
102800090309         wPos1 = %scan('&' : V2Cdir);
102900090309         IF  wPos1 > *zero;
103000090309           if  %subst( V2Cdir : wPos1 : 4 ) <> '&AAA'   and
103100090309               %subst( V2Cdir : wPos1 : 2 ) <> '&M';
103200090309             errMessage  = *on;
103300090309             errGenerico = *on;
103400090309             PosCurDir   = *on;
103500120608             V1Dmsg = $Msg(19);
103600090309             leavesr;
103700090309           endif;
103800090309           wPos2 = %scan( '&' : V2Cdir : wPos1 + 1 );
103900090309           If  wPos2 > *zero;
104000090309             if  %subst( V2Cdir : wPos2 : 4 ) <> '&AAA'   and
104100090309                 %subst( V2Cdir : wPos2 : 2 ) <> '&M';
104200090309               errMessage  = *on;
104300090309               errGenerico = *on;
104400090309               PosCurDir   = *on;
104500120608               V1Dmsg = $Msg(19);
104600090309               leavesr;
104700090309             endif;
104800090309           EndIf;
104900090309         ENDIF;
105000090309
105100090309         wPos1 = %scan('&M' : V2Cdir);
105200090309         IF  wPos1 > *zero;
105300090309           wPos2 = %scan( '&M' : V2Cdir : wPos1 + 1 );
105400090309           If  wPos2 > *zero   and
105500090309               %scan( '&M' : V2Cdir : wPos2 + 1 ) > *zero;
105600090309             errMessage  = *on;
105700090309             errGenerico = *on;
105800090309             PosCurDir   = *on;
105900120608             V1Dmsg = $Msg(20);
106000090309             leavesr;
106100090309           EndIf;
106200090309         ENDIF;
106300120607
106400120607         // Se FTP la directory deve finire con /DI
106500121016         // se non c'è il flag di Merge, in questo caso c'è il controllo
106600121016         // nella route CTR_merge
106700121016         IF  V2Caut = 'A' and V2merge = *blanks;
106800120607           wPos1 = %scan('  ':V2Cdir);
106900120607           wPos1 -= 3;
107000120607           IF  %subst(V2Cdir:wPos1:3) <> '/DI';
107100120607             ErrMessage  = *on;
107200120607             ErrGenerico = *on;
107300120607             PosCurDir   = *on;
107400120608             V1Dmsg = $Msg(21);
107500120607             leavesr;
107600120607           ENDIF;
107700120607         ENDIF;
107800090226
107900090327         // - Se codice padre <> da codice cliente
108000090306         //   la directory deve coincidere almeno fino alla 2ª subdir
108100120604         IF  V2Cccm <> V2Cksu   and   p_§JDCdir <> V2Cdir;
108200090311
108300090311           // - Controllo 1ª directory
108400090311           wPos1 = %scan( '/' : %trim( V2Cdir ) );
108500120604           wPos2 = %scan( '/' : %trim( p_§JDCdir ) );
108600090313           If  wPos1 <> wPos2                               OR
108700090313             ( wPos1  > *zero   and   wPos2 > *zero   and
108800090311               %subst( %trim( V2Cdir    ) : 1 : wPos1 ) <>
108900120604               %subst( %trim( p_§JDCdir ) : 1 : wPos2 ) )   OR
109000090313             ( wPos1  = *zero   and   wPos2  =  *zero
109100120604                                and   V2Cdir <> p_§JDCdir );
109200090311             errMessage  = *on;
109300090311             errGenerico = *on;
109400090311             PosCurDir   = *on;
109500090311             select;
109600090311               when  wPos1 =  *zero   and   wPos2 <> *zero;
109700090316                 V1Dmsg = 'Il padre ha altri rami directory (';
109800090311               when  wPos1 <> *zero   and   wPos2 =  *zero;
109900090311                 V1Dmsg = 'Il padre ha una directory unica (';
110000090311               other;
110100090311                 V1Dmsg = 'Primo ramo directory +
110200090311                           <> da quello del cod. padre (';
110300090311             EndSl;
110400120604             if  %len( %trim( p_§JDCdir ) ) <=
110500090311                 %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) ) - 1;
110600090311               V1Dmsg = %trimr( V1Dmsg )
110700120604                      + %trim( p_§JDCdir ) + ')';
110800090311             else;
110900090313               wPos1 = %len( V1Dmsg );
111000090313               wPos2 = %len( %trimr( V1Dmsg ) );
111100090311               V1Dmsg = %trimr( V1Dmsg )
111200120604                      + %trim( %subst( p_§JDCdir : 1 :
111300090311                          %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) )
111400090311                          - 4 ) )
111500090311                      + '...)';
111600090311             endif;
111700090311             leavesr;
111800090311           EndIf;
111900090311
112000090311           // - Controllo 2ª directory
112100090311           wPos1 = %scan( '/' : %trim( V2Cdir ) : wPos1 + 1 );
112200120604           wPos2 = %scan( '/' : %trim( p_§JDCdir ) : wPos2 + 1 );
112300090316           //If  wPos1 <> wPos2                               OR
112400090316           //  ( wPos1  > *zero   and   wPos2 > *zero   and
112500090316           //    %subst( %trim( V2Cdir    ) : 1 : wPos1 ) <>
112600120604           //    %subst( %trim( p_§JDCdir ) : 1 : wPos2 ) )   OR
112700090316           //  ( wPos1  = *zero   and   wPos2  =  *zero
112800120604           //                     and   V2Cdir <> p_§JDCdir );
112900090316           if  wPos1 = *zero;
113000090316             wPos1 = %len( %trimr( V2Cdir ) );
113100090316           else;
113200090316             wPos1 -= 1;
113300090316           endif;
113400090316           if  wPos2 = *zero;
113500120604             wPos2 = %len( %trimr( p_§JDCdir ) );
113600090316           else;
113700090316             wPos2 -= 1;
113800090316           endif;
113900090316           If  wPos1 <> wPos2   OR
114000090316               %subst(V2Cdir : 1 : wPos1) <>
114100120604                 %subst(p_§JDCdir : 1 : wPos2);
114200090311             errMessage  = *on;
114300090311             errGenerico = *on;
114400090311             PosCurDir   = *on;
114500090316             //select;
114600090316             //  when  wPos1 =  *zero   and   wPos2 <> *zero;
114700090316             //    V1Dmsg = 'Il padre ha altri rami directory (';
114800090316             //  when  wPos1 <> *zero   and   wPos2 =  *zero;
114900090316             //    V1Dmsg = 'Il padre ha meno rami directory (';
115000090316             //  other;
115100090313                 V1Dmsg = 'Secondo ramo directory +
115200090311                           <> da quello del cod. padre (';
115300090316             //EndSl;
115400120604             if  %len( %trim( p_§JDCdir ) ) <=
115500090311                 %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) ) - 1;
115600090311               V1Dmsg = %trimr( V1Dmsg )
115700120604                      + %trim( p_§JDCdir ) + ')';
115800090311             else;
115900090311               V1Dmsg = %trimr( V1Dmsg )
116000120604                      + %trim( %subst( p_§JDCdir : 1 :
116100090311                          %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) )
116200090311                          - 4 ) )
116300090311                      + '...)';
116400090311             endif;
116500090311             leavesr;
116600090311           EndIf;
116700090311
116800090306         ENDIF;
116900121016
117000121016         //?Immagine con Merge (LDV + DOC)
117100121016         IF  V2merge = 'S';
117200121016           exsr CTR_merge;
117300121016         ENDIF;
117400121016         IF  ErrGenerico;
117500121016           leavesr;
117600121016         ENDIF;
117700120608
117800120608         //?Data inizio e fine scansione documenti
117900120608         IF  V2Cdti  = *zeros;
118000120608           ErrMessage  = *on;
118100120608           ErrGenerico = *on;
118200120608           PosCurDti   = *on;
118300120608           V1Dmsg      = $Msg(22);
118400120608           leavesr;
118500120608         ENDIF;
118600120608         IF  V2Cdtf  = *zeros;
118700120608           ErrMessage  = *on;
118800120608           ErrGenerico = *on;
118900120608           PosCurDtf   = *on;
119000120608           V1Dmsg      = $Msg(22);
119100120608           leavesr;
119200120608         ENDIF;
119300120608         IF  V2Cdti  > *zeros;
119400120608           clear wlbdat;
119500120608           G02dat = V2Cdti;
119600120608           xsrda8(wlbdat);
119700120608           IF  G02err = '1';
119800120608             ErrMessage  = *on;
119900120608             ErrGenerico = *on;
120000120608             PosCurDti   = *on;
120100120608             V1Dmsg      = $Msg(22);
120200120608             leavesr;
120300120608           ENDIF;
120400120608           V2Cdti = G02dat;
120500120608           wdatai = G02inv;
120600120608         ENDIF;
120700120608         IF  V2Cdtf  > *zeros;
120800120608           clear wlbdat;
120900120608           G02dat = V2Cdtf;
121000120608           xsrda8(wlbdat);
121100120608           IF  G02err = '1';
121200120608             ErrMessage  = *on;
121300120608             ErrGenerico = *on;
121400120608             PosCurDtf   = *on;
121500120608             V1Dmsg      = $Msg(22);
121600120608             leavesr;
121700120608           ENDIF;
121800120608           V2Cdtf = G02dat;
121900120608           wdataf = G02inv;
122000120608         ENDIF;
122100120608         IF  wdatai > wdataf;
122200120608           ErrMessage  = *on;
122300120608           ErrGenerico = *on;
122400120608           PosCurDti   = *on;
122500120608           V1Dmsg = $Msg(23);
122600120608           leavesr;
122700120608         ENDIF;
122800071207
122900071207       ENDSR;
123000090225
123100090225       //--------------------------------------------------------------
123200120604       //?Controllo esistenza del codice padre in tabella JDC          ?
123300090225       //--------------------------------------------------------------
123400090303       BEGSR  sr_Padre;
123500090302
123600120604         // Controlla esistenza in tabella "JDC"
123700120604         clear  dJDCp;
123800090310         reset  TIBS02ds;
123900090310         T02sif = KNSIF;
124000090310         %subst(T02ke1 : 1 : 7) = %editc(V2Cksu : 'X');
124100090310         TNTBE_RicercaControllo(kpjba : tibs02ds);
124200090310         if  T02err = *blank;
124300120604           dJDCp  = T02uni;
124400090310         else;
124500090310           errMessage  = *on;
124600090310           errGenerico = *on;
124700090310           PosCurksu   = *on;
124800120604           V1Dmsg = $Msg(09);
124900090310           leavesr;
125000090310         endif;
125100090327
125200090327         // Controlla se già inserito come figlio di altro padre
125300120604         if  p_§JDCksu <> V2Cksu;
125400090327           errMessage  = *on;
125500090327           errGenerico = *on;
125600090327           PosCurKsu   = *on;
125700090327           V1Dmsg = 'Non ammesso cod. PADRE già FIGLIO di altro +
125800120604                     cliente (' + %editc(p_§JDCksu : 'X') + ')';
125900090327           leavesr;
126000090327         endif;
126100090225
126200090225       ENDSR;
126300101103
126400101103       //--------------------------------------------------------------
126500101103       //?Gestione window W01                                          ?
126600101103       //--------------------------------------------------------------
126700101103       BEGSR  sr_GesW01;
126800101103
126900101103         // -?Emissione window?
127000120604         exfmt  TBJDCW01;
127100101103
127200101103         errMessage  = *off;
127300101103         errGenerico = *off;
127400101103         clear  V1Dmsg;
127500101103
127600101103         $Video = 'D2';
127700101103
127800101103         select;
127900101103           // -?F12=Ritorno?
128000101103           when  dsp_aid = c_F12;
128100101103             leavesr;
128200101103           // -?Invio con "S"?
128300101103           when  W1Ccpy = 'S';
128400101103             if  V2Caut = *blank;
128500120604               V2Caut   = p_§JDCaut;
128600101103             endif;
128700101103             if  V2Ctpi = *blank;
128800120604               V2Ctpi   = p_§JDCtpi;
128900101103             endif;
129000101103             if  V2Cfmi = *blank;
129100120604               V2Cfmi   = p_§JDCfmi;
129200101103               clear  V2Dfmi;
129300101103             endif;
129400121016             if  V2merge = *blank;
129500121016               V2merge  = p_§JDCmerge;
129600121016             endif;
129700101103             //if  V2Cdir = *blank;      //? Directory ?
129800120604               V2Cdir   = p_§JDCdir;     //? COMUNQUE  ?
129900101103             //endif;                    //? del padre ?
130000101103             if  V2note = *blank;
130100120604               V2note   = p_§JDCnote;
130200101103             endif;
130300120605             if  V2tadu = *blank;
130400120605               V2tadu   = p_§JDCtadu;
130500120605             endif;
130600120607             if  V2cpag = *blank;
130700120607               V2cpag   = p_§JDCpag;
130800120607             endif;
130900120608             if  V2cdti = *zeros;
131000120608               data_eur = %date(p_§JDCdti:*iso);
131100120608               V2Cdti   = %dec(data_eur);
131200120608             endif;
131300120608             if  V2cdtf = *zeros;
131400120608               data_eur = %date(p_§JDCdtf:*iso);
131500120608               V2Cdtf   = %dec(data_eur);
131600120608             endif;
131700101103         endsl;
131800101103
131900101103       ENDSR;
132000121016
132100121016       //--------------------------------------------------------------
132200121016       //?Controllo Merge.
132300121016       //--------------------------------------------------------------
132400121016       BEGSR  CTR_merge;
132500121016
132600121016       // -?L'immagine se con Merge deve essere di tipo PDF
132700121016         IF  V2Ctpi <> 'PDF';
132800121016           errMessage  = *on;
132900121016           errGenerico = *on;
133000121016           V1Dmsg = 'Immagine con Merge, deve essere di tipo PDF';
133100121016           PosCurTpi = *on;
133200121016           leavesr;
133300121016         ENDIF;
133400121016
133500121016       // -?La directory deve finire con /DF
133600121016         wPos1 = %scan('  ':V2Cdir);
133700121016         wPos1 -= 3;
133800121016         IF  %subst(V2Cdir:wPos1:3) <> '/DF';
133900121016           ErrMessage  = *on;
134000121016           ErrGenerico = *on;
134100121016           PosCurDir   = *on;
134200121016           V1Dmsg = 'Immagine con Merge, l''ultimo ramo della +
134300121016                     DIR deve essere "DF"';
134400121016           leavesr;
134500121016         ENDIF;
134600121016
134700121016       // -?Cerco il corrispondente in LAC
134800121016         clear  TIBS02DS;
134900121016         T02mod = 'C';
135000121016         T02cod = 'LAC';
135100121016         T02ke1 = %editc(V2Cccm:'X');
135200121016         TNTBE_RicercaControllo(kpjba : tibs02ds);
135300121016         // -?Non trovo la tabella
135400121016         IF  T02err <> *blanks;
135500121016           // -?Immissione forzo
135600121016           IF  *in02 and not wForza;
135700121016             wForza = *on;
135800121016             ErrMessage  = *on;
135900121016             ErrGenerico = *on;
136000121016             V1Dmsg = 'Ricordarsi di attivare anche la corrispondente +
136100121016                       tabella LAC. Enter forza.';
136200121016             leavesr;
136300121016           ENDIF;
136400121016         // -?Manutenzione blocco
136500121016           IF  not *in02;
136600121016             ErrMessage  = *on;
136700121016             ErrGenerico = *on;
136800121016             V1Dmsg = 'Manca la corrispondente tabella LAC';
136900121016             leavesr;
137000121016           ENDIF;
137100121016         // -?Trovo la tabella
137200121016         ELSE;
137300121016           dLAC = T02uni;
137400121016         // -?Controllo del formato documento
137500121030           IF  V2Ctpi <> §LACtpi and not wForzaTPI;
137600121030             wForzaTPI = *on;
137700121016             ErrMessage  = *on;
137800121016             ErrGenerico = *on;
137900121016             PosCurTpi   = *on;
138000121016             V1Dmsg = 'Formato documento diverso dalla corrispondente +
138100121016                       tabella LAC --> ' + §LACtpi;
138200121016             leavesr;
138300121016           ENDIF;
138400121016         // -?Controllo il nome immagine
138500121030           IF  V2Cfmi <> §LACfmi and not wForzaFMI;
138600121030             wForzaFMI = *on;
138700121016             ErrMessage  = *on;
138800121016             ErrGenerico = *on;
138900121016             PosCurFmi   = *on;
139000121016             V1Dmsg = 'Nome immagine diverso dalla corrispondente +
139100121016                       tabella LAC --> ' + §LACfmi;
139200121016             leavesr;
139300121016           ENDIF;
139400121016         // -?Controllo la directory
139500121016           wPos2 = %scan('  ':§LACdir);
139600121016           wPos2 -= 3;
139700121016           IF  %subst(V2Cdir:1:wPos1) <>
139800121030               %subst(§LACdir:1:wPos2) and not wForzaDIR;
139900121030             wForzaDIR = *on;
140000121016             ErrMessage  = *on;
140100121016             ErrGenerico = *on;
140200121016             PosCurDir   = *on;
140300121016             V1Dmsg = 'Directory diversa dalla corrispondente +
140400121016                       tabella LAC --> ' + %subst(§LACdir:1:wPos2);
140500121016             leavesr;
140600121016           ENDIF;
140700121016         ENDIF;
140800121016
140900121016       ENDSR;
141000071206
141100071212       //--------------------------------------------------------------
141200120604       //?Aggiornamento record TNTBE00F (tab. JDC)                     ?
141300071212       //--------------------------------------------------------------
141400090306       BEGSR  Upd_TNTBE;
141500071207
141600120604         clear  dJDC;
141700120604         §JDCrag   = V2Dccm;
141800120604         §JDCdir   = V2Cdir;
141900120604         §JDCtpi   = V2Ctpi;
142000120604         §JDCfmi   = V2Cfmi;
142100121016         §JDCmerge = V2merge;
142200120604         §JDCaut   = V2Caut;
142300120604         §JDCnote  = V2note;
142400120604         §JDCksu   = V2Cksu;
142500120605         §JDCtadu  = V2tadu;
142600120607         §JDCpag   = V2Cpag;
142700120608         §JDCdti   = wdatai;
142800120608         §JDCdtf   = wdataf;
142900071207
143000120604         TBEuni    = dJDC;
143100071207
143200090306         clear  TBEftt;
143300090306         clear  TBEftr;
143400071207
143500071207         select;
143600090306           // F5=Ripristino
143700090306           when  dsp_aid = c_F05;
143800090306             clear  TBEatb;
143900090306           // F16=Annullamento
144000090306           when  dsp_aid = c_F16;
144100071207             TBEatb = 'A';
144200071207         endsl;
144300071207
144400071207         IF  NOT  %found(TNTBE01L);
144500090306           clear  TBEatb;
144600090306           clear  TBEflt;
144700090306           clear  TBEftr;
144800090306           //clear  TBEdtr;
144900090306           TBEsif = TBXsif;
145000090306           TBElin = TBXlin;
145100090306           TBEapl = TBXapl;
145200090306           TBEcod = k_TBEcod;
145300090306           TBEke1 = k_TBEke1;
145400090306           TBEke2 = k_TBEke2;
145500090306           //_____________
145600071207           WRITE TNTBE000;
145700090306           //¯¯¯¯¯¯¯¯¯¯¯¯¯
145800071207         ELSE;
145900090306           //______________
146000071207           UPDATE TNTBE000;
146100090306           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
146200071207         ENDIF;
146300071207
146400071207       ENDSR;
146500071207
146600071212       //--------------------------------------------------------------
146700090306       //?Operazioni finali.                                           ?
146800071212       //--------------------------------------------------------------
146900090306       BEGSR  sr_RoutEnd;
147000071207
147100090306         clear  TIBS69ds;
147200090304         tibs69ds.I69tla = 'C';
147300090304         tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
147400071207
147500120604         kpjbu = TNTBJDCds;
147600071207         return;
147700071207
147800071207       ENDSR;
147900090306
148000071207      /end-free
148100071207
148200071212       //--------------------------------------------------------------
148300090306       //?Schiere a tempo di compilazione.                             ?
148400071212       //--------------------------------------------------------------
148500071207
148600071207** - $MSG -------------------------------------------------------------------*
148700071207Immettere il codice cliente                                                     1
148800071207Immettere SOLO caratteri numerici                                               2
148900071207Codice errato                                                                   3
149000071207Cliente annullato                                                               4
149100071207Immettere la directory                                                          5
149200071207Carattere "\" non valido; per indicare una sottocartella utilizzare "/"         6
149300071210Caratteri & e %  non ammessi per la directory di clienti con invio automatico   7
149400120604Inserire il codice padre                                                        8
149500120604Il codice padre deve essere = al codice cliente o = a cliente già in tabella    9
149600120604Cliente da copiare NON più reperibile in tab. "JDC"                            10
149700120604Cliente già inserito in tab. "JDC"                                             11
149800120608Record NON cancellabile: trattasi di PADRE con figli (vedi xxxxxxx)            12
149900120608Tipo elaborazione errato o mancante                                            13
150000120608Tipo documento errato o mancante                                               14
150100120608Nome documento errato o mancante                                               15
150200120608Tipo invio errato o mancante                                                   16
150300120608Non ammessi spazi vuoti all'inizio del nome della directory                    17
150400120608Nome directory NON valido: non ammesso il doppio "/"                           18
150500120608Ammessi solo "&AAA" e "&M" (come caratteri sostituibili)                       19
150600120608Ammessi max 2 "&M"                                                             20
150700120608Se invio automatico l'ultimo ramo della directory deve essere "DI"             21
150800120608Data errata                                                                    22
150900120608Data "DAL" incongruente con data "AL"                                          23
