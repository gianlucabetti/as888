000100071207      *PARMS OPTION(*NOXREF) DFTACTGRP(*NO) ACTGRP(QILE)
000200101103     /*PRM dftactgrp(*no)
000300101103     /*END
000400090306
000500120605       //---------------------------------------------------------------
000600120605       //?GESTIONE TABELLA "JDC" - Jdoc: clienti per ritorno documenti
000700120605       //---------------------------------------------------------------
000800110804
000900110804      *?  ATTENZIONE!!  ?
001000110804      *?    Questa tabella è utilizzata anche dal pgm TNTA61R  ?
001100110804      *?    'Interrogazione abilitazioni clienti'              ?
001200110804      *?    In caso di aggiunta/modifica campi alla tabella    ?
001300110804      *?    verificare anche relativo pgm di interrogazione    ?
001400120604      *?    per le abilitazioni clienti --> TNTBJDC2R.         ?
001500090306
001600071206     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700071206
001800090306       //--------------------------------------------------------------
001900090306       //?Dichiarazione file.                                          ?
002000090306       //--------------------------------------------------------------
002100090306
002200090311     fAZORG01L  if   e           k disk
002300090311
002400071206     fTNTBE01L  Uf A e           k disk
002500090311
002600120604     fTNTBJDCD  cf   e             workstn
002700071207     f                                     indds(IndDspF)
002800090306     f                                     infds(InfDspF)
002900071207
003000090306       //--------------------------------------------------------------
003100090306       //?Definizione costanti.                                        ?
003200090306       //--------------------------------------------------------------
003300090306
003400090306       // - Tabella in gestione
003500120604     d c_Tab           c                   const('JDC')
003600090306
003700090306       // - Costante per controllo "caratteri solo numerici"
003800090310     d c_Digits        c                   const('0123456789')
003900090306
004000090306       // - Tasti funzionali a video
004100090306     d c_F01           c                   const(x'31')
004200090306     d c_F02           c                   const(x'32')
004300090306     d c_F03           c                   const(x'33')
004400090319     d c_F04           c                   const(x'34')
004500090306     d c_F05           c                   const(x'35')
004600090306     d c_F06           c                   const(x'36')
004700090306     d c_F07           c                   const(x'37')
004800090306     d c_F08           c                   const(x'38')
004900090306     d c_F09           c                   const(x'39')
005000090306     d c_F10           c                   const(x'3A')
005100110713     d c_F11           c                   const(x'3B')
005200090306     d c_F12           c                   const(x'3C')
005300090306     d c_F13           c                   const(x'B1')
005400090306     d c_F14           c                   const(x'B2')
005500090306     d c_F15           c                   const(x'B3')
005600090306     d c_F16           c                   const(x'B4')
005700090306     d c_F17           c                   const(x'B5')
005800090306     d c_F18           c                   const(x'B6')
005900090306     d c_F19           c                   const(x'B7')
006000090306     d c_F20           c                   const(x'B8')
006100090306     d c_F21           c                   const(x'B9')
006200090306     d c_F22           c                   const(x'BA')
006300090306     d c_F23           c                   const(x'BB')
006400090306     d c_F24           c                   const(x'BC')
006500090306     d c_Enter         c                   const(x'F1')
006600090306     d c_RollDown      c                   const(x'F4')
006700090306     d c_RollUp        c                   const(x'F5')
006800071207
006900090306       //--------------------------------------------------------------
007000090306       //?Definizione schiere.                                         ?
007100090306       //--------------------------------------------------------------
007200090306
007300090306       // - Messaggi a video
007400120608     d $Msg            s             78    dim(30)  ctdata  perrcd(1)
007500071207
007600090306       //--------------------------------------------------------------
007700090306       //?Definizione aree dati.                                       ?
007800090306       //--------------------------------------------------------------
007900090306
008000090306       // - Dati utente
008100071207     d §AzUte        e ds                  extname(AZUTE00F)
008200071207     d                                     dtaara
008300071207     d §DatiUte      e ds                  extname(dDatiUte)
008400071207     d                                     dtaara
008500071207
008600090306       //--------------------------------------------------------------
008700090306       //?Definizione strutture dati.                                  ?
008800090306       //--------------------------------------------------------------
008900090306
009000090306       // - Status
009100071207     d Psds           sds
009200071207     d   SDSpgm          *proc
009300071207
009400090306       // - InfDS
009500071207     d InfDspF         ds
009600071207     d  dsp_aid              369    369a                                        AID byte
009700071207
009800090306       // - Indicatori su DspF
009900090311     d IndDspF         ds
010000090306         // - Abilitazione tasti funzionali
010100090323     d  F4Attivo                       n   overlay(IndDspF:04)
010200090311     d  F5Attivo                       n   overlay(IndDspF:05)
010300090311     d  F6Attivo                       n   overlay(IndDspF:06)
010400090311     d  F16Attivo                      n   overlay(IndDspF:16)
010500090306         // - Emissione messaggio di errore
010600090311     d  ErrMessage                     n   overlay(IndDspF:28)
010700090306         // - Protezione campi
010800120112     d  InserCCM2                      n   overlay(IndDspF:41)
010900090306         // - Posizionamento cursore & segnalazione errore
011000090311     d  PosCurCcm                      n   overlay(IndDspF:50)
011100090311     d  PosCurAut                      n   overlay(IndDspF:51)
011200090311     d  PosCurTpi                      n   overlay(IndDspF:52)
011300090311     d  PosCurDir                      n   overlay(IndDspF:53)
011400120606     d  PosCurPag                      n   overlay(IndDspF:54)
011500120608     d  PosCurDti                      n   overlay(IndDspF:55)
011600120608     d  PosCurDtf                      n   overlay(IndDspF:56)
011700090311     d  PosCurKsu                      n   overlay(IndDspF:61)
011800120604     d  PosCurFmi                      n   overlay(IndDspF:70)
011900120112     d  PosCurCCM2                     n   overlay(IndDspF:76)
012000090306         //   - Riemissione videata
012100090311     d  ErrGenerico                    n   overlay(IndDspF:99)
012200071207
012300090306       // - Parametri ricevuti
012400071206     d KPJBA         e ds
012500120604     d TNTBJDCds     e ds                  inz
012600071207
012700090306       // - Reperimento dati utente
012800071207     d TIBS34ds      e ds
012900071207
013000090306       // - Controllo/Decodifica cliente
013100090304     d TIBS69ds      e ds                  qualified  inz
013200090304     d ds_CNACO      e ds                  extname(CNACO00F)
013300090304     d                                     qualified  inz
013400090304     d ds_CNIND      e ds                  extname(CNIND00F)
013500090304     d                                     qualified  inz
013600090304     d ds_CNCLP      e ds                  extname(CNCLP00F)
013700090304     d                                     qualified  inz
013800090304     d ds_FNCLS      e ds                  extname(FNCLS00F)
013900090304     d                                     qualified  inz
014000090306
014100090306       // - Dati record principale della tabella
014200090306     d TNTBEds       e ds                  inz  extname(TNTBE00F)
014300090306     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
014400090306     d                                          prefix(TBX:3)
014500090312
014600090312       // - Parametri per Ricerca/controllo tabelle
014700090312     d TIBS02ds      e ds                  inz
014800090312     d  T02mod       e                     inz('C')
014900090312     d  T02cod       e                     inz(c_Tab)
015000071207
015100120605       // - Tabella "JDC" = Clienti per ritorno documenti
015200120604     d dJDC          e ds                  inz
015300120604       // - Tabella "JDC" del padre per controllo legami
015400120604     d dJDCp         e ds                  extname(dJDC) inz
015500090225     d                                     prefix(p_)
015600121016
015700121016       // - Tabella "LAC" = Clienti per ritorno LDV
015800121016     d dLAC          e ds                  inz
015900120604
016000090319       // - Struttura per passaggio dati ad interrogazione tabella
016100090327     d Param01         ds                  inz
016200090327     d  P01cod                        7  0 inz
016300090327     d  P01ord                        1    inz
016400090327     d  P01ksu                        7  0 inz
016500090327     d  P01ke1                        7    inz
016600090327     d  P01ke2                       15    inz
016700090327     d  P01rit                        1    inz
016800120608
016900120608     d wlbdat          ds                  inz
017000120608     d  g02dat                 1      8  0
017100120608     d  g02inv                 9     16  0
017200120608     d  g02err                17     17
017300120608     d  g02tgi                18     22  0
017400071207
017500090306       //--------------------------------------------------------------
017600090306       //?Definizione procedure usate.                                 ?
017700090306       //--------------------------------------------------------------
017800090306
017900090306       // - Reperimento dati utente
018000090306      /copy gaitrasrc/srcProtoPR,TIBS34R
018100071207
018200090304       // - Ricerca/Controllo tabelle
018300090306      /copy gaitrasrc/srcProtoPR,TIBS02R
018400071207
018500090304       // - Controllo/Decodifica cliente
018600090306      /copy gaitrasrc/srcProtoPR,TIBS69R
018700090319
018800120604       // - Interrogazione tabella JDC
018900120604     d tntbJDC1r       pr                  extpgm('TNTBJDC1R')
019000090319     d  kpjba                              likeds(KPJBA)
019100120112     d  p_SNopz3                      1a   const  options(*nopass)
019200120608
019300120608       // - Controllo data
019400120608      /copy gaitrasrc/srcprotopr,xsrda8
019500071207
019600090306       //--------------------------------------------------------------
019700090306       //?Definizione variabili globali.                               ?
019800090306       //--------------------------------------------------------------
019900090306
020000090306       // - Flags booleani
020100090306     d $InzD01         s               n   inz(*on)
020200090306     d $InzD02         s               n   inz(*on)
020300090306     d $Fine           s               n   inz
020400090923     d $forza          s               n   inz
020500090313     d $ByPass         s               n   inz
020600120112     d $Copia          s               n   inz
020700121016     d wForza          s               n   inz
020800121030     d wForzaTPI       s               n   inz
020900121030     d wForzaFMI       s               n   inz
021000121030     d wForzaDIR       s               n   inz
021100071207
021200090306       // - Gestione video
021300071206     d $Video          s              2    inz('D1')
021400090313
021500090313       // - Indici di schiera
021600090313     d xx              s              3  0 inz
021700071207
021800090313       // - Campi di comodo
021900090306     d wPos1           s              2  0 inz
022000090306     d wPos2           s              2  0 inz
022100090316     d SAVke1          s                   like(TBEke1)    inz
022200120608     d wdatai          s              8  0 inz
022300120608     d wdataf          s              8  0 inz
022400120608     d data_eur        s               d   Datfmt(*eur)
022500120608     d data_iso        s               d   Datfmt(*iso)
022600071207
022700090306       //--------------------------------------------------------------
022800090306       //?Definizione key-list.                                        ?
022900090306       //--------------------------------------------------------------
023000090306
023100090306       // - File TNTBE01L
023200090306     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
023300090306     d                                     prefix(k_)  inz
023400071206
023500090306       //--------------------------------------------------------------
023600090306       //?Riepilogo indicatori.                                        ?
023700090306       //--------------------------------------------------------------
023800090306       // 01    - RECORD annullato
023900090306       // 02    - acceso IMMISSIONE - spento MANUTENZIONE
024000090306       // 25    - Comodo
024100090306       // 28    - Emissione del messaggio di errore a video
024200090306       // 40    - Protezione campi in prima estrazione
024300090306       // x D01:
024400090306       // 50    - Codice cliente errato o mancante
024500090306       // x D02:
024600120112       // 41    - Copia in D02: V2CCCM non protetto
024700120605       // 53    - Directory per documenti      errata
024800090306       // 61    - Codice Padre                 errato
024900120604       // 63    - Tipo data di elaborazione    errato
025000120605       // 70    - Flag nome documento          errato
025100090306       // 99    - Generico di errore
025200090306       //--------------------------------------------------------------
025300071206
025400071206     c     *Entry        plist
025500071206     c                   parm                    KPJBA
025600071207
025700071207      /free
025800071207
025900071207       // Operazioni iniziali
026000090306       exsr  sr_RoutInz;
026100071207
026200071207       // Gestione video
026300090306       DOW  $Fine = *off;
026400071207         select;
026500090306           when  $Video = 'D1';
026600090306             exsr  sr_GesD01;
026700090306           when  $Video = 'D2';
026800090306             exsr  sr_GesD02;
026900101103           when  $Video = 'W1';
027000101103             exsr  sr_GesW01;
027100071207           other;
027200071207             leave;
027300071207         endsl;
027400071207       ENDDO;
027500071207
027600071207       // Operazioni finali
027700090306       exsr  sr_RoutEnd;
027800071206
027900071212       //--------------------------------------------------------------
028000090306       //?Operazioni iniziali.                                         ?
028100071212       //--------------------------------------------------------------
028200090306       BEGSR  sr_RoutInz;
028300090306
028400090306         *inLR = *on;
028500071207
028600090306         if  kpjbu <> *blank;
028700120604           TNTBJDCds = kpjbu;
028800071207         else;
028900120604           clear  TNTBJDCds;
029000071207         endif;
029100120604         BJDCfxx = *blank;
029200120604         BJDCerr = *off;
029300120604         BJDCmsg = *blank;
029400071207
029500090306         // Reperimento dati job
029600090306         exsr  DatiJob;
029700071207
029800090306         // Impostazione nome programma a video
029900071207         V1Tpgm = SDSpgm;
030000090306
030100090306         // Aggancio dati generali della tabella in esame
030200090306         clear  k_TBEcod;
030300090306         k_TBEke1 = *zero;
030400090306         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
030500090306                = c_Tab;
030600090306         clear  k_TBEke2;
030700090306         clear  k_TBElin;
030800090306         k_TBEsif = KNSIF;
030900090306         chain(n)  %kds(k05tntbe01)  TNTBE000;
031000090306         if  not  %found(TNTBE01L);
031100090306           clear  k_TBEsif;
031200090306           chain(n)  %kds(k05tntbe01)  TNTBE000;
031300090306         endif;
031400090306         if  %found(TNTBE01L);
031500090306           xTNTBEds = TNTBEds;
031600090306         else;
031700090306           clear  xTNTBEds;
031800090306         endif;
031900071207
032000090306         // Verifica parametri ricecvuti
032100120604         if  BJDCke1  <> *blank;
032200090306           exsr  sr_InzD01;
032300090306           exsr  sr_CtrD01;
032400090306           if  errMessage = *on;
032500120604             BJDCerr = *on;
032600120604             BJDCmsg = V1Dmsg;
032700071217             $Fine  = *on;
032800071217             leavesr;
032900071217           endif;
033000071207           $Video  = 'D2';
033100071207           $InzD02 = *on;
033200071207         endif;
033300071207
033400071207       ENDSR;
033500071206
033600071212       //--------------------------------------------------------------
033700090306       //?Reperimento Dati del job (Utente/Operativi).                 ?
033800071212       //--------------------------------------------------------------
033900090306       BEGSR  DatiJob;
034000071207
034100071207         in(E) §AzUte;
034200071207         if NOT %error;
034300071207           in(E) §DatiUte;
034400071207         endif;
034500090306         if %error or RSut = *blank;
034600071207           clear TIBS34ds;
034700071207           tibs34r(tibs34ds);
034800071207           in §AzUte;
034900071207           in §DatiUte;
035000071207         endif;
035100071207
035200071207       ENDSR;
035300071206
035400071212       //--------------------------------------------------------------
035500090306       //?Gestione videata D01                                         ?
035600071212       //--------------------------------------------------------------
035700090306       BEGSR  sr_GesD01;
035800071207
035900090306         // Inizializzazione videata
036000090306         if  $InzD01 = *on;
036100090306           exsr  sr_InzD01;
036200071207           $InzD01 = *off;
036300071207         endif;
036400071207
036500090306         // Emissione testata
036600120604         write  TBJDCT01;
036700071207
036800090306         // Emissione videata
036900120604         exfmt  TBJDCD01;
037000071207         errMessage  = *off;
037100071207         errGenerico = *off;
037200090306         clear  V1Dmsg;
037300071207
037400071207         select;
037500090306           // F3=Fine
037600090306           when  dsp_aid = c_F03;
037700090306             exsr  sr_F03D01;
037800090306           // Enter
037900071207           other;
038000090306             exsr  sr_CtrD01;
038100090306             if  errGenerico;
038200071207               leavesr;
038300071207             endif;
038400071207             $Video  = 'D2';
038500071207             $InzD02 = *on;
038600071207         endsl;
038700071207
038800071207       ENDSR;
038900071206
039000071212       //--------------------------------------------------------------
039100090306       //?Inizializzazione videata D01                                 ?
039200071212       //--------------------------------------------------------------
039300090306       BEGSR  sr_InzD01;
039400071207
039500120604         clear  TBJDCD01;
039600071207
039700120604         if  BJDCke1 = *blank;
039800090306           %subst(V1Cccm : %len(V1Cccm) : 1) = '?';
039900071207         else;
040000120604           V1Cccm = %trimr(BJDCke1);
040100071207         endif;
040200071207
040300071207       ENDSR;
040400071206
040500071212       //--------------------------------------------------------------
040600090306       //?Controllo videata D01                                        ?
040700071212       //--------------------------------------------------------------
040800090306       BEGSR  sr_CtrD01;
040900071207
041000090306         IndDspF = *zero;
041100120112         reset  $Copia;
041200071207
041300071207         SELECT;
041400071207
041500120112           // - Ricerca codice cliente
041600090306           WHEN  %scan('?' : V1Cccm) > *zero;
041700120112             clear  Param01;
041800090327             P01ord = '0';
041900090327             KPJBU  = Param01;
042000120604             tntbjdc1r (KPJBA : '3');
042100090327             Param01 = KPJBU;
042200090319
042300090327             if  P01ke1 <> *zero;
042400090327               V1Cccm = P01ke1;
042500090319             endif;
042600120112             if  P01rit <> '3';
042700120112               errGenerico = *on;
042800120112               PosCurCcm   = *on;
042900120112               leavesr;
043000120112             else;
043100120112               $Copia = *on;
043200120112             endif;
043300071207
043400120112           // - Controllo immissione codice cliente
043500090306           WHEN  V1Cccm = *blank  or  V1Cccm  = *zero;
043600071207             errMessage  = *on;
043700071207             errGenerico = *on;
043800071207             PosCurCcm   = *on;
043900071207             V1Dmsg = $Msg(01);
044000071207             leavesr;
044100071207
044200120112           // - Controllo immissione solo caratteri numerici
044300090310           WHEN  %check(c_Digits : V1Cccm) > *zero;
044400071207             errMessage  = *on;
044500071207             errGenerico = *on;
044600071207             PosCurCcm   = *on;
044700071207             V1Dmsg = $Msg(02);
044800071207             leavesr;
044900071207
045000071207         ENDSL;
045100071207
045200090306         // - Controllo esistenza codice cliente in anagrafica clienti
045300120112         //   (SE non richiesta copia: il vero codice cliente verrà
045400120112         //    inserito in D02)
045500120112         If  Not  $Copia;
045600120112           clear  TIBS69ds;
045700120112           tibs69ds.I69kcc = DUTkci;
045800120112           tibs69ds.I69kac = %int(V1Cccm);
045900120112           tibs69ds.I69sif = knsif;
046000120112           tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
046100120112           if  tibs69ds.O69err = *on;
046200120112             errMessage  = *on;
046300120112             errGenerico = *on;
046400120112             PosCurCcm   = *on;
046500120112             V1Dmsg = $Msg(03);
046600120112             leavesr;
046700120112           endif;
046800120112         EndIf;
046900071207
047000090306         // - Verifica esistenza codice cliente in tabella
047100090306         reset  TNTBE000;
047200090310         k_TBEcod = c_Tab;
047300090306         k_TBEke1 = V1Cccm;
047400090306         clear  k_TBEke2;
047500090306         k_TBElin = TBXlin;
047600090306         k_TBEsif = TBXsif;
047700120112
047800120112         If  Not  $Copia;
047900120112
048000120112           chain  %kds(K05tntbe01) TNTBE000;
048100120112           // - - NON ammesso nuovo record per cliente annullato
048200120112           if  NOT  %found(TNTBE01L)  and  ds_CNACO.ACOflg <> *blank;
048300120112             errMessage  = *on;
048400120112             errGenerico = *on;
048500120112             PosCurCcm   = *on;
048600120112             V1Dmsg = $Msg(04);
048700120112             leavesr;
048800120112           endif;
048900120112
049000120112         Else;
049100120112
049200120112           chain(N)  %kds(K05tntbe01) TNTBE000;
049300120112           // - - NON ammessa copia di record inesistente
049400120112           if  NOT  %found(TNTBE01L);
049500120112             errMessage  = *on;
049600120112             errGenerico = *on;
049700120112             PosCurCcm   = *on;
049800120604             V1Dmsg = $Msg(10);
049900120112             leavesr;
050000120112           endif;
050100120112
050200120112         EndIf;
050300071207
050400120112         // - Decodifica cliente
050500090306         if  %found(TNTBE01L);
050600120604           dJDC = TBEuni;
050700120112           if  Not  $Copia;
050800120604             V1Dccm = §JDCrag;
050900120112           else;
051000120112             V1Dccm = %subst(ds_CNACO.ACOrag : 1 : %len(V1Dccm));
051100120112           endif;
051200071207         else;
051300120604           clear  dJDC;
051400090304           V1Dccm = %subst(ds_CNACO.ACOrag : 1 : %len(V1Dccm));
051500071207         endif;
051600071207
051700071207       ENDSR;
051800071206
051900071212       //--------------------------------------------------------------
052000090306       //?Gestione tasto funzionale F3 da videata D01                  ?
052100090306       //?F3=Fine                                                      ?
052200071212       //--------------------------------------------------------------
052300090306       BEGSR  sr_F03D01;
052400071207
052500090306         // Chiusura del programma
052600071207         $Fine = *on;
052700071217
052800090306         // Restituzione indicazione del tasto funzionale premuto
052900120604         BJDCfxx = '1';
053000071207
053100071207       ENDSR;
053200071206
053300071212       //--------------------------------------------------------------
053400090306       //?Gestione videata D02                                         ?
053500071212       //--------------------------------------------------------------
053600090306       BEGSR  sr_GesD02;
053700071207
053800090306         // Inizializzazione videata
053900090306         if   $InzD02 = *on;
054000090306           exsr  sr_InzD02;
054100090306           $InzD02 = *off;
054200071207         endif;
054300071207
054400090306         // Emissione testata
054500120604         write  TBJDCT01;
054600071207
054700090306         // Emissione videata
054800120604         if  BJDCopz = *blank;
054900120604           exfmt  TBJDCD02;
055000071217         else;
055100120604           write  TBJDCD02;
055200090306           exfmt  PROTECT;
055300071217         endif;
055400071207         errMessage  = *off;
055500071207         errGenerico = *off;
055600090306         clear  V1Dmsg;
055700071207
055800071207         SELECT;
055900090306           // F3=Fine
056000090306           WHEN  dsp_aid = c_F03;
056100090306             exsr  sr_F03D01;
056200090319           // F04=Interrogazione Padre
056300090319           WHEN  dsp_aid = c_F04;
056400090319             exsr  sr_F04D02;
056500090306           // F12=Ritorno
056600090306           WHEN  dsp_aid = c_F12;
056700090306             exsr  sr_F12D02;
056800090306           // Enter o F5 o F6 o F16
056900071207           OTHER;
057000090306             // - Controllo dati
057100090306             if  dsp_aid = c_F16;
057200090306               exsr  sr_CtrAnnull;
057300090306             else;
057400090306               exsr  sr_CtrD02;
057500090306             endif;
057600090306             if  errGenerico;
057700071207               leavesr;
057800071207             endif;
057900090306             // - Conferma dati:
058000090306             //   F5=Ripristino; F6=Conferma; F16=Annullamento
058100090306             if  dsp_aid = c_F05  or
058200090306                 dsp_aid = c_F06  or
058300090306                 dsp_aid = c_F16;
058400090306               exsr  Upd_TNTBE;
058500071207               if  NOT  errGenerico;
058600071207                 clear V1Tlav;
058700071207                 $Video  = 'D1';
058800090306                 //$InzD01 = *on;
058900071207               endif;
059000071207             EndIf;
059100071207
059200071207         ENDSL;
059300071207
059400071207       ENDSR;
059500071206
059600071212       //--------------------------------------------------------------
059700090306       //?Inizializzazione videata D01                                 ?
059800071212       //--------------------------------------------------------------
059900090306       BEGSR  sr_InzD02;
060000071207
060100090313         reset  $ByPass;
060200121016         reset  wForza;
060300121030         reset  wForzaTPI;
060400121030         reset  wForzaFMI;
060500121030         reset  wForzaDIR;
060600071207
060700120604         clear  TBJDCW01;
060800101103
060900120604         clear  TBJDCD02;
061000071210
061100090306         IndDspF = *zero;
061200090309
061300090309         // Impostazione indicatori per abilitazione tasti funzionali
061400090309         exsr  Set_Ind_D02;
061500090309
061600120112         // - Codice estrazione (chiave)
061700120112         if  Not  $Copia;
061800120112           V2Cccm = %int(V1Cccm);
061900120112           V2Dccm = V1Dccm;
062000120112         endif;
062100071207
062200090306         IF  %found(TNTBE01L);
062300071207
062400090306           // Impostazione dati di record trovato
062500071217           select;
062600120604             when  BJDCopz = '5';
062700071217               V1Tlav = 'VISUALIZZA';
062800120112             when  $Copia;
062900120112               V1Tlav = '  COPIA   ';
063000090306             when  TBEatb = *blank;
063100071217               V1Tlav = 'VARIAZIONE';
063200071217             other;
063300071217               V1Tlav = 'ANNULLATO ';
063400071217           endsl;
063500090225
063600090306           // Codice padre
063700120604           V2Cksu = §JDCksu;
063800090309           clear  TIBS69ds;
063900090309           clear  ds_CNACO;
064000090309           clear  ds_CNIND;
064100090309           clear  ds_CNCLP;
064200090309           clear  ds_FNCLS;
064300090310           select;
064400120604             when  §JDCksu =  V2Cccm;
064500120606               V2Dksu = V2Dccm;
064600120604             when  §JDCksu <> *zero;
064700090310               tibs69ds.I69kcc = DUTkci;
064800090310               tibs69ds.I69kac = %int(V2Cksu);
064900090310               tibs69ds.I69sif = knsif;
065000090310               tibs69r(TIBS69ds:ds_CNACO:ds_CNIND:ds_CNCLP:ds_FNCLS);
065100090310               if tibs69ds.O69err = *on;
065200090310                 V1Dmsg = $Msg(03);
065300090310                 errMessage  = *on;
065400090310                 errGenerico = *on;
065500090310                 PosCurKsu   = *on;
065600090310                 leavesr;
065700090310               endif;
065800120606               V2Dksu = ds_CNACO.ACOrag;
065900090310           endsl;
066000090225
066100120604           V2Caut   = §JDCaut;
066200120604           V2Ctpi   = §JDCtpi;
066300090225
066400120605           // Flag nome documento
066500120604           V2Cfmi = §JDCfmi;
066600090310           reset  TIBS02ds;
066700090303           T02cod = 'NIM';
066800120604           T02ke1 = §JDCfmi;
066900090304           TNTBE_RicercaControllo(kpjba : tibs02ds);
067000090306           if  T02err = *blank;
067100090303             V2Dfmi = T02uni;
067200090303           endif;
067300121016
067400121016           V2merge  = §JDCmerge;
067500090225
067600120607           V2Cpag   = §JDCpag;
067700120604           V2Cdir   = §JDCdir;
067800120604           V2note   = §JDCnote;
067900120605           v2tadu   = §JDCtadu;
068000120608           data_eur = %date(§JDCdti:*iso);
068100120608           V2Cdti   = %dec(data_eur);
068200120608           data_eur = %date(§JDCdtf:*iso);
068300120608           V2Cdtf   = %dec(data_eur);
068400090923
068500090923           *in02 = *off;
068600071207
068700071207         ELSE;
068800071207
068900090306           // Impostazione dati di record nuovo
069000071207           V1Tlav = 'IMMISSIONE';
069100090226           V2Cdir = %editc(V2Cccm:'X');
069200120605
069300120605         //?FORZO frequenza addebito MENSILE (in questo modo avrà id job mensile)
069400120605           v2tadu   = 'M';
069500090923
069600090923           *in02 = *on;
069700071207
069800071207         ENDIF;
069900071207
070000071207       ENDSR;
070100071217
070200071217       //--------------------------------------------------------------
070300090306       //?Settaggio indicatori nella videata D02 per abilitazione      ?
070400090306       //?  tasti funzionali                                           ?
070500071217       //--------------------------------------------------------------
070600090306       BEGSR  Set_Ind_D02;
070700090319
070800120606         F4Attivo  = *off;
070900090306         F5Attivo  = ((%found(TNTBE01L) and TBEatb <> *blank)
071000120604                                        and BJDCopz <> '5');
071100120604         F6Attivo  = (NOT F5Attivo and BJDCopz <> '5');
071200090306         F16Attivo = ((%found(TNTBE01L) and TBEatb = *blank)
071300120604                                        and BJDCopz <> '5');
071400120112
071500120112         // - Protezione campo chiave SE NON copia
071600120112         InserCCM2 = $Copia;
071700071217
071800071217       ENDSR;
071900090319
072000090319       //--------------------------------------------------------------
072100090319       //?Gestione tasto funzionale F04 da videata D02                 ?
072200090319       //?F04=Interrogazione Padre                                     ?
072300090319       //--------------------------------------------------------------
072400090319       BEGSR  sr_F04D02;
072500090319
072600090319         // Richiamo pgm interrogazione
072700090327         clear  Param01;
072800090327         P01ord = '1';
072900090327         P01ksu = V2Cksu;
073000090327         KPJBU  = Param01;
073100120604         tntbjdc1r (KPJBA);
073200120607         Param01 = KPJBU;
073300120607         IF  P01ke1 <> *zero;
073400120607           V2Cksu = P01ksu;
073500120607         ENDIF;
073600090319
073700090319       ENDSR;
073800090319
073900071217
074000071217       //--------------------------------------------------------------
074100090306       //?Gestione tasto funzionale F12 da videata D02                 ?
074200090306       //?F12=Ritorno                                                  ?
074300071217       //--------------------------------------------------------------
074400090306       BEGSR  sr_F12D02;
074500071217
074600090306         // Ritorno alla videata precedente se NON richiamato
074700120604         if  BJDCopz = *blank  and
074800120604             BJDCsif = *blank  and  BJDClin = *blank  and
074900120604             BJDCke1 = *blank  and  BJDCke2 = *blank;
075000071217           clear V1Tlav;
075100071217           $Video = 'D1';
075200090306         // Ritorno al pgm chiamante se richiamato
075300071217         else;
075400120604           BJDCfxx = '2';
075500071217           $Fine = *on;
075600071217         endif;
075700071217
075800071217       ENDSR;
075900090306
076000090306       //--------------------------------------------------------------
076100090306       //?Controllo "annullabilità" del record (padre senza figli)     ?
076200090306       //--------------------------------------------------------------
076300090306       BEGSR  sr_CtrAnnull;
076400090306
076500090306         IndDspF = *zero;
076600090306         // REimpostazione indicatori per abilitazione tasti funzionali
076700090306         exsr  Set_Ind_D02;
076800090306
076900090306         if  V2Cksu <= *zero;
077000120604           V2Cksu = §JDCksu;
077100090306         endif;
077200090306
077300090306         // Se figlio (non padre): fine controlli
077400090306         if  V2Cccm <> V2Cksu;
077500090306           leavesr;
077600090306         endif;
077700090306
077800090306         //?Verifica se padre con figli?
077900120604         clear  dJDCp;
078000090316         SAVke1 = TBEke1;
078100090306         setll     %kds(k05tntbe01 : 1)  TNTBE000;
078200090306         reade(n)  %kds(k05tntbe01 : 1)  TNTBE000;
078300090306
078400090306         dow  not  %eof(TNTBE01L);
078500090316           if  TBEatb = *blank   and
078600090316               %editc(V2Cccm : 'X') <> %trimr(TBEke1);
078700120604             dJDCp = TBEuni;
078800120604             if  p_§JDCksu = V2Cccm;
078900090306               errMessage  = *on;
079000090306               errGenerico = *on;
079100090306               PosCurKsu   = *on;
079200120608               V1Dmsg = $Msg(12);
079300120608               %subst(V1Dmsg:60:7) = %subst(TBEke1:1:7);
079400090306               leavesr;
079500090306             endif;
079600090306           endif;
079700090306         reade(n)  %kds(k05tntbe01 : 1)  TNTBE000;
079800090306         enddo;
079900090316
080000120112         // - Riaggancio del rec. in manutenzione/copia
080100090316         TBEke1 = SAVke1;
080200090316         reset  TNTBE000;
080300090316         k_TBEcod = c_Tab;
080400120112         k_TBEke1 = %editc( V2Cccm : 'X' );
080500090316         clear  k_TBEke2;
080600090316         k_TBElin = TBXlin;
080700090316         k_TBEsif = TBXsif;
080800090316         chain  %kds(K05tntbe01) TNTBE000;
080900090306
081000090306       ENDSR;
081100071206
081200071212       //--------------------------------------------------------------
081300090306       //?Controllo videata D02                                        ?
081400071212       //--------------------------------------------------------------
081500090306       BEGSR  sr_CtrD02;
081600071207
081700090309         %subst(IndDspF : 50) = *off;
081800120112
081900120112         IF  $Copia;
082000120112
082100120112           V1Cccm = %editc( V2Cccm : 'X' );
082200120112
082300120112           Select;
082400120112
082500120112             // -?Codice estrazione (SE copia)?
082600120112             When  V2Cccm = *zero;
082700120112               errMessage  = *on;
082800120112               errGenerico = *on;
082900120112               PosCurCcm2  = *on;
083000120112               V1Dmsg = $Msg(01);
083100120112               leavesr;
083200120112
083300120112             // -?Controllo / Decodifica?
083400120112             When  tibs69ds.I69kac <> V2Cccm  or
083500120112                   k_TBEke1 <> %editc( V2Cccm : 'X' );
083600120112               clear  TIBS69ds;
083700120112               tibs69ds.I69kcc = DUTkci;
083800120112               tibs69ds.I69kac = V2Cccm;
083900120112               tibs69ds.I69sif = knsif;
084000120112               tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
084100120112               if  tibs69ds.O69err = *on;
084200120112                 errMessage  = *on;
084300120112                 errGenerico = *on;
084400120112                 PosCurCcm2  = *on;
084500120112                 V1Dmsg = $Msg(03);
084600120112                 leavesr;
084700120112               endif;
084800120112               V2Dccm = ds_CNACO.ACOrag;
084900120112               // -?Verifica esistenza in tabella?
085000120112               k_TBEke1 = %editc( V2Cccm : 'X' );
085100120112               chain  %kds(K05tntbe01) TNTBE000;
085200120112               select;
085300120112                 // - -?NON ammesso nuovo record per cliente annullato?
085400120112                 when  NOT  %found(TNTBE01L)  and  ds_CNACO.ACOflg <> *blank;
085500120112                   errMessage  = *on;
085600120112                   errGenerico = *on;
085700120112                   PosCurCcm2  = *on;
085800120112                   V1Dmsg = $Msg(04);
085900120112                   leavesr;
086000120112                 // - -?Impossibile inserimento di record già esistente?
086100120112                 when  %found(TNTBE01L);
086200120112                   errMessage  = *on;
086300120112                   errGenerico = *on;
086400120112                   PosCurCcm2  = *on;
086500120604                   V1Dmsg = $Msg(11);
086600120112                   leavesr;
086700120112               endsl;
086800120112
086900120112           EndSl;
087000120112
087100120112         ENDIF;
087200090225
087300090306         //?Codice padre?
087400120606         V2Cksu = V2Cccm;
087500090225         select;
087600090306           when  V2Cksu  = *zero;
087700090306             errMessage  = *on;
087800090225             errGenerico = *on;
087900090303             PosCurKsu   = *on;
088000120604             V1Dmsg = $Msg(08);
088100090225             leavesr;
088200090306           when  V2Cksu = V2Cccm;
088300120606             V2Dksu = V2Dccm;
088400090225           other;
088500090327             // - Controllo esistenza codice padre come cliente
088600120604             //   in tabella JDC
088700090303             exsr  sr_Padre;
088800090303             if  errGenerico = *on;
088900090226               leavesr;
089000090225             endif;
089100120606             V2Dksu = p_§JDCrag;
089200090226         endsl;
089300101103         //?SE codice padre <> da codice cliente?
089400101103         // ?& sono in immissione?
089500101103         IF  V2Cccm <> V2Cksu   and   *in02;
089600101103           // ?=> chiedo se si vogliono riportare i dati del padre?
089700101103           If  W1Ccpy = *blank;
089800101103             $Video  = 'W1';
089900101103             leavesr;
090000101103           EndIf;
090100100521         ENDIF;
090200090311
090300090311         //?Tipo elaborazione?
090400090327           // - Obbligatorio
090500120605           IF  V2Caut <> 'A'  and  V2Caut <> 'N';
090600090327             errMessage  = *on;
090700090327             errGenerico = *on;
090800090327             PosCurAut   = *on;
090900120608             V1Dmsg = $Msg(13);
091000090327             leavesr;
091100120604         endIF;
091200090327
091300120605         //?Tipo documento
091400120917         if  V2Ctpi <> 'TIF' and V2Ctpi <> 'PDF';
091500090327           errMessage  = *on;
091600090327           errGenerico = *on;
091700090327           PosCurTpi   = *on;
091800120608           V1Dmsg = $Msg(14);
091900090327           leavesr;
092000090327         endif;
092100090226
092200120605         //?Flag nome documento
092300090310         clear  V2Dfmi;
092400090310         select;
092500090310           // - Obbligatorietà
092600090310           when  V2Cfmi = *blank;
092700090310             errMessage  = *on;
092800090310             errGenerico = *on;
092900090310             PosCurFmi   = *on;
093000120608             V1Dmsg = $Msg(15);
093100090310             leavesr;
093200090310           // - Ricerca
093300090310           when  %scan('?' : V2Cfmi) > *zero;
093400090310             clear  TIBS02ds;
093500090310             T02mod = 'R';
093600090310             T02cod = 'NIM';
093700090310             T02sif = KNSIF;
093800090310             TNTBE_RicercaControllo(kpjba : tibs02ds);
093900090310             if  T02err = *blank;
094000090310               V2Cfmi = %subst(T02ke1 : 1 : %len(V2Cfmi));
094100090310               V2Dfmi = T02uni;
094200090310               errGenerico = *on;
094300090310               leavesr;
094400090310             else;
094500090310               errMessage  = *on;
094600090310               errGenerico = *on;
094700090310               PosCurFmi   = *on;
094800090310               V1Dmsg = T02msg;
094900090310               leavesr;
095000090310             endif;
095100090310           // - Controllo
095200090310           other;
095300090310             reset  TIBS02ds;
095400090310             T02cod = 'NIM';
095500090310             T02ke1 = V2Cfmi;
095600090310             TNTBE_RicercaControllo(kpjba : tibs02ds);
095700090310             if  T02err <> *blank;
095800090310               errMessage  = *on;
095900090310               errGenerico = *on;
096000090310               PosCurFmi   = *on;
096100120608               V1Dmsg = $Msg(15);
096200090310               leavesr;
096300090310             endif;
096400090310             V2Dfmi = T02uni;
096500090310         ENDSL;
096600120606
096700120606         //?Tipo invio
096800120607         SELECT;
096900120607           WHEN  V2Cpag  = *blanks;
097000120607             ErrMessage  = *on;
097100120607             ErrGenerico = *on;
097200120606             PosCurPag   = *on;
097300120608             V1Dmsg = $Msg(16);
097400120607             leavesr;
097500120607           WHEN  V2Cpag  <> 'CLI' and V2Cpag <> 'CL1';
097600120607             ErrMessage  = *on;
097700120607             ErrGenerico = *on;
097800120607             PosCurPag   = *on;
097900120608             V1Dmsg = $Msg(16);
098000120607             leavesr;
098100120607         ENDSL;
098200090226
098300120605         //?Directory per documenti
098400090226         select;
098500090306           when  V2Cdir  = *blank;
098600090226             errMessage  = *on;
098700090226             errGenerico = *on;
098800090226             PosCurDir   = *on;
098900090226             V1Dmsg = $Msg(05);
099000090226             leavesr;
099100090316           when  %subst(V2Cdir : 1 : 1) = *blank;
099200090316             errMessage  = *on;
099300090316             errGenerico = *on;
099400090316             PosCurDir   = *on;
099500120608             V1Dmsg = $Msg(17);
099600090316             leavesr;
099700090306           when  %scan('\':V2Cdir) > *zero;
099800090226             errMessage  = *on;
099900090226             errGenerico = *on;
100000090226             PosCurDir   = *on;
100100090226             V1Dmsg = $Msg(06);
100200090226             leavesr;
100300090306           when  %scan('%':V2Cdir) > *zero   and
100400090306                           V2Caut  = 'A';
100500090226             errMessage  = *on;
100600090226             errGenerico = *on;
100700090226             PosCurDir   = *on;
100800090226             V1Dmsg = $Msg(07);
100900090226             leavesr;
101000090309           when  %scan('&':V2Cdir) > *zero  and
101100090309                           V2Caut  = 'A';
101200090309             errMessage  = *on;
101300090309             errGenerico = *on;
101400090309             PosCurDir   = *on;
101500090309             V1Dmsg = $Msg(07);
101600090309             leavesr;
101700090226         endsl;
101800090309
101900090316         wPos1 = %scan('/' : V2Cdir);
102000090316         DoW  wPos1 > *zero;
102100090316           if  %subst( V2Cdir : wPos1 : 2 ) = '//';
102200090316             errMessage  = *on;
102300090316             errGenerico = *on;
102400090316             PosCurDir   = *on;
102500120608             V1Dmsg = $Msg(18);
102600090316             leavesr;
102700090316           else;
102800090316             wPos1 = %scan('/' : V2Cdir : wPos1 + 1 );
102900090316           endif;
103000090316         EndDo;
103100090316
103200090309         wPos1 = %scan('&' : V2Cdir);
103300090309         IF  wPos1 > *zero;
103400090309           if  %subst( V2Cdir : wPos1 : 4 ) <> '&AAA'   and
103500090309               %subst( V2Cdir : wPos1 : 2 ) <> '&M';
103600090309             errMessage  = *on;
103700090309             errGenerico = *on;
103800090309             PosCurDir   = *on;
103900120608             V1Dmsg = $Msg(19);
104000090309             leavesr;
104100090309           endif;
104200090309           wPos2 = %scan( '&' : V2Cdir : wPos1 + 1 );
104300090309           If  wPos2 > *zero;
104400090309             if  %subst( V2Cdir : wPos2 : 4 ) <> '&AAA'   and
104500090309                 %subst( V2Cdir : wPos2 : 2 ) <> '&M';
104600090309               errMessage  = *on;
104700090309               errGenerico = *on;
104800090309               PosCurDir   = *on;
104900120608               V1Dmsg = $Msg(19);
105000090309               leavesr;
105100090309             endif;
105200090309           EndIf;
105300090309         ENDIF;
105400090309
105500090309         wPos1 = %scan('&M' : V2Cdir);
105600090309         IF  wPos1 > *zero;
105700090309           wPos2 = %scan( '&M' : V2Cdir : wPos1 + 1 );
105800090309           If  wPos2 > *zero   and
105900090309               %scan( '&M' : V2Cdir : wPos2 + 1 ) > *zero;
106000090309             errMessage  = *on;
106100090309             errGenerico = *on;
106200090309             PosCurDir   = *on;
106300120608             V1Dmsg = $Msg(20);
106400090309             leavesr;
106500090309           EndIf;
106600090309         ENDIF;
106700120607
106800120607         // Se FTP la directory deve finire con /DI
106900121016         // se non c'è il flag di Merge, in questo caso c'è il controllo
107000121016         // nella route CTR_merge
107100121016         IF  V2Caut = 'A' and V2merge = *blanks;
107200120607           wPos1 = %scan('  ':V2Cdir);
107300120607           wPos1 -= 3;
107400120607           IF  %subst(V2Cdir:wPos1:3) <> '/DI';
107500120607             ErrMessage  = *on;
107600120607             ErrGenerico = *on;
107700120607             PosCurDir   = *on;
107800120608             V1Dmsg = $Msg(21);
107900120607             leavesr;
108000120607           ENDIF;
108100120607         ENDIF;
108200090226
108300090327         // - Se codice padre <> da codice cliente
108400090306         //   la directory deve coincidere almeno fino alla 2ª subdir
108500120604         IF  V2Cccm <> V2Cksu   and   p_§JDCdir <> V2Cdir;
108600090311
108700090311           // - Controllo 1ª directory
108800090311           wPos1 = %scan( '/' : %trim( V2Cdir ) );
108900120604           wPos2 = %scan( '/' : %trim( p_§JDCdir ) );
109000090313           If  wPos1 <> wPos2                               OR
109100090313             ( wPos1  > *zero   and   wPos2 > *zero   and
109200090311               %subst( %trim( V2Cdir    ) : 1 : wPos1 ) <>
109300120604               %subst( %trim( p_§JDCdir ) : 1 : wPos2 ) )   OR
109400090313             ( wPos1  = *zero   and   wPos2  =  *zero
109500120604                                and   V2Cdir <> p_§JDCdir );
109600090311             errMessage  = *on;
109700090311             errGenerico = *on;
109800090311             PosCurDir   = *on;
109900090311             select;
110000090311               when  wPos1 =  *zero   and   wPos2 <> *zero;
110100090316                 V1Dmsg = 'Il padre ha altri rami directory (';
110200090311               when  wPos1 <> *zero   and   wPos2 =  *zero;
110300090311                 V1Dmsg = 'Il padre ha una directory unica (';
110400090311               other;
110500090311                 V1Dmsg = 'Primo ramo directory +
110600090311                           <> da quello del cod. padre (';
110700090311             EndSl;
110800120604             if  %len( %trim( p_§JDCdir ) ) <=
110900090311                 %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) ) - 1;
111000090311               V1Dmsg = %trimr( V1Dmsg )
111100120604                      + %trim( p_§JDCdir ) + ')';
111200090311             else;
111300090313               wPos1 = %len( V1Dmsg );
111400090313               wPos2 = %len( %trimr( V1Dmsg ) );
111500090311               V1Dmsg = %trimr( V1Dmsg )
111600120604                      + %trim( %subst( p_§JDCdir : 1 :
111700090311                          %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) )
111800090311                          - 4 ) )
111900090311                      + '...)';
112000090311             endif;
112100090311             leavesr;
112200090311           EndIf;
112300090311
112400090311           // - Controllo 2ª directory
112500090311           wPos1 = %scan( '/' : %trim( V2Cdir ) : wPos1 + 1 );
112600120604           wPos2 = %scan( '/' : %trim( p_§JDCdir ) : wPos2 + 1 );
112700090316           //If  wPos1 <> wPos2                               OR
112800090316           //  ( wPos1  > *zero   and   wPos2 > *zero   and
112900090316           //    %subst( %trim( V2Cdir    ) : 1 : wPos1 ) <>
113000120604           //    %subst( %trim( p_§JDCdir ) : 1 : wPos2 ) )   OR
113100090316           //  ( wPos1  = *zero   and   wPos2  =  *zero
113200120604           //                     and   V2Cdir <> p_§JDCdir );
113300090316           if  wPos1 = *zero;
113400090316             wPos1 = %len( %trimr( V2Cdir ) );
113500090316           else;
113600090316             wPos1 -= 1;
113700090316           endif;
113800090316           if  wPos2 = *zero;
113900120604             wPos2 = %len( %trimr( p_§JDCdir ) );
114000090316           else;
114100090316             wPos2 -= 1;
114200090316           endif;
114300090316           If  wPos1 <> wPos2   OR
114400090316               %subst(V2Cdir : 1 : wPos1) <>
114500120604                 %subst(p_§JDCdir : 1 : wPos2);
114600090311             errMessage  = *on;
114700090311             errGenerico = *on;
114800090311             PosCurDir   = *on;
114900090316             //select;
115000090316             //  when  wPos1 =  *zero   and   wPos2 <> *zero;
115100090316             //    V1Dmsg = 'Il padre ha altri rami directory (';
115200090316             //  when  wPos1 <> *zero   and   wPos2 =  *zero;
115300090316             //    V1Dmsg = 'Il padre ha meno rami directory (';
115400090316             //  other;
115500090313                 V1Dmsg = 'Secondo ramo directory +
115600090311                           <> da quello del cod. padre (';
115700090316             //EndSl;
115800120604             if  %len( %trim( p_§JDCdir ) ) <=
115900090311                 %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) ) - 1;
116000090311               V1Dmsg = %trimr( V1Dmsg )
116100120604                      + %trim( p_§JDCdir ) + ')';
116200090311             else;
116300090311               V1Dmsg = %trimr( V1Dmsg )
116400120604                      + %trim( %subst( p_§JDCdir : 1 :
116500090311                          %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) )
116600090311                          - 4 ) )
116700090311                      + '...)';
116800090311             endif;
116900090311             leavesr;
117000090311           EndIf;
117100090311
117200090306         ENDIF;
117300121016
117400121016         //?Immagine con Merge (LDV + DOC)
117500121016         IF  V2merge = 'S';
117600121016           exsr CTR_merge;
117700121016         ENDIF;
117800121016         IF  ErrGenerico;
117900121016           leavesr;
118000121016         ENDIF;
118100120608
118200120608         //?Data inizio e fine scansione documenti
118300120608         IF  V2Cdti  = *zeros;
118400120608           ErrMessage  = *on;
118500120608           ErrGenerico = *on;
118600120608           PosCurDti   = *on;
118700120608           V1Dmsg      = $Msg(22);
118800120608           leavesr;
118900120608         ENDIF;
119000120608         IF  V2Cdtf  = *zeros;
119100120608           ErrMessage  = *on;
119200120608           ErrGenerico = *on;
119300120608           PosCurDtf   = *on;
119400120608           V1Dmsg      = $Msg(22);
119500120608           leavesr;
119600120608         ENDIF;
119700120608         IF  V2Cdti  > *zeros;
119800120608           clear wlbdat;
119900120608           G02dat = V2Cdti;
120000120608           xsrda8(wlbdat);
120100120608           IF  G02err = '1';
120200120608             ErrMessage  = *on;
120300120608             ErrGenerico = *on;
120400120608             PosCurDti   = *on;
120500120608             V1Dmsg      = $Msg(22);
120600120608             leavesr;
120700120608           ENDIF;
120800120608           V2Cdti = G02dat;
120900120608           wdatai = G02inv;
121000120608         ENDIF;
121100120608         IF  V2Cdtf  > *zeros;
121200120608           clear wlbdat;
121300120608           G02dat = V2Cdtf;
121400120608           xsrda8(wlbdat);
121500120608           IF  G02err = '1';
121600120608             ErrMessage  = *on;
121700120608             ErrGenerico = *on;
121800120608             PosCurDtf   = *on;
121900120608             V1Dmsg      = $Msg(22);
122000120608             leavesr;
122100120608           ENDIF;
122200120608           V2Cdtf = G02dat;
122300120608           wdataf = G02inv;
122400120608         ENDIF;
122500120608         IF  wdatai > wdataf;
122600120608           ErrMessage  = *on;
122700120608           ErrGenerico = *on;
122800120608           PosCurDti   = *on;
122900120608           V1Dmsg = $Msg(23);
123000120608           leavesr;
123100120608         ENDIF;
123200071207
123300071207       ENDSR;
123400090225
123500090225       //--------------------------------------------------------------
123600120604       //?Controllo esistenza del codice padre in tabella JDC          ?
123700090225       //--------------------------------------------------------------
123800090303       BEGSR  sr_Padre;
123900090302
124000120604         // Controlla esistenza in tabella "JDC"
124100120604         clear  dJDCp;
124200090310         reset  TIBS02ds;
124300090310         T02sif = KNSIF;
124400090310         %subst(T02ke1 : 1 : 7) = %editc(V2Cksu : 'X');
124500090310         TNTBE_RicercaControllo(kpjba : tibs02ds);
124600090310         if  T02err = *blank;
124700120604           dJDCp  = T02uni;
124800090310         else;
124900090310           errMessage  = *on;
125000090310           errGenerico = *on;
125100090310           PosCurksu   = *on;
125200120604           V1Dmsg = $Msg(09);
125300090310           leavesr;
125400090310         endif;
125500090327
125600090327         // Controlla se già inserito come figlio di altro padre
125700120604         if  p_§JDCksu <> V2Cksu;
125800090327           errMessage  = *on;
125900090327           errGenerico = *on;
126000090327           PosCurKsu   = *on;
126100090327           V1Dmsg = 'Non ammesso cod. PADRE già FIGLIO di altro +
126200120604                     cliente (' + %editc(p_§JDCksu : 'X') + ')';
126300090327           leavesr;
126400090327         endif;
126500090225
126600090225       ENDSR;
126700101103
126800101103       //--------------------------------------------------------------
126900101103       //?Gestione window W01                                          ?
127000101103       //--------------------------------------------------------------
127100101103       BEGSR  sr_GesW01;
127200101103
127300101103         // -?Emissione window?
127400120604         exfmt  TBJDCW01;
127500101103
127600101103         errMessage  = *off;
127700101103         errGenerico = *off;
127800101103         clear  V1Dmsg;
127900101103
128000101103         $Video = 'D2';
128100101103
128200101103         select;
128300101103           // -?F12=Ritorno?
128400101103           when  dsp_aid = c_F12;
128500101103             leavesr;
128600101103           // -?Invio con "S"?
128700101103           when  W1Ccpy = 'S';
128800101103             if  V2Caut = *blank;
128900120604               V2Caut   = p_§JDCaut;
129000101103             endif;
129100101103             if  V2Ctpi = *blank;
129200120604               V2Ctpi   = p_§JDCtpi;
129300101103             endif;
129400101103             if  V2Cfmi = *blank;
129500120604               V2Cfmi   = p_§JDCfmi;
129600101103               clear  V2Dfmi;
129700101103             endif;
129800121016             if  V2merge = *blank;
129900121016               V2merge  = p_§JDCmerge;
130000121016             endif;
130100101103             //if  V2Cdir = *blank;      //? Directory ?
130200120604               V2Cdir   = p_§JDCdir;     //? COMUNQUE  ?
130300101103             //endif;                    //? del padre ?
130400101103             if  V2note = *blank;
130500120604               V2note   = p_§JDCnote;
130600101103             endif;
130700120605             if  V2tadu = *blank;
130800120605               V2tadu   = p_§JDCtadu;
130900120605             endif;
131000120607             if  V2cpag = *blank;
131100120607               V2cpag   = p_§JDCpag;
131200120607             endif;
131300120608             if  V2cdti = *zeros;
131400120608               data_eur = %date(p_§JDCdti:*iso);
131500120608               V2Cdti   = %dec(data_eur);
131600120608             endif;
131700120608             if  V2cdtf = *zeros;
131800120608               data_eur = %date(p_§JDCdtf:*iso);
131900120608               V2Cdtf   = %dec(data_eur);
132000120608             endif;
132100101103         endsl;
132200101103
132300101103       ENDSR;
132400121016
132500121016       //--------------------------------------------------------------
132600121016       //?Controllo Merge.
132700121016       //--------------------------------------------------------------
132800121016       BEGSR  CTR_merge;
132900121016
133000121016       // -?L'immagine se con Merge deve essere di tipo PDF
133100121016         IF  V2Ctpi <> 'PDF';
133200121016           errMessage  = *on;
133300121016           errGenerico = *on;
133400121016           V1Dmsg = 'Immagine con Merge, deve essere di tipo PDF';
133500121016           PosCurTpi = *on;
133600121016           leavesr;
133700121016         ENDIF;
133800121016
133900121016       // -?La directory deve finire con /DF
134000121016         wPos1 = %scan('  ':V2Cdir);
134100121016         wPos1 -= 3;
134200121016         IF  %subst(V2Cdir:wPos1:3) <> '/DF';
134300121016           ErrMessage  = *on;
134400121016           ErrGenerico = *on;
134500121016           PosCurDir   = *on;
134600121016           V1Dmsg = 'Immagine con Merge, l''ultimo ramo della +
134700121016                     DIR deve essere "DF"';
134800121016           leavesr;
134900121016         ENDIF;
135000121016
135100121016       // -?Cerco il corrispondente in LAC
135200121016         clear  TIBS02DS;
135300121016         T02mod = 'C';
135400121016         T02cod = 'LAC';
135500121016         T02ke1 = %editc(V2Cccm:'X');
135600121016         TNTBE_RicercaControllo(kpjba : tibs02ds);
135700121016         // -?Non trovo la tabella
135800121016         IF  T02err <> *blanks;
135900121016           // -?Immissione forzo
136000121016           IF  *in02 and not wForza;
136100121016             wForza = *on;
136200121016             ErrMessage  = *on;
136300121016             ErrGenerico = *on;
136400121016             V1Dmsg = 'Ricordarsi di attivare anche la corrispondente +
136500121016                       tabella LAC. Enter forza.';
136600121016             leavesr;
136700121016           ENDIF;
136800121016         // -?Manutenzione blocco
136900121016           IF  not *in02;
137000121016             ErrMessage  = *on;
137100121016             ErrGenerico = *on;
137200121016             V1Dmsg = 'Manca la corrispondente tabella LAC';
137300121016             leavesr;
137400121016           ENDIF;
137500121016         // -?Trovo la tabella
137600121016         ELSE;
137700121016           dLAC = T02uni;
137800121016         // -?Controllo del formato documento
137900121030           IF  V2Ctpi <> §LACtpi and not wForzaTPI;
138000121030             wForzaTPI = *on;
138100121016             ErrMessage  = *on;
138200121016             ErrGenerico = *on;
138300121016             PosCurTpi   = *on;
138400121016             V1Dmsg = 'Formato documento diverso dalla corrispondente +
138500121016                       tabella LAC --> ' + §LACtpi;
138600121016             leavesr;
138700121016           ENDIF;
138800121016         // -?Controllo il nome immagine
138900121030           IF  V2Cfmi <> §LACfmi and not wForzaFMI;
139000121030             wForzaFMI = *on;
139100121016             ErrMessage  = *on;
139200121016             ErrGenerico = *on;
139300121016             PosCurFmi   = *on;
139400121016             V1Dmsg = 'Nome immagine diverso dalla corrispondente +
139500121016                       tabella LAC --> ' + §LACfmi;
139600121016             leavesr;
139700121016           ENDIF;
139800121016         // -?Controllo la directory
139900121016           wPos2 = %scan('  ':§LACdir);
140000121016           wPos2 -= 3;
140100121016           IF  %subst(V2Cdir:1:wPos1) <>
140200121030               %subst(§LACdir:1:wPos2) and not wForzaDIR;
140300121030             wForzaDIR = *on;
140400121016             ErrMessage  = *on;
140500121016             ErrGenerico = *on;
140600121016             PosCurDir   = *on;
140700121016             V1Dmsg = 'Directory diversa dalla corrispondente +
140800121016                       tabella LAC --> ' + %subst(§LACdir:1:wPos2);
140900121016             leavesr;
141000121016           ENDIF;
141100121016         ENDIF;
141200121016
141300121016       ENDSR;
141400071206
141500071212       //--------------------------------------------------------------
141600120604       //?Aggiornamento record TNTBE00F (tab. JDC)                     ?
141700071212       //--------------------------------------------------------------
141800090306       BEGSR  Upd_TNTBE;
141900071207
142000120604         clear  dJDC;
142100120604         §JDCrag   = V2Dccm;
142200120604         §JDCdir   = V2Cdir;
142300120604         §JDCtpi   = V2Ctpi;
142400120604         §JDCfmi   = V2Cfmi;
142500121016         §JDCmerge = V2merge;
142600120604         §JDCaut   = V2Caut;
142700120604         §JDCnote  = V2note;
142800120604         §JDCksu   = V2Cksu;
142900120605         §JDCtadu  = V2tadu;
143000120607         §JDCpag   = V2Cpag;
143100120608         §JDCdti   = wdatai;
143200120608         §JDCdtf   = wdataf;
143300071207
143400120604         TBEuni    = dJDC;
143500071207
143600090306         clear  TBEftt;
143700090306         clear  TBEftr;
143800071207
143900071207         select;
144000090306           // F5=Ripristino
144100090306           when  dsp_aid = c_F05;
144200090306             clear  TBEatb;
144300090306           // F16=Annullamento
144400090306           when  dsp_aid = c_F16;
144500071207             TBEatb = 'A';
144600071207         endsl;
144700071207
144800071207         IF  NOT  %found(TNTBE01L);
144900090306           clear  TBEatb;
145000090306           clear  TBEflt;
145100090306           clear  TBEftr;
145200090306           //clear  TBEdtr;
145300090306           TBEsif = TBXsif;
145400090306           TBElin = TBXlin;
145500090306           TBEapl = TBXapl;
145600090306           TBEcod = k_TBEcod;
145700090306           TBEke1 = k_TBEke1;
145800090306           TBEke2 = k_TBEke2;
145900090306           //_____________
146000071207           WRITE TNTBE000;
146100090306           //¯¯¯¯¯¯¯¯¯¯¯¯¯
146200071207         ELSE;
146300090306           //______________
146400071207           UPDATE TNTBE000;
146500090306           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
146600071207         ENDIF;
146700071207
146800071207       ENDSR;
146900071207
147000071212       //--------------------------------------------------------------
147100090306       //?Operazioni finali.                                           ?
147200071212       //--------------------------------------------------------------
147300090306       BEGSR  sr_RoutEnd;
147400071207
147500090306         clear  TIBS69ds;
147600090304         tibs69ds.I69tla = 'C';
147700090304         tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
147800071207
147900120604         kpjbu = TNTBJDCds;
148000071207         return;
148100071207
148200071207       ENDSR;
148300090306
148400071207      /end-free
148500071207
148600071212       //--------------------------------------------------------------
148700090306       //?Schiere a tempo di compilazione.                             ?
148800071212       //--------------------------------------------------------------
148900071207
149000071207** - $MSG -------------------------------------------------------------------*
149100071207Immettere il codice cliente                                                     1
149200071207Immettere SOLO caratteri numerici                                               2
149300071207Codice errato                                                                   3
149400071207Cliente annullato                                                               4
149500071207Immettere la directory                                                          5
149600071207Carattere "\" non valido; per indicare una sottocartella utilizzare "/"         6
149700071210Caratteri & e %  non ammessi per la directory di clienti con invio automatico   7
149800120604Inserire il codice padre                                                        8
149900120604Il codice padre deve essere = al codice cliente o = a cliente già in tabella    9
150000120604Cliente da copiare NON più reperibile in tab. "JDC"                            10
150100120604Cliente già inserito in tab. "JDC"                                             11
150200120608Record NON cancellabile: trattasi di PADRE con figli (vedi xxxxxxx)            12
150300120608Tipo elaborazione errato o mancante                                            13
150400120608Tipo documento errato o mancante                                               14
150500120608Nome documento errato o mancante                                               15
150600120608Tipo invio errato o mancante                                                   16
150700120608Non ammessi spazi vuoti all'inizio del nome della directory                    17
150800120608Nome directory NON valido: non ammesso il doppio "/"                           18
150900120608Ammessi solo "&AAA" e "&M" (come caratteri sostituibili)                       19
151000120608Ammessi max 2 "&M"                                                             20
151100120608Se invio automatico l'ultimo ramo della directory deve essere "DI"             21
151200120608Data errata                                                                    22
151300120608Data "DAL" incongruente con data "AL"                                          23
