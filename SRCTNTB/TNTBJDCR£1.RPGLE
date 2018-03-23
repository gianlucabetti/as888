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
020800071207
020900090306       // - Gestione video
021000071206     d $Video          s              2    inz('D1')
021100090313
021200090313       // - Indici di schiera
021300090313     d xx              s              3  0 inz
021400071207
021500090313       // - Campi di comodo
021600090306     d wPos1           s              2  0 inz
021700090306     d wPos2           s              2  0 inz
021800090316     d SAVke1          s                   like(TBEke1)    inz
021900120608     d wdatai          s              8  0 inz
022000120608     d wdataf          s              8  0 inz
022100120608     d data_eur        s               d   Datfmt(*eur)
022200120608     d data_iso        s               d   Datfmt(*iso)
022300071207
022400090306       //--------------------------------------------------------------
022500090306       //?Definizione key-list.                                        ?
022600090306       //--------------------------------------------------------------
022700090306
022800090306       // - File TNTBE01L
022900090306     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
023000090306     d                                     prefix(k_)  inz
023100071206
023200090306       //--------------------------------------------------------------
023300090306       //?Riepilogo indicatori.                                        ?
023400090306       //--------------------------------------------------------------
023500090306       // 01    - RECORD annullato
023600090306       // 02    - acceso IMMISSIONE - spento MANUTENZIONE
023700090306       // 25    - Comodo
023800090306       // 28    - Emissione del messaggio di errore a video
023900090306       // 40    - Protezione campi in prima estrazione
024000090306       // x D01:
024100090306       // 50    - Codice cliente errato o mancante
024200090306       // x D02:
024300120112       // 41    - Copia in D02: V2CCCM non protetto
024400120605       // 53    - Directory per documenti      errata
024500090306       // 61    - Codice Padre                 errato
024600120604       // 63    - Tipo data di elaborazione    errato
024700120605       // 70    - Flag nome documento          errato
024800090306       // 99    - Generico di errore
024900090306       //--------------------------------------------------------------
025000071206
025100071206     c     *Entry        plist
025200071206     c                   parm                    KPJBA
025300071207
025400071207      /free
025500071207
025600071207       // Operazioni iniziali
025700090306       exsr  sr_RoutInz;
025800071207
025900071207       // Gestione video
026000090306       DOW  $Fine = *off;
026100071207         select;
026200090306           when  $Video = 'D1';
026300090306             exsr  sr_GesD01;
026400090306           when  $Video = 'D2';
026500090306             exsr  sr_GesD02;
026600101103           when  $Video = 'W1';
026700101103             exsr  sr_GesW01;
026800071207           other;
026900071207             leave;
027000071207         endsl;
027100071207       ENDDO;
027200071207
027300071207       // Operazioni finali
027400090306       exsr  sr_RoutEnd;
027500071206
027600071212       //--------------------------------------------------------------
027700090306       //?Operazioni iniziali.                                         ?
027800071212       //--------------------------------------------------------------
027900090306       BEGSR  sr_RoutInz;
028000090306
028100090306         *inLR = *on;
028200071207
028300090306         if  kpjbu <> *blank;
028400120604           TNTBJDCds = kpjbu;
028500071207         else;
028600120604           clear  TNTBJDCds;
028700071207         endif;
028800120604         BJDCfxx = *blank;
028900120604         BJDCerr = *off;
029000120604         BJDCmsg = *blank;
029100071207
029200090306         // Reperimento dati job
029300090306         exsr  DatiJob;
029400071207
029500090306         // Impostazione nome programma a video
029600071207         V1Tpgm = SDSpgm;
029700090306
029800090306         // Aggancio dati generali della tabella in esame
029900090306         clear  k_TBEcod;
030000090306         k_TBEke1 = *zero;
030100090306         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
030200090306                = c_Tab;
030300090306         clear  k_TBEke2;
030400090306         clear  k_TBElin;
030500090306         k_TBEsif = KNSIF;
030600090306         chain(n)  %kds(k05tntbe01)  TNTBE000;
030700090306         if  not  %found(TNTBE01L);
030800090306           clear  k_TBEsif;
030900090306           chain(n)  %kds(k05tntbe01)  TNTBE000;
031000090306         endif;
031100090306         if  %found(TNTBE01L);
031200090306           xTNTBEds = TNTBEds;
031300090306         else;
031400090306           clear  xTNTBEds;
031500090306         endif;
031600071207
031700090306         // Verifica parametri ricecvuti
031800120604         if  BJDCke1  <> *blank;
031900090306           exsr  sr_InzD01;
032000090306           exsr  sr_CtrD01;
032100090306           if  errMessage = *on;
032200120604             BJDCerr = *on;
032300120604             BJDCmsg = V1Dmsg;
032400071217             $Fine  = *on;
032500071217             leavesr;
032600071217           endif;
032700071207           $Video  = 'D2';
032800071207           $InzD02 = *on;
032900071207         endif;
033000071207
033100071207       ENDSR;
033200071206
033300071212       //--------------------------------------------------------------
033400090306       //?Reperimento Dati del job (Utente/Operativi).                 ?
033500071212       //--------------------------------------------------------------
033600090306       BEGSR  DatiJob;
033700071207
033800071207         in(E) §AzUte;
033900071207         if NOT %error;
034000071207           in(E) §DatiUte;
034100071207         endif;
034200090306         if %error or RSut = *blank;
034300071207           clear TIBS34ds;
034400071207           tibs34r(tibs34ds);
034500071207           in §AzUte;
034600071207           in §DatiUte;
034700071207         endif;
034800071207
034900071207       ENDSR;
035000071206
035100071212       //--------------------------------------------------------------
035200090306       //?Gestione videata D01                                         ?
035300071212       //--------------------------------------------------------------
035400090306       BEGSR  sr_GesD01;
035500071207
035600090306         // Inizializzazione videata
035700090306         if  $InzD01 = *on;
035800090306           exsr  sr_InzD01;
035900071207           $InzD01 = *off;
036000071207         endif;
036100071207
036200090306         // Emissione testata
036300120604         write  TBJDCT01;
036400071207
036500090306         // Emissione videata
036600120604         exfmt  TBJDCD01;
036700071207         errMessage  = *off;
036800071207         errGenerico = *off;
036900090306         clear  V1Dmsg;
037000071207
037100071207         select;
037200090306           // F3=Fine
037300090306           when  dsp_aid = c_F03;
037400090306             exsr  sr_F03D01;
037500090306           // Enter
037600071207           other;
037700090306             exsr  sr_CtrD01;
037800090306             if  errGenerico;
037900071207               leavesr;
038000071207             endif;
038100071207             $Video  = 'D2';
038200071207             $InzD02 = *on;
038300071207         endsl;
038400071207
038500071207       ENDSR;
038600071206
038700071212       //--------------------------------------------------------------
038800090306       //?Inizializzazione videata D01                                 ?
038900071212       //--------------------------------------------------------------
039000090306       BEGSR  sr_InzD01;
039100071207
039200120604         clear  TBJDCD01;
039300071207
039400120604         if  BJDCke1 = *blank;
039500090306           %subst(V1Cccm : %len(V1Cccm) : 1) = '?';
039600071207         else;
039700120604           V1Cccm = %trimr(BJDCke1);
039800071207         endif;
039900071207
040000071207       ENDSR;
040100071206
040200071212       //--------------------------------------------------------------
040300090306       //?Controllo videata D01                                        ?
040400071212       //--------------------------------------------------------------
040500090306       BEGSR  sr_CtrD01;
040600071207
040700090306         IndDspF = *zero;
040800120112         reset  $Copia;
040900071207
041000071207         SELECT;
041100071207
041200120112           // - Ricerca codice cliente
041300090306           WHEN  %scan('?' : V1Cccm) > *zero;
041400120112             clear  Param01;
041500090327             P01ord = '0';
041600090327             KPJBU  = Param01;
041700120604             tntbjdc1r (KPJBA : '3');
041800090327             Param01 = KPJBU;
041900090319
042000090327             if  P01ke1 <> *zero;
042100090327               V1Cccm = P01ke1;
042200090319             endif;
042300120112             if  P01rit <> '3';
042400120112               errGenerico = *on;
042500120112               PosCurCcm   = *on;
042600120112               leavesr;
042700120112             else;
042800120112               $Copia = *on;
042900120112             endif;
043000071207
043100120112           // - Controllo immissione codice cliente
043200090306           WHEN  V1Cccm = *blank  or  V1Cccm  = *zero;
043300071207             errMessage  = *on;
043400071207             errGenerico = *on;
043500071207             PosCurCcm   = *on;
043600071207             V1Dmsg = $Msg(01);
043700071207             leavesr;
043800071207
043900120112           // - Controllo immissione solo caratteri numerici
044000090310           WHEN  %check(c_Digits : V1Cccm) > *zero;
044100071207             errMessage  = *on;
044200071207             errGenerico = *on;
044300071207             PosCurCcm   = *on;
044400071207             V1Dmsg = $Msg(02);
044500071207             leavesr;
044600071207
044700071207         ENDSL;
044800071207
044900090306         // - Controllo esistenza codice cliente in anagrafica clienti
045000120112         //   (SE non richiesta copia: il vero codice cliente verrà
045100120112         //    inserito in D02)
045200120112         If  Not  $Copia;
045300120112           clear  TIBS69ds;
045400120112           tibs69ds.I69kcc = DUTkci;
045500120112           tibs69ds.I69kac = %int(V1Cccm);
045600120112           tibs69ds.I69sif = knsif;
045700120112           tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
045800120112           if  tibs69ds.O69err = *on;
045900120112             errMessage  = *on;
046000120112             errGenerico = *on;
046100120112             PosCurCcm   = *on;
046200120112             V1Dmsg = $Msg(03);
046300120112             leavesr;
046400120112           endif;
046500120112         EndIf;
046600071207
046700090306         // - Verifica esistenza codice cliente in tabella
046800090306         reset  TNTBE000;
046900090310         k_TBEcod = c_Tab;
047000090306         k_TBEke1 = V1Cccm;
047100090306         clear  k_TBEke2;
047200090306         k_TBElin = TBXlin;
047300090306         k_TBEsif = TBXsif;
047400120112
047500120112         If  Not  $Copia;
047600120112
047700120112           chain  %kds(K05tntbe01) TNTBE000;
047800120112           // - - NON ammesso nuovo record per cliente annullato
047900120112           if  NOT  %found(TNTBE01L)  and  ds_CNACO.ACOflg <> *blank;
048000120112             errMessage  = *on;
048100120112             errGenerico = *on;
048200120112             PosCurCcm   = *on;
048300120112             V1Dmsg = $Msg(04);
048400120112             leavesr;
048500120112           endif;
048600120112
048700120112         Else;
048800120112
048900120112           chain(N)  %kds(K05tntbe01) TNTBE000;
049000120112           // - - NON ammessa copia di record inesistente
049100120112           if  NOT  %found(TNTBE01L);
049200120112             errMessage  = *on;
049300120112             errGenerico = *on;
049400120112             PosCurCcm   = *on;
049500120604             V1Dmsg = $Msg(10);
049600120112             leavesr;
049700120112           endif;
049800120112
049900120112         EndIf;
050000071207
050100120112         // - Decodifica cliente
050200090306         if  %found(TNTBE01L);
050300120604           dJDC = TBEuni;
050400120112           if  Not  $Copia;
050500120604             V1Dccm = §JDCrag;
050600120112           else;
050700120112             V1Dccm = %subst(ds_CNACO.ACOrag : 1 : %len(V1Dccm));
050800120112           endif;
050900071207         else;
051000120604           clear  dJDC;
051100090304           V1Dccm = %subst(ds_CNACO.ACOrag : 1 : %len(V1Dccm));
051200071207         endif;
051300071207
051400071207       ENDSR;
051500071206
051600071212       //--------------------------------------------------------------
051700090306       //?Gestione tasto funzionale F3 da videata D01                  ?
051800090306       //?F3=Fine                                                      ?
051900071212       //--------------------------------------------------------------
052000090306       BEGSR  sr_F03D01;
052100071207
052200090306         // Chiusura del programma
052300071207         $Fine = *on;
052400071217
052500090306         // Restituzione indicazione del tasto funzionale premuto
052600120604         BJDCfxx = '1';
052700071207
052800071207       ENDSR;
052900071206
053000071212       //--------------------------------------------------------------
053100090306       //?Gestione videata D02                                         ?
053200071212       //--------------------------------------------------------------
053300090306       BEGSR  sr_GesD02;
053400071207
053500090306         // Inizializzazione videata
053600090306         if   $InzD02 = *on;
053700090306           exsr  sr_InzD02;
053800090306           $InzD02 = *off;
053900071207         endif;
054000071207
054100090306         // Emissione testata
054200120604         write  TBJDCT01;
054300071207
054400090306         // Emissione videata
054500120604         if  BJDCopz = *blank;
054600120604           exfmt  TBJDCD02;
054700071217         else;
054800120604           write  TBJDCD02;
054900090306           exfmt  PROTECT;
055000071217         endif;
055100071207         errMessage  = *off;
055200071207         errGenerico = *off;
055300090306         clear  V1Dmsg;
055400071207
055500071207         SELECT;
055600090306           // F3=Fine
055700090306           WHEN  dsp_aid = c_F03;
055800090306             exsr  sr_F03D01;
055900090319           // F04=Interrogazione Padre
056000090319           WHEN  dsp_aid = c_F04;
056100090319             exsr  sr_F04D02;
056200090306           // F12=Ritorno
056300090306           WHEN  dsp_aid = c_F12;
056400090306             exsr  sr_F12D02;
056500090306           // Enter o F5 o F6 o F16
056600071207           OTHER;
056700090306             // - Controllo dati
056800090306             if  dsp_aid = c_F16;
056900090306               exsr  sr_CtrAnnull;
057000090306             else;
057100090306               exsr  sr_CtrD02;
057200090306             endif;
057300090306             if  errGenerico;
057400071207               leavesr;
057500071207             endif;
057600090306             // - Conferma dati:
057700090306             //   F5=Ripristino; F6=Conferma; F16=Annullamento
057800090306             if  dsp_aid = c_F05  or
057900090306                 dsp_aid = c_F06  or
058000090306                 dsp_aid = c_F16;
058100090306               exsr  Upd_TNTBE;
058200071207               if  NOT  errGenerico;
058300071207                 clear V1Tlav;
058400071207                 $Video  = 'D1';
058500090306                 //$InzD01 = *on;
058600071207               endif;
058700071207             EndIf;
058800071207
058900071207         ENDSL;
059000071207
059100071207       ENDSR;
059200071206
059300071212       //--------------------------------------------------------------
059400090306       //?Inizializzazione videata D01                                 ?
059500071212       //--------------------------------------------------------------
059600090306       BEGSR  sr_InzD02;
059700071207
059800090313         reset  $ByPass;
059900121016         reset  wForza;
060000071207
060100120604         clear  TBJDCW01;
060200101103
060300120604         clear  TBJDCD02;
060400071210
060500090306         IndDspF = *zero;
060600090309
060700090309         // Impostazione indicatori per abilitazione tasti funzionali
060800090309         exsr  Set_Ind_D02;
060900090309
061000120112         // - Codice estrazione (chiave)
061100120112         if  Not  $Copia;
061200120112           V2Cccm = %int(V1Cccm);
061300120112           V2Dccm = V1Dccm;
061400120112         endif;
061500071207
061600090306         IF  %found(TNTBE01L);
061700071207
061800090306           // Impostazione dati di record trovato
061900071217           select;
062000120604             when  BJDCopz = '5';
062100071217               V1Tlav = 'VISUALIZZA';
062200120112             when  $Copia;
062300120112               V1Tlav = '  COPIA   ';
062400090306             when  TBEatb = *blank;
062500071217               V1Tlav = 'VARIAZIONE';
062600071217             other;
062700071217               V1Tlav = 'ANNULLATO ';
062800071217           endsl;
062900090225
063000090306           // Codice padre
063100120604           V2Cksu = §JDCksu;
063200090309           clear  TIBS69ds;
063300090309           clear  ds_CNACO;
063400090309           clear  ds_CNIND;
063500090309           clear  ds_CNCLP;
063600090309           clear  ds_FNCLS;
063700090310           select;
063800120604             when  §JDCksu =  V2Cccm;
063900120606               V2Dksu = V2Dccm;
064000120604             when  §JDCksu <> *zero;
064100090310               tibs69ds.I69kcc = DUTkci;
064200090310               tibs69ds.I69kac = %int(V2Cksu);
064300090310               tibs69ds.I69sif = knsif;
064400090310               tibs69r(TIBS69ds:ds_CNACO:ds_CNIND:ds_CNCLP:ds_FNCLS);
064500090310               if tibs69ds.O69err = *on;
064600090310                 V1Dmsg = $Msg(03);
064700090310                 errMessage  = *on;
064800090310                 errGenerico = *on;
064900090310                 PosCurKsu   = *on;
065000090310                 leavesr;
065100090310               endif;
065200120606               V2Dksu = ds_CNACO.ACOrag;
065300090310           endsl;
065400090225
065500120604           V2Caut   = §JDCaut;
065600120604           V2Ctpi   = §JDCtpi;
065700090225
065800120605           // Flag nome documento
065900120604           V2Cfmi = §JDCfmi;
066000090310           reset  TIBS02ds;
066100090303           T02cod = 'NIM';
066200120604           T02ke1 = §JDCfmi;
066300090304           TNTBE_RicercaControllo(kpjba : tibs02ds);
066400090306           if  T02err = *blank;
066500090303             V2Dfmi = T02uni;
066600090303           endif;
066700121016
066800121016           V2merge  = §JDCmerge;
066900090225
067000120607           V2Cpag   = §JDCpag;
067100120604           V2Cdir   = §JDCdir;
067200120604           V2note   = §JDCnote;
067300120605           v2tadu   = §JDCtadu;
067400120608           data_eur = %date(§JDCdti:*iso);
067500120608           V2Cdti   = %dec(data_eur);
067600120608           data_eur = %date(§JDCdtf:*iso);
067700120608           V2Cdtf   = %dec(data_eur);
067800090923
067900090923           *in02 = *off;
068000071207
068100071207         ELSE;
068200071207
068300090306           // Impostazione dati di record nuovo
068400071207           V1Tlav = 'IMMISSIONE';
068500090226           V2Cdir = %editc(V2Cccm:'X');
068600120605
068700120605         //?FORZO frequenza addebito MENSILE (in questo modo avrà id job mensile)
068800120605           v2tadu   = 'M';
068900090923
069000090923           *in02 = *on;
069100071207
069200071207         ENDIF;
069300071207
069400071207       ENDSR;
069500071217
069600071217       //--------------------------------------------------------------
069700090306       //?Settaggio indicatori nella videata D02 per abilitazione      ?
069800090306       //?  tasti funzionali                                           ?
069900071217       //--------------------------------------------------------------
070000090306       BEGSR  Set_Ind_D02;
070100090319
070200120606         F4Attivo  = *off;
070300090306         F5Attivo  = ((%found(TNTBE01L) and TBEatb <> *blank)
070400120604                                        and BJDCopz <> '5');
070500120604         F6Attivo  = (NOT F5Attivo and BJDCopz <> '5');
070600090306         F16Attivo = ((%found(TNTBE01L) and TBEatb = *blank)
070700120604                                        and BJDCopz <> '5');
070800120112
070900120112         // - Protezione campo chiave SE NON copia
071000120112         InserCCM2 = $Copia;
071100071217
071200071217       ENDSR;
071300090319
071400090319       //--------------------------------------------------------------
071500090319       //?Gestione tasto funzionale F04 da videata D02                 ?
071600090319       //?F04=Interrogazione Padre                                     ?
071700090319       //--------------------------------------------------------------
071800090319       BEGSR  sr_F04D02;
071900090319
072000090319         // Richiamo pgm interrogazione
072100090327         clear  Param01;
072200090327         P01ord = '1';
072300090327         P01ksu = V2Cksu;
072400090327         KPJBU  = Param01;
072500120604         tntbjdc1r (KPJBA);
072600120607         Param01 = KPJBU;
072700120607         IF  P01ke1 <> *zero;
072800120607           V2Cksu = P01ksu;
072900120607         ENDIF;
073000090319
073100090319       ENDSR;
073200090319
073300071217
073400071217       //--------------------------------------------------------------
073500090306       //?Gestione tasto funzionale F12 da videata D02                 ?
073600090306       //?F12=Ritorno                                                  ?
073700071217       //--------------------------------------------------------------
073800090306       BEGSR  sr_F12D02;
073900071217
074000090306         // Ritorno alla videata precedente se NON richiamato
074100120604         if  BJDCopz = *blank  and
074200120604             BJDCsif = *blank  and  BJDClin = *blank  and
074300120604             BJDCke1 = *blank  and  BJDCke2 = *blank;
074400071217           clear V1Tlav;
074500071217           $Video = 'D1';
074600090306         // Ritorno al pgm chiamante se richiamato
074700071217         else;
074800120604           BJDCfxx = '2';
074900071217           $Fine = *on;
075000071217         endif;
075100071217
075200071217       ENDSR;
075300090306
075400090306       //--------------------------------------------------------------
075500090306       //?Controllo "annullabilità" del record (padre senza figli)     ?
075600090306       //--------------------------------------------------------------
075700090306       BEGSR  sr_CtrAnnull;
075800090306
075900090306         IndDspF = *zero;
076000090306         // REimpostazione indicatori per abilitazione tasti funzionali
076100090306         exsr  Set_Ind_D02;
076200090306
076300090306         if  V2Cksu <= *zero;
076400120604           V2Cksu = §JDCksu;
076500090306         endif;
076600090306
076700090306         // Se figlio (non padre): fine controlli
076800090306         if  V2Cccm <> V2Cksu;
076900090306           leavesr;
077000090306         endif;
077100090306
077200090306         //?Verifica se padre con figli?
077300120604         clear  dJDCp;
077400090316         SAVke1 = TBEke1;
077500090306         setll     %kds(k05tntbe01 : 1)  TNTBE000;
077600090306         reade(n)  %kds(k05tntbe01 : 1)  TNTBE000;
077700090306
077800090306         dow  not  %eof(TNTBE01L);
077900090316           if  TBEatb = *blank   and
078000090316               %editc(V2Cccm : 'X') <> %trimr(TBEke1);
078100120604             dJDCp = TBEuni;
078200120604             if  p_§JDCksu = V2Cccm;
078300090306               errMessage  = *on;
078400090306               errGenerico = *on;
078500090306               PosCurKsu   = *on;
078600120608               V1Dmsg = $Msg(12);
078700120608               %subst(V1Dmsg:60:7) = %subst(TBEke1:1:7);
078800090306               leavesr;
078900090306             endif;
079000090306           endif;
079100090306         reade(n)  %kds(k05tntbe01 : 1)  TNTBE000;
079200090306         enddo;
079300090316
079400120112         // - Riaggancio del rec. in manutenzione/copia
079500090316         TBEke1 = SAVke1;
079600090316         reset  TNTBE000;
079700090316         k_TBEcod = c_Tab;
079800120112         k_TBEke1 = %editc( V2Cccm : 'X' );
079900090316         clear  k_TBEke2;
080000090316         k_TBElin = TBXlin;
080100090316         k_TBEsif = TBXsif;
080200090316         chain  %kds(K05tntbe01) TNTBE000;
080300090306
080400090306       ENDSR;
080500071206
080600071212       //--------------------------------------------------------------
080700090306       //?Controllo videata D02                                        ?
080800071212       //--------------------------------------------------------------
080900090306       BEGSR  sr_CtrD02;
081000071207
081100090309         %subst(IndDspF : 50) = *off;
081200120112
081300120112         IF  $Copia;
081400120112
081500120112           V1Cccm = %editc( V2Cccm : 'X' );
081600120112
081700120112           Select;
081800120112
081900120112             // -?Codice estrazione (SE copia)?
082000120112             When  V2Cccm = *zero;
082100120112               errMessage  = *on;
082200120112               errGenerico = *on;
082300120112               PosCurCcm2  = *on;
082400120112               V1Dmsg = $Msg(01);
082500120112               leavesr;
082600120112
082700120112             // -?Controllo / Decodifica?
082800120112             When  tibs69ds.I69kac <> V2Cccm  or
082900120112                   k_TBEke1 <> %editc( V2Cccm : 'X' );
083000120112               clear  TIBS69ds;
083100120112               tibs69ds.I69kcc = DUTkci;
083200120112               tibs69ds.I69kac = V2Cccm;
083300120112               tibs69ds.I69sif = knsif;
083400120112               tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
083500120112               if  tibs69ds.O69err = *on;
083600120112                 errMessage  = *on;
083700120112                 errGenerico = *on;
083800120112                 PosCurCcm2  = *on;
083900120112                 V1Dmsg = $Msg(03);
084000120112                 leavesr;
084100120112               endif;
084200120112               V2Dccm = ds_CNACO.ACOrag;
084300120112               // -?Verifica esistenza in tabella?
084400120112               k_TBEke1 = %editc( V2Cccm : 'X' );
084500120112               chain  %kds(K05tntbe01) TNTBE000;
084600120112               select;
084700120112                 // - -?NON ammesso nuovo record per cliente annullato?
084800120112                 when  NOT  %found(TNTBE01L)  and  ds_CNACO.ACOflg <> *blank;
084900120112                   errMessage  = *on;
085000120112                   errGenerico = *on;
085100120112                   PosCurCcm2  = *on;
085200120112                   V1Dmsg = $Msg(04);
085300120112                   leavesr;
085400120112                 // - -?Impossibile inserimento di record già esistente?
085500120112                 when  %found(TNTBE01L);
085600120112                   errMessage  = *on;
085700120112                   errGenerico = *on;
085800120112                   PosCurCcm2  = *on;
085900120604                   V1Dmsg = $Msg(11);
086000120112                   leavesr;
086100120112               endsl;
086200120112
086300120112           EndSl;
086400120112
086500120112         ENDIF;
086600090225
086700090306         //?Codice padre?
086800120606         V2Cksu = V2Cccm;
086900090225         select;
087000090306           when  V2Cksu  = *zero;
087100090306             errMessage  = *on;
087200090225             errGenerico = *on;
087300090303             PosCurKsu   = *on;
087400120604             V1Dmsg = $Msg(08);
087500090225             leavesr;
087600090306           when  V2Cksu = V2Cccm;
087700120606             V2Dksu = V2Dccm;
087800090225           other;
087900090327             // - Controllo esistenza codice padre come cliente
088000120604             //   in tabella JDC
088100090303             exsr  sr_Padre;
088200090303             if  errGenerico = *on;
088300090226               leavesr;
088400090225             endif;
088500120606             V2Dksu = p_§JDCrag;
088600090226         endsl;
088700101103         //?SE codice padre <> da codice cliente?
088800101103         // ?& sono in immissione?
088900101103         IF  V2Cccm <> V2Cksu   and   *in02;
089000101103           // ?=> chiedo se si vogliono riportare i dati del padre?
089100101103           If  W1Ccpy = *blank;
089200101103             $Video  = 'W1';
089300101103             leavesr;
089400101103           EndIf;
089500100521         ENDIF;
089600090311
089700090311         //?Tipo elaborazione?
089800090327           // - Obbligatorio
089900120605           IF  V2Caut <> 'A'  and  V2Caut <> 'N';
090000090327             errMessage  = *on;
090100090327             errGenerico = *on;
090200090327             PosCurAut   = *on;
090300120608             V1Dmsg = $Msg(13);
090400090327             leavesr;
090500120604         endIF;
090600090327
090700120605         //?Tipo documento
090800120917         if  V2Ctpi <> 'TIF' and V2Ctpi <> 'PDF';
090900090327           errMessage  = *on;
091000090327           errGenerico = *on;
091100090327           PosCurTpi   = *on;
091200120608           V1Dmsg = $Msg(14);
091300090327           leavesr;
091400090327         endif;
091500090226
091600120605         //?Flag nome documento
091700090310         clear  V2Dfmi;
091800090310         select;
091900090310           // - Obbligatorietà
092000090310           when  V2Cfmi = *blank;
092100090310             errMessage  = *on;
092200090310             errGenerico = *on;
092300090310             PosCurFmi   = *on;
092400120608             V1Dmsg = $Msg(15);
092500090310             leavesr;
092600090310           // - Ricerca
092700090310           when  %scan('?' : V2Cfmi) > *zero;
092800090310             clear  TIBS02ds;
092900090310             T02mod = 'R';
093000090310             T02cod = 'NIM';
093100090310             T02sif = KNSIF;
093200090310             TNTBE_RicercaControllo(kpjba : tibs02ds);
093300090310             if  T02err = *blank;
093400090310               V2Cfmi = %subst(T02ke1 : 1 : %len(V2Cfmi));
093500090310               V2Dfmi = T02uni;
093600090310               errGenerico = *on;
093700090310               leavesr;
093800090310             else;
093900090310               errMessage  = *on;
094000090310               errGenerico = *on;
094100090310               PosCurFmi   = *on;
094200090310               V1Dmsg = T02msg;
094300090310               leavesr;
094400090310             endif;
094500090310           // - Controllo
094600090310           other;
094700090310             reset  TIBS02ds;
094800090310             T02cod = 'NIM';
094900090310             T02ke1 = V2Cfmi;
095000090310             TNTBE_RicercaControllo(kpjba : tibs02ds);
095100090310             if  T02err <> *blank;
095200090310               errMessage  = *on;
095300090310               errGenerico = *on;
095400090310               PosCurFmi   = *on;
095500120608               V1Dmsg = $Msg(15);
095600090310               leavesr;
095700090310             endif;
095800090310             V2Dfmi = T02uni;
095900090310         ENDSL;
096000120606
096100120606         //?Tipo invio
096200120607         SELECT;
096300120607           WHEN  V2Cpag  = *blanks;
096400120607             ErrMessage  = *on;
096500120607             ErrGenerico = *on;
096600120606             PosCurPag   = *on;
096700120608             V1Dmsg = $Msg(16);
096800120607             leavesr;
096900120607           WHEN  V2Cpag  <> 'CLI' and V2Cpag <> 'CL1';
097000120607             ErrMessage  = *on;
097100120607             ErrGenerico = *on;
097200120607             PosCurPag   = *on;
097300120608             V1Dmsg = $Msg(16);
097400120607             leavesr;
097500120607         ENDSL;
097600090226
097700120605         //?Directory per documenti
097800090226         select;
097900090306           when  V2Cdir  = *blank;
098000090226             errMessage  = *on;
098100090226             errGenerico = *on;
098200090226             PosCurDir   = *on;
098300090226             V1Dmsg = $Msg(05);
098400090226             leavesr;
098500090316           when  %subst(V2Cdir : 1 : 1) = *blank;
098600090316             errMessage  = *on;
098700090316             errGenerico = *on;
098800090316             PosCurDir   = *on;
098900120608             V1Dmsg = $Msg(17);
099000090316             leavesr;
099100090306           when  %scan('\':V2Cdir) > *zero;
099200090226             errMessage  = *on;
099300090226             errGenerico = *on;
099400090226             PosCurDir   = *on;
099500090226             V1Dmsg = $Msg(06);
099600090226             leavesr;
099700090306           when  %scan('%':V2Cdir) > *zero   and
099800090306                           V2Caut  = 'A';
099900090226             errMessage  = *on;
100000090226             errGenerico = *on;
100100090226             PosCurDir   = *on;
100200090226             V1Dmsg = $Msg(07);
100300090226             leavesr;
100400090309           when  %scan('&':V2Cdir) > *zero  and
100500090309                           V2Caut  = 'A';
100600090309             errMessage  = *on;
100700090309             errGenerico = *on;
100800090309             PosCurDir   = *on;
100900090309             V1Dmsg = $Msg(07);
101000090309             leavesr;
101100090226         endsl;
101200090309
101300090316         wPos1 = %scan('/' : V2Cdir);
101400090316         DoW  wPos1 > *zero;
101500090316           if  %subst( V2Cdir : wPos1 : 2 ) = '//';
101600090316             errMessage  = *on;
101700090316             errGenerico = *on;
101800090316             PosCurDir   = *on;
101900120608             V1Dmsg = $Msg(18);
102000090316             leavesr;
102100090316           else;
102200090316             wPos1 = %scan('/' : V2Cdir : wPos1 + 1 );
102300090316           endif;
102400090316         EndDo;
102500090316
102600090309         wPos1 = %scan('&' : V2Cdir);
102700090309         IF  wPos1 > *zero;
102800090309           if  %subst( V2Cdir : wPos1 : 4 ) <> '&AAA'   and
102900090309               %subst( V2Cdir : wPos1 : 2 ) <> '&M';
103000090309             errMessage  = *on;
103100090309             errGenerico = *on;
103200090309             PosCurDir   = *on;
103300120608             V1Dmsg = $Msg(19);
103400090309             leavesr;
103500090309           endif;
103600090309           wPos2 = %scan( '&' : V2Cdir : wPos1 + 1 );
103700090309           If  wPos2 > *zero;
103800090309             if  %subst( V2Cdir : wPos2 : 4 ) <> '&AAA'   and
103900090309                 %subst( V2Cdir : wPos2 : 2 ) <> '&M';
104000090309               errMessage  = *on;
104100090309               errGenerico = *on;
104200090309               PosCurDir   = *on;
104300120608               V1Dmsg = $Msg(19);
104400090309               leavesr;
104500090309             endif;
104600090309           EndIf;
104700090309         ENDIF;
104800090309
104900090309         wPos1 = %scan('&M' : V2Cdir);
105000090309         IF  wPos1 > *zero;
105100090309           wPos2 = %scan( '&M' : V2Cdir : wPos1 + 1 );
105200090309           If  wPos2 > *zero   and
105300090309               %scan( '&M' : V2Cdir : wPos2 + 1 ) > *zero;
105400090309             errMessage  = *on;
105500090309             errGenerico = *on;
105600090309             PosCurDir   = *on;
105700120608             V1Dmsg = $Msg(20);
105800090309             leavesr;
105900090309           EndIf;
106000090309         ENDIF;
106100120607
106200120607         // Se FTP la directory deve finire con /DI
106300121016         // se non c'è il flag di Merge, in questo caso c'è il controllo
106400121016         // nella route CTR_merge
106500121016         IF  V2Caut = 'A' and V2merge = *blanks;
106600120607           wPos1 = %scan('  ':V2Cdir);
106700120607           wPos1 -= 3;
106800120607           IF  %subst(V2Cdir:wPos1:3) <> '/DI';
106900120607             ErrMessage  = *on;
107000120607             ErrGenerico = *on;
107100120607             PosCurDir   = *on;
107200120608             V1Dmsg = $Msg(21);
107300120607             leavesr;
107400120607           ENDIF;
107500120607         ENDIF;
107600090226
107700090327         // - Se codice padre <> da codice cliente
107800090306         //   la directory deve coincidere almeno fino alla 2ª subdir
107900120604         IF  V2Cccm <> V2Cksu   and   p_§JDCdir <> V2Cdir;
108000090311
108100090311           // - Controllo 1ª directory
108200090311           wPos1 = %scan( '/' : %trim( V2Cdir ) );
108300120604           wPos2 = %scan( '/' : %trim( p_§JDCdir ) );
108400090313           If  wPos1 <> wPos2                               OR
108500090313             ( wPos1  > *zero   and   wPos2 > *zero   and
108600090311               %subst( %trim( V2Cdir    ) : 1 : wPos1 ) <>
108700120604               %subst( %trim( p_§JDCdir ) : 1 : wPos2 ) )   OR
108800090313             ( wPos1  = *zero   and   wPos2  =  *zero
108900120604                                and   V2Cdir <> p_§JDCdir );
109000090311             errMessage  = *on;
109100090311             errGenerico = *on;
109200090311             PosCurDir   = *on;
109300090311             select;
109400090311               when  wPos1 =  *zero   and   wPos2 <> *zero;
109500090316                 V1Dmsg = 'Il padre ha altri rami directory (';
109600090311               when  wPos1 <> *zero   and   wPos2 =  *zero;
109700090311                 V1Dmsg = 'Il padre ha una directory unica (';
109800090311               other;
109900090311                 V1Dmsg = 'Primo ramo directory +
110000090311                           <> da quello del cod. padre (';
110100090311             EndSl;
110200120604             if  %len( %trim( p_§JDCdir ) ) <=
110300090311                 %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) ) - 1;
110400090311               V1Dmsg = %trimr( V1Dmsg )
110500120604                      + %trim( p_§JDCdir ) + ')';
110600090311             else;
110700090313               wPos1 = %len( V1Dmsg );
110800090313               wPos2 = %len( %trimr( V1Dmsg ) );
110900090311               V1Dmsg = %trimr( V1Dmsg )
111000120604                      + %trim( %subst( p_§JDCdir : 1 :
111100090311                          %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) )
111200090311                          - 4 ) )
111300090311                      + '...)';
111400090311             endif;
111500090311             leavesr;
111600090311           EndIf;
111700090311
111800090311           // - Controllo 2ª directory
111900090311           wPos1 = %scan( '/' : %trim( V2Cdir ) : wPos1 + 1 );
112000120604           wPos2 = %scan( '/' : %trim( p_§JDCdir ) : wPos2 + 1 );
112100090316           //If  wPos1 <> wPos2                               OR
112200090316           //  ( wPos1  > *zero   and   wPos2 > *zero   and
112300090316           //    %subst( %trim( V2Cdir    ) : 1 : wPos1 ) <>
112400120604           //    %subst( %trim( p_§JDCdir ) : 1 : wPos2 ) )   OR
112500090316           //  ( wPos1  = *zero   and   wPos2  =  *zero
112600120604           //                     and   V2Cdir <> p_§JDCdir );
112700090316           if  wPos1 = *zero;
112800090316             wPos1 = %len( %trimr( V2Cdir ) );
112900090316           else;
113000090316             wPos1 -= 1;
113100090316           endif;
113200090316           if  wPos2 = *zero;
113300120604             wPos2 = %len( %trimr( p_§JDCdir ) );
113400090316           else;
113500090316             wPos2 -= 1;
113600090316           endif;
113700090316           If  wPos1 <> wPos2   OR
113800090316               %subst(V2Cdir : 1 : wPos1) <>
113900120604                 %subst(p_§JDCdir : 1 : wPos2);
114000090311             errMessage  = *on;
114100090311             errGenerico = *on;
114200090311             PosCurDir   = *on;
114300090316             //select;
114400090316             //  when  wPos1 =  *zero   and   wPos2 <> *zero;
114500090316             //    V1Dmsg = 'Il padre ha altri rami directory (';
114600090316             //  when  wPos1 <> *zero   and   wPos2 =  *zero;
114700090316             //    V1Dmsg = 'Il padre ha meno rami directory (';
114800090316             //  other;
114900090313                 V1Dmsg = 'Secondo ramo directory +
115000090311                           <> da quello del cod. padre (';
115100090316             //EndSl;
115200120604             if  %len( %trim( p_§JDCdir ) ) <=
115300090311                 %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) ) - 1;
115400090311               V1Dmsg = %trimr( V1Dmsg )
115500120604                      + %trim( p_§JDCdir ) + ')';
115600090311             else;
115700090311               V1Dmsg = %trimr( V1Dmsg )
115800120604                      + %trim( %subst( p_§JDCdir : 1 :
115900090311                          %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) )
116000090311                          - 4 ) )
116100090311                      + '...)';
116200090311             endif;
116300090311             leavesr;
116400090311           EndIf;
116500090311
116600090306         ENDIF;
116700121016
116800121016         //?Immagine con Merge (LDV + DOC)
116900121016         IF  V2merge = 'S';
117000121016           exsr CTR_merge;
117100121016         ENDIF;
117200121016         IF  ErrGenerico;
117300121016           leavesr;
117400121016         ENDIF;
117500120608
117600120608         //?Data inizio e fine scansione documenti
117700120608         IF  V2Cdti  = *zeros;
117800120608           ErrMessage  = *on;
117900120608           ErrGenerico = *on;
118000120608           PosCurDti   = *on;
118100120608           V1Dmsg      = $Msg(22);
118200120608           leavesr;
118300120608         ENDIF;
118400120608         IF  V2Cdtf  = *zeros;
118500120608           ErrMessage  = *on;
118600120608           ErrGenerico = *on;
118700120608           PosCurDtf   = *on;
118800120608           V1Dmsg      = $Msg(22);
118900120608           leavesr;
119000120608         ENDIF;
119100120608         IF  V2Cdti  > *zeros;
119200120608           clear wlbdat;
119300120608           G02dat = V2Cdti;
119400120608           xsrda8(wlbdat);
119500120608           IF  G02err = '1';
119600120608             ErrMessage  = *on;
119700120608             ErrGenerico = *on;
119800120608             PosCurDti   = *on;
119900120608             V1Dmsg      = $Msg(22);
120000120608             leavesr;
120100120608           ENDIF;
120200120608           V2Cdti = G02dat;
120300120608           wdatai = G02inv;
120400120608         ENDIF;
120500120608         IF  V2Cdtf  > *zeros;
120600120608           clear wlbdat;
120700120608           G02dat = V2Cdtf;
120800120608           xsrda8(wlbdat);
120900120608           IF  G02err = '1';
121000120608             ErrMessage  = *on;
121100120608             ErrGenerico = *on;
121200120608             PosCurDtf   = *on;
121300120608             V1Dmsg      = $Msg(22);
121400120608             leavesr;
121500120608           ENDIF;
121600120608           V2Cdtf = G02dat;
121700120608           wdataf = G02inv;
121800120608         ENDIF;
121900120608         IF  wdatai > wdataf;
122000120608           ErrMessage  = *on;
122100120608           ErrGenerico = *on;
122200120608           PosCurDti   = *on;
122300120608           V1Dmsg = $Msg(23);
122400120608           leavesr;
122500120608         ENDIF;
122600071207
122700071207       ENDSR;
122800090225
122900090225       //--------------------------------------------------------------
123000120604       //?Controllo esistenza del codice padre in tabella JDC          ?
123100090225       //--------------------------------------------------------------
123200090303       BEGSR  sr_Padre;
123300090302
123400120604         // Controlla esistenza in tabella "JDC"
123500120604         clear  dJDCp;
123600090310         reset  TIBS02ds;
123700090310         T02sif = KNSIF;
123800090310         %subst(T02ke1 : 1 : 7) = %editc(V2Cksu : 'X');
123900090310         TNTBE_RicercaControllo(kpjba : tibs02ds);
124000090310         if  T02err = *blank;
124100120604           dJDCp  = T02uni;
124200090310         else;
124300090310           errMessage  = *on;
124400090310           errGenerico = *on;
124500090310           PosCurksu   = *on;
124600120604           V1Dmsg = $Msg(09);
124700090310           leavesr;
124800090310         endif;
124900090327
125000090327         // Controlla se già inserito come figlio di altro padre
125100120604         if  p_§JDCksu <> V2Cksu;
125200090327           errMessage  = *on;
125300090327           errGenerico = *on;
125400090327           PosCurKsu   = *on;
125500090327           V1Dmsg = 'Non ammesso cod. PADRE già FIGLIO di altro +
125600120604                     cliente (' + %editc(p_§JDCksu : 'X') + ')';
125700090327           leavesr;
125800090327         endif;
125900090225
126000090225       ENDSR;
126100101103
126200101103       //--------------------------------------------------------------
126300101103       //?Gestione window W01                                          ?
126400101103       //--------------------------------------------------------------
126500101103       BEGSR  sr_GesW01;
126600101103
126700101103         // -?Emissione window?
126800120604         exfmt  TBJDCW01;
126900101103
127000101103         errMessage  = *off;
127100101103         errGenerico = *off;
127200101103         clear  V1Dmsg;
127300101103
127400101103         $Video = 'D2';
127500101103
127600101103         select;
127700101103           // -?F12=Ritorno?
127800101103           when  dsp_aid = c_F12;
127900101103             leavesr;
128000101103           // -?Invio con "S"?
128100101103           when  W1Ccpy = 'S';
128200101103             if  V2Caut = *blank;
128300120604               V2Caut   = p_§JDCaut;
128400101103             endif;
128500101103             if  V2Ctpi = *blank;
128600120604               V2Ctpi   = p_§JDCtpi;
128700101103             endif;
128800101103             if  V2Cfmi = *blank;
128900120604               V2Cfmi   = p_§JDCfmi;
129000101103               clear  V2Dfmi;
129100101103             endif;
129200121016             if  V2merge = *blank;
129300121016               V2merge  = p_§JDCmerge;
129400121016             endif;
129500101103             //if  V2Cdir = *blank;      //? Directory ?
129600120604               V2Cdir   = p_§JDCdir;     //? COMUNQUE  ?
129700101103             //endif;                    //? del padre ?
129800101103             if  V2note = *blank;
129900120604               V2note   = p_§JDCnote;
130000101103             endif;
130100120605             if  V2tadu = *blank;
130200120605               V2tadu   = p_§JDCtadu;
130300120605             endif;
130400120607             if  V2cpag = *blank;
130500120607               V2cpag   = p_§JDCpag;
130600120607             endif;
130700120608             if  V2cdti = *zeros;
130800120608               data_eur = %date(p_§JDCdti:*iso);
130900120608               V2Cdti   = %dec(data_eur);
131000120608             endif;
131100120608             if  V2cdtf = *zeros;
131200120608               data_eur = %date(p_§JDCdtf:*iso);
131300120608               V2Cdtf   = %dec(data_eur);
131400120608             endif;
131500101103         endsl;
131600101103
131700101103       ENDSR;
131800121016
131900121016       //--------------------------------------------------------------
132000121016       //?Controllo Merge.
132100121016       //--------------------------------------------------------------
132200121016       BEGSR  CTR_merge;
132300121016
132400121016       // -?L'immagine se con Merge deve essere di tipo PDF
132500121016         IF  V2Ctpi <> 'PDF';
132600121016           errMessage  = *on;
132700121016           errGenerico = *on;
132800121016           V1Dmsg = 'Immagine con Merge, deve essere di tipo PDF';
132900121016           PosCurTpi = *on;
133000121016           leavesr;
133100121016         ENDIF;
133200121016
133300121016       // -?La directory deve finire con /DF
133400121016         wPos1 = %scan('  ':V2Cdir);
133500121016         wPos1 -= 3;
133600121016         IF  %subst(V2Cdir:wPos1:3) <> '/DF';
133700121016           ErrMessage  = *on;
133800121016           ErrGenerico = *on;
133900121016           PosCurDir   = *on;
134000121016           V1Dmsg = 'Immagine con Merge, l''ultimo ramo della +
134100121016                     DIR deve essere "DF"';
134200121016           leavesr;
134300121016         ENDIF;
134400121016
134500121016       // -?Cerco il corrispondente in LAC
134600121016         clear  TIBS02DS;
134700121016         T02mod = 'C';
134800121016         T02cod = 'LAC';
134900121016         T02ke1 = %editc(V2Cccm:'X');
135000121016         TNTBE_RicercaControllo(kpjba : tibs02ds);
135100121016         // -?Non trovo la tabella
135200121016         IF  T02err <> *blanks;
135300121016           // -?Immissione forzo
135400121016           IF  *in02 and not wForza;
135500121016             wForza = *on;
135600121016             ErrMessage  = *on;
135700121016             ErrGenerico = *on;
135800121016             V1Dmsg = 'Ricordarsi di attivare anche la corrispondente +
135900121016                       tabella LAC. Enter forza.';
136000121016             leavesr;
136100121016           ENDIF;
136200121016         // -?Manutenzione blocco
136300121016           IF  not *in02;
136400121016             ErrMessage  = *on;
136500121016             ErrGenerico = *on;
136600121016             V1Dmsg = 'Manca la corrispondente tabella LAC';
136700121016             leavesr;
136800121016           ENDIF;
136900121016         // -?Trovo la tabella
137000121016         ELSE;
137100121016           dLAC = T02uni;
137200121016         // -?Controllo del formato documento
137300121016           IF  V2Ctpi <> §LACtpi;
137400121016             ErrMessage  = *on;
137500121016             ErrGenerico = *on;
137600121016             PosCurTpi   = *on;
137700121016             V1Dmsg = 'Formato documento diverso dalla corrispondente +
137800121016                       tabella LAC --> ' + §LACtpi;
137900121016             leavesr;
138000121016           ENDIF;
138100121016         // -?Controllo il nome immagine
138200121016           IF  V2Cfmi <> §LACfmi;
138300121016             ErrMessage  = *on;
138400121016             ErrGenerico = *on;
138500121016             PosCurFmi   = *on;
138600121016             V1Dmsg = 'Nome immagine diverso dalla corrispondente +
138700121016                       tabella LAC --> ' + §LACfmi;
138800121016             leavesr;
138900121016           ENDIF;
139000121016         // -?Controllo la directory
139100121016           wPos2 = %scan('  ':§LACdir);
139200121016           wPos2 -= 3;
139300121016           IF  %subst(V2Cdir:1:wPos1) <>
139400121016               %subst(§LACdir:1:wPos2);
139500121016             ErrMessage  = *on;
139600121016             ErrGenerico = *on;
139700121016             PosCurDir   = *on;
139800121016             V1Dmsg = 'Directory diversa dalla corrispondente +
139900121016                       tabella LAC --> ' + %subst(§LACdir:1:wPos2);
140000121016             leavesr;
140100121016           ENDIF;
140200121016         ENDIF;
140300121016
140400121016       ENDSR;
140500071206
140600071212       //--------------------------------------------------------------
140700120604       //?Aggiornamento record TNTBE00F (tab. JDC)                     ?
140800071212       //--------------------------------------------------------------
140900090306       BEGSR  Upd_TNTBE;
141000071207
141100120604         clear  dJDC;
141200120604         §JDCrag   = V2Dccm;
141300120604         §JDCdir   = V2Cdir;
141400120604         §JDCtpi   = V2Ctpi;
141500120604         §JDCfmi   = V2Cfmi;
141600121016         §JDCmerge = V2merge;
141700120604         §JDCaut   = V2Caut;
141800120604         §JDCnote  = V2note;
141900120604         §JDCksu   = V2Cksu;
142000120605         §JDCtadu  = V2tadu;
142100120607         §JDCpag   = V2Cpag;
142200120608         §JDCdti   = wdatai;
142300120608         §JDCdtf   = wdataf;
142400071207
142500120604         TBEuni    = dJDC;
142600071207
142700090306         clear  TBEftt;
142800090306         clear  TBEftr;
142900071207
143000071207         select;
143100090306           // F5=Ripristino
143200090306           when  dsp_aid = c_F05;
143300090306             clear  TBEatb;
143400090306           // F16=Annullamento
143500090306           when  dsp_aid = c_F16;
143600071207             TBEatb = 'A';
143700071207         endsl;
143800071207
143900071207         IF  NOT  %found(TNTBE01L);
144000090306           clear  TBEatb;
144100090306           clear  TBEflt;
144200090306           clear  TBEftr;
144300090306           //clear  TBEdtr;
144400090306           TBEsif = TBXsif;
144500090306           TBElin = TBXlin;
144600090306           TBEapl = TBXapl;
144700090306           TBEcod = k_TBEcod;
144800090306           TBEke1 = k_TBEke1;
144900090306           TBEke2 = k_TBEke2;
145000090306           //_____________
145100071207           WRITE TNTBE000;
145200090306           //¯¯¯¯¯¯¯¯¯¯¯¯¯
145300071207         ELSE;
145400090306           //______________
145500071207           UPDATE TNTBE000;
145600090306           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
145700071207         ENDIF;
145800071207
145900071207       ENDSR;
146000071207
146100071212       //--------------------------------------------------------------
146200090306       //?Operazioni finali.                                           ?
146300071212       //--------------------------------------------------------------
146400090306       BEGSR  sr_RoutEnd;
146500071207
146600090306         clear  TIBS69ds;
146700090304         tibs69ds.I69tla = 'C';
146800090304         tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
146900071207
147000120604         kpjbu = TNTBJDCds;
147100071207         return;
147200071207
147300071207       ENDSR;
147400090306
147500071207      /end-free
147600071207
147700071212       //--------------------------------------------------------------
147800090306       //?Schiere a tempo di compilazione.                             ?
147900071212       //--------------------------------------------------------------
148000071207
148100071207** - $MSG -------------------------------------------------------------------*
148200071207Immettere il codice cliente                                                     1
148300071207Immettere SOLO caratteri numerici                                               2
148400071207Codice errato                                                                   3
148500071207Cliente annullato                                                               4
148600071207Immettere la directory                                                          5
148700071207Carattere "\" non valido; per indicare una sottocartella utilizzare "/"         6
148800071210Caratteri & e %  non ammessi per la directory di clienti con invio automatico   7
148900120604Inserire il codice padre                                                        8
149000120604Il codice padre deve essere = al codice cliente o = a cliente già in tabella    9
149100120604Cliente da copiare NON più reperibile in tab. "JDC"                            10
149200120604Cliente già inserito in tab. "JDC"                                             11
149300120608Record NON cancellabile: trattasi di PADRE con figli (vedi xxxxxxx)            12
149400120608Tipo elaborazione errato o mancante                                            13
149500120608Tipo documento errato o mancante                                               14
149600120608Nome documento errato o mancante                                               15
149700120608Tipo invio errato o mancante                                                   16
149800120608Non ammessi spazi vuoti all'inizio del nome della directory                    17
149900120608Nome directory NON valido: non ammesso il doppio "/"                           18
150000120608Ammessi solo "&AAA" e "&M" (come caratteri sostituibili)                       19
150100120608Ammessi max 2 "&M"                                                             20
150200120608Se invio automatico l'ultimo ramo della directory deve essere "DI"             21
150300120608Data errata                                                                    22
150400120608Data "DAL" incongruente con data "AL"                                          23
