000100090306       //--------------------------------------------------------------
000200110804       //?GESTIONE TABELLA "FAB" - Forzatura per AC BASE               ?
000300090306       //--------------------------------------------------------------
000400110804
000500110804      *?  ATTENZIONE!!  ?
000600110804      *?    Questa tabella è utilizzata anche dal pgm TNTA61R  ?
000700110804      *?    'Interrogazione abilitazioni clienti'              ?
000800110804      *?    In caso di aggiunta/modifica campi alla tabella    ?
000900110804      *?    verificare se sono validi per la visualizzazione   ?
001000110804      *?    da TA61                                            ?
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
002200110804     fTNTB89D   cf   e             workstn
002300071207     f                                     indds(IndDspF)
002400090306     f                                     infds(InfDspF)
002500071207
002600090306       //--------------------------------------------------------------
002700090306       //?Definizione costanti.                                        ?
002800090306       //--------------------------------------------------------------
002900090306
003000090306       // - Tabella in gestione
003100110804     d c_Tab           c                   const('FAB')
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
007000090311     d $Msg            s             78    dim(12)  ctdata  perrcd(1)
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
009700110804     d  F5Attivo                       n   overlay(IndDspF:05)
009800090311     d  F6Attivo                       n   overlay(IndDspF:06)
009900110804     d  F10Attivo                      n   overlay(IndDspF:10)
010000090311     d  F16Attivo                      n   overlay(IndDspF:16)
010100090306         // - Emissione messaggio di errore
010200090311     d  ErrMessage                     n   overlay(IndDspF:28)
010300110804         // - Indicatori di visualizzazione
010400110804     d  Interroga                      n   overlay(IndDspF:40)
010500090306         // - Posizionamento cursore & segnalazione errore
010600110804     d  PosCurOpz                      n   overlay(IndDspF:50)
010700110804     d  PosCurCcm                      n   overlay(IndDspF:51)
010800110804     d  PosCurItr                      n   overlay(IndDspF:52)
010900090306         //   - Riemissione videata
011000090311     d  ErrGenerico                    n   overlay(IndDspF:99)
011100071207
011200090306       // - Parametri ricevuti
011300071206     d KPJBA         e ds
011400110804     d TNTA61ds      e ds                  inz
011500071207
011600090306       // - Reperimento dati utente
011700071207     d TIBS34ds      e ds
011800071207
011900090306       // - Controllo/Decodifica cliente
012000090304     d TIBS69ds      e ds                  qualified  inz
012100090304     d ds_CNACO      e ds                  extname(CNACO00F)
012200090304     d                                     qualified  inz
012300090304     d ds_CNIND      e ds                  extname(CNIND00F)
012400090304     d                                     qualified  inz
012500090304     d ds_CNCLP      e ds                  extname(CNCLP00F)
012600090304     d                                     qualified  inz
012700090304     d ds_FNCLS      e ds                  extname(FNCLS00F)
012800090304     d                                     qualified  inz
012900090306
013000090306       // - Dati record principale della tabella
013100090306     d TNTBEds       e ds                  inz  extname(TNTBE00F)
013200090306     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
013300090306     d                                          prefix(TBX:3)
013400090312
013500090312       // - Parametri per Ricerca/controllo tabelle
013600090312     d TIBS02ds      e ds                  inz
013700090312     d  T02mod       e                     inz('C')
013800090312     d  T02cod       e                     inz(c_Tab)
013900071207
014000110804       // - Tabella "FAB" = Clienti per ritorno immagini
014100110804     d dFAB          e ds                  inz
014200071207
014300090306       //--------------------------------------------------------------
014400090306       //?Definizione procedure usate.                                 ?
014500090306       //--------------------------------------------------------------
014600090306
014700090306       // - Reperimento dati utente
014800090306      /copy gaitrasrc/srcProtoPR,TIBS34R
014900071207
015000090304       // - Ricerca/Controllo tabelle
015100090306      /copy gaitrasrc/srcProtoPR,TIBS02R
015200071207
015300090304       // - Controllo/Decodifica cliente
015400090306      /copy gaitrasrc/srcProtoPR,TIBS69R
015500071207
015600090306       //--------------------------------------------------------------
015700090306       //?Definizione variabili globali.                               ?
015800090306       //--------------------------------------------------------------
015900090306
016000090306       // - Flags booleani
016100110804     d $InzS01         s               n   inz(*on)
016200110804     d $InzD02         s               n   inz(*off)
016300090306     d $Fine           s               n   inz
016400071207
016500090306       // - Gestione video
016600110804     d $Video          s              2    inz('S1')
016700090313
016800090313       // - Campi di comodo
016900071207
017000090306       //--------------------------------------------------------------
017100090306       //?Definizione key-list.                                        ?
017200090306       //--------------------------------------------------------------
017300090306
017400090306       // - File TNTBE01L
017500090306     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
017600090306     d                                     prefix(k_)  inz
017700071206
017800090306       //--------------------------------------------------------------
017900090306       //?Riepilogo indicatori.                                        ?
018000090306       //--------------------------------------------------------------
018100071206
018200071206     c     *Entry        plist
018300071206     c                   parm                    KPJBA
018400110804     c                   parm                    TNTA61DS
018500071207
018600071207      /free
018700071207
018800071207       // Operazioni iniziali
018900090306       exsr  sr_RoutInz;
019000071207
019100071207       // Gestione video
019200090306       DOW  $Fine = *off;
019300071207         select;
019400110804           when  $Video = 'S1';
019500110804             exsr  sr_GesS01;
019600090306           when  $Video = 'D2';
019700090306             exsr  sr_GesD02;
019800071207           other;
019900071207             leave;
020000071207         endsl;
020100071207       ENDDO;
020200071207
020300071207       // Operazioni finali
020400090306       exsr  sr_RoutEnd;
020500071206
020600071212       //--------------------------------------------------------------
020700090306       //?Operazioni iniziali.                                         ?
020800071212       //--------------------------------------------------------------
020900090306       BEGSR  sr_RoutInz;
021000090306
021100090306         *inLR = *on;
021200071207
021300090306         // Reperimento dati job
021400090306         exsr  DatiJob;
021500071207
021600090306         // Impostazione nome programma a video
021700071207         V1Tpgm = SDSpgm;
021800090306
021900090306         // Aggancio dati generali della tabella in esame
022000090306         clear  k_TBEcod;
022100090306         k_TBEke1 = *zero;
022200090306         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
022300090306                = c_Tab;
022400090306         clear  k_TBEke2;
022500090306         clear  k_TBElin;
022600090306         k_TBEsif = KNSIF;
022700090306         chain(n)  %kds(k05tntbe01)  TNTBE000;
022800090306         if  not  %found(TNTBE01L);
022900090306           clear  k_TBEsif;
023000090306           chain(n)  %kds(k05tntbe01)  TNTBE000;
023100090306         endif;
023200090306         if  %found(TNTBE01L);
023300090306           xTNTBEds = TNTBEds;
023400090306         else;
023500090306           clear  xTNTBEds;
023600090306         endif;
023700071207
023800071207       ENDSR;
023900071206
024000071212       //--------------------------------------------------------------
024100090306       //?Reperimento Dati del job (Utente/Operativi).                 ?
024200071212       //--------------------------------------------------------------
024300090306       BEGSR  DatiJob;
024400071207
024500071207         in(E) §AzUte;
024600071207         if NOT %error;
024700071207           in(E) §DatiUte;
024800071207         endif;
024900090306         if %error or RSut = *blank;
025000071207           clear TIBS34ds;
025100071207           tibs34r(tibs34ds);
025200071207           in §AzUte;
025300071207           in §DatiUte;
025400071207         endif;
025500071207
025600071207       ENDSR;
025700110804
025800110804       //--------------------------------------------------------------
025900110804       //?Gestione videata S01
026000110804       //--------------------------------------------------------------
026100110804       BEGSR GesS01;
026200110804
026300110804         // Inizializzazione videata
026400110804         if  $InzS01 = *on;
026500110804            exsr InzS01;
026600110804            $InzS01  = *off;
026700110804         endif;
026800110804
026900110804         // Posizionamento cursore
027000110804         if  C01csr  > *zero;
027100110804           C01rcd = C01csr;
027200110804         else;
027300110804         // Se non è stato caricato nulla si riemette la videata D01
027400110804           C01rcd = 1;
027500110804           $Video = 'D1';
027600110804           errMessage  = *on;
027700110804           errGenerico = *on;
027800110804           PosCurNrv   = *on;
027900110804           V1Dmsg = $Msg(07);
028000110804           leavesr;
028100110804         endif;
028200110804
028300110804         // Emissione Testata e Piede con tasti funzionali abilitati
028400110804           write  TA87T01;
028500110804           write  TA87P01;
028600110804
028700110804
028800110804         // Emissione videata
028900110804         exfmt  TA87C01;
029000110804         reset errMessage;
029100110804         reset errGenerico;
029200110804         clear V1Dmsg;
029300110804
029400110804         SELECT;
029500110804
029600110804         // - F3=Fine
029700110804           WHEN  dsp_aid = c_F03;
029800110804             exsr F03D01;
029900110804
030000110804         // - F12=Ritorno
030100110804           WHEN  dsp_aid = c_F12;
030200110804             exsr F12S01;
030300110804
030400110804         // - Roll-Up
030500110804           WHEN  dsp_aid = c_RollUp;
030600110804
030700110804         // Invio
030800110804           OTHER;
030900110804             exsr OpzS01;
031000110804             if  errGenerico = *on;
031100110804               leavesr;
031200110804             endif;
031300110804
031400110804         ENDSL;
031500110804
031600110804       ENDSR;
031700110804
031800110804       //--------------------------------------------------------------
031900110804       //?Inizializzazione videata S01
032000110804       //--------------------------------------------------------------
032100110804       BEGSR InzS01;
032200110804
032300110804       // Pulizia subfile
032400110804         exsr pulsfl1;
032500110804
032600110804         exsr sr_ReadRec;
032700110804
032800110804         dow  $EoF   = *off and
032900110804              S01nrr < *hival;
033000110804           exsr RollUpS01;
033100110804         enddo;
033200110804       // Impaginazione subfile
033300110804       // -> forza l'impaginazione sul 1° rec. del subfile
033400110804         if S01nrr > *zero;
033500110804           C01rcd  = 1;
033600110804           C01csr  = 1;
033700110804         else;
033800110804           clear C01rcd;
033900110804         endif;
034000110804
034100110804
034200110804       ENDSR;
034300110804
034400110804       //--------------------------------------------------------------
034500110804       //?Pulizia SFL01
034600110804       //--------------------------------------------------------------
034700110804       BEGSR Pulsfl1;
034800110804
034900110804       // Pulizia subfile
035000110804         SflDsp_N    = *on;
035100110804         SflDspCtl_N = *on;
035200110804         write  TA87C01;
035300110804         SflDspCtl_N = *off;
035400110804         SflEnd      = *off;
035500110804
035600110804
035700110804         clear W_SflPag;
035800110804         clear C01rcd;
035900110804         clear C01csr;
036000110804         clear S01nrr;
036100110804         clear V1Dmsg;
036200110804         errMessage  = *off;
036300110804         errGenerico = *off;
036400110804         WindDspF    = IndDspF;
036500110804         reset WindDspFb;
036600110804         IndDspF     = WindDspF;
036700110804
036800110804       ENDSR ;
036900110804
037000110804       //--------------------------------------------------------------
037100110804       //?Caricamento di tutto il sfl
037200110804       //--------------------------------------------------------------
037300110804       BEGSR RollUpS01;
037400110804
037500110804         clear  s01nrr;
037600110804         SflNxtChg = *off;
037700110804
037800110804         // Ciclo di caricamento dati nel sfl / lettura rec. successivo
037900110804         DOW  $EoF   = *off  and
038000110804              S01nrr < *hival;
038100110804
038200110804         // - Caricamento dati nel record del subfile
038300110804           exsr  RieS01;
038400110804           If $recok = *on ;
038500110804              S01nrr += 1;
038600110804              write TA87S01;
038700110804              If s01nrr = *hival ;
038800110804               errMessage  = *on;
038900110804               errGenerico = *on;
039000110804               V1Dmsg = $Msg(16);
039100110804               leave;
039200110804              endif ;
039300110804           endif ;
039400110804
039500110804         // - Lettura record successivo nell'archivio
039600110804           exsr sr_ReadRec;
039700110804
039800110804         ENDDO;
039900110804
040000110804         // Visualizzazione del SFL (se ci sono dati)
040100110804         SflDsp_N = (S01nrr <= *zeros);
040200110804
040300110804         // Attivazione (eventuale) del SFLEND
040400110804         SflEnd  =  *on;
040500110804
040600110804         // Posizionamento cursore al 1° rcd della pagina
040700110804         if  S01nrr > *zero;
040800110804         //  wPag   = S01nrr / C_SflPag;
040900110804         //  wRig   = S01nrr - (C_SflPag * wPag);
041000110804         //  C01rcd = wPag * C_SflPag;
041100110804         //  if  wRig > *zeros;
041200110804         //    C01rcd = C01rcd + 1;
041300110804         //  else;
041400110804         //    C01rcd = C01rcd - C_SflPag + 1;
041500110804         //  endif;
041600110804             c01rcd = 1;
041700110804         else;
041800110804           clear  C01rcd;
041900110804         endif;
042000110804
042100110804         C01csr = C01rcd;
042200110804
042300110804       ENDSR;
042400110804
042500110804       //--------------------------------------------------------------
042600110804       //?Lettura record successivo
042700110804       //--------------------------------------------------------------
042800110804       BEGSR sr_ReadRec;
042900110804
043000110804         $RecOK  = *off;
043100110804
043200110804         exec sql Fetch C1 into :tivisds;
043300110804
043400110804         if  Sqlcod = 100 ;
043500110804              $Eof  = *on;
043600110804         endif;
043700110804         If  Sqlcod = 0 ;
043800110804             $RecOK  = *on;
043900110804         endif;
044000110804         if  Sqlcod  < 0 ;
044100110804              $Eof  = *on;
044200110804         endif;
044300110804       ENDSR;
044400110804
044500110804
044600110804       //--------------------------------------------------------------
044700110804       //?Caricamento singolo record nel SubFile S01
044800110804       //--------------------------------------------------------------
044900110804       BEGSR RieS01;
045000110804
045100110804         clear  TA87S01;
045200110804
045300110804       // -?controllo se utente abilitato alla trattativa
045400110804         clear tntaa1ds;
045500110804         itaa1tipo = 'N';
045600110804         itaa1nrv  = visnrv;
045700110804         itaa1caut = '§UTEGTC';
045800110804         kpjbu = tntaa1ds ;
045900110804         Tntaa1c (kpjba) ;
046000110804         tntaa1ds = kpjbu ;
046100110804         IF  OTAA1fabi='N' ;
046200110804           eval $RecOK = *off ;
046300110804           leavesr;
046400110804         ENDIF;
046500110804
046600110804         // - verifico se ha delle offerte
046700110804         setll  visnrv  tivof11l ;
046800110804         reade  visnrv  tivof11l ;
046900110804         DOW not %eof(tivof11l) ;
047000110804           If vofeso <> '*' ;
047100110804              s01off = '  OFF' ;
047200110804              if  VOFeso = 'H'   and   VOFdch > *zero;
047300110804                %subst(S01off:1:2) = 'h ';
047400110804                leave ;
047500110804              endif;
047600110804           EndIf ;
047700110804           reade  visnrv  tivof11l ;
047800110804         ENDDO ;
047900110804
048000110804
048100110804         // - Selezioni
048200110804
048300110804         select;
048400110804
048500110804           // - se richieste le sole trattative con le offerte
048600110804           when  V01off = 'S' and S01off = *blank ;
048700110804             eval $RecOK = *off ;
048800110804             leavesr ;
048900110804           // - se richieste le sole trattative senza  offerte
049000110804           when  V01off = 'N' and  S01off <> *blank ;
049100110804             eval $RecOK = *off ;
049200110804             leavesr ;
049300110804
049400110804           // - se richieste le sole trattative fittizie
049500110804           when  V01ffz = 'S' and VISffz = *blank;
049600110804             eval $RecOK = *off ;
049700110804             leavesr ;
049800110804           // - se richieste le sole trattative NON fittizie
049900110804           when  V01ffz = 'N' and VISffz <> *blank;
050000110804             eval $RecOK = *off ;
050100110804             leavesr ;
050200110804
050300110804           // - verifico se è stata richiesta parzializzazione per
050400110804           //   ragione sociale
050500110804           when  V01rag <> *blank  and
050600110804                 %scan( %trimr(V01rag) : VISrag ) = *zero;
050700110804             $RecOK  = *off;
050800110804             leavesr ;
050900110804
051000110804         endsl ;
051100110804
051200110804         // Imposto gli altri campi del subfile
051300110804
051400110804         dVis01 = VISflo;
051500110804
051600110804         H01ffz = VISffz;         // - hidden
051700110804         S01rag = VISrag;
051800110804         if  VISksc > *zero;
051900110804           S01ksc = %editc( VISksc : 'Z' );
052000110804         else;
052100110804           evalr S01ksc = %subst( %editc( VIScmm : 'Z' ) : 1 : 3 );
052200110804         endif;
052300110804         data_eur = %date(VISdat:*iso);
052400110804         //S01dta = %dec(data_eur);
052500110804         S01dta = ( ( %subdt(Data_Eur : *days)  * 10000 ) +
052600110804                    ( %subdt(Data_Eur : *months) * 100 ) +
052700110804              ( %int( %subst( %editc(VISdat : 'X') : 3 : 2 ) ) ) );
052800110804         if  VISffz <> 'S';
052900110804           S01ftv  = VIStpv + '  ' + VISffz;
053000110804         else;
053100110804           S01ftv = '   F';
053200110804         endif;
053300110804         S01esi = VISesi;
053400110804         S01esi01 = dVIS01.§VISesito;
053500110804
053600110804         S01nrv = VISnrv;
053700110804         S01cmm = VIScmm;
053800110804
053900110804         // -?decodifica commerciale
054000110804         clear  ds01;
054100110804         TBLcod = '01';
054200110804         TBLkey = %editc( VIScmm : 'X' );
054300110804         chain (TBLkut : TBLcod : TBLkey) TABEL;
054400110804         if  %found(TABEL00F)   and   TBLflg = *blank;
054500110804           ds01    = TBLuni;
054600110804           S01cmmD = §01age;
054700110804         endif;
054800110804
054900110804       ENDSR;
055000110804
055100110804       //--------------------------------------------------------------
055200110804       //?Gestione tasto funzionale F12 da videata S01
055300110804       //?F12=Ritorno
055400110804       //--------------------------------------------------------------
055500110804       BEGSR F12S01;
055600110804
055700110804         // Ritorno alla videata D01
055800110804         $Video = 'D1';
055900110804
056000110804       ENDSR;
056100110804
056200110804       //--------------------------------------------------------------
056300110804       //?Gestione opzioni subfile
056400110804       //--------------------------------------------------------------
056500110804       BEGSR OpzS01;
056600110804
056700110804         readc TA87S01;
056800110804
056900110804         DOW  NOT  %eof(TNTA87D);
057000110804
057100110804           SflNxtChg = *off;
057200110804           WindDspF  = IndDspF;
057300110804           reset WindDspFb;
057400110804           IndDspF   = WindDspF;
057500110804           C01rcd    = S01nrr;
057600110804
057700110804           SELECT;
057800110804
057900110804             // - Nessuna opzione
058000110804             WHEN  S01opz  = *blank;
058100110804
058200110804             // - Precedente segnalazione di errore
058300110804             WHEN  S01opz  = 'E';
058400110804               clear S01opz;
058500110804
058600110804             // - G = Gestione
058700110804             WHEN  S01opz  = 'G';
058800110804             //?controllo se utente abilitato alla trattativa
058900110804               clear TNTAA1DS;
059000110804               ITAA1tipo = 'N';
059100110804               ITAA1caut = '§utegtc';
059200110804               ITAA1nrv  = S01nrv;
059300110804               kpjbu     = TNTAA1DS;
059400110804               tntaa1c (kpjba);
059500110804               TNTAA1DS = kpjbu;
059600110804               IF  OTAA1fabi = 'N';
059700110804                 ErrMessage  = *on;
059800110804                 ErrGenerico = *on;
059900110804                 PosCurOpz   = *on;
060000110804                 S01opz = 'E' ;
060100110804                 V1Dmsg      = $Msg(10);
060200110804               ELSE;
060300110804
060400110804                     exsr Calltnta88  ;
060500110804                     $inzs01 = *on ;
060600110804
060700110804               ENDIF;
060800110804
060900110804             // - 5 = Visualizzazione
061000110804             WHEN  S01opz  = '5';
061100110804                exsr Calltnta88  ;
061200110804
061300110804             // - 4 = Annulla
061400110804             WHEN  S01opz  = '4';
061500110804             // verifico se visita fittizia altrimenti errore
061600110804               IF  H01ffz <> 'S'  ;
061700110804                 ErrMessage  = *on;
061800110804                 ErrGenerico = *on;
061900110804                 PosCurOpz   = *on;
062000110804                 V1Dmsg      = $Msg(17);
062100110804               ELSE;
062200110804                 exsr Sr_AnnTratt;
062300110804               ENDIF;
062400110804
062500110804             // - ? = Opzione NON valida
062600110804             OTHER;
062700110804               errMessage  = *on;
062800110804               errGenerico = *on;
062900110804               PosCurOpz   = *on;
063000110804               V1Dmsg = $Msg(09);
063100110804
063200110804           ENDSL;
063300110804
063400110804           // Aggiornamento sfl
063500110804           select;
063600110804             when  errMessage  = *on;
063700110804               SflNxtChg = *on;
063800110804               C01csr    = C01rcd;
063900110804             when  errGenerico = *on;
064000110804               C01csr    = C01rcd;
064100110804               clear  S01opz;
064200110804             other;
064300110804               C01csr    = C01rcd;
064400110804               clear  S01opz;
064500110804           endsl;
064600110804
064700110804           update TA87S01;
064800110804
064900110804           if  errMessage = *on  or  errGenerico = *on;
065000110804             leave;
065100110804           endif;
065200110804
065300110804           readc TA87S01;
065400110804
065500110804         ENDDO;
065600110804
065700110804           if  errMessage = *off and errGenerico = *off;
065800110804               $inzs01 = *on ;
065900110804           endif;
066000110804
066100110804       ENDSR;
066200071206
066300071212       //--------------------------------------------------------------
066400090306       //?Gestione videata D01                                         ?
066500071212       //--------------------------------------------------------------
066600090306       BEGSR  sr_GesD01;
066700071207
066800090306         // Inizializzazione videata
066900090306         if  $InzD01 = *on;
067000090306           exsr  sr_InzD01;
067100071207           $InzD01 = *off;
067200071207         endif;
067300071207
067400090306         // Emissione testata
067500090306         write  TN46T01;
067600071207
067700090306         // Emissione videata
067800090306         exfmt  TN46D01;
067900071207         errMessage  = *off;
068000071207         errGenerico = *off;
068100090306         clear  V1Dmsg;
068200071207
068300071207         select;
068400090306           // F3=Fine
068500090306           when  dsp_aid = c_F03;
068600090306             exsr  sr_F03D01;
068700090306           // Enter
068800071207           other;
068900090306             exsr  sr_CtrD01;
069000090306             if  errGenerico;
069100071207               leavesr;
069200071207             endif;
069300071207             $Video  = 'D2';
069400071207             $InzD02 = *on;
069500071207         endsl;
069600071207
069700071207       ENDSR;
069800071206
069900071212       //--------------------------------------------------------------
070000090306       //?Inizializzazione videata D01                                 ?
070100071212       //--------------------------------------------------------------
070200090306       BEGSR  sr_InzD01;
070300071207
070400090306         clear  TN46D01;
070500071207
070600090306         if  B46ke1 = *blank;
070700090306           %subst(V1Cccm : %len(V1Cccm) : 1) = '?';
070800071207         else;
070900090306           V1Cccm = %trimr(B46ke1);
071000071207         endif;
071100071207
071200071207       ENDSR;
071300071206
071400071212       //--------------------------------------------------------------
071500090306       //?Controllo videata D01                                        ?
071600071212       //--------------------------------------------------------------
071700090306       BEGSR  sr_CtrD01;
071800071207
071900090306         IndDspF = *zero;
072000071207
072100071207         SELECT;
072200071207
072300090306           // Ricerca codice cliente
072400090306           WHEN  %scan('?' : V1Cccm) > *zero;
072500090327             clear  Param01;
072600090327             P01ord = '0';
072700090327             KPJBU  = Param01;
072800090327             tntb461r (KPJBA);
072900090327             Param01 = KPJBU;
073000090319
073100090327             if  P01ke1 <> *zero;
073200090327               V1Cccm = P01ke1;
073300090319             endif;
073400090327             errGenerico = *on;
073500090327             PosCurCcm   = *on;
073600090327             leavesr;
073700071207
073800090306           // Controllo immissione codice cliente
073900090306           WHEN  V1Cccm = *blank  or  V1Cccm  = *zero;
074000071207             errMessage  = *on;
074100071207             errGenerico = *on;
074200071207             PosCurCcm   = *on;
074300071207             V1Dmsg = $Msg(01);
074400071207             leavesr;
074500071207
074600090306           // Controllo immissione solo caratteri numerici
074700090310           WHEN  %check(c_Digits : V1Cccm) > *zero;
074800071207             errMessage  = *on;
074900071207             errGenerico = *on;
075000071207             PosCurCcm   = *on;
075100071207             V1Dmsg = $Msg(02);
075200071207             leavesr;
075300071207
075400071207         ENDSL;
075500071207
075600090306         // Controllo esistenza codice cliente
075700090306         // - Controllo esistenza codice cliente in anagrafica clienti
075800090306         clear  TIBS69ds;
075900090304         tibs69ds.I69kcc = DUTkci;
076000090310         tibs69ds.I69kac = %int(V1Cccm);
076100090304         tibs69ds.I69sif = knsif;
076200090304         tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
076300090306         if  tibs69ds.O69err = *on;
076400071207           errMessage  = *on;
076500071207           errGenerico = *on;
076600071207           PosCurCcm   = *on;
076700090306           V1Dmsg = $Msg(03);
076800071207           leavesr;
076900071207         endif;
077000071207
077100090306         // - Verifica esistenza codice cliente in tabella
077200090306         reset  TNTBE000;
077300090310         k_TBEcod = c_Tab;
077400090306         k_TBEke1 = V1Cccm;
077500090306         clear  k_TBEke2;
077600090306         k_TBElin = TBXlin;
077700090306         k_TBEsif = TBXsif;
077800090306         chain  %kds(K05tntbe01) TNTBE000;
077900090306         // - - NON ammesso nuovo record per cliente annullato
078000090306         if  NOT  %found(TNTBE01L)  and  ds_CNACO.ACOflg <> *blank;
078100071207           errMessage  = *on;
078200071207           errGenerico = *on;
078300071207           PosCurCcm   = *on;
078400090306           V1Dmsg = $Msg(04);
078500071207           leavesr;
078600071207         endif;
078700071207
078800090306         // Decodifica cliente
078900090306         if  %found(TNTBE01L);
079000071207           dLAC = TBEuni;
079100071207           V1Dccm = §LACrag;
079200071207         else;
079300090306           clear  dLAC;
079400090304           V1Dccm = %subst(ds_CNACO.ACOrag : 1 : %len(V1Dccm));
079500071207         endif;
079600071207
079700071207       ENDSR;
079800071206
079900071212       //--------------------------------------------------------------
080000090306       //?Gestione tasto funzionale F3 da videata D01                  ?
080100090306       //?F3=Fine                                                      ?
080200071212       //--------------------------------------------------------------
080300090306       BEGSR  sr_F03D01;
080400071207
080500090306         // Chiusura del programma
080600071207         $Fine = *on;
080700071217
080800090306         // Restituzione indicazione del tasto funzionale premuto
080900071217         B46fxx = '1';
081000071207
081100071207       ENDSR;
081200071206
081300071212       //--------------------------------------------------------------
081400090306       //?Gestione videata D02                                         ?
081500071212       //--------------------------------------------------------------
081600090306       BEGSR  sr_GesD02;
081700071207
081800090306         // Inizializzazione videata
081900090306         if   $InzD02 = *on;
082000090306           exsr  sr_InzD02;
082100090306           $InzD02 = *off;
082200071207         endif;
082300071207
082400090306         // Emissione testata
082500090306         write  TN46T01;
082600071207
082700090306         // Emissione videata
082800090306         if  B46opz = *blank;
082900090306           exfmt  TN46D02;
083000071217         else;
083100090306           write  TN46D02;
083200090306           exfmt  PROTECT;
083300071217         endif;
083400071207         errMessage  = *off;
083500071207         errGenerico = *off;
083600090306         clear  V1Dmsg;
083700071207
083800071207         SELECT;
083900090306           // F3=Fine
084000090306           WHEN  dsp_aid = c_F03;
084100090306             exsr  sr_F03D01;
084200090319           // F04=Interrogazione Padre
084300090319           WHEN  dsp_aid = c_F04;
084400090319             exsr  sr_F04D02;
084500090306           // F12=Ritorno
084600090306           WHEN  dsp_aid = c_F12;
084700090306             exsr  sr_F12D02;
084800090306           // Enter o F5 o F6 o F16
084900071207           OTHER;
085000090306             // - Controllo dati
085100090306             if  dsp_aid = c_F16;
085200090306               exsr  sr_CtrAnnull;
085300090306             else;
085400090306               exsr  sr_CtrD02;
085500090306             endif;
085600090306             if  errGenerico;
085700071207               leavesr;
085800071207             endif;
085900090306             // - Conferma dati:
086000090306             //   F5=Ripristino; F6=Conferma; F16=Annullamento
086100090306             if  dsp_aid = c_F05  or
086200090306                 dsp_aid = c_F06  or
086300090306                 dsp_aid = c_F16;
086400090306               exsr  Upd_TNTBE;
086500071207               if  NOT  errGenerico;
086600071207                 clear V1Tlav;
086700071207                 $Video  = 'D1';
086800090306                 //$InzD01 = *on;
086900071207               endif;
087000071207             EndIf;
087100071207
087200071207         ENDSL;
087300071207
087400071207       ENDSR;
087500071206
087600071212       //--------------------------------------------------------------
087700090306       //?Inizializzazione videata D01                                 ?
087800071212       //--------------------------------------------------------------
087900090306       BEGSR  sr_InzD02;
088000071207
088100090306         reset  $CtrMm;
088200090313         reset  $ByPass;
088300071207
088400101103         clear  TB46W01;
088500101103
088600090306         clear  TN46D02;
088700071210
088800090306         IndDspF = *zero;
088900090309
089000090309         // Impostazione indicatori per abilitazione tasti funzionali
089100090309         exsr  Set_Ind_D02;
089200090309
089300090309         // Codice estrazione (chiave)
089400090310         V2Cccm = %int(V1Cccm);
089500090309         V2Dccm = V1Dccm;
089600071207
089700090306         IF  %found(TNTBE01L);
089800071207
089900090306           // Impostazione dati di record trovato
090000071217           select;
090100090306             when  B46opz = '5';
090200071217               V1Tlav = 'VISUALIZZA';
090300090306             when  TBEatb = *blank;
090400071217               V1Tlav = 'VARIAZIONE';
090500071217             other;
090600071217               V1Tlav = 'ANNULLATO ';
090700071217           endsl;
090800090225
090900090306           // Codice padre
091000090306           V2Cksu = §LACksu;
091100090309           clear  TIBS69ds;
091200090309           clear  ds_CNACO;
091300090309           clear  ds_CNIND;
091400090309           clear  ds_CNCLP;
091500090309           clear  ds_FNCLS;
091600090310           select;
091700090310             when  §LACksu =  V2Cccm;
091800090310               V2Dksu = V2Dccm;
091900090310             when  §LACksu <> *zero;
092000090310               tibs69ds.I69kcc = DUTkci;
092100090310               tibs69ds.I69kac = %int(V2Cksu);
092200090310               tibs69ds.I69sif = knsif;
092300090310               tibs69r(TIBS69ds:ds_CNACO:ds_CNIND:ds_CNCLP:ds_FNCLS);
092400090310               if tibs69ds.O69err = *on;
092500090310                 V1Dmsg = $Msg(03);
092600090310                 errMessage  = *on;
092700090310                 errGenerico = *on;
092800090310                 PosCurKsu   = *on;
092900090310                 leavesr;
093000090310               endif;
093100090310               V2Dksu = ds_CNACO.ACOrag;
093200090310           endsl;
093300090225
093400090225           V2Caut   = §LACaut;
093500090309           V2Ctpi   = §LACtpi;
093600090225
093700090306           // Flag nome immagine
093800090306           V2Cfmi = §LACfmi;
093900090310           reset  TIBS02ds;
094000090303           T02cod = 'NIM';
094100090303           T02ke1 = §LACfmi;
094200090304           TNTBE_RicercaControllo(kpjba : tibs02ds);
094300090306           if  T02err = *blank;
094400090303             V2Dfmi = T02uni;
094500090303           endif;
094600090225
094700090309           V2Cdir   = §LACdir;
094800090309           V2Ctpt   = §LACtpt;
094900090309           V2Cssr   = §LACssr;
095000090309           V2Clnp   = §LAClnp;
095100090309           V2Cres   = §LACres;
095200090309           V2Crec   = §LACrec;
095300090309           V2Ccsr   = §LACcsr;
095400090309
095500090309           // Prima estrazione
095600090309           clear  wDataI;
095700090309           if  §LACdatai > *zero;
095800090309             %subst(wDataI:1:2) = %subst(%editc(§LACdatai:'X'):5:2);
095900090309             %subst(wDataI:3:4) = %subst(%editc(§LACdatai:'X'):1:4);
096000090310             V2datai = %int(wDataI);
096100090309           endif;
096200090309
096300090309           // Ultima estrazione
096400090309           clear  wDatEla;
096500090309           if  §LACdatela > *zero;
096600090309             %subst(wDatEla:1:2) = %subst(%editc(§LACdatela:'X'):5:2);
096700090309             %subst(wDatEla:3:4) = %subst(%editc(§LACdatela:'X'):1:4);
096800090310             V2datela = %int(wDatEla);
096900090309           endif;
097000090309
097100090309           V2tdta   = §LACtdta;
097200090309           V2mesi   = §LACmesi;
097300071207           V2Cschgg = §LACschgg;
097400071207           V2Csch01 = §LACsch01;
097500071207           V2Csch02 = §LACsch02;
097600071207           V2Csch03 = §LACsch03;
097700071207           V2Csch04 = §LACsch04;
097800071207           V2Csch05 = §LACsch05;
097900071207           V2Csch06 = §LACsch06;
098000071207           V2Csch07 = §LACsch07;
098100071207           V2Csch08 = §LACsch08;
098200071207           V2Csch09 = §LACsch09;
098300071207           V2Csch10 = §LACsch10;
098400071207           V2Csch11 = §LACsch11;
098500071207           V2Csch12 = §LACsch12;
098600090225
098700090306           V2tadu   = §LACtadu;
098800090309           V2Ctad   = §LACtad;
098900090306           V2cksc   = §LACksc;
099000090306           V2cctr   = §LACctr;
099100090306           V2imp    = §LACimp;
099200090306           V2fimp   = §LACfimp;
099300110713           V2pgm    = §LACpgm;
099400110713           V2ele    = §LACele;
099500090306           V2note   = §LACnote;
099600090923
099700090923           *in02 = *off;
099800071207
099900071207         ELSE;
100000071207
100100090306           // Impostazione dati di record nuovo
100200071207           V1Tlav = 'IMMISSIONE';
100300090226           V2Cdir = %editc(V2Cccm:'X');
100400090923
100500090923           *in02 = *on;
100600071207
100700071207         ENDIF;
100800090923
100900090923         // Salvo i dati sensibili per la creazione addebito
101000090923         savtad = v2ctad;
101100090923         savtadu = v2tadu;
101200090923         savksc = v2cksc;
101300090923         savctr = v2cctr;
101400090923         savimp = v2imp;
101500090923         savfimp = v2fimp;
101600090923         $forza = *off;
101700071207
101800071207       ENDSR;
101900071217
102000071217       //--------------------------------------------------------------
102100090306       //?Settaggio indicatori nella videata D02 per abilitazione      ?
102200090306       //?  tasti funzionali                                           ?
102300071217       //--------------------------------------------------------------
102400090306       BEGSR  Set_Ind_D02;
102500090319
102600090323         F4Attivo  = *on;
102700090306         F5Attivo  = ((%found(TNTBE01L) and TBEatb <> *blank)
102800071217                                        and B46opz <> '5');
102900071217         F6Attivo  = (NOT F5Attivo and B46opz <> '5');
103000090306         F16Attivo = ((%found(TNTBE01L) and TBEatb = *blank)
103100071217                                        and B46opz <> '5');
103200090306
103300090306         // Se ultima estrazione impostata proteggo la prima estrazione
103400090309         ProtDATAI = (§LACdatela <> *zero);
103500071217
103600071217       ENDSR;
103700090319
103800090319       //--------------------------------------------------------------
103900090319       //?Gestione tasto funzionale F04 da videata D02                 ?
104000090319       //?F04=Interrogazione Padre                                     ?
104100090319       //--------------------------------------------------------------
104200090319       BEGSR  sr_F04D02;
104300090319
104400090319         // Richiamo pgm interrogazione
104500090327         clear  Param01;
104600090327         P01ord = '1';
104700090327         P01ksu = V2Cksu;
104800090327         KPJBU  = Param01;
104900090327         tntb461r (KPJBA);
105000090319
105100090319       ENDSR;
105200090319
105300071217
105400071217       //--------------------------------------------------------------
105500090306       //?Gestione tasto funzionale F12 da videata D02                 ?
105600090306       //?F12=Ritorno                                                  ?
105700071217       //--------------------------------------------------------------
105800090306       BEGSR  sr_F12D02;
105900071217
106000090306         // Ritorno alla videata precedente se NON richiamato
106100090306         if  B46opz = *blank  and
106200090306             B46sif = *blank  and  B46lin = *blank  and
106300090306             B46ke1 = *blank  and  B46ke2 = *blank;
106400071217           clear V1Tlav;
106500071217           $Video = 'D1';
106600090306         // Ritorno al pgm chiamante se richiamato
106700071217         else;
106800071217           B46fxx = '2';
106900071217           $Fine = *on;
107000071217         endif;
107100071217
107200071217       ENDSR;
107300090306
107400090306       //--------------------------------------------------------------
107500090306       //?Controllo "annullabilità" del record (padre senza figli)     ?
107600090306       //--------------------------------------------------------------
107700090306       BEGSR  sr_CtrAnnull;
107800090306
107900090306         IndDspF = *zero;
108000090306         // REimpostazione indicatori per abilitazione tasti funzionali
108100090306         exsr  Set_Ind_D02;
108200090306
108300090306         if  V2Cksu <= *zero;
108400090306           V2Cksu = §LACksu;
108500090306         endif;
108600090306
108700090306         // Se figlio (non padre): fine controlli
108800090306         if  V2Cccm <> V2Cksu;
108900090306           leavesr;
109000090306         endif;
109100090306
109200090306         //?Verifica se padre con figli?
109300090327         clear  dLACp;
109400090316         SAVke1 = TBEke1;
109500090306         setll     %kds(k05tntbe01 : 1)  TNTBE000;
109600090306         reade(n)  %kds(k05tntbe01 : 1)  TNTBE000;
109700090306
109800090306         dow  not  %eof(TNTBE01L);
109900090316           if  TBEatb = *blank   and
110000090316               %editc(V2Cccm : 'X') <> %trimr(TBEke1);
110100090306             dLACp = TBEuni;
110200090316             if  p_§LACksu = V2Cccm;
110300090306               errMessage  = *on;
110400090306               errGenerico = *on;
110500090306               PosCurKsu   = *on;
110600090306               V1Dmsg = 'Record NON cancellabile: trattasi di PADRE +
110700090306                         con figli (vedi ' + %trimr(TBEke1) + ')';
110800090306               leavesr;
110900090306             endif;
111000090306           endif;
111100090306         reade(n)  %kds(k05tntbe01 : 1)  TNTBE000;
111200090306         enddo;
111300090316
111400090316         // - Riaggancio del rec. in manutenzione
111500090316         TBEke1 = SAVke1;
111600090316         reset  TNTBE000;
111700090316         k_TBEcod = c_Tab;
111800090316         k_TBEke1 = V1Cccm;
111900090316         clear  k_TBEke2;
112000090316         k_TBElin = TBXlin;
112100090316         k_TBEsif = TBXsif;
112200090316         chain  %kds(K05tntbe01) TNTBE000;
112300090306
112400090306       ENDSR;
112500071206
112600071212       //--------------------------------------------------------------
112700090306       //?Controllo videata D02                                        ?
112800071212       //--------------------------------------------------------------
112900090306       BEGSR  sr_CtrD02;
113000071207
113100090309         %subst(IndDspF : 50) = *off;
113200090313
113300090313         // Verifica se variata la schedulazione dell'estrazione
113400090313         if  $ByPass;
113500090313           $ByPass = (ds_SchMm = wSavSch);
113600090313         endif;
113700090313         wSavSch = ds_SchMm;
113800090225
113900090306         //?Codice padre?
114000090225         select;
114100090306           when  V2Cksu  = *zero;
114200090306             errMessage  = *on;
114300090225             errGenerico = *on;
114400090303             PosCurKsu   = *on;
114500090225             V1Dmsg = $Msg(11);
114600090225             leavesr;
114700090306           when  V2Cksu = V2Cccm;
114800090303             V2Dksu = V2Dccm;
114900090225           other;
115000090327             // - Controllo esistenza codice padre come cliente
115100090309             //   in tabella LAC
115200090303             exsr  sr_Padre;
115300090303             if  errGenerico = *on;
115400090226               leavesr;
115500090225             endif;
115600090303             V2Dksu = p_§LACrag;
115700090226         endsl;
115800101103         //?SE codice padre <> da codice cliente?
115900101103         // ?& sono in immissione?
116000101103         IF  V2Cccm <> V2Cksu   and   *in02;
116100101103           // ?=> chiedo se si vogliono riportare i dati del padre?
116200101103           If  W1Ccpy = *blank;
116300101103             $Video  = 'W1';
116400101103             leavesr;
116500101103           EndIf;
116600101103           // ?& il padre ha già fatto estrazioni?
116700101103           If  p_§LACdatela <> *zero;
116800101103             // ?=> riporto le date del padre, proteggo il campo e non controllo?
116900101103             clear wDataI;
117000101103             %subst(wDataI:1:2) = %subst(%editc(p_§LACdatai:'X'):5:2);
117100101103             %subst(wDataI:3:4) = %subst(%editc(p_§LACdatai:'X'):1:4);
117200101103             V2datai = %int(wDataI);
117300101103             clear wDatEla;
117400101103             %subst(wDatEla:1:2) = %subst(%editc(p_§LACdatela:'X'):5:2);
117500101103             %subst(wDatEla:3:4) = %subst(%editc(p_§LACdatela:'X'):1:4);
117600101103             V2datela = %int(wDatEla);
117700101103             ProtDATAI = *on;
117800101103           EndIf;
117900100521         ENDIF;
118000090311
118100090311         //?Tipo elaborazione?
118200090327         select;
118300090327           // - Obbligatorio
118400090327           when  V2Caut <> 'A'  and  V2Caut <> 'S'  and  V2Caut <> 'N';
118500090327             errMessage  = *on;
118600090327             errGenerico = *on;
118700090327             PosCurAut   = *on;
118800090327             V1Dmsg = 'Tipo elaborazione errato o mancante';
118900090327             leavesr;
119000090327           // - Se codice padre <> da codice cliente
119100090327           //   il tipo elaborazione deve essere uguale
119200090327           when  V2Cccm <> V2Cksu   and   p_§LACaut <> V2Caut;
119300090327             errMessage  = *on;
119400090327             errGenerico = *on;
119500090327             PosCurAut   = *on;
119600090327             V1Dmsg = 'Tipo elaborazione +
119700090327                       <> quello del cod. padre ('
119800090327                    +  p_§LACaut + ')';
119900090327             leavesr;
120000090327         endsl;
120100090327
120200090327         //?Tipo immagine?
120300090327         if  V2Ctpi <> 'TIF'   and   V2Ctpi <> 'JPG';
120400090327           errMessage  = *on;
120500090327           errGenerico = *on;
120600090327           PosCurTpi   = *on;
120700090327           V1Dmsg = 'Tipo immagine errato o mancante';
120800090327           leavesr;
120900090327         endif;
121000090226
121100090306         //?Flag nome immagine?
121200090310         clear  V2Dfmi;
121300090310         select;
121400090310           // - Obbligatorietà
121500090310           when  V2Cfmi = *blank;
121600090310             errMessage  = *on;
121700090310             errGenerico = *on;
121800090310             PosCurFmi   = *on;
121900090310             V1Dmsg = 'Flag nome immagine obbligatorio';
122000090310             leavesr;
122100090310           // - Ricerca
122200090310           when  %scan('?' : V2Cfmi) > *zero;
122300090310             clear  TIBS02ds;
122400090310             T02mod = 'R';
122500090310             T02cod = 'NIM';
122600090310             T02sif = KNSIF;
122700090310             TNTBE_RicercaControllo(kpjba : tibs02ds);
122800090310             if  T02err = *blank;
122900090310               V2Cfmi = %subst(T02ke1 : 1 : %len(V2Cfmi));
123000090310               V2Dfmi = T02uni;
123100090310               errGenerico = *on;
123200090310               leavesr;
123300090310             else;
123400090310               errMessage  = *on;
123500090310               errGenerico = *on;
123600090310               PosCurFmi   = *on;
123700090310               V1Dmsg = T02msg;
123800090310               leavesr;
123900090310             endif;
124000090310           // - Controllo
124100090310           other;
124200090310             reset  TIBS02ds;
124300090310             T02cod = 'NIM';
124400090310             T02ke1 = V2Cfmi;
124500090310             TNTBE_RicercaControllo(kpjba : tibs02ds);
124600090310             if  T02err <> *blank;
124700090310               errMessage  = *on;
124800090310               errGenerico = *on;
124900090310               PosCurFmi   = *on;
125000090310               V1Dmsg = 'Flag nome immagine errato';
125100090310               leavesr;
125200090310             endif;
125300090310             V2Dfmi = T02uni;
125400090310         ENDSL;
125500090226
125600090306         //?Directory per immagini?
125700090226         select;
125800090306           when  V2Cdir  = *blank;
125900090226             errMessage  = *on;
126000090226             errGenerico = *on;
126100090226             PosCurDir   = *on;
126200090226             V1Dmsg = $Msg(05);
126300090226             leavesr;
126400090316           when  %subst(V2Cdir : 1 : 1) = *blank;
126500090316             errMessage  = *on;
126600090316             errGenerico = *on;
126700090316             PosCurDir   = *on;
126800090316             V1Dmsg = 'Non ammessi spazi vuoti all''inizio del nome +
126900090316                       della directory';
127000090316             leavesr;
127100090306           when  %scan('\':V2Cdir) > *zero;
127200090226             errMessage  = *on;
127300090226             errGenerico = *on;
127400090226             PosCurDir   = *on;
127500090226             V1Dmsg = $Msg(06);
127600090226             leavesr;
127700090306           when  %scan('%':V2Cdir) > *zero   and
127800090306                           V2Caut  = 'A';
127900090226             errMessage  = *on;
128000090226             errGenerico = *on;
128100090226             PosCurDir   = *on;
128200090226             V1Dmsg = $Msg(07);
128300090226             leavesr;
128400090309           when  %scan('&':V2Cdir) > *zero  and
128500090309                           V2Caut  = 'A';
128600090309             errMessage  = *on;
128700090309             errGenerico = *on;
128800090309             PosCurDir   = *on;
128900090309             V1Dmsg = $Msg(07);
129000090309             leavesr;
129100090226         endsl;
129200090309
129300090316         wPos1 = %scan('/' : V2Cdir);
129400090316         DoW  wPos1 > *zero;
129500090316           if  %subst( V2Cdir : wPos1 : 2 ) = '//';
129600090316             errMessage  = *on;
129700090316             errGenerico = *on;
129800090316             PosCurDir   = *on;
129900090316             V1Dmsg = 'Nome directory NON valido: non ammesso il +
130000090316                       doppio "/"';
130100090316             leavesr;
130200090316           else;
130300090316             wPos1 = %scan('/' : V2Cdir : wPos1 + 1 );
130400090316           endif;
130500090316         EndDo;
130600090316
130700090309         wPos1 = %scan('&' : V2Cdir);
130800090309         IF  wPos1 > *zero;
130900090309           if  %subst( V2Cdir : wPos1 : 4 ) <> '&AAA'   and
131000090309               %subst( V2Cdir : wPos1 : 2 ) <> '&M';
131100090309             errMessage  = *on;
131200090309             errGenerico = *on;
131300090309             PosCurDir   = *on;
131400090309             V1Dmsg = 'Ammessi solo "&AAA" e "&M" (come caratteri +
131500090309                       sostituibili)';
131600090309             leavesr;
131700090309           endif;
131800090309           wPos2 = %scan( '&' : V2Cdir : wPos1 + 1 );
131900090309           If  wPos2 > *zero;
132000090309             if  %subst( V2Cdir : wPos2 : 4 ) <> '&AAA'   and
132100090309                 %subst( V2Cdir : wPos2 : 2 ) <> '&M';
132200090309               errMessage  = *on;
132300090309               errGenerico = *on;
132400090309               PosCurDir   = *on;
132500090309               V1Dmsg = 'Ammessi solo "&AAA" e "&M" (come caratteri +
132600090309                         sostituibili)';
132700090309               leavesr;
132800090309             endif;
132900090309           EndIf;
133000090309         ENDIF;
133100090309
133200090309         wPos1 = %scan('&M' : V2Cdir);
133300090309         IF  wPos1 > *zero;
133400090309           wPos2 = %scan( '&M' : V2Cdir : wPos1 + 1 );
133500090309           If  wPos2 > *zero   and
133600090309               %scan( '&M' : V2Cdir : wPos2 + 1 ) > *zero;
133700090309             errMessage  = *on;
133800090309             errGenerico = *on;
133900090309             PosCurDir   = *on;
134000090309             V1Dmsg = 'Ammessi max 2 "&M"';
134100090309             leavesr;
134200090309           EndIf;
134300090309         ENDIF;
134400090226
134500090327         // - Se codice padre <> da codice cliente
134600090306         //   la directory deve coincidere almeno fino alla 2ª subdir
134700090306         IF  V2Cccm <> V2Cksu   and   p_§LACdir <> V2Cdir;
134800090311
134900090311           // - Controllo 1ª directory
135000090311           wPos1 = %scan( '/' : %trim( V2Cdir ) );
135100090311           wPos2 = %scan( '/' : %trim( p_§LACdir ) );
135200090313           If  wPos1 <> wPos2                               OR
135300090313             ( wPos1  > *zero   and   wPos2 > *zero   and
135400090311               %subst( %trim( V2Cdir    ) : 1 : wPos1 ) <>
135500090313               %subst( %trim( p_§LACdir ) : 1 : wPos2 ) )   OR
135600090313             ( wPos1  = *zero   and   wPos2  =  *zero
135700090313                                and   V2Cdir <> p_§LACdir );
135800090311             errMessage  = *on;
135900090311             errGenerico = *on;
136000090311             PosCurDir   = *on;
136100090311             select;
136200090311               when  wPos1 =  *zero   and   wPos2 <> *zero;
136300090316                 V1Dmsg = 'Il padre ha altri rami directory (';
136400090311               when  wPos1 <> *zero   and   wPos2 =  *zero;
136500090311                 V1Dmsg = 'Il padre ha una directory unica (';
136600090311               other;
136700090311                 V1Dmsg = 'Primo ramo directory +
136800090311                           <> da quello del cod. padre (';
136900090311             EndSl;
137000090313             if  %len( %trim( p_§LACdir ) ) <=
137100090311                 %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) ) - 1;
137200090311               V1Dmsg = %trimr( V1Dmsg )
137300090311                      + %trim( p_§LACdir ) + ')';
137400090311             else;
137500090313               wPos1 = %len( V1Dmsg );
137600090313               wPos2 = %len( %trimr( V1Dmsg ) );
137700090311               V1Dmsg = %trimr( V1Dmsg )
137800090311                      + %trim( %subst( p_§LACdir : 1 :
137900090311                          %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) )
138000090311                          - 4 ) )
138100090311                      + '...)';
138200090311             endif;
138300090311             leavesr;
138400090311           EndIf;
138500090311
138600090311           // - Controllo 2ª directory
138700090311           wPos1 = %scan( '/' : %trim( V2Cdir ) : wPos1 + 1 );
138800090311           wPos2 = %scan( '/' : %trim( p_§LACdir ) : wPos2 + 1 );
138900090316           //If  wPos1 <> wPos2                               OR
139000090316           //  ( wPos1  > *zero   and   wPos2 > *zero   and
139100090316           //    %subst( %trim( V2Cdir    ) : 1 : wPos1 ) <>
139200090316           //    %subst( %trim( p_§LACdir ) : 1 : wPos2 ) )   OR
139300090316           //  ( wPos1  = *zero   and   wPos2  =  *zero
139400090316           //                     and   V2Cdir <> p_§LACdir );
139500090316           if  wPos1 = *zero;
139600090316             wPos1 = %len( %trimr( V2Cdir ) );
139700090316           else;
139800090316             wPos1 -= 1;
139900090316           endif;
140000090316           if  wPos2 = *zero;
140100090316             wPos2 = %len( %trimr( p_§LACdir ) );
140200090316           else;
140300090316             wPos2 -= 1;
140400090316           endif;
140500090316           If  wPos1 <> wPos2   OR
140600090316               %subst(V2Cdir : 1 : wPos1) <>
140700090316                 %subst(p_§LACdir : 1 : wPos2);
140800090311             errMessage  = *on;
140900090311             errGenerico = *on;
141000090311             PosCurDir   = *on;
141100090316             //select;
141200090316             //  when  wPos1 =  *zero   and   wPos2 <> *zero;
141300090316             //    V1Dmsg = 'Il padre ha altri rami directory (';
141400090316             //  when  wPos1 <> *zero   and   wPos2 =  *zero;
141500090316             //    V1Dmsg = 'Il padre ha meno rami directory (';
141600090316             //  other;
141700090313                 V1Dmsg = 'Secondo ramo directory +
141800090311                           <> da quello del cod. padre (';
141900090316             //EndSl;
142000090313             if  %len( %trim( p_§LACdir ) ) <=
142100090311                 %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) ) - 1;
142200090311               V1Dmsg = %trimr( V1Dmsg )
142300090311                      + %trim( p_§LACdir ) + ')';
142400090311             else;
142500090311               V1Dmsg = %trimr( V1Dmsg )
142600090311                      + %trim( %subst( p_§LACdir : 1 :
142700090311                          %len( V1Dmsg ) - %len( %trimr( V1Dmsg ) )
142800090311                          - 4 ) )
142900090311                      + '...)';
143000090311             endif;
143100090311             leavesr;
143200090311           EndIf;
143300090311
143400090306         ENDIF;
143500090327
143600090327         //?Tipo porto?
143700090327         if  V2Ctpt <> 'F'   and   V2Ctpt <> 'A'   and   V2Ctpt <> 'E';
143800090327           errMessage  = *on;
143900090327           errGenerico = *on;
144000090327           PosCurTpt   = *on;
144100090327           V1Dmsg = 'Tipo porto errato o mancante';
144200090327           leavesr;
144300090327         endif;
144400090311
144500090311         //?LNP per selezione?
144600090311         if  V2Clnp <> *zero;
144700090311           setll  (V2Clnp)  AZORG;
144800090311           if  NOT  %equal(AZORG01L);
144900090311             errMessage  = *on;
145000090311             errGenerico = *on;
145100090311             PosCurLnp   = *on;
145200090311             V1Dmsg = 'Linea di partenza errata';
145300090311             leavesr;
145400090311           endif;
145500090311         endif;
145600090226
145700090309         //?Prima estrazione dal?
145800090310         select;
145900090310           // - Obbligatoria se tipo elaborazione CD
146000090310           when  V2Caut = 'S'   and   V2datai = *zero;
146100090310             errMessage  = *on;
146200090310             errGenerico = *on;
146300090310             PosCurDatai = *on;
146400090310             V1Dmsg = 'Periodo iniziale della prima estrazione +
146500090310                       obbligatorio';
146600090310             leavesr;
146700090310           // - Verifica correttezza
146800090310           when  V2datai <> *zero                                  AND
146900090310              (%subst( %editc( V2datai : 'X' ) : 1 : 2 ) < '01'   or
147000090310               %subst( %editc( V2datai : 'X' ) : 1 : 2 ) > '12'   or
147100090310               %subst( %editc( V2datai : 'X' ) : 3 : 4 ) < '2000' or
147200090310               %subst( %editc( V2datai : 'X' ) : 3 : 4 ) > %char(*year));
147300090310             errMessage  = *on;
147400090310             errGenerico = *on;
147500090310             PosCurDatai = *on;
147600090310             V1Dmsg = 'Periodo iniziale della prima estrazione +
147700090310                       errato';
147800090310             leavesr;
147900090310         endsl;
148000090306         // - Se codice padre <> da codice cliente
148100090309         //   il periodo deve essere uguale
148200100521         if  V2Cccm <> V2Cksu;
148300090310           clear  wDataI;
148400090316           %subst( wDataI : 1 : 2 ) =
148500090316                   %subst( %editc( p_§LACdatai : 'X' ) : 5 : 2 );
148600090316           %subst( wDataI : 3 : 4 ) =
148700090316                   %subst( %editc( p_§LACdatai : 'X' ) : 1 : 4 );
148800090310           if  V2datai <> %int(wDataI);
148900090310             errMessage  = *on;
149000090310             errGenerico = *on;
149100090310             PosCurDatai = *on;
149200090310             V1Dmsg = 'Periodo iniziale della prima estrazione +
149300090310                       <> da quello del cod. padre ('
149400090310                    + %subst(%editc(p_§LACdatai : 'X') : 5 : 2)
149500090310                    + '/'
149600090310                    + %subst(%editc(p_§LACdatai : 'X') : 1 : 4)
149700090310                    + ')';
149800090310             leavesr;
149900090310           endif;
150000090226         endif;
150100090306
150200090309         //?Tipo data da elaborare?
150300090310         select;
150400090327           when  V2tdta <> ' '  and  V2tdta <> 'C'  and  V2tdta <> 'S';
150500090327             errMessage  = *on;
150600090327             errGenerico = *on;
150700090327             PosCurTdta  = *on;
150800090327             V1Dmsg = '"Flag "Data da elaborare" errato';
150900090327             leavesr;
151000090310           // - Tipo data da elaborare obbligatorio
151100090310           when  V2Caut = 'S'   and   V2tdta = *blank;
151200090310             errMessage  = *on;
151300090310             errGenerico = *on;
151400090310             PosCurTdta  = *on;
151500090310             V1Dmsg = '"Data da elaborare" obbligatoria per +
151600090310                       "Tipo Elaborazione" = "S" (schedulata)';
151700090310             leavesr;
151800090310           // - Se codice padre <> da codice cliente
151900090310           //   il tipo data da elaborare deve essere uguale
152000090310           when  V2Cccm <> V2Cksu   and   p_§LACtdta <> V2tdta;
152100090310             errMessage  = *on;
152200090310             errGenerico = *on;
152300090310             PosCurTdta  = *on;
152400090310             V1Dmsg = 'Tipo data da elaborare +
152500090310                       <> da quello del cod. padre ('
152600090310                    + p_§LACtdta + ')';
152700090310             leavesr;
152800090310         endsl;
152900090309
153000090309         //?Periodo da estrarre in mesi?
153100090310         select;
153200090310           // - Periodo da estrarre in mesi obbligatorio
153300090310           when  V2Caut = 'S'   and   V2mesi = *zero;
153400090310             errMessage  = *on;
153500090310             errGenerico = *on;
153600090310             PosCurMesi  = *on;
153700090310             V1Dmsg = '"Periodo da estrarre" obbligatorio per +
153800090310                       "Tipo Elaborazione" = "S" (schedulata)';
153900090310             leavesr;
154000090310           // - Se codice padre <> da codice cliente
154100090310           //   il periodo da estrarre deve essere uguale
154200090310           when  V2Cccm <> V2Cksu   and   p_§LACmesi <> V2mesi;
154300090310             errMessage  = *on;
154400090310             errGenerico = *on;
154500090310             PosCurMesi  = *on;
154600090310             V1Dmsg = 'Periodo da estrarre +
154700090310                       <> da quello del cod. padre ('
154800090310                    + %editc(p_§LACmesi:'X') + ')';
154900090310             leavesr;
155000090310         endsl;
155100090226
155200090309         //?Schedulazione giorno e mesi x preparazione CD?
155300090226         select;
155400090318           // when (V2Caut  =  'A'    or  V2Caut  =  'N')    AND
155500090318           when  V2Caut  =  'A'    AND
155600090311                (V2datai <> *zero  or  V2tdta   <> *blank   or
155700090311                 V2mesi  <> *zero  or  V2Cschgg <> *zero    or
155800090311                 ds_SchMm <> *blank);
155900090311             errMessage  = *on;
156000090311             errGenerico = *on;
156100090311             PosCurAut   = *on;
156200090311             V1Dmsg = 'Schedulazione incompatibile con il +
156300090311                       "Tipo Elaborazione" = ' + V2Caut;
156400090311             leavesr;
156500090318           when  V2Caut = 'S'  and  (V2Cschgg <= *zero);
156600090306             errMessage  = *on;
156700090306             errGenerico = *on;
156800090306             PosCurSchGg = *on;
156900090306             V1Dmsg = 'Schedulazione obbligatoria per +
157000090306                       "Tipo Elaborazione" = "S" (schedulata)';
157100090306             leavesr;
157200090306           when  V2Caut = 'S'  and  (ds_SchMm = *blank);
157300090306             errMessage  = *on;
157400090306             errGenerico = *on;
157500090306             PosCurSch01 = *on;
157600090306             V1Dmsg = 'Indicare almeno un mese per +
157700090306                       "Tipo Elaborazione" = "S" (schedulata)';
157800090306             leavesr;
157900090311         endsl;
158000090226
158100090327         // - Se codice padre <> da codice cliente
158200090306         //   i 2 codici devono avere gli stessi dati di estrazione
158300090306         //   gg e mese e n. mesi da elaborare
158400090309         if  V2Cccm <> V2Cksu;
158500090309           select;
158600090309             when  p_§LACschgg <> V2Cschgg;
158700090226               errMessage  = *on;
158800090226               errGenerico = *on;
158900090303               PosCurSchgg = *on;
159000090309               V1Dmsg = 'Giorno creazione CD +
159100090309                         <> da quello del cod. padre ('
159200090309                      + %editc(p_§LACschgg:'X') + ')';
159300090226               leavesr;
159400090303             when  p_SchMm <> ds_SchMm;
159500090226               errMessage  = *on;
159600090226               errGenerico = *on;
159700090226               PosCursch01 = *on;
159800090309               V1Dmsg = 'Mesi di elaborazione +
159900090309                         <> da quelli del cod. padre ('
160000090309                      + p_SchMm + ')';
160100090226               leavesr;
160200090226           endsl;
160300090226         endif;
160400090309
160500090327         // - Verifica congruenza tra mese di schedulazione e
160600090309         //   mesi di estrazione
160700090309         select;
160800090311           // - Nessun dato inserito
160900090310           when  V2mesi   = *zero    and   V2Cschgg = *zero    and
161000090311                 ds_SchMm = *blank;
161100090311           // - Periodo da estrarre in mesi richiesto
161200090310           when  V2mesi = *zero;
161300090310             errMessage  = *on;
161400090310             errGenerico = *on;
161500090310             PosCurMesi  = *on;
161600090310             V1Dmsg = 'Periodo da estrarre obbligatorio SE inserita +
161700090310                       una schedulazione';
161800090310             leavesr;
161900090311           // - Giorno di schedulazione richiesto
162000090310           when  V2Cschgg = *zero;
162100090310             errMessage  = *on;
162200090310             errGenerico = *on;
162300090310             PosCurSchgg = *on;
162400090311             V1Dmsg = $Msg(08);
162500090310             leavesr;
162600090311           // - Selezione mesi richiesta
162700090311           when  ds_SchMm = *blank;
162800090310             errMessage  = *on;
162900090310             errGenerico = *on;
163000090310             PosCurSch01 = *on;
163100090311             V1Dmsg = $Msg(09);
163200090310             leavesr;
163300090311         endsl;
163400090313
163500090313         // - Conteggio/Controllo mesi schedulati
163600090313         clear  wCount;
163700090311         wSchMm_ds = %triml(ds_SchMm);
163800090313         For  xx = 1  To  %elem($SchMm);
163900090313           if  $SchMm(xx) <> *blank;
164000090313             wCount += 1;
164100090313           endif;
164200090313         EndFor;
164300090313
164400090311         select;
164500090313           // - Errori bloccanti:
164600090313           when   wCount <> *zero   and
164700090313                 (V2mesi <> (12 / wCount));
164800090313             errMessage  = *on;
164900090313             errGenerico = *on;
165000090313             PosCurSch01 = *on;
165100090313             V1Dmsg = 'Incongruenza tra il "periodo da estrarre" +
165200090313                       ed i mesi schedulati';
165300090313           // - Errori bypassabili:
165400090313           when ( (V2mesi =  2   and  ($SchMm(03) = *blank     or
165500090311                                       $SchMm(05) = *blank     or
165600090311                                       $SchMm(07) = *blank     or
165700090311                                       $SchMm(09) = *blank     or
165800090311                                       $SchMm(11) = *blank))   or
165900090313                  (V2mesi =  3   and  ($SchMm(04) = *blank     or
166000090311                                       $SchMm(07) = *blank     or
166100090311                                       $SchMm(10) = *blank))   or
166200090313                  (V2mesi =  4   and  ($SchMm(05) = *blank     or
166300090311                                       $SchMm(09) = *blank))   or
166400090313                  (V2mesi =  6   and   $SchMm(07) = *blank) )  AND
166500090313                 $ByPass =  *off;
166600090313             $ByPass = *on;
166700090309             errMessage  = *on;
166800090309             errGenerico = *on;
166900090310             PosCurSch01 = *on;
167000090313             V1Dmsg = 'Mesi schedulati in maniera irregolare... +
167100090313                       Premere Enter per forzare.';
167200090309             leavesr;
167300090313           when ( (V2mesi =  2  and  ($SchMm(02) <> *blank     or
167400090311                                      $SchMm(04) <> *blank     or
167500090311                                      $SchMm(06) <> *blank     or
167600090311                                      $SchMm(08) <> *blank     or
167700090311                                      $SchMm(10) <> *blank     or
167800090311                                      $SchMm(12) <> *blank))   OR
167900090313                  (V2mesi =  3  and  ($SchMm(02) <> *blank     or
168000090311                                      $SchMm(03) <> *blank     or
168100090311                                      $SchMm(05) <> *blank     or
168200090311                                      $SchMm(06) <> *blank     or
168300090311                                      $SchMm(08) <> *blank     or
168400090311                                      $SchMm(09) <> *blank     or
168500090311                                      $SchMm(11) <> *blank     or
168600090311                                      $SchMm(12) <> *blank))   OR
168700090313                  (V2mesi =  4  and  ($SchMm(02) <> *blank     or
168800090311                                      $SchMm(03) <> *blank     or
168900090311                                      $SchMm(04) <> *blank     or
169000090311                                      $SchMm(06) <> *blank     or
169100090311                                      $SchMm(07) <> *blank     or
169200090311                                      $SchMm(08) <> *blank     or
169300090311                                      $SchMm(10) <> *blank     or
169400090311                                      $SchMm(11) <> *blank     or
169500090311                                      $SchMm(12) <> *blank))   OR
169600090313                  (V2mesi =  6  and  ($SchMm(02) <> *blank     or
169700090311                                      $SchMm(03) <> *blank     or
169800090311                                      $SchMm(04) <> *blank     or
169900090311                                      $SchMm(05) <> *blank     or
170000090311                                      $SchMm(06) <> *blank     or
170100090311                                      $SchMm(08) <> *blank     or
170200090311                                      $SchMm(09) <> *blank     or
170300090311                                      $SchMm(10) <> *blank     or
170400090311                                      $SchMm(11) <> *blank     or
170500090311                                      $SchMm(12) <> *blank))   OR
170600090313                  (V2mesi = 12  and  ($SchMm(02) <> *blank     or
170700090311                                      $SchMm(03) <> *blank     or
170800090311                                      $SchMm(04) <> *blank     or
170900090311                                      $SchMm(05) <> *blank     or
171000090311                                      $SchMm(06) <> *blank     or
171100090311                                      $SchMm(07) <> *blank     or
171200090311                                      $SchMm(08) <> *blank     or
171300090311                                      $SchMm(09) <> *blank     or
171400090311                                      $SchMm(10) <> *blank     or
171500090311                                      $SchMm(11) <> *blank     or
171600090313                                      $SchMm(12) <> *blank)) ) AND
171700090313                 $ByPass =  *off;
171800090313             $ByPass = *on;
171900090310             errMessage  = *on;
172000090310             errGenerico = *on;
172100090311             PosCurSch01 = *on;
172200090313             V1Dmsg = 'Mesi schedulati in maniera irregolare... +
172300090313                       Premere Enter per forzare.';
172400090310             leavesr;
172500090313           // - Controllo giorno del mese (bypassabile)
172600090313           when ( (V2Cschgg > 28  and  V2Csch02 <> *blank)  or
172700090313                  (V2Cschgg > 30  and (V2Csch04 <> *blank
172800090313                                   or  V2Csch06 <> *blank
172900090313                                   or  V2Csch09 <> *blank
173000090313                                   or  V2Csch11 <> *blank)) )
173100090313                and  $CtrMm = *off;
173200090313             $CtrMm = *on;
173300090313             errMessage  = *on;
173400090313             errGenerico = *on;
173500090313             PosCurSch01 = (V2Csch01 <> *blank);
173600090313             PosCurSch02 = (V2Csch02 <> *blank);
173700090313             PosCurSch04 = (V2Csch04 <> *blank);
173800090313             PosCurSch06 = (V2Csch06 <> *blank);
173900090313             PosCurSch09 = (V2Csch09 <> *blank);
174000090313             PosCurSch11 = (V2Csch11 <> *blank);
174100090313             V1Dmsg = %replace( %char(V2Cschgg) : $Msg(10) :
174200090313                                %scan('&g':$Msg(10)) );
174300090313             leavesr;
174400090313         endsl;
174500090226
174600090309         //?Frequenza addebito?
174700090310         select;
174800090327           // - Obbligatoria
174900090327           when  V2tadu <> 'I'   and   V2tadu <> 'J'   and
175000090327                 V2tadu <> 'S'   and   V2tadu <> 'M';
175100090327             errMessage  = *on;
175200090327             errGenerico = *on;
175300090327             PosCurTadu  = *on;
175400090327             V1Dmsg = 'Frequenza addebito errata o mancante';
175500090327             leavesr;
175600090327           // - Se tipo elaborazione CD la creazione addebito non può
175700090313           //   essere mensile o settimanale
175800090310           when  V2Caut = 'S'  and  (V2tadu = 'S'  or  V2tadu = 'M');
175900090310             errMessage  = *on;
176000090310             errGenerico = *on;
176100090310             PosCurTadu  = *on;
176200090310             V1Dmsg = 'Per creazione CD non è possibile addebito +
176300090310                       mensile o settimanale';
176400090310             leavesr;
176500090327           // - Se codice padre <> da codice cliente
176600090327           //   devono avere la stessa frequenza addebito
176700090310           when  V2Cccm <> V2Cksu   and   p_§LACtadu <> V2tadu;
176800090310             errMessage  = *on;
176900090310             errGenerico = *on;
177000090310             PosCurTadu  = *on;
177100090310             V1Dmsg = 'Frequenza addebito +
177200090310                        <> da quella del cod. padre ('
177300090310                    + p_§LACtadu + ')';
177400090310             leavesr;
177500090310         endsl;
177600090309
177700090309         //?Tipo addebito (Creazione TITAS)?
177800090310         select;
177900090327           // - Obbligatorio
178000090327           when  V2Ctad <> 'N'  and  V2Ctad <> 'S'  and  V2Ctad <> 'V';
178100090327             errMessage  = *on;
178200090327             errGenerico = *on;
178300090327             PosCurTad   = *on;
178400090327             V1Dmsg = 'Flag "Creazione TITAS" errato o mancante';
178500090327             leavesr;
178600090327           // - Se "N" non impostare i dati dell'addebito forzato
178700090310           when  V2Ctad = 'N'     and
178800090310                (V2Cksc <> *zero  or  V2Cctr <> *blank
178900090310                                  or  V2imp <> *zero);
179000090310             errMessage  = *on;
179100090310             errGenerico = *on;
179200090310             PosCurTad   = *on;
179300090310             V1Dmsg = 'Se NO addebito non impostare le forzature';
179400090310             leavesr;
179500090327           // - Se codice padre <> da codice cliente
179600090310           //   devono avere lo stesso tipo addebito
179700090310           when  V2Cccm <> V2Cksu   and   p_§LACtad <> V2Ctad;
179800090310             errMessage  = *on;
179900090310             errGenerico = *on;
180000090310             PosCurTad   = *on;
180100090310             V1Dmsg = 'Tipo addebito +
180200090310                       <> da quello del cod. padre ('
180300090310                        + p_§LACtad + ')';
180400090310             leavesr;
180500090310         endsl;
180600090226
180700090311         //?Dati da forzare in tassazione:?
180800090312         //?- Codice cliente / Tariffa?
180900090312         clear  TIBS69ds;
181000090312         if  V2Cksc <> *zero;
181100090312           tibs69ds.I69kcc = DUTkci;
181200090312           tibs69ds.I69kac = %int(V2Cksc);
181300090312           tibs69ds.I69sif = knsif;
181400090312           tibs69r(TIBS69ds : ds_CNACO :
181500090312                              ds_CNIND :
181600090312                              ds_CNCLP :
181700090312                              ds_FNCLS);
181800090312         endif;
181900090312         select;
182000090312           when  tibs69ds.O69err = *on;
182100090312             errMessage  = *on;
182200090312             errGenerico = *on;
182300090312             PosCurKsc   = *on;
182400090312             V1Dmsg = 'Cliente tassazione errato';
182500090312             leavesr;
182600090312           when  V2Cksc <> *zero   and   V2Cctr = *blank;
182700090312             errMessage  = *on;
182800090312             errGenerico = *on;
182900090312             PosCurCtr   = *on;
183000090312             V1Dmsg = 'Codice tariffa obbligatorio SE +
183100090312                       inserito il cliente tassazione';
183200090312             leavesr;
183300090311           // - Non ammessa tariffa senza cliente
183400090311           when  V2Cksc =  *zero   and   V2Cctr <> *blank;
183500090311             errMessage  = *on;
183600090311             errGenerico = *on;
183700090311             PosCurKsc   = *on;
183800090311             V1Dmsg = 'Cliente tassazione obbligatorio SE +
183900090311                       inserita la relativa tariffa';
184000090311             leavesr;
184100090311           // - Ammesso solo un codice tariffa numerico
184200090311           when  V2Cctr <> *blank   and
184300090311                 %check(c_Digits : V2Cctr) > *zero;
184400090311             errMessage  = *on;
184500090311             errGenerico = *on;
184600090312             PosCurCtr   = *on;
184700090312             V1Dmsg = 'Ammessi SOLO caratteri numerici';
184800090311             leavesr;
184900090310         endsl;
185000090226
185100090327         // - Se codice padre <> da codice cliente
185200090306         //   stesso ksc/ctr/importo
185300090303         if  V2Cccm <> V2Cksu;
185400090226           select;
185500090303             when  p_§LACksc <> V2Cksc;
185600090226               errMessage  = *on;
185700090226               errGenerico = *on;
185800090303               PosCurKsc   = *on;
185900090309               V1Dmsg = 'Cliente tassazione +
186000090309                         <> da quello del cod. padre ('
186100090309                      + %editc(p_§LACksc : 'X') + ')';
186200090226               leavesr;
186300090303             when  p_§LACctr <> V2Cctr;
186400090226               errMessage  = *on;
186500090226               errGenerico = *on;
186600090303               PosCurCtr   = *on;
186700090309               V1Dmsg = 'Codice tariffa +
186800090309                         <> quello del cod. padre ('
186900090309                      + p_§LACctr + ')';
187000090226               leavesr;
187100090226           endsl;
187200090226         endif;
187300090226
187400090309         //?Importo & Tipo importo forzato?
187500090310         select;
187600090310           // - Non ammesso importo per Varia Negata
187700090310           when  V2Ctad = 'V'   and   V2imp <> *zero;
187800090310             errMessage  = *on;
187900090310             errGenerico = *on;
188000090310             PosCurImp   = *on;
188100090310             V1Dmsg = 'NON ammesso l''importo forzato per +
188200090310                       Varia Negata';
188300090310             leavesr;
188400090310           // - Non ammesso tipo importo senza importo
188500090310           when  V2imp =  *zero   and   V2fimp <> *blank;
188600090310             errMessage  = *on;
188700090310             errGenerico = *on;
188800090310             PosCurImp   = *on;
188900090310             V1Dmsg = 'Importo forzato obbligatorio SE +
189000090310                       inserito il tipo di importo';
189100090310             leavesr;
189200090312           // - Se inserito importo devono essere inseriti:
189300090312           //   · il codice cliente forzato (ksc),
189400090312           //   · la tariffa forzata (ctr) ed
189500090312           //   · il tipo importo (fimp)
189600090310           when  V2imp <> *zero;
189700090310             select;
189800090310               when  V2fimp  = *blank;
189900090310                 errMessage  = *on;
190000090310                 errGenerico = *on;
190100090310                 PosCurFimp  = *on;
190200090310                 V1Dmsg = 'Immettere il tipo importo';
190300090310                 leavesr;
190400090310               when  V2Cksc  = *zero;
190500090310                 errMessage  = *on;
190600090310                 errGenerico = *on;
190700090310                 PosCurKsc   = *on;
190800090310                 V1Dmsg = 'Immettere il cliente forzato';
190900090310                 leavesr;
191000090312               when  V2Cctr  = *blank;
191100090310                 errMessage  = *on;
191200090310                 errGenerico = *on;
191300090310                 PosCurCtr   = *on;
191400090310                 V1Dmsg = 'Immettere la tariffa forzata';
191500090310                 leavesr;
191600090310             endsl;
191700090310         endsl;
191800090312
191900090327         // Verifica compatibilità dell'importo forzato con il
192000090312         //   massimo imponibile IVA in tab. "GEI"/"LAC".
192100090312         if  V2imp <> *zero   and   V2fimp = 'T';
192200090312           // - Reperimento divisa moneta di conto gestionale
192300090312           clear  dGED;
192400090312           reset  TIBS02ds;
192500090312           T02cod = 'GED';
192600090312           T02ke1 = '1';
192700090312           TNTBE_RicercaControllo(kpjba : tibs02ds);
192800090312           if  T02err = *blank;
192900090312             dGED = T02uni;
193000090312           endif;
193100090312           // - Reperimento importi da tab. "GEI"
193200090312           //   (interessa il massimo imponibile IVA per spedizione)
193300090312           clear  dGEI;
193400090312           reset  TIBS02ds;
193500090312           T02cod = 'GEI';
193600090312           T02ke1 = §GEDcn;
193700090312           TNTBE_RicercaControllo(kpjba : tibs02ds);
193800090312           if  T02err = *blank;
193900090312             dGEI = T02uni;
194000090312           endif;
194100090312           if  V2imp > §GEImf;
194200090312             errMessage  = *on;
194300090312             errGenerico = *on;
194400090312             PosCurImp   = *on;
194500090312             V1Dmsg = 'Importo superiore al MASSIMO IMPONIBILE IVA +
194600090312                       PER SPEDIZIONE ('
194700090312                      + %trim( %editc( §GEImf : 'J' ) ) + ')';
194800090312             leavesr;
194900090312           endif;
195000090312         endif;
195100090226
195200090312         // Se codice padre <> da codice cliente
195300090306         //   stesso importo e tipo importo
195400090303         if  V2Cccm <> V2Cksu;
195500090226           select;
195600090303             when  p_§LACimp <> V2imp;
195700090226               errMessage  = *on;
195800090226               errGenerico = *on;
195900090312               PosCurImp   = *on;
196000090309               V1Dmsg = 'Importo forzato +
196100090309                         <> da quello del cod. padre ('
196200090309                      + %editc(p_§LACimp : 'J') + ')';
196300090226               leavesr;
196400090303             when  p_§LACfimp <> V2fimp;
196500090226               errMessage  = *on;
196600090226               errGenerico = *on;
196700090312               PosCurFimp  = *on;
196800090309               V1Dmsg = 'Tipo importo forzato +
196900090309                         <> da quello del cod. padre ('
197000090309                      + p_§LACfimp + ')';
197100090226               leavesr;
197200090226           endsl;
197300090225         endif;
197400090923
197500090923         //?Modifica dati sensibili per l'addebito
197600090923         if (v2tadu <> savtadu or v2ctad <> savtad or
197700090923             v2cksc <> savksc or v2cctr <> savctr or
197800090923             v2imp <> savimp or v2fimp <> savfimp) and not *in02
197900090923             and not $forza;
198000090923               errMessage  = *on;
198100090923               errGenerico = *on;
198200090923               PosCurTadu  = *on;
198300090923               V1Dmsg = 'ATTENZIONE!! Possibili problemi ' +
198400090923                        'in fase di addebito. Avvisare Monica/Nicola';
198500090923               $forza = *on;
198600090923               leavesr;
198700090923         endif;
198800110713
198900110713         //?Creazione file elenco per il cliente
199000110713         //?possibile solo se NON è elaborazione automatica (FTP)
199100110713         IF  V2pgm <> *blanks and V2caut = 'A';
199200110713           ErrMessage  = *on;
199300110713           ErrGenerico = *on;
199400110713           PosCurPgm   = *on;
199500110713           V1Dmsg = 'NON possibile creazione elenco se elaborazione '+
199600110713                    'automatica';
199700110713           leavesr;
199800110713         ENDIF;
199900110713         //?se inserito il nome del programma deve esserci anche il tipo elenco
200000110713         IF  V2pgm <> *blanks and V2ele = *blanks;
200100110713           ErrMessage  = *on;
200200110713           ErrGenerico = *on;
200300110713           PosCurEle   = *on;
200400110713           V1Dmsg = 'Immettere anche il tipo elenco da creare';
200500110713           leavesr;
200600110713         ENDIF;
200700110713         //?e viceversa se c'è il tipo elenco deve esserci anche il nome del pgm
200800110713         IF  V2pgm = *blanks and V2ele <> *blanks;
200900110713           ErrMessage  = *on;
201000110713           ErrGenerico = *on;
201100110713           PosCurPgm   = *on;
201200110713           V1Dmsg = 'Immettere anche il nome del programma x elenco da creare';
201300110713           leavesr;
201400110713         ENDIF;
201500110713         //?Se codice padre <> da codice cliente
201600110713         //?  devono avere gli stessi dati
201700110713         IF  V2Cccm <> V2Cksu;
201800110713           SELECT;
201900110713             WHEN  p_§LACpgm <> V2pgm;
202000110713               ErrMessage  = *on;
202100110713               ErrGenerico = *on;
202200110713               PosCurPgm   = *on;
202300110713               V1Dmsg = 'Nome programma +
202400110713                         <> da quello del cod. padre ('
202500110713                      + p_§LACpgm + ')';
202600110713               leavesr;
202700110713             WHEN  p_§LACele <> V2ele;
202800110713               ErrMessage  = *on;
202900110713               ErrGenerico = *on;
203000110713               PosCurEle   = *on;
203100110713               V1Dmsg = 'Tipo elenco +
203200110713                         <> da quello del cod. padre ('
203300110713                      + p_§LACele + ')';
203400110713               leavesr;
203500110713           ENDSL;
203600110713         ENDIF;
203700071207
203800071207       ENDSR;
203900090225
204000090225       //--------------------------------------------------------------
204100090306       //?Controllo esistenza del codice padre in tabella LAC          ?
204200090225       //--------------------------------------------------------------
204300090303       BEGSR  sr_Padre;
204400090302
204500090327         // Controlla esistenza in tabella "LAC"
204600090310         clear  dLACp;
204700090310         reset  TIBS02ds;
204800090310         T02sif = KNSIF;
204900090310         %subst(T02ke1 : 1 : 7) = %editc(V2Cksu : 'X');
205000090310         TNTBE_RicercaControllo(kpjba : tibs02ds);
205100090310         if  T02err = *blank;
205200090310           dLACp  = T02uni;
205300090310         else;
205400090310           errMessage  = *on;
205500090310           errGenerico = *on;
205600090310           PosCurksu   = *on;
205700090310           V1Dmsg = $Msg(12);
205800090310           leavesr;
205900090310         endif;
206000090327
206100090327         // Controlla se già inserito come figlio di altro padre
206200090327         if  p_§LACksu <> V2Cksu;
206300090327           errMessage  = *on;
206400090327           errGenerico = *on;
206500090327           PosCurKsu   = *on;
206600090327           V1Dmsg = 'Non ammesso cod. PADRE già FIGLIO di altro +
206700090327                     cliente (' + %editc(p_§LACksu : 'X') + ')';
206800090327           leavesr;
206900090327         endif;
207000090225
207100090225       ENDSR;
207200101103
207300101103       //--------------------------------------------------------------
207400101103       //?Gestione window W01                                          ?
207500101103       //--------------------------------------------------------------
207600101103       BEGSR  sr_GesW01;
207700101103
207800101103         // -?Emissione window?
207900101103         exfmt  TB46W01;
208000101103
208100101103         errMessage  = *off;
208200101103         errGenerico = *off;
208300101103         clear  V1Dmsg;
208400101103
208500101103         $Video = 'D2';
208600101103
208700101103         select;
208800101103           // -?F12=Ritorno?
208900101103           when  dsp_aid = c_F12;
209000101103             leavesr;
209100101103           // -?Invio con "S"?
209200101103           when  W1Ccpy = 'S';
209300101103             if  V2Caut = *blank;
209400101103               V2Caut   = p_§LACaut;
209500101103             endif;
209600101103             if  V2Ctpi = *blank;
209700101103               V2Ctpi   = p_§LACtpi;
209800101103             endif;
209900101103             if  V2Cfmi = *blank;
210000101103               V2Cfmi   = p_§LACfmi;
210100101103               clear  V2Dfmi;
210200101103             endif;
210300101103             //if  V2Cdir = *blank;      //? Directory ?
210400101103               V2Cdir   = p_§LACdir;     //? COMUNQUE  ?
210500101103             //endif;                    //? del padre ?
210600101103             if  V2Ctpt = *blank;
210700101103               V2Ctpt   = p_§LACtpt;
210800101103             endif;
210900101103             if  V2Cssr = *blank;
211000101103               V2Cssr   = p_§LACssr;
211100101103             endif;
211200101103             if  V2Clnp = *zero;
211300101103               V2Clnp   = p_§LAClnp;
211400101103             endif;
211500101103             if  V2Cres = *blank;
211600101103               V2Cres   = p_§LACres;
211700101103             endif;
211800101103             if  V2Crec = *blank;
211900101103               V2Crec   = p_§LACrec;
212000101103             endif;
212100101103             if  V2Ccsr = *blank;
212200101103               V2Ccsr   = p_§LACcsr;
212300101103             endif;
212400101103             if  V2tdta = *blank;
212500101103               V2tdta   = p_§LACtdta;
212600101103             endif;
212700101103             if  V2mesi = *zero;
212800101103               V2mesi   = p_§LACmesi;
212900101103             endif;
213000101103             if  V2Cschgg = *zero;
213100101103               V2Cschgg = p_§LACschgg;
213200101103             endif;
213300101103             if  ds_SchMm = *blank;
213400101103               ds_SchMm = p_SchMm;
213500101103             endif;
213600101103             if  V2tadu = *blank;
213700101103               V2tadu   = p_§LACtadu;
213800101103             endif;
213900101103             if  V2Ctad = *blank;
214000101103               V2Ctad   = p_§LACtad;
214100101103             endif;
214200101103             if  V2Cksc = *zero;
214300101103               V2Cksc   = p_§LACksc;
214400101103             endif;
214500101103             if  V2Cctr = *blank;
214600101103               V2Cctr   = p_§LACctr;
214700101103             endif;
214800101103             if  V2imp  = *zero;
214900101103               V2imp    = p_§LACimp;
215000101103             endif;
215100101103             if  V2fimp = *blank;
215200101103               V2fimp   = p_§LACfimp;
215300101103             endif;
215400110713             if  V2pgm  = *blank;
215500110713               V2pgm    = p_§LACpgm;
215600110713             endif;
215700110713             if  V2ele  = *blank;
215800110713               V2ele    = p_§LACele;
215900110713             endif;
216000101103             if  V2note = *blank;
216100101103               V2note   = p_§LACnote;
216200101103             endif;
216300101103         endsl;
216400101103
216500101103       ENDSR;
216600071206
216700071212       //--------------------------------------------------------------
216800090306       //?Aggiornamento record TNTBE00F (tab. LAC)                     ?
216900071212       //--------------------------------------------------------------
217000090306       BEGSR  Upd_TNTBE;
217100071207
217200090306         clear  dLAC;
217300071207         §LACrag   = V2Dccm;
217400071207         §LACtpt   = V2Ctpt;
217500071207         §LACres   = V2Cres;
217600071207         §LACrec   = V2Crec;
217700071207         §LACcsr   = V2Ccsr;
217800071207         §LACssr   = V2Cssr;
217900071207         §LACdir   = V2Cdir;
218000071207         §LACtpi   = V2Ctpi;
218100071207         §LACfmi   = V2Cfmi;
218200071207         §LACaut   = V2Caut;
218300090309         §LACtdta  = V2tdta;
218400071207         §LACtad   = V2Ctad;
218500071207         §LACschgg = V2Cschgg;
218600071207         §LACsch01 = V2Csch01;
218700071207         §LACsch02 = V2Csch02;
218800071207         §LACsch03 = V2Csch03;
218900071207         §LACsch04 = V2Csch04;
219000071207         §LACsch05 = V2Csch05;
219100071207         §LACsch06 = V2Csch06;
219200071207         §LACsch07 = V2Csch07;
219300071207         §LACsch08 = V2Csch08;
219400071207         §LACsch09 = V2Csch09;
219500071207         §LACsch10 = V2Csch10;
219600071207         §LACsch11 = V2Csch11;
219700071207         §LACsch12 = V2Csch12;
219800090226         §LACnote  = V2note;
219900090309         §LAClnp   = V2Clnp;
220000090309         §LACksu   = V2Cksu;
220100090309         §LACmesi  = V2mesi;
220200090313         §LACdatai = (%int( %subst( %editc( V2datai : 'X') : 3 : 4 ) )
220300090313                      * 100)
220400090313                   + %int( %subst( %editc( V2datai : 'X') : 1 : 2 ) );
220500090310         §LACdatela...
220600090313                   = (%int( %subst( %editc( V2datela : 'X') : 3 : 4 ) )
220700090319                      * 100)
220800090313                    + %int( %subst( %editc( V2datela : 'X') : 1 : 2 ) );
220900090309         §LACtadu  = V2tadu;
221000090309         §LACksc   = V2Cksc;
221100090309         §LACctr   = V2Cctr;
221200090309         §LACfimp  = V2fimp;
221300090309         §LACimp   = V2imp;
221400110713         §LACpgm   = V2pgm;
221500110713         §LACele   = V2ele;
221600071207
221700090309         TBEuni    = dLAC;
221800071207
221900090306         clear  TBEftt;
222000090306         clear  TBEftr;
222100071207
222200071207         select;
222300090306           // F5=Ripristino
222400090306           when  dsp_aid = c_F05;
222500090306             clear  TBEatb;
222600090306           // F16=Annullamento
222700090306           when  dsp_aid = c_F16;
222800071207             TBEatb = 'A';
222900071207         endsl;
223000071207
223100071207         IF  NOT  %found(TNTBE01L);
223200090306           clear  TBEatb;
223300090306           clear  TBEflt;
223400090306           clear  TBEftr;
223500090306           //clear  TBEdtr;
223600090306           TBEsif = TBXsif;
223700090306           TBElin = TBXlin;
223800090306           TBEapl = TBXapl;
223900090306           TBEcod = k_TBEcod;
224000090306           TBEke1 = k_TBEke1;
224100090306           TBEke2 = k_TBEke2;
224200090306           //_____________
224300071207           WRITE TNTBE000;
224400090306           //¯¯¯¯¯¯¯¯¯¯¯¯¯
224500071207         ELSE;
224600090306           //______________
224700071207           UPDATE TNTBE000;
224800090306           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
224900071207         ENDIF;
225000071207
225100071207       ENDSR;
225200071207
225300071212       //--------------------------------------------------------------
225400090306       //?Operazioni finali.                                           ?
225500071212       //--------------------------------------------------------------
225600090306       BEGSR  sr_RoutEnd;
225700071207
225800090306         clear  TIBS69ds;
225900090304         tibs69ds.I69tla = 'C';
226000090304         tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
226100071207
226200071217         kpjbu = TNTB46ds;
226300071207         return;
226400071207
226500071207       ENDSR;
226600090306
226700071207      /end-free
226800071207
226900071212       //--------------------------------------------------------------
227000090306       //?Schiere a tempo di compilazione.                             ?
227100071212       //--------------------------------------------------------------
227200071207
227300071207** - $MSG -------------------------------------------------------------------*
227400071207Immettere il codice cliente                                                     1
227500071207Immettere SOLO caratteri numerici                                               2
227600071207Codice errato                                                                   3
227700071207Cliente annullato                                                               4
227800071207Immettere la directory                                                          5
227900071207Carattere "\" non valido; per indicare una sottocartella utilizzare "/"         6
228000071210Caratteri & e %  non ammessi per la directory di clienti con invio automatico   7
228100071207Indicare il giorno del mese se specificato almeno un mese                       8
228200071207Specificare almeno un mese se indicato il giorno del mese                       9
228300071207Per i mesi con n° di giorni < &g verrà assunto il loro ultimo giorno -> Enter  10
228400090225Inserire il codice padre                                                       11
228500090225Il codice padre deve essere = al codice cliente o = a cliente già in tabella   12
