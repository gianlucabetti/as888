000100090409       //--------------------------------------------------------------
000200090410       //?TNTA70R - Controllo tariffe "ESPRESSO"                       ?
000300090409       //--------------------------------------------------------------
000400090409
000500090409     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000600090409
000700090409       //--------------------------------------------------------------
000800090409       //?Dichiarazione file.                                          ?
000900090409       //--------------------------------------------------------------
001000090409
001100090409     fTNTAM04L  if   e           k disk
001200090409     fTITPT01L  if   e           k disk
001300090409     fTITPD01L  if   e           k disk
001400090409
001500090409     fAZORG01L  if   e           k disk
001600130905     fAZCMM01L  if   e           k disk
001700090409     fTABEL00F  if   e           k disk
001800090409
001900090409     fWFTES00F  o    e             disk    usropn
002000090409
002100090409       //--------------------------------------------------------------
002200090409       //?Definizione costanti.                                        ?
002300090409       //--------------------------------------------------------------
002400090409
002500090409
002600090409       //--------------------------------------------------------------
002700090409       //?Definizione schiere.                                         ?
002800090409       //--------------------------------------------------------------
002900090409
003000090409
003100090409       //--------------------------------------------------------------
003200090409       //?Definizione aree dati.                                       ?
003300090409       //--------------------------------------------------------------
003400090409
003500090409       // - Dati utente
003600090409     d §AzUte        e ds                  extname(AZUTE00F)
003700090409     d                                     dtaara
003800090409     d §DatiUte      e ds                  extname(dDatiUte)
003900090409     d                                     dtaara
004000090409
004100090409       //--------------------------------------------------------------
004200090409       //?Definizione strutture dati.                                  ?
004300090409       //--------------------------------------------------------------
004400090409
004500090409       // - Parametri ricevuti
004600090409     d KPJBA         e ds
004700090409
004800090409       // - Reperimento dati utente
004900090409     d TIBS34ds      e ds
005000090409
005100090409       // - Reperimento cliente unificante
005200090409     d TIBS10ds      e ds                  inz
005300090409     d   D10tle      e                     inz('ST')
005400090409     d   D10paf      e                     inz('P')
005500090409     d ParTipExe       s              1a   inz('A')
005600090409
005700090409       // - Controllo/Decodifica cliente
005800090409     d TIBS69ds      e ds                  qualified  inz
005900090409     d ds_CNACO      e ds                  extname(CNACO00F)
006000090409     d                                     qualified  inz
006100090409     d ds_CNIND      e ds                  extname(CNIND00F)
006200090409     d                                     qualified  inz
006300090409     d ds_CNCLP      e ds                  extname(CNCLP00F)
006400090409     d                                     qualified  inz
006500090409     d ds_FNCLS      e ds                  extname(FNCLS00F)
006600090409     d                                     qualified  inz
006700090409
006800090409       // - Flag operativi di TNTAM (TAMFLO)
006900090409     d dsTA01        e ds                  inz
007000090409
007100090409       // - Campi di comodo
007200090409     d ds_Time14       ds            14    inz
007300090409     d   Wdate                        8  0 inz
007400090409     d   Wtime                        6  0 inz
007500090409
007600090409       // - Controllo ed inversione data
007700090409     d WLBdat          ds                  inz
007800090409     d   G08dat                       8  0 inz
007900090409     d   G08inv                       8  0 inz
008000090410     d   G08err                       1    inz('3')
008100090409     d   G08tgi                       5  0 inz
008200090409
008300090409       //--------------------------------------------------------------
008400090409       //?Definizione variabili globali.                               ?
008500090409       //--------------------------------------------------------------
008600090409
008700090409       // - Flag booleani
008800090409     d $ElabTP         s               n   inz
008900090409     d $OverCTS        s               n   inz
009000090409
009100090409       // - Campi per controllo "rotture" di livello
009200090409     d SAVksc          s                   like(TAMksc)  inz
009300090409     d SAVctr          s                   like(TAMctr)  inz(*hival)
009400090409     d SAVcts          s                   like(TPDcts)  inz
009500090409
009600090409       // - Campi per conteggio dati per scaglione
009700090409     d Wcount          s              2  0 inz
009800090409     d Wsg1            s                   like(TPDsgl)  inz
009900090409     d Wsg2            s                   like(TPDsgl)  inz
010000090409     d Wsg3            s                   like(TPDsgl)  inz
010100090409     d Wfd1            s                   like(TPDitr)  inz
010200090409     d Wfd2            s                   like(TPDitr)  inz
010300090409     d Wfd3            s                   like(TPDitr)  inz
010400090409     d Wmin1           s                   like(TPDmin)  inz
010500090409     d Wmin2           s                   like(TPDmin)  inz
010600090409     d Wmin3           s                   like(TPDmin)  inz
010700090409     d Wmax1           s                   like(TPDmax)  inz
010800090409     d Wmax2           s                   like(TPDmax)  inz
010900090409     d Wmax3           s                   like(TPDmax)  inz
011000090409
011100090409       // - Campi per richiamo QCMDEXC
011200090409     d Qcmd            s            128    inz
011300090409
011400090409       //--------------------------------------------------------------
011500090409       //?Definizione procedure usate.                                 ?
011600090409       //--------------------------------------------------------------
011700090409
011800090409
011900090409       // - Reperimento dati utente
012000090409      /copy gaitrasrc/srcProtoPR,TIBS34R
012100090409
012200090409       // - Controllo/Decodifica cliente
012300090409      /copy gaitrasrc/srcProtoPR,TIBS69R
012400090409
012500090409       // - Gestione codice cliente unificante
012600090409     d tibs10r         pr                  extpgm('TIBS10R')
012700090409     d  TIBS10ds                           likeds(tibs10ds)
012800090409     d  ParTipExe                     1a
012900090409
013000090409
013100090409       // - Controllo ed inversione date
013200090409      /copy gaitrasrc/srcProtoPR,XSRDA8
013300090409
013400090409       // - Esecuzione comando di sistema
013500090409      /copy gaitrasrc/srcProtoPR,QCMDEXC
013600090409
013700090409       //--------------------------------------------------------------
013800090409       //?Definizione key-list.                                        ?
013900090409       //--------------------------------------------------------------
014000090409
014100090409     d k04titpt01    e ds                  extname(TITPT01L : *key)
014200090409     d                                     inz
014300090409
014400090409     d k03tabel00    e ds                  extname(TABEL00F : *key)
014500090409     d                                     inz
014600090409
014700090409       //--------------------------------------------------------------
014800090409       //?Riepilogo indicatori.                                        ?
014900090409       //--------------------------------------------------------------
015000090409       //--------------------------------------------------------------
015100090409
015200090409       //--------------------------------------------------------------
015300090409       //?M A I N - L I N E.                                           ?
015400090409       //--------------------------------------------------------------
015500090409
015600090409     c     *Entry        plist
015700090409     c                   parm                    KPJBA
015800090409
015900090409      /free
016000090409
016100090409       //?Operazioni iniziali?
016200090409       exsr  sr_RoutInz;
016300090409
016400090409       //?Ciclo di lettura file tariffe (TNTAM)?
016500090409       setll  *zero  TNTAM000;
016600090409       read   TNTAM000;
016700090409
016800090409       dow  NOT  %eof(TNTAM04L);
016900090409
017000090409         // - Selezione singolo record di TNTAM
017100090409         exsr  sr_Selez;
017200090409
017300090409         // - Ciclo di lettura file tariffe particolari (TITPT/D)
017400090409         if  $ElabTP;
017500090409           exsr  sr_ElabTP;
017600090409           exsr  sr_WrtRec;
017700090409         endif;
017800090409
017900090409         // - Lettura record successivo di TNTAM
018000090409         read  TNTAM000;
018100090409
018200090409       enddo;
018300090409
018400090409       //?Operazioni finali?
018500090409       exsr  sr_RoutEnd;
018600090409
018700090409       //--------------------------------------------------------------
018800090409       //?Operazioni iniziali.                                         ?
018900090409       //--------------------------------------------------------------
019000090409       BEGSR  sr_RoutInz;
019100090409
019200090409         *inLR = *on;
019300090409
019400090409         // - Reperimento dati job
019500090409         exsr  sr_DatiJob;
019600090409
019700090409         // - Impostazione campi fissi
019800090409         TBLkut = 1;
019900090409         TPTftc = 'E ';
020000090409
020100090409         // - Reperimento Data & Ora correnti
020200090409         ds_Time14 = %subst( %char( %dec( %timestamp() ) )
020300090409                             : 1 : 14 );
020400090409
020500090409         // - Pulizia/Apertura del work-file
020600090409         Qcmd = 'CLRPFM FILE(';
020700090409         if  %subst( knsif : 7 : 1 ) = 'P';
020800090409            Qcmd = %trimr(Qcmd) + 'GAITRAAZP';
020900090409         else;
021000090409            Qcmd = %trimr(Qcmd) + 'GAITRAAZM';
021100090409         endif;
021200090409         Qcmd = %trimr(Qcmd) + '/WFTES00F)';
021300090409         ExecuteCommand ( Qcmd : %size(Qcmd));
021400090409
021500090409         open  WFTES00F;
021600090409
021700090409       ENDSR;
021800090409
021900090409       //--------------------------------------------------------------
022000090409       //?Reperimento Dati del job (Utente/Operativi).
022100090409       //--------------------------------------------------------------
022200090409       BEGSR  sr_DatiJob;
022300090409
022400090409         in(E) §AzUte;
022500090409         if NOT %error;
022600090409           in(E) §DatiUte;
022700090409         endif;
022800090409         if %error or RSut = *blanks;
022900090409           clear TIBS34ds;
023000090409           tibs34r(tibs34ds);
023100090409           in §AzUte;
023200090409           in §DatiUte;
023300090409         endif;
023400090409
023500090409       ENDSR;
023600090409
023700090409       //--------------------------------------------------------------
023800090409       //?Selezione record da TNTAM
023900090409       //--------------------------------------------------------------
024000090409       BEGSR  sr_Selez;
024100090409
024200090409         $ElabTP = *off;
024300090409
024400090409         // -?Elaborare solo a rottura di cliente?
024500090409         if  TAMctr = SAVctr   and  TAMksc = SAVksc;
024600090409           leavesr;
024700090409         endif;
024800090409
024900090409         dsTA01 = TAMflo;
025000090409
025100090409         // -?Selezioni?
025200090409         select;
025300090409
025400090409           // - Esclusione rec. annullati
025500090409           when  TAMatb <> *blank;
025600090409             leavesr;
025700090409
025800090409           // - Esclusione tariffe bloccate
025900090409           when  TAMbap = 'B';
026000090409             leavesr;
026100090409
026200090409           // - Esclusione delle tariffe  FED
026300090409           when  §TAfed = 'S';
026400090409             leavesr;
026500090409
026600090409         endsl;
026700090409
026800090409         // -?Reperimento dei dati anagrafici?
026900090409         //  ?per escludere le tariffe dei clienti bloccati?
027000090409         clear  TIBS69ds;
027100090409         tibs69ds.I69kcc = DUTkci;
027200090409         tibs69ds.I69kac = TAMksc;
027300090409         tibs69ds.I69kcp = TAMksc;
027400090409         tibs69r(tibs69ds : ds_cnaco : ds_cnind : ds_cnclp : ds_fncls);
027500130315         if  ds_cnaco.ACOabl <> *blanks;
027600090409           leavesr;
027700090409         endif;
027800090409
027900090409         // -?Se l'elaborazione arriva qui: trattasi di cliente del?
028000090409         //  ?quale vanno elaborate le tariffe?
028100090409         $ElabTP = *on;
028200090409
028300090409       ENDSR;
028400090409
028500090409       //--------------------------------------------------------------
028600090409       //?Elaborazione della sola tariffa "ESPRESSO"
028700090409       //--------------------------------------------------------------
028800090409       BEGSR  sr_ElabTP;
028900090409
029000090409         // -?Pulizia campi di lavoro?
029100090409         reset  $OverCTS;
029200090409         clear  SAVcts;
029300090409         clear  Wcount;
029400090409         clear  Wsg1;
029500090409         clear  Wsg2;
029600090409         clear  Wsg3;
029700090409         clear  Wfd1;
029800090409         clear  Wfd2;
029900090409         clear  Wfd3;
030000090409         clear  Wmin1;
030100090409         clear  Wmin2;
030200090409         clear  Wmin3;
030300090409         clear  Wmax1;
030400090409         clear  Wmax2;
030500090409         clear  Wmax3;
030600090409
030700090409         // -?Reperimento tipo tariffa da testata?
030800090409         TPTksc = TAMksc;
030900090409         TPTctr = TAMctr;
031000090409         TPTprg = TAMprg;
031100090409         //TPTftc = 'E ';         (già così)
031200090409         chain  %kds(k04titpt01)  TITPT000;
031300090409         if  NOT  %found(TITPT01L)   or   TPTatb <> *blank;
031400090409           leavesr;
031500090409         endif;
031600090409
031700090409         // -?Ricerca importo percentuale?
031800090409         setll  %kds(k04titpt01)  TITPD000;
031900090409         reade  %kds(k04titpt01)  TITPD000;
032000090409
032100090409         DOW  NOT  %eof(TITPD01L);
032200090409
032300090409           If  TPDatb = *blank;
032400090409
032500090409             // - Se il codice tassazione è diverso da blank:
032600090409             //     verifica se è lo stesso;
032700090409             //   altrimenti:
032800090409             //     valorizza il flag di più codici tassazione;
032900090409             select;
033000090409               when  SAVcts = *blank;
033100090409                 SAVcts = TPDcts;
033200090409               when  SAVcts <> TPDcts;
033300090409                 $OverCTS = *on;
033400090409                 leave;
033500090409             endsl;
033600090409
033700090409             // - Uniforma allo scaglione 99999 tutti gli scaglioni
033800090409             //   maggiori di 99
033900090409             if  TPDsgl >= 99;
034000090409               TPDsgl = 99999,000;
034100090409             endif;
034200090409
034300090409             // -?Conteggio per scaglione?
034400090409             Wcount += 1;
034500090409
034600090409             select;
034700090409               when  Wcount = 1;
034800090409                 Wsg1  = TPDsgl;
034900090409                 Wfd1  = TPDitr;
035000090409                 Wmin1 = TPDmin;
035100090409                 Wmax1 = TPDmax;
035200090409               when  Wcount = 2;
035300090409                 Wsg2  = TPDsgl;
035400090409                 Wfd2  = TPDitr;
035500090409                 Wmin2 = TPDmin;
035600090409                 Wmax2 = TPDmax;
035700090409               when  Wcount = 3;
035800090409                 Wsg3  = TPDsgl;
035900090409                 Wfd3  = TPDitr;
036000090409                 Wmin3 = TPDmin;
036100090409                 Wmax3 = TPDmax;
036200090409               other;
036300090409                 leave;
036400090409             endsl;
036500090409
036600090409           EndIf;
036700090409
036800090409           reade  %kds(k04titpt01)  TITPD000;
036900090409
037000090409         ENDDO;
037100090409
037200090409       ENDSR;
037300090409
037400090409       //--------------------------------------------------------------
037500090409       //?Scrittura record nel work-file
037600090409       //--------------------------------------------------------------
037700090409       BEGSR  sr_WrtRec;
037800090409
037900090409         exsr  sr_RieRec;
038000090409         //______________
038100090409         WRITE  WFTES000;
038200090409         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
038300090409
038400090409         SAVksc = TAMksc;
038500090409         SAVctr = TAMctr;
038600090409
038700090409         // - Se la tariffa che si sta elaborando deve ancora andare
038800090409         //   in vigore la si elabora, ma si dovrà elaborare anche la
038900090409         //   tariffa precedente (che è in vigore ora)
039000090409         if  TAMddt > *date;
039100090409           SAVctr = *hival;
039200090409         endif;
039300090409
039400090409       ENDSR;
039500090409
039600090409       //--------------------------------------------------------------
039700090409       //?Riempimento campi nel record del work-file
039800090409       //--------------------------------------------------------------
039900090409       BEGSR  sr_RieRec;
040000090409
040100090409         clear  WFTES000;
040200090409
040300090409         // -?utente/data/ora elaborazione?
040400090409         TESpru = knmus;
040500090409         TESdte = Wdate;
040600090409         TESore = Wtime;
040700090409
040800090409         // -?Cliente?
040900090409         TESksc  = TAMksc;
041000090409         TESkscd = ds_cnaco.ACOrag;
041100090409
041200090409         // -?Importanza cliente?
041300090409         TESclv = ds_cnclp.CLPclv;
041400090409
041500090409         // -?Data ultima spedizione fattura?
041600090409         if  ds_cnclp.CLPdus > *zero;
041700090409           reset  WLBdat;
041800090409           G08inv = ds_cnclp.CLPdus;
041900090409           //G02err = '3'         (già così)
042000090409           xsrda8(WLBdat);
042100090409           TESdus = G08inv;
042200090409         endif;
042300090409
042400090409         // -?Tariffa?
042500090409         TESctr = TAMctr;
042600090409         TESprg = TAMprg;
042700090409         TESddt = TAMddt;
042800090409         TESdst = TAMdst;
042900090409         // - Tipo tariffa
043000090409         select;
043100090409           when  §TAdpd = 'S';
043200090409             TEStpt = 'D';
043300090409           when  §TAfed = 'S';
043400090409             TEStpt = 'F';
043500090409           other;
043600090409             TEStpt = TAMfie;
043700090409           endsl;
043800090409
043900090409         // -?Flag presenza tar. ESPRESSO in TITPT?
044000090409         //  ?e tipo tariffa (in TITPT)?
044100090409         if  %found(TITPT01L)   and   TPTatb = *blank;
044200090409           TESexp = 'S';
044300090409           TEStpg = TPTtpg;
044400090409         else;
044500090409           TESexp = 'N';
044600090409         endif;
044700090409
044800090409         // -?Fattore demoltiplicatore per ogni scaglione?
044900090409         TESsg1 = Wsg1;
045000090409         TESfd1 = Wfd1;
045100090409         TESmi1 = Wmin1;
045200090409         TESma1 = Wmax1;
045300090409         TESsg2 = Wsg2;
045400090409         TESfd2 = Wfd2;
045500090409         TESmi2 = Wmin2;
045600090409         TESma2 = Wmax2;
045700090409         TESsg3 = Wsg3;
045800090409         TESfd3 = Wfd3;
045900090409         TESmi3 = Wmin3;
046000090409         TESma3 = Wmax3;
046100090409
046200090409         // -?Flag indicante se ci sono più scaglioni per lo stesso?
046300090409         //  ?codice tassazione?
046400090409         if  Wcount > 3;
046500090409           TESfsg = 'S';
046600090409         endif;
046700090409
046800090409         // -?Flag indicante se ci sono più codici tassazione?
046900090409         if  $OverCTS;
047000090409           TESfct = 'S';
047100090409         endif;
047200090409
047300090409
047400090409         // -?Ricerca del codice cliente unificante?
047500090409         reset  TIBS10ds;
047600090409         //D10tle = 'ST'          (già così)
047700090409         //D10paf = 'P'           (già così)
047800090409         D10cod = TESksc;
047900090409         D10drf = Wdate;
048000090409         //ParTipExe = 'A';       (già così)
048100090409         tibs10r ( tibs10ds : ParTipExe );
048200090409         // - - Se ci sono errori: valorizzo come unificante il cliente
048300090409         if  D10err <> *blank;
048400090409           TESkun  = TESksc;
048500090409           TESkund = TESkscd;
048600090409         else;
048700090409           TESkun = D10cop;
048800090409           // - Reperimento dati anagrafici
048900090409           clear  TIBS69ds;
049000090409           tibs69ds.I69kcc = DUTkci;
049100090410           tibs69ds.I69kac = TESkun;
049200090410           tibs69ds.I69kcp = TESkun;
049300090409           tibs69r(tibs69ds : ds_cnaco : ds_cnind
049400090409                            : ds_cnclp : ds_fncls);
049500090409           TESkund = ds_cnaco.ACOrag;
049600090409         endif;
049700090409
049800090409         // -?Verifica del codice commerciale unificante?
049900130905         chain  (ds_cnclp.CLPage)  AZCMM000;
050000130905         if  not %found(AZCMM01L);
050100090409           TEScmu  = ds_cnclp.CLPage;
050200090409           TEScmud = *all'*';
050300090409         else;
050400130905           TEScmu  = CMMuni;
050500130905           chain  (CMMuni)  AZCMM000;
050600130905           if  not %found(AZCMM01L);
050700090409             TEScmud = *all'*';
050800090409           else;
050900130905             TEScmud = CMMdes;
051000090409           endif;
051100090409         endif;
051200090409
051300090409         // -Albero commerciale unificante?
051400090409         ORGfil = %div ( TEScmu : 10000 );
051500090409         chain  (ORGfil)  AZORG;
051600090409         if  not %found(AZORG01L);
051700090409           clear  ORGfl3;
051800090409           clear  ORGcar;
051900090409         endif;
052000090409
052100090409         // -?Distretto?
052200090409         TESdis = ORGfl3;
052300090409         TBLcod = '17';
052400090409         TBLkey = TESdis;
052500090409         chain  %kds(k03tabel00)  TABEL;
052600090409         if  not %found(TABEL00F)   or   TBLflg <> *blank;
052700090409           TESdisd = *all'*';
052800090409         else;
052900090409           TESdisd = TBLuni;
053000090409         endif;
053100090409
053200090409         // -?Area?
053300090409         TEScar = ORGcar;
053400090409         TBLcod = '05';
053500090409         TBLkey = %editc(TEScar : 'X');
053600090409         chain  %kds(k03tabel00)  TABEL;
053700090409         if  not %found(TABEL00F)   or   TBLflg <> *blank;
053800090409           TEScard = *all'*';
053900090409         else;
054000090409           TEScard = TBLuni;
054100090409         endif;
054200090409
054300090409         // -?Filiale?
054400090409         TESfil = ORGfil;
054500090409         if  NOT  %found(AZORG01L)   or   ORGfva <> *blank;
054600090409           TESfild = *all'*';
054700090409         else;
054800090409           TESfild = ORGdes;
054900090409         endif;
055000090409
055100090409       ENDSR;
055200090409
055300090409       //--------------------------------------------------------------
055400090409       //?Operazioni finali.                                         ?
055500090409       //--------------------------------------------------------------
055600090409       BEGSR  sr_RoutEnd;
055700090409
055800090409         // - Stampa  la fine elaborazione
055900090409         //write  TA70D1;
056000090409
056100090409         return;
056200090409
056300090409       ENDSR;
056400090409
056500090409      /end-free
