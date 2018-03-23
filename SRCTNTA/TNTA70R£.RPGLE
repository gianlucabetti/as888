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
001600090409     fTABEL00F  if   e           k disk
001700090409
001800090409     fWFTES00F  o    e             disk    usropn
001900090409
002000090409       //--------------------------------------------------------------
002100090409       //?Definizione costanti.                                        ?
002200090409       //--------------------------------------------------------------
002300090409
002400090409
002500090409       //--------------------------------------------------------------
002600090409       //?Definizione schiere.                                         ?
002700090409       //--------------------------------------------------------------
002800090409
002900090409
003000090409       //--------------------------------------------------------------
003100090409       //?Definizione aree dati.                                       ?
003200090409       //--------------------------------------------------------------
003300090409
003400090409       // - Dati utente
003500090409     d §AzUte        e ds                  extname(AZUTE00F)
003600090409     d                                     dtaara
003700090409     d §DatiUte      e ds                  extname(dDatiUte)
003800090409     d                                     dtaara
003900090409
004000090409       //--------------------------------------------------------------
004100090409       //?Definizione strutture dati.                                  ?
004200090409       //--------------------------------------------------------------
004300090409
004400090409       // - Parametri ricevuti
004500090409     d KPJBA         e ds
004600090409
004700090409       // - Reperimento dati utente
004800090409     d TIBS34ds      e ds
004900090409
005000090409       // - Gestione tabelle: controllo e ricerca
005100090409     d TIBS02ds      e ds                  inz
005200090409     d   T02mod      e                     inz('C')
005300090409     d   T02cod      e                     inz('LAC')
005400090409
005500090409       // - Reperimento cliente unificante
005600090409     d TIBS10ds      e ds                  inz
005700090409     d   D10tle      e                     inz('ST')
005800090409     d   D10paf      e                     inz('P')
005900090409     d ParTipExe       s              1a   inz('A')
006000090409
006100090409       // - Controllo/Decodifica cliente
006200090409     d TIBS69ds      e ds                  qualified  inz
006300090409     d ds_CNACO      e ds                  extname(CNACO00F)
006400090409     d                                     qualified  inz
006500090409     d ds_CNIND      e ds                  extname(CNIND00F)
006600090409     d                                     qualified  inz
006700090409     d ds_CNCLP      e ds                  extname(CNCLP00F)
006800090409     d                                     qualified  inz
006900090409     d ds_FNCLS      e ds                  extname(FNCLS00F)
007000090409     d                                     qualified  inz
007100090409
007200090409       // - Flag operativi di TNTAM (TAMFLO)
007300090409     d dsTA01        e ds                  inz
007400090409
007500090409       // - Tab. "01" = Commerciali
007600090409     d ds01          e ds                  inz
007700090409
007800090409       // - Campi di comodo
007900090409     d ds_Time14       ds            14    inz
008000090409     d   Wdate                        8  0 inz
008100090409     d   Wtime                        6  0 inz
008200090409
008300090409       // - Controllo ed inversione data
008400090409     d WLBdat          ds                  inz
008500090409     d   G08dat                       8  0 inz
008600090409     d   G08inv                       8  0 inz
008700090410     d   G08err                       1    inz('3')
008800090409     d   G08tgi                       5  0 inz
008900090409
009000090409       //--------------------------------------------------------------
009100090409       //?Definizione variabili globali.                               ?
009200090409       //--------------------------------------------------------------
009300090409
009400090409       // - Flag booleani
009500090409     d $ElabTP         s               n   inz
009600090409     d $OverCTS        s               n   inz
009700090409
009800090409       // - Campi per controllo "rotture" di livello
009900090409     d SAVksc          s                   like(TAMksc)  inz
010000090409     d SAVctr          s                   like(TAMctr)  inz(*hival)
010100090409     d SAVcts          s                   like(TPDcts)  inz
010200090409
010300090409       // - Campi per conteggio dati per scaglione
010400090409     d Wcount          s              2  0 inz
010500090409     d Wsg1            s                   like(TPDsgl)  inz
010600090409     d Wsg2            s                   like(TPDsgl)  inz
010700090409     d Wsg3            s                   like(TPDsgl)  inz
010800090409     d Wfd1            s                   like(TPDitr)  inz
010900090409     d Wfd2            s                   like(TPDitr)  inz
011000090409     d Wfd3            s                   like(TPDitr)  inz
011100090409     d Wmin1           s                   like(TPDmin)  inz
011200090409     d Wmin2           s                   like(TPDmin)  inz
011300090409     d Wmin3           s                   like(TPDmin)  inz
011400090409     d Wmax1           s                   like(TPDmax)  inz
011500090409     d Wmax2           s                   like(TPDmax)  inz
011600090409     d Wmax3           s                   like(TPDmax)  inz
011700090409
011800090409       // - Campi per richiamo QCMDEXC
011900090409     d Qcmd            s            128    inz
012000090409
012100090409       //--------------------------------------------------------------
012200090409       //?Definizione procedure usate.                                 ?
012300090409       //--------------------------------------------------------------
012400090409
012500090409
012600090409       // - Reperimento dati utente
012700090409      /copy gaitrasrc/srcProtoPR,TIBS34R
012800090409
012900090409       // - Ricerca/Controllo tabelle
013000090409      /copy gaitrasrc/srcProtoPR,TIBS02R
013100090409
013200090409       // - Controllo/Decodifica cliente
013300090409      /copy gaitrasrc/srcProtoPR,TIBS69R
013400090409
013500090409       // - Gestione codice cliente unificante
013600090409     d tibs10r         pr                  extpgm('TIBS10R')
013700090409     d  TIBS10ds                           likeds(tibs10ds)
013800090409     d  ParTipExe                     1a
013900090409
014000090409
014100090409       // - Controllo ed inversione date
014200090409      /copy gaitrasrc/srcProtoPR,XSRDA8
014300090409
014400090409       // - Esecuzione comando di sistema
014500090409      /copy gaitrasrc/srcProtoPR,QCMDEXC
014600090409
014700090409       //--------------------------------------------------------------
014800090409       //?Definizione key-list.                                        ?
014900090409       //--------------------------------------------------------------
015000090409
015100090409     d k04titpt01    e ds                  extname(TITPT01L : *key)
015200090409     d                                     inz
015300090409
015400090409     d k03tabel00    e ds                  extname(TABEL00F : *key)
015500090409     d                                     inz
015600090409
015700090409       //--------------------------------------------------------------
015800090409       //?Riepilogo indicatori.                                        ?
015900090409       //--------------------------------------------------------------
016000090409       //--------------------------------------------------------------
016100090409
016200090409       //--------------------------------------------------------------
016300090409       //?M A I N - L I N E.                                           ?
016400090409       //--------------------------------------------------------------
016500090409
016600090409     c     *Entry        plist
016700090409     c                   parm                    KPJBA
016800090409
016900090409      /free
017000090409
017100090409       //?Operazioni iniziali?
017200090409       exsr  sr_RoutInz;
017300090409
017400090409       //?Ciclo di lettura file tariffe (TNTAM)?
017500090409       setll  *zero  TNTAM000;
017600090409       read   TNTAM000;
017700090409
017800090409       dow  NOT  %eof(TNTAM04L);
017900090409
018000090409         // - Selezione singolo record di TNTAM
018100090409         exsr  sr_Selez;
018200090409
018300090409         // - Ciclo di lettura file tariffe particolari (TITPT/D)
018400090409         if  $ElabTP;
018500090409           exsr  sr_ElabTP;
018600090409           exsr  sr_WrtRec;
018700090409         endif;
018800090409
018900090409         // - Lettura record successivo di TNTAM
019000090409         read  TNTAM000;
019100090409
019200090409       enddo;
019300090409
019400090409       //?Operazioni finali?
019500090409       exsr  sr_RoutEnd;
019600090409
019700090409       //--------------------------------------------------------------
019800090409       //?Operazioni iniziali.                                         ?
019900090409       //--------------------------------------------------------------
020000090409       BEGSR  sr_RoutInz;
020100090409
020200090409         *inLR = *on;
020300090409
020400090409         // - Reperimento dati job
020500090409         exsr  sr_DatiJob;
020600090409
020700090409         // - Impostazione campi fissi
020800090409         TBLkut = 1;
020900090409         TPTftc = 'E ';
021000090409
021100090409         // - Reperimento Data & Ora correnti
021200090409         ds_Time14 = %subst( %char( %dec( %timestamp() ) )
021300090409                             : 1 : 14 );
021400090409
021500090409         // - Pulizia/Apertura del work-file
021600090409         Qcmd = 'CLRPFM FILE(';
021700090409         if  %subst( knsif : 7 : 1 ) = 'P';
021800090409            Qcmd = %trimr(Qcmd) + 'GAITRAAZP';
021900090409         else;
022000090409            Qcmd = %trimr(Qcmd) + 'GAITRAAZM';
022100090409         endif;
022200090409         Qcmd = %trimr(Qcmd) + '/WFTES00F)';
022300090409         ExecuteCommand ( Qcmd : %size(Qcmd));
022400090409
022500090409         open  WFTES00F;
022600090409
022700090409       ENDSR;
022800090409
022900090409       //--------------------------------------------------------------
023000090409       //?Reperimento Dati del job (Utente/Operativi).
023100090409       //--------------------------------------------------------------
023200090409       BEGSR  sr_DatiJob;
023300090409
023400090409         in(E) §AzUte;
023500090409         if NOT %error;
023600090409           in(E) §DatiUte;
023700090409         endif;
023800090409         if %error or RSut = *blanks;
023900090409           clear TIBS34ds;
024000090409           tibs34r(tibs34ds);
024100090409           in §AzUte;
024200090409           in §DatiUte;
024300090409         endif;
024400090409
024500090409       ENDSR;
024600090409
024700090409       //--------------------------------------------------------------
024800090409       //?Selezione record da TNTAM
024900090409       //--------------------------------------------------------------
025000090409       BEGSR  sr_Selez;
025100090409
025200090409         $ElabTP = *off;
025300090409
025400090409         // -?Elaborare solo a rottura di cliente?
025500090409         if  TAMctr = SAVctr   and  TAMksc = SAVksc;
025600090409           leavesr;
025700090409         endif;
025800090409
025900090409         dsTA01 = TAMflo;
026000090409
026100090409         // -?Selezioni?
026200090409         select;
026300090409
026400090409           // - Esclusione rec. annullati
026500090409           when  TAMatb <> *blank;
026600090409             leavesr;
026700090409
026800090409           // - Esclusione tariffe bloccate
026900090409           when  TAMbap = 'B';
027000090409             leavesr;
027100090409
027200090409           // - Esclusione delle tariffe  FED
027300090409           when  §TAfed = 'S';
027400090409             leavesr;
027500090409
027600090409         endsl;
027700090409
027800090409         // -?Reperimento dei dati anagrafici?
027900090409         //  ?per escludere le tariffe dei clienti bloccati?
028000090409         clear  TIBS69ds;
028100090409         tibs69ds.I69kcc = DUTkci;
028200090409         tibs69ds.I69kac = TAMksc;
028300090409         tibs69ds.I69kcp = TAMksc;
028400090409         tibs69r(tibs69ds : ds_cnaco : ds_cnind : ds_cnclp : ds_fncls);
028500130315         if  ds_cnaco.ACOabl <> *blanks;
028600090409           leavesr;
028700090409         endif;
028800090409
028900090409         // -?Se l'elaborazione arriva qui: trattasi di cliente del?
029000090409         //  ?quale vanno elaborate le tariffe?
029100090409         $ElabTP = *on;
029200090409
029300090409       ENDSR;
029400090409
029500090409       //--------------------------------------------------------------
029600090409       //?Elaborazione della sola tariffa "ESPRESSO"
029700090409       //--------------------------------------------------------------
029800090409       BEGSR  sr_ElabTP;
029900090409
030000090409         // -?Pulizia campi di lavoro?
030100090409         reset  $OverCTS;
030200090409         clear  SAVcts;
030300090409         clear  Wcount;
030400090409         clear  Wsg1;
030500090409         clear  Wsg2;
030600090409         clear  Wsg3;
030700090409         clear  Wfd1;
030800090409         clear  Wfd2;
030900090409         clear  Wfd3;
031000090409         clear  Wmin1;
031100090409         clear  Wmin2;
031200090409         clear  Wmin3;
031300090409         clear  Wmax1;
031400090409         clear  Wmax2;
031500090409         clear  Wmax3;
031600090409
031700090409         // -?Reperimento tipo tariffa da testata?
031800090409         TPTksc = TAMksc;
031900090409         TPTctr = TAMctr;
032000090409         TPTprg = TAMprg;
032100090409         //TPTftc = 'E ';         (già così)
032200090409         chain  %kds(k04titpt01)  TITPT000;
032300090409         if  NOT  %found(TITPT01L)   or   TPTatb <> *blank;
032400090409           leavesr;
032500090409         endif;
032600090409
032700090409         // -?Ricerca importo percentuale?
032800090409         setll  %kds(k04titpt01)  TITPD000;
032900090409         reade  %kds(k04titpt01)  TITPD000;
033000090409
033100090409         DOW  NOT  %eof(TITPD01L);
033200090409
033300090409           If  TPDatb = *blank;
033400090409
033500090409             // - Se il codice tassazione è diverso da blank:
033600090409             //     verifica se è lo stesso;
033700090409             //   altrimenti:
033800090409             //     valorizza il flag di più codici tassazione;
033900090409             select;
034000090409               when  SAVcts = *blank;
034100090409                 SAVcts = TPDcts;
034200090409               when  SAVcts <> TPDcts;
034300090409                 $OverCTS = *on;
034400090409                 leave;
034500090409             endsl;
034600090409
034700090409             // - Uniforma allo scaglione 99999 tutti gli scaglioni
034800090409             //   maggiori di 99
034900090409             if  TPDsgl >= 99;
035000090409               TPDsgl = 99999,000;
035100090409             endif;
035200090409
035300090409             // -?Conteggio per scaglione?
035400090409             Wcount += 1;
035500090409
035600090409             select;
035700090409               when  Wcount = 1;
035800090409                 Wsg1  = TPDsgl;
035900090409                 Wfd1  = TPDitr;
036000090409                 Wmin1 = TPDmin;
036100090409                 Wmax1 = TPDmax;
036200090409               when  Wcount = 2;
036300090409                 Wsg2  = TPDsgl;
036400090409                 Wfd2  = TPDitr;
036500090409                 Wmin2 = TPDmin;
036600090409                 Wmax2 = TPDmax;
036700090409               when  Wcount = 3;
036800090409                 Wsg3  = TPDsgl;
036900090409                 Wfd3  = TPDitr;
037000090409                 Wmin3 = TPDmin;
037100090409                 Wmax3 = TPDmax;
037200090409               other;
037300090409                 leave;
037400090409             endsl;
037500090409
037600090409           EndIf;
037700090409
037800090409           reade  %kds(k04titpt01)  TITPD000;
037900090409
038000090409         ENDDO;
038100090409
038200090409       ENDSR;
038300090409
038400090409       //--------------------------------------------------------------
038500090409       //?Scrittura record nel work-file
038600090409       //--------------------------------------------------------------
038700090409       BEGSR  sr_WrtRec;
038800090409
038900090409         exsr  sr_RieRec;
039000090409         //______________
039100090409         WRITE  WFTES000;
039200090409         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
039300090409
039400090409         SAVksc = TAMksc;
039500090409         SAVctr = TAMctr;
039600090409
039700090409         // - Se la tariffa che si sta elaborando deve ancora andare
039800090409         //   in vigore la si elabora, ma si dovrà elaborare anche la
039900090409         //   tariffa precedente (che è in vigore ora)
040000090409         if  TAMddt > *date;
040100090409           SAVctr = *hival;
040200090409         endif;
040300090409
040400090409       ENDSR;
040500090409
040600090409       //--------------------------------------------------------------
040700090409       //?Riempimento campi nel record del work-file
040800090409       //--------------------------------------------------------------
040900090409       BEGSR  sr_RieRec;
041000090409
041100090409         clear  WFTES000;
041200090409
041300090409         // -?utente/data/ora elaborazione?
041400090409         TESpru = knmus;
041500090409         TESdte = Wdate;
041600090409         TESore = Wtime;
041700090409
041800090409         // -?Cliente?
041900090409         TESksc  = TAMksc;
042000090409         TESkscd = ds_cnaco.ACOrag;
042100090409
042200090409         // -?Importanza cliente?
042300090409         TESclv = ds_cnclp.CLPclv;
042400090409
042500090409         // -?Data ultima spedizione fattura?
042600090409         if  ds_cnclp.CLPdus > *zero;
042700090409           reset  WLBdat;
042800090409           G08inv = ds_cnclp.CLPdus;
042900090409           //G02err = '3'         (già così)
043000090409           xsrda8(WLBdat);
043100090409           TESdus = G08inv;
043200090409         endif;
043300090409
043400090409         // -?Tariffa?
043500090409         TESctr = TAMctr;
043600090409         TESprg = TAMprg;
043700090409         TESddt = TAMddt;
043800090409         TESdst = TAMdst;
043900090409         // - Tipo tariffa
044000090409         select;
044100090409           when  §TAdpd = 'S';
044200090409             TEStpt = 'D';
044300090409           when  §TAfed = 'S';
044400090409             TEStpt = 'F';
044500090409           other;
044600090409             TEStpt = TAMfie;
044700090409           endsl;
044800090409
044900090409         // -?Flag presenza tar. ESPRESSO in TITPT?
045000090409         //  ?e tipo tariffa (in TITPT)?
045100090409         if  %found(TITPT01L)   and   TPTatb = *blank;
045200090409           TESexp = 'S';
045300090409           TEStpg = TPTtpg;
045400090409         else;
045500090409           TESexp = 'N';
045600090409         endif;
045700090409
045800090409         // -?Fattore demoltiplicatore per ogni scaglione?
045900090409         TESsg1 = Wsg1;
046000090409         TESfd1 = Wfd1;
046100090409         TESmi1 = Wmin1;
046200090409         TESma1 = Wmax1;
046300090409         TESsg2 = Wsg2;
046400090409         TESfd2 = Wfd2;
046500090409         TESmi2 = Wmin2;
046600090409         TESma2 = Wmax2;
046700090409         TESsg3 = Wsg3;
046800090409         TESfd3 = Wfd3;
046900090409         TESmi3 = Wmin3;
047000090409         TESma3 = Wmax3;
047100090409
047200090409         // -?Flag indicante se ci sono più scaglioni per lo stesso?
047300090409         //  ?codice tassazione?
047400090409         if  Wcount > 3;
047500090409           TESfsg = 'S';
047600090409         endif;
047700090409
047800090409         // -?Flag indicante se ci sono più codici tassazione?
047900090409         if  $OverCTS;
048000090409           TESfct = 'S';
048100090409         endif;
048200090409
048300090409
048400090409         // -?Ricerca del codice cliente unificante?
048500090409         reset  TIBS10ds;
048600090409         //D10tle = 'ST'          (già così)
048700090409         //D10paf = 'P'           (già così)
048800090409         D10cod = TESksc;
048900090409         D10drf = Wdate;
049000090409         //ParTipExe = 'A';       (già così)
049100090409         tibs10r ( tibs10ds : ParTipExe );
049200090409         // - - Se ci sono errori: valorizzo come unificante il cliente
049300090409         if  D10err <> *blank;
049400090409           TESkun  = TESksc;
049500090409           TESkund = TESkscd;
049600090409         else;
049700090409           TESkun = D10cop;
049800090409           // - Reperimento dati anagrafici
049900090409           clear  TIBS69ds;
050000090409           tibs69ds.I69kcc = DUTkci;
050100090410           tibs69ds.I69kac = TESkun;
050200090410           tibs69ds.I69kcp = TESkun;
050300090409           tibs69r(tibs69ds : ds_cnaco : ds_cnind
050400090409                            : ds_cnclp : ds_fncls);
050500090409           TESkund = ds_cnaco.ACOrag;
050600090409         endif;
050700090409
050800090409         // -?Verifica del codice commerciale unificante?
050900090409         TBLcod = '01';
051000090409         TBLkey = %editc(ds_cnclp.CLPage : 'X');
051100090409         chain  %kds(k03tabel00)  TABEL;
051200090409         if  not %found(TABEL00F)   or   TBLflg <> *blank;
051300090409           TEScmu  = ds_cnclp.CLPage;
051400090409           TEScmud = *all'*';
051500090409         else;
051600090409           ds01    = TBLuni;
051700090409           TEScmu  = §01rgf;
051800090409           TBLkey = %editc(TEScmu : 'X');
051900090409           chain  %kds(k03tabel00)  TABEL;
052000090409           if  not %found(TABEL00F)   or   TBLflg <> *blank;
052100090409             TEScmud = *all'*';
052200090409           else;
052300090409             ds01    = TBLuni;
052400090409             TEScmud = §01age;
052500090409           endif;
052600090409         endif;
052700090409
052800090409         // -Albero commerciale unificante?
052900090409         ORGfil = %div ( TEScmu : 10000 );
053000090409         chain  (ORGfil)  AZORG;
053100090409         if  not %found(AZORG01L);
053200090409           clear  ORGfl3;
053300090409           clear  ORGcar;
053400090409         endif;
053500090409
053600090409         // -?Distretto?
053700090409         TESdis = ORGfl3;
053800090409         TBLcod = '17';
053900090409         TBLkey = TESdis;
054000090409         chain  %kds(k03tabel00)  TABEL;
054100090409         if  not %found(TABEL00F)   or   TBLflg <> *blank;
054200090409           TESdisd = *all'*';
054300090409         else;
054400090409           TESdisd = TBLuni;
054500090409         endif;
054600090409
054700090409         // -?Area?
054800090409         TEScar = ORGcar;
054900090409         TBLcod = '05';
055000090409         TBLkey = %editc(TEScar : 'X');
055100090409         chain  %kds(k03tabel00)  TABEL;
055200090409         if  not %found(TABEL00F)   or   TBLflg <> *blank;
055300090409           TEScard = *all'*';
055400090409         else;
055500090409           TEScard = TBLuni;
055600090409         endif;
055700090409
055800090409         // -?Filiale?
055900090409         TESfil = ORGfil;
056000090409         if  NOT  %found(AZORG01L)   or   ORGfva <> *blank;
056100090409           TESfild = *all'*';
056200090409         else;
056300090409           TESfild = ORGdes;
056400090409         endif;
056500090409
056600090409       ENDSR;
056700090409
056800090409       //--------------------------------------------------------------
056900090409       //?Operazioni finali.                                         ?
057000090409       //--------------------------------------------------------------
057100090409       BEGSR  sr_RoutEnd;
057200090409
057300090409         // - Stampa  la fine elaborazione
057400090409         //write  TA70D1;
057500090409
057600090409         return;
057700090409
057800090409       ENDSR;
057900090409
058000090409      /end-free
