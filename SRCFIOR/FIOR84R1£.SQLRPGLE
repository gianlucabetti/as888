000100140514      //--------------------------------------------------------------
000200140514      //?FIOR84R1 - Elenco Clienti con ORM commissionati - Crea WF
000300140514      //--------------------------------------------------------------
000400140514
000500140514     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600140514     h dftactgrp(*no) actgrp(*caller) bnddir('TRUL')
000700140514
000800140514      //---------------------------------------------------------------
000900140514      //?Dichiarazione file.
001000140514      //---------------------------------------------------------------
001100140514     fAZORG01L  if   e           k disk
001200140514     fAZCMM01L  if   e           k disk
001300140514     fTABEL00F  if   e           k disk
001400140514     fWFORC00F  o    e             disk
001500140514
001600140514      //---------------------------------------------------------------
001700140514      //?Definizione costanti.
001800140514      //---------------------------------------------------------------
001900140514
002000140514      //---------------------------------------------------------------
002100140514      //?Definizione schiere.
002200140514      //---------------------------------------------------------------
002300140514
002400140514      //---------------------------------------------------------------
002500140514      //?Definizione aree dati.
002600140514      //---------------------------------------------------------------
002700140514      // - Dati utente
002800140514     d §AzUte        e ds                  extname(AZUTE00F)
002900140514     d                                     dtaara
003000140514     d §DatiUte      e ds                  extname(dDatiUte)
003100140514     d                                     dtaara
003200140514
003300140514      //---------------------------------------------------------------
003400140514      //?Definizione strutture dati.
003500140514      //---------------------------------------------------------------
003600140514      // - Parametri ricevuti
003700140514     d KPJBA         e ds
003800140514     d FIOR84DS      e ds                  inz qualified
003900140514
004000140514      // - Driver per reperire mail/sms
004100140514     d FIOR900DS     e ds                  qualified inz
004200140514
004300140514      // - Ricerca/Controllo tabelle
004400140514     d TIBS02ds      e ds                  qualified inz
004500140514
004600140514      // - Reperimento dati utente
004700140514     d TIBS34DS      e ds                  qualified inz
004800140514
004900140514      // - Carico Filiali abilitate all'utente
005000140514     d TRUL31DS      e ds                  qualified inz
005100140514     d  POG                   10    759    dim(250)
005200140514
005300140514      // - File ORM
005400140514     d FNORMDS       e ds                  extname(FNORM00F)
005500140514
005600140526      // - Tabella DFT ORM
005700140526     d dDFT          e ds                  inz
005800140526
005900140514      // - Tabella Abilitazioni
006000140514     d dLAT          e ds                  inz qualified
006100140514
006200140514      // - Campo OREdati rcd G
006300140514     d dOREgen       e ds                  inz qualified
006400140514
006500140514      // - Tabella 05 = Aree
006600140514     d ds05          e ds                  qualified inz
006700140514
006800140514      // - Tabella 17 = Distretti commerciali
006900140514     d ds17          e ds                  qualified inz
007000140526
007100140526      // - Campo Abilitazioni Utente
007200140526     d dUTE01        e ds                  inz qualified
007300140526
007400140526      // - Campo ORGDE3 di AZORG00F
007500140526     d OG143         e ds                  inz qualified
007600140514
007700140514      //---------------------------------------------------------------
007800140514      //?Definizione variabili globali.
007900140514      //---------------------------------------------------------------
008000140514      // - Flags booleani
008100140514     d wEnd            s               n   inz(*off)
008200140514
008300140514       // - Stringa SQL da eseguire
008400140514     d wSQL            s           4096    varying        inz
008500140514
008600140514      // - Indici di schiera
008700140514     d xx              s              4  0 inz
008800140514     d yy              s              4  0 inz
008900140515
009000140515      // - Campi di comodo
009100140515     d sav_ORMcor      s                   like(ORMcor) inz
009200140515     d sav_ORMrso      s                   like(ORMrso) inz
009300140520     d sav_ORMloo      s                   like(ORMloo) inz
009400140520     d sav_ORMpro      s                   like(ORMpro) inz
009500140515     d wabi            s                   inz like(UTEaut)
009600140526     d wOKalert        s               n   inz(*off)
009700140515     d w0030           s                   like(ORGfil) inz
009800140514
009900140514      //---------------------------------------------------------------
010000140514      //?Definizione procedure esterne.
010100140514      //---------------------------------------------------------------
010200140514      // - Driver per reperire mail/sms
010300140514     d FIOR900R        pr                  extpgm('FIOR900R')
010400140514     d  fior900ds                          likeds(fior900ds)
010500140514
010600140514      // - Filiali e aree abilitate
010700140514     d trul31r         pr                  extpgm('TRUL31R')
010800140514     d  kpjba                              likeds(KPJBA)
010900140514     d  trul31ds                           likeds(trul31ds)
011000140514
011100140514      //---------------------------------------------------------------
011200140514      //?Definizione Prototipi.
011300140514      //---------------------------------------------------------------
011400140514      /copy gaitrasrc/srcprotopr,TIBS02R
011500140514      /copy gaitrasrc/srcprotopr,TIBS34R
011600140515      /copy gaitrasrc/srcprotopi,TIBS69R
011700140515      /copy gaitrasrc/srcprotopr,TIBS69R
011800140514
011900140514      //---------------------------------------------------------------
012000140514      //?Definizione key-list.
012100140514      //---------------------------------------------------------------
012200140514       // - File TABEL00F
012300140514     d k03tabel      e ds                  extname(TABEL00F:*key)
012400140514     d                                     prefix(k_)
012500140514     d                                     inz
012600140514
012700140514      //---------------------------------------------------------------
012800140514      //?M A I N - L I N E
012900140514      //---------------------------------------------------------------
013000140514
013100140514     c     *entry        plist
013200140514     c                   parm                    KPJBA
013300140514
013400140514      /free
013500140514
013600140514       //?Operazioni iniziali
013700140514       exsr RoutInz;
013800140514
013900140515       //?Elaboro ORM
014000140515       exsr Elabora;
014100140514
014200140514       //?Operazioni finali
014300140514       exsr RoutEnd;
014400140514
014500140514       //--------------------------------------------------------------
014600140514       //?Operazioni iniziali.
014700140514       //--------------------------------------------------------------
014800140514       BEGSR RoutInz;
014900140514
015000140514       //?Imposto la ds con i dati della KPJBU
015100140514         FIOR84ds = kpjbu;
015200140514
015300140514       //?Reperimento dati job
015400140514         exsr DatiJob;
015500140516
015600140516       //?Dati fissi
015700140516         k_TBLkut = 1;
015800140514
015900140515       //?Se filiale 999
016000140514       //?vuol dire che devo elaborare tutte le filiali abilitate
016100140515         IF  fior84ds.OR84poe = 999;
016200140514         //?Reperimento abilitazioni utente
016300140514           exsr AbiUte;
016400140514         //?Reperimento filiali abilitate
016500140514           clear TRUL31DS;
016600140514           trul31ds.I31abi = wabi;
016700140514           trul31ds.I31cdi = DUTdis;
016800140514           trul31ds.I31car = DUTare;
016900140514           trul31ds.I31cpo = DUTpou;
017000140514           trul31r (kpjba:trul31ds);
017100140514         ELSE;
017200140515           trul31ds.POG(1) = %editc(fior84ds.OR84poe:'X');
017300140514         ENDIF;
017400140514
017500140514         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
017600140526
017700140526       //?Carico i DFT ORM da filiale Utente
017800140526         clear TIBS02DS;
017900140526         clear dDFT;
018000140526         tibs02ds.T02mod = 'C';
018100140526         tibs02ds.T02cod = 'DFT';
018200140526         tibs02ds.T02ke1 = %editc(DUTpou:'X');
018300140526         tibs02ds.T02sif = knsif;
018400140526         TNTBE_RicercaControllo (kpjba:tibs02ds);
018500140526         IF  tibs02ds.T02err = *blanks;
018600140526           dDFT = tibs02ds.T02uni;
018700140526         ELSE;
018800140526       //?Carico i DFT ORM da filiale 999
018900140526           clear TIBS02DS;
019000140526           clear dDFT;
019100140526           tibs02ds.T02mod = 'C';
019200140526           tibs02ds.T02cod = 'DFT';
019300140526           tibs02ds.T02ke1 = '999';
019400140526           tibs02ds.T02sif = knsif;
019500140526           TNTBE_RicercaControllo (kpjba:tibs02ds);
019600140526           IF  tibs02ds.T02err = *blanks;
019700140526             dDFT = tibs02ds.T02uni;
019800140526           ENDIF;
019900140526         ENDIF;
020000140514
020100140514       ENDSR;
020200140514
020300140514       //--------------------------------------------------------------
020400140514       //?Reperimento Dati del job.
020500140514       //--------------------------------------------------------------
020600140514       BEGSR DatiJob;
020700140514
020800140514         in(E) §AzUte;
020900140514         IF  not %error;
021000140514           in(E) §DatiUte;
021100140514         ENDIF;
021200140514         IF  %error or rsut = *blanks;
021300140514           clear TIBS34ds;
021400140514           tibs34r(tibs34ds);
021500140514           in §AzUte;
021600140514           in §DatiUte;
021700140514         ENDIF;
021800140514
021900140514       ENDSR;
022000140514
022100140514       //--------------------------------------------------------------
022200140514       //?Reperimento Abilitazioni utente.
022300140514       //--------------------------------------------------------------
022400140514       BEGSR AbiUte;
022500140514
022600140514         clear wabi;
022700140514
022800140514         dUTE01 = UTEfaf;
022900140514
023000140514       //?Imposto l'abilitazione dell'utente
023100140514         SELECT;
023200140514         WHEN  DUTerr = 'E';
023300140514           leavesr;
023400140514         WHEN  dute01.§UTEorm <> *blanks;
023500140514           wabi = dute01.§UTEorm;
023600140514         WHEN  UTEaut <> *blanks;
023700140514           wabi = UTEaut;
023800140514         OTHER;
023900140514           IF  DUTlpo = '1';
024000140514             wabi = 'TP';
024100140514           ENDIF;
024200140514           IF  DUTlpo = '2';
024300140514             wabi = 'PO';
024400140514           ENDIF;
024500140514         ENDSL;
024600140514
024700140514       //?Controllo se abilitazione OK
024800140514         clear TIBS02DS;
024900140514         clear dLAT;
025000140514         tibs02ds.T02mod = 'C';
025100140514         tibs02ds.T02cod = 'LAT';
025200140514         tibs02ds.T02ke1 = wabi;
025300140514         tibs02ds.T02sif = knsif;
025400140514         TNTBE_RicercaControllo (kpjba:tibs02ds);
025500140514         dLAT = tibs02ds.T02uni;
025600140514
025700140514       ENDSR;
025800140514
025900140514       //--------------------------------------------------------------
026000140514       //?Elaboro ORM.
026100140514       //--------------------------------------------------------------
026200140514       BEGSR Elabora;
026300140514
026400140514         wEnd = *off;
026500140514
026600140514       //?Imposto la stringa per SQL
026700140515         exsr StringaSQL;
026800140514
026900140514       //?Dichiarazione cursore
027000140514         exec sql
027100140514           prepare S1  from :wSQL;
027200140514         exec sql
027300140514           declare ORM cursor for S1;
027400140514
027500140514       //?Apertura del cursore
027600140514         exec sql
027700140514           open ORM;
027800140514
027900140514         IF sqlcode < 0;
028000140514           wEnd = *on;
028100140514         ENDIF;
028200140514
028300140514         DOW not wEnd;
028400140514           exec sql
028500140514             fetch next from ORM into :FNORMDS;
028600140514           IF sqlcod = 100 or sqlcod < 0;
028700140514             wEnd = *on;
028800140514             leave;
028900140514           ENDIF;
029000140514
029100140514       //?Carico i dati nel file di work
029200140527           exsr CarDati;
029300140514
029400140514         ENDDO;
029500140527
029600140527       //?A fine lettura devo caricare anche l'ultimo nel file di work
029700140527         IF  ORCcom > 0;
029800140527           exsr Scrivi;
029900140527         ENDIF;
030000140514
030100140514         exec sql
030200140514           close ORM;
030300140514
030400140514       ENDSR;
030500140514
030600140514       //--------------------------------------------------------------
030700140514       //?Preparazione stringa SQL.
030800140514       //--------------------------------------------------------------
030900140514       BEGSR StringaSQL;
031000140514
031100140514       //?Seleziono gli ORM commissionati Manuali e da VAS
031200140514       //?nel periodo richiesto e per la filiale richiesta
031300140514         wSQL = 'select * from FNORM00F +
031400140514                 where ORMtco in(''M'', ''I'', ''F'') and +
031500140514                 substr(ORMflo, 6, 1) = ''S'' and +
031600140516                 ORMcor > 0 and ORMnsr = 0 and +
031700140514                 ORMdao between ' + %editc(fior84ds.OR84dtad:'X') +
031800140514                 ' and ' + %editc(fior84ds.OR84dtaa:'X') +
031900140514                 ' and digits(ORMpoe) in(';
032000140514         yy = 0;
032100140514         xx = 1;
032200140514         FOR xx by 1 to %elem(trul31ds.POG);
032300140514           IF  trul31ds.POG(xx) <> *zeros and trul31ds.POG(xx) <> *blanks;
032400140514             IF  yy > 0;
032500140514               wSQL += ', ';
032600140514             ELSE;
032700140514               yy = 1;
032800140514             ENDIF;
032900140514             wSQL += trul31ds.POG(xx);
033000140514           ENDIF;
033100140514         ENDFOR;
033200140514
033300140514         wSQL += ')';
033400140514
033500140515         wSQL += ' order by ORMcor';
033600140514
033700140514       ENDSR;
033800140514
033900140514       //--------------------------------------------------------------
034000140514       //?Carico i dati nel file di work.
034100140514       //--------------------------------------------------------------
034200140514       BEGSR CarDati;
034300140514
034400140514       //?A rottura di ordinante scrivo il file
034500140516         IF  ORMcor <> sav_ORMcor;
034600140526           IF  sav_ORMcor > 0 and ORCcom > 0;
034700140516             exsr Scrivi;
034800140516           ENDIF;
034900140514           sav_ORMcor = ORMcor;
035000140515           sav_ORMrso = ORMrso;
035100140520           sav_ORMloo = ORMloo;
035200140520           sav_ORMpro = ORMpro;
035300140514           clear WFORC000;
035400140514         ENDIF;
035500140526
035600140526       //?Verifico se ci sono i presupposti per Alert
035700140526         exsr VerificaAlert;
035800140526         IF  not wOKalert;
035900140526           leavesr;
036000140526         ENDIF;
036100140515
036200140515       //?Cerco i dati degli Alert
036300140515         fior900ds.IOR900trc = 'E';
036400140515         fior900ds.IOR900rcdm = 'MA';
036500140515         fior900ds.IOR900rcds = 'S ';
036600140515         fior900ds.IOR900poe = ORMpoe;
036700140515         fior900ds.IOR900nsr = ORMnsr;
036800140515         fior900ds.IOR900nor = ORMnor;
036900140515         fior900ds.IOR900nrv = ORMnrv;
037000140515         fior900r (fior900ds);
037100140515
037200140515       //?Tot. ORM commissionati
037300140515         ORCcom += 1;
037400140515
037500140515       //?Commissionato manuale
037600140515         IF  ORMtco = 'M';
037700140515           ORCman += 1;
037800140515         ELSE;
037900140515       //?Commissionato da VAS
038000140515           ORCvas += 1;
038100140515         ENDIF;
038200140515
038300140515       //?Se NON ho trovato i dati mi fermo qua
038400140515         IF  fior900ds.OOR900err = 'N';
038500140515           leavesr;
038600140515         ENDIF;
038700140515
038800140515       //?Sommo i dati degli Alert manuali
038900140515         IF  ORMtco = 'M';
039000140515           ORCmdati += 1;
039100140515           IF  fior900ds.OOR900mail <> *blanks;
039200140515             ORCmmail += 1;
039300140515           ENDIF;
039400140515           IF  fior900ds.OOR900sms <> *blanks;
039500140515             ORCmsms += 1;
039600140515           ENDIF;
039700140515         ENDIF;
039800140515
039900140515       //?Sommo i dati degli Alert da VAS
040000140515         IF  ORMtco <> 'M';
040100140515           ORCvdati += 1;
040200140515           IF  fior900ds.OOR900mail <> *blanks;
040300140515             ORCvmail += 1;
040400140515           ENDIF;
040500140515           IF  fior900ds.OOR900sms <> *blanks;
040600140515             ORCvsms += 1;
040700140515           ENDIF;
040800140515         ENDIF;
040900140514
041000140514       ENDSR;
041100140526
041200140526       //--------------------------------------------------------------------
041300140526       //?Verifico se OK alert.
041400140526       //--------------------------------------------------------------------
041500140526       BEGSR VerificaAlert;
041600140526
041700140526         wOKalert = *off;
041800140526
041900140526       //?Cerco il NTW del mittente codificato
042000140526         clear OG143;
042100140526         IF  ORMcra > 0;
042200140526           w0030 = %dec(%subst(%editc(ORMcra:'X'):1:3):3:0);
042300140526           chain w0030 AZORG01L;
042400140526           IF  %found(AZORG01L) and ORGfva = *blanks;
042500140526             OG143 = ORGde3;
042600140526           ENDIF;
042700140526         ENDIF;
042800140526
042900140526         SELECT;
043000140526
043100140526       //?Se mittente codificato e filiale mittente ha ntw estero
043200140526       //?o mittente NON codificato con nazione estera
043300140526       //?NO alert e NON memorizzo i dati su FNORE
043400140526         WHEN  (ORMcra > 0 and
043500140526               (og143.§OGntw = 'DPD' or og143.§OGntw = 'EEX' or
043600140526                og143.§OGntw = 'FED')) or
043700140526               (ORMcra = 0 and ORMnar <> *blanks);
043800140526
043900140526         //?Se ORM prepagato NO alert
044000140526         WHEN  ORMtor = 'P';
044100140526
044200140526         //?Se Ordinante non codificato NO alert
044300140526         WHEN  ORMcor > *zeros and
044400140526              (%subst(%editc(ORMcor:'X'):4:4) = '9999' or
044500140526               %subst(%editc(ORMcor:'X'):4:4) = '8888');
044600140526
044700140526         //?Controllo se peso/volume/bancali superano i limit massimi da DFT
044800140526         //?NO alert
044900140526         WHEN  (d§DFTpkga > 0 and ORMpkg > d§DFTpkga) or
045000140526               (d§DFTvlma > 0 and ORMvlm > d§DFTvlma) or
045100140526               (d§DFTbnca > 0 and ORMbnc > d§DFTbnca);
045200140526         OTHER;
045300140526           wOKalert = *on;
045400140526         ENDSL;
045500140526
045600140526       ENDSR;
045700140514
045800140514       //--------------------------------------------------------------
045900140514       //?Scrivo il file di work.
046000140514       //--------------------------------------------------------------
046100140514       BEGSR Scrivi;
046200140514
046300140515       //?Dati di lancio
046400140515         ORCute = knmus;
046500140515         ORCdta = %dec(%date());
046600140515         ORCpoe = fior84ds.OR84poe;
046700140515         IF  fior84ds.OR84poe = 999;
046800140515           ORCdpoe = 'TUTTE';
046900140515         ELSE;
047000140515           chain (ORCpoe) AZORG01L;
047100140515           IF  %found(AZORG01L);
047200140515             ORCdpoe = ORGdes;
047300140515           ENDIF;
047400140515         ENDIF;
047500140515         ORCdtad = fior84ds.OR84dtad;
047600140515         ORCdtaa = fior84ds.OR84dtaa;
047700140515
047800140515       //?Dati del commerciale unificante
047900140515         clear TIBS69DS;
048000140515         I69kcp = %dec(%subst(%editc(sav_ORMcor:'X'):1:7):7:0);
048100140515         tibs69r (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
048200140515         IF  O69err <> *blanks;
048300140515           clear CLPage;
048400140515         ENDIF;
048500140515         chain (CLPage) AZCMM01L;
048600140515         IF  %found(AZCMM01L);
048700140515           ORCcmm = CMMuni;
048800140515           ORCdcmm = CMMdes;
048900140515         ELSE;
049000140515           clear ORCcmm;
049100140515           clear ORCdcmm;
049200140515         ENDIF;
049300140515
049400140515       //?Ramo aziendale unificante
049500140515         w0030 = %dec(%subst(%editc(ORCcmm:'X'):1:3):3:0);
049600140515         chain (w0030) AZORG01L;
049700140515         IF  %found(AZORG01L);
049800140515           ORCdit = ORGdit;
049900140515           ORCcar = ORGcar;
050000140515         ELSE;
050100140515           clear ORCdit;
050200140515           clear ORCddit;
050300140515           clear ORCcar;
050400140515           clear ORCdcar;
050500140515         ENDIF;
050600140515
050700140515         IF  ORCdit <> *blanks;
050800140515           clear ds17;
050900140515           k_tblcod = '17';
051000140515           k_tblkey = ORCdit;
051100140515           chain %kds(K03tabel) TABEL00F;
051200140515           IF  %found( TABEL00F );
051300140515             ds17 = TBLuni;
051400140515           ENDIF;
051500140515           ORCddit = ds17.§17des;
051600140515         ENDIF;
051700140515
051800140515         IF  ORCcar > 0;
051900140515           clear ds05;
052000140515           k_tblcod = '05';
052100140515           k_tblkey = %editc(ORCcar:'X');
052200140515           chain %kds(K03tabel) TABEL00F;
052300140515           IF  %found( TABEL00F );
052400140515             ds05 = TBLuni;
052500140515           ENDIF;
052600140515           ORCdcar = ds05.§05des;
052700140515         ENDIF;
052800140515
052900140515       //?Ordinante
053000140515         ORCcor = sav_ORMcor;
053100140515         ORCrso = sav_ORMrso;
053200140520         ORCloo = sav_ORMloo;
053300140520         ORCpro = sav_ORMpro;
053400140515
053500140515       //?I totali sono stati calcolati prima
053600140514
053700140514         write WFORC000;
053800140514
053900140514       ENDSR;
054000140514
054100140514       //--------------------------------------------------------------
054200140514       //?Operazioni finali.
054300140514       //--------------------------------------------------------------
054400140514       BEGSR RoutEnd;
054500140514
054600140514       //?Reimposto la kpjbu con la ds dei dati passati
054700140514         kpjbu = FIOR84DS;
054800140514
054900140514         *inLR = *on;
055000140514         return;
055100140514
055200140514       ENDSR;
055300140514
055400140514      /end-free
