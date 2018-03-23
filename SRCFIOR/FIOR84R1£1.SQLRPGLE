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
005600140514      // - Tabella Abilitazioni
005700140514     d dLAT          e ds                  inz qualified
005800140514
005900140514      // - Campo OREdati rcd G
006000140514     d dOREgen       e ds                  inz qualified
006100140514
006200140514      // - Campo Abilitazioni Utente
006300140514     d dUTE01        e ds                  inz qualified
006400140514
006500140514      // - Tabella 05 = Aree
006600140514     d ds05          e ds                  qualified inz
006700140514
006800140514      // - Tabella 17 = Distretti commerciali
006900140514     d ds17          e ds                  qualified inz
007000140514
007100140514      //---------------------------------------------------------------
007200140514      //?Definizione variabili globali.
007300140514      //---------------------------------------------------------------
007400140514      // - Flags booleani
007500140514     d wEnd            s               n   inz(*off)
007600140514
007700140514       // - Stringa SQL da eseguire
007800140514     d wSQL            s           4096    varying        inz
007900140514
008000140514      // - Indici di schiera
008100140514     d xx              s              4  0 inz
008200140514     d yy              s              4  0 inz
008300140515
008400140515      // - Campi di comodo
008500140515     d sav_ORMcor      s                   like(ORMcor) inz
008600140515     d sav_ORMrso      s                   like(ORMrso) inz
008700140520     d sav_ORMloo      s                   like(ORMloo) inz
008800140520     d sav_ORMpro      s                   like(ORMpro) inz
008900140515     d wabi            s                   inz like(UTEaut)
009000140515     d w0030           s                   like(ORGfil) inz
009100140514
009200140514      //---------------------------------------------------------------
009300140514      //?Definizione procedure esterne.
009400140514      //---------------------------------------------------------------
009500140514      // - Driver per reperire mail/sms
009600140514     d FIOR900R        pr                  extpgm('FIOR900R')
009700140514     d  fior900ds                          likeds(fior900ds)
009800140514
009900140514      // - Filiali e aree abilitate
010000140514     d trul31r         pr                  extpgm('TRUL31R')
010100140514     d  kpjba                              likeds(KPJBA)
010200140514     d  trul31ds                           likeds(trul31ds)
010300140514
010400140514      //---------------------------------------------------------------
010500140514      //?Definizione Prototipi.
010600140514      //---------------------------------------------------------------
010700140514      /copy gaitrasrc/srcprotopr,TIBS02R
010800140514      /copy gaitrasrc/srcprotopr,TIBS34R
010900140515      /copy gaitrasrc/srcprotopi,TIBS69R
011000140515      /copy gaitrasrc/srcprotopr,TIBS69R
011100140514
011200140514      //---------------------------------------------------------------
011300140514      //?Definizione key-list.
011400140514      //---------------------------------------------------------------
011500140514       // - File TABEL00F
011600140514     d k03tabel      e ds                  extname(TABEL00F:*key)
011700140514     d                                     prefix(k_)
011800140514     d                                     inz
011900140514
012000140514      //---------------------------------------------------------------
012100140514      //?M A I N - L I N E
012200140514      //---------------------------------------------------------------
012300140514
012400140514     c     *entry        plist
012500140514     c                   parm                    KPJBA
012600140514
012700140514      /free
012800140514
012900140514       //?Operazioni iniziali
013000140514       exsr RoutInz;
013100140514
013200140515       //?Elaboro ORM
013300140515       exsr Elabora;
013400140514
013500140514       //?Operazioni finali
013600140514       exsr RoutEnd;
013700140514
013800140514       //--------------------------------------------------------------
013900140514       //?Operazioni iniziali.
014000140514       //--------------------------------------------------------------
014100140514       BEGSR RoutInz;
014200140514
014300140514       //?Imposto la ds con i dati della KPJBU
014400140514         FIOR84ds = kpjbu;
014500140514
014600140514       //?Reperimento dati job
014700140514         exsr DatiJob;
014800140516
014900140516       //?Dati fissi
015000140516         k_TBLkut = 1;
015100140514
015200140515       //?Se filiale 999
015300140514       //?vuol dire che devo elaborare tutte le filiali abilitate
015400140515         IF  fior84ds.OR84poe = 999;
015500140514         //?Reperimento abilitazioni utente
015600140514           exsr AbiUte;
015700140514         //?Reperimento filiali abilitate
015800140514           clear TRUL31DS;
015900140514           trul31ds.I31abi = wabi;
016000140514           trul31ds.I31cdi = DUTdis;
016100140514           trul31ds.I31car = DUTare;
016200140514           trul31ds.I31cpo = DUTpou;
016300140514           trul31r (kpjba:trul31ds);
016400140514         ELSE;
016500140515           trul31ds.POG(1) = %editc(fior84ds.OR84poe:'X');
016600140514         ENDIF;
016700140514
016800140514         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
016900140514
017000140514       ENDSR;
017100140514
017200140514       //--------------------------------------------------------------
017300140514       //?Reperimento Dati del job.
017400140514       //--------------------------------------------------------------
017500140514       BEGSR DatiJob;
017600140514
017700140514         in(E) §AzUte;
017800140514         IF  not %error;
017900140514           in(E) §DatiUte;
018000140514         ENDIF;
018100140514         IF  %error or rsut = *blanks;
018200140514           clear TIBS34ds;
018300140514           tibs34r(tibs34ds);
018400140514           in §AzUte;
018500140514           in §DatiUte;
018600140514         ENDIF;
018700140514
018800140514       ENDSR;
018900140514
019000140514       //--------------------------------------------------------------
019100140514       //?Reperimento Abilitazioni utente.
019200140514       //--------------------------------------------------------------
019300140514       BEGSR AbiUte;
019400140514
019500140514         clear wabi;
019600140514
019700140514         dUTE01 = UTEfaf;
019800140514
019900140514       //?Imposto l'abilitazione dell'utente
020000140514         SELECT;
020100140514         WHEN  DUTerr = 'E';
020200140514           leavesr;
020300140514         WHEN  dute01.§UTEorm <> *blanks;
020400140514           wabi = dute01.§UTEorm;
020500140514         WHEN  UTEaut <> *blanks;
020600140514           wabi = UTEaut;
020700140514         OTHER;
020800140514           IF  DUTlpo = '1';
020900140514             wabi = 'TP';
021000140514           ENDIF;
021100140514           IF  DUTlpo = '2';
021200140514             wabi = 'PO';
021300140514           ENDIF;
021400140514         ENDSL;
021500140514
021600140514       //?Controllo se abilitazione OK
021700140514         clear TIBS02DS;
021800140514         clear dLAT;
021900140514         tibs02ds.T02mod = 'C';
022000140514         tibs02ds.T02cod = 'LAT';
022100140514         tibs02ds.T02ke1 = wabi;
022200140514         tibs02ds.T02sif = knsif;
022300140514         TNTBE_RicercaControllo (kpjba:tibs02ds);
022400140514         dLAT = tibs02ds.T02uni;
022500140514
022600140514       ENDSR;
022700140514
022800140514       //--------------------------------------------------------------
022900140514       //?Elaboro ORM.
023000140514       //--------------------------------------------------------------
023100140514       BEGSR Elabora;
023200140514
023300140514         wEnd = *off;
023400140514
023500140514       //?Imposto la stringa per SQL
023600140515         exsr StringaSQL;
023700140514
023800140514       //?Dichiarazione cursore
023900140514         exec sql
024000140514           prepare S1  from :wSQL;
024100140514         exec sql
024200140514           declare ORM cursor for S1;
024300140514
024400140514       //?Apertura del cursore
024500140514         exec sql
024600140514           open ORM;
024700140514
024800140514         IF sqlcode < 0;
024900140514           wEnd = *on;
025000140514         ENDIF;
025100140514
025200140514         DOW not wEnd;
025300140514           exec sql
025400140514             fetch next from ORM into :FNORMDS;
025500140514           IF sqlcod = 100 or sqlcod < 0;
025600140514             wEnd = *on;
025700140514             leave;
025800140514           ENDIF;
025900140514
026000140514       //?Carico i dati nel file di work
026100140514           exsr CarDati;
026200140514
026300140514         ENDDO;
026400140514
026500140514         exec sql
026600140514           close ORM;
026700140514
026800140514       ENDSR;
026900140514
027000140514       //--------------------------------------------------------------
027100140514       //?Preparazione stringa SQL.
027200140514       //--------------------------------------------------------------
027300140514       BEGSR StringaSQL;
027400140514
027500140514       //?Seleziono gli ORM commissionati Manuali e da VAS
027600140514       //?nel periodo richiesto e per la filiale richiesta
027700140514         wSQL = 'select * from FNORM00F +
027800140514                 where ORMtco in(''M'', ''I'', ''F'') and +
027900140514                 substr(ORMflo, 6, 1) = ''S'' and +
028000140516                 ORMcor > 0 and ORMnsr = 0 and +
028100140514                 ORMdao between ' + %editc(fior84ds.OR84dtad:'X') +
028200140514                 ' and ' + %editc(fior84ds.OR84dtaa:'X') +
028300140514                 ' and digits(ORMpoe) in(';
028400140514         yy = 0;
028500140514         xx = 1;
028600140514         FOR xx by 1 to %elem(trul31ds.POG);
028700140514           IF  trul31ds.POG(xx) <> *zeros and trul31ds.POG(xx) <> *blanks;
028800140514             IF  yy > 0;
028900140514               wSQL += ', ';
029000140514             ELSE;
029100140514               yy = 1;
029200140514             ENDIF;
029300140514             wSQL += trul31ds.POG(xx);
029400140514           ENDIF;
029500140514         ENDFOR;
029600140514
029700140514         wSQL += ')';
029800140514
029900140515         wSQL += ' order by ORMcor';
030000140514
030100140514       ENDSR;
030200140514
030300140514       //--------------------------------------------------------------
030400140514       //?Carico i dati nel file di work.
030500140514       //--------------------------------------------------------------
030600140514       BEGSR CarDati;
030700140514
030800140514       //?A rottura di ordinante scrivo il file
030900140516         IF  ORMcor <> sav_ORMcor;
031000140516           IF  sav_ORMcor > 0;
031100140516             exsr Scrivi;
031200140516           ENDIF;
031300140514           sav_ORMcor = ORMcor;
031400140515           sav_ORMrso = ORMrso;
031500140520           sav_ORMloo = ORMloo;
031600140520           sav_ORMpro = ORMpro;
031700140514           clear WFORC000;
031800140514         ENDIF;
031900140515
032000140515       //?Cerco i dati degli Alert
032100140515         fior900ds.IOR900trc = 'E';
032200140515         fior900ds.IOR900rcdm = 'MA';
032300140515         fior900ds.IOR900rcds = 'S ';
032400140515         fior900ds.IOR900poe = ORMpoe;
032500140515         fior900ds.IOR900nsr = ORMnsr;
032600140515         fior900ds.IOR900nor = ORMnor;
032700140515         fior900ds.IOR900nrv = ORMnrv;
032800140515         fior900r (fior900ds);
032900140515
033000140515       //?Tot. ORM commissionati
033100140515         ORCcom += 1;
033200140515
033300140515       //?Commissionato manuale
033400140515         IF  ORMtco = 'M';
033500140515           ORCman += 1;
033600140515         ELSE;
033700140515       //?Commissionato da VAS
033800140515           ORCvas += 1;
033900140515         ENDIF;
034000140515
034100140515       //?Se NON ho trovato i dati mi fermo qua
034200140515         IF  fior900ds.OOR900err = 'N';
034300140515           leavesr;
034400140515         ENDIF;
034500140515
034600140515       //?Sommo i dati degli Alert manuali
034700140515         IF  ORMtco = 'M';
034800140515           ORCmdati += 1;
034900140515           IF  fior900ds.OOR900mail <> *blanks;
035000140515             ORCmmail += 1;
035100140515           ENDIF;
035200140515           IF  fior900ds.OOR900sms <> *blanks;
035300140515             ORCmsms += 1;
035400140515           ENDIF;
035500140515         ENDIF;
035600140515
035700140515       //?Sommo i dati degli Alert da VAS
035800140515         IF  ORMtco <> 'M';
035900140515           ORCvdati += 1;
036000140515           IF  fior900ds.OOR900mail <> *blanks;
036100140515             ORCvmail += 1;
036200140515           ENDIF;
036300140515           IF  fior900ds.OOR900sms <> *blanks;
036400140515             ORCvsms += 1;
036500140515           ENDIF;
036600140515         ENDIF;
036700140514
036800140514       ENDSR;
036900140514
037000140514       //--------------------------------------------------------------
037100140514       //?Scrivo il file di work.
037200140514       //--------------------------------------------------------------
037300140514       BEGSR Scrivi;
037400140514
037500140515       //?Dati di lancio
037600140515         ORCute = knmus;
037700140515         ORCdta = %dec(%date());
037800140515         ORCpoe = fior84ds.OR84poe;
037900140515         IF  fior84ds.OR84poe = 999;
038000140515           ORCdpoe = 'TUTTE';
038100140515         ELSE;
038200140515           chain (ORCpoe) AZORG01L;
038300140515           IF  %found(AZORG01L);
038400140515             ORCdpoe = ORGdes;
038500140515           ENDIF;
038600140515         ENDIF;
038700140515         ORCdtad = fior84ds.OR84dtad;
038800140515         ORCdtaa = fior84ds.OR84dtaa;
038900140515
039000140515       //?Dati del commerciale unificante
039100140515         clear TIBS69DS;
039200140515         I69kcp = %dec(%subst(%editc(sav_ORMcor:'X'):1:7):7:0);
039300140515         tibs69r (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
039400140515         IF  O69err <> *blanks;
039500140515           clear CLPage;
039600140515         ENDIF;
039700140515         chain (CLPage) AZCMM01L;
039800140515         IF  %found(AZCMM01L);
039900140515           ORCcmm = CMMuni;
040000140515           ORCdcmm = CMMdes;
040100140515         ELSE;
040200140515           clear ORCcmm;
040300140515           clear ORCdcmm;
040400140515         ENDIF;
040500140515
040600140515       //?Ramo aziendale unificante
040700140515         w0030 = %dec(%subst(%editc(ORCcmm:'X'):1:3):3:0);
040800140515         chain (w0030) AZORG01L;
040900140515         IF  %found(AZORG01L);
041000140515           ORCdit = ORGdit;
041100140515           ORCcar = ORGcar;
041200140515         ELSE;
041300140515           clear ORCdit;
041400140515           clear ORCddit;
041500140515           clear ORCcar;
041600140515           clear ORCdcar;
041700140515         ENDIF;
041800140515
041900140515         IF  ORCdit <> *blanks;
042000140515           clear ds17;
042100140515           k_tblcod = '17';
042200140515           k_tblkey = ORCdit;
042300140515           chain %kds(K03tabel) TABEL00F;
042400140515           IF  %found( TABEL00F );
042500140515             ds17 = TBLuni;
042600140515           ENDIF;
042700140515           ORCddit = ds17.§17des;
042800140515         ENDIF;
042900140515
043000140515         IF  ORCcar > 0;
043100140515           clear ds05;
043200140515           k_tblcod = '05';
043300140515           k_tblkey = %editc(ORCcar:'X');
043400140515           chain %kds(K03tabel) TABEL00F;
043500140515           IF  %found( TABEL00F );
043600140515             ds05 = TBLuni;
043700140515           ENDIF;
043800140515           ORCdcar = ds05.§05des;
043900140515         ENDIF;
044000140515
044100140515       //?Ordinante
044200140515         ORCcor = sav_ORMcor;
044300140515         ORCrso = sav_ORMrso;
044400140520         ORCloo = sav_ORMloo;
044500140520         ORCpro = sav_ORMpro;
044600140515
044700140515       //?I totali sono stati calcolati prima
044800140514
044900140514         write WFORC000;
045000140514
045100140514       ENDSR;
045200140514
045300140514       //--------------------------------------------------------------
045400140514       //?Operazioni finali.
045500140514       //--------------------------------------------------------------
045600140514       BEGSR RoutEnd;
045700140514
045800140514       //?Reimposto la kpjbu con la ds dei dati passati
045900140514         kpjbu = FIOR84DS;
046000140514
046100140514         *inLR = *on;
046200140514         return;
046300140514
046400140514       ENDSR;
046500140514
046600140514      /end-free
