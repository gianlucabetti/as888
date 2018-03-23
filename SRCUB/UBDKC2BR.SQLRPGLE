000100170727     H DECEDIT('0,') DATEDIT(*DMY.)
000200170727     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR':'TIO7') ACTGRP('BARTVAS')
000300091223
000400091223       //--------------------------------------------------------------
000500121106       // Dichiarazione file.
000600091223       //--------------------------------------------------------------
000700170803     Fedivab6i  if   e           k disk    extfile(wLibFile1) usropn
000800170508
000900170508
001000170508       //--------------------------------------------------------------
001100170508       // Definizione prototipi
001200170508       //--------------------------------------------------------------
001300170508      /COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
001400170509      /COPY GAITRASRC/SRCPROTOPR,UBISNUM
001500170731      /COPY GAITRASRC/SRCPROTOPR,UBBS10R
001600170803
001700160802
001800091223       //--------------------------------------------------------------
001900121106       // Definizione variabili.
002000091223       //--------------------------------------------------------------
002100170508      /COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
002200170509      /COPY GAITRASRC/SRCPROTOPI,UBISNUM
002300170731      /COPY GAITRASRC/SRCPROTOPI,UBBS10R
002400170801      /COPY GAITRASRC/SRCPROTOPI,UBDKC2BR
002500121106
002600170803
002700170803     D EDIVABDS      E DS                  extname(edivab0f)
002800170803     D EDIVATDS      E DS                  extname(edivat0f)
002900170727
003000170803     D wLibFile1       s             21a   inz
003100170801     D wLIB§           s             11a   inz
003200170803     D wFIL§           s             10a   inz
003300170803     D wSQL            s           1024a   inz varying
003400170803     D wTent           s              1s 0 inz
003500170803     D wCount          s              1s 0 inz
003600170803     D wKSU            s              7s 0 inz
003700170803     D wUnifKSU        s              7s 0 inz
003800170803     D wUnifBolla      s              7s 0 inz
003900170803     D wFiller1A       s              1a   inz
004000170727
004100170801     D wApi            c                   const('''')
004200170801
004300170801
004400091223
004500170803     c     *Entry        plist
004600170803     c                   parm                    iDKC2BKSU
004700170803     c                   parm                    iDKC2BTLE
004800170803     c                   parm                    iDKC2BWHO
004900170803     c                   parm                    oDKC2BEsito
005000170803     c                   parm                    oDKC2BVAB
005100091223
005200130307      /free
005300091223
005400091223       //--------------------------------------------------------------
005500121106       // M A I N - L I N E
005600091223       //--------------------------------------------------------------
005700170803
005800170803       // Inizializzazioni parametri esecuzione comandi sql
005900170803       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
006000170803
006100140806
006200160805       // Monitorizzo tutto (per necessità massima solidità stack chiamate)
006300160805       Monitor;
006400160805
006500130307       // Operazioni iniziali?
006600170508       exsr RoutInz;
006700130307
006800170801       // Eseguo reperimento spedizione da "chi sono"
006900170508       exsr RtvSped;
007000160804
007100160805       // Fine monitoring
007200160805       On-Error;
007300170801          oDKC2BEsito = -1;
007400160921          exsr exeUscita;
007500160805       Endmon;
007600170727
007700160920       // Uscita
007800160921       exsr exeUscita;
007900170803
008000091223
008100160802
008200091223       //--------------------------------------------------------------
008300121107       // Operazioni iniziali.
008400091223       //--------------------------------------------------------------
008500170726       BEGSR  RoutInz;
008600160601
008700160920         // Inizializzo l'esito a errore generico
008800170801         oDKC2BEsito = -1;
008900170511
009000170515         // Inizializzazione campi di work e di output
009100170803         clear EDIVABDS;
009200170803         clear EDIVATDS;
009300170803         clear oDKC2BVAB;
009400160608
009500160608         // Controllo i parametri di input
009600170508         exsr CtrlPIn;
009700160202
009800160202       ENDSR;
009900160802
010000160804
010100160202       //--------------------------------------------------------------
010200160202       // Controllo i parametri di input
010300160202       //--------------------------------------------------------------
010400170727       BEGSR  CtrlPIn;
010500170508
010600170727         // Se non valorizzati alcuni parametri di input => imposto default
010700170801         if iDKC2BTLE = *blanks;
010800170801            iDKC2BTLE = 'WW';
010900170727         endif;
011000170727
011100170727         // I parametri fondamentali devono essere valorizzati
011200170801         if iDKC2BKSU = *blanks OR iDKC2BKSU = *zeros;
011300170801            oDKC2BEsito = -2;
011400170727            exsr exeUscita;
011500170727         endif;
011600170801         if iDKC2BWHO = *blanks;
011700170801            oDKC2BEsito = -2;
011800170801            exsr exeUscita;
011900170801         endif;
012000170727
012100170727         // Verifico numericità parametri necessariamente numerici
012200170801         PiStr = iDKC2BKSU;
012300170727         exsr chkNum;
012400170727         if PiInt = *on;
012500170727            wKSU = PiVal;
012600170731
012700170801            // Reperisco padre del codice cliente richiesta
012800170731            iBS10CLI = wKSU;
012900170731            exsr rtvUNI;
013000170731            if oBS10COP > *zeros;
013100170731               wUnifKSU = oBS10COP;
013200170731            else;
013300170731               wUnifKSU = wKSU;
013400170731            endif;
013500170727         else;
013600170801            oDKC2BEsito = -2;
013700170727            exsr exeUscita;
013800170727         endif;
013900091223
014000091223       ENDSR;
014100121106
014200160802
014300091223       //--------------------------------------------------------------
014400160217       // Esecuzione
014500091223       //--------------------------------------------------------------
014600170508       BEGSR  RtvSped;
014700170803
014800170803         // In fase di ricerca spedizione inizializzo esito a "bolla non trovata"
014900170803         oDKC2BEsito = -3;
015000170803
015100170803         // Effettuo i tentativi di ricerca
015200170803         wTent = 1;
015300170803         dow wTent <= 2;
015400170803           select;
015500170803             when wTent = 1;
015600170803                wFIL§ = 'EDIVAT0F';
015700170803             when wTent = 2;
015800170803                // wFIL§ = 'FIVAT00F';
015900170803                wFIL§ = *blanks;
016000170803           endsl;
016100170803
016200170803           if wFIL§ <> *blanks;
016300170803             // Costruisco i filtri da applicare per la ricerca
016400170803             exsr srcUNI;
016500170803
016600170803             // Ricerco la spedizione tramite il "chi sono" ricevuto in input
016700170803             clear wCount;
016800170803             exsr srcSped;
016900170803             if wCount = 1;
017000170803                exsr rtvBolla;
017100170803             else;
017200170803                oDKC2BEsito = -3;
017300170803                exsr exeUscita;
017400170803             endif;
017500170803           endif;
017600170803
017700170803           wTent = wTent + 1;
017800170803         enddo;
017900170803
018000170803         // Se al termine delle ricerche esito no OK => uscita
018100170803         if oDKC2BEsito <> 0;
018200170803            exsr exeUscita;
018300170803         endif;
018400160217
018500160217       ENDSR;
018600170801
018700170803
018800170803       //--------------------------------------------------------------
018900170803       // Reperimento dati bolla
019000170803       //--------------------------------------------------------------
019100170803       BEGSR  rtvBolla;
019200170803
019300170803         // Se ricerca corrente per EDIVAB
019400170803         select;
019500170803            when wFIL§ = 'EDIVAT0F';
019600170803              chain (vatCCM:vatAAS:vatLNP:vatNRS:vatNSP) edivab6i;
019700170803              if %found(edivab6i);
019800170803                 // Esito OK
019900170803                 oDKC2BEsito = 0;
020000170803              endif;
020100170803            when wFIL§ = 'FIVAT00F';
020200170803         endsl;
020300170803
020400170803       ENDSR;
020500170803
020600170801
020700170801       //--------------------------------------------------------------
020800170801       // Cerco la spedizione abbinata al "chi sono" richiesto
020900170801       //--------------------------------------------------------------
021000170801       BEGSR  srcUNI;
021100170801
021200170801         // Se reperito Codice Padre cerco tra tutta la "famiglia"
021300170801         wSQL =
021400170801           ' select ' + wApi + '1' + wApi + ', ' +
021500170801           ' vatAAS, vatLNP, vatNRS, vatNSP, vatCCM ' +
021600170803           ' FROM ' + %trim(wLIB§) + %trim(wFIL§) + ' where ' +
021700170801           ' vatTRC = ' + wApi + 'E' + wApi + ' and ' +
021800170801           ' trim(vatNOT) = ' + wApi + %trim(iDKC2BWHO) + wApi +
021900170801           ' and vatCCM = ' + %char(wKSU) +
022000170801           ' union all ' +
022100170801           ' select ' + wApi + '2' + wApi + ', ' +
022200170801           ' vatAAS, vatLNP, vatNRS, vatNSP, vatCCM ' +
022300170803           ' FROM ' + %trim(wLIB§) + %trim(wFIL§) + ' where ' +
022400170801           ' vatTRC = ' + wApi + 'E' + wApi + ' and ' +
022500170801           ' trim(vatNOT) = ' + wApi + %trim(iDKC2BWHO) + wApi +
022600170801           ' and vatCCM in (' +
022700170801           ' SELECT KUNCOPF FROM TIKUN12V WHERE ' +
022800170801           ' KUNTLE = ' + wApi + iDKC2BTLE + wApi + ' AND ' +
022900170801           ' KUNCOP = ' + %char(wUnifKSU) + ' ) ' +
023000170801           ' order by 1 ';
023100170801
023200170801       ENDSR;
023300170801
023400170801
023500170801       //--------------------------------------------------------------
023600170801       // Cerca "chi sono"
023700170801       //--------------------------------------------------------------
023800170801       BEGSR  srcSped;
023900170801
024000170801         // Se istruzione SQL formata => la eseguo
024100170801         if wSQL <> *blanks;
024200170801
024300170801         // Dichiarazione cursore
024400170801         exec sql   prepare S0   from :wSQL;
024500170801         exec sql   declare C0   cursor   for S0;
024600170801
024700170801         // Apro il cursore
024800170801         exec sql open C0;
024900170801
025000170801         // Leggo i dati
025100170801         dow sqlcode = *zeros;
025200170801            exec sql
025300170803                 fetch next from C0 into :wFiller1A, :vatAAS, :vatLNP,
025400170803                                         :vatNRS,    :vatNSP, :vatCCM;
025500170803
025600170803            if sqlcode = *zeros;
025700170801               wCount = wCount + 1;
025800170803               if vatCCM = wKSU;
025900170801                  leave;
026000170801               endif;
026100170801            endif;
026200170801         enddo;
026300170801
026400170801         // Chiusura cursore
026500170801         exec sql close C0;
026600170801
026700170801         endif;
026800170801
026900170801       ENDSR;
027000170731
027100170731
027200170731       //--------------------------------------------------------------
027300170731       // Reperimento Codice Padre
027400170731       //--------------------------------------------------------------
027500170731       BEGSR rtvUni;
027600170731
027700170731         iBS10DRF  = *zeros;
027800170801         iBS10TLE  = iDKC2BTLE;
027900170731
028000170731         // Reperimento codice padre
028100170731         callp UBBS10R (iBS10DRF
028200170731                       :iBS10TLE
028300170731                       :iBS10CLI
028400170731                       :oBS10Esito
028500170731                       :oBS10COP
028600170731                       :oBS10TIP);
028700170731
028800170731       ENDSR;
028900170801
029000170801
029100170801       //--------------------------------------------------------------
029200170801       // Controllo numericità dato
029300170801       //--------------------------------------------------------------
029400170801       BEGSR chkNum;
029500170801
029600170801         // Imposto default separatore decimale
029700170801         PiDecChr = ',';
029800170801
029900170801         // Sostituisco evebtuale separatore decimale '.' in quello provisto
030000170801         PiStr = %scanrpl('.' : PiDecChr : PiStr);
030100170801
030200170801         // Verifica numericità
030300170801         callp UBISNUM_Check(PiStr
030400170801                            :PiDecChr
030500170801                            :PiVal
030600170801                            :PiNum
030700170801                            :PiInt);
030800170801
030900170801       ENDSR;
031000170801
031100170801
031200170801       //--------------------------------------------------------------
031300170801       // Uscita
031400170801       //--------------------------------------------------------------
031500170801       BEGSR exeUscita;
031600170801
031700170801         // Se tutto OK
031800170803         if oDKC2BEsito = *zeros;
031900170803            // Ritorno i dati bolla reperiti
032000170803            oDKC2BVAB = EDIVABDS;
032100170803         endif;
032200170801
032300170801         return;
032400170801
032500170801       ENDSR;
032600170727
032700170727
032800170727       //--------------------------------------------------------------
032900170727       // *INZSR
033000170727       //--------------------------------------------------------------
033100170727       BEGSR  *inzsr;
033200170801
033300170801         // Verifico il sistema AS/400 corrente
033400170803         wLibFile1 = 'FILTRAGRPF/EDIVAB6I';
033500170803         wLIB§ = 'filtraGRPF/';
033600170801         if UBRTVNETA_Rtv(PiOutSysNeta) = 0;
033700170803            if %subst(PiOutSysNeta:1:6) = 'SETRAS';
033800170803               wLibFile1 = 'FILTRAGRU/EDIVAB6I';
033900170803               wLIB§ = 'filtraGRU/';
034000170801           endif;
034100170801         endif;
034200170803         if  not %open(edivab6i);
034300170803           open  edivab6i;
034400170803         endif;
034500170727
034600170727       ENDSR;
034700091223
034800160802
034900091223      /end-free
