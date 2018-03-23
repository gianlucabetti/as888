000100091228       //==============================================================
000200170511       // Reperimento ID spedizione da RMN/RMA + CLI
000300091228       //==============================================================
000400091228
000500091228
000600160202     **
000700160202     ** ISTRUZIONI PER LA COMPILAZIONE
000800160202     **
000900170508     ** 1. Creare il modulo UBWS775R  (Opz 15 PDM)
001000161017     **    indicando TGTRLS(*CURRENT) e CLOSQLCSR(*ENDACTGRP)
001100170522     **
001200170508     ** 2. Creare/Aggiornare il programma di servizio UBWS775R  (CRTSRVPGM / UPDSRVPGM)
001300170522     **    con ACTGRP(*CALLER), e BNDDIR(UBBNDDIR)
001400170522     **      CRTSRVPGM SRVPGM(GAITRAOBJ/UBWS775R)
001500170522     **          TEXT('Driver WS Reperimento ID sped. da RMN/RMA')
001600170522     **          BNDDIR(UBBNDDIR) TGTRLS(*CURRENT)
001700170726     ** **************    EXPORT(*SRCFILE) SRCFILE(GAITRASRC/SRCSRVPGM)  <====== NO
001800170522     **
001900170508     ** 3. Creare/Aggiornare il programma UBWS775R  (CRTPGM / UPDPGM)
002000160601     **    con ACTGRP(*caller), la BNDDIR non è importante tanto non chiama niente
002100170508     **      CRTPGM PGM(GAITRAOBJ/UBWS775R) ACTGRP(*CALLER)
002200170508     **                 BNDDIR(UBBNDDIR) TGTRLS(*CURRENT)
002300170522     **
002400161017     ** 4. Eventualmente si può cancellare il modulo.
002500160202     **
002600091228
002700091228       //--------------------------------------------------------------
002800121106       // Specifiche di controllo.
002900091228       //--------------------------------------------------------------
003000091223
003100091223     h decedit('0,') datedit(*dmy/) option(*nodebugio)
003200160601     h alwnull(*inputonly)
003300091223
003400091223       //--------------------------------------------------------------
003500121106       // Dichiarazione file.
003600091223       //--------------------------------------------------------------
003700170508
003800170508
003900170508       //--------------------------------------------------------------
004000170508       // Definizione prototipi
004100170508       //--------------------------------------------------------------
004200170508      /COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
004300170509      /COPY GAITRASRC/SRCPROTOPR,UBISNUM
004400170515      /COPY GAITRASRC/SRCPROTOPR,UBBS10R
004500160218
004600160531
004700091223       //--------------------------------------------------------------
004800121106       // Definizione costanti.
004900091223       //--------------------------------------------------------------
005000100302
005100091223
005200091223       //--------------------------------------------------------------
005300121106       // Definizione schiere.
005400091223       //--------------------------------------------------------------
005500091223
005600091223
005700091223       //--------------------------------------------------------------
005800121106       // Definizione aree dati.
005900091223       //--------------------------------------------------------------
006000091223
006100091223
006200140724       //--------------------------------------------------------------
006300140724       // Definizione strutture dati.
006400140724       //--------------------------------------------------------------
006500091223
006600160802
006700091223       //--------------------------------------------------------------
006800121106       // Definizione variabili.
006900091223       //--------------------------------------------------------------
007000170508      /COPY GAITRASRC/SRCPROTOPI,UBWS775R
007100170508      /COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
007200170509      /COPY GAITRASRC/SRCPROTOPI,UBISNUM
007300170515      /COPY GAITRASRC/SRCPROTOPI,UBBS10R
007400121106
007500121106       // Campi di comodo
007600170508     D wLIB§           s             11a   inz
007700161017     D wSQL            s           1024a   inz varying
007800170511     D wWHERE_1        s            256a   inz varying
007900160802     D wCount          s              1s 0 inz
008000170509     D wTent           s              1s 0 inz
008100170515     D wUNI            s               n   inz
008200170510     D sWS775AAS       s                   inz like(iWS775AAS)
008300170726     D sWS775CLI       s                   inz like(iWS775CLI)
008400170510     D wWS775AAS       s                   inz like(iWS775AAS)
008500170511     D wWS775RMN       s                   inz like(iWS775RMN)
008600170511     D wWS775RMA       s                   inz like(iWS775RMA)
008700170511     D wWS775TIP       s                   inz like(iWS775TIP)
008800160805     D wApi            c                   const('''')
008900170508
009000170508     D  Upper          c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
009100170508     D  Lower          c                   'abcdefghijklmnopqrstuvwxyz'
009200091223
009300160802
009400091223       //--------------------------------------------------------------
009500160603       // Definizione costanti.
009600091223       //--------------------------------------------------------------
009700130307
009800160531
009900160603       //--------------------------------------------------------------
010000160603       // Definizione prototipi procedure.
010100160603       //--------------------------------------------------------------
010200160603
010300160603
010400091223       //--------------------------------------------------------------
010500121106       // Definizione key-list.
010600091223       //--------------------------------------------------------------
010700091223
010800091223
010900091223       //--------------------------------------------------------------
011000121107       // Definizione parametri procedura.
011100091223       //--------------------------------------------------------------
011200091223
011300091223     c     *Entry        plist
011400170511     c                   parm                    iWS775TIP
011500170509     c                   parm                    iWS775AAS
011600170508     c                   parm                    iWS775RMA
011700170508     c                   parm                    iWS775RMN
011800170511     c                   parm                    iWS775CLI
011900170515     c                   parm                    iWS775TLE
012000170508     c                   parm                    iWS775LANG
012100170508     c                   parm                    oWS775Esito
012200170508     c                   parm                    oWS775AAS
012300170508     c                   parm                    oWS775LNP
012400170508     c                   parm                    oWS775NRS
012500170509     c                   parm                    oWS775NSP
012600170515     c                   parm                    oWS775DSP
012700170509     c                   parm                    oWS775CLI
012800170515     c                   parm                    oWS775COP
012900170508     c                   parm                    oWS775Msg
013000091223
013100130307      /free
013200091223
013300091223       //--------------------------------------------------------------
013400121106       // M A I N - L I N E
013500091223       //--------------------------------------------------------------
013600140806
013700160805       // Monitorizzo tutto (per necessità massima solidità stack chiamate)
013800160805       Monitor;
013900160805
014000130307       // Operazioni iniziali?
014100170508       exsr RoutInz;
014200130307
014300170508       // Eseguo reperimento ID spedizione
014400170508       exsr RtvSped;
014500160804
014600160805       // Fine monitoring
014700160805       On-Error;
014800170508          oWS775Esito = -1;
014900160921          exsr exeUscita;
015000160805       Endmon;
015100160805
015200160920       // Uscita
015300160921       exsr exeUscita;
015400160802
015500160802
015600160601       //--------------------------------------------------------------
015700160601       //
015800160601       //--------------------------------------------------------------
015900160601       BEGSR  *inzsr;
016000160601
016100160601         // Inizializzazioni parametri esecuzione comandi sql
016200160802         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
016300170508
016400170508         // Verifico il sistema AS/400 corrente
016500170508         wLIB§ = 'filtra201/';
016600170508         if UBRTVNETA_Rtv(PiOutSysNeta) = 0;
016700170508            if %subst(PiOutSysNeta:1:6) <> 'SETRAS';
016800170508               wLIB§ = 'filtraprd/';
016900170508           endif;
017000170508         endif;
017100160601
017200160601       ENDSR;
017300091223
017400160802
017500091223       //--------------------------------------------------------------
017600121107       // Operazioni iniziali.
017700091223       //--------------------------------------------------------------
017800170726       BEGSR  RoutInz;
017900160601
018000160920         // Inizializzo l'esito a errore generico
018100170508         oWS775Esito = -1;
018200170511
018300170515         // Inizializzazione campi di work e di output
018400170511         clear sWS775AAS;
018500170511         clear wWS775AAS;
018600170511         clear wWS775RMN;
018700170511         clear wWS775RMA;
018800170511         clear wWS775TIP;
018900170515         clear oWS775COP;
019000160608
019100160608         // Controllo i parametri di input
019200170508         exsr CtrlPIn;
019300160202
019400160202       ENDSR;
019500160802
019600160804
019700160202       //--------------------------------------------------------------
019800160202       // Controllo i parametri di input
019900160202       //--------------------------------------------------------------
020000170508       BEGSR  CtrlPIn;
020100170508
020200170508         // I parametri fondamentali devono essere valorizzati
020300170511         if iWS775CLI = *zeros;
020400170511            oWS775Esito = -2;
020500170511            exsr exeUscita;
020600170511         endif;
020700170508         if iWS775RMA = *blanks AND iWS775RMN = *zeros;
020800170508            oWS775Esito = -2;
020900170508            exsr exeUscita;
021000160802         endif;
021100170511
021200170511         // Verifica il tipo di ricerca richiesto
021300170511         if iWS775TIP = 'N' OR
021400170511            iWS775TIP = 'A' OR
021500170515            iWS775TIP = 'B' OR
021600170726            iWS775TIP = 'O' OR
021700170511            iWS775TIP = 'X';
021800170511         else;
021900170511            oWS775Esito = -2;
022000170511            exsr exeUscita;
022100170511         endif;
022200170511
022300170511         // Se richiesto tipo ricerca 'X' (cross) verifica numericità Riferimento richiesto
022400170511         wWS775TIP = iWS775TIP;
022500170511         select;
022600170511         when iWS775TIP = 'N';
022700170511            wWS775RMN = iWS775RMN;
022800170511         when iWS775TIP = 'A';
022900170511            wWS775RMA = iWS775RMA;
023000170515         when iWS775TIP = 'B';
023100170515            wWS775RMA = iWS775RMA;
023200170515            wWS775RMN = iWS775RMN;
023300170726         when iWS775TIP = 'O';
023400170726            wWS775RMA = iWS775RMA;
023500170726            wWS775RMN = iWS775RMN;
023600170511         when iWS775TIP = 'X';
023700170511            PiStr = iWS775RMA;
023800170511            exsr chkNum;
023900170511            if PiInt = *on;
024000170511               wWS775RMN = PiVal;
024100170511               wWS775RMA = iWS775RMA;
024200170511            else;
024300170511               wWS775TIP = 'A';
024400170511               wWS775RMA = iWS775RMA;
024500170511            endif;
024600170511         endsl;
024700170511
024800170511         // Se anno spedizione non valorizzato imposto quello corrente
024900170511         if iWS775AAS = *zeros;
025000170511            sWS775AAS = %subdt(%date():*YEARS);
025100170512         else;
025200170512            sWS775AAS = iWS775AAS;
025300170511         endif;
025400170508
025500170508         // Maiuscolizzo il parametro relatovo alla lingua
025600170508         select;
025700170508           when iWS775LANG = 'IT';
025800170508           when iWS775LANG = 'EN';
025900170508           when iWS775LANG = *blanks;
026000170508                iWS775LANG = 'IT';
026100170508           other;
026200170508                iWS775LANG = %xlate(Lower:Upper:iWS775LANG);
026300170508         endsl;
026400170508         if iWS775LANG = 'IT' OR iWS775LANG = 'EN';
026500170508         else;
026600170508            iWS775LANG = 'EN';
026700170508         endif;
026800091223
026900091223       ENDSR;
027000170511
027100170511
027200170511       //--------------------------------------------------------------
027300170511       // Operazioni propedeutiche rispetto al tipo ricerca richiesto
027400170511       //--------------------------------------------------------------
027500170511       BEGSR chkTip;
027600170511
027700170511         // Imposto default separatore decimale
027800170511         clear wWHERE_1;
027900170511         select;
028000170515          when wWS775TIP = 'N';
028100170515             wWHERE_1 = ' blpRMN = ' + %char(wWS775RMN) + ' and ';
028200170515          when wWS775TIP = 'A';
028300170515             wWHERE_1 = ' blpRMA = ' + wApi + %trim(wWS775RMA) + wApi + ' and ';
028400170515          when wWS775TIP = 'B';
028500170515             wWHERE_1 = ' blpRMN = ' + %char(wWS775RMN) + ' and ' +
028600170515                        'blpRMA = ' + wApi + %trim(wWS775RMA) + wApi + ' and ';
028700170726          when wWS775TIP = 'O';
028800170726             wWHERE_1 = ' (blpRMN = ' + %char(wWS775RMN) + ' or ' +
028900170726                        'blpRMA = ' + wApi + %trim(wWS775RMA) + wApi + ') and ';
029000170515          when wWS775TIP = 'X';
029100170515             wWHERE_1 = ' (blpRMN = ' + %char(wWS775RMN) + ' or ' +
029200170515                        'blpRMA = ' + wApi + %trim(wWS775RMA) + wApi + ') and ';
029300170511         endsl;
029400170511
029500170511       ENDSR;
029600121106
029700160802
029800091223       //--------------------------------------------------------------
029900160217       // Esecuzione
030000091223       //--------------------------------------------------------------
030100170508       BEGSR  RtvSped;
030200160531
030300170511         // Compondo clausola WHERE in relazione al tipo ricerca richiesto
030400170511         exsr chkTip;
030500170511
030600170511         // Effettuo ricerce per gli <n> tentativi previsti
030700170510         wWS775AAS = sWS775AAS;
030800170510         clear wCount;
030900170510         clear wTent;
031000170510         dow wCount = *zeros AND wTent < 4;
031100170510
031200170510           // Incremento il tentativo corrente
031300170510           wTent = wTent + 1;
031400170510
031500170510           // Definisco specificita per tentativo corrente
031600170510           select;
031700170510             when wTent = 1;
031800170510               exsr srcCLI;
031900170510             when wTent = 2;
032000170515               exsr rtvUNI;
032100170510               exsr srcUNI;
032200170510             when wTent = 3 AND iWS775AAS = *zeros;
032300170510               wWS775AAS = sWS775AAS - 1;
032400170510               exsr srcCLI;
032500170510             when wTent = 4 AND iWS775AAS = *zeros;
032600170510               wWS775AAS = sWS775AAS - 1;
032700170515               exsr rtvUNI;
032800170510               exsr srcUNI;
032900170510           endsl;
033000170510
033100170510           // Ricerca spedizione con parametri del tentativo corrente
033200170510           exsr srcSped;
033300170510
033400170510           // Appena spedizione trovata => uscita
033500170510           if wCount > *zeros;
033600170510               leave;
033700170510           endif;
033800170510
033900170509         enddo;
034000170508
034100170508         // Verifico esito ricerca
034200170508         select;
034300170508            when wCount = *zeros;
034400170508               oWS775Esito = -3;
034500170508            when wCount = 1;
034600170508               oWS775Esito = *zeros;
034700170508            when wCount > 1;
034800170508               oWS775Esito = -4;
034900170508            other;
035000170508               oWS775Esito = -1;
035100170508         endsl;
035200170508
035300170508         // Se esito non OK per "sicurezza" pulisco dati di output
035400170508         if oWS775Esito = *zeros;
035500170508         else;
035600170508            clear oWS775AAS;
035700170508            clear oWS775LNP;
035800170508            clear oWS775NRS;
035900170508            clear oWS775NSP;
036000170515            clear oWS775DSP;
036100170509            clear oWS775CLI;
036200170508         endif;
036300160217
036400160217       ENDSR;
036500170509
036600170509
036700170509       //--------------------------------------------------------------
036800170509       // Cerca spedizione
036900170509       //--------------------------------------------------------------
037000170509       BEGSR  srcSped;
037100170509
037200170515         // Se istruzione SQL formata => la eseguo
037300170515         if wSQL <> *blanks;
037400170515
037500170509         // Dichiarazione cursore
037600170509         exec sql   prepare S0   from :wSQL;
037700170509         exec sql   declare C0   cursor   for S0;
037800170509
037900170509         // Apro il cursore
038000170509         exec sql open C0;
038100170509
038200170509         // Leggo i dati
038300170509         dow sqlcode = *zeros;
038400170509            exec sql
038500170509                 fetch next from C0 into :oWS775AAS,
038600170509                                         :oWS775LNP,
038700170509                                         :oWS775NRS,
038800170509                                         :oWS775NSP,
038900170515                                         :oWS775DSP,
039000170509                                         :oWS775CLI;
039100170509            if sqlcode <> 100;
039200170509               wCount = wCount + 1;
039300170509            endif;
039400170509         enddo;
039500170509
039600170509         // Chiusura cursore
039700170509         exec sql close C0;
039800170515
039900170515         endif;
040000170509
040100170509       ENDSR;
040200170509
040300170509
040400170509       //--------------------------------------------------------------
040500170509       // Gestione 1st tentativo per cod. cliente
040600170509       //--------------------------------------------------------------
040700170510       BEGSR  srcCLI;
040800170509
040900170511          wSQL =
041000170515            'select blpAAS, blpLNP, blpNRS, blpNSP, ' +
041100170515            'blpAAS*10000+blpMGS as blpDSP, blpCLI ' +
041200170511            'FROM ' + %trim(wLIB§) + 'fnblp51v ' +
041300170511            'where blpcli = ' + %char(iWS775CLI) + ' and ' +
041400170511            wWHERE_1 +
041500170511            'blpAAS = ' + %char(wWS775AAS) + ' and ' +
041600170511            'blpcbo in (' +
041700170511            'SELECT T03AKEY FROM tabel03av where ' +
041800170511            'T03A§TB1 in (select T0TBKEY from tabel0tbv where ' +
041900170511            'T0TB§FSP = ' + wApi + 'S' + wApi + ' and ' +
042000170511            'T0TB§FRI = ' + wApi + 'S' + wApi + '))';
042100170509
042200170509       ENDSR;
042300170509
042400170509
042500170509       //--------------------------------------------------------------
042600170720       // Gestione 2nd tentativo per "famiglia"
042700170509       //--------------------------------------------------------------
042800170510       BEGSR  srcUNI;
042900170509
043000170515         // Se reperito Codice Padre cerco tra tutta la "famiglia"
043100170515         if oWS775COP > *zeros;
043200170515           wSQL =
043300170515             'select blpAAS, blpLNP, blpNRS, blpNSP, ' +
043400170515             'blpAAS*10000+blpMGS as blpDSP, blpCLI ' +
043500170515             'FROM ' + %trim(wLIB§) + 'fnblp51v ' +
043600170515             'where blpcli in (' +
043700170515             'SELECT KUNCOPF FROM TIKUN12V WHERE ' +
043800170515             'KUNTLE = ' + wApi + iWS775TLE + wApi + ' AND ' +
043900170515             'KUNCOP = ' + %char(oWS775COP) + ' ) AND ' +
044000170515             wWHERE_1 +
044100170515             'blpAAS = ' + %char(wWS775AAS) + ' and ' +
044200170515             'blpcbo in (' +
044300170515             'SELECT T03AKEY FROM tabel03av where ' +
044400170515             'T03A§TB1 in (select T0TBKEY from tabel0tbv where ' +
044500170515             'T0TB§FSP = ' + wApi + 'S' + wApi + ' and ' +
044600170515             'T0TB§FRI = ' + wApi + 'S' + wApi + '))';
044700170515         else;
044800170515           clear wSQL;
044900170515         endif;
045000170509
045100170509       ENDSR;
045200170509
045300160921
045400160921       //--------------------------------------------------------------
045500160921       // Operazioni finali.
045600160921       //--------------------------------------------------------------
045700170508       BEGSR  RoutEnd;
045800160921
045900160921         // Imposto il messaggio di esito in lingua
046000160921         select;
046100170508            when iWS775LANG = 'IT';
046200160921                 select;
046300170508                    when oWS775Esito = *zeros;
046400170508                       oWS775Msg = 'OK';
046500170508                    when oWS775Esito = -1;
046600170508                       oWS775Msg = 'Errore generico';
046700170508                    when oWS775Esito = -2;
046800170508                       oWS775Msg = 'Parametri input non validi';
046900170508                    when oWS775Esito = -3;
047000170508                       oWS775Msg = 'Spedizione non trovata';
047100170508                    when oWS775Esito = -4;
047200170508                       oWS775Msg = 'Spedizione non univoca';
047300160921                 endsl;
047400160921
047500170508            when iWS775LANG = 'EN';
047600160921                 select;
047700170508                    when oWS775Esito = *zeros;
047800170508                       oWS775Msg = 'OK';
047900170508                    when oWS775Esito = -1;
048000170508                       oWS775Msg = 'Generic Error';
048100170508                    when oWS775Esito = -2;
048200170508                       oWS775Msg = 'Input Parameters not valid';
048300170508                    when oWS775Esito = -3;
048400170508                       oWS775Msg = 'Shipment not Found';
048500170508                    when oWS775Esito = -4;
048600170508                       oWS775Msg = 'Shipment not Unique';
048700160921                 endsl;
048800160921         endsl;
048900160921
049000160921       ENDSR;
049100170509
049200170509
049300170509       //--------------------------------------------------------------
049400170509       // Controllo numericità dato
049500170509       //--------------------------------------------------------------
049600170509       BEGSR chkNum;
049700170509
049800170509         // Imposto default separatore decimale
049900170509         PiDecChr = ',';
050000170509
050100170509         // Verifica numericità
050200170509         callp UBISNUM_Check(PiStr
050300170509                            :PiDecChr
050400170509                            :PiVal
050500170509                            :PiNum
050600170509                            :PiInt);
050700170509
050800170509       ENDSR;
050900170515
051000170515
051100170515       //--------------------------------------------------------------
051200170515       // Verifica unificazione
051300170515       //--------------------------------------------------------------
051400170515       BEGSR rtvUNI;
051500170515
051600170726         // Se cambia il codice cliente => resetto indicatore di reperimentio Codice Padre
051700170726         if iWS775CLI <> sWS775CLI;
051800170726            wUNI = *off;
051900170726            sWS775CLI = iWS775CLI;
052000170726         else;
052100170726            if wUNI;
052200170726               oWS775COP = oBS10COP;
052300170726            endif;
052400170726         endif;
052500170726
052600170726
052700170726         if not wUNI and iWS775TLE <> *blanks;
052800170515
052900170515            // Accendo indicatore che indica reperimento Codice Padre già effettuato
053000170515            wUNI = *on;
053100170515
053200170515            // Reperimento Codice Padre
053300170515            iBS10DRF = *zeros;
053400170515            callp UBBS10R(iBS10DRF
053500170515                         :iWS775TLE
053600170515                         :iWS775CLI
053700170515                         :oBS10Esito
053800170515                         :oBS10COP
053900170515                         :oBS10TIP);
054000170515
054100170515            if oBS10Esito = *zeros;
054200170515               oWS775COP = oBS10COP;
054300170515            endif;
054400170515
054500170515         endif;
054600170515
054700170515       ENDSR;
054800160921
054900160921
055000160921       //--------------------------------------------------------------
055100160921       // Uscita
055200160921       //--------------------------------------------------------------
055300160921       BEGSR exeUscita;
055400170508
055500170508         // Operazioni finali
055600170508         exsr RoutEnd;
055700160921
055800160921         // Chiusura cursore
055900160921         exec sql close C0;
056000160921
056100160921         return;
056200160921
056300160921       ENDSR;
056400091223
056500160802
056600091223      /end-free
