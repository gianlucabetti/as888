000100091228       //==============================================================
000200170215       // Driver reperimento Ragione Sociale per Alert e T&T
000300091228       //==============================================================
000400091228
000500091228       //--------------------------------------------------------------
000600121106       // Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700091228       //--------------------------------------------------------------
000800091228
000900100325     /*PRM dbgview(*source)
001000091223     /*END
001100160202     **
001200160202     ** ISTRUZIONI PER LA COMPILAZIONE
001300160202     **
001400170215     ** 1. Creare il modulo UBRDMALR (Opz 15 PDM)
001500170314     ** 2. Creare/Aggiornare il programma di servizio UBRSMALR (CRTSRVPGM / UPDSRVPGM)
001600170314     **    con ACTGRP(*caller) e BNDDIR=UBBNDDIR
001700170314     **    CRTSRVPGM SRVPGM(GAITRAOBJ/UBRSMALR) EXPORT(*ALL) BNDDIR(UBBNDDIR)
001800170314     **              TEXT('Reperimento Ragione Sociale per Alert/T&T')
001900170215     ** 3. per chi lo usa e per lo scopo, non serve che sia anche PGM
002000160202     ** 4. Cancellare il modulo.
002100160202     **
002200091228
002300091228       //--------------------------------------------------------------
002400121106       // Specifiche di controllo.
002500091228       //--------------------------------------------------------------
002600091223
002700091223     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002800091223     h alwnull(*inputonly)
002900091223
003000091223       //--------------------------------------------------------------
003100121106       // Dichiarazione file.
003200091223       //--------------------------------------------------------------
003300160218
003400160219     FFNBLP01L  IF   E           K DISK    extfile(wLibFileBLP)  usropn
003500160219     F                                     extdesc('FILTRAPRD/FNBLP01L')
003600160218     FFNARB01L  IF   E           K DISK    extfile(wLibFileARB)  usropn
003700160219     F                                     extdesc('FILTRAPRD/FNARB01L')
003800170704     FTITAS30C  IF   E           K DISK    usropn extfile(wLibFileTAS)
003900170704     FTITAA30C  IF   E           K DISK    usropn extfile(wLibFileTAA)
004000170704     FCNACO00F  IF   E           K DISK    usropn
004100170704     FCNIND00F  IF   E           K DISK    usropn
004200170704     FTFNTC01L  IF   E           K DISK    usropn
004300170704     FTNTBE01L  IF   E           K DISK    extfile(wLibFileTBE)  usropn
004400091223       //--------------------------------------------------------------
004500121106       // Definizione costanti.
004600091223       //--------------------------------------------------------------
004700100302
004800091223
004900091223       //--------------------------------------------------------------
005000121106       // Definizione schiere.
005100091223       //--------------------------------------------------------------
005200091223
005300091223
005400091223       //--------------------------------------------------------------
005500121106       // Definizione aree dati.
005600091223       //--------------------------------------------------------------
005700091223
005800091223
005900140724       //--------------------------------------------------------------
006000140724       // Definizione strutture dati.
006100140724       //--------------------------------------------------------------
006200140724
006300121106       // - Status
006400110516     d Psds           sds
006500110516     d   SDSpgm          *proc
006600121106     d   JobName             244    253                                         Job name
006700121106     d   JobUser             254    263                                         User name
006800121106     d   JobNumber           264    269s 0                                      Job number
006900170216
007000170216       // Tabella di passaggio dati
007100170216     d UBRSMALDS     e ds                  qualified
007200170216
007300170216       // Tabella CLI
007400170216     d dCLI          e ds                  inz
007500170216
007600170216       // - Parametri in input?
007700170216     d KPJBA         e ds
007800170216
007900091223       //--------------------------------------------------------------
008000121106       // Definizione variabili.
008100091223       //--------------------------------------------------------------
008200121106
008300121106       // Campi di comodo
008400170704       // Nome della libreria in cui è il file
008500160218     d wLibFileBLP     s             21a   inz
008600160218     d wLibFileARB     s             21a   inz
008700170315     d wLibFileTBE     s             21a   inz
008800170704     d wLibFileTAS     s             21a   inz
008900170704     d wLibFileTAA     s             21a   inz
009000160218       // Nome del sistema
009100160218     d currSysNeta     s              8a   inz
009200170216       // Cliente
009300170216     d wCliente        s              7s 0 inz
009400091223
009500091223       //--------------------------------------------------------------
009600121106       // Definizione prototipi procedure.
009700091223       //--------------------------------------------------------------
009800160218
009900170215      /copy gaitrasrc/srcProtoPI,UBRSMALR
010000170216
010100170216       // Reperimento NETA sistema AS/400 corrente
010200170216      /copy gaitrasrc/srcProtoPr,UBRTVNETA
010300130307
010400091223       //--------------------------------------------------------------
010500121106       // Definizione key-list.
010600091223       //--------------------------------------------------------------
010700091223
010800170216       // - TFNTC01L
010900170216     d k04tFNTC01    e ds                  extname(TFNTC01L : *key)
011000170216     d                                     prefix(k_)   inz
011100091223
011200091223       //--------------------------------------------------------------
011300121106       // M A I N - L I N E
011400091223       //--------------------------------------------------------------
011500140806
011600130307       // Operazioni iniziali?
011700130307       exsr sr_RoutInz;
011800130307
011900170216       // Esecuzione
012000170216       exsr esegui;
012100091223
012200121106       // Operazioni finali?
012300091223       exsr sr_RoutEnd;
012400091223
012500091223       //--------------------------------------------------------------
012600121107       // Operazioni iniziali.
012700091223       //--------------------------------------------------------------
012800091223       BEGSR  sr_RoutInz;
012900170215
013000170216         UBRSMALDS = UBRSMALR_DS;
013100170710         clear dCLI;
013200170216
013300170215         // Controllo i parametri di input
013400170215         exsr sr_CtrlPIn;
013500160218
013600170215         // Verifica del sistema AS/400 corrente
013700170710         if currSysNeta = *blanks;
013800170710         if UBRTVNETA_Rtv(currSysNeta) <> *zero;
013900170215           // esco dando errore
014000170216           UBRSMALDS.oEsito = -1;
014100170215           exsr sr_RoutEnd;
014200170215         endif;
014300160218
014400170215         // Apertura archivi
014500170215         if %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
014600170215           wLibFileBLP = 'FILTRA201/FNBLP01L';
014700170215           wLibFileARB = 'FILTRA201/FNARB01L';
014800170704           wLibFileTBE = 'GAITRAGRU/TNTBE01L';
014900170704           wLibFileTAS = 'GAITRAGRU/TITAS30C';
015000170704           wLibFileTAA = 'GAITRAGRU/TITAA30C';
015100170215         else;
015200170215           wLibFileBLP = 'FILTRAPRD/FNBLP01L';
015300170704           wLibFileARB = 'FILTRAPRD/FNARB01L';
015400170704           wLibFileTBE = 'GAITRAGRPS/TNTBE01L';
015500170704           wLibFileTAS = 'GAITRAGRPS/TITAS30C';
015600170704           wLibFileTAA = 'GAITRAGRPS/TITAA30C';
015700170215         endif;
015800170710         endif;
015900160218
016000170315         if  not %open(TNTBE01L);
016100170315           open  TNTBE01L;
016200170315         endif;
016300170215         if  not %open(TFNTC01L);
016400170215           open  TFNTC01L;
016500170215         endif;
016600160218
016700160218         // Inizializzazione variabili
016800170216         wCliente = 0;
016900170216         UBRSMALDS.oEsito = 0;
017000170216         clear UBRSMALDS.oRagSoc;
017100170710         clear UBRSMALDS.oInM;
017200170710         clear UBRSMALDS.oCAM;
017300170710         clear UBRSMALDS.oLoM;
017400170710         clear UBRSMALDS.oPrM;
017500170710         clear UBRSMALDS.oNzM;
017600160202
017700160202       ENDSR;
017800160202
017900160202       //--------------------------------------------------------------
018000160202       // Controllo i parametri di input
018100160202       //--------------------------------------------------------------
018200160202       BEGSR  sr_CtrlPIn;
018300140724
018400170215         select;
018500170216          when (UBRSMALDS.iTpRic <> 'E' and
018600170216                UBRSMALDS.iTpRic <> 'S' and
018700170710                UBRSMALDS.iTpRic <> 'L' and
018800170216                UBRSMALDS.iTpRic <> 'T')    or
018900170216               (UBRSMALDS.iOrigine <> 'P' and
019000170216                UBRSMALDS.iOrigine <> 'A' and
019100170216                UBRSMALDS.iOrigine <> 'S')   or
019200170216               UBRSMALDS.iAAS = 0            or
019300170216               UBRSMALDS.iLNP = 0            or
019400170216               UBRSMALDS.iNSP = 0 ;
019500170215           // esco dando errore
019600170216           UBRSMALDS.oEsito = -3;
019700170215           exsr sr_RoutEnd;
019800170215         endsl;
019900091223
020000091223       ENDSR;
020100121106
020200091223       //--------------------------------------------------------------
020300160217       // Esecuzione
020400091223       //--------------------------------------------------------------
020500160217       BEGSR  esegui;
020600140724
020700170215         select;
020800170215          // PARTENZA
020900170216          when UBRSMALDS.iOrigine = 'P';
021000170315           if  not %open(FNBLP01L);
021100170315             open  FNBLP01L;
021200170315           endif;
021300170215           // reperisco partenza
021400170216           chain(n) (UBRSMALDS.iAAS:UBRSMALDS.iLNP:
021500170216                     UBRSMALDS.iNRS:UBRSMALDS.iNSP)  FNBLP000;
021600170215           if not %found(fnblp01l);
021700170215             // esco dando errore
021800170216             UBRSMALDS.oEsito = -2;
021900170215             exsr sr_RoutEnd;
022000170215           endif;
022100170215          // ARRIVO
022200170216          when UBRSMALDS.iOrigine = 'A';
022300170315           if  not %open(FNARB01L);
022400170315             open  FNARB01L;
022500170315           endif;
022600170315           // reperisco arrivo
022700170216           chain(n) (UBRSMALDS.iAAS:UBRSMALDS.iLNP:
022800170216                     UBRSMALDS.iNRS:UBRSMALDS.iNSP)  FNARB000;
022900170215           if not %found(fnarb01l);
023000170215             // esco dando errore
023100170216             UBRSMALDS.oEsito = -2;
023200170215             exsr sr_RoutEnd;
023300170215           endif;
023400170315          // SEDE
023500170315          when UBRSMALDS.iOrigine = 'S';
023600170315           if  not %open(TITAS30C);
023700170315             open  TITAS30C;
023800170315           endif;
023900170315           if  not %open(TITAA30C);
024000170315             open  TITAA30C;
024100170315           endif;
024200170315           // reperisco sede
024300170315           chain(n) (UBRSMALDS.iAAS:UBRSMALDS.iLNP:
024400170315                     UBRSMALDS.iNRS:UBRSMALDS.iNSP)  TITAS30C;
024500170315           if not %found(titas30c);
024600170315             // esco dando errore
024700170315             UBRSMALDS.oEsito = -2;
024800170315             exsr sr_RoutEnd;
024900170315           endif;
025000170215         endsl;
025100160217
025200170215         // determino il codice cliente
025300170215         select;
025400170215          // PARTENZA
025500170216          when UBRSMALDS.iOrigine = 'P';
025600170216           if BLPCCM > 0;
025700170216             wCliente = BLPCCM;
025800170216           else;
025900170216             wCliente = BLPKSC;
026000170216           endif;
026100170215          // ARRIVO
026200170216          when UBRSMALDS.iOrigine = 'A';
026300170216           if ARBCCM > 0;
026400170216             wCliente = ARBCCM;
026500170216           else;
026600170216             wCliente = ARBKSC;
026700170216           endif;
026800170315          // SEDE
026900170315          when UBRSMALDS.iOrigine = 'S';
027000170315           if %subst(%editc(TASCCM:'X'):4:4) = '0000' OR
027100170315              %subst(%editc(TASCCM:'X'):4:4) = '8888' OR
027200170315              %subst(%editc(TASCCM:'X'):4:4) = '9999';
027300170315             wCliente = TASKSC;
027400170315           else;
027500170315             wCliente = TASCCM;
027600170315           endif;
027700170215         endsl;
027800170315
027900170216         // reperisco la tab. CLI per il cod.clente definito
028000170314         chain ('CLI':%editc(wCliente:'X')) tntbe01l;
028100170314         if %found(tntbe01l);
028200170314           DCLI = tbeUNI;
028300170216         else;
028400170216           // la tabella CLI non esiste per tutti i clienti, quindi se non la trovo la azzero
028500170216           clear DCLI;
028600170216         endif;
028700170710
028800170710         // valorizzo quindi i dati relativi al indirizzo
028900170710         select;
029000170710          // PARTENZA
029100170710          when UBRSMALDS.iOrigine = 'P';
029200170710               UBRSMALDS.oInM     = BLPINM;
029300170710               UBRSMALDS.oCAM     = BLPCAM;
029400170710               UBRSMALDS.oLoM     = BLPLOM;
029500170710               UBRSMALDS.oPrM     = BLPPRM;
029600170710               UBRSMALDS.oNzM     = BLPNZM;
029700170710          // ARRIVO
029800170710          when UBRSMALDS.iOrigine = 'A';
029900170710               UBRSMALDS.oInM     = ARBINM;
030000170710               UBRSMALDS.oCAM     = ARBCAM;
030100170710               UBRSMALDS.oLoM     = ARBLOM;
030200170710               UBRSMALDS.oPrM     = ARBPRM;
030300170710               UBRSMALDS.oNzM     = ARBNZM;
030400170710          // SEDE
030500170710          when UBRSMALDS.iOrigine = 'S';
030600170710             if %subst(%editc(TASCCM:'X'):4:4) = '0000' OR
030700170710                %subst(%editc(TASCCM:'X'):4:4) = '8888' OR
030800170710                %subst(%editc(TASCCM:'X'):4:4) = '9999';
030900170710               // reperisco anagrafica bolle di sede
031000170710               chain(n) (UBRSMALDS.iAAS:UBRSMALDS.iLNP:
031100170710                         UBRSMALDS.iNRS:UBRSMALDS.iNSP:'M')  TITAA30C;
031200170710               if %found(titaa30c);
031300170710                  UBRSMALDS.oInM     = TAAIND;
031400170710                  UBRSMALDS.oCAM     = TAACAP;
031500170710                  UBRSMALDS.oLoM     = TAALOC;
031600170710                  UBRSMALDS.oPrM     = TAAPRV;
031700170710                  UBRSMALDS.oNzM     = TAANAZ;
031800170710               endif;
031900170710             else;
032000170710               if  not %open(CNIND00F);
032100170710                 open  CNIND00F;
032200170710               endif;
032300170710               // reperisco indirizzo in CNIND
032400170710               chain(n) (1 : 151 : TASCCM) CNIND00F;
032500170710               if %found(cnind00f);
032600170710                  UBRSMALDS.oInM     = INDVIA;
032700170710                  UBRSMALDS.oCAM     = %char(INDCAP);
032800170710                  UBRSMALDS.oLoM     = INDCIT;
032900170710                  UBRSMALDS.oPrM     = INDPRV;
033000170710                  UBRSMALDS.oNzM     = INDSTA;
033100170710               endif;
033200170710             endif;
033300170710         endsl;
033400170216
033500170216         // se per il cliente va forzato il mittente originale
033600170216         if §CLIMITOR = 'S';
033700170216           select;
033800170216            // PARTENZA
033900170216            when UBRSMALDS.iOrigine = 'P';
034000170216             if BLPRMO <> *blank;
034100170216               UBRSMALDS.oRagSoc  = BLPRMO;
034200170216               // esco perché ho finito correttamente
034300170216               UBRSMALDS.oEsito = 0;
034400170216               exsr sr_RoutEnd;
034500170216             endif;
034600170216            // ARRIVO
034700170216            when UBRSMALDS.iOrigine = 'A';
034800170216             if ARBRMO <> *blank;
034900170216               UBRSMALDS.oRagSoc  = ARBRMO;
035000170216               // esco perché ho finito correttamente
035100170216               UBRSMALDS.oEsito = 0;
035200170216               exsr sr_RoutEnd;
035300170216             endif;
035400170315            // SEDE
035500170315            when UBRSMALDS.iOrigine = 'S';
035600170710             // reperisco anagrafica bolle di sede
035700170710             chain(n) (UBRSMALDS.iAAS:UBRSMALDS.iLNP:
035800170710                       UBRSMALDS.iNRS:UBRSMALDS.iNSP:'O')  TITAA30C;
035900170710             if %found(titaa30c);
036000170710                // se la ragione sociale è valorizzata, uso questa
036100170710                if TAARSC <> *blank;
036200170710                   UBRSMALDS.oRagSoc = TAARSC;
036300170710                  // esco perché ho finito correttamente
036400170710                  UBRSMALDS.oEsito = 0;
036500170710                  exsr sr_RoutEnd;
036600170710                endif;
036700170710             endif;
036800170216           endsl;
036900170216         endif;
037000170216
037100170315         // se per il cliente va forzato il mittente da nota AM/AS
037200170315         if §CLITFNTC = 'S'
037300170710         // e non ho già recuperato la ragione sociale
037400170710         and UBRSMALDS.oRagSoc = *blank;
037500170216           select;
037600170216            // SMS
037700170216            when UBRSMALDS.iTpRic = 'S';
037800170216             k_NTCAPL =  'C';
037900170216             k_NTCNK1 =  '0151' + %editc(wCliente:'X');
038000170216             k_NTCNK2 =  *BLANK;
038100170216             k_NTCTNT =  'AS';
038200170216             chain(n)  %kds(k04tFNTC01)  TFNTC;
038300170216             if %found();
038400170216               UBRSMALDS.oRagSoc = NTCRNT;
038500170216             endif;
038600170710
038700170216            // Email o T&T
038800170216            when UBRSMALDS.iTpRic = 'E' or
038900170710                 UBRSMALDS.iTpRic = 'T' or
039000170710                 UBRSMALDS.iTpRic = 'L';
039100170216             k_NTCAPL =  'C';
039200170216             k_NTCNK1 =  '0151' + %editc(wCliente:'X');
039300170216             k_NTCNK2 =  *BLANK;
039400170216             k_NTCTNT =  'AM';
039500170216             chain(n)  %kds(k04tFNTC01)  TFNTC;
039600170216             if %found();
039700170216               UBRSMALDS.oRagSoc = NTCRNT;
039800170216             endif;
039900170216           endsl;
040000170216         endif;
040100170216
040200170216         // se flag precedenti non a 'S' oppure la ragione sociale è ancora blank
040300170216         if UBRSMALDS.oRagSoc = *blank;
040400170710           // imposto la ragione sociale con quella del mittente (NON originale)
040500170216           select;
040600170216            // PARTENZA
040700170216            when UBRSMALDS.iOrigine = 'P';
040800170216             UBRSMALDS.oRagSoc  = BLPRSM;
040900170216            // ARRIVO
041000170216            when UBRSMALDS.iOrigine = 'A';
041100170216             UBRSMALDS.oRagSoc  = ARBRSM;
041200170315            // SEDE
041300170710            when UBRSMALDS.iOrigine = 'S';
041400170710             if %subst(%editc(TASCCM:'X'):4:4) = '0000' OR
041500170315                %subst(%editc(TASCCM:'X'):4:4) = '8888' OR
041600170315                %subst(%editc(TASCCM:'X'):4:4) = '9999';
041700170315               // reperisco anagrafica bolle di sede
041800170315               chain(n) (UBRSMALDS.iAAS:UBRSMALDS.iLNP:
041900170315                         UBRSMALDS.iNRS:UBRSMALDS.iNSP:'M')  TITAA30C;
042000170315               if not %found(titaa30c);
042100170315                 // esco dando errore
042200170315                 UBRSMALDS.oEsito = -2;
042300170315                 exsr sr_RoutEnd;
042400170315               else;
042500170710                 UBRSMALDS.oRagSoc  = TAARSC;
042600170315               endif;
042700170315             else;
042800170315               if  not %open(CNACO00F);
042900170315                 open  CNACO00F;
043000170315               endif;
043100170315               // reperisco anagrafica
043200170710               chain(n) (1 : 151 : TASCCM) CNACO00F;
043300170315               if not %found(cnaco00f);
043400170315                 // esco dando errore
043500170315                 UBRSMALDS.oEsito = -2;
043600170315                 exsr sr_RoutEnd;
043700170315               else;
043800170315                 UBRSMALDS.oRagSoc = ACORAG;
043900170517               endif;
044000170315             endif;
044100170216           endsl;
044200170216         endif;
044300170216
044400170315         // se la ragione sociale è ancora blank
044500170315         if UBRSMALDS.oRagSoc = *blank;
044600170315           // esco dando errore
044700170315           UBRSMALDS.oEsito = -2;
044800170315         // altrimenti tutto ok
044900170315         else;
045000170315           UBRSMALDS.oEsito = 0   ;
045100170315         endif;
045200160217
045300160217       ENDSR;
045400160217
045500160217       //--------------------------------------------------------------
045600160217       // Operazioni finali.
045700160217       //--------------------------------------------------------------
045800170710       BEGSR  sr_RoutEnd;
045900170216
046000170710         // Se richiesto esplicitamente nascondere dati indirizzo mittente => pulisco
046100170710         if §CLINOIMTT = 'S'       or
046200170710            UBRSMALDS.iTpRic = 'T' or
046300170710            UBRSMALDS.iTpRic = 'L';
046400170710            clear UBRSMALDS.oInM;
046500170710            clear UBRSMALDS.oCAM;
046600170710            clear UBRSMALDS.oLoM;
046700170710            clear UBRSMALDS.oPrM;
046800170710            clear UBRSMALDS.oNzM;
046900170710         endif;
047000170710
047100170216         UBRSMALR_DS = UBRSMALDS;
047200160217
047300170215         *inrt = *on;
047400140724         return;
047500160627
047600160627       ENDSR;
