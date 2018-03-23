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
018700170216                UBRSMALDS.iTpRic <> 'T')    or
018800170216               (UBRSMALDS.iOrigine <> 'P' and
018900170216                UBRSMALDS.iOrigine <> 'A' and
019000170216                UBRSMALDS.iOrigine <> 'S')   or
019100170216               UBRSMALDS.iAAS = 0            or
019200170216               UBRSMALDS.iLNP = 0            or
019300170216               UBRSMALDS.iNSP = 0 ;
019400170215           // esco dando errore
019500170216           UBRSMALDS.oEsito = -3;
019600170215           exsr sr_RoutEnd;
019700170215         endsl;
019800091223
019900091223       ENDSR;
020000121106
020100091223       //--------------------------------------------------------------
020200160217       // Esecuzione
020300091223       //--------------------------------------------------------------
020400160217       BEGSR  esegui;
020500140724
020600170215         select;
020700170215          // PARTENZA
020800170216          when UBRSMALDS.iOrigine = 'P';
020900170315           if  not %open(FNBLP01L);
021000170315             open  FNBLP01L;
021100170315           endif;
021200170215           // reperisco partenza
021300170216           chain(n) (UBRSMALDS.iAAS:UBRSMALDS.iLNP:
021400170216                     UBRSMALDS.iNRS:UBRSMALDS.iNSP)  FNBLP000;
021500170215           if not %found(fnblp01l);
021600170215             // esco dando errore
021700170216             UBRSMALDS.oEsito = -2;
021800170215             exsr sr_RoutEnd;
021900170215           endif;
022000170215          // ARRIVO
022100170216          when UBRSMALDS.iOrigine = 'A';
022200170315           if  not %open(FNARB01L);
022300170315             open  FNARB01L;
022400170315           endif;
022500170315           // reperisco arrivo
022600170216           chain(n) (UBRSMALDS.iAAS:UBRSMALDS.iLNP:
022700170216                     UBRSMALDS.iNRS:UBRSMALDS.iNSP)  FNARB000;
022800170215           if not %found(fnarb01l);
022900170215             // esco dando errore
023000170216             UBRSMALDS.oEsito = -2;
023100170215             exsr sr_RoutEnd;
023200170215           endif;
023300170315          // SEDE
023400170315          when UBRSMALDS.iOrigine = 'S';
023500170315           if  not %open(TITAS30C);
023600170315             open  TITAS30C;
023700170315           endif;
023800170315           if  not %open(TITAA30C);
023900170315             open  TITAA30C;
024000170315           endif;
024100170315           // reperisco sede
024200170315           chain(n) (UBRSMALDS.iAAS:UBRSMALDS.iLNP:
024300170315                     UBRSMALDS.iNRS:UBRSMALDS.iNSP)  TITAS30C;
024400170315           if not %found(titas30c);
024500170315             // esco dando errore
024600170315             UBRSMALDS.oEsito = -2;
024700170315             exsr sr_RoutEnd;
024800170315           endif;
024900170215         endsl;
025000160217
025100170215         // determino il codice cliente
025200170215         select;
025300170215          // PARTENZA
025400170216          when UBRSMALDS.iOrigine = 'P';
025500170216           if BLPCCM > 0;
025600170216             wCliente = BLPCCM;
025700170216           else;
025800170216             wCliente = BLPKSC;
025900170216           endif;
026000170215          // ARRIVO
026100170216          when UBRSMALDS.iOrigine = 'A';
026200170216           if ARBCCM > 0;
026300170216             wCliente = ARBCCM;
026400170216           else;
026500170216             wCliente = ARBKSC;
026600170216           endif;
026700170315          // SEDE
026800170315          when UBRSMALDS.iOrigine = 'S';
026900170315           if %subst(%editc(TASCCM:'X'):4:4) = '0000' OR
027000170315              %subst(%editc(TASCCM:'X'):4:4) = '8888' OR
027100170315              %subst(%editc(TASCCM:'X'):4:4) = '9999';
027200170315             wCliente = TASKSC;
027300170315           else;
027400170315             wCliente = TASCCM;
027500170315           endif;
027600170215         endsl;
027700170315
027800170216         // reperisco la tab. CLI per il cod.clente definito
027900170314         chain ('CLI':%editc(wCliente:'X')) tntbe01l;
028000170314         if %found(tntbe01l);
028100170314           DCLI = tbeUNI;
028200170216         else;
028300170216           // la tabella CLI non esiste per tutti i clienti, quindi se non la trovo la azzero
028400170216           clear DCLI;
028500170216         endif;
028600170710
028700170710         // valorizzo quindi i dati relativi al indirizzo
028800170710         select;
028900170710          // PARTENZA
029000170710          when UBRSMALDS.iOrigine = 'P';
029100170710               UBRSMALDS.oInM     = BLPINM;
029200170710               UBRSMALDS.oCAM     = BLPCAM;
029300170710               UBRSMALDS.oLoM     = BLPLOM;
029400170710               UBRSMALDS.oPrM     = BLPPRM;
029500170710               UBRSMALDS.oNzM     = BLPNZM;
029600170710          // ARRIVO
029700170710          when UBRSMALDS.iOrigine = 'A';
029800170710               UBRSMALDS.oInM     = ARBINM;
029900170710               UBRSMALDS.oCAM     = ARBCAM;
030000170710               UBRSMALDS.oLoM     = ARBLOM;
030100170710               UBRSMALDS.oPrM     = ARBPRM;
030200170710               UBRSMALDS.oNzM     = ARBNZM;
030300170710          // SEDE
030400170710          when UBRSMALDS.iOrigine = 'S';
030500170710             if %subst(%editc(TASCCM:'X'):4:4) = '0000' OR
030600170710                %subst(%editc(TASCCM:'X'):4:4) = '8888' OR
030700170710                %subst(%editc(TASCCM:'X'):4:4) = '9999';
030800170710               // reperisco anagrafica bolle di sede
030900170710               chain(n) (UBRSMALDS.iAAS:UBRSMALDS.iLNP:
031000170710                         UBRSMALDS.iNRS:UBRSMALDS.iNSP:'M')  TITAA30C;
031100170710               if %found(titaa30c);
031200170710                  UBRSMALDS.oInM     = TAAIND;
031300170710                  UBRSMALDS.oCAM     = TAACAP;
031400170710                  UBRSMALDS.oLoM     = TAALOC;
031500170710                  UBRSMALDS.oPrM     = TAAPRV;
031600170710                  UBRSMALDS.oNzM     = TAANAZ;
031700170710               endif;
031800170710             else;
031900170710               if  not %open(CNIND00F);
032000170710                 open  CNIND00F;
032100170710               endif;
032200170710               // reperisco indirizzo in CNIND
032300170710               chain(n) (1 : 151 : TASCCM) CNIND00F;
032400170710               if %found(cnind00f);
032500170710                  UBRSMALDS.oInM     = INDVIA;
032600170710                  UBRSMALDS.oCAM     = %char(INDCAP);
032700170710                  UBRSMALDS.oLoM     = INDCIT;
032800170710                  UBRSMALDS.oPrM     = INDPRV;
032900170710                  UBRSMALDS.oNzM     = INDSTA;
033000170710               endif;
033100170710             endif;
033200170710         endsl;
033300170216
033400170216         // se per il cliente va forzato il mittente originale
033500170216         if §CLIMITOR = 'S';
033600170216           select;
033700170216            // PARTENZA
033800170216            when UBRSMALDS.iOrigine = 'P';
033900170216             if BLPRMO <> *blank;
034000170216               UBRSMALDS.oRagSoc  = BLPRMO;
034100170216               // esco perché ho finito correttamente
034200170216               UBRSMALDS.oEsito = 0;
034300170216               exsr sr_RoutEnd;
034400170216             endif;
034500170216            // ARRIVO
034600170216            when UBRSMALDS.iOrigine = 'A';
034700170216             if ARBRMO <> *blank;
034800170216               UBRSMALDS.oRagSoc  = ARBRMO;
034900170216               // esco perché ho finito correttamente
035000170216               UBRSMALDS.oEsito = 0;
035100170216               exsr sr_RoutEnd;
035200170216             endif;
035300170315            // SEDE
035400170315            when UBRSMALDS.iOrigine = 'S';
035500170710             // reperisco anagrafica bolle di sede
035600170710             chain(n) (UBRSMALDS.iAAS:UBRSMALDS.iLNP:
035700170710                       UBRSMALDS.iNRS:UBRSMALDS.iNSP:'O')  TITAA30C;
035800170710             if %found(titaa30c);
035900170710                // se la ragione sociale è valorizzata, uso questa
036000170710                if TAARSC <> *blank;
036100170710                   UBRSMALDS.oRagSoc = TAARSC;
036200170710                  // esco perché ho finito correttamente
036300170710                  UBRSMALDS.oEsito = 0;
036400170710                  exsr sr_RoutEnd;
036500170710                endif;
036600170710             endif;
036700170216           endsl;
036800170216         endif;
036900170216
037000170315         // se per il cliente va forzato il mittente da nota AM/AS
037100170315         if §CLITFNTC = 'S'
037200170710         // e non ho già recuperato la ragione sociale
037300170710         and UBRSMALDS.oRagSoc = *blank;
037400170216           select;
037500170216            // SMS
037600170216            when UBRSMALDS.iTpRic = 'S';
037700170216             k_NTCAPL =  'C';
037800170216             k_NTCNK1 =  '0151' + %editc(wCliente:'X');
037900170216             k_NTCNK2 =  *BLANK;
038000170216             k_NTCTNT =  'AS';
038100170216             chain(n)  %kds(k04tFNTC01)  TFNTC;
038200170216             if %found();
038300170216               UBRSMALDS.oRagSoc = NTCRNT;
038400170216             endif;
038500170710
038600170216            // Email o T&T
038700170216            when UBRSMALDS.iTpRic = 'E' or
038800170710                 UBRSMALDS.iTpRic = 'T' or
038900170710                 UBRSMALDS.iTpRic = 'L';
039000170216             k_NTCAPL =  'C';
039100170216             k_NTCNK1 =  '0151' + %editc(wCliente:'X');
039200170216             k_NTCNK2 =  *BLANK;
039300170216             k_NTCTNT =  'AM';
039400170216             chain(n)  %kds(k04tFNTC01)  TFNTC;
039500170216             if %found();
039600170216               UBRSMALDS.oRagSoc = NTCRNT;
039700170216             endif;
039800170216           endsl;
039900170216         endif;
040000170216
040100170216         // se flag precedenti non a 'S' oppure la ragione sociale è ancora blank
040200170216         if UBRSMALDS.oRagSoc = *blank;
040300170710           // imposto la ragione sociale con quella del mittente (NON originale)
040400170216           select;
040500170216            // PARTENZA
040600170216            when UBRSMALDS.iOrigine = 'P';
040700170216             UBRSMALDS.oRagSoc  = BLPRSM;
040800170216            // ARRIVO
040900170216            when UBRSMALDS.iOrigine = 'A';
041000170216             UBRSMALDS.oRagSoc  = ARBRSM;
041100170315            // SEDE
041200170710            when UBRSMALDS.iOrigine = 'S';
041300170710             if %subst(%editc(TASCCM:'X'):4:4) = '0000' OR
041400170315                %subst(%editc(TASCCM:'X'):4:4) = '8888' OR
041500170315                %subst(%editc(TASCCM:'X'):4:4) = '9999';
041600170315               // reperisco anagrafica bolle di sede
041700170315               chain(n) (UBRSMALDS.iAAS:UBRSMALDS.iLNP:
041800170315                         UBRSMALDS.iNRS:UBRSMALDS.iNSP:'M')  TITAA30C;
041900170315               if not %found(titaa30c);
042000170315                 // esco dando errore
042100170315                 UBRSMALDS.oEsito = -2;
042200170315                 exsr sr_RoutEnd;
042300170315               else;
042400170710                 UBRSMALDS.oRagSoc  = TAARSC;
042500170315               endif;
042600170315             else;
042700170315               if  not %open(CNACO00F);
042800170315                 open  CNACO00F;
042900170315               endif;
043000170315               // reperisco anagrafica
043100170710               chain(n) (1 : 151 : TASCCM) CNACO00F;
043200170315               if not %found(cnaco00f);
043300170315                 // esco dando errore
043400170315                 UBRSMALDS.oEsito = -2;
043500170315                 exsr sr_RoutEnd;
043600170315               else;
043700170315                 UBRSMALDS.oRagSoc = ACORAG;
043800170517               endif;
043900170315             endif;
044000170216           endsl;
044100170216         endif;
044200170216
044300170315         // se la ragione sociale è ancora blank
044400170315         if UBRSMALDS.oRagSoc = *blank;
044500170315           // esco dando errore
044600170315           UBRSMALDS.oEsito = -2;
044700170315         // altrimenti tutto ok
044800170315         else;
044900170315           UBRSMALDS.oEsito = 0   ;
045000170315         endif;
045100160217
045200160217       ENDSR;
045300160217
045400160217       //--------------------------------------------------------------
045500160217       // Operazioni finali.
045600160217       //--------------------------------------------------------------
045700170710       BEGSR  sr_RoutEnd;
045800170216
045900170710         // Se richiesto esplicitamente nascondere dati indirizzo mittente => pulisco
046000170710         if §CLINOIMTT = 'S'       or
046100170710            UBRSMALDS.iTpRic = 'T' or
046200170710            UBRSMALDS.iTpRic = 'L';
046300170710            clear UBRSMALDS.oInM;
046400170710            clear UBRSMALDS.oCAM;
046500170710            clear UBRSMALDS.oLoM;
046600170710            clear UBRSMALDS.oPrM;
046700170710            clear UBRSMALDS.oNzM;
046800170710         endif;
046900170710
047000170216         UBRSMALR_DS = UBRSMALDS;
047100160217
047200170215         *inrt = *on;
047300140724         return;
047400160627
047500160627       ENDSR;
