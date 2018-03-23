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
003800170315     FTITAS30C  IF   E           K DISK    usropn
003900170315     FTITAA30C  IF   E           K DISK    usropn
004000170315     FCNACO00F  IF   E           K DISK    usropn
004100170215     FTFNTC01L  IF   E           K DISK    usropn
004200170314     FTNTBE01L  IF   E           K DISK    extfile(wLibFileTBE)  usropn
004300170314     F                                     extdesc('GAITRAGRPS/TNTBE01L')
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
007500170315
007600170315       // Ricerca/Controllo tabelle (TNTBE)?
007700170315     d TIBS02ds      e ds                  inz
007800170315     d   T02mod      e                     inz('C')
007900170315     d   T02cod      e                     inz('CLI')
008000170315     d   T02ke1      e                     inz
008100170315     d   T02ke2      e                     inz
008200170216
008300170216       // - Parametri in input?
008400170216     d KPJBA         e ds
008500170216
008600091223       //--------------------------------------------------------------
008700121106       // Definizione variabili.
008800091223       //--------------------------------------------------------------
008900121106
009000121106       // Campi di comodo
009100160218       // Nome della libreria in cui è il file FNBLP00F
009200160218     d wLibFileBLP     s             21a   inz
009300160218       // Nome della libreria in cui è il file FNARB00F
009400160218     d wLibFileARB     s             21a   inz
009500170315       // Nome della libreria in cui è il file TNTBE00F
009600170315     d wLibFileTBE     s             21a   inz
009700160218       // Nome del sistema
009800160218     d currSysNeta     s              8a   inz
009900170216       // Cliente
010000170216     d wCliente        s              7s 0 inz
010100091223
010200091223       //--------------------------------------------------------------
010300121106       // Definizione prototipi procedure.
010400091223       //--------------------------------------------------------------
010500170315
010600170316      /copy gaitrasrc/srcProtoPR,TIBS02R
010700160218
010800170215      /copy gaitrasrc/srcProtoPI,UBRSMALR
010900170216
011000170216       // Reperimento NETA sistema AS/400 corrente
011100170216      /copy gaitrasrc/srcProtoPr,UBRTVNETA
011200130307
011300091223       //--------------------------------------------------------------
011400121106       // Definizione key-list.
011500091223       //--------------------------------------------------------------
011600091223
011700170216       // - TFNTC01L
011800170216     d k04tFNTC01    e ds                  extname(TFNTC01L : *key)
011900170216     d                                     prefix(k_)   inz
012000091223
012100091223       //--------------------------------------------------------------
012200121106       // M A I N - L I N E
012300091223       //--------------------------------------------------------------
012400140806
012500130307       // Operazioni iniziali?
012600130307       exsr sr_RoutInz;
012700130307
012800170216       // Esecuzione
012900170216       exsr esegui;
013000091223
013100121106       // Operazioni finali?
013200091223       exsr sr_RoutEnd;
013300091223
013400091223       //--------------------------------------------------------------
013500121107       // Operazioni iniziali.
013600091223       //--------------------------------------------------------------
013700091223       BEGSR  sr_RoutInz;
013800170215
013900170216         UBRSMALDS = UBRSMALR_DS;
014000170216
014100170215         // Controllo i parametri di input
014200170215         exsr sr_CtrlPIn;
014300160218
014400170215         // Verifica del sistema AS/400 corrente
014500170215         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
014600170215           // esco dando errore
014700170216           UBRSMALDS.oEsito = -1;
014800170215           exsr sr_RoutEnd;
014900170215         endif;
015000160218
015100170215         // Apertura archivi
015200170215         if %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
015300170215           wLibFileBLP = 'FILTRA201/FNBLP01L';
015400170215           wLibFileARB = 'FILTRA201/FNARB01L';
015500170315           wLibFileTBE = 'FILTRAGRU/TNTBE01L';
015600170215         else;
015700170215           wLibFileBLP = 'FILTRAPRD/FNBLP01L';
015800170215           wLibFileARB = 'FILTRAPRD/FNARB01L';
015900170314           wLibFileTBE = 'FILTRAGRPF/TNTBE01L';
016000170215         endif;
016100160218
016200170315         if  not %open(TNTBE01L);
016300170315           open  TNTBE01L;
016400170315         endif;
016500170215         if  not %open(TFNTC01L);
016600170215           open  TFNTC01L;
016700170215         endif;
016800160218
016900160218         // Inizializzazione variabili
017000170216         wCliente = 0;
017100170216         UBRSMALDS.oEsito = 0;
017200170216         clear UBRSMALDS.oRagSoc;
017300160202
017400160202       ENDSR;
017500160202
017600160202       //--------------------------------------------------------------
017700160202       // Controllo i parametri di input
017800160202       //--------------------------------------------------------------
017900160202       BEGSR  sr_CtrlPIn;
018000140724
018100170215         select;
018200170216          when (UBRSMALDS.iTpRic <> 'E' and
018300170216                UBRSMALDS.iTpRic <> 'S' and
018400170216                UBRSMALDS.iTpRic <> 'T')    or
018500170216               (UBRSMALDS.iOrigine <> 'P' and
018600170216                UBRSMALDS.iOrigine <> 'A' and
018700170216                UBRSMALDS.iOrigine <> 'S')   or
018800170216               UBRSMALDS.iAAS = 0            or
018900170216               UBRSMALDS.iLNP = 0            or
019000170216               UBRSMALDS.iNSP = 0 ;
019100170215           // esco dando errore
019200170216           UBRSMALDS.oEsito = -3;
019300170215           exsr sr_RoutEnd;
019400170215         endsl;
019500091223
019600091223       ENDSR;
019700121106
019800091223       //--------------------------------------------------------------
019900160217       // Esecuzione
020000091223       //--------------------------------------------------------------
020100160217       BEGSR  esegui;
020200140724
020300170215         select;
020400170215          // PARTENZA
020500170216          when UBRSMALDS.iOrigine = 'P';
020600170315           if  not %open(FNBLP01L);
020700170315             open  FNBLP01L;
020800170315           endif;
020900170215           // reperisco partenza
021000170216           chain(n) (UBRSMALDS.iAAS:UBRSMALDS.iLNP:
021100170216                     UBRSMALDS.iNRS:UBRSMALDS.iNSP)  FNBLP000;
021200170215           if not %found(fnblp01l);
021300170215             // esco dando errore
021400170216             UBRSMALDS.oEsito = -2;
021500170215             exsr sr_RoutEnd;
021600170215           endif;
021700170215          // ARRIVO
021800170216          when UBRSMALDS.iOrigine = 'A';
021900170315           if  not %open(FNARB01L);
022000170315             open  FNARB01L;
022100170315           endif;
022200170315           // reperisco arrivo
022300170216           chain(n) (UBRSMALDS.iAAS:UBRSMALDS.iLNP:
022400170216                     UBRSMALDS.iNRS:UBRSMALDS.iNSP)  FNARB000;
022500170215           if not %found(fnarb01l);
022600170215             // esco dando errore
022700170216             UBRSMALDS.oEsito = -2;
022800170215             exsr sr_RoutEnd;
022900170215           endif;
023000170315          // SEDE
023100170315          when UBRSMALDS.iOrigine = 'S';
023200170315           if  not %open(TITAS30C);
023300170315             open  TITAS30C;
023400170315           endif;
023500170315           if  not %open(TITAA30C);
023600170315             open  TITAA30C;
023700170315           endif;
023800170315           // reperisco sede
023900170315           chain(n) (UBRSMALDS.iAAS:UBRSMALDS.iLNP:
024000170315                     UBRSMALDS.iNRS:UBRSMALDS.iNSP)  TITAS30C;
024100170315           if not %found(titas30c);
024200170315             // esco dando errore
024300170315             UBRSMALDS.oEsito = -2;
024400170315             exsr sr_RoutEnd;
024500170315           endif;
024600170215         endsl;
024700160217
024800170215         // determino il codice cliente
024900170215         select;
025000170215          // PARTENZA
025100170216          when UBRSMALDS.iOrigine = 'P';
025200170216           if BLPCCM > 0;
025300170216             wCliente = BLPCCM;
025400170216           else;
025500170216             wCliente = BLPKSC;
025600170216           endif;
025700170215          // ARRIVO
025800170216          when UBRSMALDS.iOrigine = 'A';
025900170216           if ARBCCM > 0;
026000170216             wCliente = ARBCCM;
026100170216           else;
026200170216             wCliente = ARBKSC;
026300170216           endif;
026400170315          // SEDE
026500170315          when UBRSMALDS.iOrigine = 'S';
026600170315           if %subst(%editc(TASCCM:'X'):4:4) = '0000' OR
026700170315              %subst(%editc(TASCCM:'X'):4:4) = '8888' OR
026800170315              %subst(%editc(TASCCM:'X'):4:4) = '9999';
026900170315             wCliente = TASKSC;
027000170315           else;
027100170315             wCliente = TASCCM;
027200170315           endif;
027300170215         endsl;
027400170216
027500170315         // reperisco la tab. CLI per il cod.clente definito
027600170316         //clear kpjbu;
027700170316         ////T02mod = 'C';              (già così)?
027800170316         ////T02cod = 'CLI';            (già così)?
027900170316         //T02ke1 = %editc(wCliente:'X');
028000170316         //clear T02ke2;
028100170316         ////T02sif = knsif;            (non serve perché quasta è una tab. solo sede)
028200170316         //TNTBE_RicercaControllo ( kpjba : tibs02ds );
028300170316         //if T02err <>*blank;
028400170316         //  T02uni = T02Uni;
028500170316         //endif;
028600170315
028700170216         // reperisco la tab. CLI per il cod.clente definito
028800170314         chain ('CLI':%editc(wCliente:'X')) tntbe01l;
028900170314         if %found(tntbe01l);
029000170314           DCLI = tbeUNI;
029100170216         else;
029200170216           // la tabella CLI non esiste per tutti i clienti, quindi se non la trovo la azzero
029300170216           clear DCLI;
029400170216         endif;
029500170216
029600170216         // se per il cliente va forzato il mittente originale
029700170216         if §CLIMITOR = 'S';
029800170216           select;
029900170216            // PARTENZA
030000170216            when UBRSMALDS.iOrigine = 'P';
030100170216             if BLPRMO <> *blank;
030200170216               UBRSMALDS.oRagSoc  = BLPRMO;
030300170216               // esco perché ho finito correttamente
030400170216               UBRSMALDS.oEsito = 0;
030500170216               exsr sr_RoutEnd;
030600170216             endif;
030700170216            // ARRIVO
030800170216            when UBRSMALDS.iOrigine = 'A';
030900170216             if ARBRMO <> *blank;
031000170216               UBRSMALDS.oRagSoc  = ARBRMO;
031100170216               // esco perché ho finito correttamente
031200170216               UBRSMALDS.oEsito = 0;
031300170216               exsr sr_RoutEnd;
031400170216             endif;
031500170315            // SEDE
031600170315            when UBRSMALDS.iOrigine = 'S';
031700170315             // reperisco anagrafica bolle di sede
031800170315             chain(n) (UBRSMALDS.iAAS:UBRSMALDS.iLNP:
031900170315                       UBRSMALDS.iNRS:UBRSMALDS.iNSP:'O')  TITAA30C;
032000170315             if not %found(titaa30c);
032100170315               // esco dando errore
032200170315               UBRSMALDS.oEsito = -2;
032300170315               exsr sr_RoutEnd;
032400170315             else;
032500170315               // se la ragione sociale è valorizzata, uso questa
032600170315               if TAARSC <> *blank;
032700170315                 UBRSMALDS.oRagSoc = TAARSC;
032800170315               endif;
032900170315             endif;
033000170216           endsl;
033100170216         endif;
033200170216
033300170315         // se per il cliente va forzato il mittente da nota AM/AS
033400170315         if §CLITFNTC = 'S'
033500170315         // e non ho già recuperato la ragione sociale
033600170315         and UBRSMALDS.oRagSoc = *blank;
033700170216           select;
033800170216            // SMS
033900170216            when UBRSMALDS.iTpRic = 'S';
034000170216             k_NTCAPL =  'C';
034100170216             k_NTCNK1 =  '0151' + %editc(wCliente:'X');
034200170216             k_NTCNK2 =  *BLANK;
034300170216             k_NTCTNT =  'AS';
034400170216             chain(n)  %kds(k04tFNTC01)  TFNTC;
034500170216             if %found();
034600170216               UBRSMALDS.oRagSoc = NTCRNT;
034700170216             endif;
034800170216            // Email o T&T
034900170216            when UBRSMALDS.iTpRic = 'E' or
035000170216                 UBRSMALDS.iTpRic = 'T';
035100170216             k_NTCAPL =  'C';
035200170216             k_NTCNK1 =  '0151' + %editc(wCliente:'X');
035300170216             k_NTCNK2 =  *BLANK;
035400170216             k_NTCTNT =  'AM';
035500170216             chain(n)  %kds(k04tFNTC01)  TFNTC;
035600170216             if %found();
035700170216               UBRSMALDS.oRagSoc = NTCRNT;
035800170216             endif;
035900170216           endsl;
036000170216         endif;
036100170216
036200170216         // se flag precedenti non a 'S' oppure la ragione sociale è ancora blank
036300170216         if UBRSMALDS.oRagSoc = *blank;
036400170216           // imposto la ragione sociale con quella del mittente NON originale
036500170216           select;
036600170216            // PARTENZA
036700170216            when UBRSMALDS.iOrigine = 'P';
036800170216             UBRSMALDS.oRagSoc  = BLPRSM;
036900170216            // ARRIVO
037000170216            when UBRSMALDS.iOrigine = 'A';
037100170216             UBRSMALDS.oRagSoc  = ARBRSM;
037200170315            // SEDE
037300170315            when UBRSMALDS.iOrigine = 'S';
037400170315             if %subst(%editc(TASCCM:'X'):4:4) = '0000' OR
037500170315                %subst(%editc(TASCCM:'X'):4:4) = '8888' OR
037600170315                %subst(%editc(TASCCM:'X'):4:4) = '9999';
037700170315               // reperisco anagrafica bolle di sede
037800170315               chain(n) (UBRSMALDS.iAAS:UBRSMALDS.iLNP:
037900170315                         UBRSMALDS.iNRS:UBRSMALDS.iNSP:'M')  TITAA30C;
038000170315               if not %found(titaa30c);
038100170315                 // esco dando errore
038200170315                 UBRSMALDS.oEsito = -2;
038300170315                 exsr sr_RoutEnd;
038400170315               else;
038500170315                 UBRSMALDS.oRagSoc = TAARSC;
038600170315               endif;
038700170315             else;
038800170315               if  not %open(CNACO00F);
038900170315                 open  CNACO00F;
039000170315               endif;
039100170315               // reperisco anagrafica
039200170315               chain(n) (1 : 151 : TASCCM)
039300170315                        CNACO00F;
039400170315               if not %found(cnaco00f);
039500170315                 // esco dando errore
039600170315                 UBRSMALDS.oEsito = -2;
039700170315                 exsr sr_RoutEnd;
039800170315               else;
039900170315                 UBRSMALDS.oRagSoc = ACORAG;
040000170315               endif;
040100170315             endif;
040200170216           endsl;
040300170216         endif;
040400170216
040500170315         // se la ragione sociale è ancora blank
040600170315         if UBRSMALDS.oRagSoc = *blank;
040700170315           // esco dando errore
040800170315           UBRSMALDS.oEsito = -2;
040900170315         // altrimenti tutto ok
041000170315         else;
041100170315           UBRSMALDS.oEsito = 0   ;
041200170315         endif;
041300160217
041400160217       ENDSR;
041500160217
041600160217       //--------------------------------------------------------------
041700160217       // Operazioni finali.
041800160217       //--------------------------------------------------------------
041900160217       BEGSR  sr_RoutEnd;
042000170216
042100170216         UBRSMALR_DS = UBRSMALDS;
042200160217
042300170215         *inrt = *on;
042400140724         return;
042500160627
042600160627       ENDSR;
