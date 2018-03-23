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
004100170517     FCNIND00F  IF   E           K DISK    usropn
004200170215     FTFNTC01L  IF   E           K DISK    usropn
004300170314     FTNTBE01L  IF   E           K DISK    extfile(wLibFileTBE)  usropn
004400170314     F                                     extdesc('GAITRAGRPS/TNTBE01L')
004500091223       //--------------------------------------------------------------
004600121106       // Definizione costanti.
004700091223       //--------------------------------------------------------------
004800100302
004900091223
005000091223       //--------------------------------------------------------------
005100121106       // Definizione schiere.
005200091223       //--------------------------------------------------------------
005300091223
005400091223
005500091223       //--------------------------------------------------------------
005600121106       // Definizione aree dati.
005700091223       //--------------------------------------------------------------
005800091223
005900091223
006000140724       //--------------------------------------------------------------
006100140724       // Definizione strutture dati.
006200140724       //--------------------------------------------------------------
006300140724
006400121106       // - Status
006500110516     d Psds           sds
006600110516     d   SDSpgm          *proc
006700121106     d   JobName             244    253                                         Job name
006800121106     d   JobUser             254    263                                         User name
006900121106     d   JobNumber           264    269s 0                                      Job number
007000170216
007100170216       // Tabella di passaggio dati
007200170216     d UBRSMALDS     e ds                  qualified
007300170216
007400170216       // Tabella CLI
007500170216     d dCLI          e ds                  inz
007600170315
007700170315       // Ricerca/Controllo tabelle (TNTBE)?
007800170315     d TIBS02ds      e ds                  inz
007900170315     d   T02mod      e                     inz('C')
008000170315     d   T02cod      e                     inz('CLI')
008100170315     d   T02ke1      e                     inz
008200170315     d   T02ke2      e                     inz
008300170216
008400170216       // - Parametri in input?
008500170216     d KPJBA         e ds
008600170216
008700091223       //--------------------------------------------------------------
008800121106       // Definizione variabili.
008900091223       //--------------------------------------------------------------
009000121106
009100121106       // Campi di comodo
009200160218       // Nome della libreria in cui è il file FNBLP00F
009300160218     d wLibFileBLP     s             21a   inz
009400160218       // Nome della libreria in cui è il file FNARB00F
009500160218     d wLibFileARB     s             21a   inz
009600170315       // Nome della libreria in cui è il file TNTBE00F
009700170315     d wLibFileTBE     s             21a   inz
009800160218       // Nome del sistema
009900160218     d currSysNeta     s              8a   inz
010000170216       // Cliente
010100170216     d wCliente        s              7s 0 inz
010200091223
010300091223       //--------------------------------------------------------------
010400121106       // Definizione prototipi procedure.
010500091223       //--------------------------------------------------------------
010600170315
010700170316      /copy gaitrasrc/srcProtoPR,TIBS02R
010800160218
010900170215      /copy gaitrasrc/srcProtoPI,UBRSMALR
011000170216
011100170216       // Reperimento NETA sistema AS/400 corrente
011200170216      /copy gaitrasrc/srcProtoPr,UBRTVNETA
011300130307
011400091223       //--------------------------------------------------------------
011500121106       // Definizione key-list.
011600091223       //--------------------------------------------------------------
011700091223
011800170216       // - TFNTC01L
011900170216     d k04tFNTC01    e ds                  extname(TFNTC01L : *key)
012000170216     d                                     prefix(k_)   inz
012100091223
012200091223       //--------------------------------------------------------------
012300121106       // M A I N - L I N E
012400091223       //--------------------------------------------------------------
012500140806
012600130307       // Operazioni iniziali?
012700130307       exsr sr_RoutInz;
012800130307
012900170216       // Esecuzione
013000170216       exsr esegui;
013100091223
013200121106       // Operazioni finali?
013300091223       exsr sr_RoutEnd;
013400091223
013500091223       //--------------------------------------------------------------
013600121107       // Operazioni iniziali.
013700091223       //--------------------------------------------------------------
013800091223       BEGSR  sr_RoutInz;
013900170215
014000170216         UBRSMALDS = UBRSMALR_DS;
014100170216
014200170215         // Controllo i parametri di input
014300170215         exsr sr_CtrlPIn;
014400160218
014500170215         // Verifica del sistema AS/400 corrente
014600170215         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
014700170215           // esco dando errore
014800170216           UBRSMALDS.oEsito = -1;
014900170215           exsr sr_RoutEnd;
015000170215         endif;
015100160218
015200170215         // Apertura archivi
015300170215         if %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
015400170215           wLibFileBLP = 'FILTRA201/FNBLP01L';
015500170215           wLibFileARB = 'FILTRA201/FNARB01L';
015600170315           wLibFileTBE = 'FILTRAGRU/TNTBE01L';
015700170215         else;
015800170215           wLibFileBLP = 'FILTRAPRD/FNBLP01L';
015900170215           wLibFileARB = 'FILTRAPRD/FNARB01L';
016000170314           wLibFileTBE = 'FILTRAGRPF/TNTBE01L';
016100170215         endif;
016200160218
016300170315         if  not %open(TNTBE01L);
016400170315           open  TNTBE01L;
016500170315         endif;
016600170215         if  not %open(TFNTC01L);
016700170215           open  TFNTC01L;
016800170215         endif;
016900160218
017000160218         // Inizializzazione variabili
017100170216         wCliente = 0;
017200170216         UBRSMALDS.oEsito = 0;
017300170216         clear UBRSMALDS.oRagSoc;
017400160202
017500160202       ENDSR;
017600160202
017700160202       //--------------------------------------------------------------
017800160202       // Controllo i parametri di input
017900160202       //--------------------------------------------------------------
018000160202       BEGSR  sr_CtrlPIn;
018100140724
018200170215         select;
018300170216          when (UBRSMALDS.iTpRic <> 'E' and
018400170216                UBRSMALDS.iTpRic <> 'S' and
018500170216                UBRSMALDS.iTpRic <> 'T')    or
018600170216               (UBRSMALDS.iOrigine <> 'P' and
018700170216                UBRSMALDS.iOrigine <> 'A' and
018800170216                UBRSMALDS.iOrigine <> 'S')   or
018900170216               UBRSMALDS.iAAS = 0            or
019000170216               UBRSMALDS.iLNP = 0            or
019100170216               UBRSMALDS.iNSP = 0 ;
019200170215           // esco dando errore
019300170216           UBRSMALDS.oEsito = -3;
019400170215           exsr sr_RoutEnd;
019500170215         endsl;
019600091223
019700091223       ENDSR;
019800121106
019900091223       //--------------------------------------------------------------
020000160217       // Esecuzione
020100091223       //--------------------------------------------------------------
020200160217       BEGSR  esegui;
020300140724
020400170215         select;
020500170215          // PARTENZA
020600170216          when UBRSMALDS.iOrigine = 'P';
020700170315           if  not %open(FNBLP01L);
020800170315             open  FNBLP01L;
020900170315           endif;
021000170215           // reperisco partenza
021100170216           chain(n) (UBRSMALDS.iAAS:UBRSMALDS.iLNP:
021200170216                     UBRSMALDS.iNRS:UBRSMALDS.iNSP)  FNBLP000;
021300170215           if not %found(fnblp01l);
021400170215             // esco dando errore
021500170216             UBRSMALDS.oEsito = -2;
021600170215             exsr sr_RoutEnd;
021700170215           endif;
021800170215          // ARRIVO
021900170216          when UBRSMALDS.iOrigine = 'A';
022000170315           if  not %open(FNARB01L);
022100170315             open  FNARB01L;
022200170315           endif;
022300170315           // reperisco arrivo
022400170216           chain(n) (UBRSMALDS.iAAS:UBRSMALDS.iLNP:
022500170216                     UBRSMALDS.iNRS:UBRSMALDS.iNSP)  FNARB000;
022600170215           if not %found(fnarb01l);
022700170215             // esco dando errore
022800170216             UBRSMALDS.oEsito = -2;
022900170215             exsr sr_RoutEnd;
023000170215           endif;
023100170315          // SEDE
023200170315          when UBRSMALDS.iOrigine = 'S';
023300170315           if  not %open(TITAS30C);
023400170315             open  TITAS30C;
023500170315           endif;
023600170315           if  not %open(TITAA30C);
023700170315             open  TITAA30C;
023800170315           endif;
023900170315           // reperisco sede
024000170315           chain(n) (UBRSMALDS.iAAS:UBRSMALDS.iLNP:
024100170315                     UBRSMALDS.iNRS:UBRSMALDS.iNSP)  TITAS30C;
024200170315           if not %found(titas30c);
024300170315             // esco dando errore
024400170315             UBRSMALDS.oEsito = -2;
024500170315             exsr sr_RoutEnd;
024600170315           endif;
024700170215         endsl;
024800160217
024900170215         // determino il codice cliente
025000170215         select;
025100170215          // PARTENZA
025200170216          when UBRSMALDS.iOrigine = 'P';
025300170216           if BLPCCM > 0;
025400170216             wCliente = BLPCCM;
025500170216           else;
025600170216             wCliente = BLPKSC;
025700170216           endif;
025800170215          // ARRIVO
025900170216          when UBRSMALDS.iOrigine = 'A';
026000170216           if ARBCCM > 0;
026100170216             wCliente = ARBCCM;
026200170216           else;
026300170216             wCliente = ARBKSC;
026400170216           endif;
026500170315          // SEDE
026600170315          when UBRSMALDS.iOrigine = 'S';
026700170315           if %subst(%editc(TASCCM:'X'):4:4) = '0000' OR
026800170315              %subst(%editc(TASCCM:'X'):4:4) = '8888' OR
026900170315              %subst(%editc(TASCCM:'X'):4:4) = '9999';
027000170315             wCliente = TASKSC;
027100170315           else;
027200170315             wCliente = TASCCM;
027300170315           endif;
027400170215         endsl;
027500170216
027600170315         // reperisco la tab. CLI per il cod.clente definito
027700170316         //clear kpjbu;
027800170316         ////T02mod = 'C';              (già così)?
027900170316         ////T02cod = 'CLI';            (già così)?
028000170316         //T02ke1 = %editc(wCliente:'X');
028100170316         //clear T02ke2;
028200170316         ////T02sif = knsif;            (non serve perché quasta è una tab. solo sede)
028300170316         //TNTBE_RicercaControllo ( kpjba : tibs02ds );
028400170316         //if T02err <>*blank;
028500170316         //  T02uni = T02Uni;
028600170316         //endif;
028700170315
028800170216         // reperisco la tab. CLI per il cod.clente definito
028900170314         chain ('CLI':%editc(wCliente:'X')) tntbe01l;
029000170314         if %found(tntbe01l);
029100170314           DCLI = tbeUNI;
029200170216         else;
029300170216           // la tabella CLI non esiste per tutti i clienti, quindi se non la trovo la azzero
029400170216           clear DCLI;
029500170216         endif;
029600170216
029700170216         // se per il cliente va forzato il mittente originale
029800170216         if §CLIMITOR = 'S';
029900170216           select;
030000170216            // PARTENZA
030100170216            when UBRSMALDS.iOrigine = 'P';
030200170216             if BLPRMO <> *blank;
030300170216               UBRSMALDS.oRagSoc  = BLPRMO;
030400170522               UBRSMALDS.oInM     = BLPINM;
030500170522               UBRSMALDS.oCAM     = BLPCAM;
030600170522               UBRSMALDS.oLoM     = BLPLOM;
030700170522               UBRSMALDS.oPrM     = BLPPRM;
030800170522               UBRSMALDS.oNzM     = BLPNZM;
030900170216               // esco perché ho finito correttamente
031000170216               UBRSMALDS.oEsito = 0;
031100170216               exsr sr_RoutEnd;
031200170216             endif;
031300170216            // ARRIVO
031400170216            when UBRSMALDS.iOrigine = 'A';
031500170216             if ARBRMO <> *blank;
031600170216               UBRSMALDS.oRagSoc  = ARBRMO;
031700170522               UBRSMALDS.oInM     = ARBINM;
031800170522               UBRSMALDS.oCAM     = ARBCAM;
031900170522               UBRSMALDS.oLoM     = ARBLOM;
032000170522               UBRSMALDS.oPrM     = ARBPRM;
032100170522               UBRSMALDS.oNzM     = ARBNZM;
032200170216               // esco perché ho finito correttamente
032300170216               UBRSMALDS.oEsito = 0;
032400170216               exsr sr_RoutEnd;
032500170216             endif;
032600170315            // SEDE
032700170315            when UBRSMALDS.iOrigine = 'S';
032800170315             // reperisco anagrafica bolle di sede
032900170315             chain(n) (UBRSMALDS.iAAS:UBRSMALDS.iLNP:
033000170315                       UBRSMALDS.iNRS:UBRSMALDS.iNSP:'O')  TITAA30C;
033100170315             if not %found(titaa30c);
033200170315               // esco dando errore
033300170315               UBRSMALDS.oEsito = -2;
033400170315               exsr sr_RoutEnd;
033500170315             else;
033600170315               // se la ragione sociale è valorizzata, uso questa
033700170315               if TAARSC <> *blank;
033800170315                 UBRSMALDS.oRagSoc = TAARSC;
033900170517                 UBRSMALDS.oInM     = TAAIND;
034000170517                 UBRSMALDS.oCAM     = TAACAP;
034100170517                 UBRSMALDS.oLoM     = TAALOC;
034200170517                 UBRSMALDS.oPrM     = TAAPRV;
034300170522                 UBRSMALDS.oNzM     = TAANAZ;
034400170315               endif;
034500170315             endif;
034600170216           endsl;
034700170216         endif;
034800170216
034900170315         // se per il cliente va forzato il mittente da nota AM/AS
035000170315         if §CLITFNTC = 'S'
035100170315         // e non ho già recuperato la ragione sociale
035200170315         and UBRSMALDS.oRagSoc = *blank;
035300170216           select;
035400170216            // SMS
035500170216            when UBRSMALDS.iTpRic = 'S';
035600170216             k_NTCAPL =  'C';
035700170216             k_NTCNK1 =  '0151' + %editc(wCliente:'X');
035800170216             k_NTCNK2 =  *BLANK;
035900170216             k_NTCTNT =  'AS';
036000170216             chain(n)  %kds(k04tFNTC01)  TFNTC;
036100170216             if %found();
036200170216               UBRSMALDS.oRagSoc = NTCRNT;
036300170517               // siccome su TFNTC non ci sono questi dati, li pulisco
036400170517               UBRSMALDS.oInM     = *blank;
036500170517               UBRSMALDS.oCAM     = *blank;
036600170517               UBRSMALDS.oLoM     = *blank;
036700170517               UBRSMALDS.oPrM     = *blank;
036800170517               UBRSMALDS.oNzM     = *blank;
036900170216             endif;
037000170216            // Email o T&T
037100170216            when UBRSMALDS.iTpRic = 'E' or
037200170216                 UBRSMALDS.iTpRic = 'T';
037300170216             k_NTCAPL =  'C';
037400170216             k_NTCNK1 =  '0151' + %editc(wCliente:'X');
037500170216             k_NTCNK2 =  *BLANK;
037600170216             k_NTCTNT =  'AM';
037700170216             chain(n)  %kds(k04tFNTC01)  TFNTC;
037800170216             if %found();
037900170216               UBRSMALDS.oRagSoc = NTCRNT;
038000170517               // siccome su TFNTC non ci sono questi dati, li pulisco
038100170517               UBRSMALDS.oInM     = *blank;
038200170517               UBRSMALDS.oCAM     = *blank;
038300170517               UBRSMALDS.oLoM     = *blank;
038400170517               UBRSMALDS.oPrM     = *blank;
038500170517               UBRSMALDS.oNzM     = *blank;
038600170216             endif;
038700170216           endsl;
038800170216         endif;
038900170216
039000170216         // se flag precedenti non a 'S' oppure la ragione sociale è ancora blank
039100170216         if UBRSMALDS.oRagSoc = *blank;
039200170216           // imposto la ragione sociale con quella del mittente NON originale
039300170216           select;
039400170216            // PARTENZA
039500170216            when UBRSMALDS.iOrigine = 'P';
039600170216             UBRSMALDS.oRagSoc  = BLPRSM;
039700170517             UBRSMALDS.oInM     = BLPINM;
039800170517             UBRSMALDS.oCAM     = BLPCAM;
039900170517             UBRSMALDS.oLoM     = BLPLOM;
040000170517             UBRSMALDS.oPrM     = BLPPRM;
040100170517             UBRSMALDS.oNzM     = BLPNZM;
040200170216            // ARRIVO
040300170216            when UBRSMALDS.iOrigine = 'A';
040400170216             UBRSMALDS.oRagSoc  = ARBRSM;
040500170517             UBRSMALDS.oInM     = ARBINM;
040600170517             UBRSMALDS.oCAM     = ARBCAM;
040700170517             UBRSMALDS.oLoM     = ARBLOM;
040800170517             UBRSMALDS.oPrM     = ARBPRM;
040900170517             UBRSMALDS.oNzM     = ARBNZM;
041000170315            // SEDE
041100170315            when UBRSMALDS.iOrigine = 'S';
041200170315             if %subst(%editc(TASCCM:'X'):4:4) = '0000' OR
041300170315                %subst(%editc(TASCCM:'X'):4:4) = '8888' OR
041400170315                %subst(%editc(TASCCM:'X'):4:4) = '9999';
041500170315               // reperisco anagrafica bolle di sede
041600170315               chain(n) (UBRSMALDS.iAAS:UBRSMALDS.iLNP:
041700170315                         UBRSMALDS.iNRS:UBRSMALDS.iNSP:'M')  TITAA30C;
041800170315               if not %found(titaa30c);
041900170315                 // esco dando errore
042000170315                 UBRSMALDS.oEsito = -2;
042100170315                 exsr sr_RoutEnd;
042200170315               else;
042300170315                 UBRSMALDS.oRagSoc = TAARSC;
042400170517                 UBRSMALDS.oInM     = TAAIND;
042500170517                 UBRSMALDS.oCAM     = TAACAP;
042600170517                 UBRSMALDS.oLoM     = TAALOC;
042700170517                 UBRSMALDS.oPrM     = TAAPRV;
042800170522                 UBRSMALDS.oNzM     = TAANAZ;
042900170315               endif;
043000170315             else;
043100170315               if  not %open(CNACO00F);
043200170315                 open  CNACO00F;
043300170315               endif;
043400170522               if  not %open(CNIND00F);
043500170517                 open  CNIND00F;
043600170517               endif;
043700170315               // reperisco anagrafica
043800170315               chain(n) (1 : 151 : TASCCM)
043900170315                        CNACO00F;
044000170315               if not %found(cnaco00f);
044100170315                 // esco dando errore
044200170315                 UBRSMALDS.oEsito = -2;
044300170315                 exsr sr_RoutEnd;
044400170315               else;
044500170315                 UBRSMALDS.oRagSoc = ACORAG;
044600170517                 // in CNACO non ci sono i dati dell'indirizzo
044700170517                 // reperisco indirizzo in CNIND
044800170517                 chain(n) (1 : 151 : TASCCM)
044900170517                        CNIND00F;
045000170517                 if not %found(cnind00f);
045100170517                   // esco dando errore
045200170517                   UBRSMALDS.oEsito = -2;
045300170517                   exsr sr_RoutEnd;
045400170517                 else;
045500170517                   UBRSMALDS.oInM     = INDVIA;
045600170522                   UBRSMALDS.oCAM     = %char(INDCAP);
045700170517                   UBRSMALDS.oLoM     = INDCIT;
045800170517                   UBRSMALDS.oPrM     = INDPRV;
045900170517                   UBRSMALDS.oNzM     = INDSTA;
046000170517                 endif;
046100170517               endif;
046200170315             endif;
046300170216           endsl;
046400170216         endif;
046500170216
046600170315         // se la ragione sociale è ancora blank
046700170315         if UBRSMALDS.oRagSoc = *blank;
046800170315           // esco dando errore
046900170315           UBRSMALDS.oEsito = -2;
047000170315         // altrimenti tutto ok
047100170315         else;
047200170315           UBRSMALDS.oEsito = 0   ;
047300170315         endif;
047400160217
047500160217       ENDSR;
047600160217
047700160217       //--------------------------------------------------------------
047800160217       // Operazioni finali.
047900160217       //--------------------------------------------------------------
048000160217       BEGSR  sr_RoutEnd;
048100170216
048200170216         UBRSMALR_DS = UBRSMALDS;
048300160217
048400170215         *inrt = *on;
048500140724         return;
048600160627
048700160627       ENDSR;
