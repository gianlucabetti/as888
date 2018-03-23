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
001500170215     ** 2. Creare/Aggiornare il programma di servizio UBRSMALR (CRTSRVPGM / UPDSRVPGM)
001600170216     **    con ACTGRP(*caller) e BNDDIR('UBRTVNETA')
001700160218     ** 2.1   aggiungere nella BNDDIR(UBBNDDIR)
001800170215     ** 3. per chi lo usa e per lo scopo, non serve che sia anche PGM
001900160202     ** 4. Cancellare il modulo.
002000160202     **
002100091228
002200091228       //--------------------------------------------------------------
002300121106       // Specifiche di controllo.
002400091228       //--------------------------------------------------------------
002500091223
002600091223     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002700091223     h alwnull(*inputonly)
002800091223
002900091223       //--------------------------------------------------------------
003000121106       // Dichiarazione file.
003100091223       //--------------------------------------------------------------
003200160218
003300160219     FFNBLP01L  IF   E           K DISK    extfile(wLibFileBLP)  usropn
003400160219     F                                     extdesc('FILTRAPRD/FNBLP01L')
003500160218     FFNARB01L  IF   E           K DISK    extfile(wLibFileARB)  usropn
003600160219     F                                     extdesc('FILTRAPRD/FNARB01L')
003700170215     FTFNTC01L  IF   E           K DISK    usropn
003800091223       //--------------------------------------------------------------
003900121106       // Definizione costanti.
004000091223       //--------------------------------------------------------------
004100100302
004200091223
004300091223       //--------------------------------------------------------------
004400121106       // Definizione schiere.
004500091223       //--------------------------------------------------------------
004600091223
004700091223
004800091223       //--------------------------------------------------------------
004900121106       // Definizione aree dati.
005000091223       //--------------------------------------------------------------
005100091223
005200091223
005300140724       //--------------------------------------------------------------
005400140724       // Definizione strutture dati.
005500140724       //--------------------------------------------------------------
005600140724
005700121106       // - Status
005800110516     d Psds           sds
005900110516     d   SDSpgm          *proc
006000121106     d   JobName             244    253                                         Job name
006100121106     d   JobUser             254    263                                         User name
006200121106     d   JobNumber           264    269s 0                                      Job number
006300170216
006400170216       // Tabella di passaggio dati
006500170216     d UBRSMALDS     e ds                  qualified
006600170216
006700170216       // Tabella CLI
006800170216     d dCLI          e ds                  inz
006900091223
007000170216       // Ricerca/Controllo tabelle (TNTBE)?
007100170216     d TIBS02ds      e ds                  inz
007200170216     d   T02mod      e                     inz('C')
007300170216     d   T02cod      e                     inz('CLI')
007400170216     d   T02ke1      e                     inz
007500170216     d   T02ke2      e                     inz
007600170216
007700170216       // - Parametri in input?
007800170216     d KPJBA         e ds
007900170216
008000091223       //--------------------------------------------------------------
008100121106       // Definizione variabili.
008200091223       //--------------------------------------------------------------
008300121106
008400121106       // Campi di comodo
008500160218       // Nome della libreria in cui è il file FNBLP00F
008600160218     d wLibFileBLP     s             21a   inz
008700160218       // Nome della libreria in cui è il file FNARB00F
008800160218     d wLibFileARB     s             21a   inz
008900160218       // Nome del sistema
009000160218     d currSysNeta     s              8a   inz
009100170216       // Cliente
009200170216     d wCliente        s              7s 0 inz
009300091223
009400091223       //--------------------------------------------------------------
009500121106       // Definizione prototipi procedure.
009600091223       //--------------------------------------------------------------
009700160218
009800170215      /copy gaitrasrc/srcProtoPI,UBRSMALR
009900170216
010000170216      /copy gaitrasrc/srcProtoPR,TIBS02R
010100170216
010200170216       // Reperimento NETA sistema AS/400 corrente
010300170216      /copy gaitrasrc/srcProtoPr,UBRTVNETA
010400130307
010500091223       //--------------------------------------------------------------
010600121106       // Definizione key-list.
010700091223       //--------------------------------------------------------------
010800091223
010900170216       // - TFNTC01L
011000170216     d k04tFNTC01    e ds                  extname(TFNTC01L : *key)
011100170216     d                                     prefix(k_)   inz
011200091223
011300091223       //--------------------------------------------------------------
011400121106       // M A I N - L I N E
011500091223       //--------------------------------------------------------------
011600140806
011700130307       // Operazioni iniziali?
011800130307       exsr sr_RoutInz;
011900130307
012000170216       // Esecuzione
012100170216       exsr esegui;
012200091223
012300121106       // Operazioni finali?
012400091223       exsr sr_RoutEnd;
012500091223
012600091223       //--------------------------------------------------------------
012700121107       // Operazioni iniziali.
012800091223       //--------------------------------------------------------------
012900091223       BEGSR  sr_RoutInz;
013000170215
013100170216         UBRSMALDS = UBRSMALR_DS;
013200170216
013300170215         // Controllo i parametri di input
013400170215         exsr sr_CtrlPIn;
013500160218
013600170215         // Verifica del sistema AS/400 corrente
013700170215         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
013800170215           // esco dando errore
013900170216           UBRSMALDS.oEsito = -1;
014000170215           exsr sr_RoutEnd;
014100170215         endif;
014200160218
014300170215         // Apertura archivi
014400170215         if %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
014500170215           wLibFileBLP = 'FILTRA201/FNBLP01L';
014600170215           wLibFileARB = 'FILTRA201/FNARB01L';
014700170215         else;
014800170215           wLibFileBLP = 'FILTRAPRD/FNBLP01L';
014900170215           wLibFileARB = 'FILTRAPRD/FNARB01L';
015000170215         endif;
015100160218
015200170215         if  not %open(FNBLP01L);
015300170215           open  FNBLP01L;
015400170215         endif;
015500170215         if  not %open(FNARB01L);
015600170215           open  FNARB01L;
015700170215         endif;
015800170215         if  not %open(TFNTC01L);
015900170215           open  TFNTC01L;
016000170215         endif;
016100160218
016200160218         // Inizializzazione variabili
016300170216         wCliente = 0;
016400170216         UBRSMALDS.oEsito = 0;
016500170216         clear UBRSMALDS.oRagSoc;
016600160202
016700160202       ENDSR;
016800160202
016900160202       //--------------------------------------------------------------
017000160202       // Controllo i parametri di input
017100160202       //--------------------------------------------------------------
017200160202       BEGSR  sr_CtrlPIn;
017300140724
017400170215         select;
017500170216          when (UBRSMALDS.iTpRic <> 'E' and
017600170216                UBRSMALDS.iTpRic <> 'S' and
017700170216                UBRSMALDS.iTpRic <> 'T')    or
017800170216               (UBRSMALDS.iOrigine <> 'P' and
017900170216                UBRSMALDS.iOrigine <> 'A' and
018000170216                UBRSMALDS.iOrigine <> 'S')   or
018100170216               UBRSMALDS.iAAS = 0            or
018200170216               UBRSMALDS.iLNP = 0            or
018300170216               UBRSMALDS.iNSP = 0 ;
018400170215           // esco dando errore
018500170216           UBRSMALDS.oEsito = -3;
018600170215           exsr sr_RoutEnd;
018700170215         endsl;
018800091223
018900091223       ENDSR;
019000121106
019100091223       //--------------------------------------------------------------
019200160217       // Esecuzione
019300091223       //--------------------------------------------------------------
019400160217       BEGSR  esegui;
019500140724
019600170215         select;
019700170215          // PARTENZA
019800170216          when UBRSMALDS.iOrigine = 'P';
019900170215           // reperisco partenza
020000170216           chain(n) (UBRSMALDS.iAAS:UBRSMALDS.iLNP:
020100170216                     UBRSMALDS.iNRS:UBRSMALDS.iNSP)  FNBLP000;
020200170215           if not %found(fnblp01l);
020300170215             // esco dando errore
020400170216             UBRSMALDS.oEsito = -2;
020500170215             exsr sr_RoutEnd;
020600170215           endif;
020700170215          // ARRIVO
020800170216          when UBRSMALDS.iOrigine = 'A';
020900170215           // reperisco partenza
021000170216           chain(n) (UBRSMALDS.iAAS:UBRSMALDS.iLNP:
021100170216                     UBRSMALDS.iNRS:UBRSMALDS.iNSP)  FNARB000;
021200170215           if not %found(fnarb01l);
021300170215             // esco dando errore
021400170216             UBRSMALDS.oEsito = -2;
021500170215             exsr sr_RoutEnd;
021600170215           endif;
021700170215         endsl;
021800160217
021900170215         // determino il codice cliente
022000170215         select;
022100170215          // PARTENZA
022200170216          when UBRSMALDS.iOrigine = 'P';
022300170216           if BLPCCM > 0;
022400170216             wCliente = BLPCCM;
022500170216           else;
022600170216             wCliente = BLPKSC;
022700170216           endif;
022800170215          // ARRIVO
022900170216          when UBRSMALDS.iOrigine = 'A';
023000170216           if ARBCCM > 0;
023100170216             wCliente = ARBCCM;
023200170216           else;
023300170216             wCliente = ARBKSC;
023400170216           endif;
023500170215         endsl;
023600170216
023700170216         // reperisco la tab. CLI per il cod.clente definito
023800170216         clear kpjbu;
023900170216         //T02mod = 'C';              (già così)?
024000170216         //T02cod = 'CLI';            (già così)?
024100170216         T02ke1 = %editc(wCliente:'X');
024200170216         clear T02ke2;
024300170216         //T02sif = knsif;            (non serve perché quasta è una tab. solo sede)
024400170216         TNTBE_RicercaControllo ( kpjba : tibs02ds );
024500170216         if T02err = *blank;
024600170216           DCLI = T02uni;
024700170216         else;
024800170216           // la tabella CLI non esiste per tutti i clienti, quindi se non la trovo la azzero
024900170216           clear DCLI;
025000170216         endif;
025100170216
025200170216         // se per il cliente va forzato il mittente originale
025300170216         if §CLIMITOR = 'S';
025400170216           select;
025500170216            // PARTENZA
025600170216            when UBRSMALDS.iOrigine = 'P';
025700170216             if BLPRMO <> *blank;
025800170216               UBRSMALDS.oRagSoc  = BLPRMO;
025900170216               // esco perché ho finito correttamente
026000170216               UBRSMALDS.oEsito = 0;
026100170216               exsr sr_RoutEnd;
026200170216             endif;
026300170216            // ARRIVO
026400170216            when UBRSMALDS.iOrigine = 'A';
026500170216             if ARBRMO <> *blank;
026600170216               UBRSMALDS.oRagSoc  = ARBRMO;
026700170216               // esco perché ho finito correttamente
026800170216               UBRSMALDS.oEsito = 0;
026900170216               exsr sr_RoutEnd;
027000170216             endif;
027100170216           endsl;
027200170216         endif;
027300170216
027400170216         // se per il cliente non andava forzato il mittente originale
027500170216         // controlle se va forzato il mittente da nota AM/AS
027600170216         if §CLITFNTC = 'S';
027700170216           select;
027800170216            // SMS
027900170216            when UBRSMALDS.iTpRic = 'S';
028000170216             k_NTCAPL =  'C';
028100170216             k_NTCNK1 =  '0151' + %editc(wCliente:'X');
028200170216             k_NTCNK2 =  *BLANK;
028300170216             k_NTCTNT =  'AS';
028400170216             chain(n)  %kds(k04tFNTC01)  TFNTC;
028500170216             if %found();
028600170216               UBRSMALDS.oRagSoc = NTCRNT;
028700170216             endif;
028800170216            // Email o T&T
028900170216            when UBRSMALDS.iTpRic = 'E' or
029000170216                 UBRSMALDS.iTpRic = 'T';
029100170216             k_NTCAPL =  'C';
029200170216             k_NTCNK1 =  '0151' + %editc(wCliente:'X');
029300170216             k_NTCNK2 =  *BLANK;
029400170216             k_NTCTNT =  'AM';
029500170216             chain(n)  %kds(k04tFNTC01)  TFNTC;
029600170216             if %found();
029700170216               UBRSMALDS.oRagSoc = NTCRNT;
029800170216             endif;
029900170216           endsl;
030000170216         endif;
030100170216
030200170216         // se flag precedenti non a 'S' oppure la ragione sociale è ancora blank
030300170216         if UBRSMALDS.oRagSoc = *blank;
030400170216           // imposto la ragione sociale con quella del mittente NON originale
030500170216           select;
030600170216            // PARTENZA
030700170216            when UBRSMALDS.iOrigine = 'P';
030800170216             UBRSMALDS.oRagSoc  = BLPRSM;
030900170216            // ARRIVO
031000170216            when UBRSMALDS.iOrigine = 'A';
031100170216             UBRSMALDS.oRagSoc  = ARBRSM;
031200170216           endsl;
031300170216         endif;
031400170216
031500170216         UBRSMALDS.oEsito = 0   ;
031600160217
031700160217       ENDSR;
031800160217
031900160217       //--------------------------------------------------------------
032000160217       // Operazioni finali.
032100160217       //--------------------------------------------------------------
032200160217       BEGSR  sr_RoutEnd;
032300170216
032400170216         UBRSMALR_DS = UBRSMALDS;
032500160217
032600170215         *inrt = *on;
032700140724         return;
032800160627
032900160627       ENDSR;
