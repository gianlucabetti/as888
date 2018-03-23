000100091228       //==============================================================
000200160901       // Driver per ottenere data decorrenza diuna versione
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
001400160901     ** 1. Creare il modulo UBVMSDDER (Opz 15 PDM)
001500160901     ** 2. Creare/Aggiornare il programma di servizio UBVMSDDER (CRTSRVPGM / UPDSRVPGM)
001600160601     **    con ACTGRP(*caller), la BNDDIR non è importante tanto non chiama niente
001700160901     ** 3. Creare/Aggiornare il programma UBVMSDDER (CRTPGM / UPDPGM)
001800160601     **    con ACTGRP(*caller), la BNDDIR non è importante tanto non chiama niente
001900160202     ** 4. Cancellare il modulo.
002000160202     **
002100091228
002200091228       //--------------------------------------------------------------
002300121106       // Specifiche di controllo.
002400091228       //--------------------------------------------------------------
002500091223
002600091223     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002700160601     h alwnull(*inputonly)
002800091223
002900091223       //--------------------------------------------------------------
003000121106       // Dichiarazione file.
003100091223       //--------------------------------------------------------------
003200160218
003300160531
003400091223       //--------------------------------------------------------------
003500121106       // Definizione costanti.
003600091223       //--------------------------------------------------------------
003700100302
003800091223
003900091223       //--------------------------------------------------------------
004000121106       // Definizione schiere.
004100091223       //--------------------------------------------------------------
004200091223
004300091223
004400091223       //--------------------------------------------------------------
004500121106       // Definizione aree dati.
004600091223       //--------------------------------------------------------------
004700091223
004800091223
004900140724       //--------------------------------------------------------------
005000140724       // Definizione strutture dati.
005100140724       //--------------------------------------------------------------
005200140724
005300121106       // - Status
005400110516     d Psds           sds
005500110516     d   SDSpgm          *proc
005600121106     d   JobName             244    253                                         Job name
005700121106     d   JobUser             254    263                                         User name
005800121106     d   JobNumber           264    269s 0                                      Job number
005900091223
006000091223       //--------------------------------------------------------------
006100121106       // Definizione variabili.
006200091223       //--------------------------------------------------------------
006300121106
006400121106       // Campi di comodo
006500160531     d wSQL            s           2048    inz  varying
006600160901     d DS_res          ds
006700160922     d  DDE                           8s 0
006800160901     d  Proc                         10
006900160901     d  KSU                           8
007000160901     D pMsgV           s           5000    inz varying
007100091223
007200091223       //--------------------------------------------------------------
007300160603       // Definizione costanti.
007400091223       //--------------------------------------------------------------
007500130307
007600160531
007700160603       //--------------------------------------------------------------
007800160603       // Definizione prototipi procedure.
007900160603       //--------------------------------------------------------------
008000160603
008100160901       /copy gaitrasrc/srcProtoPI,UBVMSDDER
008200160922       /copy gaitrasrc/srcProtoPR,UBVMSDDER
008300160603
008400091223       //--------------------------------------------------------------
008500121106       // Definizione key-list.
008600091223       //--------------------------------------------------------------
008700091223
008800091223
008900091223       //--------------------------------------------------------------
009000121107       // Definizione parametri procedura.
009100091223       //--------------------------------------------------------------
009200091223
009300091223     c     *Entry        plist
009400160921     c                   parm                    iVMSDDEOpz
009500160921     c                   parm                    iVMSDDEProc
009600160921     c                   parm                    iVMSDDEKSU
009700160921     c                   parm                    iVMSDDEDtRif
009800160921     c                   parm                    oVMSDDEDDE
009900160921     c                   parm                    oVMSDDEEsito
010000091223
010100130307      /free
010200091223
010300091223       //--------------------------------------------------------------
010400121106       // M A I N - L I N E
010500091223       //--------------------------------------------------------------
010600140806
010700130307       // Operazioni iniziali?
010800130307       exsr sr_RoutInz;
010900130307
011000140724       // attività richiesta dal chiamante se non chiamato per chiusura
011100160921       if iVMSDDEOpz = *blank;
011200160217         exsr esegui;
011300140724       endif;
011400091223
011500121106       // Operazioni finali?
011600091223       exsr sr_RoutEnd;
011700160601
011800160601       //--------------------------------------------------------------
011900160601       //
012000160601       //--------------------------------------------------------------
012100160601       BEGSR  *inzsr;
012200160601
012300160601         // Inizializzazioni parametri esecuzione comandi sql
012400160601         exec sql  set option  dynusrprf = *owner, closqlcsr = *endmod;
012500160601
012600160601       ENDSR;
012700091223
012800091223       //--------------------------------------------------------------
012900121107       // Operazioni iniziali.
013000091223       //--------------------------------------------------------------
013100091223       BEGSR  sr_RoutInz;
013200160901
013300160901         // Inizializzazione parametri di output
013400160921         clear oVMSDDEDDE     ;
013500160901
013600160901         // utilizzo indirizzi email di default
013700160926         peml = 'cedalert@brt.it';
013800160926         pcceml = 'fabrizio.gurrieri@brt.it';
013900160901         pogg = 'BARTVAS - Anomalia Mappatura in TIVMS00F';
014000160901
014100160901         // Inizializzo risultato a OK
014200160921         oVMSDDEEsito = '1';
014300160608
014400160608         // Controllo i parametri di input
014500160608         exsr sr_CtrlPIn;
014600160202
014700160202       ENDSR;
014800160202
014900160202       //--------------------------------------------------------------
015000160202       // Controllo i parametri di input
015100160202       //--------------------------------------------------------------
015200160202       BEGSR  sr_CtrlPIn;
015300160901
015400160901         // Lancio deve essere blank o 'C'
015500160921         if iVMSDDEOpz <> *blank and iVMSDDEOpz <>'C';
015600160901           // esco con errore sui parametri
015700160921           oVMSDDEEsito = 'I';
015800160901           exsr sr_RoutEnd;
015900160901         endif;
016000160901
016100160901         // Procedura deve essere <> blank
016200160921         if iVMSDDEProc = *blank;
016300160901           // esco con errore sui parametri
016400160921           oVMSDDEEsito = 'I';
016500160901           exsr sr_RoutEnd;
016600160901         endif;
016700160901
016800160915         // la data di riferimento deve essere valorizzata
016900160915         // se non valorizzata, la imposto a oggi
017000160921         if iVMSDDEDtRif = 0;
017100160921           iVMSDDEDtRif = %dec(%date());
017200160901         endif;
017300091223
017400091223       ENDSR;
017500121106
017600091223       //--------------------------------------------------------------
017700160217       // Esecuzione
017800091223       //--------------------------------------------------------------
017900160217       BEGSR  esegui;
018000160531
018100160922         wSQL = 'select vmsdde, vmsproc, vmsksu +
018200160901                 from tivms00f +
018300160901                 where +
018400160922                 vmsdde <= ' + %char(iVMSDDEDtRif) + ' and +
018500160921                 vmsproc = ''' + iVMSDDEProc + ''' and +
018600160921                 vmsksu  = ''' + iVMSDDEKSU  + ''' and +
018700160921                 vmsdsc >= ' + %char(iVMSDDEDtRif) + ' and +
018800160901                 vmsatb = '' ''' +
018900160922                ' group by vmsdde, vmsproc, vmsksu +
019000160901                  for read only';
019100160601
019200160531         // Dichiarazione cursore
019300160531         exec sql   prepare S0   from :wSQL;
019400160531         exec sql   declare C0   cursor   for S0;
019500160531         // Apro il cursore
019600160531         exec sql open C0;
019700160901         if sqlCode < 0;
019800160901           // esco con errore SQL
019900160921           oVMSDDEEsito = '2';
020000160901           exsr sr_RoutEnd;
020100160901         endif;
020200160531         // Leggo i dati
020300160531         exec sql
020400160901             fetch next from C0 into :DS_res;
020500160901
020600160531         select;
020700160531          // nessun rcd trovato
020800160531          when sqlCode = 100;
020900160901            // esco con errore tipo 3 (non trovato periodo)
021000160921            oVMSDDEEsito = '3';
021100160531            exec sql close C0;
021200160531            exsr sr_RoutEnd;
021300160531          // errore SQL
021400160531          when sqlCode < 0;
021500160531            exec sql close C0;
021600160901            // esco con errore SQL
021700160921            oVMSDDEEsito = '2';
021800160531            exsr sr_RoutEnd;
021900160901          // c'è il rcd, allora mi salvo la decorrenza
022000160901          other;
022100160921            oVMSDDEDDE = DDE;
022200160531         endsl;
022300160531
022400160901         // poi provo a rileggere i dati perché se c'è una seconda riga c'è un errore
022500160901         exec sql
022600160901             fetch next from C0 into :DS_res;
022700160901         select;
022800160901          when sqlCode = 100;
022900160901            // va bene!
023000160901          // trovato un'altro rcd
023100160901          when sqlCode = 0 or sqlCode > 0 and sqlCode <> 100;
023200160901            // esco con errore tipo 4 (trovati più periodi)
023300160921            oVMSDDEEsito = '4';
023400160901            exec sql close C0;
023500160901            exsr sr_RoutEnd;
023600160901          // nessun rcd trovato
023700160901          // errore SQL (impossibile perché l'avrei avuto prima)
023800160901          when sqlCode < 0;
023900160901            exec sql close C0;
024000160901            // esco con errore SQL
024100160921            oVMSDDEEsito = '2';
024200160901            exsr sr_RoutEnd;
024300160901         endsl;
024400160901
024500160901         // Quindi chiudo il cursore
024600160531         exec sql close C0;
024700160531
024800160531         // Se sono qui tutto OK
024900160921         oVMSDDEEsito = '1';
025000160217
025100160217       ENDSR;
025200160601
025300160601       //--------------------------------------------------------------
025400160601       // Operazioni finali.
025500160601       //--------------------------------------------------------------
025600160601       BEGSR  sr_RoutEnd;
025700160601
025800160601         // Chiusura pgm
025900160921         if iVMSDDEOpz = 'C';
026000160601           *inlr = *on;
026100160601         else;
026200160901
026300160901           // se c'è un errore
026400160921           if oVMSDDEEsito <>'1';
026500160921             oVMSDDEDDE = 0;
026600160901             // preparo il msg a seconda dell'errore (se 3 o 4)
026700160901             select;
026800160921              when oVMSDDEEsito ='3';
026900160901               pMsgV =
027000160901               ':/N' +
027100160901               'Non trovato periodo di mappatura per la data di riferimento: ' +
027200160921               %char(iVMSDDEDtRif) +
027300160901               ':/N' +
027400160921               'e Procedura: ' + iVMSDDEProc +
027500160901               ':/N' +
027600160921               'e Cliente Unificante: ' + iVMSDDEKSU +
027700160901               ':/N' +
027800160901               'L''SQL con cui si sono estratti questi dati è:' +
027900160901               ':/N' +
028000160901               wSQL ;
028100160901               // lo aggiungo nella email
028200160901               exsr AggEmail;
028300160901               // spedisco segnalazione
028400160901               exsr  sr_SendEmail;
028500160901
028600160921              when oVMSDDEEsito ='4';
028700160901               pMsgV =
028800160901               ':/N' +
028900160901               'Trovati più periodi di mappatura per la data di riferimento: ' +
029000160921               %char(iVMSDDEDtRif) +
029100160901               ':/N' +
029200160921               'e Procedura: ' + iVMSDDEProc +
029300160901               ':/N' +
029400160921               'e Cliente Unificante: ' + iVMSDDEKSU +
029500160901               ':/N' +
029600160901               'L''SQL con cui si sono estratti questi dati è:' +
029700160901               ':/N' +
029800160901               wSQL ;
029900160901               // lo aggiungo nella email
030000160901               exsr AggEmail;
030100160901               // spedisco segnalazione
030200160901               exsr  sr_SendEmail;
030300160901             endsl;
030400160901           endif;
030500160901
030600160601           *inrt = *on;
030700160601         endif;
030800160601         return;
030900160601
031000160601       ENDSR;
031100160901
031200160901       //--------------------------------------------------------------
031300160901       //?Invio e-mail di avviso errore.                               ?
031400160901       //--------------------------------------------------------------
031500160901       BEGSR  sr_SendEmail;
031600160901
031700160901         pmsg = pMsgV;
031800160901         // se sono qui, qualcosa ho scritto
031900160901         exsr EmailInvia;
032000160901
032100160901       ENDSR;
032200091223
032300091223      /end-free
032400160901
032500160901     C*------------------------------------------------------------------------*
032600160901     C* AggEmail: aggiunge rcd all'e-mail
032700160901     C*------------------------------------------------------------------------*
032800160901     C     AggEmail      BEGSR
032900160901     C*
033000160901     C* questa routine non fa niente perché pMsgV è già stato valorizzato prima
033100160901     C*
033200160901     C                   ENDSR
033300160901     C*------------------------------------------------------------------------*
033400160901     C* EmailInvia - Invio e-mail di conferma forzatura
033500160901     C*------------------------------------------------------------------------*
033600160901     C     EmailInvia    BEGSR
033700160901     C*
033800160901     C* invio e-mail
033900160901     C                   call      'TIS701C1'
034000160901     C                   parm                    peml            253
034100160901     C                   parm                    pcceml          253
034200160901     C                   parm                    pogg             44
034300160901     C                   parm                    pmsg           5000
034400160901     C                   parm                    pesito            1
034500160901     C*
034600160901     C                   ENDSR
