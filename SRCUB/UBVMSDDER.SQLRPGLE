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
013800160901         pogg = 'BARTVAS - Anomalia Mappatura in TIVMS00F';
013900160901
014000160901         // Inizializzo risultato a OK
014100160921         oVMSDDEEsito = '1';
014200160608
014300160608         // Controllo i parametri di input
014400160608         exsr sr_CtrlPIn;
014500160202
014600160202       ENDSR;
014700160202
014800160202       //--------------------------------------------------------------
014900160202       // Controllo i parametri di input
015000160202       //--------------------------------------------------------------
015100160202       BEGSR  sr_CtrlPIn;
015200160901
015300160901         // Lancio deve essere blank o 'C'
015400160921         if iVMSDDEOpz <> *blank and iVMSDDEOpz <>'C';
015500160901           // esco con errore sui parametri
015600160921           oVMSDDEEsito = 'I';
015700160901           exsr sr_RoutEnd;
015800160901         endif;
015900160901
016000160901         // Procedura deve essere <> blank
016100160921         if iVMSDDEProc = *blank;
016200160901           // esco con errore sui parametri
016300160921           oVMSDDEEsito = 'I';
016400160901           exsr sr_RoutEnd;
016500160901         endif;
016600160901
016700160915         // la data di riferimento deve essere valorizzata
016800160915         // se non valorizzata, la imposto a oggi
016900160921         if iVMSDDEDtRif = 0;
017000160921           iVMSDDEDtRif = %dec(%date());
017100160901         endif;
017200091223
017300091223       ENDSR;
017400121106
017500091223       //--------------------------------------------------------------
017600160217       // Esecuzione
017700091223       //--------------------------------------------------------------
017800160217       BEGSR  esegui;
017900160531
018000160922         wSQL = 'select vmsdde, vmsproc, vmsksu +
018100160901                 from tivms00f +
018200160901                 where +
018300160922                 vmsdde <= ' + %char(iVMSDDEDtRif) + ' and +
018400160921                 vmsproc = ''' + iVMSDDEProc + ''' and +
018500160921                 vmsksu  = ''' + iVMSDDEKSU  + ''' and +
018600160921                 vmsdsc >= ' + %char(iVMSDDEDtRif) + ' and +
018700160901                 vmsatb = '' ''' +
018800160922                ' group by vmsdde, vmsproc, vmsksu +
018900160901                  for read only';
019000160601
019100160531         // Dichiarazione cursore
019200160531         exec sql   prepare S0   from :wSQL;
019300160531         exec sql   declare C0   cursor   for S0;
019400160531         // Apro il cursore
019500160531         exec sql open C0;
019600160901         if sqlCode < 0;
019700160901           // esco con errore SQL
019800160921           oVMSDDEEsito = '2';
019900160901           exsr sr_RoutEnd;
020000160901         endif;
020100160531         // Leggo i dati
020200160531         exec sql
020300160901             fetch next from C0 into :DS_res;
020400160901
020500160531         select;
020600160531          // nessun rcd trovato
020700160531          when sqlCode = 100;
020800160901            // esco con errore tipo 3 (non trovato periodo)
020900160921            oVMSDDEEsito = '3';
021000160531            exec sql close C0;
021100160531            exsr sr_RoutEnd;
021200160531          // errore SQL
021300160531          when sqlCode < 0;
021400160531            exec sql close C0;
021500160901            // esco con errore SQL
021600160921            oVMSDDEEsito = '2';
021700160531            exsr sr_RoutEnd;
021800160901          // c'è il rcd, allora mi salvo la decorrenza
021900160901          other;
022000160921            oVMSDDEDDE = DDE;
022100160531         endsl;
022200160531
022300160901         // poi provo a rileggere i dati perché se c'è una seconda riga c'è un errore
022400160901         exec sql
022500160901             fetch next from C0 into :DS_res;
022600160901         select;
022700160901          when sqlCode = 100;
022800160901            // va bene!
022900160901          // trovato un'altro rcd
023000160901          when sqlCode = 0 or sqlCode > 0 and sqlCode <> 100;
023100160901            // esco con errore tipo 4 (trovati più periodi)
023200160921            oVMSDDEEsito = '4';
023300160901            exec sql close C0;
023400160901            exsr sr_RoutEnd;
023500160901          // nessun rcd trovato
023600160901          // errore SQL (impossibile perché l'avrei avuto prima)
023700160901          when sqlCode < 0;
023800160901            exec sql close C0;
023900160901            // esco con errore SQL
024000160921            oVMSDDEEsito = '2';
024100160901            exsr sr_RoutEnd;
024200160901         endsl;
024300160901
024400160901         // Quindi chiudo il cursore
024500160531         exec sql close C0;
024600160531
024700160531         // Se sono qui tutto OK
024800160921         oVMSDDEEsito = '1';
024900160217
025000160217       ENDSR;
025100160601
025200160601       //--------------------------------------------------------------
025300160601       // Operazioni finali.
025400160601       //--------------------------------------------------------------
025500160601       BEGSR  sr_RoutEnd;
025600160601
025700160601         // Chiusura pgm
025800160921         if iVMSDDEOpz = 'C';
025900160601           *inlr = *on;
026000160601         else;
026100160901
026200160901           // se c'è un errore
026300160921           if oVMSDDEEsito <>'1';
026400160921             oVMSDDEDDE = 0;
026500160901             // preparo il msg a seconda dell'errore (se 3 o 4)
026600160901             select;
026700160921              when oVMSDDEEsito ='3';
026800160901               pMsgV =
026900160901               ':/N' +
027000160901               'Non trovato periodo di mappatura per la data di riferimento: ' +
027100160921               %char(iVMSDDEDtRif) +
027200160901               ':/N' +
027300160921               'e Procedura: ' + iVMSDDEProc +
027400160901               ':/N' +
027500160921               'e Cliente Unificante: ' + iVMSDDEKSU +
027600160901               ':/N' +
027700160901               'L''SQL con cui si sono estratti questi dati è:' +
027800160901               ':/N' +
027900160901               wSQL ;
028000160901               // lo aggiungo nella email
028100160901               exsr AggEmail;
028200160901               // spedisco segnalazione
028300160901               exsr  sr_SendEmail;
028400160901
028500160921              when oVMSDDEEsito ='4';
028600160901               pMsgV =
028700160901               ':/N' +
028800160901               'Trovati più periodi di mappatura per la data di riferimento: ' +
028900160921               %char(iVMSDDEDtRif) +
029000160901               ':/N' +
029100160921               'e Procedura: ' + iVMSDDEProc +
029200160901               ':/N' +
029300160921               'e Cliente Unificante: ' + iVMSDDEKSU +
029400160901               ':/N' +
029500160901               'L''SQL con cui si sono estratti questi dati è:' +
029600160901               ':/N' +
029700160901               wSQL ;
029800160901               // lo aggiungo nella email
029900160901               exsr AggEmail;
030000160901               // spedisco segnalazione
030100160901               exsr  sr_SendEmail;
030200160901             endsl;
030300160901           endif;
030400160901
030500160601           *inrt = *on;
030600160601         endif;
030700160601         return;
030800160601
030900160601       ENDSR;
031000160901
031100160901       //--------------------------------------------------------------
031200160901       //?Invio e-mail di avviso errore.                               ?
031300160901       //--------------------------------------------------------------
031400160901       BEGSR  sr_SendEmail;
031500160901
031600160901         pmsg = pMsgV;
031700160901         // se sono qui, qualcosa ho scritto
031800160901         exsr EmailInvia;
031900160901
032000160901       ENDSR;
032100091223
032200091223      /end-free
032300160901
032400160901     C*------------------------------------------------------------------------*
032500160901     C* AggEmail: aggiunge rcd all'e-mail
032600160901     C*------------------------------------------------------------------------*
032700160901     C     AggEmail      BEGSR
032800160901     C*
032900160901     C* questa routine non fa niente perché pMsgV è già stato valorizzato prima
033000160901     C*
033100160901     C                   ENDSR
033200160901     C*------------------------------------------------------------------------*
033300160901     C* EmailInvia - Invio e-mail di conferma forzatura
033400160901     C*------------------------------------------------------------------------*
033500160901     C     EmailInvia    BEGSR
033600160901     C*
033700160901     C* invio e-mail
033800160901     C                   call      'TIS701C1'
033900160901     C                   parm                    peml            253
034000160901     C                   parm                    pcceml          253
034100160901     C                   parm                    pogg             44
034200160901     C                   parm                    pmsg           5000
034300160901     C                   parm                    pesito            1
034400160901     C*
034500160901     C                   ENDSR
