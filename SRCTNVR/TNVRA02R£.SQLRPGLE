000100111128       //==============================================================
000200120405       // TRULSGNTPR - Aggiorna FISGN da SETRAS ad AS888
000300120405       // Questa funzione serve per aggiornare e/o aggiungere dati al file FISGN00F in modo che la f
000400120405       // possa avere sempre una massa critica di dati di test.
000500120405       // Siccome POSITROVA, quando si porta sul suo DB i dati, elimina i rcd già stampati (SGNST2>0
000600120405       // vecchi di una settimana
000700120405       // per evitare che accada questo sui dati di test viene detto che
000800120405       // - il rcd è da stampare
000900120405       // - è del prossimo anno. Se mese/giorno sono il 29 febbraio,  per evitare errori lo imposto
001000120405       //==============================================================
001100111128
001200120404       // per compilare si deve avere in linea la lib. FILTRAPRD
001300120404       // OGNI volta che si compila ricordarsi:
001400120210       // - portare il pgm su SETRAS in una libreria che esista come nome anche su AS888
001500120210       // - creare su SETRAS il pacchetto SQL che risiederà su AS888:
001600120404       //   CRTSQLPKG PGM(TRULSGNTPR) RDB(AS888)
001700120209
001800111128       //--------------------------------------------------------------
001900120207       // Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
002000111128       //--------------------------------------------------------------
002100111128
002200111128     /*PRM  dbgview(*source)
002300111128     /*END
002400111128
002500111128       //--------------------------------------------------------------
002600120207       // Specifiche di controllo.                                     ?
002700111128       //--------------------------------------------------------------
002800111128
002900111128     h decedit('0,') datedit(*dmy/) option(*nodebugio)
003000111128     h dftactgrp(*no)
003100111128     h alwnull(*inputonly)
003200111128
003300111128       //--------------------------------------------------------------
003400120207       // Dichiarazione file.                                          ?
003500111128       //--------------------------------------------------------------
003600120413
003700120413
003800111128       //--------------------------------------------------------------
003900120207       // Definizione costanti.                                        ?
004000111128       //--------------------------------------------------------------
004100111128
004200120413
004300111128       //--------------------------------------------------------------
004400120207       // Definizione schiere.                                         ?
004500111128       //--------------------------------------------------------------
004600111128
004700111128
004800111128       //--------------------------------------------------------------
004900120207       // Definizione aree dati.                                       ?
005000111128       //--------------------------------------------------------------
005100111128
005200111128
005300111128       //--------------------------------------------------------------
005400120207       // Definizione strutture dati.                                  ?
005500111128       //--------------------------------------------------------------
005600120413     d FIARG00F      e ds
005700120413     d FNARB00F      e ds
005800120416     d DatiXStampa     ds
005900120413     d  AAS_non                            like(ARBAAS)
006000120413     d  LNP_non                            like(ARBLNP)
006100120413     d  NRS_non                            like(ARBNRS)
006200120413     d  NSP_non                            like(ARBNSP)
006300120413     d  AAS                                like(ARBAAS)
006400120413     d  LNP                                like(ARBLNP)
006500120413     d  NRS                                like(ARBNRS)
006600120413     d  NSP                                like(ARBNSP)
006700120413     d  TGI                                like(ARGTGI)
006800120413     d  CGI                                like(ARGCGI)
006900120413     d  IND                                like(ARBIND)
007000120413     d  CAD                                like(ARBCAD)
007100120413     d  LOD                                like(ARBLOD)
007200120413     d  PRD                                like(ARBPRD)
007300120413     d  Dt_Sped                       8s 0
007400111128
007500111128       //--------------------------------------------------------------
007600120207       // Definizione variabili globali.                               ?
007700111128       //--------------------------------------------------------------
007800111128
007900120208       // - Stringa SQL da eseguire
008000120413     d wSQl            s           5000    inz  varying
008100111128
008200120208       // - Parametri SQL
008300120207
008400120207       // - Flags booleani
008500120208     d ErrBlock        s               n   inz(*off)
008600120210     d Qcmd            s            500    inz
008700120207
008800120207       // - Variabili di appoggio
008900120405     d wX              s              5s 0
009000120413     d wDataDa         s              8s 0
009100120413     d wDataA          s              8s 0
009200120413     d wRighe          s              3s 0
009300111128
009400111128       //--------------------------------------------------------------
009500120210       // Definizione procedure usate
009600111128       //--------------------------------------------------------------
009700120207
009800120210       // - Parametri API QCAPCMD (Process Commands)
009900120207      /copy qSysInc/qRpgleSrc,QCAPCMD
010000120207      /copy gaitrasrc/srcProtoPR,QCAPCMD
010100120207
010200120210       // - Parametri gestione errori API
010300120207      /copy qSysInc/qRpgleSrc,QUSEC
010400120404
010500120413     d XXXX002R        pr
010600120413     d  in_DtRif                      8s 0
010700120413     d  out_Errore                    1
010800120404
010900120413     d XXXX002R        pi
011000120413     d  in_DtRif                      8s 0
011100120413     d  out_Errore                    1
011200111128
011300111128       //--------------------------------------------------------------
011400120208       // Definizione key-list.                                        ?
011500111128       //--------------------------------------------------------------
011600111128
011700111128
011800111128       //--------------------------------------------------------------
011900120208       // M A I N - L I N E                                            ?
012000111128       //--------------------------------------------------------------
012100111128
012200111128      /free
012300111128
012400120208       // - Operazioni iniziali
012500111128       exsr  sr_RoutInz;
012600111128
012700120207       // - Gestione video
012800120413       exsr  RepDati;
012900120207
013000120208       // - Operazioni finali
013100111128       exsr  sr_RoutEnd;
013200111128
013300111128       //--------------------------------------------------------------
013400120208       // Operazioni iniziali
013500111128       //--------------------------------------------------------------
013600111128       BEGSR sr_RoutInz;
013700111128
013800111128         *inLR = *on;
013900120413         out_Errore=*blank;
014000111128
014100120208         // - Impostazione opzioni per SQL
014200111128         exec SQL   set option   DynUsrPrf = *owner,
014300111128                                 CloSqlCsr = *endmod;
014400111128
014500120413         // il range delle date va da -8 a -1 della data in ingresso
014600120413         wDataDa=%dec(%date(in_DtRif)-%days(8));
014700120413         wDataA =%dec(%date(in_DtRif)-%days(1));
014800120413
014900111128       ENDSR;
015000111128
015100111128       //--------------------------------------------------------------
015200120208       // Apertura cursore C1
015300111128       //--------------------------------------------------------------
015400111128       BEGSR  sr_OpenCursor;
015500111128
015600120208         // - Dichiarazione del cursore
015700111128         exec sql  prepare S1  from :wSQL;
015800111128         exec sql  declare C1  cursor for S1;
015900111128
016000120208         // - Apertura del cursore
016100111128         exec sql   open C1;
016200111128
016300111128       ENDSR;
016400111128
016500111128       //--------------------------------------------------------------
016600120208       // Chiusura cursore C1
016700111128       //--------------------------------------------------------------
016800111128       BEGSR  sr_CloseCursor;
016900111128
017000120208         // - Chiusura del cursore
017100111128         exec sql   close C1;
017200111128
017300111128       ENDSR;
017400111128
017500111128       //--------------------------------------------------------------
017600120208       // Lettura cursore C1
017700111128       //--------------------------------------------------------------
017800111128       BEGSR  sr_ReadCursor;
017900111128
018000111128         select;
018100111128
018200120208           // - Fine lettura
018300111128           when  sqlCode = 100;
018400120413             //non devo fare nulla perché uscirò dal ciclo
018500120208           leavesr;
018600111128
018700120208           // - Errore
018800111128           when  sqlCode < *zero;
018900111128             exsr  sr_PrintErr;
019000120413             // errore sql: guarda la stampa
019100120413             out_Errore = '1';
019200120413             ErrBlock = *on;
019300120413             // - Chiusura cursore (visto che esco con leavesr)
019400120413             exsr  sr_CloseCursor;
019500120413             leavesr;
019600111128
019700120208           // - Elaborazione
019800111128           other;
019900120413             // stampo
020000120413             exsr Stampa;
020100111128
020200111128         endsl;
020300111128
020400111128       ENDSR;
020500120207
020600120207       //--------------------------------------------------------------
020700120413       // Reperimento ciclico dati
020800120207       //--------------------------------------------------------------
020900120413       BEGSR  RepDati;
021000120208
021100120208         ErrBlock = *off;
021200120208
021300120413         // - Reperisci dati
021400120208
021500120413         clear DatiXStampa;
021600120208         // - Creazione stringa SQL da eseguire
021700120413         wSQL = 'with '                                                        +
021800120413                'NON_ATTRIBUITE as '                                           +
021900120413                '(select * from filtra201/fnarb00f '                           +
022000120413                'join filtra201/fiarg00f on '                                  +
022100120413                'arbaas=argaas and arblnp=arglnp and '                         +
022200120413                'arbnrs=argnrs and arbnsp=argnsp '                             +
022300120413                'where arbdbr = ' + in_DtRif + ' and argtgi<>''A''), '         +
022400120413                 ' '                                                           +
022500120413                'ATTRIBUITE as '                                               +
022600120413                '(select * from filtra201/fnarb00f '                           +
022700120413                'join filtra201/fiarg00f on '                                  +
022800120413                'arbaas=argaas and arblnp=arglnp and '                         +
022900120413                'arbnrs=argnrs and arbnsp=argnsp '                             +
023000120413                'where arbdbr between ' + wDataDa + ' and ' + wDataA + ' '     +
023100120413                'and argtgi=''M'' and argcgi<>'' '') '                         +
023200120413                ' '                                                            +
023300120413                'select '                                                      +
023400120413                'NON_ATTRIBUITE.argaas, NON_ATTRIBUITE.arglnp, '               +
023500120413                'NON_ATTRIBUITE.argnrs, NON_ATTRIBUITE.argnsp, '               +
023600120413                'ATTRIBUITE.argaas, ATTRIBUITE.arglnp, ATTRIBUITE.argnrs, '    +
023700120413                'ATTRIBUITE.argnsp, ATTRIBUITE.argtgi, ATTRIBUITE.argcgi, '    +
023800120413                'ATTRIBUITE.arbind, ATTRIBUITE.arbcad, ATTRIBUITE.arblod, '    +
023900120413                'ATTRIBUITE.arbprd, '                                          +
024000120413                'ATTRIBUITE.arbaas*10000+ATTRIBUITE.arbmgs as DT_SPED_ATT '    +
024100120413                'from ATTRIBUITE join NON_ATTRIBUITE on '                      +
024200120413                'ATTRIBUITE.arbind = NON_ATTRIBUITE.arbind and '               +
024300120413                'ATTRIBUITE.arbcad = NON_ATTRIBUITE.arbcad and '               +
024400120413                'ATTRIBUITE.arblod = NON_ATTRIBUITE.arblod and '               +
024500120413                'ATTRIBUITE.arbprd = NON_ATTRIBUITE.arbprd '                   +
024600120413                'order by DT_SPED_ATT desc';
024700120413
024800120208         // - Apertura cursore
024900120208         exsr  sr_OpenCursor;
025000120413         DoU  SQLCode = 100 or SQLCode < 0 or wRighe = 30;
025100120413           exec sql  fetch next  from C1  into :DatiXStampa;
025200120208           exsr  sr_ReadCursor;
025300120413           wRighe = wRighe + 1;
025400120208         EndDo;
025500120208         // - Chiusura cursore
025600120208         exsr  sr_CloseCursor;
025700120208
025800120208       ENDSR;
025900120405
026000120405       //--------------------------------------------------------------
026100120413       // Stampo la riga estratta
026200120405       //--------------------------------------------------------------
026300120413       BEGSR  Stampa;
026400120405
026500120207
026600111128       ENDSR;
026700120215
026800120215
026900120215       //--------------------------------------------------------------
027000120215       // Esecuzione del comando (già impostato)
027100120215       //--------------------------------------------------------------
027200120215       BEGSR  sr_ExecCmd;
027300120215
027400111128         clear Qcap0100;
027500111128         Qcabcsdh = *off;
027600111128         Qcapa    = *off;
027700111128         Qcacmdss = *off;
027800111128         Qcaerved = *allX'00';
027900111128
028000111128         clear Qusec;
028100111128         Qusbprv  = %size(Qusec);
028200111128
028300111128         ProcessCommands ( Qcmd : %len( %trimr( Qcmd ) ) : Qcap0100 :
028400111128                           %size(Qcap0100) : 'CPOP0100' : *omit :
028500111128                           0 : 0 : Qusec);
028600111128
028700120210         // - Stampa DUMP + JobLog  &  Chiusura *pgm  in caso di errore
028800111128         if  Qusei <> *blank;
028900111128           exsr  sr_PrintErr;
029000111128         endif;
029100111128
029200111128       ENDSR;
029300111128
029400111128       //--------------------------------------------------------------
029500120210       // Stampa segnalazione dell'errore rilevato
029600111128       //--------------------------------------------------------------
029700111128       BEGSR  sr_PrintErr;
029800111128
029900120210         // - Stampa del Dump
030000111128         Dump(A);
030100111128
030200120210         // - Stampa del Job-Log
030300111128         Qcmd = 'DSPJOBLOG job(*) output(*print)';
030400111128         exsr  sr_ExecCmd;
030500111128
030600111128       ENDSR;
030700111128
030800111128       //--------------------------------------------------------------
030900120208       // Operazioni finali
031000111128       //--------------------------------------------------------------
031100111128       BEGSR  sr_RoutEnd;
031200111128
031300120208         // - Uscita
031400120413         return;
031500111128
031600111128       ENDSR;
031700111128
031800111128      /end-free
