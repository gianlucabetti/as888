000100111128       //==============================================================
000200120213       // TRULKSCR1 - Aggiornamento cliente da SETRAS ad AS888
000300120213       //             Seconda fase
000400111128       //==============================================================
000500111128
000600120210       // OGNI volta che si compila ricordarsi:
000700120210       // - portare il pgm su SETRAS in una libreria che esista come nome anche su AS888
000800120210       // - creare su SETRAS il pacchetto SQL che risiederà su AS888:
000900120213       //   CRTSQLPKG PGM(TRULKSCR1) RDB(AS888)
001000120209
001100111128       //--------------------------------------------------------------
001200120207       // Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
001300111128       //--------------------------------------------------------------
001400111128
001500111128     /*PRM  dbgview(*source)
001600111128     /*END
001700111128
001800111128       //--------------------------------------------------------------
001900120207       // Specifiche di controllo.                                     ?
002000111128       //--------------------------------------------------------------
002100111128
002200111128     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002300120213     h dftactgrp(*no) actgrp(*caller)
002400111128     h alwnull(*inputonly)
002500111128
002600111128       //--------------------------------------------------------------
002700120207       // Dichiarazione file.                                          ?
002800111128       //--------------------------------------------------------------
002900111128
003000111128
003100111128       //--------------------------------------------------------------
003200120207       // Definizione costanti.                                        ?
003300111128       //--------------------------------------------------------------
003400111128
003500111128
003600111128       //--------------------------------------------------------------
003700120207       // Definizione schiere.                                         ?
003800111128       //--------------------------------------------------------------
003900111128
004000111128
004100111128       //--------------------------------------------------------------
004200120207       // Definizione aree dati.                                       ?
004300111128       //--------------------------------------------------------------
004400111128
004500111128
004600111128       //--------------------------------------------------------------
004700120207       // Definizione strutture dati.                                  ?
004800111128       //--------------------------------------------------------------
004900120213     d FNCBA00F      e ds                  occurs(10)
005000120215     d TITAD00F      e ds                  occurs(32766)
005100120213     d TITPT00F      e ds                  occurs(1000)
005200120213     d TITPD00F      e ds                  occurs(1200)
005300120213     d TITGC00F      e ds                  occurs(50)
005400120213     d FNACR00F      e ds                  occurs(60)
005500120213     d FNACR10F      e ds                  occurs(60)
005600120213     d FNSPE00F      e ds                  occurs(10)
005700120213     d FNSP200F      e ds                  occurs(10)
005800120213     D TFNTC00F      e ds                  occurs(30)
005900111128
006000111128       //--------------------------------------------------------------
006100120207       // Definizione variabili globali.                               ?
006200111128       //--------------------------------------------------------------
006300111128
006400120208       // - Stringa SQL da eseguire
006500111128     d wSQl            s           1024    inz  varying
006501131111       // - stringa per costruzione LIKE nell'SQL con VIDKSC
006502131111     d proLike         s              7
006600111128
006700120208       // - Parametri SQL
006800120207
006900120207
007000120207       // - Flags booleani
007100120208     d ErrBlock        s               n   inz(*off)
007200120210     d ErrInsert       s               n   inz(*off)
007300120207
007400120207       // - Variabili di appoggio
007500120207     d DBRemote        s             10    inz('AS888')
007600120215     d Qcmd            s            500    inz
007700120213     d FNCBAOccurs     s              3s 0
007800120307     d TITADOccurs     s              5s 0
007900120307     d TITPTOccurs     s              4s 0
008000120307     d TITPDOccurs     s              4s 0
008100120213     d TITGCOccurs     s              3s 0
008200120213     d FNACROccurs     s              3s 0
008300120215     d FNACR1Occurs    s              3s 0
008400120213     d FNSPEOccurs     s              3s 0
008500120213     d FNSP2Occurs     s              3s 0
008600120213     D TFNTCOccurs     s              3s 0
008700111128
008800111128       //--------------------------------------------------------------
008900120210       // Definizione procedure usate
009000111128       //--------------------------------------------------------------
009100120207
009200120210       // - Parametri API QCAPCMD (Process Commands)
009300120207      /copy qSysInc/qRpgleSrc,QCAPCMD
009400120207      /copy gaitrasrc/srcProtoPR,QCAPCMD
009500120207
009600120210       // - Parametri gestione errori API
009700120207      /copy qSysInc/qRpgleSrc,QUSEC
009800120215
009900120215     d TRULKSCR1       pr
010000120216     d  VIDKSC                        7p 0
010100120215     d  EsisteTNTAM                   1n
010200120215     d  V1DMSG                       78
010300120215     D  VIDUSR                       10
010400120215     D  VIDPWD                       10
010500131111     D  VIDSOS                        1
010600120215
010700120215     d TRULKSCR1       pi
010800120216     d  VIDKSC                        7p 0
010900120215     d  EsisteTNTAM                   1n
011000120215     d  V1DMSG                       78
011100120215     d  VIDUSR                       10
011200120215     d  VIDPWD                       10
011300131111     D  VIDSOS                        1
011400111128
011500111128       //--------------------------------------------------------------
011600120208       // Definizione key-list.                                        ?
011700111128       //--------------------------------------------------------------
011800111128
011900111128
012000111128       //--------------------------------------------------------------
012100120208       // M A I N - L I N E                                            ?
012200111128       //--------------------------------------------------------------
012300111128
012400111128      /free
012500111128
012600120208       // - Operazioni iniziali
012700111128       exsr  sr_RoutInz;
012800111128
012900120214       // - Esecuzione
013000120213       exsr  sr_Esegui;
013100120207
013200120208       // - Operazioni finali
013300111128       exsr  sr_RoutEnd;
013400111128
013500111128       //--------------------------------------------------------------
013600120208       // Operazioni iniziali
013700111128       //--------------------------------------------------------------
013800111128       BEGSR sr_RoutInz;
013900111128
014000120213         *inRT = *on;
014100111128
014200120208         // - Impostazione opzioni per SQL
014300111128         exec SQL   set option   DynUsrPrf = *owner,
014400111128                                 CloSqlCsr = *endmod;
014500111128
014600120214         // *** devo ctrl che CNACO esista! ***
014700111128
014800111128       ENDSR;
014900111128
015000111128       //--------------------------------------------------------------
015100120208       // Apertura cursore C1
015200111128       //--------------------------------------------------------------
015300111128       BEGSR  sr_OpenCursor;
015400111128
015500120208         // - Dichiarazione del cursore
015600111128         exec sql  prepare S1  from :wSQL;
015700111128         exec sql  declare C1  cursor for S1;
015800111128
015900120208         // - Apertura del cursore
016000111128         exec sql   open C1;
016100111128
016200111128       ENDSR;
016300111128
016400111128       //--------------------------------------------------------------
016500120208       // Chiusura cursore C1
016600111128       //--------------------------------------------------------------
016700111128       BEGSR  sr_CloseCursor;
016800111128
016900120208         // - Chiusura del cursore
017000111128         exec sql   close C1;
017100111128
017200111128       ENDSR;
017300111128
017400111128       //--------------------------------------------------------------
017500120208       // Lettura cursore C1
017600111128       //--------------------------------------------------------------
017700111128       BEGSR  sr_ReadCursor;
017800111128
017900111128         select;
018000111128
018100120208           // - Fine lettura
018200111128           when  sqlCode = 100;
018300120208             //non devo fare nulla perché devo scriveree tutti i rcd di tutti i cursori letti
018400120208           leavesr;
018500111128
018600120208           // - Errore
018700111128           when  sqlCode < *zero;
018800111128             exsr  sr_PrintErr;
018900111128
019000120208           // - Elaborazione
019100111128           other;
019200120208             //non devo fare nulla adesso perché prima devo leggere tutti i rcd di tutti i cursori
019300111128
019400111128         endsl;
019500111128
019600111128       ENDSR;
019700120214
019800120214       //--------------------------------------------------------------
019900120214       // Esecuzione
020000120214       //--------------------------------------------------------------
020100120214       BEGSR  sr_Esegui;
020200120214
020300120214         // - Copio i dati da aggiungere su AS888
020400120214         exsr  sr_CopiaDatiCliente;
020500120214         if ErrBlock = *on;
020600120214           leavesr;
020700120214         endif;
020800120214
020900120214         // - Scrivo dati da aggiungere se AS888
021000120214         exsr  sr_ConnectToAS888;
021100120214
021200120214       ENDSR;
021300120208
021400120208       //--------------------------------------------------------------
021500120208       // Reperisco i dati da copiare
021600120208       //--------------------------------------------------------------
021700120208       BEGSR  sr_CopiaDatiCliente;
021800120208
021900120208         ErrBlock = *off;
022000120208
022100120208         // - Reperisci dati da aggiungere
022200120208
022300120214         //reperisco dati FNCBA
022400120210         //---------------------
022500120214         clear *all FNCBA00F;
022600120208         // - Creazione stringa SQL da eseguire
022700120214         wSQL = 'select * from FNCBA00F +
022800120214                 where CBAKSC = ' + %char(VIDKSC);
022900120208         // - Apertura cursore
023000120208         exsr  sr_OpenCursor;
023100120214         FNCBAoccurs = 1;
023200120209         // punto all'occorrenza da valorizzare
023300120214         %occur(FNCBA00F) = FNCBAoccurs;
023400120208         DoU  SQLCode = 100 or SQLCode < 0;
023500120214           exec sql  fetch next  from C1  into :FNCBA00F;
023600120214           exsr  sr_ReadCursor;
023700120208           select;
023800120209           // errore
023900120209           when SQLCode < 0;
024000120214             V1Dmsg = 'Errore in  lettura FNCBA00F - vedi stampa errore';
024100120208             ErrBlock = *on;
024200120208             // - Chiusura cursore (visto che esco con leavesr)
024300120208             exsr  sr_CloseCursor;
024400120208             leavesr;
024500120209           // nessuna lettura
024600120215           when SQLCode = 100 and FNCBAoccurs = 1;
024700120214             // non faccio nulla
024800120209           // lettura
024900120209           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
025000120214             FNCBAoccurs = FNCBAoccurs + 1;
025100120214             // punto all'occorrenza da valorizzare
025200120214             %occur(FNCBA00F) = FNCBAoccurs;
025300120209           // fine lettura
025400120209           other;
025500120209             // non faccio nulla
025600120209           endsl;
025700120208         EndDo;
025800120208         // - Chiusura cursore
025900120208         exsr  sr_CloseCursor;
026000120209         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
026100120209         // le occorrenze davvero valorizzate sono inferiori di 1
026200120214         FNCBAoccurs = FNCBAoccurs - 1;
026300120208
026400120214         //reperisco dati TITAD
026500120214         //---------------------
026600120214         clear *all TITAD00F;
026700120214         // - Creazione stringa SQL da eseguire
026800120214         wSQL = 'select * from TITAD00F +
026900120214                 where TADKSC = ' + %char(VIDKSC);
027000120214         // - Apertura cursore
027100120214         exsr  sr_OpenCursor;
027200120214         TITADoccurs = 1;
027300120214         // punto all'occorrenza da valorizzare
027400120214         %occur(TITAD00F) = TITADoccurs;
027500120418         // ciclo finché non sono finiti i rcd da leggere
027600120418         DoU  SQLCode = 100
027700120418         // oppure non c'è stato un errore bloccante (non servirebbe perché nella sr_ReadCursor fa return per questo)
027800120418          or SQLCode < 0
027900120418         // oppure siamo al limite delle occorrenze memorizzabili
028000120418         // in questo caso faccio il controllo sulle ocorrenze perché è possibile che ci siano più rcd leggibili, negli
028100120418         // altri casi le occurs sono dimensionate in modo che questo non accada.
028200120418          or TITADoccurs > %elem(TITAD00F);
028300120214           exec sql  fetch next  from C1  into :TITAD00F;
028400120214           exsr  sr_ReadCursor;
028500120214           select;
028600120214           // errore
028700120214           when SQLCode < 0;
028800120214             V1Dmsg = 'Errore in  lettura TITAD00F - vedi stampa errore';
028900120214             ErrBlock = *on;
029000120214             // - Chiusura cursore (visto che esco con leavesr)
029100120214             exsr  sr_CloseCursor;
029200120214             leavesr;
029300120214           // nessuna lettura
029400120214           when SQLCode = 100 and TITADoccurs = 1;
029500120214             // non faccio nulla
029600120214           // lettura
029700120214           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
029800120214             TITADoccurs = TITADoccurs + 1;
029900120215             // punto all'occorrenza da valorizzare se non siamo già arrivati a fine occorrenza
030000120215             if TITADoccurs <= %elem(TITAD00F);
030100120215               // punto all'occorrenza da valorizzare
030200120215               %occur(TITAD00F) = TITADoccurs;
030300120215             endif;
030400120214           // fine lettura
030500120214           other;
030600120214             // non faccio nulla
030700120214           endsl;
030800120214         EndDo;
030900120214         // - Chiusura cursore
031000120214         exsr  sr_CloseCursor;
031100120214         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
031200120214         // le occorrenze davvero valorizzate sono inferiori di 1
031300120214         TITADoccurs = TITADoccurs - 1;
031400120208
031500120214         //reperisco dati TITPD
031600120214         //---------------------
031700120214         clear *all TITPD00F;
031800120214         // - Creazione stringa SQL da eseguire
031900120214         wSQL = 'select * from TITPD00F +
032000120214                 where TPDKSC = ' + %char(VIDKSC);
032100120214         // - Apertura cursore
032200120214         exsr  sr_OpenCursor;
032300120214         TITPDoccurs = 1;
032400120214         // punto all'occorrenza da valorizzare
032500120214         %occur(TITPD00F) = TITPDoccurs;
032600120214         DoU  SQLCode = 100 or SQLCode < 0;
032700120214           exec sql  fetch next  from C1  into :TITPD00F;
032800120214           exsr  sr_ReadCursor;
032900120214           select;
033000120214           // errore
033100120214           when SQLCode < 0;
033200120214             V1Dmsg = 'Errore in  lettura TITPD00F - vedi stampa errore';
033300120214             ErrBlock = *on;
033400120214             // - Chiusura cursore (visto che esco con leavesr)
033500120214             exsr  sr_CloseCursor;
033600120214             leavesr;
033700120214           // nessuna lettura
033800120214           when SQLCode = 100 and TITPDoccurs = 1;
033900120214             // non faccio nulla
034000120214           // lettura
034100120214           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
034200120214             TITPDoccurs = TITPDoccurs + 1;
034300120214             // punto all'occorrenza da valorizzare
034400120214             %occur(TITPD00F) = TITPDoccurs;
034500120214           // fine lettura
034600120214           other;
034700120214             // non faccio nulla
034800120214           endsl;
034900120214         EndDo;
035000120214         // - Chiusura cursore
035100120214         exsr  sr_CloseCursor;
035200120214         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
035300120214         // le occorrenze davvero valorizzate sono inferiori di 1
035400120214         TITPDoccurs = TITPDoccurs - 1;
035500120214
035600120214         //reperisco dati TITPT
035700120214         //---------------------
035800120214         clear *all TITPT00F;
035900120214         // - Creazione stringa SQL da eseguire
036000120214         wSQL = 'select * from TITPT00F +
036100120214                 where TPTKSC = ' + %char(VIDKSC);
036200120214         // - Apertura cursore
036300120214         exsr  sr_OpenCursor;
036400120214         TITPToccurs = 1;
036500120214         // punto all'occorrenza da valorizzare
036600120214         %occur(TITPT00F) = TITPToccurs;
036700120214         DoU  SQLCode = 100 or SQLCode < 0;
036800120214           exec sql  fetch next  from C1  into :TITPT00F;
036900120214           exsr  sr_ReadCursor;
037000120214           select;
037100120214           // errore
037200120214           when SQLCode < 0;
037300120214             V1Dmsg = 'Errore in  lettura TITPT00F - vedi stampa errore';
037400120214             ErrBlock = *on;
037500120214             // - Chiusura cursore (visto che esco con leavesr)
037600120214             exsr  sr_CloseCursor;
037700120214             leavesr;
037800120214           // nessuna lettura
037900120214           when SQLCode = 100 and TITPToccurs = 1;
038000120214             // non faccio nulla
038100120214           // lettura
038200120214           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
038300120216             TITPToccurs = TITPToccurs + 1;
038400120214             // punto all'occorrenza da valorizzare
038500120216             %occur(TITPT00F) = TITPToccurs;
038600120214           // fine lettura
038700120214           other;
038800120214             // non faccio nulla
038900120214           endsl;
039000120214         EndDo;
039100120214         // - Chiusura cursore
039200120214         exsr  sr_CloseCursor;
039300120214         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
039400120214         // le occorrenze davvero valorizzate sono inferiori di 1
039500120214         TITPToccurs = TITPToccurs - 1;
039600120214
039700120214         //reperisco dati TITGC
039800120214         //---------------------
039900120214         clear *all TITGC00F;
040000120214         // - Creazione stringa SQL da eseguire
040100120214         wSQL = 'select * from TITGC00F +
040200120214                 where TGCKSC = ' + %char(VIDKSC);
040300120214         // - Apertura cursore
040400120214         exsr  sr_OpenCursor;
040500120214         TITGCoccurs = 1;
040600120214         // punto all'occorrenza da valorizzare
040700120214         %occur(TITGC00F) = TITGCoccurs;
040800120214         DoU  SQLCode = 100 or SQLCode < 0;
040900120214           exec sql  fetch next  from C1  into :TITGC00F;
041000120214           exsr  sr_ReadCursor;
041100120214           select;
041200120214           // errore
041300120214           when SQLCode < 0;
041400120214             V1Dmsg = 'Errore in  lettura TITGC00F - vedi stampa errore';
041500120214             ErrBlock = *on;
041600120214             // - Chiusura cursore (visto che esco con leavesr)
041700120214             exsr  sr_CloseCursor;
041800120214             leavesr;
041900120214           // nessuna lettura
042000120214           when SQLCode = 100 and TITGCoccurs = 1;
042100120214             // non faccio nulla
042200120214           // lettura
042300120214           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
042400120214             TITGCoccurs = TITGCoccurs + 1;
042500120214             // punto all'occorrenza da valorizzare
042600120214             %occur(TITGC00F) = TITGCoccurs;
042700120214           // fine lettura
042800120214           other;
042900120214             // non faccio nulla
043000120214           endsl;
043100120214         EndDo;
043200120214         // - Chiusura cursore
043300120214         exsr  sr_CloseCursor;
043400120214         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
043500120214         // le occorrenze davvero valorizzate sono inferiori di 1
043600120214         TITGCoccurs = TITGCoccurs - 1;
043700120214
043800120214         //reperisco dati FNACR
043900120214         //---------------------
044000120214         clear *all FNACR00F;
044100120214         // - Creazione stringa SQL da eseguire
044200120214         wSQL = 'select * from FNACR00F +
044300120216                 where substr(digits(ACRCRO) , 1 , 7) = ' +
044400120216                 %editc(VIDKSC:'X');
044500120214         // - Apertura cursore
044600120214         exsr  sr_OpenCursor;
044700120214         FNACRoccurs = 1;
044800120214         // punto all'occorrenza da valorizzare
044900120214         %occur(FNACR00F) = FNACRoccurs;
045000120214         DoU  SQLCode = 100 or SQLCode < 0;
045100120214           exec sql  fetch next  from C1  into :FNACR00F;
045200120214           exsr  sr_ReadCursor;
045300120214           select;
045400120214           // errore
045500120214           when SQLCode < 0;
045600120214             V1Dmsg = 'Errore in  lettura FNACR00F - vedi stampa errore';
045700120214             ErrBlock = *on;
045800120214             // - Chiusura cursore (visto che esco con leavesr)
045900120214             exsr  sr_CloseCursor;
046000120214             leavesr;
046100120214           // nessuna lettura
046200120214           when SQLCode = 100 and FNACRoccurs = 1;
046300120214             // non faccio nulla
046400120214           // lettura
046500120214           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
046600120214             FNACRoccurs = FNACRoccurs + 1;
046700120214             // punto all'occorrenza da valorizzare
046800120214             %occur(FNACR00F) = FNACRoccurs;
046900120214           // fine lettura
047000120214           other;
047100120214             // non faccio nulla
047200120214           endsl;
047300120214         EndDo;
047400120214         // - Chiusura cursore
047500120214         exsr  sr_CloseCursor;
047600120214         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
047700120214         // le occorrenze davvero valorizzate sono inferiori di 1
047800120214         FNACRoccurs = FNACRoccurs - 1;
047900120214
048000120214         //reperisco dati FNACR1
048100120214         //---------------------
048200120214         clear *all FNACR10F;
048300120214         // - Creazione stringa SQL da eseguire
048400120216         wSQL = 'select * from FNACR10F +
048500120216                 where substr(digits(ACR1CRO) , 1 , 7) = ' +
048600120216                 %editc(VIDKSC:'X');
048700120214         // - Apertura cursore
048800120214         exsr  sr_OpenCursor;
048900120216         FNACR1occurs = 1;
049000120214         // punto all'occorrenza da valorizzare
049100120214         %occur(FNACR10F) = FNACR1occurs;
049200120214         DoU  SQLCode = 100 or SQLCode < 0;
049300120214           exec sql  fetch next  from C1  into :FNACR10F;
049400120214           exsr  sr_ReadCursor;
049500120214           select;
049600120214           // errore
049700120214           when SQLCode < 0;
049800120214             V1Dmsg = 'Errore in  lettura FNACR10F - vedi stampa errore';
049900120214             ErrBlock = *on;
050000120214             // - Chiusura cursore (visto che esco con leavesr)
050100120214             exsr  sr_CloseCursor;
050200120214             leavesr;
050300120214           // nessuna lettura
050400120214           when SQLCode = 100 and FNACR1occurs = 1;
050500120214             // non faccio nulla
050600120214           // lettura
050700120214           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
050800120214             FNACR1occurs = FNACR1occurs + 1;
050900120214             // punto all'occorrenza da valorizzare
051000120214             %occur(FNACR10F) = FNACR1occurs;
051100120214           // fine lettura
051200120214           other;
051300120214             // non faccio nulla
051400120214           endsl;
051500120214         EndDo;
051600120214         // - Chiusura cursore
051700120214         exsr  sr_CloseCursor;
051800120214         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
051900120214         // le occorrenze davvero valorizzate sono inferiori di 1
052000120214         FNACR1occurs = FNACR1occurs - 1;
052100120214
052200120214         //reperisco dati FNSPE
052300120214         //---------------------
052400120214         clear *all FNSPE00F;
052500120214         // - Creazione stringa SQL da eseguire
052600120214         wSQL = 'select * from FNSPE00F +
052700120214                 where SPECLI = ' + %char(VIDKSC) + ' and SPEFLS=''L''';
052800120214         // - Apertura cursore
052900120214         exsr  sr_OpenCursor;
053000120214         FNSPEoccurs = 1;
053100120214         // punto all'occorrenza da valorizzare
053200120214         %occur(FNSPE00F) = FNSPEoccurs;
053300120214         DoU  SQLCode = 100 or SQLCode < 0;
053400120214           exec sql  fetch next  from C1  into :FNSPE00F;
053500120214           exsr  sr_ReadCursor;
053600120214           select;
053700120214           // errore
053800120214           when SQLCode < 0;
053900120214             V1Dmsg = 'Errore in  lettura FNSPE00F - vedi stampa errore';
054000120214             ErrBlock = *on;
054100120214             // - Chiusura cursore (visto che esco con leavesr)
054200120214             exsr  sr_CloseCursor;
054300120214             leavesr;
054400120214           // nessuna lettura
054500120214           when SQLCode = 100 and FNSPEoccurs = 1;
054600120214             // non faccio nulla
054700120214           // lettura
054800120214           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
054900120214             FNSPEoccurs = FNSPEoccurs + 1;
055000120214             // punto all'occorrenza da valorizzare
055100120214             %occur(FNSPE00F) = FNSPEoccurs;
055200120214           // fine lettura
055300120214           other;
055400120214             // non faccio nulla
055500120214           endsl;
055600120214         EndDo;
055700120214         // - Chiusura cursore
055800120214         exsr  sr_CloseCursor;
055900120214         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
056000120214         // le occorrenze davvero valorizzate sono inferiori di 1
056100120214         FNSPEoccurs = FNSPEoccurs - 1;
056200120214
056300120214         //reperisco dati FNSP2
056400120214         //---------------------
056500120214         clear *all FNSP200F;
056600120214         // - Creazione stringa SQL da eseguire
056700120214         wSQL = 'select * from FNSP200F +
056800120214                 where SP2CLI = ' + %char(VIDKSC);
056900120214         // - Apertura cursore
057000120214         exsr  sr_OpenCursor;
057100120214         FNSP2occurs = 1;
057200120214         // punto all'occorrenza da valorizzare
057300120214         %occur(FNSP200F) = FNSP2occurs;
057400120214         DoU  SQLCode = 100 or SQLCode < 0;
057500120214           exec sql  fetch next  from C1  into :FNSP200F;
057600120214           exsr  sr_ReadCursor;
057700120214           select;
057800120214           // errore
057900120214           when SQLCode < 0;
058000120214             V1Dmsg = 'Errore in  lettura FNSP200F - vedi stampa errore';
058100120214             ErrBlock = *on;
058200120214             // - Chiusura cursore (visto che esco con leavesr)
058300120214             exsr  sr_CloseCursor;
058400120214             leavesr;
058500120214           // nessuna lettura
058600120214           when SQLCode = 100 and FNSP2occurs = 1;
058700120214             // non faccio nulla
058800120214           // lettura
058900120214           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
059000120214             FNSP2occurs = FNSP2occurs + 1;
059100120214             // punto all'occorrenza da valorizzare
059200120214             %occur(FNSP200F) = FNSP2occurs;
059300120214           // fine lettura
059400120214           other;
059500120214             // non faccio nulla
059600120214           endsl;
059700120214         EndDo;
059800120214         // - Chiusura cursore
059900120214         exsr  sr_CloseCursor;
060000120214         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
060100120214         // le occorrenze davvero valorizzate sono inferiori di 1
060200120214         FNSP2occurs = FNSP2occurs - 1;
060300120215
060400120215         //reperisco dati TFNTC
060500120215         //---------------------
060600120215         clear *all TFNTC00F;
060700120215         // - Creazione stringa SQL da eseguire
060800120215         wSQL = 'select * from TFNTC00F +
060900120216                 where NTCNK1 = ''0151' + %editc(VIDKSC:'X') +
061000120216                 ''' and NTCAPL = ''C''';
061100120215         // - Apertura cursore
061200120215         exsr  sr_OpenCursor;
061300120215         TFNTCoccurs = 1;
061400120215         // punto all'occorrenza da valorizzare
061500120215         %occur(TFNTC00F) = TFNTCoccurs;
061600120215         DoU  SQLCode = 100 or SQLCode < 0;
061700120215           exec sql  fetch next  from C1  into :TFNTC00F;
061800120215           exsr  sr_ReadCursor;
061900120215           select;
062000120215           // errore
062100120215           when SQLCode < 0;
062200120215             V1Dmsg = 'Errore in  lettura TFNTC00F - vedi stampa errore';
062300120215             ErrBlock = *on;
062400120215             // - Chiusura cursore (visto che esco con leavesr)
062500120215             exsr  sr_CloseCursor;
062600120215             leavesr;
062700120215           // nessuna lettura
062800120215           when SQLCode = 100 and TFNTCoccurs = 1;
062900120215             // non faccio nulla
063000120215           // lettura
063100120215           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
063200120215             TFNTCoccurs = TFNTCoccurs + 1;
063300120215             // punto all'occorrenza da valorizzare
063400120215             %occur(TFNTC00F) = TFNTCoccurs;
063500120215           // fine lettura
063600120215           other;
063700120215             // non faccio nulla
063800120215           endsl;
063900120215         EndDo;
064000120215         // - Chiusura cursore
064100120215         exsr  sr_CloseCursor;
064200120215         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
064300120215         // le occorrenze davvero valorizzate sono inferiori di 1
064400120215         TFNTCoccurs = TFNTCoccurs - 1;
064500120214
064600120214       ENDSR;
064700120214
064800120214       //--------------------------------------------------------------
064900120207       // Scrivo rcd estratti
065000111128       //--------------------------------------------------------------
065100120208       BEGSR  sr_ConnectToAS888;
065200111128
065300120210         ErrInsert = *off;
065400120210
065500120207         // mi connetto ad AS888
065600120207         exec sql  connect to :DBRemote user :VIDUSR using :VIDPWD;
065700120208         if SQLCode < 0;
065800120208           //emetto msg di errore perentorio
065900120216           V1Dmsg = 'FASE 2: non riuscita connessione ad AS888 +
066000120216                     - SCOLLEGARSI e riprovare';
066100120208           leavesr;
066200120208         endif;
066300131111
066400131111         // se richiesta la sovrapposizione, prima cancello i rcd
066500131111         if VIDSOS='S';
066600131111           exsr sr_DeleteRcd;
066700131111         endif;
066800120207
066900120208         // esecuzione scrittura rcd estratti
067000120208         exsr sr_InsertRcd;
067100120208
067200120208         // mi disconnetto da AS888
067300120208         exec sql  disconnect :DBRemote;
067400120208         // se la disconnessione ha dato errore
067500120208         if SQLCode < 0;
067600120208           //emetto msg di errore perentorio
067700120210           if ErrInsert = *on;
067800120216             V1Dmsg = 'FASE 2: agg. NON eseguito ED errore in +
067900120216                       connessione SETRAS - SCOLLEGARSI!';
068000120210           else;
068100120216             V1Dmsg = 'FASE 2: agg. eseguito ma errore in +
068200120210                       connessione SETRAS - SCOLLEGARSI!';
068300120210           endif;
068400120208           leavesr;
068500120208         endif;
068600120208
068700120216         // mi riconnetto a SETRAS
068800120216         // siccome in BRT gli SQLRPGLE si compilano col parametro RDBCNNMTH = *DUW, la connect ad AS888 NON ha chiuso
068900120216         // la connessione a SETRAS, l'ha resa dorminte. In questo caso NON serve un cmd CONNECT RESET perché  darebbe
069000120216         // errore SQL0842 = Connessione al database relazionale &1 già esistente.
069100120216         // Però, la connessione al DB locale (SETRAS) esiste se prima c'è stato almeno uno statement sql se quel DB.
069200120216         // In questo caso la SET CONNECTION dà errore SQL0842 = Il collegamento al database relazionale &1 non esiste
069300120216         // e va eseguita la CONNECT RESET.
069400120216         // in questo pgm dovrebbe sempre esserci prima uno statement SQL su SETRAS per leggere i dati,
069500120216         // ma non si sa mai... magari vengono fatte modifiche
069600120216         // Per cui, dopo aver disconnesso AS888, fare il cmd SET CONNECTION per "risvegliare" SETRAS e, se dà errore
069700120216         // provare con CONNECT RESET.
069800120216         // Solo se entrambi hanno dato errore, emettere un msg a video.
069900120216         exec sql  set connection SETRAS;
070000120216         if SQLCode < 0;
070100120216           // provo con CONNECT RESET
070200120216           exec sql  connect reset;
070300120216           // se anche in questo caso ho errore, emetto msg
070400120216           if SQLCode < 0;
070500120216             //emetto msg di errore perentorio
070600120216             if ErrInsert = *on;
070700120216               V1Dmsg = 'FASE 1: agg. NON eseguito ED errore in +
070800120216                         connessione SETRAS - SCOLLEGARSI!';
070900120216             else;
071000120216               V1Dmsg = 'FASE 1: agg. eseguito ma errore in +
071100120216                         connessione DB SETRAS - SCOLLEGARSI!';
071200120216             endif;
071300120216             leavesr;
071400120216           endif;
071500120216         endif;
071600120207
071700111128       ENDSR;
071800131111
071900131111       //--------------------------------------------------------------
072000131111       // Cancello rcd già presenti
072100131111       //--------------------------------------------------------------
072200131111       BEGSR  sr_DeleteRcd;
072300131111
072400131111         // cancello i record sul FNCBA00F di test
072500131111         EXEC SQL delete from UNITRAGRP/FNCBA00F
072600131111                where CBAKSC=char(:VIDKSC);
072700131111         if SQLCode < 0;
072800131111           // se c'è un errore, probabilmente è perché manca il rcd da cancellare, per cui vado avanti lo stesso
072900131111         endif;
073000131111
073100131111         // cancello i record sul TITAD00F di test
073200131111         EXEC SQL delete from UNITRAGRP/TITAD00F
073300131111                where TADKSC=char(:VIDKSC);
073400131111         if SQLCode < 0;
073500131111           // se c'è un errore, probabilmente è perché manca il rcd da cancellare, per cui vado avanti lo stesso
073600131111         endif;
073700131111
073800131111         // cancello i record sul TITPD00F di test
073900131111         EXEC SQL delete from UNITRAGRP/TITPD00F
074000131111                where TPDKSC=char(:VIDKSC);
074100131111         if SQLCode < 0;
074200131111           // se c'è un errore, probabilmente è perché manca il rcd da cancellare, per cui vado avanti lo stesso
074300131111         endif;
074400131111
074500131111         // cancello i record sul TITPT00F di test
074600131111         EXEC SQL delete from UNITRAGRP/TITPT00F
074700131111                where TPTKSC=char(:VIDKSC);
074800131111         if SQLCode < 0;
074900131111           // se c'è un errore, probabilmente è perché manca il rcd da cancellare, per cui vado avanti lo stesso
075000131111         endif;
075100131111
075200131111         // cancello i record sul TITGC00F di test
075300131111         EXEC SQL delete from UNITRAGRP/TITGC00F
075400131111                where TGCKSC=char(:VIDKSC);
075500131111         if SQLCode < 0;
075600131111           // se c'è un errore, probabilmente è perché manca il rcd da cancellare, per cui vado avanti lo stesso
075700131111         endif;
075800131111
075900131111         // cancello i record sul FNACR00F di test
076000131111         proLike = %editc(VIDKSC:'X');
076100131111         EXEC SQL delete from UNITRAGRP/FNACR00F
076200131111                 where substr(digits(ACRCRO) , 1 , 7) = :proLike;
076300131111         if SQLCode < 0;
076400131111           // se c'è un errore, probabilmente è perché manca il rcd da cancellare, per cui vado avanti lo stesso
076500131111         endif;
076600131111
076700131111         // cancello i record sul FNACR10F di test
076800131111         proLike = %editc(VIDKSC:'X');
076900131111         EXEC SQL delete from UNITRAGRP/FNACR10F
077000131111                 where substr(digits(ACR1CRO) , 1 , 7) = :proLike;
077100131111         if SQLCode < 0;
077200131111           // se c'è un errore, probabilmente è perché manca il rcd da cancellare, per cui vado avanti lo stesso
077300131111         endif;
077400131111
077500131111         // cancello i record sul FNSPE00F di test
077600131111         EXEC SQL delete from UNITRAGRP/FNSPE00F
077700131111                where SPECLI=char(:VIDKSC) and SPEFLS='L';
077800131111         if SQLCode < 0;
077900131111           // se c'è un errore, probabilmente è perché manca il rcd da cancellare, per cui vado avanti lo stesso
078000131111         endif;
078100131111
078200131111         // cancello i record sul FNSP200F di test
078300131111         EXEC SQL delete from UNITRAGRP/FNSP200F
078400131111                where SP2CLI=char(:VIDKSC);
078500131111         if SQLCode < 0;
078600131111           // se c'è un errore, probabilmente è perché manca il rcd da cancellare, per cui vado avanti lo stesso
078700131111         endif;
078800131111
078900131111         // cancello i record sul TFNTC00F di test
079000131111         proLike = %editc(VIDKSC:'X');
079100131111         EXEC SQL delete from UNITRAGRP/TFNTC00F
079200131111                 where NTCNK1 = '0151' concat :proLike and NTCAPL='C';
079300131111         if SQLCode < 0;
079400131111           // se c'è un errore, probabilmente è perché manca il rcd da cancellare, per cui vado avanti lo stesso
079500131111         endif;
079600131111
079700131111         // se sono qui vuol dire che ho scritto tutto per cui committo
079800131111         exec sql commit;
079900131111
080000131111       ENDSR;
080100111128
080200120208       //--------------------------------------------------------------
080300120208       // Scrivo rcd estratti
080400120208       //--------------------------------------------------------------
080500120208       BEGSR  sr_InsertRcd;
080600120208
080700120215         // aggiungo i record letti al FNCBA00F di test
080800120215         if FNCBAoccurs > 0;
080900120227           EXEC SQL insert into UNITRAGRP/FNCBA00F :FNCBAoccurs rows
081000120227                    values(:FNCBA00F)
081100120210                    with UR;
081200120208           if SQLCode < 0;
081300120208             // se ho avuto un errore nell'insert, a regime sarà solo per chiave duplicata, ma non è un file vincolante
081400120208             // per cui lo segnalo solo ma proseguo
081500120215             V1Dmsg = 'Errore di copia sul file FNCBA00F +
081600120208                       - controllare';
081700120208           endif;
081800120208         endif;
081900120215
082000120215         // se ho almeno un rcd di TNTAM, devo avere almeno un rcd tra TITAD, TITPT, TITPD e TITGC
082100120215         if EsisteTNTAM = *on and
082200120215           TITADoccurs = 0 and TITPToccurs = 0 and TITPDoccurs = 0 and
082300120215           TITGCoccurs = 0;
082400120215           V1Dmsg = 'Copiato TNTAM ma non ci sono rcd su +
082500120215                     TITAD, TITPT, TITPD, TITGC';
082600120215         endif;
082700120215
082800120215         // aggiungo i record letti al TITAD00F di test
082900120215         if TITADoccurs > 0;
083000120227           EXEC SQL insert into UNITRAGRP/TITAD00F :TITADoccurs rows
083100120227                    values(:TITAD00F)
083200120215                    with UR;
083300120215           if SQLCode < 0;
083400120215             // se ho avuto un errore nell'insert, a regime sarà solo per chiave duplicata, ma non è un file vincolante
083500120215             // per cui lo segnalo solo ma proseguo
083600120215             V1Dmsg = 'Errore di copia sul file TITAD00F +
083700120215                       - controllare';
083800120215           endif;
083900120215         endif;
084000120215
084100120215         // aggiungo i record letti al TITPT00F di test
084200120215         if TITPToccurs > 0;
084300120227           EXEC SQL insert into UNITRAGRP/TITPT00F :TITPToccurs rows
084400120227                    values(:TITPT00F)
084500120215                    with UR;
084600120215           if SQLCode < 0;
084700120215             // se ho avuto un errore nell'insert, a regime sarà solo per chiave duplicata, ma non è un file vincolante
084800120215             // per cui lo segnalo solo ma proseguo
084900120215             V1Dmsg = 'Errore di copia sul file TITPT00F +
085000120215                       - controllare';
085100120215           endif;
085200120215         endif;
085300120215
085400120215         // aggiungo i record letti al TITPD00F di test
085500120215         if TITPDoccurs > 0;
085600120227           EXEC SQL insert into UNITRAGRP/TITPD00F :TITPDoccurs rows
085700120227                    values(:TITPD00F)
085800120215                    with UR;
085900120215           if SQLCode < 0;
086000120215             // se ho avuto un errore nell'insert, a regime sarà solo per chiave duplicata, ma non è un file vincolante
086100120215             // per cui lo segnalo solo ma proseguo
086200120215             V1Dmsg = 'Errore di copia sul file TITPD00F +
086300120215                       - controllare';
086400120215           endif;
086500120215         endif;
086600120215
086700120215         // aggiungo i record letti al TITGC00F di test
086800120215         if TITGCoccurs > 0;
086900120227           EXEC SQL insert into UNITRAGRP/TITGC00F :TITGCoccurs rows
087000120227                    values(:TITGC00F)
087100120215                    with UR;
087200120215           if SQLCode < 0;
087300120215             // se ho avuto un errore nell'insert, a regime sarà solo per chiave duplicata, ma non è un file vincolante
087400120215             // per cui lo segnalo solo ma proseguo
087500120215             V1Dmsg = 'Errore di copia sul file TITGC00F +
087600120215                       - controllare';
087700120215           endif;
087800120215         endif;
087900120215
088000120215         // aggiungo i record letti al FNACR00F di test
088100120215         if FNACRoccurs > 0;
088200120227           EXEC SQL insert into UNITRAGRP/FNACR00F :FNACRoccurs rows
088300120227                    values(:FNACR00F)
088400120215                    with UR;
088500120215           if SQLCode < 0;
088600120215             // se ho avuto un errore nell'insert, a regime sarà solo per chiave duplicata, ma non è un file vincolante
088700120215             // per cui lo segnalo solo ma proseguo
088800120215             V1Dmsg = 'Errore di copia sul file FNACR00F +
088900120215                       - controllare';
089000120215           endif;
089100120215         endif;
089200120215
089300120215         // aggiungo i record letti al FNACR10F di test
089400120215         if FNACR1occurs > 0;
089500120227           EXEC SQL insert into UNITRAGRP/FNACR10F :FNACR1occurs rows
089600120227                    values(:FNACR10F)
089700120215                    with UR;
089800120215           if SQLCode < 0;
089900120215             // se ho avuto un errore nell'insert, a regime sarà solo per chiave duplicata, ma non è un file vincolante
090000120215             // per cui lo segnalo solo ma proseguo
090100120215             V1Dmsg = 'Errore di copia sul file FNACR10F +
090200120215                       - controllare';
090300120215           endif;
090400120215         endif;
090500120215
090600120215         // aggiungo i record letti al FNSPE00F di test
090700120215         if FNSPEoccurs > 0;
090800120227           EXEC SQL insert into UNITRAGRP/FNSPE00F :FNSPEoccurs rows
090900120227                    values(:FNSPE00F)
091000120215                    with UR;
091100120215           if SQLCode < 0;
091200120215             // se ho avuto un errore nell'insert, a regime sarà solo per chiave duplicata, ma non è un file vincolante
091300120215             // per cui lo segnalo solo ma proseguo
091400120215             V1Dmsg = 'Errore di copia sul file FNSPE00F +
091500120215                       - controllare';
091600120215           endif;
091700120215         endif;
091800120215
091900120215         // aggiungo i record letti al FNSP200F di test
092000120215         if FNSP2occurs > 0;
092100120227           EXEC SQL insert into UNITRAGRP/FNSP200F :FNSP2occurs rows
092200120227                    values(:FNSP200F)
092300120215                    with UR;
092400120215           if SQLCode < 0;
092500120215             // se ho avuto un errore nell'insert, a regime sarà solo per chiave duplicata, ma non è un file vincolante
092600120215             // per cui lo segnalo solo ma proseguo
092700120215             V1Dmsg = 'Errore di copia sul file FNSP200F +
092800120215                       - controllare';
092900120215           endif;
093000120215         endif;
093100120215
093200120215         // aggiungo i record letti al TFNTC00F di test
093300120215         if TFNTCoccurs > 0;
093400120227           EXEC SQL insert into UNITRAGRP/TFNTC00F :TFNTCoccurs rows
093500120227                    values(:TFNTC00F)
093600120215                    with UR;
093700120215           if SQLCode < 0;
093800120215             // se ho avuto un errore nell'insert, a regime sarà solo per chiave duplicata, ma non è un file vincolante
093900120215             // per cui lo segnalo solo ma proseguo
094000120215             V1Dmsg = 'Errore di copia sul file TFNTC00F +
094100120215                       - controllare';
094200120215           endif;
094300120215         endif;
094400120208
094500120210         // se sono qui vuol dire che ho scritto tutto per cui committo
094600120210         exec sql commit;
094700120210
094800120208       ENDSR;
094900120208
095000111128       //--------------------------------------------------------------
095100120210       // Esecuzione del comando (già impostato)
095200111128       //--------------------------------------------------------------
095300111128       BEGSR  sr_ExecCmd;
095400111128
095500111128         clear Qcap0100;
095600111128         Qcabcsdh = *off;
095700111128         Qcapa    = *off;
095800111128         Qcacmdss = *off;
095900111128         Qcaerved = *allX'00';
096000111128
096100111128         clear Qusec;
096200111128         Qusbprv  = %size(Qusec);
096300111128
096400111128         ProcessCommands ( Qcmd : %len( %trimr( Qcmd ) ) : Qcap0100 :
096500111128                           %size(Qcap0100) : 'CPOP0100' : *omit :
096600111128                           0 : 0 : Qusec);
096700111128
096800120210         // - Stampa DUMP + JobLog  &  Chiusura *pgm  in caso di errore
096900111128         if  Qusei <> *blank;
097000111128           exsr  sr_PrintErr;
097100111128         endif;
097200111128
097300111128       ENDSR;
097400111128
097500111128       //--------------------------------------------------------------
097600120210       // Stampa segnalazione dell'errore rilevato
097700111128       //--------------------------------------------------------------
097800111128       BEGSR  sr_PrintErr;
097900111128
098000120210         // - Stampa del Dump
098100111128         Dump(A);
098200111128
098300120210         // - Stampa del Job-Log
098400111128         Qcmd = 'DSPJOBLOG job(*) output(*print)';
098500111128         exsr  sr_ExecCmd;
098600111128
098700111128       ENDSR;
098800111128
098900111128       //--------------------------------------------------------------
099000120208       // Operazioni finali
099100111128       //--------------------------------------------------------------
099200111128       BEGSR  sr_RoutEnd;
099300111128
099400120208         // - Uscita
099500111128         return;
099600111128
099700111128       ENDSR;
099800111128
099900111128      /end-free
