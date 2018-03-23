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
010500120215
010600120215     d TRULKSCR1       pi
010700120216     d  VIDKSC                        7p 0
010800120215     d  EsisteTNTAM                   1n
010900120215     d  V1DMSG                       78
011000120215     d  VIDUSR                       10
011100120215     d  VIDPWD                       10
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
012700120214       // - Esecuzione
012800120213       exsr  sr_Esegui;
012900120207
013000120208       // - Operazioni finali
013100111128       exsr  sr_RoutEnd;
013200111128
013300111128       //--------------------------------------------------------------
013400120208       // Operazioni iniziali
013500111128       //--------------------------------------------------------------
013600111128       BEGSR sr_RoutInz;
013700111128
013800120213         *inRT = *on;
013900111128
014000120208         // - Impostazione opzioni per SQL
014100111128         exec SQL   set option   DynUsrPrf = *owner,
014200111128                                 CloSqlCsr = *endmod;
014300111128
014400120214         // *** devo ctrl che CNACO esista! ***
014500111128
014600111128       ENDSR;
014700111128
014800111128       //--------------------------------------------------------------
014900120208       // Apertura cursore C1
015000111128       //--------------------------------------------------------------
015100111128       BEGSR  sr_OpenCursor;
015200111128
015300120208         // - Dichiarazione del cursore
015400111128         exec sql  prepare S1  from :wSQL;
015500111128         exec sql  declare C1  cursor for S1;
015600111128
015700120208         // - Apertura del cursore
015800111128         exec sql   open C1;
015900111128
016000111128       ENDSR;
016100111128
016200111128       //--------------------------------------------------------------
016300120208       // Chiusura cursore C1
016400111128       //--------------------------------------------------------------
016500111128       BEGSR  sr_CloseCursor;
016600111128
016700120208         // - Chiusura del cursore
016800111128         exec sql   close C1;
016900111128
017000111128       ENDSR;
017100111128
017200111128       //--------------------------------------------------------------
017300120208       // Lettura cursore C1
017400111128       //--------------------------------------------------------------
017500111128       BEGSR  sr_ReadCursor;
017600111128
017700111128         select;
017800111128
017900120208           // - Fine lettura
018000111128           when  sqlCode = 100;
018100120208             //non devo fare nulla perché devo scriveree tutti i rcd di tutti i cursori letti
018200120208           leavesr;
018300111128
018400120208           // - Errore
018500111128           when  sqlCode < *zero;
018600111128             exsr  sr_PrintErr;
018700111128
018800120208           // - Elaborazione
018900111128           other;
019000120208             //non devo fare nulla adesso perché prima devo leggere tutti i rcd di tutti i cursori
019100111128
019200111128         endsl;
019300111128
019400111128       ENDSR;
019500120214
019600120214       //--------------------------------------------------------------
019700120214       // Esecuzione
019800120214       //--------------------------------------------------------------
019900120214       BEGSR  sr_Esegui;
020000120214
020100120214         // - Copio i dati da aggiungere su AS888
020200120214         exsr  sr_CopiaDatiCliente;
020300120214         if ErrBlock = *on;
020400120214           leavesr;
020500120214         endif;
020600120214
020700120214         // - Scrivo dati da aggiungere se AS888
020800120214         exsr  sr_ConnectToAS888;
020900120214
021000120214       ENDSR;
021100120208
021200120208       //--------------------------------------------------------------
021300120208       // Reperisco i dati da copiare
021400120208       //--------------------------------------------------------------
021500120208       BEGSR  sr_CopiaDatiCliente;
021600120208
021700120208         ErrBlock = *off;
021800120208
021900120208         // - Reperisci dati da aggiungere
022000120208
022100120214         //reperisco dati FNCBA
022200120210         //---------------------
022300120214         clear *all FNCBA00F;
022400120208         // - Creazione stringa SQL da eseguire
022500120214         wSQL = 'select * from FNCBA00F +
022600120214                 where CBAKSC = ' + %char(VIDKSC);
022700120208         // - Apertura cursore
022800120208         exsr  sr_OpenCursor;
022900120214         FNCBAoccurs = 1;
023000120209         // punto all'occorrenza da valorizzare
023100120214         %occur(FNCBA00F) = FNCBAoccurs;
023200120208         DoU  SQLCode = 100 or SQLCode < 0;
023300120214           exec sql  fetch next  from C1  into :FNCBA00F;
023400120214           exsr  sr_ReadCursor;
023500120208           select;
023600120209           // errore
023700120209           when SQLCode < 0;
023800120214             V1Dmsg = 'Errore in  lettura FNCBA00F - vedi stampa errore';
023900120208             ErrBlock = *on;
024000120208             // - Chiusura cursore (visto che esco con leavesr)
024100120208             exsr  sr_CloseCursor;
024200120208             leavesr;
024300120209           // nessuna lettura
024400120215           when SQLCode = 100 and FNCBAoccurs = 1;
024500120214             // non faccio nulla
024600120209           // lettura
024700120209           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
024800120214             FNCBAoccurs = FNCBAoccurs + 1;
024900120214             // punto all'occorrenza da valorizzare
025000120214             %occur(FNCBA00F) = FNCBAoccurs;
025100120209           // fine lettura
025200120209           other;
025300120209             // non faccio nulla
025400120209           endsl;
025500120208         EndDo;
025600120208         // - Chiusura cursore
025700120208         exsr  sr_CloseCursor;
025800120209         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
025900120209         // le occorrenze davvero valorizzate sono inferiori di 1
026000120214         FNCBAoccurs = FNCBAoccurs - 1;
026100120208
026200120214         //reperisco dati TITAD
026300120214         //---------------------
026400120214         clear *all TITAD00F;
026500120214         // - Creazione stringa SQL da eseguire
026600120214         wSQL = 'select * from TITAD00F +
026700120214                 where TADKSC = ' + %char(VIDKSC);
026800120214         // - Apertura cursore
026900120214         exsr  sr_OpenCursor;
027000120214         TITADoccurs = 1;
027100120214         // punto all'occorrenza da valorizzare
027200120214         %occur(TITAD00F) = TITADoccurs;
027300120418         // ciclo finché non sono finiti i rcd da leggere
027400120418         DoU  SQLCode = 100
027500120418         // oppure non c'è stato un errore bloccante (non servirebbe perché nella sr_ReadCursor fa return per questo)
027600120418          or SQLCode < 0
027700120418         // oppure siamo al limite delle occorrenze memorizzabili
027800120418         // in questo caso faccio il controllo sulle ocorrenze perché è possibile che ci siano più rcd leggibili, negli
027900120418         // altri casi le occurs sono dimensionate in modo che questo non accada.
028000120418          or TITADoccurs > %elem(TITAD00F);
028100120214           exec sql  fetch next  from C1  into :TITAD00F;
028200120214           exsr  sr_ReadCursor;
028300120214           select;
028400120214           // errore
028500120214           when SQLCode < 0;
028600120214             V1Dmsg = 'Errore in  lettura TITAD00F - vedi stampa errore';
028700120214             ErrBlock = *on;
028800120214             // - Chiusura cursore (visto che esco con leavesr)
028900120214             exsr  sr_CloseCursor;
029000120214             leavesr;
029100120214           // nessuna lettura
029200120214           when SQLCode = 100 and TITADoccurs = 1;
029300120214             // non faccio nulla
029400120214           // lettura
029500120214           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
029600120214             TITADoccurs = TITADoccurs + 1;
029700120215             // punto all'occorrenza da valorizzare se non siamo già arrivati a fine occorrenza
029800120215             if TITADoccurs <= %elem(TITAD00F);
029900120215               // punto all'occorrenza da valorizzare
030000120215               %occur(TITAD00F) = TITADoccurs;
030100120215             endif;
030200120214           // fine lettura
030300120214           other;
030400120214             // non faccio nulla
030500120214           endsl;
030600120214         EndDo;
030700120214         // - Chiusura cursore
030800120214         exsr  sr_CloseCursor;
030900120214         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
031000120214         // le occorrenze davvero valorizzate sono inferiori di 1
031100120214         TITADoccurs = TITADoccurs - 1;
031200120208
031300120214         //reperisco dati TITPD
031400120214         //---------------------
031500120214         clear *all TITPD00F;
031600120214         // - Creazione stringa SQL da eseguire
031700120214         wSQL = 'select * from TITPD00F +
031800120214                 where TPDKSC = ' + %char(VIDKSC);
031900120214         // - Apertura cursore
032000120214         exsr  sr_OpenCursor;
032100120214         TITPDoccurs = 1;
032200120214         // punto all'occorrenza da valorizzare
032300120214         %occur(TITPD00F) = TITPDoccurs;
032400120214         DoU  SQLCode = 100 or SQLCode < 0;
032500120214           exec sql  fetch next  from C1  into :TITPD00F;
032600120214           exsr  sr_ReadCursor;
032700120214           select;
032800120214           // errore
032900120214           when SQLCode < 0;
033000120214             V1Dmsg = 'Errore in  lettura TITPD00F - vedi stampa errore';
033100120214             ErrBlock = *on;
033200120214             // - Chiusura cursore (visto che esco con leavesr)
033300120214             exsr  sr_CloseCursor;
033400120214             leavesr;
033500120214           // nessuna lettura
033600120214           when SQLCode = 100 and TITPDoccurs = 1;
033700120214             // non faccio nulla
033800120214           // lettura
033900120214           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
034000120214             TITPDoccurs = TITPDoccurs + 1;
034100120214             // punto all'occorrenza da valorizzare
034200120214             %occur(TITPD00F) = TITPDoccurs;
034300120214           // fine lettura
034400120214           other;
034500120214             // non faccio nulla
034600120214           endsl;
034700120214         EndDo;
034800120214         // - Chiusura cursore
034900120214         exsr  sr_CloseCursor;
035000120214         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
035100120214         // le occorrenze davvero valorizzate sono inferiori di 1
035200120214         TITPDoccurs = TITPDoccurs - 1;
035300120214
035400120214         //reperisco dati TITPT
035500120214         //---------------------
035600120214         clear *all TITPT00F;
035700120214         // - Creazione stringa SQL da eseguire
035800120214         wSQL = 'select * from TITPT00F +
035900120214                 where TPTKSC = ' + %char(VIDKSC);
036000120214         // - Apertura cursore
036100120214         exsr  sr_OpenCursor;
036200120214         TITPToccurs = 1;
036300120214         // punto all'occorrenza da valorizzare
036400120214         %occur(TITPT00F) = TITPToccurs;
036500120214         DoU  SQLCode = 100 or SQLCode < 0;
036600120214           exec sql  fetch next  from C1  into :TITPT00F;
036700120214           exsr  sr_ReadCursor;
036800120214           select;
036900120214           // errore
037000120214           when SQLCode < 0;
037100120214             V1Dmsg = 'Errore in  lettura TITPT00F - vedi stampa errore';
037200120214             ErrBlock = *on;
037300120214             // - Chiusura cursore (visto che esco con leavesr)
037400120214             exsr  sr_CloseCursor;
037500120214             leavesr;
037600120214           // nessuna lettura
037700120214           when SQLCode = 100 and TITPToccurs = 1;
037800120214             // non faccio nulla
037900120214           // lettura
038000120214           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
038100120216             TITPToccurs = TITPToccurs + 1;
038200120214             // punto all'occorrenza da valorizzare
038300120216             %occur(TITPT00F) = TITPToccurs;
038400120214           // fine lettura
038500120214           other;
038600120214             // non faccio nulla
038700120214           endsl;
038800120214         EndDo;
038900120214         // - Chiusura cursore
039000120214         exsr  sr_CloseCursor;
039100120214         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
039200120214         // le occorrenze davvero valorizzate sono inferiori di 1
039300120214         TITPToccurs = TITPToccurs - 1;
039400120214
039500120214         //reperisco dati TITGC
039600120214         //---------------------
039700120214         clear *all TITGC00F;
039800120214         // - Creazione stringa SQL da eseguire
039900120214         wSQL = 'select * from TITGC00F +
040000120214                 where TGCKSC = ' + %char(VIDKSC);
040100120214         // - Apertura cursore
040200120214         exsr  sr_OpenCursor;
040300120214         TITGCoccurs = 1;
040400120214         // punto all'occorrenza da valorizzare
040500120214         %occur(TITGC00F) = TITGCoccurs;
040600120214         DoU  SQLCode = 100 or SQLCode < 0;
040700120214           exec sql  fetch next  from C1  into :TITGC00F;
040800120214           exsr  sr_ReadCursor;
040900120214           select;
041000120214           // errore
041100120214           when SQLCode < 0;
041200120214             V1Dmsg = 'Errore in  lettura TITGC00F - vedi stampa errore';
041300120214             ErrBlock = *on;
041400120214             // - Chiusura cursore (visto che esco con leavesr)
041500120214             exsr  sr_CloseCursor;
041600120214             leavesr;
041700120214           // nessuna lettura
041800120214           when SQLCode = 100 and TITGCoccurs = 1;
041900120214             // non faccio nulla
042000120214           // lettura
042100120214           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
042200120214             TITGCoccurs = TITGCoccurs + 1;
042300120214             // punto all'occorrenza da valorizzare
042400120214             %occur(TITGC00F) = TITGCoccurs;
042500120214           // fine lettura
042600120214           other;
042700120214             // non faccio nulla
042800120214           endsl;
042900120214         EndDo;
043000120214         // - Chiusura cursore
043100120214         exsr  sr_CloseCursor;
043200120214         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
043300120214         // le occorrenze davvero valorizzate sono inferiori di 1
043400120214         TITGCoccurs = TITGCoccurs - 1;
043500120214
043600120214         //reperisco dati FNACR
043700120214         //---------------------
043800120214         clear *all FNACR00F;
043900120214         // - Creazione stringa SQL da eseguire
044000120214         wSQL = 'select * from FNACR00F +
044100120216                 where substr(digits(ACRCRO) , 1 , 7) = ' +
044200120216                 %editc(VIDKSC:'X');
044300120214         // - Apertura cursore
044400120214         exsr  sr_OpenCursor;
044500120214         FNACRoccurs = 1;
044600120214         // punto all'occorrenza da valorizzare
044700120214         %occur(FNACR00F) = FNACRoccurs;
044800120214         DoU  SQLCode = 100 or SQLCode < 0;
044900120214           exec sql  fetch next  from C1  into :FNACR00F;
045000120214           exsr  sr_ReadCursor;
045100120214           select;
045200120214           // errore
045300120214           when SQLCode < 0;
045400120214             V1Dmsg = 'Errore in  lettura FNACR00F - vedi stampa errore';
045500120214             ErrBlock = *on;
045600120214             // - Chiusura cursore (visto che esco con leavesr)
045700120214             exsr  sr_CloseCursor;
045800120214             leavesr;
045900120214           // nessuna lettura
046000120214           when SQLCode = 100 and FNACRoccurs = 1;
046100120214             // non faccio nulla
046200120214           // lettura
046300120214           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
046400120214             FNACRoccurs = FNACRoccurs + 1;
046500120214             // punto all'occorrenza da valorizzare
046600120214             %occur(FNACR00F) = FNACRoccurs;
046700120214           // fine lettura
046800120214           other;
046900120214             // non faccio nulla
047000120214           endsl;
047100120214         EndDo;
047200120214         // - Chiusura cursore
047300120214         exsr  sr_CloseCursor;
047400120214         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
047500120214         // le occorrenze davvero valorizzate sono inferiori di 1
047600120214         FNACRoccurs = FNACRoccurs - 1;
047700120214
047800120214         //reperisco dati FNACR1
047900120214         //---------------------
048000120214         clear *all FNACR10F;
048100120214         // - Creazione stringa SQL da eseguire
048200120216         wSQL = 'select * from FNACR10F +
048300120216                 where substr(digits(ACR1CRO) , 1 , 7) = ' +
048400120216                 %editc(VIDKSC:'X');
048500120214         // - Apertura cursore
048600120214         exsr  sr_OpenCursor;
048700120216         FNACR1occurs = 1;
048800120214         // punto all'occorrenza da valorizzare
048900120214         %occur(FNACR10F) = FNACR1occurs;
049000120214         DoU  SQLCode = 100 or SQLCode < 0;
049100120214           exec sql  fetch next  from C1  into :FNACR10F;
049200120214           exsr  sr_ReadCursor;
049300120214           select;
049400120214           // errore
049500120214           when SQLCode < 0;
049600120214             V1Dmsg = 'Errore in  lettura FNACR10F - vedi stampa errore';
049700120214             ErrBlock = *on;
049800120214             // - Chiusura cursore (visto che esco con leavesr)
049900120214             exsr  sr_CloseCursor;
050000120214             leavesr;
050100120214           // nessuna lettura
050200120214           when SQLCode = 100 and FNACR1occurs = 1;
050300120214             // non faccio nulla
050400120214           // lettura
050500120214           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
050600120214             FNACR1occurs = FNACR1occurs + 1;
050700120214             // punto all'occorrenza da valorizzare
050800120214             %occur(FNACR10F) = FNACR1occurs;
050900120214           // fine lettura
051000120214           other;
051100120214             // non faccio nulla
051200120214           endsl;
051300120214         EndDo;
051400120214         // - Chiusura cursore
051500120214         exsr  sr_CloseCursor;
051600120214         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
051700120214         // le occorrenze davvero valorizzate sono inferiori di 1
051800120214         FNACR1occurs = FNACR1occurs - 1;
051900120214
052000120214         //reperisco dati FNSPE
052100120214         //---------------------
052200120214         clear *all FNSPE00F;
052300120214         // - Creazione stringa SQL da eseguire
052400120214         wSQL = 'select * from FNSPE00F +
052500120214                 where SPECLI = ' + %char(VIDKSC) + ' and SPEFLS=''L''';
052600120214         // - Apertura cursore
052700120214         exsr  sr_OpenCursor;
052800120214         FNSPEoccurs = 1;
052900120214         // punto all'occorrenza da valorizzare
053000120214         %occur(FNSPE00F) = FNSPEoccurs;
053100120214         DoU  SQLCode = 100 or SQLCode < 0;
053200120214           exec sql  fetch next  from C1  into :FNSPE00F;
053300120214           exsr  sr_ReadCursor;
053400120214           select;
053500120214           // errore
053600120214           when SQLCode < 0;
053700120214             V1Dmsg = 'Errore in  lettura FNSPE00F - vedi stampa errore';
053800120214             ErrBlock = *on;
053900120214             // - Chiusura cursore (visto che esco con leavesr)
054000120214             exsr  sr_CloseCursor;
054100120214             leavesr;
054200120214           // nessuna lettura
054300120214           when SQLCode = 100 and FNSPEoccurs = 1;
054400120214             // non faccio nulla
054500120214           // lettura
054600120214           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
054700120214             FNSPEoccurs = FNSPEoccurs + 1;
054800120214             // punto all'occorrenza da valorizzare
054900120214             %occur(FNSPE00F) = FNSPEoccurs;
055000120214           // fine lettura
055100120214           other;
055200120214             // non faccio nulla
055300120214           endsl;
055400120214         EndDo;
055500120214         // - Chiusura cursore
055600120214         exsr  sr_CloseCursor;
055700120214         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
055800120214         // le occorrenze davvero valorizzate sono inferiori di 1
055900120214         FNSPEoccurs = FNSPEoccurs - 1;
056000120214
056100120214         //reperisco dati FNSP2
056200120214         //---------------------
056300120214         clear *all FNSP200F;
056400120214         // - Creazione stringa SQL da eseguire
056500120214         wSQL = 'select * from FNSP200F +
056600120214                 where SP2CLI = ' + %char(VIDKSC);
056700120214         // - Apertura cursore
056800120214         exsr  sr_OpenCursor;
056900120214         FNSP2occurs = 1;
057000120214         // punto all'occorrenza da valorizzare
057100120214         %occur(FNSP200F) = FNSP2occurs;
057200120214         DoU  SQLCode = 100 or SQLCode < 0;
057300120214           exec sql  fetch next  from C1  into :FNSP200F;
057400120214           exsr  sr_ReadCursor;
057500120214           select;
057600120214           // errore
057700120214           when SQLCode < 0;
057800120214             V1Dmsg = 'Errore in  lettura FNSP200F - vedi stampa errore';
057900120214             ErrBlock = *on;
058000120214             // - Chiusura cursore (visto che esco con leavesr)
058100120214             exsr  sr_CloseCursor;
058200120214             leavesr;
058300120214           // nessuna lettura
058400120214           when SQLCode = 100 and FNSP2occurs = 1;
058500120214             // non faccio nulla
058600120214           // lettura
058700120214           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
058800120214             FNSP2occurs = FNSP2occurs + 1;
058900120214             // punto all'occorrenza da valorizzare
059000120214             %occur(FNSP200F) = FNSP2occurs;
059100120214           // fine lettura
059200120214           other;
059300120214             // non faccio nulla
059400120214           endsl;
059500120214         EndDo;
059600120214         // - Chiusura cursore
059700120214         exsr  sr_CloseCursor;
059800120214         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
059900120214         // le occorrenze davvero valorizzate sono inferiori di 1
060000120214         FNSP2occurs = FNSP2occurs - 1;
060100120215
060200120215         //reperisco dati TFNTC
060300120215         //---------------------
060400120215         clear *all TFNTC00F;
060500120215         // - Creazione stringa SQL da eseguire
060600120215         wSQL = 'select * from TFNTC00F +
060700120216                 where NTCNK1 = ''0151' + %editc(VIDKSC:'X') +
060800120216                 ''' and NTCAPL = ''C''';
060900120215         // - Apertura cursore
061000120215         exsr  sr_OpenCursor;
061100120215         TFNTCoccurs = 1;
061200120215         // punto all'occorrenza da valorizzare
061300120215         %occur(TFNTC00F) = TFNTCoccurs;
061400120215         DoU  SQLCode = 100 or SQLCode < 0;
061500120215           exec sql  fetch next  from C1  into :TFNTC00F;
061600120215           exsr  sr_ReadCursor;
061700120215           select;
061800120215           // errore
061900120215           when SQLCode < 0;
062000120215             V1Dmsg = 'Errore in  lettura TFNTC00F - vedi stampa errore';
062100120215             ErrBlock = *on;
062200120215             // - Chiusura cursore (visto che esco con leavesr)
062300120215             exsr  sr_CloseCursor;
062400120215             leavesr;
062500120215           // nessuna lettura
062600120215           when SQLCode = 100 and TFNTCoccurs = 1;
062700120215             // non faccio nulla
062800120215           // lettura
062900120215           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
063000120215             TFNTCoccurs = TFNTCoccurs + 1;
063100120215             // punto all'occorrenza da valorizzare
063200120215             %occur(TFNTC00F) = TFNTCoccurs;
063300120215           // fine lettura
063400120215           other;
063500120215             // non faccio nulla
063600120215           endsl;
063700120215         EndDo;
063800120215         // - Chiusura cursore
063900120215         exsr  sr_CloseCursor;
064000120215         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
064100120215         // le occorrenze davvero valorizzate sono inferiori di 1
064200120215         TFNTCoccurs = TFNTCoccurs - 1;
064300120214
064400120214       ENDSR;
064500120214
064600120214       //--------------------------------------------------------------
064700120207       // Scrivo rcd estratti
064800111128       //--------------------------------------------------------------
064900120208       BEGSR  sr_ConnectToAS888;
065000111128
065100120210         ErrInsert = *off;
065200120210
065300120207         // mi connetto ad AS888
065400120207         exec sql  connect to :DBRemote user :VIDUSR using :VIDPWD;
065500120208         if SQLCode < 0;
065600120208           //emetto msg di errore perentorio
065700120216           V1Dmsg = 'FASE 2: non riuscita connessione ad AS888 +
065800120216                     - SCOLLEGARSI e riprovare';
065900120208           leavesr;
066000120208         endif;
066100120207
066200120208         // esecuzione scrittura rcd estratti
066300120208         exsr sr_InsertRcd;
066400120208
066500120208         // mi disconnetto da AS888
066600120208         exec sql  disconnect :DBRemote;
066700120208         // se la disconnessione ha dato errore
066800120208         if SQLCode < 0;
066900120208           //emetto msg di errore perentorio
067000120210           if ErrInsert = *on;
067100120216             V1Dmsg = 'FASE 2: agg. NON eseguito ED errore in +
067200120216                       connessione SETRAS - SCOLLEGARSI!';
067300120210           else;
067400120216             V1Dmsg = 'FASE 2: agg. eseguito ma errore in +
067500120210                       connessione SETRAS - SCOLLEGARSI!';
067600120210           endif;
067700120208           leavesr;
067800120208         endif;
067900120208
068000120216         // mi riconnetto a SETRAS
068100120216         // siccome in BRT gli SQLRPGLE si compilano col parametro RDBCNNMTH = *DUW, la connect ad AS888 NON ha chiuso
068200120216         // la connessione a SETRAS, l'ha resa dorminte. In questo caso NON serve un cmd CONNECT RESET perché  darebbe
068300120216         // errore SQL0842 = Connessione al database relazionale &1 già esistente.
068400120216         // Però, la connessione al DB locale (SETRAS) esiste se prima c'è stato almeno uno statement sql se quel DB.
068500120216         // In questo caso la SET CONNECTION dà errore SQL0842 = Il collegamento al database relazionale &1 non esiste
068600120216         // e va eseguita la CONNECT RESET.
068700120216         // in questo pgm dovrebbe sempre esserci prima uno statement SQL su SETRAS per leggere i dati,
068800120216         // ma non si sa mai... magari vengono fatte modifiche
068900120216         // Per cui, dopo aver disconnesso AS888, fare il cmd SET CONNECTION per "risvegliare" SETRAS e, se dà errore
069000120216         // provare con CONNECT RESET.
069100120216         // Solo se entrambi hanno dato errore, emettere un msg a video.
069200120216         exec sql  set connection SETRAS;
069300120216         if SQLCode < 0;
069400120216           // provo con CONNECT RESET
069500120216           exec sql  connect reset;
069600120216           // se anche in questo caso ho errore, emetto msg
069700120216           if SQLCode < 0;
069800120216             //emetto msg di errore perentorio
069900120216             if ErrInsert = *on;
070000120216               V1Dmsg = 'FASE 1: agg. NON eseguito ED errore in +
070100120216                         connessione SETRAS - SCOLLEGARSI!';
070200120216             else;
070300120216               V1Dmsg = 'FASE 1: agg. eseguito ma errore in +
070400120216                         connessione DB SETRAS - SCOLLEGARSI!';
070500120216             endif;
070600120216             leavesr;
070700120216           endif;
070800120216         endif;
070900120207
071000111128       ENDSR;
071100111128
071200120208       //--------------------------------------------------------------
071300120208       // Scrivo rcd estratti
071400120208       //--------------------------------------------------------------
071500120208       BEGSR  sr_InsertRcd;
071600120208
071700120215         // aggiungo i record letti al FNCBA00F di test
071800120215         if FNCBAoccurs > 0;
071900120227           EXEC SQL insert into UNITRAGRP/FNCBA00F :FNCBAoccurs rows
072000120227                    values(:FNCBA00F)
072100120210                    with UR;
072200120208           if SQLCode < 0;
072300120208             // se ho avuto un errore nell'insert, a regime sarà solo per chiave duplicata, ma non è un file vincolante
072400120208             // per cui lo segnalo solo ma proseguo
072500120215             V1Dmsg = 'Errore di copia sul file FNCBA00F +
072600120208                       - controllare';
072700120208           endif;
072800120208         endif;
072900120215
073000120215         // se ho almeno un rcd di TNTAM, devo avere almeno un rcd tra TITAD, TITPT, TITPD e TITGC
073100120215         if EsisteTNTAM = *on and
073200120215           TITADoccurs = 0 and TITPToccurs = 0 and TITPDoccurs = 0 and
073300120215           TITGCoccurs = 0;
073400120215           V1Dmsg = 'Copiato TNTAM ma non ci sono rcd su +
073500120215                     TITAD, TITPT, TITPD, TITGC';
073600120215         endif;
073700120215
073800120215         // aggiungo i record letti al TITAD00F di test
073900120215         if TITADoccurs > 0;
074000120227           EXEC SQL insert into UNITRAGRP/TITAD00F :TITADoccurs rows
074100120227                    values(:TITAD00F)
074200120215                    with UR;
074300120215           if SQLCode < 0;
074400120215             // se ho avuto un errore nell'insert, a regime sarà solo per chiave duplicata, ma non è un file vincolante
074500120215             // per cui lo segnalo solo ma proseguo
074600120215             V1Dmsg = 'Errore di copia sul file TITAD00F +
074700120215                       - controllare';
074800120215           endif;
074900120215         endif;
075000120215
075100120215         // aggiungo i record letti al TITPT00F di test
075200120215         if TITPToccurs > 0;
075300120227           EXEC SQL insert into UNITRAGRP/TITPT00F :TITPToccurs rows
075400120227                    values(:TITPT00F)
075500120215                    with UR;
075600120215           if SQLCode < 0;
075700120215             // se ho avuto un errore nell'insert, a regime sarà solo per chiave duplicata, ma non è un file vincolante
075800120215             // per cui lo segnalo solo ma proseguo
075900120215             V1Dmsg = 'Errore di copia sul file TITPT00F +
076000120215                       - controllare';
076100120215           endif;
076200120215         endif;
076300120215
076400120215         // aggiungo i record letti al TITPD00F di test
076500120215         if TITPDoccurs > 0;
076600120227           EXEC SQL insert into UNITRAGRP/TITPD00F :TITPDoccurs rows
076700120227                    values(:TITPD00F)
076800120215                    with UR;
076900120215           if SQLCode < 0;
077000120215             // se ho avuto un errore nell'insert, a regime sarà solo per chiave duplicata, ma non è un file vincolante
077100120215             // per cui lo segnalo solo ma proseguo
077200120215             V1Dmsg = 'Errore di copia sul file TITPD00F +
077300120215                       - controllare';
077400120215           endif;
077500120215         endif;
077600120215
077700120215         // aggiungo i record letti al TITGC00F di test
077800120215         if TITGCoccurs > 0;
077900120227           EXEC SQL insert into UNITRAGRP/TITGC00F :TITGCoccurs rows
078000120227                    values(:TITGC00F)
078100120215                    with UR;
078200120215           if SQLCode < 0;
078300120215             // se ho avuto un errore nell'insert, a regime sarà solo per chiave duplicata, ma non è un file vincolante
078400120215             // per cui lo segnalo solo ma proseguo
078500120215             V1Dmsg = 'Errore di copia sul file TITGC00F +
078600120215                       - controllare';
078700120215           endif;
078800120215         endif;
078900120215
079000120215         // aggiungo i record letti al FNACR00F di test
079100120215         if FNACRoccurs > 0;
079200120227           EXEC SQL insert into UNITRAGRP/FNACR00F :FNACRoccurs rows
079300120227                    values(:FNACR00F)
079400120215                    with UR;
079500120215           if SQLCode < 0;
079600120215             // se ho avuto un errore nell'insert, a regime sarà solo per chiave duplicata, ma non è un file vincolante
079700120215             // per cui lo segnalo solo ma proseguo
079800120215             V1Dmsg = 'Errore di copia sul file FNACR00F +
079900120215                       - controllare';
080000120215           endif;
080100120215         endif;
080200120215
080300120215         // aggiungo i record letti al FNACR10F di test
080400120215         if FNACR1occurs > 0;
080500120227           EXEC SQL insert into UNITRAGRP/FNACR10F :FNACR1occurs rows
080600120227                    values(:FNACR10F)
080700120215                    with UR;
080800120215           if SQLCode < 0;
080900120215             // se ho avuto un errore nell'insert, a regime sarà solo per chiave duplicata, ma non è un file vincolante
081000120215             // per cui lo segnalo solo ma proseguo
081100120215             V1Dmsg = 'Errore di copia sul file FNACR10F +
081200120215                       - controllare';
081300120215           endif;
081400120215         endif;
081500120215
081600120215         // aggiungo i record letti al FNSPE00F di test
081700120215         if FNSPEoccurs > 0;
081800120227           EXEC SQL insert into UNITRAGRP/FNSPE00F :FNSPEoccurs rows
081900120227                    values(:FNSPE00F)
082000120215                    with UR;
082100120215           if SQLCode < 0;
082200120215             // se ho avuto un errore nell'insert, a regime sarà solo per chiave duplicata, ma non è un file vincolante
082300120215             // per cui lo segnalo solo ma proseguo
082400120215             V1Dmsg = 'Errore di copia sul file FNSPE00F +
082500120215                       - controllare';
082600120215           endif;
082700120215         endif;
082800120215
082900120215         // aggiungo i record letti al FNSP200F di test
083000120215         if FNSP2occurs > 0;
083100120227           EXEC SQL insert into UNITRAGRP/FNSP200F :FNSP2occurs rows
083200120227                    values(:FNSP200F)
083300120215                    with UR;
083400120215           if SQLCode < 0;
083500120215             // se ho avuto un errore nell'insert, a regime sarà solo per chiave duplicata, ma non è un file vincolante
083600120215             // per cui lo segnalo solo ma proseguo
083700120215             V1Dmsg = 'Errore di copia sul file FNSP200F +
083800120215                       - controllare';
083900120215           endif;
084000120215         endif;
084100120215
084200120215         // aggiungo i record letti al TFNTC00F di test
084300120215         if TFNTCoccurs > 0;
084400120227           EXEC SQL insert into UNITRAGRP/TFNTC00F :TFNTCoccurs rows
084500120227                    values(:TFNTC00F)
084600120215                    with UR;
084700120215           if SQLCode < 0;
084800120215             // se ho avuto un errore nell'insert, a regime sarà solo per chiave duplicata, ma non è un file vincolante
084900120215             // per cui lo segnalo solo ma proseguo
085000120215             V1Dmsg = 'Errore di copia sul file TFNTC00F +
085100120215                       - controllare';
085200120215           endif;
085300120215         endif;
085400120208
085500120210         // se sono qui vuol dire che ho scritto tutto per cui committo
085600120210         exec sql commit;
085700120210
085800120208       ENDSR;
085900120208
086000111128       //--------------------------------------------------------------
086100120210       // Esecuzione del comando (già impostato)
086200111128       //--------------------------------------------------------------
086300111128       BEGSR  sr_ExecCmd;
086400111128
086500111128         clear Qcap0100;
086600111128         Qcabcsdh = *off;
086700111128         Qcapa    = *off;
086800111128         Qcacmdss = *off;
086900111128         Qcaerved = *allX'00';
087000111128
087100111128         clear Qusec;
087200111128         Qusbprv  = %size(Qusec);
087300111128
087400111128         ProcessCommands ( Qcmd : %len( %trimr( Qcmd ) ) : Qcap0100 :
087500111128                           %size(Qcap0100) : 'CPOP0100' : *omit :
087600111128                           0 : 0 : Qusec);
087700111128
087800120210         // - Stampa DUMP + JobLog  &  Chiusura *pgm  in caso di errore
087900111128         if  Qusei <> *blank;
088000111128           exsr  sr_PrintErr;
088100111128         endif;
088200111128
088300111128       ENDSR;
088400111128
088500111128       //--------------------------------------------------------------
088600120210       // Stampa segnalazione dell'errore rilevato
088700111128       //--------------------------------------------------------------
088800111128       BEGSR  sr_PrintErr;
088900111128
089000120210         // - Stampa del Dump
089100111128         Dump(A);
089200111128
089300120210         // - Stampa del Job-Log
089400111128         Qcmd = 'DSPJOBLOG job(*) output(*print)';
089500111128         exsr  sr_ExecCmd;
089600111128
089700111128       ENDSR;
089800111128
089900111128       //--------------------------------------------------------------
090000120208       // Operazioni finali
090100111128       //--------------------------------------------------------------
090200111128       BEGSR  sr_RoutEnd;
090300111128
090400120208         // - Uscita
090500111128         return;
090600111128
090700111128       ENDSR;
090800111128
090900111128      /end-free
