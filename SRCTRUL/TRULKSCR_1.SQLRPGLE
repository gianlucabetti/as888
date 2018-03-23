000100111128       //==============================================================
000200120208       // TRULKSCR - Aggiornamento cliente da SETRAS ad AS888
000300111128       //==============================================================
000400111128
000500120210       // OGNI volta che si compila ricordarsi:
000600120210       // - portare il pgm su SETRAS in una libreria che esista come nome anche su AS888
000700120210       // - creare su SETRAS il pacchetto SQL che risiederà su AS888:
000800120209       //   CRTSQLPKG PGM(TRULKSCR) RDB(AS888)
000900120209
001000111128       //--------------------------------------------------------------
001100120207       // Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
001200111128       //--------------------------------------------------------------
001300111128
001400111128     /*PRM  dbgview(*source)
001500111128     /*END
001600111128
001700111128       //--------------------------------------------------------------
001800120207       // Specifiche di controllo.                                     ?
001900111128       //--------------------------------------------------------------
002000111128
002100111128     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002200111128     h dftactgrp(*no)
002300111128     h alwnull(*inputonly)
002400111128
002500111128       //--------------------------------------------------------------
002600120207       // Dichiarazione file.                                          ?
002700111128       //--------------------------------------------------------------
002800120207       // - Video gestione tabella "SCK"
002900120207     fTRULKSCD  cf   e             workstn
003000120207     f                                     indds(IndDspF)
003100120207     f                                     infds(InfDspF)
003200111128
003300111128
003400111128       //--------------------------------------------------------------
003500120207       // Definizione costanti.                                        ?
003600111128       //--------------------------------------------------------------
003700120207
003800120207       // - Tasti funzionali a video
003900120207     d c_F01           c                   const(x'31')
004000120207     d c_F02           c                   const(x'32')
004100120207     d c_F03           c                   const(x'33')
004200120207     d c_F05           c                   const(x'35')
004300120207     d c_F06           c                   const(x'36')
004400120207     d c_F07           c                   const(x'37')
004500120207     d c_F08           c                   const(x'38')
004600120207     d c_F09           c                   const(x'39')
004700120207     d c_F10           c                   const(x'3A')
004800120207     d c_F12           c                   const(x'3C')
004900120207     d c_F13           c                   const(x'B1')
005000120207     d c_F14           c                   const(x'B2')
005100120207     d c_F15           c                   const(x'B3')
005200120207     d c_F16           c                   const(x'B4')
005300120207     d c_F17           c                   const(x'B5')
005400120207     d c_F18           c                   const(x'B6')
005500120207     d c_F19           c                   const(x'B7')
005600120207     d c_F20           c                   const(x'B8')
005700120207     d c_F21           c                   const(x'B9')
005800120207     d c_F22           c                   const(x'BA')
005900120207     d c_F23           c                   const(x'BB')
006000120207     d c_F24           c                   const(x'BC')
006100120207     d c_Enter         c                   const(x'F1')
006200120207     d c_RollDown      c                   const(x'F4')
006300120207     d c_RollUp        c                   const(x'F5')
006400111128
006500111128
006600111128       //--------------------------------------------------------------
006700120207       // Definizione schiere.                                         ?
006800111128       //--------------------------------------------------------------
006900111128
007000111128
007100111128       //--------------------------------------------------------------
007200120207       // Definizione aree dati.                                       ?
007300111128       //--------------------------------------------------------------
007400111128
007500111128
007600111128       //--------------------------------------------------------------
007700120207       // Definizione strutture dati.                                  ?
007800111128       //--------------------------------------------------------------
007900120207     d CNACO00F      e ds                  occurs(1)
008000120208     d CNCLP00F      e ds                  occurs(1)
008100120208     d CNIND00F      e ds                  occurs(1)
008200120208     d FNCLS00F      e ds                  occurs(1)
008300120208     d TABEL00F      e ds                  occurs(20)
008400120208     d TNTAM00F      e ds                  occurs(50)
008500120208     d TNTBE00F      e ds                  occurs(20)
008600111128
008700111128       //--------------------------------------------------------------
008800120207       // Definizione variabili globali.                               ?
008900111128       //--------------------------------------------------------------
009000111128
009100120208       // - Stringa SQL da eseguire
009200111128     d wSQl            s           1024    inz  varying
009300111128
009400120208       // - Parametri SQL
009500120207
009600120207       // - InfDS
009700120207     d InfDspF         ds
009800120207     d   dsp_aid             369    369a                                        AID byte
009900120207
010000120207       // - Indicatori su DspF
010100120207     d IndDspF         ds
010200120207        // - Indicatori di errore
010300120208     d  ErrMsg                        1n   overlay(IndDspF : 28)
010400120207     d  ErrGenerico                   1n   overlay(IndDspF : 99)
010500120207
010600120207       // - Flags booleani
010700120207     d $Fine           s               n   inz(*off)
010800120207     d $InzD01         s               n   inz(*on)
010900120208     d ErrBlock        s               n   inz(*off)
011000120210     d ErrInsert       s               n   inz(*off)
011100120210     d Qcmd            s            500    inz
011200120207
011300120207       // - Campi associati al video
011400120207     d $Video          s              2    inz('D1')
011500120207
011600120207       // - Variabili di appoggio
011700120207     d DBRemote        s             10    inz('AS888')
011800120208     d CNACOOccurs     s              3s 0
011900120208     d CNCLPOccurs     s              3s 0
012000120208     d CNINDOccurs     s              3s 0
012100120208     d FNCLSOccurs     s              3s 0
012200120208     d TABELOccurs     s              3s 0
012300120208     d TNTAMOccurs     s              3s 0
012400120208     d TNTBEOccurs     s              3s 0
012500111128
012600111128       //--------------------------------------------------------------
012700120210       // Definizione procedure usate
012800111128       //--------------------------------------------------------------
012900120207
013000120210       // - Parametri API QCAPCMD (Process Commands)
013100120207      /copy qSysInc/qRpgleSrc,QCAPCMD
013200120207      /copy gaitrasrc/srcProtoPR,QCAPCMD
013300120207
013400120210       // - Parametri gestione errori API
013500120207      /copy qSysInc/qRpgleSrc,QUSEC
013600111128
013700111128       //--------------------------------------------------------------
013800120208       // Definizione key-list.                                        ?
013900111128       //--------------------------------------------------------------
014000111128
014100111128
014200111128       //--------------------------------------------------------------
014300120208       // M A I N - L I N E                                            ?
014400111128       //--------------------------------------------------------------
014500111128
014600111128      /free
014700111128
014800120208       // - Operazioni iniziali
014900111128       exsr  sr_RoutInz;
015000111128
015100120207       // - Gestione video
015200120207       DOW  $Fine = *off;
015300120207         select;
015400120207           when  $Video = 'D1';
015500120207             exsr  sr_GesD01;
015600120207           other;
015700120207             $Fine = *on;
015800120207         endsl;
015900120207       ENDDO;
016000120207
016100120208       // - Operazioni finali
016200111128       exsr  sr_RoutEnd;
016300111128
016400111128       //--------------------------------------------------------------
016500120208       // Operazioni iniziali
016600111128       //--------------------------------------------------------------
016700111128       BEGSR sr_RoutInz;
016800111128
016900111128         *inLR = *on;
017000120207         IndDspF  = *off;
017100111128
017200120208         // - Impostazione opzioni per SQL
017300111128         exec SQL   set option   DynUsrPrf = *owner,
017400111128                                 CloSqlCsr = *endmod;
017500111128
017600111128
017700111128       ENDSR;
017800111128
017900111128       //--------------------------------------------------------------
018000120208       // Apertura cursore C1
018100111128       //--------------------------------------------------------------
018200111128       BEGSR  sr_OpenCursor;
018300111128
018400120208         // - Dichiarazione del cursore
018500111128         exec sql  prepare S1  from :wSQL;
018600111128         exec sql  declare C1  cursor for S1;
018700111128
018800120208         // - Apertura del cursore
018900111128         exec sql   open C1;
019000111128
019100111128       ENDSR;
019200111128
019300111128       //--------------------------------------------------------------
019400120208       // Chiusura cursore C1
019500111128       //--------------------------------------------------------------
019600111128       BEGSR  sr_CloseCursor;
019700111128
019800120208         // - Chiusura del cursore
019900111128         exec sql   close C1;
020000111128
020100111128       ENDSR;
020200111128
020300111128       //--------------------------------------------------------------
020400120208       // Lettura cursore C1
020500111128       //--------------------------------------------------------------
020600111128       BEGSR  sr_ReadCursor;
020700111128
020800111128         select;
020900111128
021000120208           // - Fine lettura
021100111128           when  sqlCode = 100;
021200120208             //non devo fare nulla perché devo scriveree tutti i rcd di tutti i cursori letti
021300120208           leavesr;
021400111128
021500120208           // - Errore
021600111128           when  sqlCode < *zero;
021700111128             exsr  sr_PrintErr;
021800111128
021900120208           // - Elaborazione
022000111128           other;
022100120208             //non devo fare nulla adesso perché prima devo leggere tutti i rcd di tutti i cursori
022200111128
022300111128         endsl;
022400111128
022500111128       ENDSR;
022600120207
022700120207       //--------------------------------------------------------------
022800120208       // Gestione videata D01
022900120207       //--------------------------------------------------------------
023000120207       BEGSR  sr_GesD01;
023100120207
023200120207         // - Inizializzazione videata
023300120207         if  $InzD01   = *on;
023400120207           exsr  sr_InzD01;
023500120207           $InzD01  = *off;
023600120207         endif;
023700120207
023800120207         // - Emissione Testata e Piede con tasti funzionali abilitati
023900120208         if  ErrGenerico = *off and ErrMsg = *off;
024000120207           write  trkuT01;
024100120207           write  trkuP01;
024200120207         endif;
024300120207
024400120207         // - Emissione videata
024500120207         exfmt  trkuD01;
024600120207
024700120208         reset  ErrMsg;
024800120207         reset  ErrGenerico;
024900120207         clear  V1Dmsg;
025000120207
025100120207         SELECT;
025200120207
025300120207           // - F3=Fine
025400120207           WHEN  dsp_aid = c_F03;
025500120207             exsr sr_F03D01;
025600120207
025700120207           // - F12=Ritorno
025800120207           WHEN  dsp_aid = c_F12;
025900120207             exsr sr_F12D01;
026000120207
026100120207           // - Invio / F6=Conferma
026200120207           OTHER;
026300120207             exsr  sr_CtrD01;
026400120207             if  ErrGenerico = *on;
026500120207               leavesr;
026600120207             endif;
026700120207             if  dsp_aid = c_F06;
026800120207               exsr sr_F06D01;
026900120207             endif;
027000120207
027100120207         ENDSL;
027200120207
027300120207       ENDSR;
027400120207
027500120207       //--------------------------------------------------------------
027600120208       // Inizializzazione videata D01
027700120207       //--------------------------------------------------------------
027800120207       BEGSR  sr_InzD01;
027900120207
028000120207         // - Pulizia videata
028100120207         clear  trkuD01;
028200120207
028300120207       ENDSR;
028400120207
028500120207       //--------------------------------------------------------------
028600120207       //?Gestione tasto funzionale F3 da videata D01                  ?
028700120207       //?F3=Fine                                                      ?
028800120207       //--------------------------------------------------------------
028900120207       BEGSR  sr_F03D01;
029000120207
029100120207         // - Restituzione indicazione del tasto premuto al chiamante
029200120207         //OKPJopz = 'F03';
029300120207
029400120207         // - Chiusura del programma
029500120207         $Fine = *on;
029600120207
029700120207       ENDSR;
029800120207
029900120207       //--------------------------------------------------------------
030000120208       // Gestione tasto funzionale F6 da videata D01
030100120208       // F6=Conferma
030200120207       //--------------------------------------------------------------
030300120207       BEGSR  sr_F06D01;
030400120208
030500120208         // - Copio i dati da aggiungere su AS888
030600120208         exsr  sr_CopiaDatiCliente;
030700120208         if ErrBlock = *on;
030800120208           leavesr;
030900120208         endif;
031000120207
031100120208         // - Scrivo dati da aggiungere se AS888
031200120208         exsr  sr_ConnectToAS888;
031300120207
031400120207         // - Restituzione indicazione del tasto premuto al chiamante
031500120207         //OKPJopz = 'F06';
031600120207
031700120208         // - Chiusura del programma (solo se non ho errori da mostrare)
031800120208         if ErrMsg = *off;
031900120208           $Fine = *on;
032000120208         endif;
032100120207
032200120207       ENDSR;
032300120208
032400120207       //--------------------------------------------------------------
032500120208       // Gestione tasto funzionale F12 da videata D01
032600120208       // F12=Ritorno
032700120207       //--------------------------------------------------------------
032800120207       BEGSR  sr_F12D01;
032900120207
033000120207         // - Restituzione indicazione del tasto premuto al chiamante
033100120207         //OKPJopz = 'F12';
033200120207
033300120207         // - Chiusura del programma
033400120207         $Fine = *on;
033500120207
033600120207       ENDSR;
033700120207
033800120207       //--------------------------------------------------------------
033900120207       // Controllo videata D01                                        ?
034000120207       //--------------------------------------------------------------
034100120207       BEGSR  sr_CtrD01;
034200120207
034300120207         IndDspF  = *off;
034400120208
034500120208         // l'utente remoto va indicato
034600120208         if VIDUsr = *blank;
034700120208           V1Dmsg = 'Utente remoto obbligatorio';
034800120208           ErrMsg = *on;
034900120208           ErrGenerico = *on;
035000120208           leavesr;
035100120208         endif;
035200120208
035300120208         // la pwd dell'utente remoto va indicata
035400120208         if VIDPwd = *blank;
035500120208           V1Dmsg = 'Pwd utente remoto obbligatoria';
035600120208           ErrMsg = *on;
035700120208           ErrGenerico = *on;
035800120208           leavesr;
035900120208         endif;
036000120208
036100120208         // il cliente va indicato
036200120208         if VIDKsc = 0;
036300120208           V1Dmsg = 'Cliente obbligatorio';
036400120208           ErrMsg = *on;
036500120208           ErrGenerico = *on;
036600120208           leavesr;
036700120208         endif;
036800120207
036900120207       ENDSR;
037000120208
037100120208       //--------------------------------------------------------------
037200120208       // Reperisco i dati da copiare
037300120208       //--------------------------------------------------------------
037400120208       BEGSR  sr_CopiaDatiCliente;
037500120208
037600120208         ErrBlock = *off;
037700120208
037800120208         // - Reperisci dati da aggiungere
037900120208
038000120208         //reperisco dati CNACO
038100120210         //---------------------
038200120208         clear *all CNACO00F;
038300120208         // - Creazione stringa SQL da eseguire
038400120208         wSQL = 'select * from CNACO00F +
038500120209                 where ACOKSC = ' + %char(VIDKSC) + ' and ACOKCC=151';
038600120208         // - Apertura cursore
038700120208         exsr  sr_OpenCursor;
038800120208         CNACOoccurs = 1;
038900120209         // punto all'occorrenza da valorizzare
039000120209         %occur(CNACO00F) = CNACOoccurs;
039100120208         DoU  SQLCode = 100 or SQLCode < 0;
039200120209           exec sql  fetch next  from C1  into :CNACO00F;
039300120208           exsr  sr_ReadCursor;
039400120208           // se non viene letto rcd alla prima lettura, significa che non esiste un cliente con quel codice
039500120210           // errore bloccante
039600120208           select;
039700120209           // errore
039800120209           when SQLCode < 0;
039900120209             V1Dmsg = 'Errore in  lettura CNACO00F - vedi stampa errore';
040000120208             ErrMsg = *on;
040100120208             ErrGenerico = *on;
040200120208             ErrBlock = *on;
040300120208             // - Chiusura cursore (visto che esco con leavesr)
040400120208             exsr  sr_CloseCursor;
040500120208             leavesr;
040600120209           // nessuna lettura
040700120209           when SQLCode = 100 and CNACOoccurs = 1;
040800120209             V1Dmsg = 'Cliente inesistente';
040900120209             ErrMsg = *on;
041000120209             ErrGenerico = *on;
041100120209             ErrBlock = *on;
041200120209             // - Chiusura cursore (visto che esco con leavesr)
041300120209             exsr  sr_CloseCursor;
041400120209             leavesr;
041500120209           // lettura
041600120209           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
041700120208             CNACOoccurs = CNACOoccurs + 1;
041800120209             // punto all'occorrenza da valorizzare - per CNACO00F non lo faccio perché ha 1 sola occorrenza
041900120209             // %occur(CNACO00F) = CNACOoccurs;
042000120209           // fine lettura
042100120209           other;
042200120209             // non faccio nulla
042300120209           endsl;
042400120208         EndDo;
042500120208         // - Chiusura cursore
042600120208         exsr  sr_CloseCursor;
042700120209         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
042800120209         // le occorrenze davvero valorizzate sono inferiori di 1
042900120209         CNACOoccurs = CNACOoccurs - 1;
043000120208
043100120208         //reperisco dati CNCLP
043200120210         //---------------------
043300120208         clear *all CNCLP00F;
043400120208         // - Creazione stringa SQL da eseguire
043500120208         wSQL = 'select * from CNCLP00F +
043600120209                 where CLPKSC = ' + %char(VIDKSC) + ' and CLPKCC=151';
043700120208         // - Apertura cursore
043800120208         exsr  sr_OpenCursor;
043900120208         CNCLPoccurs = 1;
044000120209         // punto all'occorrenza da valorizzare
044100120209         %occur(CNCLP00F) = CNCLPoccurs;
044200120208         DoU  SQLCode = 100 or SQLCode < 0;
044300120208           exec sql  fetch next  from C1  into :CNCLP00F;
044400120208           exsr  sr_ReadCursor;
044500120208           // se non viene letto rcd alla prima lettura, significa che non esiste un cliente con quel codice
044600120208           // improbabile avere un errore del genere quì, si sarebbe dovuto bloccare prima su CNACO00F
044700120210           // però è accaduto!
044800120210           // errore bloccante
044900120209           select;
045000120209           // errore
045100120209           when SQLCode < 0;
045200120209             V1Dmsg = 'Errore in  lettura CNCLP00F - vedi stampa errore';
045300120209             ErrMsg = *on;
045400120209             ErrGenerico = *on;
045500120209             ErrBlock = *on;
045600120209             // - Chiusura cursore (visto che esco con leavesr)
045700120209             exsr  sr_CloseCursor;
045800120209             leavesr;
045900120209           // nessuna lettura
046000120209           when SQLCode = 100 and CNCLPoccurs = 1;
046100120210             V1Dmsg = 'Cliente non inserito completamente';
046200120210             ErrMsg = *on;
046300120210             ErrGenerico = *on;
046400120210             ErrBlock = *on;
046500120210             // - Chiusura cursore (visto che esco con leavesr)
046600120210             exsr  sr_CloseCursor;
046700120210             leavesr;
046800120209           // lettura
046900120209           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
047000120209             CNCLPoccurs = CNCLPoccurs + 1;
047100120209             // punto all'occorrenza da valorizzare - per CNCLP00F non lo faccio perché ha 1 sola occorrenza
047200120209             // %occur(CNCLP00F) = CNCLPoccurs;
047300120209           // fine lettura
047400120209           other;
047500120209             // non faccio nulla
047600120209           endsl;
047700120208         EndDo;
047800120208         // - Chiusura cursore
047900120208         exsr  sr_CloseCursor;
048000120209         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
048100120209         // le occorrenze davvero valorizzate sono inferiori di 1
048200120209         CNCLPoccurs = CNCLPoccurs - 1;
048300120208
048400120208         //reperisco dati CNIND
048500120210         //---------------------
048600120208         clear *all CNIND00F;
048700120208         // - Creazione stringa SQL da eseguire
048800120208         wSQL = 'select * from CNIND00F +
048900120209                 where INDKSC = ' + %char(VIDKSC) + ' and INDKCC=151';
049000120208         // - Apertura cursore
049100120208         exsr  sr_OpenCursor;
049200120208         CNINDoccurs = 1;
049300120209         // punto all'occorrenza da valorizzare
049400120209         %occur(CNIND00F) = CNINDoccurs;
049500120208         DoU  SQLCode = 100 or SQLCode < 0;
049600120208           exec sql  fetch next  from C1  into :CNIND00F;
049700120208           exsr  sr_ReadCursor;
049800120208           // se non viene letto rcd alla prima lettura, significa che non esiste un cliente con quel codice
049900120208           // improbabile avere un errore del genere quì, si sarebbe dovuto bloccare prima su CNACO00F
050000120210           // però è accaduto!
050100120210           // errore bloccante
050200120209           select;
050300120209           // errore
050400120209           when SQLCode < 0;
050500120209             V1Dmsg = 'Errore in  lettura CNIND00F - vedi stampa errore';
050600120209             ErrMsg = *on;
050700120209             ErrGenerico = *on;
050800120209             ErrBlock = *on;
050900120209             // - Chiusura cursore (visto che esco con leavesr)
051000120209             exsr  sr_CloseCursor;
051100120209             leavesr;
051200120209           // nessuna lettura
051300120209           when SQLCode = 100 and CNINDoccurs = 1;
051400120210             V1Dmsg = 'Cliente non inserito completamente';
051500120210             ErrMsg = *on;
051600120210             ErrGenerico = *on;
051700120210             ErrBlock = *on;
051800120210             // - Chiusura cursore (visto che esco con leavesr)
051900120210             exsr  sr_CloseCursor;
052000120210             leavesr;
052100120209           // lettura
052200120209           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
052300120208             CNINDoccurs = CNINDoccurs + 1;
052400120209             // punto all'occorrenza da valorizzare - per CNIND00F non lo faccio perché ha 1 sola occorrenza
052500120209             // %occur(CNIND00F) = CNINDoccurs;
052600120209           // fine lettura
052700120209           other;
052800120209             // non faccio nulla
052900120209           endsl;
053000120208         EndDo;
053100120208         // - Chiusura cursore
053200120208         exsr  sr_CloseCursor;
053300120209         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
053400120209         // le occorrenze davvero valorizzate sono inferiori di 1
053500120209         CNINDoccurs = CNINDoccurs - 1;
053600120208
053700120208         //reperisco dati FNCLS
053800120210         //---------------------
053900120208         clear *all FNCLS00F;
054000120208         // - Creazione stringa SQL da eseguire
054100120208         wSQL = 'select * from FNCLS00F +
054200120208                 where CLSKSC = ' + %char(VIDKSC);
054300120208         // - Apertura cursore
054400120208         exsr  sr_OpenCursor;
054500120208         FNCLSoccurs = 1;
054600120209         // punto all'occorrenza da valorizzare
054700120209         %occur(FNCLS00F) = FNCLSoccurs;
054800120208         DoU  SQLCode = 100 or SQLCode < 0;
054900120208           exec sql  fetch next  from C1  into :FNCLS00F;
055000120208           exsr  sr_ReadCursor;
055100120208           // se non viene letto rcd alla prima lettura, significa che non esiste un cliente con quel codice
055200120208           // improbabile avere un errore del genere quì, si sarebbe dovuto bloccare prima su CNACO00F
055300120210           // però è accaduto!
055400120210           // errore bloccante
055500120209           select;
055600120209           // errore
055700120209           when SQLCode < 0;
055800120209             V1Dmsg = 'Errore in  lettura FNCLS00F - vedi stampa errore';
055900120209             ErrMsg = *on;
056000120209             ErrGenerico = *on;
056100120209             ErrBlock = *on;
056200120209             // - Chiusura cursore (visto che esco con leavesr)
056300120209             exsr  sr_CloseCursor;
056400120209             leavesr;
056500120209           // nessuna lettura
056600120209           when SQLCode = 100 and FNCLSoccurs = 1;
056700120210             V1Dmsg = 'Cliente non inserito completamente';
056800120210             ErrMsg = *on;
056900120210             ErrGenerico = *on;
057000120210             ErrBlock = *on;
057100120210             // - Chiusura cursore (visto che esco con leavesr)
057200120210             exsr  sr_CloseCursor;
057300120210             leavesr;
057400120209           // lettura
057500120209           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
057600120208             FNCLSoccurs = FNCLSoccurs + 1;
057700120209             // punto all'occorrenza da valorizzare - per FNCLS00F non lo faccio perché ha 1 sola occorrenza
057800120209             // %occur(FNCLS00F) = FNCLSoccurs;
057900120209           // fine lettura
058000120209           other;
058100120209             // non faccio nulla
058200120209           endsl;
058300120208         EndDo;
058400120208         // - Chiusura cursore
058500120208         exsr  sr_CloseCursor;
058600120209         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
058700120209         // le occorrenze davvero valorizzate sono inferiori di 1
058800120209         FNCLSoccurs = FNCLSoccurs - 1;
058900120208
059000120208         //reperisco dati TABEL
059100120210         //---------------------
059200120208         clear *all TABEL00F;
059300120208         // - Creazione stringa SQL da eseguire
059400120209         wSQL = 'select * from TABEL00F +
059500120209                 where TBLKEY like ''%' + %char(VIDKSC) + '%''';
059600120208         // - Apertura cursore
059700120208         exsr  sr_OpenCursor;
059800120209         TABELoccurs = 1;
059900120209         // punto all'occorrenza da valorizzare
060000120209         %occur(TABEL00F) = TABELoccurs;
060100120208         DoU  SQLCode = 100 or SQLCode < 0;
060200120208           exec sql  fetch next  from C1  into :TABEL00F;
060300120208           exsr  sr_ReadCursor;
060400120208           // è possibile che non venga letto rcd alla prima lettura
060500120208           // quindi non blocco il flusso
060600120209           select;
060700120209           // errore
060800120209           when SQLCode < 0;
060900120209             V1Dmsg = 'Errore in  lettura TABEL00F - vedi stampa errore';
061000120209             ErrMsg = *on;
061100120209             ErrGenerico = *on;
061200120209             ErrBlock = *on;
061300120209             // - Chiusura cursore (visto che esco con leavesr)
061400120209             exsr  sr_CloseCursor;
061500120209             leavesr;
061600120209           // nessuna lettura
061700120209           when SQLCode = 100 and TABELoccurs = 1;
061800120209             // non faccio nulla
061900120209           // lettura
062000120209           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
062100120208             TABELoccurs = TABELoccurs + 1;
062200120209             // punto all'occorrenza da valorizzare
062300120209             %occur(TABEL00F) = TABELoccurs;
062400120209           // fine lettura
062500120209           other;
062600120209             // non faccio nulla
062700120209           endsl;
062800120208         EndDo;
062900120208         // - Chiusura cursore
063000120208         exsr  sr_CloseCursor;
063100120209         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
063200120209         // le occorrenze davvero valorizzate sono inferiori di 1
063300120209         TABELoccurs = TABELoccurs - 1;
063400120208
063500120208         //reperisco dati TNTAM
063600120210         //---------------------
063700120208         clear *all TNTAM00F;
063800120208         // - Creazione stringa SQL da eseguire
063900120208         wSQL = 'select * from TNTAM00F +
064000120208                 where TAMKSC =' + %char(VIDKSC);
064100120208         // - Apertura cursore
064200120208         exsr  sr_OpenCursor;
064300120208         TNTAMoccurs = 1;
064400120209         // punto all'occorrenza da valorizzare
064500120209         %occur(TNTAM00F) = TNTAMoccurs;
064600120208         DoU  SQLCode = 100 or SQLCode < 0;
064700120208           exec sql  fetch next  from C1  into :TNTAM00F;
064800120208           exsr  sr_ReadCursor;
064900120208           // è possibile che non venga letto rcd alla prima lettura (cliente totalmente nuovo)
065000120208           // quindi non blocco il flusso
065100120209           select;
065200120209           // errore
065300120209           when SQLCode < 0;
065400120209             V1Dmsg = 'Errore in  lettura TNTAM00F - vedi stampa errore';
065500120209             ErrMsg = *on;
065600120209             ErrGenerico = *on;
065700120209             ErrBlock = *on;
065800120209             // - Chiusura cursore (visto che esco con leavesr)
065900120209             exsr  sr_CloseCursor;
066000120209             leavesr;
066100120209           // nessuna lettura
066200120209           when SQLCode = 100 and TNTAMoccurs = 1;
066300120209             // non faccio nulla
066400120209           // lettura
066500120209           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
066600120208             TNTAMoccurs = TNTAMoccurs + 1;
066700120209             // punto all'occorrenza da valorizzare
066800120209             %occur(TNTAM00F) = TNTAMoccurs;
066900120209           // fine lettura
067000120209           other;
067100120209             // non faccio nulla
067200120209           endsl;
067300120208         EndDo;
067400120208         // - Chiusura cursore
067500120208         exsr  sr_CloseCursor;
067600120209         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
067700120209         // le occorrenze davvero valorizzate sono inferiori di 1
067800120209         TNTAMoccurs = TNTAMoccurs - 1;
067900120208
068000120208         //reperisco dati TNTBE
068100120210         //---------------------
068200120208         clear *all TNTBE00F;
068300120208         // - Creazione stringa SQL da eseguire
068400120208         wSQL = 'select * from TNTBE00F +
068500120209                 where TBEKE1 like ''%' + %char(VIDKSC) + '%'' +
068600120209                 or TBEKE2 like ''%'
068700120209                  + %char(VIDKSC) + '%''';
068800120208         // - Apertura cursore
068900120208         exsr  sr_OpenCursor;
069000120209         TNTBEoccurs = 1;
069100120209         // punto all'occorrenza da valorizzare
069200120209         %occur(TNTBE00F) = TNTBEoccurs;
069300120208         DoU  SQLCode = 100 or SQLCode < 0;
069400120208           exec sql  fetch next  from C1  into :TNTBE00F;
069500120208           exsr  sr_ReadCursor;
069600120208           // è possibile che non venga letto rcd alla prima lettura
069700120208           // quindi non blocco il flusso
069800120209           select;
069900120209           // errore
070000120209           when SQLCode < 0;
070100120209             V1Dmsg = 'Errore in  lettura TNTBE00F - vedi stampa errore';
070200120209             ErrMsg = *on;
070300120209             ErrGenerico = *on;
070400120209             ErrBlock = *on;
070500120209             // - Chiusura cursore (visto che esco con leavesr)
070600120209             exsr  sr_CloseCursor;
070700120209             leavesr;
070800120209           // nessuna lettura
070900120209           when SQLCode = 100 and TNTBEoccurs = 1;
071000120209             // non faccio nulla
071100120209           // lettura
071200120209           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
071300120208             TNTBEoccurs = TNTBEoccurs + 1;
071400120209             // punto all'occorrenza da valorizzare
071500120209             %occur(TNTBE00F) = TNTBEoccurs;
071600120209           // fine lettura
071700120209           other;
071800120209             // non faccio nulla
071900120209           endsl;
072000120208         EndDo;
072100120208         // - Chiusura cursore
072200120208         exsr  sr_CloseCursor;
072300120209         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
072400120209         // le occorrenze davvero valorizzate sono inferiori di 1
072500120209         TNTBEoccurs = TNTBEoccurs - 1;
072600120208
072700120208       ENDSR;
072800111128
072900111128       //--------------------------------------------------------------
073000120207       // Scrivo rcd estratti
073100111128       //--------------------------------------------------------------
073200120208       BEGSR  sr_ConnectToAS888;
073300111128
073400120210         ErrMsg = *off;
073500120210         ErrGenerico = *off;
073600120210         ErrInsert = *off;
073700120210
073800120207         // mi connetto ad AS888
073900120207         exec sql  connect to :DBRemote user :VIDUSR using :VIDPWD;
074000120208         if SQLCode < 0;
074100120208           //emetto msg di errore perentorio
074200120208           V1Dmsg = 'ATTENZIONE: non riuscita connessione ad AS888 +
074300120208                     - scollegarsi e riprovare';
074400120208           ErrMsg = *on;
074500120208           ErrGenerico = *on;
074600120208           leavesr;
074700120208         endif;
074800120207
074900120208         // esecuzione scrittura rcd estratti
075000120208         exsr sr_InsertRcd;
075100120208
075200120208         // mi disconnetto da AS888
075300120208         exec sql  disconnect :DBRemote;
075400120208         // se la disconnessione ha dato errore
075500120208         if SQLCode < 0;
075600120208           //emetto msg di errore perentorio
075700120210           if ErrInsert = *on;
075800120210             V1Dmsg = 'ATTENZIONE: agg. NON eseguito ED errore in +
075900120210                       connessione SETRAS - SCOLLEGARSI!';
076000120210           else;
076100120210             V1Dmsg = 'ATTENZIONE: agg. eseguito ma errore in +
076200120210                       connessione SETRAS - SCOLLEGARSI!';
076300120210           endif;
076400120208           ErrMsg = *on;
076500120208           ErrGenerico = *on;
076600120208           leavesr;
076700120208         endif;
076800120208
076900120208         // mi riconnetto a SETRAS
077000120210         // siccome in BRT gli SQLRPGLE si compilano col parametro RDBCNNMTH = *DUW, la connect ad AS888 NON ha chiuso
077100120210         // la connessione a SETRAS, l'ha resa dorminte. In questo caso NON serve un cmd CONNECT RESET perché  darebbe
077200120210         // errore SQL0842 = Connessione al database relazionale &1 già esistente.
077300120210         // Basta, dopo aver disconnesso AS888, fare il cmd SET CONNECTION per "risvegliare" SETRAS
077400120210         //exec sql  connect reset;
077500120210         //if SQLCode < 0;
077600120208           //emetto msg di errore perentorio
077700120210         //  if ErrInsert = *on;
077800120210         //    V1Dmsg = 'ATTENZIONE: agg. NON eseguito ED errore in +
077900120210         //              connessione SETRAS - SCOLLEGARSI!';
078000120210         //  else;
078100120210         //    V1Dmsg = 'ATTENZIONE: agg. eseguito ma errore in +
078200120210         //              connessione DB SETRAS - SCOLLEGARSI!';
078300120210         //  endif;
078400120210         //  ErrMsg = *on;
078500120210         //  ErrGenerico = *on;
078600120210         //  leavesr;
078700120210         //endif;
078800120208         exec sql  set connection SETRAS;
078900120208         if SQLCode < 0;
079000120208           //emetto msg di errore perentorio
079100120210           if ErrInsert = *on;
079200120210             V1Dmsg = 'ATTENZIONE: agg. NON eseguito ED errore in +
079300120210                       connessione SETRAS - SCOLLEGARSI!';
079400120210           else;
079500120210             V1Dmsg = 'ATTENZIONE: agg. eseguito ma errore in +
079600120210                       connessione DB SETRAS - SCOLLEGARSI!';
079700120210           endif;
079800120208           ErrMsg = *on;
079900120208           ErrGenerico = *on;
080000120208           leavesr;
080100120208         endif;
080200120207
080300111128       ENDSR;
080400111128
080500120208       //--------------------------------------------------------------
080600120208       // Scrivo rcd estratti
080700120208       //--------------------------------------------------------------
080800120208       BEGSR  sr_InsertRcd;
080900120208
081000120210         // aggiungo il record letto al CNACO00F di test
081100120210         EXEC SQL insert into CNACO00F :CNACOoccurs rows values(:CNACO00F)
081200120210                  with UR;
081300120210         if SQLCode < 0;
081400120210         // se ho avuto un errore nell'insert, a regime sarà solo per chiave duplicata, quindi interrompo la copia
081500120210         // dando errore
081600120210           exec sql rollback;
081700120210           V1Dmsg = 'Cliente già esistente su AS888 +
081800120210                     - controllare';
081900120210           ErrMsg = *on;
082000120210           ErrGenerico = *on;
082100120210           ErrInsert = *on;
082200120210           leavesr;
082300120210         endif;
082400120210
082500120210         // aggiungo il record letto al CNCLP00F di test
082600120210         if CNCLPoccurs > 0;
082700120210           EXEC SQL insert into CNCLP00F :CNCLPoccurs rows values(:CNCLP00F)
082800120210                    with UR;
082900120210           if SQLCode < 0;
083000120210             // improbabile avere un errore nell'insert, si sarebbe dovuto bloccare prima su CNACO00F
083100120210             // ma lo testo lo stesso per il rollback
083200120210             exec sql rollback;
083300120210             V1Dmsg = 'Errore in insert CNCLP00F su AS888';
083400120210             ErrMsg = *on;
083500120210             ErrGenerico = *on;
083600120210             ErrInsert = *on;
083700120210             leavesr;
083800120210           endif;
083900120210         endif;
084000120210
084100120210         // aggiungo il record letto al CNIND00F di test
084200120210         if CNINDoccurs > 0;
084300120210           EXEC SQL insert into CNIND00F :CNINDoccurs rows values(:CNIND00F)
084400120210                    with UR;
084500120210           if SQLCode < 0;
084600120210             // improbabile avere un errore nell'insert, si sarebbe dovuto bloccare prima su CNACO00F
084700120210             // ma lo testo lo stesso per il rollback
084800120210             exec sql rollback;
084900120210             V1Dmsg = 'Errore in insert CNIND00F su AS888';
085000120210             ErrMsg = *on;
085100120210             ErrGenerico = *on;
085200120210             ErrInsert = *on;
085300120210             leavesr;
085400120210           endif;
085500120210         endif;
085600120210
085700120210         // aggiungo il record letto al FNCLS00F di test
085800120210         if FNCLSoccurs > 0;
085900120210           EXEC SQL insert into FNCLS00F :FNCLSoccurs rows values(:FNCLS00F)
086000120210                    with UR;
086100120210           if SQLCode < 0;
086200120210             // improbabile avere un errore nell'insert, si sarebbe dovuto bloccare prima su CNACO00F
086300120210             // ma lo testo lo stesso per il rollback
086400120210             exec sql rollback;
086500120210             V1Dmsg = 'Errore in insert FNCLS00F su AS888';
086600120210             ErrMsg = *on;
086700120210             ErrGenerico = *on;
086800120210             ErrInsert = *on;
086900120210             leavesr;
087000120210           endif;
087100120210         endif;
087200120210
087300120210         // aggiungo i record letti al TABEL00F di test
087400120210         if TABELoccurs > 0;
087500120210           EXEC SQL insert into TABEL00F :TABELoccurs rows values(:TABEL00F)
087600120210                    with UR;
087700120210           if SQLCode < 0;
087800120210             // se ho avuto un errore nell'insert, a regime sarà solo per chiave duplicata, ma non è un file vincolante
087900120210             // per cui lo segnalo solo ma proseguo
088000120210             V1Dmsg = 'Errore di copia sui file tabelle +
088100120210                       - controllare';
088200120210             ErrMsg = *on;
088300120210             ErrGenerico = *on;
088400120210           endif;
088500120210         endif;
088600120210
088700120210         // aggiungo il record letto al TNTAM00F di test
088800120210         if TNTAMoccurs > 0;
088900120210           EXEC SQL insert into TNTAM00F :TNTAMoccurs rows values(:TNTAM00F)
089000120210                    with UR;
089100120210           if SQLCode < 0;
089200120210             // improbabile avere un errore nell'insert, si sarebbe dovuto bloccare prima su CNACO00F
089300120210             // quindi non prevedo il caso
089400120210           endif;
089500120210         endif;
089600120210
089700120210         // aggiungo i record letti al TNTBE00F di test
089800120210         if TNTBEoccurs > 0;
089900120210           EXEC SQL insert into TNTBE00F :TNTBEoccurs rows values(:TNTBE00F)
090000120210                    with UR;
090100120210           if SQLCode < 0;
090200120210             // se ho avuto un errore nell'insert, a regime sarà solo per chiave duplicata, ma non è un file vincolante
090300120210             // per cui lo segnalo solo ma proseguo
090400120210             V1Dmsg = 'Errore di copia sui file tabelle +
090500120210                       - controllare';
090600120210             ErrMsg = *on;
090700120210             ErrGenerico = *on;
090800120210           endif;
090900120210         endif;
091000120210
091100120210         // se sono qui vuol dire che ho scritto tutto per cui committo
091200120210         exec sql commit;
091300120210
091400120210       ENDSR;
091500120208
091600111128       //--------------------------------------------------------------
091700120210       // Esecuzione del comando (già impostato)
091800111128       //--------------------------------------------------------------
091900111128       BEGSR  sr_ExecCmd;
092000111128
092100111128         clear Qcap0100;
092200111128         Qcabcsdh = *off;
092300111128         Qcapa    = *off;
092400111128         Qcacmdss = *off;
092500111128         Qcaerved = *allX'00';
092600111128
092700111128         clear Qusec;
092800111128         Qusbprv  = %size(Qusec);
092900111128
093000111128         ProcessCommands ( Qcmd : %len( %trimr( Qcmd ) ) : Qcap0100 :
093100111128                           %size(Qcap0100) : 'CPOP0100' : *omit :
093200111128                           0 : 0 : Qusec);
093300111128
093400120210         // - Stampa DUMP + JobLog  &  Chiusura *pgm  in caso di errore
093500111128         if  Qusei <> *blank;
093600111128           exsr  sr_PrintErr;
093700111128         endif;
093800111128
093900111128       ENDSR;
094000111128
094100111128       //--------------------------------------------------------------
094200120210       // Stampa segnalazione dell'errore rilevato
094300111128       //--------------------------------------------------------------
094400111128       BEGSR  sr_PrintErr;
094500111128
094600120210         // - Stampa del Dump
094700111128         Dump(A);
094800111128
094900120210         // - Stampa del Job-Log
095000111128         Qcmd = 'DSPJOBLOG job(*) output(*print)';
095100111128         exsr  sr_ExecCmd;
095200111128
095300111128       ENDSR;
095400111128
095500111128       //--------------------------------------------------------------
095600120208       // Operazioni finali
095700111128       //--------------------------------------------------------------
095800111128       BEGSR  sr_RoutEnd;
095900111128
096000120208         // - Uscita
096100111128         return;
096200111128
096300111128       ENDSR;
096400111128
096500111128      /end-free
