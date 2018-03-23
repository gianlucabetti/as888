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
002800120215       // - Video
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
009300131111       // - stringa per costruzione LIKE nell'SQL con VIDKSC
009400131111     d proLike         s              7
009500111128
009600120208       // - Parametri SQL
009700120207
009800120207       // - InfDS
009900120207     d InfDspF         ds
010000120207     d   dsp_aid             369    369a                                        AID byte
010100120207
010200120207       // - Indicatori su DspF
010300120207     d IndDspF         ds
010400120207        // - Indicatori di errore
010500120208     d  ErrMsg                        1n   overlay(IndDspF : 28)
010600120207     d  ErrGenerico                   1n   overlay(IndDspF : 99)
010700120704
010800120704     d KPJBA         e ds
010900120207
011000120207       // - Flags booleani
011100120207     d $Fine           s               n   inz(*off)
011200120207     d $InzD01         s               n   inz(*on)
011300120208     d ErrBlock        s               n   inz(*off)
011400120210     d ErrInsert       s               n   inz(*off)
011500120210     d Qcmd            s            500    inz
011600120207
011700120207       // - Campi associati al video
011800120207     d $Video          s              2    inz('D1')
011900120207
012000120207       // - Variabili di appoggio
012100120207     d DBRemote        s             10    inz('AS888')
012200120208     d CNACOOccurs     s              3s 0
012300120208     d CNCLPOccurs     s              3s 0
012400120208     d CNINDOccurs     s              3s 0
012500120208     d FNCLSOccurs     s              3s 0
012600120208     d TABELOccurs     s              3s 0
012700120208     d TNTAMOccurs     s              3s 0
012800120208     d TNTBEOccurs     s              3s 0
012900120215     d EsisteCNACO     s               n
013000120215     d EsisteTNTAM     s               n
013100111128
013200111128       //--------------------------------------------------------------
013300120210       // Definizione procedure usate
013400111128       //--------------------------------------------------------------
013500120207
013600120210       // - Parametri API QCAPCMD (Process Commands)
013700120207      /copy qSysInc/qRpgleSrc,QCAPCMD
013800120207      /copy gaitrasrc/srcProtoPR,QCAPCMD
013900120207
014000120210       // - Parametri gestione errori API
014100120207      /copy qSysInc/qRpgleSrc,QUSEC
014200120704
014300120704     d TRULKSCR        pr
014400120704     d  Arch                        502
014500120704
014600120704     d TRULKSCR        pi
014700120704     d  Arch                        502
014800120215
014900120215     d TRULKSCR1       pr                  extpgm('TRULKSCR1')
015000120215     d  VIDKSC                        7p 0
015100120215     d  EsisteTNTAM                   1n
015200120215     d  V1DMSG                       78
015300120215     D  VIDUSR                       10
015400120215     D  VIDPWD                       10
015500131108     D  VIDSOS                        1
015600111128
015700111128       //--------------------------------------------------------------
015800120208       // Definizione key-list.                                        ?
015900111128       //--------------------------------------------------------------
016000111128
016100111128
016200111128       //--------------------------------------------------------------
016300120208       // M A I N - L I N E                                            ?
016400111128       //--------------------------------------------------------------
016500111128
016600111128      /free
016700111128
016800120208       // - Operazioni iniziali
016900111128       exsr  sr_RoutInz;
017000111128
017100120207       // - Gestione video
017200120207       DOW  $Fine = *off;
017300120207         select;
017400120207           when  $Video = 'D1';
017500120207             exsr  sr_GesD01;
017600120207           other;
017700120207             $Fine = *on;
017800120207         endsl;
017900120207       ENDDO;
018000120207
018100120208       // - Operazioni finali
018200111128       exsr  sr_RoutEnd;
018300111128
018400111128       //--------------------------------------------------------------
018500120208       // Operazioni iniziali
018600111128       //--------------------------------------------------------------
018700111128       BEGSR sr_RoutInz;
018800111128
018900111128         *inLR = *on;
019000120207         IndDspF  = *off;
019100120704         KPJBA = Arch;
019200111128
019300120208         // - Impostazione opzioni per SQL
019400111128         exec SQL   set option   DynUsrPrf = *owner,
019500111128                                 CloSqlCsr = *endmod;
019600111128
019700111128
019800111128       ENDSR;
019900111128
020000111128       //--------------------------------------------------------------
020100120208       // Apertura cursore C1
020200111128       //--------------------------------------------------------------
020300111128       BEGSR  sr_OpenCursor;
020400111128
020500120208         // - Dichiarazione del cursore
020600111128         exec sql  prepare S1  from :wSQL;
020700111128         exec sql  declare C1  cursor for S1;
020800111128
020900120208         // - Apertura del cursore
021000111128         exec sql   open C1;
021100111128
021200111128       ENDSR;
021300111128
021400111128       //--------------------------------------------------------------
021500120208       // Chiusura cursore C1
021600111128       //--------------------------------------------------------------
021700111128       BEGSR  sr_CloseCursor;
021800111128
021900120208         // - Chiusura del cursore
022000111128         exec sql   close C1;
022100111128
022200111128       ENDSR;
022300111128
022400111128       //--------------------------------------------------------------
022500120208       // Lettura cursore C1
022600111128       //--------------------------------------------------------------
022700111128       BEGSR  sr_ReadCursor;
022800111128
022900111128         select;
023000111128
023100120208           // - Fine lettura
023200111128           when  sqlCode = 100;
023300120208             //non devo fare nulla perché devo scriveree tutti i rcd di tutti i cursori letti
023400120208           leavesr;
023500111128
023600120208           // - Errore
023700111128           when  sqlCode < *zero;
023800111128             exsr  sr_PrintErr;
023900111128
024000120208           // - Elaborazione
024100111128           other;
024200120208             //non devo fare nulla adesso perché prima devo leggere tutti i rcd di tutti i cursori
024300111128
024400111128         endsl;
024500111128
024600111128       ENDSR;
024700120207
024800120207       //--------------------------------------------------------------
024900120208       // Gestione videata D01
025000120207       //--------------------------------------------------------------
025100120207       BEGSR  sr_GesD01;
025200120207
025300120207         // - Inizializzazione videata
025400120207         if  $InzD01   = *on;
025500120207           exsr  sr_InzD01;
025600120207           $InzD01  = *off;
025700120207         endif;
025800120207
025900120207         // - Emissione Testata e Piede con tasti funzionali abilitati
026000120208         if  ErrGenerico = *off and ErrMsg = *off;
026100120207           write  trkuT01;
026200120207           write  trkuP01;
026300120207         endif;
026400120207
026500120207         // - Emissione videata
026600120207         exfmt  trkuD01;
026700120207
026800120208         reset  ErrMsg;
026900120207         reset  ErrGenerico;
027000120207         clear  V1Dmsg;
027100120207
027200120207         SELECT;
027300120207
027400120207           // - F3=Fine
027500120207           WHEN  dsp_aid = c_F03;
027600120207             exsr sr_F03D01;
027700120207
027800120207           // - F12=Ritorno
027900120207           WHEN  dsp_aid = c_F12;
028000120207             exsr sr_F12D01;
028100120207
028200120207           // - Invio / F6=Conferma
028300120207           OTHER;
028400120207             exsr  sr_CtrD01;
028500120207             if  ErrGenerico = *on;
028600120207               leavesr;
028700120207             endif;
028800120207             if  dsp_aid = c_F06;
028900120207               exsr sr_F06D01;
029000120207             endif;
029100120207
029200120207         ENDSL;
029300120207
029400120207       ENDSR;
029500120207
029600120207       //--------------------------------------------------------------
029700120208       // Inizializzazione videata D01
029800120207       //--------------------------------------------------------------
029900120207       BEGSR  sr_InzD01;
030000120207
030100120207         // - Pulizia videata
030200120207         clear  trkuD01;
030300120207
030400120207       ENDSR;
030500120207
030600120207       //--------------------------------------------------------------
030700120207       //?Gestione tasto funzionale F3 da videata D01                  ?
030800120207       //?F3=Fine                                                      ?
030900120207       //--------------------------------------------------------------
031000120207       BEGSR  sr_F03D01;
031100120207
031200120207         // - Chiusura del programma
031300120207         $Fine = *on;
031400120207
031500120207       ENDSR;
031600120207
031700120207       //--------------------------------------------------------------
031800120208       // Gestione tasto funzionale F6 da videata D01
031900120208       // F6=Conferma
032000120207       //--------------------------------------------------------------
032100120207       BEGSR  sr_F06D01;
032200120208
032300120216         // Fase 1
032400120216
032500120216         // Copio i dati da aggiungere su AS888
032600120216         exsr  CopiaDaSetras;
032700120216         if ErrBlock = *on;
032800120216           leavesr;
032900120216         endif;
033000120207
033100120216         // Scrivo dati da aggiungere se AS888
033200120216         exsr  CopiaSuAS888;
033300120216         if ErrBlock = *on or ErrInsert = *on;
033400120216           leavesr;
033500120216         endif;
033600120207
033700120216         // Se possibile l'esecuzione della fase 2
033800120216         // ctrl esistenza di TNTAM sul DB remoto
033900120216         exsr EsistenzaTNTAM;
034000120216         //richiamo apposito pgm per fase 2
034100131108         TRULKSCR1(VIDKSC:EsisteTNTAM:V1DMSG:VIDUSR:VIDPWD:VIDSOS);
034200120216         // se ho ricevuto un msg di errore, attivo il flag
034300120216         if V1DMSG <> *blank;
034400120216           ErrMsg = *on;
034500120216         endif;
034600120215
034700120208         // - Chiusura del programma (solo se non ho errori da mostrare)
034800120208         if ErrMsg = *off;
034900120208           $Fine = *on;
035000120208         endif;
035100120207
035200120207       ENDSR;
035300120208
035400120207       //--------------------------------------------------------------
035500120208       // Gestione tasto funzionale F12 da videata D01
035600120208       // F12=Ritorno
035700120207       //--------------------------------------------------------------
035800120207       BEGSR  sr_F12D01;
035900120207
036000120207         // - Chiusura del programma
036100120207         $Fine = *on;
036200120207
036300120207       ENDSR;
036400120207
036500120207       //--------------------------------------------------------------
036600120207       // Controllo videata D01                                        ?
036700120207       //--------------------------------------------------------------
036800120207       BEGSR  sr_CtrD01;
036900120207
037000120207         IndDspF  = *off;
037100120208
037200120208         // l'utente remoto va indicato
037300120208         if VIDUsr = *blank;
037400120208           V1Dmsg = 'Utente remoto obbligatorio';
037500120208           ErrMsg = *on;
037600120208           ErrGenerico = *on;
037700120208           leavesr;
037800120208         endif;
037900120208
038000120208         // la pwd dell'utente remoto va indicata
038100120208         if VIDPwd = *blank;
038200120208           V1Dmsg = 'Pwd utente remoto obbligatoria';
038300120208           ErrMsg = *on;
038400120208           ErrGenerico = *on;
038500120208           leavesr;
038600120208         endif;
038700120208
038800120215         // il cliente va indicato
038900120215         if VIDKsc = 0;
039000120215           V1Dmsg = 'Cliente obbligatorio';
039100120215           ErrMsg = *on;
039200120215           ErrGenerico = *on;
039300120215           leavesr;
039400120215         endif;
039500120207
039600120207       ENDSR;
039700120208
039800120208       //--------------------------------------------------------------
039900120208       // Reperisco i dati da copiare
040000120208       //--------------------------------------------------------------
040100120216       BEGSR  CopiaDaSetras;
040200120208
040300120208         ErrBlock = *off;
040400120208
040500120208         // - Reperisci dati da aggiungere
040600120208
040700120208         //reperisco dati CNACO
040800120210         //---------------------
040900120208         clear *all CNACO00F;
041000120208         // - Creazione stringa SQL da eseguire
041100120208         wSQL = 'select * from CNACO00F +
041200120209                 where ACOKSC = ' + %char(VIDKSC) + ' and ACOKCC=151';
041300120208         // - Apertura cursore
041400120208         exsr  sr_OpenCursor;
041500120208         CNACOoccurs = 1;
041600120209         // punto all'occorrenza da valorizzare
041700120209         %occur(CNACO00F) = CNACOoccurs;
041800120208         DoU  SQLCode = 100 or SQLCode < 0;
041900120209           exec sql  fetch next  from C1  into :CNACO00F;
042000120208           exsr  sr_ReadCursor;
042100120208           // se non viene letto rcd alla prima lettura, significa che non esiste un cliente con quel codice
042200120210           // errore bloccante
042300120208           select;
042400120209           // errore
042500120209           when SQLCode < 0;
042600120209             V1Dmsg = 'Errore in  lettura CNACO00F - vedi stampa errore';
042700120208             ErrMsg = *on;
042800120208             ErrGenerico = *on;
042900120208             ErrBlock = *on;
043000120208             // - Chiusura cursore (visto che esco con leavesr)
043100120208             exsr  sr_CloseCursor;
043200120208             leavesr;
043300120209           // nessuna lettura
043400120209           when SQLCode = 100 and CNACOoccurs = 1;
043500120209             V1Dmsg = 'Cliente inesistente';
043600120209             ErrMsg = *on;
043700120209             ErrGenerico = *on;
043800120209             ErrBlock = *on;
043900120209             // - Chiusura cursore (visto che esco con leavesr)
044000120209             exsr  sr_CloseCursor;
044100120209             leavesr;
044200120209           // lettura
044300120209           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
044400120208             CNACOoccurs = CNACOoccurs + 1;
044500120209             // punto all'occorrenza da valorizzare - per CNACO00F non lo faccio perché ha 1 sola occorrenza
044600120209             // %occur(CNACO00F) = CNACOoccurs;
044700120209           // fine lettura
044800120209           other;
044900120209             // non faccio nulla
045000120209           endsl;
045100120208         EndDo;
045200120208         // - Chiusura cursore
045300120208         exsr  sr_CloseCursor;
045400120209         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
045500120209         // le occorrenze davvero valorizzate sono inferiori di 1
045600120209         CNACOoccurs = CNACOoccurs - 1;
045700120208
045800120208         //reperisco dati CNCLP
045900120210         //---------------------
046000120208         clear *all CNCLP00F;
046100120208         // - Creazione stringa SQL da eseguire
046200120208         wSQL = 'select * from CNCLP00F +
046300120209                 where CLPKSC = ' + %char(VIDKSC) + ' and CLPKCC=151';
046400120208         // - Apertura cursore
046500120208         exsr  sr_OpenCursor;
046600120208         CNCLPoccurs = 1;
046700120209         // punto all'occorrenza da valorizzare
046800120209         %occur(CNCLP00F) = CNCLPoccurs;
046900120208         DoU  SQLCode = 100 or SQLCode < 0;
047000120208           exec sql  fetch next  from C1  into :CNCLP00F;
047100120208           exsr  sr_ReadCursor;
047200120208           // se non viene letto rcd alla prima lettura, significa che non esiste un cliente con quel codice
047300120208           // improbabile avere un errore del genere quì, si sarebbe dovuto bloccare prima su CNACO00F
047400120210           // però è accaduto!
047500120210           // errore bloccante
047600120209           select;
047700120209           // errore
047800120209           when SQLCode < 0;
047900120209             V1Dmsg = 'Errore in  lettura CNCLP00F - vedi stampa errore';
048000120209             ErrMsg = *on;
048100120209             ErrGenerico = *on;
048200120209             ErrBlock = *on;
048300120209             // - Chiusura cursore (visto che esco con leavesr)
048400120209             exsr  sr_CloseCursor;
048500120209             leavesr;
048600120209           // nessuna lettura
048700120209           when SQLCode = 100 and CNCLPoccurs = 1;
048800120210             V1Dmsg = 'Cliente non inserito completamente';
048900120210             ErrMsg = *on;
049000120210             ErrGenerico = *on;
049100120210             ErrBlock = *on;
049200120210             // - Chiusura cursore (visto che esco con leavesr)
049300120210             exsr  sr_CloseCursor;
049400120210             leavesr;
049500120209           // lettura
049600120209           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
049700120209             CNCLPoccurs = CNCLPoccurs + 1;
049800120209             // punto all'occorrenza da valorizzare - per CNCLP00F non lo faccio perché ha 1 sola occorrenza
049900120209             // %occur(CNCLP00F) = CNCLPoccurs;
050000120209           // fine lettura
050100120209           other;
050200120209             // non faccio nulla
050300120209           endsl;
050400120208         EndDo;
050500120208         // - Chiusura cursore
050600120208         exsr  sr_CloseCursor;
050700120209         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
050800120209         // le occorrenze davvero valorizzate sono inferiori di 1
050900120209         CNCLPoccurs = CNCLPoccurs - 1;
051000120208
051100120208         //reperisco dati CNIND
051200120210         //---------------------
051300120208         clear *all CNIND00F;
051400120208         // - Creazione stringa SQL da eseguire
051500120208         wSQL = 'select * from CNIND00F +
051600120209                 where INDKSC = ' + %char(VIDKSC) + ' and INDKCC=151';
051700120208         // - Apertura cursore
051800120208         exsr  sr_OpenCursor;
051900120208         CNINDoccurs = 1;
052000120209         // punto all'occorrenza da valorizzare
052100120209         %occur(CNIND00F) = CNINDoccurs;
052200120208         DoU  SQLCode = 100 or SQLCode < 0;
052300120208           exec sql  fetch next  from C1  into :CNIND00F;
052400120208           exsr  sr_ReadCursor;
052500120208           // se non viene letto rcd alla prima lettura, significa che non esiste un cliente con quel codice
052600120208           // improbabile avere un errore del genere quì, si sarebbe dovuto bloccare prima su CNACO00F
052700120210           // però è accaduto!
052800120210           // errore bloccante
052900120209           select;
053000120209           // errore
053100120209           when SQLCode < 0;
053200120209             V1Dmsg = 'Errore in  lettura CNIND00F - vedi stampa errore';
053300120209             ErrMsg = *on;
053400120209             ErrGenerico = *on;
053500120209             ErrBlock = *on;
053600120209             // - Chiusura cursore (visto che esco con leavesr)
053700120209             exsr  sr_CloseCursor;
053800120209             leavesr;
053900120209           // nessuna lettura
054000120209           when SQLCode = 100 and CNINDoccurs = 1;
054100120210             V1Dmsg = 'Cliente non inserito completamente';
054200120210             ErrMsg = *on;
054300120210             ErrGenerico = *on;
054400120210             ErrBlock = *on;
054500120210             // - Chiusura cursore (visto che esco con leavesr)
054600120210             exsr  sr_CloseCursor;
054700120210             leavesr;
054800120209           // lettura
054900120209           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
055000120208             CNINDoccurs = CNINDoccurs + 1;
055100120209             // punto all'occorrenza da valorizzare - per CNIND00F non lo faccio perché ha 1 sola occorrenza
055200120209             // %occur(CNIND00F) = CNINDoccurs;
055300120209           // fine lettura
055400120209           other;
055500120209             // non faccio nulla
055600120209           endsl;
055700120208         EndDo;
055800120208         // - Chiusura cursore
055900120208         exsr  sr_CloseCursor;
056000120209         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
056100120209         // le occorrenze davvero valorizzate sono inferiori di 1
056200120209         CNINDoccurs = CNINDoccurs - 1;
056300120208
056400120208         //reperisco dati FNCLS
056500120210         //---------------------
056600120208         clear *all FNCLS00F;
056700120208         // - Creazione stringa SQL da eseguire
056800120208         wSQL = 'select * from FNCLS00F +
056900120208                 where CLSKSC = ' + %char(VIDKSC);
057000120208         // - Apertura cursore
057100120208         exsr  sr_OpenCursor;
057200120208         FNCLSoccurs = 1;
057300120209         // punto all'occorrenza da valorizzare
057400120209         %occur(FNCLS00F) = FNCLSoccurs;
057500120208         DoU  SQLCode = 100 or SQLCode < 0;
057600120208           exec sql  fetch next  from C1  into :FNCLS00F;
057700120208           exsr  sr_ReadCursor;
057800120208           // se non viene letto rcd alla prima lettura, significa che non esiste un cliente con quel codice
057900120208           // improbabile avere un errore del genere quì, si sarebbe dovuto bloccare prima su CNACO00F
058000120210           // però è accaduto!
058100120210           // errore bloccante
058200120209           select;
058300120209           // errore
058400120209           when SQLCode < 0;
058500120209             V1Dmsg = 'Errore in  lettura FNCLS00F - vedi stampa errore';
058600120209             ErrMsg = *on;
058700120209             ErrGenerico = *on;
058800120209             ErrBlock = *on;
058900120209             // - Chiusura cursore (visto che esco con leavesr)
059000120209             exsr  sr_CloseCursor;
059100120209             leavesr;
059200120209           // nessuna lettura
059300120209           when SQLCode = 100 and FNCLSoccurs = 1;
059400120210             V1Dmsg = 'Cliente non inserito completamente';
059500120210             ErrMsg = *on;
059600120210             ErrGenerico = *on;
059700120210             ErrBlock = *on;
059800120210             // - Chiusura cursore (visto che esco con leavesr)
059900120210             exsr  sr_CloseCursor;
060000120210             leavesr;
060100120209           // lettura
060200120209           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
060300120208             FNCLSoccurs = FNCLSoccurs + 1;
060400120209             // punto all'occorrenza da valorizzare - per FNCLS00F non lo faccio perché ha 1 sola occorrenza
060500120209             // %occur(FNCLS00F) = FNCLSoccurs;
060600120209           // fine lettura
060700120209           other;
060800120209             // non faccio nulla
060900120209           endsl;
061000120208         EndDo;
061100120208         // - Chiusura cursore
061200120208         exsr  sr_CloseCursor;
061300120209         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
061400120209         // le occorrenze davvero valorizzate sono inferiori di 1
061500120209         FNCLSoccurs = FNCLSoccurs - 1;
061600120208
061700120208         //reperisco dati TABEL
061800120210         //---------------------
061900120208         clear *all TABEL00F;
062000120208         // - Creazione stringa SQL da eseguire
062100120209         wSQL = 'select * from TABEL00F +
062200120216                 where TBLKEY like ''%' + %editc(VIDKSC:'X') + '%''';
062300120208         // - Apertura cursore
062400120208         exsr  sr_OpenCursor;
062500120209         TABELoccurs = 1;
062600120209         // punto all'occorrenza da valorizzare
062700120209         %occur(TABEL00F) = TABELoccurs;
062800120208         DoU  SQLCode = 100 or SQLCode < 0;
062900120208           exec sql  fetch next  from C1  into :TABEL00F;
063000120208           exsr  sr_ReadCursor;
063100120208           // è possibile che non venga letto rcd alla prima lettura
063200120208           // quindi non blocco il flusso
063300120209           select;
063400120209           // errore
063500120209           when SQLCode < 0;
063600120209             V1Dmsg = 'Errore in  lettura TABEL00F - vedi stampa errore';
063700120209             ErrMsg = *on;
063800120209             ErrGenerico = *on;
063900120209             ErrBlock = *on;
064000120209             // - Chiusura cursore (visto che esco con leavesr)
064100120209             exsr  sr_CloseCursor;
064200120209             leavesr;
064300120209           // nessuna lettura
064400120209           when SQLCode = 100 and TABELoccurs = 1;
064500120209             // non faccio nulla
064600120209           // lettura
064700120209           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
064800120208             TABELoccurs = TABELoccurs + 1;
064900120209             // punto all'occorrenza da valorizzare
065000120209             %occur(TABEL00F) = TABELoccurs;
065100120209           // fine lettura
065200120209           other;
065300120209             // non faccio nulla
065400120209           endsl;
065500120208         EndDo;
065600120208         // - Chiusura cursore
065700120208         exsr  sr_CloseCursor;
065800120209         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
065900120209         // le occorrenze davvero valorizzate sono inferiori di 1
066000120209         TABELoccurs = TABELoccurs - 1;
066100120208
066200120208         //reperisco dati TNTAM
066300120210         //---------------------
066400120208         clear *all TNTAM00F;
066500120208         // - Creazione stringa SQL da eseguire
066600120208         wSQL = 'select * from TNTAM00F +
066700120208                 where TAMKSC =' + %char(VIDKSC);
066800120208         // - Apertura cursore
066900120208         exsr  sr_OpenCursor;
067000120208         TNTAMoccurs = 1;
067100120209         // punto all'occorrenza da valorizzare
067200120209         %occur(TNTAM00F) = TNTAMoccurs;
067300120208         DoU  SQLCode = 100 or SQLCode < 0;
067400120208           exec sql  fetch next  from C1  into :TNTAM00F;
067500120208           exsr  sr_ReadCursor;
067600120208           // è possibile che non venga letto rcd alla prima lettura (cliente totalmente nuovo)
067700120208           // quindi non blocco il flusso
067800120209           select;
067900120209           // errore
068000120209           when SQLCode < 0;
068100120209             V1Dmsg = 'Errore in  lettura TNTAM00F - vedi stampa errore';
068200120209             ErrMsg = *on;
068300120209             ErrGenerico = *on;
068400120209             ErrBlock = *on;
068500120209             // - Chiusura cursore (visto che esco con leavesr)
068600120209             exsr  sr_CloseCursor;
068700120209             leavesr;
068800120209           // nessuna lettura
068900120209           when SQLCode = 100 and TNTAMoccurs = 1;
069000120209             // non faccio nulla
069100120209           // lettura
069200120209           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
069300120208             TNTAMoccurs = TNTAMoccurs + 1;
069400120209             // punto all'occorrenza da valorizzare
069500120209             %occur(TNTAM00F) = TNTAMoccurs;
069600120209           // fine lettura
069700120209           other;
069800120209             // non faccio nulla
069900120209           endsl;
070000120208         EndDo;
070100120208         // - Chiusura cursore
070200120208         exsr  sr_CloseCursor;
070300120209         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
070400120209         // le occorrenze davvero valorizzate sono inferiori di 1
070500120209         TNTAMoccurs = TNTAMoccurs - 1;
070600120208
070700120208         //reperisco dati TNTBE
070800120210         //---------------------
070900120208         clear *all TNTBE00F;
071000120208         // - Creazione stringa SQL da eseguire
071100120208         wSQL = 'select * from TNTBE00F +
071200120216                 where TBEKE1 like ''%' + %editc(VIDKSC:'X') + '%'' +
071300120209                 or TBEKE2 like ''%'
071400120216                  + %editc(VIDKSC:'X') + '%''';
071500120208         // - Apertura cursore
071600120208         exsr  sr_OpenCursor;
071700120209         TNTBEoccurs = 1;
071800120209         // punto all'occorrenza da valorizzare
071900120209         %occur(TNTBE00F) = TNTBEoccurs;
072000120208         DoU  SQLCode = 100 or SQLCode < 0;
072100120208           exec sql  fetch next  from C1  into :TNTBE00F;
072200120208           exsr  sr_ReadCursor;
072300120208           // è possibile che non venga letto rcd alla prima lettura
072400120208           // quindi non blocco il flusso
072500120209           select;
072600120209           // errore
072700120209           when SQLCode < 0;
072800120209             V1Dmsg = 'Errore in  lettura TNTBE00F - vedi stampa errore';
072900120209             ErrMsg = *on;
073000120209             ErrGenerico = *on;
073100120209             ErrBlock = *on;
073200120209             // - Chiusura cursore (visto che esco con leavesr)
073300120209             exsr  sr_CloseCursor;
073400120209             leavesr;
073500120209           // nessuna lettura
073600120209           when SQLCode = 100 and TNTBEoccurs = 1;
073700120209             // non faccio nulla
073800120209           // lettura
073900120209           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
074000120208             TNTBEoccurs = TNTBEoccurs + 1;
074100120209             // punto all'occorrenza da valorizzare
074200120209             %occur(TNTBE00F) = TNTBEoccurs;
074300120209           // fine lettura
074400120209           other;
074500120209             // non faccio nulla
074600120209           endsl;
074700120208         EndDo;
074800120208         // - Chiusura cursore
074900120208         exsr  sr_CloseCursor;
075000120209         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
075100120209         // le occorrenze davvero valorizzate sono inferiori di 1
075200120209         TNTBEoccurs = TNTBEoccurs - 1;
075300120208
075400120208       ENDSR;
075500111128
075600111128       //--------------------------------------------------------------
075700120207       // Scrivo rcd estratti
075800111128       //--------------------------------------------------------------
075900120216       BEGSR  CopiaSuAS888;
076000111128
076100120210         ErrMsg = *off;
076200120210         ErrGenerico = *off;
076300120216         ErrBlock = *off;
076400120216         ErrInsert = *off;
076500120210
076600120207         // mi connetto ad AS888
076700120216         exsr ConnectToAS888;
076800120216         if ErrBlock = *on;
076900120216           leavesr;
077000120216         endif;
077100120207
077200131108         // se richiesta la sovrapposizione, prima cancello i rcd
077300131108         if VIDSOS='S';
077400131108           exsr sr_DeleteRcd;
077500131108         endif;
077600131108
077700131108         // esecuzione scrittura rcd estratti
077800131108         exsr sr_InsertRcd;
077900120208
078000120208         // mi disconnetto da AS888
078100120216         exsr DisconnectFromAS888;
078200120216         if ErrBlock = *on;
078300120216           leavesr;
078400120216         endif;
078500120208
078600120208         // mi riconnetto a SETRAS
078700120216         exsr ConnectToSetras;
078800120216         if ErrBlock = *on;
078900120216           leavesr;
079000120216         endif;
079100120207
079200111128       ENDSR;
079300120216
079400120216       //--------------------------------------------------------------
079500120216       // Connessione
079600120216       //--------------------------------------------------------------
079700120216       BEGSR  ConnectToAS888;
079800120216
079900120216         // mi connetto ad AS888
080000120216         exec sql  connect to :DBRemote user :VIDUSR using :VIDPWD;
080100120216         if SQLCode < 0;
080200120216           //emetto msg di errore perentorio
080300120216           V1Dmsg = 'FASE 1: non riuscita connessione ad AS888 +
080400120216                     - SCOLLEGARE la sessione e riprovare';
080500120216           ErrMsg = *on;
080600120216           ErrGenerico = *on;
080700120216           ErrBlock = *on;
080800120216         endif;
080900120216
081000120216       ENDSR;
081100120216
081200120216       //--------------------------------------------------------------
081300120216       // Disconnessione
081400120216       //--------------------------------------------------------------
081500120216       BEGSR  DisconnectFromAS888;
081600120216
081700120216         // mi disconnetto da AS888
081800120216         exec sql  disconnect :DBRemote;
081900120216         // se la disconnessione ha dato errore
082000120216         if SQLCode < 0;
082100120216           //emetto msg di errore perentorio
082200120216           if ErrInsert = *on;
082300120216             V1Dmsg = 'FASE 1: agg. NON eseguito ED errore in +
082400120216                       connessione SETRAS - SCOLLEGARSI!';
082500120216           else;
082600120216             V1Dmsg = 'FASE 1: agg. eseguito ma errore in +
082700120216                       connessione SETRAS - SCOLLEGARSI!';
082800120216           endif;
082900120216           ErrMsg = *on;
083000120216           ErrGenerico = *on;
083100120216           ErrBlock = *on;
083200120216         endif;
083300120216
083400120216       ENDSR;
083500120216
083600120216       //--------------------------------------------------------------
083700120216       // Riconnessione a SETRAS
083800120216       //--------------------------------------------------------------
083900120216       BEGSR  ConnectToSetras;
084000120216
084100120216         // mi riconnetto a SETRAS
084200120216         // siccome in BRT gli SQLRPGLE si compilano col parametro RDBCNNMTH = *DUW, la connect ad AS888 NON ha chiuso
084300120216         // la connessione a SETRAS, l'ha resa dorminte. In questo caso NON serve un cmd CONNECT RESET perché  darebbe
084400120216         // errore SQL0842 = Connessione al database relazionale &1 già esistente.
084500120216         // Però, la connessione al DB locale (SETRAS) esiste se prima c'è stato almeno uno statement sql se quel DB.
084600120216         // In questo caso la SET CONNECTION dà errore SQL0842 = Il collegamento al database relazionale &1 non esiste
084700120216         // e va eseguita la CONNECT RESET.
084800120216         // in questo pgm dovrebbe sempre esserci prima uno statement SQL su SETRAS per leggere i dati,
084900120216         // ma non si sa mai... magari vengono fatte modifiche
085000120216         // Per cui, dopo aver disconnesso AS888, fare il cmd SET CONNECTION per "risvegliare" SETRAS e, se dà errore
085100120216         // provare con CONNECT RESET.
085200120216         // Solo se entrambi hanno dato errore, emettere un msg a video.
085300120216         exec sql  set connection SETRAS;
085400120216         if SQLCode < 0;
085500120216           // provo con CONNECT RESET
085600120216           exec sql  connect reset;
085700120216           // se anche in questo caso ho errore, emetto msg
085800120216           if SQLCode < 0;
085900120216             //emetto msg di errore perentorio
086000120216             if ErrInsert = *on;
086100120216               V1Dmsg = 'FASE 1: agg. NON eseguito ED errore in +
086200120216                         connessione SETRAS - SCOLLEGARSI!';
086300120216             else;
086400120216               V1Dmsg = 'FASE 1: agg. eseguito ma errore in +
086500120216                         connessione DB SETRAS - SCOLLEGARSI!';
086600120216             endif;
086700120216             ErrMsg = *on;
086800120216             ErrGenerico = *on;
086900120216             ErrBlock = *on;
087000120216           endif;
087100120216         endif;
087200120216
087300120216       ENDSR;
087400131108
087500131108       //--------------------------------------------------------------
087600131108       // Cancello rcd già presenti
087700131108       //--------------------------------------------------------------
087800131108       BEGSR  sr_DeleteRcd;
087900131108
088000131108         // cancello il record sul CNACO00F di test
088100131108         EXEC SQL delete from UNITRAGRP/CNACO00F
088200131108                  where ACOKSC=:VIDKSC and ACOKCC='151';
088300131108         if SQLCode < 0;
088400131108         // se ho avuto un errore nella delete, a regime sarà solo perché non c'è un rcd per il cliente su CNACO,
088500131108         // quindi, molto probabilmente, in nessuno degli altri file
088600131108         // per cui esco dalla routine
088700131108           leavesr;
088800131108         endif;
088900131108
089000131108         // cancello i record sul CNCLP00F di test
089100131108         EXEC SQL delete from UNITRAGRP/CNCLP00F
089200131108                where CLPKSC=:VIDKSC and CLPKCC='151';
089300131108         if SQLCode < 0;
089400131108           // se c'è un errore, probabilmente è perché manca il rcd da cancellare, per cui vado avanti lo stesso
089500131108         endif;
089600131108
089700131108         // cancello i record sul CNIND00F di test
089800131108         EXEC SQL delete from UNITRAGRP/CNIND00F
089900131108                where INDKSC=:VIDKSC and INDKCC='151';
090000131108         if SQLCode < 0;
090100131108           // se c'è un errore, probabilmente è perché manca il rcd da cancellare, per cui vado avanti lo stesso
090200131108         endif;
090300131108
090400131108         // cancello i record sul FNCLS00F di test
090500131108         EXEC SQL delete from UNITRAGRP/FNCLS00F
090600131108                where CLSKSC=:VIDKSC;
090700131108         if SQLCode < 0;
090800131108           // se c'è un errore, probabilmente è perché manca il rcd da cancellare, per cui vado avanti lo stesso
090900131108         endif;
091000131108
091100131111         // cancello i record sul TABEL00F di test di sede
091200131111         proLike = %editc(VIDKSC:'X');
091300131111         EXEC SQL delete from GAITRAGRPS/TABEL00F
091400131111                where TBLKEY like '%' concat :proLike concat '%';
091500131108         if SQLCode < 0;
091600131108           // se c'è un errore, probabilmente è perché manca il rcd da cancellare, per cui vado avanti lo stesso
091700131108         endif;
091800131111
091900131111         // cancello i record sul TABEL00F di test di filiale
092000131111         proLike = %editc(VIDKSC:'X');
092100131111         EXEC SQL delete from FILTRAGRPF/TABEL00F
092200131111                where TBLKEY like '%' concat :proLike concat '%';
092300131111         if SQLCode < 0;
092400131111           // se c'è un errore, probabilmente è perché manca il rcd da cancellare, per cui vado avanti lo stesso
092500131111         endif;
092600131111
092700131111         // cancello i record sul TNTAM00F di test
092800131111         EXEC SQL delete from UNITRAGRP/TNTAM00F
092900131111                 where TAMKSC = char(:VIDKSC);
093000131111         if SQLCode < 0;
093100131111           // se c'è un errore, probabilmente è perché manca il rcd da cancellare, per cui vado avanti lo stesso
093200131111         endif;
093300131108
093400131111         // cancello i record sul TNTBE00F di test di sede
093500131111         proLike = %editc(VIDKSC:'X');
093600131111         EXEC SQL delete from GAITRAGRPS/TNTBE00F
093700131111                 where TBEKE1 like '%' concat :proLike concat '%'
093800131111                 or TBEKE2 like '%' concat :proLike concat 'X';
093900131111         if SQLCode < 0;
094000131111           // se c'è un errore, probabilmente è perché manca il rcd da cancellare, per cui vado avanti lo stesso
094100131111         endif;
094200131111
094300131111         // cancello i record sul TNTBE00F di test di filiale
094400131111         proLike = %editc(VIDKSC:'X');
094500131111         EXEC SQL delete from FILTRAGRPF/TNTBE00F
094600131111                 where TBEKE1 like '%' concat :proLike concat '%'
094700131111                 or TBEKE2 like '%' concat :proLike concat 'X';
094800131111         if SQLCode < 0;
094900131111           // se c'è un errore, probabilmente è perché manca il rcd da cancellare, per cui vado avanti lo stesso
095000131111         endif;
095100131108
095200131108         // se sono qui vuol dire che ho scritto tutto per cui committo
095300131108         exec sql commit;
095400131108
095500131108       ENDSR;
095600111128
095700120208       //--------------------------------------------------------------
095800120208       // Scrivo rcd estratti
095900120208       //--------------------------------------------------------------
096000120208       BEGSR  sr_InsertRcd;
096100120208
096200120208         // aggiungo il record letto al CNACO00F di test
096300120227         EXEC SQL insert into UNITRAGRP/CNACO00F :CNACOoccurs rows
096400120227                  values(:CNACO00F)
096500120210                  with UR;
096600120208         if SQLCode < 0;
096700120208         // se ho avuto un errore nell'insert, a regime sarà solo per chiave duplicata, quindi interrompo la copia
096800120208         // dando errore
096900120210           exec sql rollback;
097000120210           V1Dmsg = 'Cliente già esistente su AS888 +
097100120208                     - controllare';
097200120208           ErrMsg = *on;
097300120208           ErrGenerico = *on;
097400120210           ErrInsert = *on;
097500120208           leavesr;
097600120208         endif;
097700120208
097800120208         // aggiungo il record letto al CNCLP00F di test
097900120208         if CNCLPoccurs > 0;
098000120227           EXEC SQL insert into UNITRAGRP/CNCLP00F :CNCLPoccurs rows
098100120227                    values(:CNCLP00F)
098200120210                    with UR;
098300120208           if SQLCode < 0;
098400120208             // improbabile avere un errore nell'insert, si sarebbe dovuto bloccare prima su CNACO00F
098500120210             // ma lo testo lo stesso per il rollback
098600120210             exec sql rollback;
098700120210             V1Dmsg = 'Errore in insert CNCLP00F su AS888';
098800120210             ErrMsg = *on;
098900120210             ErrGenerico = *on;
099000120210             ErrInsert = *on;
099100120210             leavesr;
099200120208           endif;
099300120208         endif;
099400120208
099500120208         // aggiungo il record letto al CNIND00F di test
099600120208         if CNINDoccurs > 0;
099700120227           EXEC SQL insert into UNITRAGRP/CNIND00F :CNINDoccurs rows
099800120227                    values(:CNIND00F)
099900120210                    with UR;
100000120208           if SQLCode < 0;
100100120210             // improbabile avere un errore nell'insert, si sarebbe dovuto bloccare prima su CNACO00F
100200120210             // ma lo testo lo stesso per il rollback
100300120210             exec sql rollback;
100400120210             V1Dmsg = 'Errore in insert CNIND00F su AS888';
100500120210             ErrMsg = *on;
100600120210             ErrGenerico = *on;
100700120210             ErrInsert = *on;
100800120210             leavesr;
100900120208           endif;
101000120208         endif;
101100120208
101200120208         // aggiungo il record letto al FNCLS00F di test
101300120208         if FNCLSoccurs > 0;
101400120227           EXEC SQL insert into UNITRAGRP/FNCLS00F :FNCLSoccurs rows
101500120227                    values(:FNCLS00F)
101600120210                    with UR;
101700120208           if SQLCode < 0;
101800120210             // improbabile avere un errore nell'insert, si sarebbe dovuto bloccare prima su CNACO00F
101900120210             // ma lo testo lo stesso per il rollback
102000120210             exec sql rollback;
102100120210             V1Dmsg = 'Errore in insert FNCLS00F su AS888';
102200120210             ErrMsg = *on;
102300120210             ErrGenerico = *on;
102400120210             ErrInsert = *on;
102500120210             leavesr;
102600120208           endif;
102700120208         endif;
102800120208
102900120227         // aggiungo i record letti al TABEL00F di test in sede
103000120208         if TABELoccurs > 0;
103100120227           EXEC SQL insert into GAITRAGRPS/TABEL00F :TABELoccurs rows
103200120227                    values(:TABEL00F)
103300120210                    with UR;
103400120208           if SQLCode < 0;
103500120208             // se ho avuto un errore nell'insert, a regime sarà solo per chiave duplicata, ma non è un file vincolante
103600120208             // per cui lo segnalo solo ma proseguo
103700120227             V1Dmsg = 'Errore di copia sui file tabelle SEDE +
103800120208                       - controllare';
103900120208             ErrMsg = *on;
104000120208             ErrGenerico = *on;
104100120208           endif;
104200120208         endif;
104300120227
104400120227         // aggiungo i record letti al TABEL00F di test in filiale
104500120227         if TABELoccurs > 0;
104600120227           EXEC SQL insert into FILTRAGRPF/TABEL00F :TABELoccurs rows
104700120227                    values(:TABEL00F)
104800120227                    with UR;
104900120227           if SQLCode < 0;
105000120227             // se ho avuto un errore nell'insert, a regime sarà solo per chiave duplicata, ma non è un file vincolante
105100120227             // per cui lo segnalo solo ma proseguo
105200120227             V1Dmsg = 'Errore di copia sui file tabelle FILIALE +
105300120227                       - controllare';
105400120227             ErrMsg = *on;
105500120227             ErrGenerico = *on;
105600120227           endif;
105700120227         endif;
105800120208
105900120208         // aggiungo il record letto al TNTAM00F di test
106000120208         if TNTAMoccurs > 0;
106100120227           EXEC SQL insert into UNITRAGRP/TNTAM00F :TNTAMoccurs rows
106200120227                    values(:TNTAM00F)
106300120210                    with UR;
106400120208           if SQLCode < 0;
106500120208             // improbabile avere un errore nell'insert, si sarebbe dovuto bloccare prima su CNACO00F
106600120208             // quindi non prevedo il caso
106700120208           endif;
106800120208         endif;
106900120208
107000120227         // aggiungo i record letti al TNTBE00F di test di sede
107100120208         if TNTBEoccurs > 0;
107200120227           EXEC SQL insert into GAITRAGRPS/TNTBE00F :TNTBEoccurs rows
107300120227                    values(:TNTBE00F)
107400120210                    with UR;
107500120208           if SQLCode < 0;
107600120208             // se ho avuto un errore nell'insert, a regime sarà solo per chiave duplicata, ma non è un file vincolante
107700120208             // per cui lo segnalo solo ma proseguo
107800120227             V1Dmsg = 'Errore di copia sui file tabelle SEDE +
107900120208                       - controllare';
108000120208             ErrMsg = *on;
108100120208             ErrGenerico = *on;
108200120208           endif;
108300120208         endif;
108400120227
108500120227         // aggiungo i record letti al TNTBE00F di test di filiale
108600120227         if TNTBEoccurs > 0;
108700120227           EXEC SQL insert into FILTRAGRPF/TNTBE00F :TNTBEoccurs rows
108800120227                    values(:TNTBE00F)
108900120227                    with UR;
109000120227           if SQLCode < 0;
109100120227             // se ho avuto un errore nell'insert, a regime sarà solo per chiave duplicata, ma non è un file vincolante
109200120227             // per cui lo segnalo solo ma proseguo
109300120227             V1Dmsg = 'Errore di copia sui file tabelle FILIALE +
109400120227                       - controllare';
109500120227             ErrMsg = *on;
109600120227             ErrGenerico = *on;
109700120227           endif;
109800120227         endif;
109900120208
110000120210         // se sono qui vuol dire che ho scritto tutto per cui committo
110100120210         exec sql commit;
110200120210
110300120208       ENDSR;
110400120215
110500120215       //--------------------------------------------------------------
110600120215       // Controlla esistenza TNTAM per il cliente da copiare
110700120215       //--------------------------------------------------------------
110800120215       BEGSR  EsistenzaTNTAM;
110900120215
111000120216         EsisteTNTAM = *off;
111100120215
111200120216         // se ho eseguito la fase 1 questa informazione ce l'ho in canna
111300120216         if TNTAMoccurs > 0;
111400120216           EsisteTNTAM = *on;
111500120216         endif;
111600120215
111700120215       ENDSR;
111800120215
111900120215       //--------------------------------------------------------------
112000120215       // Esecuzione del comando (già impostato)
112100120215       //--------------------------------------------------------------
112200120215       BEGSR  sr_ExecCmd;
112300120215
112400111128         clear Qcap0100;
112500111128         Qcabcsdh = *off;
112600111128         Qcapa    = *off;
112700111128         Qcacmdss = *off;
112800111128         Qcaerved = *allX'00';
112900111128
113000111128         clear Qusec;
113100111128         Qusbprv  = %size(Qusec);
113200111128
113300111128         ProcessCommands ( Qcmd : %len( %trimr( Qcmd ) ) : Qcap0100 :
113400111128                           %size(Qcap0100) : 'CPOP0100' : *omit :
113500111128                           0 : 0 : Qusec);
113600111128
113700120210         // - Stampa DUMP + JobLog  &  Chiusura *pgm  in caso di errore
113800111128         if  Qusei <> *blank;
113900111128           exsr  sr_PrintErr;
114000111128         endif;
114100111128
114200111128       ENDSR;
114300111128
114400111128       //--------------------------------------------------------------
114500120210       // Stampa segnalazione dell'errore rilevato
114600111128       //--------------------------------------------------------------
114700111128       BEGSR  sr_PrintErr;
114800111128
114900120210         // - Stampa del Dump
115000111128         Dump(A);
115100111128
115200120210         // - Stampa del Job-Log
115300111128         Qcmd = 'DSPJOBLOG job(*) output(*print)';
115400111128         exsr  sr_ExecCmd;
115500111128
115600111128       ENDSR;
115700111128
115800111128       //--------------------------------------------------------------
115900120208       // Operazioni finali
116000111128       //--------------------------------------------------------------
116100111128       BEGSR  sr_RoutEnd;
116200111128
116300120208         // - Uscita
116400111128         return;
116500111128
116600111128       ENDSR;
116700111128
116800111128      /end-free
