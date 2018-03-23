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
010500120704
010600120704     d KPJBA         e ds
010700120207
010800120207       // - Flags booleani
010900120207     d $Fine           s               n   inz(*off)
011000120207     d $InzD01         s               n   inz(*on)
011100120208     d ErrBlock        s               n   inz(*off)
011200120210     d ErrInsert       s               n   inz(*off)
011300120210     d Qcmd            s            500    inz
011400120207
011500120207       // - Campi associati al video
011600120207     d $Video          s              2    inz('D1')
011700120207
011800120207       // - Variabili di appoggio
011900120207     d DBRemote        s             10    inz('AS888')
012000120208     d CNACOOccurs     s              3s 0
012100120208     d CNCLPOccurs     s              3s 0
012200120208     d CNINDOccurs     s              3s 0
012300120208     d FNCLSOccurs     s              3s 0
012400120208     d TABELOccurs     s              3s 0
012500120208     d TNTAMOccurs     s              3s 0
012600120208     d TNTBEOccurs     s              3s 0
012700120215     d EsisteCNACO     s               n
012800120215     d EsisteTNTAM     s               n
012900111128
013000111128       //--------------------------------------------------------------
013100120210       // Definizione procedure usate
013200111128       //--------------------------------------------------------------
013300120207
013400120210       // - Parametri API QCAPCMD (Process Commands)
013500120207      /copy qSysInc/qRpgleSrc,QCAPCMD
013600120207      /copy gaitrasrc/srcProtoPR,QCAPCMD
013700120207
013800120210       // - Parametri gestione errori API
013900120207      /copy qSysInc/qRpgleSrc,QUSEC
014000120704
014100120704     d TRULKSCR        pr
014200120704     d  Arch                        502
014300120704
014400120704     d TRULKSCR        pi
014500120704     d  Arch                        502
014600120215
014700120215     d TRULKSCR1       pr                  extpgm('TRULKSCR1')
014800120215     d  VIDKSC                        7p 0
014900120215     d  EsisteTNTAM                   1n
015000120215     d  V1DMSG                       78
015100120215     D  VIDUSR                       10
015200120215     D  VIDPWD                       10
015300111128
015400111128       //--------------------------------------------------------------
015500120208       // Definizione key-list.                                        ?
015600111128       //--------------------------------------------------------------
015700111128
015800111128
015900111128       //--------------------------------------------------------------
016000120208       // M A I N - L I N E                                            ?
016100111128       //--------------------------------------------------------------
016200111128
016300111128      /free
016400111128
016500120208       // - Operazioni iniziali
016600111128       exsr  sr_RoutInz;
016700111128
016800120207       // - Gestione video
016900120207       DOW  $Fine = *off;
017000120207         select;
017100120207           when  $Video = 'D1';
017200120207             exsr  sr_GesD01;
017300120207           other;
017400120207             $Fine = *on;
017500120207         endsl;
017600120207       ENDDO;
017700120207
017800120208       // - Operazioni finali
017900111128       exsr  sr_RoutEnd;
018000111128
018100111128       //--------------------------------------------------------------
018200120208       // Operazioni iniziali
018300111128       //--------------------------------------------------------------
018400111128       BEGSR sr_RoutInz;
018500111128
018600111128         *inLR = *on;
018700120207         IndDspF  = *off;
018800120704         KPJBA = Arch;
018900111128
019000120208         // - Impostazione opzioni per SQL
019100111128         exec SQL   set option   DynUsrPrf = *owner,
019200111128                                 CloSqlCsr = *endmod;
019300111128
019400111128
019500111128       ENDSR;
019600111128
019700111128       //--------------------------------------------------------------
019800120208       // Apertura cursore C1
019900111128       //--------------------------------------------------------------
020000111128       BEGSR  sr_OpenCursor;
020100111128
020200120208         // - Dichiarazione del cursore
020300111128         exec sql  prepare S1  from :wSQL;
020400111128         exec sql  declare C1  cursor for S1;
020500111128
020600120208         // - Apertura del cursore
020700111128         exec sql   open C1;
020800111128
020900111128       ENDSR;
021000111128
021100111128       //--------------------------------------------------------------
021200120208       // Chiusura cursore C1
021300111128       //--------------------------------------------------------------
021400111128       BEGSR  sr_CloseCursor;
021500111128
021600120208         // - Chiusura del cursore
021700111128         exec sql   close C1;
021800111128
021900111128       ENDSR;
022000111128
022100111128       //--------------------------------------------------------------
022200120208       // Lettura cursore C1
022300111128       //--------------------------------------------------------------
022400111128       BEGSR  sr_ReadCursor;
022500111128
022600111128         select;
022700111128
022800120208           // - Fine lettura
022900111128           when  sqlCode = 100;
023000120208             //non devo fare nulla perché devo scriveree tutti i rcd di tutti i cursori letti
023100120208           leavesr;
023200111128
023300120208           // - Errore
023400111128           when  sqlCode < *zero;
023500111128             exsr  sr_PrintErr;
023600111128
023700120208           // - Elaborazione
023800111128           other;
023900120208             //non devo fare nulla adesso perché prima devo leggere tutti i rcd di tutti i cursori
024000111128
024100111128         endsl;
024200111128
024300111128       ENDSR;
024400120207
024500120207       //--------------------------------------------------------------
024600120208       // Gestione videata D01
024700120207       //--------------------------------------------------------------
024800120207       BEGSR  sr_GesD01;
024900120207
025000120207         // - Inizializzazione videata
025100120207         if  $InzD01   = *on;
025200120207           exsr  sr_InzD01;
025300120207           $InzD01  = *off;
025400120207         endif;
025500120207
025600120207         // - Emissione Testata e Piede con tasti funzionali abilitati
025700120208         if  ErrGenerico = *off and ErrMsg = *off;
025800120207           write  trkuT01;
025900120207           write  trkuP01;
026000120207         endif;
026100120207
026200120207         // - Emissione videata
026300120207         exfmt  trkuD01;
026400120207
026500120208         reset  ErrMsg;
026600120207         reset  ErrGenerico;
026700120207         clear  V1Dmsg;
026800120207
026900120207         SELECT;
027000120207
027100120207           // - F3=Fine
027200120207           WHEN  dsp_aid = c_F03;
027300120207             exsr sr_F03D01;
027400120207
027500120207           // - F12=Ritorno
027600120207           WHEN  dsp_aid = c_F12;
027700120207             exsr sr_F12D01;
027800120207
027900120207           // - Invio / F6=Conferma
028000120207           OTHER;
028100120207             exsr  sr_CtrD01;
028200120207             if  ErrGenerico = *on;
028300120207               leavesr;
028400120207             endif;
028500120207             if  dsp_aid = c_F06;
028600120207               exsr sr_F06D01;
028700120207             endif;
028800120207
028900120207         ENDSL;
029000120207
029100120207       ENDSR;
029200120207
029300120207       //--------------------------------------------------------------
029400120208       // Inizializzazione videata D01
029500120207       //--------------------------------------------------------------
029600120207       BEGSR  sr_InzD01;
029700120207
029800120207         // - Pulizia videata
029900120207         clear  trkuD01;
030000120207
030100120207       ENDSR;
030200120207
030300120207       //--------------------------------------------------------------
030400120207       //?Gestione tasto funzionale F3 da videata D01                  ?
030500120207       //?F3=Fine                                                      ?
030600120207       //--------------------------------------------------------------
030700120207       BEGSR  sr_F03D01;
030800120207
030900120207         // - Chiusura del programma
031000120207         $Fine = *on;
031100120207
031200120207       ENDSR;
031300120207
031400120207       //--------------------------------------------------------------
031500120208       // Gestione tasto funzionale F6 da videata D01
031600120208       // F6=Conferma
031700120207       //--------------------------------------------------------------
031800120207       BEGSR  sr_F06D01;
031900120208
032000120216         // Fase 1
032100120216
032200120216         // Copio i dati da aggiungere su AS888
032300120216         exsr  CopiaDaSetras;
032400120216         if ErrBlock = *on;
032500120216           leavesr;
032600120216         endif;
032700120207
032800120216         // Scrivo dati da aggiungere se AS888
032900120216         exsr  CopiaSuAS888;
033000120216         if ErrBlock = *on or ErrInsert = *on;
033100120216           leavesr;
033200120216         endif;
033300120207
033400120216         // Se possibile l'esecuzione della fase 2
033500120216         // ctrl esistenza di TNTAM sul DB remoto
033600120216         exsr EsistenzaTNTAM;
033700120216         //richiamo apposito pgm per fase 2
033800120216         TRULKSCR1(VIDKSC:EsisteTNTAM:V1DMSG:VIDUSR:VIDPWD);
033900120216         // se ho ricevuto un msg di errore, attivo il flag
034000120216         if V1DMSG <> *blank;
034100120216           ErrMsg = *on;
034200120216         endif;
034300120215
034400120208         // - Chiusura del programma (solo se non ho errori da mostrare)
034500120208         if ErrMsg = *off;
034600120208           $Fine = *on;
034700120208         endif;
034800120207
034900120207       ENDSR;
035000120208
035100120207       //--------------------------------------------------------------
035200120208       // Gestione tasto funzionale F12 da videata D01
035300120208       // F12=Ritorno
035400120207       //--------------------------------------------------------------
035500120207       BEGSR  sr_F12D01;
035600120207
035700120207         // - Chiusura del programma
035800120207         $Fine = *on;
035900120207
036000120207       ENDSR;
036100120207
036200120207       //--------------------------------------------------------------
036300120207       // Controllo videata D01                                        ?
036400120207       //--------------------------------------------------------------
036500120207       BEGSR  sr_CtrD01;
036600120207
036700120207         IndDspF  = *off;
036800120208
036900120208         // l'utente remoto va indicato
037000120208         if VIDUsr = *blank;
037100120208           V1Dmsg = 'Utente remoto obbligatorio';
037200120208           ErrMsg = *on;
037300120208           ErrGenerico = *on;
037400120208           leavesr;
037500120208         endif;
037600120208
037700120208         // la pwd dell'utente remoto va indicata
037800120208         if VIDPwd = *blank;
037900120208           V1Dmsg = 'Pwd utente remoto obbligatoria';
038000120208           ErrMsg = *on;
038100120208           ErrGenerico = *on;
038200120208           leavesr;
038300120208         endif;
038400120208
038500120215         // il cliente va indicato
038600120215         if VIDKsc = 0;
038700120215           V1Dmsg = 'Cliente obbligatorio';
038800120215           ErrMsg = *on;
038900120215           ErrGenerico = *on;
039000120215           leavesr;
039100120215         endif;
039200120207
039300120207       ENDSR;
039400120208
039500120208       //--------------------------------------------------------------
039600120208       // Reperisco i dati da copiare
039700120208       //--------------------------------------------------------------
039800120216       BEGSR  CopiaDaSetras;
039900120208
040000120208         ErrBlock = *off;
040100120208
040200120208         // - Reperisci dati da aggiungere
040300120208
040400120208         //reperisco dati CNACO
040500120210         //---------------------
040600120208         clear *all CNACO00F;
040700120208         // - Creazione stringa SQL da eseguire
040800120208         wSQL = 'select * from CNACO00F +
040900120209                 where ACOKSC = ' + %char(VIDKSC) + ' and ACOKCC=151';
041000120208         // - Apertura cursore
041100120208         exsr  sr_OpenCursor;
041200120208         CNACOoccurs = 1;
041300120209         // punto all'occorrenza da valorizzare
041400120209         %occur(CNACO00F) = CNACOoccurs;
041500120208         DoU  SQLCode = 100 or SQLCode < 0;
041600120209           exec sql  fetch next  from C1  into :CNACO00F;
041700120208           exsr  sr_ReadCursor;
041800120208           // se non viene letto rcd alla prima lettura, significa che non esiste un cliente con quel codice
041900120210           // errore bloccante
042000120208           select;
042100120209           // errore
042200120209           when SQLCode < 0;
042300120209             V1Dmsg = 'Errore in  lettura CNACO00F - vedi stampa errore';
042400120208             ErrMsg = *on;
042500120208             ErrGenerico = *on;
042600120208             ErrBlock = *on;
042700120208             // - Chiusura cursore (visto che esco con leavesr)
042800120208             exsr  sr_CloseCursor;
042900120208             leavesr;
043000120209           // nessuna lettura
043100120209           when SQLCode = 100 and CNACOoccurs = 1;
043200120209             V1Dmsg = 'Cliente inesistente';
043300120209             ErrMsg = *on;
043400120209             ErrGenerico = *on;
043500120209             ErrBlock = *on;
043600120209             // - Chiusura cursore (visto che esco con leavesr)
043700120209             exsr  sr_CloseCursor;
043800120209             leavesr;
043900120209           // lettura
044000120209           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
044100120208             CNACOoccurs = CNACOoccurs + 1;
044200120209             // punto all'occorrenza da valorizzare - per CNACO00F non lo faccio perché ha 1 sola occorrenza
044300120209             // %occur(CNACO00F) = CNACOoccurs;
044400120209           // fine lettura
044500120209           other;
044600120209             // non faccio nulla
044700120209           endsl;
044800120208         EndDo;
044900120208         // - Chiusura cursore
045000120208         exsr  sr_CloseCursor;
045100120209         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
045200120209         // le occorrenze davvero valorizzate sono inferiori di 1
045300120209         CNACOoccurs = CNACOoccurs - 1;
045400120208
045500120208         //reperisco dati CNCLP
045600120210         //---------------------
045700120208         clear *all CNCLP00F;
045800120208         // - Creazione stringa SQL da eseguire
045900120208         wSQL = 'select * from CNCLP00F +
046000120209                 where CLPKSC = ' + %char(VIDKSC) + ' and CLPKCC=151';
046100120208         // - Apertura cursore
046200120208         exsr  sr_OpenCursor;
046300120208         CNCLPoccurs = 1;
046400120209         // punto all'occorrenza da valorizzare
046500120209         %occur(CNCLP00F) = CNCLPoccurs;
046600120208         DoU  SQLCode = 100 or SQLCode < 0;
046700120208           exec sql  fetch next  from C1  into :CNCLP00F;
046800120208           exsr  sr_ReadCursor;
046900120208           // se non viene letto rcd alla prima lettura, significa che non esiste un cliente con quel codice
047000120208           // improbabile avere un errore del genere quì, si sarebbe dovuto bloccare prima su CNACO00F
047100120210           // però è accaduto!
047200120210           // errore bloccante
047300120209           select;
047400120209           // errore
047500120209           when SQLCode < 0;
047600120209             V1Dmsg = 'Errore in  lettura CNCLP00F - vedi stampa errore';
047700120209             ErrMsg = *on;
047800120209             ErrGenerico = *on;
047900120209             ErrBlock = *on;
048000120209             // - Chiusura cursore (visto che esco con leavesr)
048100120209             exsr  sr_CloseCursor;
048200120209             leavesr;
048300120209           // nessuna lettura
048400120209           when SQLCode = 100 and CNCLPoccurs = 1;
048500120210             V1Dmsg = 'Cliente non inserito completamente';
048600120210             ErrMsg = *on;
048700120210             ErrGenerico = *on;
048800120210             ErrBlock = *on;
048900120210             // - Chiusura cursore (visto che esco con leavesr)
049000120210             exsr  sr_CloseCursor;
049100120210             leavesr;
049200120209           // lettura
049300120209           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
049400120209             CNCLPoccurs = CNCLPoccurs + 1;
049500120209             // punto all'occorrenza da valorizzare - per CNCLP00F non lo faccio perché ha 1 sola occorrenza
049600120209             // %occur(CNCLP00F) = CNCLPoccurs;
049700120209           // fine lettura
049800120209           other;
049900120209             // non faccio nulla
050000120209           endsl;
050100120208         EndDo;
050200120208         // - Chiusura cursore
050300120208         exsr  sr_CloseCursor;
050400120209         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
050500120209         // le occorrenze davvero valorizzate sono inferiori di 1
050600120209         CNCLPoccurs = CNCLPoccurs - 1;
050700120208
050800120208         //reperisco dati CNIND
050900120210         //---------------------
051000120208         clear *all CNIND00F;
051100120208         // - Creazione stringa SQL da eseguire
051200120208         wSQL = 'select * from CNIND00F +
051300120209                 where INDKSC = ' + %char(VIDKSC) + ' and INDKCC=151';
051400120208         // - Apertura cursore
051500120208         exsr  sr_OpenCursor;
051600120208         CNINDoccurs = 1;
051700120209         // punto all'occorrenza da valorizzare
051800120209         %occur(CNIND00F) = CNINDoccurs;
051900120208         DoU  SQLCode = 100 or SQLCode < 0;
052000120208           exec sql  fetch next  from C1  into :CNIND00F;
052100120208           exsr  sr_ReadCursor;
052200120208           // se non viene letto rcd alla prima lettura, significa che non esiste un cliente con quel codice
052300120208           // improbabile avere un errore del genere quì, si sarebbe dovuto bloccare prima su CNACO00F
052400120210           // però è accaduto!
052500120210           // errore bloccante
052600120209           select;
052700120209           // errore
052800120209           when SQLCode < 0;
052900120209             V1Dmsg = 'Errore in  lettura CNIND00F - vedi stampa errore';
053000120209             ErrMsg = *on;
053100120209             ErrGenerico = *on;
053200120209             ErrBlock = *on;
053300120209             // - Chiusura cursore (visto che esco con leavesr)
053400120209             exsr  sr_CloseCursor;
053500120209             leavesr;
053600120209           // nessuna lettura
053700120209           when SQLCode = 100 and CNINDoccurs = 1;
053800120210             V1Dmsg = 'Cliente non inserito completamente';
053900120210             ErrMsg = *on;
054000120210             ErrGenerico = *on;
054100120210             ErrBlock = *on;
054200120210             // - Chiusura cursore (visto che esco con leavesr)
054300120210             exsr  sr_CloseCursor;
054400120210             leavesr;
054500120209           // lettura
054600120209           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
054700120208             CNINDoccurs = CNINDoccurs + 1;
054800120209             // punto all'occorrenza da valorizzare - per CNIND00F non lo faccio perché ha 1 sola occorrenza
054900120209             // %occur(CNIND00F) = CNINDoccurs;
055000120209           // fine lettura
055100120209           other;
055200120209             // non faccio nulla
055300120209           endsl;
055400120208         EndDo;
055500120208         // - Chiusura cursore
055600120208         exsr  sr_CloseCursor;
055700120209         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
055800120209         // le occorrenze davvero valorizzate sono inferiori di 1
055900120209         CNINDoccurs = CNINDoccurs - 1;
056000120208
056100120208         //reperisco dati FNCLS
056200120210         //---------------------
056300120208         clear *all FNCLS00F;
056400120208         // - Creazione stringa SQL da eseguire
056500120208         wSQL = 'select * from FNCLS00F +
056600120208                 where CLSKSC = ' + %char(VIDKSC);
056700120208         // - Apertura cursore
056800120208         exsr  sr_OpenCursor;
056900120208         FNCLSoccurs = 1;
057000120209         // punto all'occorrenza da valorizzare
057100120209         %occur(FNCLS00F) = FNCLSoccurs;
057200120208         DoU  SQLCode = 100 or SQLCode < 0;
057300120208           exec sql  fetch next  from C1  into :FNCLS00F;
057400120208           exsr  sr_ReadCursor;
057500120208           // se non viene letto rcd alla prima lettura, significa che non esiste un cliente con quel codice
057600120208           // improbabile avere un errore del genere quì, si sarebbe dovuto bloccare prima su CNACO00F
057700120210           // però è accaduto!
057800120210           // errore bloccante
057900120209           select;
058000120209           // errore
058100120209           when SQLCode < 0;
058200120209             V1Dmsg = 'Errore in  lettura FNCLS00F - vedi stampa errore';
058300120209             ErrMsg = *on;
058400120209             ErrGenerico = *on;
058500120209             ErrBlock = *on;
058600120209             // - Chiusura cursore (visto che esco con leavesr)
058700120209             exsr  sr_CloseCursor;
058800120209             leavesr;
058900120209           // nessuna lettura
059000120209           when SQLCode = 100 and FNCLSoccurs = 1;
059100120210             V1Dmsg = 'Cliente non inserito completamente';
059200120210             ErrMsg = *on;
059300120210             ErrGenerico = *on;
059400120210             ErrBlock = *on;
059500120210             // - Chiusura cursore (visto che esco con leavesr)
059600120210             exsr  sr_CloseCursor;
059700120210             leavesr;
059800120209           // lettura
059900120209           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
060000120208             FNCLSoccurs = FNCLSoccurs + 1;
060100120209             // punto all'occorrenza da valorizzare - per FNCLS00F non lo faccio perché ha 1 sola occorrenza
060200120209             // %occur(FNCLS00F) = FNCLSoccurs;
060300120209           // fine lettura
060400120209           other;
060500120209             // non faccio nulla
060600120209           endsl;
060700120208         EndDo;
060800120208         // - Chiusura cursore
060900120208         exsr  sr_CloseCursor;
061000120209         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
061100120209         // le occorrenze davvero valorizzate sono inferiori di 1
061200120209         FNCLSoccurs = FNCLSoccurs - 1;
061300120208
061400120208         //reperisco dati TABEL
061500120210         //---------------------
061600120208         clear *all TABEL00F;
061700120208         // - Creazione stringa SQL da eseguire
061800120209         wSQL = 'select * from TABEL00F +
061900120216                 where TBLKEY like ''%' + %editc(VIDKSC:'X') + '%''';
062000120208         // - Apertura cursore
062100120208         exsr  sr_OpenCursor;
062200120209         TABELoccurs = 1;
062300120209         // punto all'occorrenza da valorizzare
062400120209         %occur(TABEL00F) = TABELoccurs;
062500120208         DoU  SQLCode = 100 or SQLCode < 0;
062600120208           exec sql  fetch next  from C1  into :TABEL00F;
062700120208           exsr  sr_ReadCursor;
062800120208           // è possibile che non venga letto rcd alla prima lettura
062900120208           // quindi non blocco il flusso
063000120209           select;
063100120209           // errore
063200120209           when SQLCode < 0;
063300120209             V1Dmsg = 'Errore in  lettura TABEL00F - vedi stampa errore';
063400120209             ErrMsg = *on;
063500120209             ErrGenerico = *on;
063600120209             ErrBlock = *on;
063700120209             // - Chiusura cursore (visto che esco con leavesr)
063800120209             exsr  sr_CloseCursor;
063900120209             leavesr;
064000120209           // nessuna lettura
064100120209           when SQLCode = 100 and TABELoccurs = 1;
064200120209             // non faccio nulla
064300120209           // lettura
064400120209           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
064500120208             TABELoccurs = TABELoccurs + 1;
064600120209             // punto all'occorrenza da valorizzare
064700120209             %occur(TABEL00F) = TABELoccurs;
064800120209           // fine lettura
064900120209           other;
065000120209             // non faccio nulla
065100120209           endsl;
065200120208         EndDo;
065300120208         // - Chiusura cursore
065400120208         exsr  sr_CloseCursor;
065500120209         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
065600120209         // le occorrenze davvero valorizzate sono inferiori di 1
065700120209         TABELoccurs = TABELoccurs - 1;
065800120208
065900120208         //reperisco dati TNTAM
066000120210         //---------------------
066100120208         clear *all TNTAM00F;
066200120208         // - Creazione stringa SQL da eseguire
066300120208         wSQL = 'select * from TNTAM00F +
066400120208                 where TAMKSC =' + %char(VIDKSC);
066500120208         // - Apertura cursore
066600120208         exsr  sr_OpenCursor;
066700120208         TNTAMoccurs = 1;
066800120209         // punto all'occorrenza da valorizzare
066900120209         %occur(TNTAM00F) = TNTAMoccurs;
067000120208         DoU  SQLCode = 100 or SQLCode < 0;
067100120208           exec sql  fetch next  from C1  into :TNTAM00F;
067200120208           exsr  sr_ReadCursor;
067300120208           // è possibile che non venga letto rcd alla prima lettura (cliente totalmente nuovo)
067400120208           // quindi non blocco il flusso
067500120209           select;
067600120209           // errore
067700120209           when SQLCode < 0;
067800120209             V1Dmsg = 'Errore in  lettura TNTAM00F - vedi stampa errore';
067900120209             ErrMsg = *on;
068000120209             ErrGenerico = *on;
068100120209             ErrBlock = *on;
068200120209             // - Chiusura cursore (visto che esco con leavesr)
068300120209             exsr  sr_CloseCursor;
068400120209             leavesr;
068500120209           // nessuna lettura
068600120209           when SQLCode = 100 and TNTAMoccurs = 1;
068700120209             // non faccio nulla
068800120209           // lettura
068900120209           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
069000120208             TNTAMoccurs = TNTAMoccurs + 1;
069100120209             // punto all'occorrenza da valorizzare
069200120209             %occur(TNTAM00F) = TNTAMoccurs;
069300120209           // fine lettura
069400120209           other;
069500120209             // non faccio nulla
069600120209           endsl;
069700120208         EndDo;
069800120208         // - Chiusura cursore
069900120208         exsr  sr_CloseCursor;
070000120209         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
070100120209         // le occorrenze davvero valorizzate sono inferiori di 1
070200120209         TNTAMoccurs = TNTAMoccurs - 1;
070300120208
070400120208         //reperisco dati TNTBE
070500120210         //---------------------
070600120208         clear *all TNTBE00F;
070700120208         // - Creazione stringa SQL da eseguire
070800120208         wSQL = 'select * from TNTBE00F +
070900120216                 where TBEKE1 like ''%' + %editc(VIDKSC:'X') + '%'' +
071000120209                 or TBEKE2 like ''%'
071100120216                  + %editc(VIDKSC:'X') + '%''';
071200120208         // - Apertura cursore
071300120208         exsr  sr_OpenCursor;
071400120209         TNTBEoccurs = 1;
071500120209         // punto all'occorrenza da valorizzare
071600120209         %occur(TNTBE00F) = TNTBEoccurs;
071700120208         DoU  SQLCode = 100 or SQLCode < 0;
071800120208           exec sql  fetch next  from C1  into :TNTBE00F;
071900120208           exsr  sr_ReadCursor;
072000120208           // è possibile che non venga letto rcd alla prima lettura
072100120208           // quindi non blocco il flusso
072200120209           select;
072300120209           // errore
072400120209           when SQLCode < 0;
072500120209             V1Dmsg = 'Errore in  lettura TNTBE00F - vedi stampa errore';
072600120209             ErrMsg = *on;
072700120209             ErrGenerico = *on;
072800120209             ErrBlock = *on;
072900120209             // - Chiusura cursore (visto che esco con leavesr)
073000120209             exsr  sr_CloseCursor;
073100120209             leavesr;
073200120209           // nessuna lettura
073300120209           when SQLCode = 100 and TNTBEoccurs = 1;
073400120209             // non faccio nulla
073500120209           // lettura
073600120209           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
073700120208             TNTBEoccurs = TNTBEoccurs + 1;
073800120209             // punto all'occorrenza da valorizzare
073900120209             %occur(TNTBE00F) = TNTBEoccurs;
074000120209           // fine lettura
074100120209           other;
074200120209             // non faccio nulla
074300120209           endsl;
074400120208         EndDo;
074500120208         // - Chiusura cursore
074600120208         exsr  sr_CloseCursor;
074700120209         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
074800120209         // le occorrenze davvero valorizzate sono inferiori di 1
074900120209         TNTBEoccurs = TNTBEoccurs - 1;
075000120208
075100120208       ENDSR;
075200111128
075300111128       //--------------------------------------------------------------
075400120207       // Scrivo rcd estratti
075500111128       //--------------------------------------------------------------
075600120216       BEGSR  CopiaSuAS888;
075700111128
075800120210         ErrMsg = *off;
075900120210         ErrGenerico = *off;
076000120216         ErrBlock = *off;
076100120216         ErrInsert = *off;
076200120210
076300120207         // mi connetto ad AS888
076400120216         exsr ConnectToAS888;
076500120216         if ErrBlock = *on;
076600120216           leavesr;
076700120216         endif;
076800120207
076900120208         // esecuzione scrittura rcd estratti
077000120208         exsr sr_InsertRcd;
077100120208
077200120208         // mi disconnetto da AS888
077300120216         exsr DisconnectFromAS888;
077400120216         if ErrBlock = *on;
077500120216           leavesr;
077600120216         endif;
077700120208
077800120208         // mi riconnetto a SETRAS
077900120216         exsr ConnectToSetras;
078000120216         if ErrBlock = *on;
078100120216           leavesr;
078200120216         endif;
078300120207
078400111128       ENDSR;
078500120216
078600120216       //--------------------------------------------------------------
078700120216       // Connessione
078800120216       //--------------------------------------------------------------
078900120216       BEGSR  ConnectToAS888;
079000120216
079100120216         // mi connetto ad AS888
079200120216         exec sql  connect to :DBRemote user :VIDUSR using :VIDPWD;
079300120216         if SQLCode < 0;
079400120216           //emetto msg di errore perentorio
079500120216           V1Dmsg = 'FASE 1: non riuscita connessione ad AS888 +
079600120216                     - SCOLLEGARE la sessione e riprovare';
079700120216           ErrMsg = *on;
079800120216           ErrGenerico = *on;
079900120216           ErrBlock = *on;
080000120216         endif;
080100120216
080200120216       ENDSR;
080300120216
080400120216       //--------------------------------------------------------------
080500120216       // Disconnessione
080600120216       //--------------------------------------------------------------
080700120216       BEGSR  DisconnectFromAS888;
080800120216
080900120216         // mi disconnetto da AS888
081000120216         exec sql  disconnect :DBRemote;
081100120216         // se la disconnessione ha dato errore
081200120216         if SQLCode < 0;
081300120216           //emetto msg di errore perentorio
081400120216           if ErrInsert = *on;
081500120216             V1Dmsg = 'FASE 1: agg. NON eseguito ED errore in +
081600120216                       connessione SETRAS - SCOLLEGARSI!';
081700120216           else;
081800120216             V1Dmsg = 'FASE 1: agg. eseguito ma errore in +
081900120216                       connessione SETRAS - SCOLLEGARSI!';
082000120216           endif;
082100120216           ErrMsg = *on;
082200120216           ErrGenerico = *on;
082300120216           ErrBlock = *on;
082400120216         endif;
082500120216
082600120216       ENDSR;
082700120216
082800120216       //--------------------------------------------------------------
082900120216       // Riconnessione a SETRAS
083000120216       //--------------------------------------------------------------
083100120216       BEGSR  ConnectToSetras;
083200120216
083300120216         // mi riconnetto a SETRAS
083400120216         // siccome in BRT gli SQLRPGLE si compilano col parametro RDBCNNMTH = *DUW, la connect ad AS888 NON ha chiuso
083500120216         // la connessione a SETRAS, l'ha resa dorminte. In questo caso NON serve un cmd CONNECT RESET perché  darebbe
083600120216         // errore SQL0842 = Connessione al database relazionale &1 già esistente.
083700120216         // Però, la connessione al DB locale (SETRAS) esiste se prima c'è stato almeno uno statement sql se quel DB.
083800120216         // In questo caso la SET CONNECTION dà errore SQL0842 = Il collegamento al database relazionale &1 non esiste
083900120216         // e va eseguita la CONNECT RESET.
084000120216         // in questo pgm dovrebbe sempre esserci prima uno statement SQL su SETRAS per leggere i dati,
084100120216         // ma non si sa mai... magari vengono fatte modifiche
084200120216         // Per cui, dopo aver disconnesso AS888, fare il cmd SET CONNECTION per "risvegliare" SETRAS e, se dà errore
084300120216         // provare con CONNECT RESET.
084400120216         // Solo se entrambi hanno dato errore, emettere un msg a video.
084500120216         exec sql  set connection SETRAS;
084600120216         if SQLCode < 0;
084700120216           // provo con CONNECT RESET
084800120216           exec sql  connect reset;
084900120216           // se anche in questo caso ho errore, emetto msg
085000120216           if SQLCode < 0;
085100120216             //emetto msg di errore perentorio
085200120216             if ErrInsert = *on;
085300120216               V1Dmsg = 'FASE 1: agg. NON eseguito ED errore in +
085400120216                         connessione SETRAS - SCOLLEGARSI!';
085500120216             else;
085600120216               V1Dmsg = 'FASE 1: agg. eseguito ma errore in +
085700120216                         connessione DB SETRAS - SCOLLEGARSI!';
085800120216             endif;
085900120216             ErrMsg = *on;
086000120216             ErrGenerico = *on;
086100120216             ErrBlock = *on;
086200120216           endif;
086300120216         endif;
086400120216
086500120216       ENDSR;
086600111128
086700120208       //--------------------------------------------------------------
086800120208       // Scrivo rcd estratti
086900120208       //--------------------------------------------------------------
087000120208       BEGSR  sr_InsertRcd;
087100120208
087200120208         // aggiungo il record letto al CNACO00F di test
087300120227         EXEC SQL insert into UNITRAGRP/CNACO00F :CNACOoccurs rows
087400120227                  values(:CNACO00F)
087500120210                  with UR;
087600120208         if SQLCode < 0;
087700120208         // se ho avuto un errore nell'insert, a regime sarà solo per chiave duplicata, quindi interrompo la copia
087800120208         // dando errore
087900120210           exec sql rollback;
088000120210           V1Dmsg = 'Cliente già esistente su AS888 +
088100120208                     - controllare';
088200120208           ErrMsg = *on;
088300120208           ErrGenerico = *on;
088400120210           ErrInsert = *on;
088500120208           leavesr;
088600120208         endif;
088700120208
088800120208         // aggiungo il record letto al CNCLP00F di test
088900120208         if CNCLPoccurs > 0;
089000120227           EXEC SQL insert into UNITRAGRP/CNCLP00F :CNCLPoccurs rows
089100120227                    values(:CNCLP00F)
089200120210                    with UR;
089300120208           if SQLCode < 0;
089400120208             // improbabile avere un errore nell'insert, si sarebbe dovuto bloccare prima su CNACO00F
089500120210             // ma lo testo lo stesso per il rollback
089600120210             exec sql rollback;
089700120210             V1Dmsg = 'Errore in insert CNCLP00F su AS888';
089800120210             ErrMsg = *on;
089900120210             ErrGenerico = *on;
090000120210             ErrInsert = *on;
090100120210             leavesr;
090200120208           endif;
090300120208         endif;
090400120208
090500120208         // aggiungo il record letto al CNIND00F di test
090600120208         if CNINDoccurs > 0;
090700120227           EXEC SQL insert into UNITRAGRP/CNIND00F :CNINDoccurs rows
090800120227                    values(:CNIND00F)
090900120210                    with UR;
091000120208           if SQLCode < 0;
091100120210             // improbabile avere un errore nell'insert, si sarebbe dovuto bloccare prima su CNACO00F
091200120210             // ma lo testo lo stesso per il rollback
091300120210             exec sql rollback;
091400120210             V1Dmsg = 'Errore in insert CNIND00F su AS888';
091500120210             ErrMsg = *on;
091600120210             ErrGenerico = *on;
091700120210             ErrInsert = *on;
091800120210             leavesr;
091900120208           endif;
092000120208         endif;
092100120208
092200120208         // aggiungo il record letto al FNCLS00F di test
092300120208         if FNCLSoccurs > 0;
092400120227           EXEC SQL insert into UNITRAGRP/FNCLS00F :FNCLSoccurs rows
092500120227                    values(:FNCLS00F)
092600120210                    with UR;
092700120208           if SQLCode < 0;
092800120210             // improbabile avere un errore nell'insert, si sarebbe dovuto bloccare prima su CNACO00F
092900120210             // ma lo testo lo stesso per il rollback
093000120210             exec sql rollback;
093100120210             V1Dmsg = 'Errore in insert FNCLS00F su AS888';
093200120210             ErrMsg = *on;
093300120210             ErrGenerico = *on;
093400120210             ErrInsert = *on;
093500120210             leavesr;
093600120208           endif;
093700120208         endif;
093800120208
093900120227         // aggiungo i record letti al TABEL00F di test in sede
094000120208         if TABELoccurs > 0;
094100120227           EXEC SQL insert into GAITRAGRPS/TABEL00F :TABELoccurs rows
094200120227                    values(:TABEL00F)
094300120210                    with UR;
094400120208           if SQLCode < 0;
094500120208             // se ho avuto un errore nell'insert, a regime sarà solo per chiave duplicata, ma non è un file vincolante
094600120208             // per cui lo segnalo solo ma proseguo
094700120227             V1Dmsg = 'Errore di copia sui file tabelle SEDE +
094800120208                       - controllare';
094900120208             ErrMsg = *on;
095000120208             ErrGenerico = *on;
095100120208           endif;
095200120208         endif;
095300120227
095400120227         // aggiungo i record letti al TABEL00F di test in filiale
095500120227         if TABELoccurs > 0;
095600120227           EXEC SQL insert into FILTRAGRPF/TABEL00F :TABELoccurs rows
095700120227                    values(:TABEL00F)
095800120227                    with UR;
095900120227           if SQLCode < 0;
096000120227             // se ho avuto un errore nell'insert, a regime sarà solo per chiave duplicata, ma non è un file vincolante
096100120227             // per cui lo segnalo solo ma proseguo
096200120227             V1Dmsg = 'Errore di copia sui file tabelle FILIALE +
096300120227                       - controllare';
096400120227             ErrMsg = *on;
096500120227             ErrGenerico = *on;
096600120227           endif;
096700120227         endif;
096800120208
096900120208         // aggiungo il record letto al TNTAM00F di test
097000120208         if TNTAMoccurs > 0;
097100120227           EXEC SQL insert into UNITRAGRP/TNTAM00F :TNTAMoccurs rows
097200120227                    values(:TNTAM00F)
097300120210                    with UR;
097400120208           if SQLCode < 0;
097500120208             // improbabile avere un errore nell'insert, si sarebbe dovuto bloccare prima su CNACO00F
097600120208             // quindi non prevedo il caso
097700120208           endif;
097800120208         endif;
097900120208
098000120227         // aggiungo i record letti al TNTBE00F di test di sede
098100120208         if TNTBEoccurs > 0;
098200120227           EXEC SQL insert into GAITRAGRPS/TNTBE00F :TNTBEoccurs rows
098300120227                    values(:TNTBE00F)
098400120210                    with UR;
098500120208           if SQLCode < 0;
098600120208             // se ho avuto un errore nell'insert, a regime sarà solo per chiave duplicata, ma non è un file vincolante
098700120208             // per cui lo segnalo solo ma proseguo
098800120227             V1Dmsg = 'Errore di copia sui file tabelle SEDE +
098900120208                       - controllare';
099000120208             ErrMsg = *on;
099100120208             ErrGenerico = *on;
099200120208           endif;
099300120208         endif;
099400120227
099500120227         // aggiungo i record letti al TNTBE00F di test di filiale
099600120227         if TNTBEoccurs > 0;
099700120227           EXEC SQL insert into FILTRAGRPF/TNTBE00F :TNTBEoccurs rows
099800120227                    values(:TNTBE00F)
099900120227                    with UR;
100000120227           if SQLCode < 0;
100100120227             // se ho avuto un errore nell'insert, a regime sarà solo per chiave duplicata, ma non è un file vincolante
100200120227             // per cui lo segnalo solo ma proseguo
100300120227             V1Dmsg = 'Errore di copia sui file tabelle FILIALE +
100400120227                       - controllare';
100500120227             ErrMsg = *on;
100600120227             ErrGenerico = *on;
100700120227           endif;
100800120227         endif;
100900120208
101000120210         // se sono qui vuol dire che ho scritto tutto per cui committo
101100120210         exec sql commit;
101200120210
101300120208       ENDSR;
101400120215
101500120215       //--------------------------------------------------------------
101600120215       // Controlla esistenza TNTAM per il cliente da copiare
101700120215       //--------------------------------------------------------------
101800120215       BEGSR  EsistenzaTNTAM;
101900120215
102000120216         EsisteTNTAM = *off;
102100120215
102200120216         // se ho eseguito la fase 1 questa informazione ce l'ho in canna
102300120216         if TNTAMoccurs > 0;
102400120216           EsisteTNTAM = *on;
102500120216         endif;
102600120215
102700120215       ENDSR;
102800120215
102900120215       //--------------------------------------------------------------
103000120215       // Esecuzione del comando (già impostato)
103100120215       //--------------------------------------------------------------
103200120215       BEGSR  sr_ExecCmd;
103300120215
103400111128         clear Qcap0100;
103500111128         Qcabcsdh = *off;
103600111128         Qcapa    = *off;
103700111128         Qcacmdss = *off;
103800111128         Qcaerved = *allX'00';
103900111128
104000111128         clear Qusec;
104100111128         Qusbprv  = %size(Qusec);
104200111128
104300111128         ProcessCommands ( Qcmd : %len( %trimr( Qcmd ) ) : Qcap0100 :
104400111128                           %size(Qcap0100) : 'CPOP0100' : *omit :
104500111128                           0 : 0 : Qusec);
104600111128
104700120210         // - Stampa DUMP + JobLog  &  Chiusura *pgm  in caso di errore
104800111128         if  Qusei <> *blank;
104900111128           exsr  sr_PrintErr;
105000111128         endif;
105100111128
105200111128       ENDSR;
105300111128
105400111128       //--------------------------------------------------------------
105500120210       // Stampa segnalazione dell'errore rilevato
105600111128       //--------------------------------------------------------------
105700111128       BEGSR  sr_PrintErr;
105800111128
105900120210         // - Stampa del Dump
106000111128         Dump(A);
106100111128
106200120210         // - Stampa del Job-Log
106300111128         Qcmd = 'DSPJOBLOG job(*) output(*print)';
106400111128         exsr  sr_ExecCmd;
106500111128
106600111128       ENDSR;
106700111128
106800111128       //--------------------------------------------------------------
106900120208       // Operazioni finali
107000111128       //--------------------------------------------------------------
107100111128       BEGSR  sr_RoutEnd;
107200111128
107300120208         // - Uscita
107400111128         return;
107500111128
107600111128       ENDSR;
107700111128
107800111128      /end-free
