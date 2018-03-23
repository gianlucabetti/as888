000100111128       //==============================================================
000200120416       // TNVRA02R - Verificare quante delle spedizioni per le quali Tellus non è riuscito ad
000300120416       // attribuire in automatico il giro consegna sarebbero "forzabili" o "suggeribili"
000400120416       // usando il giro già attribuito manualmente dall'utente su precedenti spedizioni, aventi
000500120416       // medesimo indirizzo.
000600120405       //==============================================================
000700111128
000800120404       // per compilare si deve avere in linea la lib. FILTRAPRD
000900111128
001000111128     /*PRM  dbgview(*source)
001100111128     /*END
001200111128
001300111128       //--------------------------------------------------------------
001400120207       // Specifiche di controllo.                                     ?
001500111128       //--------------------------------------------------------------
001600111128
001700111128     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001800120416     h dftactgrp(*no) bnddir('UBBNDDIR') actgrp(*caller)
001900111128     h alwnull(*inputonly)
002000111128
002100111128       //--------------------------------------------------------------
002200120207       // Dichiarazione file.                                          ?
002300111128       //--------------------------------------------------------------
002400120413
002500120413
002600111128       //--------------------------------------------------------------
002700120207       // Definizione costanti.                                        ?
002800111128       //--------------------------------------------------------------
002900111128
003000120413
003100111128       //--------------------------------------------------------------
003200120207       // Definizione schiere.                                         ?
003300111128       //--------------------------------------------------------------
003400111128
003500111128
003600111128       //--------------------------------------------------------------
003700120207       // Definizione aree dati.                                       ?
003800111128       //--------------------------------------------------------------
003900111128
004000111128
004100111128       //--------------------------------------------------------------
004200120207       // Definizione strutture dati.                                  ?
004300111128       //--------------------------------------------------------------
004400120413     d FIARG00F      e ds
004500120413     d FNARB00F      e ds
004600120416     d DatiXStampa     ds                  occurs(30)
004700120413     d  AAS_non                            like(ARBAAS)
004800120413     d  LNP_non                            like(ARBLNP)
004900120413     d  NRS_non                            like(ARBNRS)
005000120413     d  NSP_non                            like(ARBNSP)
005100120413     d  AAS                                like(ARBAAS)
005200120413     d  LNP                                like(ARBLNP)
005300120413     d  NRS                                like(ARBNRS)
005400120413     d  NSP                                like(ARBNSP)
005500120413     d  TGI                                like(ARGTGI)
005600120413     d  CGI                                like(ARGCGI)
005700120413     d  IND                                like(ARBIND)
005800120413     d  CAD                                like(ARBCAD)
005900120413     d  LOD                                like(ARBLOD)
006000120413     d  PRD                                like(ARBPRD)
006100120413     d  Dt_Sped                       8s 0
006200120416     d  TotNonAtt                     5s 0
006300111128
006400111128       //--------------------------------------------------------------
006500120207       // Definizione variabili globali.                               ?
006600111128       //--------------------------------------------------------------
006700111128
006800120208       // - Stringa SQL da eseguire
006900120413     d wSQl            s           5000    inz  varying
007000111128
007100120208       // - Parametri SQL
007200120207
007300120207       // - Flags booleani
007400120208     d ErrBlock        s               n   inz(*off)
007500120207
007600120207       // - Variabili di appoggio
007700120416     d Qcmd            s            500    inz
007800120405     d wX              s              5s 0
007900120413     d wDataDa         s              8s 0
008000120413     d wDataA          s              8s 0
008100120416     d Lib             s             10
008200120416     D currSysNeta     s              8A   inz('*NULL')
008300120416     d Dati_Occurs     s              3s 0
008400111128
008500111128       //--------------------------------------------------------------
008600120210       // Definizione procedure usate
008700111128       //--------------------------------------------------------------
008800120207
008900120210       // - Parametri API QCAPCMD (Process Commands)
009000120207      /copy qSysInc/qRpgleSrc,QCAPCMD
009100120207      /copy gaitrasrc/srcProtoPR,QCAPCMD
009200120207
009300120210       // - Parametri gestione errori API
009400120207      /copy qSysInc/qRpgleSrc,QUSEC
009500120416
009600120416      /COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
009700120416      /COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
009800120404
009900120416     d TNVRA02R        pr
010000120416     d  in_DtRif                      8s 0
010100120413     d  out_Errore                    1
010200120404
010300120416     d TNVRA02R        pi
010400120416     d  in_DtRif                      8s 0
010500120413     d  out_Errore                    1
010600111128
010700111128       //--------------------------------------------------------------
010800120208       // Definizione key-list.                                        ?
010900111128       //--------------------------------------------------------------
011000111128
011100111128
011200111128       //--------------------------------------------------------------
011300120208       // M A I N - L I N E                                            ?
011400111128       //--------------------------------------------------------------
011500111128
011600111128      /free
011700111128
011800120208       // - Operazioni iniziali
011900111128       exsr  sr_RoutInz;
012000111128
012100120207       // - Gestione video
012200120413       exsr  RepDati;
012300120207
012400120208       // - Operazioni finali
012500111128       exsr  sr_RoutEnd;
012600111128
012700111128       //--------------------------------------------------------------
012800120208       // Operazioni iniziali
012900111128       //--------------------------------------------------------------
013000111128       BEGSR sr_RoutInz;
013100111128
013200111128         *inLR = *on;
013300120413         out_Errore=*blank;
013400111128
013500120208         // - Impostazione opzioni per SQL
013600111128         exec SQL   set option   DynUsrPrf = *owner,
013700111128                                 CloSqlCsr = *endmod;
013800111128
013900120413         // il range delle date va da -8 a -1 della data in ingresso
014000120413         wDataDa=%dec(%date(in_DtRif)-%days(8));
014100120413         wDataA =%dec(%date(in_DtRif)-%days(1));
014200120416
014300120416         // Reperisco il sistema AS/400 corrente
014400120416         // e, a seconda di qual è, imposto la libreria corretta per ogni file
014500120416         callp     UBRTVNETA_Rtv(currSysNeta);
014600120416         if %subst(currSysNeta:1:6) = 'SETRAS';
014700120416           Lib = 'FILTRA201';
014800120416         else;
014900120416           Lib = 'FILTRAPRD';
015000120416         endif;
015100120413
015200111128       ENDSR;
015300111128
015400111128       //--------------------------------------------------------------
015500120208       // Apertura cursore C1
015600111128       //--------------------------------------------------------------
015700111128       BEGSR  sr_OpenCursor;
015800111128
015900120208         // - Dichiarazione del cursore
016000111128         exec sql  prepare S1  from :wSQL;
016100111128         exec sql  declare C1  cursor for S1;
016200111128
016300120208         // - Apertura del cursore
016400111128         exec sql   open C1;
016500111128
016600111128       ENDSR;
016700111128
016800111128       //--------------------------------------------------------------
016900120208       // Chiusura cursore C1
017000111128       //--------------------------------------------------------------
017100111128       BEGSR  sr_CloseCursor;
017200111128
017300120208         // - Chiusura del cursore
017400111128         exec sql   close C1;
017500111128
017600111128       ENDSR;
017700111128
017800111128       //--------------------------------------------------------------
017900120208       // Lettura cursore C1
018000111128       //--------------------------------------------------------------
018100111128       BEGSR  sr_ReadCursor;
018200111128
018300111128         select;
018400111128
018500120208           // - Fine lettura
018600111128           when  sqlCode = 100;
018700120413             //non devo fare nulla perché uscirò dal ciclo
018800120208           leavesr;
018900111128
019000120208           // - Errore
019100111128           when  sqlCode < *zero;
019200111128             exsr  sr_PrintErr;
019300120413             // errore sql: guarda la stampa
019400120413             out_Errore = '1';
019500120413             ErrBlock = *on;
019600120416             // - Chiusura cursore
019700120413             exsr  sr_CloseCursor;
019800120416             // - Uscita dal pgm
019900120416             exsr  sr_RoutEnd;
020000120413             leavesr;
020100111128
020200120208           // - Elaborazione
020300111128           other;
020400120416             Dati_occurs = Dati_occurs + 1;
020500120416             // punto all'occorrenza da valorizzare se non siamo già arrivati a fine occorrenza
020600120416             if Dati_occurs <= %elem(DatiXStampa);
020700120416               // punto all'occorrenza da valorizzare
020800120416               %occur(DatiXStampa) = Dati_occurs;
020900120416             endif;
021000111128
021100111128         endsl;
021200111128
021300111128       ENDSR;
021400120207
021500120207       //--------------------------------------------------------------
021600120413       // Reperimento ciclico dati
021700120207       //--------------------------------------------------------------
021800120413       BEGSR  RepDati;
021900120208
022000120208         ErrBlock = *off;
022100120208
022200120413         // - Reperisci dati
022300120208
022400120413         clear DatiXStampa;
022500120208         // - Creazione stringa SQL da eseguire
022600120413         wSQL = 'with '                                                        +
022700120416                'NON_ATT as '                                           +
022800120416                '(select * from ' + %trim(Lib) + '/fnarb00f '                  +
022900120416                'join ' + %trim(Lib) + '/fiarg00f on '                         +
023000120413                'arbaas=argaas and arblnp=arglnp and '                         +
023100120413                'arbnrs=argnrs and arbnsp=argnsp '                             +
023200120416                'where arbdbr = ' + %char(in_DtRif) + ' and argtgi<>''A''), '  +
023300120416                ' '                                                            +
023400120416                'NON_ATT_TOT as '                                      +
023500120416                '(select count(*) as TOT_NON from NON_ATT), '           +
023600120416                ' '                                                            +
023700120416                'ATT as '                                               +
023800120416                '(select * from ' + %trim(Lib) + '/fnarb00f '                  +
023900120416                'join ' + %trim(Lib) + '/fiarg00f on '                         +
024000120413                'arbaas=argaas and arblnp=arglnp and '                         +
024100120413                'arbnrs=argnrs and arbnsp=argnsp '                             +
024200120416                'where arbdbr between ' + %char(wDataDa) + ' and '             +
024300120416                %char(wDataA) +
024400120416                ' and argtgi=''M'' and argcgi<>'' '') '                        +
024500120413                ' '                                                            +
024600120413                'select '                                                      +
024700120416                'NON_ATT.argaas, NON_ATT.arglnp, '               +
024800120416                'NON_ATT.argnrs, NON_ATT.argnsp, '               +
024900120416                'ATT.argaas, ATT.arglnp, ATT.argnrs, '    +
025000120416                'ATT.argnsp, ATT.argtgi, ATT.argcgi, '    +
025100120416                'ATT.arbind, ATT.arbcad, ATT.arblod, '    +
025200120416                'ATT.arbprd, '                                          +
025300120416                'ATT.arbaas*10000+ATT.arbmgs as DT_SPED_ATT, '   +
025400120416                'NON_ATT_TOT.TOT_NON '                                  +
025500120416                'from ATT, NON_ATT, NON_ATT_TOT where '          +
025600120416                'ATT.arbind = NON_ATT.arbind and '               +
025700120416                'ATT.arbcad = NON_ATT.arbcad and '               +
025800120416                'ATT.arblod = NON_ATT.arblod and '               +
025900120416                'ATT.arbprd = NON_ATT.arbprd '                   +
026000120413                'order by DT_SPED_ATT desc';
026100120413
026200120208         // - Apertura cursore
026300120208         exsr  sr_OpenCursor;
026400120416         Dati_occurs = 1;
026500120416         // punto all'occorrenza da valorizzare
026600120416         %occur(DatiXStampa) = Dati_occurs;
026700120416         // ciclo finché non ho finito le righe da leggere
026800120416         DoU  SQLCode = 100
026900120416         // oppure non ho avuto un errore
027000120416           or SQLCode < 0
027100120416         // oppure le righe lette sono quelle massime
027200120416           or Dati_occurs > %elem(DatiXStampa);
027300120413           exec sql  fetch next  from C1  into :DatiXStampa;
027400120208           exsr  sr_ReadCursor;
027500120208         EndDo;
027600120416
027700120416         // - stampo i dati estratti se ne ho letto almeno 1
027800120416         IF Dati_occurs > 0;
027900120416           exsr Stampa;
028000120416         endif;
028100120416
028200120208         // - Chiusura cursore
028300120208         exsr  sr_CloseCursor;
028400120208
028500120208       ENDSR;
028600120405
028700120405       //--------------------------------------------------------------
028800120413       // Stampo la riga estratta
028900120405       //--------------------------------------------------------------
029000120413       BEGSR  Stampa;
029100120405
029200120207
029300111128       ENDSR;
029400120215
029500120215
029600120215       //--------------------------------------------------------------
029700120215       // Esecuzione del comando (già impostato)
029800120215       //--------------------------------------------------------------
029900120215       BEGSR  sr_ExecCmd;
030000120215
030100111128         clear Qcap0100;
030200111128         Qcabcsdh = *off;
030300111128         Qcapa    = *off;
030400111128         Qcacmdss = *off;
030500111128         Qcaerved = *allX'00';
030600111128
030700111128         clear Qusec;
030800111128         Qusbprv  = %size(Qusec);
030900111128
031000111128         ProcessCommands ( Qcmd : %len( %trimr( Qcmd ) ) : Qcap0100 :
031100111128                           %size(Qcap0100) : 'CPOP0100' : *omit :
031200111128                           0 : 0 : Qusec);
031300111128
031400120210         // - Stampa DUMP + JobLog  &  Chiusura *pgm  in caso di errore
031500111128         if  Qusei <> *blank;
031600111128           exsr  sr_PrintErr;
031700111128         endif;
031800111128
031900111128       ENDSR;
032000111128
032100111128       //--------------------------------------------------------------
032200120210       // Stampa segnalazione dell'errore rilevato
032300111128       //--------------------------------------------------------------
032400111128       BEGSR  sr_PrintErr;
032500111128
032600120210         // - Stampa del Dump
032700111128         Dump(A);
032800111128
032900120210         // - Stampa del Job-Log
033000111128         Qcmd = 'DSPJOBLOG job(*) output(*print)';
033100111128         exsr  sr_ExecCmd;
033200111128
033300111128       ENDSR;
033400111128
033500111128       //--------------------------------------------------------------
033600120208       // Operazioni finali
033700111128       //--------------------------------------------------------------
033800111128       BEGSR  sr_RoutEnd;
033900111128
034000120208         // - Uscita
034100120413         return;
034200111128
034300111128       ENDSR;
034400111128
034500111128      /end-free
