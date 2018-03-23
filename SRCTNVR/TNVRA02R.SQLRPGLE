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
004400120417     d kpjba         e ds
004500120413     d FIARG00F      e ds
004600120413     d FNARB00F      e ds
004700120417     d DatiXStampa     ds                  qualified dim(30)
004800120413     d  AAS_non                            like(ARBAAS)
004900120413     d  LNP_non                            like(ARBLNP)
005000120413     d  NRS_non                            like(ARBNRS)
005100120413     d  NSP_non                            like(ARBNSP)
005200120413     d  AAS                                like(ARBAAS)
005300120413     d  LNP                                like(ARBLNP)
005400120413     d  NRS                                like(ARBNRS)
005500120413     d  NSP                                like(ARBNSP)
005600120413     d  TGI                                like(ARGTGI)
005700120413     d  CGI                                like(ARGCGI)
005800120413     d  IND                                like(ARBIND)
005900120413     d  CAD                                like(ARBCAD)
006000120413     d  LOD                                like(ARBLOD)
006100120413     d  PRD                                like(ARBPRD)
006200120413     d  Dt_Sped                       8s 0
006300120416     d  TotNonAtt                     5s 0
006400111128
006500111128       //--------------------------------------------------------------
006600120207       // Definizione variabili globali.                               ?
006700111128       //--------------------------------------------------------------
006800111128
006900120208       // - Stringa SQL da eseguire
007000120413     d wSQl            s           5000    inz  varying
007100111128
007200120208       // - Parametri SQL
007300120207
007400120207       // - Flags booleani
007500120208     d ErrBlock        s               n   inz(*off)
007600120207
007700120207       // - Variabili di appoggio
007800120416     d Qcmd            s            500    inz
007900120405     d wX              s              5s 0
008000120413     d wDataDa         s              8s 0
008100120413     d wDataA          s              8s 0
008200120416     d Lib             s             10
008300120416     D currSysNeta     s              8A   inz('*NULL')
008400120417     d Dati_NrRows     s              3s 0
008500111128
008600111128       //--------------------------------------------------------------
008700120210       // Definizione procedure usate
008800111128       //--------------------------------------------------------------
008900120207
009000120210       // - Parametri API QCAPCMD (Process Commands)
009100120207      /copy qSysInc/qRpgleSrc,QCAPCMD
009200120207      /copy gaitrasrc/srcProtoPR,QCAPCMD
009300120207
009400120210       // - Parametri gestione errori API
009500120207      /copy qSysInc/qRpgleSrc,QUSEC
009600120416
009700120416      /COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
009800120416      /COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
009900120404
010000120416     d TNVRA02R        pr
010100120416     d  in_DtRif                      8s 0
010200120417     d  in_kpjba                           like(kpjba)
010300120413     d  out_Errore                    1
010400120404
010500120416     d TNVRA02R        pi
010600120416     d  in_DtRif                      8s 0
010700120417     d  in_kpjba                           like(kpjba)
010800120413     d  out_Errore                    1
010900120417
011000120417     d TNVRA03R        pr                  extpgm('TNVRA03R')
011100120417     d  in_Dati                            likeds(DatiXStampa) dim(30)
011200120417     d  in_NrRows                          like(Dati_NrRows)
011300120417     d  in_DtRif                      8s 0
011400120417     d  in_kpjba                           like(kpjba)
011500120417     d  out_Errore                    1
011600111128
011700111128       //--------------------------------------------------------------
011800120208       // Definizione key-list.                                        ?
011900111128       //--------------------------------------------------------------
012000111128
012100111128
012200111128       //--------------------------------------------------------------
012300120208       // M A I N - L I N E                                            ?
012400111128       //--------------------------------------------------------------
012500111128
012600111128      /free
012700111128
012800120208       // - Operazioni iniziali
012900111128       exsr  sr_RoutInz;
013000111128
013100120207       // - Gestione video
013200120413       exsr  RepDati;
013300120207
013400120208       // - Operazioni finali
013500111128       exsr  sr_RoutEnd;
013600111128
013700111128       //--------------------------------------------------------------
013800120208       // Operazioni iniziali
013900111128       //--------------------------------------------------------------
014000111128       BEGSR sr_RoutInz;
014100111128
014200111128         *inLR = *on;
014300120413         out_Errore=*blank;
014400111128
014500120208         // - Impostazione opzioni per SQL
014600111128         exec SQL   set option   DynUsrPrf = *owner,
014700111128                                 CloSqlCsr = *endmod;
014800111128
014900120413         // il range delle date va da -8 a -1 della data in ingresso
015000120413         wDataDa=%dec(%date(in_DtRif)-%days(8));
015100120413         wDataA =%dec(%date(in_DtRif)-%days(1));
015200120416
015300120416         // Reperisco il sistema AS/400 corrente
015400120416         // e, a seconda di qual è, imposto la libreria corretta per ogni file
015500120416         callp     UBRTVNETA_Rtv(currSysNeta);
015600120416         if %subst(currSysNeta:1:6) = 'SETRAS';
015700120416           Lib = 'FILTRA201';
015800120416         else;
015900120416           Lib = 'FILTRAPRD';
016000120416         endif;
016100120413
016200111128       ENDSR;
016300111128
016400111128       //--------------------------------------------------------------
016500120208       // Apertura cursore C1
016600111128       //--------------------------------------------------------------
016700111128       BEGSR  sr_OpenCursor;
016800111128
016900120208         // - Dichiarazione del cursore
017000111128         exec sql  prepare S1  from :wSQL;
017100111128         exec sql  declare C1  cursor for S1;
017200111128
017300120208         // - Apertura del cursore
017400111128         exec sql   open C1;
017500111128
017600111128       ENDSR;
017700111128
017800111128       //--------------------------------------------------------------
017900120208       // Chiusura cursore C1
018000111128       //--------------------------------------------------------------
018100111128       BEGSR  sr_CloseCursor;
018200111128
018300120208         // - Chiusura del cursore
018400111128         exec sql   close C1;
018500111128
018600111128       ENDSR;
018700111128
018800111128       //--------------------------------------------------------------
018900120208       // Lettura cursore C1
019000111128       //--------------------------------------------------------------
019100111128       BEGSR  sr_ReadCursor;
019200111128
019300111128         select;
019400111128
019500120208           // - Fine lettura
019600111128           when  sqlCode = 100;
019700120417             // For a FETCH statement, SQLERRD(3) contains the number of rows
019800120417             Dati_NrRows=sqlErrD(3);
019900120417             //non devo fare nulla perché leggo tutte le righe in un blocco
020000120417             leavesr;
020100111128
020200120208           // - Errore
020300111128           when  sqlCode < *zero;
020400111128             exsr  sr_PrintErr;
020500120413             // errore sql: guarda la stampa
020600120417             out_Errore = '7';
020700120417             Dati_NrRows=0;
020800120416             // - Chiusura cursore
020900120413             exsr  sr_CloseCursor;
021000120416             // - Uscita dal pgm
021100120416             exsr  sr_RoutEnd;
021200120413             leavesr;
021300111128
021400120208           // - Elaborazione
021500111128           other;
021600120417             Dati_NrRows=sqlErrD(3);
021700120417             //non devo fare nulla perché leggo tutte le righe in un blocco
021800120417             leavesr;
021900111128
022000111128         endsl;
022100111128
022200111128       ENDSR;
022300120207
022400120207       //--------------------------------------------------------------
022500120413       // Reperimento ciclico dati
022600120207       //--------------------------------------------------------------
022700120413       BEGSR  RepDati;
022800120208
022900120208         ErrBlock = *off;
023000120208
023100120413         // - Reperisci dati
023200120208
023300120413         clear DatiXStampa;
023400120208         // - Creazione stringa SQL da eseguire
023500120413         wSQL = 'with '                                                        +
023600120416                'NON_ATT as '                                           +
023700120416                '(select * from ' + %trim(Lib) + '/fnarb00f '                  +
023800120416                'join ' + %trim(Lib) + '/fiarg00f on '                         +
023900120413                'arbaas=argaas and arblnp=arglnp and '                         +
024000120413                'arbnrs=argnrs and arbnsp=argnsp '                             +
024100120416                'where arbdbr = ' + %char(in_DtRif) + ' and argtgi<>''A''), '  +
024200120416                ' '                                                            +
024300120416                'NON_ATT_TOT as '                                      +
024400120416                '(select count(*) as TOT_NON from NON_ATT), '           +
024500120416                ' '                                                            +
024600120416                'ATT as '                                               +
024700120416                '(select * from ' + %trim(Lib) + '/fnarb00f '                  +
024800120416                'join ' + %trim(Lib) + '/fiarg00f on '                         +
024900120413                'arbaas=argaas and arblnp=arglnp and '                         +
025000120413                'arbnrs=argnrs and arbnsp=argnsp '                             +
025100120416                'where arbdbr between ' + %char(wDataDa) + ' and '             +
025200120416                %char(wDataA) +
025300120416                ' and argtgi=''M'' and argcgi<>'' '') '                        +
025400120413                ' '                                                            +
025500120413                'select '                                                      +
025600120416                'NON_ATT.argaas, NON_ATT.arglnp, '               +
025700120416                'NON_ATT.argnrs, NON_ATT.argnsp, '               +
025800120416                'ATT.argaas, ATT.arglnp, ATT.argnrs, '    +
025900120416                'ATT.argnsp, ATT.argtgi, ATT.argcgi, '    +
026000120416                'ATT.arbind, ATT.arbcad, ATT.arblod, '    +
026100120416                'ATT.arbprd, '                                          +
026200120416                'ATT.arbaas*10000+ATT.arbmgs as DT_SPED_ATT, '   +
026300120416                'NON_ATT_TOT.TOT_NON '                                  +
026400120416                'from ATT, NON_ATT, NON_ATT_TOT where '          +
026500120416                'ATT.arbind = NON_ATT.arbind and '               +
026600120416                'ATT.arbcad = NON_ATT.arbcad and '               +
026700120416                'ATT.arblod = NON_ATT.arblod and '               +
026800120416                'ATT.arbprd = NON_ATT.arbprd '                   +
026900120413                'order by DT_SPED_ATT desc';
027000120413
027100120208         // - Apertura cursore
027200120208         exsr  sr_OpenCursor;
027300120417         exec sql  fetch next  from C1  for 30 rows into :DatiXStampa;
027400120417         exsr  sr_ReadCursor;
027500120416
027600120416         // - stampo i dati estratti se ne ho letto almeno 1
027700120417         IF Dati_NrRows > 0;
027800120416           exsr Stampa;
027900120417         else;
028000120417           out_Errore = '4';
028100120416         endif;
028200120416
028300120208         // - Chiusura cursore
028400120208         exsr  sr_CloseCursor;
028500120208
028600120208       ENDSR;
028700120405
028800120405       //--------------------------------------------------------------
028900120413       // Stampo la riga estratta
029000120405       //--------------------------------------------------------------
029100120413       BEGSR  Stampa;
029200120405
029300120417         TNVRA03R(DatiXStampa:Dati_NrRows:in_DtRif:kpjba:out_Errore);
029400120417         // se ho ricevuto un msg di errore, lo passo indietro come '6'=Errore in stampa
029500120417         select;
029600120417         when out_Errore = '1';
029700120417           out_Errore = '6';
029800120417         endsl;
029900120207
030000111128       ENDSR;
030100120215
030200120215
030300120215       //--------------------------------------------------------------
030400120215       // Esecuzione del comando (già impostato)
030500120215       //--------------------------------------------------------------
030600120215       BEGSR  sr_ExecCmd;
030700120215
030800111128         clear Qcap0100;
030900111128         Qcabcsdh = *off;
031000111128         Qcapa    = *off;
031100111128         Qcacmdss = *off;
031200111128         Qcaerved = *allX'00';
031300111128
031400111128         clear Qusec;
031500111128         Qusbprv  = %size(Qusec);
031600111128
031700111128         ProcessCommands ( Qcmd : %len( %trimr( Qcmd ) ) : Qcap0100 :
031800111128                           %size(Qcap0100) : 'CPOP0100' : *omit :
031900111128                           0 : 0 : Qusec);
032000111128
032100120210         // - Stampa DUMP + JobLog  &  Chiusura *pgm  in caso di errore
032200111128         if  Qusei <> *blank;
032300111128           exsr  sr_PrintErr;
032400111128         endif;
032500111128
032600111128       ENDSR;
032700111128
032800111128       //--------------------------------------------------------------
032900120210       // Stampa segnalazione dell'errore rilevato
033000111128       //--------------------------------------------------------------
033100111128       BEGSR  sr_PrintErr;
033200111128
033300120210         // - Stampa del Dump
033400111128         Dump(A);
033500111128
033600120210         // - Stampa del Job-Log
033700111128         Qcmd = 'DSPJOBLOG job(*) output(*print)';
033800111128         exsr  sr_ExecCmd;
033900111128
034000111128       ENDSR;
034100111128
034200111128       //--------------------------------------------------------------
034300120208       // Operazioni finali
034400111128       //--------------------------------------------------------------
034500111128       BEGSR  sr_RoutEnd;
034600111128
034700120208         // - Uscita
034800120413         return;
034900111128
035000111128       ENDSR;
035100111128
035200111128      /end-free
